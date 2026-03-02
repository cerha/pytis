# -*- coding: utf-8 -*-

# Copyright (C) 2026 Tomáš Cerha <cerha@truecode.cz>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import contextlib
import dataclasses
import functools
import threading
import sqlalchemy as sa
import sqlalchemy.orm as orm
import pytis.data.gensqlalchemy
import pytis
import typing
from sqlalchemy.sql.elements import ColumnElement

SQLTabular = type[pytis.data.gensqlalchemy.SQLTable] | type[pytis.data.gensqlalchemy.SQLView]


class Database:
    """Encapsulates database engine and session factory.

    Create once at application startup via Database.create(config_path) and
    pass the instance to add_api_routes().  Call dispose() on shutdown to
    release all pooled connections.

    """

    def __init__(self, engine: sa.engine.Engine, session: orm.sessionmaker):
        self._engine = engine
        self.session = session

    @classmethod
    def create(cls, config_path: str) -> 'Database':
        """Initialise pytis config and create engine and session factory.

        Args:
            config_path: Path to the pytis configuration file.

        Returns:
            A ready-to-use Database instance.

        """
        pytis.config.add_command_line_options(('pytis', '--config', config_path))
        engine = sa.create_engine(
            sa.engine.URL.create(
                drivername='postgresql+psycopg2',
                host=pytis.config.dbhost,
                port=pytis.config.dbport,
                database=pytis.config.dbname,
            ),
            # Credentials are passed through connect_args rather than embedded
            # in the URL so they are never included in SQLAlchemy's string
            # representation of the engine (logged on connection errors, etc.).
            connect_args={
                'user': pytis.config.dbuser,
                'password': pytis.config.dbpass,
            },
            pool_pre_ping=True,
            future=True,  # Simplify future transition to SQLAlchemy 2.0.
        )
        session = orm.sessionmaker(
            bind=engine, autoflush=False, autocommit=False,
            expire_on_commit=False, future=True,
        )
        return cls(engine=engine, session=session)

    def check_connection(self) -> bool:
        """Verify database connectivity by executing a trivial query."""
        with self._engine.connect() as conn:
            with conn.execute(sa.text('SELECT 1')) as result:
                return result.scalar() == 1

    @contextlib.asynccontextmanager
    async def lifespan(self, app: typing.Any):
        """FastAPI lifespan context manager.

        Disposes the connection pool on application shutdown.  Pass as::

            app = FastAPI(lifespan=db.lifespan)

        The ``app`` argument is provided by FastAPI but is not used here.

        """
        yield
        self._engine.dispose()


class NonUniqueKeyError(RuntimeError):
    """Raised when a key expected to be unique matches multiple rows."""

class PayloadError(ValueError):
    """Invalid payload for insert/update (unknown columns, invalid shape)."""

class DataConsistencyError(RuntimeError):
    """Raised when stored relational data references a non-existent row.

    Signals a referential-integrity gap not enforced by the database (e.g. a
    FK value that no longer has a matching row in the target table).  Relation
    handlers raise this; CRUD methods catch it and map it to HTTP 500.

    """


class ConstraintViolationError(RuntimeError):
    """Database constraint violation with structured metadata."""
    _ERROR_MAP = {
        "23505": "unique violation",
        "23503": "foreign key violation",
        "23502": "not null violation",
        "23514": "check violation",
    }

    def __init__(self, e: sa.exc.IntegrityError):
        super().__init__(str(e))
        diag = getattr(e.orig, "diag", None)
        self._detail = dict(
            error=self._ERROR_MAP.get(getattr(e.orig, "pgcode", None), "integrity_error"),
            constraint=getattr(diag, "constraint_name", None) if diag else None,
            column=getattr(diag, "column_name", None) if diag else None,
        )

    @property
    def detail(self) -> dict:
        return self._detail


# Sorting direction constants.
#
# Names are deliberately identical to pytis.data.ASCENDENT / pytis.data.DESCENDANT
# so that code using both layers does not require a mental mapping.  The values
# are plain strings rather than integers (pytis uses str here too) so that
# debugging output is self-explanatory.
ASCENDENT = 'ASCENDENT'
DESCENDANT = 'DESCENDANT'

# A sorting specification: a sequence of (column_name, direction) pairs.
# direction must be ASCENDENT or DESCENDANT.
# Example: (('created_at', DESCENDANT), ('name', ASCENDENT))
Sorting = tuple[tuple[str, str], ...]


class Operator:
    """Base class for all condition operators accepted by PytisAccessor.

    Design
    ------

    Operator instances represent query conditions as plain Python objects.
    Column references are plain strings (column names), not SA Column objects,
    so callers use the names they already know from the spec.

    Each subclass implements expression(table), which translates the condition
    to a SQLAlchemy ColumnElement bound to a specific table.  Column-name
    validation (PayloadError on unknown names) is performed there via the
    shared _col() helper.  PytisAccessor passes its own table and calls
    condition.expression(table) directly — it never builds SA clauses itself.

    Relation to pytis.data operators
    ---------------------------------

    pytis.data defines a parallel Operator hierarchy with the same conceptual
    operators (EQ, NE, LT, LE, GT, GE, AND, OR, NOT, WM, IN, …).  The key
    difference is that pytis.data operands are pytis.data.Value instances
    (value + pytis type), because the pytis data layer owns both type
    validation and query construction.

    Here, FastAPI/Pydantic already handle type validation before values reach
    the accessor, so wrapping plain Python values in pytis.data.Value would be
    pure overhead.  Plain Python values are used directly and SQLAlchemy
    handles type coercion at the driver level.

    Operator names are aligned with pytis.data where semantics match:

        EQ, NE, LT, LE, GT, GE, AND, OR, NOT
            Identical semantics to their pytis.data counterparts.

        IN
            Simple value-list membership (SQL: col IN (...)).
            pytis.data uses ANY_OF for this pattern (expanded to OR-of-EQ)
            and reserves IN for a subquery form.  We use SQL IN directly.

        LIKE
            Pattern matching with SQL LIKE / ILIKE syntax (% and _ wildcards).
            pytis.data calls the equivalent WM ("wildcard match") and uses its
            own wildcard syntax (* → %, ? → _).

        EQ('col', None) / NE('col', None)
            Translate to IS NULL / IS NOT NULL respectively, mirroring the
            pytis.data convention of EQ('col', Value(type, None)) for null
            tests.  No separate IsNull/IsNotNull classes are needed.

    """

    def _col(self, table: sa.Table, name: str) -> sa.Column:
        """Return the SA Column for name, raising PayloadError if unknown."""
        c = table.c.get(name)
        if c is None:
            raise PayloadError(f"Unknown column in condition: {name!r}")
        return c

    def expression(self, table: sa.Table) -> ColumnElement:
        """Translate this operator to a SQLAlchemy WHERE clause element.

        Args:
            table: The SA Table whose columns are referenced by name.

        Raises:
            PayloadError: If a column name does not exist in table.

        """
        raise NotImplementedError


# --- comparison operators ---------------------------------------------------

@dataclasses.dataclass(frozen=True, slots=True)
class EQ(Operator):
    """Equality: column = value.  EQ('col', None) translates to IS NULL."""
    column: str
    value: typing.Any

    def expression(self, table: sa.Table) -> ColumnElement:
        c = self._col(table, self.column)
        return c.is_(None) if self.value is None else c == self.value


@dataclasses.dataclass(frozen=True, slots=True)
class NE(Operator):
    """Inequality: column != value.  NE('col', None) translates to IS NOT NULL."""
    column: str
    value: typing.Any

    def expression(self, table: sa.Table) -> ColumnElement:
        c = self._col(table, self.column)
        return c.isnot(None) if self.value is None else c != self.value


@dataclasses.dataclass(frozen=True, slots=True)
class LT(Operator):
    """Less-than: column < value."""
    column: str
    value: typing.Any

    def expression(self, table: sa.Table) -> ColumnElement:
        return self._col(table, self.column) < self.value


@dataclasses.dataclass(frozen=True, slots=True)
class LE(Operator):
    """Less-than-or-equal: column <= value."""
    column: str
    value: typing.Any

    def expression(self, table: sa.Table) -> ColumnElement:
        return self._col(table, self.column) <= self.value


@dataclasses.dataclass(frozen=True, slots=True)
class GT(Operator):
    """Greater-than: column > value."""
    column: str
    value: typing.Any

    def expression(self, table: sa.Table) -> ColumnElement:
        return self._col(table, self.column) > self.value


@dataclasses.dataclass(frozen=True, slots=True)
class GE(Operator):
    """Greater-than-or-equal: column >= value."""
    column: str
    value: typing.Any

    def expression(self, table: sa.Table) -> ColumnElement:
        return self._col(table, self.column) >= self.value


@dataclasses.dataclass(frozen=True, slots=True)
class IN(Operator):
    """Value-list membership: column IN (values).

    Translates to a SQL IN clause.  An empty values tuple produces a
    condition that never matches any row (equivalent to SQL FALSE).

    """
    column: str
    values: tuple[typing.Any, ...]

    def expression(self, table: sa.Table) -> ColumnElement:
        if not self.values:
            return sa.false()
        return self._col(table, self.column).in_(self.values)


@dataclasses.dataclass(frozen=True, slots=True)
class LIKE(Operator):
    """Pattern match: column LIKE pattern (or ILIKE when ignore_case=True).

    Use standard SQL wildcard syntax: % matches any sequence of characters,
    _ matches exactly one character.

    """
    column: str
    pattern: str
    ignore_case: bool = False

    def expression(self, table: sa.Table) -> ColumnElement:
        c = self._col(table, self.column)
        return c.ilike(self.pattern) if self.ignore_case else c.like(self.pattern)


# --- logical combinators ----------------------------------------------------

class AND(Operator):
    """Logical conjunction: all conditions must hold."""
    __slots__ = ('conditions',)

    def __init__(self, *conditions: Operator):
        self.conditions = conditions

    def expression(self, table: sa.Table) -> ColumnElement:
        return sa.and_(*[c.expression(table) for c in self.conditions])


class OR(Operator):
    """Logical disjunction: at least one condition must hold."""
    __slots__ = ('conditions',)

    def __init__(self, *conditions: Operator):
        self.conditions = conditions

    def expression(self, table: sa.Table) -> ColumnElement:
        return sa.or_(*[c.expression(table) for c in self.conditions])


class NOT(Operator):
    """Logical negation: condition must not hold."""
    __slots__ = ('condition',)

    def __init__(self, condition: Operator):
        self.condition = condition

    def expression(self, table: sa.Table) -> ColumnElement:
        return sa.not_(self.condition.expression(table))


class PytisAccessor:
    """Low-level database accessor built on SQLAlchemy ORM.

    This class provides a thin, explicit abstraction over a single SQLTable/SQLView
    definition. It exposes:

        - mapped ORM entity (imperative mapping),
        - filtered column metadata (excluding technical columns),
        - CRUD operations working with API-level keys (dict of column→value).

    Design principles:

        - No hidden policy or business logic.
        - No automatic translation between API keys and primary keys.
        - Explicit error signaling (PayloadError, NonUniqueKeyError,
          ConstraintViolationError).
        - Single-column database primary key is required internally.

    This class is transport-agnostic and framework-agnostic. It does not know
    about FastAPI or HTTP semantics.

    """
    _registry = orm.registry()
    _EXCLUDE_COLUMNS = {'vytvoril', 'vytvoreno', 'zmenil', 'zmeneno'}
    _lock = threading.Lock()

    def __init__(self, spec: SQLTabular):
        """Initialize accessor for a specific SQLTable/SQLView spec.

        Args:
            spec: Table/view specification class.

        Raises:
            ValueError: If the underlying table has no primary key or a
                        multi-column primary key (currently unsupported).

        """
        with pytis.data.gensqlalchemy.local_search_path(spec.default_search_path()):
            table = pytis.data.gensqlalchemy.object_by_class(spec)
        pk_columns = list(table.primary_key.columns)
        if len(pk_columns) != 1:
            raise ValueError(f'Expected single-column primary key, got {len(pk_columns)}')
        self._primary_key = pk = pk_columns[0]
        self._spec = spec
        self._table = table
        self._entity = self._create_entity(table)
        self._columns = [c for c in table.columns if c.name not in self._EXCLUDE_COLUMNS]
        self._column_names = {c.name for c in self._columns}
        self._sorting: Sorting = ((pk.name, ASCENDENT),)

    @classmethod
    @functools.lru_cache(maxsize=None)
    def _create_entity(cls, table) -> type:
        entity = type(f'{table.name}Row', (), {})
        cls._registry.map_imperatively(
            entity,
            table,
            include_properties=[
                c.name for c in table.columns
                if c.name not in cls._EXCLUDE_COLUMNS
            ],
        )
        return entity

    @classmethod
    def create(cls, spec) -> 'PytisAccessor':
        """Return a cached accessor for a given spec, thread-safely.

        The lock ensures that concurrent first-calls for the same spec do not
        both execute __init__ (and therefore map_imperatively) simultaneously.
        After the first call the cached result is returned inside the lock,
        so the overhead for subsequent calls is just one lock acquire/release.

        """
        with cls._lock:
            return cls._create_cached(spec)

    @classmethod
    @functools.lru_cache(maxsize=None)
    def _create_cached(cls, spec) -> 'PytisAccessor':
        return cls(spec)

    @property
    def spec(self) -> SQLTabular:
        """Original SQLTable/SQLView specification."""
        return self._spec

    @property
    def table(self) -> sa.Table:
        """Underlying SQLAlchemy Table object."""
        return self._table

    @property
    def columns(self) -> list[sa.Column]:
        """List of exposed SQLAlchemy columns (technical columns excluded)."""
        return self._columns

    @property
    def primary_key(self) -> sa.Column:
        """Database primary key column (single-column only)."""
        return self._primary_key

    def row(self, session: orm.Session, condition: Operator):
        """Return a single row matching condition.

        Args:
            session: SQLAlchemy session.
            condition: Filter condition as an Operator tree.  For a typical
                single-column key lookup, pass ``EQ('column', value)``.

        Returns:
            ORM entity instance or None if no matching row exists.

        Raises:
            PayloadError: If a column name in the condition is unknown.
            NonUniqueKeyError: If the condition matches more than one row.

        Notes:
            The condition is not required to match the primary key; it may
            represent any unique constraint chosen by the API layer.

        """
        stmt = (
            sa.select(self._entity)
            .where(condition.expression(self._table))
            .limit(2)
        )
        rows = session.execute(stmt).scalars().all()
        if not rows:
            return None
        if len(rows) != 1:
            raise NonUniqueKeyError(f"Condition matches multiple rows: {condition!r}")
        return rows[0]

    def rows(self, session: orm.Session, *,
             condition: Operator | None = None,
             sorting: Sorting | None = None,
             limit: int | None = None,
             offset: int | None = None):
        """Return rows, optionally filtered, ordered, and paged.

        Args:
            session: SQLAlchemy session.
            condition: Optional filter condition as an Operator tree.
                None means no filtering (all rows returned).
            sorting: Optional ordering specification as a sequence of
                ``(column_name, direction)`` pairs, where direction is
                ``ASCENDENT`` or ``DESCENDANT``.
                Defaults to ascending order by primary key.
            limit: Maximum number of rows.  None means no limit.
            offset: Number of rows to skip.  None means no offset.

        Returns:
            List of ORM entity instances.

        Raises:
            PayloadError: If a column name in the sorting spec is unknown.
            ValueError: If an unknown direction is given.

        """
        if sorting is None:
            sorting = self._sorting
        order_by = []
        for col_name, direction in sorting:
            c = self._table.c.get(col_name)
            if c is None:
                raise PayloadError(f"Unknown sort column: {col_name!r}")
            if direction == ASCENDENT:
                order_by.append(c.asc())
            elif direction == DESCENDANT:
                order_by.append(c.desc())
            else:
                raise ValueError(f"Unknown sort direction: {direction!r}")
        stmt = sa.select(self._entity).order_by(*order_by)
        if condition is not None:
            stmt = stmt.where(condition.expression(self._table))
        if limit is not None:
            stmt = stmt.limit(limit)
        if offset is not None:
            stmt = stmt.offset(offset)
        return session.execute(stmt).scalars().all()

    def insert(self, session: orm.Session, **values):
        """Insert a new row.

        Args:
            session: SQLAlchemy session.
            **values: Column values.

        Returns:
            Newly created ORM entity instance.

        Raises:
            PayloadError: If unknown columns are provided.
            ConstraintViolationError: On database constraint violation.

        """
        unknown = set(values) - self._column_names
        if unknown:
            raise PayloadError(f"Unknown columns: {sorted(unknown)}")

        obj = self._entity()
        for k, v in values.items():
            setattr(obj, k, v)

        session.add(obj)
        try:
            session.flush()
        except sa.exc.IntegrityError as e:
            raise ConstraintViolationError(e) from e
        return obj

    def update(self, session: orm.Session, condition: Operator, **values):
        """Update a row matching condition.

        Args:
            session: SQLAlchemy session.
            condition: Filter condition identifying the row to update.
            **values: Columns to update.

        Returns:
            Updated entity instance or None if not found.

        Raises:
            PayloadError: On unknown columns, attempt to modify the primary
                key, or unknown column names in the condition.
            NonUniqueKeyError: If condition matches multiple rows.
            ConstraintViolationError: On database constraint violation.

        """
        unknown = set(values) - self._column_names
        if unknown:
            raise PayloadError(f"Unknown columns: {sorted(unknown)}")
        if self._primary_key.name in values:
            raise PayloadError("Primary key update is not supported")
        obj = self.row(session, condition)
        if obj is None:
            return None
        for k, v in values.items():
            setattr(obj, k, v)
        try:
            session.flush()
        except sa.exc.IntegrityError as e:
            raise ConstraintViolationError(e) from e
        return obj

    def delete(self, session: orm.Session, condition: Operator) -> bool:
        """Delete a row matching condition.

        Args:
            session: SQLAlchemy session.
            condition: Filter condition identifying the row to delete.

        Returns:
            True if deleted, False if not found.

        Raises:
            PayloadError: If a column name in the condition is unknown.
            NonUniqueKeyError: If condition matches multiple rows.
            ConstraintViolationError: On database constraint violation.

        """
        obj = self.row(session, condition)
        if obj is None:
            return False
        session.delete(obj)
        try:
            session.flush()
        except sa.exc.IntegrityError as e:
            raise ConstraintViolationError(e) from e
        return True

    def delete_instance(self, session: orm.Session, obj: typing.Any) -> None:
        """Mark an already-loaded ORM instance for deletion without flushing.

        Unlike delete(), this method does not flush the session.  It is
        intended for use in batch-delete loops where the caller controls when
        the deletes are flushed (and wraps the flush in a ConstraintViolationError
        handler if needed).

        Args:
            session: SQLAlchemy session.
            obj: ORM entity instance to delete.

        """
        session.delete(obj)
