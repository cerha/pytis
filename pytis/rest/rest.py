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

import dataclasses
import fastapi
import fastapi.security
import inspect
import pydantic
import sqlalchemy as sa
import sqlalchemy.orm as orm
import typing

from .db import (
    NonUniqueKeyError, PayloadError, ConstraintViolationError, DataConsistencyError,
    Operator, AND, EQ, IN, ASCENDENT, DESCENDANT, Sorting, Database, PytisAccessor,
    SQLTable, SQLTabular,
)

_api_key_header = fastapi.security.APIKeyHeader(name='X-API-Key', auto_error=True)


def api_key_dependency(get_key: typing.Callable[[], str | None]) -> fastapi.params.Depends:
    """Return a FastAPI dependency that validates the X-API-Key request header.

    `get_key` is called on every request to retrieve the expected key, so the
    value can change without restarting the process:

    ```
    router = fastapi.APIRouter(
        dependencies=[api_key_dependency(lambda: pytis.config.api_key)],
    )
    ```

    Arguments:
      get_key: Zero-argument callable returning the expected API key string, or
        `None` / empty string when not configured.

    Returns:
      A `fastapi.Depends` instance ready for use in `dependencies=`.

    """
    def verify(api_key: str = fastapi.Security(_api_key_header)) -> None:
        expected = get_key()
        if not expected:
            raise fastapi.HTTPException(
                status_code=fastapi.status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail='API key not configured on server.',
            )
        if api_key != expected:
            raise fastapi.HTTPException(
                status_code=fastapi.status.HTTP_403_FORBIDDEN,
                detail='Invalid API key.',
            )
    return fastapi.Depends(verify)


def datafield(*, default=dataclasses.MISSING, doc: str | None = None):
    """Create a dataclass field with optional documentation metadata.

    This helper keeps dataclass specs readable while still attaching attribute-
    level documentation in a structured way.

    Arguments:
      default: Field default value. If omitted, the field is required. doc: Human-
        readable documentation string for this attribute.

    Returns:
      A `dataclasses.Field` instance suitable for use in dataclass specs.

    """
    metadata = {'doc': doc} if doc else None
    if default is dataclasses.MISSING:
        return dataclasses.field(metadata=metadata)
    return dataclasses.field(default=default, metadata=metadata)


@dataclasses.dataclass(frozen=True, slots=True)
class ForeignKey:
    """Describe a simple foreign-key relation between parent and child table.

    This relation type represents a direct reference stored in the parent table.
    It is typically used for 1:1 or N:1 relations (the parent references exactly
    one target record).

    The target column does not have to be a primary key. The referenced table
    and column are resolved by introspecting the SQLAlchemy table definition of
    the parent table.

    The API layer may: * accept nested object data instead of a raw FK value *
    resolve the target entity by its API-level key * insert/update the target
    record and assign its internal PK automatically

    Notes:
      We intentionally do not bake an assumption "FK always targets PK" into the
      spec. The exact referenced column is obtained from SQLAlchemy metadata on
      the parent column.

    """
    fk: str = datafield(doc='FK column on parent table.')


@dataclasses.dataclass(frozen=True, slots=True)
class BindingTable:
    """Describe a relation realized through a binding (join) table.

    This relation type represents 1:N or N:N relations implemented via a
    separate binding table. The binding table contains:

      * a foreign key referencing the parent entity
      * a foreign key referencing the target entity
      * optionally additional columns carrying relation-specific attributes
        (e.g. validity intervals, roles, flags, etc.)

    From the API perspective, this relation is always represented as a
    collection (list) of nested objects.

    Read semantics (GET):
      * The relation is serialized as a list of target objects.
      * If expose=False, binding-table technical columns (PK, FK columns)
        are hidden.
      * If expose=False, additional binding-table columns may be projected
        onto the nested target object. The current implementation merges
        them directly into the nested object.

    Write semantics (POST / PUT / PATCH):
      * The nested list represents the desired final state of the relation.
      * The implementation performs a synchronization:
        - existing bindings not present in the request are removed
        - new bindings present in the request are created
        - bindings present in both are updated
      * Target entities are matched using their API-level key, not by
        internal primary key values.
      * If the target entity does not exist, it may be created depending
        on the execution policy.

    Modes:

        expose = True (not implemented yet)
            The binding table is exposed as a first-class API resource.
            Its primary key and all columns are visible and handled explicitly.

        expose = False (default)
            The binding table is projected:

    """

    table: SQLTabular = datafield(doc='Binding table definition.')
    parent_fk: str = datafield(doc='FK on binding table to parent.')
    target_fk: str = datafield(doc='FK on binding table to target.')
    extra: tuple[str, ...] | None = datafield(
        default=None,
        doc='Extra binding columns exposed in API; None = auto-detect.',
    )
    expose: bool = datafield(
        default=False,
        doc='If True, binding table itself is exposed in API. Not yet implemented.',
    )


@dataclasses.dataclass(frozen=True, slots=True)
class ResourceSpec:
    """Declarative specification of a REST resource derived from database metadata.

    A ResourceSpec is a long-lived, module-level object describing how a
    database table (or view) is exposed through the API.

    It defines:

    - the underlying database structure (`table`), - the API-visible unique key
    (`key`), - optional nested resources (`relations`), - how a nested resource
    is linked to its parent (`via`), - high-level write policy (`upsert`), - a
    base filter condition (`condition`), - a default sort order (`sorting`), -
    and columns to hide from the API (`exclude`).

    **Top-level vs nested resources**

    A ResourceSpec may represent:

    * a top-level API endpoint (`via=None`), exposed under `/<name>`

    * a nested resource, linked to its parent using either `ForeignKey` or
    `BindingTable` via `via=...`

    Nested resources are not separate API endpoints unless explicitly exposed.
    They are materialized inside their parent’s JSON representation.

    **Unified relation model**

    All relations follow a single conceptual model:

    Nested values represent the desired final state of the relation.

    This is a state-based API, not an operation-based RPC interface.

    On write operations (POST / PUT / PATCH):

    - The nested JSON structure expresses the target state. - Target entities
    are identified by their API-level key, not by internal database primary
    keys. - Upsert behavior (create-missing / update-existing) is controlled by
    policy (`upsert`).

    **Relation-specific realization**

    ForeignKey (1:1 or N:1):

    - JSON shape: single object or null. - Writing assigns or clears the FK
    column. - Target record may be matched, updated, created, or rejected
    according to policy.

    BindingTable (1:N or N:N):

    - JSON shape: list of objects. - The list represents the complete desired
    set of related entities. - The implementation synchronizes the binding
    table:

    In both cases the client does not manipulate internal FK values or binding-
    table rows directly unless explicitly exposed.

    **Key semantics**

    The API-level key (`key`) is distinct from the internal primary key:

    - It defines how entities are identified in URLs and nested structures. - It
    may correspond to a unique constraint rather than the DB PK. - It allows
    avoiding exposure of internal surrogate identifiers.

    For composite keys the values are encoded into a single URL path segment
    using `key_separator` (default `"-"`), e.g. `/accounts/1234567890-0100` for
    `key=('account_id', 'bank_code')`.

    """
    name: str = datafield(doc='Public API name of the resource.')
    table: SQLTabular = datafield(doc='Underlying SQLTable/SQLView class.')

    key: tuple[str, ...] | None = datafield(
        default=None,
        doc='API-visible unique key columns.',
    )

    relations: tuple['ResourceSpec', ...] = datafield(
        default=(),
        doc='Nested resources.',
    )

    via: ForeignKey | BindingTable | None = datafield(
        default=None,
        doc='Relation descriptor linking this resource to its parent.',
    )

    upsert: bool = datafield(
        default=True,
        doc='Whether nested entities are upserted by key.',
    )

    condition: Operator | None = datafield(
        default=None,
        doc='Base filter condition applied to all get and list operations.',
    )

    sorting: Sorting | None = datafield(
        default=None,
        doc='Default sort order for list operations. Each entry is (column, ASCENDENT|DESCENDANT). '
            'Defaults to ascending by key if not set.',
    )

    exclude: tuple[str, ...] = datafield(
        default=(),
        doc='Column names to exclude from the API.',
    )

    tag: str | None = datafield(
        default=None,
        doc='Swagger/OpenAPI tag for grouping endpoints. Defaults to the resource name.',
    )

    key_separator: str = datafield(
        default='-',
        doc=(
            'Separator used to encode composite key values in URL path segments. '
            'For a composite key (account_id, bank_code) with separator "-" the '
            'path segment looks like "1234567890-0100". '
            'Ignored for single-column keys.'
        ),
    )


def annotate(fn: typing.Callable[..., typing.Any], **params: typing.Any) -> None:
    """Set endpoint annotations and signature for FastAPI/OpenAPI introspection.

    Pass desired public parameters as keyword arguments in their intended order.

    Example: `annotate(fn, iban=str, payload=PatchModel)`.

    This sets `fn.__annotations__` for those names and `fn.__signature__` with
    POSITIONAL_OR_KEYWORD parameters in that order.

    """
    fn.__annotations__ = dict(getattr(fn, '__annotations__', {}) or {})
    fn.__annotations__.update(params)
    fn.__signature__ = inspect.Signature(parameters=[
        inspect.Parameter(
            name,
            inspect.Parameter.POSITIONAL_OR_KEYWORD,
            annotation=annotation,
        )
        for name, annotation in params.items()
    ])


_VALID_OPERATIONS = frozenset({'get', 'list', 'create', 'update', 'delete'})

# Type for a single operation's authorisation spec:
#   True               — enabled, no auth required
#   False              — explicitly disabled (same effect as omitting the key)
#   callable           — enabled; callable auto-wrapped in fastapi.Depends
#   list of callables  — enabled; each callable auto-wrapped in fastapi.Depends
OperationAuth = bool | typing.Callable | list[typing.Callable]


def add_api_routes(router: fastapi.APIRouter, db: Database, spec: ResourceSpec,
                   operations: dict[str, OperationAuth]) -> None:
    """Register CRUD-ish REST endpoints for a `ResourceSpec`.

    Arguments:
      router: FastAPI router to register routes on. spec: Resource specification.
      db: Database instance. operations: Mapping of operation name to
        authorisation spec.  A missing key or `False` disables that operation.
        `True` enables it without restrictions.  A callable is auto-wrapped in
        `fastapi.Depends` and run as an authorisation check before the handler.  A
        list of callables runs all checks. Valid keys: 'get', 'list', 'create',
        'update', 'delete'.

    Raises:
      `ValueError`: If `operations` contains unknown keys.

    **Route closures and `**kwargs`**

    FastAPI discovers path/query parameters from a function's *real* call
    signature (via `inspect.signature`), not from `__annotations__` alone. The
    path-key name and type differ per resource (e.g. `account_number: int` for
    accounts, `code: str` for code-tables), so a static parameter list is not
    possible here.

    `annotate` patches both `__annotations__` and `__signature__` with the
    correct name/type immediately after the `def`, giving FastAPI and
    OpenAPI/Swagger everything they need.  `**kwargs` in the closure source is
    the mechanism that lets the function accept that dynamically-named argument
    at runtime without knowing its name at definition time.

    """
    unknown = set(operations) - _VALID_OPERATIONS
    if unknown:
        raise ValueError(
            f"Unknown operation(s) for resource {spec.name!r}: {sorted(unknown)}. "
            f"Valid keys: {sorted(_VALID_OPERATIONS)}."
        )

    def _deps(operation: str) -> list | None:
        """Return a Depends list for operation, or None if the operation is disabled."""
        auth = operations.get(operation, False)
        if auth is False:
            return None
        elif auth is True:
            return []
        elif callable(auth):
            return [fastapi.Depends(auth)]
        else:
            return [fastapi.Depends(f) for f in auth]

    handler = TopLevelResourceHandler(spec, db)
    prefix = '/' + spec.name
    tags = [spec.tag or spec.name]
    out_model = handler.model('out')
    key = handler.key

    # GET one
    if (d := _deps('get')) is not None:
        def get_one(**kwargs):  # **kwargs receives the dynamic path key; see docstring above.
            return handler.get_one(kwargs[key.name])

        annotate(get_one, **{key.name: key.annotation})
        router.add_api_route(
            prefix + '/{' + key.name + '}',
            get_one,
            methods=['GET'],
            response_model=out_model,
            tags=tags,
            summary='Get one',
            description='Returns a single record identified by its key.',
            dependencies=d,
        )

    # GET list
    if (d := _deps('list')) is not None:
        def list_many(
            limit: int = fastapi.Query(
                100, ge=1, le=1000, description='Maximum number of records to return.',
            ),
            offset: int = fastapi.Query(
                0, ge=0, description='Number of records to skip before returning results.',
            ),
        ):
            return handler.list_many(limit=limit, offset=offset)

        router.add_api_route(
            prefix,
            list_many,
            methods=['GET'],
            response_model=list[out_model],
            tags=tags,
            summary='List',
            description='Returns records ordered by the key with limit/offset pagination.',
            dependencies=d,
        )

    # POST
    if (d := _deps('create')) is not None:
        def create_one(payload):
            return handler.create_one(payload.model_dump())

        annotate(create_one, payload=handler.model('create'))
        router.add_api_route(
            prefix,
            create_one,
            methods=['POST'],
            status_code=201,
            response_model=out_model,
            tags=tags,
            summary='Create',
            description='Inserts a new record.',
            dependencies=d,
        )

    # PATCH
    if (d := _deps('update')) is not None:
        def update_one(payload, **kwargs):  # **kwargs = dynamic path key; see docstring.
            return handler.update_one(kwargs[key.name], payload.model_dump(exclude_unset=True))

        annotate(update_one, **{key.name: key.annotation, 'payload': handler.model('patch')})
        router.add_api_route(
            prefix + '/{' + key.name + '}',
            update_one,
            methods=['PATCH'],
            response_model=out_model,
            tags=tags,
            summary='Patch',
            description='Partially updates a record. Only provided fields are updated.',
            dependencies=d,
        )

    # DELETE
    if (d := _deps('delete')) is not None:
        def delete_one(**kwargs):  # **kwargs = dynamic path key; see docstring.
            handler.delete_one(kwargs[key.name])
            return fastapi.Response(status_code=204)

        annotate(delete_one, **{key.name: key.annotation})
        router.add_api_route(
            prefix + '/{' + key.name + '}',
            delete_one,
            methods=['DELETE'],
            status_code=204,
            response_class=fastapi.Response,
            tags=tags,
            summary='Delete',
            description='Deletes a record by its key.',
            dependencies=d,
        )


class ResourceHandler:
    """Common database-access base for both top-level and nested handlers.

    Provides everything needed to read and write a single resource table:

    **Responsibilities**

    - Access the database via `PytisAccessor`. - Provide Pydantic models derived
    from SQLAlchemy column metadata. - Implement internal helpers (_materialize,
    _insert, _update) invoked by relation handlers and
    `TopLevelResourceHandler`.

    **Non-responsibilities**

    - HTTP routing (handled by `add_api_routes` / FastAPI). - Direct
    manipulation of FastAPI objects. - Session / transaction lifecycle (owned by
    `TopLevelResourceHandler`). - Schema caching across unrelated resources.

    **Separation of concerns**

    spec                     → declarative (`ResourceSpec`) ResourceHandler →
    database access + model generation (shared base) NestedResourceHandler →
    target-resolution + relation interface ForeignRelationHandler   → FK
    relation read/write BindingRelationHandler   → binding-table relation
    read/write TopLevelResourceHandler  → session lifecycle + CRUD endpoints
    add_api_routes           → transport layer (route registration only)

    """

    def __init__(self, spec: ResourceSpec):
        self._spec = spec
        self._accessor = PytisAccessor.create(spec.table)
        self._models: dict[str, type[pydantic.BaseModel]] = {}
        key = spec.key or (self._accessor.primary_key.name,)
        for col in key:
            if col not in self._accessor.table.c:
                raise ValueError(f'Unknown key column: {col!r}')
        self._key_cols = tuple(key)
        relations: list['NestedResourceHandler'] = []
        for rel in self._spec.relations:
            if rel.via is None:
                raise ValueError(f"Relation {rel.name!r} must define via")
            if isinstance(rel.via, ForeignKey):
                relations.append(self._init_fk_relation(rel))
            elif isinstance(rel.via, BindingTable):
                relations.append(self._init_binding_relation(rel))
            else:
                raise ValueError(f"Unknown relation via: {rel.via!r}")
        self._relations = relations
        self._relations_by_name = {rel.name: rel for rel in self._relations}
        # Columns excluded from all generated Pydantic models:
        #   - FK columns owned by a ForeignRelationHandler (clients use the
        #     nested object; the handler resolves and assigns the FK internally)
        #   - columns listed explicitly in spec.exclude
        # Computed here (after _relations is final) and consulted by _model().
        # Note: ForeignRelationHandler is defined later in this module, but the
        # isinstance() call is a runtime check so the forward reference is fine.
        self._exclude: frozenset[str] = frozenset(
            rel._fk_column.name
            for rel in self._relations
            if isinstance(rel, ForeignRelationHandler)
        ) | frozenset(spec.exclude)
        col_names = {c.name for c in self._accessor.columns}
        for name in spec.exclude:
            if name not in col_names:
                raise ValueError(f"Unknown column in ResourceSpec.exclude: {name!r}")

    def _check_exclude_for_inserts(self) -> None:
        """Raise `ValueError` if any explicitly excluded column would prevent INSERT."""
        # FK columns handled by a ForeignRelationHandler and the primary key are
        # exempt — the handler fills in FK values automatically, and the PK is
        # managed separately.
        fk_col_names = frozenset(
            rel._fk_column.name
            for rel in self._relations
            if isinstance(rel, ForeignRelationHandler)
        )
        pk_name = self._accessor.primary_key.name
        for name in self._spec.exclude:
            if name in fk_col_names or name == pk_name:
                continue
            col = self._accessor.table.c[name]
            if not col.nullable and col.server_default is None and col.default is None:
                raise ValueError(
                    f"Column {name!r} in ResourceSpec.exclude is NOT NULL with no "
                    f"default; INSERT operations will fail."
                )

    def _column_type(self, col: sa.Column) -> type | None:
        """Return a best-effort Python type for a SQLAlchemy Column."""
        try:
            return col.type.python_type
        except (NotImplementedError, AttributeError, TypeError):
            return None

    def _resolve_fk_reference(self, column: sa.Column) -> sa.Column:
        fks = list(column.foreign_keys)
        if not fks:
            raise ValueError(f"Column {column.name!r} has no foreign key metadata")
        if len(fks) != 1:
            raise ValueError(f"Column {column.name!r} has multiple foreign keys; ambiguous")
        return fks[0].column

    def _init_fk_relation(self, rel: ResourceSpec) -> 'ForeignRelationHandler':
        """Validate and construct a `ForeignRelationHandler` for a `ForeignKey` spec."""
        if rel.via.fk not in self._accessor.table.c:
            raise ValueError(f"Unknown FK column {rel.via.fk!r} on {self._spec.name!r}")
        fk_col = self._accessor.table.c[rel.via.fk]
        ref_col = self._resolve_fk_reference(fk_col)
        relation = ForeignRelationHandler(rel, fk_col, ref_col)
        if ref_col.table.name != relation._accessor.table.name:
            raise ValueError(
                f"Foreign key {rel.via.fk!r} does not reference {rel.table.__name__}"
            )
        return relation

    def _init_binding_relation(self, rel: ResourceSpec) -> 'BindingRelationHandler':
        """Validate and construct a `BindingRelationHandler` for a `BindingTable` spec."""
        if rel.via.expose:
            raise ValueError("BindingTable expose=True is not supported yet")
        binding_accessor = PytisAccessor.create(rel.via.table)
        table = binding_accessor.table
        if rel.via.parent_fk not in table.c:
            raise ValueError(f"Unknown binding parent FK {rel.via.parent_fk!r}")
        if rel.via.target_fk not in table.c:
            raise ValueError(f"Unknown binding target FK {rel.via.target_fk!r}")
        parent_fk_col = table.c[rel.via.parent_fk]
        target_fk_col = table.c[rel.via.target_fk]
        parent_ref = self._resolve_fk_reference(parent_fk_col)
        target_ref = self._resolve_fk_reference(target_fk_col)
        if parent_ref.table.name != self._accessor.table.name:
            raise ValueError(
                f"Binding parent FK {rel.via.parent_fk!r} does not reference {self._spec.name!r}"
            )
        if rel.via.extra is not None:
            binding_cols = {c.name for c in binding_accessor.columns}
            for col in rel.via.extra:
                if col not in binding_cols:
                    raise ValueError(f"Unknown binding extra column {col!r}")
            extra_names = tuple(rel.via.extra)
        else:
            extra_names = tuple(
                c.name for c in binding_accessor.columns
                if c.name not in {
                    binding_accessor.primary_key.name,
                    rel.via.parent_fk,
                    rel.via.target_fk,
                }
            )
        relation = BindingRelationHandler(
            rel, binding_accessor,
            parent_fk_col, parent_ref, target_fk_col, target_ref,
            extra_names,
        )
        if target_ref.table.name != relation._accessor.table.name:
            raise ValueError(
                f"Binding target FK {rel.via.target_fk!r} does not reference {rel.table.__name__}"
            )
        return relation

    def _relation_fields(self, kind: str) -> dict[str, tuple[typing.Any, typing.Any]]:
        fields: dict[str, tuple[typing.Any, typing.Any]] = {}
        for rel in self._relations:
            t = rel.model(kind)
            default = ... if kind == 'out' else None
            if isinstance(rel, ForeignRelationHandler):
                fields[rel.name] = (t | None, default)
            else:
                fields[rel.name] = (list[t] if kind == 'out' else list[t] | None, default)
        return fields

    def _model(self, kind: str) -> type[pydantic.BaseModel]:
        if kind not in ('out', 'create', 'patch', 'nested'):
            raise ValueError(f'Unknown model kind: {kind!r}')
        # For model kinds that correspond to INSERT operations, verify that no
        # explicitly excluded column would silently break INSERT.  'create' is
        # used for top-level POST; 'nested' is used for nested write payloads
        # where upsert=True means the target may be inserted.
        if ((self._spec.exclude and issubclass(self._spec.table, SQLTable)
             and (kind == 'create' or (kind == 'nested' and self._spec.upsert)))):
            self._check_exclude_for_inserts()
        accessor = self._accessor
        pk = accessor.primary_key
        # The DB primary key is excluded whenever it is not part of the API
        # key (surrogate/internal identifier).  When it IS the API key it is
        # still excluded from 'create' if the DB generates it automatically.
        pk_internal = pk.name not in self._key_cols
        pk_db_generated = (
            getattr(pk, 'identity', None) is not None
            or pk.autoincrement is True
            or pk.server_default is not None
        )

        def field(col: sa.Column) -> tuple[typing.Any, typing.Any]:
            t = self._column_type(col) or typing.Any
            if col.nullable:
                t = t | None
            if kind == 'out':
                # All fields present for read operations.
                required = True
            elif kind == 'create':
                # Required if NOT NULL and no default (server-side or client-side).
                has_default = (col.server_default is not None) or (col.default is not None)
                required = (not col.nullable) and (not has_default)
            else:  # kind in ('patch', 'nested'):
                # All fields optional on update (only provided fields are applied).
                required = False
            return (t, pydantic.Field(... if required else None, description=col.doc))

        # The DB primary key is excluded whenever it is not part of the API key.
        # In that case it is a surrogate/internal identifier that clients should
        # never see or supply.  When the PK *is* the API key (key= omitted or
        # explicitly set to the PK column), it is kept — but only in models
        # where the client is expected to know it (out, create when not
        # DB-generated).
        # Columns pre-excluded by __init__: FK columns owned by nested
        # relation handlers and any columns listed in spec.exclude.
        exclude = set(self._exclude)
        # Exclude PK when internal (not the API key), always on 'patch' (the
        # key is immutable), or on 'create' when the DB generates it.
        if pk_internal or kind == 'patch' or (kind == 'create' and pk_db_generated):
            exclude.add(pk.name)

        config = (pydantic.ConfigDict(from_attributes=True) if kind == 'out'
                  else pydantic.ConfigDict(extra='forbid'))

        fields = {c.name: field(c) for c in accessor.columns if c.name not in exclude}
        fields.update(self._relation_fields(kind))
        return pydantic.create_model(
            f'{self._spec.table.__name__}{kind.capitalize()}',
            __config__=config,
            **fields,
        )

    def model(self, kind: str) -> type[pydantic.BaseModel]:
        """Return a Pydantic model for this resource.

        Arguments:
          kind: 'out' | 'create' | 'patch' | 'nested'

        Returns:
          Pydantic model class.

        Pydantic models are generated from SQLAlchemy column metadata:

        - 'out'    model: full record representation (from_attributes=True). -
        'create' model: excludes DB-generated PKs. - 'patch'  model: all fields
        optional; PK never present. - 'nested' model: for nested writes;
        includes API key columns and excludes DB PK if it is not part of the API
        key.

        Models are created per handler instance and not globally cached. In the
        current architecture each ResourceSpec is typically registered once
        during application startup, so model caching provides no practical
        benefit and would only add complexity.

        """
        try:
            model = self._models[kind]
        except KeyError:
            model = self._models[kind] = self._model(kind)
        return model

    def _materialize(self, session: orm.Session, row: typing.Any) -> dict[str, typing.Any]:
        """Serialize row with nested relations in spec order."""
        data = {c.name: getattr(row, c.name) for c in self._accessor.columns}
        for rel in self._relations:
            data[rel.name] = rel.materialize(session, row)
        return data

    def _split_payload(self, payload: dict[str, typing.Any]) -> tuple[dict[str, typing.Any],
                                                                     dict[str, typing.Any]]:
        base: dict[str, typing.Any] = {}
        relations: dict[str, typing.Any] = {}
        for key, value in payload.items():
            if key in self._relations_by_name:
                relations[key] = value
            else:
                base[key] = value
        return base, relations

    def _insert(self, session: orm.Session, payload: dict[str, typing.Any]):
        base, relations = self._split_payload(payload)
        for rel in self._relations:
            if rel.name in relations:
                base.update(rel.collect_updates(session, relations[rel.name]))
        row = self._accessor.insert(session, **base)
        for rel in self._relations:
            if rel.name in relations:
                rel.apply_nested(session, row, relations[rel.name])
        return row

    def _update(self, session: orm.Session, condition: Operator,
                payload: dict[str, typing.Any], *, allow_key_update: bool = True):
        base, relations = self._split_payload(payload)
        if not allow_key_update:
            for k in self._key_cols:
                base.pop(k, None)
        for rel in self._relations:
            if rel.name in relations:
                base.update(rel.collect_updates(session, relations[rel.name]))
        if base:
            row = self._accessor.update(session, condition, **base)
        else:
            row = self._accessor.row(session, condition)
        if row is None:
            return None
        for rel in self._relations:
            if rel.name in relations:
                rel.apply_nested(session, row, relations[rel.name])
        return row


class NestedResourceHandler(ResourceHandler):
    """Base for `ForeignRelationHandler` and `BindingRelationHandler`.

    Extends `ResourceHandler` with the interface and target-resolution logic
    shared by both relation handler types:

    - name property derived from spec - materialize / collect_updates /
    apply_nested protocol - _key_from_payload / _resolve_target target-
    resolution helpers

    Subclasses provide concrete materialize implementations and override
    collect_updates (`ForeignRelationHandler`) or apply_nested
    (`BindingRelationHandler`) as appropriate.

    """

    @property
    def name(self) -> str:
        """Public API name of this nested relation."""
        return self._spec.name

    def materialize(self, session: orm.Session, parent_row: typing.Any):
        """Return nested JSON for this relation based on a parent row."""
        raise NotImplementedError

    def collect_updates(self, session: orm.Session,
                        payload: typing.Any) -> dict[str, typing.Any]:
        """Return column updates to apply to the parent row before insert/update."""
        return {}

    def apply_nested(self, session: orm.Session, parent_row: typing.Any,
                     payload: typing.Any) -> None:
        """Apply nested changes that require the parent row to already exist."""
        return None

    def _key_from_payload(self, payload: dict[str, typing.Any]) -> Operator:
        """Extract and validate API-level key columns from payload.

        Returns an `EQ` operator for a single-column key, or an `AND` of `EQ`
        operators for a composite key.

        Raises:
          `PayloadError`: If any key column is absent or its value has the wrong
            Python type.

        """
        conditions: list[Operator] = []
        for name in self._key_cols:
            if name not in payload:
                raise PayloadError(f"Missing key column: {name!r}")
            value = payload[name]
            col = self._accessor.table.c[name]  # always present; validated in __init__
            try:
                expected_type = col.type.python_type
            except (NotImplementedError, AttributeError, TypeError):
                expected_type = None
            if ((expected_type is not None and value is not None
                 and not isinstance(value, expected_type))):
                raise PayloadError(
                    f"Invalid type for key {name!r}: expected {expected_type.__name__}, "
                    f"got {type(value).__name__}"
                )
            conditions.append(EQ(name, value))
        return conditions[0] if len(conditions) == 1 else AND(*conditions)

    def _resolve_target(self, session: orm.Session, payload: dict[str, typing.Any]):
        """Fetch or create/update the target row described by payload.

        Extracts the key from payload via _key_from_payload, then:

        - If the row exists and payload contains no non-key fields, returns it
          as-is.
        - If the row exists and payload contains non-key fields, updates it
          (only when spec.upsert is True; raises PayloadError otherwise).
        - If the row does not exist, inserts it (only when spec.upsert is
          True; raises PayloadError otherwise).

        Raises:
          `PayloadError`: Key columns missing or of wrong type, target not found
            when upsert is False, updates attempted when upsert is False, or the row
            disappears between the initial lookup and the subsequent update
            (TOCTOU).

        """
        condition = self._key_from_payload(payload)
        try:
            target = self._accessor.row(session, condition)
        except NonUniqueKeyError as e:
            raise PayloadError(str(e)) from e
        if target is None:
            if not self._spec.upsert:
                raise PayloadError(f"Target not found for {self.name!r}")
            target = self._insert(session, payload)
        else:
            update_payload = {k: v for k, v in payload.items() if k not in self._key_cols}
            if update_payload:
                if self._spec.upsert:
                    target = self._update(session, condition, update_payload,
                                          allow_key_update=False)
                    if target is None:
                        # Row disappeared between the row() fetch and _update() — treat
                        # it the same way as "not found on initial lookup".
                        raise PayloadError(f"Target not found for {self.name!r}")
                else:
                    raise PayloadError(f"Updates are not allowed for {self.name!r}")
        return target


class ForeignRelationHandler(NestedResourceHandler):
    """Nested handler for `ForeignKey` relations (1:1 or N:1).

    IS the resource handler for the target table, extended with the FK column
    metadata needed to read and write the relation.

    """

    def __init__(self, spec: ResourceSpec, fk_column: sa.Column, ref_column: sa.Column):
        super().__init__(spec)
        self._fk_column = fk_column
        self._ref_column = ref_column

    def model(self, kind: str) -> type[pydantic.BaseModel]:
        """Return the Pydantic model for a FK-relation item.

        FK-relation items have only two distinct shapes: 'out' for read
        responses and 'nested' for all write payloads.  Parent model kinds
        'create' and 'patch' are normalised to 'nested'.

        """
        return super().model('out' if kind == 'out' else 'nested')

    def materialize(self, session: orm.Session, parent_row: typing.Any):
        """Return nested object (or None) for a `ForeignKey` relation."""
        fk_value = getattr(parent_row, self._fk_column.name)
        if fk_value is None:
            return None
        target = self._accessor.row(
            session,
            EQ(self._ref_column.name, fk_value),
        )
        if target is None:
            raise DataConsistencyError(
                f"Related record not found for {self.name!r}",
            )
        return self._materialize(session, target)

    def collect_updates(self, session: orm.Session,
                        payload: typing.Any) -> dict[str, typing.Any]:
        """Resolve target and return FK assignment for the parent row."""
        if payload is None:
            if not self._fk_column.nullable:
                raise PayloadError(f"Field {self.name!r} cannot be null")
            return {self._fk_column.name: None}
        if not isinstance(payload, dict):
            raise PayloadError(f"Expected object for {self.name!r}")
        target = self._resolve_target(session, payload)
        ref_value = getattr(target, self._ref_column.name)
        return {self._fk_column.name: ref_value}


class BindingRelationHandler(NestedResourceHandler):
    """Nested handler for `BindingTable` relations (1:N or N:N).

    IS the resource handler for the target table, extended with the binding
    table metadata needed to read and synchronize the relation.

    """

    def __init__(self,
                 spec: ResourceSpec,
                 binding_accessor: PytisAccessor,
                 parent_fk_column: sa.Column,
                 parent_ref_column: sa.Column,
                 target_fk_column: sa.Column,
                 target_ref_column: sa.Column,
                 extra_names: tuple[str, ...]):
        super().__init__(spec)
        self._binding_accessor = binding_accessor
        self._parent_fk_column = parent_fk_column
        self._parent_ref_column = parent_ref_column
        self._target_fk_column = target_fk_column
        self._target_ref_column = target_ref_column
        self._extra_names = extra_names

    def model(self, kind: str) -> type[pydantic.BaseModel]:
        """Return the Pydantic model for a binding-relation item.

        Binding items have only two distinct shapes: 'out' for read responses
        and 'nested' for all write payloads.  The parent model kinds 'create'
        and 'patch' are both normalised to 'nested' so the write model is shared
        and built only once.

        """
        return super().model('out' if kind == 'out' else 'nested')

    def _model(self, kind: str) -> type[pydantic.BaseModel]:
        """Build target-resource model, extended with extra binding-table columns."""
        base = super()._model(kind)
        if not self._extra_names:
            return base
        binding_required = (kind == 'out')
        binding_table = self._binding_accessor.table
        binding_fields: dict[str, tuple[typing.Any, typing.Any]] = {}
        for name in self._extra_names:
            col = binding_table.c[name]
            t = self._column_type(col) or typing.Any
            if col.nullable:
                t = t | None
            binding_fields[name] = (
                t, pydantic.Field(... if binding_required else None, description=col.doc),
            )
        return pydantic.create_model(
            f'{base.__name__}{self.name.capitalize()}{kind.capitalize()}Binding',
            __base__=base,
            **binding_fields,
        )

    def materialize(self, session: orm.Session, parent_row: typing.Any):
        """Return list of nested objects for a `BindingTable` relation."""
        parent_value = getattr(parent_row, self._parent_ref_column.name)
        bindings = self._binding_accessor.rows(
            session,
            condition=EQ(self._parent_fk_column.name, parent_value),
        )
        if not bindings:
            return []
        # Batch-load all target rows in one IN query instead of one per binding.
        target_values = [getattr(b, self._target_fk_column.name) for b in bindings]
        targets_by_ref = {
            getattr(t, self._target_ref_column.name): t
            for t in self._accessor.rows(
                session,
                condition=IN(self._target_ref_column.name, tuple(target_values)),
            )
        }
        items: list[dict[str, typing.Any]] = []
        for binding in bindings:
            target_value = getattr(binding, self._target_fk_column.name)
            target = targets_by_ref.get(target_value)
            if target is None:
                raise DataConsistencyError(
                    f"Related record not found for {self.name!r}",
                )
            item = self._materialize(session, target)
            if self._extra_names:
                item.update({name: getattr(binding, name) for name in self._extra_names})
            items.append(item)
        return items

    def _split_item(self, payload: typing.Any) -> tuple[dict[str, typing.Any],
                                                        dict[str, typing.Any]]:
        if not isinstance(payload, dict):
            raise PayloadError(f"Expected object in {self.name!r}")
        binding: dict[str, typing.Any] = {}
        if self._extra_names:
            for name in self._extra_names:
                if name in payload:
                    binding[name] = payload[name]
        target_payload = {k: v for k, v in payload.items() if k not in binding}
        return target_payload, binding

    def apply_nested(self, session: orm.Session, parent_row: typing.Any,
                     payload: typing.Any) -> None:
        """Synchronize binding rows to match the desired list of targets."""
        if payload is None:
            raise PayloadError(f"Expected list for {self.name!r}, got None")
        if not isinstance(payload, list):
            raise PayloadError(
                f"Expected list for {self.name!r}, got {type(payload).__name__}"
            )
        parent_value = getattr(parent_row, self._parent_ref_column.name)
        existing_rows = self._binding_accessor.rows(
            session,
            condition=EQ(self._parent_fk_column.name, parent_value),
        )
        existing: dict[tuple[typing.Any, ...], typing.Any] = {}
        if existing_rows:
            # Batch-load all target rows in one IN query instead of one per binding.
            target_values = [getattr(b, self._target_fk_column.name) for b in existing_rows]
            targets_by_ref = {
                getattr(t, self._target_ref_column.name): t
                for t in self._accessor.rows(
                    session,
                    condition=IN(self._target_ref_column.name, tuple(target_values)),
                )
            }
            for binding in existing_rows:
                target_value = getattr(binding, self._target_fk_column.name)
                target = targets_by_ref.get(target_value)
                if target is None:
                    raise DataConsistencyError(f"Related record not found for {self.name!r}",)
                key_tuple = tuple(getattr(target, k) for k in self._key_cols)
                existing[key_tuple] = binding

        desired: dict[tuple[typing.Any, ...], tuple[typing.Any, dict[str, typing.Any]]] = {}
        for item in payload:
            target_payload, binding_payload = self._split_item(item)
            target = self._resolve_target(session, target_payload)
            key_tuple = tuple(getattr(target, k) for k in self._key_cols)
            if key_tuple in desired:
                raise PayloadError(f"Duplicate target in {self.name!r} payload")
            desired[key_tuple] = (target, binding_payload)

        for key_tuple, (target, binding_payload) in desired.items():
            if key_tuple in existing:
                binding_row = existing[key_tuple]
                for name, value in binding_payload.items():
                    setattr(binding_row, name, value)
            else:
                values = {
                    self._parent_fk_column.name: parent_value,
                    self._target_fk_column.name: getattr(target, self._target_ref_column.name),
                }
                values.update(binding_payload)
                self._binding_accessor.insert(session, **values)

        for key_tuple, binding_row in existing.items():
            if key_tuple not in desired:
                self._binding_accessor.delete_instance(session, binding_row)

        try:
            session.flush()
        except sa.exc.IntegrityError as e:
            raise ConstraintViolationError(e) from e


class TopLevelResourceHandler(ResourceHandler):
    """Resource handler for top-level API endpoints.

    Extends `ResourceHandler` with the concerns exclusive to top-level
    resources:

    - Enforcement of a single-column key (required for URL path parameters). - A
    `ResourceKey` encapsulating the path parameter name and Python type. - The
    five public CRUD methods that own the session/transaction lifecycle.

    Created exclusively by `add_api_routes`; never used as a nested target.

    **CRUD semantics**

    GET:

    POST:

    PATCH:

    DELETE:

    **Key handling**

    - Single-column keys: path parameter name matches the column name, type
    matches the column's Python type. - Composite keys: all column values are
    joined by `key_separator` (default `"-"`) into a single path segment of type
    `str`. The path parameter is named `key`, e.g. `/accounts/{key}` for key
    `(account_id, bank_code)`. - Internal database primary keys may differ from
    API-level keys.

    """

    @dataclasses.dataclass(frozen=True, slots=True)
    class ResourceKey:
        """API-level identifier of a resource.

        Encapsulates the URL path parameter name, its Python type, the
        underlying key column names, and (for composite keys) the separator used
        to encode multiple values into a single path segment.

        For a single-column key the path parameter carries the column value
        directly with its natural Python type.  For a composite key the path
        parameter is always `str` and encodes all column values joined by
        `separator`, e.g. `"1234567890-0100"` for `(account_id, bank_code)` with
        separator `"-"`.

        """
        name: str
        type: type
        columns: tuple[str, ...]
        separator: str

        @property
        def annotation(self) -> type:
            """Type annotation for the FastAPI path parameter.

            For single-column keys this is just `self.type`.  For composite
            keys it wraps `str` in `typing.Annotated` with a `fastapi.Path`
            description that explains the separator-encoded format so the
            information appears in the generated OpenAPI documentation.

            """
            if len(self.columns) == 1:
                return self.type
            else:
                parts = self.separator.join(f'{{{c}}}' for c in self.columns)
                desc = (
                    f'Composite key encoded as the {len(self.columns)} column values '
                    f'joined by {self.separator!r}: {parts}.'
                )
                return typing.Annotated[str, fastapi.Path(description=desc)]

        def condition(self, raw_value: typing.Any) -> Operator:
            """Build a filter condition from a URL path segment value.

            For a single-column key returns `EQ` `(col, raw_value)`. For a
            composite key splits `raw_value` on the separator and returns `AND`
            of `EQ` instances for each part.

            Raises:
              `ValueError`: If the composite key value does not contain the expected
                number of parts.

            """
            if len(self.columns) == 1:
                return EQ(self.columns[0], raw_value)
            parts = str(raw_value).split(self.separator, len(self.columns) - 1)
            if len(parts) != len(self.columns):
                raise ValueError(
                    f"Expected {len(self.columns)} parts separated by "
                    f"{self.separator!r}, got {str(raw_value)!r}"
                )
            return AND(*(EQ(col, part) for col, part in zip(self.columns, parts)))

    def __init__(self, spec: ResourceSpec, db: Database):
        super().__init__(spec)
        self._db = db
        if len(self._key_cols) == 1:
            t = self._column_type(self._accessor.table.c[self._key_cols[0]]) or str
        else:
            t = str
        self._key = self.ResourceKey(
            name=self._key_cols[0] if len(self._key_cols) == 1 else 'key',
            type=t,
            columns=self._key_cols,
            separator=spec.key_separator,
        )

    @property
    def key(self) -> ResourceKey:
        """API-level resource identifier."""
        return self._key

    def get_one(self, item_id: typing.Any):
        """Return one record identified by its key (404 if not found)."""
        try:
            with self._db.session() as session:
                try:
                    condition = self._key.condition(item_id)
                except ValueError as e:
                    raise fastapi.HTTPException(status_code=422, detail=str(e))
                if self._spec.condition is not None:
                    condition = AND(self._spec.condition, condition)
                row = self._accessor.row(session, condition)
                if row is None:
                    raise fastapi.HTTPException(status_code=404, detail="Not found")
                return self._materialize(session, row)
        except PayloadError as e:
            raise fastapi.HTTPException(status_code=422, detail=str(e)) from e
        except (NonUniqueKeyError, DataConsistencyError) as e:
            raise fastapi.HTTPException(status_code=500, detail=str(e)) from e

    def list_many(self, *, limit: int, offset: int) -> list[typing.Any]:
        """Return a page of records ordered by the resource key."""
        try:
            with self._db.session() as session:
                rows = self._accessor.rows(
                    session,
                    condition=self._spec.condition,
                    sorting=self._spec.sorting or tuple(
                        (col, ASCENDENT) for col in self._key.columns
                    ),
                    limit=limit,
                    offset=offset,
                )
                return [self._materialize(session, row) for row in rows]
        except (NonUniqueKeyError, DataConsistencyError) as e:
            raise fastapi.HTTPException(status_code=500, detail=str(e)) from e

    def create_one(self, payload: dict[str, typing.Any]):
        """Insert a new record and return it."""
        try:
            with self._db.session.begin() as session:
                row = self._insert(session, payload)
                return self._materialize(session, row)
        except ConstraintViolationError as e:
            raise fastapi.HTTPException(status_code=409, detail=e.detail) from e
        except PayloadError as e:
            raise fastapi.HTTPException(status_code=422, detail=str(e)) from e
        except (NonUniqueKeyError, DataConsistencyError) as e:
            raise fastapi.HTTPException(status_code=500, detail=str(e)) from e

    def update_one(self, item_id: typing.Any, payload: dict[str, typing.Any]):
        """Partially update a record and return it (or None if not found)."""
        try:
            with self._db.session.begin() as session:
                try:
                    condition = self._key.condition(item_id)
                except ValueError as e:
                    raise fastapi.HTTPException(status_code=422, detail=str(e))
                row = self._update(session, condition, payload)
                if row is None:
                    raise fastapi.HTTPException(status_code=404, detail='Not found')
                return self._materialize(session, row)
        except ConstraintViolationError as e:
            raise fastapi.HTTPException(status_code=409, detail=e.detail) from e
        except PayloadError as e:
            raise fastapi.HTTPException(status_code=422, detail=str(e)) from e
        except (NonUniqueKeyError, DataConsistencyError) as e:
            raise fastapi.HTTPException(status_code=500, detail=str(e)) from e

    def delete_one(self, item_id: typing.Any) -> None:
        """Delete a record by key (404 if not found)."""
        try:
            with self._db.session.begin() as session:
                try:
                    condition = self._key.condition(item_id)
                except ValueError as e:
                    raise fastapi.HTTPException(status_code=422, detail=str(e))
                ok = self._accessor.delete(session, condition)
                if not ok:
                    raise fastapi.HTTPException(status_code=404, detail='Not found')
        except ConstraintViolationError as e:
            raise fastapi.HTTPException(status_code=409, detail=e.detail) from e
        except (NonUniqueKeyError, DataConsistencyError) as e:
            raise fastapi.HTTPException(status_code=500, detail=str(e)) from e
