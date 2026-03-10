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

"""Tests for pytis.rest (pytis.rest.db and pytis.rest.rest).

Async-migration readiness
--------------------------

Tests are organised in three layers to minimise the number of changes
needed when the framework switches from sync (psycopg2/SQLAlchemy 1.4)
to async (asyncpg/SQLAlchemy 2.0)::

    Layer 1 — pure Python (zero changes ever):
        TestOperators, TestResourceHandlerModel
        No sessions, no database.  These test operator expression
        generation and Pydantic model derivation from SA column metadata.

    Layer 2 — session fixture (ONE fixture change per file):
        TestPytisAccessor
        Test bodies call accessor methods (row, rows, insert, update,
        delete) through the ``session`` fixture.  On async migration only
        the ``session`` fixture body changes; all test bodies stay
        identical.  The async form is documented in the fixture docstring.

    Layer 3 — HTTP client (zero changes ever):
        TestHttpRoutes
        FastAPI's TestClient handles both sync and async route handlers
        transparently, so no test code changes are needed when route
        closures become ``async def``.

Running integration tests
--------------------------

``TestPytisAccessor`` and ``TestHttpRoutes`` require a PostgreSQL database.
Set ``PYTIS_REST_TEST_DB`` to a connection URL (default:
``postgresql://localhost/test``).  Tests are automatically skipped when
the database is unreachable.

"""

import os
import pytest
import sqlalchemy as sa
import sqlalchemy.orm as orm
from unittest.mock import MagicMock

# Skip entire file when optional REST dependencies are not installed.
pytest.importorskip('fastapi')
pytest.importorskip('pydantic')

import fastapi  # noqa: E402
from fastapi.testclient import TestClient  # noqa: E402
import pytis.data as pd  # noqa: E402
import pytis.data.gensqlalchemy as gsql  # noqa: E402
from pytis.rest.db import (  # noqa: E402
    ASCENDENT, DESCENDANT,
    EQ, NE, LT, LE, GT, GE, IN, LIKE, AND, OR, NOT,
    PytisAccessor, Database,
    PayloadError, NonUniqueKeyError, ConstraintViolationError,
)
from pytis.rest.rest import (  # noqa: E402
    ResourceSpec, ForeignKey,
    TopLevelResourceHandler,
    add_api_routes,
)


# ---------------------------------------------------------------------------
# Plain SA table for Operator tests (no gensqlalchemy, no database needed)
# ---------------------------------------------------------------------------

_op_meta = sa.MetaData()
_op_table = sa.Table(
    '_pytis_rest_op_test', _op_meta,
    sa.Column('id', sa.Integer, primary_key=True),
    sa.Column('name', sa.String(50)),
    sa.Column('score', sa.Integer),
)


# ---------------------------------------------------------------------------
# Gensqlalchemy table specifications used by integration tests
# ---------------------------------------------------------------------------

class PytisRestTestCategory(gsql.SQLTable):
    """Test category table (parent for FK relation tests)."""
    name = 'pytis_rest_test_categories'
    fields = (
        gsql.PrimaryColumn('id', pd.Serial()),
        gsql.Column('label', pd.String(not_null=True)),
    )
    depends_on = ()


class PytisRestTestItem(gsql.SQLTable):
    """Test item table with a nullable FK to categories."""
    name = 'pytis_rest_test_items'
    fields = (
        gsql.PrimaryColumn('id', pd.Serial()),
        gsql.Column('code', pd.String(not_null=True), unique=True),
        gsql.Column('label', pd.String()),
        gsql.Column('score', pd.Integer()),
        gsql.Column('category_id', pd.Integer(),
                   references=gsql.r.PytisRestTestCategory),
    )
    depends_on = (PytisRestTestCategory,)


# ---------------------------------------------------------------------------
# Resource specs used by model-generation and HTTP tests
# ---------------------------------------------------------------------------

# Flat spec: ``code`` is the API key; internal ``id`` (serial PK) is hidden.
_ITEM_SPEC = ResourceSpec(
    name='items',
    table=PytisRestTestItem,
    key=('code',),
)

_CATEGORY_SPEC = ResourceSpec(
    name='categories',
    table=PytisRestTestCategory,
)

_ALL_OPS = dict(get=True, list=True, create=True, update=True, delete=True)

# ---------------------------------------------------------------------------
# Database / session fixtures
# ---------------------------------------------------------------------------

_TEST_DSN = os.environ.get('PYTIS_REST_TEST_DB', 'postgresql://localhost/test')


@pytest.fixture(scope='session')
def engine():
    """Session-scoped engine.  Tests that depend on it are skipped when the
    database is unreachable.  Also creates/drops the test tables.
    """
    try:
        eng = sa.create_engine(_TEST_DSN, future=True, pool_pre_ping=True)
        with eng.connect() as conn:
            conn.execute(sa.text('SELECT 1'))
    except Exception as exc:
        pytest.skip(f'Test database unreachable ({exc})')

    # Lazily materialise gensqlalchemy SA Table objects so they are in
    # _metadata, then create the real tables if they do not exist yet.
    with gsql.local_search_path(PytisRestTestItem.default_search_path()):
        item_table = gsql.object_by_class(PytisRestTestItem)
    with gsql.local_search_path(PytisRestTestCategory.default_search_path()):
        cat_table = gsql.object_by_class(PytisRestTestCategory)
    # Use SA's own DDL to create/drop the tables.  We deliberately bypass
    # gensqlalchemy's SQLTable.create() because it calls conn.execute() with a
    # plain string ('SET SEARCH_PATH TO ...'), which SA 1.4 future=True rejects.
    # Always drop-then-create so any schema changes (e.g. added constraints)
    # are applied even when leftover tables exist from a previous interrupted run.
    with eng.begin() as conn:
        conn.execute(sa.schema.DropTable(item_table, if_exists=True))
        conn.execute(sa.schema.DropTable(cat_table, if_exists=True))
        conn.execute(sa.schema.CreateTable(cat_table))
        conn.execute(sa.schema.CreateTable(item_table))

    yield eng

    with eng.begin() as conn:
        conn.execute(sa.schema.DropTable(item_table, if_exists=True))
        conn.execute(sa.schema.DropTable(cat_table, if_exists=True))
    eng.dispose()


@pytest.fixture
def session(engine):
    """Transactional session that rolls back all changes after each test.

    This is the **only fixture that needs to change** when migrating to
    async.  All test bodies that use this fixture remain unchanged.

    Async form (SQLAlchemy 2.0 + asyncpg)::

        @pytest.fixture
        async def session(async_engine):
            async with async_engine.connect() as conn:
                async with conn.begin():
                    async with AsyncSession(
                        conn, autoflush=False, expire_on_commit=False,
                    ) as sess:
                        yield sess
                    await conn.rollback()

    """
    conn = engine.connect()
    trans = conn.begin()
    sess = orm.Session(bind=conn, autoflush=False, expire_on_commit=False, future=True)
    try:
        yield sess
    finally:
        sess.close()
        if trans.is_active:
            trans.rollback()
        conn.close()


@pytest.fixture(scope='session')
def db(engine):
    """Database instance backed by the test engine."""
    return Database(
        engine=engine,
        session=orm.sessionmaker(
            bind=engine, autoflush=False, autocommit=False,
            expire_on_commit=False, future=True,
        ),
    )


@pytest.fixture(scope='session')
def client(db):
    """FastAPI TestClient with item + category routes.

    TestClient works identically for both sync and async route handlers,
    so nothing here changes on the async migration.
    """
    app = fastapi.FastAPI()
    router = fastapi.APIRouter()
    add_api_routes(router, db, _ITEM_SPEC, operations=_ALL_OPS)
    add_api_routes(router, db, _CATEGORY_SPEC, operations=_ALL_OPS)
    app.include_router(router)
    with TestClient(app) as c:
        yield c


# ---------------------------------------------------------------------------
# Layer 1: Operator expression tests — pure Python, no database
# ---------------------------------------------------------------------------

class TestOperators:
    """Test Operator.expression() output.  No database required."""

    def test_eq_value(self):
        expr = EQ('name', 'Alice').expression(_op_table)
        assert '_pytis_rest_op_test.name = :name_1' in str(expr)

    def test_eq_none_is_null(self):
        expr = EQ('name', None).expression(_op_table)
        assert '_pytis_rest_op_test.name IS NULL' in str(expr)

    def test_ne_value(self):
        expr = NE('name', 'Alice').expression(_op_table)
        assert '!=' in str(expr) or '<>' in str(expr)

    def test_ne_none_is_not_null(self):
        expr = NE('name', None).expression(_op_table)
        assert 'IS NOT NULL' in str(expr)

    def test_lt(self):
        expr = LT('score', 10).expression(_op_table)
        assert '_pytis_rest_op_test.score < :score_1' in str(expr)

    def test_le(self):
        expr = LE('score', 10).expression(_op_table)
        assert '_pytis_rest_op_test.score <= :score_1' in str(expr)

    def test_gt(self):
        expr = GT('score', 10).expression(_op_table)
        assert '_pytis_rest_op_test.score > :score_1' in str(expr)

    def test_ge(self):
        expr = GE('score', 10).expression(_op_table)
        assert '_pytis_rest_op_test.score >= :score_1' in str(expr)

    def test_in_with_values(self):
        expr = IN('score', (1, 2, 3)).expression(_op_table)
        assert 'IN' in str(expr)

    def test_in_empty_produces_false(self):
        expr = IN('score', ()).expression(_op_table)
        assert str(expr) == 'false'

    def test_like(self):
        expr = LIKE('name', 'A%').expression(_op_table)
        assert 'LIKE' in str(expr).upper()

    def test_like_ignore_case_uses_ilike(self):
        expr = LIKE('name', 'a%', ignore_case=True).expression(_op_table)
        sql = str(expr).upper()
        # SA renders ilike() as ILIKE on PostgreSQL dialect and as
        # LOWER(col) LIKE LOWER(val) on the default string dialect.
        assert 'ILIKE' in sql or ('LOWER' in sql and 'LIKE' in sql)

    def test_and_combines(self):
        expr = AND(EQ('name', 'Alice'), GT('score', 5)).expression(_op_table)
        assert 'AND' in str(expr).upper()

    def test_or_combines(self):
        expr = OR(EQ('name', 'Alice'), EQ('name', 'Bob')).expression(_op_table)
        assert 'OR' in str(expr).upper()

    def test_not_negates(self):
        expr = NOT(EQ('name', 'Alice')).expression(_op_table)
        sql = str(expr).upper()
        # SA may optimise NOT(col = val) to col != val or col <> val.
        assert 'NOT' in sql or '!=' in sql or '<>' in sql

    def test_nested_and_or(self):
        expr = AND(
            OR(EQ('name', 'X'), EQ('name', 'Y')),
            GE('score', 0),
        ).expression(_op_table)
        sql = str(expr).upper()
        assert 'OR' in sql
        assert 'AND' in sql

    def test_unknown_column_raises_payload_error(self):
        with pytest.raises(PayloadError, match='Unknown column'):
            EQ('no_such_column', 'x').expression(_op_table)


# ---------------------------------------------------------------------------
# Layer 1: ResourceHandler model-generation tests — pure Python, no database
# ---------------------------------------------------------------------------

class TestResourceHandlerModel:
    """Test Pydantic model generation from SA column metadata.

    Model generation is purely computational (no I/O), so these tests
    never change on the async migration.
    """

    @pytest.fixture(scope='class')
    def handler(self):
        # TopLevelResourceHandler.__init__ calls PytisAccessor.create() which
        # reflects the gensqlalchemy SA Table — no actual DB connection needed.
        db = MagicMock(spec=Database)
        return TopLevelResourceHandler(_ITEM_SPEC, db)

    def test_out_model_includes_api_columns(self, handler):
        fields = handler.model('out').model_fields
        assert 'code' in fields
        assert 'label' in fields
        assert 'score' in fields
        assert 'category_id' in fields

    def test_out_model_excludes_internal_pk(self, handler):
        # 'id' is the DB PK but not the API key ('code' is); it must be hidden.
        assert 'id' not in handler.model('out').model_fields

    def test_create_model_excludes_internal_pk(self, handler):
        assert 'id' not in handler.model('create').model_fields

    def test_create_model_code_is_required(self, handler):
        # 'code' is NOT NULL with no default → required in create.
        field = handler.model('create').model_fields['code']
        assert field.is_required()

    def test_create_model_label_is_optional(self, handler):
        # 'label' is nullable → optional in create.
        field = handler.model('create').model_fields['label']
        assert not field.is_required()

    def test_patch_model_all_optional(self, handler):
        for name, field_info in handler.model('patch').model_fields.items():
            assert not field_info.is_required(), (
                f"Field {name!r} should be optional in patch model"
            )

    def test_patch_model_excludes_pk(self, handler):
        assert 'id' not in handler.model('patch').model_fields

    def test_invalid_model_kind_raises(self, handler):
        with pytest.raises(ValueError, match='Unknown model kind'):
            handler.model('bogus')

    def test_key_property(self, handler):
        assert handler.key.name == 'code'
        assert handler.key.type is str


# ---------------------------------------------------------------------------
# Layer 2: PytisAccessor integration tests
# ---------------------------------------------------------------------------

class TestPytisAccessor:
    """Test PytisAccessor CRUD via a real PostgreSQL session.

    Each test receives a ``session`` that rolls back on teardown.
    When migrating to async, **only the ``session`` fixture changes**;
    every test body stays identical.
    """

    @pytest.fixture(scope='class')
    def accessor(self):
        return PytisAccessor.create(PytisRestTestItem)

    def test_insert_returns_entity(self, accessor, session):
        row = accessor.insert(session, code='ALPHA')
        assert row.code == 'ALPHA'
        assert row.id is not None

    def test_row_finds_inserted(self, accessor, session):
        accessor.insert(session, code='FIND_ME', label='Found')
        row = accessor.row(session, EQ('code', 'FIND_ME'))
        assert row is not None
        assert row.label == 'Found'

    def test_row_returns_none_when_missing(self, accessor, session):
        assert accessor.row(session, EQ('code', 'NO_SUCH_CODE')) is None

    def test_rows_with_condition(self, accessor, session):
        accessor.insert(session, code='ROWS_A', score=1)
        accessor.insert(session, code='ROWS_B', score=2)
        rows = accessor.rows(session, condition=IN('code', ('ROWS_A', 'ROWS_B')))
        assert len(rows) == 2

    def test_rows_sorted_ascending(self, accessor, session):
        accessor.insert(session, code='SORT_Z', score=9)
        accessor.insert(session, code='SORT_A', score=1)
        rows = accessor.rows(
            session,
            condition=IN('code', ('SORT_A', 'SORT_Z')),
            sorting=(('score', ASCENDENT),),
        )
        assert rows[0].score == 1
        assert rows[1].score == 9

    def test_rows_sorted_descending(self, accessor, session):
        accessor.insert(session, code='DESC_Z', score=9)
        accessor.insert(session, code='DESC_A', score=1)
        rows = accessor.rows(
            session,
            condition=IN('code', ('DESC_A', 'DESC_Z')),
            sorting=(('score', DESCENDANT),),
        )
        assert rows[0].score == 9
        assert rows[1].score == 1

    def test_rows_limit(self, accessor, session):
        for i in range(5):
            accessor.insert(session, code=f'LIM_{i}')
        rows = accessor.rows(
            session,
            condition=LIKE('code', 'LIM_%'),
            limit=3,
        )
        assert len(rows) == 3

    def test_rows_offset(self, accessor, session):
        for i in range(4):
            accessor.insert(session, code=f'OFF_{i:02d}')
        all_rows = accessor.rows(
            session,
            condition=LIKE('code', 'OFF_%'),
            sorting=(('code', ASCENDENT),),
        )
        paged = accessor.rows(
            session,
            condition=LIKE('code', 'OFF_%'),
            sorting=(('code', ASCENDENT),),
            offset=2,
        )
        assert paged[0].code == all_rows[2].code

    def test_update_changes_field(self, accessor, session):
        accessor.insert(session, code='UPD_ME', label='Before')
        updated = accessor.update(session, EQ('code', 'UPD_ME'), label='After')
        assert updated is not None
        assert updated.label == 'After'

    def test_update_returns_none_when_not_found(self, accessor, session):
        assert accessor.update(session, EQ('code', 'GHOST'), label='x') is None

    def test_update_rejects_pk_change(self, accessor, session):
        accessor.insert(session, code='PK_KEEP')
        with pytest.raises(PayloadError, match='Primary key'):
            accessor.update(session, EQ('code', 'PK_KEEP'), id=999)

    def test_delete_removes_row(self, accessor, session):
        accessor.insert(session, code='DEL_ME')
        assert accessor.delete(session, EQ('code', 'DEL_ME')) is True
        assert accessor.row(session, EQ('code', 'DEL_ME')) is None

    def test_delete_returns_false_when_not_found(self, accessor, session):
        assert accessor.delete(session, EQ('code', 'GHOST')) is False

    def test_insert_unknown_column_raises(self, accessor, session):
        with pytest.raises(PayloadError, match='Unknown columns'):
            accessor.insert(session, code='OK', bogus_col='oops')

    def test_row_raises_on_non_unique_match(self, accessor, session):
        # Insert two rows sharing the same score value so the EQ('score', …)
        # condition matches more than one row.
        accessor.insert(session, code='NUQ_1', score=42)
        accessor.insert(session, code='NUQ_2', score=42)
        with pytest.raises(NonUniqueKeyError):
            accessor.row(session, EQ('score', 42))

    def test_insert_duplicate_raises_constraint_violation(self, accessor, session):
        accessor.insert(session, code='ONCE')
        with pytest.raises(ConstraintViolationError):
            accessor.insert(session, code='ONCE')


# ---------------------------------------------------------------------------
# Layer 3: HTTP route integration tests — full stack via TestClient
# ---------------------------------------------------------------------------

class TestHttpRoutes:
    """End-to-end HTTP tests via FastAPI's TestClient.

    TestClient works identically for sync and async route handlers, so
    **no test code changes are needed** when routes become ``async def``.
    """

    @pytest.fixture(autouse=True)
    def clean(self, engine):
        """Truncate test tables after each test so they start empty."""
        yield
        with engine.begin() as conn:
            conn.execute(sa.text('TRUNCATE pytis_rest_test_items CASCADE'))
            conn.execute(sa.text('TRUNCATE pytis_rest_test_categories CASCADE'))

    def test_list_empty(self, client):
        r = client.get('/items')
        assert r.status_code == 200
        assert r.json() == []

    def test_create_returns_201(self, client):
        r = client.post('/items', json={'code': 'NEW', 'label': 'New Item'})
        assert r.status_code == 201
        assert r.json()['code'] == 'NEW'

    def test_create_and_list(self, client):
        client.post('/items', json={'code': 'A', 'label': 'Alpha'})
        client.post('/items', json={'code': 'B', 'label': 'Beta'})
        r = client.get('/items')
        assert r.status_code == 200
        codes = {item['code'] for item in r.json()}
        assert codes == {'A', 'B'}

    def test_get_one(self, client):
        client.post('/items', json={'code': 'GET_ME', 'label': 'Target'})
        r = client.get('/items/GET_ME')
        assert r.status_code == 200
        assert r.json()['code'] == 'GET_ME'
        assert r.json()['label'] == 'Target'

    def test_get_one_not_found(self, client):
        r = client.get('/items/NO_SUCH_CODE')
        assert r.status_code == 404

    def test_create_duplicate_returns_409(self, client):
        client.post('/items', json={'code': 'DUP'})
        r = client.post('/items', json={'code': 'DUP'})
        assert r.status_code == 409

    def test_patch_updates_field(self, client):
        client.post('/items', json={'code': 'PATCH_ME', 'label': 'Before'})
        r = client.patch('/items/PATCH_ME', json={'label': 'After'})
        assert r.status_code == 200
        assert r.json()['label'] == 'After'
        # Verify persistence: a fresh GET must reflect the update.
        assert client.get('/items/PATCH_ME').json()['label'] == 'After'

    def test_patch_not_found_returns_404(self, client):
        r = client.patch('/items/GHOST', json={'label': 'x'})
        assert r.status_code == 404

    def test_delete(self, client):
        client.post('/items', json={'code': 'BYE'})
        r = client.delete('/items/BYE')
        assert r.status_code == 204
        assert client.get('/items/BYE').status_code == 404

    def test_delete_not_found_returns_404(self, client):
        r = client.delete('/items/GHOST')
        assert r.status_code == 404

    def test_list_pagination(self, client):
        for i in range(6):
            client.post('/items', json={'code': f'PAGE_{i:02d}'})
        page1 = client.get('/items?limit=3&offset=0').json()
        page2 = client.get('/items?limit=3&offset=3').json()
        assert len(page1) == 3
        assert len(page2) == 3
        # No overlap between pages.
        assert not {i['code'] for i in page1} & {i['code'] for i in page2}
