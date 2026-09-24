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

Tests are organised in three layers to minimise the number of changes needed
when the framework switches from sync (psycopg2/SQLAlchemy 1.4) to async
(asyncpg/SQLAlchemy 2.0):

Layer 1 — pure Python (zero changes ever): TestOperators,
TestResourceHandlerModel No sessions, no database.  These test operator
expression generation and Pydantic model derivation from SA column metadata.

Layer 2 — session fixture (ONE fixture change per file): TestPytisAccessor
Test bodies call accessor methods (row, rows, insert, update, delete)
through the `session` fixture.  On async migration only the `session`
fixture body changes; all test bodies stay identical.  The async form is
documented in the fixture docstring.

Layer 3 — HTTP client (zero changes ever): TestHttpRoutes FastAPI's
TestClient handles both sync and async route handlers transparently, so no
test code changes are needed when route closures become `async def`.

Running integration tests
--------------------------

`TestPytisAccessor` and `TestHttpRoutes` require a PostgreSQL database. Set
`PYTIS_REST_TEST_DB` to a connection URL (default:
`postgresql://localhost/test`).  Tests are automatically skipped when the
database is unreachable.

"""

import hashlib
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
    ResourceSpec, ForeignKey, Header, Body, Raw, Identity, Derived, Default, Status,
    _Request, _answer,
    TopLevelResourceHandler,
    add_api_routes, api_key_dependency,
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
        # Sloupec s výchozí hodnotou: ukazuje, co se stane s polem, které klient
        # nepošle, proti poli poslanému jako null.
        gsql.Column('status', pd.String(), default='new'),
        # Binární sloupec: syrové tělo požadavku přichází jako bajty.
        gsql.Column('content', pd.Binary()),
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

def _derive_label(payload, operation):
    """Provider: derive 'label' from 'code' and refuse one code."""
    if payload.get('code') == 'BAD':
        raise PayloadError("Code 'BAD' is not accepted")
    if 'code' not in payload:
        # Nothing to derive from: an update that does not touch 'code'.
        return {}
    return {'label': payload['code'].lower()}


# Same table as _ITEM_SPEC under another name, with every kind of source:
# 'score' from a header, 'label' derived from another value, 'status' a default
# overriding the one in the database.
_SOURCED_SPEC = ResourceSpec(
    name='sourced-items',
    table=PytisRestTestItem,
    key=('code',),
    sources={
        'score': Header('X-Score', type=int),
        'label': (Header('X-Label'), Derived(_derive_label)),
        'status': Default('sourced'),
    },
)

def _derive_code(payload, operation):
    """Return a code derived from the raw body, so the raw value is visible here."""
    raw = payload.get('content')
    return {'code': hashlib.sha256(raw).hexdigest()[:8]} if raw else {}


#: The body is one column as it arrived, so every other column says where it
#: comes from; 'status' says "from nowhere" and is left to the database.
_RAW_SPEC = ResourceSpec(
    name='raw-items',
    table=PytisRestTestItem,
    key=('code',),
    # The body is not worth repeating in the response.
    unreturned=('content',),
    sources={
        'content': Raw(limit=64),
        'code': Derived(_derive_code),
        'score': Header('X-Score', type=int),
        'label': (),
        'status': (),
        'category_id': (),
    },
)

#: The database decides what happened, so the answer follows 'status'.
_STATUS_SPEC = ResourceSpec(
    name='status-items',
    table=PytisRestTestItem,
    key=('code',),
    create_status=Status(
        decide=lambda record: 200 if record['status'] == 'seen' else 201,
        descriptions={201: 'The item is new.', 200: 'The item was already known.'},
    ),
    update_status=Status(
        decide=lambda record: 200 if record['status'] == 'seen' else 201,
        descriptions={201: 'The item is new.', 200: 'The item was already known.'},
    ),
)

#: The record says who wrote it, on the authority of the key it arrived with.
_IDENTIFIED_SPEC = ResourceSpec(
    name='identified-items',
    table=PytisRestTestItem,
    key=('code',),
    sources={'label': Identity()},
)

_ALL_OPS = dict(get=True, list=True, create=True, update=True, delete=True)

#: What the secured client accepts.  Held in a dict rather than in a plain
#: global so that a test can monkeypatch it.
_CONFIG = {'api_key': {'hub': 'hub-key', 'other': 'other-key'}}

# ---------------------------------------------------------------------------
# Database / session fixtures
# ---------------------------------------------------------------------------

_TEST_DSN = os.environ.get('PYTIS_REST_TEST_DB', 'postgresql://localhost/test')


@pytest.fixture(scope='session')
def engine():
    """Session-scoped engine.  Tests that depend on it are skipped when the database is unreachable.  Also creates/drops the test tables.

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

    This is the **only fixture that needs to change** when migrating to async.
    All test bodies that use this fixture remain unchanged.

    Async form (SQLAlchemy 2.0 + asyncpg): `@pytest.fixture async def session(async_engine):`.

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

    TestClient works identically for both sync and async route handlers, so
    nothing here changes on the async migration.

    """
    app = fastapi.FastAPI()
    router = fastapi.APIRouter()
    add_api_routes(router, db, _ITEM_SPEC, operations=_ALL_OPS)
    add_api_routes(router, db, _CATEGORY_SPEC, operations=_ALL_OPS)
    add_api_routes(router, db, _SOURCED_SPEC, operations=_ALL_OPS)
    add_api_routes(router, db, _RAW_SPEC, operations=_ALL_OPS)
    add_api_routes(router, db, _STATUS_SPEC, operations=_ALL_OPS)
    app.include_router(router)
    with TestClient(app) as c:
        yield c


@pytest.fixture(scope='session')
def secured_client(db):
    """FastAPI TestClient whose routes require an API key."""
    app = fastapi.FastAPI()
    router = fastapi.APIRouter(
        dependencies=[api_key_dependency(lambda: _CONFIG['api_key'])],
    )
    add_api_routes(router, db, _IDENTIFIED_SPEC, operations=_ALL_OPS)
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

    Model generation is purely computational (no I/O), so these tests never
    change on the async migration.

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


class TestValueSources:
    """Test ResourceSpec.sources where no database is needed.

    What the models contain is pure computation, and a rejected payload never
    reaches the insert, so a mock Database suffices.  What actually gets written
    is covered by the HTTP tests.

    """

    @pytest.fixture(scope='class')
    def handler(self):
        return TopLevelResourceHandler(_SOURCED_SPEC, MagicMock(spec=Database))

    @pytest.fixture(scope='class')
    def raw_handler(self):
        return TopLevelResourceHandler(_RAW_SPEC, MagicMock(spec=Database))

    def test_a_header_sourced_column_is_not_in_the_body(self, handler):
        # 'score' comes from X-Score and 'label' from X-Label before anything
        # else, so the body has no place for either.
        for name in ('score', 'label'):
            assert name not in handler.model('create').model_fields
            assert name not in handler.model('patch').model_fields
            # Reading is unaffected.
            assert name in handler.model('out').model_fields

    def test_only_a_source_other_than_the_body_makes_a_column_optional(self):
        # 'code' is NOT NULL without a default, so the client must send it...
        plain = TopLevelResourceHandler(_ITEM_SPEC, MagicMock(spec=Database))
        assert plain.model('create').model_fields['code'].is_required()
        # ...and saying out loud that it comes from the body changes nothing.
        spelt = TopLevelResourceHandler(
            ResourceSpec(name='x', table=PytisRestTestItem, key=('code',),
                         sources={'code': Body()}),
            MagicMock(spec=Database),
        )
        assert spelt.model('create').model_fields['code'].is_required()
        # A source other than the body takes the column out of the body
        # altogether, so the question of requiring it there does not arise.
        sourced = TopLevelResourceHandler(
            ResourceSpec(name='x', table=PytisRestTestItem, key=('code',),
                         sources={'code': Derived(_derive_label)}),
            MagicMock(spec=Database),
        )
        assert 'code' not in sourced.model('create').model_fields

    def test_a_column_with_an_empty_chain_is_readable_but_not_writable(self):
        handler = TopLevelResourceHandler(
            ResourceSpec(name='x', table=PytisRestTestItem, key=('code',),
                         sources={'status': ()}),
            MagicMock(spec=Database),
        )
        assert 'status' not in handler.model('create').model_fields
        assert 'status' not in handler.model('patch').model_fields
        assert 'status' in handler.model('out').model_fields
        # Nothing fills it either, so the database decides.
        assert handler._apply_sources(_Request({'code': 'A'}, {}), 'create') == {'code': 'A'}

    def test_sources_are_tried_in_order(self, handler):
        # The header wins over the provider; the provider only fills in what is
        # left, and a default applies to an insert.
        assert handler._apply_sources(
            _Request({'code': 'A'}, {'label': 'sent'}), 'create') == {
            'code': 'A', 'label': 'sent', 'status': 'sourced',
        }
        assert handler._apply_sources(_Request({'code': 'A'}, {}), 'create') == {
            'code': 'A', 'label': 'a', 'status': 'sourced',
        }

    def test_the_body_wins_only_where_it_is_declared_first(self):
        # The same two sources either way round, so only the order decides.
        def handler_for(*sources):
            return TopLevelResourceHandler(
                ResourceSpec(name='x', table=PytisRestTestItem, key=('code',),
                             sources={'label': sources}),
                MagicMock(spec=Database),
            )
        request = _Request({'label': 'body'}, {'label': 'header'})
        assert handler_for(Header('X-Label'), Body())._apply_sources(
            request, 'create')['label'] == 'header'
        assert handler_for(Body(), Header('X-Label'))._apply_sources(
            request, 'create')['label'] == 'body'

    def test_a_default_does_not_apply_to_an_update(self, handler):
        # A value absent from a partial payload means "do not change it".
        assert handler._apply_sources(_Request({}, {'label': 'x'}), 'update') == {'label': 'x'}

    def test_a_provider_decides_what_an_update_changes(self, handler):
        # Nothing to derive from, so the provider returns nothing.
        assert handler._apply_sources(_Request({}, {'score': 3}), 'update') == {'score': 3}
        # The value it derives from is changing, so the derived one changes too.
        assert handler._apply_sources(_Request({'code': 'B'}, {}), 'update') == {
            'code': 'B', 'label': 'b',
        }

    def test_raw_refuses_to_share_the_body(self):
        # 'label' takes the whole body, so 'code' cannot be read from it, and
        # neither can the columns that say nothing and would be by default.
        with pytest.raises(ValueError) as e:
            TopLevelResourceHandler(
                ResourceSpec(name='x', table=PytisRestTestItem, key=('code',),
                             sources={'content': Raw(), 'code': Body()}),
                MagicMock(spec=Database),
            )
        assert 'code' in str(e.value) and 'score' in str(e.value)

    def test_raw_cannot_be_one_source_among_several(self):
        with pytest.raises(ValueError) as e:
            TopLevelResourceHandler(
                ResourceSpec(name='x', table=PytisRestTestItem, key=('code',),
                             sources={'content': (Raw(), Default('x'))}),
                MagicMock(spec=Database),
            )
        assert 'whole value or nothing' in str(e.value)

    def test_the_raw_body_is_not_a_field_of_anything(self, raw_handler):
        assert raw_handler.raw == 'content'
        assert raw_handler.model('create').model_fields == {}
        # ...and 'unreturned' keeps it out of the response as well.
        assert 'content' not in raw_handler.model('out').model_fields
        assert 'label' in raw_handler.model('out').model_fields

    def test_the_raw_body_reaches_the_values_and_the_providers(self, raw_handler):
        assert raw_handler._apply_sources(
            _Request({}, {'score': 7}, raw=b'hello'), 'create') == {
            'content': b'hello',
            'code': hashlib.sha256(b'hello').hexdigest()[:8],
            'score': 7,
        }

    def test_a_rejecting_provider_is_reported_as_422(self):
        handler = TopLevelResourceHandler(_SOURCED_SPEC, MagicMock(spec=Database))
        with pytest.raises(fastapi.HTTPException) as e:
            handler.create_one(_Request({'code': 'BAD'}, {}))
        assert e.value.status_code == 422


# ---------------------------------------------------------------------------
# Layer 2: PytisAccessor integration tests
# ---------------------------------------------------------------------------

class TestPytisAccessor:
    """Test PytisAccessor CRUD via a real PostgreSQL session.

    Each test receives a `session` that rolls back on teardown. When migrating
    to async, **only the `session` fixture changes**; every test body stays
    identical.

    """

    @pytest.fixture(scope='class')
    def accessor(self):
        return PytisAccessor.create(PytisRestTestItem)

    def test_insert_rejects_a_value_the_database_refuses(self, accessor, session):
        # A value of the wrong shape fails in the database (SQL class 22), not
        # in a constraint; the client needs 422, not 500.
        with pytest.raises(PayloadError):
            accessor.insert(session, code='BAD_SCORE', score='not a number')

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

    TestClient works identically for sync and async route handlers, so **no test
    code changes are needed** when routes become `async def`.

    """

    @pytest.fixture(autouse=True)
    def clean(self, engine):
        """Truncate test tables after each test so they start empty."""
        yield
        with engine.begin() as conn:
            conn.execute(sa.text('TRUNCATE pytis_rest_test_items CASCADE'))
            conn.execute(sa.text('TRUNCATE pytis_rest_test_categories CASCADE'))

    def test_create_takes_the_whole_body_as_one_value(self, client):
        # The sender posts the document as it is, with no envelope and any
        # content type, and it is stored byte for byte.
        document = '<Doc>ěščř</Doc>'.encode('utf-8')
        r = client.post('/raw-items', content=document,
                        headers={'Content-Type': 'application/xml', 'X-Score': '7'})
        assert r.status_code == 201
        assert r.json()['score'] == 7
        # 'code' was derived from the raw body, so the provider saw it.
        assert r.json()['code'] == hashlib.sha256(document).hexdigest()[:8]
        # 'status' declares an empty chain, so the database default applied.
        assert r.json()['status'] == 'new'
        # The body itself is not returned, though it was written.
        assert 'content' not in client.get(
            '/raw-items/' + hashlib.sha256(document).hexdigest()[:8]).json()

    def test_create_refuses_a_body_over_the_limit(self, client):
        # Declared length is checked before anything is read...
        r = client.post('/raw-items', content=b'x' * 65)
        assert r.status_code == 413
        # ...and so is what actually arrives, when the client declares nothing.
        r = client.post('/raw-items', content=iter([b'x' * 40, b'x' * 40]))
        assert r.status_code == 413
        assert client.post('/raw-items', content=b'x' * 64).status_code == 201

    def test_create_rejects_nothing_about_the_body(self, client):
        # Whatever arrives is the value, including what no parser would accept.
        r = client.post('/raw-items', content=b'\x00not json')
        assert r.status_code == 201

    def test_create_answers_by_what_the_database_decided(self, client):
        # The column says the row is new, so the answer is the plain 201...
        r = client.post('/status-items', json={'code': 'A', 'status': 'new'})
        assert r.status_code == 201
        # ...and when it says otherwise, the answer follows it, even though a
        # row was written either way.
        r = client.post('/status-items', json={'code': 'B', 'status': 'seen'})
        assert r.status_code == 200
        assert r.json()['code'] == 'B'
        # An unmapped value keeps the default.
        assert client.post(
            '/status-items', json={'code': 'C', 'status': 'other'}).status_code == 201

    def test_update_answers_the_same_way(self, client):
        client.post('/status-items', json={'code': 'A', 'status': 'new'})
        r = client.patch('/status-items/A', json={'status': 'seen'})
        assert r.status_code == 200
        assert client.patch('/status-items/A', json={'status': 'new'}).status_code == 201

    def test_an_undeclared_status_is_a_server_error(self, client):
        # The schema would otherwise lie about what the endpoint answers.
        with pytest.raises(fastapi.HTTPException) as e:
            _answer(Status(decide=lambda record: 418, descriptions={200: ''}),
                    'items', {}, fastapi.Response())
        assert e.value.status_code == 500
        assert '418' in e.value.detail

    def test_the_declared_status_codes_are_in_the_schema(self, client):
        paths = client.get('/openapi.json').json()['paths']
        post = paths['/status-items']['post']
        assert set(post['responses']) >= {'200', '201'}
        assert post['responses']['200']['description'] == 'The item was already known.'
        assert post['responses']['201']['description'] == 'The item is new.'
        patch = paths['/status-items/{code}']['patch']
        assert set(patch['responses']) >= {'200', '201'}

    def test_a_structured_error_reaches_the_client(self, client, engine):
        # A trigger that puts JSON in DETAIL decides what the sender is told,
        # so the sender does not have to match on the message text.
        with engine.begin() as conn:
            conn.execute(sa.text("""
                CREATE OR REPLACE FUNCTION pytis_rest_test_refuse() RETURNS trigger AS $$
                BEGIN
                    IF new.code = 'NOPE' THEN
                        RAISE EXCEPTION 'The code % is not allowed here.', new.code
                            USING ERRCODE = '22023',
                                  DETAIL = json_build_object('code', 'forbidden_code',
                                                             'sent', new.code)::text;
                    END IF;
                    RETURN new;
                END; $$ LANGUAGE plpgsql"""))
            conn.execute(sa.text('CREATE TRIGGER pytis_rest_test_refuse BEFORE INSERT '
                                 'ON pytis_rest_test_items FOR EACH ROW '
                                 'EXECUTE FUNCTION pytis_rest_test_refuse()'))
        try:
            r = client.post('/items', json={'code': 'NOPE'})
            assert r.status_code == 422
            assert r.json()['detail'] == {
                'code': 'forbidden_code', 'sent': 'NOPE',
                'message': 'The code NOPE is not allowed here.',
            }
            # Without such a structure the message alone comes back, as before.
            assert client.post('/items', json={'code': 'FINE'}).status_code == 201
        finally:
            with engine.begin() as conn:
                conn.execute(sa.text('DROP TRIGGER pytis_rest_test_refuse '
                                     'ON pytis_rest_test_items'))
                conn.execute(sa.text('DROP FUNCTION pytis_rest_test_refuse'))

    def test_the_error_codes_are_in_the_schema(self, client):
        paths = client.get('/openapi.json').json()['paths']
        assert set(paths['/items']['post']['responses']) >= {'409', '422'}
        assert set(paths['/items/{code}']['patch']['responses']) >= {'404', '409', '422'}
        # Only an endpoint that limits the body can answer 413.
        assert '413' in paths['/raw-items']['post']['responses']
        assert '413' not in paths['/items']['post']['responses']

    def test_an_unauthenticated_request_is_refused_as_such(self, secured_client):
        # 401, not 403: the caller has not said who it is, so the answer has to
        # ask, which is what the WWW-Authenticate header does.
        r = secured_client.get('/identified-items')
        assert r.status_code == 401
        assert r.headers['WWW-Authenticate'] == 'ApiKey'
        assert secured_client.get(
            '/identified-items', headers={'X-API-Key': 'wrong'}).status_code == 401
        assert secured_client.get(
            '/identified-items', headers={'X-API-Key': 'hub-key'}).status_code == 200

    def test_the_record_says_which_key_wrote_it(self, secured_client):
        r = secured_client.post('/identified-items', json={'code': 'FROM-HUB'},
                                headers={'X-API-Key': 'hub-key'})
        assert r.status_code == 201
        assert r.json()['label'] == 'hub'
        # Another key, another name.
        r = secured_client.post('/identified-items', json={'code': 'FROM-OTHER'},
                                headers={'X-API-Key': 'other-key'})
        assert r.json()['label'] == 'other'
        # And the sender has no say in it: the column is not a body field, so
        # claiming to be someone else does not even get as far as being ignored.
        r = secured_client.post('/identified-items',
                                json={'code': 'IMPOSTOR', 'label': 'hub'},
                                headers={'X-API-Key': 'other-key'})
        assert r.status_code == 422

    def test_a_key_without_a_name_is_a_misconfiguration(self, secured_client, monkeypatch):
        # A bare key would authenticate without saying who, leaving nothing to
        # record and nothing to authorise against, so it is refused as the
        # server's fault rather than quietly accepted.
        monkeypatch.setitem(_CONFIG, 'api_key', 'plain-key')
        assert secured_client.get(
            '/identified-items', headers={'X-API-Key': 'plain-key'}).status_code == 500

    def test_list_empty(self, client):
        r = client.get('/items')
        assert r.status_code == 200
        assert r.json() == []

    def test_create_returns_201(self, client):
        r = client.post('/items', json={'code': 'NEW', 'label': 'New Item'})
        assert r.status_code == 201
        assert r.json()['code'] == 'NEW'

    def test_create_applies_the_database_default(self, client):
        # A column with a default keeps it whether the client omits the field or
        # sends it as null: the ORM leaves a None value out of the insert when
        # the column has a default, so a default cannot be overridden by null.
        assert client.post('/items', json={'code': 'OMITTED'}).json()['status'] == 'new'
        assert client.post('/items', json={'code': 'NULLED', 'status': None}
                           ).json()['status'] == 'new'

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

    def test_header_source_fills_the_column(self, client):
        r = client.post('/sourced-items', json={'code': 'HDR'}, headers={'X-Score': '42'})
        assert r.status_code == 201
        assert r.json()['score'] == 42

    def test_header_source_is_declared_in_the_schema(self, client):
        parameters = client.get('/openapi.json').json()['paths']['/sourced-items']['post'][
            'parameters']
        assert {p['name'] for p in parameters} == {'X-Score', 'X-Label'}
        assert all(p['in'] == 'header' and not p['required'] for p in parameters)

    def test_header_source_cannot_be_sent_in_the_body(self, client):
        r = client.post('/sourced-items', json={'code': 'INBODY', 'score': 42})
        assert r.status_code == 422

    def test_sources_are_tried_in_the_declared_order(self, client):
        # Header first, then the provider, then whatever the database has.
        sent = client.post('/sourced-items', json={'code': 'FIRST'},
                           headers={'X-Label': 'from header'}).json()
        assert sent['label'] == 'from header'
        derived = client.post('/sourced-items', json={'code': 'SECOND'}).json()
        assert derived['label'] == 'second'

    def test_default_source_beats_the_database_default(self, client):
        assert client.post('/sourced-items', json={'code': 'DEF'}).json()['status'] == 'sourced'
        assert client.post('/items', json={'code': 'DB'}).json()['status'] == 'new'

    def test_a_rejected_value_never_reaches_the_database(self, client):
        assert client.post('/sourced-items', json={'code': 'BAD'}).status_code == 422
        assert client.get('/items/BAD').status_code == 404

    def test_update_takes_a_header_too(self, client):
        client.post('/sourced-items', json={'code': 'PATCHED'})
        r = client.patch('/sourced-items/PATCHED', json={}, headers={'X-Score': '7'})
        assert r.status_code == 200
        assert r.json()['score'] == 7

    def test_list_pagination(self, client):
        for i in range(6):
            client.post('/items', json={'code': f'PAGE_{i:02d}'})
        page1 = client.get('/items?limit=3&offset=0').json()
        page2 = client.get('/items?limit=3&offset=3').json()
        assert len(page1) == 3
        assert len(page2) == 3
        # No overlap between pages.
        assert not {i['code'] for i in page1} & {i['code'] for i in page2}
