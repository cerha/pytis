#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2005 OUI Technology Ltd.
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from __future__ import print_function
from __future__ import division
from builtins import range

import pytest
import time

import pytis.form as pf
import pytis.form.grid as grid
import pytis.presentation as pp
import pytis.data as pd
import pytis.data.gensqlalchemy as sql
import pytis.remote
import pytis.util
import pytis

from pytis.api import app
from pytis.data.test import DBTest, connector, dbconnection, execute

connection_data = {'database': 'test'}

"""Non-interactive tests of pytis.form (the wx application).

Run this test in local mode like:

  pytest lib/pytis/form/test.py -v --disable-warnings

or in remote mode similarly through a Pytis2Go terminal.

"""


class TestDataTable(DBTest):

    @pytest.fixture
    def table(self, execute):
        try:
            execute('create table grid_test'
                     '(id int, name text, price decimal(10, 2), flag bool)')
            yield 'grid_test'
        finally:
            try:
                execute('drop table grid_test')
            except Exception:
                pass

    @pytest.fixture
    def spec(self):
        class Specification(pp.Specification):
            table = 'grid_test'
            fields = (
                pp.Field('id', type=pd.Integer()),
                pp.Field('name', type=pd.String()),
                pp.Field('price', type=pd.Float(precision=2)),
                pp.Field('flag', type=pd.Boolean()),
            )
        return Specification()

    @pytest.fixture
    def data(self, table, spec, dbconnection):
        return spec.data_spec().create(connection_data=dbconnection)

    def grid_table(self, spec, data, row_style=None, field_style={}, sorting=None, grouping=()):
        fields = [f.clone(pp.Field(f.id(), style=field_style.get(f.id()))) for f in spec.fields]
        row = pp.PresentedRow(fields, data, None)
        count = data.select(sort=sorting or ())  # , async_count=True)
        return grid.DataTable(data, row, row.fields(), count, row_style=row_style,
                              sorting=sorting, grouping=grouping)

    def test_cell_value(self, connector, spec, data):
        self.insert(connector, data, (
            (1, 'Apple', 12.3, False),
            (2, 'Banana', 23.4, True),
            (3, 'Strawberry', 2.34, False),
            (4, 'Orange', None, True),
        ))
        t = self.grid_table(spec, data)
        try:
            assert t.cell_value(0, 0) == '1'
            assert t.cell_value(1, 2) == '23.40'
            assert t.cell_value(3, 2) == ''
            assert t.cell_value(3, 3) == 'T'
            assert t.cell_value(4, 2) is None
            assert t.cell_value(3, 4) is None
        finally:
            data.close()

    def test_cell_style(self, connector, spec, data):
        self.insert(connector, data, (
            (1, 'Apple', 12.3, False),
            (2, 'Banana', 23.4, True),
            (3, 'Strawberry', 2.34, True),
            (4, 'Orange', None, True),
        ))
        t = self.grid_table(
            spec, data,
            row_style=lambda r: pp.Style(background='#ffa') if r['id'].value() % 2 else None,
            field_style={'price': lambda r: pp.Style(bold=True) if r['flag'].value() else None},
        )
        try:
            assert t.cell_style(4, 1) is None
            assert t.cell_style(3, 4) is None
            assert t.cell_style(0, 2) == pp.Style(background='#ffa')
            assert t.cell_style(1, 3) == pp.Style()
            assert t.cell_style(1, 2) == pp.Style(bold=True)
            assert t.cell_style(2, 2) == pp.Style(bold=True, background='#ffa')
        finally:
            data.close()

    def test_grouping(self, connector, spec, data):
        sequence = (
            # Sequence of name, number of rows with flag=False, number of rows with flag=True.
            ('Charles', 44, 21), ('Jerry', 21, 34), ('Joe', 25, 14), ('Mike', 67, 21),
            ('Paul', 16, 8), ('Peter', 6, 56), ('Robert', 24, 36), ('Sam', 4, 81), ('Tom', 64, 32),
        )

        def rows():
            i = 0
            for name, false_count, true_count in sequence:
                for count, flag in ((false_count, False), (true_count, True)):
                    for x in range(count):
                        i += 1
                        # Generate some randomly looking price value.
                        price = (i % 8 + 140 * (i % 6 + count) / (2 + i % 6)) / 100
                        yield (i, name, price, flag)

        self.insert(connector, data, rows())
        t = self.grid_table(spec, data, sorting=(('name', pd.ASCENDENT),
                                                 ('flag', pd.ASCENDENT),
                                                 ('price', pd.ASCENDENT),),
                            grouping=('name', 'flag'))
        try:
            for row in range(0, 200, 16):
                # The flag matches the group value because the group changes on each flag change...
                assert t.group(row) is (t.cell_value(row, 3) == 'T')
            # But if we jump more than 80 rows away, the cache is discarded
            # and we start from False again.
            assert t.group(300) is False
            # And the flag is now the negation of the group until we jump too far again.
            for row in range(330, t.number_of_rows(), 16):
                assert t.group(row) is (t.cell_value(row, 3) == 'F')
        finally:
            data.close()

    def test_buffer_fetching(self, connector, spec, data):
        # This test can be used to examine row buffer filling strategy
        # and database performance on bigger jumps.  It is practical to
        # add something like:
        #
        # print('  ', query.format()[:140])
        #
        # into PostgreSQLConnector._pg_query() in postgresql.py and
        # run pytest with -s to see how the buffer is filled in response
        # to row requests.
        def rows(count):
            i = 0
            while i < count:
                i += 1
                yield (i, 'Row number %d' % i, 1.86 * i, i % 3 == 0)
        self.insert(connector, data, rows(9500))

        t = self.grid_table(spec, data, sorting=(('id', pd.ASCENDENT),))
        try:
            for n in (30, 29, 28, 27, 26, 9440, 9439, 9438, 9437, 9436,
                      50, 199, 200, 299, 220, 160, 120, 80, 40, 0,
                      4444, 0, 2453, 3890, 9499, 99, 140, 98, 30):
                print(n)
                assert t.cell_value(n, 0) == str(n + 1)
        finally:
            data.close()

    def emulate_real_grid(self, t):
        t.cell_style(0, 0)
        # The faster the user scrolls, the less rows the grid asks for.  If we stop
        # scrolling (or scroll slow enough) it renders the full page (30 rows in our
        # example.
        for (start, count) in ((29, 30), (50, 3), (199, 1), (299, 30), (160, 30),
                               (120, 5), (80, 3), (40, 2), (29, 30),
                               (140, 3), (360, 6), (1120, 4), (2453, 8),
                               (3890, 30), (9499, 30)):
            # The grid first checks for style and value of each visible cell
            # from bottom up and from right to left.
            for row in range(start, start - count, -1):
                for col in range(3, -1, -1):
                    t.cell_style(row, col)
                    t.group(row)
                    t.cell_value(row, col)
            if count == 30:
                # Then if we stop scrolling, the grid checks for the same
                # styles once again from top to bottom and from right to
                # left.  Maybe it renders backgrounds and fonts separately?
                for row in range(start - count + 1, start + 1):
                    for col in range(0, 4):
                        t.cell_style(row, col)
                        t.group(row)

    def test_grid_performance(self, connector, spec, data, benchmark):
        def rows(count):
            i = 0
            while i < count:
                i += 1
                yield (i, 'Row number %d' % i, 1.86 * i, i % 3 == 0)
        self.insert(connector, data, rows(9500))
        t = self.grid_table(
            spec, data,
            sorting=(('name', pd.ASCENDENT),
                     ('flag', pd.ASCENDENT),
                     ('price', pd.ASCENDENT),),
            grouping=('name', 'flag'),
            row_style=lambda r: pp.Style(background='#ffa') if r['id'].value() % 2 else None,
            field_style={'price': lambda r: pp.Style(bold=True) if r['flag'].value() else None},
        )
        try:
            benchmark(self.emulate_real_grid, t)
        finally:
            data.close()


class user_params(sql.SQLTable):
    name = 'user_params'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('login', pytis.data.Name(not_null=True)),
              sql.Column('name', pytis.data.String(not_null=True)),
              sql.Column('number', pytis.data.Integer(not_null=True)),
              sql.Column('enabled', pytis.data.Boolean(not_null=True)),
              )
    init_columns = ('login', 'name', 'number', 'enabled')
    init_values = (
        ('test', 'Test User', 64, True),
        ('bob', 'Bob User', 1024, False),
    )


class UserParams(pp.Specification):
    table = user_params
    public = True


class square(sql.SQLPlFunction):
    name = 'square'
    arguments = (
        sql.Column('x', pytis.data.Integer()),
    )
    result_type = pytis.data.Integer()
    def body(self):
        return """
begin
  return x * x;
end;
        """


class Application(pp.Application):
    def params(self):
        return (
            pp.SharedParams('user', 'test.UserParams',
                            pd.EQ('login', pd.sval('test'))),
        )


@pytest.fixture(scope='class')
def initdb(execute):
    dbdefs = (
        ('pytis.dbdefs.db_pytis_config', ('EPytisConfig', 'XChanges', 'EPytisFormSettings',
                                          'EPytisFormProfileBase', 'EPytisFormProfileParams',
                                          'EPytisAggregatedViews')),
        ('pytis.form.test', ('user_params', 'square')),
    )
    init_sql = []
    teardown_sql = []
    for module, names in dbdefs:
        regexp = '^({})'.format('|'.join(names))
        init_sql.append(sql.capture(sql.gsql_module, module, regexp=regexp, plpython3=True))
        names_sql = sql.capture(sql.gsql_module, module, names_only=True, regexp=regexp, plpython3=True)
        for name in names_sql.splitlines():
            if not name.startswith(b'None '):
                teardown_sql.append(b'drop ' + name
                                    .replace(b'TABLE ', b' table if exists ')
                                    .replace(b'VIEW ', b' view if exists ')
                                    .replace(b'TRIGGER ', b'function if exists ')
                                    .replace(b'FUNCTION ', b'function if exists ')
                                    .replace(b'SEQUENCE ', b'sequence if exists ')
                                    .replace(b'TYPE ', b'type if exists ')
                                    .replace(b'::', b' ')
                                    + b' cascade')
    #init_sql.extend(("insert into c_pytis_crypto_names (name) values ('test')",
    #          "insert into e_pytis_crypto_keys (name, username, key) "
    #          "values ('test', current_user, "
    #          "pytis_crypto_store_key('somekey', 'somepassword'))",
    #          "create table cfoo (id serial, x bytea, y bytea, z bytea)"))
    try:
        for part in init_sql:
            execute(part)
        yield
    finally:
        for part in teardown_sql:
            execute(part)


@pytest.fixture(scope='class')
def initconfig(dbconnection):
    pytis.config.resolver = pytis.util.Resolver(search=('pytis.form.test', 'pytis.form', 'pytis.defs'))
    pytis.config.dbconnection = dbconnection


@pytest.fixture(scope='class')
def initapp(initconfig, initdb):
    # Avoid removing the info file (to allow running tests multiple times).
    pytis.remote.keep_x2go_info_file()
    # TODO: When we start the main wx app thread, the tests randomly end with
    # SIGABRT, SIGSEGV or SIGTRAP.  Probabbly because the wx app doesn't run in
    # the main thread (see https://wiki.wxpython.org/MainLoopAsThread).  But for
    # the currently present tests it doesn't seem to be necessary to run the app
    # thread, so we just create the Application instance.
    #threading.Thread(target=pf.Application(headless=True).run).start()
    pf.Application(headless=True)
    yield
    # Release the pytis.form.Application instance from pytis.api.app to make sure
    # the tests outside this class don't use the previously created instance.
    pf.app.ExitMainLoop()
    app.exit()
    app.release()


@pytest.mark.usefixtures('initapp')
class TestApp(DBTest):
    """Tests running inside wx Application environment.

    Starts a headless wx application in a separate thread and runs test cases
    within its environment.  It may be useful for testing fetures which require
    the wx Application to exist.

    To debug:
    gdb python (in the appropriate venv)
    (gdb) run -m pytest src/pytis/lib/pytis/form/test.py::TestApp -v --disable-warnings

    """
    def test_api_form(self):
        import time
        assert app.form is None
        f = app.run_form('test.UserParams')
        assert app.form is not None
        # TODO: Testing form attributes below is very fragile.  Sometimes it
        # works, but most often it causes SIGABRT, SIGSEGV or SIGTRAP.
        assert app.form.name == 'test.UserParams'
        #assert app.form.query_fields is not None
        #assert app.form.query_fields.row is not None
        #assert app.form.query_fields.row['min'].value() == 10

    def test_shared_params(self):
        assert app.param.user.name == 'Test User'
        assert app.param.user.number == 64
        assert app.param.user.enabled == True
        with pytest.raises(AttributeError):
            app.param.user.xval
        with pytest.raises(AttributeError):
            app.param.user.xyz = 1

    def test_shared_param_callbacks(self):
        def callback():
            names.append(app.param.user.name)
        name = app.param.user.name
        try:
            names = []
            app.param.user.add_callback('name', callback)
            app.param.user.name = 'x'
            assert app.param.user.name == 'x'
            pf.app.wx_yield()  # Let the callbacks be called (randomly unreliable).
            time.sleep(0.2)
            app.param.user.name = 'y'
            assert app.param.user.name == 'y'
            pf.app.wx_yield()  # Let the callbacks be called (randomly unreliable).
            time.sleep(0.2)
            assert names == ['x', 'y']
        finally:
            app.param.user.name = name

    def test_dbfunction(self):
        assert pd.dbfunction(square, 5) == 25
        assert pd.dbfunction('square', 4) == 16
        assert pd.dbfunction('reverse', 'Hello World') == 'dlroW olleH'

    def test_write_and_read_file(self):
        filename = '/tmp/test.txt'
        for m, data in (
                ('b', b'some bytes'),
                ('t', u'some text'),
        ):
            app.write_file(data, filename, mode='w'+m)
            with app.open_file(filename, mode='r'+m) as f:
                assert f.name == filename
                assert f.read() == data

    def test_write_file_type_errors(self):
        filename = '/tmp/test.txt'
        with pytest.raises(TypeError):
            app.write_file(u'text', filename, mode='wb')
        with pytest.raises(TypeError):
            app.write_file(b'bytes', filename, mode='w')
        with pytest.raises(TypeError):
            app.write_file(8, filename, mode='wb')


def test_shared_params(initconfig, initdb):
    # Shared params must work with the pytis.form.Application instance
    # (tessted in TestApp above) as well as without it (with automatically
    # created pytis.api.BaseApplication).
    assert app.param.user.name == 'Test User'
    assert app.param.user.number == 64
