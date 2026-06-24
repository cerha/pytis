#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import print_function
from __future__ import division
from builtins import range

import copy
import functools
import io
import os
import pytest
import sys
import tempfile
import threading
import time

import rpyc
from rpyc.utils.server import ThreadedServer

import pytis.form.application as _pfa
import pytis.form.grid as grid
import pytis.presentation as pp
import pytis.data as pd
import pytis.data.gensqlalchemy as sql
import pytis.remote
import pytis.util
import pytis

from pytis.api import app
from pytis.data.test import DBTest, connector, dbconnection, execute
from pytis.remote.remote import RPCInfo
from pytis.remote.test_integration import MockClientUIBackend, _TestRPyCService, _find_free_port

connection_data = {'database': 'test'}
"""Tests of pytis.form.

Run in local mode:

  pytest pytis/form/test.py -v --disable-warnings

or in remote mode through a Pytis2Go terminal.

TestDataTable (grid) and TestAppDB run headlessly (DB only, no display).
TestAppFileIO and TestAppWriteSelectedFile test file I/O through both local
and RPyC (mocked) paths without a display.
Tests requiring a live wx application live in pytis/demo/test_app.py and
are invoked via 'python -m pytis.run --run-tests'.

"""


class TestListFormSelection:

    @pytest.fixture
    def selection(self):
        from pytis.form.list import ListForm
        try:
            import unittest.mock as mock
        except ImportError:
            import mock
        block = mock.Mock(GetTopRow=mock.Mock(return_value=0),
                          GetBottomRow=mock.Mock(return_value=2))
        grid = mock.Mock(GetSelectedRowBlocks=mock.Mock(return_value=[block]))
        data = mock.Mock(selection_id=42)
        table = mock.Mock(record=lambda n: n)
        return ListForm.Selection(form=None, data=data, grid=grid, table=table)

    def test_len(self, selection):
        assert len(selection) == 3

    def test_iterates_all_rows(self, selection):
        assert list(selection) == [0, 1, 2]

    def test_processed_increments(self, selection):
        assert selection.processed == 0
        for n, row in enumerate(selection, 1):
            assert selection.processed == n

    def test_processed_resets_on_new_iteration(self, selection):
        list(selection)
        assert selection.processed == 3
        list(selection)
        assert selection.processed == 3

    def test_not_invalidated_on_normal_iteration(self, selection):
        list(selection)
        assert not selection.invalidated

    def test_invalidated_on_cursor_change(self, selection, monkeypatch):
        # Simulate cursor ID changing mid-iteration (e.g. DB reconnect).
        try:
            import unittest.mock as mock
        except ImportError:
            import mock
        import pytis.form.list as list_module
        monkeypatch.setattr(list_module, 'app', mock.Mock())
        it = iter(selection)
        next(it)
        selection._data.selection_id = 99  # cursor changed
        with pytest.raises(StopIteration):
            next(it)
        assert selection.invalidated
        assert selection.processed == 1

    def test_copy_does_not_exhaust_original(self, selection):
        # Regression: copy.copy() shared the iterator so the pre-scan consumed it.
        for row in copy.copy(selection):
            break  # pre-scan, exits early
        assert list(selection) == [0, 1, 2]

    def test_each_for_loop_starts_fresh(self, selection):
        assert list(selection) == [0, 1, 2]
        assert list(selection) == [0, 1, 2]


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
    # This is here only in order to allow testing query fields API in TestApp
    query_fields = pp.QueryFields((
        pp.Field('min', "Min", type=pytis.data.Integer(not_null=True), default=10),
        pp.Field('max', "Max", type=pytis.data.Integer(not_null=True), default=50),
    ), autoinit=True)


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
            pp.SharedParams('user', 'UserParams',
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
    try:
        for part in init_sql:
            execute(part)
        yield
    finally:
        for part in teardown_sql:
            execute(part)


@pytest.fixture(scope='class')
def initconfig(dbconnection):
    pytis.config.resolver = pytis.util.Resolver(search=('pytis.form.test', 'pytis.form',
                                                        'pytis.defs', 'pytis.demo'))
    pytis.config.dbconnection = dbconnection


class _TestApplication(object):
    """Minimal stub exercising Application file I/O methods without wx.

    Copies _api_attributes and _ExposedFileWrapper as class attributes so that
    pytis.api.app.init() accepts the instance.  All other method lookups are
    delegated to Application via __getattr__, which binds the method to this
    instance at call time.  This lets us exercise the real Application
    implementation without inheriting wx.App (which would need a display).
    """
    _api_attributes = _pfa.Application._api_attributes
    _ExposedFileWrapper = _pfa.Application._ExposedFileWrapper

    def __init__(self):
        self._recent_directories = {}

    def __getattr__(self, name):
        method = getattr(_pfa.Application, name, None)
        if callable(method):
            return functools.partial(getattr(method, '__func__', method), self)
        raise AttributeError(name)

    def client_mode(self):
        raise NotImplementedError("Patch via monkeypatch in each test")


def _start_rpyc_server():
    """Start an in-process RPyC server and return (server, connection)."""
    port = _find_free_port()
    server = ThreadedServer(
        _TestRPyCService,
        port=port,
        protocol_config={'allow_all_attrs': True, 'allow_public_attrs': True},
    )
    t = threading.Thread(target=server.start)
    t.daemon = True
    t.start()
    time.sleep(0.1)
    connection = rpyc.connect(
        'localhost', port,
        config={'allow_all_attrs': True, 'allow_public_attrs': True},
    )
    return server, connection


@pytest.fixture
def pytis_app():
    """Register _TestApplication with pytis.api.app for the duration of the test."""
    instance = _TestApplication()
    app.init(instance)
    yield instance
    app.release()


@pytest.fixture(params=['local', 'rpyc'])
def app_mode(request, pytis_app, monkeypatch):
    """Parametrized fixture exercising local and RPyC remote file I/O paths."""
    if request.param == 'local':
        monkeypatch.setattr(pytis_app, 'client_mode', lambda: 'local')
        yield 'local'
        return

    server, connection = _start_rpyc_server()
    old_connection = RPCInfo.connection
    RPCInfo.connection = connection
    monkeypatch.setattr(pytis_app, 'client_mode', lambda: 'remote')
    yield 'rpyc'

    RPCInfo.connection.close()
    RPCInfo.connection = old_connection
    server.close()


@pytest.fixture
def rpyc_app(pytis_app, monkeypatch):
    """RPyC-only mode fixture for tests that simulate the remote file dialog."""
    server, connection = _start_rpyc_server()
    old_connection = RPCInfo.connection
    RPCInfo.connection = connection
    monkeypatch.setattr(pytis_app, 'client_mode', lambda: 'remote')
    yield pytis_app

    MockClientUIBackend._selected_file = None
    RPCInfo.connection.close()
    RPCInfo.connection = old_connection
    server.close()


class TestAppFileIO:
    """Tests for pytis.api.app file I/O through local and RPyC remote paths."""

    def test_write_and_read_binary(self, app_mode):
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            app.write_file(b'\x00\xff\xaa', fname, mode='wb')
            with app.open_file(fname, mode='rb') as f:
                assert f.read() == b'\x00\xff\xaa'
        finally:
            os.remove(fname)

    def test_write_and_read_text(self, app_mode):
        """Text written via write_file must survive a round-trip through open_file."""
        fd, fname = tempfile.mkstemp(suffix='.txt')
        os.close(fd)
        try:
            app.write_file(u"Žluťoučký kůň\n", fname, mode='w')
            with app.open_file(fname, mode='r') as f:
                assert f.read() == u"Žluťoučký kůň\n"
        finally:
            os.remove(fname)

    def test_open_file_default_encoding_is_utf8(self, app_mode):
        """open_file(mode='r') must default to UTF-8, not the system locale."""
        fd, fname = tempfile.mkstemp(suffix='.txt')
        os.close(fd)
        try:
            with io.open(fname, 'wb') as f:
                f.write(u"Žluťoučký kůň".encode('utf-8'))
            with app.open_file(fname, mode='r') as f:
                assert f.read() == u"Žluťoučký kůň"
        finally:
            os.remove(fname)

    def test_file_name_attribute(self, app_mode):
        """The file object returned by open_file must expose the path as .name."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            app.write_file(b'test', fname, mode='wb')
            with app.open_file(fname, mode='rb') as f:
                assert f.name == fname
        finally:
            os.remove(fname)

    def test_context_manager_closes_file(self, app_mode):
        """File objects returned by open_file must support the context manager protocol."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            app.write_file(b'abc', fname, mode='wb')
            with app.open_file(fname, mode='rb') as f:
                data = f.read()
            assert data == b'abc'
        finally:
            os.remove(fname)

    def test_iterator_over_text_lines(self, app_mode):
        """File objects returned by open_file must support iteration over lines."""
        fd, fname = tempfile.mkstemp(suffix='.txt')
        os.close(fd)
        try:
            app.write_file(u"line1\nline2\n", fname, mode='w')
            with app.open_file(fname, mode='r') as f:
                lines = list(f)
            assert lines == [u"line1\n", u"line2\n"]
        finally:
            os.remove(fname)

    def test_seek_and_read(self, app_mode):
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            app.write_file(b'abcdef', fname, mode='wb')
            with app.open_file(fname, mode='rb') as f:
                assert f.read(3) == b'abc'
                f.seek(0)
                assert f.read(1) == b'a'
                assert f.read(2) == b'bc'
        finally:
            os.remove(fname)

    @pytest.mark.skipif(sys.version_info[0] < 3, reason="Python 3 only")
    def test_bytes_in_text_mode_raises_type_error(self, app_mode):
        """Writing bytes to a text-mode file must raise TypeError in Python 3."""
        fd, fname = tempfile.mkstemp(suffix='.txt')
        os.close(fd)
        try:
            with pytest.raises(TypeError):
                app.write_file(b'\x00\xff', fname, mode='w')
        finally:
            os.remove(fname)

    @pytest.mark.skipif(sys.version_info[0] < 3, reason="Python 3 only")
    def test_str_in_binary_mode_raises_type_error(self, app_mode):
        """Writing str to a binary-mode file must raise TypeError in Python 3."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            with pytest.raises(TypeError):
                app.write_file(u"text", fname, mode='wb')
        finally:
            os.remove(fname)


class TestAppWriteSelectedFile:
    """Tests for app.write_selected_file() through the RPyC remote path.

    MockClientUIBackend._selected_file stands in for the GUI file dialog:
    setting it to a path before the call simulates the user picking that file,
    leaving it None simulates the user cancelling the dialog.
    """

    def test_write_and_read_text(self, rpyc_app):
        fd, fname = tempfile.mkstemp(suffix='.txt')
        os.close(fd)
        try:
            MockClientUIBackend._selected_file = fname
            result = app.write_selected_file(u"Žluťoučký kůň\n", 'test.txt', mode='w')
            assert result == fname
            with app.open_file(fname, mode='r') as f:
                assert f.read() == u"Žluťoučký kůň\n"
        finally:
            os.remove(fname)

    def test_write_and_read_binary(self, rpyc_app):
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            MockClientUIBackend._selected_file = fname
            result = app.write_selected_file(b'\x00\xff\xaa', 'test.bin', mode='wb')
            assert result == fname
            with app.open_file(fname, mode='rb') as f:
                assert f.read() == b'\x00\xff\xaa'
        finally:
            os.remove(fname)

    def test_cancelled_dialog_returns_none(self, rpyc_app):
        """When the user cancels the dialog (select_file returns None), write_selected_file
        must return None without raising."""
        MockClientUIBackend._selected_file = None
        result = app.write_selected_file(b'data', 'test.bin', mode='wb')
        assert result is None

    @pytest.mark.skipif(sys.version_info[0] < 3, reason="Python 3 only")
    def test_str_in_binary_mode_raises(self, rpyc_app):
        """Writing str to a binary-mode selected file must raise an exception in Python 3."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            MockClientUIBackend._selected_file = fname
            with pytest.raises(Exception):
                app.write_selected_file(u"text", 'test.bin', mode='wb')
        finally:
            os.remove(fname)

    def test_invalid_type_raises(self, rpyc_app):
        """Writing a non-bytes, non-str value must raise an exception."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            MockClientUIBackend._selected_file = fname
            with pytest.raises(Exception):
                app.write_selected_file(8, 'test.bin', mode='wb')
        finally:
            os.remove(fname)


class TestAppDB:
    """DB-dependent tests for app.param, pd.dbfunction and pytis.util.data_object.

    These work without a wx application: BaseApplication is auto-created when
    app.param is first accessed, and DB access does not need a display.
    """

    def test_shared_params(self, initconfig, initdb):
        assert app.param.user.name == 'Test User'
        assert app.param.user.number == 64
        assert app.param.user.enabled is True
        with pytest.raises(AttributeError):
            app.param.user.xval
        with pytest.raises(AttributeError):
            app.param.user.xyz = 1

    def test_dbfunction(self, initconfig, initdb):
        assert pd.dbfunction(square, 5) == 25
        assert pd.dbfunction('square', 4) == 16
        assert pd.dbfunction('reverse', 'Hello World') == 'dlroW olleH'

    def test_data_object_by_specification_name(self, initconfig, initdb):
        for spec_name, key in (
                ('UserParams', 'id'),
                ('misc.Products', 'product_id'),
                ('misc.Tree', 'id'),
        ):
            data = pytis.util.data_object(spec_name)
            assert isinstance(data, pd.DBData)
            assert [c.id() for c in data.key()] == [key]

    def test_data_object_by_data_specification(self, initconfig, initdb):
        import pytis.dbdefs.demo
        for data_spec, key in (
                (user_params, 'id'),
                (pytis.dbdefs.demo.Products, 'product_id'),
                (pytis.dbdefs.demo.Tree, 'id'),
        ):
            data = pytis.util.data_object(data_spec)
            assert isinstance(data, pd.DBData)
            assert [c.id() for c in data.key()] == [key]

    def test_data_object_columns(self, initconfig, initdb):
        import pytis.dbdefs.demo
        data1 = pytis.util.data_object('misc.Tree')
        data2 = pytis.util.data_object(pytis.dbdefs.demo.Tree)
        for c1, c2 in zip(data1.columns(), data2.columns()):
            assert isinstance(c1.type(), type(c2.type()))


