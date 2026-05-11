# -*- coding: utf-8 -*-

# Copyright (C) 2026 Tomáš Cerha <t.cerha@gmail.com>
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

"""Integration tests for pytis.api.app file I/O methods.

Tests the wrapping, transforms, and type checks in Application.api_open_file,
api_write_file, api_make_selected_file and related methods
(pytis/form/application.py) without requiring X2Go, wx initialization, or a
display.

The key mechanism: _TestApplication copies _api_attributes and
_ExposedFileWrapper from Application and delegates all other method lookups to
Application methods via __getattr__, binding them to the _TestApplication
instance at call time.  This lets us exercise the real implementation without
inheriting wx.App (which would need a display).

We patch client_mode() to control whether local or remote paths are exercised.

Two parametrized transport modes for TestAppFileIO:
- 'local': client_mode() returns 'local', file I/O goes through io.open().
- 'rpyc':  client_mode() returns 'remote', file I/O goes through RPyC +
           Application._ExposedFileWrapper (text mode emulation layer).

TestAppWriteSelectedFile uses rpyc mode only (the remote path uses a real
file dialog in local mode; MockClientUIBackend._selected_file simulates it
without any display).

TestAppDB tests app.param, pd.dbfunction and pytis.util.data_object through a
live database connection without a wx application.  These correspond to tests
in TestApp (pytis/form/test.py) that are currently gated behind initapp (which
requires DISPLAY).

The JSON transport will be added after the rpc-json branch is merged.

Tests NOT covered here (require wx or interactive GUI):
- test_api_form, test_query_fields_api, test_shared_param_callbacks (wx only)
- test_write_selected_file, test_write_selected_file_type_errors (interactive)
- TestDataTable (grid tests already work headlessly in pytis/form/test.py)
"""

from __future__ import unicode_literals
from __future__ import print_function

import functools
import io
import os
import sys
import tempfile
import threading
import time

import pytest
import rpyc
from rpyc.utils.server import ThreadedServer

import pytis.api
import pytis.data as pd
import pytis.remote
import pytis.util
from pytis.api import app
from pytis.data.test import DBTest, connector, dbconnection, execute
from pytis.remote.remote import RPCInfo
from pytis.remote.test_integration import MockClientUIBackend, _TestRPyCService, _find_free_port

try:
    import pytis.form.application as _pfa
    _app_available = True
except ImportError:
    _app_available = False

pytestmark = pytest.mark.skipif(
    not _app_available,
    reason="pytis.form.application not importable",
)

# Import DB fixtures and spec classes from pytis.form.test so pytest discovers
# them as available fixtures for TestAppDB.  The resolver (set by initconfig)
# searches 'pytis.form.test', so Application and UserParams are found there.
from pytis.form.test import initdb, initconfig, user_params, square  # noqa: F401,E402


class _TestApplication(object):
    """Minimal stub providing only the file I/O methods from Application.

    Copies _api_attributes and _ExposedFileWrapper as class attributes so that
    pytis.api.app.init() accepts the instance.  All other method lookups are
    delegated to Application via __getattr__, which binds the method to this
    instance at call time.  This avoids inheriting wx.App (which would need a
    display) while still exercising the real Application implementation.
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
    pytis.api.app.init(instance)
    yield instance
    pytis.api.app.release()


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
            pytis.api.app.write_file(b'\x00\xff\xaa', fname, mode='wb')
            with pytis.api.app.open_file(fname, mode='rb') as f:
                assert f.read() == b'\x00\xff\xaa'
        finally:
            os.remove(fname)

    def test_write_and_read_text(self, app_mode):
        """Text written via write_file must survive a round-trip through open_file."""
        fd, fname = tempfile.mkstemp(suffix='.txt')
        os.close(fd)
        try:
            pytis.api.app.write_file(u"Žluťoučký kůň\n", fname, mode='w')
            with pytis.api.app.open_file(fname, mode='r') as f:
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
            with pytis.api.app.open_file(fname, mode='r') as f:
                assert f.read() == u"Žluťoučký kůň"
        finally:
            os.remove(fname)

    def test_file_name_attribute(self, app_mode):
        """The file object returned by open_file must expose the path as .name."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            pytis.api.app.write_file(b'test', fname, mode='wb')
            with pytis.api.app.open_file(fname, mode='rb') as f:
                assert f.name == fname
        finally:
            os.remove(fname)

    def test_context_manager_closes_file(self, app_mode):
        """File objects returned by open_file must support the context manager protocol."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            pytis.api.app.write_file(b'abc', fname, mode='wb')
            with pytis.api.app.open_file(fname, mode='rb') as f:
                data = f.read()
            assert data == b'abc'
        finally:
            os.remove(fname)

    def test_iterator_over_text_lines(self, app_mode):
        """File objects returned by open_file must support iteration over lines."""
        fd, fname = tempfile.mkstemp(suffix='.txt')
        os.close(fd)
        try:
            pytis.api.app.write_file(u"line1\nline2\n", fname, mode='w')
            with pytis.api.app.open_file(fname, mode='r') as f:
                lines = list(f)
            assert lines == [u"line1\n", u"line2\n"]
        finally:
            os.remove(fname)

    def test_seek_and_read(self, app_mode):
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            pytis.api.app.write_file(b'abcdef', fname, mode='wb')
            with pytis.api.app.open_file(fname, mode='rb') as f:
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
                pytis.api.app.write_file(b'\x00\xff', fname, mode='w')
        finally:
            os.remove(fname)

    @pytest.mark.skipif(sys.version_info[0] < 3, reason="Python 3 only")
    def test_str_in_binary_mode_raises_type_error(self, app_mode):
        """Writing str to a binary-mode file must raise TypeError in Python 3."""
        fd, fname = tempfile.mkstemp(suffix='.bin')
        os.close(fd)
        try:
            with pytest.raises(TypeError):
                pytis.api.app.write_file(u"text", fname, mode='wb')
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

    These correspond to tests in TestApp (pytis/form/test.py) that are
    currently gated behind initapp (which requires DISPLAY).  They work here
    because BaseApplication is auto-created when app.param is first accessed,
    and no wx application is needed for DB access.

    Not covered (require wx): test_api_form, test_query_fields_api,
    test_shared_param_callbacks.
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
