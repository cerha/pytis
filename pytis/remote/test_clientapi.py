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

"""Unit tests for `pytis.remote.clientapi`, `client`, and related classes.

All tests run without a display, without SSH and without X2Go.  No RPyC or
pytis2go installation is required.

**Test classes**

- `TestFileWrapper` — exercises `FileWrapper` directly: read, write, seek,
  encryption hooks, context manager.  Uses real files; no network.

- `TestPytisClientAPIService` — exercises `PytisClientAPIService` via a
  `MockClientUIBackend` that avoids any GUI dialogs.  No network.

- `TestFileProxy` — exercises `FileProxy` (from `pytis.remote.client`) using
  a `MockClient` that records `request` calls.  Verifies base64
  encoding/decoding for binary modes, routing of each file operation to the
  correct protocol action, close idempotency, and context manager behaviour.

- `TestServiceClientFileMethods` — exercises `ServiceClient` file-factory
  methods (`open_file`, `open_selected_file`, `make_selected_file`,
  `make_temporary_file`) using the same `MockClient`.  Verifies that each
  method returns a properly initialised `FileProxy` and that cancel responses
  (`None`) propagate correctly.

For live end-to-end tests inside an X2Go session see `test.py`.  For
interactive UI-backend tests see `test_ui_backends.py`.

"""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

import base64
import io
import json
import os
import pytest

from pytis.remote import clientapi
from pytis.remote.client import FileProxy, ServiceClient


# ---------------------------------------------------------------------------
# Mock UIBackend — avoids any GUI toolkit or filesystem dialogs
# ---------------------------------------------------------------------------

class MockClientUIBackend:
    """Scriptable stand-in for `ClientUIBackend`.

    All dialog methods return pre-configured values stored as instance
    attributes.  Tests set them directly before exercising the service method
    under test.
    """

    def __init__(self):
        self._clipboard = ''
        self._select_file_result = None
        self._select_directory_result = '/mock/dir'

    def init(self):
        pass

    def name(self):
        return 'MockUIBackend'

    def get_clipboard_text(self):
        return self._clipboard

    def set_clipboard_text(self, text):
        self._clipboard = text

    def enter_text(self, **kwargs):
        return None

    def select_file(self, **kwargs):
        return self._select_file_result

    def select_directory(self, **kwargs):
        return self._select_directory_result

    def select_option(self, **kwargs):
        return None


class MockPytisClientAPIService(clientapi.PytisClientAPIService):
    """PytisClientAPIService with UIBackend replaced by `MockClientUIBackend`."""

    def _create_client_instance(self):
        return MockClientUIBackend()


# ---------------------------------------------------------------------------
# FileWrapper — basic read/write/seek/close operations
# ---------------------------------------------------------------------------

class TestFileWrapper:
    """Tests for `FileWrapper` with direct method calls (no remote transport)."""

    def _encrypt(self, f):
        return base64.b64encode(f.read().encode('utf-8'))

    def _decrypt(self, data):
        return base64.b64decode(data).decode('utf-8')

    def _test_read(self, filename, data, mode='r', **kwargs):
        with open(filename, 'w') as f:
            f.write(data)
        wrapper = clientapi.FileWrapper(filename, mode=mode, **kwargs)
        data2 = wrapper.read()
        if 'encrypt' in kwargs:
            data2 = self._decrypt(data2)
        wrapper.close()
        assert data2 == data

    def _test_write(self, filename, data, mode='w', **kwargs):
        wrapper = clientapi.FileWrapper(filename, mode=mode, **kwargs)
        if 'decrypt' in kwargs:
            data_to_write = self._encrypt(io.StringIO(data))
        else:
            data_to_write = data
        wrapper.write(data_to_write)
        wrapper.flush()
        wrapper.close()
        with open(filename) as f:
            data2 = f.read()
        assert data2 == data

    def test_read(self, tmp_path):
        self._test_read(str(tmp_path / 'test.txt'), "foo('bar')='baz'")

    def test_read_handle(self, tmp_path):
        filename = str(tmp_path / 'handle.txt')
        fd = os.open(filename, os.O_RDWR | os.O_CREAT, 0o600)
        # FileWrapper takes ownership of fd; no close needed after.
        self._test_read(filename, "foo('bar')='baz'", handle=fd)

    def test_encryption(self, tmp_path):
        self._test_read(str(tmp_path / 'enc.txt'), "foo('bar')='baz'",
                        encrypt=self._encrypt)

    def test_write(self, tmp_path):
        self._test_write(str(tmp_path / 'test.txt'), "foo('bar')='baz'")

    def test_write_handle(self, tmp_path):
        filename = str(tmp_path / 'handle.txt')
        fd = os.open(filename, os.O_RDWR | os.O_CREAT, 0o600)
        self._test_write(filename, "foo('bar')='baz'", handle=fd)

    def test_decryption(self, tmp_path):
        self._test_write(str(tmp_path / 'dec.txt'), "foo('bar')='baz'",
                         decrypt=self._decrypt)

    def test_context_manager(self, tmp_path):
        f = tmp_path / 'ctx.bin'
        with clientapi.FileWrapper(str(f), mode='wb') as w:
            w.write(b'context manager')
        assert f.read_bytes() == b'context manager'

    def test_readline_binary(self, tmp_path):
        f = tmp_path / 'lines.bin'
        f.write_bytes(b'line1\nline2\n')
        wrapper = clientapi.FileWrapper(str(f), mode='rb')
        assert wrapper.readline() == b'line1\n'
        assert wrapper.readline() == b'line2\n'
        wrapper.close()

    def test_readlines_binary(self, tmp_path):
        f = tmp_path / 'lines.bin'
        f.write_bytes(b'a\nb\n')
        wrapper = clientapi.FileWrapper(str(f), mode='rb')
        assert wrapper.readlines() == [b'a\n', b'b\n']
        wrapper.close()

    def test_seek(self, tmp_path):
        f = tmp_path / 'seek.bin'
        f.write_bytes(b'abcdefgh')
        wrapper = clientapi.FileWrapper(str(f), mode='rb')
        wrapper.seek(4)
        assert wrapper.read() == b'efgh'
        wrapper.close()

    def test_name(self, tmp_path):
        f = tmp_path / 'named.txt'
        f.write_text('x')
        wrapper = clientapi.FileWrapper(str(f), mode='r')
        assert wrapper.name == str(f)
        wrapper.close()


# ---------------------------------------------------------------------------
# PytisClientAPIService — protocol-neutral API with mock UIBackend
# ---------------------------------------------------------------------------

class TestPytisClientAPIService:
    """Tests for `PytisClientAPIService` using `MockClientUIBackend`.

    All protocol and transport are bypassed — methods are called directly.
    UIBackend is mocked so no GUI toolkit or display is needed.
    """

    @pytest.fixture
    def svc(self):
        return MockPytisClientAPIService()

    def test_client_info(self, svc):
        info = json.loads(svc.exposed_client_info())
        assert info['os_name'] is not None
        assert 'python_version' in info
        assert 'rpyc_version' in info
        assert info['backend_name'] == 'MockUIBackend'

    def test_clipboard(self, svc):
        svc.exposed_set_clipboard_text('hello world')
        assert svc.exposed_get_clipboard_text() == 'hello world'

    def test_clipboard_unicode(self, svc):
        for text in ('foo', 'Žluťoučký kůň!', '頁設是'):
            svc.exposed_set_clipboard_text(text)
            assert svc.exposed_get_clipboard_text() == text

    def test_open_file_binary_read(self, svc, tmp_path):
        f = tmp_path / 'test.bin'
        f.write_bytes(b'\x00\x01\x02\xff')
        wrapper = svc.exposed_open_file(str(f), 'rb')
        assert wrapper.read() == b'\x00\x01\x02\xff'
        wrapper.close()

    def test_open_file_text_read(self, svc, tmp_path):
        f = tmp_path / 'test.txt'
        f.write_text('Žluťoučký kůň\n', encoding='utf-8')
        wrapper = svc.exposed_open_file(str(f), 'r', encoding='utf-8')
        assert wrapper.read() == 'Žluťoučký kůň\n'
        wrapper.close()

    def test_open_file_write(self, svc, tmp_path):
        f = tmp_path / 'write.bin'
        wrapper = svc.exposed_open_file(str(f), 'wb')
        wrapper.write(b'written')
        wrapper.close()
        assert f.read_bytes() == b'written'

    def test_open_file_partial_read(self, svc, tmp_path):
        f = tmp_path / 'partial.bin'
        f.write_bytes(b'open_file test \xc4\x8d\xc5\x99')
        wrapper = svc.exposed_open_file(str(f), 'rb')
        assert wrapper.read(4) == b'open'
        assert wrapper.read() == b'_file test \xc4\x8d\xc5\x99'
        wrapper.close()

    def test_open_file_seek(self, svc, tmp_path):
        f = tmp_path / 'seek.bin'
        f.write_bytes(b'abcdef')
        wrapper = svc.exposed_open_file(str(f), 'rb')
        wrapper.seek(3)
        assert wrapper.read() == b'def'
        wrapper.close()

    def test_open_file_readline(self, svc, tmp_path):
        f = tmp_path / 'lines.txt'
        f.write_text('Žluťoučký\nkůň\n', encoding='utf-8')
        wrapper = svc.exposed_open_file(str(f), 'r', encoding='utf-8')
        assert wrapper.readline() == 'Žluťoučký\n'
        wrapper.seek(0)
        assert tuple(wrapper.readlines()) == ('Žluťoučký\n', 'kůň\n')
        wrapper.close()

    def test_open_file_binary_after_text_write(self, svc, tmp_path):
        """After writing a text file, open in binary mode reads raw bytes."""
        f = tmp_path / 'encoded.txt'
        with svc.exposed_open_file(str(f), 'w', encoding='utf-8') as w:
            w.write('Žluťoučký\nkůň\n')
        with svc.exposed_open_file(str(f), 'rb') as r:
            assert r.read() == b'\xc5\xbdlu\xc5\xa5ou\xc4\x8dk\xc3\xbd\nk\xc5\xaf\xc5\x88\n'

    def test_make_temporary_file_basic(self, svc):
        """make_temporary_file creates a writable FileWrapper."""
        wrapper = svc.exposed_make_temporary_file(suffix='.bin', mode='wb')
        assert wrapper is not None
        path = wrapper.name
        assert path.endswith('.bin')
        try:
            wrapper.write(b'temp content')
            wrapper.close()
            assert open(path, 'rb').read() == b'temp content'
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_make_temporary_file_explicit_close(self, svc):
        """Explicit write() and close() must work without raising.

        Regression: over RPyC, explicit `.close()` on the `FileWrapper` returned
        by `make_temporary_file` used to fail with
        ``AttributeError("cannot access 'close'")`` because the base class
        returns a plain `FileWrapper` with no ``exposed_*`` aliases.  The RPyC
        adapter overrides this method to return `ExposedFileWrapper`.
        """
        wrapper = svc.exposed_make_temporary_file(suffix='.bin', mode='wb')
        path = wrapper.name
        try:
            wrapper.write(b'explicit close test\n')
            wrapper.close()
            assert open(path, 'rb').read() == b'explicit close test\n'
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_make_temporary_file_context_manager(self, svc):
        """make_temporary_file works with the context manager protocol."""
        with svc.exposed_make_temporary_file(suffix='.txt', mode='w',
                                             encoding='utf-8') as f:
            f.write('Žluťoučký\nkůň\n')
            path = f.name
        try:
            assert io.open(path, encoding='utf-8').read() == 'Žluťoučký\nkůň\n'
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_make_temporary_file_encoded_roundtrip(self, svc):
        """make_temporary_file with encoding writes correct UTF-8 bytes."""
        with svc.exposed_make_temporary_file(suffix='.txt', mode='w',
                                             encoding='utf-8') as f:
            f.write('Žluťoučký\nkůň\n')
            path = f.name
        try:
            assert open(path, 'rb').read() == (
                b'\xc5\xbdlu\xc5\xa5ou\xc4\x8dk\xc3\xbd\nk\xc5\xaf\xc5\x88\n'
            )
        finally:
            if os.path.exists(path):
                os.unlink(path)

    def test_open_selected_file_cancel(self, svc):
        """open_selected_file returns None when the user cancels the dialog."""
        svc._client._select_file_result = None
        assert svc.exposed_open_selected_file() is None

    def test_open_selected_file_returns_wrapper(self, svc, tmp_path):
        f = tmp_path / 'selected.bin'
        f.write_bytes(b'selected content')
        svc._client._select_file_result = str(f)
        wrapper = svc.exposed_open_selected_file()
        assert wrapper is not None
        assert wrapper.read() == b'selected content'
        wrapper.close()

    def test_make_selected_file_cancel(self, svc):
        """make_selected_file returns None when the user cancels the dialog."""
        svc._client._select_file_result = None
        assert svc.exposed_make_selected_file() is None

    def test_make_selected_file_returns_wrapper(self, svc, tmp_path):
        f = tmp_path / 'save.bin'
        svc._client._select_file_result = str(f)
        wrapper = svc.exposed_make_selected_file(mode='wb')
        assert wrapper is not None
        wrapper.write(b'saved content')
        wrapper.close()
        assert f.read_bytes() == b'saved content'

    def test_select_directory(self, svc):
        svc._client._select_directory_result = '/mock/chosen/dir'
        assert svc.exposed_select_directory() == '/mock/chosen/dir'

    def test_select_file(self, svc):
        svc._client._select_file_result = '/mock/chosen/file.pdf'
        assert svc.exposed_select_file() == '/mock/chosen/file.pdf'

    def test_select_file_cancel(self, svc):
        svc._client._select_file_result = None
        assert svc.exposed_select_file() is None


# ---------------------------------------------------------------------------
# FileProxy — client-side encoding/decoding and protocol correctness
# ---------------------------------------------------------------------------

class MockClient:
    """Records `request()` calls and returns pre-configured results."""

    def __init__(self):
        self.calls = []
        self._results = {}

    def set_result(self, action, result):
        self._results[action] = result

    def request(self, action, **kwargs):
        self.calls.append((action, kwargs))
        return self._results.get(action)


class TestFileProxy:
    """Unit tests for `FileProxy` — no server, no network, no pytis2go."""

    def _make(self, mode='rb', handle=1, path='/tmp/f'):
        client = MockClient()
        fp = FileProxy(client, handle, path, mode)
        return fp, client

    def test_read_binary_decodes_base64(self):
        fp, mock = self._make(mode='rb')
        mock.set_result('file_read', base64.b64encode(b'hello').decode('ascii'))
        assert fp.read() == b'hello'
        assert mock.calls[-1] == ('file_read', {'handle': 1, 'size': -1})

    def test_read_partial_passes_size(self):
        fp, mock = self._make(mode='rb')
        mock.set_result('file_read', base64.b64encode(b'ab').decode('ascii'))
        assert fp.read(2) == b'ab'
        assert mock.calls[-1] == ('file_read', {'handle': 1, 'size': 2})

    def test_read_text_returns_as_is(self):
        fp, mock = self._make(mode='r')
        mock.set_result('file_read', 'hello text')
        assert fp.read() == 'hello text'

    def test_readline_binary_decodes_base64(self):
        fp, mock = self._make(mode='rb')
        mock.set_result('file_readline', base64.b64encode(b'line\n').decode('ascii'))
        assert fp.readline() == b'line\n'
        assert mock.calls[-1] == ('file_readline', {'handle': 1})

    def test_readline_text_returns_as_is(self):
        fp, mock = self._make(mode='r')
        mock.set_result('file_readline', 'line\n')
        assert fp.readline() == 'line\n'

    def test_readlines_binary_decodes_each(self):
        fp, mock = self._make(mode='rb')
        mock.set_result('file_readlines', [
            base64.b64encode(b'a\n').decode('ascii'),
            base64.b64encode(b'b\n').decode('ascii'),
        ])
        assert fp.readlines() == [b'a\n', b'b\n']

    def test_write_binary_encodes_base64(self):
        fp, mock = self._make(mode='wb')
        fp.write(b'\x00\xff')
        action, kwargs = mock.calls[-1]
        assert action == 'file_write'
        assert base64.b64decode(kwargs['data']) == b'\x00\xff'

    def test_write_text_passes_as_is(self):
        fp, mock = self._make(mode='w')
        fp.write('hello')
        assert mock.calls[-1] == ('file_write', {'handle': 1, 'data': 'hello'})

    def test_seek(self):
        fp, mock = self._make()
        fp.seek(4)
        assert mock.calls[-1] == ('file_seek', {'handle': 1, 'pos': 4, 'whence': 0})

    def test_flush(self):
        fp, mock = self._make()
        fp.flush()
        assert mock.calls[-1] == ('file_flush', {'handle': 1})

    def test_close_sends_request(self):
        fp, mock = self._make()
        fp.close()
        assert mock.calls[-1] == ('file_close', {'handle': 1})

    def test_close_is_idempotent(self):
        fp, mock = self._make()
        fp.close()
        fp.close()
        assert sum(1 for a, _ in mock.calls if a == 'file_close') == 1

    def test_context_manager(self):
        fp, mock = self._make()
        with fp:
            pass
        assert mock.calls[-1][0] == 'file_close'

    def test_name_property(self):
        fp, mock = self._make(path='/tmp/named.bin')
        mock.set_result('file_name', '/tmp/named.bin')
        assert fp.name == '/tmp/named.bin'

    def test_mode_property(self):
        fp, mock = self._make(mode='rb')
        assert fp.mode == 'rb'


class TestServiceClientFileMethods:
    """Unit tests for `ServiceClient` file-factory methods."""

    def _make_client(self, handle_result=None):
        client = ServiceClient.__new__(ServiceClient)
        mock = MockClient()
        if handle_result is not None:
            mock.set_result(list(handle_result.keys())[0],
                            list(handle_result.values())[0])
        client.request = mock.request
        return client, mock

    def test_open_file_returns_file_proxy(self):
        client, mock = self._make_client()
        mock.set_result('open_file', {'handle': 7, 'path': '/tmp/x.bin'})
        fp = client.open_file('/tmp/x.bin', 'rb')
        assert isinstance(fp, FileProxy)
        assert fp._handle == 7
        assert fp.mode == 'rb'

    def test_open_selected_file_returns_proxy(self):
        client, mock = self._make_client()
        mock.set_result('open_selected_file', {'handle': 3, 'path': '/tmp/sel.bin'})
        fp = client.open_selected_file()
        assert isinstance(fp, FileProxy)
        assert fp._handle == 3

    def test_open_selected_file_cancel_returns_none(self):
        client, mock = self._make_client()
        mock.set_result('open_selected_file', None)
        assert client.open_selected_file() is None

    def test_make_selected_file_returns_proxy(self):
        client, mock = self._make_client()
        mock.set_result('make_selected_file', {'handle': 5, 'path': '/tmp/save.bin'})
        fp = client.make_selected_file(mode='wb')
        assert isinstance(fp, FileProxy)
        assert fp.mode == 'wb'

    def test_make_selected_file_cancel_returns_none(self):
        client, mock = self._make_client()
        mock.set_result('make_selected_file', None)
        assert client.make_selected_file() is None

    def test_make_temporary_file_returns_proxy(self):
        client, mock = self._make_client()
        mock.set_result('make_temporary_file', {'handle': 9, 'path': '/tmp/pytistmpXXX.bin'})
        fp = client.make_temporary_file(suffix='.bin', mode='wb')
        assert isinstance(fp, FileProxy)
        assert fp._path.endswith('.bin')


