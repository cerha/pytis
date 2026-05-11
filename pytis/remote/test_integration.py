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

"""Integration tests for pytis.remote using an in-process simulated connection.

The RPyC transport fixture starts an in-process ThreadedServer and sets
RPCInfo.connection directly, bypassing X2Go/Pytis2Go infrastructure.
Tests run as if the client service were running on a real remote machine.

The JSON transport fixture is guarded by a try/import and will be activated
automatically once the rpc-json branch is merged into master.  Until then,
running with transport='json' is skipped.
"""

from __future__ import unicode_literals
from __future__ import print_function

import os
import socket
import threading
import time

import pytest
import rpyc
from rpyc.utils.server import ThreadedServer

import pytis.remote
from pytis.remote.clientapi import PytisClientAPIService
from pytis.remote.remote import RPCInfo, _request

# JSON transport is only available after rpc-json branch is merged into master.
# Once pytis.remote.client.ServiceClient is importable, JSON tests run automatically.
try:
    from pytis.remote.client import ServiceClient  # noqa: F401
    _json_transport_available = True
except ImportError:
    _json_transport_available = False

_transports = ['rpyc'] + (['json'] if _json_transport_available else [])


class MockClientUIBackend(object):
    """Mock UI backend for testing — no actual GUI required."""

    _selected_file = None  # Set per-test to simulate file dialog selection.

    def __init__(self):
        self._clipboard = None

    def init(self):
        pass

    def name(self):
        return 'MockClientUIBackend'

    def get_clipboard_text(self):
        return self._clipboard

    def set_clipboard_text(self, text):
        self._clipboard = text

    def select_file(self, **kwargs):
        return type(self)._selected_file


class _TestRPyCService(PytisClientAPIService, rpyc.Service):
    """RPyC service using MockClientUIBackend for headless testing.

    The redundant rpyc.Service base is harmless in master (where PytisClientAPIService
    already inherits it) and required in rpc-json branch (where it does not).
    """

    def exposed_echo(self, text=''):
        return text

    def exposed_x2goclient_version(self):
        return 'test-rpyc-1.0'

    def exposed_extensions(self):
        return ['PytisClientAPIService']

    def _create_client_instance(self):
        return MockClientUIBackend()


def _find_free_port():
    # TODO NOPY2: with socket.socket() as s: ... (no s.close() needed)
    s = socket.socket()
    s.bind(('', 0))
    port = s.getsockname()[1]
    s.close()
    return port


@pytest.fixture(params=_transports)
def remote_service(request):
    """Fixture providing a live in-process simulated pytis.remote connection.

    For 'rpyc': starts a ThreadedServer and connects RPCInfo.connection to it.
    For 'json': skipped until rpc-json branch is merged into master.
    """
    if request.param == 'json':
        # NOTE: Implement _json_service_fixture() after rpc-json branch is merged.
        # The JSON protocol server needs ServiceClient and related classes from that branch.
        pytest.skip("JSON transport requires rpc-json branch to be merged")

    port = _find_free_port()
    server = ThreadedServer(
        _TestRPyCService,
        port=port,
        protocol_config={'allow_all_attrs': True, 'allow_public_attrs': True},
    )
    t = threading.Thread(target=server.start)
    t.daemon = True
    t.start()
    time.sleep(0.1)  # Let the server start accepting connections.

    old_connection = RPCInfo.connection
    RPCInfo.connection = rpyc.connect(
        'localhost', port,
        config={'allow_all_attrs': True, 'allow_public_attrs': True},
    )
    yield request.param

    RPCInfo.connection.close()
    RPCInfo.connection = old_connection
    server.close()


class TestRemoteLocal:
    """Integration tests for pytis.remote through an in-process simulated connection."""

    def test_echo(self, remote_service):
        assert _request('echo', 'hello') == 'hello'

    def test_binary_file_write_read(self, remote_service):
        with pytis.remote.make_temporary_file(suffix='.bin', mode='wb') as f:
            f.write(b'\x00\xff\xaa\xbb')
            fname = f.name
        with pytis.remote.open_file(fname, mode='rb') as f:
            assert f.read() == b'\x00\xff\xaa\xbb'
        os.remove(fname)

    def test_text_file_write_read(self, remote_service):
        with pytis.remote.make_temporary_file(suffix='.txt', mode='w', encoding='utf-8') as f:
            f.write("Žluťoučk\xfd kůň\n")
            fname = f.name
        with pytis.remote.open_file(fname, mode='r', encoding='utf-8') as f:
            assert f.read() == "Žluťoučk\xfd kůň\n"
        os.remove(fname)

    def test_binary_file_seek_and_read(self, remote_service):
        with pytis.remote.make_temporary_file(suffix='.bin', mode='wb') as f:
            f.write(b'abcdef')
            fname = f.name
        with pytis.remote.open_file(fname, mode='rb') as f:
            assert f.read(3) == b'abc'
            f.seek(0)
            assert f.read(1) == b'a'
            assert f.read(2) == b'bc'
        os.remove(fname)

    def test_text_file_readline(self, remote_service):
        with pytis.remote.make_temporary_file(suffix='.txt', mode='w', encoding='utf-8') as f:
            f.write("line1\nline2\n")
            fname = f.name
        with pytis.remote.open_file(fname, mode='r', encoding='utf-8') as f:
            assert f.readline() == "line1\n"
            assert f.readline() == "line2\n"
        os.remove(fname)

    def test_text_file_readlines(self, remote_service):
        with pytis.remote.make_temporary_file(suffix='.txt', mode='w', encoding='utf-8') as f:
            f.write("line1\nline2\n")
            fname = f.name
        with pytis.remote.open_file(fname, mode='r', encoding='utf-8') as f:
            assert tuple(f.readlines()) == ("line1\n", "line2\n")
        os.remove(fname)

    def test_clipboard(self, remote_service):
        pytis.remote.set_clipboard_text('test clipboard')
        assert pytis.remote.get_clipboard_text() == 'test clipboard'
        pytis.remote.set_clipboard_text('updated')
        assert pytis.remote.get_clipboard_text() == 'updated'

    def test_binary_type_error_in_text_mode(self, remote_service):
        """Writing bytes to a text-mode file must raise TypeError (Python 3 only)."""
        if not hasattr(bytes, '__iter__') or str is bytes:
            pytest.skip("Python 2 text/binary mode distinction does not apply")
        with pytis.remote.make_temporary_file(suffix='.txt', mode='w') as f:
            with pytest.raises((TypeError, Exception)):
                f.write(b'\x00\xff')
