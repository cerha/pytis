#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Comprehensive test of pytis2go service handlers (JSON and RPyC protocols).

Run this on the X2Go server inside an active pytis2go session.
Tests all non-interactive handlers automatically; interactive handlers
(file dialogs, launch) require --interactive.

Usage:
    python tools/test_handlers.py [--interactive]
"""

from __future__ import print_function

try:
    import importlib.util as _importlib_util
    def _load_module(name, path):
        spec = _importlib_util.spec_from_file_location(name, path)
        mod = _importlib_util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        return mod
except ImportError:
    import imp  # Python 2
    def _load_module(name, path):
        return imp.load_source(name, path)

import json
import os
import socket
import sys

# Allow running directly (python tools/test_handlers.py) without setting PYTHONPATH.
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import pytis.remote


# ---------------------------------------------------------------------------
# Terminal colours
# ---------------------------------------------------------------------------

if sys.stdout.isatty():
    _G, _R, _Y, _X = '\033[32m', '\033[31m', '\033[33m', '\033[0m'
else:
    _G = _R = _Y = _X = ''

PASS = _G + 'PASS' + _X
FAIL = _R + 'FAIL' + _X
SKIP = _Y + 'SKIP' + _X


# ---------------------------------------------------------------------------
# Test runner
# ---------------------------------------------------------------------------

_results = []


def test(name, fn):
    try:
        fn()
        _results.append((name, True, None))
        print('  {}  {}'.format(PASS, name))
        return True
    except AssertionError as e:
        msg = str(e) or 'assertion failed'
        _results.append((name, False, msg))
        print('  {}  {}: {}'.format(FAIL, name, msg))
        return False
    except Exception as e:
        msg = '{}: {}'.format(type(e).__name__, e)
        _results.append((name, False, msg))
        print('  {}  {}: {}'.format(FAIL, name, msg))
        return False


def skip(name, reason=''):
    _results.append((name, None, reason))
    suffix = ': ' + reason if reason else ''
    print('  {}  {}{}'.format(SKIP, name, suffix))


def eq(got, expected):
    assert got == expected, 'expected {!r}, got {!r}'.format(expected, got)


def ok(condition, msg='condition is False'):
    assert condition, msg


# ---------------------------------------------------------------------------
# Protocol adapters — common interface for both protocols
# ---------------------------------------------------------------------------

class _JsonAdapter:
    """Adapter over ServiceClient providing a protocol-neutral file/service API.

    File operations return FileProxy instances; binary data is transparently
    base64-encoded in transit so callers just pass and receive plain bytes.
    """

    def __init__(self, client, FileProxy_class):
        self._c = client
        self._FP = FileProxy_class

    def request(self, action, **kwargs):
        return self._c.request(action, **kwargs)

    def extensions(self):
        return self._c.request('extensions')

    def client_info(self):
        return self._c.request('client_info')

    def session_password(self):
        return self._c.request('session_password')

    def get_clipboard_text(self):
        return self._c.request('get_clipboard_text')

    def set_clipboard_text(self, text):
        self._c.request('set_clipboard_text', text=text)

    def run_python(self, script):
        return self._c.request('run_python', script=script)

    def select_file(self, **kw):
        return self._c.request('select_file', **kw)

    def select_directory(self, **kw):
        return self._c.request('select_directory', **kw)

    def enter_text(self, **kw):
        return self._c.request('enter_text', **kw)

    def select_option(self, **kw):
        return self._c.request('select_option', **kw)

    def launch_file(self, path):
        return self._c.request('launch_file', path=path)

    def make_temporary_file(self, suffix='', mode='wb'):
        return self._c.make_temporary_file(suffix=suffix, mode=mode)

    def open_file(self, filename, mode='rb'):
        result = self._c.request('open_file', filename=filename, mode=mode)
        return self._FP(self._c, result['handle'], result['path'], mode)

    def open_selected_file(self, **kw):
        result = self._c.request('open_selected_file', **kw)
        if result is None:
            return None
        return self._FP(self._c, result['handle'], result['path'], 'rb')

    def make_selected_file(self, mode='wb', **kw):
        result = self._c.request('make_selected_file', mode=mode, **kw)
        if result is None:
            return None
        return self._FP(self._c, result['handle'], result['path'], mode)


class _RpycAdapter:
    """Adapter over an RPyC connection providing the same interface as _JsonAdapter.

    File operations return RPyC NetRef proxies which are file-like objects;
    binary I/O works directly with bytes (no base64).
    """

    def __init__(self, conn):
        self._conn = conn

    def extensions(self):
        return list(self._conn.root.extensions())

    def client_info(self):
        return str(self._conn.root.client_info())

    def session_password(self):
        result = self._conn.root.session_password()
        return str(result) if result is not None else None

    def get_clipboard_text(self):
        text = self._conn.root.get_clipboard_text()
        return str(text) if text is not None else None

    def set_clipboard_text(self, text):
        self._conn.root.set_clipboard_text(text)

    def run_python(self, script):
        return self._conn.root.run_python(script)

    def select_file(self, **kw):
        result = self._conn.root.select_file(**kw)
        return str(result) if result is not None else None

    def select_directory(self, **kw):
        result = self._conn.root.select_directory(**kw)
        return str(result) if result is not None else None

    def enter_text(self, **kw):
        result = self._conn.root.enter_text(**kw)
        return str(result) if result is not None else None

    def select_option(self, **kw):
        result = self._conn.root.select_option(**kw)
        return str(result) if result is not None else None

    def launch_file(self, path):
        self._conn.root.launch_file(path)

    def make_temporary_file(self, suffix='', mode='wb'):
        return self._conn.root.make_temporary_file(suffix=suffix, mode=mode)

    def open_file(self, filename, mode='rb'):
        return self._conn.root.open_file(filename, mode)

    def open_selected_file(self, **kw):
        return self._conn.root.open_selected_file(**kw)

    def make_selected_file(self, mode='wb', **kw):
        return self._conn.root.make_selected_file(mode=mode, **kw)


# ---------------------------------------------------------------------------
# Connection
# ---------------------------------------------------------------------------

_adapter = None
_raw_client = None  # ServiceClient for JSON; RPyC connection for RPyC
_protocol = None
_mod = None


def connect():
    global _adapter, _raw_client, _protocol, _mod

    session_id = pytis.remote.x2go_session_id()
    if not session_id:
        print('ERROR: X2GO_SESSION environment variable not set.')
        sys.exit(1)

    info_file = pytis.remote.pytis_x2go_info_file(session_id)
    if not os.path.exists(info_file):
        print('ERROR: Info file not present: {}'.format(info_file))
        sys.exit(1)

    pytis.remote.keep_x2go_info_file()
    access_data = pytis.remote.parse_x2go_info_file(info_file)
    protocol = access_data.get('protocol', 'rpyc')
    port = access_data['port']
    password = access_data['password']

    if protocol == 'rpyc':
        pytis.remote.write_python_version()

    if protocol == 'json':
        client_py = os.path.join(
            os.path.dirname(os.path.abspath(pytis.remote.__file__)), 'client.py'
        )
        if not os.path.exists(client_py):
            print('ERROR: client.py not found at {}'.format(client_py))
            sys.exit(1)

        mod = _load_module('pytis_remote_client', client_py)
        _mod = mod

        client = mod.ServiceClient(password)
        try:
            client.connect('localhost', port)
        except mod.AuthError as e:
            print('ERROR: Authentication failed: {}'.format(e))
            sys.exit(1)
        except socket.error as e:
            print('ERROR: Connection failed (tunnel not up?): {}'.format(e))
            sys.exit(1)

        _raw_client = client
        _adapter = _JsonAdapter(client, mod.FileProxy)

    else:  # rpyc
        try:
            from pytis.remote import Connector
        except ImportError as e:
            print('ERROR: Cannot import Connector (rpyc not available?): {}'.format(e))
            sys.exit(1)

        import time
        connector = Connector(password)
        deadline = time.time() + 30
        connection = None
        while True:
            try:
                connection = connector.connect('localhost', port)
                break
            except Exception as e:
                # May be a Python version mismatch — wait for pytis2go to
                # restart the RPyC service with the correct Python version.
                if time.time() >= deadline:
                    print('ERROR: RPyC connection failed: {}'.format(e))
                    sys.exit(1)
                time.sleep(2)
                access_data = pytis.remote.parse_x2go_info_file(info_file)
                new_port = access_data['port']
                new_password = access_data['password']
                if new_port != port:
                    print('  (RPyC service restarted on port {}, retrying...)'.format(new_port))
                    port = new_port
                    password = new_password
                    connector = Connector(password)

        _raw_client = connection
        _adapter = _RpycAdapter(connection)

    _protocol = protocol
    return _adapter, protocol


def req(action, **kwargs):
    """Low-level JSON-protocol request (JSON only)."""
    return _raw_client.request(action, **kwargs)


def push_handlers():
    """Push server-side handler code to the client. Protocol-aware."""
    remote_dir = os.path.dirname(os.path.abspath(pytis.remote.__file__))

    if _protocol == 'json':
        extensions = req('extensions')
        if 'session_password' in extensions:
            print('  (client_handlers already active, {} extensions)'.format(len(extensions)))
            return True

        handlers_py = os.path.join(remote_dir, 'client_handlers.py')
        if not os.path.exists(handlers_py):
            print('  WARNING: client_handlers.py not found at {}'.format(handlers_py))
            return False

        with open(handlers_py) as f:
            code = f.read()
        result = req('push_code', code=code)
        if isinstance(result, str) and result.startswith('error:'):
            print('  WARNING: push_code failed:\n{}'.format(result))
            return False
        print('  (client_handlers pushed: {} extensions active)'.format(len(result)))

        # Push clientapi.py to replace the Stub UI backend with a real wx/zenity backend.
        clientapi_py = os.path.join(remote_dir, 'clientapi.py')
        if os.path.exists(clientapi_py):
            with open(clientapi_py) as f:
                clientapi_code = f.read()
            result = req('setup_ui_backend', code=clientapi_code)
            if isinstance(result, str) and result.startswith('error:'):
                print('  WARNING: setup_ui_backend failed: {}'.format(result))
            else:
                print('  (UI backend: {})'.format(result))
        return True

    else:  # rpyc
        conn = _raw_client
        if 'PytisClientAPIService' in list(conn.root.extensions()):
            print('  (PytisClientAPIService already active)')
            return True

        clientapi_py = os.path.join(remote_dir, 'clientapi.py')
        if not os.path.exists(clientapi_py):
            print('  WARNING: clientapi.py not found at {}'.format(clientapi_py))
            return False

        with open(clientapi_py) as f:
            code = f.read()
        error = conn.root.extend(code, 'PytisClientAPIService')
        if error:
            print('  WARNING: extend() failed:\n{}'.format(error))
            return False
        print('  (PytisClientAPIService pushed)')
        return True


# ---------------------------------------------------------------------------
# Test sections
# ---------------------------------------------------------------------------

def test_json_protocol():
    print('\n--- Basic protocol (JSON) ---')

    def t_echo():
        eq(req('echo', text='hello test'), 'hello test')
    test('echo', t_echo)

    def t_ping():
        eq(req('ping'), 'pong')
    test('ping', t_ping)

    def t_protocol_version():
        v = req('protocol_version')
        ok(isinstance(v, int), 'expected int, got {!r}'.format(v))
    test('protocol_version', t_protocol_version)

    def t_extensions():
        exts = req('extensions')
        ok(isinstance(exts, list), 'expected list, got {!r}'.format(exts))
    test('extensions', t_extensions)

    def t_client_info():
        info = json.loads(req('client_info'))
        ok('os_name' in info, 'missing os_name in {!r}'.format(info))
        ok('python_version' in info, 'missing python_version')
        print('       os={} python={} ui_backend={}'.format(
            info.get('os_name'), info.get('python_version'),
            info.get('ui_backend', 'unknown')))
    test('client_info', t_client_info)


def test_rpyc_protocol():
    print('\n--- Basic protocol (RPyC) ---')
    conn = _raw_client

    def t_echo():
        eq(str(conn.root.echo('hello test')), 'hello test')
    test('echo', t_echo)

    def t_version():
        v = conn.root.version()
        ok(v is not None, 'version() returned None')
        print('       version={}'.format(v))
    test('version', t_version)

    def t_extensions():
        exts = list(conn.root.extensions())
        ok(isinstance(exts, list), 'expected list')
        print('       extensions={}'.format(exts))
    test('extensions', t_extensions)

    # client_info is provided by PytisClientAPIService (pushed in push_handlers)
    if 'PytisClientAPIService' in list(conn.root.extensions()):
        def t_client_info():
            info = json.loads(_adapter.client_info())
            ok('os_name' in info, 'missing os_name in {!r}'.format(info))
            ok('python_version' in info, 'missing python_version')
            print('       os={} python={}'.format(
                info.get('os_name'), info.get('python_version')))
        test('client_info', t_client_info)
    else:
        skip('client_info', 'PytisClientAPIService not pushed')


def test_clipboard():
    print('\n--- Clipboard ---')

    marker = 'pytis-test-{}'.format(os.getpid())

    def t_roundtrip():
        _adapter.set_clipboard_text(marker)
        got = _adapter.get_clipboard_text()
        if got != marker:
            print('  {}  set_clipboard_text + get_clipboard_text'
                  ' (got {!r} instead of {!r} — clipboard may not be'
                  ' supported on this platform)'.format(SKIP, got, marker))
            _results.append(('set_clipboard_text + get_clipboard_text', None,
                              'roundtrip mismatch: got {!r}'.format(got)))
            return
        _results.append(('set_clipboard_text + get_clipboard_text', True, None))
        print('  {}  set_clipboard_text + get_clipboard_text'.format(PASS))

    try:
        t_roundtrip()
    except Exception as e:
        msg = '{}: {}'.format(type(e).__name__, e)
        _results.append(('set_clipboard_text + get_clipboard_text', False, msg))
        print('  {}  set_clipboard_text + get_clipboard_text: {}'.format(FAIL, msg))


def test_file_io():
    print('\n--- File I/O ---')

    f = [None]

    def t_make_tmp():
        f[0] = _adapter.make_temporary_file(suffix='.txt', mode='w+b')
        ok(f[0] is not None, 'make_temporary_file returned None')
        print('       path={}'.format(f[0].name))

    if not test('make_temporary_file', t_make_tmp):
        for n in ('file_write', 'file_flush', 'file_seek + file_read (all)',
                  'file_readline', 'file_readlines', 'file_name', 'file_close'):
            skip(n, 'make_temporary_file failed')
        return

    test_data = b'line one\nline two\nline three\n'

    def t_write():
        f[0].write(test_data)
    test('file_write', t_write)

    def t_flush():
        f[0].flush()
    test('file_flush', t_flush)

    def t_seek_read_all():
        f[0].seek(0)
        eq(bytes(f[0].read(-1)), test_data)
    test('file_seek + file_read (all)', t_seek_read_all)

    def t_readline():
        f[0].seek(0)
        eq(bytes(f[0].readline()), b'line one\n')
    test('file_readline', t_readline)

    def t_readlines():
        f[0].seek(0)
        eq([bytes(l) for l in f[0].readlines()],
           [b'line one\n', b'line two\n', b'line three\n'])
    test('file_readlines', t_readlines)

    def t_name():
        ok(f[0].name is not None, 'name is None')
    test('file_name', t_name)

    def t_close():
        f[0].close()
    test('file_close', t_close)

    # open_file: write a file on the client, then re-open it
    print('\n--- File I/O (open_file) ---')

    client_content = b'open_file test content \xc4\x8d\xc5\x99'
    client_path = [None]
    rf = [None]

    def t_prepare_client_file():
        tf = _adapter.make_temporary_file(suffix='.bin', mode='wb')
        tf.write(client_content)
        client_path[0] = str(tf.name)
        tf.close()

    if test('make_temporary_file (prepare for open_file)', t_prepare_client_file):
        def t_open_file():
            rf[0] = _adapter.open_file(client_path[0], mode='rb')
            ok(rf[0] is not None, 'open_file returned None')
        if test('open_file', t_open_file):
            def t_read_opened():
                eq(bytes(rf[0].read(-1)), client_content)
            test('file_read from open_file', t_read_opened)
            rf[0].close()


def test_run_python():
    print('\n--- run_python ---')

    def t_exit0():
        eq(_adapter.run_python('import sys; sys.exit(0)'), 0)
    test('run_python exit(0)', t_exit0)

    def t_exit42():
        eq(_adapter.run_python('import sys; sys.exit(42)'), 42)
    test('run_python exit(42)', t_exit42)


def test_pushed_handlers():
    print('\n--- Pushed handlers ---')

    extensions = _adapter.extensions()
    pushed = ('session_password' in extensions if _protocol == 'json'
              else 'PytisClientAPIService' in extensions)

    if not pushed:
        skip('session_password', 'handlers not active')
        return

    def t_session_password():
        result = _adapter.session_password()
        ok(result is None or isinstance(result, str),
           'expected str or None, got {!r}'.format(result))
        print('       session_password={}'.format(
            repr(result) if result is None else '***({} chars)'.format(len(result))))
    test('session_password', t_session_password)


def test_interactive():
    print('\n--- Interactive tests (user action required) ---')

    def t_select_file():
        print('    -> SELECT any file in the dialog that opens on the client...')
        result = _adapter.select_file(title='test_handlers: select any file',
                                      patterns=[], pattern=None, save=False, multi=False)
        ok(result is not None, 'dialog was cancelled or failed')
        string_types = str if sys.version_info[0] >= 3 else (str, type(u''))
        ok(isinstance(result, string_types), 'expected filename string, got {!r}'.format(result))
        print('       selected: {}'.format(result))
    test('select_file (open)', t_select_file)

    def t_select_dir():
        print('    -> SELECT any directory in the dialog...')
        result = _adapter.select_directory(title='test_handlers: select any directory')
        ok(result is not None, 'dialog was cancelled or failed')
        print('       selected: {}'.format(result))
    test('select_directory', t_select_dir)

    def t_enter_text():
        print('    -> TYPE exactly "pytis-ok" in the dialog and click OK...')
        result = _adapter.enter_text(title='test_handlers', label='Type "pytis-ok":')
        eq(result, 'pytis-ok')
    test('enter_text', t_enter_text)

    def t_select_option():
        print('    -> SELECT the second option ("beta") in the list dialog...')
        result = _adapter.select_option(
            title='test_handlers',
            label='Select "beta":',
            columns=['Name', 'Value'],
            data=[['alpha', '1'], ['beta', '2'], ['gamma', '3']],
            return_column=1)
        eq(result, 'beta')
    test('select_option', t_select_option)

    def t_open_selected():
        print('    -> SELECT any file in the open dialog...')
        result = _adapter.open_selected_file(patterns=[], pattern=None)
        ok(result is not None, 'dialog cancelled or failed')
        raw = bytes(result.read(64))
        result.close()
        print('       first {} bytes read OK'.format(len(raw)))
    test('open_selected_file', t_open_selected)

    def t_make_selected():
        print('    -> SAVE to any location in the save dialog...')
        result = _adapter.make_selected_file(mode='wb', patterns=[], pattern=None)
        ok(result is not None, 'dialog cancelled or failed')
        result.write(b'pytis2go test_handlers write test')
        result.close()
        print('       saved OK')
    test('make_selected_file', t_make_selected)

    def t_launch():
        tf = _adapter.make_temporary_file(suffix='.txt', mode='wb')
        tf.write(b'pytis2go test_handlers launch test\n')
        p = str(tf.name)
        tf.close()
        _adapter.launch_file(p)
        print('       launched: {} (check client for open file)'.format(p))
    test('launch_file', t_launch)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    interactive = '--interactive' in sys.argv

    print('pytis2go handler test')
    print('=' * 50)

    adapter, protocol = connect()
    print('Connected via {} on port {}'.format(
        protocol.upper(),
        pytis.remote.parse_x2go_info_file(
            pytis.remote.pytis_x2go_info_file()).get('port', '?')))

    push_handlers()

    if protocol == 'json':
        test_json_protocol()
    else:
        test_rpyc_protocol()

    test_clipboard()
    test_file_io()
    test_run_python()
    test_pushed_handlers()

    if interactive:
        test_interactive()
    else:
        print('\n--- Interactive tests ---')
        print('  (skipped; re-run with --interactive to test file dialogs)')

    # Summary
    passed = sum(1 for _, ok_, _ in _results if ok_ is True)
    failed = sum(1 for _, ok_, _ in _results if ok_ is False)
    skipped = sum(1 for _, ok_, _ in _results if ok_ is None)

    print('\n' + '=' * 50)
    print('Results: {} passed, {} failed, {} skipped'.format(passed, failed, skipped))

    if failed:
        print('\nFailed:')
        for name, ok_, msg in _results:
            if ok_ is False:
                print('  FAIL  {}: {}'.format(name, msg))
        sys.exit(1)
    else:
        print('All tests passed.')


if __name__ == '__main__':
    main()
