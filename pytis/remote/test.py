#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2011-2018 OUI Technology Ltd.
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

"""Tests for `pytis.remote` in a live Pytis2Go session.

Run inside an X2Go session with an active pytis2go connection:

`pytis/remote/test.py`

Interactive tests open file-chooser dialogs on the client machine; enable them
by setting the environment variable `PYTIS_TEST_INTERACTIVE` or passing
`--interactive` command line argument:

`pytis/remote/test.py --interactive`

"""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

import os
import pytest

import pytis.remote


def interactive(test):
    envvar = 'PYTIS_TEST_INTERACTIVE'
    return pytest.mark.skipif(not os.getenv(envvar), reason="{} not set".format(envvar))(test)


class TestRemote:
    """Tests for `pytis.remote` functions in a live Pytis2Go session.

    The test suite must run inside an active X2Go session connected to a
    pytis2go client.  The entire class is skipped when `X2GO_SESSION` is not
    set.

    """

    @classmethod
    def setup_class(cls):
        if not os.getenv('X2GO_SESSION'):
            pytest.skip("Not within an X2Go session.")
        pytis.remote.keep_x2go_info_file()
        if not pytis.remote.client_connection_ok():
            pytest.skip("pytis2go connection not available.")

    def test_clientapi_loaded(self):
        assert pytis.remote.RPCInfo.client_api_pushed

    def test_client_info(self):
        info = pytis.remote.client_info()
        assert info is not None
        assert info.os_name is not None
        assert info.python_version is not None

    def test_connection_echo(self):
        assert pytis.remote.client_connection_ok()

    def test_clipboard(self):
        pytis.remote.set_clipboard_text('foo')
        assert pytis.remote.get_clipboard_text() == 'foo'

    def test_file_io(self):
        test_data = b'line one\nline two\nline three\n'
        with pytis.remote.make_temporary_file(suffix='.bin', mode='w+b') as f:
            f.write(test_data)
            f.flush()
            f.seek(0)
            assert f.read() == test_data
            f.seek(0)
            assert f.readline() == b'line one\n'
            f.seek(0)
            assert list(f.readlines()) == [b'line one\n', b'line two\n', b'line three\n']
            assert f.name is not None

    def test_file_explicit_close(self):
        # Regression test: explicit write() and close() calls (not via context
        # manager) must work.  The context manager routes through __exit__ which
        # is in RPyC's safe_attrs, so it succeeds even without exposed_write /
        # exposed_close on the remote object — but explicit CALLATTR 'write' and
        # CALLATTR 'close' require the exposed_* aliases to be present.
        # Previously, RPyCPytisClientAPIService.exposed_make_temporary_file
        # returned a plain FileWrapper (no exposed_* methods), which caused
        # AttributeError("cannot access 'close'") when close() was called
        # explicitly (e.g. in application.py's api_launch_file() finally block).
        test_data = b'explicit close test\n'
        f = pytis.remote.make_temporary_file(suffix='.bin', mode='wb')
        assert f is not None
        fname = f.name
        f.write(test_data)
        f.close()  # Must not raise AttributeError("cannot access 'close'")
        with pytis.remote.open_file(fname, mode='rb') as g:
            assert g.read() == test_data

    def test_open_file(self):
        content = b'open_file test \xc4\x8d\xc5\x99'
        with pytis.remote.make_temporary_file(suffix='.bin', mode='wb') as f:
            f.write(content)
            fname = f.name
        with pytis.remote.open_file(fname, mode='rb') as f:
            assert f.read(4) == b'open'
            assert f.read() == b'_file test \xc4\x8d\xc5\x99'
        with pytis.remote.open_file(fname, mode='wb') as f:
            f.write(b'replaced')
        with pytis.remote.open_file(fname, mode='rb') as f:
            assert f.read() == b'replaced'

    def test_encoded_file_operations(self):
        with pytis.remote.make_temporary_file(suffix='.txt', mode='w', encoding='utf-8') as f:
            f.write("Žluťoučký\nkůň\n")
            fname = f.name
        with pytis.remote.open_file(fname, mode='r', encoding='utf-8') as f:
            assert f.read() == "Žluťoučký\nkůň\n"
            f.seek(0)
            assert f.readline() == "Žluťoučký\n"
            f.seek(0)
            assert tuple(f.readlines()) == ("Žluťoučký\n", "kůň\n")
        with pytis.remote.open_file(fname, mode='rb') as f:
            assert f.read() == b'\xc5\xbdlu\xc5\xa5ou\xc4\x8dk\xc3\xbd\nk\xc5\xaf\xc5\x88\n'
            f.seek(0)
            assert f.readline() == b'\xc5\xbdlu\xc5\xa5ou\xc4\x8dk\xc3\xbd\n'
            f.seek(0)
            assert tuple(f.readlines()) == (b'\xc5\xbdlu\xc5\xa5ou\xc4\x8dk\xc3\xbd\n',
                                            b'k\xc5\xaf\xc5\x88\n')

    def test_run_python(self):
        with pytis.remote.make_temporary_file(suffix='.txt', mode='wb') as f:
            f.write(b'a')
            fname = f.name
        pytis.remote.run_python("with open('{}', 'ab') as f: f.write(b'bc')".format(fname))
        with pytis.remote.open_file(fname, mode='rb') as f:
            assert f.read() == b'abc'
        pytis.remote.run_python("import os; os.remove('{}')".format(fname))
        assert pytis.remote.run_python('print("hello world")') == 0
        assert pytis.remote.run_python('script_with_some_error()') == 1
        assert pytis.remote.run_python('import sys; sys.exit(24)') == 24
        assert pytis.remote.run_python('import sys; sys.exit(42)') == 42

    @interactive
    def test_file_dialogs(self):
        print("\n    -> SELECT any directory...")
        directory = pytis.remote.select_directory(title="Select a directory")
        assert directory is not None
        print("       selected: {}".format(directory))
        print("    -> SELECT a PDF file...")
        path = pytis.remote.select_file(directory=directory, pattern='*.pdf',
                                        title="Select a PDF file in this directory")
        assert path
        with pytis.remote.open_file(path, mode='rb') as f:
            assert f.read(4) == b'%PDF'
        print("       selected: {}".format(path))
        print("    -> CHECK that the file opens on the client...")
        pytis.remote.launch_file(path)

    @interactive
    def test_open_selected_file(self):
        print("\n    -> SELECT any file in the open dialog that appears on the client...")
        result = pytis.remote.open_selected_file(patterns=[], pattern=None)
        assert result is not None
        data = result.read(64)
        result.close()
        print("       first {} bytes read OK".format(len(data)))

    @interactive
    def test_make_selected_file(self):
        print("\n    -> SAVE to any location in the save dialog that appears on the client...")
        result = pytis.remote.make_selected_file(mode='wb', patterns=[], pattern=None)
        assert result is not None
        result.write(b'pytis2go test write')
        result.close()
        print("       saved OK")


if __name__ == '__main__':
    import sys
    args = [__file__, '-v', '-s']
    if '--interactive' in sys.argv:
        os.environ['PYTIS_TEST_INTERACTIVE'] = '1'
    sys.exit(pytest.main(args))
