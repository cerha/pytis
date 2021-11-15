#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2021 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

import os
import pytest

import pytis.remote


def interactive(test):
    envvar = 'PYTIS_TEST_INTERACTIVE'
    return pytest.mark.skipif(not os.getenv(envvar), reason="{} not set.".format(envvar))(test)


class TestRemote:
    """Test functions in 'pytis.remote' in remote mode.

    This test must be run through Pytis2go.  Launch a terminal through Pytis2go
    client and run the test inside the terminal.

    This test is only run if the remote environment exists.  Particularly if
    the environment variable 'X2GO_SESSION' exists and the session file in
    ~/.x2go/ssh contains connection data.

    """

    @classmethod
    def setup_class(cls):
        if not os.getenv('X2GO_SESSION'):
            pytest.skip("Not within an X2Go session.")
        # Avoid removing the info file (to allow running tests multiple times).
        pytis.remote.keep_x2go_info_file()

    def test_clipboard(self):
        pytis.remote.set_clipboard_text('foo')
        assert pytis.remote.get_clipboard_text() == 'foo'

    def test_basic_file_operations(self):
        with pytis.remote.open_file('test.txt', mode='wb') as f:
            f.write(b'abc')
            f.flush()  # Just to test that the call does not fail...
        with pytis.remote.open_file('test.txt', mode='rb') as f:
            assert f.read() == b'abc'
            f.seek(0)
            assert f.read(1) == b'a'
            assert f.read(10) == b'bc'

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

    def test_run_python(script):
        with pytis.remote.make_temporary_file(suffix='.txt', mode='w') as f:
            f.write('a')
            fname = f.name
        pytis.remote.run_python("with open('{}', 'a') as f: f.write('bc')".format(fname))
        with pytis.remote.open_file(fname, mode='r') as f:
            assert f.read() == b'abc'
        pytis.remote.run_python("import os; os.remove('{}')".format(fname))

    @interactive
    def test_file_dialogs(self):
        directory = pytis.remote.select_directory(title=("Select a directory"))
        assert directory is not None
        path = pytis.remote.select_file(directory=directory, pattern='*.pdf',
                                            title="Select a PDF file in this directory")
        assert path and path.startswith(directory)
        with pytis.remote.open_file(path, mode='r') as f:
            assert f.read(4) == b'%PDF'
        pytis.remote.launch_file(path)
