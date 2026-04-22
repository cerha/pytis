# -*- coding: utf-8 -*-

# Copyright (C) 2020-2026 Tomáš Cerha <t.cerha@gmail.com>
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

"""Interactive tests for `ClientUIBackend` subclasses defined in `clientapi.py`.

Each test opens a real GUI dialog on the local machine and verifies the
result.  All tests in this file require a display and human interaction:
they are skipped by default and only run when ``--interactive`` is passed to pytest.

Backend selection is controlled by the `TEST_UI_BACKENDS` environment
variable: a comma-separated list of backend names (e.g. ``wx,tk``) or
``all`` to run every available backend.  Available names: ``wx``, ``tk``,
``zenity``, ``win32``, ``mac``.

Protocol independence: tests here are protocol-neutral — they instantiate
`ClientUIBackend` subclasses directly, with no network or RPC transport.

For non-interactive client API tests (file I/O, clipboard, …) see
`test_clientapi.py`.  For live end-to-end connection tests see `test.py`.

"""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

import os

import pytest

from pytis.remote import clientapi


def _backend_mark(name):
    """Return a skip mark for backend ``name`` unless it is in TEST_UI_BACKENDS."""
    envvar = 'TEST_UI_BACKENDS'
    backends = os.getenv(envvar) or ''
    return pytest.mark.skipif(
        backends != 'all' and name not in backends.split(','),
        reason="Backend '{}' not in {} (set {}=all or {}={} to run)".format(
            name, envvar, envvar, envvar, name),
    )


@pytest.mark.interactive
class ClientUIBackendTest:
    """Base class for backend-specific interactive test suites.

    Derived classes set `_BACKEND` to the name of the `ClientUIBackend`
    subclass under test.  Tests open real GUI dialogs and require a human
    to interact with them — they are decorated ``@pytest.mark.interactive``
    at class level so the conftest skip logic applies to all methods at once.

    `test_00_clipboard` is the only exception: it does not open a dialog and
    could in principle run non-interactively, but it still requires a
    working clipboard stack on the test machine, which in practice is only
    available in the same GUI session used for the other tests.

    """

    _BACKEND = None

    def setup_method(self):
        backend_cls = getattr(clientapi, self._BACKEND)
        self._backend = backend_cls()
        self._backend.init()

    def _confirm(self, text):
        answer = self._backend.select_option(
            title="Confirm test result",
            label=text,
            columns=('Confirm',),
            data=(('Yes',), ('No',)),
        )
        assert answer == 'Yes'

    def test_00_clipboard(self):
        for text in ('foo', 'Žluťoučký kůň!', '頁設是'):
            self._backend.set_clipboard_text(text)
            assert self._backend.get_clipboard_text() == text

    def test_01_enter_password(self):
        text = self._backend.enter_text(title="Password dialog test",
                                        label='Enter password "foo":', password=True)
        assert text == 'foo'

    def test_02_select_option(self):
        answer = self._backend.select_option(
            title="Selection dialog test",
            label="Select the second option:",
            columns=('Id', 'Title'),
            data=(('001', 'First option'),
                  ('002', 'Second option'),
                  ('003', 'Third Option')),
        )
        assert answer == '002'

    def test_03_select_file(self):
        filename = self._backend.select_file(
            title="Select an image file, please",
            patterns=(('Image files', ('*.jpg', '*.jpeg', '*.png')),),
        )
        self._confirm('You selected "%s"' % filename)

    def test_04_select_file_save(self):
        filename = self._backend.select_file(title="Select a file to save, please",
                                             save=True)
        self._confirm('You selected "%s"' % filename)

    def test_05_select_multiple_files(self):
        filenames = self._backend.select_file(title="Select some files, please",
                                              multi=True)
        self._confirm('You selected %d files' % len(filenames or ()))

    def test_06_select_directory(self):
        directory = self._backend.select_directory(title="Select a directory, please")
        self._confirm('You selected "%s"' % directory)


@_backend_mark('wx')
class TestWxUIBackend(ClientUIBackendTest):
    _BACKEND = 'WxUIBackend'


@_backend_mark('tk')
class TestTkUIBackend(ClientUIBackendTest):
    _BACKEND = 'TkUIBackend'


@_backend_mark('zenity')
class TestZenityUIBackend(ClientUIBackendTest):
    _BACKEND = 'ZenityUIBackend'


@_backend_mark('win32')
class TestWin32UIBackend(ClientUIBackendTest):
    _BACKEND = 'Win32UIBackend'


@_backend_mark('mac')
class TestMacUIBackend(ClientUIBackendTest):
    _BACKEND = 'MacUIBackend'
