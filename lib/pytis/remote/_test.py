#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011-2016 Brailcom, o.p.s.
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

import unittest
import pytis.remote.pytisproc
import pytis.remote.clientui
import tempfile
import cStringIO as StringIO
import os

class ClientUIBackend(unittest.TestCase):
    _BACKEND = None

    @classmethod
    def setUpClass(cls):
        if cls._BACKEND is None:
            raise unittest.SkipTest('')
        cls._backend = cls._BACKEND()

    def test_clipboard(self):
        for text in ('foo', u'foo', u'Žluťoučký kůň!'):
            self._backend.set_clipboard_text(text)
            self.assertEqual(self._backend.get_clipboard_text(), text)

    def test_enter_text(self):
        text = self._backend.enter_text(label='Enter "foo":')
        self.assertEqual(text, 'foo')

    def test_select_option(self):
        answer = self._backend.select_option(label="Select the second option",
                                             columns=('Option', 'Description'),
                                             data=(('First', 'blah...'),
                                                   ('Second', 'blah blah...'),
                                                   ('Third', 'blah blah blah...')))
        self.assertEqual(answer, 'Second')

    def test_select_directory(self):
        directory = self._backend.select_directory()
        answer = self._backend.select_option(label='You selected "%s"' % directory,
                                      columns=('Confirm',),
                                      data=(('Yes',), ('No',)))
        self.assertEqual(answer, 'Yes')

    def test_select_file(self):
        filename = self._backend.select_file()
        answer = self._backend.select_option(label='You selected "%s"' % filename,
                                     columns=('Confirm',),
                                     data=(('Yes',), ('No',)))
        self.assertEqual(answer, 'Yes')


class WxUIBackend(ClientUIBackend):
    _BACKEND = pytis.remote.clientui.WxUIBackend

class TkUIBackend(ClientUIBackend):
    _BACKEND = pytis.remote.clientui.TkUIBackend

class ZenityUIBackend(ClientUIBackend):
    _BACKEND = pytis.remote.clientui.ZenityUIBackend

class Win32UIBackend(ClientUIBackend):
    _BACKEND = pytis.remote.clientui.Win32UIBackend


class FileWrapper(unittest.TestCase):

    def _encrypt(self, f):
        return f.read().encode('base64')

    def _decrypt(self, data):
        return data.decode('base64')

    def _test_read(self, filename, data, mode='r', **kwargs):
        try:
            with file(filename, 'w') as f:
                f.write(data)
            wrapper = pytis.remote.pytisproc.ExposedFileWrapper(filename, mode=mode, **kwargs)
            data2 = wrapper.exposed_read()
            if 'encrypt' in kwargs:
                data2 = self._decrypt(data2)
            wrapper.exposed_close()
            self.assertEqual(data2, data)
        finally:
            os.unlink(filename)

    def _test_write(self, filename, data, mode='w', **kwargs):
        try:
            wrapper = pytis.remote.pytisproc.ExposedFileWrapper(filename, mode=mode, **kwargs)
            if 'decrypt' in kwargs:
                data_to_write = self._encrypt((StringIO.StringIO(data)))
            else:
                data_to_write = data
            wrapper.exposed_write(data_to_write)
            wrapper.exposed_flush()
            wrapper.exposed_close()
            with file(filename) as f:
                data2 = f.read()
            self.assertEqual(data2, data)
        finally:
            os.unlink(filename)

    def test_read(self):
        filename = tempfile.mkstemp(prefix='pytistmp', suffix='txt')[1]
        self._test_read(filename, "foo('bar')='baz'")

    def test_read_handle(self):
        handle, filename = tempfile.mkstemp(prefix='pytistmp', suffix='txt')
        self._test_read(filename, "foo('bar')='baz'", handle=handle)

    def test_encryption(self):
        filename = tempfile.mkstemp(prefix='pytistmp', suffix='txt')[1]
        self._test_read(filename, "foo('bar')='baz'", encrypt=self._encrypt)

    def test_write(self):
        filename = tempfile.mkstemp(prefix='pytistmp', suffix='txt')[1]
        self._test_write(filename, "foo('bar')='baz'")

    def test_write_handle(self):
        handle, filename = tempfile.mkstemp(prefix='pytistmp', suffix='txt')
        self._test_write(filename, "foo('bar')='baz'", handle=handle)

    def test_decryption(self):
        filename = tempfile.mkstemp(prefix='pytistmp', suffix='txt')[1]
        self._test_write(filename, "foo('bar')='baz'", decrypt=self._decrypt)


if __name__ == '__main__':
    unittest.main()
