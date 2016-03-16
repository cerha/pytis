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
import tempfile
import cStringIO as StringIO
import os


class Client(unittest.TestCase):

    def test_clipboard(self):
        client = pytis.remote.pytisproc.ClientSideOperations()
        for text in ('foo', u'foo', u'Žluťoučký kůň!'):
            client.set_clipboard_text(text)
            self.assertEqual(client.get_clipboard_text(), text)

    def test_enter_text(self):
        client = pytis.remote.pytisproc.ClientSideOperations()
        text = client.enter_text(label='Enter "foo":')
        self.assertEqual(text, 'foo')

    def test_select_option(self):
        client = pytis.remote.pytisproc.ClientSideOperations()
        option = client.select_option(label="Select the second option",
                                      columns=('Option', 'Description'),
                                      data=(('First', 'blah...'),
                                            ('Second', 'blah blah...'),
                                            ('Third', 'blah blah blah...')))
        self.assertEqual(option[0], 'Second')


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
