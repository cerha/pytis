# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2007-2016 OUI Technology Ltd.
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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

from __future__ import unicode_literals

import pytis.data
import pytis.form
from pytis.presentation import Binding, Specification, Editable, Action, computer
from pytis.util import translations
import pytis.extensions
import pytis.util

import pytis.dbdefs.demo as dbdefs
from pytis.api import app

MB = 1024 * 1024

_ = translations('pytis-demo')


class BigBinary(pytis.data.Binary, pytis.data.Big):
    pass


class BinaryData(Specification):
    """Basic binary fields demo.

    The table only stores the file contents in a binary field.  It is possible
    to load and save the binary values as files.  The file name is stored in
    another field, which is initialized through a computer.  The computer uses
    the method 'Binary.Data.filename()' to obtain the original file name from
    the inner value of the field 'data'.

    """
    public = True
    table = dbdefs.BinaryData
    title = "Binary data"

    def _customize_fields(self, fields):
        fields.set_property('width', id=8, descr=60)
        fields.modify('id', default=pytis.util.nextval('binary_data_id_seq'))
        fields.modify('filename', editable=Editable.NEVER, computer=computer(self._filename))
        fields.modify('data', filename='filename', type=BigBinary())

    def _filename(self, row, data):
        return data and data.filename() or ''
    columns = ('filename', 'descr')
    layout = ('id', 'data', 'descr')

    def actions(self):
        return (Action('upload_encrypted', _("Encrypt and upload"), self._upload_encrypted),
                Action('download_decrypted', _("Download and decrypt"), self._download_decrypted),
                )

    def bindings(self):
        return (Binding('viewer', _("Viewer"), content=self._pdf, content_type='pdf'),)

    def _pdf(self, row):
        # The row argument doesn't contain 'pytis.data.Big' columns.
        full_row = row.data().row(row['id'])
        return full_row['data'].value()

    def _select_encryption_keys(self):
        dbconnection = pytis.config.dbconnection
        key_table = pytis.extensions.crypto_key_table(dbconnection)
        rows = key_table.select_map(pytis.util.identity)
        keys = [(False, r['username'].value(), r['name'].value(),) for r in rows]
        checked = pytis.form.run_dialog(pytis.form.CheckListDialog,
                                        message=_("Select recipient keys"),
                                        columns=(_("User"), _("Area")),
                                        items=keys)
        if checked is None:
            return None
        function = pytis.data.DBFunctionDefault('pytis_crypto_user_contact', dbconnection)
        keys = []
        for r, c in zip(rows, checked):
            if checked:
                row = pytis.data.Row((('username', r['username'],),))
                key = function.call(row)[0][1].value()
                if key:
                    keys.append(key)
        return keys

    def _upload_encrypted(self, row):
        if not pytis.remote.client_available():
            app.error(_("Remote connection not available."))
            return
        keys = self._select_encryption_keys()
        # Or use keys = [] to let the user select keys from the client machine keyring.
        if keys is None:
            app.error(_("No key selected."))
            return
        f = pytis.remote.open_selected_file(encrypt=keys)
        if f is None:
            app.error(_("No file selected."))
            return
        try:
            fname = f.name
            if '\\' in fname:
                import ntpath
                p = ntpath
            else:
                import os
                p = os.path
            data = ''
            while True:
                next_data = f.read()
                if not next_data:
                    break
                data += next_data
        finally:
            f.close()
        if not data:
            app.error(_("Encryption error."))
            return
        filename = p.split(fname)[-1]
        type_ = row['data'].type()
        try:
            buf = type_.Data(data, filename=filename)
        except pytis.data.ValidationError as e:
            app.echo(e.message(), 'error')
        except IOError as e:
            app.echo(_("Error reading file:") + ' ' + str(e), 'error')
        else:
            row.data().update(row['id'],
                              pytis.data.Row((('filename', pytis.data.sval(filename)),
                                              ('data', pytis.data.Value(type_, buf)),)))

    def _download_decrypted(self, row):
        if not pytis.remote.client_available():
            app.error(_("Remote connection not available."))
            return
        data = row.data().row(row['id'])['data'].value()
        if data is None:
            app.message(_("No data in this row. %s"))
            return
        f = pytis.remote.make_selected_file(filename=row['filename'].value(), decrypt=True)
        if f is None:
            return
        f.write(data)
        f.close()


class Images(Specification):
    """Simple table containing images as binary data.

    This view demonstrates the use of the 'Image' data type and 'ImageField'
    input field (which is used automatically for fields of this type).  The
    'ImageField' displays an image thumbnail within the input form.  The
    arguments 'width' and 'height' of the field specification determine the
    thumbnail size in pixels (in contrast to the common case, where these
    values are interpreted as character sizes).  Cliking the thumbnail opens
    the image in an image viewer.

    As oposed to the generic binary field, this field will not allow loading
    data other than a valid image in one of the supported image formats.

    Practical examples of computers computing their values from various image
    properties are also demonstrated.  The actual image as a 'PIL.Image'
    instance can be accessed through the 'image()' method of the binary field's
    value.

    """
    public = True
    title = _("Images")
    table = dbdefs.Images

    def _customize_fields(self, fields):
        fields.set_property('width', id=8, data=130, filename=30, title=30, descr=60)
        fields.modify('id', default=pytis.util.nextval('images_id_seq'))
        fields.modify('data', height=130,
                      type=pytis.data.Image(maxlen=(2 * MB), formats=('JPEG', 'PNG'), not_null=True,
                                            minsize=(130, 130), maxsize=(3000, 3000)),
                      filename='filename', filename_extensions=('jpg', 'jpeg', 'gif', 'png'))
        fields.modify('filename', editable=Editable.ALWAYS, computer=computer(self._filename))
        fields.modify('size', editable=Editable.NEVER, computer=computer(self._size))

    def _size(self, row, data):
        return data and '%dx%d' % data.image().size or ''

    def _filename(self, row, data):
        return data and data.filename() or ''

    columns = ('id', 'filename', 'title', 'size')
    layout = ('data', 'title', 'descr')
