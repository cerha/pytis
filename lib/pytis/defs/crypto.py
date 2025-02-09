# -*- coding: utf-8 -*-

# Copyright (C) 2019-2023 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2014-2015 OUI Technology Ltd.
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

from __future__ import print_function
from builtins import range

import pytis.data
import pytis.extensions
from pytis.api import app
from pytis.presentation import Specification, Field, Binding
from pytis.util import rsa_encrypt, translations
from pytis.dbdefs import db_pytis_crypto
from pytis.dbdefs.db_pytis_crypto import PytisCryptoDbKey

_ = translations('pytis')


class NewAdminPasswd(Specification):
    public = True

    data_cls = pytis.data.RestrictedMemData
    title = _(u"Změna administrátorského hesla")

    def fields(self):
        return (
            Field('old_password', _(u'Stávající heslo'), width=20,
                  type=pytis.data.Password, md5=False, verify=False),
            Field('new_password', _(u'Nové heslo'), width=20,
                  type=pytis.data.Password, maxlen=32, minlen=8, md5=False,
                  strength=self._check_strength),
        )

    def _check_strength(self, passwd):
        lower = upper = digits = False
        for char in passwd:
            if char.isalpha() and char.islower():
                lower = True
            elif char.isalpha() and char.isupper():
                upper = True
            elif char.isdigit():
                digits = True
        if not (lower and upper and digits):
            return _(u"Použijte, prosím, kombinaci malých a velkých znaků a číslic.")


class CryptoAreas(Specification):
    public = True
    table = db_pytis_crypto.CPytisCryptoNames
    title = _(u"Šifrovací oblasti")
    sorting = (('name', pytis.data.ASCENDENT,),)
    bindings = (Binding('users', _(u"Uživatelé"), 'crypto.Users', binding_column='name'),)

    def actions(self):
        return (pytis.presentation.Action('change_password', _(u"Změnit heslo administrátora"),
                                          self._change_admin_password),)

    def _change_admin_password(self, row):
        area = row['name'].value()
        if not area:
            return
        db_key = pytis.data.dbfunction(PytisCryptoDbKey, 'pytis')
        connection_data = pytis.config.dbconnection
        key_id, key = pytis.extensions.crypto_admin_key(area, 'admin', connection_data)
        if not key_id or not key:
            app.error(_(u"Nebyl nalezen klíč administrátora pro tuto oblast"))
            return
        row = app.new_record("crypto.NewAdminPasswd", multi_insert=False)
        if not row:
            return
        old_password = row["old_password"].value()
        if not pytis.extensions.check_crypto_password(key, old_password, connection_data):
            app.error(_(u"Chybné heslo"))
        encrypted_old_password = rsa_encrypt(db_key, old_password)
        new_password = row["new_password"].value()
        encrypted_new_password = rsa_encrypt(db_key, new_password)
        function = pytis.data.DBFunctionDefault('pytis_crypto_change_password', connection_data)
        row = pytis.data.Row((('id_', key_id,),
                              ('old_psw', pytis.data.sval(encrypted_old_password),),
                              ('new_psw', pytis.data.sval(encrypted_new_password),),))
        if not function.call(row)[0][0].value():
            app.error(_(u"Heslo se změnit nepodařilo"))
            return
        app.message(_(u"Heslo bylo změněno"))

    def on_new_record(self, *args, **kwargs):
        return None

    def on_edit_record(self, row):
        return None

    def on_delete_record(self, row):
        return None


class Users(Specification):
    public = True
    table = db_pytis_crypto.EPytisCryptoKeys
    title = _(u"Uživatelé")

    def _customize_fields(self, fields):
        fields.modify('name',
                      editable=pytis.presentation.Computer(
                          self._editable, depends=()))
        fields.modify('username',
                      editable=pytis.presentation.Computer(
                          self._editable, depends=('name',)))
        fields.append(
            Field('admin_password', _(u"Heslo přihlášeného uživatele"),
                  width=24, virtual=True,
                  type=pytis.data.Password(not_null=True, verify=False),
                  editable=pytis.presentation.Computer(self._editable, depends=('name',))
                  ))
        fields.append(
            Field('admin_address', _(u"E-mailová adresa administrátora"),
                  width=24, virtual=True,
                  type=pytis.data.String(not_null=True),
                  editable=pytis.presentation.Computer(self._editable, depends=('name',)),
                  ))
        fields.append(
            Field('user_password', _(u"Nové heslo pro uživatele"),
                  virtual=True, type=pytis.data.String(),
                  editable=pytis.presentation.Editable.NEVER
                  ))

    columns = ('username', 'fresh',)
    # layout = ('name', 'username', 'admin_address', 'admin_password',)

    def layout(self):
        return pytis.presentation.VGroup(
            'name', 'admin_password', 'username', 'admin_address',
            pytis.presentation.HGroup(
                'user_password',
                pytis.presentation.Button(
                    _(u"Generovat heslo"), self._gen_password),
                pytis.presentation.Button(
                    _(u"Zrušit heslo"), self._clear_password)
            ))

    def _editable(self, row, col):
        name = row['name'].value()
        if col == 'name':
            return not name
        elif col == 'admin_password':
            return name is not None
        if name and name not in app.decrypted_areas():
            return False
        else:
            return True

    def _clear_password(self, row):
        row['user_password'] = pytis.data.sval(None)

    def _gen_password(self, row):
        import os
        import random
        import string
        length = 10
        chars = string.ascii_letters + string.digits
        random.seed = (os.urandom(1024))
        password = ''.join(random.choice(chars) for i in range(length))
        row['user_password'] = pytis.data.sval(password)

    def on_new_record(self, prefill=None, transaction=None):
        main_form_row = app.main_form.row
        if not main_form_row:
            return
        area = main_form_row['name'].value()
        fields = [f for f in self.view_spec().fields()
                  if f.id() in ('name', 'username', 'admin_address', 'admin_password',
                                'user_password')]
        fields = self._Fields(fields)
        for f in fields:
            if f.virtual():
                fields.modify(f.id(), virtual=False)
        record = app.input_form(title=_(u"Nový uživatel šifrované oblasti"),
                                fields=fields, layout=self.view_spec().layout(),
                                check=self.view_spec().check(), prefill={'name': area})
        if not record:
            return
        if not transaction:
            transaction = pytis.data.DBTransactionDefault(pytis.config.dbconnection)
        error = pytis.extensions.add_crypto_user(
            area,
            record['username'].value(),
            pytis.config.dbuser,
            record['admin_password'].value(),
            record['admin_address'].value(),
            pytis.config.dbconnection,
            transaction=transaction,
            user_password=record['user_password'].value(),
        )
        if error:
            transaction.rollback()
            app.error("Error: %s" % (error,))
        else:
            transaction.commit()

    def check(self, row):
        area = row['name'].value()
        if not area or area not in app.decrypted_areas():
            return
        connection_data = pytis.config.dbconnection
        key_id, key = pytis.extensions.crypto_admin_key(area, pytis.config.dbuser, connection_data)
        if not key_id or not key:
            app.error(_(u"Nebyl nalezen klíč pro tuto oblast"))
            return 'name'
        crypto_password = row['admin_password'].value()
        if not pytis.extensions.check_crypto_password(key, crypto_password, connection_data):
            app.error(_(u"Chybné heslo"))
            return 'admin_password'

    def on_edit_record(self, row):
        app.warning(_(u"Uživatele lze jen přidávat a odebírat"))
        return None

    def on_delete_record(self, row):
        if row['username'].value() == 'admin':
            app.error(_(u"Administrátory mazat nelze"))
            return None
        return True
