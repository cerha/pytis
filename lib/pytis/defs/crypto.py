# -*- coding: utf-8 -*-

# Copyright (C) 2014 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import copy

import pytis.data
import pytis.extensions
from pytis.presentation import Specification, Field, Binding
from pytis.util import rsa_encrypt, translations
from pytis import dbdefs

_ = translations('pytis')

class CryptoAreas(Specification):
    public = True
    table = dbdefs.CPytisCryptoNames
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
        db_key = pytis.extensions.dbfunction('pytis_crypto_db_key',
                                             ('key_name_', pytis.data.sval('pytis'),))
        import config
        connection_data = config.dbconnection
        key_id, key = pytis.extensions.crypto_admin_key(area, 'admin', connection_data)
        if not key_id or not key:
            pytis.form.run_dialog(pytis.form.Error,
                                  _("Nebyl nalezen klíč administrátora pro tuto oblast"))
            return
        while True:
            old_password = pytis.form.run_dialog(pytis.form.InputDialog, passwd=True,
                                                 prompt=_("Stávající heslo"), message='')
            if not old_password:
                return
            if pytis.extensions.check_crypto_password(key, old_password, connection_data):
                break
            pytis.form.run_dialog(pytis.form.Error, _("Chybné heslo"))
        encrypted_old_password = rsa_encrypt(db_key, old_password)
        while True:
            new_password = pytis.form.run_dialog(pytis.form.InputDialog, passwd=True,
                                                 prompt=_("Nové heslo"), message='')
            if not new_password:
                return
            new_password_2 = pytis.form.run_dialog(pytis.form.InputDialog, passwd=True,
                                                   prompt=_("Nové heslo ještě jednou"), message='')
            if new_password_2 is None:
                return
            if new_password == new_password_2:
                break
            pytis.form.run_dialog(pytis.form.Error, _("Zadaná nová hesla si neodpovídají"))
        encrypted_new_password = rsa_encrypt(db_key, new_password)
        function = pytis.data.DBFunctionDefault('pytis_crypto_change_password', connection_data)
        row = pytis.data.Row((('id_', key_id,),
                              ('old_psw', pytis.data.sval(encrypted_old_password),),
                              ('new_psw', pytis.data.sval(encrypted_new_password),),))
        if not function.call(row)[0][0].value():
            pytis.form.run_dialog(pytis.form.Error, _("Heslo se změnit nepodařilo"))
            return
        pytis.form.run_dialog(pytis.form.Message, _("Heslo bylo změněno"))

    def on_new_record(self, *args, **kwargs):
        return None
    def on_edit_record(self, row):
        return None
    def on_delete_record(self, row):
        return None

class Users(Specification):
    public = True
    table = dbdefs.EPytisCryptoKeys
    title = _(u"Uživatelé")
    def _customize_fields(self, fields):
        fields.append(Field('admin_address', _(u"E-mailová adresa administrátora"),
                            virtual=True, type=pytis.data.String(not_null=True)))
        fields.append(Field('admin_password', _(u"Heslo administrátora"),
                            virtual=True, type=pytis.data.Password(not_null=True, verify=False)))
    columns = ('username', 'fresh',)
    layout = ('name', 'username', 'admin_address', 'admin_password',)
    
    class _InsertForm(pytis.form.PopupInsertForm):
        def _commit_form(self, close=True):
            import config
            connection_data = config.dbconnection
            row = self._row
            error = pytis.extensions.add_crypto_user(row['name'].value(),
                                                     row['username'].value(),
                                                     'admin',
                                                     row['admin_password'].value(),
                                                     row['admin_address'].value(),
                                                     connection_data, transaction=self._transaction)
            if error:
                pytis.form.run_dialog(pytis.form.Error, "Error: %s" % (error,))
                return None
            if self._governing_transaction is None and self._transaction is not None:
                self._transaction.commit()
            return self._row

    def on_new_record(self, prefill=None, transaction=None):
        area = pytis.form.current_form()._main_form.current_row()['name']
        if prefill is None:
            prefill = {}
        else:
            prefill = copy.copy(prefill)
        prefill['name'] = area
        return pytis.form.run_form(self._InsertForm, 'crypto.Users',
                                   prefill=prefill, transaction=transaction)
        
    def on_edit_record(self, row):
        pytis.form.run_dialog(pytis.form.Warning, _(u"Uživatele lze jen přidávat a odebírat"))
        return None

    def on_delete_record(self, row):
        if row['username'].value() == 'admin':
            pytis.form.run_dialog(pytis.form.Error, _(u"Administrátory mazat nelze"))
            return None
        return True
