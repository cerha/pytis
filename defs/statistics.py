# -*- coding: iso-8859-2 -*-

# Copyright (C) 2010 Brailcom, o.p.s.
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

import pytis.data
import pytis.extensions
import pytis.form
import pytis.presentation
from pytis.presentation import Binding, Field, Specification
import pytis.util

class FormShortStatistics(Specification):
    public = True
    table = 'ev_pytis_form_short_summary'
    title = _("Přehled používaných formulářů")
    fields = (
        Field('form', _("Jméno formuláře")),
        Field('class', _("Třída formuláře")),
        Field('n_users', _("Počet uživatelů")),
        Field('n_open', _("Počet otevření")),
        Field('avg_start', _("Průměrná doba startu")),
        Field('last_used', _("Poslední spuštění")),
        )
    bindings = (Binding('users', _("Uživatelé"), 'statistics.FormUsers',
                        condition=(lambda row: pytis.data.AND(pytis.data.EQ('form', row['form']),
                                                              pytis.data.EQ('class', row['class'])))),
                )

class FormStatistics(Specification):
    public = True
    table = 'ev_pytis_form_summary'
    title = _("Podrobný přehled používaných formulářů")
    fields = (
        Field('form', _("Jméno formuláře")),
        Field('class', _("Třída formuláře")),
        Field('info', _("Parametry formuláře")),
        Field('n_users', _("Počet uživatelů")),
        Field('n_open', _("Počet otevření")),
        Field('avg_start', _("Průměrná doba startu")),
        Field('last_used', _("Poslední spuštění")),
        )
    bindings = (Binding('users', _("Uživatelé"), 'statistics.FormUsers',
                        condition=(lambda row: pytis.data.AND(pytis.data.EQ('form', row['form']),
                                                              pytis.data.EQ('class', row['class']),
                                                              pytis.data.EQ('info', row['info'])))),
                )

class FormUsers(Specification):
    public = True
    table = 'ev_pytis_form_users'
    title = _("Uživatelé formuláře")
    fields = (
        Field('login', _("Login")),
        Field('form', _("Jméno formuláře")),
        Field('class', _("Třída formuláře")),
        Field('info', _("Parametry formuláře")),
        Field('n_open', _("Počet otevření")),
        Field('last_used', _("Poslední spuštění")),
        )
    columns = ('login', 'info', 'n_open', 'last_used',)

class FormUserList(Specification):
    public = True
    table = 'ev_pytis_form_user_list'
    title = _("Uživatelé")
    fields = (
        Field('login', _("Login")),
        )
    bindings = (Binding('users', _("Formuláře"), 'statistics.FormUserStatistics',
                        condition=(lambda row: pytis.data.EQ('login', row['login']))),
                )

class FormUserStatistics(Specification):
    public = True
    table = 'ev_pytis_form_users'
    title = _("Formuláře uživatele")
    fields = (
        Field('login', _("Login")),
        Field('form', _("Jméno formuláře")),
        Field('class', _("Třída formuláře")),
        Field('info', _("Parametry formuláře")),
        Field('n_open', _("Počet otevření")),
        Field('last_used', _("Poslední spuštění")),
        )
    columns = ('form', 'class', 'info', 'n_open', 'last_used',)

class FormUserStatisticsNoinfo(Specification):
    public = True
    table = 'ev_pytis_form_users_noinfo'
    title = _("Formuláře uživatele")
    fields = (
        Field('shortname', _("Akce")),
        Field('login', _("Login")),
        Field('form', _("Jméno formuláře")),
        Field('class', _("Třída formuláře")),
        Field('n_open', _("Počet otevření")),
        Field('last_used', _("Poslední spuštění")),
        )
    columns = ('login', 'class', 'n_open', 'last_used',)
    
    def on_new_record(self, prefill=None, transaction=None):
        try:
            pytis.form.run_form(pytis.form.PopupEditForm, 'Nastaveni.BvUsersCfg',
                                select_row=pytis.extensions.cfg_param('id', cfgspec='Nastaveni.BvUsersCfg'))
        except pytis.util.ResolverFileError:
            pytis.form.run_dialog(pytis.form.Warning, _("Tato databáze neobsahuje uživatelskou konfiguraci"))
