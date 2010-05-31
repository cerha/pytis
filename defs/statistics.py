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
import pytis.presentation
from pytis.presentation import Binding, Field, Specification

class FormShortStatistics(Specification):
    public = True
    table = 'ev_pytis_form_short_summary'
    title = _("P�ehled pou��van�ch formul���")
    fields = (
        Field('form', _("Jm�no formul��e")),
        Field('class', _("T��da formul��e")),
        Field('n_users', _("Po�et u�ivatel�")),
        Field('n_open', _("Po�et otev�en�")),
        Field('avg_start', _("Pr�m�rn� doba startu")),
        Field('last_used', _("Posledn� spu�t�n�")),
        )
    bindings = (Binding('users', _("U�ivatel�"), 'statistics.FormUsers',
                        condition=(lambda row: pytis.data.AND(pytis.data.EQ('form', row['form']),
                                                              pytis.data.EQ('class', row['class'])))),
                )

class FormStatistics(Specification):
    public = True
    table = 'ev_pytis_form_summary'
    title = _("Podrobn� p�ehled pou��van�ch formul���")
    fields = (
        Field('form', _("Jm�no formul��e")),
        Field('class', _("T��da formul��e")),
        Field('info', _("Parametry formul��e")),
        Field('n_users', _("Po�et u�ivatel�")),
        Field('n_open', _("Po�et otev�en�")),
        Field('avg_start', _("Pr�m�rn� doba startu")),
        Field('last_used', _("Posledn� spu�t�n�")),
        )
    bindings = (Binding('users', _("U�ivatel�"), 'statistics.FormUsers',
                        condition=(lambda row: pytis.data.AND(pytis.data.EQ('form', row['form']),
                                                              pytis.data.EQ('class', row['class']),
                                                              pytis.data.EQ('info', row['info'])))),
                )

class FormUsers(Specification):
    public = True
    table = 'ev_pytis_form_users'
    title = _("U�ivatel� formul��e")
    fields = (
        Field('login', _("Login")),
        Field('form', _("Jm�no formul��e")),
        Field('class', _("T��da formul��e")),
        Field('info', _("Parametry formul��e")),
        Field('n_open', _("Po�et otev�en�")),
        Field('last_used', _("Posledn� spu�t�n�")),
        )
    columns = ('login', 'info', 'n_open', 'last_used',)

class FormUserList(Specification):
    public = True
    table = 'ev_pytis_form_user_list'
    title = _("U�ivatel�")
    fields = (
        Field('login', _("Login")),
        )
    bindings = (Binding('users', _("Formul��e"), 'statistics.FormUserStatistics',
                        condition=(lambda row: pytis.data.EQ('login', row['login']))),
                )

class FormUserStatistics(Specification):
    public = True
    table = 'ev_pytis_form_users'
    title = _("Formul��e u�ivatele")
    fields = (
        Field('login', _("Login")),
        Field('form', _("Jm�no formul��e")),
        Field('class', _("T��da formul��e")),
        Field('info', _("Parametry formul��e")),
        Field('n_open', _("Po�et otev�en�")),
        Field('last_used', _("Posledn� spu�t�n�")),
        )
    columns = ('form', 'class', 'info', 'n_open', 'last_used',)
