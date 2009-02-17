# -*- coding: iso-8859-2 -*-

# Copyright (C) 2009 Brailcom, o.p.s.
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

import pytis.presentation
from pytis.extensions import Field

class ApplicationRolePurposes(pytis.presentation.Specification):
    table = 'c_pytis_role_purposes'
    title = "Účely rolí"
    fields = (
        Field('purposeid', "Id"),
        Field('purpose', "Účel"),
        )

class ApplicationRoles(pytis.presentation.Specification):
    table = 'e_pytis_roles'
    title = "Role"
    fields = (
        Field('roleid', "Id",
              descr="Interní číselný identifikátor role."),
        Field('name', "Název",
              fixed=True,
              descr="Stručný název role nebo uživatelské jméno v databázi."),
        Field('description', "Popis",
              descr=_("Popis určení role.")),
        Field('purposeid', "Účel", codebook='menu.ApplicationRolePurposes',
              fixed=True,
              descr="Význam role: normální, uživatelský účet, správcovská."),
        Field('deleted', "Datum zrušení",
              fixed=True,
              descr="Je-li nastaveno, role je od daného data neaktivní."),
        )
    columns = ('name', 'description', 'purposeid', 'deleted',)
    sorting = (('name', pytis.data.ASCENDENT,),)
