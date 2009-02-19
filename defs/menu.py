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

import pytis.data
import pytis.form
import pytis.presentation
from pytis.extensions import Field, nextval

class ApplicationRolePurposes(pytis.presentation.Specification):
    table = 'c_pytis_role_purposes'
    title = "Účely rolí"
    fields = (
        Field('purposeid', "Id"),
        Field('purpose', "Účel"),
        )
    cb = pytis.presentation.CodebookSpec(display='purpose')

class ApplicationRolesSpecification(pytis.presentation.Specification):
    
    def _row_editable(self, row):
        return row['purposeid'].value() != 1
    
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, "Správcovské role nelze editovat")
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['roleid'])

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, "Správcovské role nelze mazat")
            return False
        return True
        
    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete záznam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
    
class ApplicationRoles(ApplicationRolesSpecification):
    table = 'ev_pytis_roles'
    title = "Role"
    fields = (
        Field('roleid', "Id", default=nextval('e_pytis_roles_roleid_seq')),
        Field('name', "Název",
              fixed=True,
              descr="Stručný název role nebo uživatelské jméno v databázi."),
        Field('description', "Popis",
              descr=_("Popis určení role.")),
        Field('purposeid', "Kód účelu", codebook='menu.ApplicationRolePurposes',
              fixed=True,
              descr="Kód role: normální, uživatelský účet, správcovská."),              
        Field('purpose', "Účel",
              fixed=True,
              descr="Význam role: normální, uživatelský účet, správcovská."),              
        Field('deleted', "Datum zrušení",
              fixed=True,
              descr="Je-li nastaveno, role je od daného data neaktivní."),
        )
    columns = ('name', 'description', 'purpose', 'deleted',)
    layout = ('name', 'description', 'purposeid',)
    sorting = (('name', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='name')
    bindings = {'menu.ApplicationRolesMembership':
                    pytis.presentation.BindingSpec(
                      title=_("Role members"),
                      condition=(lambda row: pytis.data.OR(pytis.data.EQ('roleid', row['roleid'],),
                                                           pytis.data.EQ('member', row['roleid'],))))
                }
    
    def on_delete_record(self, row):
        if self._row_deleteable(row):
            pytis.form.run_dialog(pytis.form.Warning, "Role nelze mazat, nastavte datum zrušení")
        return None

class ApplicationRolesMembership(ApplicationRolesSpecification):
    table = 'ev_pytis_role_members'
    title = "Členství v rolích"
    fields = (
        Field('id', "Id", default=nextval('e_pytis_role_members_id_seq')),
        Field('roleid', "Id obsahující role", codebook='menu.ApplicationRoles'),
        Field('member', "Id obsažené role", codebook='menu.ApplicationRoles'),
        Field('name', "Role jako skupina",
              fixed=True,
              descr="Role, do níž jsou zahrnuty jiné role."),
        Field('mname', "Role jako člen",
              fixed=True,
              descr="Role, která je členem skupinové role."),
        Field('purposeid', "Účel", codebook='menu.ApplicationRolePurposes'),
        Field('mpurposeid', "Účel", codebook='menu.ApplicationRolePurposes'),
        )
    columns = ('name', 'mname',)
    layout = ('roleid', 'member',)
    sorting = (('name', pytis.data.ASCENDENT,), ('mname', pytis.data.ASCENDENT,),)

    def _row_editable(self, row):
        return row['purposeid'].value() != 1 and row['mpurposeid'] != 1
