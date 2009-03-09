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
from pytis.presentation import Editable
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
        return row['purposeid'].value() != 'admn'
    
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
              fixed=True, selection_type=pytis.presentation.SelectionType.CHOICE,
              descr="Kód role: normální, uživatelský účet, správcovská."),              
        Field('purpose', "Účel",
              fixed=True,
              descr="Význam role: normální, uživatelský účet, správcovská."),              
        Field('deleted', "Datum zrušení",
              fixed=True,
              descr="Je-li nastaveno, role je od daného data neaktivní."),
        )
    columns = ('name', 'description', 'purpose', 'deleted',)
    layout = ('name', 'description', 'purposeid', 'deleted',)
    sorting = (('name', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='name')
    bindings = (pytis.presentation.Binding("Zahrnuté role", 'menu.ApplicationRolesMembers', id='members',
                                           condition=(lambda row: pytis.data.OR(pytis.data.EQ('roleid', row['roleid'],),))),
                pytis.presentation.Binding("Členství v rolích", 'menu.ApplicationRolesOwners', id='owners',
                                           condition=(lambda row: pytis.data.OR(pytis.data.EQ('member', row['roleid'],)))),
                )
    
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
        Field('description', "Popis",
              descr=_("Popis určení role.")),
        Field('mdescription', "Popis",
              descr=_("Popis určení role.")),
        )
    columns = ('name', 'mname',)
    layout = ('roleid', 'member',)
    sorting = (('name', pytis.data.ASCENDENT,), ('mname', pytis.data.ASCENDENT,),)

    def _row_editable(self, row):
        return row['purposeid'].value() != 1 and row['mpurposeid'] != 1

class ApplicationRolesOwners(ApplicationRolesMembership):
    title = "Členství v rolích"
    columns = ('name', 'description',)

class ApplicationRolesMembers(ApplicationRolesMembership):
    title = "Vlastněné role"
    columns = ('mname', 'mdescription',)

class ApplicationActions(pytis.presentation.Specification):
    table = 'c_pytis_menu_actions'
    title = "Uživatelské akce"
    fields = (
        Field('actionid', "Id"),
        Field('name', "Id", editable=Editable.NEVER),
        Field('description', "Id"),
        )
    columns = ('name', 'description',)
    layout = ('name', 'description',)
    sorting = (('name', pytis.data.ASCENDENT,),)    
    cb = pytis.presentation.CodebookSpec(display='name')
        
class ApplicationMenu(pytis.presentation.Specification):
    table = 'ev_pytis_menu'
    title = "Menu"
    fields = (
        Field('menuid', "Id", default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', "Id obsahující role"),
        Field('title', "Titulek položky menu"),
        Field('ititle', "Titulek položky menu"),
        Field('parent', "Rodičovské menu", codebook='menu.ApplicationMenus'),
        Field('position', "Pozice v menu",
              fixed=True),
        Field('fullposition', "Pozice v celém menu"),
        Field('actionid', "Navěšená akce", codebook='menu.ApplicationActions'),
        Field('action', "Navěšená akce"),
        )
    columns = ('ititle', 'position', 'action',)
    layout = ('title', 'position',)
    sorting = (('fullposition', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')
    bindings = {'menu.ApplicationMenuRights':
                    pytis.presentation.BindingSpec(title="Práva položky menu", binding_column='menuid')
                }
    
class ApplicationMenus(pytis.presentation.Specification):
    # This is almost the same as ApplicationMenu.
    # But we can't inherit from it because pytis would suffer from infinite
    # recursion.
    table = 'ev_pytis_menu_parents'
    title = "Menu"
    fields = (
        Field('menuid', "Id", default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', "Id obsahující role"),
        Field('title', "Titulek položky menu"),
        Field('ititle', "Titulek položky menu"),
        Field('parent', "Rodičovské menu"),
        Field('position', "Pozice v menu",
              fixed=True),
        Field('fullposition', "Pozice v celém menu"),
        Field('actionid', "Navěšená akce", codebook='menu.ApplicationActions'),
        Field('action', "Navěšená akce"),
        )
    columns = ('ititle', 'position', 'action',)
    layout = ('title', 'position',)
    sorting = (('fullposition', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')

class ApplicationRights(pytis.presentation.Specification):
    table = 'c_pytis_access_rights'
    title = "Seznam práv"
    fields = (
        Field('rightid', "Právo", fixed=True),
        Field('description', "Popis"),
        )
    columns = ('rightid', 'description',)
    layout = ('rightid', 'description',)
    cb = pytis.presentation.CodebookSpec(display='rightid')        

class ApplicationMenuRights(pytis.presentation.Specification):
    table = 'ev_pytis_menu_rights'
    title = "Práva"
    fields = (
        Field('id', "Id", default=nextval('e_pytis_menu_rights_id_seq')),
        Field('menuid', "Id Menu", codebook='menu.ApplicationMenu'),
        Field('title', "Položka menu"),
        Field('roleid', "Id Role", codebook='menu.ApplicationRoles'),
        Field('name', "Role", fixed=True),
        Field('rightid', "Právo", codebook='menu.ApplicationRights',
              fixed=True),
        Field('granted', "Povoleno/zakázáno", fixed=True),
        )
    columns = ('title', 'name', 'rightid', 'granted',)
    layout = ('menuid', 'roleid', 'rightid', 'granted',)
    sorting = (('name', pytis.data.ASCENDENT,), ('rightid', pytis.data.ASCENDENT,),)

