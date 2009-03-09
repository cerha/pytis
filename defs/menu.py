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
    title = "��ely rol�"
    fields = (
        Field('purposeid', "Id"),
        Field('purpose', "��el"),
        )
    cb = pytis.presentation.CodebookSpec(display='purpose')

class CommonApplicationRolePurposes(ApplicationRolePurposes):
    condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'admn'))

class ApplicationRolesSpecification(pytis.presentation.Specification):
    
    def _row_editable(self, row):
        return row['purposeid'].value() != 'admn'
    
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, "Spr�vcovsk� role nelze editovat")
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['roleid'])

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, "Spr�vcovsk� role nelze mazat")
            return False
        return True
        
    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete z�znam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
    
class ApplicationRoles(ApplicationRolesSpecification):
    table = 'ev_pytis_roles'
    title = "Role"
    fields = (
        Field('roleid', "Id", default=nextval('e_pytis_roles_roleid_seq')),
        Field('member', "",     # to allow binding of ApplicationRolesOwners
              virtual=True, computer=pytis.presentation.computer(lambda row, roleid: roleid)),
        Field('name', "N�zev",
              fixed=True,
              descr="Stru�n� n�zev role nebo u�ivatelsk� jm�no v datab�zi."),
        Field('description', "Popis",
              descr=_("Popis ur�en� role.")),
        Field('purposeid', "K�d ��elu", codebook='menu.CommonApplicationRolePurposes',
              fixed=True, selection_type=pytis.presentation.SelectionType.CHOICE,
              descr="K�d role: norm�ln�, u�ivatelsk� ��et, spr�vcovsk�."),
        Field('purpose', "��el",
              fixed=True,
              descr="V�znam role: norm�ln�, u�ivatelsk� ��et, spr�vcovsk�."),              
        Field('deleted', "Datum zru�en�",
              fixed=True,
              descr="Je-li nastaveno, role je od dan�ho data neaktivn�."),
        )
    columns = ('name', 'description', 'purpose', 'deleted',)
    layout = ('name', 'description', 'purposeid', 'deleted',)
    sorting = (('name', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='name')
    bindings = (pytis.presentation.Binding("Zahrnut� role", 'menu.ApplicationRolesMembers', id='members',
                                           binding_column='roleid'),
                pytis.presentation.Binding("�lenstv� v rol�ch", 'menu.ApplicationRolesOwners', id='owners',
                                           binding_column='member'),
                )
    
    def on_delete_record(self, row):
        if self._row_deleteable(row):
            pytis.form.run_dialog(pytis.form.Warning, "Role nelze mazat, nastavte datum zru�en�")
        return None

class ApplicationApplicationRoles(ApplicationRoles):
    condition = pytis.data.EQ('purposeid', pytis.data.Value(pytis.data.String(), 'appl'))

class CommonApplicationRoles(ApplicationRoles):
    condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'admn'))

class ApplicationRolesMembership(ApplicationRolesSpecification):
    table = 'ev_pytis_valid_role_members'
    title = "�lenstv� v rol�ch"
    fields = (
        Field('id', "Id", default=nextval('e_pytis_role_members_id_seq')),
        Field('roleid', "Id obsahuj�c� role", codebook='menu.ApplicationApplicationRoles'),
        Field('member', "Id obsa�en� role", codebook='menu.CommonApplicationRoles'),
        Field('name', "Role jako skupina",
              fixed=True,
              descr="Role, do n�� jsou zahrnuty jin� role."),
        Field('mname', "Role jako �len",
              fixed=True,
              descr="Role, kter� je �lenem skupinov� role."),
        Field('purposeid', "��el", codebook='menu.ApplicationRolePurposes'),
        Field('mpurposeid', "��el", codebook='menu.ApplicationRolePurposes'),
        Field('description', "Popis",
              descr=_("Popis ur�en� role.")),
        Field('mdescription', "Popis",
              descr=_("Popis ur�en� role.")),
        )
    columns = ('name', 'mname',)
    layout = ('roleid', 'member',)
    sorting = (('name', pytis.data.ASCENDENT,), ('mname', pytis.data.ASCENDENT,),)

    def _row_editable(self, row):
        # This is here to prevent deletion of admin role memberships
        return row['purposeid'].value() != 'admn' and row['mpurposeid'].value() != 'admn'
    
    def on_edit_record(self, row):
        pytis.form.run_dialog(pytis.form.Warning, _("P�i�azen� rol� nelze editovat, jen p�id�vat a mazat"))
        return None

class ApplicationRolesOwners(ApplicationRolesMembership):
    title = "�lenstv� v rol�ch"
    columns = ('name', 'description',)

class ApplicationRolesMembers(ApplicationRolesMembership):
    title = "Vlastn�n� role"
    columns = ('mname', 'mdescription',)

class ApplicationActions(pytis.presentation.Specification):
    table = 'c_pytis_menu_actions'
    title = "U�ivatelsk� akce"
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
        Field('name', "Id obsahuj�c� role"),
        Field('title', "Titulek polo�ky menu"),
        Field('ititle', "Titulek polo�ky menu"),
        Field('parent', "Rodi�ovsk� menu", codebook='menu.ApplicationMenus'),
        Field('position', "Pozice v menu",
              fixed=True),
        Field('fullposition', "Pozice v cel�m menu"),
        Field('actionid', "Nav�en� akce", codebook='menu.ApplicationActions'),
        Field('action', "Nav�en� akce"),
        )
    columns = ('ititle', 'position', 'action',)
    layout = ('title', 'position',)
    sorting = (('fullposition', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')
    bindings = {'menu.ApplicationMenuRights':
                    pytis.presentation.BindingSpec(title="Pr�va polo�ky menu", binding_column='menuid')
                }
    
class ApplicationMenus(pytis.presentation.Specification):
    # This is almost the same as ApplicationMenu.
    # But we can't inherit from it because pytis would suffer from infinite
    # recursion.
    table = 'ev_pytis_menu_parents'
    title = "Menu"
    fields = (
        Field('menuid', "Id", default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', "Id obsahuj�c� role"),
        Field('title', "Titulek polo�ky menu"),
        Field('ititle', "Titulek polo�ky menu"),
        Field('parent', "Rodi�ovsk� menu"),
        Field('position', "Pozice v menu",
              fixed=True),
        Field('fullposition', "Pozice v cel�m menu"),
        Field('actionid', "Nav�en� akce", codebook='menu.ApplicationActions'),
        Field('action', "Nav�en� akce"),
        )
    columns = ('ititle', 'position', 'action',)
    layout = ('title', 'position',)
    sorting = (('fullposition', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')

class ApplicationRights(pytis.presentation.Specification):
    table = 'c_pytis_access_rights'
    title = "Seznam pr�v"
    fields = (
        Field('rightid', "Pr�vo", fixed=True),
        Field('description', "Popis"),
        )
    columns = ('rightid', 'description',)
    layout = ('rightid', 'description',)
    cb = pytis.presentation.CodebookSpec(display='rightid')        

class ApplicationMenuRights(pytis.presentation.Specification):
    table = 'ev_pytis_menu_rights'
    title = "Pr�va"
    fields = (
        Field('id', "Id", default=nextval('e_pytis_menu_rights_id_seq')),
        Field('menuid', "Id Menu", codebook='menu.ApplicationMenu'),
        Field('title', "Polo�ka menu"),
        Field('roleid', "Id Role", codebook='menu.ApplicationRoles'),
        Field('name', "Role", fixed=True),
        Field('rightid', "Pr�vo", codebook='menu.ApplicationRights',
              fixed=True),
        Field('granted', "Povoleno/zak�z�no", fixed=True),
        )
    columns = ('title', 'name', 'rightid', 'granted',)
    layout = ('menuid', 'roleid', 'rightid', 'granted',)
    sorting = (('name', pytis.data.ASCENDENT,), ('rightid', pytis.data.ASCENDENT,),)

