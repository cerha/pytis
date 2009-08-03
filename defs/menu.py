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

### Roles

class ApplicationRolePurposes(pytis.presentation.Specification):
    table = 'c_pytis_role_purposes'
    title = _("��ely rol�")
    fields = (
        Field('purposeid', _("Identifik�tor")),
        Field('purpose', _("Popis")),
        )
    cb = pytis.presentation.CodebookSpec(display='purpose')

class CommonApplicationRolePurposes(ApplicationRolePurposes):
    condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'admn'))

class ApplicationRolesSpecification(pytis.presentation.Specification):
    
    access_rights = pytis.data.AccessRights((None, (['admin_roles'], pytis.data.Permission.ALL)),)
    
    def _row_editable(self, row):
        return row['purposeid'].value() != 'admn'
    
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Spr�vcovsk� role nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['name'])

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Spr�vcovsk� role nelze mazat"))
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
    title = _("Role")
    fields = (
        Field('roleid', "",     # to allow binding of ApplicationRolesMembers
              virtual=True, computer=pytis.presentation.computer(lambda row, name: name)),
        Field('member', "",     # to allow binding of ApplicationRolesOwners
              virtual=True, computer=pytis.presentation.computer(lambda row, name: name)),
        Field('name', _("N�zev"),
              fixed=True,
              descr=_("Stru�n� n�zev role nebo u�ivatelsk� jm�no v datab�zi.")),
        Field('description', "Popis",
              descr=_("Popis ur�en� role.")),
        Field('purposeid', _("��el"), codebook='menu.CommonApplicationRolePurposes',
              fixed=True, selection_type=pytis.presentation.SelectionType.CHOICE,
              descr=_("��el role: norm�ln�, u�ivatelsk� ��et, spr�vcovsk�.")),
        Field('purpose', _("��el"),
              fixed=True,
              descr=_("V�znam role: norm�ln�, u�ivatelsk� ��et, spr�vcovsk�.")),              
        Field('deleted', _("Datum zru�en�"),
              fixed=True,
              descr=_("Je-li nastaveno, role je od dan�ho data neaktivn�.")),
        )
    columns = ('name', 'description', 'purpose', 'deleted',)
    layout = ('name', 'description', 'purposeid', 'deleted',)
    sorting = (('name', pytis.data.ASCENDENT,),)
    bindings = (pytis.presentation.Binding(_("Obsahuje role"), 'menu.ApplicationRolesMembers', id='members',
                                           binding_column='roleid'),
                pytis.presentation.Binding(_("Pat�� do rol�"), 'menu.ApplicationRolesOwners', id='owners',
                                           binding_column='member'),
                pytis.presentation.Binding(_("N�hled menu"), 'menu.ApplicationRoleMenu', id='menu',
                                           binding_column='roleid'),
                )
    
    def on_delete_record(self, row):
        if self._row_deleteable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Role nelze mazat, nastavte datum zru�en�"))
        return None

class ApplicationApplicationRoles(ApplicationRoles):
    table = 'ev_pytis_valid_roles'
    condition = pytis.data.EQ('purposeid', pytis.data.Value(pytis.data.String(), 'appl'))

class CommonApplicationRoles(ApplicationRoles):
    table = 'ev_pytis_valid_roles'
    condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'admn'))

class ApplicationRolesMembership(ApplicationRolesSpecification):
    table = 'ev_pytis_valid_role_members'
    title = "�lenstv� v rol�ch"
    fields = (
        Field('id', "", default=nextval('e_pytis_role_members_id_seq')),
        Field('roleid', _("Skupina"), fixed=True, codebook='menu.ApplicationApplicationRoles',
              descr=_("Role, do n� jsou zahrnuty jin� role.")),
        Field('member', _("Obsa�en� role"), fixed=True, codebook='menu.CommonApplicationRoles',
              descr=_("Role, kter� je �lenem skupinov� role.")),
        Field('purposeid', "", codebook='menu.ApplicationRolePurposes'),
        Field('mpurposeid', "", codebook='menu.ApplicationRolePurposes'),
        Field('description', _("Popis"),
              descr=_("Popis ur�en� role.")),
        Field('mdescription', _("Popis"),
              descr=_("Popis ur�en� role.")),
        )
    columns = ('roleid', 'member',)
    layout = ('roleid', 'member',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('member', pytis.data.ASCENDENT,),)

    def _row_editable(self, row):
        # This is here to prevent deletion of admin role memberships
        return row['purposeid'].value() != 'admn' and row['mpurposeid'].value() != 'admn'
    
    def on_edit_record(self, row):
        pytis.form.run_dialog(pytis.form.Warning, _("P�i�azen� rol� nelze editovat, jen p�id�vat a mazat"))
        return None

class ApplicationRolesOwners(ApplicationRolesMembership):
    title = _("Za�azen� do skupiny")
    columns = ('roleid', 'description',)

class ApplicationRolesMembers(ApplicationRolesMembership):
    title = _("P�i�azen� role")
    columns = ('member', 'mdescription',)

### Actions

class ApplicationActions(pytis.presentation.Specification):
    table = 'c_pytis_menu_actions'
    title = _("U�ivatelsk� akce")
    fields = (
        Field('name', _("Action"), editable=Editable.NEVER),
        Field('shortname', "", editable=Editable.NEVER),
        Field('description', _("Description")),
        )
    columns = ('name', 'description',)
    layout = ('name', 'description',)
    sorting = (('name', pytis.data.ASCENDENT,),)    
    cb = pytis.presentation.CodebookSpec(display='name')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)

### Menus

def _position_range(value):
    if value < 100 or value > 999:
        return _("Hodnota sm� b�t pouze v rozsahu 100-999")
    return None
class ApplicationMenu(pytis.presentation.Specification):
    table = 'ev_pytis_menu'
    title = _("Menu")
    fields = (
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', _("Id obsahuj�c� role")),
        Field('title', _("Titulek polo�ky menu")),
        Field('ititle', _("Titulek polo�ky menu")),
        Field('parent', _("Rodi�ovsk� menu"), codebook='menu.ApplicationMenus'),
        Field('position', _("Pozice v menu"), type=pytis.data.Integer(constraints=(_position_range,)),
              fixed=True, default=500),
        Field('indentation', ""),
        Field('fullposition', _("Pozice v cel�m menu")),
        Field('action', _("Nav�en� akce"), codebook='menu.ApplicationActions'),
        )
    columns = ('ititle', 'position', 'action',)
    layout = ('title', 'position', 'parent',)
    sorting = (('fullposition', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)
    def on_edit_record(self, row):
        if not row['indentation'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Polo�ku odpov�daj�c� cel�mu menu nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['menuid'])    
    def on_delete_record(self, row):
        if not row['position'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Polo�ku odpov�daj�c� cel�mu menu nelze smazat"))
            return None
        if row['name'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Koncov� polo�ky menu nelze mazat"))
            return None
        if row.data().select(condition=pytis.data.EQ('parent', row['menuid'])) > 0:
            pytis.form.run_dialog(pytis.form.Warning, _("Nelze mazat polo�ky obsahuj�c� jin� polo�ky"))
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete z�znam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
    
class ApplicationMenuM(ApplicationMenu):
    condition = pytis.data.NE('title', pytis.data.Value(pytis.data.String(), None))
    bindings = (pytis.presentation.Binding(_("Rozpis pr�v polo�ky menu"), 'menu.ApplicationMenuRights', id='raw_rights',
                                           binding_column='menuid'),
                pytis.presentation.Binding(_("Pr�va polo�ky menu"), 'menu.ApplicationSummaryRights', id='summary_rights',
                                           binding_column='menuid'),
                )

class ApplicationMenus(pytis.presentation.Specification):
    # This is almost the same as ApplicationMenu.
    # But we can't inherit from it because pytis would suffer from infinite
    # recursion.
    table = 'ev_pytis_menu_parents'
    title = _("Menu")
    fields = (
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', _("Id obsahuj�c� role")),
        Field('title', _("Titulek polo�ky menu")),
        Field('ititle', _("Titulek polo�ky menu")),
        Field('parent', _("Rodi�ovsk� menu")),
        Field('position', _("Pozice v menu"), type=pytis.data.Integer(constraints=(_position_range,)),
              fixed=True, default=500),
        Field('fullposition', _("Pozice v cel�m menu")),
        Field('action', _("Nav�en� akce"), codebook='menu.ApplicationActions'),
        )
    columns = ('ititle',)
    layout = ('title', 'position',)
    sorting = (('fullposition', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)

### Rights

class ApplicationRights(pytis.presentation.Specification):
    table = 'c_pytis_access_rights'
    title = "Seznam pr�v"
    fields = (
        Field('rightid', "Pr�vo", fixed=True),
        Field('description', "Popis"),
        )
    columns = ('rightid', 'description',)
    layout = ('rightid', 'description',)
    cb = pytis.presentation.CodebookSpec(display='description')

def _shortname_computer(row):
    # This is duplicate with genmenu, how to share it?
    action = row.cb_value('menuid', 'action').value()
    action_components = action.split('/')
    if action_components[0] == 'form':
        shortname = 'form/' + action_components[-1]
    else:
        shortname = action
    return shortname
class ApplicationMenuRights(pytis.presentation.Specification):
    table = 'ev_pytis_menu_rights'
    title = _("Pr�va")
    fields = (
        Field('id', "Id", default=nextval('e_pytis_action_rights_id_seq')),
        Field('menuid', "", codebook='menu.ApplicationMenu'),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('shortname', _("Primitivn� akce"), editable=pytis.presentation.Editable.NEVER,
              fixed=True, computer=pytis.presentation.computer(_shortname_computer)),
        Field('rightid', _("Pr�vo"), codebook='menu.ApplicationRights',
              fixed=True),
        Field('system', _("Syst�mov�"), fixed=True),
        Field('granted', _("Ano/Ne"), fixed=True, default=True),
        )
    columns = ('menuid', 'roleid', 'shortname', 'rightid', 'system', 'granted',)
    layout = ('shortname', 'roleid', 'rightid', 'granted',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('rightid', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def _row_editable(self, row):
        return not row['system'].value()
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Syst�mov� pr�va nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['id'])
    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Syst�mov� pr�va nelze mazat"))
            return False
        return True
    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete z�znam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])

class ApplicationSummaryRights(pytis.presentation.Specification):
    table = 'ev_pytis_summary_rights'
    title = _("Souhrnn� pr�va")
    fields = (
        Field('menuid', "", codebook='menu.ApplicationMenu'),
        Field('name', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('rights', _("Pr�va")),
        )
    columns = ('menuid', 'name', 'rights',)
    layout = ('menuid', 'name', 'rights',)
    sorting = (('name', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)

class ApplicationRoleMenu(pytis.presentation.Specification):
    table = 'ev_pytis_role_menu'
    title = _("Menu u�ivatele")
    fields = (
        Field('menuid', "", codebook='menu.ApplicationMenu'),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('rights', _("Pr�va")),
        Field('ititle', _("Titulek polo�ky menu"), fixed=True),
        Field('fullposition', _("Pozice v cel�m menu")),
        )
    columns = ('ititle', 'roleid', 'rights',)
    layout = ('ititle', 'roleid', 'rights',)
    sorting = (('fullposition', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
