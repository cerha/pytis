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

def _ititle_computer(row, title, position):
    indentation = ' ' * len(position)
    return indentation + title
class ApplicationMenu(pytis.presentation.Specification):
    table = 'ev_pytis_menu'
    title = _("Menu")
    fields = (
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', _("Id obsahuj�c� role")),
        Field('title', _("Titulek polo�ky menu")),
        Field('ititle', _("Titulek polo�ky menu"), type=pytis.data.String(), virtual=True,
              computer=pytis.presentation.computer(_ititle_computer)),
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions'),
        Field('action', _("Nav�en� akce"), codebook='menu.ApplicationActions',
              descr=_("Akce aplikace vyvolan� polo�kou menu")),
        Field('locked', _("Z�kaz editace"), fixed=True, editable=pytis.presentation.Editable.NEVER),
        )
    columns = ('ititle', 'action', 'locked',)
    layout = ('title', 'position',)
    sorting = (('position', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)
    def on_edit_record(self, row):
        if row['locked'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Tuto polo�ku menu nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['menuid'])    
    def on_delete_record(self, row):
        if row['locked'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Tuto polo�ku menu nelze smazat"))
            return None
        if row['name'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Koncov� polo�ky menu nelze mazat"))
            return None
        wm_value = pytis.data.WMValue(pytis.data.String(), row['position'].value() + '?*')
        if row.data().select(condition=pytis.data.WM('position', wm_value)) > 0:
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
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)

def _iposition_computer(row, title, position):
    indentation = ' ' * len(position)
    if position and position[-1] in '02468':
        result = indentation + '++++'
    else:
        result = indentation + (title or '')
    return result
class ApplicationMenuPositions(pytis.presentation.Specification):
    table = 'ev_pytis_menu_positions'
    title = _("Menu")
    fields = (
        Field('position', _("Pozice v menu"), fixed=True),
        Field('title', _("Titulek polo�ky menu")),
        Field('ititle', _("Titulek polo�ky menu"), type=pytis.data.String(), virtual=True,
              computer=pytis.presentation.computer(_iposition_computer)),
        )
    columns = ('position', 'ititle',)
    layout = ('title', 'position',)
    sorting = (('position', pytis.data.ASCENDENT,),)
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
    action_components = (action or '').split('/')
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
              fixed=True, computer=pytis.presentation.computer(_shortname_computer),
              descr=_("Identifik�tor akce souvisej�c� s danou polo�kou menu")),
        Field('rightid', _("Pr�vo"), codebook='menu.ApplicationRights',
              fixed=True,
              descr=_("P�id�len� nebo odebran� pr�vo")),
        Field('system', _("Syst�mov�"), fixed=True,
              descr=_("Jde o nem�nn� pr�vo definov�no tv�rcem aplikace?")),
        Field('granted', _("Ano/Ne"), fixed=True, default=True,
              descr=_("Je pr�vo povoleno (ano) nebo zak�z�no (ne)?")),
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
        Field('rights', _("Pr�va"), fixed=True),
        Field('rights_show', _("Menu"), fixed=True,
              descr=_("Zobrazen� v menu")),
        Field('rights_view', _("N�hled"), fixed=True,
              descr=_("Zobrazen� formul��e")),
        Field('rights_insert', _("Vlo�en�"), fixed=True,
              descr=_("Vlo�en� nov�ho z�znamu")),
        Field('rights_update', _("Editace"), fixed=True,
              descr=_("Zm�na hodnot existuj�c�ho z�znamu")),
        Field('rights_delete', _("Smaz�n�"), fixed=True,
              descr=_("Smaz�n� existuj�c�ho z�znamu")),
        Field('rights_print', _("Tisk"), fixed=True,
              descr=_("Tisk formul��e")),
        Field('rights_export', _("Export"), fixed=True,
              descr=_("Export formul��e do souboru")),
        Field('rights_call', _("Spu�t�n�"), fixed=True,
              descr=_("Spu�t�n� funkce (net�k� se formul���)")),
        )
    columns = ('menuid', 'name', 'rights_show', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('menuid', 'name', 'rights_show', 'rights_view', 'rights_insert', 'rights_update',
              'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    sorting = (('name', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def on_new_record(self, *args, **kwargs):
        return None
    def on_edit_record(self, row):
        return None
    def on_delete_record(self, row):
        return None

class ApplicationRoleMenu(pytis.presentation.Specification):
    table = 'ev_pytis_role_menu'
    title = _("Menu u�ivatele")
    fields = (
        Field('menuid', "", codebook='menu.ApplicationMenu'),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('title', _("Titulek polo�ky menu"), fixed=True),
        Field('ititle', _("Titulek polo�ky menu"), type=pytis.data.String(), virtual=True,
              computer=pytis.presentation.computer(_ititle_computer)),
        Field('position', _("Pozice v menu")),
        Field('rights', _("Pr�va")),
        Field('rights_show', _("Menu"), fixed=True,
              descr=_("Zobrazen� v menu")),
        Field('rights_view', _("N�hled"), fixed=True,
              descr=_("Zobrazen� formul��e")),
        Field('rights_insert', _("Vlo�en�"), fixed=True,
              descr=_("Vlo�en� nov�ho z�znamu")),
        Field('rights_update', _("Editace"), fixed=True,
              descr=_("Zm�na hodnot existuj�c�ho z�znamu")),
        Field('rights_delete', _("Smaz�n�"), fixed=True,
              descr=_("Smaz�n� existuj�c�ho z�znamu")),
        Field('rights_print', _("Tisk"), fixed=True,
              descr=_("Tisk formul��e")),
        Field('rights_export', _("Export"), fixed=True,
              descr=_("Export formul��e do souboru")),
        Field('rights_call', _("Spu�t�n�"), fixed=True,
              descr=_("Spu�t�n� funkce (net�k� se formul���)")),
        )
    columns = ('ititle', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    sorting = (('position', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def on_new_record(self, *args, **kwargs):
        return None
    def on_edit_record(self, row):
        return None
    def on_delete_record(self, row):
        return None
