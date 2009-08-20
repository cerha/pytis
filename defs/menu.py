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
    title = _("Účely rolí")
    fields = (
        Field('purposeid', _("Identifikátor")),
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
            pytis.form.run_dialog(pytis.form.Warning, _("Správcovské role nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['name'])

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Správcovské role nelze mazat"))
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
    title = _("Role")
    fields = (
        Field('roleid', "",     # to allow binding of ApplicationRolesMembers
              virtual=True, computer=pytis.presentation.computer(lambda row, name: name)),
        Field('member', "",     # to allow binding of ApplicationRolesOwners
              virtual=True, computer=pytis.presentation.computer(lambda row, name: name)),
        Field('name', _("Název"),
              fixed=True,
              descr=_("Stručný název role nebo uživatelské jméno v databázi.")),
        Field('description', "Popis",
              descr=_("Popis určení role.")),
        Field('purposeid', _("Účel"), codebook='menu.CommonApplicationRolePurposes',
              fixed=True, selection_type=pytis.presentation.SelectionType.CHOICE,
              descr=_("Účel role: normální, uživatelský účet, správcovská.")),
        Field('purpose', _("Účel"),
              fixed=True,
              descr=_("Význam role: normální, uživatelský účet, správcovská.")),              
        Field('deleted', _("Datum zrušení"),
              fixed=True,
              descr=_("Je-li nastaveno, role je od daného data neaktivní.")),
        )
    columns = ('name', 'description', 'purpose', 'deleted',)
    layout = ('name', 'description', 'purposeid', 'deleted',)
    sorting = (('name', pytis.data.ASCENDENT,),)
    bindings = (pytis.presentation.Binding(_("Obsahuje role"), 'menu.ApplicationRolesMembers', id='members',
                                           binding_column='roleid'),
                pytis.presentation.Binding(_("Patří do rolí"), 'menu.ApplicationRolesOwners', id='owners',
                                           binding_column='member'),
                pytis.presentation.Binding(_("Náhled menu"), 'menu.ApplicationRoleMenu', id='menu',
                                           binding_column='roleid'),
                )
    
    def on_delete_record(self, row):
        if self._row_deleteable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Role nelze mazat, nastavte datum zrušení"))
        return None

class ApplicationApplicationRoles(ApplicationRoles):
    table = 'ev_pytis_valid_roles'
    condition = pytis.data.EQ('purposeid', pytis.data.Value(pytis.data.String(), 'appl'))

class CommonApplicationRoles(ApplicationRoles):
    table = 'ev_pytis_valid_roles'
    condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'admn'))

class ApplicationRolesMembership(ApplicationRolesSpecification):
    table = 'ev_pytis_valid_role_members'
    title = "Členství v rolích"
    fields = (
        Field('id', "", default=nextval('e_pytis_role_members_id_seq')),
        Field('roleid', _("Skupina"), fixed=True, codebook='menu.ApplicationApplicationRoles',
              descr=_("Role, do níž jsou zahrnuty jiné role.")),
        Field('member', _("Obsažená role"), fixed=True, codebook='menu.CommonApplicationRoles',
              descr=_("Role, která je členem skupinové role.")),
        Field('purposeid', "", codebook='menu.ApplicationRolePurposes'),
        Field('mpurposeid', "", codebook='menu.ApplicationRolePurposes'),
        Field('description', _("Popis"),
              descr=_("Popis určení role.")),
        Field('mdescription', _("Popis"),
              descr=_("Popis určení role.")),
        )
    columns = ('roleid', 'member',)
    layout = ('roleid', 'member',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('member', pytis.data.ASCENDENT,),)

    def _row_editable(self, row):
        # This is here to prevent deletion of admin role memberships
        return row['purposeid'].value() != 'admn' and row['mpurposeid'].value() != 'admn'
    
    def on_edit_record(self, row):
        pytis.form.run_dialog(pytis.form.Warning, _("Přiřazení rolí nelze editovat, jen přidávat a mazat"))
        return None

class ApplicationRolesOwners(ApplicationRolesMembership):
    title = _("Zařazení do skupiny")
    columns = ('roleid', 'description',)

class ApplicationRolesMembers(ApplicationRolesMembership):
    title = _("Přiřazení role")
    columns = ('member', 'mdescription',)

### Actions

class ApplicationActions(pytis.presentation.Specification):
    table = 'c_pytis_menu_actions'
    title = _("Uživatelské akce")
    fields = (
        Field('fullname', _("Action"), editable=Editable.NEVER),
        Field('shortname', "", editable=Editable.NEVER),
        Field('description', _("Description")),
        )
    columns = ('fullname', 'description',)
    layout = ('fullname', 'description',)
    sorting = (('fullname', pytis.data.ASCENDENT,),)    
    cb = pytis.presentation.CodebookSpec(display='fullname')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)

class ApplicationShortActions(pytis.presentation.Specification):
    table = 'c_pytis_menu_actions'
    title = _("Uživatelské akce")
    fields = (
        Field('shortname', "", editable=Editable.NEVER),
        )
    sorting = (('shortname', pytis.data.ASCENDENT,),)    
    cb = pytis.presentation.CodebookSpec(display='shortname')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)

class SummaryIds(pytis.presentation.Specification):
    table = 'ev_pytis_summary_rights'
    fields = (
        Field('summaryid', _("")),
        )
    cb = pytis.presentation.CodebookSpec(display='summaryid')
    access_rights = pytis.data.AccessRights((None, (None, pytis.data.Permission.VIEW)),)
    
### Menus

class _Title(pytis.presentation.PrettyFoldable, pytis.data.String):
    def __init__(self, **kwargs):
        super(_Title, self).__init__(tree_column_id='position', **kwargs)

def _xaction_computer(row, fullname):
    if fullname is None or fullname.startswith('menu/'):
        result = ''
    else:
        result = fullname
    return result
class ApplicationMenu(pytis.presentation.Specification):
    table = 'ev_pytis_menu'
    title = _("Menu")
    fields = (
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', _("Id obsahující role")),
        Field('title', _("Titulek položky menu"), type=_Title()),
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions'),
        Field('fullname', _("Navěšená akce"), codebook='menu.ApplicationActions',
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('xaction', _("Navěšená akce"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('locked', _("Zákaz editace"), fixed=True, editable=pytis.presentation.Editable.NEVER),
        )
    columns = ('title', 'xaction', 'locked',)
    layout = ('title', 'position',)
    sorting = (('position', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)
    def on_edit_record(self, row):
        if row['locked'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Tuto položku menu nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['menuid'])    
    def on_delete_record(self, row):
        if row['locked'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Tuto položku menu nelze smazat"))
            return None
        if row['name'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Koncové položky menu nelze mazat"))
            return None
        wm_value = pytis.data.WMValue(pytis.data.String(), row['position'].value() + '?*')
        if row.data().select(condition=pytis.data.WM('position', wm_value)) > 0:
            pytis.form.run_dialog(pytis.form.Warning, _("Nelze mazat položky obsahující jiné položky"))
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete záznam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
    
class ApplicationMenuM(ApplicationMenu):
    table = 'ev_pytis_menu_structure'
    fields = (
        Field('fullname', _("Navěšená akce"), codebook='menu.ApplicationActions',
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('shortname', _(""), codebook='menu.ApplicationShortActions'),
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq')),
        Field('summaryid', _("Id")),
        Field('title', _("Titulek položky menu"), type=_Title()),
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions'),
        Field('xaction', _("Navěšená akce"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('locked', _("Zákaz editace"), fixed=True, editable=pytis.presentation.Editable.NEVER),
        )
    columns = ('title', 'fullname', 'locked',)
    bindings = (pytis.presentation.Binding(_("Rozpis práv položky menu"), 'menu.ApplicationMenuRights', id='raw_rights',
                                           binding_column='shortname'),
                pytis.presentation.Binding(_("Práva položky menu"), 'menu.ApplicationSummaryRights', id='summary_rights',
                                           binding_column='summaryid'),
                )
    access_rights = pytis.data.AccessRights((None, (['admin_menu'],
                                                    pytis.data.Permission.VIEW,
                                                    pytis.data.Permission.EXPORT,
                                                    pytis.data.Permission.PRINT)),)

class ApplicationMenuPositions(pytis.presentation.Specification):
    table = 'ev_pytis_menu_positions'
    title = _("Menu")
    fields = (
        Field('position', _("Pozice v menu"), fixed=True),
        Field('title', _("Titulek položky menu"), type=_Title()),
        )
    columns = ('title', 'position',)
    layout = ('title', 'position',)
    sorting = (('position', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)

### Rights

class ApplicationRights(pytis.presentation.Specification):
    table = 'c_pytis_access_rights'
    title = "Seznam práv"
    fields = (
        Field('rightid', "Právo", fixed=True),
        Field('description', "Popis"),
        )
    columns = ('rightid', 'description',)
    layout = ('rightid', 'description',)
    cb = pytis.presentation.CodebookSpec(display='description')

class ApplicationMenuRights(pytis.presentation.Specification):
    table = 'e_pytis_action_rights'
    title = _("Práva")
    fields = (
        Field('id', "Id", default=nextval('e_pytis_action_rights_id_seq')),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('shortname', _("Akce"), editable=pytis.presentation.Editable.NEVER,
              codebook='menu.ApplicationShortActions',
              descr=_("Identifikátor akce související s danou položkou menu")),
        Field('rightid', _("Právo"), codebook='menu.ApplicationRights',
              fixed=True,
              descr=_("Přidělené nebo odebrané právo")),
        Field('system', _("Systémové"), fixed=True,
              descr=_("Jde o neměnné právo definováno tvůrcem aplikace?")),
        Field('granted', _("Ano/Ne"), fixed=True, default=True,
              descr=_("Je právo povoleno (ano) nebo zakázáno (ne)?")),
        )
    columns = ('roleid', 'rightid', 'system', 'granted',)
    layout = ('shortname', 'roleid', 'rightid', 'granted',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('rightid', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def _row_editable(self, row):
        return not row['system'].value()
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Systémová práva nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['id'])
    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Systémová práva nelze mazat"))
            return False
        return True
    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete záznam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])

class ApplicationSummaryRights(pytis.presentation.Specification):
    table = 'ev_pytis_summary_rights'
    title = _("Souhrnná práva")
    fields = (
        Field('summaryid', "", codebook='menu.SummaryIds', fixed=True),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('rights', _("Práva"), fixed=True),
        Field('rights_show', _("Menu"), fixed=True,
              descr=_("Zobrazení v menu")),
        Field('rights_view', _("Náhled"), fixed=True,
              descr=_("Zobrazení formuláře")),
        Field('rights_insert', _("Vložení"), fixed=True,
              descr=_("Vložení nového záznamu")),
        Field('rights_update', _("Editace"), fixed=True,
              descr=_("Změna hodnot existujícího záznamu")),
        Field('rights_delete', _("Smazání"), fixed=True,
              descr=_("Smazání existujícího záznamu")),
        Field('rights_print', _("Tisk"), fixed=True,
              descr=_("Tisk formuláře")),
        Field('rights_export', _("Export"), fixed=True,
              descr=_("Export formuláře do souboru")),
        Field('rights_call', _("Spuštění"), fixed=True,
              descr=_("Spuštění funkce (netýká se formulářů)")),
        )
    columns = ('roleid', 'rights_show', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('roleid', 'rights_show', 'rights_view', 'rights_insert', 'rights_update',
              'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    sorting = (('roleid', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def on_new_record(self, *args, **kwargs):
        return None
    def on_edit_record(self, row):
        return None
    def on_delete_record(self, row):
        return None

class ApplicationRoleMenu(pytis.presentation.Specification):
    table = 'ev_pytis_role_menu'
    title = _("Menu uživatele")
    fields = (
        Field('menuid', "", codebook='menu.ApplicationMenu'),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('title', _("Titulek položky menu"), fixed=True),
        Field('position', _("Pozice v menu")),
        Field('rights', _("Práva")),
        Field('rights_show', _("Menu"), fixed=True,
              descr=_("Zobrazení v menu")),
        Field('rights_view', _("Náhled"), fixed=True,
              descr=_("Zobrazení formuláře")),
        Field('rights_insert', _("Vložení"), fixed=True,
              descr=_("Vložení nového záznamu")),
        Field('rights_update', _("Editace"), fixed=True,
              descr=_("Změna hodnot existujícího záznamu")),
        Field('rights_delete', _("Smazání"), fixed=True,
              descr=_("Smazání existujícího záznamu")),
        Field('rights_print', _("Tisk"), fixed=True,
              descr=_("Tisk formuláře")),
        Field('rights_export', _("Export"), fixed=True,
              descr=_("Export formuláře do souboru")),
        Field('rights_call', _("Spuštění"), fixed=True,
              descr=_("Spuštění funkce (netýká se formulářů)")),
        )
    columns = ('title', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
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
