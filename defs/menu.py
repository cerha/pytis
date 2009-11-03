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

import copy

import pytis.data
import pytis.extensions
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
    def actions(self): return (
        pytis.presentation.Action(_("Zkop�rovat role od..."), self._copy_roles,
                                  descr=("Nastaven� aplika�n�ch rol� dle jin�ho u�ivatele.")),
        )
    bindings = (pytis.presentation.Binding(_("Obsahuje role"), 'menu.ApplicationRolesMembers', id='members',
                                           binding_column='roleid'),
                pytis.presentation.Binding(_("Pat�� do rol�"), 'menu.ApplicationRolesOwners', id='owners',
                                           binding_column='member'),
                pytis.presentation.Binding(_("N�hled menu"), 'menu.ApplicationRoleMenu', id='menu',
                                           arguments=(lambda row: dict(roleid=row['roleid'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), False,)))),
                pytis.presentation.Binding(_("N�hled chystan�ho menu"), 'menu.ApplicationPreviewRoleMenu', id='previewmenu',
                                           arguments=(lambda row: dict(roleid=row['roleid'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), True)))),
                pytis.presentation.Binding(_("Roz���en� n�hled menu"), 'menu.ApplicationRoleMenuExtended', id='extmenu',
                                           arguments=(lambda row: dict(roleid=row['roleid']))),
                pytis.presentation.Binding(_("Roz���en� n�hled chystan�ho menu"), 'menu.ApplicationPreviewRoleMenuExtended', id='previewextmenu',
                                           arguments=(lambda row: dict(roleid=row['roleid'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), True)))),
                )
    
    def on_delete_record(self, row):
        if self._row_deleteable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Role nelze mazat, nastavte datum zru�en�"))
        return None
    
    def _copy_roles(self, row):
        template_row = pytis.extensions.run_cb('menu.UserApplicationRolesCodebook')
        if template_row is None:
            return
        pytis.extensions.dbfunction('pytis_copy_role', ('copy_from', template_row['name'],), ('copy_to', row['name'],))

class UserApplicationRolesCodebook(ApplicationRoles):
    table = 'ev_pytis_valid_roles'
    columns = ('name', 'description',)
    condition = pytis.data.EQ('purposeid', pytis.data.Value(pytis.data.String(), 'user'))

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
              descr=_("Role, do n�� jsou zahrnuty jin� role.")),
        Field('member', _("Obsa�en� role"), fixed=True, codebook='menu.CommonApplicationRoles',
              descr=_("Role, kter� je �lenem skupinov� role.")),
        Field('purposeid', "", codebook='menu.ApplicationRolePurposes'),
        Field('purpose', _("��el role"),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
        Field('mpurposeid', "", codebook='menu.ApplicationRolePurposes'),
        Field('mpurpose', _("��el role"),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
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
    columns = ('roleid', 'purpose', 'description',)

class ApplicationRolesMembers(ApplicationRolesMembership):
    title = _("P�i�azen� role")
    columns = ('member', 'mpurpose', 'mdescription',)


### Actions

class ApplicationActions(pytis.presentation.Specification):
    table = 'c_pytis_menu_actions'
    title = _("U�ivatelsk� akce")
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
    table = 'ev_pytis_short_actions'
    title = _("U�ivatelsk� akce")
    fields = (
        Field('shortname', "", editable=Editable.NEVER),
        )
    sorting = (('shortname', pytis.data.ASCENDENT,),)    
    cb = pytis.presentation.CodebookSpec(display='shortname')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)


### Menus

class _Title(pytis.presentation.PrettyFoldable, pytis.data.String):
    def __init__(self, **kwargs):
        super(_Title, self).__init__(tree_column_id='position',
                                     subcount_column_id='position_nsub',
                                     **kwargs)

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
        Field('name', _("Id obsahuj�c� role")),
        Field('title', _("Titulek polo�ky menu"), type=_Title()),
        Field('xtitle', _("Titulek polo�ky menu"), type=_Title()),
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions'),
        Field('next_position', _(""), default='0'),
        Field('position_nsub'),
        Field('fullname', _("Nav�en� akce"), codebook='menu.ApplicationActions',
              descr=_("Akce aplikace vyvolan� polo�kou menu")),
        Field('xaction', _("Nav�en� akce"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              descr=_("Akce aplikace vyvolan� polo�kou menu")),
        Field('locked', _("Z�kaz editace"), fixed=True, editable=pytis.presentation.Editable.NEVER),
        )
    columns = ('xtitle', 'xaction', 'locked',)
    layout = ('title', 'position',)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)
    initial_folding = pytis.form.FoldableForm.Folding(level=2)
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
        wm_value = row['position'].value() + '.*'
        if (row.data().select(condition=pytis.data.AND(pytis.data.LTreeMatch('position', wm_value),
                                                       pytis.data.NE('position', row['position'])))
            > 0):
            pytis.form.run_dialog(pytis.form.Warning, _("Nelze mazat polo�ky obsahuj�c� jin� polo�ky"))
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete z�znam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
    
class ApplicationMenuM(pytis.presentation.Specification):
    table = 'ev_pytis_menu_structure'
    title = _("Menu")
    fields = (
        Field('fullname', _("Nav�en� akce"), codebook='menu.ApplicationActions',
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Akce aplikace vyvolan� polo�kou menu")),
        Field('shortname', _(""), codebook='menu.ApplicationShortActions',
              editable=pytis.presentation.Editable.NEVER),
        Field('actiontype', _("Typ polo�ky"), fixed=True,
              editable=pytis.presentation.Editable.NEVER),
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq'),
              editable=pytis.presentation.Editable.NEVER),
        Field('title', _("Titulek polo�ky menu"), type=_Title(),
              editable=pytis.presentation.Editable.NEVER),
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions',
              editable=pytis.presentation.Editable.NEVER),
        Field('position_nsub',
              editable=pytis.presentation.Editable.NEVER),
        Field('xaction', _("Nav�en� akce"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Akce aplikace vyvolan� polo�kou menu")),
        Field('locked', _("Z�kaz editace"), fixed=True,
              editable=pytis.presentation.Editable.NEVER),
        Field('description', _("Pozn�mka")),
        )
    columns = ('title', 'actiontype', 'fullname', 'description',)
    layout = ('title', 'position', 'actiontype', 'fullname', 'description',)
    bindings = (pytis.presentation.Binding(_("Rozpis pr�v podle rol�"), 'menu.ApplicationMenuRightsFoldable', id='role_rights',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       column=pytis.data.Value(pytis.data.String(), 'roleid',)))),
                pytis.presentation.Binding(_("Rozpis pr�v podle sloupc�"), 'menu.ApplicationMenuRightsFoldableColumn', id='column_rights',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       column=pytis.data.Value(pytis.data.String(), 'colname',)))),
                pytis.presentation.Binding(_("Pr�va polo�ky menu"), 'menu.ApplicationSummaryRights', id='summary_rights',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), False,),
                                                                       multirights=ApplicationMenuM._multiform_row(row),
                                                                       ))),
                pytis.presentation.Binding(_("Chystan� pr�va polo�ky"), 'menu.ApplicationPreviewRights', id='preview_rights',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), True,),
                                                                       multirights=ApplicationMenuM._multiform_row(row),
                                                                       ))),
                )
    def actions(self): return (
        pytis.presentation.Action(_("Zkop�rovat pr�va z..."), self._copy_rights,
                                  descr=("Zkop�rov�n� pr�v z jin� polo�ky.")),
        pytis.presentation.Action(_("Odstranit nadbyte�n� pr�va"), self._remove_redundant,
                                  descr=("Odstran�n� polo�ek pr�v, kter� nemaj� vliv na v�sledn� pr�va.")),
        )
    access_rights = pytis.data.AccessRights((None, (['admin_menu'],
                                                    pytis.data.Permission.VIEW,
                                                    pytis.data.Permission.EXPORT,
                                                    pytis.data.Permission.PRINT,
                                                    pytis.data.Permission.UPDATE,)))
    def initial_folding(self):
        folding = pytis.form.FoldableForm.Folding(level=2)
        folding.expand('8', level=0)
        return folding

    @staticmethod
    def _multiform_row(row):
        fullname = row['fullname'].value() or ''
        result = fullname[:4] != 'sub/' and (fullname.find('::') >= 0 or fullname.find('.Multi') >= 0)
        return pytis.data.Value(pytis.data.Boolean(), result)
    
    ## Form actions

    def _copy_rights(self, row):
        template_row = pytis.extensions.run_cb('menu.ApplicationMenuCodebook')
        if template_row is None:
            return
        pytis.extensions.dbfunction('pytis_copy_rights', ('copy_from', template_row['shortname'],), ('copy_to', row['shortname'],))

    def _remove_redundant(self, row):
        if not pytis.form.run_dialog(pytis.form.Question,
                                     (_("Opravdu chcete odstranit nadbyte�n� pr�va polo�ky \"%s\"?") %
                                      (row['title'].value() or '',))):
            return
        result = pytis.extensions.dbfunction('pytis_remove_redundant', ('shortname', row['shortname'],),)

class ApplicationMenuCodebook(pytis.presentation.Specification):
    table = 'ev_pytis_menu_structure'
    title = _("Menu")
    fields = (
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions',
              editable=pytis.presentation.Editable.NEVER),
        Field('position_nsub',
              editable=pytis.presentation.Editable.NEVER),
        Field('fullname', _("Nav�en� akce"), codebook='menu.ApplicationActions',
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Akce aplikace vyvolan� polo�kou menu")),
        Field('shortname', _(""), codebook='menu.ApplicationShortActions',
              editable=pytis.presentation.Editable.NEVER),
        Field('actiontype', _("Typ polo�ky"), fixed=True,
              editable=pytis.presentation.Editable.NEVER),
        Field('title', _("Titulek polo�ky menu"), type=_Title(),
              editable=pytis.presentation.Editable.NEVER),
        Field('description', _("Pozn�mka")),
        )
    columns = ('title', 'actiontype', 'description',)
    sorting=(('position', pytis.data.ASCENDENT,),)
    initial_folding = pytis.form.FoldableForm.Folding(level=None)
    access_rights = pytis.data.AccessRights((None, (['admin_menu'],
                                                    pytis.data.Permission.VIEW,
                                                    pytis.data.Permission.EXPORT,
                                                    pytis.data.Permission.PRINT,
                                                    pytis.data.Permission.UPDATE,)))

class ApplicationMenuPositions(pytis.presentation.Specification):
    table = 'ev_pytis_menu_positions'
    title = _("Menu")
    fields = (
        Field('position', _("Pozice v menu"), fixed=True),
        Field('title', _("Titulek polo�ky menu"), type=_Title()),
        Field('xtitle', _("Titulek polo�ky menu"), type=_Title()),
        )
    columns = ('xtitle',)
    layout = ('title', 'position',)
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

class ApplicationMenuRights(pytis.presentation.Specification):
    table = 'ev_pytis_action_rights'
    title = _("Pr�va")
    fields = (
        Field('id', "Id", default=nextval('e_pytis_action_rights_id_seq')),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('purpose', _("��el role"),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
        Field('shortname', _("Akce"), editable=pytis.presentation.Editable.NEVER,
              codebook='menu.ApplicationShortActions',
              descr=_("Identifik�tor akce souvisej�c� s danou polo�kou menu")),
        Field('colname', _("Sloupec"), editable=pytis.presentation.Editable.NEVER,
              fixed=True,
              descr=_("Sloupec, na kter� se pr�vo vztahuje")),
        Field('rightid', _("Pr�vo"), codebook='menu.ApplicationRights',
              fixed=True,
              descr=_("P�id�len� nebo odebran� pr�vo")),
        Field('system', _("Syst�mov�"), fixed=True,
              descr=_("Jde o nem�nn� pr�vo definov�no tv�rcem aplikace?")),
        Field('granted', _("Ano/Ne"), fixed=True, default=True,
              descr=_("Je pr�vo povoleno (ano) nebo zak�z�no (ne)?")),
        Field('redundant', _("Nadbyte�n�"), fixed=True,
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Je toto pr�vo nadbyte�n�, bez vlivu na v�sledn� pr�va?")),
        )
    columns = ('roleid', 'purpose', 'colname', 'rightid', 'system', 'granted', 'redundant',)
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
    def proc_spec(self):
        def commit_changes():
            if pytis.extensions.dbfunction("pytis_update_summary_rights"):
                message = _("Zm�ny aplikov�ny")
            else:
                message = _("Prov�d�n� zm�n uzam�eno, zm�ny nebyly aplikov�ny")
            pytis.form.run_dialog(pytis.form.Message, message)
        return {'commit_changes': commit_changes}
    
class _RightsTree(pytis.presentation.PrettyFoldable, pytis.data.String):
    def __init__(self, **kwargs):
        super(_RightsTree, self).__init__(tree_column_id='tree',
                                          subcount_column_id='subcount',
                                          **kwargs)

class ApplicationMenuRightsFoldable(pytis.presentation.Specification):
    table = 'pytis_action_rights_foldable'
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('column', "", type=pytis.data.String()),
                 )
    title = _("Pr�va")
    fields = (
        Field('id', "Id", type=pytis.data.Integer()),
        Field('tree', "", type=pytis.data.LTree()),
        Field('subcount', "", type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=_RightsTree(),
              fixed=True),
        Field('purpose', _("��el role"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Akce"), type=pytis.data.String(),
              descr=_("Identifik�tor akce souvisej�c� s danou polo�kou menu")),
        Field('colname', _("Sloupec"), type=pytis.data.String(),
              fixed=True,
              descr=_("Sloupec, na kter� se pr�vo vztahuje")),
        Field('rightid', _("Pr�vo"), type=pytis.data.String(),
              fixed=True,
              descr=_("P�id�len� nebo odebran� pr�vo")),
        Field('system', _("Syst�mov�"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Jde o nem�nn� pr�vo definov�no tv�rcem aplikace?")),
        Field('granted', _("Ano/Ne"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_("Je pr�vo povoleno (ano) nebo zak�z�no (ne)?")),
        Field('redundant', _("Nadbyte�n�"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Je toto pr�vo nadbyte�n�, bez vlivu na v�sledn� pr�va?")),
        )
    columns = ('roleid', 'purpose', 'colname', 'rightid', 'system', 'granted', 'redundant',)
    layout = ('shortname', 'roleid', 'purpose', 'rightid', 'granted',)
    sorting = (('tree', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def on_new_record(self, prefill=None, transaction=None):
        shortname = pytis.form.current_form()._main_form.current_row()['shortname']
        if prefill is None:
            prefill = {}
        else:
            prefill = copy.copy(prefill)
        prefill['shortname'] = shortname
        new = pytis.form.new_record('menu.ApplicationMenuRights',
                                    prefill=prefill, transaction=transaction,
                                    block_on_new_record=True, multi_insert=True)
        if new:
            return new
        return None
    def _row_editable(self, row):
        return not row['system'].value() and row['id'] >= 0
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Syst�mov� pr�va nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm, 'menu.ApplicationMenuRights', select_row=row['id'])
    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Syst�mov� pr�va nelze mazat"))
            return False
        return True
    def on_delete_record(self, row):
        form = pytis.form.current_form()
        if not self._row_deleteable(row):
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete z�znam zcela vymazat?")):
            return None
        data = pytis.extensions.data_object('menu.ApplicationMenuRights')
        data.delete((row['id'],))
        return 1

class ApplicationMenuRightsFoldableColumn(ApplicationMenuRightsFoldable):
    table = 'pytis_action_rights_foldable'
    fields = (
        Field('id', "Id", type=pytis.data.Integer()),
        Field('tree', "", type=pytis.data.LTree()),
        Field('subcount', "", type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=pytis.data.String(),
              fixed=True),
        Field('purpose', _("��el role"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Akce"), type=pytis.data.String(),
              descr=_("Identifik�tor akce souvisej�c� s danou polo�kou menu")),
        Field('colname', _("Sloupec"), type=_RightsTree(),
              fixed=True,
              descr=_("Sloupec, na kter� se pr�vo vztahuje")),
        Field('rightid', _("Pr�vo"), type=pytis.data.String(),
              fixed=True,
              descr=_("P�id�len� nebo odebran� pr�vo")),
        Field('system', _("Syst�mov�"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Jde o nem�nn� pr�vo definov�no tv�rcem aplikace?")),
        Field('granted', _("Ano/Ne"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_("Je pr�vo povoleno (ano) nebo zak�z�no (ne)?")),
        Field('redundant', _("Nadbyte�n�"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Je toto pr�vo nadbyte�n�, bez vlivu na v�sledn� pr�va?")),
        )
    columns = ('colname', 'roleid', 'purpose', 'rightid', 'system', 'granted', 'redundant',)

class _MenuidPreviewType(pytis.data.Integer):
    def default_value(self):
        return pytis.data.Value(self, 0)

class ApplicationSummaryRights(pytis.presentation.Specification):
    table = 'pytis_view_summary_rights'
    title = _("Souhrnn� pr�va")
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 Field('multirights', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('roleid', _("Role"), type=pytis.data.String(not_null=True),
              fixed=True),
        Field('purpose', _("��el role"), type=pytis.data.String(),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
        Field('columns', _("Sloupce"), type=pytis.data.String(),
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Sloupce, na kter� je pr�vo aplikov�no; nen�-li uvedeno, tak v�echny.")),
        Field('rights', _("Pr�va"), type=pytis.data.String(), fixed=True),
        Field('rights_show', _("Menu"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Zobrazen� v menu")),
        Field('rights_view', _("N�hled"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Zobrazen� formul��e")),
        Field('rights_insert', _("Vlo�en�"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Vlo�en� nov�ho z�znamu")),
        Field('rights_update', _("Editace"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Zm�na hodnot existuj�c�ho z�znamu")),
        Field('rights_delete', _("Smaz�n�"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Smaz�n� existuj�c�ho z�znamu")),
        Field('rights_print', _("Tisk"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Tisk formul��e")),
        Field('rights_export', _("Export"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Export formul��e do souboru")),
        Field('rights_call', _("Spu�t�n�"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Spu�t�n� funkce (net�k� se formul���)")),
        )
    columns = ('roleid', 'purpose', 'columns', 'rights_show', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('roleid', 'purpose', 'rights_show', 'rights_view', 'rights_insert', 'rights_update',
              'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    sorting = (('roleid', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def on_new_record(self, *args, **kwargs):
        return None
    def on_edit_record(self, row):
        return None
    def on_delete_record(self, row):
        return None

class ApplicationPreviewRights(ApplicationSummaryRights):
    table = 'pytis_view_summary_rights'
    title = _("Chystan� pr�va")

class ApplicationRoleMenu(pytis.presentation.Specification):
    table = 'pytis_view_role_menu'
    title = _("Menu u�ivatele")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('menuid', "", type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=pytis.data.String(),
              fixed=True),
        Field('title', _("Titulek polo�ky menu"), type=_Title(), fixed=True),
        Field('position', _("Pozice v menu"), type=pytis.data.String()),
        Field('position_nsub', type=pytis.data.Integer()),
        Field('rights', _("Pr�va"), type=pytis.data.String()),
        Field('rights_show', _("Menu"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Zobrazen� v menu")),
        Field('rights_view', _("N�hled"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Zobrazen� formul��e")),
        Field('rights_insert', _("Vlo�en�"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Vlo�en� nov�ho z�znamu")),
        Field('rights_update', _("Editace"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Zm�na hodnot existuj�c�ho z�znamu")),
        Field('rights_delete', _("Smaz�n�"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Smaz�n� existuj�c�ho z�znamu")),
        Field('rights_print', _("Tisk"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Tisk formul��e")),
        Field('rights_export', _("Export"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Export formul��e do souboru")),
        Field('rights_call', _("Spu�t�n�"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Spu�t�n� funkce (net�k� se formul���)")),
        )
    columns = ('title', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    initial_folding = pytis.form.FoldableForm.Folding(level=None)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def on_new_record(self, *args, **kwargs):
        return None
    def on_edit_record(self, row):
        return None
    def on_delete_record(self, row):
        return None

class ApplicationPreviewRoleMenu(ApplicationRoleMenu):
    table = 'pytis_view_role_menu'
    title = _("Chystan� menu u�ivatele")
    
class ApplicationRoleMenuExtended(ApplicationRoleMenu):
    table = 'pytis_view_extended_role_menu'
    title = _("Roz���en� menu u�ivatele")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (ApplicationRoleMenu.fields +
              (Field('actiontype', _("Typ polo�ky"), fixed=True, editable=pytis.presentation.Editable.NEVER,
               type=pytis.data.String()),))
    columns = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)

class ApplicationPreviewRoleMenuExtended(ApplicationRoleMenuExtended):
    table = 'pytis_view_extended_role_menu'
    title = _("Chystan� roz���en� menu u�ivatele")
