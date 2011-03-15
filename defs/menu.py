# -*- coding: iso-8859-2 -*-

# Copyright (C) 2009, 2010, 2011 Brailcom, o.p.s.
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
    public = True
    table = 'c_pytis_role_purposes'
    title = _("Účely rolí")
    fields = (
        Field('purposeid', _("Identifikátor")),
        Field('purpose', _("Popis")),
        )
    cb = pytis.presentation.CodebookSpec(display='purpose')

class CommonApplicationRolePurposes(ApplicationRolePurposes):
    public = True
    condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'admn'))

class _ApplicationRolesSpecification(pytis.presentation.Specification):
    public = False
    
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
    
class ApplicationRoles(_ApplicationRolesSpecification):
    public = True
    table = 'ev_pytis_roles'
    title = _("Role")
    fields = (
        Field('roleid', _("Skupina"),     # to allow binding of ApplicationRolesMembers
              virtual=True, computer=pytis.presentation.computer(lambda row, name: name)),
        Field('member', _("Obsažená role"),     # to allow binding of ApplicationRolesOwners
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
    def actions(self): return (
        pytis.presentation.Action('copy_roles', _("Zkopírovat role od..."), self._copy_roles,
                                  descr=("Nastavení aplikačních rolí dle jiného uživatele.")),
        )
    bindings = (pytis.presentation.Binding('members', _("Obsahuje role"), 'menu.ApplicationRolesMembers',
                                           binding_column='roleid'),
                pytis.presentation.Binding('owners', _("Patří do rolí"), 'menu.ApplicationRolesOwners',
                                           binding_column='member'),
                pytis.presentation.Binding('menu', _("Náhled menu"), 'menu.ApplicationRoleMenu',
                                           arguments=(lambda row: dict(roleid=row['roleid'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), False,)))),
                pytis.presentation.Binding('previewmenu', _("Náhled chystaného menu"), 'menu.ApplicationPreviewRoleMenu',
                                           arguments=(lambda row: dict(roleid=row['roleid'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), True)))),
                pytis.presentation.Binding('extmenu', _("Rozšířený náhled menu"), 'menu.ApplicationRoleMenuExtended',
                                           arguments=(lambda row: dict(roleid=row['roleid']))),
                pytis.presentation.Binding('previewextmenu', _("Rozšířený náhled chystaného menu"), 'menu.ApplicationPreviewRoleMenuExtended',
                                           arguments=(lambda row: dict(roleid=row['roleid'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), True)))),
                )
    
    def on_delete_record(self, row):
        if self._row_deleteable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Role nelze mazat, nastavte datum zrušení"))
        return None
    
    def _copy_roles(self, row):
        template_row = pytis.extensions.run_cb('menu.UserApplicationRolesCodebook')
        if template_row is None:
            return
        pytis.extensions.dbfunction('pytis_copy_role', ('copy_from', template_row['name'],), ('copy_to', row['name'],))

class UserApplicationRolesCodebook(ApplicationRoles):
    public = True
    table = 'ev_pytis_valid_roles'
    columns = ('name', 'description',)
    condition = pytis.data.EQ('purposeid', pytis.data.Value(pytis.data.String(), 'user'))

class ApplicationApplicationRoles(ApplicationRoles):
    public = True
    table = 'ev_pytis_valid_roles'
    condition = pytis.data.EQ('purposeid', pytis.data.Value(pytis.data.String(), 'appl'))

class CommonApplicationRoles(ApplicationRoles):
    public = True
    table = 'ev_pytis_valid_roles'
    condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'admn'))

class ApplicationRolesMembership(_ApplicationRolesSpecification):
    public = True
    table = 'ev_pytis_valid_role_members'
    title = "Členství v rolích"
    fields = (
        Field('id', _("Id"), default=nextval('e_pytis_role_members_id_seq')),
        Field('roleid', _("Skupina"), fixed=True, codebook='menu.ApplicationApplicationRoles',
              descr=_("Role, do níž jsou zahrnuty jiné role.")),
        Field('member', _("Obsažená role"), fixed=True, codebook='menu.CommonApplicationRoles',
              descr=_("Role, která je členem skupinové role.")),
        Field('purposeid', _("Identifikátor účelu role"), codebook='menu.ApplicationRolePurposes'),
        Field('purpose', _("Účel role"),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
        Field('mpurposeid', _("Identifikátor účelu role"), codebook='menu.ApplicationRolePurposes'),
        Field('mpurpose', _("Účel role"),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
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
    public = True
    title = _("Zařazení do skupiny")
    columns = ('roleid', 'purpose', 'description',)

class ApplicationRolesMembers(ApplicationRolesMembership):
    public = True
    title = _("Přiřazení role")
    columns = ('member', 'mpurpose', 'mdescription',)


### Actions

class ApplicationActions(pytis.presentation.Specification):
    public = True
    table = 'c_pytis_menu_actions'
    title = _("Uživatelské akce")
    fields = (
        Field('fullname', _("Navěšená akce"), editable=Editable.NEVER),
        Field('shortname', _("Akce"), editable=Editable.NEVER),
        Field('description', _("Popis")),
        )
    columns = ('fullname', 'description',)
    layout = ('fullname', 'description',)
    sorting = (('fullname', pytis.data.ASCENDENT,),)    
    cb = pytis.presentation.CodebookSpec(display='fullname')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)

class ApplicationShortActions(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_short_actions'
    title = _("Uživatelské akce")
    fields = (
        Field('shortname', _("Akce"), editable=Editable.NEVER),
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
    public = True
    table = 'ev_pytis_menu'
    title = _("Menu")
    fields = (
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq')),
        Field('name', _("Id obsahující role")),
        Field('title', _("Titulek položky menu"), type=_Title()),
        Field('xtitle', _("Titulek položky menu"), type=_Title()),
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions'),
        Field('next_position', _("Následující pozice v menu"), default='0'),
        Field('position_nsub', _("Počet poduzlů")),
        Field('fullname', _("Navěšená akce"), codebook='menu.ApplicationActions',
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('xaction', _("Navěšená akce"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('locked', _("Zákaz editace"), fixed=True, editable=pytis.presentation.Editable.NEVER),
        )
    columns = ('xtitle', 'xaction', 'locked',)
    layout = ('title', 'position',)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)
    folding = pytis.form.FoldableForm.Folding(level=2)
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
        wm_value = row['position'].value() + '.*'
        if (row.data().select(condition=pytis.data.AND(pytis.data.LTreeMatch('position', wm_value),
                                                       pytis.data.NE('position', row['position'])))
            > 0):
            pytis.form.run_dialog(pytis.form.Warning, _("Nelze mazat položky obsahující jiné položky"))
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete záznam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
    
class ApplicationMenuM(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_structure'
    title = _("Menu")
    fields = (
        Field('fullname', _("Navěšená akce"), codebook='menu.ApplicationActions',
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('shortname', _("Akce"), codebook='menu.ApplicationShortActions',
              editable=pytis.presentation.Editable.NEVER),
        Field('actiontype', _("Typ položky"), fixed=True,
              editable=pytis.presentation.Editable.NEVER),
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq'),
              editable=pytis.presentation.Editable.NEVER),
        Field('title', _("Titulek položky menu"), type=_Title(),
              editable=pytis.presentation.Editable.NEVER),
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions',
              editable=pytis.presentation.Editable.NEVER),
        Field('position_nsub', _("Počet poduzlů"),
              editable=pytis.presentation.Editable.NEVER),
        Field('xaction', _("Navěšená akce"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('locked', _("Zákaz editace"), fixed=True,
              editable=pytis.presentation.Editable.NEVER),
        Field('description', _("Poznámka")),
        )
    columns = ('title', 'actiontype', 'fullname', 'description',)
    layout = ('title', 'position', 'actiontype', 'fullname', 'description',)
    bindings = (pytis.presentation.Binding('role_rights', _("Rozpis práv podle rolí"), 'menu.ApplicationMenuRightsFoldable',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       column=pytis.data.Value(pytis.data.String(), 'roleid',)))),
                pytis.presentation.Binding('column_rights', _("Rozpis práv podle sloupců"), 'menu.ApplicationMenuRightsFoldableColumn',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       column=pytis.data.Value(pytis.data.String(), 'colname',)))),
                pytis.presentation.Binding('summary_rights', _("Práva položky menu"), 'menu.ApplicationSummaryRights',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), False,),
                                                                       multirights=ApplicationMenuM._multiform_row(row),
                                                                       ))),
                pytis.presentation.Binding('preview_rights', _("Chystaná práva položky"), 'menu.ApplicationPreviewRights',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       new=pytis.data.Value(pytis.data.Boolean(), True,),
                                                                       multirights=ApplicationMenuM._multiform_row(row),
                                                                       ))),
                pytis.presentation.Binding('changes', _("Chystané změny"), 'menu.ApplicationChangedRights',
                                           arguments=(lambda row: dict(shortname=row['shortname'],
                                                                       multirights=ApplicationMenuM._multiform_row(row),
                                                                       ))),
                pytis.presentation.Binding('users', _("Uživatelé"), 'statistics.FormUserStatisticsNoinfo',
                                           condition=(lambda row: pytis.data.EQ('shortname', row['shortname']))),
                )
    def actions(self): return (
        pytis.presentation.Action('copy_rights', _("Zkopírovat práva z..."), self._copy_rights,
                                  descr=("Zkopírování práv z jiné položky.")),
        pytis.presentation.Action('a_odstranit_nadbytecna_prava', _("Odstranit nadbytečná práva"), self._remove_redundant,
                                  descr=("Odstranění položek práv, které nemají vliv na výsledná práva.")),
        )
    access_rights = pytis.data.AccessRights((None, (['admin_menu'],
                                                    pytis.data.Permission.VIEW,
                                                    pytis.data.Permission.EXPORT,
                                                    pytis.data.Permission.PRINT,
                                                    pytis.data.Permission.UPDATE,)))
    def folding(self):
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
                                     (_("Opravdu chcete odstranit nadbytečná práva položky \"%s\"?") %
                                      (row['title'].value() or '',))):
            return
        result = pytis.extensions.dbfunction('pytis_remove_redundant', ('shortname', row['shortname'],),)

class ApplicationMenuCodebook(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_structure'
    title = _("Menu")
    fields = (
        Field('position', _("Pozice v menu"), fixed=True, codebook='menu.ApplicationMenuPositions',
              editable=pytis.presentation.Editable.NEVER),
        Field('position_nsub', _("Počet poduzlů"),
              editable=pytis.presentation.Editable.NEVER),
        Field('fullname', _("Navěšená akce"), codebook='menu.ApplicationActions',
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Akce aplikace vyvolaná položkou menu")),
        Field('shortname', _("Akce"), codebook='menu.ApplicationShortActions',
              editable=pytis.presentation.Editable.NEVER),
        Field('actiontype', _("Typ položky"), fixed=True,
              editable=pytis.presentation.Editable.NEVER),
        Field('title', _("Titulek položky menu"), type=_Title(),
              editable=pytis.presentation.Editable.NEVER),
        Field('description', _("Poznámka")),
        )
    columns = ('title', 'actiontype', 'description',)
    sorting=(('position', pytis.data.ASCENDENT,),)
    folding = pytis.form.FoldableForm.Folding(level=None)
    access_rights = pytis.data.AccessRights((None, (['admin_menu'],
                                                    pytis.data.Permission.VIEW,
                                                    pytis.data.Permission.EXPORT,
                                                    pytis.data.Permission.PRINT,
                                                    pytis.data.Permission.UPDATE,)))

class ApplicationMenuPositions(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_positions'
    title = _("Menu")
    fields = (
        Field('position', _("Pozice v menu"), fixed=True),
        Field('title', _("Titulek položky menu"), type=_Title()),
        Field('xtitle', _("Titulek položky menu"), type=_Title()),
        )
    columns = ('xtitle',)
    layout = ('title', 'position',)
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)


### Rights

class ApplicationRights(pytis.presentation.Specification):
    public = True
    table = 'c_pytis_access_rights'
    title = "Seznam práv"
    fields = (
        Field('rightid', "Právo", fixed=True),
        Field('description', "Popis"),
        )
    columns = ('rightid', 'description',)
    layout = ('rightid', 'description',)
    cb = pytis.presentation.CodebookSpec(display='description')

def _colname_description(row, shortname, colname):
    components = shortname.split('/')
    if components[0] != 'form':
        return None
    if not colname:
        return _("Celý formulář")
    spec_name = components[1]
    resolver = pytis.util.resolver()
    try:
        view_spec = resolver.get(spec_name, 'view_spec')
    except ResolverError:
        return None
    field = view_spec.field(colname)
    if field is None:
        return None
    description = field.label()
    return description
class ApplicationColumns(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_colnames'
    title = "Sloupce"
    fields = (
        Field('colname', _("Identifikátor"), fixed=True),
        Field('shortname', _("Akce")),
        Field('description', _("Popis"), type=pytis.data.String(),
              virtual=True, computer=pytis.presentation.computer(_colname_description)),
        )
    columns = ('colname', 'description',)
    layout = ('colname', 'description',)
    cb = pytis.presentation.CodebookSpec(display='colname')

class _ApplicationMenuRightsBase(pytis.presentation.Specification):
    public = False
    
    def _multiaction_check(self):
        main_form = pytis.form.current_form().main_form()
        shortname = main_form.current_row()['shortname']
        if not shortname.value():
            return
        data = main_form.data(init_select=False)
        items = data.select_map(lambda row: (row['menuid'].value(), row['title'].value()),
                                condition=pytis.data.EQ('shortname', shortname))
        if len(items) > 1:
            current_menuid = main_form.current_row()['menuid'].value()
            message = _("Pozor, tato položka se vyskytuje i na jiných místech menu:")
            for menuid, title in items:
                if menuid != current_menuid:
                    message = message + '\n  ' + (title or _("[bez titulku]"))
            pytis.form.run_dialog(pytis.form.Warning, message)
    def _codebook_rights_check(self):
        main_form = pytis.form.current_form().main_form()
        shortname = main_form.current_row()['shortname'].value()
        components = shortname.split('/')
        if components[0] != 'form':
            return
        spec_name = components[1]
        menu_checker = pytis.extensions.MenuChecker()
        errors = (menu_checker.check_codebook_rights(spec_name, new=True) +
                  menu_checker.check_reverse_codebook_rights(spec_name, new=True))
        if errors:
            message = ('\n'.join(errors)) + '\n\n\n'
            pytis.form.run_dialog(pytis.form.Warning, "Rozporuplná přístupová práva k číselníku",
                                  report=message)
    def _before_edit_checks(self):
        self._multiaction_check()
    def _after_edit_checks(self):
        self._codebook_rights_check()
    
class ApplicationMenuRights(_ApplicationMenuRightsBase):
    public = True
    table = 'ev_pytis_action_rights'
    title = _("Práva")
    fields = (
        Field('id', _("Id"), default=nextval('e_pytis_action_rights_id_seq')),
        Field('roleid', _("Role"), codebook='menu.ApplicationRoles',
              fixed=True),
        Field('purpose', _("Účel role"),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
        Field('shortname', _("Akce"), editable=pytis.presentation.Editable.NEVER,
              codebook='menu.ApplicationShortActions',
              descr=_("Identifikátor akce související s danou položkou menu")),
        Field('colname', _("Sloupec"),
              fixed=True,
              codebook='menu.ApplicationColumns', not_null=False,
              runtime_filter=pytis.presentation.Computer(lambda row: pytis.data.EQ('shortname', row['shortname']), depends=('shortname',)),
              descr=_("Sloupec, na který se právo vztahuje")),
        Field('rightid', _("Právo"), codebook='menu.ApplicationRights',
              fixed=True,
              descr=_("Přidělené nebo odebrané právo")),
        Field('system', _("Systémové"), fixed=True,
              descr=_("Jde o neměnné právo definováno tvůrcem aplikace?")),
        Field('granted', _("Ano/Ne"), fixed=True, default=True,
              descr=_("Je právo povoleno (ano) nebo zakázáno (ne)?")),
        Field('redundant', _("Nadbytečné"), fixed=True,
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Je toto právo nadbytečné, bez vlivu na výsledná práva?")),
        )
    columns = ('roleid', 'purpose', 'colname', 'rightid', 'system', 'granted', 'redundant',)
    layout = ('shortname', 'roleid', 'rightid', 'granted', 'colname',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('rightid', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def _row_editable(self, row):
        return not row['system'].value()
    def on_new_record(self, *args, **kwargs):
        self._before_edit_checks()
        result = pytis.form.run_form(pytis.form.PopupInsertForm,
                                     'menu.ApplicationMenuRights',
                                     *args, **kwargs)
        if result:
            self._after_edit_checks()
        return result
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Systémová práva nelze editovat"))
            return None
        self._before_edit_checks()
        result = pytis.form.run_form(pytis.form.PopupEditForm, 'menu.'+self.__class__.__name__, select_row=row['id'])
        if result:
            self._after_edit_checks()
        return result
    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Systémová práva nelze mazat"))
            return False
        return True
    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        self._before_edit_checks()
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete záznam zcela vymazat?")):
            return None
        result = row.data().delete((row['id'],))
        if result:
            self._after_edit_checks()
        else:
            result = "Řádek se nepodařilo vymazat"
        return result
    def proc_spec(self):
        def commit_changes():
            if pytis.extensions.dbfunction("pytis_update_summary_rights"):
                message = _("Změny aplikovány")
            else:
                message = _("Provádění změn uzamčeno, změny nebyly aplikovány")
            pytis.form.run_dialog(pytis.form.Message, message)
        return {'commit_changes': commit_changes}
    
class _RightsTree(pytis.presentation.PrettyFoldable, pytis.data.String):
    def __init__(self, **kwargs):
        super(_RightsTree, self).__init__(tree_column_id='tree',
                                          subcount_column_id='subcount',
                                          **kwargs)

class ApplicationMenuRightsFoldable(_ApplicationMenuRightsBase):
    public = True
    table = 'pytis_action_rights_foldable'
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('column', "", type=pytis.data.String()),
                 )
    title = _("Práva")
    fields = (
        Field('id', _("Id"), type=pytis.data.Integer()),
        Field('tree', _("Stromový identifikátor"), type=pytis.data.LTree()),
        Field('subcount', _("Počet poduzlů"), type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=_RightsTree(),
              fixed=True),
        Field('purpose', _("Účel role"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Akce"), type=pytis.data.String(),
              descr=_("Identifikátor akce související s danou položkou menu")),
        Field('colname', _("Sloupec"), type=pytis.data.String(),
              fixed=True,
              descr=_("Sloupec, na který se právo vztahuje")),
        Field('rightid', _("Právo"), type=pytis.data.String(),
              fixed=True,
              descr=_("Přidělené nebo odebrané právo")),
        Field('system', _("Systémové"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Jde o neměnné právo definováno tvůrcem aplikace?")),
        Field('granted', _("Ano/Ne"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_("Je právo povoleno (ano) nebo zakázáno (ne)?")),
        Field('redundant', _("Nadbytečné"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Je toto právo nadbytečné, bez vlivu na výsledná práva?")),
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
        self._before_edit_checks()
        new = pytis.form.new_record('menu.ApplicationMenuRights',
                                    prefill=prefill, transaction=transaction,
                                    block_on_new_record=True, multi_insert=True)
        if new:
            self._after_edit_checks()
        return new or None
    def _row_editable(self, row):
        return not row['system'].value() and row['id'].value() >= 0
    def on_edit_record(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Systémová práva nelze editovat"))
            return None
        self._before_edit_checks()
        result = pytis.form.run_form(pytis.form.PopupEditForm, 'menu.ApplicationMenuRights', select_row=row['id'])
        if result:
            self._after_edit_checks()
        return result
    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _("Systémová práva nelze mazat"))
            return False
        return True
    def on_delete_record(self, row):
        form = pytis.form.current_form()
        if not self._row_deleteable(row):
            return None
        self._before_edit_checks()
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete záznam zcela vymazat?")):
            return None
        data = pytis.extensions.data_object('menu.ApplicationMenuRights')
        result = data.delete((row['id'],))
        if result:
            self._after_edit_checks()
        else:
            result = "Řádek se nepodařilo vymazat"
        return 1

class ApplicationMenuRightsFoldableColumn(ApplicationMenuRightsFoldable):
    public = True
    table = 'pytis_action_rights_foldable'
    fields = (
        Field('id', _("Id"), type=pytis.data.Integer()),
        Field('tree', _("Stromový identifikátor"), type=pytis.data.LTree()),
        Field('subcount', _("Počet poduzlů"), type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=pytis.data.String(),
              fixed=True),
        Field('purpose', _("Účel role"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Akce"), type=pytis.data.String(),
              descr=_("Identifikátor akce související s danou položkou menu")),
        Field('colname', _("Sloupec"), type=_RightsTree(),
              fixed=True,
              descr=_("Sloupec, na který se právo vztahuje")),
        Field('rightid', _("Právo"), type=pytis.data.String(),
              fixed=True,
              descr=_("Přidělené nebo odebrané právo")),
        Field('system', _("Systémové"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Jde o neměnné právo definováno tvůrcem aplikace?")),
        Field('granted', _("Ano/Ne"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_("Je právo povoleno (ano) nebo zakázáno (ne)?")),
        Field('redundant', _("Nadbytečné"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Je toto právo nadbytečné, bez vlivu na výsledná práva?")),
        )
    columns = ('colname', 'roleid', 'purpose', 'rightid', 'system', 'granted', 'redundant',)

class _MenuidPreviewType(pytis.data.Integer):
    def default_value(self):
        return pytis.data.Value(self, 0)

class ApplicationSummaryRights(pytis.presentation.Specification):
    public = True
    table = 'pytis_view_summary_rights'
    title = _("Souhrnná práva")
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 Field('multirights', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('roleid', _("Role"), type=pytis.data.String(not_null=True),
              fixed=True),
        Field('purpose', _("Účel role"), type=pytis.data.String(),
              editable=pytis.presentation.Editable.NEVER, fixed=True),
        Field('columns', _("Sloupce"), type=pytis.data.String(),
              editable=pytis.presentation.Editable.NEVER,
              descr=_("Sloupce, na které je právo aplikováno; není-li uvedeno, tak všechny.")),
        Field('rights', _("Práva"), type=pytis.data.String(), fixed=True),
        Field('rights_show', _("Menu"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Zobrazení v menu")),
        Field('rights_view', _("Náhled"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Zobrazení formuláře")),
        Field('rights_insert', _("Vložení"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Vložení nového záznamu")),
        Field('rights_update', _("Editace"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Změna hodnot existujícího záznamu")),
        Field('rights_delete', _("Smazání"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Smazání existujícího záznamu")),
        Field('rights_print', _("Tisk"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Tisk formuláře")),
        Field('rights_export', _("Export"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Export formuláře do souboru")),
        Field('rights_call', _("Spuštění"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Spuštění funkce (netýká se formulářů)")),
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
    public = True
    table = 'pytis_view_summary_rights'
    title = _("Chystaná práva")

class ApplicationChangedRights(ApplicationSummaryRights):
    public = True
    table = 'pytis_changed_rights'
    title = _("Chystané změny práv")
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('roleid', "", type=pytis.data.String()),
                 Field('multirights', "", type=pytis.data.Boolean()),
                 )
    fields = (ApplicationSummaryRights.fields +
              (Field('change', _("Nové"), type=pytis.data.Boolean(),
                     editable=pytis.presentation.Editable.NEVER, fixed=True),))
    _STYLE_OLD = pytis.presentation.Style(background=pytis.presentation.Color.GRAY10)
    def row_style(self, row):
        if not row['change'].value():
            return self._STYLE_OLD

class ApplicationRoleMenu(pytis.presentation.Specification):
    public = True
    table = 'pytis_view_role_menu'
    title = _("Menu uživatele")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('menuid', _("Identifikátor položky menu"), type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=pytis.data.String(),
              fixed=True),
        Field('title', _("Titulek položky menu"), type=_Title(), fixed=True),
        Field('position', _("Pozice v menu"), type=pytis.data.String()),
        Field('position_nsub', _("Počet poduzlů"), type=pytis.data.Integer()),
        Field('rights', _("Práva"), type=pytis.data.String()),
        Field('rights_show', _("Menu"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Zobrazení v menu")),
        Field('rights_view', _("Náhled"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Zobrazení formuláře")),
        Field('rights_insert', _("Vložení"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Vložení nového záznamu")),
        Field('rights_update', _("Editace"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Změna hodnot existujícího záznamu")),
        Field('rights_delete', _("Smazání"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Smazání existujícího záznamu")),
        Field('rights_print', _("Tisk"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Tisk formuláře")),
        Field('rights_export', _("Export"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Export formuláře do souboru")),
        Field('rights_call', _("Spuštění"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Spuštění funkce (netýká se formulářů)")),
        )
    columns = ('title', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    folding = pytis.form.FoldableForm.Folding(level=None)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)
    def on_new_record(self, *args, **kwargs):
        return None
    def on_edit_record(self, row):
        return None
    def on_delete_record(self, row):
        return None

class ApplicationPreviewRoleMenu(ApplicationRoleMenu):
    public = True
    table = 'pytis_view_role_menu'
    title = _("Chystané menu uživatele")
    
class ApplicationRoleMenuExtended(ApplicationRoleMenu):
    public = True
    table = 'pytis_view_extended_role_menu'
    title = _("Rozšířené menu uživatele")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (ApplicationRoleMenu.fields +
              (Field('actiontype', _("Typ položky"), fixed=True, editable=pytis.presentation.Editable.NEVER,
               type=pytis.data.String()),))
    columns = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)

class ApplicationPreviewRoleMenuExtended(ApplicationRoleMenuExtended):
    public = True
    table = 'pytis_view_extended_role_menu'
    title = _("Chystané rozšířené menu uživatele")
