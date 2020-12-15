# -*- coding: utf-8 -*-

# Copyright (C) 2019, 2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2009-2015 OUI Technology Ltd.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
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
import pytis.util
from pytis.util import nextval
from pytis.presentation import Editable, Field, procedure

_ = pytis.util.translations('pytis-defs')

# Roles


class ApplicationRolePurposes(pytis.presentation.Specification):
    public = True
    table = 'c_pytis_role_purposes'
    title = _(u"Účely rolí")
    fields = (
        Field('purposeid', _("Identifier")),
        Field('purpose', _("Description")),
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
            pytis.form.run_dialog(pytis.form.Warning, _(u"Správcovské role nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm,
                                   'menu.' + self.__class__.__name__,
                                   select_row=row['name'])

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _(u"Správcovské role nelze mazat"))
            return False
        return True

    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        if not pytis.form.run_dialog(pytis.form.Question,
                                     _(u"Opravdu chcete záznam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])


class ApplicationRoles(_ApplicationRolesSpecification):
    public = True
    table = 'ev_pytis_roles'
    title = _(u"Role")
    fields = (
        Field('roleid', _(u"Skupina"),     # to allow binding of ApplicationRolesMembers
              virtual=True, editable=Editable.NEVER,
              computer=pytis.presentation.computer(lambda row, name: name)),
        Field('member', _(u"Obsažená role"),     # to allow binding of ApplicationRolesOwners
              virtual=True, editable=Editable.NEVER,
              computer=pytis.presentation.computer(lambda row, name: name)),
        Field('name', _("Title"),
              fixed=True,
              descr=_(u"Stručný název role nebo uživatelské jméno v databázi.")),
        Field('description', "Popis",
              descr=_(u"Popis určení role.")),
        Field('purposeid', _(u"Účel"), not_null=True, codebook='menu.CommonApplicationRolePurposes',
              fixed=True, selection_type=pytis.presentation.SelectionType.CHOICE,
              descr=_(u"Účel role: normální, uživatelský účet, správcovská.")),
        Field('purpose', _(u"Účel"),
              fixed=True,
              descr=_(u"Význam role: normální, uživatelský účet, správcovská.")),
        Field('deleted', _(u"Datum zrušení"),
              fixed=True,
              descr=_(u"Je-li nastaveno, role je od daného data neaktivní.")),
    )
    columns = ('name', 'description', 'purpose', 'deleted',)
    layout = ('name', 'description', 'purposeid', 'deleted',)
    sorting = (('name', pytis.data.ASCENDENT,),)

    def profiles(self):
        return pytis.presentation.Profiles(
            pytis.presentation.Profile(
                'nezrusene-role', _("Nezrušené role"),
                filter=pytis.data.EQ('deleted', pytis.data.dval(None))),
            default="nezrusene-role"
        )

    def row_style(self, row):
        if row["deleted"].value() is not None:
            return pytis.presentation.Style(background=pytis.presentation.Color.GRAY10,
                                            foreground=pytis.presentation.Color.BLACK)

    def actions(self):
        return (
            pytis.presentation.Action('copy_roles', _(u"Zkopírovat role od..."), self._copy_roles,
                                      descr=("Nastavení aplikačních rolí dle jiného uživatele.")),
        )
    bindings = (pytis.presentation.Binding('members', _(u"Obsahuje role"),
                                           'menu.ApplicationRolesMembers',
                                           binding_column='roleid'),
                pytis.presentation.Binding('owners', _(u"Patří do rolí"),
                                           'menu.ApplicationRolesOwners',
                                           binding_column='member'),
                pytis.presentation.Binding('menu', _(u"Náhled menu"), 'menu.ApplicationRoleMenu',
                                           arguments=(lambda row:
                                                      dict(roleid=row['roleid'],
                                                           new=pytis.data.bval(False)))),
                pytis.presentation.Binding('previewmenu', _(u"Náhled chystaného menu"),
                                           'menu.ApplicationPreviewRoleMenu',
                                           arguments=(lambda row:
                                                      dict(roleid=row['roleid'],
                                                           new=pytis.data.bval(True)))),
                pytis.presentation.Binding('extmenu', _(u"Rozšířený náhled menu"),
                                           'menu.ApplicationRoleMenuExtended',
                                           arguments=(lambda row: dict(roleid=row['roleid']))),
                pytis.presentation.Binding('previewextmenu', _(u"Rozšířený náhled chystaného menu"),
                                           'menu.ApplicationPreviewRoleMenuExtended',
                                           arguments=(lambda row:
                                                      dict(roleid=row['roleid'],
                                                           new=pytis.data.bval(True)))),
                pytis.presentation.Binding('log', _(u"Log Akcí"), 'logging.FormActionLogView',
                                           condition=(lambda r:
                                                      pytis.data.EQ('username', r['roleid']))),
                )

    def on_delete_record(self, row):
        if self._row_deleteable(row):
            pytis.form.run_dialog(pytis.form.Warning,
                                  _(u"Role nelze mazat, nastavte datum zrušení"))
        return None

    def _copy_roles(self, row):
        template_row = pytis.extensions.run_cb('menu.UserApplicationRolesCodebook')
        if template_row is None:
            return
        pytis.extensions.dbfunction('pytis_copy_role',
                                    ('copy_from', template_row['name'],), ('copy_to', row['name'],))


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
        Field('id', _(u"Id"), default=nextval('e_pytis_role_members_id_seq')),
        Field('roleid', _(u"Skupina"), fixed=True, not_null=True,
              codebook='menu.ApplicationApplicationRoles',
              descr=_(u"Role, do níž jsou zahrnuty jiné role.")),
        Field('member', _(u"Obsažená role"), fixed=True, not_null=True,
              codebook='menu.CommonApplicationRoles',
              descr=_(u"Role, která je členem skupinové role.")),
        Field('purposeid', _(u"Identifikátor účelu role"), not_null=True,
              codebook='menu.ApplicationRolePurposes'),
        Field('purpose', _(u"Účel role"),
              editable=Editable.NEVER, fixed=True),
        Field('mpurposeid', _(u"Identifikátor účelu role"), not_null=True,
              codebook='menu.ApplicationRolePurposes'),
        Field('mpurpose', _(u"Účel role"),
              editable=Editable.NEVER, fixed=True),
        Field('description', _("Description"),
              descr=_(u"Popis určení role.")),
        Field('mdescription', _("Description"),
              descr=_(u"Popis určení role.")),
    )
    columns = ('roleid', 'member',)
    layout = ('roleid', 'member',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('member', pytis.data.ASCENDENT,),)

    def _row_editable(self, row):
        # This is here to prevent deletion of admin role memberships
        return row['purposeid'].value() != 'admn' and row['mpurposeid'].value() != 'admn'

    def on_edit_record(self, row):
        pytis.form.run_dialog(pytis.form.Warning,
                              _(u"Přiřazení rolí nelze editovat, jen přidávat a mazat"))
        return None


class ApplicationRolesOwners(ApplicationRolesMembership):
    public = True
    title = _(u"Zařazení do skupiny")
    columns = ('roleid', 'purpose', 'description',)


class ApplicationRolesMembers(ApplicationRolesMembership):
    public = True
    title = _(u"Přiřazení role")
    columns = ('member', 'mpurpose', 'mdescription',)


# Actions

class ApplicationActions(pytis.presentation.Specification):
    public = True
    table = 'c_pytis_menu_actions'
    title = _(u"Uživatelské akce")
    fields = (
        Field('fullname', _(u"Navěšená akce"), editable=Editable.NEVER),
        Field('shortname', _("Action"), editable=Editable.NEVER),
        Field('description', _("Description")),
    )
    columns = ('fullname', 'description',)
    layout = ('fullname', 'description',)
    sorting = (('fullname', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='fullname')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)


class ApplicationShortActions(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_short_actions'
    title = _(u"Uživatelské akce")
    fields = (
        Field('shortname', _("Action"), editable=Editable.NEVER),
    )
    sorting = (('shortname', pytis.data.ASCENDENT,),)
    cb = pytis.presentation.CodebookSpec(display='shortname')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)


# Menus

class _Title(pytis.presentation.PrettyFoldable, pytis.data.String):

    def _init(self, **kwargs):
        super(_Title, self)._init(tree_column_id='position',
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
    table = 'ev_pytis_translated_menu'
    title = _(u"Menu")
    fields = (
        Field('id', _(u"Id"), type=pytis.data.String(),
              default=(lambda: pytis.util.current_language() + '/' +
                       str(nextval('e_pytis_menu_menuid_seq')()))),
        Field('menuid', _(u"MenuId")),
        Field('name', _(u"Id obsahující role")),
        Field('title', _(u"Základní titulek položky menu"), type=_Title()),
        Field('xtitle', _(u"Odvozený titulek položky menu"), type=_Title()),
        Field('t_title', _(u"Přeložený titulek položky menu"), type=_Title(),
              editable=pytis.presentation.computer(lambda row, title: title is not None)),
        Field('t_xtitle', _(u"Titulek položky menu"), type=_Title()),
        Field('language', _(u"Jazyk"), default=pytis.util.current_language),
        Field('position', _(u"Pozice v menu"), fixed=True, not_null=True,
              codebook='menu.ApplicationMenuPositions'),
        Field('next_position', _(u"Následující pozice v menu"), default='0'),
        Field('position_nsub', _(u"Počet poduzlů")),
        Field('fullname', _(u"Navěšená akce"), not_null=True, codebook='menu.ApplicationActions',
              descr=_(u"Akce aplikace vyvolaná položkou menu")),
        Field('xaction', _(u"Navěšená akce"), virtual=True,
              editable=pytis.presentation.Editable.NEVER,
              computer=pytis.presentation.computer(_xaction_computer),
              descr=_(u"Akce aplikace vyvolaná položkou menu")),
        Field('locked', _(u"Zákaz editace"), fixed=True,
              editable=Editable.NEVER),
        Field('dirty', _(u"Neaktuální překlad"), fixed=True,
              editable=Editable.NEVER),
    )
    columns = ('t_xtitle', 'xaction', 'locked',)
    layout = ('title', 't_title', 'position',)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)
    folding = pytis.form.FoldableForm.Folding(level=2)

    def condition(self):
        return pytis.data.EQ('language', pytis.data.sval(pytis.util.current_language()))

    def on_edit_record(self, row):
        if row['locked'].value():
            pytis.form.run_dialog(pytis.form.Warning, _(u"Tuto položku menu nelze editovat"))
            return None
        return pytis.form.run_form(pytis.form.PopupEditForm,
                                   'menu.' + self.__class__.__name__, select_row=row['id'])

    def on_delete_record(self, row):
        if row['locked'].value():
            pytis.form.run_dialog(pytis.form.Warning, _(u"Tuto položku menu nelze smazat"))
            return None
        if row['name'].value():
            pytis.form.run_dialog(pytis.form.Warning, _(u"Koncové položky menu nelze mazat"))
            return None
        wm_value = row['position'].value() + '.*'
        if ((row.data().select(condition=pytis.data.AND(pytis.data.LTreeMatch('position',
                                                                              wm_value),
                                                        pytis.data.NE('position', row['position'])))
             > 0)):
            pytis.form.run_dialog(pytis.form.Warning,
                                  _(u"Nelze mazat položky obsahující jiné položky"))
            return None
        if not pytis.form.run_dialog(pytis.form.Question,
                                     _(u"Opravdu chcete záznam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])


class ApplicationMenuM(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_structure'
    title = _(u"Menu")
    fields = (
        Field('fullname', _(u"Navěšená akce"), not_null=True, codebook='menu.ApplicationActions',
              editable=Editable.NEVER,
              descr=_(u"Akce aplikace vyvolaná položkou menu")),
        Field('shortname', _("Action"), not_null=True, codebook='menu.ApplicationShortActions',
              editable=Editable.NEVER),
        Field('actiontype', _(u"Typ položky"), fixed=True,
              editable=Editable.NEVER),
        Field('menuid', _(u"Id"), default=nextval('e_pytis_menu_menuid_seq'),
              editable=Editable.NEVER),
        Field('title', _(u"Titulek položky menu"), type=_Title(),
              editable=Editable.NEVER),
        Field('position', _(u"Pozice v menu"), fixed=True, not_null=True,
              codebook='menu.ApplicationMenuPositions',
              editable=Editable.NEVER,
              type=pytis.data.LTree(not_null=True)),
        Field('position_nsub', _(u"Počet poduzlů"),
              editable=Editable.NEVER),
        Field('xaction', _(u"Navěšená akce"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              editable=Editable.NEVER,
              descr=_(u"Akce aplikace vyvolaná položkou menu")),
        Field('locked', _(u"Zákaz editace"), fixed=True,
              editable=Editable.NEVER),
        Field('description', _(u"Poznámka")),
    )
    columns = ('title', 'actiontype', 'fullname', 'description',)
    layout = ('title', 'position', 'actiontype', 'fullname', 'description',)

    def bindings(self):
        multiform_row = ApplicationMenuM._multiform_row
        return (pytis.presentation.Binding('role_rights', _(u"Rozpis práv podle rolí"),
                                           'menu.ApplicationMenuRightsFoldable',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           column=pytis.data.sval('roleid',)))),
                pytis.presentation.Binding('column_rights', _(u"Rozpis práv podle sloupců"),
                                           'menu.ApplicationMenuRightsFoldableColumn',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           column=pytis.data.sval('colname',)))),
                pytis.presentation.Binding('summary_rights', _(u"Práva položky menu"),
                                           'menu.ApplicationSummaryRights',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           new=pytis.data.bval(False),
                                                           multirights=multiform_row(row),
                                                           ))),
                pytis.presentation.Binding('preview_rights', _(u"Chystaná práva položky"),
                                           'menu.ApplicationPreviewRights',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           new=pytis.data.bval(True),
                                                           multirights=multiform_row(row),
                                                           ))),
                pytis.presentation.Binding('changes', _(u"Chystané změny"),
                                           'menu.ApplicationChangedRights',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           multirights=multiform_row(row),
                                                           ))),
                pytis.presentation.Binding('users', _(u"Uživatelé"),
                                           'statistics.FormUserStatisticsNoinfo',
                                           condition=(lambda row:
                                                      pytis.data.EQ('shortname',
                                                                    row['shortname']))),
                pytis.presentation.Binding('profiles', _("Profiles"), 'profiles.FormProfiles',
                                           condition=self._profiles_binding_condition),
                pytis.presentation.Binding('settings', _(u"Nastavení"), 'profiles.FormSettings',
                                           condition=self._spec_name_form_name_binding_condition),
                pytis.presentation.Binding('log', _(u"Log Akcí"), 'logging.FormActionLogView',
                                           condition=self._spec_name_form_name_binding_condition),
                )

    def actions(self):
        return (
            pytis.presentation.Action('copy_rights', _(u"Zkopírovat práva z..."), self._copy_rights,
                                      descr=("Zkopírování práv z jiné položky.")),
            pytis.presentation.Action('a_odstranit_nadbytecna_prava',
                                      _(u"Odstranit nadbytečná práva"), self._remove_redundant,
                                      descr=("Odstranění položek práv, které nemají vliv "
                                             "na výsledná práva.")),
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
        result = (fullname[:4] != 'sub/' and
                  (fullname.find('::') >= 0 or fullname.find('.Multi') >= 0))
        return pytis.data.Value(pytis.data.Boolean(), result)

    def _spec_name_form_name_binding_condition(self, row):
        split_fullname = row['fullname'].value().split('/')
        if len(split_fullname) == 5 and split_fullname[0] == 'form':
            return pytis.data.AND(pytis.data.EQ('spec_name', pytis.data.sval(split_fullname[2])),
                                  pytis.data.EQ('form_name', pytis.data.sval(split_fullname[1])))
        else:
            return pytis.data.OR()

    def _profiles_binding_condition(self, row):
        split_shortname = row['shortname'].value().split('/')
        if split_shortname[0] == 'form':
            return pytis.data.EQ('spec_name', pytis.data.sval(split_shortname[1]))
        else:
            return pytis.data.OR()

    # Form actions

    def _copy_rights(self, row):
        template_row = pytis.extensions.run_cb('menu.ApplicationMenuCodebook')
        if template_row is None:
            return
        from_shortname = template_row['shortname']
        to_shortname = row['shortname']
        # Check compatibility of column names (this doesn't do the right thing
        # if there are columns with the same names but different purposes, but
        # it is generally responsibility of the DMP admin to copy rights only
        # between compatible objects).
        rows = pytis.extensions.dbfunction('pytis_columns_in_rights',
                                           ('shortname', from_shortname,))
        if not isinstance(rows, (list, tuple)):
            rows = [[rows]]
        if rows:
            from_columns = set([r[0] for r in rows])
            components = to_shortname.value().split('/')
            spec_name = components[1]
            view_spec = pytis.config.resolver.get(spec_name, 'view_spec')
            to_columns = set([f.id() for f in view_spec.fields()])
            missing_columns = from_columns - to_columns
            if missing_columns:
                message = (_(u"Ve specifikaci chybí tyto sloupce práv: %s.\n"
                             u"Chcete práva přesto zkopírovat?") %
                           (', '.join(list(missing_columns)),))
                if not pytis.form.run_dialog(pytis.form.Question, message):
                    return
        pytis.extensions.dbfunction('pytis_copy_rights',
                                    ('copy_from', from_shortname,), ('copy_to', to_shortname,))

    def _remove_redundant(self, row):
        if not pytis.form.run_dialog(pytis.form.Question,
                                     (_(u"Opravdu chcete odstranit nadbytečná práva položky "
                                        "\"%s\"?") %
                                      (row['title'].value() or '',))):
            return
        pytis.extensions.dbfunction('pytis_remove_redundant', ('shortname', row['shortname'],),)


class ApplicationMenuCodebook(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_structure'
    title = _(u"Menu")
    fields = (
        Field('position', _(u"Pozice v menu"), fixed=True, not_null=True,
              codebook='menu.ApplicationMenuPositions',
              editable=Editable.NEVER),
        Field('position_nsub', _(u"Počet poduzlů"),
              editable=Editable.NEVER),
        Field('fullname', _(u"Navěšená akce"), not_null=True, codebook='menu.ApplicationActions',
              editable=Editable.NEVER,
              descr=_(u"Akce aplikace vyvolaná položkou menu")),
        Field('shortname', _("Action"), not_null=True, codebook='menu.ApplicationShortActions',
              editable=Editable.NEVER),
        Field('actiontype', _(u"Typ položky"), fixed=True,
              editable=Editable.NEVER),
        Field('title', _(u"Titulek položky menu"), type=_Title(),
              editable=Editable.NEVER),
        Field('description', _(u"Poznámka")),
    )
    columns = ('title', 'actiontype', 'description',)
    sorting = (('position', pytis.data.ASCENDENT,),)
    folding = pytis.form.FoldableForm.Folding(level=None)
    access_rights = pytis.data.AccessRights((None, (['admin_menu'],
                                                    pytis.data.Permission.VIEW,
                                                    pytis.data.Permission.EXPORT,
                                                    pytis.data.Permission.PRINT,
                                                    pytis.data.Permission.UPDATE,)))


class ApplicationMenuPositions(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_positions'
    title = _(u"Menu")
    fields = (
        Field('position', _(u"Pozice v menu"), fixed=True, type=pytis.data.LTree()),
        Field('title', _(u"Titulek položky menu"), type=_Title()),
        Field('xtitle', _(u"Titulek položky menu"), type=_Title()),
    )
    columns = ('xtitle',)
    layout = ('title', 'position',)
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)


# Rights

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


class ColnameEnumerator(pytis.data.Enumerator):

    def values(self, argument=None):
        if argument is None:
            return ()
        resolver = pytis.config.resolver
        try:
            specification = resolver.specification(argument)
        except pytis.util.ResolverError:
            return None
        fields = specification.view_spec().fields()
        return [f.id() for f in fields]


class ColnameData(pytis.data.RestrictedMemData):

    def __init__(self, *args, **kwargs):
        self._last_argument = None
        super(ColnameData, self).__init__(*args, **kwargs)

    def _update_data(self, arguments):
        argument = arguments.get('argument')
        if argument == self._last_argument:
            return
        self._last_argument = argument
        if argument is None:
            self._mem_data = []
            return
        resolver = pytis.config.resolver
        try:
            specification = resolver.specification(argument)
        except pytis.util.ResolverError:
            return None
        fields = specification.view_spec().fields()
        self._mem_data = mem_data = []
        for f in fields:
            label = (f.column_label() or f.label() or f.id())
            mem_data.append(pytis.data.Row((('colname', pytis.data.sval(f.id()),),
                                            ('label', pytis.data.sval(label),),)))

    def row(self, key, columns=None, arguments={}, transaction=None):
        self._update_data(arguments)
        return super(ColnameData, self).row(key, columns=columns)

    def select(self, condition=None, reuse=False, sort=None, columns=None, transaction=None,
               arguments={}, async_count=False, stop_check=None, timeout_callback=None, limit=None):
        self._update_data(arguments)
        return super(ColnameData, self).select(condition=condition, reuse=reuse, sort=sort,
                                               columns=columns, transaction=transaction,
                                               arguments=arguments, async_count=async_count,
                                               stop_check=stop_check,
                                               timeout_callback=timeout_callback, limit=limit)

    def search(self, condition, direction=pytis.data.FORWARD, transaction=None, arguments={}):
        self._update_data(arguments)
        return super(ColnameData, self).search(condition, direction=direction,
                                               transaction=transaction)


class ColnameCodebook(pytis.presentation.Specification):
    public = True
    title = _("Sloupce")
    data_cls = ColnameData
    cb = pytis.presentation.CodebookSpec(display='label', enable_autocompletion=False)

    def fields(self):
        return (
            pytis.presentation.Field('colname', _("Identifier"), type=pytis.data.String,
                                     editable=Editable.NEVER, not_null=True),
            pytis.presentation.Field('label', _("Description"), type=pytis.data.String,
                                     editable=Editable.NEVER, not_null=True),
        )


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
            message = _(u"Pozor, tato položka se vyskytuje i na jiných místech menu:")
            for menuid, title in items:
                if menuid != current_menuid:
                    message = message + '\n  ' + (title or _(u"[bez titulku]"))
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
    title = _(u"Práva")

    def fields(self):
        return (
            Field('id', _(u"Id"), default=nextval('e_pytis_action_rights_id_seq')),
            Field('roleid', _(u"Role"), not_null=True, codebook='menu.ApplicationRoles',
                  fixed=True),
            Field('purpose', _(u"Účel role"),
                  editable=Editable.NEVER, fixed=True),
            Field('shortname', _("Action"), editable=Editable.NEVER,
                  not_null=True, codebook='menu.ApplicationShortActions',
                  descr=_(u"Identifikátor akce související s danou položkou menu")),
            Field('colname', _(u"Sloupec"),
                  fixed=True, codebook='menu.ColnameCodebook', not_null=False,
                  runtime_arguments=pytis.presentation.computer(self._specification_arguments),
                  descr=_(u"Sloupec, na který se právo vztahuje")),
            Field('rightid', _(u"Právo"), not_null=True, codebook='menu.ApplicationRights',
                  fixed=True,
                  descr=_(u"Přidělené nebo odebrané právo")),
            Field('system', _(u"Systémové"), fixed=True,
                  descr=_(u"Jde o neměnné právo definováno tvůrcem aplikace?")),
            Field('granted', _(u"Ano/Ne"), fixed=True, default=True,
                  descr=_(u"Je právo povoleno (ano) nebo zakázáno (ne)?")),
            Field('redundant', _(u"Nadbytečné"), fixed=True,
                  editable=Editable.NEVER,
                  descr=_(u"Je toto právo nadbytečné, bez vlivu na výsledná práva?")),
        )
    columns = ('roleid', 'purpose', 'colname', 'rightid', 'system', 'granted', 'redundant',)
    layout = ('shortname', 'roleid', 'rightid', 'granted', 'colname',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('rightid', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)

    def _row_editable(self, row):
        return not row['system'].value()

    def _specification_arguments(self, row, shortname):
        if shortname is None:
            return {}
        components = shortname.split('/')
        if len(components) < 2:
            return {}
        return dict(argument=components[1])

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
            pytis.form.run_dialog(pytis.form.Warning, _(u"Systémová práva nelze editovat"))
            return None
        self._before_edit_checks()
        result = pytis.form.run_form(pytis.form.PopupEditForm,
                                     'menu.' + self.__class__.__name__, select_row=row['id'])
        if result:
            self._after_edit_checks()
        return result

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _(u"Systémová práva nelze mazat"))
            return False
        return True

    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        self._before_edit_checks()
        if not pytis.form.run_dialog(pytis.form.Question,
                                     _(u"Opravdu chcete záznam zcela vymazat?")):
            return None
        result = row.data().delete((row['id'],))
        if result:
            self._after_edit_checks()
        else:
            result = "Řádek se nepodařilo vymazat"
        return result

    @procedure
    def commit_changes(self):
        if pytis.extensions.dbfunction("pytis_update_summary_rights"):
            message = _(u"Změny aplikovány")
        else:
            message = _(u"Provádění změn uzamčeno, změny nebyly aplikovány")
        pytis.form.run_dialog(pytis.form.Message, message)


class _RightsTree(pytis.presentation.PrettyFoldable, pytis.data.String):

    def _init(self, **kwargs):
        super(_RightsTree, self)._init(tree_column_id='tree',
                                       subcount_column_id='subcount',
                                       **kwargs)


class ApplicationMenuRightsFoldable(_ApplicationMenuRightsBase):
    public = True
    table = 'pytis_action_rights_foldable'
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('column', "", type=pytis.data.String()),
                 )
    title = _(u"Práva")
    fields = (
        Field('id', _(u"Id"), type=pytis.data.Integer()),
        Field('tree', _(u"Stromový identifikátor"), type=pytis.data.LTree()),
        Field('subcount', _(u"Počet poduzlů"), type=pytis.data.Integer()),
        Field('roleid', _(u"Role"), type=_RightsTree(),
              fixed=True),
        Field('purpose', _(u"Účel role"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Action"), type=pytis.data.String(),
              descr=_(u"Identifikátor akce související s danou položkou menu")),
        Field('colname', _(u"Sloupec"), type=pytis.data.String(),
              fixed=True,
              descr=_(u"Sloupec, na který se právo vztahuje")),
        Field('rightid', _(u"Právo"), type=pytis.data.String(),
              fixed=True,
              descr=_(u"Přidělené nebo odebrané právo")),
        Field('system', _(u"Systémové"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_(u"Jde o neměnné právo definováno tvůrcem aplikace?")),
        Field('granted', _(u"Ano/Ne"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_(u"Je právo povoleno (ano) nebo zakázáno (ne)?")),
        Field('redundant', _(u"Nadbytečné"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_(u"Je toto právo nadbytečné, bez vlivu na výsledná práva?")),
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
            pytis.form.run_dialog(pytis.form.Warning, _(u"Systémová práva nelze editovat"))
            return None
        self._before_edit_checks()
        result = pytis.form.run_form(pytis.form.PopupEditForm, 'menu.ApplicationMenuRights',
                                     select_row=row['id'])
        if result:
            self._after_edit_checks()
        return result

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            pytis.form.run_dialog(pytis.form.Warning, _(u"Systémová práva nelze mazat"))
            return False
        return True

    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        self._before_edit_checks()
        if not pytis.form.run_dialog(pytis.form.Question,
                                     _(u"Opravdu chcete záznam zcela vymazat?")):
            return None
        data = pytis.util.data_object('menu.ApplicationMenuRights')
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
        Field('id', _(u"Id"), type=pytis.data.Integer()),
        Field('tree', _(u"Stromový identifikátor"), type=pytis.data.LTree()),
        Field('subcount', _(u"Počet poduzlů"), type=pytis.data.Integer()),
        Field('roleid', _(u"Role"), type=pytis.data.String(),
              fixed=True),
        Field('purpose', _(u"Účel role"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Action"), type=pytis.data.String(),
              descr=_(u"Identifikátor akce související s danou položkou menu")),
        Field('colname', _(u"Sloupec"), type=_RightsTree(),
              fixed=True,
              descr=_(u"Sloupec, na který se právo vztahuje")),
        Field('rightid', _(u"Právo"), type=pytis.data.String(),
              fixed=True,
              descr=_(u"Přidělené nebo odebrané právo")),
        Field('system', _(u"Systémové"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_(u"Jde o neměnné právo definováno tvůrcem aplikace?")),
        Field('granted', _(u"Ano/Ne"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_(u"Je právo povoleno (ano) nebo zakázáno (ne)?")),
        Field('redundant', _(u"Nadbytečné"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_(u"Je toto právo nadbytečné, bez vlivu na výsledná práva?")),
    )
    columns = ('colname', 'roleid', 'purpose', 'rightid', 'system', 'granted', 'redundant',)


class _MenuidPreviewType(pytis.data.Integer):

    def default_value(self):
        return pytis.data.Value(self, 0)


class ApplicationSummaryRights(pytis.presentation.Specification):
    public = True
    table = 'pytis_view_summary_rights'
    title = _(u"Souhrnná práva")
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 Field('multirights', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('roleid', _(u"Role"), type=pytis.data.String(not_null=True),
              fixed=True),
        Field('purpose', _(u"Účel role"), type=pytis.data.String(),
              editable=Editable.NEVER, fixed=True),
        Field('columns', _(u"Sloupce"), type=pytis.data.String(),
              editable=Editable.NEVER,
              descr=_(u"Sloupce, na které je právo aplikováno; není-li uvedeno, tak všechny.")),
        Field('rights', _(u"Práva"), type=pytis.data.String(), fixed=True),
        Field('rights_show', _(u"Menu"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Zobrazení v menu")),
        Field('rights_view', _(u"Náhled"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Zobrazení formuláře")),
        Field('rights_insert', _(u"Vložení"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Vložení nového záznamu")),
        Field('rights_update', _(u"Editace"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Změna hodnot existujícího záznamu")),
        Field('rights_delete', _(u"Smazání"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Smazání existujícího záznamu")),
        Field('rights_print', _(u"Tisk"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Tisk formuláře")),
        Field('rights_export', _(u"Export"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Export formuláře do souboru")),
        Field('rights_call', _(u"Spuštění"), type=pytis.data.Boolean(), fixed=True,
              descr=_(u"Spuštění funkce (netýká se formulářů)")),
    )
    columns = ('roleid', 'purpose', 'columns', 'rights_show', 'rights_view', 'rights_insert',
               'rights_update', 'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
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
    title = _(u"Chystaná práva")


class ApplicationChangedRights(ApplicationSummaryRights):
    public = True
    table = 'pytis_changed_rights'
    title = _(u"Chystané změny práv")
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('roleid', "", type=pytis.data.String()),
                 Field('multirights', "", type=pytis.data.Boolean()),
                 )
    fields = (ApplicationSummaryRights.fields +
              (Field('change', _(u"Nové"), type=pytis.data.Boolean(),
                     editable=Editable.NEVER, fixed=True),))
    _STYLE_OLD = pytis.presentation.Style(background=pytis.presentation.Color.GRAY10)

    def row_style(self, row):
        if not row['change'].value():
            return self._STYLE_OLD


class ApplicationRoleMenu(pytis.presentation.Specification):
    public = True
    table = 'pytis_view_role_menu'
    title = _(u"Menu uživatele")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('menuid', _(u"Identifikátor položky menu"), type=pytis.data.Integer()),
        Field('roleid', _(u"Role"), type=pytis.data.String(),
              fixed=True),
        Field('title', _(u"Titulek položky menu"), type=_Title(), fixed=True),
        Field('position', _(u"Pozice v menu"), type=pytis.data.String()),
        Field('position_nsub', _(u"Počet poduzlů"), type=pytis.data.Integer()),
        Field('rights', _(u"Práva"), type=pytis.data.String()),
        Field('rights_show', _(u"Menu"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Zobrazení v menu")),
        Field('rights_view', _(u"Náhled"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Zobrazení formuláře")),
        Field('rights_insert', _(u"Vložení"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Vložení nového záznamu")),
        Field('rights_update', _(u"Editace"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Změna hodnot existujícího záznamu")),
        Field('rights_delete', _(u"Smazání"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Smazání existujícího záznamu")),
        Field('rights_print', _(u"Tisk"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Tisk formuláře")),
        Field('rights_export', _(u"Export"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Export formuláře do souboru")),
        Field('rights_call', _(u"Spuštění"), fixed=True, type=pytis.data.Boolean(),
              descr=_(u"Spuštění funkce (netýká se formulářů)")),
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
    title = _(u"Chystané menu uživatele")


class ApplicationRoleMenuExtended(ApplicationRoleMenu):
    public = True
    table = 'pytis_view_extended_role_menu'
    title = _(u"Rozšířené menu uživatele")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (ApplicationRoleMenu.fields +
              (Field('actiontype', _(u"Typ položky"), fixed=True,
                     editable=Editable.NEVER,
                     type=pytis.data.String()),))
    columns = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
              'rights_delete', 'rights_print', 'rights_export', 'rights_call',)


class ApplicationPreviewRoleMenuExtended(ApplicationRoleMenuExtended):
    public = True
    table = 'pytis_view_extended_role_menu'
    title = _(u"Chystané rozšířené menu uživatele")
