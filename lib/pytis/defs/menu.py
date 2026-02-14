# -*- coding: utf-8 -*-

# Copyright (C) 2019-2026 Tomáš Cerha <t.cerha@gmail.com>
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

from __future__ import unicode_literals
from __future__ import print_function

import copy

import pytis.data
import pytis.dbdefs.db_pytis_menu as dbdefs
import pytis.extensions
import pytis.presentation
import pytis.util
from pytis.api import app
from pytis.util import nextval
from pytis.presentation import Editable, Field, procedure


_ = pytis.util.translations('pytis-defs')

# Roles


class ApplicationRolePurposes(pytis.presentation.Specification):
    public = True
    table = 'c_pytis_role_purposes'
    title = _("Role Purposes")
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
            app.warning(_("Administrator roles cannot be edited"))
            return None
        return app.edit_record(self, row=row, block_on_edit_record=True)

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            app.warning(_("Administrator roles cannot be deleted"))
            return False
        return True

    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        if not app.question(_("Do you really want to delete this record permanently?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])


class ApplicationRoles(_ApplicationRolesSpecification):
    public = True
    table = 'ev_pytis_roles'
    title = _("Roles")
    fields = (
        Field('roleid', _("Group"),     # to allow binding of ApplicationRolesMembers
              virtual=True, editable=Editable.NEVER,
              computer=pytis.presentation.computer(lambda row, name: name)),
        Field('member', _("Included Role"),     # to allow binding of ApplicationRolesOwners
              virtual=True, editable=Editable.NEVER,
              computer=pytis.presentation.computer(lambda row, name: name)),
        Field('name', _("Title"), fixed=True,
              descr=_("A short role name or database username.")),
        Field('description', _("Description"),
              descr=_("Role description.")),
        Field('purposeid', _("Purpose"), not_null=True,
              codebook='menu.CommonApplicationRolePurposes',
              fixed=True, selection_type=pytis.presentation.SelectionType.CHOICE,
              descr=_("Role purpose: normal, user account, administrator.")),
        Field('purpose', _("Purpose"), fixed=True,
              descr=_("Purpose meaning: normal, user account, administrator.")),
        Field('deleted', _("Deactivation Date"), fixed=True,
              descr=_("If set, the role is inactive from that date.")),
    )
    columns = ('name', 'description', 'purpose', 'deleted',)
    layout = ('name', 'description', 'purposeid', 'deleted',)
    sorting = (('name', pytis.data.ASCENDENT,),)

    def profiles(self):
        return pytis.presentation.Profiles(
            pytis.presentation.Profile(
                'nezrusene-role', _("Active roles"),
                filter=pytis.data.EQ('deleted', pytis.data.dval(None))),
            default="nezrusene-role"
        )

    def row_style(self, row):
        if row["deleted"].value() is not None:
            return pytis.presentation.Style(background=pytis.presentation.Color.GRAY10,
                                            foreground=pytis.presentation.Color.BLACK)

    def actions(self):
        return (
            pytis.presentation.Action(
                'copy_roles', _("Copy roles from..."), self._copy_roles,
                descr=_("Configure application roles based on another user.")),
        )
    bindings = (pytis.presentation.Binding('members', _("Contains roles"),
                                           'menu.ApplicationRolesMembers',
                                           binding_column='roleid'),
                pytis.presentation.Binding('owners', _("Belongs to roles"),
                                           'menu.ApplicationRolesOwners',
                                           binding_column='member'),
                pytis.presentation.Binding('menu', _("Menu preview"), 'menu.ApplicationRoleMenu',
                                           arguments=(lambda row:
                                                      dict(roleid=row['roleid'],
                                                           new=pytis.data.bval(False)))),
                pytis.presentation.Binding('previewmenu', _("Preview of pending menu"),
                                           'menu.ApplicationPreviewRoleMenu',
                                           arguments=(lambda row:
                                                      dict(roleid=row['roleid'],
                                                           new=pytis.data.bval(True)))),
                pytis.presentation.Binding('extmenu', _("Extended menu preview"),
                                           'menu.ApplicationRoleMenuExtended',
                                           arguments=(lambda row: dict(roleid=row['roleid']))),
                pytis.presentation.Binding('previewextmenu', _("Preview of pending extended menu"),
                                           'menu.ApplicationPreviewRoleMenuExtended',
                                           arguments=(lambda row:
                                                      dict(roleid=row['roleid'],
                                                           new=pytis.data.bval(True)))),
                )

    def on_delete_record(self, row):
        if self._row_deleteable(row):
            app.warning(_("Roles cannot be deleted; set the deactivation date."))
        return None

    def _copy_roles(self, row):
        template = app.codebook('menu.UserApplicationRolesCodebook')
        if template is None:
            return
        pytis.data.dbfunction(dbdefs.PytisCopyRole, template['name'].value(), row['name'].value())


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
    title = _("Role membership")
    fields = (
        Field('id', _("Id"), default=nextval('e_pytis_role_members_id_seq')),
        Field('roleid', _("Group"), fixed=True, not_null=True,
              codebook='menu.ApplicationApplicationRoles',
              descr=_("A role that contains other roles.")),
        Field('member', _("Included Role"), fixed=True, not_null=True,
              codebook='menu.CommonApplicationRoles',
              descr=_("A role that is a member of the group role.")),
        Field('purposeid', _("Role purpose identifier"), not_null=True,
              codebook='menu.ApplicationRolePurposes'),
        Field('purpose', _("Purpose"),
              editable=Editable.NEVER, fixed=True),
        Field('mpurposeid', _("Role purpose identifier"), not_null=True,
              codebook='menu.ApplicationRolePurposes'),
        Field('mpurpose', _("Purpose"),
              editable=Editable.NEVER, fixed=True),
        Field('description', _("Description"),
              descr=_("Role description.")),
        Field('mdescription', _("Description"),
              descr=_("Role description.")),
    )
    columns = ('roleid', 'member',)
    layout = ('roleid', 'member',)
    sorting = (('roleid', pytis.data.ASCENDENT,), ('member', pytis.data.ASCENDENT,),)

    def _row_editable(self, row):
        # This is here to prevent deletion of admin role memberships
        return row['purposeid'].value() != 'admn' and row['mpurposeid'].value() != 'admn'

    def on_edit_record(self, row):
        app.warning(_("Role memberships cannot be edited; only added or deleted."))
        return None


class ApplicationRolesOwners(ApplicationRolesMembership):
    public = True
    title = _("Group membership")
    columns = ('roleid', 'purpose', 'description',)


class ApplicationRolesMembers(ApplicationRolesMembership):
    public = True
    title = _("Role assignment")
    columns = ('member', 'mpurpose', 'mdescription',)


# Actions

class ApplicationActions(pytis.presentation.Specification):
    public = True
    table = 'c_pytis_menu_actions'
    title = _("User Actions")
    fields = (
        Field('fullname', _("Attached action"), editable=Editable.NEVER),
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
    title = _("User Actions")
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
    title = _("Menu")
    fields = (
        Field('id', _("Id"), type=pytis.data.String(),
              default=(lambda: pytis.util.current_language() + '/' +
                       str(nextval('e_pytis_menu_menuid_seq')()))),
        Field('menuid', _("MenuId")),
        Field('name', _("ID containing roles")),
        Field('title', _("Base menu item title"), type=_Title()),
        Field('xtitle', _("Derived menu item title"), type=_Title()),
        Field('t_title', _("Translated menu item title"), type=_Title(),
              editable=pytis.presentation.computer(lambda row, title: title is not None)),
        Field('t_xtitle', _("Menu item title"), type=_Title()),
        Field('language', _("Language"), default=pytis.util.current_language),
        Field('position', _("Menu position"), fixed=True, not_null=True,
              codebook='menu.ApplicationMenuPositions'),
        Field('next_position', _("Next menu position"), default='0'),
        Field('position_nsub', _("Number of child nodes")),
        Field('fullname', _("Attached action"), not_null=True, codebook='menu.ApplicationActions',
              descr=_("Application action triggered by the menu item")),
        Field('xaction', _("Attached action"), virtual=True,
              editable=pytis.presentation.Editable.NEVER,
              computer=pytis.presentation.computer(_xaction_computer),
              descr=_("Application action triggered by the menu item")),
        Field('locked', _("Locked"), fixed=True,
              editable=Editable.NEVER),
        Field('dirty', _("Outdated translation"), fixed=True,
              editable=Editable.NEVER),
    )
    columns = ('t_xtitle', 'xaction', 'locked',)
    layout = ('title', 't_title', 'position',)
    cb = pytis.presentation.CodebookSpec(display='title')
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)
    folding = pytis.presentation.Folding(level=2)

    def condition(self):
        return pytis.data.EQ('language', pytis.data.sval(pytis.util.current_language()))

    def on_edit_record(self, row):
        if row['locked'].value():
            app.warning(_("This menu item cannot be edited"))
            return None
        return app.edit_record(self, row=row, block_on_edit_record=True)

    def on_delete_record(self, row):
        if row['locked'].value():
            app.warning(_("This menu item cannot be deleted"))
            return None
        if row['name'].value():
            app.warning(_("Leaf menu items cannot be deleted"))
            return None
        wm_value = row['position'].value() + '.*'
        if ((row.data().select(condition=pytis.data.AND(pytis.data.LTreeMatch('position',
                                                                              wm_value),
                                                        pytis.data.NE('position', row['position'])))
             > 0)):
            app.warning(_("Cannot delete items that contain other items"))
            return None
        if not app.question(_("Do you really want to delete this record permanently?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])


class ApplicationMenuM(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_structure'
    title = _("Menu")
    fields = (
        Field('fullname', _("Attached action"), not_null=True, codebook='menu.ApplicationActions',
              editable=Editable.NEVER,
              descr=_("Application action triggered by the menu item")),
        Field('shortname', _("Action"), not_null=True, codebook='menu.ApplicationShortActions',
              editable=Editable.NEVER),
        Field('actiontype', _("Item type"), fixed=True,
              editable=Editable.NEVER),
        Field('menuid', _("Id"), default=nextval('e_pytis_menu_menuid_seq'),
              editable=Editable.NEVER),
        Field('title', _("Menu item title"), type=_Title(),
              editable=Editable.NEVER),
        Field('position', _("Menu position"), fixed=True, not_null=True,
              codebook='menu.ApplicationMenuPositions',
              editable=Editable.NEVER,
              type=pytis.data.LTree(not_null=True)),
        Field('position_nsub', _("Number of child nodes"),
              editable=Editable.NEVER),
        Field('xaction', _("Attached action"), virtual=True,
              computer=pytis.presentation.computer(_xaction_computer),
              editable=Editable.NEVER,
              descr=_("Application action triggered by the menu item")),
        Field('locked', _("Locked"), fixed=True,
              editable=Editable.NEVER),
        Field('description', _("Note")),
    )
    columns = ('title', 'actiontype', 'fullname', 'description',)
    layout = ('title', 'position', 'actiontype', 'fullname', 'description',)

    def bindings(self):
        multiform_row = ApplicationMenuM._multiform_row
        return (pytis.presentation.Binding('role_rights', _("Rights breakdown by roles"),
                                           'menu.ApplicationMenuRightsFoldable',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           column=pytis.data.sval('roleid',)))),
                pytis.presentation.Binding('column_rights', _("Rights breakdown by columns"),
                                           'menu.ApplicationMenuRightsFoldableColumn',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           column=pytis.data.sval('colname',)))),
                pytis.presentation.Binding('summary_rights', _("Menu item rights"),
                                           'menu.ApplicationSummaryRights',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           new=pytis.data.bval(False),
                                                           multirights=multiform_row(row),
                                                           ))),
                pytis.presentation.Binding('preview_rights', _("Pending menu item rights"),
                                           'menu.ApplicationPreviewRights',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           new=pytis.data.bval(True),
                                                           multirights=multiform_row(row),
                                                           ))),
                pytis.presentation.Binding('changes', _("Pending changes"),
                                           'menu.ApplicationChangedRights',
                                           arguments=(lambda row:
                                                      dict(shortname=row['shortname'],
                                                           multirights=multiform_row(row),
                                                           ))),
                pytis.presentation.Binding('users', _("Users"),
                                           'statistics.FormUserStatisticsNoinfo',
                                           condition=(lambda row:
                                                      pytis.data.EQ('shortname',
                                                                    row['shortname']))),
                pytis.presentation.Binding('profiles', _("Profiles"), 'profiles.FormProfiles',
                                           condition=self._profiles_binding_condition),
                pytis.presentation.Binding('settings', _("Settings"), 'profiles.FormSettings',
                                           condition=self._spec_name_form_name_binding_condition),
                )

    def actions(self):
        return (
            pytis.presentation.Action('copy_rights', _("Copy rights from..."), self._copy_rights,
                                      descr=_("Copy permissions from another item.")),
            pytis.presentation.Action('a_odstranit_nadbytecna_prava',
                                      _("Remove redundant rights"), self._remove_redundant,
                                      descr=_("Remove permission entries that have no effect "
                                              "on the resulting permissions.")),
        )
    access_rights = pytis.data.AccessRights((None, (['admin_menu'],
                                                    pytis.data.Permission.VIEW,
                                                    pytis.data.Permission.EXPORT,
                                                    pytis.data.Permission.PRINT,
                                                    pytis.data.Permission.UPDATE,)))

    folding = pytis.presentation.Folding(level=2)
    #folding.expand('8', level=0)

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
        template_row = app.codebook('menu.ApplicationMenuCodebook')
        if template_row is None:
            return
        from_shortname = template_row['shortname'].value()
        to_shortname = row['shortname'].value()
        # Check compatibility of column names (this doesn't do the right thing
        # if there are columns with the same names but different purposes, but
        # it is generally responsibility of the DMP admin to copy rights only
        # between compatible objects).
        rows = pytis.data.dbfunction(dbdefs.PytisColumnsInRights, from_shortname)
        if not isinstance(rows, (list, tuple)):
            rows = [[rows]]
        if rows:
            from_columns = set([r[0] for r in rows])
            components = to_shortname.split('/')
            spec_name = components[1]
            view_spec = pytis.config.resolver.get(spec_name, 'view_spec')
            to_columns = set([f.id() for f in view_spec.fields()])
            missing_columns = from_columns - to_columns
            if missing_columns:
                if not app.question(_("This specification is missing these rights columns: %s.\n"
                                      u"Do you want to copy the rights anyway?") %
                                    (', '.join(list(missing_columns)),)):
                    return
        pytis.data.dbfunction(dbdefs.PytisCopyRights, from_shortname, to_shortname)

    def _remove_redundant(self, row):
        if not app.question((_("Do you really want to remove redundant rights for the item "
                               "\"%s\"?") %
                             (row['title'].value() or '',))):
            return
        pytis.data.dbfunction(dbdefs.PytisRemoveRedundant, row['shortname'].value())


class ApplicationMenuCodebook(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_menu_structure'
    title = _("Menu")
    fields = (
        Field('position', _("Menu position"), fixed=True, not_null=True,
              codebook='menu.ApplicationMenuPositions',
              editable=Editable.NEVER),
        Field('position_nsub', _("Number of child nodes"),
              editable=Editable.NEVER),
        Field('fullname', _("Attached action"), not_null=True, codebook='menu.ApplicationActions',
              editable=Editable.NEVER,
              descr=_("Application action triggered by the menu item")),
        Field('shortname', _("Action"), not_null=True, codebook='menu.ApplicationShortActions',
              editable=Editable.NEVER),
        Field('actiontype', _("Item type"), fixed=True,
              editable=Editable.NEVER),
        Field('title', _("Menu item title"), type=_Title(),
              editable=Editable.NEVER),
        Field('description', _("Note")),
    )
    columns = ('title', 'actiontype', 'description',)
    sorting = (('position', pytis.data.ASCENDENT,),)
    folding = pytis.presentation.Folding(level=None)
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
        Field('position', _("Menu position"), fixed=True, type=pytis.data.LTree()),
        Field('title', _("Menu item title"), type=_Title()),
        Field('xtitle', _("Menu item title"), type=_Title()),
    )
    columns = ('xtitle',)
    layout = ('title', 'position',)
    access_rights = pytis.data.AccessRights((None, (['admin_menu'], pytis.data.Permission.ALL)),)


# Rights

class ApplicationRights(pytis.presentation.Specification):
    public = True
    table = 'c_pytis_access_rights'
    title = _("Rights list")
    fields = (
        Field('rightid', _("Right"), fixed=True),
        Field('description', _("Description")),
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
    title = _("Columns")
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
        main_form = app.main_form
        shortname = main_form.row['shortname']
        if not shortname.value():
            return
        data = main_form.row.data()
        items = data.select_map(lambda row: (row['menuid'].value(), row['title'].value()),
                                condition=pytis.data.EQ('shortname', shortname))
        if len(items) > 1:
            current_menuid = main_form.row['menuid'].value()
            message = _("Warning, this item also appears elsewhere in the menu:")
            for menuid, title in items:
                if menuid != current_menuid:
                    message = message + '\n  ' + (title or _("[no title]"))
            app.warning(message)

    def _codebook_rights_check(self):
        shortname = app.main_form.row['shortname'].value()
        components = shortname.split('/')
        if components[0] != 'form':
            return
        spec_name = components[1]
        menu_checker = pytis.extensions.MenuChecker()
        errors = (menu_checker.check_codebook_rights(spec_name, new=True) +
                  menu_checker.check_reverse_codebook_rights(spec_name, new=True))
        if errors:
            app.warning(_("Contradictory codebook access rights."),
                        content=('\n'.join(errors)) + '\n\n\n')

    def _before_edit_checks(self):
        self._multiaction_check()

    def _after_edit_checks(self):
        self._codebook_rights_check()


class ApplicationMenuRights(_ApplicationMenuRightsBase):
    public = True
    table = 'ev_pytis_action_rights'
    title = _("Rights")

    def fields(self):
        return (
            Field('id', _("Id"), default=nextval('e_pytis_action_rights_id_seq')),
            Field('roleid', _("Role"), not_null=True, codebook='menu.ApplicationRoles',
                  fixed=True),
            Field('purpose', _("Purpose"),
                  editable=Editable.NEVER, fixed=True),
            Field('shortname', _("Action"), editable=Editable.NEVER,
                  not_null=True, codebook='menu.ApplicationShortActions',
                  descr=_("Identifier of the action associated with this menu item")),
            Field('colname', _("Column"),
                  fixed=True, codebook='menu.ColnameCodebook', not_null=False,
                  runtime_arguments=pytis.presentation.computer(self._specification_arguments),
                  descr=_("The column the right applies to")),
            Field('rightid', _("Right"), not_null=True, codebook='menu.ApplicationRights',
                  fixed=True,
                  descr=_("The granted or revoked right")),
            Field('system', _("System"), fixed=True,
                  descr=_("Is this an immutable right defined by the application author?")),
            Field('granted', _("Yes/No"), fixed=True, default=True,
                  descr=_("Is the right granted (yes) or denied (no)?")),
            Field('redundant', _("Redundant"), fixed=True,
                  editable=Editable.NEVER,
                  descr=_("Is this right redundant, without effect on the resulting rights?")),
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

    def on_new_record(self, prefill=None, transaction=None):
        self._before_edit_checks()
        result = app.new_record(self, prefill=prefill, transaction=transaction,
                                block_on_new_record=True)
        if result:
            self._after_edit_checks()
        return result

    def on_edit_record(self, row):
        if not self._row_editable(row):
            app.warning(_("System rights cannot be edited"))
            return None
        self._before_edit_checks()
        result = app.edit_record(self, row=row, block_on_edit_record=True)
        if result:
            self._after_edit_checks()
        return result

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            app.warning(_("System rights cannot be deleted"))
            return False
        return True

    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        self._before_edit_checks()
        if not app.question(_("Do you really want to delete this record permanently?")):
            return None
        result = row.data().delete((row['id'],))
        if result:
            self._after_edit_checks()
        else:
            result = _("Failed to delete the row.")
        return result

    @procedure
    def commit_changes(self):
        if pytis.data.dbfunction(dbdefs.PytisUpdateSummaryRights):
            message = _("Changes applied")
        else:
            message = _("Applying changes is locked; changes were not applied")
        app.message(message)


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
    title = _("Rights")
    fields = (
        Field('id', _("Id"), type=pytis.data.Integer()),
        Field('tree', _("Tree identifier"), type=pytis.data.LTree()),
        Field('subcount', _("Number of child nodes"), type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=_RightsTree(),
              fixed=True),
        Field('purpose', _("Purpose"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Action"), type=pytis.data.String(),
              descr=_("Identifier of the action associated with this menu item")),
        Field('colname', _("Column"), type=pytis.data.String(),
              fixed=True,
              descr=_("The column the right applies to")),
        Field('rightid', _("Right"), type=pytis.data.String(),
              fixed=True,
              descr=_("The granted or revoked right")),
        Field('system', _("System"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Is this an immutable right defined by the application author?")),
        Field('granted', _("Yes/No"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_("Is the right granted (yes) or denied (no)?")),
        Field('redundant', _("Redundant"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Is this right redundant, without effect on the resulting rights?")),
    )
    columns = ('roleid', 'purpose', 'colname', 'rightid', 'system', 'granted', 'redundant',)
    layout = ('shortname', 'roleid', 'purpose', 'rightid', 'granted',)
    sorting = (('tree', pytis.data.ASCENDENT,),)
    access_rights = pytis.data.AccessRights((None, (['admin'], pytis.data.Permission.ALL)),)

    def on_new_record(self, prefill=None, transaction=None):
        shortname = app.main_form.row['shortname']
        if prefill is None:
            prefill = {}
        else:
            prefill = copy.copy(prefill)
        prefill['shortname'] = shortname
        self._before_edit_checks()
        new = app.new_record('menu.ApplicationMenuRights',
                             prefill=prefill, transaction=transaction,
                             block_on_new_record=True, multi_insert=True)
        if new:
            self._after_edit_checks()
        return new or None

    def _row_editable(self, row):
        return not row['system'].value() and row['id'].value() >= 0

    def on_edit_record(self, row):
        if not self._row_editable(row):
            app.warning(_("System rights cannot be edited"))
            return None
        self._before_edit_checks()
        result = app.edit_record('menu.ApplicationMenuRights', row=row, block_on_edit_record=True)
        if result:
            self._after_edit_checks()
        return result

    def _row_deleteable(self, row):
        if not self._row_editable(row):
            app.warning(_("System rights cannot be deleted"))
            return False
        return True

    def on_delete_record(self, row):
        if not self._row_deleteable(row):
            return None
        self._before_edit_checks()
        if not app.question(_("Do you really want to delete this record permanently?")):
            return None
        data = pytis.util.data_object('menu.ApplicationMenuRights')
        result = data.delete((row['id'],))
        if result:
            self._after_edit_checks()
        else:
            result = _("Failed to delete the row.")
        return 1


class ApplicationMenuRightsFoldableColumn(ApplicationMenuRightsFoldable):
    public = True
    table = 'pytis_action_rights_foldable'
    fields = (
        Field('id', _("Id"), type=pytis.data.Integer()),
        Field('tree', _("Tree identifier"), type=pytis.data.LTree()),
        Field('subcount', _("Number of child nodes"), type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=pytis.data.String(),
              fixed=True),
        Field('purpose', _("Purpose"), type=pytis.data.String(),
              fixed=True),
        Field('shortname', _("Action"), type=pytis.data.String(),
              descr=_("Identifier of the action associated with this menu item")),
        Field('colname', _("Column"), type=_RightsTree(),
              fixed=True,
              descr=_("The column the right applies to")),
        Field('rightid', _("Right"), type=pytis.data.String(),
              fixed=True,
              descr=_("The granted or revoked right")),
        Field('system', _("System"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Is this an immutable right defined by the application author?")),
        Field('granted', _("Yes/No"), type=pytis.data.Boolean(),
              fixed=True, default=True,
              descr=_("Is the right granted (yes) or denied (no)?")),
        Field('redundant', _("Redundant"), type=pytis.data.Boolean(),
              fixed=True,
              descr=_("Is this right redundant, without effect on the resulting rights?")),
    )
    columns = ('colname', 'roleid', 'purpose', 'rightid', 'system', 'granted', 'redundant',)


class _MenuidPreviewType(pytis.data.Integer):

    def default_value(self):
        return pytis.data.Value(self, 0)


class ApplicationSummaryRights(pytis.presentation.Specification):
    public = True
    table = 'pytis_view_summary_rights'
    title = _("Summary rights")
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 Field('multirights', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('roleid', _("Role"), type=pytis.data.String(not_null=True),
              fixed=True),
        Field('purpose', _("Purpose"), type=pytis.data.String(),
              editable=Editable.NEVER, fixed=True),
        Field('columns', _("Columns"), type=pytis.data.String(),
              editable=Editable.NEVER,
              descr=_("Columns the right applies to; if empty, all columns.")),
        Field('rights', _("Rights"), type=pytis.data.String(), fixed=True),
        Field('rights_show', _("Menu"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Show in menu")),
        Field('rights_view', _("View"), type=pytis.data.Boolean(), fixed=True,
              descr=_("View the form")),
        Field('rights_insert', _("Insert"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Insert a new record")),
        Field('rights_update', _("Update"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Update an existing record")),
        Field('rights_delete', _("Delete"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Delete an existing record")),
        Field('rights_print', _("Print"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Print the form")),
        Field('rights_export', _("Export"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Export the form to a file")),
        Field('rights_call', _("Run"), type=pytis.data.Boolean(), fixed=True,
              descr=_("Run a function (not applicable to forms)")),
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
    title = _("Pending rights")


class ApplicationChangedRights(ApplicationSummaryRights):
    public = True
    table = 'pytis_changed_rights'
    title = _("Pending rights changes")
    arguments = (Field('shortname', "", type=pytis.data.String()),
                 Field('roleid', "", type=pytis.data.String()),
                 Field('multirights', "", type=pytis.data.Boolean()),
                 )
    fields = (ApplicationSummaryRights.fields +
              (Field('change', _("New"), type=pytis.data.Boolean(),
                     editable=Editable.NEVER, fixed=True),))
    _STYLE_OLD = pytis.presentation.Style(background=pytis.presentation.Color.GRAY10)

    def row_style(self, row):
        if not row['change'].value():
            return self._STYLE_OLD


class ApplicationRoleMenu(pytis.presentation.Specification):
    public = True
    table = 'pytis_view_role_menu'
    title = _("User menu")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (
        Field('menuid', _("Menu item identifier"), type=pytis.data.Integer()),
        Field('roleid', _("Role"), type=pytis.data.String(),
              fixed=True),
        Field('title', _("Menu item title"), type=_Title(), fixed=True),
        Field('position', _("Menu position"), type=pytis.data.String()),
        Field('position_nsub', _("Number of child nodes"), type=pytis.data.Integer()),
        Field('rights', _("Rights"), type=pytis.data.String()),
        Field('rights_show', _("Menu"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Show in menu")),
        Field('rights_view', _("View"), fixed=True, type=pytis.data.Boolean(),
              descr=_("View the form")),
        Field('rights_insert', _("Insert"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Insert a new record")),
        Field('rights_update', _("Update"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Update an existing record")),
        Field('rights_delete', _("Delete"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Delete an existing record")),
        Field('rights_print', _("Print"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Print the form")),
        Field('rights_export', _("Export"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Export the form to a file")),
        Field('rights_call', _("Run"), fixed=True, type=pytis.data.Boolean(),
              descr=_("Run a function (not applicable to forms)")),
    )
    columns = ('title', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
              'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    folding = pytis.presentation.Folding(level=None)
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
    title = _("Pending user menu")


class ApplicationRoleMenuExtended(ApplicationRoleMenu):
    public = True
    table = 'pytis_view_extended_role_menu'
    title = _("Extended user menu")
    arguments = (Field('roleid', "", type=pytis.data.String()),
                 Field('new', "", type=pytis.data.Boolean()),
                 )
    fields = (ApplicationRoleMenu.fields +
              (Field('actiontype', _("Item type"), fixed=True,
                     editable=Editable.NEVER,
                     type=pytis.data.String()),))
    columns = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
               'rights_delete', 'rights_print', 'rights_export', 'rights_call',)
    layout = ('title', 'actiontype', 'roleid', 'rights_view', 'rights_insert', 'rights_update',
              'rights_delete', 'rights_print', 'rights_export', 'rights_call',)


class ApplicationPreviewRoleMenuExtended(ApplicationRoleMenuExtended):
    public = True
    table = 'pytis_view_extended_role_menu'
    title = _("Pending extended user menu")
