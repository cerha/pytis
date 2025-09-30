# -*- coding: utf-8 -*-

# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2009-2017 OUI Technology Ltd.
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

"""Pytis CMS specifications.

Pytis CMS is a fat client application for management of database structures, which define the
behavior of a web application.

Wiking modules which implement a Wiking application, which reads these database structures and
behaves according to them can be found in the 'web' submodule of this module.

"""
from __future__ import unicode_literals
from __future__ import print_function

import os
import socket

import lcg
import pytis
import pytis.data as pd
import pytis.presentation as pp
import pytis.util
from pytis.presentation import Field, VGroup, Binding, Action, CodebookSpec, computer
from pytis.api import app
from pytis.dbdefs import db_pytis_cms as dbdefs

_ = pytis.util.translations('pytis-defs')

ASC = pd.ASCENDENT
DESC = pd.DESCENDANT
ALWAYS = pp.Editable.ALWAYS
ONCE = pp.Editable.ONCE
NEVER = pp.Editable.NEVER


def nextval(seq):
    def conn_spec():
        return pytis.config.dbconnection
    counter = pd.DBCounterDefault(seq, conn_spec)
    return lambda transaction=None: counter.next(transaction=transaction)



class _TreeOrder(pp.PrettyFoldable, pd.String):

    def _init(self, **kwargs):
        super(_TreeOrder, self)._init(tree_column_id='tree_order',
                                      subcount_column_id='tree_order_nsub',
                                      **kwargs)


class Specification(pp.Specification):
    access_rights = pd.AccessRights((None, (('cms_user', 'cms_admin'), pd.Permission.ALL)))
    public = True

    def _name(self, name, needed_in_wiking=True):
        # Hack to allow namespaced spec names in wx app and plain module names
        # in Wiking (method overriden in pytis.cms.web.Specification).
        return 'cms.' + name


class Languages(Specification):
    title = _("Languages")
    help = _("Management of languages available in CMS.")
    table = dbdefs.CmsLanguages

    def _customize_fields(self, fields):
        fields.modify('lang_id', default=nextval('cms_languages_lang_id_seq'))
        fields.modify('lang', label=_("Code"), width=2, column_width=6, fixed=True,
                      filter=pp.TextFilter.ALPHANUMERIC, post_process=pp.PostProcess.LOWER)
        fields.append(Field('name', _("Title"), virtual=True, editable=NEVER,
                            computer=computer(lambda record, lang: self._language_name(lang))))

    def _language_name(self, lang):
        lcg_dir = os.environ.get('LCGDIR', '/usr/local/share/lcg')
        translation_dir = os.path.join(lcg_dir, 'translations')
        t = lcg.GettextTranslator('cs', path=(translation_dir,), fallback=True)
        return t.gettext(lcg.language_name(lang))

    def cb(self):
        return CodebookSpec(display=lambda lang: self._language_name(lang), prefer_display=True)
    sorting = ('lang', ASC),
    layout = ('lang',)
    columns = ('lang', 'name')


class Modules(Specification):
    title = _("Modules")
    help = _("Management of extension modules available for use in pages.")
    table = dbdefs.CmsModules
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))

    def _customize_fields(self, fields):
        fields.modify('mod_id', default=nextval('cms_modules_mod_id_seq'))
        fields.modify('modname', label=_("Title"), width=32)
        fields.append(Field('descr', label=_("Description"), width=64, virtual=True,
                            editable=NEVER, computer=computer(self._descr)))

    def _module(self, modname):
        if modname:
            for python_module_name in self._SEARCH_MODULES:
                python_module = __import__(python_module_name)
                for component in python_module_name.split('.')[1:]:
                    python_module = getattr(python_module, component)
                if hasattr(python_module, modname):
                    module = getattr(python_module, modname)
                    import wiking
                    if isinstance(module, type) and issubclass(module, wiking.Module):
                        return module
        return None

    def _descr(self, rrecord, modname):
        import wiking
        module = wiking.module(modname)
        if module:
            return module.descr() or module.title()
        else:
            return None
    sorting = ('modname', ASC),
    cb = CodebookSpec(display='modname', prefer_display=True)
    layout = ('modname', 'descr')
    columns = ('modname', 'descr')

    def bindings(self):
        return (Binding('actions', _("Available actions of this module"), self._name('Actions'),
                        'mod_id'),)

    def actions(self):
        return (Action('reload', _("Reload available actions"), self._reload_actions),)

    def on_delete_record(self, record):
        data = pytis.util.data_object(self._name('Menu'))
        count = data.select(condition=pd.EQ('mod_id', record['mod_id']))
        data.close()
        if count:
            return _("This module is used on existing pages.\n"
                     "If you want to delete it, remove all linked pages first.")
        else:
            return True
    # def on_new_record(self, prefill, transaction=None):
    #    import pytis.form
    #    record = app.new_record(self, prefill=prefill,
    #                            block_on_new_record=True, transaction=transaction)
    #    if record:
    #
    #    return record

    def _reload_actions(self, record):
        import wiking

        def action_descr(module, action):
            if issubclass(module, wiking.PytisModule):
                for a in module.Spec.actions:
                    if a.name() == action:
                        return a.descr() or a.title()
            try:
                return dict(self._DEFAULT_ACTIONS)[action]
            except KeyError:
                method = getattr(module, 'action_' + action)
                docstring = method.__doc__
                return docstring and docstring.splitlines()[0] or _("Not specified")
        module = wiking.module(record['modname'].value())
        if module:
            data = pytis.util.data_object(self._name('Actions'))
            existing_actions = {
                row['name'].value(): row['action_id']
                for row in data.rows(condition=pd.EQ('mod_id', record['mod_id']))
            }
            actions = [attr[7:] for attr in dir(module)
                       if attr.startswith('action_') and callable(getattr(module, attr))]
            default_actions = [a[0] for a in self._DEFAULT_ACTIONS]
            # Order default actions first and in the order of self._DEFAULT_ACTIONS.
            actions.sort(key=lambda a: str(default_actions.index(a)) if a in default_actions else a)
            descriptions = [action_descr(module, action) for action in actions]
            import pytis.form
            result = pytis.form.app.run_dialog(
                pytis.form.CheckListDialog, title=_("Found actions"),
                message=_("Select the actions you want to make available to web users:"),
                items=[(a in existing_actions, a, d)
                       for a, d in zip(actions, descriptions)],
                columns=(_("Action"), _("Description"))
            )
            if result is not None:
                # TODO: Use a transaction.  Respect existing actions.
                for i, action in enumerate(actions):
                    if result[i]:
                        description_value = pd.Value(pd.String(), descriptions[i] or None)
                        try:
                            key = existing_actions[action]
                        except KeyError:
                            rowdata = [('mod_id', record['mod_id']),
                                       ('name', pd.Value(pd.String(), action)),
                                       ('description', description_value)]
                            data.insert(pd.Row(rowdata))
                        else:
                            data.update((key,), pd.Row((('description', description_value),)))

    _DEFAULT_ACTIONS = (
        ('view', _("View record")),
        ('list', _("List all records")),
        ('export', _("Export table to CSV")),
        ('rss', _("Show RSS feeds")),
        ('insert', _("Insert new record")),
        ('update', _("Edit existing record")),
        ('delete', _("Delete record")),
        ('print_field', _("Export printable fields to PDF")),
    )
    _SEARCH_MODULES = ()
    """Defines list of names python modules which should be searched for available Wiking modules.

    Should be defined in the derived class to make module descriptions work.

    """


class MenuParents(Specification):
    # Codebook of parent items for Menu (to prevent recursion).
    title = _("Menu")
    table = dbdefs.CmsMenu

    def _customize_fields(self, fields):
        fields.modify('title_or_identifier', label=_("Title"), width=32, type=_TreeOrder)
        fields.modify('identifier', label=_("Identifier"), width=20)
        fields.modify('modname', label=_("Module"))
        # fields.modify('description', _("Description"), width=64)
    sorting = ('tree_order', ASC),
    columns = ('title_or_identifier', 'identifier', 'modname')
    cb = CodebookSpec(display='title_or_identifier', prefer_display=True)


class Menu(Specification):
    title = _("Menu")
    help = _("Management of main menu items, their hierarchy and content.")
    table = dbdefs.CmsMenu

    def _parent_filter(self, record, lang):
        return pd.EQ('lang', pd.Value(pd.String(), lang))

    def _customize_fields(self, fields):
        fields.modify('identifier', label=_("Identifier"), width=20, fixed=True, editable=ONCE,
                      type=pd.RegexString(maxlen=32, not_null=True,
                                          regex='^[a-zA-Z][0-9a-zA-Z_-]*$'),
                      descr=_("The identifier will be used as the page URL path.  "
                              "It may be used "
                              "in links to this page within the text of other pages. A valid "
                              "identifier may contain only ASCII letters, "
                              "digits, hyphens and underscores, and must start with a letter."))
        fields.modify('lang', label=_("Language"), editable=ONCE, codebook=self._name('Languages'),
                      value_column='lang', not_null=True, selection_type=pp.SelectionType.CHOICE)
        fields.modify('title_or_identifier', label=_("Title"), width=30, type=_TreeOrder())
        fields.modify('title', label=_("Title"), width=20,
                      descr=_("Menu item title - short and descriptive."))
        fields.modify('heading', label=_("Heading"), width=32,
                      descr=_("Main heading displayed on the page.  If left empty, "
                              "the menu item title will be used.  "
                              "That title is sometimes too short because it is used in menus, "
                              "so you can provide a longer variant here."))
        fields.modify('description', label=_("Description"), width=72,
                      descr=_("Short page description (shown in menu as a tooltip)."))
        fields.modify('content', label=_("Content"), compact=True, height=20, width=80,
                      text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage,
                      descr=_("Page text formatted as LCG structured text (wiki)"))
        fields.modify('mod_id', label=_("Module"), type=pd.Integer(), not_null=False,
                      codebook=self._name('Modules', False), allow_codebook_insert=True,
                      descr=_("Select an extension module displayed inside the page.  "
                              "Leave empty for a plain text page."))
        fields.modify('modname', label=_("Module"))
        fields.modify('parent', label=_("Parent item"), type=pd.Integer(), not_null=False,
                      codebook=self._name('MenuParents', False), value_column='menu_item_id',
                      runtime_filter=computer(self._parent_filter),
                      descr=_("Select the immediate parent item in the menu hierarchy.  "
                              "Leave empty for pages at the top level of the menu."))
        fields.modify('published', label=_("Published"), width=6, default=True, fixed=True,
                      descr=_("Controls availability of the item independently for each "
                              "language version."))
        fields.modify('ord', label=_("Ordering"), width=8, editable=ALWAYS, fixed=True,
                      descr=_("Enter a number denoting the order of the item in menu between "
                              "pages of the same hierarchy level.  Leave empty to put the item "
                              "automatically to bottom."))

    def _attachment_storage(self, record, base_uri=None):
        # Determines the directory for storing the attachments for the 'content' field.
        storage = os.environ.get('PYTIS_CMS_ATTACHMENTS_STORAGE')
        if storage and record['identifier'].value():
            if storage.startswith('http://') or storage.startswith('https://'):
                uri = storage + '/' + record['identifier'].value()
                spec_name = self._action_spec_name()
                readonly = not (app.has_access(spec_name, pytis.data.Permission.UPDATE) or
                                app.has_access(spec_name, pytis.data.Permission.INSERT))
                return pp.HttpAttachmentStorage(uri, readonly=readonly)
            elif storage.startswith('db:'):
                table, column_id = storage.split(':')[1:]
                return pp.DbAttachmentStorage(table, column_id, record[column_id].value())
            else:
                directory = os.path.join(storage, record['identifier'].value())
                return pp.FileAttachmentStorage(directory, base_uri=base_uri)
        else:
            return None

    def _check_menu_order_condition(self, record):
        # Return a list of 'pd.Operator' instances to find menu items with duplicate order at the
        # same level.  The returned operators will be applied in conjunction.  Designed to allow
        # overriding in a derived class with more complicated menu structure.
        return [pd.EQ('parent', record['parent']),
                pd.EQ('ord', record['ord']),
                pd.NE('menu_item_id', record['menu_item_id'])]

    def check(self, record):
        data = record.data()
        if record['parent'].value() is not None:
            if record['parent'].value() == record['menu_item_id'].value():
                return ('parent', _("An item cannot be its own parent."))
            if not record.new():
                # A new record can't be a parent of existing records.
                condition = pd.AND(pd.EQ('menu_item_id', record['parent']),
                                   pd.LTreeDescendant('tree_order', record['tree_order']))
                count = data.select(condition=condition)
                data.close()
                if count:
                    return ('parent', _("Cannot assign a child item as a parent "
                                        "(cycle in the hierarchy)."))
        count = data.select(condition=pd.AND(*self._check_menu_order_condition(record)))
        data.close()
        if count:
            return ('ord', _("Another item at this menu level already has the same order number."))

    def on_delete_record(self, record):
        data = record.data()
        count = data.select(condition=pd.EQ('parent', record['menu_item_id']))
        data.close()
        if count:
            return _("This item has child items.\n"
                     "If you want to delete it, delete all child items first.")
        else:
            return True

    def row_style(self, record):
        if not record['published'].value():
            return pp.Style(foreground='#777', background='#eee')
        else:
            return None
    sorting = ('tree_order', ASC),
    layout = (VGroup('identifier', 'parent', 'mod_id', 'ord',
                     label=_("Global properties (shared by all languages)")),
              VGroup('lang', 'title', 'heading', 'description', 'content', 'published',
                     label=_("Page text (for the current language version)")))
    columns = ('title_or_identifier', 'identifier', 'modname', 'ord', 'published')

    def bindings(self):
        return (
            Binding('rights', _("Access rights"), self._name('Rights'),
                    condition=lambda r: pd.EQ('menu_item_id', r['menu_item_id']),
                    prefill=lambda r: {'menu_item_id': r['menu_item_id'].value(),
                                       'mod_id': r['mod_id'].value()}),
            Binding('content', _("Content"), content=self._page_content),
        )

    def _page_content(self, row):
        text = row['content'].value()
        if text:
            storage = self._attachment_storage(row)
            if storage:
                try:
                    resources = storage.resources()
                except pp.AttachmentStorage.StorageError as e:
                    app.echo(_("Error accessing attachment storage: %s", e), kind='error')
                    resources = ()
            else:
                resources = ()
            return pytis.util.parse_lcg_text(text, resources=resources)
        else:
            return ''


class Users(Specification):
    title = _("Users")
    help = _("Management of user accounts, which can then be assigned to roles.")
    table = dbdefs.CmsUsers

    def _customize_fields(self, fields):
        fields.modify('uid', label=_("UID"), width=5, default=nextval('cms_users_uid_seq'))
        fields.modify('login', label=_("Username"), width=16)
        fields.modify('fullname', label=_("Full name"), width=40)
        fields.modify('passwd', label=_("Password"),
                      type=pd.Password(not_null=True, minlen=4, md5=True))

    layout = ('login', 'fullname', 'passwd')
    columns = ('uid', 'login', 'fullname')
    cb = CodebookSpec(display='fullname')
    sorting = (('login', ASC),)

    def bindings(self):
        return (Binding('roles', _("User roles"), self._name('UserRoles'), 'uid'),
                Binding('sessions', _("Login history"), self._name('UserSessionLog'),
                        'uid'),
                )


class Roles(Specification):
    title = _("User roles")
    help = _("Management of available user roles, which can be assigned to users.")
    table = dbdefs.CmsRoles

    def _customize_fields(self, fields):
        fields.modify('role_id', default=nextval('cms_roles_role_id_seq'))
        fields.modify('name', label=_("Title"), width=16)
        fields.modify('system_role', label=_("System role"), width=16, type=pd.String(not_null=True))
        fields.modify('description', label=_("Description"), width=64)

    layout = ('name', 'description')
    columns = ('name', 'description')
    condition = pd.EQ('system_role', pd.Value(pd.String(), None))

    def bindings(self):
        return (
            Binding('users', _("Users assigned to this role"), self._name('RoleUsers'),
                    'role_id'),
        )
    cb = CodebookSpec(display='name')


class SystemRoles(Roles):
    title = _("System roles")
    help = _("System roles are assigned to users automatically.")
    layout = columns = ('name', 'system_role', 'description')
    condition = pd.NE('system_role', pd.Value(pd.String(), None))
    bindings = ()
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))


class AllRoles(Roles):
    title = _("Roles")
    help = _("Roles available for assigning action permissions "
             "(includes both system and user-defined roles).")
    condition = None


class UserRoles(Specification):
    title = _("User role assignments")
    help = _("Management of user role assignments for individual users.")
    table = dbdefs.CmsUserRoles

    def _customize_fields(self, fields):
        fields.modify('user_role_id', default=nextval('cms_user_role_assignment_user_role_id_seq'))
        fields.modify('role_id', label=_("Role"), not_null=True, codebook=self._name('Roles', False))
        fields.modify('system_role')
        fields.modify('uid', label=_('UID'), not_null=True, codebook=self._name('Users', False),
                      width=5)
        fields.modify('login', label=_("Username"), width=16)
        fields.modify('fullname', label=_("Full name"), width=50)
        fields.modify('name', label=_("Role name"), width=16)
        fields.modify('description', label=_("Description"), width=50)

    layout = ('role_id',)
    columns = ('name', 'description')


class RoleUsers(UserRoles):
    help = _("Management of user assignments for individual user roles.")
    layout = ('uid',)
    columns = ('uid', 'login', 'fullname')


class Actions(Specification):
    title = _("Available actions")
    help = _("List of supported actions for individual modules.")
    table = dbdefs.CmsActions
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))

    def _customize_fields(self, fields):
        fields.modify('action_id', default=nextval('cms_actions_action_id_seq'))
        fields.modify('mod_id', label=_("Module"), not_null=True, codebook=self._name('Modules', False))
        fields.modify('name', label=_("Title"), width=16)
        fields.modify('description', label=_("Description"), width=64)

    sorting = (('action_id', ASC),)
    layout = ('name', 'description')
    columns = ('name', 'description')
    cb = CodebookSpec(display='name')


class GenericActions(Actions):
    title = _("Actions common to all menu items")
    help = _("List of supported actions common to all menu items.")
    condition = pd.EQ('mod_id', pd.Value(pd.Integer(), None))
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))


class Rights(Specification):
    title = _("Access rights")
    help = _("Assignment of module action permissions for individual user roles.")
    table = dbdefs.CmsRights

    def _customize_fields(self, fields):
        fields.modify('rights_assignment_id',
                      default=nextval('cms_rights_assignment_rights_assignment_id_seq'))
        fields.modify('menu_item_id')
        fields.modify('role_id', label=_("Role"), not_null=True, codebook=self._name('AllRoles', False))
        fields.modify('role_name', label=_("Role"))
        fields.modify('system_role')
        fields.modify('mod_id')
        fields.modify('action_id', label=_("Action"), codebook=self._name('Actions', False),
                      not_null=True, runtime_filter=computer(self._action_filter))
        fields.modify('action_name', label=_("Action Name"))
        fields.modify('action_description', label=_("Description"), width=30, editable=NEVER)

    grouping = 'role_id'
    sorting = (('role_name', ASC), ('mod_id', DESC), ('action_id', ASC))
    columns = ('role_name', 'action_name', 'action_description')
    layout = ('role_id', 'action_id')

    def _action_filter(self, record, mod_id):
        return pd.OR(pd.EQ('mod_id', record['mod_id']),
                     pd.EQ('mod_id', pd.Value(pd.Integer(), None)))

    def row_style(self, record):
        if not record['mod_id'].value():
            return None
        else:
            return pp.Style(background='#ffd')


class _Log(Specification):
    public = False

    def _customize_fields(self, fields):
        fields.modify('ip_address', label=_("IP address"), width=12, editable=NEVER)
        fields.modify('user_agent', label=_("User agent"), column_width=25, width=80)
        fields.modify('referer', label=_("Referrer"), column_width=25, width=80)
        fields.append(Field('hostname', label=_("Hostname"), virtual=True, editable=NEVER,
                            computer=computer(self._hostname), column_width=15, width=40))

    def _hostname(self, row, ip_address):
        try:
            hostname = socket.gethostbyaddr(ip_address)[0]
        except Exception:
            hostname = _("Unknown")
        return hostname


class SessionLog(_Log):
    title = _("Login log")
    help = _("Log of information about user logins to the web.")
    table = dbdefs.CmsSessionLog
    public = True

    def _customize_fields(self, fields):
        super(SessionLog, self)._customize_fields(fields)
        fields.modify('log_id')
        fields.modify('session_id')
        fields.modify('uid', not_null=True, codebook=self._name('Users'))
        fields.modify('login', label=_("Login"), width=8)
        fields.modify('fullname', label=_("Name"), width=15)
        fields.modify('success', label=_("Success"), width=3)
        fields.modify('start_time', label=_("Start"), width=17)
        fields.modify('duration', label=_("Duration"), width=17)
        fields.modify('active', label=_("Active"))

    def row_style(self, row):
        if row['success'].value():
            return None
        else:
            return pp.Style(foreground='#f00')
    layout = ('start_time', 'duration', 'active', 'success', 'fullname', 'login',
              'ip_address', 'hostname', 'user_agent', 'referer')
    columns = ('start_time', 'duration', 'active', 'success', 'fullname', 'login',
               'ip_address', 'user_agent')
    sorting = (('start_time', DESC),)


class UserSessionLog(SessionLog):
    """Login log customization for user sideform."""
    public = True
    layout = ('start_time', 'duration', 'active', 'success', 'ip_address', 'hostname',
              'user_agent', 'referer')
    columns = ('start_time', 'duration', 'active', 'success', 'ip_address', 'user_agent')


class AccessLog(_Log):
    title = _("Access log")
    help = _("Log of information about user access to individual web pages/modules.")
    table = dbdefs.CmsAccessLogData
    public = True

    def _customize_fields(self, fields):
        super(AccessLog, self)._customize_fields(fields)
        fields.modify('log_id')
        fields.modify('timestamp', label=_("Date and time"), width=17)
        fields.modify('uri', label=_("Path"), width=17)
        fields.modify('uid', label=_("User"), not_null=True, codebook=self._name('Users'))
        fields.modify('modname', label=_("Module"), width=17)
        fields.modify('action', label=_("Action"), width=17)

    layout = ('timestamp', 'uri', 'uid', 'modname', 'action',
              'ip_address', 'hostname', 'user_agent', 'referer')
    columns = ('timestamp', 'uri', 'uid', 'modname', 'action',
              'ip_address', 'user_agent', 'referer')
    sorting = (('timestamp', DESC),)


class Themes(Specification):

    title = _("Color themes")
    help = _("Management of color themes.")
    table = dbdefs.CmsThemes

    def _customize_fields(self, fields):
        fields.modify('theme_id')
        fields.modify('name', label=_("Title"), nocopy=True)

        def field(id, label, descr=None):
            fields.modify(id, label=label, descr=descr, type=pd.Color())

        field('foreground', _("Text"),
              descr=_("Foreground color of the main content areas.")),
        field('background', _("Background"),
              descr=_("Background color of the main content areas (page body text, panels)."))
        field('highlight_bg', _("Highlight background"),
              descr=_("Background-color highlighting can be used in various places, "
                      "e.g. to distinguish the current menu item, the current language, "
                      "a record found in a form, etc."))
        field('link', _("Link"),
              descr=_("Text color of an unvisited link."))
        field('link_visited', _("Visited link"),
              descr=_("Text color of a visited link. If left empty, the same color as for an "
                      "unvisited link will be used."))
        field('link_hover', _("Hover link"),
              descr=_("Link color on mouse hover. If left empty, the link will not react to "
                      "mouse hover."))
        field('border', _("Border"))
        field('heading_fg', _("Text"),
              descr=_("Heading colors are used to highlight titles of chapters, panels, and "
                      "other elements that act as headings. Depending on the styles used, "
                      "some heading types may use a different background color, while others "
                      "may be underlined only (see below). The text color is shared for both "
                      "heading types."))
        field('heading_bg', _("Background"),
              descr=_("Background color for headings that use background-color highlighting. "
                      "Typically these are headers of page sections (menu, panels, etc.) and "
                      "higher-level headings."))
        field('heading_line', _("Underline"),
              descr=_("Underline color for headings that use underlining for emphasis. "
                      "Typically these are lower-level headings."))
        field('frame_fg', _("Text"),
              descr=_("Frames are generally used to distinguish standalone page elements such as "
                      "forms, toolbars, indexes, etc. Frames in this sense have nothing to do "
                      "with HTML frames. If you leave the text color empty, the default text "
                      "color will be used (often you will want to set only the background and "
                      "border for frames)."))
        field('frame_bg', _("Background"),
              descr=_("Background color of frames to visually distinguish them from the rest of "
                      "the page content."))
        field('frame_border', _("Border"),
              descr=_("Border color of the frame area at the transition between the frame "
                      "background and its underlying area."))
        field('top_fg', _("Text"),
              descr=_("Text usually does not appear directly on background areas, but some "
                      "styles may require it."))
        field('top_bg', _("Background"),
              descr=_("Background areas form the surroundings of the main page content and "
                      "panels."))
        field('top_border', _("Border"),
              descr=_("Specifies the border color at the transition between the background area "
                      "and the content parts of the page."))
        field('error_fg', _("Text"))
        field('error_bg', _("Background"))
        field('error_border', _("Border"))
        field('message_fg', _("Text"))
        field('message_bg', _("Background"))
        field('message_border', _("Border"))
        field('meta_fg', _("Text"))
        field('meta_bg', _("Background"),
              descr=_("Highlighting of the informational line in detail views, e.g. date and "
                      "author information in an article list."))
        field('table_cell', _("Default cell background"))
        field('table_cell2', _("Shaded cell background"))
        field('help', _("Form help"))
        field('inactive_folder', _("Inactive tab"))

    layout = pp.HGroup(pp.VGroup(
        'name',
        pp.LVGroup(_("Standard page colors"),
                   ('foreground', 'background', 'highlight_bg', 'link', 'link_visited',
                    'link_hover', 'border')),
        pp.LVGroup(_("Tables"),
                   ('table_cell', 'table_cell2')),
        pp.LVGroup(_("Miscellaneous"),
                   ('help', 'inactive_folder')),
    ), pp.VGroup(
        pp.LVGroup(_("Heading colors"),
                   ('heading_fg', 'heading_bg', 'heading_line')),
        pp.LVGroup(_("Frames"),
                   ('frame_fg', 'frame_bg', 'frame_border')),
        pp.LVGroup(_("Record metadata"),
                   ('meta_fg', 'meta_bg')),
        pp.LVGroup(_("Background area colors"),
                   ('top_bg', 'top_border')),
    ), pp.VGroup(
        pp.LVGroup(_("Error messages"),
                   ('error_fg', 'error_bg', 'error_border')),
        pp.LVGroup(_("Informational messages"),
                   ('message_fg', 'message_bg', 'message_border')),
    ))
    columns = ('name',)
    cb = CodebookSpec(display='name', prefer_display=True)
