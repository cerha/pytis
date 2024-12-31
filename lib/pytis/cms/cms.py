# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
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
    title = _("Jazyky")
    help = _("Správa jazyků dostupných v CMS.")
    table = dbdefs.CmsLanguages

    def _customize_fields(self, fields):
        fields.modify('lang_id', default=nextval('cms_languages_lang_id_seq'))
        fields.modify('lang', label=_("Kód"), width=2, column_width=6, fixed=True,
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
    title = _("Moduly")
    help = _("Správa rozšiřujících modulů použitelných ve stránkách.")
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
        return (Binding('actions', _("Dostupné akce tohoto modulu"), self._name('Actions'),
                        'mod_id'),)

    def actions(self):
        return (Action('reload', _("Přenačíst dostupné akce"), self._reload_actions),)

    def on_delete_record(self, record):
        data = pytis.util.data_object(self._name('Menu'))
        count = data.select(condition=pd.EQ('mod_id', record['mod_id']))
        data.close()
        if count:
            return _("Modul je používán v existujících stránkách.\n"
                     "Pokud jej chcete vymazat, zrušte nejprve všechny navázané stránky.")
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
                return docstring and docstring.splitlines()[0] or _("Neuvedeno")
        module = wiking.module(record['modname'].value())
        if module:
            data = pytis.util.data_object(self._name('Actions'))
            data.select(condition=pd.EQ('mod_id', record['mod_id']))
            existing_actions = {}
            while True:
                row = data.fetchone()
                if row is None:
                    break
                else:
                    existing_actions[row['name'].value()] = row['action_id']
            data.close()
            actions = [attr[7:] for attr in dir(module)
                       if attr.startswith('action_') and callable(getattr(module, attr))]
            default_actions = [a[0] for a in self._DEFAULT_ACTIONS]
            # Order default actions first and in the order of self._DEFAULT_ACTIONS.
            actions.sort(key=lambda a: str(default_actions.index(a)) if a in default_actions else a)
            descriptions = [action_descr(module, action) for action in actions]
            import pytis.form
            result = pytis.form.app.run_dialog(
                pytis.form.CheckListDialog, title=_("Nalezené akce"),
                message=_("Zaškrtněte akce, které chcete zpřístupnit webovým "
                          "uživatelům:"),
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
        ('view', _("Zobrazení záznam")),
        ('list', _("Výpis všech záznamů")),
        ('export', _("Export tabulky do CSV formát")),
        ('rss', _("Zobrazení RSS kanálů")),
        ('insert', _("Vložení nového záznam")),
        ('update', _("Editace stávajícího záznam")),
        ('delete', _("Smazání záznam")),
        ('print_field', _("Export tisknutelných políček do PDF")),
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
        fields.modify('modname', label=_("Modul"))
        # fields.modify('description', _("Description"), width=64)
    sorting = ('tree_order', ASC),
    columns = ('title_or_identifier', 'identifier', 'modname')
    cb = CodebookSpec(display='title_or_identifier', prefer_display=True)


class Menu(Specification):
    title = _("Menu")
    help = _("Správa položek hlavního menu, jejich hierarchie a obsahu.")
    table = dbdefs.CmsMenu

    def _parent_filter(self, record, lang):
        return pd.EQ('lang', pd.Value(pd.String(), lang))

    def _customize_fields(self, fields):
        fields.modify('identifier', label=_("Identifier"), width=20, fixed=True, editable=ONCE,
                      type=pd.RegexString(maxlen=32, not_null=True,
                                          regex='^[a-zA-Z][0-9a-zA-Z_-]*$'),
                      descr=_("Identifikátor bude vystupovat jako vnější adresa stránky.  "
                              "Může být použit "
                              "v odkazech na tuto stránku v rámci textu jiných stránek. Platný "
                              "identifikátor může obsahovat pouze písmena bez diakritiky, "
                              "číslice, pomlčky a podtržítka a musí začínat písmenem."))
        fields.modify('lang', label=_("Jazyk"), editable=ONCE, codebook=self._name('Languages'),
                      value_column='lang', not_null=True, selection_type=pp.SelectionType.CHOICE)
        fields.modify('title_or_identifier', label=_("Title"), width=30, type=_TreeOrder())
        fields.modify('title', label=_("Title"), width=20,
                      descr=_("Název položky menu - krátký a výstižný."))
        fields.modify('heading', label=_("Nadpis"), width=32,
                      descr=_("Hlavní nadpis při zobrazení stránky.  Pokud ponecháte nevyplněné, "
                              "použije se název položky.  Ten je ale někdy kvůli použití v menu "
                              "příliš krátký, takže zde je možné určit jeho delší variantu."))
        fields.modify('description', label=_("Description"), width=72,
                      descr=_("Stručný popis stránky (zobrazen v menu jako tooltip)."))
        fields.modify('content', label=_("Content"), compact=True, height=20, width=80,
                      text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage,
                      descr=_("Text stránky formátovaný jako LCG strukturovaný text (wiki)"))
        fields.modify('mod_id', label=_("Modul"), type=pd.Integer(), not_null=False,
                      codebook=self._name('Modules', False), allow_codebook_insert=True,
                      descr=_("Vyberte rozšiřující modul zobrazený uvnitř stránky.  "
                              "Ponechte prázdné pro prostou textovou stránku."))
        fields.modify('modname', label=_("Modul"))
        fields.modify('parent', label=_("Parent item"), type=pd.Integer(), not_null=False,
                      codebook=self._name('MenuParents', False), value_column='menu_item_id',
                      runtime_filter=computer(self._parent_filter),
                      descr=_("Vyberte bezprostředně nadřízenou položku v hierarchii menu.  "
                              "Ponechte prázdné pro stránky na nejvyšší úrovni menu."))
        fields.modify('published', label=_("Zveřejněno"), width=6, default=True, fixed=True,
                      descr=_("Umožňuje řídit dostupnost dané položky nezávisle pro kažou "
                              "jazykovou verzi."))
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
                return ('parent', _("Položka nemůže být nadřízená sama sobě."))
            if not record.new():
                # A new record can't be a parent of existing records.
                condition = pd.AND(pd.EQ('menu_item_id', record['parent']),
                                   pd.LTreeDescendant('tree_order', record['tree_order']))
                count = data.select(condition=condition)
                data.close()
                if count:
                    return ('parent', _("Nelze přiřadit podřízenou položku jako nadřízenou "
                                        "(cyklus v hierarchii)."))
        count = data.select(condition=pd.AND(*self._check_menu_order_condition(record)))
        data.close()
        if count:
            return ('ord', _("Stejné pořadové číslo už má jiná položka na této úrovni menu."))

    def on_delete_record(self, record):
        data = record.data()
        count = data.select(condition=pd.EQ('parent', record['menu_item_id']))
        data.close()
        if count:
            return _("Položka má podřízené položky.\n"
                     "Pokud ji chcete vymazat, vymažte nejprve všechny podpoložky.")
        else:
            return True

    def row_style(self, record):
        if not record['published'].value():
            return pp.Style(foreground='#777', background='#eee')
        else:
            return None
    sorting = ('tree_order', ASC),
    layout = (VGroup('identifier', 'parent', 'mod_id', 'ord',
                     label=_("Globální vlastnosti (společné pro všechny jazyky)")),
              VGroup('lang', 'title', 'heading', 'description', 'content', 'published',
                     label=_("Text stránky (pro aktuální jazykovou verzi)")))
    columns = ('title_or_identifier', 'identifier', 'modname', 'ord', 'published')

    def bindings(self):
        return (
            Binding('rights', _("Přístupová práva"), self._name('Rights'),
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
                    app.echo(_("Chyba přísutu k úložišti příloh: %s", e), kind='error')
                    resources = ()
            else:
                resources = ()
            return pytis.util.parse_lcg_text(text, resources=resources)
        else:
            return ''


class Users(Specification):
    title = _("Uživatelé")
    help = _("Správa uživatelských účtů, které je poté možno zařazovat do rolí.")
    table = dbdefs.CmsUsers

    def _customize_fields(self, fields):
        fields.modify('uid', label=_("UID"), width=5, default=nextval('cms_users_uid_seq'))
        fields.modify('login', label=_("Přihlašovací jméno"), width=16)
        fields.modify('fullname', label=_("Celé jméno"), width=40)
        fields.modify('passwd', label=_("Heslo"),
                      type=pd.Password(not_null=True, minlen=4, md5=True))

    layout = ('login', 'fullname', 'passwd')
    columns = ('uid', 'login', 'fullname')
    cb = CodebookSpec(display='fullname')
    sorting = (('login', ASC),)

    def bindings(self):
        return (Binding('roles', _("Uživatelské role"), self._name('UserRoles'), 'uid'),
                Binding('sessions', _("Historie přihlášení"), self._name('UserSessionLog'),
                        'uid'),
                )


class Roles(Specification):
    title = _("Uživatelské role")
    help = _("Správa dostupných uživatelských rolí, které je možné přiřazovat uživatelům.")
    table = dbdefs.CmsRoles

    def _customize_fields(self, fields):
        fields.modify('role_id', default=nextval('cms_roles_role_id_seq'))
        fields.modify('name', label=_("Title"), width=16)
        fields.modify('system_role', label=_("Systémová role"), width=16, type=pd.String(not_null=True))
        fields.modify('description', label=_("Description"), width=64)

    layout = ('name', 'description')
    columns = ('name', 'description')
    condition = pd.EQ('system_role', pd.Value(pd.String(), None))

    def bindings(self):
        return (
            Binding('users', _("Uživatelé zařazení do této role"), self._name('RoleUsers'),
                    'role_id'),
        )
    cb = CodebookSpec(display='name')


class SystemRoles(Roles):
    title = _("Systémové role")
    help = _("Systémové role jsou uživatelům přiřazeny automaticky.")
    layout = columns = ('name', 'system_role', 'description')
    condition = pd.NE('system_role', pd.Value(pd.String(), None))
    bindings = ()
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))


class AllRoles(Roles):
    title = _("Role")
    help = _("Role dostupné pro přiřazování práv akcím (obsahuje systémové i uživatelsky "
             "definované role).")
    condition = None


class UserRoles(Specification):
    title = _("Přiřazení uživatelských rolí")
    help = _("Správa přiřazení uživatelských rolí jednotlivým uživatelům.")
    table = dbdefs.CmsUserRoles

    def _customize_fields(self, fields):
        fields.modify('user_role_id', default=nextval('cms_user_role_assignment_user_role_id_seq'))
        fields.modify('role_id', label=_("Role"), not_null=True, codebook=self._name('Roles', False))
        fields.modify('system_role')
        fields.modify('uid', label=_('UID'), not_null=True, codebook=self._name('Users', False),
                      width=5)
        fields.modify('login', label=_("Přihlašovací jméno"), width=16)
        fields.modify('fullname', label=_("Celé jméno"), width=50)
        fields.modify('name', label=_("Název role"), width=16)
        fields.modify('description', label=_("Description"), width=50)

    layout = ('role_id',)
    columns = ('name', 'description')


class RoleUsers(UserRoles):
    help = _("Správa přiřazení uživatelů jednotlivým uživatelským rolím.")
    layout = ('uid',)
    columns = ('uid', 'login', 'fullname')


class Actions(Specification):
    title = _("Dostupné akce")
    help = _("Výčet podporovaných akcí pro jednotlivé moduly.")
    table = dbdefs.CmsActions
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))

    def _customize_fields(self, fields):
        fields.modify('action_id', default=nextval('cms_actions_action_id_seq'))
        fields.modify('mod_id', label=_("Modul"), not_null=True, codebook=self._name('Modules', False))
        fields.modify('name', label=_("Title"), width=16)
        fields.modify('description', label=_("Description"), width=64)

    sorting = (('action_id', ASC),)
    layout = ('name', 'description')
    columns = ('name', 'description')
    cb = CodebookSpec(display='name')


class GenericActions(Actions):
    title = _("Akce společné pro všechny položky men")
    help = _("Výčet podporovaných akcí společných pro všechny položky menu.")
    condition = pd.EQ('mod_id', pd.Value(pd.Integer(), None))
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))


class Rights(Specification):
    title = _("Přístupová práva")
    help = _("Přiřazení práv k akcím modulů pro jednotlivé uživatelské role.")
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
        fields.modify('ip_address', label=_("IP adresa"), width=12, editable=NEVER)
        fields.modify('user_agent', label=_("User agent"), column_width=25, width=80)
        fields.modify('referer', label=_("Referer"), column_width=25, width=80)
        fields.append(Field('hostname', label=_("Hostname"), virtual=True, editable=NEVER,
                            computer=computer(self._hostname), column_width=15, width=40))

    def _hostname(self, row, ip_address):
        try:
            hostname = socket.gethostbyaddr(ip_address)[0]
        except Exception:
            hostname = _("Neznámý")
        return hostname


class SessionLog(_Log):
    title = _("Log přihlášení")
    help = _("Záznam informací o přihlášení uživatelů k webu.")
    table = dbdefs.CmsSessionLog
    public = True

    def _customize_fields(self, fields):
        super(SessionLog, self)._customize_fields(fields)
        fields.modify('log_id')
        fields.modify('session_id')
        fields.modify('uid', not_null=True, codebook=self._name('Users'))
        fields.modify('login', label=_("Login"), width=8)
        fields.modify('fullname', label=_("Jméno"), width=15)
        fields.modify('success', label=_("Úspěch"), width=3)
        fields.modify('start_time', label=_("Začátek"), width=17)
        fields.modify('duration', label=_("Trvání"), width=17)
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
    title = _("Log přístupů")
    help = _("Záznam informací o přístupu uživatelů k jednotlivým stránkám/modulům webu.")
    table = dbdefs.CmsAccessLogData
    public = True

    def _customize_fields(self, fields):
        super(AccessLog, self)._customize_fields(fields)
        fields.modify('log_id')
        fields.modify('timestamp', label=_("Datum a čas"), width=17)
        fields.modify('uri', label=_("Cesta"), width=17)
        fields.modify('uid', label=_("User"), not_null=True, codebook=self._name('Users'))
        fields.modify('modname', label=_("Modul"), width=17)
        fields.modify('action', label=_("Action"), width=17)

    layout = ('timestamp', 'uri', 'uid', 'modname', 'action',
              'ip_address', 'hostname', 'user_agent', 'referer')
    columns = ('timestamp', 'uri', 'uid', 'modname', 'action',
              'ip_address', 'user_agent', 'referer')
    sorting = (('timestamp', DESC),)


class Themes(Specification):

    title = _("Barevné Motivy")
    help = _("Správa barevných motivů.")
    table = dbdefs.CmsThemes

    def _customize_fields(self, fields):
        fields.modify('theme_id')
        fields.modify('name', label=_("Title"), nocopy=True)

        def field(id, label, descr=None):
            fields.modify(id, label=label, descr=descr, type=pd.Color())

        field('foreground', _("Text"),
              descr=("Barva popředí hlavních obsahových ploch.")),
        field('background', _("Pozadí"),
              descr=("Barva pozadí hlavních obsahových ploch (vlastní text stránky, panely)."))
        field('highlight_bg', _("Zvýrazněné pozadí"),
              descr=_("Zvýraznění pomocí barvy pozadí může být použito na různých místech, "
                      "např. pro odlišení aktuální položky menu, aktuálního jazyka, nalezeného "
                      "záznamu ve formuláři apod."))
        field('link', _("Odkaz"),
              descr=_("Barva textu nenavštíveného odkazu."))
        field('link_visited', _("Navštívený odkaz"),
              descr=_("Barva textu navštíveného odkazu.  Pokud ponecháte prázdné, bude "
                      "použita stejná barva jako u nenavštíveného odkazu."))
        field('link_hover', _("Aktivní odkaz"),
              descr=_("Obarvení odkazu při při pohybu myší nad ním.  Pokud ponecháte "
                      "prázdné, nebude odkaz na pohyb myši reagovat."))
        field('border', _("Orámování"))
        field('heading_fg', _("Text"),
              descr=_("Barvy nadpisů jsou používány pro zvýrazenění názvů kapitol, panelů a "
                      "jiných elementů majících povahu nadpisu či záhlaví.  V závislosti na "
                      "použitých stylech mohou některé typy nadpisů používat odlišnou barvu "
                      "pozadí, jiné mohou být pouze podtrženy (viz dále).  Barva textu je "
                      "však společná pro oba typy nadpisů."))
        field('heading_bg', _("Pozadí"),
              descr=_("Barva pozadí pro nadpisy, které používají zvýraznění barvou pozadí."
                      "Typicky jsou to záhlaví částí stránky (menu, panely apod.) a nadpisy "
                      "vyšších úrovní."))
        field('heading_line', _("Podtržení"),
              descr=_("Barva podtržení pro nadpisy, které používají zvýraznění podtržením."
                      "Typicky jsou to nadpisy nižších úrovní."))
        field('frame_fg', _("Text"),
              descr=_("Rámy jsou obecně využívány pro odlišení samostatných prvků stránky, "
                      "jako jsou například formuláře, nástrojové lišty, rejstříky apod. "
                      "Rámy v tomto smyslu nemají nic splečného s HTML rámy (frame).  Pokud "
                      "barvu textu nevyplníte, bude použita standardní barva textu (často u "
                      "rámů budete chtít nastavit pouze barvu pozadí a orámování)."))
        field('frame_bg', _("Pozadí"),
              descr=_("Barva pozadí rámů pro jejich vizuální odlišení od ostatního obsahu "
                      "stránky."))
        field('frame_border', _("Orámování"),
              descr=_("Barva orámování okrajů plochy rámu na přechodu mezi barvou pozadí rámu "
                      "a jeho podkladem."))
        field('top_fg', _("Text"),
              descr=_("Na podkladových plochách se většinou text přímo nevyskytuje, ale "
                      "některé styly to mohou předepisovat."))
        field('top_bg', _("Pozadí"),
              descr=_("Podkladové plochy tvoří okolí hlavního obsahu stránky a panelů."))
        field('top_border', _("Orámování"),
              descr=_("Určuje barvu rámečku na přechodu mezi podkladovou plochou a obsahovými "
                      "částmi stránky"))
        field('error_fg', _("Text"))
        field('error_bg', _("Pozadí"))
        field('error_border', _("Orámování"))
        field('message_fg', _("Text"))
        field('message_bg', _("Pozadí"))
        field('message_border', _("Orámování"))
        field('meta_fg', _("Text"))
        field('meta_bg', _("Pozadí"),
              descr=_("Odlišení informačního řádku v detailních výpisech, jako např. "
                      "informace o datu a autorovi v seznamu článků"))
        field('table_cell', _("Standardní pozadí buňky"))
        field('table_cell2', _("Stínované pozadí buňky"))
        field('help', _("Nápověda formuláře"))
        field('inactive_folder', _("Neaktivní záložka"))

    layout = pp.HGroup(pp.VGroup(
        'name',
        pp.LVGroup(_("Standardní barvy stránky"),
                   ('foreground', 'background', 'highlight_bg', 'link', 'link_visited',
                    'link_hover', 'border')),
        pp.LVGroup(_("Tabulky"),
                   ('table_cell', 'table_cell2')),
        pp.LVGroup(_("Různé"),
                   ('help', 'inactive_folder')),
    ), pp.VGroup(
        pp.LVGroup(_("Barvy nadpisů"),
                   ('heading_fg', 'heading_bg', 'heading_line')),
        pp.LVGroup(_("Rámy"),
                   ('frame_fg', 'frame_bg', 'frame_border')),
        pp.LVGroup(_("Meta data záznam"),
                   ('meta_fg', 'meta_bg')),
        pp.LVGroup(_("Barvy podkladových ploch"),
                   ('top_bg', 'top_border')),
    ), pp.VGroup(
        pp.LVGroup(_("Chybové zprávy"),
                   ('error_fg', 'error_bg', 'error_border')),
        pp.LVGroup(_("Informační zprávy"),
                   ('message_fg', 'message_bg', 'message_border')),
    ))
    columns = ('name',)
    cb = CodebookSpec(display='name', prefer_display=True)
