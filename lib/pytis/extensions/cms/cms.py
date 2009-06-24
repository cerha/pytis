# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2009 Brailcom, o.p.s.
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

"""Pytis CMS specifications.

Pytis CMS is a fat client application for management of database structures, which define the
behavior of a web application.

Wiking modules which implement a Wiking application, which reads these database structures and
behaves according to them can be found in the 'web' submodule of this module.

"""

import socket
import lcg
import pytis.data as pd, pytis.presentation as pp
from pytis.presentation import Field, Fields, HGroup, VGroup, Binding, Action, CodebookSpec, \
     Computer, CbComputer, computer
from pytis.extensions import nextval, ONCE, NEVER, ALWAYS
ASC = pd.ASCENDENT
DESC = pd.DESCENDANT


class Specification(pp.Specification):
    access_rights = pd.AccessRights((None, (('cms_user', 'cms_admin'), pd.Permission.ALL)))
    def _spec_name(self, name, needed_in_wiking=True):
        # Hack to allow namespaced spec names in wx app and plain module names in Wiking (the
        # specification is inherited by the Wiking Module).
        return 'cms.'+ name

class Languages(Specification):
    title = _("Jazyky")
    help = _("Správa jazyků dostupných v CMS.")
    table = 'cms_languages'
    def fields(self): return (
        Field('lang_id', default=nextval('cms_languages_lang_id_seq')),
        Field('lang', _("Kód"), width=2, column_width=6, fixed=True,
              filter=pp.TextFilter.ALPHANUMERIC, post_process=pp.PostProcess.LOWER),
        Field('name', _("Název"), virtual=True,
              computer=computer(lambda record, lang: self._language_name(lang))),
        )
    def _language_name(self, lang):
        t = lcg.GettextTranslator('cs', path=(lcg.config.translation_dir,), fallback=True)
        return t.translate(lcg.language_name(lang))
    def cb(self):
        return CodebookSpec(display=lambda lang: self._language_name(lang), prefer_display=True)
    sorting = ('lang', ASC),
    layout = ('lang',)
    columns = ('lang', 'name')


class Modules(Specification):
    title = _("Moduly")
    help = _("Správa rozšiřujících modulů použitelných ve stránkách.")
    table = 'cms_modules'
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))
    def fields(self): return (
        Field('mod_id', default=nextval('cms_modules_mod_id_seq')),
        Field('modname', _("Název"), width=32),
        Field('descr', _("Popis"), width=64, virtual=True, computer=computer(self._descr)),
        )
    def _module(self, modname):
        if modname:
            for python_module_name in self._SEARCH_MODULES:
                python_module = __import__(python_module_name)
                if hasattr(python_module, modname):
                    module = getattr(python_module, modname)
                    import wiking
                    if type(module) == type(wiking.Module) and issubclass(module, wiking.Module):
                        return module
        return None
    def _descr(self, rrecord, modname):
        module = self._module(modname)
        if module:
            return module.descr() or module.title()
        else:
            return None
    sorting = ('modname', ASC),
    cb = CodebookSpec(display='modname', prefer_display=True)
    layout = ('modname', 'descr')
    columns = ('modname', 'descr')
    def bindings(self):
        return (Binding(_("Dostupné akce tohoto modulu"), self._spec_name('Actions'), 'mod_id'),)
    def actions(self):
        return (Action(_("Přenačíst dostupné akce"), self._reload_actions),)
    def on_delete_record(self, record):
        import pytis.form
        data = pytis.form.create_data_object(self._spec_name('Menu'))
        count = data.select(condition=pd.EQ('mod_id', record['mod_id']))
        data.close()
        if count:
            return _("Modul je používán v existujících stránkách.\n"
                     "Pokud jej chcete vymazat, zrušte nejprve všechny navázané stránky.")
        else:
            return True
    #def on_new_record(self, prefill, transaction=None):
    #    import pytis.form
    #    record = pytis.form.new_record(self._spec_name('Modules'), prefill=prefill,
    #                                   block_on_new_record=True, transaction=transaction)
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
                method = getattr(module, 'action_'+action)
                docstring = method.__doc__
                return docstring and docstring.splitlines()[0] or _(u"Neuvedeno")
        module = self._module(record['modname'].value())
        if module:
            from pytis.form import run_dialog, CheckListDialog, create_data_object
            actions = [attr[7:] for attr in dir(module)
                       if attr.startswith('action_') and callable(getattr(module, attr)) \
                       and attr != 'action_subpath']
            default_actions = [a[0] for a in self._DEFAULT_ACTIONS]
            # Order default actions first and in the order of self._DEFAULT_ACTIONS.
            order = lambda a: a in default_actions and (default_actions.index(a)+1) or a
            actions.sort(lambda a, b: cmp(order(a), order(b)))
            descriptions = [action_descr(module, action) for action in actions] 
            result = run_dialog(CheckListDialog, title=_("Nalezené akce"),
                                message=_("Zaškrtněte akce, které chcete zpřístupnit webovým uživatelům:"), 
                                items=zip([True for a in actions], actions, descriptions),
                                columns=(_("Akce"), _("Popis")))
            if result is not None:
                # TODO: Use a transaction.  Respect existing actions.
                data = create_data_object(self._spec_name('Actions'))
                data.select(condition=pd.EQ('mod_id', record['mod_id']))
                existing_actions = {}
                while True:
                    row = data.fetchone()
                    if row is None:
                        break
                    else:
                        existing_actions[row['name'].value()] = row['action_id']
                data.close()
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
        ('view',    _(u"Zobrazení záznamu")),
        ('list',    _(u"Výpis všech záznamů")),
        ('insert',  _(u"Vložení nového záznamu")),
        ('update',  _(u"Editace stávajícího záznamu")),
        ('delete',  _(u"Smazání záznamu")),
        )
    _SEARCH_MODULES = ()
    """Defines list of names python modules which should be searched for available Wiking modules.

    Should be defined in the derived class to make module descriptions work.
    
    """


class MenuParents(Specification):
    # Codebook of parent items for Menu (to prevent recursion).
    title = _("Menu")
    table = 'cms_menu'
    def fields(self): return (
        Field('menu_id'),
        Field('menu_item_id'),
        Field('tree_order', type=pd.TreeOrder()),
        Field('lang'),
        Field('title_or_identifier', _("Název"), width=32),
        Field('identifier', _("Identifikátor"), width=20),
        Field('modname', _("Modul")),
        #Field('description', _("Popis"), width=64),
        )
    sorting = ('tree_order', ASC),
    columns = ('title_or_identifier', 'identifier', 'modname')
    cb = CodebookSpec(display='title_or_identifier', prefer_display=True)


class Menu(Specification):
    title = _("Menu")
    help = _("Správa položek hlavního menu, jejich hierarchie a obsahu.")
    table = 'cms_menu'
    def _parent_filter(self, record, lang):
        return pd.EQ('lang', pd.Value(pd.String(), lang))
    def fields(self): return (
        Field('menu_id'),
        Field('menu_item_id'),
        Field('tree_order', type=pd.TreeOrder()),
        Field('identifier', _("Identifikátor"), width=20, fixed=True, editable=ONCE,
              type=pd.RegexString(maxlen=32, not_null=True, regex='^[a-zA-Z][0-9a-zA-Z_-]*$'),
              descr=_("Identifikátor bude vystupovat jako vnější adresa stránky.  Může být použit "
                      "v odkazech na tuto stránku v rámci textu jiných stránek. Platný "
                      "identifikátor může obsahovat pouze písmena bez diakritiky, číslice, "
                      "pomlčky a podtržítka a musí začínat písmenem.")),
        Field('lang', _("Jazyk"), editable=ONCE, codebook=self._spec_name('Languages'),
              value_column='lang', selection_type=pp.SelectionType.CHOICE),
        Field('title_or_identifier', _("Název"), width=30),
        Field('title', _("Název"), width=32, not_null=True),
        Field('description', _("Popis"), width=64,
              descr=_("Stručný popis stránky (zobrazen v menu jako tooltip).")),
        Field('content', _("Obsah"), type=pd.StructuredText(), compact=True, height=20, width=80,
              descr=_("Text stránky formátovaný jako LCG strukturovaný text (wiki)")),
        Field('mod_id', _("Modul"), not_null=False,
              codebook=self._spec_name('Modules', False),
              descr=_("Vyberte rozšiřující modul zobrazený uvnitř stránky.  Ponechte prázdné pro "
                      "prostou textovou stránku.")),
        Field('modname', _("Modul")),
        Field('parent', _("Nadřízená položka"), not_null=False,
              codebook=self._spec_name('MenuParents', False), value_column='menu_item_id',
              runtime_filter=computer(self._parent_filter),
              descr=_("Vyberte bezprostředně nadřízenou položku v hierarchii menu.  Ponechte "
                      "prázdné pro stránky na nejvyšší úrovni menu.")),
        Field('published', _("Zveřejněno"), width=6, default=True, fixed=True, 
              descr=_("Umožňuje řídit dostupnost dané položky nezávisle pro kažou jazykovou "
                      "verzi.")),
        Field('ord', _("Pořadí"), width=8, editable=ALWAYS, fixed=True, 
              descr=_("Zadejte číslo určující pořadí položky v menu (mezi stránkami na stejné "
                      "úrovni hierarchie.  Pokud nevyplníte, stránka bude automaticky zařazena "
                      "na konec.")),
        )
    def check(self, record):
        data = record.data()
        if record['parent'].value() is not None:
            if record['parent'].value() == record['menu_item_id'].value():
                return ('parent', _("Položka nemůže být nadřízená sama sobě."))
            if not record.new():
                # A new record can't be a parent of existing records.
                tree_order = pd.WMValue(pd.String(), record['tree_order'].value()+'*')
                count = data.select(condition=pd.AND(pd.EQ('menu_item_id', record['parent']),
                                                     pd.WM('tree_order', tree_order)))
                data.close()
                if count:
                    return ('parent', _("Nelze přiřadit podřízenou položku jako nadřízenou "
                                        "(cyklus v hierarchii)."))
        count = data.select(condition=pd.AND(pd.EQ('parent', record['menu_item_id']),
                                             pd.EQ('ord', record['ord'])))
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
              VGroup('lang', 'title', 'description', 'content', 'published',
                     label=_("Text stránky (pro aktuální jazykovou verzi)")))
    columns = ('title_or_identifier', 'identifier', 'modname', 'ord', 'published')
    def bindings(self):
        return (Binding(_("Přístupová práva"), self._spec_name('Rights'),
                        condition=lambda r: pd.EQ('menu_item_id', r['menu_item_id'])),)


class Users(Specification):
    title = _("Uživatelé")
    help = _("Správa uživatelských účtů, které je poté možno zařazovat do rolí.")
    table = 'cms_users'
    def fields(self): return (
        Field('uid', _("UID"), width=5),
        Field('login', _("Přihlašovací jméno"), width=16),
        Field('fullname', _("Celé jméno"), width=40),
        Field('passwd', _("Heslo"),
              type=pd.Password(not_null=True, minlen=4, md5=True)),
        )
    layout = ('login', 'fullname', 'passwd')
    columns = ('uid', 'login', 'fullname')
    cb = CodebookSpec(display='fullname')
    sorting = (('login', ASC),)
    def bindings(self):
        return (Binding(_("Uživatelské role"), self._spec_name('UserRoles'), 'uid'),
                Binding(_("Historie přihlášení"), self._spec_name('UserSessionLog'), 'uid'),
                )


class Roles(Specification):
    title = _("Uživatelské role")
    help = _("Správa dostupných uživatelských rolí, které je možné přiřazovat uživatelům.")
    table = 'cms_roles'
    def fields(self): return (
        Field('role_id', default=nextval('cms_roles_role_id_seq')),
        Field('name', _("Název"), width=16),
        Field('system_role', _("Systémová role"), width=16, not_null=True),
        Field('description', _("Popis"), width=64),
        )
    layout = ('name', 'description')
    columns = ('name', 'description')
    condition = pd.EQ('system_role', pd.Value(pd.String(), None))
    def bindings(self): return (
        Binding(_("Uživatelé zařazení do této role"), self._spec_name('RoleUsers'), 'role_id'),
        )
    cb = CodebookSpec(display='name')

class SystemRoles(Roles):
    title = _("Systémové role")
    help = _("Systémové role jsou uživatelům přiřazeny automaticky.")
    layout = columns = ('name', 'system_role', 'description')
    condition = pd.NE('system_role', pd.Value(pd.String(), None))
    bindings = None
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))

    

    
class UserRoles(Specification):
    title = _("Přiřazení uživatelských rolí")
    help = _("Správa přiřazení uživatelských rolí jednotlivým uživatelům.")
    table = 'cms_user_roles'
    def fields(self): return (
        Field('user_role_id', default=nextval('cms_user_role_assignment_user_role_id_seq')),
        Field('role_id', _("Role"), codebook=self._spec_name('Roles', False)),
        Field('system_role'),
        Field('uid', _('UID'), codebook=self._spec_name('Users', False), width=5),
        Field('login', _("Přihlašovací jméno"), width=16),
        Field('fullname', _("Celé jméno"), width=50),
        Field('name', _("Název role"), width=16),
        Field('description', _("Popis"), width=50))
    layout = ('role_id',)
    columns = ('name', 'description')


class RoleUsers(UserRoles):
    help = _("Správa přiřazení uživatelů jednotlivým uživatelským rolím.")
    layout = ('uid',)
    columns = ('uid', 'login', 'fullname')


class Actions(Specification):
    title = _("Dostupné akce")
    help = _("Výčet podporovaných akcí pro jednotlivé moduly.")
    table = 'cms_actions'
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))
    def fields(self): return (
        Field('action_id', default=nextval('cms_actions_action_id_seq')),
        Field('mod_id', _("Modul"), codebook=self._spec_name('Modules', False)),
        Field('name', _("Název"), width=16),
        Field('description', _("Popis"), width=64))
    sorting = (('action_id', ASC),)
    layout = ('name', 'description')
    columns = ('name', 'description')

    
class GenericActions(Actions):
    title = _("Akce společné pro všechny položky menu")
    help = _("Výčet podporovaných akcí společných pro všechny položky menu.")
    condition = pd.EQ('mod_id', pd.Value(pd.Integer(), None))
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))

    
class Rights(Specification):
    title = _("Přístupová práva")
    help = _("Přiřazení práv k akcím modulů pro jednotlivé uživatelské role.")
    table = 'cms_rights'
    def fields(self): return (
        Field('right_id'),
        Field('menu_item_id'),
        Field('role_id'),
        Field('role_name', _("Role"), editable=NEVER),
        Field('system_role'),
        Field('mod_id'),
        Field('action_id'),
        Field('action_name', _("Akce"), editable=NEVER),
        Field('action_description', _("Popis"), width=30, editable=NEVER),
        Field('permitted', _("Povoleno"), width=3, fixed=True))
    grouping = 'role_id'
    sorting = (('role_name', ASC), ('mod_id', DESC), ('action_id', ASC))
    layout = columns = ('role_name', 'action_name', 'action_description', 'permitted')
    def row_style(self, record):
        if not record['mod_id'].value():
            return None
        else:
            return pp.Style(background='#ffd')
    def actions(self):
        return (Action(_("Povolit/zakázat"), self._toggle_permission, hotkey=' '),)
    def _toggle_permission(self, record):
        permitted = pd.Value(pd.Boolean(), not record['permitted'].value())
        record.data().update((record['right_id'],), pd.Row((('permitted', permitted),)))
        

class SessionLog(Specification):
    title = _("Log přihlášení")
    help = _("Záznam informací o přihlášení uživatelů k webu.")
    table = 'cms_session_log'
    def fields(self): return (
        Field('log_id'),
        Field('session_id'),
        Field('uid', codebook=self._spec_name('Users')),
        Field('login', _("Login"), width=8),
        Field('fullname', _("Jméno"), width=15),
        Field('success', _("Úspěch"), width=3),
        Field('start_time', _("Začátek"), width=17),
        Field('duration', _("Trvání"), width=17),
        Field('active', _("Aktivní")),
        Field('ip_address', _("IP adresa"), width=12, editable=NEVER),
        Field('hostname', _("Hostname"), virtual=True, computer=computer(self._hostname),
              column_width=15, width=40),
        Field('user_agent', _("User agent"), column_width=25, width=80),
        Field('referer', _("Referer"), column_width=25, width=80))
    def _hostname(self, row, ip_address):
        try:
            hostname = socket.gethostbyaddr(ip_address)[0]
        except:
            hostname = _("Neznámý")
        return hostname
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
    layout = ('start_time', 'duration', 'active', 'success', 'ip_address', 'hostname',
              'user_agent', 'referer')
    columns = ('start_time', 'duration', 'active', 'success', 'ip_address', 'user_agent')
    
