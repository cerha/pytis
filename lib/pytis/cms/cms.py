# -*- coding: utf-8 -*-
#
# Copyright (C) 2009, 2010, 2011 Brailcom, o.p.s.
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

from __future__ import unicode_literals

"""Pytis CMS specifications.

Pytis CMS is a fat client application for management of database structures, which define the
behavior of a web application.

Wiking modules which implement a Wiking application, which reads these database structures and
behaves according to them can be found in the 'web' submodule of this module.

"""

import collections
import socket
import lcg
import config
import pytis.data as pd, pytis.presentation as pp
from pytis.presentation import Field, Fields, HGroup, VGroup, Binding, Action, CodebookSpec, \
     Computer, CbComputer, computer

ASC = pd.ASCENDENT
DESC = pd.DESCENDANT
ALWAYS = pp.Editable.ALWAYS
ONCE = pp.Editable.ONCE
NEVER = pp.Editable.NEVER

def nextval(seq):
    def conn_spec():
        return config.dbconnection
    counter = pd.DBCounterDefault(seq, conn_spec)
    return lambda transaction=None: counter.next(transaction=transaction)


class _TreeOrder(pp.PrettyFoldable, pd.String):
    def __init__(self, **kwargs):
        super(_TreeOrder, self).__init__(tree_column_id='tree_order',
                                         subcount_column_id='tree_order_nsub',
                                         **kwargs)


class Specification(pp.Specification):
    access_rights = pd.AccessRights((None, (('cms_user', 'cms_admin'), pd.Permission.ALL)))
    public = True
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
        import os
        lcg_dir = os.environ.get('LCGDIR', '/usr/local/share/lcg')
        translation_dir = os.path.join(lcg_dir, 'translations')
        t = lcg.GettextTranslator('cs', path=(translation_dir,), fallback=True)
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
        return (Binding('actions', _("Dostupné akce tohoto modul"), self._spec_name('Actions'),
                        'mod_id'),)
    def actions(self):
        return (Action('reload', _("Přenačíst dostupné akce"), self._reload_actions),)
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
                return docstring and docstring.splitlines()[0] or _("Neuvedeno")
        module = self._module(record['modname'].value())
        if module:
            from pytis.form import run_dialog, CheckListDialog, create_data_object
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
            actions = [attr[7:] for attr in dir(module)
                       if attr.startswith('action_') and isinstance(getattr(module, attr), collections.Callable)]
            default_actions = [a[0] for a in self._DEFAULT_ACTIONS]
            # Order default actions first and in the order of self._DEFAULT_ACTIONS.
            order = lambda a: a in default_actions and (default_actions.index(a)+1) or a
            actions.sort(lambda a, b: cmp(order(a), order(b)))
            descriptions = [action_descr(module, action) for action in actions] 
            result = run_dialog(CheckListDialog, title=_("Nalezené akce"),
                                message=_("Zaškrtněte akce, které chcete zpřístupnit webovým "
                                          "uživatelům:"),
                                items=zip([a in existing_actions for a in actions],
                                          actions, descriptions),
                                columns=(_("Akce"), _("Popis")))
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
        ('view',    _("Zobrazení záznam")),
        ('list',    _("Výpis všech záznamů")),
        ('export',  _("Export tabulky do CSV formát")),
        ('rss',     _("Zobrazení RSS kanálů")),
        ('insert',  _("Vložení nového záznam")),
        ('update',  _("Editace stávajícího záznam")),
        ('delete',  _("Smazání záznam")),
        ('print_field', _("Export tisknutelných políček do PDF")),
        )
    _SEARCH_MODULES = ()
    """Defines list of names python modules which should be searched for available Wiking modules.

    Should be defined in the derived class to make module descriptions work.
    
    """


class MenuParents(Specification):
    # Codebook of parent items for Menu (to prevent recursion).
    title = _("Men")
    table = 'cms_menu'
    def fields(self): return (
        Field('menu_id'),
        Field('menu_item_id'),
        Field('tree_order'),
        Field('lang'),
        Field('title_or_identifier', _("Název"), width=32, type=_TreeOrder),
        Field('identifier', _("Identifikátor"), width=20),
        Field('modname', _("Modul")),
        #Field('description', _("Popis"), width=64),
        )
    sorting = ('tree_order', ASC),
    columns = ('title_or_identifier', 'identifier', 'modname')
    cb = CodebookSpec(display='title_or_identifier', prefer_display=True)


class Menu(Specification):
    title = _("Men")
    help = _("Správa položek hlavního menu, jejich hierarchie a obsahu.")
    table = 'cms_menu'
    def _parent_filter(self, record, lang):
        return pd.EQ('lang', pd.Value(pd.String(), lang))
    def fields(self): return (
        Field('menu_id'),
        Field('menu_item_id'),
        Field('tree_order'),
        Field('tree_order_nsub'),
        Field('identifier', _("Identifikátor"), width=20, fixed=True, editable=ONCE,
              type=pd.RegexString(maxlen=32, not_null=True, regex='^[a-zA-Z][0-9a-zA-Z_-]*$'),
              descr=_("Identifikátor bude vystupovat jako vnější adresa stránky.  Může být použit "
                      "v odkazech na tuto stránku v rámci textu jiných stránek. Platný "
                      "identifikátor může obsahovat pouze písmena bez diakritiky, číslice, "
                      "pomlčky a podtržítka a musí začínat písmenem.")),
        Field('lang', _("Jazyk"), editable=ONCE, codebook=self._spec_name('Languages'),
              value_column='lang', selection_type=pp.SelectionType.CHOICE),
        Field('title_or_identifier', _("Název"), width=30, type=_TreeOrder),
        Field('title', _("Název"), width=20, not_null=True, maxlen=32,
              descr=_("Název položky menu - krátký a výstižný.")),
        Field('heading', _("Nadpis"), width=32, maxlen=40,
              descr=_("Hlavní nadpis při zobrazení stránky.  Pokud ponecháte nevyplněné, "
                      "použije se název položky.  Ten je ale někdy kvůli použití v menu příliš "
                      "krátký, takže zde je možné určit jeho delší variantu.")),
        Field('description', _("Popis"), width=72,
              descr=_("Stručný popis stránky (zobrazen v menu jako tooltip).")),
        Field('content', _("Obsah"), type=pd.StructuredText(), compact=True, height=20, width=80,
              descr=_("Text stránky formátovaný jako LCG strukturovaný text (wiki)")),
        Field('mod_id', _("Modul"), not_null=False,
              codebook=self._spec_name('Modules', False), allow_codebook_insert=True,
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
            Binding('rights', _("Přístupová práva"), self._spec_name('Rights'),
                    condition=lambda r: pd.EQ('menu_item_id', r['menu_item_id']),
                    prefill=lambda r: {'menu_item_id': r['menu_item_id'].value(),
                                       'mod_id': r['mod_id'].value()}),
            )


class Users(Specification):
    title = _("Uživatelé")
    help = _("Správa uživatelských účtů, které je poté možno zařazovat do rolí.")
    table = 'cms_users'
    def fields(self): return (
        Field('uid', _("UID"), width=5, default=nextval('cms_users_uid_seq')),
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
        return (Binding('roles', _("Uživatelské role"), self._spec_name('UserRoles'), 'uid'),
                Binding('sessions', _("Historie přihlášení"), self._spec_name('UserSessionLog'),
                        'uid'),
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
        Binding('users', _("Uživatelé zařazení do této role"), self._spec_name('RoleUsers'),
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
    help = _("Role dostupné pro přiřazování práv akcím (obsahuje systémové i uživatelsky definované role).")
    condition = None

    
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
    table = 'cms_rights'
    def fields(self): return (
        Field('rights_assignment_id',
              default=nextval('cms_rights_assignment_rights_assignment_id_seq')),
        Field('menu_item_id'),
        Field('role_id', _("Role"), codebook=self._spec_name('AllRoles', False)),
        Field('role_name', _("Role")),
        Field('system_role'),
        Field('mod_id'),
        Field('action_id', _("Akce"), codebook=self._spec_name('Actions', False),
              runtime_filter=computer(self._action_filter)),
        Field('action_name', _("Akce")),
        Field('action_description', _("Popis"), width=30, editable=NEVER),
        )
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
    def fields(self): return (
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
        
class SessionLog(_Log):
    title = _("Log přihlášení")
    help = _("Záznam informací o přihlášení uživatelů k webu.")
    table = 'cms_session_log'
    public = True
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
        ) + super(SessionLog, self).fields()
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
    table = 'cms_access_log_data'
    public = True
    def fields(self): return (
        Field('log_id'),
        Field('timestamp', _("Datum a čas"), width=17),
        Field('uri', _("Cesta"), width=17),
        Field('uid', _("Uživatel"), codebook=self._spec_name('Users')),
        Field('modname', _("Modul"), width=17),
        Field('action', _("Akce"), width=17),
        ) + super(AccessLog, self).fields()
    layout = ('timestamp', 'uri', 'uid', 'modname', 'action',
              'ip_address', 'hostname', 'user_agent', 'referer')
    columns = ('timestamp', 'uri', 'uid', 'modname', 'action',
               'ip_address', 'user_agent', 'referer')
    sorting = (('timestamp', DESC),)

    
class Themes(Specification):
    class _Field(Field):
        def __init__(self, id, label, descr=None):
            Field.__init__(self, id, label, descr=descr, type=pd.Color(),
                           dbcolumn=id.replace('-','_'))
    title = _("Barevné Motivy")
    help = _("Správa barevných motivů.")
    table = 'cms_themes'
    fields = (
        Field('theme_id'),
        Field('name', _("Název"), nocopy=True),
        _Field('foreground', _("Text"),
               descr=("Barva popředí hlavních obsahových ploch.")),
        _Field('background', _("Pozadí"),
               descr=("Barva pozadí hlavních obsahových ploch (vlastní text stránky, panely).")),
        _Field('highlight-bg', _("Zvýrazněné pozadí"),
               descr=_("Zvýraznění pomocí barvy pozadí může být použito na různých místech, "
                       "např. pro odlišení aktuální položky menu, aktuálního jazyka, nalezeného "
                       "záznamu ve formuláři apod.")),
        _Field('link', _("Odkaz"),
               descr=_("Barva textu nenavštíveného odkazu.")),
        _Field('link-visited', _("Navštívený odkaz"),
               descr=_("Barva textu navštíveného odkazu.  Pokud ponecháte prázdné, bude "
                       "použita stejná barva jako u nenavštíveného odkazu.")),
        _Field('link-hover', _("Aktivní odkaz"),
               descr=_("Obarvení odkazu při při pohybu myší nad ním.  Pokud ponecháte "
                       "prázdné, nebude odkaz na pohyb myši reagovat.")),
        _Field('border', _("Orámování")),
        _Field('heading-fg', _("Text"),
               descr=_("Barvy nadpisů jsou používány pro zvýrazenění názvů kapitol, panelů a "
                       "jiných elementů majících povahu nadpisu či záhlaví.  V závislosti na "
                       "použitých stylech mohou některé typy nadpisů používat odlišnou barvu "
                       "pozadí, jiné mohou být pouze podtrženy (viz dále).  Barva textu je "
                       "však společná pro oba typy nadpisů.")),
        _Field('heading-bg', _("Pozadí"),
               descr=_("Barva pozadí pro nadpisy, které používají zvýraznění barvou pozadí."
                       "Typicky jsou to záhlaví částí stránky (menu, panely apod.) a nadpisy "
                       "vyšších úrovní.")),
        _Field('heading-line', _("Podtržení"),
               descr=_("Barva podtržení pro nadpisy, které používají zvýraznění podtržením."
                       "Typicky jsou to nadpisy nižších úrovní.")),
        _Field('frame-fg', _("Text"),
               descr=_("Rámy jsou obecně využívány pro odlišení samostatných prvků stránky, "
                       "jako jsou například formuláře, nástrojové lišty, rejstříky apod. "
                       "Rámy v tomto smyslu nemají nic splečného s HTML rámy (frame).  Pokud "
                       "barvu textu nevyplníte, bude použita standardní barva textu (často u "
                       "rámů budete chtít nastavit pouze barvu pozadí a orámování).")),
        _Field('frame-bg', _("Pozadí"),
               descr=_("Barva pozadí rámů pro jejich vizuální odlišení od ostatního obsahu "
                       "stránky.")),
        _Field('frame-border', _("Orámování"),
               descr=_("Barva orámování okrajů plochy rámu na přechodu mezi barvou pozadí rámu "
                       "a jeho podkladem.")),
        _Field('top-fg', _("Text"),
               descr=_("Na podkladových plochách se většinou text přímo nevyskytuje, ale "
                       "některé styly to mohou předepisovat.")),
        _Field('top-bg', _("Pozadí"),
               descr=_("Podkladové plochy tvoří okolí hlavního obsahu stránky a panelů.")),
        _Field('top-border', _("Orámování"),
               descr=_("Určuje barvu rámečku na přechodu mezi podkladovou plochou a obsahovými "
                       "částmi stránky")),
        _Field('error-fg', _("Text")),
        _Field('error-bg', _("Pozadí")),
        _Field('error-border', _("Orámování")),
        _Field('message-fg', _("Text")),
        _Field('message-bg', _("Pozadí")),
        _Field('message-border', _("Orámování")),
        _Field('meta-fg', _("Text")),
        _Field('meta-bg', _("Pozadí"),
               descr=_("Odlišení informačního řádku v detailních výpisech, jako např. "
                       "informace o datu a autorovi v seznamu článků")),
        _Field('table-cell', _("Standardní pozadí buňky")),
        _Field('table-cell2', _("Stínované pozadí buňky")),
        _Field('help', _("Nápověda formuláře")),
        _Field('inactive-folder', _("Neaktivní záložka")),
        )
    layout = pp.HGroup(pp.VGroup(
        'name',
        pp.LVGroup(_("Standardní barvy stránky"),
                   ('foreground', 'background', 'highlight-bg', 'link', 'link-visited',
                    'link-hover', 'border')),
        pp.LVGroup(_("Tabulky"),
                   ('table-cell', 'table-cell2')),
        pp.LVGroup(_("Různé"),
                   ('help', 'inactive-folder')),
        ), pp.VGroup(
        pp.LVGroup(_("Barvy nadpisů"),
                   ('heading-fg', 'heading-bg', 'heading-line')),
        pp.LVGroup(_("Rámy"),
                   ('frame-fg', 'frame-bg', 'frame-border')),
        pp.LVGroup(_("Meta data záznam"),
                   ('meta-fg', 'meta-bg')),
        pp.LVGroup(_("Barvy podkladových ploch"),
                   ('top-bg', 'top-border')),
        ), pp.VGroup(
        pp.LVGroup(_("Chybové zprávy"),
                   ('error-fg', 'error-bg', 'error-border')),
        pp.LVGroup(_("Informační zprávy"),
                   ('message-fg', 'message-bg', 'message-border')),
        ))
    columns = ('name',)
    cb = CodebookSpec(display='name', prefer_display=True)

