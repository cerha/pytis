# -*- coding: iso-8859-2 -*-
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

"""Pytis CMS specifications.

Pytis CMS is a fat client application for management of database structures, which define the
behavior of a web application.

Wiking modules which implement a Wiking application, which reads these database structures and
behaves according to them can be found in the 'web' submodule of this module.

"""

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
    help = _("Spr�va jazyk� dostupn�ch v CMS.")
    table = 'cms_languages'
    def fields(self): return (
        Field('lang_id', default=nextval('cms_languages_lang_id_seq')),
        Field('lang', _("K�d"), width=2, column_width=6, fixed=True,
              filter=pp.TextFilter.ALPHANUMERIC, post_process=pp.PostProcess.LOWER),
        Field('name', _("N�zev"), virtual=True,
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
    help = _("Spr�va roz�i�uj�c�ch modul� pou�iteln�ch ve str�nk�ch.")
    table = 'cms_modules'
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))
    def fields(self): return (
        Field('mod_id', default=nextval('cms_modules_mod_id_seq')),
        Field('modname', _("N�zev"), width=32),
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
        return (Binding('actions', _("Dostupn� akce tohoto modulu"), self._spec_name('Actions'),
                        'mod_id'),)
    def actions(self):
        return (Action('reload', _("P�ena��st dostupn� akce"), self._reload_actions),)
    def on_delete_record(self, record):
        import pytis.form
        data = pytis.form.create_data_object(self._spec_name('Menu'))
        count = data.select(condition=pd.EQ('mod_id', record['mod_id']))
        data.close()
        if count:
            return _("Modul je pou��v�n v existuj�c�ch str�nk�ch.\n"
                     "Pokud jej chcete vymazat, zru�te nejprve v�echny nav�zan� str�nky.")
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
                       if attr.startswith('action_') and callable(getattr(module, attr))]
            default_actions = [a[0] for a in self._DEFAULT_ACTIONS]
            # Order default actions first and in the order of self._DEFAULT_ACTIONS.
            order = lambda a: a in default_actions and (default_actions.index(a)+1) or a
            actions.sort(lambda a, b: cmp(order(a), order(b)))
            descriptions = [action_descr(module, action) for action in actions] 
            result = run_dialog(CheckListDialog, title=_("Nalezen� akce"),
                                message=_("Za�krtn�te akce, kter� chcete zp��stupnit webov�m "
                                          "u�ivatel�m:"),
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
        ('view',    _(u"Zobrazen� z�znamu")),
        ('list',    _(u"V�pis v�ech z�znam�")),
        ('export',  _(u"Export tabulky do CSV form�tu")),
        ('rss',     _(u"Zobrazen� RSS kan�l�")),
        ('insert',  _(u"Vlo�en� nov�ho z�znamu")),
        ('update',  _(u"Editace st�vaj�c�ho z�znamu")),
        ('delete',  _(u"Smaz�n� z�znamu")),
        ('print_field', _(u"Export tisknuteln�ch pol��ek do PDF")),
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
        Field('tree_order'),
        Field('lang'),
        Field('title_or_identifier', _("N�zev"), width=32, type=_TreeOrder),
        Field('identifier', _("Identifik�tor"), width=20),
        Field('modname', _("Modul")),
        #Field('description', _("Popis"), width=64),
        )
    sorting = ('tree_order', ASC),
    columns = ('title_or_identifier', 'identifier', 'modname')
    cb = CodebookSpec(display='title_or_identifier', prefer_display=True)


class Menu(Specification):
    title = _("Menu")
    help = _("Spr�va polo�ek hlavn�ho menu, jejich hierarchie a obsahu.")
    table = 'cms_menu'
    def _parent_filter(self, record, lang):
        return pd.EQ('lang', pd.Value(pd.String(), lang))
    def fields(self): return (
        Field('menu_id'),
        Field('menu_item_id'),
        Field('tree_order'),
        Field('tree_order_nsub'),
        Field('identifier', _("Identifik�tor"), width=20, fixed=True, editable=ONCE,
              type=pd.RegexString(maxlen=32, not_null=True, regex='^[a-zA-Z][0-9a-zA-Z_-]*$'),
              descr=_("Identifik�tor bude vystupovat jako vn�j�� adresa str�nky.  M��e b�t pou�it "
                      "v odkazech na tuto str�nku v r�mci textu jin�ch str�nek. Platn� "
                      "identifik�tor m��e obsahovat pouze p�smena bez diakritiky, ��slice, "
                      "poml�ky a podtr��tka a mus� za��nat p�smenem.")),
        Field('lang', _("Jazyk"), editable=ONCE, codebook=self._spec_name('Languages'),
              value_column='lang', selection_type=pp.SelectionType.CHOICE),
        Field('title_or_identifier', _("N�zev"), width=30, type=_TreeOrder),
        Field('title', _("N�zev"), width=20, not_null=True, maxlen=32,
              descr=_("N�zev polo�ky menu - kr�tk� a v�sti�n�.")),
        Field('heading', _("Nadpis"), width=32, maxlen=40,
              descr=_("Hlavn� nadpis p�i zobrazen� str�nky.  Pokud ponech�te nevypln�n�, "
                      "pou�ije se n�zev polo�ky.  Ten je ale n�kdy kv�li pou�it� v menu p��li� "
                      "kr�tk�, tak�e zde je mo�n� ur�it jeho del�� variantu.")),
        Field('description', _("Popis"), width=72,
              descr=_("Stru�n� popis str�nky (zobrazen v menu jako tooltip).")),
        Field('content', _("Obsah"), type=pd.StructuredText(), compact=True, height=20, width=80,
              descr=_("Text str�nky form�tovan� jako LCG strukturovan� text (wiki)")),
        Field('mod_id', _("Modul"), not_null=False,
              codebook=self._spec_name('Modules', False), allow_codebook_insert=True,
              descr=_("Vyberte roz�i�uj�c� modul zobrazen� uvnit� str�nky.  Ponechte pr�zdn� pro "
                      "prostou textovou str�nku.")),
        Field('modname', _("Modul")),
        Field('parent', _("Nad��zen� polo�ka"), not_null=False,
              codebook=self._spec_name('MenuParents', False), value_column='menu_item_id',
              runtime_filter=computer(self._parent_filter),
              descr=_("Vyberte bezprost�edn� nad��zenou polo�ku v hierarchii menu.  Ponechte "
                      "pr�zdn� pro str�nky na nejvy��� �rovni menu.")),
        Field('published', _("Zve�ejn�no"), width=6, default=True, fixed=True, 
              descr=_("Umo��uje ��dit dostupnost dan� polo�ky nez�visle pro ka�ou jazykovou "
                      "verzi.")),
        Field('ord', _("Po�ad�"), width=8, editable=ALWAYS, fixed=True, 
              descr=_("Zadejte ��slo ur�uj�c� po�ad� polo�ky v menu (mezi str�nkami na stejn� "
                      "�rovni hierarchie.  Pokud nevypln�te, str�nka bude automaticky za�azena "
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
                return ('parent', _("Polo�ka nem��e b�t nad��zen� sama sob�."))
            if not record.new():
                # A new record can't be a parent of existing records.
                condition = pd.AND(pd.EQ('menu_item_id', record['parent']),
                                   pd.LTreeDescendant('tree_order', record['tree_order']))
                count = data.select(condition=condition)
                data.close()
                if count:
                    return ('parent', _("Nelze p�i�adit pod��zenou polo�ku jako nad��zenou "
                                        "(cyklus v hierarchii)."))
        count = data.select(condition=pd.AND(*self._check_menu_order_condition(record)))
        data.close()
        if count:
            return ('ord', _("Stejn� po�adov� ��slo u� m� jin� polo�ka na t�to �rovni menu."))

        
    def on_delete_record(self, record):
        data = record.data()
        count = data.select(condition=pd.EQ('parent', record['menu_item_id']))
        data.close()
        if count:
            return _("Polo�ka m� pod��zen� polo�ky.\n"
                     "Pokud ji chcete vymazat, vyma�te nejprve v�echny podpolo�ky.")
        else:
            return True
    def row_style(self, record):
        if not record['published'].value():
            return pp.Style(foreground='#777', background='#eee')
        else:
            return None
    sorting = ('tree_order', ASC),
    layout = (VGroup('identifier', 'parent', 'mod_id', 'ord',
                     label=_("Glob�ln� vlastnosti (spole�n� pro v�echny jazyky)")),
              VGroup('lang', 'title', 'heading', 'description', 'content', 'published',
                     label=_("Text str�nky (pro aktu�ln� jazykovou verzi)")))
    columns = ('title_or_identifier', 'identifier', 'modname', 'ord', 'published')
    def bindings(self):
        return (
            Binding('rights', _("P��stupov� pr�va"), self._spec_name('Rights'),
                    condition=lambda r: pd.EQ('menu_item_id', r['menu_item_id']),
                    prefill=lambda r: {'menu_item_id': r['menu_item_id'].value(),
                                       'mod_id': r['mod_id'].value()}),
            )


class Users(Specification):
    title = _("U�ivatel�")
    help = _("Spr�va u�ivatelsk�ch ��t�, kter� je pot� mo�no za�azovat do rol�.")
    table = 'cms_users'
    def fields(self): return (
        Field('uid', _("UID"), width=5, default=nextval('cms_users_uid_seq')),
        Field('login', _("P�ihla�ovac� jm�no"), width=16),
        Field('fullname', _("Cel� jm�no"), width=40),
        Field('passwd', _("Heslo"),
              type=pd.Password(not_null=True, minlen=4, md5=True)),
        )
    layout = ('login', 'fullname', 'passwd')
    columns = ('uid', 'login', 'fullname')
    cb = CodebookSpec(display='fullname')
    sorting = (('login', ASC),)
    def bindings(self):
        return (Binding('roles', _("U�ivatelsk� role"), self._spec_name('UserRoles'), 'uid'),
                Binding('sessions', _("Historie p�ihl�en�"), self._spec_name('UserSessionLog'),
                        'uid'),
                )


class Roles(Specification):
    title = _("U�ivatelsk� role")
    help = _("Spr�va dostupn�ch u�ivatelsk�ch rol�, kter� je mo�n� p�i�azovat u�ivatel�m.")
    table = 'cms_roles'
    def fields(self): return (
        Field('role_id', default=nextval('cms_roles_role_id_seq')),
        Field('name', _("N�zev"), width=16),
        Field('system_role', _("Syst�mov� role"), width=16, not_null=True),
        Field('description', _("Popis"), width=64),
        )
    layout = ('name', 'description')
    columns = ('name', 'description')
    condition = pd.EQ('system_role', pd.Value(pd.String(), None))
    def bindings(self): return (
        Binding('users', _("U�ivatel� za�azen� do t�to role"), self._spec_name('RoleUsers'),
                'role_id'),
        )
    cb = CodebookSpec(display='name')

class SystemRoles(Roles):
    title = _("Syst�mov� role")
    help = _("Syst�mov� role jsou u�ivatel�m p�i�azeny automaticky.")
    layout = columns = ('name', 'system_role', 'description')
    condition = pd.NE('system_role', pd.Value(pd.String(), None))
    bindings = ()
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))


class AllRoles(Roles):
    title = _("Role")
    help = _("Role dostupn� pro p�i�azov�n� pr�v akc�m (obsahuje syst�mov� i u�ivatelsky definovan� role).")
    condition = None

    
class UserRoles(Specification):
    title = _("P�i�azen� u�ivatelsk�ch rol�")
    help = _("Spr�va p�i�azen� u�ivatelsk�ch rol� jednotliv�m u�ivatel�m.")
    table = 'cms_user_roles'
    def fields(self): return (
        Field('user_role_id', default=nextval('cms_user_role_assignment_user_role_id_seq')),
        Field('role_id', _("Role"), codebook=self._spec_name('Roles', False)),
        Field('system_role'),
        Field('uid', _('UID'), codebook=self._spec_name('Users', False), width=5),
        Field('login', _("P�ihla�ovac� jm�no"), width=16),
        Field('fullname', _("Cel� jm�no"), width=50),
        Field('name', _("N�zev role"), width=16),
        Field('description', _("Popis"), width=50))
    layout = ('role_id',)
    columns = ('name', 'description')


class RoleUsers(UserRoles):
    help = _("Spr�va p�i�azen� u�ivatel� jednotliv�m u�ivatelsk�m rol�m.")
    layout = ('uid',)
    columns = ('uid', 'login', 'fullname')


class Actions(Specification):
    title = _("Dostupn� akce")
    help = _("V��et podporovan�ch akc� pro jednotliv� moduly.")
    table = 'cms_actions'
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))
    def fields(self): return (
        Field('action_id', default=nextval('cms_actions_action_id_seq')),
        Field('mod_id', _("Modul"), codebook=self._spec_name('Modules', False)),
        Field('name', _("N�zev"), width=16),
        Field('description', _("Popis"), width=64))
    sorting = (('action_id', ASC),)
    layout = ('name', 'description')
    columns = ('name', 'description')
    cb = CodebookSpec(display='name')

    
class GenericActions(Actions):
    title = _("Akce spole�n� pro v�echny polo�ky menu")
    help = _("V��et podporovan�ch akc� spole�n�ch pro v�echny polo�ky menu.")
    condition = pd.EQ('mod_id', pd.Value(pd.Integer(), None))
    access_rights = pd.AccessRights((None, ('cms_admin', pd.Permission.ALL)),
                                    (None, ('cms_user', pd.Permission.VIEW)))

    
class Rights(Specification):
    title = _("P��stupov� pr�va")
    help = _("P�i�azen� pr�v k akc�m modul� pro jednotliv� u�ivatelsk� role.")
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
            hostname = _("Nezn�m�")
        return hostname
        
class SessionLog(_Log):
    title = _("Log p�ihl�en�")
    help = _("Z�znam informac� o p�ihl�en� u�ivatel� k webu.")
    table = 'cms_session_log'
    public = True
    def fields(self): return (
        Field('log_id'),
        Field('session_id'),
        Field('uid', codebook=self._spec_name('Users')),
        Field('login', _("Login"), width=8),
        Field('fullname', _("Jm�no"), width=15),
        Field('success', _("�sp�ch"), width=3),
        Field('start_time', _("Za��tek"), width=17),
        Field('duration', _("Trv�n�"), width=17),
        Field('active', _("Aktivn�")),
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
    title = _("Log p��stup�")
    help = _("Z�znam informac� o p��stupu u�ivatel� k jednotliv�m str�nk�m/modul�m webu.")
    table = 'cms_access_log_data'
    public = True
    def fields(self): return (
        Field('log_id'),
        Field('timestamp', _("Datum a �as"), width=17),
        Field('uri', _("Cesta"), width=17),
        Field('uid', _("U�ivatel"), codebook=self._spec_name('Users')),
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
    title = _("Barevn� Motivy")
    help = _("Spr�va barevn�ch motiv�.")
    table = 'cms_themes'
    fields = (
        Field('theme_id'),
        Field('name', _("N�zev"), nocopy=True),
        _Field('foreground', _("Text"),
               descr=("Barva pop�ed� hlavn�ch obsahov�ch ploch.")),
        _Field('background', _("Pozad�"),
               descr=("Barva pozad� hlavn�ch obsahov�ch ploch (vlastn� text str�nky, panely).")),
        _Field('highlight-bg', _("Zv�razn�n� pozad�"),
               descr=_("Zv�razn�n� pomoc� barvy pozad� m��e b�t pou�ito na r�zn�ch m�stech, "
                       "nap�. pro odli�en� aktu�ln� polo�ky menu, aktu�ln�ho jazyka, nalezen�ho "
                       "z�znamu ve formul��i apod.")),
        _Field('link', _("Odkaz"),
               descr=_("Barva textu nenav�t�ven�ho odkazu.")),
        _Field('link-visited', _("Nav�t�ven� odkaz"),
               descr=_("Barva textu nav�t�ven�ho odkazu.  Pokud ponech�te pr�zdn�, bude "
                       "pou�ita stejn� barva jako u nenav�t�ven�ho odkazu.")),
        _Field('link-hover', _("Aktivn� odkaz"),
               descr=_("Obarven� odkazu p�i p�i pohybu my�� nad n�m.  Pokud ponech�te "
                       "pr�zdn�, nebude odkaz na pohyb my�i reagovat.")),
        _Field('border', _("Or�mov�n�")),
        _Field('heading-fg', _("Text"),
               descr=_("Barvy nadpis� jsou pou��v�ny pro zv�razen�n� n�zv� kapitol, panel� a "
                       "jin�ch element� maj�c�ch povahu nadpisu �i z�hlav�.  V z�vislosti na "
                       "pou�it�ch stylech mohou n�kter� typy nadpis� pou��vat odli�nou barvu "
                       "pozad�, jin� mohou b�t pouze podtr�eny (viz d�le).  Barva textu je "
                       "v�ak spole�n� pro oba typy nadpis�.")),
        _Field('heading-bg', _("Pozad�"),
               descr=_("Barva pozad� pro nadpisy, kter� pou��vaj� zv�razn�n� barvou pozad�."
                       "Typicky jsou to z�hlav� ��st� str�nky (menu, panely apod.) a nadpisy "
                       "vy���ch �rovn�.")),
        _Field('heading-line', _("Podtr�en�"),
               descr=_("Barva podtr�en� pro nadpisy, kter� pou��vaj� zv�razn�n� podtr�en�m."
                       "Typicky jsou to nadpisy ni���ch �rovn�.")),
        _Field('frame-fg', _("Text"),
               descr=_("R�my jsou obecn� vyu��v�ny pro odli�en� samostatn�ch prvk� str�nky, "
                       "jako jsou nap��klad formul��e, n�strojov� li�ty, rejst��ky apod. "
                       "R�my v tomto smyslu nemaj� nic sple�n�ho s HTML r�my (frame).  Pokud "
                       "barvu textu nevypln�te, bude pou�ita standardn� barva textu (�asto u "
                       "r�m� budete cht�t nastavit pouze barvu pozad� a or�mov�n�).")),
        _Field('frame-bg', _("Pozad�"),
               descr=_("Barva pozad� r�m� pro jejich vizu�ln� odli�en� od ostatn�ho obsahu "
                       "str�nky.")),
        _Field('frame-border', _("Or�mov�n�"),
               descr=_("Barva or�mov�n� okraj� plochy r�mu na p�echodu mezi barvou pozad� r�mu "
                       "a jeho podkladem.")),
        _Field('top-fg', _("Text"),
               descr=_("Na podkladov�ch ploch�ch se v�t�inou text p��mo nevyskytuje, ale "
                       "n�kter� styly to mohou p�edepisovat.")),
        _Field('top-bg', _("Pozad�"),
               descr=_("Podkladov� plochy tvo�� okol� hlavn�ho obsahu str�nky a panel�.")),
        _Field('top-border', _("Or�mov�n�"),
               descr=_("Ur�uje barvu r�me�ku na p�echodu mezi podkladovou plochou a obsahov�mi "
                       "��stmi str�nky")),
        _Field('error-fg', _("Text")),
        _Field('error-bg', _("Pozad�")),
        _Field('error-border', _("Or�mov�n�")),
        _Field('message-fg', _("Text")),
        _Field('message-bg', _("Pozad�")),
        _Field('message-border', _("Or�mov�n�")),
        _Field('meta-fg', _("Text")),
        _Field('meta-bg', _("Pozad�"),
               descr=_("Odli�en� informa�n�ho ��dku v detailn�ch v�pisech, jako nap�. "
                       "informace o datu a autorovi v seznamu �l�nk�")),
        _Field('table-cell', _("Standardn� pozad� bu�ky")),
        _Field('table-cell2', _("St�novan� pozad� bu�ky")),
        _Field('help', _("N�pov�da formul��e")),
        _Field('inactive-folder', _("Neaktivn� z�lo�ka")),
        )
    layout = pp.HGroup(pp.VGroup(
        'name',
        pp.LVGroup(_("Standardn� barvy str�nky"),
                   ('foreground', 'background', 'highlight-bg', 'link', 'link-visited',
                    'link-hover', 'border')),
        pp.LVGroup(_("Tabulky"),
                   ('table-cell', 'table-cell2')),
        pp.LVGroup(_("R�zn�"),
                   ('help', 'inactive-folder')),
        ), pp.VGroup(
        pp.LVGroup(_("Barvy nadpis�"),
                   ('heading-fg', 'heading-bg', 'heading-line')),
        pp.LVGroup(_("R�my"),
                   ('frame-fg', 'frame-bg', 'frame-border')),
        pp.LVGroup(_("Meta data z�znamu"),
                   ('meta-fg', 'meta-bg')),
        pp.LVGroup(_("Barvy podkladov�ch ploch"),
                   ('top-bg', 'top-border')),
        ), pp.VGroup(
        pp.LVGroup(_("Chybov� zpr�vy"),
                   ('error-fg', 'error-bg', 'error-border')),
        pp.LVGroup(_("Informa�n� zpr�vy"),
                   ('message-fg', 'message-bg', 'message-border')),
        ))
    columns = ('name',)
    cb = CodebookSpec(display='name', prefer_display=True)

