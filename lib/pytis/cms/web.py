# -*- coding: utf-8 -*-
# Copyright (C) 2006, 2007, 2008, 2009 Brailcom, o.p.s.
# Author: Tomas Cerha.
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

"""Wiking modules for web applications based on Pytis CMS.

Pytis CMS is a fat client application for management of database structures, which define the
behavior of a web application.  Wiking modules defined here implement a Wiking application, which
reads these database structures and behaves according to them.

Example application using these Wiking modules can be found in pytis-demo.

"""

import os, re, mx.DateTime, lcg, wiking
import hashlib, binascii
import pytis.util, pytis.presentation as pp, pytis.data as pd, pytis.web as pw
import cms

class Specification(wiking.Specification):
    access_rights = None
    def _spec_name(self, name, needed_in_wiking=True):
        if needed_in_wiking:
            return name
        else:
            return None
    

class Languages(wiking.PytisModule):
    class Spec(Specification, cms.Languages):
        pass

    def languages(self):
        return [str(r['lang'].value()) for r in self._data.get_rows()]


class Menu(wiking.PytisModule):

    class Spec(Specification, cms.Menu):
        pass
    
    _ITEM_PATH_LENGTH = 1
    _SEPARATOR = re.compile('^====+\s*$', re.MULTILINE)
    _SUBSTITUTION_PROVIDERS = ()
    """Mapping of substitution variable names and modules providing their values.

    Sequence of pairs (NAME, MODULE), where NAME is the string name of substitution variable and
    MODULE is the string name of the module providing substitution values for this variable.  The
    module must be derived from SubstitutionProvider class.  Such substitution variables are always
    used as dictionaries, so the mapped module in fact provides the dictionary keys and their
    values.

    Example:

    _SUBSTITUTION_PROVIDERS = (('cfg', 'Config'),
                               ('user', 'UserConfig'))

    Page text may than contain a variable '$cfg.optionxy' which will be substituted by the value of
    field 'optionxy' obtained from the module 'Config' and a variable '$user.language' which will
    be substituted by the value of 'language' field from the current user's record.  See also
    'SubstitutionProvider' documentation for information how to implement such modules.

    """
    EMBED_BINDING_ID = 'data'

    def _resolve(self, req):
        kwargs = self._resolve_menu_args(req)
        rows = self._data.get_rows(published=True, **kwargs)
        if not rows:
            if self._data.get_rows(**kwargs):
                raise wiking.Forbidden()
            else:
                raise wiking.NotFound()
        variants = [str(row['lang'].value()) for row in rows]
        lang = req.prefered_language(variants)
        row = rows[variants.index(lang)]
        del req.unresolved_path[:self._ITEM_PATH_LENGTH]
        # This is a big hack, but we need to mark the real number defined by the (potentially
        # overriden) menu class.  This number is later used in 'EmbeddablePytisModule' class below.
        req.menu_path_length = self._ITEM_PATH_LENGTH
        return row

    def _resolve_menu_args(self, req):
        """Return arguments identifying the current menu item rows.

        Returns arguments for self._data.get_rows() which limit returned rows to match the current
        menu item (multiple rows only represent language variants of the item).

        """
        return dict(identifier=req.unresolved_path[0])
    
    def _menu_item_rows(self, req, **kwargs):
        return self._data.get_rows(req, **kwargs)
    
    def _embed_binding(self, modname):
        return wiking.Binding(modname, modname, None, id=self.EMBED_BINDING_ID,
                              condition=lambda r: None)

    def _bindings(self, req, record):
        bindings = super(Menu, self)._bindings(req, record)
        modname = record['modname'].value()
        if modname:
            bindings.append(self._embed_binding(modname))
        return bindings

    def _action(self, req, record=None):
        # The only supported action of this module is `view' and the `action' argument is ignored
        # here (left for the embedded module action resolution).
        if record is not None and req.unresolved_path:
            return 'subpath'
        else:
            return 'view'
    
    def _handle(self, req, action, **kwargs):
        # HACK: We need to store the current resolved menu item somewhere.
        # Wiking support for resolution tracking would be a clean solution.
        req.cms_current_menu_record = kwargs.get('record')
        return super(Menu, self)._handle(req, action, **kwargs)

    def _pytis_redirect_origin(self, req):
        """Find the original module if the request was forwarded due to pytis redirection.
        
        Returns module instance of the original request handler (before pytis redirection) or None
        
        """
        module = None
        forwards = req.forwards()
        i = len(forwards) - 1
        while i > 0 and forwards[i].arg('pytis_redirect'):
            module = forwards[i-1].module()
            i -= 1
        return module

    def permitted_roles(self, req, module, action=None, record=None, **kwargs):
        #wiking.debug("...", module.name(), action, hasattr(req, 'cms_current_menu_record'))
        if hasattr(req, 'cms_current_menu_record'):
            rights = self._module('Rights')
            menu_record = req.cms_current_menu_record
            menu_item_id = menu_record['menu_item_id'].value()
            if module is self and action in ('view', 'subpath') \
                   and record is not None and record['menu_item_id'].value() == menu_item_id:
                return rights.permitted_roles(menu_item_id, 'visit')
            else:
                # Access rights are defined for menu items which are bound to the original module
                # before pytis redirection.
                module = self._pytis_redirect_origin(req) or module
                if module.name() == menu_record['modname'].value():
                    if action == 'subpath':
                        # Infer 'subpath' rights from 'view' rights to hide this mysterious
                        # action from the CMS user.
                        action = 'view'
                    return rights.permitted_roles(menu_item_id, action)
        return ()

    def _menu_item_identifier(self, row):
        return str(row['identifier'].value())

    def menu(self, req):
        children = {None: []}
        translations = {}
        rights = self._module('Rights')
        def item(row):
            menu_item_id = row['menu_item_id'].value()
            identifier = self._menu_item_identifier(row)
            titles, descriptions = translations[menu_item_id]
            title = lcg.SelfTranslatableText(identifier, translations=titles)
            descr = lcg.SelfTranslatableText('', translations=descriptions)
            modname = row['modname'].value()
            if modname is not None:
                try:
                    module = self._module(modname)
                except AttributeError:
                    # We want the website to work even if the module was uninstalled or renamed. 
                    submenu = []
                else:
                    submenu = list(module.submenu(req, menu_item_id))
            else:
                submenu = []
            submenu += [item(r) for r in children.get(menu_item_id, ())]
            # TODO: Optimize (avoid separate DB queries for each menu item).
            hidden = not req.check_roles(rights.permitted_roles(menu_item_id, 'show'))
            active = req.check_roles(rights.permitted_roles(menu_item_id, 'visit'))
            return wiking.MenuItem(identifier, title, descr=descr, hidden=hidden, active=active,
                                   variants=titles.keys(), submenu=submenu,)
        # First process all rows and build a dictionary of descendants for each item and
        # translations of titles and descriptions.  Then construct the menu structure.
        for row in self._menu_item_rows(req, published=True, sorting=self._sorting):
            menu_item_id = row['menu_item_id'].value()
            if not translations.has_key(menu_item_id):
                parent = row['parent'].value()
                if not children.has_key(parent):
                    children[parent] = []
                children[parent].append(row)
                translations[menu_item_id] = ({}, {})
            titles, descriptions = translations[menu_item_id]
            lang = str(row['lang'].value())
            titles[lang] = row['title_or_identifier'].value()
            if row['description'].value() is not None:
                descriptions[lang] = row['description'].value()
        return [item(row) for row in children[None]]
               # TODO: Add hidden menu items for static mapping items.
               #[MenuItem('_doc', _("Wiking Documentation"), hidden=True)]

    def _document_title(self, req, record):
        if record:
            return record['heading'].export() or record['title'].export()
        else:
            return super(Menu, self)._document_title(req, record)

    def globals(self, req):
        return dict([(name, self._module(modname).dict(req))
                     for name, modname in self._SUBSTITUTION_PROVIDERS])
    
    def action_view(self, req, record):
        text = record['content'].value()
        modname = record['modname'].value()
        if text:
            parser = lcg.Parser()
            if self._SEPARATOR.search(text):
                pre, post = [parser.parse(part) for part in self._SEPARATOR.split(text, maxsplit=2)]
            else:
                pre, post = parser.parse(text), []
        else:
            pre, post = [], []
        if modname is not None:
            binding = self._embed_binding(modname)
            result = req.forward(self._module(modname), binding=binding, record=record,
                                 title=record['title'].value())
            if isinstance(result, (int, tuple)):
                # The request has already been served by the embedded module. 
                return result
            else:
                # Embed the resulting document into the current menu item content.
                assert isinstance(result, wiking.Document)
                content = result.content()
                if isinstance(content, (tuple, list)):
                    content = list(content)
                else:
                    content = [content]
                document = result.clone(title=self._document_title(req, record), subtitle=None,
                                        content=pre+content+post, globals=self.globals(req))
        elif text is None and record['parent'].value() is None:
            # Redirect to the first subitem from empty top level items.
            rows = self._data.get_rows(parent=record['menu_item_id'].value(), published=True,
                                       sorting=self._sorting)
            # TODO: Use only items, which are visible to the current user (access rights). 
            if rows:
                return req.redirect('/'+self._menu_item_identifier(rows[0]))
            else:
                document = self._document(req, [], record, globals=self.globals(req))
        else:
            document = self._document(req, pre+post, record, globals=self.globals(req))
        return document
    
    def module_uri(self, req, modname):
        row = self._data.get_row(modname=modname)
        if row:
            return '/'+ self._menu_item_identifier(row) +'/'+ self.EMBED_BINDING_ID
        else:
            return None


class UserRoles(wiking.PytisModule):
    class Spec(Specification, cms.UserRoles):
        pass

    @classmethod
    def role(cls, role_id, system_role):
        return system_role or 'cms-role-%d' % role_id
    
    def roles(self, uid):
        """Return list of user's roles as unique string identifiers."""
        return [self.role(row['role_id'].value(), row['system_role'].value())
                for row in self._data.get_rows(uid=uid)]

    
class Rights(wiking.PytisModule):
    class Spec(Specification, cms.Rights):
        pass

    def permitted_roles(self, menu_item_id, action):
        """Return list of roles as unique string identifiers."""
        return [UserRoles.role(row['role_id'].value(), row['system_role'].value())
                for row in self._data.get_rows(menu_item_id=menu_item_id,
                                               action_name=action, permitted=True)]


class Users(wiking.PytisModule):
    class Spec(Specification, cms.Users):
        pass

    def _user_row(self, login):
        """Override this method to customize the user search condition."""
        return self._data.get_row(login=login)
                    
    def _user_args(self, row):
        """Override this method to customize the 'User' instance constructor arguments."""
        uid = row['uid'].value()
        roles = [wiking.Roles.USER] + self._module('UserRoles').roles(uid)
        return dict(uid=uid,
                    name=row['fullname'].value(),
                    password=row['passwd'].value(),
                    roles=roles, data=row)

    def user(self, login):
        row = self._user_row(login)
        if row:
            return wiking.User(login, **self._user_args(row))
        else:
            return None
    

class Session(wiking.PytisModule, wiking.Session):
    """Implement Wiking session management by storing session information in the database."""
    class Spec(Specification):
        table = 'cms_session'
        fields = [pp.Field(_id) for _id in ('session_id', 'uid', 'session_key', 'last_access')]

    def init(self, req, user):
        data = self._data
        # Delete all expired records first...
        now = mx.DateTime.now().gmtime()
        expiration = mx.DateTime.TimeDelta(hours=wiking.cfg.session_expiration)
        data.delete_many(pd.LE('last_access', pd.Value(pd.DateTime(), now - expiration)))
        # Create new data row for this session.
        session_key = self._new_session_key()
        row, success = data.insert(data.make_row(uid=user.uid(),
                                                 session_key=session_key,
                                                 last_access=now))
        # Log session start for login history tracking.
        self._module('SessionLog').log(req, now, row['session_id'].value(),
                                       user.uid(), user.login())
        return session_key
        
    def failure(self, req, user, login):
        self._module('SessionLog').log(req, mx.DateTime.now().gmtime(), None,
                                       user and user.uid(), login)
        
    def check(self, req, user, session_key):
        row = self._data.get_row(uid=user.uid(), session_key=session_key)
        if row:
            now = mx.DateTime.now().gmtime()
            expiration = mx.DateTime.TimeDelta(hours=wiking.cfg.session_expiration)
            if row['last_access'].value() > now - expiration:
                self._data.update((row['session_id'],), self._data.make_row(last_access=now))
                return True
        return False

    def close(self, req, user, session_key):
        self._data.delete_many(pd.AND(pd.EQ('uid', pd.Value(pd.Integer(), user.uid())),
                                      pd.EQ('session_key', pd.Value(pd.DateTime(), session_key))))
            

class SessionLog(wiking.PytisModule):
    class Spec(wiking.Specification):
        table = 'cms_session_log_data'
        fields = [pp.Field(_id) for _id in
                  ('log_id', 'session_id', 'uid', 'login', 'success', 'start_time',
                   'ip_address', 'user_agent', 'referer')]

    def log(self, req, time, session_id, uid, login):
        row = self._data.make_row(session_id=session_id, uid=uid, login=login,
                                  success=session_id is not None, start_time=time,
                                  ip_address=req.header('X-Forwarded-For') or req.remote_host(),
                                  referer=req.header('Referer'),
                                  user_agent=req.header('User-Agent'))
        self._data.insert(row)
            
    
class Application(wiking.CookieAuthentication, wiking.Application):
    
    _MAPPING = dict(wiking.Application._MAPPING,
                    _resources='Resources')
    
    _RIGHTS = {'Documentation': (wiking.Roles.ANYONE,),
               'Stylesheets': (wiking.Roles.ANYONE,),
               'Resources': (wiking.Roles.ANYONE,)}
    
    def _auth_user(self, req, login):
        return self._module('Users').user(login)
        
    def _auth_check_password(self, user, password):
        user_password = user.password()
        if user_password is None:
            return False
        else:
            user_password = user_password.strip()
        if user_password.startswith('{'):
            # We have a LDAP style password
            s = re.match('{(.*)}(.*)', user_password)
            if s and len(s.groups()) == 2:
                hash_alg = s.group(1)
                hash_value = s.group(2)
                encode = 'base64'
            else:
                return False
        else:
            # We suppose to have a hexencoded md5 style password
            hash_alg = 'MD5'
            hash_value = user_password
            encode = 'hex'
        h = hashlib.new(hash_alg)
        h.update(password)
        if encode == "base64":
            encoded = binascii.b2a_base64(h.digest()).strip()
        else:
            encoded = binascii.b2a_hex(h.digest()).strip()
        return hash_value == encoded
    
    def authorize(self, req, module, action=None, record=None, **kwargs):
        try:
            roles = self._RIGHTS[module.name()]
        except KeyError:
            roles = self._module('Menu').permitted_roles(req, module, action=action,
                                                         record=record, **kwargs)
        else:
            if action is not None and isinstance(roles, dict):
                roles = roles.get(action, ())
        if req.check_roles(roles):
            return True
        elif wiking.Roles.OWNER in roles and record is not None and req.user() is not None \
                 and isinstance(module, wiking.PytisModule):
            return module.check_owner(req.user(), record)
        else:
            return False
    
    def menu(self, req):
        return self._module('Menu').menu(req)

    def languages(self):
        return self._module('Languages').languages()

    def module_uri(self, req, modname):
        uri = super(Application, self).module_uri(req, modname)
        if uri is None:
            uri = self._module('Menu').module_uri(req, modname)
        return uri
    
    def handle(self, req):
        if req.unresolved_path:
            try:
                modname = self._MAPPING[req.unresolved_path[0]]
            except KeyError:
                modname = 'Menu'
            else:
                # Consume the unresolved path if it was in static mapping or leave it for further
                # resolution when passing to Menu.
                del req.unresolved_path[0]
            return req.forward(self._module(modname))
        else:
            return super(Application, self).handle(req)
    
    def panels(self, req, lang):
        return [wiking.LoginPanel()]
        
    
class Embeddable(object):
    """Mix-in class for modules which may be embedded into page content."""
    
    def submenu(self, req, menu_item_id):
        """Return a list of 'MenuItem' instances to insert into the main menu.
        
        The submenu will appear in the main menu under the item of a page which embeds the module.
        The items returned by this method will always be placed above any items defined by the menu
        structure.

        """
        return []
    

class EmbeddablePytisModule(wiking.PytisModule, Embeddable):
    _USE_BINDING_PARENT_TITLE = False
    _BROWSE_FORM_LIMITS = (50, 100, 200, 300, 500)
    
    def _current_base_uri(self, req, record=None):
        uri = super(EmbeddablePytisModule, self)._current_base_uri(req, record=record)
        if len(uri.lstrip('/').split('/')) == req.menu_path_length:
            uri += '/'+ Menu.EMBED_BINDING_ID
        return uri

    def _binding_parent_uri(self, req):
        fw = self._binding_forward(req)
        if fw:
            path = fw.uri().lstrip('/').split('/')
            if len(path) == req.menu_path_length+2 and path[-1] == fw.arg('binding').id():
                return '/'+ '/'.join(path[:req.menu_path_length])
        return super(EmbeddablePytisModule, self)._binding_parent_uri(req)

    def _document(self, req, content, record=None, **kwargs):
        globals = self._module('Menu').globals(req)
        if kwargs.has_key('globals') and kwargs['globals'] is not None:
            kwargs['globals'].update(globals)
        else:
            kwargs['globals'] = globals
        return super(EmbeddablePytisModule, self)._document(req, content, record=record, **kwargs)


class SubstitutionProvider(wiking.PytisModule):
    """Base class for modules providing variable substitution.

    Variables provided by modules derived from this class may be used for variable substitution in
    CMS texts.  The constant `Menu._SUBSTITUTION_PROVIDERS' must be used to define which concrete
    substitution modules (derived from this class) are available.
    
    Derived modules just need to define a pytis specification (class 'Spec').  The fields included
    in this specification will be used as substitution variables.  More precisely the field
    identifiers may by used as dictionary keys, where the dictionary name is given by
    `Menu._SUBSTITUTION_PROVIDERS' mapping (see the docstring of this constant for more info).

    The dictionary values are taken from this module's data row.  The first row is used by default.
    If you want to select the used row based on some more specific condition, you may override the
    method 'dict()' and pass selection arguments to 'SubstitutionDict' constructor.  These
    arguments are passed to the 'self._data.get_rows()' call.  If more rows are returned, the first
    one is used in any case.  Here are a few examples:

      def dict(self, req):
          # Always use the row with the value of column 'id' equal to 1.
          return self.SubstitutionDict(self._data, id=1)

      def dict(self, req):
          # Use the row specific for the current user.
          return self.SubstitutionDict(self._data, uid=req.user().uid())

      def dict(self, req):
          # Use the first row matching a pytis condition.
          return self.SubstitutionDict(self._data,
                                       condition=pd.AND(pd.EQ('xx', ...),
                                                        pd.GE('yy', ...)),
                                       sorting=(('xy', pd.ASC),))

    """
    class SubstitutionDict(object):
        def __init__(self, data, **kwargs):
            self._data = data
            self._kwargs = kwargs
            self._row = None
        def __getitem__(self, key):
            if self._row is None:
                # Perform the database access only when needed.
                rows = self._data.get_rows(limit=1, **self._kwargs)
                if rows:
                    self._row = rows[0]
                else:
                    self._row = pd.Row(())
            return self._row[key].export()
        
    def dict(self, req):
        return self.SubstitutionDict(self._data)


class Themes(wiking.PytisModule):
    class Spec(Specification, cms.Themes):
        pass
    
    class Theme(wiking.Theme):
        def __init__(self, row):
            self._theme_id = row['theme_id'].value()
            colors = [(c.id(), row[c.id()].value())
                      for c in self.COLORS if row[c.id()].value() is not None]
            super(Themes.Theme, self).__init__(colors=dict(colors))
        def theme_id(self):
            return self._theme_id
        
    def theme(self, theme_id):
        return self.Theme(self._data.get_row(theme_id=theme_id))

