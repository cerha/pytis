# -*- coding: utf-8 -*-

# Copyright (c) 2019-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2006-2017 OUI Technology Ltd.
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

Pytis CMS is a fat client application for management of database structures,
which define the behavior of a web application.  Wiking modules defined here
implement a Wiking application, which reads these database structures and
behaves according to them.

Example application using these Wiking modules can be found in pytis-demo.

The applications built on top of Pytis CMS may be extended by so called
embedded modules -- dynamic content embeddable into CMS pages.  Embedded
modules are Wiking modules derived from `EmbeddablePytisModule' (for modules
bound to pytis data object) or `EmbeddableModule' (for non database modules).
See the documentation of these classes for more information.

The class 'Application' implements the necessary methods defined by
`wiking.Application' API (constructing the application menu out of the database
defined menu structure, authentication, access rights checking etc.  However
this class may be further customized for a particular application.  Full API
defined and documented by `wiking.Application' may be used.

"""

from __future__ import print_function

import datetime
import os
import re
import lcg
import wiking
import hashlib
import binascii
import pytis.util
import pytis.presentation as pp
import pytis.data as pd
from . import cms

unistr = type(u'')  # Python 2/3 transition hack.


class Specification(wiking.Specification):
    access_rights = None

    def _name(self, name, needed_in_wiking=True):
        if needed_in_wiking:
            return name
        else:
            return None


class RestrictedPytisModule(wiking.PytisModule):
    """wiking.PytisModule which passes authorization requests to Application.authorized_roles()."""

    def _authorized(self, req, action, record=None, **kwargs):
        roles = wiking.module.Application.authorized_roles(req, self, action=action, record=record)
        if req.check_roles(roles):
            return True
        else:
            return False


class Languages(RestrictedPytisModule):

    class Spec(Specification, cms.Languages):
        pass

    def languages(self):
        return [str(r['lang'].value()) for r in self._data.get_rows()]


class Menu(RestrictedPytisModule):

    class Spec(Specification, cms.Menu):
        pass

    _SEPARATOR = re.compile(r'^====+\s*$', re.MULTILINE)
    _SUBSTITUTION_PROVIDERS = ()
    """Sequence of names of modules providing substitution variables.

    Each string in the sequence must be a name of a module derived from
    'SubstitutionProvider' (see its documentation for information about
    variable substitution).

    """
    EMBED_BINDING_ID = 'data'
    ITEM_PATH_LENGTH = 1

    def _resolve(self, req):
        kwargs = self._resolve_menu_args(req)
        rows = self._data.get_rows(published=True, **kwargs)
        if not rows:
            if self._data.get_rows(**kwargs):
                raise wiking.Forbidden()
            else:
                raise wiking.NotFound()
        variants = [str(row['lang'].value()) for row in rows]
        lang = req.preferred_language(variants)
        row = rows[variants.index(lang)]
        del req.unresolved_path[:self.ITEM_PATH_LENGTH]
        return row

    def _resolve_menu_args(self, req):
        """Return arguments identifying the current menu item rows.

        Returns arguments for self._data.get_rows() which limit returned rows to match the current
        menu item (multiple rows only represent language variants of the item).

        """
        return dict(identifier=req.unresolved_path[0])

    def _menu_item_rows(self, req, **kwargs):
        return self._data.get_rows(**kwargs)

    def _embed_binding(self, modname):
        return pp.Binding(self.EMBED_BINDING_ID, '', modname, condition=lambda r: None)

    def _bindings(self, req, record):
        bindings = list(super(Menu, self)._bindings(req, record))
        modname = record['modname'].value()
        if modname:
            bindings.append(self._embed_binding(modname))
        return bindings

    def _action(self, req, record=None):
        # The only supported action of this module is `view' and the `action' argument is ignored
        # here (left for the embedded module action resolution).
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
            module = forwards[i - 1].module()
            i -= 1
        return module

    def permitted_roles(self, req, module, action=None, record=None, **kwargs):
        # wiking.debug("...", module.name(), action, hasattr(req, 'cms_current_menu_record'))
        if hasattr(req, 'cms_current_menu_record'):
            rights = wiking.module.Rights
            menu_record = req.cms_current_menu_record
            menu_item_id = menu_record['menu_item_id'].value()
            if module is self and action == 'view' and record is not None \
                    and record['menu_item_id'].value() == menu_item_id:
                return rights.permitted_roles(menu_item_id, 'visit')
            else:
                # Access rights are defined for menu items which are bound to the original module
                # before pytis redirection.
                module = self._pytis_redirect_origin(req) or module
                if module.name() == menu_record['modname'].value():
                    return rights.permitted_roles(menu_item_id, action)
        return ()

    def _attachment_storage(self, record):
        base_uri = '/' + self._menu_item_identifier(record) + '/attachments/'
        # We can't use record.attachment_storage() as we need to pass custom arguments.
        return self._view.field('content').attachment_storage()(record, base_uri)

    def _menu_item_identifier(self, row):
        return str(row['identifier'].value())

    def _handle_subpath(self, req, record):
        if req.unresolved_path[0] == 'attachments':
            storage = self._attachment_storage(record)
            if storage:
                resource = storage.find_resource_by_uri(req.uri())
                if resource and resource.src_file():
                    return wiking.serve_file(req, resource.src_file())
            raise wiking.NotFound()
        return super(Menu, self)._handle_subpath(req, record)

    def menu(self, req):
        children = {None: []}
        translations = {}
        rights = wiking.module.Rights

        def item(row):
            menu_item_id = row['menu_item_id'].value()
            identifier = self._menu_item_identifier(row)
            titles, descriptions = translations[menu_item_id]
            title = lcg.SelfTranslatableText(identifier, translations=titles)
            descr = lcg.SelfTranslatableText('', translations=descriptions)
            modname = row['modname'].value()
            if modname is not None:
                try:
                    module = wiking.module(modname)
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
                                   variants=tuple(titles.keys()), submenu=submenu,)
        # First process all rows and build a dictionary of descendants for each item and
        # translations of titles and descriptions.  Then construct the menu structure.
        for row in self._menu_item_rows(req, published=True, sorting=self._sorting):
            menu_item_id = row['menu_item_id'].value()
            if menu_item_id not in translations:
                parent = row['parent'].value()
                if parent not in children:
                    children[parent] = []
                children[parent].append(row)
                translations[menu_item_id] = ({}, {})
            titles, descriptions = translations[menu_item_id]
            lang = str(row['lang'].value())
            titles[lang] = row['title_or_identifier'].value()
            if row['description'].value() is not None:
                descriptions[lang] = row['description'].value()
        # TODO: Add hidden menu items for static mapping items.
        # [MenuItem('_doc', _(u"Wiking Documentation"), hidden=True)]
        return [item(row) for row in children[None]]

    def _document_title(self, req, record):
        if record:
            return record['heading'].export() or record['title'].export()
        else:
            return super(Menu, self)._document_title(req, record)

    def globals(self, req):
        globals = {}
        for modname in self._SUBSTITUTION_PROVIDERS:
            module = wiking.module(modname)
            globals.update(module.variables(req))
        return globals

    def action_view(self, req, record):
        text = record['content'].value()
        modname = record['modname'].value()
        globals = self.globals(req)
        storage = self._attachment_storage(record)
        if storage:
            resources = storage.resources()
        else:
            resources = ()
        if text:
            text = lcg.MacroParser(globals=globals).parse(text)
            parser = lcg.Parser()
            if self._SEPARATOR.search(text):
                pre, post = [parser.parse(part)
                             for part in self._SEPARATOR.split(text, maxsplit=2)]
            else:
                pre, post = parser.parse(text), []
        else:
            pre, post = [], []
        if modname is not None:
            binding = self._embed_binding(modname)
            result = req.forward(wiking.module(modname), binding=binding, record=record,
                                 title=record['title'].value())
            if isinstance(result, wiking.Document):
                # Embed the resulting document into the current menu item content.
                content = result.content()
                if isinstance(content, (tuple, list)):
                    content = list(content)
                else:
                    content = [content]
                document = result.clone(title=self._document_title(req, record), subtitle=None,
                                        content=lcg.Container(pre + content + post,
                                                              resources=resources),
                                        globals=globals)
            else:
                # A wiking.Response instance has been returned by the embedded module.
                return result
        elif text is None and record['parent'].value() is None:
            # Redirect to the first subitem from empty top level items.
            rows = self._data.get_rows(parent=record['menu_item_id'].value(), published=True,
                                       sorting=self._sorting)
            # TODO: Use only items, which are visible to the current user (access rights).
            if rows:
                raise wiking.Redirect('/' + self._menu_item_identifier(rows[0]))
            else:
                document = self._document(req, lcg.Container([], resources=resources),
                                          record, globals=globals)
        else:
            document = self._document(req, lcg.Container(pre + post, resources=resources),
                                      record, globals=globals)
        if modname is None:
            # Module access is logged in EmbeddablePytisModule._handle().
            wiking.module.AccessLog.log(req, None, None)
        return document

    def module_uri(self, req, modname):
        row = self._data.get_row(modname=modname)
        if row:
            return '/' + self._menu_item_identifier(row) + '/' + self.EMBED_BINDING_ID
        else:
            return None


class UserRoles(RestrictedPytisModule):

    class Spec(Specification, cms.UserRoles):
        pass

    @classmethod
    def role(cls, numeric_role_id, system_role, name):
        if system_role:
            role_id = system_role
        else:
            role_id = 'cms-role-%d' % numeric_role_id
        return wiking.Role(role_id, name)

    def roles(self, uid):
        """Return list of user's roles as 'wiking.Role' instances."""
        return [self.role(row['role_id'].value(), row['system_role'].value(),
                          row['name'].value())
                for row in self._data.get_rows(uid=uid)]


class Rights(RestrictedPytisModule):

    class Spec(Specification, cms.Rights):
        pass

    def permitted_roles(self, menu_item_id, action):
        """Return list of roles as unique string identifiers."""
        return [UserRoles.role(row['role_id'].value(), row['system_role'].value(),
                               row['role_name'].value())
                for row in self._data.get_rows(menu_item_id=menu_item_id, action_name=action)]


class Users(RestrictedPytisModule):

    class Spec(Specification, cms.Users):
        pass

    def _user_row(self, req, **kwargs):
        """Override this method to customize the user search condition."""
        return self._data.get_row(**kwargs)

    def _user_args(self, req, row):
        """Override this method to customize the 'User' instance constructor arguments."""
        uid = row['uid'].value()
        return dict(uid=uid,
                    login=row['login'].value(),
                    name=row['fullname'].value(),
                    password=row['passwd'].value(),
                    roles=[wiking.Roles.AUTHENTICATED] + wiking.module.UserRoles.roles(uid),
                    data=row)

    def user(self, req, login=None, uid=None):
        if login:
            row = self._user_row(req, login=login)
        elif uid:
            row = self._user_row(req, uid=uid)
        else:
            raise ProgramError("Invalid arguments")
        if row:
            return wiking.User(**self._user_args(req, row))
        else:
            return None


class Session(RestrictedPytisModule, wiking.Session):
    """Implement Wiking session management by storing session information in the database."""
    class Spec(Specification):
        table = 'cms_session'
        fields = [pp.Field(_id) for _id in ('session_id', 'uid', 'session_key', 'last_access')]

    def _split_key(self, key):
        try:
            session_id, session_key = key.split(':')
            return int(session_id), session_key
        except (AttributeError, ValueError):
            return None, None

    def _is_expired(self, row):
        expiration = datetime.timedelta(hours=wiking.cfg.session_expiration)
        return row['last_access'].value() < pd.DateTime.datetime(False) - expiration

    def _update_last_access(self, row):
        self._data.update((row['session_id'],),
                          self._data.make_row(last_access=pd.DateTime.datetime(False)))

    def _new_session(self, uid, session_key):
        data = self._data
        expiration = datetime.timedelta(hours=wiking.cfg.session_expiration)
        # Delete all expired records first (can't do in trigger due to the configuration option).
        data.delete_many(pd.LE('last_access',
                               pd.Value(pd.DateTime(),
                                        pd.DateTime.datetime(False) - expiration)))
        return data.insert(data.make_row(
            session_key=session_key,
            uid=uid,
            last_access=pd.DateTime.datetime(False))
        )[0]

    def init(self, req, user, auth_type, reuse=False):
        if reuse:
            row = self._data.get_row(uid=user.uid())
            if row and not self._is_expired(row):
                self._update_last_access(row)
                return None
        row = self._new_session(user.uid(), wiking.generate_random_string(64))
        return row['session_id'].export() + ':' + row['session_key'].value()

    def check(self, req, session_key):
        session_id, session_key = self._split_key(session_key)
        row = self._data.row(pd.ival(session_id))
        if row and row['session_key'].value() == session_key and not self._is_expired(row):
            self._update_last_access(row)
            return wiking.module.Users.user(req, uid=row['uid'].value())
        else:
            return None

    def close(self, req, session_key):
        session_id, session_key = self._split_key(session_key)
        # We don't verify session_key here, because we know that
        # 'CookieAuthenticationProvider.authenticate()' only calls
        # 'close()' after 'check()' or 'init()', but this is not
        # required by definition.  Thus it should be either
        # documented somewhere or not relied upon.
        self._data.delete(pd.ival(session_id))


class SessionLog(RestrictedPytisModule):

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


class AccessLog(RestrictedPytisModule):

    class Spec(wiking.Specification):
        table = 'cms_access_log_data'
        fields = [pp.Field(_id) for _id in
                  ('log_id', 'timestamp', 'uri', 'uid', 'modname', 'action',
                   'ip_address', 'user_agent', 'referer')]

    def log(self, req, modname, action):
        row = self._data.make_row(timestamp=pd.DateTime.datetime(),
                                  uri=req.uri(),
                                  uid=req.user() and req.user().uid(),
                                  modname=modname,
                                  action=action,
                                  ip_address=req.header('X-Forwarded-For') or req.remote_host(),
                                  referer=req.header('Referer'),
                                  user_agent=req.header('User-Agent'))
        self._data.insert(row)


class Application(wiking.Application):

    _MAPPING = dict(wiking.Application._MAPPING,
                    _attachments='HttpAttachmentStorageBackend')

    _RIGHTS = {'Documentation': (wiking.Roles.ANYONE,),
               'Resources': (wiking.Roles.ANYONE,),
               'SiteIcon': (wiking.Roles.ANYONE,),
               'HttpAttachmentStorageBackend': (wiking.Roles.ANYONE,)}


    def authenticate(self, req, login, password, auth_type):
        user = wiking.module.Users.user(req, login)
        if user:
            stored_password = user.password()
            if stored_password:
                stored_password = stored_password.strip()
                match = re.match('{(.*)}(.*)', stored_password)
                if match and len(match.groups()) == 2:
                    # We have a LDAP style password
                    hash_alg, hash_value = match.groups()
                    encode = binascii.b2a_base64
                else:
                    # We suppose to have a hexencoded md5 style password
                    hash_alg, hash_value = 'MD5', stored_password
                    encode = binascii.b2a_hex
                h = hashlib.new(hash_alg)
                # Note: h.update() accepts bytes in Python 3, but str and unicode in Python 2.
                h.update(password.encode('utf-8'))
                encoded = encode(h.digest()).strip()
                if hash_value.encode('ascii') == encoded:
                    return user
        return None

    def authorized_roles(self, req, module, action=None, record=None):
        try:
            roles = self._RIGHTS[module.name()]
        except KeyError:
            roles = wiking.module.Menu.permitted_roles(req, module, action=action, record=record)
        else:
            if action is not None and isinstance(roles, dict):
                roles = roles.get(action, ())
        return roles

    def menu(self, req):
        return wiking.module.Menu.menu(req)

    def languages(self):
        return wiking.module.Languages.languages()

    def module_uri(self, req, modname):
        uri = super(Application, self).module_uri(req, modname)
        if uri is None:
            uri = wiking.module.Menu.module_uri(req, modname)
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
            return req.forward(wiking.module(modname))
        else:
            return super(Application, self).handle(req)

    def panels(self, req, lang):
        return [wiking.LoginPanel()]


class EmbeddableModule(wiking.Module, wiking.ActionHandler):
    """Base class for modules which may be embedded into page content.

    Most CMS modules will be derived from 'EmbeddablePytisModule', but this module allows creation
    of modules not bound to database.  As this module is derived from 'wiking.ActionHandler' with
    action 'view' as the default action, so you usually just define the method 'action_view()' to
    display the content.

    """

    def submenu(self, req, menu_item_id):
        """Return a list of 'MenuItem' instances to insert into the main menu.

        The submenu will appear in the main menu under the item of a page which embeds the module.
        The items returned by this method will always be placed above any items defined by the menu
        structure.

        """
        return []

    def _authorized(self, req, action=None):
        return req.check_roles(wiking.module.Application.authorized_roles(req, self, action=action))

    def _default_action(self, req):
        return 'view'

    def _handle(self, req, action, **kwargs):
        result = super(EmbeddableModule, self)._handle(req, action, **kwargs)
        if isinstance(result, wiking.Document):
            # Log only displayed pages, not objects displayed on them, such as images.  If logging
            # of downloaded files is desired, the module will need to do it explicitly.
            wiking.module.AccessLog.log(req, self.name(), action)
        return result


class EmbeddablePytisModule(RestrictedPytisModule, wiking.PytisRssModule, EmbeddableModule):
    """Base class for pytis modules which may be embedded into page content.

    This class only modifies the generic class `RestrictedPytisModule' to be usable within Pytis CMS
    pages.  Documentation of the parent class should be used for Pytis CMS module development.  It
    applies completely except for the following specific customizations:

      * The method `_document()' automatically adds global variables defined through
        `Menu._SUBSTITUTION_PROVIDERS'.  The variables may be used within all content.

      * The methods `_current_base_uri()' and `_binding_parent_uri()' are modified to respect the
        specific way of embedding modules into CMS pages.

      * The constant `_USE_BINDING_PARENT_TITLE' is set to False by default.

      * The constant `_BROWSE_FORM_LIMITS' is set to (50, 100, 200, 300, 500) by default.

    """
    _USE_BINDING_PARENT_TITLE = False
    _BROWSE_FORM_LIMITS = (50, 100, 200, 300, 500)

    def _current_base_uri(self, req, record=None):
        menu = wiking.module.Menu
        uri = super(EmbeddablePytisModule, self)._current_base_uri(req, record=record)
        if len(uri.lstrip('/').split('/')) == menu.ITEM_PATH_LENGTH:
            uri += '/' + menu.EMBED_BINDING_ID
        return uri

    def _binding_parent_uri(self, req):
        fw = self._binding_forward(req)
        if fw:
            path = fw.uri().lstrip('/').split('/')
            path_length = wiking.module.Menu.ITEM_PATH_LENGTH
            if len(path) == path_length + 2 and path[-1] == fw.arg('binding').id():
                return '/' + '/'.join(path[:path_length])
        return super(EmbeddablePytisModule, self)._binding_parent_uri(req)

    def _document(self, req, content, record=None, **kwargs):
        globals = wiking.module.Menu.globals(req)
        if 'globals' in kwargs and kwargs['globals'] is not None:
            kwargs['globals'].update(globals)
        else:
            kwargs['globals'] = globals
        return super(EmbeddablePytisModule, self)._document(req, content, record=record, **kwargs)


class SubstitutionProvider(wiking.Module):
    """Base class for modules providing variable substitution.

    Variables provided by modules derived from this class may be used for
    variable substitution in CMS texts.  The constant
    `Menu._SUBSTITUTION_PROVIDERS' must be used to define which concrete
    substitution modules (derived from this class) are available.

    Derived modules need to implement the method 'variables()' returning the
    dictionary of substitution values defined by the module.

    Example:

    If the method 'variables()' returns the dictionary:
       {'foo': 'bar', 'count': 5, 'cfg': {'min': 3, 'max': 8}}
    the CMS page text:
       The $foo has $count items ranging from $cfg.min to $cfg.max.
    will be substituted by:
       The bar has 5 items ranging from 3 to 8.

    """

    def variables(self, req):
        """Return the substitution values defined by this module as a dictionary."""
        return {}


class DataSubstitutionProvider(RestrictedPytisModule, SubstitutionProvider):
    """Base class for modules providing variable substitution from a pytis data object.

    This class makes it possible to simply define substitution variables
    obtaining their substitution values from a pytis data object.

    Derived modules need to define a pytis specification (class 'Spec') and the
    method 'variables()' which defines concrete variable names and values.  The
    values may use the predefined helper classes, such as 'SingleRowDict' (see
    its documentation for more information).

    """
    class SingleRowDict(object):
        """Dictionary-like substitution variable taking its values from one data row.

        The dictionary keys are field identifiers and their substitution values
        are the exported field values.  The concrete data row used for
        substitution may be specified by constructor arguments (see below).
        The database lookup is only performed when needed.

        If you want to select the row based on some specific condition, you may
        pass selection arguments to 'SingleRowDict' constructor.  These
        arguments are actually passed to 'self._data.get_rows()' when the first
        dictionary value is needed and the returned row is stored for further
        substitutions.  If more rows are returned, the first one is used in any
        case.  Here are a few examples:

        Usage examples:

          def variables(self, req):
              # Return the first row from the data object.
              return {'cfg': self.SingleRowDict(self._data)}

          def variables(self, req):
              # Always use the row with the value of column 'id' equal to 1.
              return {'foo': self.SingleRowDict(self._data, id=1)}

          def variables(self, req):
              # Use the row specific for the current user.
              return {'user': self.SingleRowDict(self._data, uid=req.user().uid())}

          def variables(self, req):
              # Use the first row matching a pytis condition.
              return {'foo': self.SingleRowDict(self._data,
                                                condition=pd.AND(pd.EQ('xx', ...),
                                                                 pd.GE('yy', ...)),
                                                sorting=(('xy', pd.ASC),))}

        """

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

        def get(self, key, default=None):
            try:
                return self[key]
            except KeyError:
                return default


class Themes(RestrictedPytisModule):

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


class HttpAttachmentStorageBackend(wiking.Module, wiking.RequestHandler):
    """Implements the server side of 'pp.HttpAttachmentStorage'.

    The environment variable 'PYTIS_CMS_ATTACHMENTS_STORAGE' must be set to a
    filesystem path where attachments are stored on the server side.  The
    'pytis.presentation.FileAttachmentStorage' backend is actually used by the
    server side to store attachments.  The HTTP storage works as a HTTP proxy
    between the application and the server side file storage backend.  The http
    address of the server side storage must be set in the environment variable
    'PYTIS_CMS_ATTACHMENTS_STORAGE' on the client side.

    Override the method '_authorize()' to control authorization per directory.

    """

    def __init__(self, *args, **kwargs):
        super(HttpAttachmentStorageBackend, self).__init__(*args, **kwargs)
        self._data = pd.dbtable('e_pytis_http_attachment_storage_keys',
                                ('key_id', 'username', 'uri', 'readonly', 'key'))

    def _handle(self, req):
        directory = os.environ.get('PYTIS_CMS_ATTACHMENTS_STORAGE')
        if not directory or not req.unresolved_path:
            raise wiking.Forbidden
        assert not (directory.startswith('http://') or directory.startswith('https://'))
        identifier = req.unresolved_path[0]
        del req.unresolved_path[0]
        storage = pp.FileAttachmentStorage(os.path.join(directory, identifier),
                                           self._base_uri(req) + '/' + identifier)
        action = req.param('action')
        if action is None:
            if req.unresolved_path:
                resource = storage.find_resource_by_uri(req.uri())
                if resource:
                    return wiking.serve_file(req, resource.src_file())
                else:
                    raise wiking.NotFound
            else:
                data = req.param('data')
                if data:
                    if req.param('authorized_readonly') is not False:
                        raise wiking.AuthorizationError()
                    return self._insert(req, storage, data.filename(), data.file())
                else:
                    return self._list(req, storage)
        else:
            filename = req.unresolved_path[0]
            del req.unresolved_path[0]
            if req.unresolved_path:
                raise wiking.BadRequest()
            if action == 'retrieve':
                return self._retrieve(req, storage, filename)
            elif action == 'info':
                return self._info(req, storage, filename)
            elif action == 'update':
                if req.param('authorized_readonly') is not False:
                    raise wiking.AuthorizationError()
                return self._update(req, storage, filename)
            else:
                raise wiking.BadRequest()

    def _authorize(self, req):
        uri = req.server_uri(current=True) + '/' + '/'.join(req.path[:2])
        username = req.header('X-Pytis-Attachment-Storage-Username')
        key = req.header('X-Pytis-Attachment-Storage-Key')
        if username and key:
            data = self._data
            data.select(condition=pd.AND(pd.EQ('uri', pd.sval(uri)),
                                         pd.EQ('username', pd.sval(username))))
            row = data.fetchone()
            data.close()
            if row and row['key'].value() == key:
                req.set_param('authorized_readonly', row['readonly'].value())
                return
        raise wiking.AuthorizationError()

    def _resource_info(self, resource):
        info = resource.info() or {}
        if resource.title():
            info['title'] = resource.title()
        if resource.descr():
            info['descr'] = resource.descr()
        if isinstance(resource, lcg.Image):
            thumbnail = resource.thumbnail()
            if thumbnail:
                info['has_thumbnail'] = True
                info['thumbnail_size'] = thumbnail.size()
            info['size'] = resource.size()
        return info

    def _list(self, req, storage):
        import json
        result = [[r.filename(), self._resource_info(r)] for r in storage.resources()]
        return wiking.Response(json.dumps(result), content_type='application/json')

    def _info(self, req, storage, filename):
        resource = storage.resource(filename)
        if resource:
            import json
            info = self._resource_info(resource)
            return wiking.Response(json.dumps(info), content_type='application/json')
        else:
            raise wiking.NotFound

    def _retrieve(self, req, storage, filename):
        f = storage.retrieve(filename)
        if f:
            def proxy(f):
                try:
                    while True:
                        data = f.read(524288)
                        if not data:
                            break
                        yield data
                finally:
                    f.close()
            return wiking.Response(proxy(f), content_type='application/octet-stream')
        else:
            raise wiking.NotFound

    def _insert(self, req, storage, filename, data):
        import json
        values = json.loads(req.param('values'))
        try:
            storage.insert(filename, data, values)
        except Exception as e:
            response = str(e)
        else:
            response = 'OK'
        return wiking.Response(response, content_type='text/plain')

    def _update(self, req, storage, filename):
        import json
        values = json.loads(req.param('values'))
        try:
            storage.update(filename, values)
        except Exception as e:
            response = str(e)
        else:
            response = 'OK'
        return wiking.Response(response, content_type='text/plain')
