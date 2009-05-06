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

import os, re, md5, lcg, wiking, pytis.util, pytis.presentation as pp, pytis.data as pd
import cms

class Specification(wiking.Specification):
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
    
    _SEPARATOR = re.compile('^====+\s*$', re.MULTILINE)

    def _resolve(self, req):
        identifier = req.unresolved_path[0]
        rows = self._menu_item_rows(req, identifier=identifier, published=True)
        if not rows:
            if self._menu_item_rows(req, identifier=identifier):
                raise wiking.Forbidden()
            else:
                raise wiking.NotFound()
        variants = [str(row['lang'].value()) for row in rows]
        lang = req.prefered_language(variants)
        row = rows[variants.index(lang)]
        del req.unresolved_path[0]
        return row

    def _bindings(self, req, record):
        bindings = super(Menu, self)._bindings(req, record)
        modname = record['modname'].value()
        if modname:
            cls = wiking.cfg.resolver.wiking_module_cls(modname)
            if cls and issubclass(cls, EmbeddablePytisModule):
                bindings.append(cls.binding())
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

    def _menu_item_rows(self, req, **kwargs):
        return self._data.get_rows(**kwargs)
    
    def authorize(self, req, module, action=None, record=None, **kwargs):
        #wiking.debug("...", module.name(), action, hasattr(req, 'cms_current_menu_record'))
        if hasattr(req, 'cms_current_menu_record'):
            rights = self._module('Rights')
            menu_record = req.cms_current_menu_record
            menu_item_id = menu_record['menu_item_id'].value()
            if module is self and action in ('view', 'subpath') \
                   and record is not None and record['menu_item_id'].value() == menu_item_id:
                roles = rights.permitted_roles(menu_item_id, 'visit')
            else:
                # Find the original module if the request was forwarded due to pytis redirection
                # (access rights are defined for menu items which are bound to the original module).
                forwards = req.forwards()
                i = len(forwards) - 1
                while i > 0 and forwards[i].arg('pytis_redirect'):
                    module = forwards[i-1].module()
                    i -= 1
                if module.name() == menu_record['modname'].value():
                    roles = rights.permitted_roles(menu_item_id, action)
                else:
                    roles = ()
            #wiking.debug(">>>", module.name(), menu_record['modname'].value(), action,
            #             menu_item_id, roles, req.check_roles(roles))
            return req.check_roles(roles)
        else:
            return False
    
    def menu(self, req):
        children = {None: []}
        translations = {}
        rights = self._module('Rights')
        def item(row):
            menu_item_id = row['menu_item_id'].value()
            identifier = str(row['identifier'].value())
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
    
    def action_view(self, req, record, err=None, msg=None):
        # Main content
        modname = record['modname'].value()
        if modname is not None:
            module = self._module(modname)
            content = module.embed(req, record)
            if isinstance(content, (int, tuple)):
                # The request has already been served by the embedded module. 
                return content
        else:
            content = []
        text = record['content'].value()
        if text:
            if self._SEPARATOR.search(text):
                pre, post = self._SEPARATOR.split(text, maxsplit=2)
            else:
                pre, post = text, ''
            parser = lcg.Parser()
            sections = parser.parse(pre) + content + parser.parse(post)
            content = [lcg.SectionContainer(sections, toc_depth=0)]
        if not content and record['parent'].value() is None:
            rows = self._data.get_rows(parent=record['menu_item_id'].value(), published=True,
                                       sorting=self._sorting)
            # TODO: Use only items, which are visible to the current user (access rights). 
            if rows:
                return req.redirect('/'+rows[0]['identifier'].value())
        return self._document(req, content, record, err=err, msg=msg)
    
    # TODO: Handle embeded modules without binding (not EmbeddablePytisModule).
    #def action_subpath(self, req, record):
    #    return super(Menu, self).action_subpath(req, record)
    
    def module_uri(self, modname):
        row = self._data.get_row(modname=modname)
        if row:
            return '/'+ row['identifier'].value() +'/data'
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

    def user(self, login):
        """Return the list of user's roles as unique string identifiers."""
        row = self._data.get_row(login=login)
        if row:
            uid = row['uid'].value()
            roles = [wiking.Roles.USER] + self._module('UserRoles').roles(uid)
            return wiking.User(login, uid=uid, name=row['fullname'].value(),
                               roles=roles, data=row)
        else:
            return None

    def check_password(self, user, password):
        return user.data()['passwd'].value() == md5.new(password).hexdigest()
        
    
class Session(wiking.PytisModule, wiking.Session):
    class Spec(wiking.Specification):
        table = 'cms_session'
        fields = (pp.Field('session_id'),
                  pp.Field('uid'),
                  pp.Field('key'),
                  pp.Field('expire'))
    
    def init(self, req, user):
        # Nasty hack: Remove all expired records first.
        self._data.delete_many(pd.AND(pd.EQ('uid', pd.Value(pd.Integer(), user.uid())),
                                      pd.LT('expire', pd.Value(pd.DateTime(),
                                                               pd.DateTime.current_gmtime()))))
        session_key = self._new_session_key()
        row = self._data.make_row(uid=user.uid(), key=session_key, expire=self._expiration())
        self._data.insert(row)
        return session_key
        
    def failure(self, req, user, login):
        pass
        
    def check(self, req, user, key):
        row = self._data.get_row(uid=user.uid(), key=key)
        if row and not self._expired(row['expire'].value()):
            self._record(req, row).update(expire=self._expiration())
            return True
        else:
            return False

    def close(self, req, user, key):
        row = self._data.get_row(uid=user.uid(), key=key)
        if row:
            self._delete(self._record(req, row))


class SessionLog(wiking.PytisModule):
    class Spec(wiking.Specification):
        table = 'cms_session_log_data'
        fields = [pp.Field(_id) for _id in
                  ('id', 'uid', 'start_time', 'ip', 'success', 'user_agent', 'referer')]

    def log(self, req, user, success):
        row = self._data.make_row(uid=user.uid(),
                                  start_time=pd.DateTime.current_gmtime(),
                                  ip=req.header('X-Forwarded-For') or req.remote_host(),
                                  success=success,
                                  user_agent=req.header('User-Agent'),
                                  referer=req.header('Referer'))
        result = self._data.insert(row)
            
    
class Application(wiking.CookieAuthentication, wiking.Application):
    
    _MAPPING = dict(wiking.Application._MAPPING,
                    _resources='Resources')
    
    _RIGHTS = {'Documentation': (wiking.Roles.ANYONE,),
               'Stylesheets': (wiking.Roles.ANYONE,),
               'Resources': (wiking.Roles.ANYONE,)}
    
    def _auth_user(self, req, login):
        return self._module('Users').user(login)
        
    def _auth_check_password(self, user, password):
        return self._module('Users').check_password(user, password)

    def authorize(self, req, module, **kwargs):
        try:
            roles = self._RIGHTS[module.name()]
        except KeyError:
            return self._module('Menu').authorize(req, module, **kwargs)
        else:
            return req.check_roles(roles)
    
    def menu(self, req):
        return self._module('Menu').menu(req)

    def languages(self):
        return self._module('Languages').languages()

    def module_uri(self, modname):
        uri = super(Application, self).module_uri(modname)
        if uri is None:
            uri = self._module('Menu').module_uri(modname)
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
    """Mix-in class for modules which may be embedded into page content.

    Derived classes must implement the 'embed()' method to produce the content to be embedded into
    page text.  The method 'submenu()' also allows the module to extend the main menu.

    """
    
    def submenu(self, req, menu_item_id):
        """Return a list of 'MenuItem' instances to insert into the main menu.
        
        The submenu will appear in the main menu under the item of a page which embeds the module.
        The items returned by this method will always be placed above any items defined by the menu
        structure.

        """
        return []
    
    def embed(self, req, menu_item_record):
        """Return a list of content instances extending the page content.

        The returned value can also be an integer to indicate that the request has already been
        served (with the resulting status code).
        
        """
        return []



class EmbeddablePytisModule(wiking.PytisModule, Embeddable):
    _EMBED_BINDING_COLUMN = None
    _HONOUR_SPEC_TITLE = True

    @staticmethod
    def _embed_condition(row):
        return None
    
    @classmethod
    def binding(cls):
        return wiking.Binding(cls.title(), cls.name(), cls._EMBED_BINDING_COLUMN,
                              condition=cls._embed_condition, id='data')
    
    def embed(self, req, menu_item_record):
        if self._application.authorize(req, self, action='list'):
            return [self.related(req, self.binding(), menu_item_record, req.uri())]
        else:
            #return [lcg.coerce(_("Access denied."))]
            return []
            
        

