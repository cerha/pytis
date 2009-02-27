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

import os, re, lcg, wiking, pytis.util
import cms


class Languages(wiking.PytisModule):
    class Spec(wiking.Specification, cms.Languages):
        pass

    def languages(self):
        return [str(r['lang'].value()) for r in self._data.get_rows()]


class Menu(wiking.PytisModule):

    class Spec(wiking.Specification, cms.Menu):
        def _cbname(self, name):
            # Some codebooks are not needed for the web module, so just ignore them.
            if name in ('Languages'):
                return name
            else:
                return None
    
    _SEPARATOR = re.compile('^====+\s*$', re.MULTILINE)

    @classmethod
    def binding_id(cls, modname):
        return pytis.util.camel_case_to_lower(modname, '-')
    
    def _resolve(self, req):
        identifier = req.unresolved_path[0]
        rows = self._data.get_rows(identifier=identifier, published=True)
        if not rows:
            if self._data.get_rows(identifier=identifier):
                raise wiking.Forbidden()
            else:
                raise wiking.NotFound()
        variants = [str(row['lang'].value()) for row in rows]
        lang = req.prefered_language(variants)
        del req.unresolved_path[0]
        return rows[variants.index(lang)]

    def _bindings(self, req, record):
        bindings = super(Menu, self)._bindings(req, record)
        modname = record['modname'].value()
        if modname:
            cls = wiking.cfg.resolver.wiking_module_cls(modname)
            if cls and issubclass(cls, EmbeddablePytisModule):
                bindings.append(cls.binding())
        return bindings
    
    def menu(self, req):
        children = {None: []}
        translations = {}
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
            hidden = False # TODO: get visibility for the current user
            return wiking.MenuItem(identifier, title, descr=descr, hidden=hidden,
                                   variants=titles.keys(), submenu=submenu)
        # First process all rows and build a dictionary of descendants for each item and
        # translations of titles and descriptions.  Then construct the menu structure.
        for row in self._data.get_rows(sorting=self._sorting, published=True):
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
            if isinstance(content, int):
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
            return '/'+ row['identifier'].value() +'/'+ self.binding_id(row['modname'].value())
        else:
            return None
    
    
class Application(wiking.CookieAuthentication, wiking.Application):

    _MAPPING = dict(wiking.Application._MAPPING,
                    _resources='Resources')
    
    def menu(self, req):
        return self._module('Menu').menu(req)

    def languages(self):
        return self._module('Languages').languages()

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
    
    #def panels(self, req, lang):
    #    return
    
    #def _auth_user(self, req, login):
    #    return self._module('Authentication').user(req, login)
        
    #def _auth_check_password(self, user, password):
    #    return user.data().check_passwd(password)
    
    #def authorize(self, req, module, action=None, record=None, **kwargs):
    #    return True

    
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
    
    def embed(self, req, record):
        """Return a list of content instances extending the page content.

        The returned value can also be an integer to indicate that the request has already been
        served (with the resulting status code).
        
        """
        return []



class EmbeddablePytisModule(wiking.PytisModule, Embeddable):
    _EMBED_BINDING_COLUMN = None

    @staticmethod
    def _embed_condition(row):
        return None
    
    @classmethod
    def binding(cls):
        return wiking.Binding(cls.title(), cls.name(), cls._EMBED_BINDING_COLUMN,
                              condition=cls._embed_condition,
                              id=Menu.binding_id(cls.name()))
    
    def embed(self, req, record):
        return [self.related(req, self.binding(), record, req.uri())]

