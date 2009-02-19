#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2009 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
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

import copy
import sys
import types

import pytis.form
import pytis.util

#actions: (actionid), name, description
#menu: (menuid)?, title, parent, position, actionid

def modulify(obj, name):
    module = obj.__module__
    if module[:len('pytis.')] != 'pytis.':
        name = '%s.%s' % (obj.__module__, name,)
    return name

def super_menu_id(menu):
    title = menu.title()
    id_title = title.replace(' ', '-')
    return ('menu/%s' % (id_title,))

def menu_item_id(menu):
    args = copy.copy(menu.args())
    command = menu.command().name()
    appstring = 'Application.'
    if command[:len(appstring)] == appstring:
        command = command[len(appstring):]
    if command == 'RUN_FORM':
        form_class = args['form_class']; del args['form_class']
        form_name = args['name']; del args['name']
        extra = ''
        if args.has_key('binding'):
            extra = ('/binding=%s' % (args['binding'],)); del args['binding']
        if not args:
            class_name = modulify(form_class, form_class.__name__)
            return ('item/%s/%s/%s%s' % (command, class_name, form_name, extra,))
    elif command == 'NEW_RECORD':
        form_name = args['name']; del args['name']
        if not args:
            return ('item/%s/%s' % (command, form_name,))
    elif command == 'HANDLED_ACTION':
        handler = args['handler']; del args['handler']
        if not args and type(handler) == types.FunctionType:
            name = modulify(handler, handler.func_name)
            return ('item/%s/%s' % (command, name,))
    if args:
        return '??? %s %s' % (command, menu.args(),)
    return ('item/%s' % (command,))

def dump_menu(menu, indent=0):
    if isinstance(menu, pytis.form.Menu):
        print '%s* %s: %s' % (' '*indent, menu.title(), super_menu_id(menu),)
        dump_menu(menu.items(), indent=indent+2)
    elif isinstance(menu, pytis.form.MItem):
        print '%s- %s: %s' % (' '*indent, menu.title(), menu_item_id(menu))
    elif isinstance(menu, pytis.form.MSeparator):
        print '%s----' % (' '*indent,)
    elif isinstance(menu, tuple):
        for m in menu:
            dump_menu(m, indent=indent)        
    else:
        print 'Unknown menu: %s' % (menu,)

def run(def_dir):
    resolver = pytis.util.FileResolver(def_dir)
    menu = resolver.get('application', 'menu')
    dump_menu(menu)

if __name__ == '__main__':
    run(sys.argv[1])
