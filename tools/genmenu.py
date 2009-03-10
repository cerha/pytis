#!/usr/bin/env python
# -*- coding: iso-8859-2 -*-

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

import psycopg2 as dbapi

import pytis.form
import pytis.util

class Serial(object):
    _counter = pytis.util.Counter()
    def __init__(self):
        self.id = Serial._counter.next()

class Action(Serial):
    def __init__(self, name, description):
        Serial.__init__(self)
        self.name = name
        self.description = description

class Menu(Serial):
    def __init__(self, name, title, parent, position, action=None):
        Serial.__init__(self)
        self.name = name
        self.title = title
        self.parent = parent
        self.position = position
        self.action = action
        self.children = []

class Rights(object):
    def __init__(self, rights, action):
        self.rights = rights
        self.action = action

def super_menu_id(menu, menu_items):
    title = menu.title()
    id_title = title.replace(' ', '-')
    base_id = 'menu/%s' % (id_title,)
    menu_id = base_id
    i = 1
    while menu_items.has_key(menu_id):
        menu_id = base_id + str(i)
        i += 1
    return menu_id

def process_menu(menu, parent, menu_items, actions, position=100):
    if isinstance(menu, pytis.form.Menu):
        menu_id = super_menu_id(menu, menu_items)
        menu_items[menu_id] = supmenu = Menu(name=None, title=menu.title(), parent=parent, position=position)
        parent.children.append(supmenu)
        process_menu(menu.items(), supmenu, menu_items, actions)
    elif isinstance(menu, pytis.form.MItem):
        action_id = menu.action_id()
        if action_id is None:
            print "Menu item without action id, add one explicitly:", menu.title()
            return
        action = actions.get(action_id)
        if action is None:
            actions[action_id] = action = Action(name=action_id, description=menu.help())
        else:
            if action.description is None:
                action.description = menu.help()
        menu_id = menu.menu_item_id()
        if menu_id is None:
            print "Menu with empty menu id, add one explicitly:", menu.title()
            return
        if menu_items.has_key(menu_id):
            print "Duplicate menu id, change it:", menu.title()
            return
        menu_items[menu_id] = submenu = Menu(name=menu_id, title=menu.title(), parent=parent, position=position, action=action)
        parent.children.append(submenu)        
    elif isinstance(menu, pytis.form.MSeparator):
        parent.children.append(Menu(name=None, title=None, parent=parent, position=position))
    elif isinstance(menu, tuple):
        for m in menu:
            process_menu(m, parent, menu_items, actions, position=position)
            position += 10
    else:
        print 'Unknown menu: %s' % (menu,)

def process_rights(resolver, actions):
    rights = {}
    for action in actions.values():
        action_name = action.name
        if rights.has_key(action_name):
            continue
        action_components = action_name.split('/')
        if action_components[0] != 'form':
            print "Non-form action, no rights assigned: %s" % (action_name,)
            continue
        form_name = action_components[2]
        try:
            access_rights = resolver.get(form_name, 'access_spec')
        except Exception, e:
            print "Couldn't get access rights for form %s: %s" % (form_name, e,)
            continue
        if access_rights is None:
            print "No access rights specified for form %s" % (form_name,)
            continue
        rights[action_name] = Rights(access_rights, action)
    return rights

def fill_actions(cursor, actions):
    for action in actions.values():
        cursor.execute("insert into c_pytis_menu_actions (actionid, name, description) values(%s, %s, %s)",
                       (action.id, action.name, action.description,))

def fill_rights(cursor, rights):
    roles = {}
    for right in rights.values():
        action = right.action
        for specification in right.rights.specification():
            if specification[0] is not None:
                print "Can't process column specifications for action: %s" % (action.name,)
                continue
            for groups_permissions in specification[1:]:
                groups = groups_permissions[0]
                if not pytis.util.is_sequence(groups):
                    groups = [groups]
                permissions = groups_permissions[1:]
                if pytis.data.Permission.ALL in permissions:
                    permissions = pytis.data.Permission.all_permissions()
                permissions = [p.lower() for p in permissions]
                action_id = action.id
                for group in groups:
                    if not roles.has_key(group):
                        cursor.execute(("insert into e_pytis_roles (name, description, purposeid)"
                                        "values (%s, %s, %s)"),
                                       (group, "", 'appl',))
                        roles[group] = None
                    for permission in permissions:
                        cursor.execute(("insert into e_pytis_action_rights (actionid, roleid, rightid, system) "
                                        "values(%s, %s, %s, %s)"),
                                       (action_id, group, permission, True,))

def fill_menu_items(cursor, menu, fullposition='', indentation=''):
    fullposition += str(menu.position)
    parent = menu.parent and -menu.parent.id
    action = menu.action and menu.action.id
    cursor.execute(("insert into e_pytis_menu(menuid, name, title, parent, position, fullposition, indentation, actionid) "
                    "values(%s, %s, %s, %s, %s, %s, %s, %s)"),
                   (-menu.id, menu.name, menu.title, parent, menu.position, fullposition, indentation, action,))
    next_indentation = indentation + '   '
    for m in menu.children:
        fill_menu_items(cursor, m, fullposition=fullposition, indentation=next_indentation)
    
def run(def_dir):
    resolver = pytis.util.FileResolver(def_dir)
    menu = resolver.get('application', 'menu')
    top = Menu(name='', title=_("CELÉ MENU"), parent=None, position=0, action=None)
    menu_items = {}
    actions = {}
    process_menu(menu, top, menu_items, actions)
    rights = process_rights(resolver, actions)
    connection = dbapi.connect(database='pytis-demo')
    cursor = connection.cursor()
    cursor.execute("set client_encoding to 'latin2'") # grrr
    cursor.execute("delete from e_pytis_menu")
    cursor.execute("delete from e_pytis_action_rights")
    cursor.execute("delete from c_pytis_menu_actions")
    cursor.execute("delete from e_pytis_roles where purposeid = 'appl'")
    fill_actions(cursor, actions)
    fill_rights(cursor, rights)
    fill_menu_items(cursor, top)
    connection.commit()

if __name__ == '__main__':
    run(sys.argv[1])
