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
        menu_items[menu_id] = supmenu = Menu(name=menu_id, title=menu.title(), parent=parent, position=position)
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
        menu_id = super_menu_id(menu, menu_items)
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

def fill_actions(cursor, actions):
    for action in actions.values():
        cursor.execute("insert into c_pytis_menu_actions (actionid, name, description) values(%s, %s, %s)",
                       (action.id, action.name, action.description,))

def fill_menu_items(cursor, menu, fullposition=''):
    fullposition += str(menu.position)
    parent = menu.parent and menu.parent.id
    action = menu.action and menu.action.id
    cursor.execute("insert into e_pytis_menu(menuid, name, title, parent, position, fullposition, actionid) values(%s, %s, %s, %s, %s, %s, %s)",
                   (menu.id, menu.name, menu.title, parent, menu.position, fullposition, action,))
    for m in menu.children:
        fill_menu_items(cursor, m, fullposition=fullposition)
    
def run(def_dir):
    resolver = pytis.util.FileResolver(def_dir)
    menu = resolver.get('application', 'menu')
    top = Menu(name='', title="Top", parent=None, position=0, action=None)
    menu_items = {}
    actions = {}
    process_menu(menu, top, menu_items, actions)
    connection = dbapi.connect(database='pytis-demo')
    cursor = connection.cursor()
    cursor.execute("set client_encoding to 'latin2'") # grrr
    cursor.execute("delete from e_pytis_menu")
    cursor.execute("delete from c_pytis_menu_actions")
    fill_actions(cursor, actions)
    fill_menu_items(cursor, top)
    connection.commit()

if __name__ == '__main__':
    run(sys.argv[1])
