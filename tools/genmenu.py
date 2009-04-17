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
import optparse
import string
import sys
import types

import psycopg2 as dbapi

import pytis.form
import pytis.util


class Configuration(object):
    dbparameters = dict(host=None, database=None, user=None, password=None)

class Serial(object):
    _counter = pytis.util.Counter()
    def __init__(self):
        self.id = Serial._counter.next()

class Action(object):
    def __init__(self, name, description, shortname=None):
        self.name = name
        self.description = description
        if shortname is None:
            shortname = name
        self.shortname = shortname

class Menu(Serial):
    def __init__(self, name, title, parent, position, action=None, help=None, hotkey=None):
        Serial.__init__(self)
        self.name = name
        self.title = title
        self.parent = parent
        self.position = position
        self.action = action
        self.help = help
        self.hotkey = hotkey
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

def process_menu(menu, parent, menu_items, actions, position=110):
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
            action_components = action_id.split('/')
            if action_components[0] == 'form':
                shortname = 'form/' + action_components[-1]
            else:
                shortname = action_id
            actions[action_id] = action = Action(name=action_id, shortname=shortname, description=menu.help())
        else:
            if action.description is None:
                action.description = menu.help()
        menu_id = action_id
        while menu_items.has_key(menu_id):
            menu_id = menu_id + '+'
        help = menu.help()
        hotkey = menu.hotkey()
        if hotkey == (None,):
            hotkey_spec = None
        else:
            hotkey_spec = string.join([key.replace(' ', 'SPC') for key in hotkey], ' ')
        menu_items[menu_id] = submenu = Menu(name=menu_id, title=menu.title(), parent=parent,
                                             position=position, action=action, help=help,
                                             hotkey=hotkey_spec)
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
    def add_rights(form_name, action, action_name):
        try:
            access_rights = resolver.get(form_name, 'access_spec')
        except Exception, e:
            print "Couldn't get access rights for form %s: %s" % (form_name, e,)
            return
        if access_rights is None:
            print "No access rights specified for form %s" % (form_name,)
            return
        rights[action_name] = Rights(access_rights, action)
    for action in actions.values():
        action_name = action.name
        if rights.has_key(action_name):
            continue
        action_components = action_name.split('/')
        if action_components[0] != 'form':
            print "Non-form action, no rights assigned: %s" % (action_name,)
            continue
        form_name = action_components[2]
        form_components = form_name.split('::')
        if len(form_components) <= 2:
            add_rights(form_name, action, action_name)
    return rights

def fill_actions(cursor, actions):
    for action in actions.values():
        cursor.execute("insert into c_pytis_menu_actions (name, shortname, description) values(%s, %s, %s)",
                       (action.name, action.shortname, action.description,))

def fill_rights(cursor, rights):
    roles = {}
    for r in ('admin', 'admin_menu', 'admin_roles',):
        roles[r] = None
    for right in rights.values():
        action = right.action
        for specification in right.rights.specification():
            columns = specification[0]
            if columns is None:
                columns = (None,)
            for groups_permissions in specification[1:]:
                groups = groups_permissions[0]
                if not pytis.util.is_sequence(groups):
                    if groups is None:
                        groups = '*'
                    groups = [groups]
                permissions = groups_permissions[1:]
                if pytis.data.Permission.ALL in permissions:
                    permissions = pytis.data.Permission.all_permissions()
                permissions = [p.lower() for p in permissions]
                action_name = action.shortname
                for group in groups:
                    if group and not roles.has_key(group):
                        cursor.execute(("insert into e_pytis_roles (name, description, purposeid) "
                                        "values (%s, %s, %s)"),
                                       (group, "", 'appl',))
                        roles[group] = None
                    for permission in permissions:
                        for c in columns:
                            cursor.execute(("insert into e_pytis_action_rights (action, roleid, rightid, system, granted, colname) "
                                            "values(%s, %s, %s, %s, %s, %s)"),
                                           (action_name, group, permission, True, True, c,))

def fill_menu_items(cursor, menu, fullposition='', indentation=''):
    fullposition += str(menu.position)
    parent = menu.parent and -menu.parent.id
    action = menu.action and menu.action.name
    cursor.execute(("insert into e_pytis_menu "
                    "(menuid, name, title, parent, position, fullposition, indentation, action, help, hotkey) "
                    "values(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)"),
                   (-menu.id, menu.name, menu.title, parent, menu.position, fullposition, indentation, action,
                     menu.help, menu.hotkey,))
    next_indentation = indentation + '   '
    for m in menu.children:
        fill_menu_items(cursor, m, fullposition=fullposition, indentation=next_indentation)

def transfer_roles(cursor):
    excluded_roles = ('postgres',)
    semi_excluded_roles = ('admin', 'admin_roles', 'admin_menu',) + excluded_roles
    # roles
    cursor.execute("select rolname, rolcanlogin from pg_roles")
    roles_rows = [cursor.fetchone() for i in range(cursor.rowcount)]
    for role, login in roles_rows:
        if role not in semi_excluded_roles:
            if login:
                purpose = 'user'
            else:
                purpose = 'appl'
            cursor.execute("insert into e_pytis_roles (name, purposeid) values (%s, %s)",
                           (role, purpose,))
    # membership
    cursor.execute("select roles1.rolname as owner, roles2.rolname as member "
                   "from pg_auth_members, pg_roles as roles1, pg_roles as roles2 "
                   "where pg_auth_members.roleid = roles1.oid and pg_auth_members.member = roles2.oid")
    membership_rows = [cursor.fetchone() for i in range(cursor.rowcount)]
    for owner, member in membership_rows:
        if owner not in excluded_roles and member not in excluded_roles:
            cursor.execute("insert into e_pytis_role_members (roleid, member) values (%s, %s)",
                           (owner, member,))

def recompute_tables(cursor):
    print "Computing membership..."
    # Update role membership
    cursor.execute("select pytis_update_transitive_roles()")
    # Retrieve roles
    roles = {}
    cursor.execute("select roleid, member from a_pytis_valid_role_members")
    for i in range(cursor.rowcount):
        roleid, member = cursor.fetchone()
        members = roles.get(member)
        if members is None:
            roles[member] = members = []
        members.append(roleid)
    print "Computing membership...done"
    # Delete old rights
    cursor.execute("delete from a_pytis_computed_summary_rights")
    # Retrieve rights
    cursor.execute("select rightid, granted, roleid, menuid, system from ev_pytis_menu_rights")
    class RawRights(object):
        def __init__(self):
            self.system = []
            self.allowed = []
            self.forbidden = []
    raw_rights = {}
    for i in range(cursor.rowcount):
        rightid, granted, roleid, menuid, system = cursor.fetchone()
        item_rights = raw_rights.get(menuid)
        if item_rights is None:
            raw_rights[menuid] = item_rights = {}
        role_rights = item_rights.get(roleid)
        if role_rights is None:
            item_rights[roleid] = role_rights = RawRights()
        if system:
            r = role_rights.system
        elif granted:
            r = role_rights.allowed
        else:
            r = role_rights.forbidden
        r.append(rightid)
    # Compute rights
    print "Computing rights..."
    class Rights(object):
        def __init__(self, total, allowed, forbidden, parent):
            self.total = total
            self.allowed = allowed
            self.forbidden = forbidden
            self.parent = parent
    computed_rights = {}
    cursor.execute("select menuid, name, parent from e_pytis_menu order by fullposition")
    for i in range(cursor.rowcount):
        menuid, name, parent = cursor.fetchone()
        if not menuid:
            continue
        menu_rights = raw_rights.get(menuid, {})
        for roleid, role_roles in roles.items():
            max_ = []
            allowed = []
            forbidden = []
            for role in role_roles:
                raw = menu_rights.get(role) or RawRights()
                max_ += raw.system
                allowed += raw.allowed
                forbidden += raw.forbidden
            max_rights = []
            for r in max_:
                if r not in max_rights:
                    max_rights.append(r)
            forbidden_rights = []
            for r in forbidden:
                if r not in forbidden_rights:
                    forbidden_rights.append(r)
            allowed_rights = []
            for r in allowed:
                if r not in forbidden_rights and r not in allowed_rights:
                    allowed_rights.append(r)
            raw = menu_rights.get('*') or RawRights()
            for r in raw.system:
                if r not in max_rights:
                    max_rights.append(r)
            for r in raw.forbidden:
                if r not in forbidden_rights and f not in allowed_rights:
                    forbidden_rights.append(r)
            for r in raw.allowed:
                if r not in forbidden_rights and f not in allowed_rights:
                    allowed_rights.append(r)
            if name:
                if not max_rights:
                    for r in menu_rights.values():
                        if r:
                            break
                    else:
                        max_rights = ['view', 'insert', 'update', 'delete', 'print', 'export', 'call']
            else:
                max_rights = None
            parent_menuid = parent
            while parent_menuid is not None:
                parent_rights = computed_rights[(parent_menuid, roleid,)]
                for r in parent_rights.forbidden:
                    if r not in forbidden_rights and r not in allowed_rights:
                        forbidden_rights.append(r)                
                for r in parent_rights.allowed:
                    if r not in forbidden_rights and r not in allowed_rights:
                        allowed_rights.append(r)
                parent_menuid = parent_rights.parent
            if max_rights is None:
                max_rights = allowed_rights
            rights = [right for right in max_rights if right not in forbidden_rights]
            if 'show' not in forbidden_rights:
                rights.append('show')
            rights.sort()
            computed_rights[(menuid, roleid)] = Rights(total=rights, allowed=allowed_rights, forbidden=forbidden_rights, parent=parent)
    print "Computing rights...done"
    # Store computed rights
    print "Storing rights..."
    for (menuid, roleid), all_rights in computed_rights.items():
        rights = all_rights.total
        cursor.execute("insert into a_pytis_computed_summary_rights (menuid, roleid, rights) values(%s, %s, %s)",
                       (menuid, roleid, string.join(rights, ' '),))
    print "Storing rights...done"
            
def parse_options():
    usage = "usage: %prog [options] DEF_DIRECTORY"
    parser = optparse.OptionParser(usage)
    parser.add_option("-H", "--host", default=None, action="store", dest="host")
    parser.add_option("-d", "--database", default=None, action="store", dest="database")
    parser.add_option("-U", "--user", default=None, action="store", dest="user")
    parser.add_option("-P", "--password", default=None, action="store", dest="password")
    options, args = parser.parse_args()
    dbparameters = Configuration.dbparameters
    dbparameters['host'] = options.host
    dbparameters['database'] = options.database
    dbparameters['user'] = options.user
    dbparameters['password'] = options.password
    if len(args) != 1:
        parser.print_help()
        sys.exit(1)
    return args
    
def run():
    args = parse_options()
    def_dir = args[0]
    resolver = pytis.util.FileResolver(def_dir)
    menu = resolver.get('application', 'menu')
    top = Menu(name=None, title=_("CELÉ MENU"), parent=None, position=0, action=None)
    menu_items = {}
    actions = {}
    print "Retrieving menu..."
    process_menu(menu, top, menu_items, actions)
    print "Retrieving menu...done"
    print "Retrieving rights..."
    rights = process_rights(resolver, actions)
    print "Retrieving rights...done"
    parameters = {}
    for k, v in Configuration.dbparameters.items():
        if v is not None:
            parameters[k] = v
    connection = dbapi.connect(**parameters)
    cursor = connection.cursor()
    print "Deleting old data..."
    cursor.execute("set client_encoding to 'latin2'") # grrr
    cursor.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('genmenu')")
    cursor.execute("delete from e_pytis_menu")
    cursor.execute("delete from e_pytis_action_rights")
    cursor.execute("delete from c_pytis_menu_actions")
    cursor.execute("delete from e_pytis_role_members where id >= 0")
    cursor.execute("delete from e_pytis_roles where purposeid != 'admn'")
    print "Deleting old data...done"
    print "Inserting actions..."
    fill_actions(cursor, actions)
    print "Inserting actions...done"
    print "Inserting rights..."
    fill_rights(cursor, rights)
    print "Inserting rights...done"
    print "Inserting menu..."
    fill_menu_items(cursor, top)
    print "Inserting menu...done"
    print "Importing roles..."
    transfer_roles(cursor)
    print "Importing roles...done"
    recompute_tables(cursor)
    cursor.execute("delete from e_pytis_disabled_dmp_triggers")
    connection.commit()

if __name__ == '__main__':
    run()
