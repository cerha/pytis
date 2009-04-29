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
import optparse
import string
import sys
import types

import psycopg2 as dbapi

import pytis.data
import pytis.extensions
import pytis.form
import pytis.util

def cfg_param(*args, **kwargs):
    return pytis.data.Value(None, None)
import pytis.extensions
pytis.extensions.cfg_param = cfg_param
pytis.extensions.misc.cfg_param = cfg_param

_current_form_name = None
def is_in_groups(groups):
    print "Warning: is_in_groups called in %s for groups %s" % (_current_form_name, groups,)
    return True
pytis.extensions.is_in_groups = is_in_groups

def run_procedure_mitem(title, name, proc_name, hotkey=None, groups=None, enabled=None, **kwargs):
    cmd = pytis.form.Application.COMMAND_RUN_PROCEDURE
    if groups is not None:
        assert isinstance(groups, (tuple, list))
        assert enabled is None or callable(enabled)
        if groups is not None:
            if enabled is not None:
                enabled = "Both groups and enabled specified"
            else:
                enabled = groups
    return pytis.form.MItem(title, command=cmd, hotkey=hotkey,
                            args=dict(spec_name=name, proc_name=proc_name,
                                      enabled=enabled, **kwargs),
                            help='Spustit proceduru "%s"' % title)
pytis.extensions.run_procedure_mitem = run_procedure_mitem
pytis.extensions.rp = run_procedure_mitem

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
    def __init__(self, name, title, parent, position, action=None, help=None, hotkey=None, system=False):
        Serial.__init__(self)
        self.name = name
        self.title = title
        self.parent = parent
        self.position = position
        self.action = action
        self.help = help
        self.hotkey = hotkey
        self.system = system
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

def process_menu(menu, parent, menu_items, actions, rights, position, system=False):
    if isinstance(menu, pytis.form.Menu):
        menu_id = super_menu_id(menu, menu_items)
        menu_items[menu_id] = supmenu = Menu(name=None, title=menu.title(), parent=parent, position=position, system=system)
        parent.children.append(supmenu)
        process_menu(menu.items(), supmenu, menu_items, actions, rights, position=position+'11', system=system)
    elif isinstance(menu, pytis.form.MItem):
        action_id = menu.action_id()
        if action_id is None:
            print "Error: Special menu item action, define command specification:", menu.title()
            return
        action = actions.get(action_id)
        if action is None:
            action_components = action_id.split('/')
            action_kind = action_components[0]
            if action_kind == 'form':
                shortname = 'form/' + action_components[2]
            else:
                shortname = action_id
            actions[action_id] = action = Action(name=action_id, shortname=shortname, description=menu.help())
            if action_kind == 'proc':
                enabled = menu.args().get('enabled')
                if isinstance(enabled, basestring):
                    print "Error: Procedure with unhandled `enabled': ", enabled, menu.title()
                elif pytis.util.is_sequence(enabled):
                    proc_rights = (None, (enabled, pytis.data.Permission.CALL,),)
                    other_proc_rights = rights.get(action_id)
                    if other_proc_rights:
                        proc_rights = proc_rights + other_proc_rights.rights.specification()
                    rights[action_id] = Rights(pytis.data.AccessRights(proc_rights), action)
                elif enabled is not None:
                    print "Error: Unexpected `enabled' value: ", enabled, menu.title()
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
                                             hotkey=hotkey_spec, system=system)
        parent.children.append(submenu)        
    elif isinstance(menu, pytis.form.MSeparator):
        parent.children.append(Menu(name=None, title=None, parent=parent, position=position, system=system))
    elif isinstance(menu, tuple):
        for m in menu:
            process_menu(m, parent, menu_items, actions, rights, position=position, system=system)
            if parent.parent is None:
                system = False
            position = str(long(position) + 2)
    else:
        print 'Unknown menu: %s' % (menu,)

def process_rights(resolver, actions, rights):
    def add_rights(form_name, action, action_name):
        global _current_form_name
        if form_name.find(':') != -1:
            if form_name.find('::') == -1:
                print "Error: Unhandled obsolete form specification %s" % (form_name,)
            return
        pos = form_name.rfind('.')
        if pos == -1:
            print 'Not caring about access rights of form %s' % (form_name,)
            return
        module_name = form_name[:pos].replace('.', '/')
        class_name = form_name[pos+1:]
        _current_form_name = form_name
        try:
            form_spec = resolver.get_object(module_name, class_name)
            access_rights = form_spec.access_rights
            if callable(access_rights):
                access_rights = access_rights()
        except Exception, e:
            print "Couldn't get access rights for form %s: %s" % (form_name, e,)
            return
        if access_rights is None:
            print "No access rights specified for form %s" % (form_name,)
            return
        rights[action_name] = Rights(access_rights, action)
    for action in (actions.values() +
                   [Action('form/*/menu.ApplicationMenu', '', 'form/menu.ApplicationMenu'),
                    Action('form/*/menu.ApplicationMenuM', '', 'form/menu.ApplicationMenuM'),
                    Action('form/*/menu.ApplicationRoles', '', 'form/menu.ApplicationRoles')]):
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
    for r in ('*', 'admin', 'admin_menu', 'admin_roles',):
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
    return roles

def fill_menu_items(cursor, menu, position=''):
    position += str(menu.position)
    parent = menu.parent and -menu.parent.id
    action = menu.action and menu.action.name
    if menu.system:
        locked = 'T'
    else:
        locked = 'F'
    cursor.execute(("insert into e_pytis_menu "
                    "(menuid, name, title, position, action, help, hotkey, locked) "
                    "values(%s, %s, %s, %s, %s, %s, %s, %s)"),
                   (-menu.id, menu.name, menu.title, menu.position, action,
                     menu.help, menu.hotkey, locked,))
    for m in menu.children:
        fill_menu_items(cursor, m, position=position)

def transfer_roles(cursor, present_roles):
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
            if not present_roles.has_key(role):
                cursor.execute("insert into e_pytis_roles (name, purposeid) values (%s, %s)",
                               (role, purpose,))
            elif purpose != 'appl':
                cursor.execute("update e_pytis_roles set purposeid=%s where name = %s",
                               (purpose, role,))
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
    cursor.execute("select pytis_update_transitive_roles()")
    print "Computing membership...done"
    print "Computing rights..."
    cursor.execute("select pytis_update_summary_rights()")
    print "Computing rights...done"
            
def parse_options():
    usage = "usage: %prog [options] DEF_DIRECTORY"
    parser = optparse.OptionParser(usage)
    parser.add_option("-H", "--host", default=None, action="store", dest="host")
    parser.add_option("-d", "--database", default=None, action="store", dest="database")
    parser.add_option("-U", "--user", default=None, action="store", dest="user")
    parser.add_option("-P", "--password", default=None, action="store", dest="password")
    parser.add_option("--delete", action="store_true", dest="delete_only")
    options, args = parser.parse_args()
    dbparameters = Configuration.dbparameters
    dbparameters['host'] = options.host
    dbparameters['database'] = options.database
    dbparameters['user'] = options.user
    dbparameters['password'] = options.password
    if (options.delete_only and args or
        not options.delete_only and len(args) != 1):
        parser.print_help()
        sys.exit(1)
    return options, args
    
def run():
    options, args = parse_options()
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
    if options.delete_only:
        connection.commit()
        return
    def_dir = args[0]
    import config
    config.def_dir = def_dir
    resolver = pytis.util.resolver()
    menu = resolver.get('application', 'menu')
    menu[0]._items = ((pytis.form.Menu(_("Správa menu a uživatelských rolí"),
                                       (pytis.extensions.run_form_mitem(_("Menu"), 'menu.ApplicationMenu',
                                                                        pytis.form.BrowseForm),
                                        pytis.extensions.run_form_mitem(_("Práva menu"), 'menu.ApplicationMenuM',
                                                                        pytis.form.MultiBrowseDualForm),
                                        pytis.extensions.run_form_mitem(_("Uživatelské role"), 'menu.ApplicationRoles',
                                                                        pytis.form.MultiBrowseDualForm),)),)
                      + menu[0]._items)
    top = Menu(name=None, title=_("CELÉ MENU"), parent=None, position='', action=None, system=True)
    menu_items = {}
    actions = {}
    rights = {}
    print "Retrieving menu..."
    process_menu(menu, top, menu_items, actions, rights, position='11', system=True)
    print "Retrieving menu...done"
    print "Retrieving rights..."
    process_rights(resolver, actions, rights)
    print "Retrieving rights...done"
    print "Inserting actions..."
    fill_actions(cursor, actions)
    print "Inserting actions...done"
    print "Inserting rights..."
    roles = fill_rights(cursor, rights)
    print "Inserting rights...done"
    print "Inserting menu..."
    fill_menu_items(cursor, top)
    print "Inserting menu...done"
    print "Importing roles..."
    transfer_roles(cursor, roles)
    print "Importing roles...done"
    recompute_tables(cursor)
    cursor.execute("delete from e_pytis_disabled_dmp_triggers where id = 'genmenu'")
    connection.commit()

if __name__ == '__main__':
    run()
