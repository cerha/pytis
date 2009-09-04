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
import os
import string
import sys
import types

import psycopg2 as dbapi

import pytis.data
import pytis.extensions
import pytis.form
import pytis.presentation
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
    def __init__(self, name, description, shortname=None, title=None, subactions=()):
        self.name = name
        self.description = description
        if shortname is None:
            shortname = name
        self.shortname = shortname
        self.title = title
        self.subactions = subactions

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

def process_menu(resolver, menu, parent, menu_items, actions, rights, position, system=False):
    if isinstance(menu, pytis.form.Menu):
        menu_id = super_menu_id(menu, menu_items)
        menu_items[menu_id] = supmenu = Menu(name=None, title=menu.title(), parent=parent, position=position, system=system)
        parent.children.append(supmenu)
        if position:
            next_position = position+'.1111'
        else:
            next_position = '1111'
        process_menu(resolver, menu.items(), supmenu, menu_items, actions, rights, position=next_position, system=system)
    elif isinstance(menu, pytis.form.MItem):
        action_id = menu.action_id()
        if action_id is None:
            print "Error: Special menu item action, define command specification:", menu.title()
            return
        action = actions.get(action_id)
        subactions = []
        if action is None:
            action_components = action_id.split('/')
            action_kind = action_components[0]
            spec_title = None
            if action_kind == 'form':
                form_name = action_components[2]
                shortname = 'form/' + form_name
                form_name_components = form_name.split('.')
                form_module = string.join(form_name_components[:-1], '/')
                base_form_name = form_name_components[-1]
                try:
                    spec_title = resolver.get_object(form_module, base_form_name).title
                except:
                    pass
                bindings = None
                form_class = eval(action_components[1])
                if issubclass(form_class, pytis.form.DualForm):
                    pos = form_name.find('::')
                    if pos == -1:
                        try:
                            bindings = resolver.get_object(form_module, base_form_name).bindings
                        except:
                            pass
                    else:
                        def binding(name):
                            form_name_components = name.split('.')
                            form_module = string.join(form_name_components[:-1], '/')
                            base_form_name = form_name_components[-1]
                            try:
                                title = resolver.get_object(form_module, base_form_name).title
                            except:
                                title = ''
                            return pytis.presentation.Binding(id=name, title=title, name=('form/%s' % (name,)),
                                                              binding_column='dummy')
                        bindings = (binding(form_name[:pos]), binding(form_name[pos+2:]),)
                if pytis.util.is_sequence(bindings):
                    for i in range(len(bindings)):
                        b = bindings[i]
                        subaction_id = 'sub/%02d/%s' % (i, action_id,)
                        subaction_title = b.title()
                        subaction_shortname = 'form/'+b.name()
                        actions[subaction_id] = Action(subaction_id, '',
                                                       shortname=subaction_shortname,
                                                       title=subaction_title)
                        subactions.append(subaction_id)
            else:
                shortname = action_id
            actions[action_id] = action = Action(name=action_id, shortname=shortname,
                                                 description=menu.help(), title=spec_title,
                                                 subactions=subactions)
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
            process_menu(resolver, m, parent, menu_items, actions, rights, position=position, system=system)
            if parent.parent is None:
                system = False
            position_labels = string.split(position, '.')
            position_labels[-1] = str(long(position[-4:]) + 1)
            position = string.join(position_labels, '.')
    else:
        print 'Unknown menu: %s' % (menu,)

def process_rights(resolver, actions, rights, def_dir):
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
            print "Error: Couldn't get access rights for form %s: %s" % (form_name, e,)
            return
        if access_rights is None:
            print "No access rights specified for form %s, assuming everything permitted" % (form_name,)
            access_rights = pytis.data.AccessRights((None, (None, pytis.data.Permission.ALL)),)
        rights[action_name] = Rights(access_rights, action)
    for action in (actions.values() +
                   [Action('form/*/menu.ApplicationMenu', '', 'form/menu.ApplicationMenu'),
                    Action('form/*/menu.ApplicationMenuM', '', 'form/menu.ApplicationMenuM'),
                    Action('sub/01/form/*/menu.ApplicationMenuM', '', 'form/menu.ApplicationMenuRights',
                           title='Rozpis práv položky menu'),
                    Action('sub/02/form/*/menu.ApplicationMenuM', '', 'form/menu.ApplicationSummaryRights',
                           title='Práva položky menu'),
                    Action('form/*/menu.ApplicationRoles', '', 'form/menu.ApplicationRoles'),
                    Action('sub/01/form/*/menu.ApplicationRoles', '', 'form/menu.ApplicationRolesMembers',
                           title='Obsahuje role'),
                    Action('sub/02/form/*/menu.ApplicationRoles', '', 'form/menu.ApplicationRolesOwners',
                           title='Patří do rolí'),
                    Action('sub/03/form/*/menu.ApplicationRoles', '', 'form/menu.ApplicationRolesMenu',
                           title='Náhled menu'),
                    Action('sub/04/form/*/menu.ApplicationRoles', '', 'form/menu.ApplicationRolesExtendedMenu',
                           title='Rozšířený náhled menu'),
                    ]):
        action_name = action.name
        if rights.has_key(action_name):
            continue
        action_components = action_name.split('/')
        if len(action_components) == 2 and action_components[0] == 'RUN_FORM':
            try:
                command, args = resolver.get('commands', action_components[1])
                if not issubclass(args['form_class'], pytis.form.Form):
                    raise Exception()
                form_name = args['name']
            except:
                print 'Warning: failed to retrieve RUN_FORM command: %s' % (action_name,)
        elif action_components[0] == 'NEW_RECORD':
            form_name = action_components[1]
        elif action_components[0] == 'form':
            form_name = action_components[2]
        elif action_components[0] == 'sub':
            continue
        else:
            print "Non-form action, no rights assigned: %s" % (action_name,)
            continue
        form_components = form_name.split('::')
        if len(form_components) <= 2:
            add_rights(form_name, action, action_name)
    actions_shortnames = {}
    for shortname in [a.shortname for a in actions.values() if a.shortname.find('::') == -1]:
        actions_shortnames[shortname] = True
    actions_extra_shortnames = copy.copy(actions_shortnames)
    def_dir_len = len(def_dir.split('/'))
    for root, dirs, files in os.walk(def_dir):
        relative_root_path = root.split('/')[def_dir_len:]
        if relative_root_path:
            relative_root = os.path.join(*relative_root_path) + '/'
        else:
            relative_root = ''
        for f in files:
            if f.endswith('.py'):
                module_name = relative_root + f[:-3]
                try:
                    module = resolver.get_module(module_name)
                except pytis.util.ResolverFileError:
                    print 'Warning: module not loaded: %s' % (module,)
                    continue
                module_identifier = module_name.replace('/', '.')
                for spec_attr in [o for o in dir(module) if o[0] != '_']:
                    spec = getattr(module, spec_attr)
                    if isinstance(spec, type) and issubclass(spec, pytis.form.Specification):
                        spec_name = module_identifier + '.' + spec.__name__
                        action_shortname = 'form/'+spec_name
                        if not actions_shortnames.has_key(action_shortname):
                            actions_shortnames[action_shortname] = True
                            action_name = 'form/*/'+spec_name
                            try:
                                spec_title = spec.title
                            except:
                                spec_title = None
                            actions[action_name] = action = Action(action_name, '', action_shortname,
                                                                   title=spec_title)
                            add_rights(spec_name, action, action_name)
                        else:
                            try:
                                del actions_extra_shortnames[action_shortname]
                            except KeyError:
                                pass
    actions['label/1'] = Action('label/1', None, title="SAMOSTATNÉ AKCE")
    if actions_extra_shortnames:
        print 'Warning: actions without met specifications: %s' % (actions_extra_shortnames.keys(),)
    return rights

def fill_actions(cursor, actions):
    for action in actions.values():
        cursor.execute("insert into c_pytis_menu_actions (fullname, shortname, action_title, description) values(%s, %s, %s, %s)",
                       (action.name, action.shortname, action.title, action.description,))

def check_actions(cursor, actions, update):
    subactions = {}
    cursor.execute("select fullname from c_pytis_menu_actions where fullname like 'sub/%'")
    while True:
        row = cursor.fetchone()
        if row is None:
            break
        fullname = row[0]
        parent_name = fullname[7:]
        subactions_list = subactions.get(parent_name)
        if subactions_list is None:
            subactions_list = subactions[parent_name] = []
        subactions_list.append(fullname)
    cursor.execute("select fullname, shortname, action_title from c_pytis_menu_actions")
    actions = copy.copy(actions)
    missing_titles = []
    while True:
        row = cursor.fetchone()
        if row is None:
            break
        fullname, shortname, title = row
        if fullname[:5] == 'menu/':
            continue
        action = actions.get(fullname)
        if action is None:
            print 'Check: Extra action: %s (%s)' % (fullname, shortname,)
        else:
            del actions[fullname]
            db_subactions_list = subactions.get(fullname)
            subactions_list = copy.copy(list(action.subactions))
            subactions_list.sort()
            if db_subactions_list:
                db_subactions_list.sort()
            else:
                db_subactions_list = []
            if subactions_list != db_subactions_list:
                print 'Check: Subactions mismatch: %s app="%s" db="%s"' % (fullname, action.subactions, db_subactions_list,)
            if not title and action.title:
                missing_titles.append((fullname, action.title,))
    for fullname, action in actions.items():
        print 'Check: Missing action: %s (%s)' % (fullname, action.shortname,)
    for fullname, title in missing_titles:
        if update:
            print 'Setting title for %s: %s' % (fullname , title,)
            cursor.execute("update c_pytis_menu_actions set action_title=%s where fullname=%s", (title, fullname,))
        else:
            print 'Missing action title in %s: %s' % (fullname , title,)

def fill_rights(cursor, rights, check_rights=None):
    already_stored = {}
    roles = {}
    for r in ('*', 'admin', 'admin_menu', 'admin_roles',):
        roles[r] = None
    for right in rights.values():
        action = right.action
        for specification in right.rights.specification():
            columns = specification[0]
            if columns is None:
                columns = (None,)
            elif isinstance(columns, str):
                columns = [columns]
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
                    if check_rights is None:
                        if group and not roles.has_key(group):
                            cursor.execute(("insert into e_pytis_roles (name, description, purposeid) "
                                            "values (%s, %s, %s)"),
                                           (group, "", 'appl',))
                            roles[group] = None
                    for permission in permissions:
                        for c in columns:
                            if check_rights is None:
                                key = (action_name, group, permission, c,)
                                if already_stored.has_key(key):
                                    continue
                                cursor.execute(("insert into e_pytis_action_rights (shortname, roleid, rightid, system, granted, colname) "
                                                "values(%s, %s, %s, %s, %s, %s)"),
                                               (action_name, group, permission, True, True, c,))
                                already_stored[key] = True
                            else:
                                action_info = check_rights[action_name] = check_rights.get(action_name, {})
                                group_info = action_info[group] = action_info.get(group, {})
                                columns = group_info[permission] = group_info.get(permission, [])
                                if c not in columns:
                                    columns.append(c)
                                    columns.sort()
    return roles

def check_rights(cursor, rights):
    app_rights = {}
    fill_rights(cursor, rights, app_rights)
    db_rights = {}
    cursor.execute("select shortname, roleid, rightid, colname from e_pytis_action_rights where system = 'T'")
    while True:
        row = cursor.fetchone()
        if row is None:
            break
        action_name, group, permission, colname = row
        action_info = db_rights[action_name] = db_rights.get(action_name, {})
        group_info = action_info[group] = action_info.get(group, {})
        columns = group_info[permission] = group_info.get(permission, [])
        if colname in columns:
            print 'Check: Multiple permission in the database:', action_name, group, permission, colname
        else:
            columns.append(colname)
            columns.sort()
    for action_name in app_rights.keys():
        db_action_info = db_rights.get(action_name)
        if db_action_info is None:
            print 'Check: Missing action in the database:', action_name
            continue
        app_action_info = app_rights[action_name]
        for group in app_action_info.keys():
            db_group_info = db_action_info.get(group)
            if db_group_info is None:
                print 'Check: Missing group rights in the database:', action_name, group
                continue
            app_group_info = app_action_info[group]
            for permission in app_group_info.keys():
                db_columns = db_group_info.get(permission)
                if columns is None:
                    print 'Check: Missing permission in the database:', action_name, group, permission
                    continue
                if None in db_columns:
                    db_columns = 'ALL'
                app_columns = app_group_info.get(permission)
                if None in app_columns:
                    app_columns = 'ALL'
                if app_columns != db_columns:
                    print 'Check: Different column sets:', action_name, group, permission, app_columns, db_columns
    
def fill_menu_items(cursor, menu, position=''):
    if position:
        position = position + '.' + str(menu.position)
    else:
        position = position + str(menu.position)
    parent = menu.parent and -menu.parent.id
    action = menu.action and menu.action.name
    if menu.system:
        locked = 'T'
    else:
        locked = 'F'
    cursor.execute(("insert into e_pytis_menu "
                    "(menuid, name, title, position, next_position, fullname, help, hotkey, locked) "
                    "values(%s, %s, %s, %s, %s, %s, %s, %s, %s)"),
                   (-menu.id, menu.name, menu.title, menu.position, menu.position+'4', action,
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
    print "Creating actions structure..."
    cursor.execute("select pytis_update_actions_structure()")
    print "Creating actions structure...done"
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
    parser.add_option("--check", action="store_true", dest="check_only")
    parser.add_option("--check-update", action="store_true", dest="check_update")
    options, args = parser.parse_args()
    if options.check_update:
        options.check_only = True
    dbparameters = Configuration.dbparameters
    dbparameters['host'] = options.host
    dbparameters['database'] = options.database
    dbparameters['user'] = options.user
    dbparameters['password'] = options.password
    if (options.delete_only and args or
        not options.delete_only and len(args) != 1):
        parser.print_help()
        sys.exit(1)
    if options.delete_only and options.check_only:
        sys.stderr.write("Error: Can't check and delete at the same time.\n")
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
    check_only = options.check_only
    cursor.execute("set client_encoding to 'latin2'") # grrr
    if not check_only:
        print "Deleting old data..."
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
    top = Menu(name=None, title=_("CELÉ MENU"), parent=None, position='2', action=None, system=True)
    menu_items = {}
    actions = {}
    rights = {}
    print "Retrieving menu..."
    process_menu(resolver, menu, top, menu_items, actions, rights, position='2.1111', system=True)
    print "Retrieving menu...done"
    print "Retrieving rights..."
    process_rights(resolver, actions, rights, def_dir)
    print "Retrieving rights...done"
    if check_only:
        print "Checking actions..."
        check_actions(cursor, actions, options.check_update)
        print "Checking actions...done"
        print "Checking rights..."
        roles = check_rights(cursor, rights)
        print "Checking rights...done"
    else:
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
