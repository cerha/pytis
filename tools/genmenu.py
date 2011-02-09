#!/usr/bin/env python
# -*- coding: iso-8859-2 -*-

# Copyright (C) 2009, 2010, 2011 Brailcom, o.p.s.
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

import collections
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

import pytis.data.postgresql
pg_escape = pytis.data.postgresql.pg_escape

_current_form_name = None
def is_in_groups(groups):
    print "Warning: is_in_groups called in %s for groups %s" % (_current_form_name, groups,)
    return True
pytis.extensions.is_in_groups = is_in_groups

def run_procedure_mitem(title, name, proc_name, hotkey=None, groups=None, enabled=None, **kwargs):
    cmd = pytis.form.Application.COMMAND_RUN_PROCEDURE
    if groups is not None:
        assert isinstance(groups, (tuple, list))
        assert enabled is None or isinstance(enabled, collections.Callable)
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
    while menu_id in menu_items:
        menu_id = base_id + str(i)
        i += 1
    return menu_id

def command_form(resolver, form_string):
    try:
        command, args = resolver.get('app_commands', form_string)
        form_class = args['form_class']
        if not issubclass(form_class, pytis.form.Form):
            raise Exception()
        form_name = args['name']
    except:
        print 'Warning: Failed to retrieve RUN_FORM command: %s' % (form_string,)
        return None, None
    return form_name, form_class

def process_menu(resolver, menu, parent, menu_items, actions, rights, position, system=False):
    def process_spec(form_name, form_class, action_id, subactions):
        shortname = 'form/' + form_name
        form_name_components = form_name.split('.')
        form_module = string.join(form_name_components[:-1], '/')
        base_form_name = form_name_components[-1]
        try:
            spec = resolver.get_object(form_module, base_form_name)
        except:
            spec = None
        if spec is None:
            spec_instance = None
        else:
            try:
                spec_instance = spec(resolver)
            except Exception, e:
                spec_instance = None
                print "Error: Can't create specification instance to get title of %s: %s" % (spec.__name__, e,)
        if spec_instance is not None:
            try:
                spec_title = spec_instance.view_spec().title()
            except:
                pass
        # Subforms
        bindings = None
        def binding(name):
            form_name_components = name.split('.')
            form_module = string.join(form_name_components[:-1], '/')
            base_form_name = form_name_components[-1]
            try:
                spec = resolver.get_object(form_module, base_form_name)
            except:
                spec = None
            if spec is None:
                title = ''
            else:
                try:
                    title = spec(resolver).view_spec().title()
                except Exception, e:
                    title = ''
                    print "Error: Can't create specification instance to get title of %s: %s" % (spec.__name__, e,)
            return pytis.presentation.Binding(id=name, title=title, name=name,
                                              binding_column='dummy')
        if issubclass(form_class, pytis.form.DualForm):
            pos = form_name.find('::')
            if pos == -1:
                try:
                    spec = resolver.get_object(form_module, base_form_name)
                    bindings = spec(resolver).view_spec().bindings()
                    bindings = (binding(form_name),) + tuple(bindings)
                except Exception, e:
                    bindings = None
                    print "Warning: Can't import bindings of %s: %s" % (form_name, e,)
            else:
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
        return spec_instance
    if isinstance(menu, basestring):
        subactions = []
        components = menu.split('/')
        if len(components) != 5:
            print 'Error: Invalid specification fullname:', menu
            return
        __, form_class_name, form_name, __, __ = components
        form_class = eval(form_class_name)
        action_id = menu
        shortname = 'form/%s' % (form_name,)
        spec_instance = process_spec(form_name, form_class, action_id, subactions)
        spec_title = spec_instance.view_spec().title()
        description = spec_instance.view_spec().description() or ''
        actions[action_id] = Action(name=action_id, description=description, shortname=shortname,
                                    title=spec_title, subactions=subactions)
    elif isinstance(menu, pytis.form.Menu):
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
            if action_kind == 'form' or action_kind == 'RUN_FORM':
                if action_kind == 'RUN_FORM':
                    form_name, form_class = command_form(resolver, action_components[1])
                else:
                    form_name = action_components[2]
                    form_class = eval(action_components[1])
                process_spec(form_name, form_class, action_id, subactions)
                shortname = 'form/' + form_name
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
        while menu_id in menu_items:
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
        print 'Error: Unknown menu item: %s' % (menu,)

def process_form_actions(resolver, actions, rights):
    for action in actions.values():
        shortname = action.shortname
        components = shortname.split('/')
        if components[0] == 'form':
            form_name = components[1]
            form_name_components = form_name.split('.')
            form_module = string.join(form_name_components[:-1], '/')
            base_form_name = form_name_components[-1]
            try:
                spec = resolver.get_object(form_module, base_form_name)
            except:
                continue
        if hasattr(spec, 'actions'):
            try:
                spec_instance = spec(resolver)
            except:
                spec_instance = None
                print "Error: Can't create specification instance to get form actions of %s" % (spec.__name__,)
            if spec_instance is not None:
                form_actions = []
                def add_form_actions(actions):
                    for a in actions:
                        if isinstance(a, pytis.presentation.Action):
                            form_actions.append(a)
                        elif isinstance(a, pytis.presentation.ActionGroup):
                            add_form_actions(a.actions())
                        elif pytis.util.is_sequence(a):
                            add_form_actions(a)                                    
                        else:
                            print "Error: Unknown form action class in %s: %s" % (spec.__name__, a,)
                add_form_actions(spec_instance.view_spec().actions())
                for a in form_actions:
                    form_action_id = 'action/%s/%s' % (a.id(), form_name,)
                    actions[form_action_id] = action = Action(form_action_id, a.descr(),
                                                              shortname=form_action_id,
                                                              title=a.title(raw=True))
                    formaction_rights = (None, (a.access_groups(), pytis.data.Permission.CALL,),)
                    rights[form_action_id] = Rights(pytis.data.AccessRights(formaction_rights), action)

def process_rights(resolver, actions, rights, def_dir, spec_fullname):
    def add_rights(form_name, action, action_name):
        global _current_form_name
        if form_name.find(':') != -1:
            if form_name.find('::') == -1:
                print "Error: Unhandled obsolete form specification %s" % (form_name,)
            return
        pos = form_name.rfind('.')
        if pos == -1:
            print 'Note: Not caring about access rights of form %s' % (form_name,)
            return
        module_name = form_name[:pos].replace('.', '/')
        class_name = form_name[pos+1:]
        _current_form_name = form_name
        try:
            form_spec = resolver.get_object(module_name, class_name)
            form_spec_instance = form_spec(resolver)
            access_rights = form_spec_instance.data_spec().access_rights()
        except Exception, e:
            print "Error: Couldn't get access rights for form %s: %s" % (form_name, e,)
            return
        if access_rights is None:
            print "Note: No access rights specified for form %s, assuming everything permitted" % (form_name,)
            access_rights = pytis.data.AccessRights((None, (None, pytis.data.Permission.ALL)),)
        rights[action_name] = Rights(access_rights, action)
    action_list = actions.values()
    if spec_fullname is None:
        action_list = action_list + [Action('form/*/menu.ApplicationMenu', '', 'form/menu.ApplicationMenu'),
                                     Action('form/*/menu.ApplicationMenuM', '', 'form/menu.ApplicationMenuM'),
                                     Action('form/*/menu.ApplicationRoles', '', 'form/menu.ApplicationRoles'),
                                     ]
    for action in action_list:
        action_name = action.name
        if action_name in rights:
            continue
        action_components = action.name.split('/')
        if len(action_components) == 2 and action_components[0] == 'RUN_FORM':
            form_name, form_class = command_form(resolver, action_components[1])
        elif action_components[0] == 'NEW_RECORD':
            form_name = action_components[1]
        elif action_components[0] == 'form':
            form_name = action_components[2]
        elif action_components[0] == 'sub':
            continue
        else:
            print "Note: Non-form action, no rights assigned: %s" % (action_name,)
            continue
        form_components = form_name.split('::')
        if len(form_components) <= 2:
            add_rights(form_name, action, action_name)
    if spec_fullname is not None:
        return rights
    actions_shortnames = {}
    for a in actions.values():
        if a.name.find('::') == -1 and a.name[:4] != 'sub/':
            actions_shortnames[a.shortname] = True
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
                    print 'Warning: Module not loaded: %s' % (module_name,)
                    continue
                module_identifier = module_name.replace('/', '.')
                for spec_attr in [o for o in dir(module)]:
                    spec = getattr(module, spec_attr)
                    if isinstance(spec, type) and issubclass(spec, pytis.form.Specification) and spec.public:
                        if spec_attr[0] == '_':
                            print 'Warning: Public specification starting with underscore: %s.%s' % (module_identifier, spec_attr,)
                        spec_name = module_identifier + '.' + spec.__name__
                        action_shortname = 'form/'+spec_name
                        if action_shortname in actions_shortnames:
                            try:
                                del actions_extra_shortnames[action_shortname]
                            except KeyError:
                                pass
                        else:
                            actions_shortnames[action_shortname] = True
                            action_name = 'form/*/'+spec_name
                            try:
                                spec_title = spec.title
                            except:
                                spec_title = None
                            actions[action_name] = action = Action(action_name, '', action_shortname,
                                                                   title=spec_title)
                            add_rights(spec_name, action, action_name)
                    elif isinstance(spec, type) and issubclass(spec, pytis.form.Specification) and spec_attr != 'Specification':
                        print 'Note: Private specification, ignored: %s.%s' % (module_identifier, spec_attr,)
    actions['label/1'] = Action('label/1', None, title="SAMOSTATNÉ AKCE")
    if actions_extra_shortnames:
        print 'Warning: Actions without met specifications: %s' % (actions_extra_shortnames.keys(),)
    return rights

def fill_actions(cursor, actions):
    for action in actions.values():
        cursor.execute("insert into c_pytis_menu_actions (fullname, shortname, action_title, description) values (%s, %s, %s, %s)",
                       (action.name, action.shortname, action.title, action.description,))

def check_actions(cursor, actions, update, spec_fullname):
    subactions = {}
    if spec_fullname is None:
        sub_pattern = 'sub/%'
        condition = 'true'
    else:
        specification = spec_fullname.split('/')[2]
        sub_pattern = 'sub/%%/form/%%/%s/%%' % (specification,)
        condition = "shortname like '%%/%s'" % (specification,)
    query = "select fullname, shortname from c_pytis_menu_actions where fullname like '%s'" % (sub_pattern,)
    cursor.execute(query)
    while True:
        row = cursor.fetchone()
        if row is None:
            break
        fullname, shortname = row
        index = fullname[4:6]
        parent_name = fullname[7:]
        subactions_dict = subactions.get(parent_name)
        if subactions_dict is None:
            subactions_dict = subactions[parent_name] = {}
        subactions_dict[shortname] = index
    cursor.execute("select fullname, shortname, action_title from c_pytis_menu_actions where %s" % (condition,))
    missing_actions = copy.copy(actions)
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
            del missing_actions[fullname]
            db_subactions_dict = subactions.get(fullname, {})
            subactions_list = action.subactions
            subactions_dict = {}
            for i in range(len(subactions_list)):
                key = actions[subactions_list[i]].shortname
                subactions_dict[key] = '%02d' % (i,)
            for sub_shortname, index in subactions_dict.items():
                if db_subactions_dict.get(sub_shortname) is not None:
                    del subactions_dict[sub_shortname]
                    del db_subactions_dict[sub_shortname]
            for sub_shortname, index in db_subactions_dict.items():
                print 'Check: Extra subaction: %s %s %s' % (fullname, index, sub_shortname,)
                if update:
                    print ("Update: delete from c_pytis_menu_actions where fullname='sub/%s/%s';" %
                           (index, fullname,))
            for sub_shortname, index in subactions_dict.items():
                print 'Check: Missing subaction: %s %s %s' % (fullname, index, sub_shortname,)
                if update:
                    subaction = actions['sub/%s/%s' % (index, fullname,)]
                    sub_name = ('sub/%02d' % (i,)) + subaction.name[6:]
                    print ("Update: insert into c_pytis_menu_actions (fullname, shortname, action_title, description) values ('%s', '%s', '%s', '%s');" %
                           (sub_name, subaction.shortname, subaction.title, subaction.description,))
            if not title and action.title:
                missing_titles.append((fullname, action.title,))
    for fullname, action in missing_actions.items():
        if fullname[:4] != 'sub/':
            print 'Check: Missing action: %s (%s)' % (fullname, action.shortname,)
            if update:
                print ("Update: insert into c_pytis_menu_actions (fullname, shortname, action_title, description) values ('%s', '%s', '%s', '%s');" %
                       (action.name, action.shortname, action.title, (action.description or ''),))
                subactions_list = action.subactions
                for i in range(len(subactions_list)):
                    subaction_id = subactions_list[i]
                    subaction = actions[subaction_id]
                    print ("Update: insert into c_pytis_menu_actions (fullname, shortname, action_title, description) values ('%s', '%s', '%s', '%s');" %
                           (subaction.name, subaction.shortname, subaction.title, subaction.description,))
    for fullname, title in missing_titles:
        print 'Check: Missing action title in %s: %s' % (fullname , title,)
        if update:
            print "Update: update c_pytis_menu_actions set action_title='%s' where fullname='%s';" % (pg_escape(title), pg_escape(fullname),)

def fill_rights(cursor, rights, check_rights=None, spec_name=None):
    already_stored = {}
    roles = {}
    for r in ('*', 'admin', 'admin_menu', 'admin_roles',):
        roles[r] = None
    already_stored = {}
    for right in rights.values():
        action = right.action
        action_name = action.shortname
        if spec_name is not None and action_name != ('form/%s' % (spec_name,)):
            continue
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
                for group in groups:
                    if check_rights is None:
                        if group and group not in roles:
                            cursor.execute(("insert into e_pytis_roles (name, description, purposeid) "
                                            "values (%s, %s, %s)"),
                                           (group, "", 'appl',))
                            roles[group] = None
                    for permission in permissions:
                        for c in columns:
                            if check_rights is None:
                                key = (action_name, group, permission, c,)
                                if key in already_stored:
                                    continue
                                cursor.execute(("insert into e_pytis_action_rights (shortname, roleid, rightid, system, granted, colname, status) "
                                                "values (%s, %s, %s, %s, %s, %s, 0)"),
                                               (action_name, group, permission, True, True, c,))
                                already_stored[key] = True
                            else:
                                action_info = check_rights[action_name] = check_rights.get(action_name, {})
                                group_info = action_info[group] = action_info.get(group, {})
                                permitted_columns = group_info[permission] = group_info.get(permission, [])
                                if c not in permitted_columns:
                                    permitted_columns.append(c)
                                    permitted_columns.sort()
    return roles

def check_rights(cursor, rights, update, spec_fullname):
    app_rights = {}
    if spec_fullname is None:
        condition = "true"
    else:
        spec_name = spec_fullname.split('/')[2]
        if update:
            condition = "false"
            print "Update: delete from e_pytis_action_rights where shortname='form/%s'" % (spec_name,)
        else:
            condition = "shortname='form/%s'" % (spec_name,)
    fill_rights(cursor, rights, app_rights, spec_name)
    db_rights = {}
    cursor.execute("select id, shortname, roleid, rightid, colname from e_pytis_action_rights where system = 'T' and status<=0 and %s" % (condition,))
    while True:
        row = cursor.fetchone()
        if row is None:
            break
        row_id, action_name, group, permission, colname = row
        action_info = db_rights[action_name] = db_rights.get(action_name, {})
        group_info = action_info[group] = action_info.get(group, {})
        columns = group_info[permission] = group_info.get(permission, [])
        if colname in columns:
            print 'Check: Multiple permission in the database:', action_name, group, permission, colname
            if update:
                print "Update: delete from e_pytis_action_rights where id=%s;" % (row_id,)
        else:
            columns.append(colname)
            columns.sort()
    for action_name in app_rights.keys():
        db_action_info = db_rights.get(action_name)
        if db_action_info is None:
            print 'Check: Missing action rights:', action_name
            missing = True
        else:
            missing = False
        rights_seen = {}
        app_action_info = app_rights[action_name]
        for group in app_action_info.keys():
            if not missing:
                db_group_info = db_action_info.get(group)
                if db_group_info is None:
                    print 'Check: Missing group rights:', action_name, group
                    continue
            app_group_info = app_action_info[group]
            for permission, columns in app_group_info.items():
                if missing:
                    for c in (columns or [None]):
                        if c is None:
                            cc = 'NULL'
                        else:
                            cc = "'%s'" % (c,)
                        if group is None:
                            gg = '*'
                        else:
                            gg = group
                        rights_args = (pg_escape(action_name), pg_escape(gg), permission, cc,)
                        if rights_args in rights_seen:
                            continue
                        if update:
                            print (("Update: insert into e_pytis_action_rights (shortname, roleid, rightid, system, granted, colname, status) "
                                    "values ('%s', '%s', '%s', 't', 't', %s, 0);") % rights_args)
                        rights_seen[rights_args] = True
                    continue
                db_columns = db_group_info.get(permission)
                if db_columns is None:
                    print 'Check: Missing permission:', action_name, group, permission
                    continue
                if None in db_columns:
                    db_columns = 'ALL'
                app_columns = app_group_info.get(permission)
                if None in app_columns:
                    app_columns = 'ALL'
                if app_columns != db_columns:
                    print 'Check: Different column sets:', action_name, group, permission, \
                        'app='+str(app_columns), 'db='+str(db_columns)
    
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
                    "values (%s, %s, %s, %s, %s, %s, %s, %s, %s)"),
                   (-menu.id, menu.name, menu.title, menu.position, menu.position+'4', action,
                     menu.help, menu.hotkey, locked,))
    for m in menu.children:
        fill_menu_items(cursor, m, position=position)

def add_menu_item(actions, spec_fullname, position):
    action = actions[spec_fullname]
    print "Update: delete from e_pytis_menu where fullname='%s'" % (spec_fullname,)
    print "Update: insert into e_pytis_menu (name, title, position, fullname, help, locked) values ('%s', '%s', '%s', '%s', '%s', False)" % (action.name, action.title, position, spec_fullname, action.description,)

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
            if role not in present_roles:
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
            
def parse_options():
    usage = """usage: %prog [options] DEF_DIRECTORY"""
    parser = optparse.OptionParser(usage)
    parser.add_option("-H", "--host", default=None, action="store", dest="host")
    parser.add_option("-d", "--database", default=None, action="store", dest="database")
    parser.add_option("-U", "--user", default=None, action="store", dest="user")
    parser.add_option("-P", "--password", default=None, action="store", dest="password")
    parser.add_option("--check", action="store_true", dest="check_only",
                      help="Only check DMP data, do not modify the database")
    parser.add_option("--check-update", action="store_true", dest="check_update",
                      help="The same as --check, but print SQL commands needed for update")
    parser.add_option("--check-spec", default=None, action="store", dest="check_spec",
                      help="Check specification identified by given fullname and print SQL commands for update")
    parser.add_option("--position", default=None, action="store", dest="position",
                      help="Position within the menu, useful only with --check-spec")
    parser.add_option("--reset-rights", action="store_true", dest="reset_rights",
                      help="Reset access rights of given specification, useful only with --check-spec")
    parser.add_option("--rebuild", action="store_true", dest="rebuild",
                      help="Delete all DMP data and import them from DEF_DIRECTORY specifications again")
    parser.add_option("--delete", action="store_true", dest="delete_only",
                      help="Just delete everything from DMP tables and exit")
    options, args = parser.parse_args()
    if options.check_update or options.check_spec:
        options.check_only = True
    dbparameters = Configuration.dbparameters
    dbparameters['host'] = options.host
    dbparameters['database'] = options.database
    dbparameters['user'] = options.user
    dbparameters['password'] = options.password
    if (options.delete_only and args or
        not options.delete_only and len(args) != 1 or
        not options.delete_only and not options.rebuild and not options.check_only and not options.check_update):
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
    import config               # this is pytis virtual module
    config.dbconnection = pytis.data.DBConnection(**parameters)
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
                                                                        pytis.form.MultiBrowseDualForm),
                                        pytis.extensions.run_procedure_mitem(_("Aplikace změn práv"),
                                                                             'menu.ApplicationMenuRights', 'commit_changes'),
                                        pytis.form.MItem(_("Přenačtení menu a práv"),
                                                         command=pytis.form.Application.COMMAND_RELOAD_RIGHTS),
                                        )),)
                      + menu[0]._items)
    top = Menu(name=None, title=_("CELÉ MENU"), parent=None, position='2', action=None, system=True)
    menu_items = {}
    actions = {}
    rights = {}
    print "Retrieving menu..."
    process_menu(resolver, (options.check_spec or menu), top, menu_items, actions, rights, position='2.1111', system=True)
    process_form_actions(resolver, actions, rights)
    print "Retrieving menu...done"
    if options.check_spec is None or options.reset_rights:
        print "Retrieving rights..."
        process_rights(resolver, actions, rights, def_dir, options.check_spec)
        print "Retrieving rights...done"
    if check_only:
        print "Checking actions..."
        check_actions(cursor, actions, options.check_update, options.check_spec)
        print "Checking actions...done"
        if options.check_spec is None or options.reset_rights:
            print "Checking rights..."
            roles = check_rights(cursor, rights, options.check_update, options.check_spec)
            print "Checking rights...done"
        if options.check_spec is not None and options.position is not None and options.check_update:
            add_menu_item(actions, options.check_spec, options.position)
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
