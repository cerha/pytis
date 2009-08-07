# -*- coding: utf-8 -*-

"""Gensql definitions for dynamic application menus and access rights."""

db_rights = globals().get('Gall_pytis', None)
if not db_rights:
    raise ProgramError('No rights specified! Please define Gpytis_menu')

_std_table_nolog('e_pytis_disabled_dmp_triggers',
                 (P('id', TUser),),
                 """This table allows disabling some trigger calls.
Supported values (flags) are:
genmenu -- initial insertion and deletion on certain tables
""")

### Roles

_std_table_nolog('c_pytis_role_purposes',
                 (P('purposeid', 'char(4)'),
                  C('purpose', 'varchar(32)', constraints=('unique', 'not null',)),),
                 """There are three kinds of roles:
1. Menu and access administrator roles.  Definitions of these roles may be changed
   only by the database administrators.
2. Roles corresponding to system accounts (login roles).
3. Pure application roles.
""",
                 init_values=(("'admn'", "'Správcovská'",),
                              ("'user'", "'Uživatelská'",),
                              ("'appl'", "'Aplikační'",),),
                 grant=db_rights
                 )

_std_table('e_pytis_roles',
           (P('name', TUser),
            C('description', 'varchar(64)'),
            C('purposeid', 'char(4)', constraints=('not null',), default="'appl'",
              references='c_pytis_role_purposes'),
            C('deleted', TDate),),
            """Application user roles.""",
           init_values=(("'*'", "'Zástupná role pro všechny role'", "'admn'", 'NULL',),
                        ("'admin_roles'", "'Administrátor rolí'", "'admn'", 'NULL',),
                        ("'admin_menu'", "'Administrátor menu'", "'admn'", 'NULL',),
                        ("'admin'", "'Administrátor rolí a menu'", "'admn'", 'NULL',),
                        ),
           grant=db_rights,
           depends=('c_pytis_role_purposes',))
def e_pytis_roles_trigger():
    class Roles(BaseTriggerObject):
        def _pg_escape(self, val):
            return str(val).replace("'", "''")
        def _update_roles(self):
            plpy.execute("select pytis_update_transitive_roles()")
        def _update_rights(self):
            plpy.execute("select pytis_update_summary_rights()")
        def _do_after_insert(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            role = self._pg_escape(self._new['name'])
            plpy.execute("insert into a_pytis_valid_role_members(roleid, member) values ('%s', '%s')" %
                         (role, role,))
            self._update_rights()
        def _do_after_update(self):
            if self._new['deleted'] != self._old['deleted']:
                self._update_roles()
                self._update_rights()
        def _do_after_delete(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            self._update_roles()
            self._update_rights()
    roles = Roles(TD)
    return roles.do_trigger()
_trigger_function('e_pytis_roles_trigger', body=e_pytis_roles_trigger,
                  depends=('e_pytis_roles', 'e_pytis_disabled_dmp_triggers', 'a_pytis_valid_role_members',
                           'pytis_update_transitive_roles', 'pytis_update_summary_rights',))
sql_raw("""
create trigger e_pytis_roles_update_after after insert or update or delete on e_pytis_roles
for each row execute procedure e_pytis_roles_trigger();
""",
        name='e_pytis_roles_triggers',
        depends=('e_pytis_roles_trigger',))

viewng('ev_pytis_valid_roles',
       (SelectRelation('e_pytis_roles', alias='main',
                       condition='main.deleted is null or main.deleted > now()'),
        SelectRelation('c_pytis_role_purposes', alias='codebook', exclude_columns=('purposeid',),
                       condition='main.purposeid = codebook.purposeid', jointype=JoinType.INNER),
        ),
       grant=db_rights,
       depends=('e_pytis_roles', 'c_pytis_role_purposes',)
       )

viewng('ev_pytis_roles',
       (SelectRelation('e_pytis_roles', alias='t1'),
        SelectRelation('c_pytis_role_purposes', alias='t2', exclude_columns=('purposeid',),
                       condition='t1.purposeid = t2.purposeid', jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_roles',),
       update_order=('e_pytis_roles',),
       delete_order=('e_pytis_roles',),
       grant=db_rights,
       depends=('e_pytis_roles', 'c_pytis_role_purposes',)
       )

_std_table('e_pytis_role_members',
           (P('id', TSerial,
              doc="Just to make logging happy"),
            C('roleid', TUser, constraints=('not null',), references='e_pytis_roles on update cascade'),
            C('member', TUser, constraints=('not null',), references='e_pytis_roles on update cascade'),
            ),
           """Mutual memberships of roles.
Entries in this table define `member's of each `roleid'.
""",
           init_values=(('-1', "'admin_roles'", "'admin'",),
                        ('-2', "'admin_menu'", "'admin'",),
                        ),
           grant=db_rights,
           depends=('e_pytis_roles',))
def e_pytis_role_members_trigger():
    class Roles(BaseTriggerObject):
        def _pg_escape(self, val):
            return str(val).replace("'", "''")
        def _update_roles(self):
            plpy.execute("select pytis_update_transitive_roles()")
        def _update_rights(self):
            plpy.execute("select pytis_update_summary_rights()")
        def _do_after_insert(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            self._update_roles()
            self._update_rights()
        def _do_after_update(self):
            self._update_roles()
            self._update_rights()
            if self._new['member'] != self._old['member']:
                self._update_rights()
        def _do_after_delete(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            self._update_roles()
            self._update_rights()
    roles = Roles(TD)
    return roles.do_trigger()
_trigger_function('e_pytis_role_members_trigger', body=e_pytis_role_members_trigger,
                  depends=('e_pytis_role_members', 'e_pytis_disabled_dmp_triggers', 'a_pytis_valid_role_members',
                           'pytis_update_transitive_roles', 'pytis_update_summary_rights',))
sql_raw("""
create trigger e_pytis_role_members_all_after after insert or update or delete on e_pytis_role_members
for each row execute procedure e_pytis_role_members_trigger();
""",
        name='e_pytis_role_members_triggers',
        depends=('e_pytis_role_members_trigger',))

viewng('ev_pytis_valid_role_members',
       (SelectRelation('e_pytis_role_members', alias='main'),
        SelectRelation('ev_pytis_valid_roles', alias='roles1', exclude_columns=('purpose',),
                       condition='roles1.name = main.roleid', jointype=JoinType.INNER),
        SelectRelation('ev_pytis_valid_roles', alias='roles2', exclude_columns=('purpose',),
                       column_aliases=(('name', 'mname',),
                                       ('description', 'mdescription',),
                                       ('purposeid', 'mpurposeid',),
                                       ('deleted', 'mdeleted',),),
                       condition='roles2.name = main.member', jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_role_members',),
       update_order=('e_pytis_role_members',),
       delete_order=('e_pytis_role_members',),
       grant=db_rights,
       depends=('e_pytis_role_members', 'ev_pytis_valid_roles',)
       )

_std_table_nolog('a_pytis_valid_role_members',
                 (C('roleid', TUser, constraints=('not null',), references='e_pytis_roles on delete cascade on update cascade'),
                  C('member', TUser, constraints=('not null',), references='e_pytis_roles on delete cascade on update cascade'),),
                 """Complete membership of roles, including transitive relations.
This table is modified only by triggers.
""",
                 depends=('e_pytis_roles',))
def pytis_update_transitive_roles():
    membership = {}
    for row in plpy.execute("select name from ev_pytis_valid_roles"):
        role = row['name']
        membership[role] = []
    for row in plpy.execute("select name, mname from ev_pytis_valid_role_members"):
        role = row['name']
        member = row['mname']
        membership[member].append(role)
    total_membership = {}
    for role in membership.keys():
        all_roles = []
        new_roles = [role]
        while new_roles:
            r = new_roles.pop()
            if r in all_roles:
                continue
            all_roles.append(r)
            new_roles += membership.get(r, [])
        total_membership[role] = all_roles
    def _pg_escape(val):
        return str(val).replace("'", "''")
    plpy.execute("delete from a_pytis_valid_role_members")
    for role, total_roles in total_membership.items():
        for total in total_roles:
            plpy.execute("insert into a_pytis_valid_role_members (roleid, member) values ('%s', '%s')" %
                         (_pg_escape(total), _pg_escape(role)))
    return True
_plpy_function('pytis_update_transitive_roles', (), TBoolean,
               body=pytis_update_transitive_roles)

### Actions

_std_table_nolog('c_pytis_menu_actions',
                 (P('fullname', TString),
                  C('shortname', TString, constraints=('not null',)),
                  C('description', TString),
                  ),
                 """List of available (pre-defined and visible) application actions.""",
                 grant=db_rights
                 )
def c_pytis_menu_actions_trigger():
    class Menu(BaseTriggerObject):
        def _pg_escape(self, val):
            return str(val).replace("'", "''")
        def _update_all(self):
            plpy.execute("select pytis_update_actions_structure()")
        def _do_after_insert(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu' or id='positions'"):
                return
            self._update_all()
        def _do_after_update(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu' or id='positions'"):
                return
            self._update_all()
        def _do_after_delete(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu' or id='positions'"):
                return
            self._update_all()
    menu = Menu(TD)
    return menu.do_trigger()
_trigger_function('c_pytis_menu_actions_trigger', body=c_pytis_menu_actions_trigger,
                  depends=('c_pytis_menu_actions', 'a_pytis_actions_structure', 'e_pytis_disabled_dmp_triggers',))
sql_raw("""
create trigger c_pytis_menu_actions_all_after_rights after insert or update or delete on c_pytis_menu_actions
for each statement execute procedure c_pytis_menu_actions_trigger();
""",
        name='c_pytis_menu_actions_triggers',
        depends=('c_pytis_menu_actions_trigger',))

def pytis_matching_actions(complex_action, simple_action):
    complex_action = args[0]
    simple_action = args[1]
    if complex_action is None:
        return False
    components = complex_action.split('/')
    if components[0] == 'form':
        last_components = components[2].split('::')
        if len(last_components) == 2:
            import string
            prefix = components[0] + '/'
            result = (prefix + last_components[0] + '::' + last_components[1] == simple_action)
        else:
            result = ('form/'+components[2] == simple_action)
    else:
        result = (complex_action == simple_action)
    return result
_plpy_function('pytis_matching_actions', (TString, TString), TBoolean,
               body=pytis_matching_actions)

### Menus

_std_table('e_pytis_menu',
           (P('menuid', TSerial),
            C('name', TString, constraints=('unique',),
              doc="Unique identifiers of terminal menu items.  NULL for non-terminal items and separators."),
            C('title', 'varchar(64)',
              doc='User title of the item. If NULL then it is a separator.'),
            C('position', TLTree, constraints=('not null', 'unique',),
              doc=("Unique identifier of menu item placement within menu. "
                   "The top-menu item position is ''. "
                   "Each submenu has exactly one label more than its parent. ")),
            C('next_position', TLTree, constraints=('not null', 'unique',),
              doc=("Free position just after this menu item.")),
            C('fullname', TString, references='c_pytis_menu_actions',
              doc=("Application action assigned to the menu item."
                   "Menu items bound to submenus should have this value NULL; "
                   "if they do not, the assigned action is ignored.")),
            C('help', TString,
              doc=("Arbitrary single-line help string.")),
            C('hotkey', TString,
              doc=("Sequence of command keys, separated by single spaces."
                   "The space key is represented by SPC string.")),
            C('locked', TBoolean, default=False,
              doc=("Iff true, this item may not be edited.")),
            ),
           """Menu structure definition.""",
           grant=db_rights,
           depends=('c_pytis_menu_actions',))
def e_pytis_menu_trigger():
    class Menu(BaseTriggerObject):
        def _pg_escape(self, val):
            return str(val).replace("'", "''")
        ## BEFORE
        def _maybe_new_action(self, old=None):
            if not self._new['name'] and self._new['title'] and (old is None or not old['title']):
                # New non-terminal menu item
                self._new['fullname'] = action = 'menu/' + str(self._new['menuid'])
                plpy.execute(("insert into c_pytis_menu_actions (fullname, shortname, description) "
                              "values ('%s', '%s', '%s')") % (action, action, self._pg_escape("Menu '%s'" % (self._new['title'])),))
                self._return_code = self._RETURN_CODE_MODYFY
        def _do_before_insert(self):
            self._maybe_new_action()
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
        def _do_before_update(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='positions'"):
                return
            self._maybe_new_action(old=self._old)
        def _do_before_delete(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            # If there are any children, reject deletion
            data = plpy.execute("select * from e_pytis_menu where position like '%s_%%'" %
                                (self._old['position'],),
                                1)
            if data:
                self._return_code = self._RETURN_CODE_SKIP
        ## AFTER      
        def _update_positions(self, new=None, old=None):
            if old and new and old['position'] == new['position']:
                return
            if old and new:
                old_position = old['position']
                new_position = new['position']
                plpy.execute("update e_pytis_menu set position='%s'||subpath(position, nlevel('%s')) where position <@ '%s'" %
                             (new_position, old_position, old_position,))
            if new:
                data = plpy.execute("select * from e_pytis_menu order by position where position != ''")
                sequences = {}
                for row in data:
                    position = row[i]['position'].split('.')
                    next_position = row[i]['next_position'].split('.')
                    position_length = len(position)
                    if not sequences.has_key(position_length):
                        sequences[position_length] = []
                    sequences[position_length].append((position, next_position,))
                import string
                def update_next_position(position, next_position):
                    plpy.execute("update e_pytis_menu set next_position='%s' where position='%s'" %
                                 (string.join(next_position, '.'), string.join(position, '.'),))
                for position_list in sequences.items():
                    position_list_len = len(position_list)
                    for i in range(position_list_len - 1):
                        position = position_list[i][0]
                        next_position = position_list[i][1]
                        next_item_position = position_list[i+1][0]
                        if (len(position) != len(next_position) or
                            position >= next_position or
                            next_position >= next_item_position):
                            suffix = position[-1]
                            new_suffix = str(long(long() + long(next_item_position[-1]) / 2))
                            if new_suffix == suffix:
                                new_suffix = suffix + '4'
                            next_position = position[:-1] + [new_suffix]
                            update_next_position(position, next_position)
                    last_item = position_list[position_list_len - 1]
                    position, next_position = last_item
                    if (len(position) != len(next_position) or
                        position >= next_position):
                        suffix = position[-1]
                        if suffix[-1] == '9':
                            next_position = position[:-1] + [position[-1] + '4']
                        else:
                            next_position = position[:-1] + [str(long(position[-1]) + 1)]
                        update_next_position(position, next_position)
        def _do_after_insert(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            plpy.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('positions')")
            self._update_positions(new=self._new)
            plpy.execute("delete from e_pytis_disabled_dmp_triggers where id='positions'")
        def _do_after_update(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='positions'"):
                return
            plpy.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('positions')")
            if not self._new['name'] and self._old['title'] and not self._new['title']:
                # Non-terminal item changed to separator
                plpy.execute("delete from c_pytis_menu_actions where fullname = '%s'" % (self._old['fullname'],))
                plpy.execute("delete from e_pytis_action_rights where shortname = '%s'" % (self._old['fullname'],))
            self._update_positions(new=self._new, old=self._old)
            plpy.execute("delete from e_pytis_disabled_dmp_triggers where id='positions'")
        def _do_after_delete(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            plpy.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('positions')")
            if not self._old['name'] and self._old['title']:
                # Non-terminal menu item
                plpy.execute("delete from c_pytis_menu_actions where fullname = '%s'" % (self._old['fullname'],))
                plpy.execute("delete from e_pytis_action_rights where shortname = '%s'" % (self._old['fullname'],))
            self._update_positions(old=self._old)
            plpy.execute("delete from e_pytis_disabled_dmp_triggers where id='positions'")
    menu = Menu(TD)
    return menu.do_trigger()
_trigger_function('e_pytis_menu_trigger', body=e_pytis_menu_trigger,
                  depends=('e_pytis_menu', 'c_pytis_menu_actions', 'e_pytis_disabled_dmp_triggers',))
sql_raw("""
create trigger e_pytis_menu_all_before before insert or update or delete on e_pytis_menu
for each row execute procedure e_pytis_menu_trigger();
create trigger e_pytis_menu_all_after after insert or update or delete on e_pytis_menu
for each row execute procedure e_pytis_menu_trigger();
""",
        name='e_pytis_menu_triggers',
        depends=('e_pytis_menu_trigger',))
def e_pytis_menu_trigger_rights():
    class Menu(BaseTriggerObject):
        def _pg_escape(self, val):
            return str(val).replace("'", "''")
        def _update_all(self):
            plpy.execute("select pytis_update_summary_rights()")
            plpy.execute("select pytis_update_actions_structure()")
        def _do_after_insert(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu' or id='positions'"):
                return
            self._update_all()
        def _do_after_update(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu' or id='positions'"):
                return
            self._update_all()
        def _do_after_delete(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu' or id='positions'"):
                return
            self._update_all()
    menu = Menu(TD)
    return menu.do_trigger()
_trigger_function('e_pytis_menu_trigger_rights', body=e_pytis_menu_trigger_rights,
                  depends=('e_pytis_menu', 'pytis_update_summary_rights', 'e_pytis_disabled_dmp_triggers', 'a_pytis_actions_structure',))
sql_raw("""
create trigger e_pytis_menu_all_after_rights after insert or update or delete on e_pytis_menu
for each statement execute procedure e_pytis_menu_trigger_rights();
""",
        name='e_pytis_menu_triggers_rights',
        depends=('e_pytis_menu_trigger_rights',))

viewng('ev_pytis_menu',
       (SelectRelation('e_pytis_menu', alias='main', exclude_columns=('fullname',)),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('description',),
                       condition='main.fullname = actions.fullname', jointype=JoinType.LEFT_OUTER),
        ),
       insert_order=('e_pytis_menu',),
       update_order=('e_pytis_menu',),
       delete_order=('e_pytis_menu',),
       grant=db_rights,
       depends=('e_pytis_menu', 'c_pytis_menu_actions',)
       )

viewng('ev_pytis_menu_structure',
       (SelectRelation('a_pytis_actions_structure', alias='structure', exclude_columns=('menuid',)),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'fullname', 'position',),
                       condition='structure.menuid = menu.menuid', jointype=JoinType.LEFT_OUTER),
        ),
       insert_order=(),
       update_order=(),
       delete_order=(),
       grant=db_rights,
       depends=('e_pytis_menu', 'a_pytis_actions_structure',)
       )

viewng('ev_pytis_menu_all_positions',
       (SelectSet(Select((SelectRelation('e_pytis_menu', alias='menu1', exclude_columns=('*',),
                                         condition=""),),
                         include_columns=(V(None, 'position', 'position'),))),
        SelectSet(Select((SelectRelation('e_pytis_menu', alias='menu2', exclude_columns=('*',),
                                         condition="position != ''"),),
                         include_columns=(V(None, 'position', "next_position"),)),
                  settype=UNION),
        SelectSet(Select((SelectRelation('e_pytis_menu', alias='menu3', exclude_columns=('*',),
                                         condition="name is NULL and title is not NULL"),),
                         include_columns=(V(None, 'position', "position||'1'"),)),
                  settype=UNION),),
       insert=(),
       update=(),
       delete=(),
       grant=db_rights,
       depends=('e_pytis_menu',))

viewng('ev_pytis_menu_positions',
       (SelectRelation('ev_pytis_menu_all_positions', alias='positions'),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'fullname', 'position', 'hotkey', 'help', 'locked',),
                       condition='positions.position = menu.position', jointype=JoinType.LEFT_OUTER),),
       insert_order=(),
       update_order=(),
       delete_order=(),
       grant=db_rights,
       depends=('ev_pytis_menu', 'ev_pytis_menu_all_positions',)
       )

### Access rights

_std_table_nolog('c_pytis_access_rights',
                 (P('rightid', 'varchar(8)'),
                  C('description', 'varchar(32)', constraints=('not null',)),
                  ),
                 """Available rights.  Not all rights make sense for all actions and menus.""",
                 init_values=(("'show'", _("'Viditelnost položek menu'"),),
                              ("'view'", _("'Prohlížení existujících záznamů'"),),
                              ("'insert'", _("'Vkládání nových záznamů'"),),
                              ("'update'", _("'Editace existujících záznamů'"),),
                              ("'delete'", _("'Mazání záznamů'"),),
                              ("'print'", _("'Tisky'"),),
                              ("'export'", _("'Exporty'"),),
                              ("'call'", _("'Spouštění aplikačních procedur'"),),
                              ),
                 grant=db_rights)

_std_table('e_pytis_action_rights',
           (P('id', TSerial,
              doc="Just to make logging happy"),
            C('shortname', TString, constraints=('not null',)),
            C('roleid', TUser, references='e_pytis_roles', constraints=('not null',)),
            C('rightid', 'varchar(8)', references='c_pytis_access_rights', constraints=('not null',)),
            C('colname', TUser),
            C('system', TBoolean, constraints=('not null',), default=False,
              doc="Iff true, this is a system (noneditable) permission."),
            C('granted', TBoolean, constraints=('not null',), default=True,
              doc="If true the right is granted, otherwise it is denied; system rights are always granted."),
            ),
           """Assignments of access rights to actions.

Extent of each action right is strictly limited by its granted system
permissions.  Non-system rights can only further limit the system rights.  If
there is no system permission for a given action, the action is forbidden.  The
resulting right is not broader than the intersection of all the related
permissions.

Some actions, e.g. dual form actions, form its access rights set by inclusion
of other action rights.

Access rights of non-terminal menu items, identified by action name
'menu/MENUID' define default rights, used when no non-system access right
definition is present for a terminal item.  More nested access rights of
non-terminal menu items have higher precedence.

Action rights are supported and used in the application, but they are not
exposed in the current user interface directly.  In future they may also
support extended rights assignment, e.g. in context menus etc.
""",
           grant=db_rights,
           depends=('c_pytis_menu_actions', 'e_pytis_roles', 'c_pytis_access_rights',)
           )
def e_pytis_action_rights_trigger():
    class Rights(BaseTriggerObject):
        def _pg_escape(self, val):
            return str(val).replace("'", "''")
        def _update_rights(self):
            plpy.execute("select pytis_update_summary_rights()")
        def _do_after_insert(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            self._update_rights()
        def _do_after_update(self):
            self._update_rights()
        def _do_after_delete(self):
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
            self._update_rights()
    rights = Rights(TD)
    return rights.do_trigger()
_trigger_function('e_pytis_action_rights_trigger', body=e_pytis_action_rights_trigger,
                  doc="Updates summary access rights.",
                  depends=('e_pytis_action_rights', 'e_pytis_disabled_dmp_triggers', 'pytis_update_summary_rights',))
sql_raw("""
create trigger e_pytis_action_rights_all_after after insert or update or delete on e_pytis_action_rights
for each statement execute procedure e_pytis_action_rights_trigger();
""",
        name='e_pytis_action_rights_triggers',
        depends=('e_pytis_action_rights_trigger',))

viewng('ev_pytis_user_system_rights',
       (SelectRelation('e_pytis_action_rights', alias='rights',
                       condition="rights.system = 'T' and roleid = '*' or roleid in (select roleid from ev_pytis_user_roles)"),
        ),
       grant=db_rights,
       depends=('e_pytis_action_rights', 'ev_pytis_user_roles',))
    
viewng('ev_pytis_menu_rights',
       (SelectRelation('a_pytis_actions_structure', alias='actions', exclude_columns=('menuid',)),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'position', 'fullname',),
                       condition='menu.fullname = actions.fullname', jointype=JoinType.LEFT_OUTER),
        SelectRelation('e_pytis_action_rights', alias='rights', exclude_columns=('fullname', 'shortname', 'action',),
                       condition='actions.shortname = rights.shortname', jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_action_rights',),
       update_order=('e_pytis_action_rights',),
       delete_order=('e_pytis_action_rights',),
       grant=db_rights,
       depends=('e_pytis_menu', 'e_pytis_action_rights', 'a_pytis_actions_structure',)
       )

### Summarization

_std_table_nolog('a_pytis_computed_summary_rights',
                 (C('shortname', TString, constraints=('not null',)),
                  C('menuid', TInteger, references='e_pytis_menu on delete cascade on update cascade'),
                  C('roleid', TString, constraints=('not null',), references='e_pytis_roles on delete cascade on update cascade'),
                  C('rights', TString, constraints=('not null',)),
                  ),
                 """Precomputed summary access rights as a single line string.
This table is modified only by triggers.
""",
                 depends=('c_pytis_menu_actions', 'e_pytis_roles',))

def pytis_update_summary_rights():
    if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='positions'"):
        return
    import string
    # Retrieve roles
    roles = {}
    for row in plpy.execute("select roleid, member from a_pytis_valid_role_members"):
        roleid, member = row['roleid'], row['member']
        members = roles.get(member)
        if members is None:
            roles[member] = members = []
        members.append(roleid)
    # Retrieve rights
    class RawRights(object):
        def __init__(self):
            self.system = []
            self.allowed = []
            self.forbidden = []
    raw_rights = {}
    for row in plpy.execute("select rightid, granted, roleid, menuid, shortname, system from ev_pytis_menu_rights"):
        rightid, granted, roleid, menuid, action, system = row['rightid'], row['granted'], row['roleid'], row['menuid'], row['shortname'], row['system']
        keys = [(action, menuid,)]
        if menuid is not None:
            keys.append((action, None,))
        for key in keys:
            key = (action, menuid,)
            item_rights = raw_rights.get(key)
            if item_rights is None:
                raw_rights[key] = item_rights = {}
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
    class Rights(object):
        def __init__(self, total, allowed, forbidden, parent):
            self.total = total
            self.allowed = allowed
            self.forbidden = forbidden
            self.parent = parent
    computed_rights = {}
    position2parent = {}
    menuid2action = {}
    action2menuids = {}
    for row in plpy.execute("select menuid, e_pytis_menu.name, position, c_pytis_menu_actions.fullname, shortname "
                            "from c_pytis_menu_actions left outer join e_pytis_menu "
                            "on c_pytis_menu_actions.fullname = e_pytis_menu.fullname "
                            "order by position"):
        menuid, name, position, action = row['menuid'], row['name'], row['position'], row['shortname']
        menu_rights = raw_rights.get((action, menuid,), {})
        if menuid:
            position2parent[position] = menuid
            menuid2action[menuid] = action
            action_menuids = action2menuids.get(action, [])
            action_menuids.append(menuid)
            action2menuids[action] = action_menuids
        if position:
            parent = position2parent[string.join(position.split('.')[:-1], '.')]
        else:
            parent = None
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
                if r not in forbidden_rights and r not in allowed_rights:
                    forbidden_rights.append(r)
            for r in raw.allowed:
                if r not in forbidden_rights and r not in allowed_rights:
                    allowed_rights.append(r)
            if name:
                if not max_rights:
                    for r in menu_rights.values():
                        if r.system:
                            break
                    else:
                        max_rights = ['view', 'insert', 'update', 'delete', 'print', 'export', 'call']
            else:
                max_rights = None
            parent_menuid = parent
            while parent_menuid is not None:
                parent_rights = computed_rights[(parent_menuid, roleid,)]
                for r in parent_rights.forbidden:
                    if r not in forbidden_rights and (r == 'show' or r not in allowed_rights):
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
            computed_rights[(menuid or action, roleid,)] = Rights(total=rights, allowed=allowed_rights, forbidden=forbidden_rights, parent=parent)
    # Insertion of new rights to the database takes most of the time, so we make only real changes
    old_rights = {}
    for row in plpy.execute("select shortname, menuid, roleid, rights from a_pytis_computed_summary_rights"):
        old_rights[(row['shortname'], row['menuid'], row['roleid'],)] = row['rights']
    def _pg_escape(val):
        return str(val).replace("'", "''")
    for short_key, all_rights in computed_rights.items():
        menuid_or_action, roleid = short_key
        if isinstance(menuid_or_action, int):
            menuid = menuid_or_action
            action = menuid2action[menuid]
        else:
            menuid = None
            action = menuid_or_action
            total = all_rights.total
            for mid in action2menuids.get(action, []):
                for r in computed_rights[(mid, roleid,)].total:
                    if r not in total:
                        total.append(r)
            all_rights.total = total
        key = action, menuid, roleid
        rights = string.join(all_rights.total, ' ')
        old_item_rights = old_rights.get(key)
        if rights != old_item_rights:
            if old_item_rights is None:
                plpy.execute("insert into a_pytis_computed_summary_rights (shortname, menuid, roleid, rights) values('%s', %s, '%s', '%s')" %
                             (_pg_escape(action), menuid or "NULL", _pg_escape(roleid), rights,))
            else:
                plpy.execute("update a_pytis_computed_summary_rights set rights='%s' where shortname='%s' and menuid=%s and roleid='%s'" %
                             (rights, _pg_escape(action), menuid or "NULL", _pg_escape(roleid),))
        try:
            del old_rights[key]
        except KeyError:
            pass
    for action, menuid, roleid in old_rights.keys():
        plpy.execute("delete from a_pytis_computed_summary_rights where shortname='%s' and menuid=%s and roleid='%s'" %
                     (_pg_escape(action), menuid or "NULL", _pg_escape(roleid),))
_plpy_function('pytis_update_summary_rights', (), TBoolean,
               body=pytis_update_summary_rights,
               depends=('a_pytis_computed_summary_rights', 'a_pytis_valid_role_members', 'ev_pytis_menu_rights', 'e_pytis_menu',),)

_std_table_nolog('a_pytis_actions_structure',
                 (C('fullname', TString, constraints=('not null',)),
                  C('shortname', TString, constraints=('not null',)),
                  C('menuid', TInteger),
                  C('position', TString, constraints=('not null',)),
                  ),
                 """Precomputed actions structure as presented to menu admin.
Item positions and indentations are determined by positions.
This table is modified only by triggers.
""",
                 depends=())
sql_raw("create index a_pytis_actions_structure_index on a_pytis_actions_structure(position);",
        depends=('a_pytis_actions_structure',))

def pytis_update_actions_structure():
    import string
    def _pg_escape(val):
        return str(val).replace("'", "''")
    plpy.execute("delete from a_pytis_actions_structure")
    actions = {}
    def add_row(fullname, action, menuid, position):
        if menuid is None:
            menuid = 'NULL'
        plpy.execute("insert into a_pytis_actions_structure (fullname, shortname, menuid, position) values('%s', '%s', %s, '%s')" %
                     (_pg_escape(fullname), _pg_escape(action), menuid, position,))
        actions[action] = True
    for row in plpy.execute("select menuid, position, c_pytis_menu_actions.fullname, shortname from e_pytis_menu, c_pytis_menu_actions "
                            "where e_pytis_menu.fullname = c_pytis_menu_actions.fullname "
                            "order by position"):
        menuid, position, action, fullname = row['menuid'], row['position'], row['shortname'], row['fullname']
        add_row(fullname, action, menuid, position)
        action_components = action.split('/')
        if action_components[0] == 'form':
            specifications = action_components[1].split('::')
            if len(specifications) == 2:
                for i in range(len(specifications)):
                    subaction = 'form/' + specifications[i]
                    subposition = '%s%02d' % (position, i,)
                    add_row(subaction, subaction, None, subposition)
    position = str(int(position[:2]) + 1) + '.0001'
    for row in plpy.execute("select distinct shortname from c_pytis_menu_actions order by shortname"):
        action = row['shortname']
        if actions.has_key(action):
            continue
        add_row(action, action, None, position)
        position_components = position.split('.')
        position = string.join(position_components[:-1] + ['%04d' % (int(position_components[-1]) + 1)], '.')
_plpy_function('pytis_update_actions_structure', (), TBoolean,
               body=pytis_update_actions_structure,
               depends=('a_pytis_actions_structure', 'e_pytis_menu', 'c_pytis_menu_actions',),)
    
viewng('ev_pytis_summary_rights_raw',
       (SelectRelation('a_pytis_actions_structure', alias='structure', exclude_columns=('position',)),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'position', 'fullname', 'menuid',),
                       condition="structure.menuid = menu.menuid", jointype=JoinType.LEFT_OUTER),
        SelectRelation('ev_pytis_valid_roles', alias='roles', exclude_columns=('description', 'purposeid', 'deleted',),
                       jointype=JoinType.CROSS),
        SelectRelation('a_pytis_computed_summary_rights', alias='summary', exclude_columns=('shortname', 'roleid', 'menuid',),
                       condition="structure.shortname = summary.shortname and roles.name = summary.roleid" , jointype=JoinType.INNER),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('e_pytis_menu', 'ev_pytis_valid_roles', 'a_pytis_computed_summary_rights',)
       )

viewng('ev_pytis_summary_rights',
       (SelectRelation('ev_pytis_summary_rights_raw', alias='rights', condition='rights is not null'),),
       include_columns=(V(None, 'rights_show', "strpos(rights, 'show')::bool"),
                        V(None, 'rights_view', "strpos(rights, 'view')::bool"),
                        V(None, 'rights_insert', "strpos(rights, 'insert')::bool"),
                        V(None, 'rights_update', "strpos(rights, 'update')::bool"),
                        V(None, 'rights_delete', "strpos(rights, 'delete')::bool"),
                        V(None, 'rights_print', "strpos(rights, 'print')::bool"),
                        V(None, 'rights_export', "strpos(rights, 'export')::bool"),
                        V(None, 'rights_call', "strpos(rights, 'call')::bool"),
                        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('ev_pytis_summary_rights_raw',)
       )

viewng('ev_pytis_role_menu_raw',
       (SelectRelation('ev_pytis_menu', alias='menu', exclude_columns=('name', 'fullname',)),
        SelectRelation('ev_pytis_valid_roles', alias='roles', exclude_columns=('description', 'purposeid', 'deleted',),
                       jointype=JoinType.CROSS,
                       column_aliases=(('name', 'roleid',),),
                       ),
        SelectRelation('a_pytis_computed_summary_rights', alias='summary', exclude_columns=('menuid', 'roleid', 'shortname',),
                       condition="menu.menuid = summary.menuid and roles.name = summary.roleid" , jointype=JoinType.INNER),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('ev_pytis_menu', 'ev_pytis_valid_roles', 'a_pytis_computed_summary_rights',)
       )

viewng('ev_pytis_role_menu',
       (SelectRelation('ev_pytis_role_menu_raw', alias='menu', condition="rights like '%show%'"),),
       include_columns=(V(None, 'rights_show', "strpos(rights, 'show')::bool"),
                        V(None, 'rights_view', "strpos(rights, 'view')::bool"),
                        V(None, 'rights_insert', "strpos(rights, 'insert')::bool"),
                        V(None, 'rights_update', "strpos(rights, 'update')::bool"),
                        V(None, 'rights_delete', "strpos(rights, 'delete')::bool"),
                        V(None, 'rights_print', "strpos(rights, 'print')::bool"),
                        V(None, 'rights_export', "strpos(rights, 'export')::bool"),
                        V(None, 'rights_call', "strpos(rights, 'call')::bool"),
                        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('ev_pytis_role_menu_raw',)
       )

viewng('ev_pytis_user_menu',
       (SelectRelation('e_pytis_menu', alias='menu'),
        SelectRelation('a_pytis_computed_summary_rights', alias='rights', exclude_columns=('menuid', 'roleid', 'shortname',),
                       condition=("menu.menuid = rights.menuid and "
                                  "rights.roleid = user and "
                                  "rights.rights like '%show%'"),
                       jointype=JoinType.INNER),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('e_pytis_menu',)
       )

viewng('ev_pytis_user_rights',
       (SelectRelation('a_pytis_computed_summary_rights', alias='rights', exclude_columns=('menuid', 'roleid', 'shortname',),
                       condition="rights.roleid = user"),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('*'),
                       condition="rights.menuid = menu.menuid", jointype=JoinType.INNER),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('fullname', 'description',),
                       condition="menu.fullname = actions.fullname", jointype=JoinType.INNER),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('a_pytis_computed_summary_rights', 'e_pytis_menu', 'c_pytis_menu_actions',)
       )

viewng('ev_pytis_user_roles',
       (SelectRelation('a_pytis_valid_role_members', alias='members', exclude_columns=('member'),
                       condition="members.member = user"),
        SelectRelation('e_pytis_roles', alias='roles', exclude_columns=('*'),
                       condition="members.member = roles.name and roles.purposeid = 'user'", jointype=JoinType.INNER),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('a_pytis_valid_role_members',))
