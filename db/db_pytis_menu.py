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
              references='c_pytis_role_purposes on update cascade'),
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
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
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
Entries in this table define members of each roleid.
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
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
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
        SelectRelation('ev_pytis_valid_roles', alias='roles1',
                       condition='roles1.name = main.roleid', jointype=JoinType.INNER),
        SelectRelation('ev_pytis_valid_roles', alias='roles2',
                       column_aliases=(('name', 'mname',),
                                       ('description', 'mdescription',),
                                       ('purposeid', 'mpurposeid',),
                                       ('purpose', 'mpurpose',),
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

_std_table_nolog('c_pytis_action_types',
                 (P('type', 'char(4)'),
                  C('description', TString),
                  ),
                 """List of defined action types.""",
                 init_values=(("'----'", _("''"),),
                              ("'item'", _("'Položka menu'"),),
                              ("'menu'", _("'Menu'"),),
                              ("'sepa'", _("'Separátor'"),),
                              ("'spec'", _("'Specifikace'"),),
                              ("'subf'", _("'Podformulář'"),),
                              ("'proc'", _("'Procedura'"),),
                              ),
                 grant=db_rights
                 )
_std_table_nolog('c_pytis_menu_actions',
                 (P('fullname', TString),
                  C('shortname', TString, constraints=('not null',)),
                  C('action_title', TString),
                  C('description', TString),
                  ),
                 """List of available application actions.""",
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
                  depends=('c_pytis_menu_actions', 'pytis_update_actions_structure', 'e_pytis_disabled_dmp_triggers',))
sql_raw("""
create trigger c_pytis_menu_actions_all_after_rights after insert or update or delete on c_pytis_menu_actions
for each statement execute procedure c_pytis_menu_actions_trigger();
""",
        name='c_pytis_menu_actions_triggers',
        depends=('c_pytis_menu_actions_trigger',))

sql_raw("""
create or replace view ev_pytis_short_actions
as select distinct shortname from c_pytis_menu_actions;
""",
        name='ev_pytis_short_actions',
        depends=('c_pytis_menu_actions',))

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
            C('fullname', TString, references='c_pytis_menu_actions on update cascade',
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
sql_raw("create index e_pytis_menu_position_index on e_pytis_menu using gist (position);",
        depends=('e_pytis_menu',))
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
            old_position = self._old['position']
            data = plpy.execute("select * from e_pytis_menu where position ~ '%s.*' and position != '%s'" %
                                (old_position, old_position,),
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
                data = plpy.execute("select position, next_position from e_pytis_menu where position != '' order by position")
                sequences = {}
                for row in data:
                    position = row['position'].split('.')
                    next_position = row['next_position'].split('.')
                    position_stamp = tuple(position[:-1])
                    if not sequences.has_key(position_stamp):
                        sequences[position_stamp] = []
                    sequences[position_stamp].append((position, next_position,))
                import string
                def update_next_position(position, next_position):
                    plpy.execute("update e_pytis_menu set next_position='%s' where position='%s'" %
                                 (string.join(next_position, '.'), string.join(position, '.'),))
                for position_list in sequences.values():
                    position_list_len = len(position_list)
                    for i in range(position_list_len - 1):
                        position = position_list[i][0]
                        next_position = position_list[i][1]
                        next_item_position = position_list[i+1][0]
                        if (len(position) != len(next_position) or
                            position >= next_position or
                            next_position >= next_item_position):
                            suffix = position[-1]
                            next_suffix = next_item_position[-1]
                            suffix += '0' * max(len(next_suffix) - len(suffix), 0)
                            next_suffix += '0' * max(len(suffix) - len(next_suffix), 0)
                            new_suffix = str(long((long(suffix) + long(next_suffix)) / 2))
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
            plpy.execute("select pytis_update_actions_structure()")
            plpy.execute("select pytis_update_summary_rights()")
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
                  depends=('e_pytis_menu', 'pytis_update_summary_rights', 'pytis_update_actions_structure', 'e_pytis_disabled_dmp_triggers',))
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
       include_columns=(V(None, 'position_nsub',
                          "(select count(*)-1 from e_pytis_menu where position <@ main.position)"),
                        V(None, 'xtitle',
                          "coalesce(main.title, '――――')"),),
       insert_order=('e_pytis_menu',),
       update_order=('e_pytis_menu',),
       delete_order=('e_pytis_menu',),
       grant=db_rights,
       depends=('e_pytis_menu', 'c_pytis_menu_actions',)
       )

viewng('ev_pytis_menu_structure',
       (SelectRelation('a_pytis_actions_structure', alias='structure', exclude_columns=('menuid',)),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'fullname', 'position', 'title',),
                       condition='structure.menuid = menu.menuid', jointype=JoinType.LEFT_OUTER),
        SelectRelation('c_pytis_action_types', alias='atypes', exclude_columns=('type',),
                       column_aliases=(('description', 'actiontype',),),
                       condition='structure.type = atypes.type', jointype=JoinType.LEFT_OUTER),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('fullname', 'shortname', 'action_title',),
                       condition='structure.fullname = actions.fullname', jointype=JoinType.LEFT_OUTER)
        ),
       include_columns=(V(None, 'position_nsub',
                          "(select count(*)-1 from a_pytis_actions_structure where position <@ structure.position)"),
                        V(None, 'title',
                          "coalesce(menu.title, '('||actions.action_title||')')"),
                        ),
       insert_order=(),
       update_order=('c_pytis_menu_actions',),
       delete_order=(),
       grant=db_rights,
       depends=('e_pytis_menu', 'a_pytis_actions_structure', 'c_pytis_menu_actions',)
       )

def pytis_first_position(position):
    position = args[0]
    start = '1' + '0' * (len(position) - 1)
    first = str(long((long(start) + long(position)) / 2))
    if first == start:
        first += '8'
    return first
_plpy_function('pytis_first_position', (TString,), TString,
               body=pytis_first_position,
               depends=())
viewng('ev_pytis_menu_all_positions',
       (SelectSet(Select((SelectRelation('e_pytis_menu', alias='menu1', exclude_columns=('*',),
                                         condition=""),),
                         include_columns=(V(None, 'position', 'position'),
                                          V(None, 'xtitle', "coalesce(menu1.title, '――――')"),
                                          ))),
        SelectSet(Select((SelectRelation('e_pytis_menu', alias='menu2', exclude_columns=('*',),
                                         condition="position != ''"),),
                         include_columns=(V(None, 'position', "next_position"),
                                          V(None, 'xtitle', "''"),
                                          )),
                  settype=UNION),
        SelectSet(Select((SelectRelation('e_pytis_menu', alias='menu3', exclude_columns=('*',),
                                         condition="name is NULL and title is not NULL"),),
                         include_columns=(V(None, 'position', "menu3.position||pytis_first_position(subpath((select position from e_pytis_menu where position <@ menu3.position and position != menu3.position union select '9' order by position limit 1), -1)::text)::ltree"),
                                          V(None, 'xtitle', "''"),
                                          )),
                  settype=UNION),),
       insert=(),
       update=(),
       delete=(),
       grant=db_rights,
       depends=('e_pytis_menu', 'pytis_first_position',))

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
            C('roleid', TUser, references='e_pytis_roles on update cascade', constraints=('not null',)),
            C('rightid', 'varchar(8)', references='c_pytis_access_rights on update cascade', constraints=('not null',)),
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
sql_raw("""
alter table e_pytis_action_rights add unique (shortname, roleid, rightid, colname, system, granted);
""",
        name='e_pytis_action_rights_constraints',
        depends=('e_pytis_action_rights',))
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
            if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                return
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

viewng('ev_pytis_action_rights',
       (SelectRelation('e_pytis_action_rights', alias='rights'),
        SelectRelation('e_pytis_roles', alias='roles', exclude_columns=('*',),
                       condition="rights.roleid = roles.name", jointype=JoinType.LEFT_OUTER),
        SelectRelation('c_pytis_role_purposes', alias='purposes', exclude_columns=('purposeid',),
                       condition="roles.purposeid = purposes.purposeid", jointype=JoinType.LEFT_OUTER),
        ),
       insert_order=('e_pytis_action_rights',),
       update_order=('e_pytis_action_rights',),
       delete_order=('e_pytis_action_rights',),
       grant=db_rights,
       depends=('e_pytis_action_rights', 'e_pytis_roles', 'c_pytis_role_purposes',))
       
viewng('ev_pytis_user_system_rights',
       (SelectRelation('e_pytis_action_rights', alias='rights',
                       condition="rights.system = 'T' and roleid = '*' or roleid in (select roleid from ev_pytis_user_roles)"),
        ),
       grant=db_rights,
       depends=('e_pytis_action_rights', 'ev_pytis_user_roles',))
    
viewng('ev_pytis_menu_rights',
       (SelectRelation('a_pytis_actions_structure', alias='actions', exclude_columns=('menuid',)),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'position', 'fullname',),
                       condition='menu.menuid = actions.menuid', jointype=JoinType.LEFT_OUTER),
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
                  C('summaryid', TString),
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
    import copy, string
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
        rightid, granted, roleid, menuid, shortname, system = row['rightid'], row['granted'], row['roleid'], row['menuid'], row['shortname'], row['system']
        keys = [(shortname, menuid,)]
        if menuid is not None:
            keys.append((shortname, None,))
        for key in keys:
            key = (shortname, menuid,)
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
    # Retrieve subactions
    subactions = {}
    for row in plpy.execute("select fullname, shortname from c_pytis_menu_actions where fullname like 'sub/%' order by fullname"):
        fullname, shortname = row['fullname'], row['shortname']
        parent = fullname[fullname.find('/', 4)+1:]
        parent_subactions = subactions.get(parent)
        if parent_subactions is None:
            parent_subactions = subactions[parent] = []
        parent_subactions.append(shortname)
    # Compute rights
    class Rights(object):
        def __init__(self, total, allowed, forbidden, parent, subforms):
            self.total = total
            self.allowed = allowed
            self.forbidden = forbidden
            self.parent = parent
            self.subforms = subforms
    computed_rights = {}
    position2parent = {}
    menuid2action = {}
    action2menuids = {}
    for row in plpy.execute("select menuid, type, position, c_pytis_menu_actions.fullname, c_pytis_menu_actions.shortname "
                            "from c_pytis_menu_actions left outer join a_pytis_actions_structure "
                            "on c_pytis_menu_actions.fullname = a_pytis_actions_structure.fullname "
                            "order by position"):
        menuid, type_, position, shortname, fullname = row['menuid'], row['type'], row['position'], row['shortname'], row['fullname']
        menu_rights = raw_rights.get((shortname, menuid,), {})
        subforms = subactions.get(fullname, ())
        if menuid:
            position2parent[position] = menuid
            menuid2action[menuid] = shortname
            action_menuids = action2menuids.get(shortname, [])
            action_menuids.append(menuid)
            action2menuids[shortname] = action_menuids
            fullname_components = fullname.split('/')
        if position and position.find('.') != -1 and menuid:
            parent = position2parent[string.join(position.split('.')[:-1], '.')]
        else:
            parent = None
        for roleid, role_roles in roles.items():
            max_ = []
            allowed = []
            if fullname[:5] == 'form/' or fullname[:9] == 'RUN_FORM/':
                forbidden = ['call']
            elif fullname[:7] == 'handle/':
                forbidden = ['view', 'insert', 'update', 'delete', 'print', 'export']
            else:
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
            if menuid and type_ is None:
                max_rights = None
            else:
                if not max_rights:
                    for r in menu_rights.values():
                        if r.system:
                            break
                    else:
                        max_rights = ['view', 'insert', 'update', 'delete', 'print', 'export', 'call']
            if menuid:
                menu_allowed_rights = copy.copy(allowed_rights)
                menu_forbidden_rights = copy.copy(forbidden_rights)
                parent_menuid = parent
                while parent_menuid is not None:
                    parent_rights = computed_rights[(parent_menuid, roleid,)]
                    for r in parent_rights.forbidden:
                        if r not in menu_forbidden_rights and (r == 'show' or r not in menu_allowed_rights):
                            menu_forbidden_rights.append(r)                
                    for r in parent_rights.allowed:
                        if r not in menu_forbidden_rights and r not in menu_allowed_rights:
                            menu_allowed_rights.append(r)
                    parent_menuid = parent_rights.parent
            def store_rights(menu_or_action, max_rights, allowed_rights, forbidden_rights):
                if max_rights is None:
                    max_rights = allowed_rights
                rights = [right for right in max_rights if right not in forbidden_rights]
                if 'show' not in forbidden_rights:
                    rights.append('show')
                rights.sort()
                computed_rights[(menu_or_action, roleid,)] = Rights(total=rights, allowed=allowed_rights, forbidden=forbidden_rights, parent=parent,
                                                                    subforms=subforms)
            store_rights(shortname, max_rights, allowed_rights, forbidden_rights)
            if menuid:
                store_rights(menuid, max_rights, menu_allowed_rights, menu_forbidden_rights)
    # Insertion of new rights to the database takes most of the time, so we make only real changes
    old_rights = {}
    for row in plpy.execute("select shortname, menuid, roleid, rights from a_pytis_computed_summary_rights"):
        old_rights[(row['shortname'], row['menuid'], row['roleid'],)] = row['rights']
    def _pg_escape(val):
        return str(val).replace("'", "''")
    for short_key, all_rights in computed_rights.items():
        menuid_or_action, roleid = short_key
        total = all_rights.total
        if isinstance(menuid_or_action, int):
            menuid = menuid_or_action
            shortname = menuid2action[menuid]
        else:
            menuid = None
            shortname = menuid_or_action
            for mid in action2menuids.get(shortname, []):
                for r in computed_rights[(mid, roleid,)].total:
                    if r not in total:
                        total.append(r)
            all_rights.total = total
        # Multiform view rights are valid if they are permitted by the main
        # form and (in case of the VIEW right) at least one of the side forms.
        subforms = all_rights.subforms
        if subforms:
            r = computed_rights.get((subforms[0], roleid,))
            if r is not None:
                total = [rr for rr in total if rr in r.total]
            subforms_total = []
            for sub in subforms[1:]:
                r = computed_rights.get((sub, roleid,))
                if r is None:
                    subforms_total = ['show', 'view', 'insert', 'update', 'delete', 'print', 'export', 'call']
                    break
                else:
                    subforms_total += r.total
            all_rights.total = [r for r in total if r != 'view' or r in subforms_total]
        # Format and store the rights
        key = shortname, menuid, roleid
        rights = string.join(all_rights.total, ' ')
        old_item_rights = old_rights.get(key)
        if rights != old_item_rights:
            summaryid = '%s+%s' % (menuid or '', shortname,)
            if old_item_rights is None:
                plpy.execute(("insert into a_pytis_computed_summary_rights (shortname, menuid, roleid, rights, summaryid) "
                              "values('%s', %s, '%s', '%s', '%s')") %
                             (_pg_escape(shortname), menuid or "NULL", _pg_escape(roleid), rights, _pg_escape(summaryid),))
            else:
                plpy.execute("update a_pytis_computed_summary_rights set rights='%s' where summaryid='%s' and roleid='%s'" %
                             (rights, _pg_escape(summaryid), _pg_escape(roleid),))
        try:
            del old_rights[key]
        except KeyError:
            pass
    for shortname, menuid, roleid in old_rights.keys():
        plpy.execute("delete from a_pytis_computed_summary_rights where shortname='%s' and menuid=%s and roleid='%s'" %
                     (_pg_escape(shortname), menuid or "NULL", _pg_escape(roleid),))
_plpy_function('pytis_update_summary_rights', (), TBoolean,
               body=pytis_update_summary_rights,
               depends=('a_pytis_computed_summary_rights', 'a_pytis_valid_role_members', 'ev_pytis_menu_rights', 'a_pytis_actions_structure',),)

_std_table_nolog('a_pytis_actions_structure',
                 (C('fullname', TString, constraints=('not null',)),
                  C('shortname', TString, constraints=('not null',)),
                  C('menuid', TInteger),
                  C('summaryid', TString),
                  C('position', TLTree, constraints=('not null',)),
                  C('type', 'char(4)', constraints=('not null',),
                    references='c_pytis_action_types'),
                  ),
                 """Precomputed actions structure as presented to menu admin.
Item positions and indentations are determined by positions.
This table is modified only by triggers.
""",
                 depends=('c_pytis_action_types',))
sql_raw("create index a_pytis_actions_structure_index on a_pytis_actions_structure using gist (position);",
        depends=('a_pytis_actions_structure',))

def pytis_update_actions_structure():
    import string
    def _pg_escape(val):
        return str(val).replace("'", "''")
    plpy.execute("delete from a_pytis_actions_structure")
    subactions = {}
    for row in plpy.execute("select fullname, shortname from c_pytis_menu_actions where fullname like 'sub/%' order by fullname"):
        fullname, shortname = row['fullname'], row['shortname']
        parent = fullname[fullname.find('/', 4)+1:]
        parent_subactions = subactions.get(parent)
        if parent_subactions is None:
            parent_subactions = subactions[parent] = []
        parent_subactions.append((fullname, shortname,))
    actions = {}
    def add_row(fullname, shortname, menuid, position):
        summaryid = '%s+%s' % (menuid or '', shortname,)
        if fullname[:4] == 'sub/':
            item_type = 'subf'
        elif position.find('.') == -1:
            item_type = '----'
        elif menuid is None:
            item_type = 'spec'
        elif not fullname:
            item_type = 'sepa'
        elif fullname[:5] == 'menu/':
            item_type = 'menu'
        else:
            item_type = 'item'
        if menuid is None:
            menuid = 'NULL'
        plpy.execute(("insert into a_pytis_actions_structure (fullname, shortname, menuid, position, summaryid, type) "
                      "values('%s', '%s', %s, '%s', '%s', '%s') ") %
                     (_pg_escape(fullname), _pg_escape(shortname), menuid, position, summaryid, item_type,))
        actions[shortname] = True
    for row in plpy.execute("select menuid, position, c_pytis_menu_actions.fullname, shortname "
                            "from e_pytis_menu, c_pytis_menu_actions "
                            "where e_pytis_menu.fullname = c_pytis_menu_actions.fullname "
                            "order by position"):
        menuid, position, shortname, fullname = row['menuid'], row['position'], row['shortname'], row['fullname']
        add_row(fullname, shortname, menuid, position)
        action_components = shortname.split('/')
        fullname_components = fullname.split('/')
        if action_components[0] == 'form' or action_components[0] == 'RUN_FORM':
            subaction_list = subactions.get(fullname, ())
            for i in range(len(subaction_list)):
                sub_fullname, sub_shortname = subaction_list[i]
                subposition = '%s.%02d' % (position, i,)
                add_row(sub_fullname, sub_shortname, None, subposition)
    position = '8.0001'
    add_row('label/1', 'label/1', None, '8')
    for row in plpy.execute("select fullname, shortname from c_pytis_menu_actions order by shortname"):
        fullname, shortname = row['fullname'], row['shortname']
        if actions.has_key(shortname):
            continue
        add_row(fullname, shortname, None, position)
        position_components = position.split('.')
        position = string.join(position_components[:-1] + ['%04d' % (int(position_components[-1]) + 1)], '.')
_plpy_function('pytis_update_actions_structure', (), TBoolean,
               body=pytis_update_actions_structure,
               depends=('a_pytis_actions_structure', 'e_pytis_menu', 'c_pytis_menu_actions',),)
    
viewng('ev_pytis_summary_rights_raw',
       (SelectRelation('a_pytis_computed_summary_rights', alias='summary'),
        SelectRelation('e_pytis_roles', alias='roles', exclude_columns=('*',),
                       condition="summary.roleid = roles.name", jointype=JoinType.LEFT_OUTER),
        SelectRelation('c_pytis_role_purposes', alias='purposes', exclude_columns=('purposeid',),
                       condition="roles.purposeid = purposes.purposeid", jointype=JoinType.LEFT_OUTER),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('e_pytis_roles', 'a_pytis_computed_summary_rights', 'c_pytis_role_purposes',)
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

viewng('ev_pytis_extended_role_menu_raw',
       (SelectRelation('a_pytis_actions_structure', alias='structure', exclude_columns=('menuid',)),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'fullname', 'position', 'title',),
                       condition='structure.menuid = menu.menuid', jointype=JoinType.LEFT_OUTER),
        SelectRelation('ev_pytis_valid_roles', alias='roles', exclude_columns=('description', 'purposeid', 'deleted',),
                       jointype=JoinType.CROSS,
                       column_aliases=(('name', 'roleid',),),
                       ),
        SelectRelation('a_pytis_computed_summary_rights', alias='summary', exclude_columns=('menuid', 'roleid', 'shortname', 'summaryid',),
                       condition="structure.summaryid = summary.summaryid and roles.name = summary.roleid" , jointype=JoinType.INNER),
        SelectRelation('c_pytis_action_types', alias='atypes', exclude_columns=('type',),
                       column_aliases=(('description', 'actiontype',),),
                       condition='structure.type = atypes.type', jointype=JoinType.LEFT_OUTER,
                       ),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('*',),
                       condition='structure.fullname = actions.fullname', jointype=JoinType.LEFT_OUTER)
        ),
       include_columns=(V(None, 'position_nsub',
                          "(select count(*)-1 from a_pytis_actions_structure where position <@ structure.position)"),
                        V(None, 'title',
                          "coalesce(menu.title, '('||actions.action_title||')')"),
                        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('a_pytis_actions_structure', 'e_pytis_menu', 'ev_pytis_valid_roles', 'a_pytis_computed_summary_rights',
                'c_pytis_action_types',)
       )

viewng('ev_pytis_extended_role_menu',
       (SelectRelation('ev_pytis_extended_role_menu_raw', alias='menu', condition="rights like '%show%'"),),
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
       depends=('ev_pytis_extended_role_menu_raw',)
       )

viewng('ev_pytis_user_menu',
       (SelectRelation('e_pytis_menu', alias='menu',
                       condition="(name is null and title is null) or (rights.rights like '%show%')"),
        SelectRelation('a_pytis_computed_summary_rights', alias='rights', exclude_columns=('menuid', 'roleid', 'shortname',),
                       condition=("menu.menuid = rights.menuid and "
                                  "rights.roleid = user"),
                       jointype=JoinType.LEFT_OUTER),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('e_pytis_menu', 'a_pytis_computed_summary_rights',)
       )

viewng('ev_pytis_user_rights',
       (SelectRelation('a_pytis_computed_summary_rights', alias='rights', exclude_columns=('menuid', 'roleid',),
                       condition="rights.roleid = user"),
        ),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('a_pytis_computed_summary_rights',)
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
