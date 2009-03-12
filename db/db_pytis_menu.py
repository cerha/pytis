# -*- coding: iso-8859-2 -*-

"""Gensql definitions for dynamic application menus and access rights."""

db_rights = globals().get('Gall_pytis', None)
if not db_rights:
    raise ProgramError('No rights specified! Please define Gpytis_menu')

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
                              ("'appl'", "'Aplikační'",),)
            )

_std_table('e_pytis_roles',
           (P('name', TUser),
            C('description', 'varchar(64)'),
            C('purposeid', 'char(4)', constraints=('not null',), default="'appl'",
              references='c_pytis_role_purposes'),
            C('deleted', TDate),),
            """Application user roles.""",
           init_values=(("'admin_roles'", "'Administrátor rolí'", "'admn'", 'NULL',),
                        ("'admin_menu'", "'Administrátor menu'", "'admn'", 'NULL',),
                        ("'admin'", "'Administrátor rolí a menu'", "'admn'", 'NULL',),
                        ),
           depends=('c_pytis_role_purposes',))

viewng('ev_pytis_valid_roles',
       (SelectRelation('e_pytis_roles', alias='main',
                       condition='main.deleted is null or main.deleted > now()'),
        SelectRelation('c_pytis_role_purposes', alias='codebook', exclude_columns=('purposeid',),
                       condition='main.purposeid = codebook.purposeid', jointype=JoinType.INNER),
        ),
       grant=db_rights,
       depends=('e_pytis_roles', 'c_pytis_role_purposes',)
       )

viewng('ev_pytis_user_roles',
       (SelectRelation('e_pytis_roles', alias='main',
                       condition=("(main.deleted is null or main.deleted > now()) and "
                                  "main.purposeid = 'user' and "
                                  "main.name = session_user")),
        SelectRelation('pg_roles', alias='roles', key_column='pg_roles', exclude_columns=('*',),
                       condition=("main.name = roles.rolname and "
                                  "pg_has_role(roles.rolname, 'member')"),
                       jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_roles',),
       update_order=('e_pytis_roles',),
       delete_order=('e_pytis_roles',),
       grant=db_rights,
       depends=('e_pytis_roles',)
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
            C('roleid', TUser, constraints=('not null',), references='e_pytis_roles'),
            C('member', TUser, constraints=('not null',), references='e_pytis_roles'),
            ),
           """Mutual memberships of roles.
Entries in this table define `member's of each `roleid'.
""",
           init_values=(('-1', "'admin_roles'", "'admin'",),
                        ('-2', "'admin_menu'", "'admin'",),
                        ),
           depends=('e_pytis_roles',))

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

### Actions

_std_table_nolog('c_pytis_menu_actions',
                 (P('name', TString),
                  C('shortname', TString, constraints=('not null',)),
                  C('description', TString),
                  ),
                 """List of available (pre-defined and visible) application actions."""
                 )

### Menus

_std_table('e_pytis_menu',
           (P('menuid', TSerial),
            C('name', TString, constraints=('unique',),
              doc="Unique identifiers of terminal menu items.  NULL for non-terminal items and separators."),
            C('title', 'varchar(64)',
              doc='User title of the item. If NULL then it is a separator.'),
            C('parent', TInteger, references='e_pytis_menu',
              doc="Parent menu item, NULL for top level items."),
            C('position', TInteger, constraints=('not null',),
              doc=("Order of the item within the given submenu. "
                   "Lower numbers put the item higher. "
                   "No two items in the same submenu should have the same order number; "
                   "if they do, their mutual order is undefined.")),
            C('fullposition', TString,
              doc=("Full position including parent items. "
                   "Lexicographical ordering of all menu items. ")),
            C('indentation', 'varchar(8)',
              doc="Indentation of the menu item in the menu structure"),
            C('action', TString, references='c_pytis_menu_actions',
              doc=("Application action assigned to the menu item."
                   "Menu items bound to submenus should have this value NULL; "
                   "if they do not, the assigned action is ignored.")),
            ),
           """Menu structure definition.""",
           depends=('c_pytis_menu_actions',))

def e_pytis_menu_trigger():
    class Menu(BaseTriggerObject):
        def _pg_escape(self, val):
            return str(val).replace("'", "''")
        def _do_before_insert(self, old=None):
            parent_id = self._new['parent']
            if not parent_id:
                return
            data = plpy.execute("select indentation, fullposition from e_pytis_menu where menuid = %s" %
                                (parent_id,))
            parent = data[0]
            self._new['indentation'] = parent['indentation'] + '   '
            self._new['fullposition'] = parent['fullposition'] + str(self._new['position'])
            if not self._new['name'] and self._new['title'] and (old is None or not old['title']):
                # New non-terminal menu item
                self._new['action'] = action = 'menu/' + str(self._new['menuid'])
                plpy.execute(("insert into c_pytis_menu_actions (name, shortname, description) "
                              "values ('%s', '%s', '%s')") % (action, action, self._pg_escape("Menu '%s'" % (self._new['title'])),))
            self._return_code = self._RETURN_CODE_MODYFY
        def _do_before_update(self):
            self._do_before_insert(old=self._old)
        def _do_after_update(self):
            plpy.execute("update e_pytis_menu set parent=parent where parent=%s" %
                         (self._new['menuid'],))
            if not self._new['name'] and self._old['title'] and not self._new['title']:
                # Non-terminal item changed to separator
                plpy.execute("delete from c_pytis_actions where name = '%s'" % (self._old['action'],))
                plpy.execute("delete from e_pytis_action_rights where action = '%s'" % (self._old['action'],))
        def _do_before_delete(self):
            data = plpy.execute("select * from e_pytis_menu where parent=%s" %
                                (self._old['menuid'],))
            if data:
                self._return_code = self._RETURN_CODE_SKIP
        def _do_after_delete(self):
            if not self._old['name'] and self._old['title']:
                # Non-terminal menu item
                plpy.execute("delete from c_pytis_actions where name = '%s'" % (self._old['action'],))
                plpy.execute("delete from e_pytis_action_rights where action = '%s'" % (self._old['action'],))
    menu = Menu(TD)
    return menu.do_trigger()
_trigger_function('e_pytis_menu_trigger', body=e_pytis_menu_trigger,
                  doc="Updates indentations and fullpositions",
                  depends=('e_pytis_menu',))
sql_raw("""
create trigger e_pytis_menu_all_before before insert or update or delete on e_pytis_menu
for each row execute procedure e_pytis_menu_trigger();
create trigger e_pytis_menu_all_after after update or delete on e_pytis_menu
for each row execute procedure e_pytis_menu_trigger();
""",
        name='e_pytis_menu_triggers',
        depends=('e_pytis_menu_trigger',))

viewng('ev_pytis_menu',
       (SelectRelation('e_pytis_menu', alias='main', exclude_columns=('action',)),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('description',),
                       column_aliases=(('name', 'action',),),
                       condition='main.action = actions.name', jointype=JoinType.LEFT_OUTER),
        ),
       include_columns=(V(None, 'ititle', "main.indentation || ' ' || main.title"),),
       insert_order=('e_pytis_menu',),
       update_order=('e_pytis_menu',),
       delete_order=('e_pytis_menu',),
       grant=db_rights,
       depends=('e_pytis_menu', 'c_pytis_menu_actions',)
       )

viewng('ev_pytis_menu_parents',
       (SelectRelation('ev_pytis_menu', alias='main',
                       condition='main.name is null and main.title is not null'),),
       insert_order=('e_pytis_menu',),
       update_order=('e_pytis_menu',),
       delete_order=('e_pytis_menu',),
       grant=db_rights,
       depends=('ev_pytis_menu',)
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
                              ))

_std_table('e_pytis_action_rights',
           (P('id', TSerial,
              doc="Just to make logging happy"),
            C('action', TString, constraints=('not null',)),
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
           depends=('c_pytis_menu_actions', 'e_pytis_roles', 'c_pytis_access_rights',)
           )

viewng('ev_pytis_menu_rights',
       (SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'parent', 'position', 'fullposition', 'indentation', 'action',)),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('*',),
                       condition='menu.action = actions.name', jointype=JoinType.INNER),
        SelectRelation('e_pytis_action_rights', alias='rights', exclude_columns=(),
                       condition='actions.shortname = rights.action', jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_action_rights',),
       update_order=('e_pytis_action_rights',),
       delete_order=('e_pytis_action_rights',),
       grant=db_rights,
       depends=('e_pytis_menu', 'c_pytis_menu_actions', 'e_pytis_action_rights',)
       )

