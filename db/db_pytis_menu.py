# -*- coding: utf-8 -*-

"""Gensql definitions for dynamic application menus and access rights."""

db_rights = globals().get('Gall_pytis', None)
if not db_rights:
    raise ProgramError('No rights specified! Please define Gpytis_menu')

_std_table_nolog('e_pytis_disabled_dmp_triggers',
                 (P('id', TUser),),
                 """This table allows disabling some trigger calls.
Right now inserting any value into it disables insert and delete trigger actions over
computed tables.  This allows computing the tables separately.""")

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
                 (P('name', TString),
                  C('shortname', TString, constraints=('not null',)),
                  C('description', TString),
                  ),
                 """List of available (pre-defined and visible) application actions.""",
                 grant=db_rights
                 )

def pytis_matching_actions(complex_action, simple_action):
    complex_action = args[0]
    simple_action = args[1]
    if complex_action is None:
        return False
    components = complex_action.split('/')
    if components[0] == 'form':
        last_components = components[-1].split('::')
        if len(last_components) == 2:
            import string
            prefix = string.join(components[:-1], '/')
            result = ((prefix + last_components[0]) == simple_action or
                      (prefix + last_components[1]) == simple_action)
        else:
            result = ('form/'+components[-1] == simple_action)
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
            C('position', TString, constraints=('not null', 'unique',),
              doc=("Unique identifier of menu item placement within menu. "
                   "The top-menu item position is ''. "
                   "Each submenu has position two characters wider than its parent. "
                   "The two character suffix identifies position within the submenu, "
                   "lower numbers put the item higher. "
                   "Only odd numbers from 11 to 99 are allowed, "
                   "other numbers are reserved for other purposes. "
                   "Note these rules limit maximum number of items within a given submenu to 45.")),
            C('action', TString, references='c_pytis_menu_actions',
              doc=("Application action assigned to the menu item."
                   "Menu items bound to submenus should have this value NULL; "
                   "if they do not, the assigned action is ignored.")),
            C('help', TString,
              doc=("Arbitrary single-line help string.")),
            C('hotkey', TString,
              doc=("Sequence of command keys, separated by single spaces."
                   "The space key is represented by SPC string.")),
            C('locked', TBoolean,
              doc=("Iff true, this item may not be edited.")),
            ),
           """Menu structure definition.""",
           grant=db_rights,
           depends=('c_pytis_menu_actions',))


viewng('ev_pytis_menu',
       (SelectRelation('e_pytis_menu', alias='main', exclude_columns=('action',)),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('description',),
                       column_aliases=(('name', 'action',),),
                       condition='main.action = actions.name', jointype=JoinType.LEFT_OUTER),
        ),
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
                              ),
                 grant=db_rights)

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
           grant=db_rights,
           depends=('c_pytis_menu_actions', 'e_pytis_roles', 'c_pytis_access_rights',)
           )

viewng('ev_pytis_user_system_rights',
       (SelectRelation('e_pytis_action_rights', alias='rights',
                       condition="rights.system = 'T' and roleid = '*' or roleid in (select roleid from ev_pytis_user_roles)"),
        ),
       grant=db_rights,
       depends=('e_pytis_action_rights', 'ev_pytis_user_roles',))
    
viewng('ev_pytis_menu_rights',
       (SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'position', 'action',)),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('*',),
                       condition='pytis_matching_actions(menu.action, actions.shortname)', jointype=JoinType.INNER),
        SelectRelation('e_pytis_action_rights', alias='rights', exclude_columns=(),
                       condition='actions.shortname = rights.action', jointype=JoinType.INNER,
                       column_aliases=(('action', 'shortname',),)),
        ),
       insert_order=('e_pytis_action_rights',),
       update_order=('e_pytis_action_rights',),
       delete_order=('e_pytis_action_rights',),
       grant=db_rights,
       depends=('e_pytis_menu', 'c_pytis_menu_actions', 'e_pytis_action_rights',)
       )

### Summarization

_std_table_nolog('a_pytis_computed_summary_rights',
                 (C('menuid', TInteger, constraints=('not null',), references='e_pytis_menu on delete cascade on update cascade'),
                  C('roleid', TString, constraints=('not null',), references='e_pytis_roles on delete cascade on update cascade'),
                  C('rights', TString, constraints=('not null',)),
                  ),
                 """Precomputed summary access rights as a single line string.
This table is modified only by triggers.
""",
                 depends=('e_pytis_menu', 'e_pytis_roles',))

viewng('ev_pytis_summary_rights_raw',
       (SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('name', 'position', 'action',)),
        SelectRelation('ev_pytis_valid_roles', alias='roles', exclude_columns=('description', 'purposeid', 'deleted',),
                       jointype=JoinType.CROSS),
        SelectRelation('a_pytis_computed_summary_rights', alias='summary', exclude_columns=('menuid', 'roleid',),
                       condition="menu.menuid = summary.menuid and roles.name = summary.roleid" , jointype=JoinType.INNER),
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
       (SelectRelation('ev_pytis_menu', alias='menu', exclude_columns=('name', 'action',)),
        SelectRelation('ev_pytis_valid_roles', alias='roles', exclude_columns=('description', 'purposeid', 'deleted',),
                       jointype=JoinType.CROSS,
                       column_aliases=(('name', 'roleid',),),
                       ),
        SelectRelation('a_pytis_computed_summary_rights', alias='summary', exclude_columns=('menuid', 'roleid',),
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
        SelectRelation('a_pytis_computed_summary_rights', alias='rights', exclude_columns=('menuid', 'roleid',),
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
       (SelectRelation('a_pytis_computed_summary_rights', alias='rights', exclude_columns=('menuid', 'roleid',),
                       condition="rights.roleid = user"),
        SelectRelation('e_pytis_menu', alias='menu', exclude_columns=('*'),
                       condition="rights.menuid = menu.menuid", jointype=JoinType.INNER),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('name', 'description',),
                       condition="menu.action = actions.name", jointype=JoinType.INNER),
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
