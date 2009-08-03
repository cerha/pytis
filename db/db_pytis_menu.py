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
                 init_values=(("'admn'", "'Spr�vcovsk�'",),
                              ("'user'", "'U�ivatelsk�'",),
                              ("'appl'", "'Aplika�n�'",),)
            )

_std_table('e_pytis_roles',
           (P('name', TUser),
            C('description', 'varchar(64)'),
            C('purposeid', 'char(4)', constraints=('not null',), default="'appl'",
              references='c_pytis_role_purposes'),
            C('deleted', TDate),),
            """Application user roles.""",
           init_values=(("'admin_roles'", "'Administr�tor rol�'", "'admn'", 'NULL',),
                        ("'admin_menu'", "'Administr�tor menu'", "'admn'", 'NULL',),
                        ("'admin'", "'Administr�tor rol� a menu'", "'admn'", 'NULL',),
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

### Menus

_std_table_nolog('c_pytis_menu_actions',
                 (P('actionid', TInteger),
                  C('name', TString, constraints=('not null', 'unique',)),
                  C('description', TString),
                  ),
                 """List of available (pre-defined and visible) application actions."""
                 )

_std_table('e_pytis_menu',
           (P('menuid', TSerial),
            C('name', TString, constraints=('unique',),
              doc="Unique identifiers of terminal menu items.  NULL for non-terminal items and separators."),
            C('title', 'varchar(64)',
              doc='User title of the item. If NULL then it is a separator.'),
            C('parent', TInteger, references='e_pytis_menu',
              doc="Parent menu item, NULL for top level items."),
            C('position', TInteger,
              doc=("Order of the item within the given submenu. "
                   "Lower numbers put the item higher. "
                   "No two items in the same submenu should have the same order number; "
                   "if they do, their mutual order is undefined.")),
            C('fullposition', TString,
              doc=("Full position including parent items. "
                   "Lexicographical ordering of all menu items. ")),
            C('indentation', 'varchar(8)',
              doc="Indentation of the menu item in the menu structure"),
            C('actionid', TInteger, references='c_pytis_menu_actions',
              doc=("Application action assigned to the menu item."
                   "Menu items bound to submenus should have this value NULL; "
                   "if they do not, the assigned action is ignored.")),
            ),
           """Menu structure definition.""",
           depends=('c_pytis_menu_actions',))

viewng('ev_pytis_menu',
       (SelectRelation('e_pytis_menu', alias='main'),
        SelectRelation('c_pytis_menu_actions', alias='actions', exclude_columns=('actionid', 'description',),
                       column_aliases=(('name', 'action',),),
                       condition='main.actionid = actions.actionid', jointype=JoinType.LEFT_OUTER),
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
                 init_values=(("'show'", "'Viditelnost polo�ek menu'",),
                              ("'view'", "'Prohl�en� existuj�c�ch z�znam�'",),
                              ("'insert'", "'Vkl�d�n� nov�ch z�znam�'",),
                              ("'edit'", "'Editace existuj�c�ch z�znam�'",),
                              ("'delete'", "'Maz�n� z�znam�'",),
                              ("'print'", "'Tisky'",),
                              ("'export'", "'Exporty'",),
                              ("'run'", "'Spou�t�n� aplika�n�ch procedur'",),
                              ))

_std_table('e_pytis_action_rights',
           (P('id', TSerial,
              doc="Just to make logging happy"),
            C('actionid', TInteger, references='c_pytis_menu_actions', constraints=('not null',)),
            C('roleid', TUser, references='e_pytis_roles', constraints=('not null',)),
            C('rightid', 'varchar(8)', references='c_pytis_access_rights', constraints=('not null',)),
            ),
           """Assignments of access rights to actions.
Actions have access rights given here and only those access rights,
unless overridden by menu items access rights.

Action rights are supported in the application, but they are not exposed in the
current user interface.  They are introduced to support extend rights
assignment, e.g. in context menus etc.
""",
           depends=('c_pytis_menu_actions', 'e_pytis_roles', 'c_pytis_access_rights',)
           )

_std_table('e_pytis_menu_rights',
           (P('id', TSerial),
            C('menuid', TInteger, references='e_pytis_menu', constraints=('not null',)),
            C('roleid', TUser, references='e_pytis_roles', constraints=('not null',)),
            C('rightid', 'varchar(8)', references='c_pytis_access_rights', constraints=('not null',)),
            C('granted', TBoolean, constraints=('not null',),
              doc="If true the right is granted, otherwise it is denied"),
            ),
           """Assignments of access rights to menu items.
These right assignments have preference over rights assigned to actions.
If a right is assigned or denied to a terminal menu item, it takes absolute precedence.
If a right is assigned or denied to a non-terminal menu item, it applies
implicitly to all the items included in it.  But particular terminal or
non-terminal menu items can override the assignment.
Action rights take precedence over implicit rights set by non-terminal menu
items, but have lower precedence than terminal menu items rights.
""",
           depends=('e_pytis_menu', 'e_pytis_roles', 'c_pytis_access_rights',)
           )

viewng('ev_pytis_menu_rights',
       (SelectRelation('e_pytis_menu_rights', alias='main'),
        SelectRelation('e_pytis_menu', alias='menu',
                       exclude_columns=('menuid', 'name', 'parent', 'position', 'fullposition', 'indentation', 'actionid',),
                       condition='main.menuid = menu.menuid', jointype=JoinType.INNER),
        SelectRelation('e_pytis_roles', alias='roles', exclude_columns=('description', 'purposeid',),
                       condition='main.roleid = roles.name', jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_menu_rights',),
       update_order=('e_pytis_menu_rights',),
       delete_order=('e_pytis_menu_rights',),
       grant=db_rights,
       depends=('e_pytis_menu_rights', 'e_pytis_menu', 'e_pytis_roles',)
       )
