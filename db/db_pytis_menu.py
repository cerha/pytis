# -*- coding: utf-8 -*-

"""Gensql definitions for dynamic application menus and access rights."""

import pytis.data

C = Column
P = PrimaryColumn
V = ViewColumn

TBoolean = pytis.data.Boolean()
TDate = pytis.data.Date()
TDateTime = pytis.data.DateTime()
TInteger = pytis.data.Integer()
TSerial = pytis.data.Serial()
TString = pytis.data.String()
TUser = 'name'

#db_rights = globals().get('Gpytis_menu', None)
db_rights = (('all', 'pdm',),)
if not db_rights:
    raise ProgramError('No rights specified! Please define Gpytis_menu')

def Ctimestamp(name, doc=None):
    return C(name, TDateTime, constraints=('NOT NULL',), default='now()', doc=doc)
def Cuser(name, doc=None):
    return C(name, TUser, constraints=('NOT NULL',), default='user', doc=doc)

table('_changes',
      (Cuser('vytvoril'),
       Ctimestamp('vytvoreno'),
       Cuser('zmenil'),
       Ctimestamp('zmeneno')),
      grant=db_rights,
      doc="""Sloupečky zaznamenávající uživatele a časy vytvoření a změn údajů.
      Je určena k tomu, aby ji dědily všechny ostatní tabulky."""
      )

table('_inserts',
      (P('id', TSerial,
         doc="identifikace řádku"),
       Cuser('vytvoril'),
       Ctimestamp('vytvoreno'),
       C('tabulka', TString),
       C('klic', TString),
       ),
      view=(TableView((V(None, 'datum', 'vytvoreno::date'),
                       V(None, 'cas', 'vytvoreno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      name='v_inserts',
                      grant=db_rights,
                      update=None,
                      insert=None,
                      delete=None),
            TableView((V(None, 'datum', 'vytvoreno::date'),
                       V(None, 'cas', 'vytvoreno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      join="vytvoril = current_user",
                      name='v_inserts_user',
                      grant=db_rights,
                      update=None,
                      insert=None,
                      delete=None)),
      grant=db_rights,
      doc="""Tabulka zaznamenávající přidávání záznamů standardních
      tabulek."""
      )

table('_updates',
      (P('id', TSerial,
         doc="identifikace změnového řádku"),
       Cuser('zmenil'),
       Ctimestamp('zmeneno'),
       C('tabulka', TString),
       C('klic', TString),
       C('zmeny', TString)
       ),
      view=(TableView((V(None, 'datum', 'zmeneno::date'),
                       V(None, 'cas', 'zmeneno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      name='v_updates',
                      grant=db_rights,
                      update=None,
                      insert=None,
                      delete=None),
            TableView((V(None, 'datum', 'zmeneno::date'),
                       V(None, 'cas', 'zmeneno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      join="zmenil = current_user",
                      name='v_updates_user',
                      grant=db_rights,
                      update=None,
                      insert=None,
                      delete=None)),
      grant=db_rights,
      doc="""Tabulka zaznamenávající změny v záznamech standardních
      tabulek."""
      )

table('_deletes',
      (P('id', TSerial,
         doc="identifikace řádku"),
       Cuser('smazal'),
       Ctimestamp('smazano'),
       C('tabulka', TString),
       C('klic', TString)),
      view=(TableView((V(None, 'datum', 'smazano::date'),
                      V(None, 'cas', 'smazano::time'),
                      V('oid', 'oid')),
                     exclude=(),
                     name='v_deletes',
                     grant=db_rights,
                     update=None,
                     insert=None,
                     delete=None),
            TableView((V(None, 'datum', 'smazano::date'),
                       V(None, 'cas', 'smazano::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      join = "smazal = current_user",
                      name='v_deletes_user',
                      grant=db_rights,
                      update=None,
                      insert=None,
                      delete=None)),            
      grant=db_rights,
      doc="""Tabulka zaznamenávající vymazávání záznamů ve standardních
      tabulkách."""
      )

def _log_update_trigger():
    def pg_escape(val):
        return val.replace("'", "''")
    event = TD["event"]
    if event == "DELETE":
        newold = "old"
        table = "_deletes"
    elif event == "INSERT":
        newold = "new"
        table = "_inserts"
    else:
        newold = "new"
        table = "_updates"
    tabid = TD["relid"]
    q = "select relname from pg_class where oid = %s" % tabid
    q = plpy.execute(q)
    tabulka = q[0]["relname"]
    klice = TD["args"][0].split(',')
    klicestr = ','.join(["%s: %s" % (k, str(TD[newold][k]))
                         for k in klice])
    # pro INSERT a DELETE zaznamenáme tabulku a klíč
    if event in ("DELETE", "INSERT"):
        q = """insert into %s (tabulka, klic)
               select '%s', '%s'
            """ % (table, tabulka, klicestr)
        q = plpy.execute(q)
        return None
    # Pro UPDATE zaznamenáme kromě jména tabulky a klíče i změny v položkách
    zmeny = []
    for k in TD["new"].keys():
        if TD["new"][k] != TD["old"][k]:
            zmeny.append("""%s: %s -> %s""" % (k, pg_escape(str(TD["old"][k])),
                                               pg_escape(str(TD["new"][k]))))
    if zmeny != []:        
        zmenystr = """\n""".join(zmeny)        
        q = """insert into _updates (tabulka, klic, zmeny)
               select '%s', '%s', '%s'
            """ % (tabulka, klicestr, zmenystr)
        q = plpy.execute(q)
    return None

function('_log_update_trigger', (), 'trigger',
         body=_log_update_trigger,
         doc="""Slouží k evidenci editací nad záznamy tabulek.""",
         depends=('_inserts', '_deletes', '_updates'))

def _std_table(name, columns, doc, grant=db_rights, **kwargs):
    return table(name, columns, inherits=('_changes',), grant=grant,
                 doc=doc, upd_log_trigger='_log_update_trigger',
                 **kwargs)

def _std_table_nolog(name, columns, doc, grant=db_rights, **kwargs):
    return table(name, columns, inherits=('_changes',), grant=grant,
                 doc=doc, **kwargs)


### The menu schema itself

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
           (P('roleid', TSerial),
            C('name', TUser, constraints=('unique', 'not null',),),
            C('description', 'varchar(64)'),
            C('purposeid', 'char(4)', constraints=('not null',), default="'appl'",
              references='c_pytis_role_purposes'),
            C('deleted', TDate),),
            """Application user roles.""",
           init_values=(('-1', "'admin_roles'", "'Administrátor rolí'", "'admn'", 'NULL',),
                        ('-2', "'admin_menu'", "'Administrátor menu'", "'admn'", 'NULL',),
                        ('-3', "'admin'", "'Administrátor rolí a menu'", "'admn'", 'NULL',),
                        ),
           depends=('c_pytis_role_purposes',))

viewng('ev_pytis_valid_roles',
       (SelectRelation('e_pytis_roles', alias='main',
                       condition='main.deleted is null or main.deleted > now()'),
        ),
       grant=db_rights,
       depends=('e_pytis_roles',)
       )

viewng('ev_pytis_user_roles',
       (SelectRelation('e_pytis_roles', alias='main',
                       condition=("(main.deleted is null or main.deleted > now()) and "
                                  "main.purposeid = 'user' and "
                                  "pg_has_role(main.name, 'member') and "
                                  "main.name = session_user")),
        ),
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
            C('roleid', TInteger, constraints=('not null',), references='e_pytis_roles'),
            C('member', TInteger, constraints=('not null',), references='e_pytis_roles'),
            ),
           """Mutual memberships of roles.
Entries in this table define `member's of each `roleid'.
""",
           init_values=(('-1', '-1', '-3',),
                        ('-2', '-2', '-3',),
                        ),
           depends=('e_pytis_roles',))

viewng('ev_pytis_valid_role_members',
       (SelectRelation('e_pytis_role_members', alias='main'),
        SelectRelation('ev_pytis_valid_roles', alias='roles1',
                       exclude_columns=('roleid', 'name', 'description', 'purposeid', 'deleted',),
                       condition="roles1.roleid = main.roleid", jointype=JoinType.INNER),
        SelectRelation('ev_pytis_valid_roles', alias='roles2',
                       exclude_columns=('roleid', 'name', 'description', 'purposeid', 'deleted',),
                       condition="roles1.roleid = main.member", jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_role_members',),
       update_order=('e_pytis_role_members',),
       delete_order=('e_pytis_role_members',),
       grant=db_rights,
       depends=('e_pytis_role_members', 'ev_pytis_valid_roles',)       
       )

viewng('ev_pytis_role_members',
       (SelectRelation('e_pytis_role_members', alias='main'),
        SelectRelation('e_pytis_roles', alias='t1', exclude_columns=('roleid',),
                       condition='t1.roleid = main.roleid', jointype=JoinType.INNER),
        SelectRelation('e_pytis_roles', alias='t2', exclude_columns=('roleid',),
                       column_aliases=(('name', 'mname',),
                                       ('description', 'mdescription',),
                                       ('purposeid', 'mpurposeid',),
                                       ('deleted', 'mdeleted',),),
                       condition='t2.roleid = main.member', jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_role_members',),
       update_order=('e_pytis_role_members',),
       delete_order=('e_pytis_role_members',),
       grant=db_rights,
       depends=('e_pytis_role_members', 'e_pytis_roles',)
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
                 init_values=(("'show'", "'Viditelnost položek menu'",),
                              ("'view'", "'Prohlížení existujících záznamů'",),
                              ("'insert'", "'Vkládání nových záznamů'",),
                              ("'edit'", "'Editace existujících záznamů'",),
                              ("'delete'", "'Mazání záznamů'",),
                              ("'print'", "'Tisky'",),
                              ("'export'", "'Exporty'",),
                              ("'run'", "'Spouštění aplikačních procedur'",),
                              ))

_std_table('e_pytis_action_rights',
           (P('id', TSerial,
              doc="Just to make logging happy"),
            C('actionid', TInteger, references='c_pytis_menu_actions', constraints=('not null',)),
            C('roleid', TInteger, references='e_pytis_roles', constraints=('not null',)),
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
            C('roleid', TInteger, references='e_pytis_roles', constraints=('not null',)),
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
        SelectRelation('e_pytis_roles', alias='roles', exclude_columns=('roleid', 'description', 'purposeid',),
                       condition='main.roleid = roles.roleid', jointype=JoinType.INNER),
        ),
       insert_order=('e_pytis_menu_rights',),
       update_order=('e_pytis_menu_rights',),
       delete_order=('e_pytis_menu_rights',),
       grant=db_rights,
       depends=('e_pytis_menu_rights', 'e_pytis_menu', 'e_pytis_roles',)
       )
