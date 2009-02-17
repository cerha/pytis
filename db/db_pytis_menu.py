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

_std_table_nolog('c_pytis_role_purposes',
                 (P('purposeid', TInteger),
                  C('purpose', 'varchar(32)', constraints=('unique', 'not null',)),),
                 """There are three kinds of roles:
1. Menu and access administrator roles.  Definitions of these roles may be changed
   only by the database administrators.
2. Roles corresponding to system accounts (login roles).
3. Pure application roles.
""",
                 init_values=(('1', "'Správcovská'",),
                              ('2', "'Uživatelská'",),
                              ('3', "'Aplikační'",),)
            )

_std_table('e_pytis_roles',
           (P('roleid', TSerial),
            C('name', TUser, constraints=('unique', 'not null',),),
            C('description', 'varchar(64)'),
            C('purposeid', TInteger, constraints=('not null',), default="1",
              references='c_pytis_role_purposes'),
            C('deleted', TDate),),
            """Application user roles.""",
           init_values=(('-1', "'admin_roles'", "'Administrátor rolí'", '1', 'NULL',),
                        ('-2', "'admin_menu'", "'Administrátor menu'", '1', 'NULL',),
                        ('-3', "'admin'", "'Administrátor rolí a menu'", '1', 'NULL',),
                        ),
           depends=('c_pytis_role_purposes',))

_std_table('e_pytis_role_members',
           (P('id', TSerial,
              doc="Just to make logging happy"),
            C('roleid', TInteger, constraints=('not null',), references='e_pytis_roles'),
            C('member', TInteger, constraints=('not null',), references='e_pytis_roles'),
            ),
           """Mutual memberships of roles.
Entries in this table define `member's of each `roleid'.
""",
           init_values=(('-1', '1', '3',),
                        ('-2', '2', '3',),
                        ),
           depends=('e_pytis_roles',))
