#-*- coding: utf-8 -*-
"""Tento soubor obsahuje definice databázových objektů pro ukládání a čtení
uživatelské konfigurace. Do hlavního db.py jednotlivých projektů
se přidá symbolickým přilinkováním do odpovídajícího adresáře db v projektu
a přidáním příkazů

Gpytis_config = <práva pro objekty tohoto souboru>
execfile('db_pytis_config.py', copy.copy(globals()))
"""

db_rights = globals().get('Gpytis_config', None)
db_schemas = globals().get('Gpytis_config_schemas', None)

if not db_rights:
    raise ProgramError('No rights specified! Please define Gpytis_config')

table('_pytis_config',
      (P('uzivatel', TUser),
       C('config', TString)),
      grant=db_rights,
      schemas=db_schemas,
      doc="""Tabulka pro ukládání configu pytisu."""      
      )

function('read_pytis_config',
         (),
         TString,
         body=("insert into _pytis_config (uzivatel, config) "
               "select current_user, NULL::text "
               "where (select current_user) not in "
               "(select uzivatel from _pytis_config); "
               "select config from _pytis_config "
               "where uzivatel = (select current_user)"),
         doc="""Funkce na zjišťování configu pro daného uživatele""",
         depends=('_pytis_config',))

function('write_pytis_config',
         (TString,),
         TString,
         body=("update _pytis_config set config = $1 "
               "where uzivatel = (select current_user);"
               "insert into _pytis_config (uzivatel, config) "
               "select current_user, $1::text "
               "where (select current_user) not in "
               "(select uzivatel from _pytis_config); "               
               "select config from _pytis_config "
               "where uzivatel = (select current_user)"
               ),
         doc="""Funkce na zápis configu pro daného uživatele.""",
         depends=('_pytis_config',))


table('e_pytis_form_profiles',
      (P('id', TSerial),
       C('username', TUser, constraints=('NOT NULL',)),
       C('fullname', TString, constraints=('NOT NULL',)),
       C('profile_id', TString, ),
       C('profile_data', TString, constraints=('NOT NULL',))),
      sql='UNIQUE (username, fullname, profile_id)',
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis form configuration storage."""
      )
