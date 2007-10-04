#-*- coding: iso-8859-2 -*-
"""Tento soubor obsahuje definice databázových objektů pro ukládání a čtení
uživatelské konfigurace. Do hlavního db.py jednotlivých projektů
se přidá symbolickým přilinkováním do odpovídajícího adresáře db v projektu
a přidáním příkazů

Gpytis_config = <práva pro objekty tohoto souboru>
execfile('db_pytis_config.py', copy.copy(globals()))
"""

db_rights = globals().get('Gpytis_config', None)

if not db_rights:
    raise ProgramError('No rights specified! Please define Gpytis_config')

table('_pytis_config',
      (P('uzivatel', TUser),
       C('config', TString)),
      grant=db_rights,
      doc="""Tabulka pro ukládání configu pytisu."""      
      )

function('read_pytis_config',
         (),
         TString,
         body=("insert into _pytis_config select current_user, NULL::text "
               "where (select current_user) not in "
               "(select uzivatel from _pytis_config); "
               "select config from _pytis_config "
               "where (select current_user) = "
               "(select uzivatel from _pytis_config)"),
         doc="""Funkce na zjišťování configu pro daného uživatele""",
         depends=('_pytis_config',))

function('write_pytis_config',
         (TString,),
         TString,
         body=("update _pytis_config set config = $1 "
               "where (select current_user) = "
               "(select uzivatel from _pytis_config); "
               "insert into _pytis_config select current_user, $1::text "
               "where (select current_user) not in "
               "(select uzivatel from _pytis_config); "
               "select config from _pytis_config "
               "where (select current_user) = "
               "(select uzivatel from _pytis_config)"
               ),
         doc="""Funkce na zápis configu pro daného uživatele.""",
         depends=('_pytis_config',))

