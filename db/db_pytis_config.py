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

table('e_pytis_config',
      (P('username', TUser, constraints=('NOT NULL',)),
       C('config', TString, constraints=('NOT NULL',))),
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis application configuration storage."""
      )

table('e_pytis_form_profiles',
      (P('id', TSerial),
       C('username', TUser, constraints=('NOT NULL',)),
       C('fullname', TString, constraints=('NOT NULL',)),
       C('profile_id', TString, constraints=('NOT NULL',)),
       C('profile_name', TString, constraints=('NOT NULL',)),
       C('pickle', TString, constraints=('NOT NULL',)),
       C('dump', TString),
       C('errors', TString)),
      sql='UNIQUE (username, fullname, profile_id)',
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis form configuration storage."""
      )
