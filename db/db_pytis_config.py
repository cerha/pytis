#-*- coding: utf-8 -*-
"""Tento soubor obsahuje definice databázových objektů pro ukládání a čtení
uživatelské konfigurace. Do hlavního db.py jednotlivých projektů
se přidá symbolickým přilinkováním do odpovídajícího adresáře db v projektu
a přidáním příkazů

Gpytis_config = <práva pro objekty tohoto souboru>
execfile('db_pytis_config.py', copy.copy(globals()))
"""

db_rights = globals().get('Gall_pytis', None)
db_schemas = globals().get('Gpytis_schemas', None)

if not db_rights:
    raise ProgramError('No rights specified! Please define Gall_pytis')

_std_table_nolog('e_pytis_config',
      (P('id', TSerial),
       C('username', TUser, constraints=('UNIQUE NOT NULL',)),
       C('pickle', TString, constraints=('NOT NULL',)),
       ),
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis application configuration storage."""
      )

_std_table_nolog('e_pytis_form_settings',
      (P('id', TSerial),
       C('username', TUser, constraints=('NOT NULL',)),
       C('spec_name', TString, constraints=('NOT NULL',)),
       C('form_name', TString, constraints=('NOT NULL',)),
       C('pickle', TString, constraints=('NOT NULL',)),
       C('dump', TString),
       ),
      sql='UNIQUE (username, spec_name, form_name)',
      grant=db_rights,
      schemas=db_schemas,
      doc="""Storage of pytis profile independent form settings."""
      )

_std_table_nolog('e_pytis_form_profiles',
      (P('id', TSerial),
       C('username', TUser, constraints=('NOT NULL',)),
       C('fullname', TString, constraints=('NOT NULL',)),
       C('profile_id', TString, constraints=('NOT NULL',)),
       C('title', TString, constraints=('NOT NULL',)),
       C('pickle', TString, constraints=('NOT NULL',)),
       C('dump', TString),
       C('errors', TString),
       ),
      sql='UNIQUE (username, fullname, profile_id)',
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis form configuration storage."""
      )

_std_table_nolog('e_pytis_aggregated_views',
      (P('id', TSerial),
       C('username', TUser, constraints=('NOT NULL',)),
       C('spec_name', TString, constraints=('NOT NULL',)),
       C('aggregated_view_id', TString, constraints=('NOT NULL',)),
       C('title', TString, constraints=('NOT NULL',)),
       C('pickle', TString, constraints=('NOT NULL',)),
       ),
      sql='UNIQUE (username, spec_name, aggregated_view_id)',
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis aggregated views storage."""
      )
