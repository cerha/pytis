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
Relation = SelectRelation

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

_std_table_nolog('e_pytis_form_profile_base',
      (P('id', TSerial),
       C('username', TUser, constraints=('NOT NULL',)),
       C('spec_name', TString, constraints=('NOT NULL',)),
       C('profile_id', TString, constraints=('NOT NULL',)),
       C('title', TString, constraints=('NOT NULL',)),
       C('pickle', TString, constraints=('NOT NULL',)),
       C('dump', TString),
       C('errors', TString),
       ),
      sql='UNIQUE (username, spec_name, profile_id)',
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis form configuration storage."""
      )

_std_table_nolog('e_pytis_form_profile_params',
      (P('id', TSerial),
       C('username', TUser, constraints=('NOT NULL',)),
       C('spec_name', TString, constraints=('NOT NULL',)),
       C('form_name', TString, constraints=('NOT NULL',)),
       C('profile_id', TString, constraints=('NOT NULL',)),
       C('pickle', TString, constraints=('NOT NULL',)),
       C('dump', TString),
       C('errors', TString),
       ),
      sql='''UNIQUE (username, spec_name, form_name, profile_id)''',
      grant=db_rights,
      schemas=db_schemas,
      doc="""Pytis form profile form type specific parameters."""
      )

viewng('ev_pytis_form_profiles',
       relations=(Relation('e_pytis_form_profile_base', alias='profile', key_column='id',
                           exclude_columns=('id', 'username', 'spec_name', 'profile_id', 'pickle', 'dump', 'errors')),
                  Relation('e_pytis_form_profile_params', alias='params', key_column='lang',
                           jointype=JoinType.RIGHT_OUTER,
                           condition=('profile.username = params.username and '
                                      'profile.spec_name = params.spec_name and '
                                      'profile.profile_id = params.profile_id'),
                           exclude_columns=('id', 'pickle', 'dump', 'errors')),
                  ),
       include_columns=(ViewColumn(None, alias='id', sql="profile.id||'.'||params.id"),
                        ViewColumn(None, alias='fullname', sql="'form/'|| params.form_name ||'/'|| profile.spec_name ||'//'"),
                        ViewColumn(None, alias='errors', sql="case when profile.errors is not null and params.errors is not null then profile.errors ||'\n'||params.errors else coalesce(profile.errors, params.errors) end"),
                        ViewColumn(None, alias='dump', sql="case when profile.dump is not null and params.dump is not null then profile.dump ||'\n'||params.dump else coalesce(profile.dump, params.dump) end"),
                        ViewColumn(None, alias='user_defined', sql="profile.id is not NULL"),
                        ViewColumn('profile.pickle', alias='pickled_filter'), 
                        ViewColumn('params.pickle', alias='pickled_params'), 
                        ),
       insert=None,
       update=None,
       delete=('delete from e_pytis_form_profile_base where username = old.username and '
               'spec_name = old.spec_name and profile_id = old.profile_id'),
       depends=('e_pytis_form_profile_base', 'e_pytis_form_profile_params'),
       grant=db_rights,
       schemas=db_schemas,
       doc="Pytis profiles.",
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
