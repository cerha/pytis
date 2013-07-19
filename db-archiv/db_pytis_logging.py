#-*- coding: utf-8 -*-
"""Definice databázových objektů pro logování uživatelských operací. 

"""
db_rights = globals().get('Gall_pytis', None)
db_schemas = globals().get('Gpytis_schemas', None)

if not db_rights:
    raise ProgramError('No rights specified! Please define Gall_pytis')

_std_table_nolog('e_pytis_action_log',
    (P('id', TSerial),
     C('timestamp', TDateTime, constraints=('NOT NULL',)),
     C('username', TUser, constraints=('NOT NULL',)),
     C('spec_name', TString, constraints=('NOT NULL',)),
     C('form_name', TString, constraints=('NOT NULL',)),
     C('action', TString, constraints=('NOT NULL',)),
     C('info', TString),
     ),
    grant=db_rights,
    schemas=db_schemas,
    doc="""Pytis user actions log."""
    )
