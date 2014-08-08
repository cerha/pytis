# -*- coding: utf-8

from __future__ import unicode_literals

import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import default_access_rights, pytis_schemas
from pytis.dbdefs.db_pytis_common import XChanges

class EPytisActionLog(sql.SQLTable):
    """Pytis user actions log."""
    name = 'e_pytis_action_log'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('timestamp', pytis.data.DateTime(not_null=True)),
              sql.Column('username', pytis.data.Name(not_null=True)),
              sql.Column('spec_name', pytis.data.String(not_null=True)),
              sql.Column('form_name', pytis.data.String(not_null=True)),
              sql.Column('action', pytis.data.String(not_null=True)),
              sql.Column('info', pytis.data.String(not_null=False)),
              )
    inherits = (XChanges,)
    with_oids = True
    depends_on = ()
    access_rights = default_access_rights.value(globals())
