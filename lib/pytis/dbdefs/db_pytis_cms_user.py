# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs import cms_schemas

class CmsUsersTable(sql.SQLTable):
    name = 'cms_users_table'
    schemas = cms_schemas.value(globals())
    fields = (
              sql.PrimaryColumn('uid', pytis.data.Serial()),
             )
    with_oids = True
    depends_on = ()
    access_rights = ()

