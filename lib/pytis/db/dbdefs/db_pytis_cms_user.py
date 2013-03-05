# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class CmsUsersTable(sql.SQLTable):
    name = 'cms_users_table'
    schemas = db.cms_schemas.value(globals())
    fields = (
              sql.PrimaryColumn('uid', pytis.data.Serial()),
             )
    with_oids = True
    depends_on = ()
    access_rights = ()

