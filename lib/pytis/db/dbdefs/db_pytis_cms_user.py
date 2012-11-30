# -*- coding: utf-8

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class CmsUsersTable(sql.SQLTable):
    name = 'cms_users_table'
    fields = (
              sql.PrimaryColumn('uid', pytis.data.Serial(not_null=False)),
             )
    with_oids = True
    depends_on = ()
    access_rights = ()

