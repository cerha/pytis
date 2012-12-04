# -*- coding: utf-8

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class CmsUsers(sql.SQLTable):
    name = 'cms_users'
    fields = (
              sql.PrimaryColumn('uid', pytis.data.Serial()),
              sql.Column('login', pytis.data.String(not_null=True), unique=True),
              sql.Column('fullname', pytis.data.String(not_null=True)),
              sql.Column('passwd', pytis.data.String(not_null=True)),
             )
    with_oids = True
    depends_on = ()
    access_rights = ()

