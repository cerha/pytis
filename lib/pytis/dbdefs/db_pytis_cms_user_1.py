# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs import cms_schemas

class CmsUsers(sql.SQLTable):
    name = 'pytis_cms_users'
    schemas = cms_schemas.value(globals())
    fields = (
              sql.PrimaryColumn('uid', pytis.data.Serial()),
              sql.Column('login', pytis.data.String(not_null=True), unique=True),
              sql.Column('fullname', pytis.data.String(not_null=True)),
              sql.Column('passwd', pytis.data.String(not_null=True)),
             )
    with_oids = True
    depends_on = ()
    access_rights = ()

