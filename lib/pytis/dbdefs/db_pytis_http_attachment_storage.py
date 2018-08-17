# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import http_attachment_storage_rights, pytis_schemas

class EPytisHttpAttachmentStorageKeys(sql.SQLTable):
    """Store HttpAttachmentStorage access keys."""
    name = 'e_pytis_http_attachment_storage_keys'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('key_id', pytis.data.Serial()),
              sql.Column('username', pytis.data.String(not_null=True)),
              sql.Column('uri', pytis.data.String(not_null=True)),
              sql.Column('created', pytis.data.DateTime(not_null=True),
                         default=sqlalchemy.text('now()')),
              sql.Column('readonly', pytis.data.Boolean(not_null=True), default=True),
              sql.Column('key', pytis.data.String(not_null=True)),
              )
    with_oids = True
    unique = (('username', 'uri',),)
    depends_on = ()
    access_rights = http_attachment_storage_rights.value(globals())
