#-*- coding: utf-8 -*-
"""Gensql specification of database objects used by Pytis CMS.

This specification is designed to be included in project specific gensql script.

Requirements:

The top level gensql script must define the variable
'http_attachment_storage_rights' which defines the DB rights to be used for the
table storing http storage access keys.  These rights should contain 'select'
(read only) rights for the role used by the webserver (such as 'www-data') and
read/write rights for all DB users using the storage.

"""
import pytis.data as pd

table('e_pytis_http_attachment_storage_keys',
      doc="Store HttpAttachmentStorage access keys.",
      columns=(PrimaryColumn('key_id', pd.Serial()),
               Column('username', pd.String(), constraints=('not null',)),
               Column('uri', pd.String(), constraints=('not null',)),
               Column('created', pd.DateTime(), constraints=('not null',), default='now()'),
               Column('readonly', pd.Boolean(), constraints=('not null',), default='True'),
               Column('key', pd.String(), constraints=('not null',))),
      sql='UNIQUE (username, uri)',
      grant=http_attachment_storage_rights)
