# -*- coding: utf-8

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class CmsUserRoleAssignment(sql.SQLTable):
    """Binding table assigning CMS roles to CMS users."""
    name = 'cms_user_role_assignment'
    schemas = db.cms_schemas.value(globals())
    fields = (
              sql.PrimaryColumn('user_role_id', pytis.data.Serial()),
              sql.Column('uid', pytis.data.Integer(not_null=True), references=sql.gA(db.cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('role_id', pytis.data.Integer(not_null=True), references=sql.gA('cms_roles', ondelete='CASCADE')),
             )
    with_oids = True
    unique = (('uid', 'role_id',),)
    depends_on = (db.CmsRoles,)
    access_rights = db.cms_rights.value(globals())

class CmsSession(sql.SQLTable):
    """Web user session information for authentication and login history."""
    name = 'cms_session'
    schemas = db.cms_schemas.value(globals())
    fields = (
              sql.PrimaryColumn('session_id', pytis.data.Serial()),
              sql.Column('uid', pytis.data.Integer(not_null=True), references=sql.gA(db.cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('session_key', pytis.data.String(not_null=True)),
              sql.Column('last_access', pytis.data.DateTime(not_null=True)),
             )
    with_oids = True
    unique = (('uid', 'session_key',),)
    depends_on = ()
    access_rights = db.cms_rights_rw.value(globals())

class CmsSessionLogData(sql.SQLTable):
    """Log of web user logins (underlying data)."""
    name = 'cms_session_log_data'
    schemas = db.cms_schemas.value(globals())
    fields = (
              sql.PrimaryColumn('log_id', pytis.data.Serial()),
              sql.Column('session_id', pytis.data.Integer(not_null=False), references=sql.gA('cms_session', ondelete='SET NULL')),
              sql.Column('uid', pytis.data.Integer(not_null=False), references=sql.gA(db.cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('login', pytis.data.String(not_null=True)),
              sql.Column('success', pytis.data.Boolean(not_null=True), default=False),
              sql.Column('start_time', pytis.data.DateTime(not_null=True)),
              sql.Column('end_time', pytis.data.DateTime(not_null=False)),
              sql.Column('ip_address', pytis.data.String(not_null=True)),
              sql.Column('user_agent', pytis.data.String(not_null=False)),
              sql.Column('referer', pytis.data.String(not_null=False)),
             )
    with_oids = True
    depends_on = ()
    access_rights = db.cms_rights_rw.value(globals())

class CmsAccessLogData(sql.SQLTable):
    """Log of cms page access."""
    name = 'cms_access_log_data'
    schemas = db.cms_schemas.value(globals())
    fields = (
              sql.PrimaryColumn('log_id', pytis.data.Serial()),
              sql.Column('timestamp', pytis.data.DateTime(not_null=True)),
              sql.Column('uri', pytis.data.String(not_null=True)),
              sql.Column('uid', pytis.data.Integer(not_null=False), references=sql.gA(db.cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('modname', pytis.data.String(not_null=False)),
              sql.Column('action', pytis.data.String(not_null=False)),
              sql.Column('ip_address', pytis.data.String(not_null=True)),
              sql.Column('user_agent', pytis.data.String(not_null=False)),
              sql.Column('referer', pytis.data.String(not_null=False)),
             )
    with_oids = True
    depends_on = ()
    access_rights = db.cms_rights_rw.value(globals())

class X177(sql.SQLRaw):
    name = '@177'
    schemas = db.cms_schemas.value(globals())
    @classmethod
    def sql(class_):
        return """create or replace rule session_delete as on delete to cms_session do ( update cms_session_log_data set end_time=old.last_access WHERE session_id=old.session_id;);"""
    depends_on = (CmsSession, CmsSessionLogData,)

