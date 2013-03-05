# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class CmsUserRoles(sql.SQLView):
    name = 'cms_user_roles'
    schemas = db.cms_schemas.value(globals())
    @classmethod
    def query(cls):
        a_ = sql.t.CmsUserRoleAssignment.alias('a')
        u = sql.t.CmsUsers.alias('u')
        r_ = sql.t.CmsRoles.alias('r')
        return sqlalchemy.select(
            cls._exclude(a_) +
            cls._exclude(r_, 'role_id') +
            [u.c.login.label('login'),
             u.c.fullname.label('fullname')],
            from_obj=[a_.join(u, sql.gR('a.uid = u.uid')).join(r_, sql.gR('a.role_id = r.role_id'))]
            )

    def on_insert(self):
        return ("INSERT INTO cms_user_role_assignment (user_role_id, uid, role_id) VALUES (new.user_role_id, new.uid, new.role_id) RETURNING user_role_id, uid, role_id, NULL::text, NULL::text, NULL::text, NULL::text, NULL::text",)
    def on_update(self):
        return ("UPDATE cms_user_role_assignment SET uid = new.uid, role_id = new.role_id WHERE user_role_id=old.user_role_id",)
    def on_delete(self):
        return ("DELETE FROM cms_user_role_assignment WHERE user_role_id = old.user_role_id",)
    depends_on = (db.CmsUserRoleAssignment, db.CmsUsers, db.CmsRoles,)
    access_rights = db.cms_rights.value(globals())

class CmsSessionLog(sql.SQLView):
    """Log of web user logins (user visible information)."""
    name = 'cms_session_log'
    schemas = db.cms_schemas.value(globals())
    @classmethod
    def query(cls):
        l = sql.t.CmsSessionLogData.alias('l')
        s = sql.t.CmsSession.alias('s')
        u = sql.t.CmsUsers.alias('u')
        return sqlalchemy.select(
            cls._exclude(l, 'end_time') +
            [u.c.fullname.label('fullname'),
             sql.gL("coalesce(l.end_time, s.last_access) - l.start_time").label('duration'),
             sql.gL("s.session_id IS NOT NULL AND age(s.last_access)<'1 hour'").label('active')],
            from_obj=[l.outerjoin(s, sql.gR('l.session_id = s.session_id')).join(u, sql.gR('l.uid = u.uid'))]
            )

    def on_insert(self):
        return ("""INSERT INTO cms_session_log_data (session_id, uid, login, success, 
                                                    start_time, ip_address, user_agent, referer)
               VALUES (new.session_id, new.uid, new.login, new.success, 
                       new.start_time, new.ip_address, new.user_agent, new.referer)
               RETURNING log_id, session_id, uid, login, success, 
                         start_time, ip_address, user_agent, referer,
                         NULL::text, NULL::interval, NULL::boolean""",)
    update_order = (db.CmsSessionLogData,)
    no_update_columns = ('duration', 'active',)
    delete_order = (db.CmsSessionLogData,)
    depends_on = (db.CmsSession, db.CmsSessionLogData, db.CmsUsers,)
    access_rights = db.cms_rights.value(globals())

