# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs import XChanges, default_access_rights, pytis_schemas

class FUserCfg(sql.SQLRaw):
    name = 'f_user_cfg'
    schemas = pytis_schemas.value(globals())
    @classmethod
    def sql(class_):
        return """
create or replace function f_user_cfg_datum_od() returns date as $$
begin
  if (select count(*) from pg_class where relname='bv_users_cfg') > 0 then
    return (select datum_od from bv_users_cfg);
  elsif(select count(*) from pg_class where relname='cv_users_cfg') > 0 then
    return (select datum_od from cv_users_cfg);
  else
    return '2000-01-01'::date;
  end if;
end;
$$ language plpgsql stable;
create or replace function f_user_cfg_datum_do() returns date as $$
begin
  if (select count(*) from pg_class where relname='bv_users_cfg') > 0 then
    return (select datum_do from bv_users_cfg);
  elsif(select count(*) from pg_class where relname='cv_users_cfg') > 0 then
    return (select datum_do from cv_users_cfg);
  else
    return '2099-12-31'::date;
  end if;
end;
$$ language plpgsql stable;
"""
    depends_on = ()

class EPytisFormLog(sql.SQLTable):
    """
    Statistics about using forms by users and form opening performance.
    form is fully qualified specification name.
    class is pytis class name of the form instance.
    login is login name of the user who has opened the form.
    info is optional extra information provided by the form (e.g. sorting used).
    t_start is the time when user invoked the form opening command.
    t_show is the time when the form got actually ready for operation after its start.
    """
    name = 'e_pytis_form_log'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('form', pytis.data.String(not_null=True), index=True),
              sql.Column('class', pytis.data.String(not_null=True), index=True),
              sql.Column('info', pytis.data.String(not_null=False), index=True),
              sql.Column('login', pytis.data.Name(not_null=True), index=True),
              sql.Column('t_start', pytis.data.DateTime(not_null=True), index=True),
              sql.Column('t_show', pytis.data.DateTime(not_null=True)),
              )
    inherits = (XChanges,)
    with_oids = True
    depends_on = ()
    access_rights = default_access_rights.value(globals())

class PytisLogForm(sql.SQLFunction):
    schemas = pytis_schemas.value(globals())
    name = 'pytis_log_form'
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.DateTime()),
                 sql.Column('', pytis.data.DateTime()),)
    result_type = pytis.data.Integer()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisFormLog,)
    access_rights = default_access_rights.value(globals())

    def body(self):
        return """
insert into e_pytis_form_log (form, class, info, login, t_start, t_show)
       values($1, $2, $3, user, $4, $5) returning id;
"""

class EvPytisFormSummary(sql.SQLView):
    name = 'ev_pytis_form_summary'
    schemas = pytis_schemas.value(globals())
    @classmethod
    def query(cls):
        log = sql.t.EPytisFormLog.alias('log')
        return sqlalchemy.select(
            cls._exclude(log, 'id', 'login', 't_start', 't_show') +
            [sql.gL("count(distinct login)").label('n_users'),
             sql.gL("count(t_start)").label('n_open'),
             sql.gL("extract('epoch' from avg(t_show-t_start))").label('avg_start'),
             sql.gL("max(t_start)").label('last_used')],
            from_obj=[log]
        ).group_by('form', 'class', 'info')

    depends_on = (EPytisFormLog,)
    access_rights = default_access_rights.value(globals())

class EvPytisFormShortSummary(sql.SQLView):
    name = 'ev_pytis_form_short_summary'
    schemas = pytis_schemas.value(globals())
    @classmethod
    def query(cls):
        log = sql.t.EPytisFormLog.alias('log')
        return sqlalchemy.select(
            cls._exclude(log, 'id', 'login', 't_start', 't_show', 'info') +
            [sql.gL("count(distinct login)").label('n_users'),
             sql.gL("count(t_start)").label('n_open'),
             sql.gL("extract('epoch' from avg(t_show-t_start))").label('avg_start'),
             sql.gL("max(t_start)").label('last_used')],
            from_obj=[log]
        ).group_by('form', 'class')

    depends_on = (EPytisFormLog,)
    access_rights = default_access_rights.value(globals())

class EvPytisFormUsers(sql.SQLView):
    name = 'ev_pytis_form_users'
    schemas = pytis_schemas.value(globals())
    @classmethod
    def query(cls):
        log = sql.t.EPytisFormLog.alias('log')
        return sqlalchemy.select(
            cls._exclude(log, 'id', 't_start', 't_show') +
            [sql.gL("count(t_start)").label('n_open'),
             sql.gL("max(t_start)").label('last_used')],
            from_obj=[log]
        ).group_by('form', 'class', 'info', 'login')

    depends_on = (EPytisFormLog,)
    access_rights = default_access_rights.value(globals())

class EvPytisFormUsersNoinfo(sql.SQLView):
    name = 'ev_pytis_form_users_noinfo'
    schemas = pytis_schemas.value(globals())
    @classmethod
    def query(cls):
        log = sql.t.EPytisFormLog.alias('log')
        return sqlalchemy.select(
            cls._exclude(log, 'id', 'info', 't_start', 't_show') +
            [sql.gL("count(t_start)").label('n_open'),
             sql.gL("max(t_start)").label('last_used'),
             sql.gL("'form/'||form").label('shortname')],
            from_obj=[log],
            whereclause='log.t_start between f_user_cfg_datum_od() and f_user_cfg_datum_do()'
        ).group_by('form', 'class', 'login')

    depends_on = (EPytisFormLog, FUserCfg,)
    access_rights = default_access_rights.value(globals())

class EvPytisFormUserList(sql.SQLView):
    name = 'ev_pytis_form_user_list'
    schemas = pytis_schemas.value(globals())
    @classmethod
    def query(cls):
        log = sql.t.EPytisFormLog.alias('log')
        return sqlalchemy.select(
            cls._exclude(log, 'id', 'form', 'class', 'info', 't_start', 't_show'),
            from_obj=[log]
        ).group_by('login')

    depends_on = (EPytisFormLog,)
    access_rights = default_access_rights.value(globals())
