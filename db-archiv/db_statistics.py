# -*- coding: utf-8 -*-

db_schemas = globals().get('Gpytis_schemas', None)
db_rights = globals().get('Gall_pytis', None)

if not db_rights:
    raise ProgramError('No rights specified! Please define Gall_pytis')


sql_raw("""
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
""",
        schemas=db_schemas,
        name='f_user_cfg')

# Form statistics

_std_table_nolog('e_pytis_form_log',
                 (P('id', TSerial),
                  C('form', TString, constraints=('NOT NULL',), index=True),
                  C('class', TString, constraints=('NOT NULL',), index=True),
                  C('info', TString, index=True),
                  C('login', TUser, constraints=('NOT NULL',), index=True),
                  C('t_start', TDateTime, constraints=('NOT NULL',), index=True),
                  C('t_show', TDateTime, constraints=('NOT NULL',)),
                  ),
                 schemas=db_schemas,
                 doc="""
Statistics about using forms by users and form opening performance.
form is fully qualified specification name.
class is pytis class name of the form instance.
login is login name of the user who has opened the form.
info is optional extra information provided by the form (e.g. sorting used).
t_start is the time when user invoked the form opening command.
t_show is the time when the form got actually ready for operation after its start.
""")

function('pytis_log_form', (TString, TString, TString, TDateTime, TDateTime), TInteger,
         body="""
insert into e_pytis_form_log (form, class, info, login, t_start, t_show) values($1, $2, $3, user, $4, $5) returning id;
""",
         grant=Gall_pytis,
         schemas=db_schemas,
         depends=('e_pytis_form_log',))

viewng('ev_pytis_form_summary',
       (R('e_pytis_form_log', alias='log', exclude_columns=('id', 'login', 't_start', 't_show',)),
        ),
       group_by="form, class, info",
       include_columns=(V(None, 'n_users', "count(distinct login)"),
                        V(None, 'n_open', "count(t_start)"),
                        V(None, 'avg_start', "extract('epoch' from avg(t_show-t_start))"),
                        V(None, 'last_used', "max(t_start)"),
                        ),
       insert=None,
       update=None,
       delete=None,
       grant=Gall_pytis,
       schemas=db_schemas,
       depends=('e_pytis_form_log',)
       )

viewng('ev_pytis_form_short_summary',
       (R('e_pytis_form_log', alias='log', exclude_columns=('id', 'login', 't_start', 't_show', 'info',)),
        ),
       group_by="form, class",
       include_columns=(V(None, 'n_users', "count(distinct login)"),
                        V(None, 'n_open', "count(t_start)"),
                        V(None, 'avg_start', "extract('epoch' from avg(t_show-t_start))"),
                        V(None, 'last_used', "max(t_start)"),
                        ),
       insert=None,
       update=None,
       delete=None,
       grant=Gall_pytis,
       schemas=db_schemas,
       depends=('e_pytis_form_log',)
       )

viewng('ev_pytis_form_users',
       (R('e_pytis_form_log', alias='log', exclude_columns=('id', 't_start', 't_show',)),
        ),
       group_by="form, class, info, login",
       include_columns=(V(None, 'n_open', "count(t_start)"),
                        V(None, 'last_used', "max(t_start)"),
                        ),
       insert=None,
       update=None,
       delete=None,
       grant=Gall_pytis,
       schemas=db_schemas,
       depends=('e_pytis_form_log',)
       )

viewng('ev_pytis_form_users_noinfo',
       (R('e_pytis_form_log', alias='log', exclude_columns=('id', 'info', 't_start', 't_show',),
           condition="log.t_start between f_user_cfg_datum_od() and f_user_cfg_datum_do()"),
        ),
       group_by="form, class, login",
       include_columns=(V(None, 'n_open', "count(t_start)"),
                        V(None, 'last_used', "max(t_start)"),
                        V(None, 'shortname', "'form/'||form"),
                        ),
       insert=None,
       update=None,
       delete=None,
       grant=Gall_pytis,
       schemas=db_schemas,
       depends=('e_pytis_form_log', 'f_user_cfg',)
       )

viewng('ev_pytis_form_user_list',
       (R('e_pytis_form_log', alias='log', exclude_columns=('id', 'form', 'class', 'info', 't_start', 't_show',)),
        ),
       group_by="login",
       insert=None,
       update=None,
       delete=None,
       grant=Gall_pytis,
       schemas=db_schemas,
       depends=('e_pytis_form_log',)
       )
