declare
  date_to_1 date;
  tablename text;
begin
  if date_from > '2099-12-31'::date then
    date_from := '2099-12-31'::date;
  end if;
  if date_to > '2099-12-31'::date then
    date_to := '2099-12-31'::date;
  end if;
  if date_from < '2000-01-01'::date then
    date_from := '2000-01-01'::date;
  end if;
  if date_to < '2000-01-01'::date then
    date_to := '2000-01-01'::date;
  end if;

  date_to_1 := date_to + '1 day'::interval;
  return query select * from v_changes v
                      where date_from <= v.timestamp and v.timestamp < date_to_1 and
                            v.username = coalesce(username_, v.username) and
                            v.tablename = coalesce(tablename_, v.tablename) and
                            v.key_value::text = coalesce(key_value_, v.key_value) and
                            v.detail like '%'||coalesce(detail_, '%')||'%' and
                            (search_path_ is null or schemaname in
                              (select * from
                                 regexp_split_to_table(coalesce(search_path_, ''), ' *, *')));
  for tablename in select relname from pg_class join pg_namespace on relnamespace = pg_namespace.oid
                          where nspname = 'public' and
                                relname ~ '^t_changes_......_......x*$' and
                                substring(relname from 11 for 6) <= to_char(date_to, 'YYMMDD') and
                                substring(relname from 18 for 6) >= to_char(date_from, 'YYMMDD')
  loop
    return query
      execute concat(
               'select t.*, detail ',
                       'from public.', tablename, ' t join ',
                       'public.t_changes_detail_', substring(tablename from 11),
                       ' d using(id) ',
                       'where $1 <= t.timestamp and t.timestamp < $2 and ',
                             't.username = coalesce($3, t.username) and ',
                             't.tablename = coalesce($4, t.tablename) and ',
                             't.key_value::text = coalesce($5, t.key_value) and ',
                             'coalesce(d.detail, '''') like ''%''||coalesce($6, ''%'')||''%'' and ',
                             '($7 is null or schemaname in (select * from '
                             'regexp_split_to_table(coalesce($7, ''''), '' *, *'')))')
              using date_from, date_to_1, username_, tablename_, key_value_, detail_, search_path_;
  end loop;
end;
