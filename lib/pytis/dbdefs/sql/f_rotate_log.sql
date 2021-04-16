declare
  n_records int := 1000000;
  table_lock bigint := 201302131505;
  n_id int;
  time_from timestamp;
  time_to timestamp;
  date_from text;
  date_to text;
  tablename text;
  dtablename text;
  n int;
begin
  perform pg_advisory_xact_lock(table_lock);
  loop
    if (select count(*) < n_records from t_changes) then
      exit;
    end if;
    select id into strict n_id from public.t_changes order by id offset n_records-1 limit 1;
    select min(timestamp) into strict time_from from public.t_changes;
    select max(timestamp) into strict time_to from public.t_changes where id<=n_id;
    date_from := to_char(time_from, 'YYMMDD');
    date_to := to_char(time_to, 'YYMMDD');
    tablename := concat('t_changes_', date_from, '_', date_to);
    dtablename := concat('t_changes_detail_', date_from, '_', date_to);
    loop
      if (select count(*)=0 from pg_class join pg_namespace on relnamespace = pg_namespace.oid
                             where relname = tablename and nspname = 'public') then
        exit;
      end if;
      tablename := concat(tablename, 'x');
      dtablename := concat(dtablename, 'x');
    end loop;
    execute concat('create table public.', tablename, '
                    as select * from public.t_changes where id<=', n_id);
    execute concat('create table public.', dtablename, '
                    as select * from public.t_changes_detail where id<=', n_id);
    delete from public.t_changes where id<=n_id;
    execute concat('create index ', tablename, '__id__index on public.', tablename, ' (id)');
    execute concat('create index ', tablename, '__timestamp__index on public.', tablename, '
                    (timestamp)');
    execute concat('create index ', tablename, '__username__index on public.', tablename, '
                    (username)');
    execute concat('create index ', tablename, '__tablename__index on public.', tablename, '
                    (tablename)');
    execute concat('create index ', tablename, '__key_value__index on public.', tablename, '
                    (key_value)');
    execute concat('create index ', dtablename, '__id__index on public.', dtablename, '
                    (id)');
  end loop;
end;
