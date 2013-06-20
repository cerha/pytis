declare
  id_ int;
  current_record record;
  c text;
  cc text;
  t text;
  v text;
  key_column_ text := tg_argv[0];
  key_value_ text := null;
  detail_ text := '';
  changed bool;
begin
  if tg_op = 'DELETE' then
    current_record := old;
  else
    current_record := new;
  end if;
  for c in select regexp_split_to_table(key_column_, ', *') loop
    cc := quote_ident(c);
    execute concat('select $1.', cc, '::text') into strict v using current_record;
    if key_value_ is null then
      key_value_ := v;
    else
      key_value_ := concat(key_value_, ',', v);
    end if;
  end loop;
  for c, t in select a.attname, t.typname
              from pg_class r, pg_namespace nsp, pg_attribute a, pg_type t
              where r.relname = tg_table_name and r.relnamespace = nsp.oid and nsp.nspname = tg_table_schema and
                    a.attrelid = r.oid and a.atttypid = t.oid and a.attnum > 0
  loop
    cc := quote_ident(c);
    if tg_op = 'UPDATE' then
      execute concat('select coalesce($1.', cc, '::text, '''') != coalesce($2.', cc, '::text, '''')') into strict changed using old, new;
      if changed then
        if detail_ != '' then
          detail_ := concat(detail_, '
');
        end if;
        execute concat('select
                       (case when $1.', cc, ' is not null and $3 = ''bytea'' then ''BINARY'' when $1.', cc, ' is null then ''NULL'' else $1.', cc, '::text end) ||
                 '' -> '' ||
                       (case when $2.', cc, ' is not null and $3 = ''bytea'' then ''BINARY'' when $2.', cc, ' is null then ''NULL'' else $2.', cc, '::text end)')
                into strict v using old, new, t;
        detail_ := concat(detail_, c, ': ', v);
      end if;
    else
      execute concat('select $1.', cc, ' is not null') into strict changed using current_record;
      if changed then
        if detail_ != '' then
          detail_ := concat(detail_, '
');
        end if;
        execute concat('select (case when $1.', cc, ' is not null and $2 = ''bytea'' then ''BINARY'' else $1.', cc, '::text end)')
                into strict v using current_record, t;
        detail_ := concat(detail_, c, ': ', v);
      end if;
    end if;
  end loop;
  if tg_op != 'UPDATE' or detail_ != '' then
    insert into t_changes (timestamp, username, schemaname, tablename, operation, key_column, key_value)
           values (now(), session_user, tg_table_schema, tg_table_name, tg_op, key_column_, key_value_)
           returning id into strict id_;
    insert into t_changes_detail (id, detail) values (id_, detail_);
  end if;
  return null;
end;
