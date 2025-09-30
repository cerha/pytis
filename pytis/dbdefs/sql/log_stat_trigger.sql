declare
  -- Popis proměnných:
  --
  -- Klíčový sloupec tabulky, jehož hodnota se nesmí měnit. To znemožňuje použití logovacího statement
  -- triggeru pro některé naše tabulky, kde je primární sloupec kód číselníku, který uživatelé
  -- čas od času mění (což je nehezká věc, které bychom měli zabránit rozšířením tabulky o ID které
  -- by doplnilo číselníkový kód, který nyní slouží jako primární klíč tabulky). Většina tabulek
  -- (a zvláště ty, kde bude tuto trigger funkci výhodné použít) má primární sloupec typu serial
  -- a je zajištěno, že se měnit nemůže. Dále, na rozdíl od logovacího each row triggeru,
  -- počítáme s tím, že klíč tabulky bude vždy jedno-sloupcový. Pokud by někde existovala tabulka
  -- s více primárními sloupci, musel by se key_column, key_value sestavit z více hodnot v cyklu,
  -- zatím na to, vzhledem k tomu že to nepoužíváme, rezignujeme.
  key_column text := tg_argv[0];
  -- Selecty pro vytvoření detailu logu. Pro insert a delete vytvoří string kde na každém řádku
  -- bude klíč slupce a jeho hodnota pro všechny sloupce tabulky. Pro update bude na každém řádku
  -- klíč sloupce původní a nová hodnota pro všechny políčka kde došlo ke změně hodnoty.
  -- Null hodnoty jsou nahrazeny stringem "NULL" a místo hodnoty binárního políčka uvádíme string
  -- "BINARY".
  col_ins_del_maska text := 'format(''%1$s: %2$s\\n'',
                                    case when %1$s is NULL then ''NULL''
                                         when ''%3$s'' = ''bytea'' then ''BINARY''
                                         else %1$s::text end)';
  col_upd_maska text := 'case when o.%1$s is distinct from n.%1$s then
                         format(''%1$s: %2$s -> %2$s\\n'',
                                case when o.%1$s is NULL then ''NULL''
                                     when ''%3$s'' = ''bytea'' then ''BINARY''
                                     else o.%1$s::text end,
                                case when n.%1$s is NULL then ''NULL''
                                     when ''%3$s'' = ''bytea'' then ''BINARY''
                                     else n.%1$s::text end)
                              else '''' end';
  -- Proměnná do které uložíme vygenerovanou část SQL dotazu pro vytvoření detailu logu
  oselect text;
  -- SQL příkaz pro vložení logu při insert a delete nebo při update logované tabulky
  qinsdel text := 'insert into public.v_changes
            (timestamp, username, schemaname, tablename, operation, key_column, key_value, detail)
            select now(), session_user, ''%s'', ''%s'', ''%s'', ''%s'', %s::text, rtrim(%s, ''\n'')
            from %s;';
  qupd text := 'insert into public.v_changes
          (timestamp, username, schemaname, tablename, operation, key_column, key_value, detail)
          select timestamp, username, schemaname, tablename, operation, key_column, key_value,
                 rtrim(detail, ''\n'')
               from (
          select now() as timestamp, session_user as username, ''%s'' as schemaname,
                 ''%s'' as tablename, ''%s'' as operation, ''%s'' as key_column,
                 n.%s::text as key_value, %s as detail
          from old_table o, new_table n where n.%s = o.%s) a where detail != '''';';
begin
  if tg_op in ('DELETE', 'INSERT') then
    -- Vytvoříme část SQL selectu pro vytvoření detailu logu s hodnotami při insert a delete
    -- logované tabulky
    select string_agg(format(col_ins_del_maska, a.attname, '%s', t.typname),
                      ' || ') as oselect
    from pg_class r, pg_namespace nsp, pg_attribute a, pg_type t into oselect
    where r.relname = tg_table_name
    and r.relnamespace = nsp.oid
    and nsp.nspname = tg_table_schema
    and a.attrelid = r.oid
    and a.atttypid = t.oid
    and a.attnum > 0;
    -- zalogování změny v tabulce při insert a delete
    execute(format(qinsdel, tg_table_schema, tg_table_name, tg_op, key_column, key_column, oselect,
                   case when tg_op = 'INSERT' then 'new_table' else 'old_table' end));
  else
    -- Vytvoříme část SQL selectu pro vytvoření detailu logu s hodnotami při update logované tabulky
    select string_agg(format(col_upd_maska, a.attname,  '%s', t.typname),
                        ' || ') as oselect
      from pg_class r, pg_namespace nsp, pg_attribute a, pg_type t into oselect
     where r.relname = tg_table_name
      and r.relnamespace = nsp.oid
      and nsp.nspname = tg_table_schema
      and a.attrelid = r.oid
      and a.atttypid = t.oid
       and a.attnum > 0;
    -- zalogování změny v tabulce při update
    execute(format(qupd, tg_table_schema, tg_table_name, tg_op,
                   key_column, key_column, oselect, key_column, key_column));
  end if;
  return null;
end;
