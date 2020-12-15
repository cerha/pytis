# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import Base_LogSQLTable, Base_PyFunction, Base_PyTriggerFunction, \
    XDeletes, XInserts, XUpdates, default_access_rights, pytis_schemas


class PartitioningTrigger(Base_PyTriggerFunction):
    name = 'partitioning_trigger'
    arguments = ()
    result_type = sql.G_CONVERT_THIS_FUNCTION_TO_TRIGGER
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def partitioning_trigger():
        """Updatuje datum a místo odeslání"""
        class Part(Base_PyTriggerFunction.Util.BaseTriggerObject):

            def _get_table_name(self):
                max_id = int(self._args[0])
                step = int(self._args[1])
                key_serial = self._args[2]
                if self._event in ('insert', 'update'):
                    key = self._new[key_serial]
                else:
                    key = self._old[key_serial]
                count_tables = (key - 1) // step
                if key > max_id:
                    min_id_table = (max_id // step) * step + 1
                    max_id_table = None
                if count_tables == 0:
                    min_id_table = None
                    max_id_table = step
                else:
                    min_id_table = count_tables * step + 1
                    max_id_table = count_tables * step + step
                return "%s_%s_%s" % (self._table_name, min_id_table, max_id_table), key_serial, key

            def _do_before_delete(self):
                table, id_key, key = self._get_table_name()
                plpy.execute("delete from %s where %s = %s" % (table, id_key, key))
                self._return_code = self._RETURN_CODE_SKIP

            def _do_before_insert(self):
                table, id_key, key = self._get_table_name()
                values = ", ".join([Base_PyFunction.Util.pg_val(x) for x in self._new.values()])
                keys = ", ".join([x for x in self._new.keys()])
                plpy.execute("insert into %s (%s) values (%s)" % (table, keys, values))
                self._return_code = self._RETURN_CODE_SKIP

            def _do_before_update(self):
                table, id_key, key = self._get_table_name()
                updates = ", ".join(["%s = %s" % (x, Base_PyFunction.Util.pg_val(self._new[x]))
                                     for x in self._new.keys()])
                plpy.execute("update %s set %s where %s = %s" % (table, updates, id_key, key))
                self._return_code = self._RETURN_CODE_SKIP
        # MAIN
        part = Part(TD)
        result = part.do_trigger()
        return result


class CommaAggregate(sql.SQLRaw):
    name = 'comma_aggregate'

    @classmethod
    def sql(class_):
        return """create or replace function comma_aggregate(text,text) returns
text as '
begin
  if length($1)>0 and length($2)>0 then
      return $1 || '', '' || $2;
  elsif length($2)>0 then
    return $2;
  end if;
  return $1;
end;
' language 'plpgsql';
"""
    depends_on = ()


class CommaConcat(sql.SQLRaw):
    name = 'comma_concat'

    @classmethod
    def sql(class_):
        return """create aggregate comma_concat (basetype=text, sfunc=comma_aggregate,
stype=text, initcond='' );"""
    depends_on = (CommaAggregate,)


class LineFeedAggregate(sql.SQLRaw):
    name = 'line_feed_aggregate'

    @classmethod
    def sql(class_):
        return """create or replace function line_feed_aggregate(text,text) returns
text as '
begin
  if length($1)>0 and length($2)>0 then
      return $1 || ''
'' || $2;
  elsif length($2)>0 then
    return $2;
  end if;
  return $1;
end;
' language 'plpgsql';
"""
    depends_on = ()


class LineFeedConcat(sql.SQLRaw):
    name = 'line_feed_concat'

    @classmethod
    def sql(class_):
        return """create aggregate line_feed_concat (basetype=text, sfunc=line_feed_aggregate,
stype=text, initcond='' );"""
    depends_on = (LineFeedAggregate,)


class SpaceAggregate(sql.SQLRaw):
    name = 'space_aggregate'

    @classmethod
    def sql(class_):
        return """create or replace function space_aggregate(text,text) returns
text as '
begin
  if length($1)>0 and length($2)>0 then
      return $1 || '' '' || $2;
  elsif length($2)>0 then
    return $2;
  end if;
  return $1;
end;
' language 'plpgsql';
"""
    depends_on = ()


class SpaceConcat(sql.SQLRaw):
    name = 'space_concat'

    @classmethod
    def sql(class_):
        return """create aggregate space_concat (basetype=text, sfunc=space_aggregate,
stype=text, initcond='' );"""
    depends_on = (SpaceAggregate,)


class GenMirrorSpec(Base_PyFunction):
    """Vygeneruje základní specifikace pro seznam tabulek"""
    name = 'gen_mirror_spec'
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def gen_mirror_spec(tables):
        """Vygeneruje základní specifikace pro seznam tabulek"""
        tables = [t.strip() for t in args[0].split(',')]
        specs = []
        for table in tables:
            class_name = "%s%s" % (table[0:1].upper(), table[1:])
            q = """select '    fields = (' as fields
                   union all
                   (SELECT '        Field(''' || a.attname || ''', _("' || a.attname || '"), ),'
                   FROM pg_catalog.pg_attribute a, pg_catalog.pg_class c
                   WHERE pg_catalog.pg_table_is_visible(c.oid)
                   AND c.relname = '%s'
                   AND c.oid = a.attrelid
                   AND a.attnum > 0
                   AND NOT a.attisdropped
                   AND a.attname not in ('vytvoril','vytvoreno','zmenil','zmeneno'))
                   union all
                   (select '       )')
                   union all
                   (SELECT '    columns = (' ||
                      array_to_string(array_agg('''' || a.attname || ''''), ', ') || ')'
                   FROM pg_catalog.pg_attribute a, pg_catalog.pg_class c
                   WHERE pg_catalog.pg_table_is_visible(c.oid)
                   AND c.relname = '%s'
                   AND c.oid = a.attrelid
                   AND a.attnum > 0
                   AND NOT a.attisdropped
                   AND a.attname not in ('vytvoril','vytvoreno','zmenil','zmeneno'))
               """ % (table, table)
            q = plpy.execute(q)
            fields = "\n".join([r["fields"] for r in q])
            spec = ('class %s(Specification):\n    public = True\n\n    table = %s%s%s\n'
                    '    title = _("%s")\n\n'
                    ) % (class_name, "'", table, "'", class_name)
            specs.append(spec + fields)
        return "\n\n\n".join(specs)


class Log(sql.SQLTable):
    """Tabulka pro logování provedených DML příkazů."""
    name = 'log'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('command', pytis.data.String(not_null=True)),
              sql.Column('login', pytis.data.Name(not_null=True), default=sqlalchemy.text('user')),
              sql.Column('timestamp', pytis.data.DateTime(not_null=True),
                         default=sqlalchemy.text('now()')),
              )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class OnlyDigits(sql.SQLFunction):
    """Pomocná funkce pro CHECK constraint."""
    name = 'only_digits'
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select ($1 ~ '^[0-9]+$')"


class FDateYear(sql.SQLFunction):
    """Pomocná funkce pro agregační matici pytisu."""
    schemas = pytis_schemas.value(globals())
    name = 'f_date_year'
    arguments = (sql.Column('', pytis.data.Date()),)
    result_type = pytis.data.Integer()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select date_part('year', $1)::int"


class FDateHalfyear(sql.SQLFunction):
    """Pomocná funkce pro agregační matici pytisu."""
    schemas = pytis_schemas.value(globals())
    name = 'f_date_halfyear'
    arguments = (sql.Column('', pytis.data.Date()),)
    result_type = pytis.data.Integer()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select case when date_part('month', $1) < 7 then 1 else 2 end::int"


class FDateQuarter(sql.SQLFunction):
    """Pomocná funkce pro agregační matici pytisu."""
    schemas = pytis_schemas.value(globals())
    name = 'f_date_quarter'
    arguments = (sql.Column('', pytis.data.Date()),)
    result_type = pytis.data.Integer()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select date_part('quarter', $1)::int"


class FDateMonth(sql.SQLFunction):
    """Pomocná funkce pro agregační matici pytisu."""
    schemas = pytis_schemas.value(globals())
    name = 'f_date_month'
    arguments = (sql.Column('', pytis.data.Date()),)
    result_type = pytis.data.Integer()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select date_part('month', $1)::int"


class XChanges(sql.SQLTable):
    """Sloupečky zaznamenávající uživatele a časy vytvoření a změn údajů.
    Je určena k tomu, aby ji dědily všechny ostatní tabulky."""
    name = '_changes'
    fields = (sql.Column('vytvoril', pytis.data.Name(not_null=True),
                         default=sqlalchemy.text('user')),
              sql.Column('vytvoreno', pytis.data.DateTime(not_null=True),
                         default=sqlalchemy.text('now()')),
              sql.Column('zmenil', pytis.data.Name(not_null=True),
                         default=sqlalchemy.text('user')),
              sql.Column('zmeneno', pytis.data.DateTime(not_null=True),
                         default=sqlalchemy.text('now()')),
              )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class TempnamesSeq(sql.SQLSequence):
    name = 'tempnames_seq'
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class NewTempname(sql.SQLFunction):
    """Pomocná funkce pro generování unikátních jmen."""
    name = 'new_tempname'
    arguments = ()
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select '__t' || nextval('tempnames_seq')::text as jmeno"


class VInserts(sql.SQLView):
    """Tabulka zaznamenávající přidávání záznamů standardních
    tabulek."""
    name = 'v_inserts'

    @classmethod
    def query(cls):
        _inserts = sql.t.XInserts
        return sqlalchemy.select(
            [sql.gL("vytvoreno::date").label('datum'),
             sql.gL("vytvoreno::time").label('cas'),
             _inserts.c.id.label('id'),
             _inserts.c.vytvoril.label('vytvoril'),
             _inserts.c.vytvoreno.label('vytvoreno'),
             _inserts.c.tabulka.label('tabulka'),
             _inserts.c.klic.label('klic')],
            from_obj=[_inserts])
    depends_on = (XInserts,)
    access_rights = default_access_rights.value(globals())


class VInsertsUser(sql.SQLView):
    """Tabulka zaznamenávající přidávání záznamů standardních
    tabulek."""
    name = 'v_inserts_user'

    @classmethod
    def query(cls):
        _inserts = sql.t.XInserts
        return sqlalchemy.select(
            [sql.gL("vytvoreno::date").label('datum'),
             sql.gL("vytvoreno::time").label('cas'),
             _inserts.c.id.label('id'),
             _inserts.c.vytvoril.label('vytvoril'),
             _inserts.c.vytvoreno.label('vytvoreno'),
             _inserts.c.tabulka.label('tabulka'),
             _inserts.c.klic.label('klic')],
            from_obj=[_inserts],
            whereclause=_inserts.c.vytvoril == sqlalchemy.text('current_user'),
        )
    depends_on = (XInserts,)
    access_rights = default_access_rights.value(globals())


class VUpdates(sql.SQLView):
    """Tabulka zaznamenávající změny v záznamech standardních
    tabulek."""
    name = 'v_updates'

    @classmethod
    def query(cls):
        _updates = sql.t.XUpdates
        return sqlalchemy.select(
            [sql.gL("zmeneno::date").label('datum'),
             sql.gL("zmeneno::time").label('cas'),
             _updates.c.id.label('id'),
             _updates.c.zmenil.label('zmenil'),
             _updates.c.zmeneno.label('zmeneno'),
             _updates.c.tabulka.label('tabulka'),
             _updates.c.klic.label('klic'),
             _updates.c.zmeny.label('zmeny')],
            from_obj=[_updates],
        )
    depends_on = (XUpdates,)
    access_rights = default_access_rights.value(globals())


class VUpdatesUser(sql.SQLView):
    """Tabulka zaznamenávající změny v záznamech standardních
    tabulek."""
    name = 'v_updates_user'

    @classmethod
    def query(cls):
        _updates = sql.t.XUpdates
        return sqlalchemy.select(
            [sql.gL("zmeneno::date").label('datum'),
             sql.gL("zmeneno::time").label('cas'),
             _updates.c.id.label('id'),
             _updates.c.zmenil.label('zmenil'),
             _updates.c.zmeneno.label('zmeneno'),
             _updates.c.tabulka.label('tabulka'),
             _updates.c.klic.label('klic'),
             _updates.c.zmeny.label('zmeny')],
            from_obj=[_updates],
            whereclause=_updates.c.zmenil == sqlalchemy.text('current_user'),
        )
    depends_on = (XUpdates,)
    access_rights = default_access_rights.value(globals())


class VDeletes(sql.SQLView):
    """Tabulka zaznamenávající vymazávání záznamů ve standardních
    tabulkách."""
    name = 'v_deletes'

    @classmethod
    def query(cls):
        _deletes = sql.t.XDeletes
        return sqlalchemy.select(
            [sql.gL("smazano::date").label('datum'),
             sql.gL("smazano::time").label('cas'),
             _deletes.c.id.label('id'),
             _deletes.c.smazal.label('smazal'),
             _deletes.c.smazano.label('smazano'),
             _deletes.c.tabulka.label('tabulka'),
             _deletes.c.klic.label('klic')],
            from_obj=[_deletes],
        )
    depends_on = (XDeletes,)
    access_rights = default_access_rights.value(globals())


class VDeletesUser(sql.SQLView):
    """Tabulka zaznamenávající vymazávání záznamů ve standardních
    tabulkách."""
    name = 'v_deletes_user'

    @classmethod
    def query(cls):
        _deletes = sql.t.XDeletes
        return sqlalchemy.select(
            [sql.gL("smazano::date").label('datum'),
             sql.gL("smazano::time").label('cas'),
             _deletes.c.id.label('id'),
             _deletes.c.smazal.label('smazal'),
             _deletes.c.smazano.label('smazano'),
             _deletes.c.tabulka.label('tabulka'),
             _deletes.c.klic.label('klic')],
            from_obj=[_deletes],
            whereclause=_deletes.c.smazal == sqlalchemy.text('current_user'),
        )
    depends_on = (XDeletes,)
    access_rights = default_access_rights.value(globals())


class XChangesStatistic(sql.SQLTable):
    """Tabulka pro statistiky změn v tabulkách."""
    name = '_changes_statistic'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), doc="identifikace řádku"),
              sql.Column('uzivatel', pytis.data.Name(not_null=True),
                         default=sqlalchemy.text('user')),
              sql.Column('datum', pytis.data.Date(not_null=False)),
              sql.Column('inserts', pytis.data.Integer(not_null=False)),
              sql.Column('updates', pytis.data.Integer(not_null=False)),
              sql.Column('deletes', pytis.data.Integer(not_null=False)),
              )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class XChangesStatisticTotal(sql.SQLRaw):
    name = '_changes_statistic_total'

    @classmethod
    def sql(class_):
        return """
create or replace view _changes_statistic_total as
select uzivatel,
       sum(inserts) as inserts,
       sum(updates) as updates,
       sum(deletes) as deletes
from _changes_statistic
group by uzivatel;

CREATE OR REPLACE RULE _changes_statistic_total_ins AS
 ON INSERT TO _changes_statistic_total DO INSTEAD
 NOTHING;

CREATE OR REPLACE RULE _changes_statistic_total_upd AS
 ON UPDATE TO _changes_statistic_total DO INSTEAD
 NOTHING;

CREATE OR REPLACE RULE _changes_statistic_total_del AS
 ON DELETE TO _changes_statistic_total DO INSTEAD
 NOTHING;
"""
    depends_on = (XChangesStatistic,)


class UpdateStatistic(Base_PyFunction):
    """Aktualizuje tabulku statistiky"""
    name = 'update_statistic'
    arguments = ()
    result_type = pytis.data.Integer()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (XInserts, XDeletes, XUpdates,)
    access_rights = ()

    @staticmethod
    def update_statistic():
        # Minimální a maximální datum
        q = """select min(datum)::date as minimum
                from
                  (select min(vytvoreno) as datum
                     from _inserts
                   UNION
                   select min(zmeneno) as datum
                     from _updates
                   UNION
                   select min(smazano) as datum
                     from _deletes
                    ) m
            """
        q = plpy.execute(q)
        minimum = q[0]["minimum"]
        q = """select max(datum)::date as maximum
                from
                  (select max(vytvoreno) as datum
                     from _inserts
                   UNION
                   select max(zmeneno) as datum
                     from _updates
                   UNION
                   select max(smazano) as datum
                     from _deletes
                    ) m
            """
        q = plpy.execute(q)
        maximum = q[0]["maximum"]
        # Vygenerujeme nové řádky do _changes_statistic
        # Nejrychlejší bude všechno to smazat a nasypat to tam znovu
        q = plpy.execute("delete from _changes_statistic")
        q = """insert into _changes_statistic
               (uzivatel, datum, inserts, updates, deletes)
               select jmeno,
                      '%s'::date + c.cislo as datum,
                      0, 0, 0
               from generate_series(0, '%s'::date - '%s'::date) c(cislo),
                    (select distinct vytvoril as jmeno from _inserts
                     union
                     select distinct zmenil as jmeno from _updates
                     union
                     select distinct smazal as jmeno from _deletes
                     ) j
            """ % (minimum, maximum, minimum)
        q = plpy.execute(q)
        q = """select new_tempname() as temp"""
        q = plpy.execute(q)
        temp = q[0]["temp"]
        # Aktualizujeme inserts, updates a deletes
        for u, d, t, p in (('vytvoril', 'vytvoreno', '_inserts', 'inserts'),
                           ('zmenil', 'zmeneno', '_updates', 'updates'),
                           ('smazal', 'smazano', '_deletes', 'deletes'),
                           ):
            q = """create temp table %s as
                   select uzivatel, datum, count(*) as pocet
                     from
                       (select %s as uzivatel, %s::date as datum
                          from %s) c
                   group by uzivatel, datum
                """ % (temp, u, d, t)
            q = plpy.execute(q)
            q = """update _changes_statistic
                      set %s = pocet
                     from %s
                    where _changes_statistic.uzivatel = %s.uzivatel
                      and _changes_statistic.datum = %s.datum
            """ % (p, temp, temp, temp)
            q = plpy.execute(q)
            plpy.execute("drop table %s" % (temp))
        # Vrátíme počet řádků tabulky
        q = """select count(*) as pocet from _changes_statistic as pocet"""
        q = plpy.execute(q)
        pocet = q[0]["pocet"]
        return pocet


class TChanges(sql.SQLTable):
    """Log of data changes."""
    name = 't_changes'
    schemas = (('public',),)
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('timestamp', pytis.data.DateTime(not_null=True), index=True),
              sql.Column('username', pytis.data.String(not_null=True), index=True),
              sql.Column('schemaname', pytis.data.String(not_null=True)),
              sql.Column('tablename', pytis.data.String(not_null=True), index=True),
              sql.Column('operation', pytis.data.String(not_null=True),
                         doc="One of: INSERT, UPDATE, DELETE"),
              sql.Column('key_column', pytis.data.String(not_null=True)),
              sql.Column('key_value', pytis.data.String(not_null=True), index=True),
              )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class TChangesDetail(sql.SQLTable):
    """Detail information about database changes."""
    name = 't_changes_detail'
    schemas = (('public',),)
    fields = (sql.Column('id', pytis.data.Integer(not_null=True), index=True,
                         references=sql.a(sql.r.TChanges.id, onupdate='CASCADE',
                                          ondelete='CASCADE')),
              sql.Column('detail', pytis.data.String(not_null=True)),
              )
    depends_on = (TChanges,)
    access_rights = default_access_rights.value(globals())


class VChanges(sql.SQLView):
    name = 'v_changes'
    schemas = (('public',),)

    @classmethod
    def query(cls):
        changes = sql.t.TChanges.alias('changes')
        detail = sql.t.TChangesDetail.alias('detail')
        return sqlalchemy.select(
            cls._exclude(changes) +
            cls._exclude(detail, 'id'),
            from_obj=[changes.outerjoin(detail, changes.c.id == detail.c.id)]
        )

    insert_order = ()
    update_order = ()
    delete_order = ()
    depends_on = (TChanges, TChangesDetail,)
    access_rights = default_access_rights.value(globals())


class LogTriggerSupport(sql.SQLRaw):
    name = 'log_trigger_support'

    @classmethod
    def sql(class_):
        return """
create or replace function f_rotate_log() returns void as $$
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
$$ language plpgsql;

create or replace function f_view_log(date_from date, date_to date, username_ text, tablename_ text,
                                       key_value_ text, detail_ text, search_path_ text)
        returns setof v_changes as $$
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
                             '($7 is null or
                               schemaname in (select * from regexp_
                                                split_to_table(coalesce($7,
                                                                         ''''), '' *, *'')))')
              using date_from, date_to_1, username_, tablename_, key_value_, detail_, search_path_;
  end loop;
end;
$$ language plpgsql stable;
"""
    depends_on = (TChanges, TChangesDetail, VChanges,)


class XUpdateColumnZmeneno(sql.SQLRaw):
    name = '_update_column_zmeneno'

    @classmethod
    def sql(class_):
        return """
CREATE OR REPLACE FUNCTION _update_column_zmeneno()
        RETURNS TRIGGER AS $$
        BEGIN
           NEW.zmeneno = current_timestamp;
           NEW.zmenil = current_user;
           RETURN NEW;
        END;
        $$ language 'plpgsql';"""
    depends_on = ()


class Disabletriggers(sql.SQLRaw):
    name = 'DisableTriggers'

    @classmethod
    def sql(class_):
        return """
CREATE FUNCTION DisableTriggers(Name) RETURNS BOOLEAN AS '
DECLARE rel ALIAS FOR $1; rows INTEGER;
BEGIN
  UPDATE pg_class SET reltriggers = 0
    WHERE relname ~~* rel;
  GET DIAGNOSTICS rows = ROW_COUNT;
  IF rows > 0 THEN
    RETURN TRUE;
  ELSE
    RAISE NOTICE ''Relation does not exist'';
    RETURN False;
  END IF;
END;
' LANGUAGE plpgsql;"""
    depends_on = ()


class Enabletriggers(sql.SQLRaw):
    name = 'EnableTriggers'

    @classmethod
    def sql(class_):
        return """
CREATE FUNCTION EnableTriggers(Name) RETURNS BOOLEAN AS '
DECLARE rel ALIAS FOR $1; rows INTEGER;
BEGIN
  UPDATE pg_class SET reltriggers =
    (SELECT Count(*) FROM pg_trigger
      WHERE pg_class.oid = tgrelid)
    WHERE relname ~~* rel;
  GET DIAGNOSTICS rows = ROW_COUNT;
  IF rows > 0 THEN
    RETURN TRUE;
  ELSE
    RAISE NOTICE ''Relation does not exist'';
    RETURN False;
  END IF;
END;
' LANGUAGE plpgsql;"""
    depends_on = ()


class DropTemptables(Base_PyFunction):
    """Slouží k zrušení dočasných temporery tabulek.
    Funkce otestuje, zda tabulky uvedené v seznamu existují a případně je dropne.
    """
    name = 'drop_temptables'
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = pytis.data.Integer()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def drop_temptables(tables):
        """Slouží k zrušení dočasných temporery tabulek. Funkce otestuje, zda tabulky uvedené
        v seznamu existují a případně je dropne."""
        str_tables = args[0]
        if str_tables is None:
            return 0
        tables = set([x.strip() for x in str_tables.split(",")])
        q = """select c.relname as name
               from pg_catalog.pg_class c
               left join pg_catalog.pg_user u ON u.usesysid = c.relowner
               left join pg_catalog.pg_namespace n on (n.oid = c.relnamespace)
               where n.nspname not in ('pg_catalog', 'pg_toast')
               and pg_catalog.pg_table_is_visible(c.oid)
               and c.relkind = 'r' and n.nspname like 'pg_temp%'"""
        q = plpy.execute(q)
        if len(q) == 0:
            return 0
        tables.intersection_update(set([r["name"] for r in q]))
        pocet = len(tables)
        if pocet != 0:
            plpy.execute("drop table %s" % ",".join(tables))
        return pocet


class CTypFormular(Base_LogSQLTable):
    """Slouží jako číselník typů formulářů"""
    name = 'c_typ_formular'
    fields = (sql.PrimaryColumn('id', pytis.data.String(maxlen=2, not_null=False)),
              sql.Column('popis', pytis.data.String(not_null=False)),
              )
    inherits = (XChanges,)
    init_columns = ('id', 'popis')
    init_values = (('BF', 'Jednoduchý náhled',),
                   ('DF', 'Duální náhled',),
                   )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EasterDate(Base_PyFunction):
    """Pro udaný rok (parametr) vrátí datum velikonoční
    neděle."""
    name = 'easter_date'
    arguments = (sql.Column('', pytis.data.Integer()),)
    result_type = pytis.data.Date()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def easter_date(rok):
        """Pro udaný rok (parametr) vrátí datum velikonoční neděle."""
        # Podle Oudionova algoritmu
        rok = args[0]
        c = int(rok // 100)
        n = rok - 19 * int(rok // 19)
        k = int((c - 17) // 25)
        i1 = c - int(c // 4) - int((c - k) // 3) + 19 * n + 15
        i2 = i1 - 30 * int(i1 // 30)
        i3 = i2 - int(i2 // 28) * (1 - int(i2 // 28) * int(29 // (i2 + 1)) * int((21 - n) // 11))
        a1 = rok + int(rok // 4) + i3 + 2 - c + int(c // 4)
        a2 = a1 - 7 * int(a1 // 7)
        x = i3 - a2
        m = 3 + int((x + 40) // 44)
        d = x + 28 - 31 * int(m // 4)
        datum = """%s-%s-%s""" % (rok, m, d)
        return datum
