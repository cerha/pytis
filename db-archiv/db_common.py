# -*- coding: utf-8 -*-

import pytis.data

# Konstanty pro datové typy
TBoolean  = pytis.data.Boolean(not_null=False)
TDate     = pytis.data.Date()
TTime     = pytis.data.Time()
TDateTime = pytis.data.DateTime()
TInteger  = pytis.data.Integer()
TLTree    = pytis.data.LTree()
TMoney    = 'numeric(15,2)'
TKurz     = 'numeric(12,6)'
TOid      = pytis.data.Oid()
TSerial   = pytis.data.Serial()
TString   = pytis.data.String()
TUser     = 'name'
TImage    = pytis.data.Image()

# Aliasy
C = Column
P = PrimaryColumn
V = ViewColumn
R = SelectRelation
S = Select
AT = ArgumentType
RT = ReturnType
SS = SelectSet
INNER = JoinType.INNER
LOUTER = JoinType.LEFT_OUTER
ROUTER = JoinType.RIGHT_OUTER
CROSS = JoinType.CROSS
UNION = SelectSetType.UNION
UNION_ALL = SelectSetType.UNION_ALL
EXCEPT = SelectSetType.EXCEPT
EXCEPT_ALL = SelectSetType.EXCEPT_ALL
INTERSECT = SelectSetType.INTERSECT

db_schemas = globals().get('Gpytis_schemas', None)


def Ctimestamp(name, doc=None):
    return C(name, TDateTime, constraints=('NOT NULL',), default='now()', doc=doc)
def Cuser(name, doc=None):
    return C(name, TUser, constraints=('NOT NULL',), default='user', doc=doc)

Gall_pytis = globals().get('Gall_pytis', (('all', 'pytis'),))
Grights_log_update = globals().get('Grights_log_update', (('all', 'pytis'),))

def _std_table(name, columns, doc, grant=Gall_pytis, **kwargs):
    return table(name, columns, inherits=('_changes',), grant=grant,
                 doc=doc, upd_log_trigger='_log_update_trigger',
                 **kwargs)

def _std_table_nolog(name, columns, doc, grant=Gall_pytis, **kwargs):
    return table(name, columns, inherits=('_changes',), grant=grant,
                 doc=doc, **kwargs)

def _std_view_raw(name, columns, fromitems, where=None, 
                 groupby=None, having=None, 
                 insert=None, update=None, delete=None, grant=Gall_pytis,
                  **kwargs):
    return view_sql_raw(name, columns, fromitems, where=where,
                        groupby=groupby, having=having,
                        insert=insert, update=update, delete=delete,
                        grant=grant, **kwargs)

# Pomocná funkce, jejíž obsah se připojí k funkcím, definovaným jako _std_function
def _plpy_include():
    """Funkce pro formátování pythonovský proměných různých typů, pro použití v SQL příkazech
    plpythonu (například plpy.execute)"""
    
    TMoney    = 'numeric(15,2)'
    TKurz     = 'numeric(12,6)'
    def pg_escape(val):
        return str(val).replace("'", "''")
    def boolean(val):
        if val is None:
            return "NULL"
        return val and "TRUE" or "FALSE"
    def string(val):
        return val is not None and "'%s'" % (pg_escape(val)) or "NULL"
    def num(val):
        return val is not None and "%s" % (val) or "NULL"
    def pg_val(val):
        if val is None:
            pg_value = "NULL"
        elif isinstance(val, (float, int)):
            pg_value = "%s" % (val)
        elif isinstance(val, bool):
            pg_value = val and "TRUE" or "FALSE"
        else:
            pg_value = "'%s'" % (pg_escape(val))
        return pg_value
    
    
def _plpy_function(name, input_types, output_type, body=None,
                   use_functions=(), **kwargs):
    """Pro definici plpython funkcí. Doplňuje základní funkce pro formátování pythonovských
    typů pro použití v SQL výrzech plpy.execute atp."""
    return function(name, input_types, output_type, body=body,
                    use_functions=(_plpy_include,) + use_functions, **kwargs)

def _plpy_trigger_include():
    """Funkce pro vytváření python trigger funkcí"""

    class BaseTriggerObject(object):

        _RETURN_CODE_MODYFY = "MODIFY"
        _RETURN_CODE_SKIP = "SKIP"
        _RETURN_CODE_OK = None
        
        def __init__(self, TD):
            self._TD = TD
            self._event = TD["event"].lower()
            self._when = TD["when"].lower()
            self._level = TD["level"].lower() 
            self._name = TD["name"].lower()
            self._table_name = TD["table_name"].lower()
            self._table_schema = TD["table_schema"].lower()
            self._table_oid = TD["relid"]
            self._args = TD["args"]
            # 
            self._new = self._old = None
            if self._event in ('insert', 'update'):
                self._new = TD["new"]
            if self._event in ('delete', 'update'):
                self._old = TD["old"]
            #
            self._return_code = self._RETURN_CODE_OK    

        def _do_after_insert(self):
            pass

        def _do_after_update(self):
            pass

        def _do_after_delete(self):
            pass

        def _do_before_insert(self):
            pass

        def _do_before_update(self):
            pass

        def _do_before_delete(self):
            pass        
         
        def do_trigger(self):
            if self._when == 'before':
                if self._event == 'insert':
                    self._do_before_insert()
                elif self._event == 'update':
                    self._do_before_update()                    
                elif self._event == 'delete':
                    self._do_before_delete()
            elif self._when == 'after':
                if self._event == 'insert':
                    self._do_after_insert()
                elif self._event == 'update':
                    self._do_after_update()                    
                elif self._event == 'delete':
                    self._do_after_delete()
            return self._return_code
        
def _trigger_function(name, body=None, use_functions=(), **kwargs):
    """Pro definici plpython funkcí, které jsou využívány pro vytváření triggrů."""
    return function(name, (), 'trigger', body=body,
                    use_functions=(_plpy_include, _plpy_trigger_include) + use_functions,
                    **kwargs)


def _plpy_control_include():
    """Definice html tabulky z databázových dat."""
    
    def _html_table(columns_labels,rows):
        def st(val):
            if val is None or str(val).strip() == '':
                return '&nbsp;'
            return str(val).replace(' ','&nbsp;')
        html_rows=[]
        if len(columns_labels) == 0:
            return None
        html_rows.append('<table>\\n<tr>')
        [html_rows.append('<td><b>'+st(x[1])+'</b></td>') for x in columns_labels]
        html_rows.append('</tr>')                         
        for row in rows:
            html_rows.append('<tr>')
            [html_rows.append('<td>'+st(row[x[0]])+'</td>') \
             for x in columns_labels]
            html_rows.append('</tr>')
        html_rows.append('</table>')
        html_table = '\\n'.join(html_rows)
        return html_table.replace("'", "''")

def _control_function(name, input_types, output_type, body=None,
                      use_functions=(), **kwargs):
    """Pro definici plpython kontrolních funkcí. Přidává na výstup html formátování"""
    return function(name, input_types, output_type, body=body,
                    use_functions=(_plpy_include, _plpy_control_include,) + use_functions, **kwargs)

def partitioning_trigger():
    """Updatuje datum a místo odeslání"""
    class Part(BaseTriggerObject):

        def _get_table_name(self):
            max_id = int(self._args[0])
            step = int(self._args[1])
            key_serial = self._args[2]
            if self._event in ('insert', 'update'):
                key = self._new[key_serial]
            else:
                key = self._old[key_serial]
            count_tables = (key - 1) / step
            if key > max_id:
                min_id_table = (max_id / step) * step + 1
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
            values = ", ".join([pg_val(x) for x in self._new.values()])
            keys = ", ".join([x for x in self._new.keys()])
            plpy.execute("insert into %s (%s) values (%s)" % (table, keys, values))
            self._return_code = self._RETURN_CODE_SKIP

        def _do_before_update(self):
            table, id_key, key = self._get_table_name()
            updates = ", ".join(["%s = %s" % (x, pg_val(self._new[x])) for x in \
                                 self._new.keys()])
            plpy.execute("update %s set %s where %s = %s" % (table, updates, id_key, key))
            self._return_code = self._RETURN_CODE_SKIP        
            
            
    # MAIN
    part = Part(TD)
    result = part.do_trigger()
    return result
    
_trigger_function('partitioning_trigger', body=partitioning_trigger, doc=())

def partitioning_table(master_table, key_serial, max_id, step, index_columns=(),
                       triggers_types=('insert', 'update', 'delete')):
    def gen_partitioning_trigger(typ, master_table, key_serial, max_id, step):
        return sql_raw(
            ("create trigger %s_before_%s_trigger before %s on %s "
             "for each row execute procedure partitioning_trigger(%s, %s, '%s');"
             ) % (master_table, typ, typ, master_table, max_id, step, key_serial),
            name="%s_before_%s_trigger" % (master_table, typ),
            depends=(master_table, 'partitioning_trigger'))
        
    def gen_table(master_table, key_serial, min_id, max_id):
        if min_id is None:
            check = "CHECK (%s <= %s)" % (key_serial, max_id)
        elif max_id is None:
            check = "CHECK (%s >= %s)" % (key_serial, min_id)
        else:
           check = "CHECK (%s between %s and %s)" % (key_serial, min_id, max_id) 
        return table('%s_%s_%s' % (master_table, min_id, max_id), (),
                     inherits=(master_table,), sql=check, depends=(master_table,))
    
    def gen_idx(master_table, column, min_id, max_id):
        return sql_raw(
            ("CREATE INDEX %s_%s_%s_%s_idx ON %s_%s_%s (%s);"
             ) % (master_table, min_id, max_id, column, master_table, min_id, max_id, column),
            name='%s_%s_%s_%s_index' % (master_table, min_id, max_id, column),
            depends=('%s_%s_%s' % (master_table, min_id, max_id),))

    for typ in triggers_types:
        gen_partitioning_trigger(typ, master_table, key_serial, max_id, step)
    count_tables = max_id / step + 1   
    for x in range(count_tables):
        if x == 0:
            min_id_table = None
            max_id_table = step
        else:
            min_id_table = x * step + 1
            max_id_table = x * step + step
        if max_id_table > max_id:
           max_id_table = None 
        gen_table(master_table, key_serial, min_id_table, max_id_table)
        for col in index_columns:
            gen_idx(master_table, col, min_id_table, max_id_table)


## Uživatelská funkce pro agregování  string hodnot
sql_raw("""create or replace function comma_aggregate(text,text) returns 
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
""",
        name="comma_aggregate",
        depends=())

sql_raw("""create aggregate comma_concat (basetype=text, sfunc=comma_aggregate,
stype=text, initcond='' );""",
        name="comma_concat",
        depends=('comma_aggregate',))


sql_raw("""create or replace function line_feed_aggregate(text,text) returns 
text as '
begin
  if length($1)>0 and length($2)>0 then
      return $1 || ''\n'' || $2;
  elsif length($2)>0 then
    return $2;
  end if;
  return $1;
end;
' language 'plpgsql';
""",
        name="line_feed_aggregate",
        depends=())

sql_raw("""create aggregate line_feed_concat (basetype=text, sfunc=line_feed_aggregate,
stype=text, initcond='' );""",
        name="line_feed_concat",
        depends=('line_feed_aggregate',))


sql_raw("""create or replace function space_aggregate(text,text) returns 
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
""",
        name="space_aggregate",
        depends=())

sql_raw("""create aggregate space_concat (basetype=text, sfunc=space_aggregate,
stype=text, initcond='' );""",
        name="space_concat",
        depends=('space_aggregate',))

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
               (SELECT '    columns = (' || array_to_string(array_agg('''' || a.attname || ''''), ', ') || ')'
               FROM pg_catalog.pg_attribute a, pg_catalog.pg_class c
               WHERE pg_catalog.pg_table_is_visible(c.oid)
               AND c.relname = '%s'
               AND c.oid = a.attrelid
               AND a.attnum > 0
               AND NOT a.attisdropped
               AND a.attname not in ('vytvoril','vytvoreno','zmenil','zmeneno'))
           """ % (table, table)
        q = plpy.execute(q)
        fields = "\\n".join([r["fields"] for r in q])
        spec = ('class %s(Specification):\\n    public = True\\n\\n    table = %s%s%s\\n    title = _("%s")\\n\\n'
                ) % (class_name, "'", table, "'", class_name)
        specs.append(spec + fields)
    return "\\n\\n\\n".join(specs)

function('gen_mirror_spec', (TString,), TString, body=gen_mirror_spec,
         doc=("Vygeneruje základní specifikace pro seznam tabulek"))


##################################
### Logovací a pomocné tabulky ###
##################################

table('log',
      (P('id',      TSerial),
       C('command', TString, constraints=('NOT NULL',)),
       Cuser('login'),
       Ctimestamp('timestamp')),
      grant=Gall_pytis,
      doc="Tabulka pro logování provedených DML příkazů.")

function('only_digits', (TString,), TBoolean,
         "select ($1 ~ ''^[0-9]+$'')",
         doc="Pomocná funkce pro CHECK constraint.")

function('f_date_year', (TDate,), TInteger, "select date_part(''year'', $1)::int",
         schemas=db_schemas,
         doc="Pomocná funkce pro agregační matici pytisu.")
function('f_date_halfyear', (TDate,), TInteger,
         "select case when date_part(''month'', $1) < 7 then 1 else 2 end::int",
         schemas=db_schemas,
         doc="Pomocná funkce pro agregační matici pytisu.")
function('f_date_quarter', (TDate,), TInteger, "select date_part(''quarter'', $1)::int",
         schemas=db_schemas,
         doc="Pomocná funkce pro agregační matici pytisu.")
function('f_date_month', (TDate,), TInteger, "select date_part(''month'', $1)::int",
         schemas=db_schemas,
         doc="Pomocná funkce pro agregační matici pytisu.")

table('_changes',
      (Cuser('vytvoril'),
       Ctimestamp('vytvoreno'),
       Cuser('zmenil'),
       Ctimestamp('zmeneno')),
      grant=Gall_pytis,
      doc="""Sloupečky zaznamenávající uživatele a časy vytvoření a změn údajů.
      Je určena k tomu, aby ji dědily všechny ostatní tabulky."""
      )

sequence('tempnames_seq', grant=Gall_pytis)

function('new_tempname',(), TString,
         "select ''__t'' || nextval(''tempnames_seq'')::text as jmeno",
         doc="Pomocná funkce pro generování unikátních jmen.")

table('_inserts',
      (P('id', TSerial,
         doc="identifikace řádku"),
       Cuser('vytvoril'),
       Ctimestamp('vytvoreno'),
       C('tabulka', TString),
       C('klic', TString),
       ),
      view=(TableView((V(None, 'datum', 'vytvoreno::date'),
                       V(None, 'cas', 'vytvoreno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      name='v_inserts',
                      grant=Gall_pytis,
                      update=None,
                      insert=None,
                      delete=None),
            TableView((V(None, 'datum', 'vytvoreno::date'),
                       V(None, 'cas', 'vytvoreno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      join="vytvoril = current_user",
                      name='v_inserts_user',
                      grant=Gall_pytis,
                      update=None,
                      insert=None,
                      delete=None)),
      grant=Grights_log_update,
      doc="""Tabulka zaznamenávající přidávání záznamů standardních
      tabulek."""
      )


table('_updates',
      (P('id', TSerial,
         doc="identifikace změnového řádku"),
       Cuser('zmenil'),
       Ctimestamp('zmeneno'),
       C('tabulka', TString),
       C('klic', TString),
       C('zmeny', TString)
       ),
      view=(TableView((V(None, 'datum', 'zmeneno::date'),
                       V(None, 'cas', 'zmeneno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      name='v_updates',
                      grant=Gall_pytis,
                      update=None,
                      insert=None,
                      delete=None),
            TableView((V(None, 'datum', 'zmeneno::date'),
                       V(None, 'cas', 'zmeneno::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      join="zmenil = current_user",
                      name='v_updates_user',
                      grant=Gall_pytis,
                      update=None,
                      insert=None,
                      delete=None)),
      grant=Grights_log_update,
      doc="""Tabulka zaznamenávající změny v záznamech standardních
      tabulek."""
      )

table('_deletes',
      (P('id', TSerial,
         doc="identifikace řádku"),
       Cuser('smazal'),
       Ctimestamp('smazano'),
       C('tabulka', TString),
       C('klic', TString)),
      view=(TableView((V(None, 'datum', 'smazano::date'),
                      V(None, 'cas', 'smazano::time'),
                      V('oid', 'oid')),
                     exclude=(),
                     name='v_deletes',
                     grant=Gall_pytis,
                     update=None,
                     insert=None,
                     delete=None),
            TableView((V(None, 'datum', 'smazano::date'),
                       V(None, 'cas', 'smazano::time'),
                       V('oid', 'oid')),
                      exclude=(),
                      join = "smazal = current_user",
                      name='v_deletes_user',
                      grant=Gall_pytis,
                      update=None,
                      insert=None,
                      delete=None)),            
      grant=Grights_log_update,
      doc="""Tabulka zaznamenávající vymazávání záznamů ve standardních
      tabulkách."""
      )

table('_changes_statistic',
      (P('id', TSerial,
         doc="identifikace řádku"),
       Cuser('uzivatel'),
       C('datum', TDate),
       C('inserts', TInteger),
       C('updates', TInteger),
       C('deletes', TInteger)),
      grant=Gall_pytis,
      doc="""Tabulka pro statistiky změn v tabulkách."""
      )

sql_raw("""
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
""",
        name = '_changes_statistic_total',
        depends = ('_changes_statistic',)
    )

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
    for u, d, t, p in (('vytvoril', 'vytvoreno' ,'_inserts', 'inserts'),
                       ('zmenil', 'zmeneno' ,'_updates', 'updates'),
                       ('smazal', 'smazano' ,'_deletes', 'deletes'),
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

f_update_statistic = function('update_statistic', (), TInteger,
                              body=update_statistic,
                              doc="""Aktualizuje tabulku statistiky""",
                              depends=('_inserts', '_deletes', '_updates',))


def _log_update_trigger():
    def pg_escape(val):
        return val.replace("'", "''").replace(chr(92),2*chr(92))
    event = TD["event"]
    if event == "DELETE":
        newold = "old"
        table = "_deletes"
    elif event == "INSERT":
        newold = "new"
        table = "_inserts"
    else:
        newold = "new"
        table = "_updates"
    tabid = TD["relid"]
    q = "select relname from pg_class where oid = %s" % tabid
    q = plpy.execute(q)
    tabulka = q[0]["relname"]
    klice = TD["args"][0].split(',')
    klicestr = ','.join(["%s: %s" % (k, str(TD[newold][k]))
                         for k in klice])
    # pro INSERT a DELETE zaznamenáme tabulku a klíč
    if event in ("DELETE", "INSERT"):
        q = """insert into %s (tabulka, klic)
               select '%s', '%s'
            """ % (table, tabulka, klicestr)
        q = plpy.execute(q)
        return None
    # Pro UPDATE zaznamenáme kromě jména tabulky a klíče i změny v položkách
    zmeny = []
    # Zjistime bytea sloupce
    q = """select a.attname
             from pg_class r, pg_namespace nsp, pg_attribute a, pg_type t
            where r.relname = '%s' and r.relnamespace = nsp.oid and nsp.nspname = '%s'
              and a.attrelid = r.oid
              and a.atttypid = t.oid
              and t.typname = 'bytea'
        """ % (TD["table_name"], TD["table_schema"])
    rows = plpy.execute(q)
    if rows and len(rows) > 0:
        bytea_cols = [r["attname"] for r in rows]
    else:
        bytea_cols = []
    for k in TD["new"].keys():
        if TD["new"][k] != TD["old"][k]:
            if k in bytea_cols:
                zmena = "%s: MODIFIED" % k
            else:
                zmena = """%s: %s -> %s""" % (k, pg_escape(str(TD["old"][k])),
                                              pg_escape(str(TD["new"][k])))
            zmeny.append(zmena)
    if zmeny != []:        
        zmenystr = """\n""".join(zmeny)        
        q = """insert into _updates (tabulka, klic, zmeny)
               select '%s', '%s', '%s'
            """ % (tabulka, klicestr, zmenystr)
        q = plpy.execute(q)
    return None

function('_log_update_trigger', (), 'trigger',
         body=_log_update_trigger,
         #security_definer=True,
         doc="""Slouží k evidenci editací nad záznamy tabulek.""",
         depends=('_inserts', '_deletes', '_updates'))

table('t_changes',
      (P('id', TSerial),
       C('timestamp', TDateTime, constraints=('not null',), index=True),
       C('username', TString, constraints=('not null',), index=True),
       C('schemaname', TString, constraints=('not null',)),
       C('tablename', TString, constraints=('not null',), index=True),
       C('operation', TString, constraints=('not null',),
         doc="One of: INSERT, UPDATE, DELETE"),
       C('key_column', TString, constraints=('not null',)),
       C('key_value', TString, constraints=('not null',), index=True),
       ),
      schemas=('public',),
      doc="Log of data changes.",
      grant=Gall_pytis,
      depends=())
table('t_changes_detail',
      (C('id', TInteger, constraints=('not null',), index=True,
         references='t_changes on update cascade on delete cascade'),
       C('detail', TString, constraints=('not null',)),
       ),
      schemas=('public',),
      doc="Detail information about database changes.",
      grant=Gall_pytis,
      depends=('t_changes',))

viewng('v_changes',
       (SelectRelation('t_changes', alias='changes'),
        SelectRelation('t_changes_detail', alias='detail', exclude_columns=('id',),
                       jointype=JoinType.LEFT_OUTER,
                       condition="changes.id=detail.id"),
        ),
       insert_order=(),
       update_order=(),
       delete_order=(),
       schemas=('public',),
       grant=Gall_pytis,
       depends=('t_changes', 't_changes_detail',))

function('log_trigger', (), 'trigger',
"""
declare
  id_ int;
  current_record record;
  c text;
  cc text;
  t text;
  v text;
  key_column_ text := tg_argv[0];
  key_value_ text := null;
  detail_ text := '''';
  changed bool;
begin
  if tg_op = ''DELETE'' then
    current_record := old;
  else
    current_record := new;
  end if;
  for c in select regexp_split_to_table(key_column_, '', *'') loop
    cc := quote_ident(c);
    execute concat(''select $1.'', cc, ''::text'') into strict v using current_record;
    if key_value_ is null then
      key_value_ := v;
    else
      key_value_ := concat(key_value_, '','', v);
    end if;
  end loop;
  for c, t in select a.attname, t.typname
              from pg_class r, pg_namespace nsp, pg_attribute a, pg_type t
              where r.relname = tg_table_name and r.relnamespace = nsp.oid and nsp.nspname = tg_table_schema and
                    a.attrelid = r.oid and a.atttypid = t.oid and a.attnum > 0
  loop
    cc := quote_ident(c);
    if tg_op = ''UPDATE'' then
      execute concat(''select coalesce($1.'', cc, ''::text, '''''''') != coalesce($2.'', cc, ''::text, '''''''')'') into strict changed using old, new;
      if changed then
        if detail_ != '''' then
          detail_ := concat(detail_, ''\n'');
        end if;
        execute concat(''select
                       (case when $1.'', cc, '' is not null and $3 = ''''bytea'''' then ''''BINARY'''' when $1.'', cc, '' is null then ''''NULL'''' else $1.'', cc, ''::text end) ||
                 '''' -> '''' ||
                       (case when $2.'', cc, '' is not null and $3 = ''''bytea'''' then ''''BINARY'''' when $2.'', cc, '' is null then ''''NULL'''' else $2.'', cc, ''::text end)'')
                into strict v using old, new, t;
        detail_ := concat(detail_, c, '': '', v);
      end if;
    else
      execute concat(''select $1.'', cc, '' is not null'') into strict changed using current_record;
      if changed then
        if detail_ != '''' then
          detail_ := concat(detail_, ''\n'');
        end if;
        execute concat(''select (case when $1.'', cc, '' is not null and $2 = ''''bytea'''' then ''''BINARY'''' else $1.'', cc, ''::text end)'')
                into strict v using current_record, t;
        detail_ := concat(detail_, c, '': '', v);
      end if;
    end if;
  end loop;
  if tg_op != ''UPDATE'' or detail_ != '''' then
    insert into t_changes (timestamp, username, schemaname, tablename, operation, key_column, key_value)
           values (now(), session_user, tg_table_schema, tg_table_name, tg_op, key_column_, key_value_)
           returning id into strict id_;
    insert into t_changes_detail (id, detail) values (id_, detail_);
  end if;
  return null;
end;
""",
         security_definer=True, language='plpgsql')

sql_raw("""
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
    execute concat('create table public.', tablename, ' as select * from public.t_changes where id<=', n_id);
    execute concat('create table public.', dtablename, ' as select * from public.t_changes_detail where id<=', n_id);
    delete from public.t_changes where id<=n_id;
    execute concat('create unique index ', tablename, '__id__index on public.', tablename, ' (id)');
    execute concat('create unique index ', tablename, '__timestamp__index on public.', tablename, ' (timestamp)');
    execute concat('create unique index ', tablename, '__username__index on public.', tablename, ' (username)');
    execute concat('create unique index ', tablename, '__tablename__index on public.', tablename, ' (tablename)');
    execute concat('create unique index ', tablename, '__key_value__index on public.', tablename, ' (key_value)');
    execute concat('create unique index ', dtablename, '__id__index on public.', dtablename, ' (id)');
  end loop;
end;
$$ language plpgsql;

create or replace function f_view_log(date_from date, date_to date, username_ text, tablename_ text, key_value_ text, detail_ text, search_path_ text) returns setof v_changes as $$
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
                            (search_path_ is null or schemaname in (select * from regexp_split_to_table(coalesce(search_path_, ''), ' *, *')));
  for tablename in select relname from pg_class join pg_namespace on relnamespace = pg_namespace.oid
                          where nspname = 'public' and
                                relname ~ '^t_changes_......_......x*$' and
                                substring(relname from 11 for 6) <= to_char(date_to, 'YYMMDD') and
                                substring(relname from 18 for 6) >= to_char(date_from, 'YYMMDD')
  loop
    return query
      execute concat('select t.*, detail ',
                             'from public.', tablename, ' t join ',
                             'public.t_changes_detail_', substring(tablename from 11), ' d using(id) ',
                             'where $1 <= t.timestamp and t.timestamp < $2 and ',
                                   't.username = coalesce($3, t.username) and ',
                                   't.tablename = coalesce($4, t.tablename) and ',
                                   't.key_value::text = coalesce($5, t.key_value) and ',
                                   'coalesce(d.detail, '''') like ''%''||coalesce($6, ''%'')||''%'' and ',
                                   '($7 is null or schemaname in (select * from regexp_split_to_table(coalesce($7, ''''), '' *, *'')))')
              using date_from, date_to_1, username_, tablename_, key_value_, detail_, search_path_;
  end loop;
end;
$$ language plpgsql stable;
""",
        schemas=('public',),
        name='log_trigger_support',
        depends=('t_changes', 't_changes_detail',))

# Slouží ke změně políčka zmeneno z děděné tabulky _changes
sql_raw("""
CREATE OR REPLACE FUNCTION _update_column_zmeneno()
	RETURNS TRIGGER AS $$
	BEGIN
	   NEW.zmeneno = current_timestamp; 
	   NEW.zmenil = current_user; 
	   RETURN NEW;
	END;
	$$ language 'plpgsql';""",
        name="_update_column_zmeneno"
        )

# Vypne všechny triggers na tabulkou předanou jako parametr funkce.
sql_raw("""
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
' LANGUAGE plpgsql WITH (isstrict);""",
        name="DisableTriggers")

# Zapne všechny triggers na tabulkou předanou jako parametr funkce pokud byly dočasně vypnuté.
sql_raw("""
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
' LANGUAGE plpgsql WITH (isstrict);""",
        name="EnableTriggers")


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
   
function('drop_temptables', (TString,), TInteger,  body=drop_temptables,
    doc=("Slouží k zrušení dočasných temporery tabulek. Funkce otestuje, zda tabulky uvedené "
         "v seznamu existují a případně je dropne."))

# Typ formuláře pro ruční spouštění v pytisu
_std_table('c_typ_formular',
           (P('id', pytis.data.String(maxlen=2)),
            C('popis', TString)),
           doc="Slouží jako číselník typů formulářů",
           init_values=(("'BF'", "'Jednoduchý náhled'"),
                        ("'DF'", "'Duální náhled'"))
           )


def easter_date(rok):
    """Pro udaný rok (parametr) vrátí datum velikonoční neděle."""

    # Podle Oudionova algoritmu
    rok = args[0]
    c = int(rok/100)
    n = rok - 19 * int(rok / 19)
    k = int((c - 17) / 25)
    i1 = c - int(c / 4) - int((c - k) / 3) + 19 * n + 15
    i2 = i1 - 30 * int(i1 / 30)
    i3 = i2 - int(i2 / 28) * (1 - int(i2 / 28) * int(29 / (i2 + 1)) * \
                              int((21 - n) / 11))
    a1 = rok + int(rok / 4) + i3 + 2 - c + int(c/4)
    a2 = a1 - 7 * int(a1 / 7)
    l = i3 - a2
    m = 3 + int((l + 40) / 44)
    d = l + 28 - 31 * int(m / 4)
    datum = """%s-%s-%s""" % (rok, m, d)
    return datum
    
f_easter_date = function('easter_date', (TInteger,), TDate,
                       body=easter_date,
                       doc="""Pro udaný rok (parametr) vrátí datum velikonoční
                       neděle.""")

