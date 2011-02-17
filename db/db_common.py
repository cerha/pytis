# -*- coding: utf-8 -*-

import pytis.data

TBoolean  = pytis.data.Boolean()
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
CROSS = JoinType.CROSS
UNION = SelectSetType.UNION
UNION_ALL = SelectSetType.UNION_ALL
EXCEPT_ALL = SelectSetType.EXCEPT_ALL
INTERSECT = SelectSetType.INTERSECT


def Ctimestamp(name, doc=None):
    return C(name, TDateTime, constraints=('NOT NULL',), default='now()', doc=doc)
def Cuser(name, doc=None):
    return C(name, TUser, constraints=('NOT NULL',), default='user', doc=doc)

Gall_pytis = (('all', 'pytis'),)

def _std_table(name, columns, doc, grant=Gall_pytis, **kwargs):
    return table(name, columns, inherits=('_changes',), grant=grant,
                 doc=doc, upd_log_trigger='_log_update_trigger',
                 **kwargs)

def _std_table_nolog(name, columns, doc, grant=Gall_pytis, **kwargs):
    return table(name, columns, inherits=('_changes',), grant=grant,
                 doc=doc, **kwargs)

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

    class BaseTriggerObject:

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
    """Funkce pro formátování pythonovský proměných různých typů, pro použití v SQL příkazech
    plpythonu (například plpy.execute). Přidává možnost html výstupu."""
    
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
                    use_functions=(_plpy_control_include,) + use_functions, **kwargs)

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
         "SELECT ($1 ~ ''^[0-9]+$'')",
         doc="Pomocná funkce pro CHECK constraint.")

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
      grant=Gall_pytis,
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
      grant=Gall_pytis,
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
      grant=Gall_pytis,
      doc="""Tabulka zaznamenávající vymazávání záznamů ve standardních
      tabulkách."""
      )

def _log_update_trigger():
    def pg_escape(val):
        return val.replace("'", "''")
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
    for k in TD["new"].keys():
        if TD["new"][k] != TD["old"][k]:
            zmeny.append("""%s: %s -> %s""" % (k, pg_escape(str(TD["old"][k])),
                                               pg_escape(str(TD["new"][k]))))
    if zmeny != []:        
        zmenystr = """\n""".join(zmeny)        
        q = """insert into _updates (tabulka, klic, zmeny)
               select '%s', '%s', '%s'
            """ % (tabulka, klicestr, zmenystr)
        q = plpy.execute(q)
    return None

function('_log_update_trigger', (), 'trigger',
         body=_log_update_trigger,
         doc="""Slouží k evidenci editací nad záznamy tabulek.""",
         depends=('_inserts', '_deletes', '_updates'))

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

_std_table('c_typ_formular',
           (P('id', pytis.data.String(minlen=2, maxlen=2)),
            C('popis', TString)),
           doc="Slouží jako číselník typů formulářů",
           init_values=(("'BF'", "'Jednoduchý náhled'"),
                        ("'DF'", "'Duální náhled'"))
           )

