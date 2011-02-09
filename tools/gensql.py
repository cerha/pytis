#! /usr/bin/env python
# -*- coding: iso-8859-2 -*-
#
# N�stroj pro zpracov�n� specifikac� datab�z�
# 
# Copyright (C) 2002, 2003, 2005, 2009, 2010, 2011 Brailcom, o.p.s.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Tento program slou�� ke zpracov�n� specifikace struktury datab�ze.

Poskytuje n�sleduj�c� funkce:

- Generov�n� SQL p��kaz� pro inicializaci obsahu datab�ze.

- Kontrola specifikace proti obsahu datab�ze a generov�n� SQL p��kaz� pro
  aktualizaci obsahu datab�ze.

- R�zn� konzisten�n� kontroly.

(Ne v�echny tyto funkce jsou v�sou�asn� dob� implementov�ny.)

Pro informaci o�pou�it� programu jej spus�te s�argumentem '--help'.

Specifika�n� soubor datab�ze m� podobu pythonov�ho modulu.  P�i jeho zpracov�n�
je takov� modul na�ten s�namespace tohoto programu, nen� tedy nutno nic
zvl�tn�ho importovat (a�je pot�eba d�vat pozor na konflikty jmen).

Ve specifikaci je mo�no pou��vat v�echny ve�ejn� symboly definovan� v�tomto
programu.  Pomoc� specifika�n�ch t��d lze definovat zejm�na tabulky, views,
sekvence a funkce.  Pro definici t�chto objekt� se nepou��vaj� p��mo
konstruktory odpov�daj�c�ch t��d, n�br� obaluj�c� funkce, viz nap��klad
'table()', 'view()', 'sequence()', 'function()'.  Pokud pro n�jak� objekt
nebo ��el nen� definov�n p��slu�n� specifika�n� objekt, lze vyu��t funkce
'sql_raw()' a 'sql_raw_input()', umo��uj�c� zadat p��mo SQL p��kazy.  Na
objekty zadan� p��mo prost�ednictv�m SQL p��kaz� nejsou aplikov�ny ��dn�
kontroly.

"""

import copy
import functools
import getopt
import inspect
import operator
import re
import string
import sys
import UserDict
import types

from pytis.util import *
import pytis.data


exit_code = 0
_EXIT_USAGE = 1
_EXIT_NOT_IMPLEMENTED = 2
_EXIT_ERROR = 3

def _signal_error(message):
    sys.stderr.write(message)
    global exit_code
    exit_code = _EXIT_ERROR

class GensqlError(Exception):
    """V�jimka signalizovan� t�mto programem p�i chyb�ch specifikace."""
    

def _gsql_escape(s):
    return s.replace("'", "\\'")


def _gsql_column_table_column(column):
    if is_sequence(column):
        column = find(1, column,
                      test=(lambda x, y: x is not None and y is not None))
    pos = column.rfind('.')
    if pos == -1:
        result = None, column
    else:
        result = column[:pos], column[pos+1:]
    return result


def _gsql_format_type(type_):
    if type(type_) == type(''):
        result = type_
    elif type_.__class__ == pytis.data.String:
        minlen = type_.minlen()
        maxlen = type_.maxlen()
        if maxlen is None:
            result = 'text'
        elif maxlen == minlen:
            result = 'char(%d)' % maxlen
        else:
            result = 'varchar(%d)' % maxlen
    else:
        MAPPING = {pytis.data.Integer:   'int',
                   pytis.data.Serial:    'serial',
                   pytis.data.Oid:       'oid',
                   pytis.data.Float:     'numeric',
                   pytis.data.Boolean:   'bool',
                   pytis.data.DateTime:  'timestamp(0)',
                   pytis.data.Date:      'date',
                   pytis.data.Time:      'time',
                   pytis.data.Binary:    'bytea',
                   pytis.data.Image:     'bytea',
                   pytis.data.Color:     'varchar(7)',
                   pytis.data.LTree:    'ltree',
                   }
        try:
            result = MAPPING[type_.__class__]
        except KeyError:
            raise ProgramError('Unknown type', type_)
    return result
        
def _gsql_warning(message):
    if _GsqlConfig.warnings:
        return '-- WARNING: %s\n' % message
    else:
        return ''


class _GsqlSpec(object):

    _SQL_NAME = ''
    _PGSQL_TYPE = ''
    
    _counter = Counter()
    _groups = None
    _group_list = []
    _seen_names = []
    
    def __init__(self, name, depends=(), doc=None, grant=()):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no objektu, string nebo 'None' (v�kter�m�to p��pad� je
            jm�no vygenerov�no)
          depends -- sekvence instanc� '_GsqlSpec' nebo jejich jmen (strings),
            na kter�ch definice objektu z�vis�.  V�echny tyto objekty mus� b�t
            v�datab�zi vytvo�eny d��ve ne� tento objekt.  �ada z�vislost� m��e
            b�t rozpozn�v�na automaticky, n�kter� v�ak nikoliv, zejm�na ty,
            kter� vypl�vaj� z�nahrubo zadan�ch SQL p��kaz�, a�ty je pak nutno
            uv�st explicitn� v�tomto argumentu.  Druhotn�m krit�riem �azen�
            objekt� je vedle z�vislost� jejich s�riov� ��slo.
          doc -- dokumentace objektu, string nebo 'None' (��dn� dokumentace)
          grant -- specifikace p��stupov�ch pr�v objektu pro p��kaz GRANT;
            sekvence dvouprvkov�ch sekvenc� tvaru (SPEC, GROUP), kde SPEC je
            string SQL specifikace druhu p��stupu (nap��klad 'INSERT' nebo
            'ALL') a GROUP je SQL string ur�uj�c� odpov�daj�c� skupinu
            u�ivatel�

        """
        self._serial_number = self._counter.next()
        if name is None:
            name = '@%d' % self._serial_number
        self._set_name(name)
        self._depends = depends
        self._doc = doc
        self._grant = grant
        for g in grant:
            group = g[1]
            if group not in _GsqlSpec._group_list:
                _GsqlSpec._group_list.append(group)
        self._schemas = None

    def _set_name(self, name):
        if name in self._seen_names:
            raise ProgramError("Duplicate object name", name)
        self._name = name
        self._seen_names.append(name)
        
    def _set_schemas(self, schemas):
        if schemas is not None:
            assert isinstance(schemas, (tuple, list,)), ('invalid schema list', schemas,)
            if __debug__:
                for s in schemas:
                    assert isinstance(s, basestring), ('invalid schema', s,)
            self._schemas = schemas

    def _grant_command(self, gspec):
        right, group = gspec
        return 'GRANT %s ON %s TO GROUP %s;\n' % (right, self.name(), group)

    def _revoke_command(self):
        groups = _GsqlSpec._groups
        if groups is None:
            groups = ', '.join(map(lambda x: "GROUP %s" % x, _GsqlSpec._group_list))
            if groups:
                groups = ', %s' % groups
            _GsqlSpec._groups = groups
        return 'REVOKE ALL ON %s FROM PUBLIC%s;\n' % (self.name(), groups)

    def name(self):
        """Vra� jm�no objektu zadan� v�konstruktoru."""
        return self._name

    def extra_names(self):
        """Return sequence of additional associated object names.
        
        For instance, index names or names of sequences corresponding to serial
        columns may be returned.
        """
        return ()

    def depends(self):
        """Vra� tuple objekt�, na kter�ch tento objekt z�vis�.

        Z�vislostn� tuple je zkonstruov�no z�argumentu konstruktoru 'depends'.

        """
        return self._depends

    def serial_number(self):
        """Vra� po�adov� ��slo objektu.

        Po�adov� ��sla jsou objekt�m p�i�azov�na postupn� dle po�ad� jejich
        definic.

        """
        return self._serial_number

    def output(self, *args, **kwargs):
        """Vra� string obsahuj�c� SQL p��kazy nutn� k�vygenerov�n� objektu.

        V�t�to t��d� metoda vyvol�v� v�jimku 'ProgramError'.

        """
        output = self._output(*args, **kwargs)
        if self._schemas is None:
            result = output
        else:
            result = ''
            for s in self._schemas:
                result += self._search_path_command(s)
                result += output
            result += 'SET search_path TO "$user",public;\n'
        return result

    def _output(self):
        raise ProgramError('Not implemented')

    def _search_path_command(self, search_path):
        return "SET search_path TO %s;\n" % (search_path,)

    def outputall(self):
        """Vra� string obsahuj�c� SQL p��kazy nutn� k�vygenerov�n� objektu
        v�etn� dat.


        """
        return self.output()

    def reoutput(self):
        """Vra� string obsahuj�c� SQL p��kazy pro redefinici objektu.

        Pro t��dy, pro kter� redefinice objektu nem� smysl, vrac� pr�zdn�
        �et�zec.

        """
        return ''

    def db_all_names(class_, connection):
        """Vra� sekvenci jmen v�ech objekt� v�datab�zi dan�ho typu.

        Argumenty:

          connection -- PgConnection objekt zp��stup�uj�c� p��slu�nou datab�zi
          
        """
        data = connection.query(("select relname from pg_class, pg_namespace "+
                                 "where relkind='%s' and "+
                                 "pg_class.relnamespace=pg_namespace.oid and "+
                                 "pg_namespace.nspname='public'") %
                                class_._PGSQL_TYPE)
        names = []
        for i in range(data.ntuples):
            names.append(data.getvalue(i, 0))
        return names
    db_all_names = classmethod(db_all_names)

    def db_update(self, connection):
        """Vra� SQL p��kazy pot�ebn� k�updatu datab�ze dle aktu�ln� definice.

        Metoda p�edpokl�d�, �e objekt odpov�daj�c�ho jm�na a typu v�datab�zi
        existuje.

        Argumenty:
        
          connection -- PgConnection objekt zp��stup�uj�c� p��slu�nou datab�zi

        """
        return '-- %s up to date\n' % self.name()

    def db_remove(class_, name):
        """Vra� SQL p��kazy (string) pot�ebn� k�odstran�n� objektu z�datab�ze.

        Argumenty:

          name -- jm�no objektu, kter� m� b�t odstran�n; string
          
        """
        class_sql_name = class_._SQL_NAME
        if class_sql_name:
            result = "DROP %s %s;\n" % (class_sql_name, name,)
        else:
            result = "-- DROP ??? %s;\n" % (name,)
        return result
    db_remove = classmethod(db_remove)


class Column(object):
    """�lo�n� t��da specifikace sloupce."""
    
    def __init__(self, name, type, constraints=(), references=None,
                 default=None, index=None, doc=None):
        """Nastav atributy.

        Argumenty:

          name -- jm�no sloupce, SQL string
          type -- typ sloupce, bu� instance t��dy 'pytis.data.Type', nebo SQL
            string
          constraints -- sekvence omezen� na sloupec, SQL strings; zde se
            neuv�d� 'PRIMARY KEY', definice sloupce prim�rn�ho kl��e se
            prov�d� t��dou 'PrimaryColumn'
          references -- odkazovan� tabulka sloupce (\"REFERENCES\"), SQL
            strings
          default -- implicitn� hodnota sloupce, SQL string
          index -- if 'True', create index for this column; if a dictionary
            then it defines additional index options, currently only
            'method=METHOD' is supported where METHOD is the index method
          doc -- dokumentace sloupce, string nebo 'None' (��dn� dokumentace)

        """
        self.name = name
        self.type = type
        self.constraints = constraints
        self.references = references
        self.default = default
        self.index = index
        self.doc = doc

class PrimaryColumn(Column):
    """�lo�n� t��da specifikace sloupce, kter� je prim�rn�m kl��em.

    Stejn� jako 'Column', av�ak slou�� exkluzivn� pro definici sloupc�, kter�
    jsou prim�rn�mi kl��i.

    """

class ViewColumn(object):
    """�lo�n� t��da specifikace sloupce view."""

    def __init__(self, name, alias=None, sql=None, type=None,
                 insert='', update=''):
        """Nastav atributy.

        Argumenty:

          name -- jm�no sloupce, SQL string ve tvaru 'TABULKA.SLOUPEC'.  Ve
            view neasociovan�ch s�konkr�tn� jedinou tabulkou m��e b�t t�
            sekvence jmen sloupc�, potom bude odpov�daj�c� view sjednocen�m
            v�ce tabulek prost�ednictv�m oper�toru UNION a v�'name' nezm�n�n�
            tabulky budou na m�st� tohoto sloupce generovat NULL.
          alias -- p�ezd�vka sloupce (\"AS\"), SQL string nebo 'None'; je-li
            'None', bude nastaveno na hodnotu 'name'.
          sql -- obecn� SQL string umo��uj�c� specifikaci view sloupce jako SQL
            v�razu; je-li specifikov�n, mus� b�t tak� specifikov�n alias a name
            mus� b�t None.
          type -- explicitn� ur�en� typ -- hlavn� vyu�it� je pro UNIONY, kde se
            vyskytuj� NULL hodnoty.  
          insert -- �et�zec, kter� se pou�ije pro vlo�en� hodnoty do sloupce
            tabulky pro defaultn� generovan� insert rule. Je-li '', pou�ije se
            standardn� new hodnota, je-li None, nebude se sloupec v insert rulu
            vyskytovat.
          update -- �et�zec, kter� se pou�ije pro aktualizaci hodnoty sloupce
            tabulky pro defaultn� generovan� update rule. Je-li '', pou�ije se
            standardn� new hodnota, je-li None, nebude se sloupec v update rulu
            vyskytovat.

        """
        assert (is_sequence(name) and not sql) or \
               (is_sequence(sql) and alias and not name) or \
               (name and not sql) or \
               (sql and alias and not name)
        if not name:
            self.name = alias
            self.insert = None
            self.update = None
        else:    
            self.name = name
            self.insert = insert
            self.update = update
        if name and not alias:
            alias = _gsql_column_table_column(name)[1]
        self.alias = alias
        self.sql = sql
        self.type = type
        if self.insert == '':
            self.insert = 'new.%s' % (alias)
        if self.update == '':
            self.update = 'new.%s' % (alias)


class JoinType(object):
    """Specifika�n� t��da pro typy join�."""
    FROM = 'FROM'
    INNER = 'INNER'
    LEFT_OUTER = 'LEFT_OUTER'
    RIGHT_OUTER = 'RIGHT_OUTER'
    FULL_OUTER = 'FULL_OUTER'
    CROSS = 'CROSS'
    TEMPLATES = {FROM: 'FROM %s %s%s',
                 INNER: 'INNER JOIN %s %s\n ON (%s)',                       
                 LEFT_OUTER: 'LEFT OUTER JOIN %s %s\n ON (%s)',
                 RIGHT_OUTER: 'RIGHT OUTER JOIN %s %s\n ON (%s)',
                 FULL_OUTER: 'FULL OUTER JOIN %s %s\n ON (%s)',
                 CROSS: 'CROSS JOIN %s %s%s',
                 }
    
class SelectRelation(object):
    """�lo�n� t��da specifikace relace pro select."""
    def __init__(self, relation, alias=None,
                 key_column=None, exclude_columns=(), column_aliases=(),
                 jointype=JoinType.FROM, condition=None,
                 insert_columns=(), update_columns=(),
                 schema=None
                 ):
        """Specifikace relace pro SQL select.
        Argumenty:

          relation -- n�zev tabulky nebo view, p��p. instance Select pro
            pou�it� jako subselect ve ViewNG.
          alias -- alias pro pou�it� p�i v�b�ru sloupc�.
          key_column -- n�zev sloupce, kter� je pro danou relaci kl��em.
          exclude_columns -- seznam n�zv� sloupc�, kter� nemaj� b�t do view
            zahrnuty. Je-li None, budou pou�ity v�echny sloupce ze v�ech
            relac� (pozor na konflikty jmen!). Pokud je v seznamu
            znak '*', t�k� se specifikace v�ech sloupc� dan� relace.
          columns_aliases -- seznam dvojic (column, alias) uv�d�j�c�
            alias pro uveden� sloupec.
          jointype -- typ joinu.
          condition -- podm�nka, kter� se pou�ije ve specifikaci ON
            nebo v p��pad� typu FROM jako WHERE.
          schema -- nen�-li 'None', jde o �et�zec ur�uj�c� sch�ma, kter� se m�
            pou��t pro tuto relaci
        """
        assert jointype != JoinType.CROSS or \
               condition is None
        assert isinstance(relation, types.StringTypes) or \
               isinstance(relation, Select)
        assert schema is None or isinstance(schema, basestring), schema
        self.relation = relation
        self.schema = schema
        self.alias = alias
        self.key_column = key_column
        self.exclude_columns = exclude_columns
        self.column_aliases = column_aliases
        self.jointype = jointype
        self.condition = condition
        self.insert_columns = insert_columns
        self.update_columns = update_columns


class SelectSetType(object):
    """Specifika�n� t��da pro typy kombinace select�."""
    UNION = 'UNION'
    INTERSECT = 'INTERSECT'
    EXCEPT = 'EXCEPT'
    UNION_ALL = 'UNION_ALL'
    INTERSECT_ALL = 'INTERSECT_ALL'
    EXCEPT_ALL = 'EXCEPT_ALL'

class SelectSet(object):
    """�lo�n� t��da specifikace kombinace select�."""
    _FORMAT_SET = {
        SelectSetType.UNION: 'UNION',
        SelectSetType.INTERSECT: 'INTERSECT',
        SelectSetType.EXCEPT: 'EXCEPT',
        SelectSetType.UNION_ALL: 'UNION ALL',
        SelectSetType.INTERSECT_ALL: 'INTERSECT ALL',
        SelectSetType.EXCEPT_ALL: 'EXCEPT ALL',
        }
    def __init__(self, select, 
                 settype=None
                 ):
        """�lo�n� t��da specifikace kombinace select�.
        Argumenty:

          select -- n�zev pojmenovan�ho selectu nebo instance
            Select.
          settype -- typ spojen� select� - konstanta t��dy SelectSetType. 
        """
        self.select = select
        self.settype = settype    

    def format_select(self, indent=0):
        output = self.select.format_select(indent=indent)        
        if self.settype:
            outputset = ' ' * (indent+1) + self._FORMAT_SET[self.settype]
            output = '%s\n%s' % (outputset, output)
        return output    

    def sort_columns(self, aliases):
        self.select.sort_columns(aliases)

class TableView(object):
    """�lo�n� t��da specifikace view asociovan�ho s�tabulkou.

    Tato t��da se vyu��v� pouze ve specifikaci t��dy '_GsqlTable'.

    """

    def __init__(self, columns, exclude=None, name=None, **kwargs):
        """Nastav atributy.

        Argumenty:
        
          columns -- stejn� jako v�'_GsqlView.__init__()', v�dy jsou
            automaticky p�id�ny v�echny definovan� sloupce tabulky neuveden�
            v�argumentu 'exclude'
          exclude -- sekvence jmen sloupc� (SQL strings) dan� tabulky, kter�
            nemaj� b�t do view zahrnuty; sm� b�t t� 'None', v�kter�m�to
            p��pad� je ekvivalentn� pr�zdn� sekvenci
          name -- jm�no view; sm� b�t t� 'None', v�kter�m�to p��pad� je jm�no
            odvozeno ze jm�na tabulky
          kwargs -- kl��ov� argumenty p�edan� konstruktoru t��dy '_GsqlView';
            sm� se jednat pouze o�voliteln� argumenty onoho konstruktoru,
            povinn� argumenty jsou dopln�ny ve t��d� '_GsqlTable' automaticky

        """
        assert is_sequence(columns)
        assert exclude is None or is_sequence(exclude)
        self.columns = columns
        self.exclude = exclude or ()
        self.name = name
        self.kwargs = kwargs


class _GsqlType(_GsqlSpec):
    """Specifikace SQL typu."""

    _SQL_NAME = 'TYPE'
    _PGSQL_TYPE = 'c'
    
    def __init__(self, name, columns, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          name -- n�zev typu
          columns -- specifikace sloupc� a jejich typ�, sekvence instanc�
            t��dy Column
        """    
        super(_GsqlType, self).__init__(name, **kwargs)
        self._columns = columns
        
    def _column_column(self, column):        
        return _gsql_column_table_column(column.name)[1]

    def _format_column(self, column):
        result = '%s %s' % (self._column_column(column),
                            _gsql_format_type(column.type))
        return result

    def _output(self):
        columns = [self._format_column(c) for c in self._columns]
        columns = string.join(columns, ',\n        ')
        result = ('CREATE TYPE %s AS (\n%s);\n' %
                  (self._name, columns))
        return result


class ArgumentType(object):
    """�lo�n� t��da specifikace typu argumentu pro funkce.

    Tato t��da se vyu��v� pouze ve specifikaci t��dy '_GsqlFunction'.
    """
    def __init__(self, typ, name='', out=False):
        """Nastav atributy.

        Argumenty:
        
          typ -- n�zev typu, instance pytis.data.Type nebo _GsqlType.
          name -- voliteln� jm�no argumentu
          out -- je-li True, jde o v�stupn� argument
        """
        self.typ = typ
        self.name = name
        self.out = out

class ReturnType(object):
    """�lo�n� t��da specifikace n�vratov�ho typu.

    Tato t��da se vyu��v� pouze ve specifikaci t��dy '_GsqlFunction'.

    """
    def __init__(self, name, setof=False):
        """Nastav atributy.

        Argumenty:
        
          name -- n�zev typu, instance pytis.data.Type nebo _GsqlType.
          setof -- je-li True, jde o n�vratov� typ SETOF
        """
        self.name = name
        self.setof = setof

class _GsqlSchema(_GsqlSpec):
    """Specifikace SQL sch�matu."""
    
    _SQL_NAME = 'SCHEMA'
    
    def __init__(self, name, owner=None):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no sch�matu, SQL string
          owner -- voliteln� �et�zec ud�vaj�c� vlastn�ka sch�matu.
            Pozor, uvede-li se, pak mus� b�t vytv��en� provedeno superuserem.
        """
        super(_GsqlSchema, self).__init__(name, **kwargs)
        self._owner = owner
        
    def _output(self):
        if self._owner is not None:
            owner = " AUTHORIZATION %s" % self._owner
        else:
            owner = ''
        result = 'CREATE SCHEMA %s%s;\n' % (self._name, owner)
        return result

        
class _GsqlTable(_GsqlSpec):
    """Specifikace SQL tabulky."""
    
    _SQL_NAME = 'TABLE'
    _PGSQL_TYPE = 'r'
    
    def __init__(self, name, columns, inherits=None, view=None,
                 with_oids=True, sql=None, schemas=None,
                 on_insert=None, on_update=None, on_delete=None,
                 init_values=(), init_columns=(),
                 tablespace=None, upd_log_trigger=None,
                 indexes=(), **kwargs):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no tabulky, SQL string
          columns -- specifikace sloupc� tabulky, sekvence instanc� t��dy
            'Column'
          inherits -- sekvence jmen pod�d�n�ch tabulek (SQL strings) nebo
            'None' (ned�d� se ��dn� tabulka)
          sql -- SQL string SQL definic p�idan� za definici sloupc�, lze jej
            vyu��t pro dopln�n� SQL konstrukc� souvisej�c�ch se sloupci, kter�
            nelze zadat jin�m zp�sobem
          schemas -- nen�-li 'None', definuje sch�mata, ve kter�ch m� b�t
            tabulka vytvo�ena, jde o sekvenci �et�zc� obsahuj�c�ch textov�
            definice postgresov� search_path ur�uj�c� search_path nastavenou
            p�i vytv��en� tabulky, tabulka je samostatn� vytvo�ena pro ka�d�
            z prvk� t�to sekvence
          on_insert -- dopl�uj�c� akce, kter� maj� b�t provedeny p�i vlo�en�
            z�znamu do tabulky, SQL string
          on_update -- dopl�uj�c� akce, kter� maj� b�t provedeny p�i updatu
            z�znamu tabulky, SQL string
          on_delete -- dopl�uj�c� akce, kter� maj� b�t provedeny p�i smaz�n�
            z�znamu z�tabulky, SQL string
          view -- specifikace view asociovan�ho s�tabulkou, instance t��dy
            'TableView'.  Takov� view m��e obsahovat pouze sloupce z�t�to
            tabulky a tak� p�eb�r� n�sleduj�c� jej� vlastnosti (nejsou-li
            v�definici view uvedeny explicitn�): kl��ov� sloupce, dokumentaci,
            specifikaci p��stupov�ch pr�v.  Sm� b�t t� sekvence instanc� t��dy
            'TableView', pak je definov�n odpov�daj�c� po�et views.
            V�cetabulkov� view, je nutno specifikovat pomoc� t��dy
            '_GsqlView'.
          init_values -- sekvence inici�ln�ch hodnot vlo�en�ch do
            tabulky. ka�d� prvek je sekvence SQL strings odpov�daj�c� sloupc�m
            z�argumentu 'init_columns' nebo v�em sloupc�m tabulky v�dan�m
            po�ad�
          init_columns -- sekvence jmen sloupc� nebo instanc� t��dy 'Column',
            pro n� jsou definov�ny 'init_values'; je-li pr�zdnou sekvenc�,
            jsou to v�echny sloupce tabulky
          tablespace -- n�zev tablespace, ve kter�m bude tabulka vytvo�ena  
          upd_log_trigger -- n�zev trigger funkce, kter� bude logovat zm�ny v
            z�znamech, nebo None, pokud se nem� logovat.
          indexes -- sequence of multicolumn index definitions; each element of
            the sequence is a tuple of the form (COLUMNS, OPTIONS), where
            COLUMNS is sequence of column names and OPTIONS is (possibly empty)
            dictionary of index options.  Currently the only supported index
            option is 'method=METHOD' where 'METHOD' is a string defining the
            index method.
          kwargs -- argumenty p�edan� konstruktoru p�edka

        """
        super(_GsqlTable, self).__init__(name, **kwargs)
        self._columns = columns
        self._inherits = inherits or ()
        self._views = map(self._make_view, xtuple(view or ()))
        self._with_oids = with_oids
        self._sql = sql
        self._set_schemas(schemas)
        self._on_insert, self._on_update, self._on_delete = \
          on_insert, on_update, on_delete
        self._init_values = init_values
        self._tablespace = tablespace
        self._upd_log_trigger = upd_log_trigger
        if not init_columns:
            init_columns = columns
        self._init_columns = [isinstance(c, str) and c
                              or self._column_column(c)
                              for c in init_columns]
        self._indexes = indexes

    def _full_column_name(self, column):
        if not _gsql_column_table_column(column.name)[0]:
            column.name = "%s.%s" % (self._name, column.name)
        return column    

    def _column_column(self, column):        
        return _gsql_column_table_column(column.name)[1]

    def _grant_command(self, gspec, name=None):
        if not name:
            name = self.name()
        right, group = gspec
        return 'GRANT %s ON %s TO GROUP %s;\n' % (right, name, group)

    def _make_view(self, view):
        if view is not None:
            vcolumns = list(view.columns)
            for c in self._columns:                
                if _gsql_column_table_column(c.name)[0] is None:
                    c.name = "%s.%s" % (self._name, c.name)
                if find(c.name, vcolumns, key=lambda c: c.name) is None:
                    vcolumns.append(ViewColumn(c.name))
            kwargs = view.kwargs
            if 'key_columns' not in kwargs:
                kwargs['key_columns'] = None
            vcolumns = map(self._full_column_name, vcolumns)
            vcolumns = filter(lambda x: self._column_column(x) not in
                              view.exclude, vcolumns)
            # Remove also columns specified like table.name
            vcolumns = filter(lambda x: x.name not in view.exclude, vcolumns)
            args = (view.name or self, vcolumns)
            
            if 'doc' not in kwargs:
                kwargs['doc'] = self._doc
            if 'grant' not in kwargs:
                kwargs['grant'] = self._grant
            if 'depends' not in kwargs:
                kwargs['depends'] = (self._name,)                
            else:
                if self._name not in kwargs['depends']:
                    kwargs['depends'] = kwargs['depends'] + (self._name,)
            view = _GsqlView(*args, **kwargs)
        return view
        
    def _format_column(self, column):
        cconstraints = column.constraints
        if isinstance(column, PrimaryColumn):
            cconstraints = ('PRIMARY KEY',) + cconstraints
        if column.references is not None:
            cconstraints = ('REFERENCES %s' % column.references,) + \
                           cconstraints
        constraints = string.join(cconstraints, ' ')
        if column.default:
            default = ' DEFAULT %s' % column.default
        else:
            default = ''
        result = '%s %s %s%s' % (self._column_column(column),
                                 _gsql_format_type(column.type),
                                 constraints, default)
        return result

    def _format_column_doc(self, column):
        full_column = self._full_column_name(column)
        return "COMMENT ON COLUMN %s IS '%s';\n" % \
               (full_column.name, column.doc)

    def _format_index(self, index_spec):
        columns, options = index_spec
        def strip(name):
            return name.split('.')[-1]
        columns = [strip(c) for c in columns]
        columns_string = string.join(columns, ', ')
        columns_name = string.join(columns, '_')
        name = '%s__%s__index' % (self._name, columns_name,)
        method_spec = (isinstance(options, dict) and options.get('method'))
        if method_spec:
            method = ' USING %s' % (method_spec,)
        else:
            method = ''
        return "CREATE INDEX %s ON %s%s (%s);\n" % (name, self._name, method, columns_string,)

    def _format_rule(self, action):
        laction = string.lower(action)
        body = self.__dict__['_on_'+laction]
        if body is None:
            result = ''
        else:
            result = 'CREATE OR REPLACE RULE %s_%s AS ON %s TO %s DO %s;\n' % \
                     (self._name, laction[:3], action, self._name, body)
        return result

    def _format_init_values(self, init_values):
        insert_string = 'INSERT INTO %s (%s) VALUES (%%s);\n' % \
                        (self._name, string.join(self._init_columns, ','))
        init_inserts = [insert_string % string.join(v, ',')
                        for v in init_values]
        return string.join(init_inserts)

    def name(self):
        """Vra� jm�no tabulky zadan� v�konstruktoru."""
        return self._name
    
    def extra_names(self):
        names = []
        for c in self._columns:
            if isinstance(c.type, pytis.data.Serial):
                cname = c.name.split('.')[-1]
                names.append('%s_%s_seq' % (self._name, cname,))
        return names

    def columns(self):
        """Vra� specifikaci sloupc� zadanou v�konstruktoru."""
        return self._columns

    def key_columns(self):
        """Vra� seznam n�zv� kl��ov�ch sloupc�."""
        kcols = []
        for c in self._columns:
            if isinstance(c, PrimaryColumn):
                kcols.append(self._full_column_name(c).name)
        return kcols        
    
    def _output(self, _re=False, _all=False):
        if not _re:
            columns = map(self._format_column, self._columns)
            columns = string.join(columns, ',\n        ')
            if self._sql:
                columns = columns + ',\n        %s' % self._sql
            columns = '        %s\n' % columns
            if self._inherits:
                inherits = ' INHERITS (%s)' % string.join(self._inherits, ',')
            else:
                inherits = ''
            if self._with_oids:
                with_oids = ' WITH OIDS'
            else:
                with_oids = ' WITHOUT OIDS'
            if self._tablespace:
                tablespace = ' TABLESPACE %s' % self._tablespace
            else:
                tablespace = ''
        result = ''
        if not _re:
            result = result + ('CREATE TABLE %s (\n%s)%s%s%s;\n' %
                               (self._name, columns, inherits, with_oids,
                                tablespace))
        if self._doc is not None:
            doc = "COMMENT ON TABLE %s IS '%s';\n" % \
                  (self._name, _gsql_escape(self._doc))
            result = result + doc
        result = result + self._revoke_command()
        for g in self._grant:
            result = result + self._grant_command(g)
        for c in self._columns:
            ct = c.type
            cn = self._column_column(c)
            if isinstance(ct, str) and ct.lower() == 'serial' \
               or ct.__class__ == pytis.data.Serial:
                seqname = "%s_%s_seq" % (self._name, cn)
                for g in self._grant:                    
                    result = result + self._grant_command(g, seqname)
            if c.doc is not None:
                result = result + self._format_column_doc(c)
            if c.index is True:
                result = result + self._format_index(((c.name,), {},))
            elif isinstance(c.index, dict):
                result = result + self._format_index(((c.name,), c.index,))
            elif c.index is not None:
                raise ProgramError("Invalid column index specification",
                                   (self._name, c.name, c.index,))
        for index_spec in self._indexes:
            result = result + self._format_index(index_spec)
        for action in 'INSERT', 'UPDATE', 'DELETE':
            result = result + self._format_rule(action)
        if not _re and _all:
            result = result + self._format_init_values(self._init_values)
        if self._upd_log_trigger:
            keys = ','.join([_gsql_column_table_column(k)[1]
                             for k in self.key_columns()])
            result = result + ('CREATE TRIGGER %s_ins_log AFTER INSERT ON '
                               '%s FOR EACH ROW EXECUTE PROCEDURE '
                               '%s("%s");\n'
                               'CREATE TRIGGER %s_upd_log AFTER UPDATE ON '
                               '%s FOR EACH ROW EXECUTE PROCEDURE '
                               '%s("%s");\n'                               
                               'CREATE TRIGGER %s_del_log AFTER DELETE ON '
                               '%s FOR EACH ROW EXECUTE PROCEDURE '
                               '%s("%s");\n'
                               ) % (self._name, self._name,
                                    self._upd_log_trigger, keys,
                                    self._name, self._name,
                                    self._upd_log_trigger, keys,
                                    self._name, self._name,
                                    self._upd_log_trigger, keys)
        return result

    def outputall(self):
        return self.output(_re=False, _all=True)

    def reoutput(self):
        return self.output(_re=True)

    def db_update(self, connection):
        schemas = self._schemas
        if schemas is None:
            result = self._db_update(connection, 'public')
        else:
            result = ''
            for s in schemas:
                sname = s.split(',')[0].strip()
                result += self._db_update(connection, sname)
        return result

    def _db_update(self, connection, schema):
        name = self.name()
        query_args = dict(name=name, schema=schema)
        # Inheritance
        data = connection.query(
            ("select anc.relname from pg_class succ, pg_class anc, pg_inherits, pg_namespace "
             "where succ.relname='%(name)s' and pg_inherits.inhrelid=succ.oid and "
             "pg_inherits.inhparent=anc.oid and "
             "succ.relnamespace=pg_namespace.oid and "+
             "pg_namespace.nspname='%(schema)s'") % query_args)
        inherits = []
        for i in range(data.ntuples):
            inherits.append(data.getvalue(i, 0))
        if tuple(self._inherits) != tuple(inherits):
            result = '-- Inheritance mismatch, replacing the whole table\n'
            result = result + self.__class__.db_remove(name) + self.output()
            return result
        # Columns: name, type, primaryp, references, default,
        # constraints (unique, not null)
        result = ''
        data = connection.query(
            ("select attname, typname as typename, atttypmod as typelen, attnotnull as notnull "
             "from pg_attribute, pg_class, pg_namespace, pg_type "
             "where pg_class.relname='%(name)s' and "
             "pg_class.relnamespace=pg_namespace.oid and pg_namespace.nspname='%(schema)s' and "
             "pg_attribute.attrelid=pg_class.oid and not pg_attribute.attisdropped and pg_attribute.atttypid=pg_type.oid and pg_attribute.attnum>0 and pg_attribute.attislocal") % query_args)
        fnames = [data.fname(j) for j in range(1, data.nfields)]
        dbcolumns = {}
        for i in range(data.ntuples):
            cname = data.getvalue(i, 0)
            other = {}
            for j in range(1, data.nfields):
                k, v = fnames[j-1], data.getvalue(i, j)
                other[k] = v
            dbcolumns[cname] = other
        data = connection.query(
            ("select count(*) from pg_index, pg_class, pg_namespace "
             "where pg_class.relname='%(name)s' and "
             "pg_class.relnamespace=pg_namespace.oid and pg_namespace.nspname='%(schema)s' and "
             "pg_index.indrelid=pg_class.oid and pg_index.indisunique and pg_index.indkey[1] is not null") % query_args)
        if data.getvalue(0, 0) != 0:
            result = (result +
                      _gsql_warning("Can't handle multicolumn indexes in %s" %
                                    name))
        data = connection.query(
                ("select attname as column, indisprimary as primary, indisunique as unique "
                 "from pg_index, pg_class, pg_namespace, pg_attribute "
                 "where pg_class.relname='%(name)s' and "
                 "pg_class.relnamespace=pg_namespace.oid and pg_namespace.nspname='%(schema)s' and "
                 "pg_index.indrelid=pg_class.oid and pg_attribute.attrelid=pg_class.oid and pg_attribute.attnum=pg_index.indkey[0] and pg_index.indkey[1] is null and pg_attribute.attnum>0 and pg_attribute.attislocal") % query_args)
        for i in range(data.ntuples):
            cname = data.getvalue(i, 0)
            cproperties = dbcolumns[cname]
            cproperties['primaryp'] = cproperties.get('primaryp') or data.getvalue(i, 1)
            cproperties['uniquep'] = cproperties.get('uniquep') or data.getvalue(i, 2)
        data = connection.query(
                ("select attname, adsrc from pg_attribute, pg_attrdef, pg_class, pg_namespace "
                 "where pg_class.relname='%(name)s' and "
                 "pg_class.relnamespace=pg_namespace.oid and pg_namespace.nspname='%(schema)s' and "
                 "pg_attribute.attrelid=pg_class.oid and pg_attrdef.adrelid=pg_class.oid and pg_attribute.attnum=pg_attrdef.adnum and pg_attribute.attnum>0 and pg_attribute.attislocal") % query_args)
        for i in range(data.ntuples):
            dbcolumns[data.getvalue(i, 0)]['default'] = data.getvalue(i, 1)
        columns = list(copy.copy(self._columns))
        def column_changed(column, dbcolumn):
            def normalize(s):
                s = string.lower(string.strip(s))
                s = re.sub('  +', ' ', s)
                return s
            cname = column.name
            primaryp = isinstance(column, PrimaryColumn)
            # type
            default = column.default
            constraints = map(normalize, column.constraints)
            ct = column.type
            if isinstance(ct, str):
                TYPE_ALIASES = {'int2': 'smallint'}
                if ct != dbcolumn['typename'] and ct != TYPE_ALIASES.get(dbcolumn['typename']):
                    sys.stdout.write(
                        _gsql_warning('Possible mismatch in raw column type of %s(%s): %s x %s' %
                                      (name, cname, ct, dbcolumn['typename'],)))
            else:
                if ct.__class__ == pytis.data.Serial:
                    if default is None:
                        default = ('nextval(\'%s_%s_seq\'::regclass)'
                                   % (name, _gsql_column_table_column(cname)[1]))
                    ctclass = pytis.data.Integer
                    if not primaryp:
                        constraints.append('not null')
                else:
                    for c in ct.__class__.__mro__:
                        if c in (pytis.data.Boolean,
                                 pytis.data.Date,
                                 pytis.data.DateTime,
                                 pytis.data.Float,
                                 pytis.data.Integer,
                                 pytis.data.LTree,
                                 pytis.data.Oid,
                                 pytis.data.String,
                                 pytis.data.Time,
                                 ):
                            ctclass = c
                            break
                    else:
                        ctclass = ct.__class__
                if ctclass is pytis.data.Boolean:
                    if default.lower() in ("'f'", "'false'", "false"):
                        default = 'false'
                    elif default.lower() in ("'t'", "'true'", "true"):
                        default = 'true'
                TYPE_MAPPING = {'bool': pytis.data.Boolean,
                                'bytea': pytis.data.Binary,
                                'bpchar': pytis.data.String,
                                'char': pytis.data.String,
                                'date': pytis.data.Date,
                                'time': pytis.data.Time,
                                'smallint': pytis.data.Integer,
                                'int2': pytis.data.Integer,
                                'int4': pytis.data.Integer,
                                'int8': pytis.data.Integer,
                                'bigint': pytis.data.Integer,
                                'numeric': pytis.data.Float,
                                'ltree': pytis.data.LTree,
                                'oid': pytis.data.Oid,
                                'name': pytis.data.String,
                                'text': pytis.data.String,
                                'timestamp': pytis.data.DateTime,
                                'timestamptz': pytis.data.DateTime,
                                'varchar': pytis.data.String}
                if (ctclass is not TYPE_MAPPING[dbcolumn['typename']]):
                    return ('Type mismatch (%s x %s)' %
                            (ctclass, TYPE_MAPPING[dbcolumn['typename']]))
                if column.type.__class__ == pytis.data.String:
                    l = column.type.maxlen()
                    if l:
                        if l != dbcolumn['typelen'] - 4:
                            return ('Type mismatch (length: %s x %s)' %
                                    (l, dbcolumn['typelen']))
                    else:
                        if dbcolumn['typename'] != 'text':
                            return ('Type mismatch (text x %s)' %
                                    dbcolumn['typename'])
            # not null
            if xor(primaryp or 'not null' in constraints,
                   dbcolumn.get('notnull')):
                return 'Not null status mismatch'
            # unique
            if xor(primaryp or 'unique' in constraints,
                   dbcolumn.get('uniquep')):
                message = 'Unique status mismatch'
                return message
            # default
            MAPPINGS = {'user': '"current_user"()',
                        'session_user': '"session_user"()'}
            if default in MAPPINGS:
                default = MAPPINGS[default]
            dbcolumn_default = dbcolumn.get('default')
            if dbcolumn_default is not None and not dbcolumn.get('primaryp'):
                pos = dbcolumn_default.find('::')
                if pos != -1:
                    dbcolumn_default = dbcolumn_default[:pos]
            if default != dbcolumn_default:
                return ('Default value mismatch (%s x %s)' %
                        (default, dbcolumn.get('default')))
            # primaryp
            if xor(primaryp, dbcolumn.get('primaryp')):
                return 'Primary status mismatch'
            # OK
            return ''
        def tmp_col_name():
            i = 0
            while True:
                name = 'tmp%d' % i
                if name not in dbcolumns:
                    return name
                i = i + 1
        def column_name(column):
            return _gsql_column_table_column(column.name)[1]
        for cname, other in dbcolumns.items():
            i = position(cname, columns, key=column_name)
            if i is None:
                result = result + ("ALTER TABLE %s DROP COLUMN %s;\n" %
                                   (name, cname))
            else:
                changed = column_changed(columns[i], other)
                if changed:
                    tmp = tmp_col_name()
                    result = (
                        result +
                        ("-- Column `%s' changed: %s\n" % (cname, changed)) +
                        ('ALTER TABLE %s RENAME COLUMN %s TO %s;\n' %
                         (name, cname, tmp)) +
                        ('ALTER TABLE %s ADD COLUMN %s;\n' %
                         (name, self._format_column(columns[i]))) +
                        ('UPDATE %s SET %s=%s;\n' % (name, cname, tmp)) +
                        ("ALTER TABLE %s DROP COLUMN %s;\n" % (name, tmp)))
                del columns[i]
        for c in columns:
            result = result + ('ALTER TABLE %s ADD COLUMN %s;\n' %
                               (name, self._format_column(c)))
        # Triggers
        pass
        # Raw SQL
        if self._sql is not None:
            result = (result +
                      _gsql_warning(
                        'SQL statements present, not checked in %s' % name))
        # Initial values
        init_values = []
        for values in self._init_values:
            if tuple(values) == ('now()',):
                result = (result +
                          _gsql_warning(
                            "`now()' initial value not checked in %s" % name))
                continue
            spec = []
            for col, value in zip(self._init_columns, values):
                if value.lower() == 'null':
                    spec.append('%s is %s' % (col, value,))
                else:
                    spec.append('%s=%s' % (col, value,))
            specstring = string.join(spec, ' AND ')
            query = "SELECT COUNT(*) FROM %s WHERE %s" % (name, specstring)
            data = connection.query(query)
            if data.getvalue(0, 0) == 0:
                init_values.append(values)
        result = result + self._format_init_values(init_values)
        # Done
        if not result:
            result = super(_GsqlTable, self).db_update(connection)
        return result

    
class Select(_GsqlSpec):
    """Specifikace SQL selectu."""

    def __init__(self, relations, include_columns=(),
                 group_by=None, having=None, order_by=None, limit=None,
                 **kwargs):
        """Inicializuj instanci.
        Argumenty:

          relations -- sekvence instanc� t��dy SelectRelation nebo SelectSet.
          include_columns -- seznam instanc� t��dy ViewColumn, kter� budou
            p�id�ny ke sloupc�m z relations (typicky v�razov� sloupce).
          group_by -- string pro klauzuli GROUP BY.  
          having -- string pro klauzuli HAVING.  
          order_by -- string pro klauzuli GROUP BY.  
          limit -- hodnota limit pro select.  
        """
        super(Select, self).__init__(None, **kwargs)
        self._relations = relations
        self._group_by = group_by
        self._having = having
        self._order_by = order_by
        self._limit = limit
        self._include_columns = include_columns
        self._columns = []
        self._set = self._is_set()

    def _is_set(self):        
        first = self._relations[0]
        for r in self._relations:
            if not sameclass(r, first):
                raise ProgramError('Different classes in relation sequence.')
        if isinstance(first, SelectSet):
            if self._group_by:
                raise ProgramError('Bad Syntax: group by')
            if self._having:
                raise ProgramError('Bad Syntax: having')
            if self._include_columns:
                raise ProgramError('Bad Syntax: include_columns')               
            return True
        else:
            if self._relations[0].jointype != JoinType.FROM:
                raise ProgramError('Bad Syntax: First join must be FROM')
            return False
        
    def _column_column(self, column):
        return _gsql_column_table_column(column.name)[1]

    def _format_column(self, column):
        if isinstance(column, str):
            cname = alias = column
        else:
            if column.sql:
                cname = column.sql
            else:    
                cname = column.name
            alias = column.alias
        if type is None:
            cname = 'NULL'
        elif isinstance(type, pytis.data.Type):
            cname = 'NULL::%s' % _gsql_format_type(type)
        elif isinstance(type, str):
            cname = 'NULL::%s' % type
        return '%s AS %s' % (cname, alias)

    def _format_columns(self, indent=0):
        COLSEP = ',\n%s' % (' '*(indent+1))
        result = COLSEP.join([self._format_column(c)
                              for c in self._columns])
        return result

    def _format_relations(self, indent=1):
        def format_relation(i,rel):
            jtype = rel.jointype
            if isinstance(rel.relation, Select):
                sel = rel.relation.format_select(indent=indent+1)
                sel = sel.strip().strip('\n')
                relation = '\n%s(%s)' % (' '*(indent+1), sel)
            elif (isinstance(rel, SelectRelation) and
                  rel.schema is not None):
                relation = '%s.%s' % (rel.schema, rel.relation,)
            else:
                relation = rel.relation
            alias = rel.alias or ''
            if i == 0 or rel.condition is None:
                condition = ''
            else:    
                condition = rel.condition
            return JoinType.TEMPLATES[jtype] % (relation, alias, condition)
        # Prvn� relace je FROM a podm�nka nebude pou�ita
        wherecondition = self._relations[0].condition
        joins = [format_relation(i,r) for i,r in enumerate(self._relations)]
        result = '\n '.join(joins)
        if wherecondition:
            result = '%s\n WHERE %s' % (result, wherecondition)
        return result


    def set_columns(self, relation_columns):
        self._relation_columns = relation_columns
        vcolumns = []
        aliases = []
        def make_columns(cols, column_aliases, rel_alias):
            for c in cols:
                if isinstance(c, ViewColumn):
                    if c.alias:
                        cname = c.alias
                    else:
                        cname = c.name
                else:                    
                    cname = self._column_column(c)
                if '*' in r.exclude_columns or cname in r.exclude_columns:
                    continue
                calias = assoc(cname, column_aliases)
                if not calias:
                    calias = cname
                else:
                    calias = calias[1]
                if calias not in aliases:
                    aliases.append(calias)
                    vname = '%s.%s' % (rel_alias, cname)                    
                    vcolumns.append(ViewColumn(vname, alias=calias))
                else:
                    raise ProgramError('Duplicate column name', calias)
        if self._set:
            for r in self._relations:
                vcolumns = r.select.set_columns(self._relation_columns)
                self._columns.append(vcolumns)
        else:
            for r in self._relations:
                rel_alias = r.alias
                column_aliases = r.column_aliases
                if isinstance(r.relation, Select):
                    r.relation.set_columns(self._relation_columns)
                    columnlist = r.relation.columns()                    
                else:    
                    if '*' in r.exclude_columns:
                        continue
                    columnlist = self._relation_columns[r.relation]
                make_columns(columnlist, column_aliases, rel_alias)            
            for c in self._include_columns:
                if c.alias not in aliases:
                    aliases.append(c.alias)
                    vcolumns.append(c)
                else:
                    raise ProgramError('Duplicate column name', c.alias, aliases)
            self._columns = vcolumns
        return self._columns

    def columns(self):
        if self._set:
            columns = self._columns[0]
        else:
            columns = self._columns
        return [ViewColumn(c.alias) for c in columns]           
            

    def sort_columns(self, aliases):
        # Check length and column aliases
        from sets import Set
        colaliases = [c.alias for c in self._columns]
        missed = Set(aliases) ^ Set(colaliases)
        if len(missed) != 0:
            names = ' | '.join(missed)
            raise ProgramError('Different columns in SelectSet', names)            
        # Reorder
        columns = []
        for a in aliases:
            col = find(a, self._columns, key=lambda c: c.alias)
            if col is None:
                raise ProgramError("Can't find columns alias in SelectSet", a)
            else:
                columns.append(col)
        self._columns = columns        

    def format_select(self, indent=0):
        if not self._set:
            relations = self._format_relations(indent=indent)
            columns = self._format_columns(indent=indent)
            select = '%sSELECT\n\t%s\n %s\n' % (' '*(indent+1), columns,
                                                relations)
            if self._group_by:
                select = '%s GROUP BY %s\n' % (select, self._group_by)
            if self._having:
                select = '%s HAVING %s\n' % (select, self._having)
        else:
            #self.sort_set_columns()
            selects = []
            aliases = [c.alias for c in self._columns[0]]
            for r in self._relations:
                r.sort_columns(aliases)
                selects.append(r.format_select(indent=indent))
            select = ''.join(selects)
        if self._order_by:
            select = '%s ORDER BY %s\n' % (select, self._order_by)
        if self._limit:
            select = '%s LIMIT %s\n' % (select, self._limit)
        return select

    def _output(self):
        return ''
    

class _GsqlViewNG(Select):
    """Specifikace view (nov�)."""
    
    _SQL_NAME = 'VIEW'
    _PGSQL_TYPE = 'v'

    _INSERT = 'INSERT'
    _UPDATE = 'UPDATE'
    _DELETE = 'DELETE'
   
    def __init__(self, name, relations, schemas=None,
                 insert=(), update=(), delete=(),
                 insert_order=None, update_order=None, delete_order=None,
                 **kwargs):
        """Inicializuj instanci.
        Argumenty:

          name -- jm�no view jako SQL string
          relations -- sekvence instanc� t��dy SelectRelation
          schemas -- nen�-li 'None', definuje sch�mata, ve kter�ch m� b�t
            view vytvo�eno, jde o sekvenci �et�zc� obsahuj�c�ch textov�
            definice postgresov� search_path ur�uj�c� search_path nastavenou
            p�i vytv��en� view, view je samostatn� vytvo�eno pro ka�d�
            z prvk� t�to sekvence
          insert -- specifikace akce nad view p�i proveden� operace INSERT.
            Je-li 'None', je operace blokov�na, neprov�d� se p�i n� nic.  Je-li
            string, jedn� se o�SQL string definuj�c� kompletn� DO INSTEAD akci.
            Je-li sekvenc� SQL strings, definuj� tyto strings SQL p��kazy
            proveden� nav�c po vlo�en� p�edan�ch hodnot sloupc� do tabulky
            prvn�ho sloupce view.
          update -- specifikace akce nad view p�i proveden� operace UPDATE.
            Je-li 'None', je operace blokov�na, neprov�d� se p�i n� nic.  Je-li
            string, jedn� se o�SQL string definuj�c� kompletn� DO INSTEAD akci.
            Je-li sekvenc� SQL strings, definuj� tyto strings SQL p��kazy
            proveden� nav�c po updatu p�edan�ch hodnot sloupc� v�tabulce
            prvn�ho sloupce view.
          delete -- specifikace akce nad view p�i proveden� operace DELETE.
            Je-li 'None', je operace blokov�na, neprov�d� se p�i n� nic.  Je-li
            string, jedn� se o�SQL string definuj�c� kompletn� DO INSTEAD akci.
            Je-li sekvenc� SQL strings, definuj� tyto strings SQL p��kazy
            proveden� nav�c po smaz�n� dan�ho z�znamu z�tabulky prvn�ho sloupce
            view.
          insert_order -- None nebo sekvence n�zv� SelectRelation. Pokud je None,
            budou defaultn� inserty v insert rulu provedeny pro jednotliv�
            relace v po�ad� jejich uveden�, pokud je uvedena sekvence n�zv�,
            budou provedeny v uveden�m po�ad�. Vynech�n�m n�zvu SelectRelation
            se doc�l� toho, �e insert pro danou relaci nebude v�bec proveden.
          update_order -- None nebo sekvence n�zv� SelectRelation. Pokud je None,
            budou defaultn� updaty v update rulu provedeny pro jednotliv�
            relace v po�ad� jejich uveden�, pokud je uvedena sekvence n�zv�,
            budou provedeny v uveden�m po�ad�. Vynech�n�m n�zvu SelectRelation
            se doc�l� toho, �e update pro danou relaci nebude v�bec proveden.
          delete_order -- None nebo sekvence n�zv� SelectRelation. Pokud je None,
            budou defaultn� delety v delete rulu provedeny pro jednotliv�
            relace v po�ad� jejich uveden�, pokud je uvedena sekvence n�zv�,
            budou provedeny v uveden�m po�ad�. Vynech�n�m n�zvu SelectRelation
            se doc�l� toho, �e delete pro danou relaci nebude v�bec proveden.
        """
        #assert relations[0].jointype == JoinType.FROM
        super(ViewNG, self).__init__(relations, **kwargs)
        self._set_name(name)
        self._set_schemas(schemas)
        self._insert = insert
        self._update = update
        self._delete = delete
        self._insert_order = insert_order
        self._update_order = update_order
        self._delete_order = delete_order
        self._columns = []

    def _format_rule(self, kind, table_keys):
        def relations(list_order):
            if list_order is None:
                rels = self._relations
            else:
                rels = []
                for r in list_order:
                    rel = find(r, self._relations, lambda x: x.relation)
                    if rel is not None and \
                       not isinstance(rel.relation, SelectRelation):
                        rels.append(rel)
            return rels
        def get_key_column(relation):
            key = relation.key_column
            if not key:
                try:
                    key = table_keys[relation.relation]
                except:
                    pass
            if isinstance(key, (tuple,list)):
                return _gsql_column_table_column(key[0])[1]
            else:
                return _gsql_column_table_column(key)[1]
        def get_default_body(kind):
            columns = self._columns
            body = []
            def make_table_name(r):
                table_name = r.relation
                if isinstance(r, SelectRelation) and r.schema is not None:
                    table_name = '%s.%s' % (r.schema, table_name,)
                return table_name
            if kind == self._INSERT:
                for r in relations(self._insert_order):
                    table_name = make_table_name(r)
                    table_alias = r.alias or table_name
                    column_names = []
                    column_values = []
                    for c in columns:
                        if c.name is None:
                            continue
                        rel, col = _gsql_column_table_column(c.name)
                        if rel == table_alias and col != 'oid' and c.insert:
                            column_names.append(col)
                            if c.insert == '':
                                val = 'new.%s' % (c.alias)
                            else:    
                                val = c.insert
                                column_values.append(val)
                    bodycolumns = ',\n      '.join(column_names)
                    values = ',\n      '.join(column_values)
                    if len(column_names) > 0:
                        body.append('INSERT INTO %s (\n      %s)\n     '
                                    'VALUES (\n      %s)' % (table_name,
                                                             bodycolumns,
                                                             values))
                action = self._insert
            elif kind == self._UPDATE:
                command = 'UPDATE'
                suffix = 'upd'
                for r in relations(self._update_order):
                    table_name = make_table_name(r)
                    if isinstance(r.relation, Select):
                        continue
                    table_alias = r.alias or table_name
                    column_names = []
                    values = []                
                    for c in columns:
                        if c.name is None:
                            continue
                        rel, col = _gsql_column_table_column(c.name)
                        if rel == table_alias and col != 'oid' and c.update:
                            column_names.append(col)
                            if c.update == '':
                                val = 'new.%s' % (c.alias)
                            else:    
                                val = c.update                            
                                values.append(val)
                    settings = ',\n      '.join(['%s = %s' % (c, v)
                                                 for c, v in zip(column_names,
                                                                 values)])
                    if len(column_names) > 0:
                        key_column = get_key_column(r)
                        if not key_column:
                            raise ProgramError("Update rule: no key column "
                                           "specified", table_name)
                        condition = '%s = old.%s' % (key_column, key_column)                        
                        body.append('UPDATE %s SET\n      %s \n    WHERE %s' % 
                                    (table_name, settings, condition))
                action = self._update
            elif kind == self._DELETE:
                command = 'DELETE'
                suffix = 'del'
                for r in relations(self._delete_order):
                    table_name = make_table_name(r)
                    if isinstance(r.relation, Select):
                        continue
                    key_column = get_key_column(r)
                    if not key_column:
                        raise ProgramError("Delete rule: no key column "
                                           "specified", table_name)
                    condition = '%s = old.%s' % (key_column, key_column)
                    body.append('DELETE FROM %s \n    WHERE %s' % (table_name,
                                                                   condition,))
                action = self._delete
            else:
                raise ProgramError('Invalid rule specifier', kind)
            return body
        def get_params(kind):
            if kind == self._INSERT:
                command = 'INSERT'
                suffix = 'ins'
                action = self._insert
            elif kind == self._UPDATE:
                command = 'UPDATE'
                suffix = 'upd'
                action = self._update
            elif kind == self._DELETE:
                command = 'DELETE'
                suffix = 'del'
                action = self._delete
            else:    
                raise ProgramError('Invalid rule specifier', kind)
            return action, command, suffix
        action, command, suffix = get_params(kind)
        if action is None:
            body = 'NOTHING'
        elif is_sequence(action):
            if self._set:
                body = []
            else:    
                body = get_default_body(kind)
            body = list(body) + list(action)
            if len(body) == 0:
                body = 'NOTHING'
            else:
                body = '(%s;)' % (';\n    '.join(body))
        else:
            body = action
        return ('CREATE OR REPLACE RULE %s_%s AS\n ON %s TO %s DO INSTEAD \n' + \
                '    %s;\n\n') % \
               (self._name, suffix, command, self._name, body)
        
    def _output(self, table_keys):
        select = self.format_select()
        result = 'CREATE OR REPLACE VIEW %s AS\n%s;\n\n' % \
                 (self._name, select)        
        for kind in (self._INSERT, self._UPDATE, self._DELETE):
            result = result + self._format_rule(kind, table_keys)
        if self._doc is not None:
            doc = "COMMENT ON VIEW %s IS '%s';\n" % \
                  (self._name, _gsql_escape(self._doc))
            result = result + doc
        result = result + self._revoke_command()
        for g in self._grant:
            result = result + self._grant_command(g)
        return result

    def outputall(self, table_keys):
        return self.output(table_keys)
    
    def reoutput(self, table_keys):
        return self.output(table_keys)

ViewNG = _GsqlViewNG    

class _GsqlView(_GsqlSpec):
    """Specifikace view."""
    
    _SQL_NAME = 'VIEW'
    _PGSQL_TYPE = 'v'

    _INSERT = 'INSERT'
    _UPDATE = 'UPDATE'
    _DELETE = 'DELETE'
    
    def __init__(self, name, columns, key_columns=None,
                 table_alias=None, join=None,
                 insert=(), update=(), delete=(), **kwargs):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no view jako SQL string; m��e b�t t� instance t��dy
            '_GsqlTable', v�kter�m�to p��pad� je jm�no view zkonstruov�no ze
            jm�na tabulky p�id�n�m p�smene 'v' p�ed prvn� podtr��tko v�n�zvu
            tabulky nebo na jeho konec, pokud ��dn� podtr��tko neobsahuje
          columns -- specifikace sloupc� view, sekvence instanc� t��dy
            'ViewColumn'
          table_alias -- pokud nen� None, ur�uje pomoc� sekvence
            dvojic (TAB, ALIAS) aliasy k tabulk�m, kter� se ve view pou��vaj�
          key_columns -- sekvence jmen kl��ov�ch sloupc� view, tj. sloupc�
            jednozna�n� identifikuj�c�ch z�znam, strings
          join -- ur�uje zp�sob spojen� tabulek, je-li view slo�eno z�v�ce
            tabulek.  Je-li 'None', nen� na spojen� tabulek kladena ��dn�
            podm�nka; je-li string, jedn� se o�SQL string ur�uj�c� podm�nku
            spojen� tabulek; je-li sekvence dvojic '(COL1, COL2)', ka�d�
            z�t�chto dvojic ur�uje rela�n� spojen� tabulek prost�ednictv�m
            dan�ch sloupc�, kter� mus� b�t uvedeny sv�m n�zvem v�etn� tabulky
            jako SQL strings.  Je-li view sjednocen�m v�ce tabulek, m� argument
            stejnou podobu a� na to, �e je sekvenc� v��e uveden�ch specifikac�
            (nen�-li 'None'), v�po�ad� odpov�daj�c�m po�ad� tabulek ve
            specifikaci sloupc�.
          insert -- specifikace akce nad view p�i proveden� operace INSERT.
            Je-li 'None', je operace blokov�na, neprov�d� se p�i n� nic.  Je-li
            string, jedn� se o�SQL string definuj�c� kompletn� DO INSTEAD akci.
            Je-li sekvenc� SQL strings, definuj� tyto strings SQL p��kazy
            proveden� nav�c po vlo�en� p�edan�ch hodnot sloupc� do tabulky
            prvn�ho sloupce view.
          update -- specifikace akce nad view p�i proveden� operace UPDATE.
            Je-li 'None', je operace blokov�na, neprov�d� se p�i n� nic.  Je-li
            string, jedn� se o�SQL string definuj�c� kompletn� DO INSTEAD akci.
            Je-li sekvenc� SQL strings, definuj� tyto strings SQL p��kazy
            proveden� nav�c po updatu p�edan�ch hodnot sloupc� v�tabulce
            prvn�ho sloupce view.
          delete -- specifikace akce nad view p�i proveden� operace DELETE.
            Je-li 'None', je operace blokov�na, neprov�d� se p�i n� nic.  Je-li
            string, jedn� se o�SQL string definuj�c� kompletn� DO INSTEAD akci.
            Je-li sekvenc� SQL strings, definuj� tyto strings SQL p��kazy
            proveden� nav�c po smaz�n� dan�ho z�znamu z�tabulky prvn�ho sloupce
            view.
          kwargs -- argumenty p�edan� konstruktoru p�edka

        """
        assert is_sequence(table_alias) or table_alias is None
        if isinstance(name, _GsqlTable):
            table = name
            tname = table.name()
            pos = tname.find('_')
            if pos == -1:
                pos = len(tname)
            name = tname[:pos] + 'v' + tname[pos:]
        super(_GsqlView, self).__init__(name, **kwargs)
        self._columns = columns
        self._table_alias = table_alias
        self._join = join
        self._insert = insert
        self._update = update
        self._delete = delete
        self._key_columns = key_columns
        self._complexp = some(lambda c: is_sequence(c.name)
                              or is_sequence(c.sql),
                              self._columns)
        self._simple_columns, self._complex_columns = \
                              self._split_columns()
        self._complex_len = self._complex_columns_length()
        self._tables_from, self._tables = self._make_tables()

    def _column_table(self, column):
        return _gsql_column_table_column(column.name)[0]

    def _column_column(self, column):
        return _gsql_column_table_column(column.name)[1]


    def _split_columns(self):
        """Rozd�l� sloupce na jednoduch� a slo�en� (UNION)."""       
        if not self._complexp:
            return self._columns, None
        simple_columns = []
        complex_columns = []
        for c in self._columns:
            if is_sequence(c.name) or is_sequence(c.sql):
                complex_columns.append(c)
            else:
                simple_columns.append(c)
        return simple_columns, complex_columns        


    def _complex_columns_length(self):
        if not self._complexp:
            return None
        complex_len = is_sequence(self._complex_columns[0].name) and \
                      len(self._complex_columns[0].name) or \
                      len(self._complex_columns[0].sql) 
        for c in self._complex_columns[1:]:
            clen = is_sequence(c.name) and len(c.name) or len(c.sql)
            if clen != complex_len:
                raise GensqlError("Non-matching column length", c)
        return complex_len

      
    def _make_tables(self):
        """Vytvo�� seznam tabulek pro klauzuli from
           a seznam jmen pou�it�ch tabulek."""       
        column_tables = []
        if self._complexp:
            for i in range(self._complex_len):
                icomplex_tables = []
                for c in self._complex_columns:
                    if is_sequence(c.name) and c.name[i]:                    
                        tname = _gsql_column_table_column(c.name[i])[0]
                        if tname and tname not in icomplex_tables:
                            icomplex_tables.append(tname)
                column_tables.append(icomplex_tables)            
        else:
            simple_tables = [self._column_table(t)
                             for t in self._columns
                             if self._column_table(t) is not None]
            for t in simple_tables:
                if not t in column_tables:
                    column_tables.append(t)
        # Now we have the list of used tables
        # and we can check the appropriate use of table aliases            
        if self._table_alias:
            table_alias_tables = [x[0] for x in self._table_alias]
            table_alias_aliases = [x[1] for x in self._table_alias]
            tables_from = []
            tables = []
            if self._complexp:
                for tc in column_tables:
                    caliases = []
                    ctables_from = []
                    ctables = []
                    for t in tc:
                        if t in table_alias_tables:
                            raise ProgramError('Table name instead of alias specified',
                                               t)
                        if t in table_alias_aliases and t not in caliases:
                            caliases.append(t)
                            t = rassoc(t, self._table_alias)
                            ctables_from.append(' '.join(t))
                            ctables.append(t[0])
                        else:
                            ctables_from.append(t)
                            ctables.append(t)
                    tables_from.append(ctables_from)
                    tables.append(ctables)
            else:
                aliases = []
                for t in column_tables:
                    if t in table_alias_tables:
                        raise ProgramError('Table name instead of alias specified',
                                           t)
                    if t in table_alias_aliases and t not in aliases:
                        aliases.append(t)
                        t = rassoc(t, self._table_alias)
                        table_from = ' '.join(t)
                        table = t[0]
                    else:
                        table_from = table = t
                    if table_from not in tables_from:
                        tables_from.append(table_from)
                    if table not in tables:
                        tables.append(table)
        else:
            tables_from = tables = column_tables
        return tables_from, tables

               
    def _format_column(self, column, i=None, type=UNDEFINED):
        if isinstance(column, str):
            cname = alias = column
        else:
            if i is None:
                if column.sql:
                    cname = column.sql
                else:    
                    cname = column.name
            else:
                if column.sql:               
                    cname = column.sql[i]
                else:    
                    cname = column.name[i]
            alias = column.alias
        if type is None:
            cname = 'NULL'
        elif isinstance(type, pytis.data.Type):
            cname = 'NULL::%s' % _gsql_format_type(type)
        elif isinstance(type, str):
            cname = 'NULL::%s' % type
        return '%s AS %s' % (cname, alias)

    def _format_complex_columns(self):
        COLSEP = ',\n\t'
        def format_simple_columns(columns):
            return string.join(map(self._format_column, columns), COLSEP)
        if self._complexp:
            result_base = format_simple_columns(self._simple_columns)
            result = []
            for i in range(self._complex_len):
                colspecs = []
                for c in self._complex_columns:
                    names = c.name
                    if not c.sql and names[i] is None:
                        name = names[i]                      
                        type = c.type
#                         for j in range(len(names)):
#                             if i != j and names[j] is not None:
#                                 table_name, jcol_name = \
#                                   _gsql_column_table_column(names[j])
#                                 tc = find(names[j],
#                                           _gsql_defs[table_name].columns(),
#                                           key=(lambda c: c.name))
#                                 type = tc.type
#                                 name = jcol_name
#                                 break
                        spec = self._format_column(c, type=type)
                    else:
                        spec = self._format_column(c, i)
                    colspecs.append(spec)
                result_complex = string.join(colspecs, COLSEP)
                if result_base != '':
                    result_base = result_base + COLSEP
                result.append(result_base + result_complex)
        else:
            result = format_simple_columns(self._columns)
        return result

    def _format_rule(self, kind):
        table_columns = {}
        if not self._complexp:
            for t in self._tables:
                table_alias = assoc(t, self._table_alias or ())
                table_alias = table_alias and table_alias[1] or t
                cols = []
                for c in self._columns:
                    if not is_sequence(c.name[0]):
                        tname = self._column_table(c)
                        cname = self._column_column(c)
                        if tname == table_alias and cname != 'oid':
                            cols.append(c)
                table_columns[t] = cols
        if kind is self._INSERT:
            command = 'INSERT'
            suffix = 'ins'
            body = []
            if not self._complexp:
                for t in self._tables:
                    column_names = [self._column_column(c)
                                    for c in table_columns[t]
                                    if c.insert]
                    column_values = [c.insert == '' and
                              'new.%s' % (c.alias) or
                              c.insert                                
                              for c in table_columns[t]
                              if c.insert]                
                    columns = ',\n      '.join(column_names)
                    values = ',\n      '.join(column_values)
                    if column_names:
                        body.append('INSERT INTO %s (\n      %s)\n     VALUES (\n      %s)' % \
                                    (t, columns, values))
            action = self._insert
            body = string.join(body, ';\n    ')
        elif kind is self._UPDATE:
            command = 'UPDATE'
            suffix = 'upd'
            body = []
            if not self._complexp:
                for t in self._tables:
                    column_names = [self._column_column(c)
                                    for c in table_columns[t]
                                    if c.update]                
                    values = [c.update == '' and
                              'new.%s' % (c.alias) or
                              c.update                                
                              for c in table_columns[t]
                              if c.update]                
                    settings = ',\n      '.join(['%s = %s' % (c, v)
                                                 for c, v in zip(column_names,
                                                                 values)])
                    rels = ['%s = old.%s' % (c, _gsql_column_table_column(c)[1])
                            for c in self._key_columns
                            if _gsql_column_table_column(c)[0] == t]
                    condition = string.join(rels, ' AND ')
                    if column_names:
                        body.append('UPDATE %s SET\n      %s \n    WHERE %s' % \
                                    (t, settings, condition))
            action = self._update
            body = string.join(body, ';\n    ')
        elif kind is self._DELETE:
            command = 'DELETE'
            suffix = 'del'
            body = []
            if not self._complexp:
                for t in self._tables:
                    rels = ['%s = old.%s' % (c, _gsql_column_table_column(c)[1])
                            for c in self._key_columns
                            if _gsql_column_table_column(c)[0] == t]
                    condition = string.join(rels, ' AND ')
                    body.append('DELETE FROM %s \n    WHERE %s' % (t, condition))
            action = self._delete
            body = string.join(body, ';\n    ')            
        else:
            raise ProgramError('Invalid rule specifier', kind)
        if action is None:
            body = 'NOTHING'
        elif is_sequence(action):
            body = '(%s;\n    %s)' % (body, string.join(action, ';\n    '))
        else:
            body = action
        return ('CREATE OR REPLACE RULE %s_%s AS\n ON %s TO %s DO INSTEAD \n' + \
                '    %s;\n\n') % \
               (self._name, suffix, command, self._name, body)

    def columns(self):
        return self._columns
        
    def _output(self, table_keys):
        columns = self._format_complex_columns()
        is_union = is_sequence(columns)        
        if self._key_columns is None:
            if is_union:
                tables = functools.reduce(operator.add, self._tables)
            else:
                tables = self._tables
            key_columns = []
            for t in tables:
                if t in table_keys:
                    key_columns = key_columns + table_keys[t]
            self._key_columns = key_columns
        if self._join is None:
            where = ''
        else:
            def gen_where(spec):
                if spec is None:
                    return ''
                where = ' WHERE '
                if isinstance(spec, str):
                    join = spec
                else:
                    rels = ['%s = %s' % r for r in spec]
                    join = string.join(rels, ' AND ')
                where = where + join
                return where
            if is_sequence(columns):
                where = map(gen_where, self._join)
            else:
                where = gen_where(self._join)
        if is_union:
            tables = map(lambda t: string.join(t, ', '), self._tables_from)
            selections = [' SELECT\n\t%s\n FROM %s\n%s' % (c, t, w)
                          for t, c, w in zip(tables,
                                             columns, where)]
            body = string.join(selections, ' UNION ALL\n')
        else:
            tables = string.join(self._tables_from, ', ')
            body = ' SELECT\n\t%s\n FROM %s\n%s' % (columns, tables, where)
        result = 'CREATE OR REPLACE VIEW %s AS\n%s;\n\n' % \
                 (self._name, body)
        for kind in (self._INSERT, self._UPDATE, self._DELETE):
            result = result + self._format_rule(kind)
        if self._doc is not None:
            doc = "COMMENT ON VIEW %s IS '%s';\n" % \
                  (self._name, _gsql_escape(self._doc))
            result = result + doc
        result = result + self._revoke_command()
        for g in self._grant:
            result = result + self._grant_command(g)
        return result

    def outputall(self, table_keys):
        return self.output(table_keys)
    
    def reoutput(self, table_keys):
        return self.output(table_keys)


class _GsqlFunction(_GsqlSpec):
    """Specifikace SQL funkce."""

    _SQL_NAME = 'FUNCTION'
    
    def __init__(self, name, arguments, output_type, body=None, security_definer=False,
                 use_functions=(), schemas=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no funkce, SQL string nebo pythonov� funkce (v�kter�m�to
            p��pad� je jm�no SQL funkce shodn� s�jej�m)
          arguments -- sekvence argument� funkce ve spr�vn�m po�ad�;
            ka�d� prvek sekvence je bu� instance t��dy 'pytis.data.Type',
            SQL string nebo instance t��dy ArgumentType
          output_type -- typ n�vratov� hodnoty funkce; instance t��dy
            'pytis.data.Type', SQL string, nebo instance t��dy ReturnType
            nebo None (po��t�-li se se specifikov�n�m n�vratov� hodnoty pomoc�
            modifik�toru 'out' v 'arguments')
          use_functions -- sekvence pythonov�ch funkc�, jejich� definice maj�
            b�t p�id�ny p�ed definici funkce samotn�
          schemas -- nen�-li 'None', definuje sch�mata, ve kter�ch m� b�t
            funkce vytvo�ena, jde o sekvenci �et�zc� obsahuj�c�ch textov�
            definice postgresov� search_path ur�uj�c� search_path nastavenou
            p�i vytv��en� funkce, funkce je samostatn� vytvo�ena pro ka�d�
            z prvk� t�to sekvence
          body -- definice funkce; m��e b�t bu� SQL string obsahuj�c� t�lo
            funkce ve form� SQL p��kaz�, nebo pythonov� funkce s�dostupn�m
            zdrojov�m k�dem tvo��c� t�lo funkce v�jazyce plpython, nebo 'None',
            v�kter�m�to p��pad� je funkce pythonov� a mus� b�t hodnotou
            argumentu 'name'
          security_definer -- if True, add 'SECURITY DEFINER' to function definition
          kwargs -- argumenty p�edan� konstruktoru p�edka

        """
        if isinstance(name, str):
            the_name = name
        else:
            the_name = name.__name__
        super(_GsqlFunction, self).__init__(name, **kwargs)
        self._ins, self._outs = self._split_arguments(arguments)
        self._output_type = output_type
        self._use_functions = use_functions
        if body is None:
            body = name
        self._body = body
        self._security_definer = security_definer
        self._set_schemas(schemas)
        if self._doc is None and not isinstance(body, str):
            self._doc = body.__doc__

    def _split_arguments(self, arguments):
        ins = []
        outs = []
        for a in arguments:
            if isinstance(a, ArgumentType):
                if a.out:
                    outs.append(a)
                else:
                    ins.append(a)
            else:
                ins.append(ArgumentType(a))
        return ins, outs        

    def _format_body(self, body):
        if isinstance(body, str):
            if self._use_functions:
                raise GensqlError(
                    "Non-empty use-function list for a non-Python function",
                    self._name)
            result = "'%s' LANGUAGE SQL" % body
        else:
            def get_source(f):
                try:
                    lines, __ = inspect.getsourcelines(f)
                except IOError:
                    raise GensqlError(
                        "Source code of %s not available in `%s'" %
                        (f, self._name))
                skip = 1
                if f.__doc__ is not None:
                    skip = skip + len(string.split(f.__doc__, '\n'))
                lines = [l for l in lines if l.strip() != '']
                return _gsql_escape(string.join(lines[skip:], ''))
            source_list = map(get_source, tuple(self._use_functions)+(body,))
            # plpython nem� r�d pr�zdn� ��dky
            source_text = string.join(source_list, '')
            result = "'%s' LANGUAGE plpythonu" % source_text
        return result

    def _format_arguments(self):
        args = []
        for a in self._ins:
            typ = _gsql_format_type(a.typ)
            arg = "in %s %s" % (a.name, typ)
            args.append(arg.replace('  ',' '))
        for a in self._outs:
            typ = _gsql_format_type(a.typ)
            arg = "out %s %s" % (a.name, typ)
            args.append(arg.replace('  ',' '))
        return ', '.join(args)

    def _format_output_type(self):
        if isinstance(self._output_type, ReturnType):            
            output_type = self._output_type.name
            if self._output_type.setof:
                output_type = 'SETOF ' + output_type
        else:
            output_type = _gsql_format_type(self._output_type)
        return output_type    

    def _format_returns(self):
        if self._output_type is None:
            # Output type must be present in OUT arguments
            if len(self._outs) == 0:
                raise GensqlError(
                    "No output type or output arguments  specified for `%s'" %
                    self._name)
            else:
                returns = ''
        elif isinstance(self._output_type, ReturnType):
            if len(self._outs) > 0:
                # We can have both output_type and out arguments,
                # only to specify "SETOF RECORD" type                
                if not (self._output_type.setof and
                        self._output_type.name.upper() == 'RECORD'):
                    raise GensqlError(
                        "For out arguments only 'SETOF RECORD' output type "
                        "can be specified in `%s'" % self._name)
                else:
                    returns = 'RETURNS SETOF RECORD'
            else:    
                output_type = self._output_type.name
                if self._output_type.setof:
                    output_type = 'SETOF ' + output_type
                    returns = 'RETURNS ' + output_type
        else:
            if len(self._outs) > 0:
                raise GensqlError(
                    "For out arguments only 'SETOF RECORD' output type "
                    "can be specified in `%s'" % self._name)
            output_type = _gsql_format_type(self._output_type)
            returns = 'RETURNS %s' % output_type
        return returns

    def _format_security(self):
        if self._security_definer:
            return " SECURITY DEFINER"
        else:
            return ""
    
    def _output(self):
        # input_types = string.join(map(_gsql_format_type, self._input_types),
        #                          ',')
        # output_type = _gsql_format_type(self._output_type)
        arguments = self._format_arguments()
        # output_type = self._format_output_type()
        returns = self._format_returns()
        body = self._format_body(self._body)
        security = self._format_security()
        result = 'CREATE OR REPLACE FUNCTION %s (%s) %s\nAS %s%s;\n' % \
                 (self._name, arguments, returns, body, security)
        if self._doc:
        #    doc = "COMMENT ON FUNCTION %s (%s) IS '%s';\n" % \
        #          (self._name, input_types, _gsql_escape(self._doc))
            doc = "COMMENT ON FUNCTION %s (%s) IS '%s';\n" % \
                  (self._name, arguments, _gsql_escape(self._doc))
            result = result + doc
        return result

    def reoutput(self):
        return self.output()

    def db_all_names(self, connection):
        data = connection.query("select proname from pg_proc, pg_namespace where "
                                "pg_proc.pronamespace=pg_namespace.oid and "
                                "pg_namespace.nspname='public'")
        names = []
        for i in range(data.ntuples):
            n = data.getvalue(i, 0)
            if n not in ('plpythonu_call_handler', 'plpgsql_call_handler'):
                names.append(n)
        return names
    db_all_names = classmethod(db_all_names)
    

class _GsqlSequence(_GsqlSpec):
    """Specifikace sekvence (\"SEQUENCE\")."""

    _SQL_NAME = 'SEQUENCE'
    _PGSQL_TYPE = 'S'
    
    def __init__(self, name, increment=None, minvalue=None,
                 maxvalue=None, start=None, cycle=None, schemas=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no sekvence, SQL string
          increment, minvalue, maxvalue, start, cycle -- viz 'create sequence'
          schemas -- nen�-li 'None', definuje sch�mata, ve kter�ch m� b�t
            datab�zov� sekvence vytvo�ena, jde o tuple nebo list �et�zc�
            obsahuj�c�ch textov� definice postgresov� search_path ur�uj�c�
            search_path nastavenou p�i vytv��en� datab�zov� sekvence, tabulka
            je samostatn� vytvo�ena pro ka�d� z prvk� tohoto tuple nebo listu
          kwargs -- argumenty p�edan� konstruktoru p�edka

        """
        super(_GsqlSequence, self).__init__(name, **kwargs)
        self._increment = increment
        self._minvalue = minvalue
        self._maxvalue = maxvalue
        self._start = start
        self._cycle = cycle
        self._set_schemas(schemas)
        
    def _output(self):
        result = 'CREATE SEQUENCE %s' % self._name
        if self._increment:
            result = result + ' INCREMENT %s' % self._increment
        if self._minvalue:
            result = result + ' MINVALUE %s' % self._minvalue
        if self._maxvalue:
            result = result + ' MAXVALUE %s' % self._maxvalue
        if self._start:
            result = result + ' START %s' % self._start
        if self._cycle:
            result = result + ' CYCLE'
        result = result + ';\n'
        if self._doc is not None:
            doc = "COMMENT ON SEQUENCE %s IS '%s';\n" % \
                  (self._name, _gsql_escape(self._doc))
            result = result + doc
        result = result + self._revoke_command()
        for g in self._grant:
            result = result + self._grant_command(g)
        return result


class _GsqlRaw(_GsqlSpec):
    """Prost� SQL p��kazy."""
    
    def __init__(self, sql, file_name=None, schemas=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          sql -- SQL p��kazy, SQL string
          file_name -- byly-li SQL p��kazy na�teny jako kompletn� obsah
            n�jak�ho SQL souboru, je tento argument jm�nem onoho souboru
            (v�jak�koliv podob�, tento argument m� pouze funkci dokumenta�n�);
            v�jin�m p��pad� je 'None'
          schemas -- nen�-li 'None', definuje sch�mata, ve kter�ch maj� b�t
            p��kazy provedeny, jde o sekvenci �et�zc� obsahuj�c�ch textov�
            definice postgresov� search_path ur�uj�c� search_path nastavenou
            p�i prov�d�n� p��kaz�, p��kazy jsou samostatn� provedeny pro ka�d�
            z prvk� t�to sekvence
          kwargs -- argumenty p�edan� konstruktoru p�edka

        """
        if 'name' not in kwargs:
            kwargs['name'] = None
        super(_GsqlRaw, self).__init__(**kwargs)
        self._sql = sql
        self._file_name = file_name
        self._set_schemas(schemas)

    def _output(self):
        result = self._sql + '\n'
        if self._file_name:
            result = '''
            
---------------------------
-- Included file: %s --
---------------------------

%s

-- End of included file: %s
''' % (self._file_name, result, self._file_name)
        return result

    def reoutput(self):
        sys.stdout.write(
            _gsql_warning("Raw SQL commands not considered: %s" %
                          self.name()))
        return super(_GsqlRaw, self).reoutput()

    def db_update(self, connection):
        return _gsql_warning('Raw command not considered: %s' % self.name())


class _GviewsqlRaw(_GsqlSpec):
    """View definovan� prost�mi SQL p��kazy."""
   
    def __init__(self, name, columns, fromitems, where=None, 
                 groupby=None, having=None,
                 insert=None, update=None, delete=None,
                 **kwargs):
        """Inicializuj instanci.

        Argumenty:

          name -- n�zev view
          columns -- textov� vyjmenovan� sloupce
          fromitems -- textov� vyjmenovan� relace a joins
          where -- textov� vyjmenovan� podm�nky
          groupby -- textov� vyjmenovan� GROUP BY
          having -- textov� vyjmenovan� HAVING
          insert -- None nebo textov� vyjmenovan� akce pro insert rule
          update -- None nebo textov� vyjmenovan� akce pro update rule
          delete -- None nebo textov� vyjmenovan� akce pro delete rule
          kwargs -- argumenty p�edan� konstruktoru p�edka
        """
        super(_GviewsqlRaw, self).__init__(name, **kwargs)
        self._columns = columns
        self._fromitems = fromitems
        self._where = where
        self._groupby = groupby
        self._having = having
        self._insert = insert
        self._update = update
        self._delete = delete

    def _format_rule(self, kind, action):
        suffixes = {'INSERT': 'ins',
                    'UPDATE': 'upd',
                    'DELETE': 'del',
                    }
        if action is None:
            body = 'NOTHING'
        elif is_sequence(action):
            body = '(%s;\n    %s)' % (body, string.join(action, ';\n    '))
        else:
            body = action
        rule = ("CREATE OR REPLACE RULE %s_%s\n"
                "AS ON %s TO %s DO INSTEAD\n"
                "%s;\n\n") % (self._name, suffixes[kind],
                           kind, self._name, body)
        return rule

    def _output(self):
        if isinstance(self._columns, types.TupleType):
            self._columns = ', '.join(self._columns)
        body = "SELECT %s\nFROM %s\n" % (self._columns, self._fromitems)
        if self._where:
            body += "WHERE %s\n" % self._where
        if self._groupby:
            body += "GROUP BY %s\n" % self._groupby
        if self._having:
            body += "HAVING %s\n" % self._having
        result = "CREATE OR REPLACE VIEW %s AS\n%s;\n\n" % \
                 (self._name, body)
        for kind, action in (('INSERT', self._insert),
                             ('UPDATE', self._update),
                             ('DELETE', self._delete)):
            result = result + self._format_rule(kind, action)
        if self._doc is not None:
            doc = "COMMENT ON VIEW %s IS '%s';\n" % (self._name,
                                                     self._doc)
        result = result + self._revoke_command()
        for g in self._grant:
            result = result + self._grant_command(g)
        return result

    def reoutput(self):
        sys.stdout.write(
            _gsql_warning("Raw SQL commands not considered: %s" %
                          self.name()))
        return super(_GviewsqlRaw, self).reoutput()

    def db_update(self, connection):
        return _gsql_warning('Raw command not considered: %s' % self.name())


class _GsqlDefs(UserDict.UserDict):

    def __init__(self):
        UserDict.UserDict.__init__(self)
        self._resolved = []
        self._unresolved = []
        self._table_keys = {}
        self._relation_columns = {}
        
    def _resolvedp(self, spec):
        missing = some(lambda d: d not in self._resolved, spec.depends())
        return not missing

    def _update_unresolved(self):
        resolved, unresolved = self._resolved, self._unresolved
        while True:
            new_unresolved = []
            for o in unresolved:
                if self._resolvedp(self[o]):
                    self._add_resolved(o)
                else:
                    new_unresolved.append(o)
            if len(unresolved) == len(new_unresolved):
                self._unresolved = unresolved
                break
            else:
                unresolved = new_unresolved

    def _process_resolved(self, function):
        for o in self._unresolved:
            missing = [d for d in self[o].depends() if d not in self._resolved]
            _signal_error('Unresolved object: %s %s\n' % (o, missing,))
        resolved = {}
        for o in self._resolved:
            function(o)

    def _add_resolved(self, name):
        i = len(self._resolved)
        for o in self._resolved:
            if name in self[o].depends():
                i = self._resolved.index(o)
                break
        self._resolved.insert(i, name)
        # Pro tables a views updatujeme seznam sloupc�
        spec = self[name]
        if isinstance(spec, _GsqlTable) or isinstance(spec, _GsqlView):
            self._relation_columns[name] = spec.columns()
        elif isinstance(spec, Select):
            spec.set_columns(self._relation_columns)
            self._relation_columns[name] = spec.columns()
        
    def add(self, spec):
        name = spec.name()
        if isinstance(spec, _GsqlTable):
            if name not in self._table_keys:
                self._table_keys[name] = spec.key_columns()
            for v in spec._views:
                vname = v.name()
                self[vname] = v
                if self._resolvedp(v):
                    self._add_resolved(vname)
                    self._update_unresolved()
                else:
                    self._unresolved.append(vname)
        if name in self:
            _signal_error("Duplicate objects for name `%s': %s %s\n" %
                          (name, spec, self[name],))
        self[name] = spec
        if self._resolvedp(spec):
            self._add_resolved(name)
            self._update_unresolved()
        else:
            self._unresolved.append(name)

    def gensql(self):
        def process(o):
            if isinstance(self[o], (_GsqlView, _GsqlViewNG)):
                sys.stdout.write(self[o].output(self._table_keys))
            else:
                sys.stdout.write(self[o].output())
            sys.stdout.write('\n')
        self._process_resolved(process)

    def gensqlall(self):
        def process(o):
            if isinstance(self[o], (_GsqlView, _GsqlViewNG)):
                sys.stdout.write(self[o].outputall(self._table_keys))
            else:
                sys.stdout.write(self[o].outputall())
            sys.stdout.write('\n')
        self._process_resolved(process)

    def regensql(self):
        def process(o):
            if isinstance(self[o], (_GsqlView, _GsqlViewNG)):
                sys.stdout.write(self[o].reoutput(self._table_keys))
            else:
                sys.stdout.write(self[o].reoutput())
            sys.stdout.write('\n')
        self._process_resolved(process)

    def get_connection(self):
        from pyPgSQL import libpq
        while True:
            connection_string = ''
            for option, accessor in (('user', _GsqlConfig.dbuser),
                                     ('password', _GsqlConfig.dbpassword),
                                     ('host', _GsqlConfig.dbhost),
                                     ('dbname', _GsqlConfig.dbname)):
                value = accessor
                if value != None:
                    connection_string = "%s %s='%s'" % \
                                        (connection_string, option, value)
            try:
                connection = libpq.PQconnectdb(connection_string)
            except libpq.DatabaseError, e:
                if string.find(e.args[0], 'password') >= 0:
                    import getpass
                    stdout = sys.stdout
                    sys.stdout = sys.stderr
                    try:
                        _GsqlConfig.dbpassword = getpass.getpass()
                    finally:
                        sys.stdout = stdout
                    continue
                raise
            break
        return connection

    def check_db(self, _quietly=False):
        # Open the database connection
        connection = self.get_connection()
        # Found all relevant objects in all the object classes
        all_names = []
        names = {}
        sql_types_classes = {}
        classes = []
        def process(name):
            o = self[name]
            c = o.__class__
            if c not in classes:
                sql_type = c._SQL_NAME
                classes.append(c)
                sql_types_classes[sql_type] = c
                if sql_type not in names:
                    db_names = c.db_all_names(connection)
                    names[sql_type] = db_names
                    for d in db_names:
                        if d not in all_names:
                            all_names.append(d)
        self._process_resolved(process)
        # Remove objects of wrong types and build the list of objects to update
        to_create = []
        to_update = []
        to_remove = []
        def process(name):
            o = self[name]
            i = position(name, all_names)
            if i is None:
                to_create.append(o)
            else:
                del all_names[i]
                c = o.__class__
                sql_type = c._SQL_NAME
                if name in names[sql_type]:
                    to_update.append(o)
                    names[sql_type].remove(name)
                else:
                    to_remove.append(o)
                for n in o.extra_names():
                    try:
                        all_names.remove(n)
                    except ValueError:
                        pass
        self._process_resolved(process)
        for o in to_remove:
            sys.stdout.write(o.db_remove(o.name()))
        # Remove orphans
        for sql_type, ns in names.items():
            c = sql_types_classes[sql_type]
            for n in ns:
                if n in all_names:
                    if _GsqlConfig.check_presence:
                        sys.stdout.write('EXTRA: %s\n' % (n,))
                    else:
                        sys.stdout.write(c.db_remove(n))
        # Create and update objects
        for o in to_create:
            if _GsqlConfig.check_presence:
                sys.stdout.write('MISSING: %s\n' % (o.name(),))
            elif isinstance(o, (_GsqlView, _GsqlViewNG)):
                sys.stdout.write(o.output(self._table_keys))
            else:
                sys.stdout.write(o.output())
        for o in to_update:
            update_commands = o.db_update(connection)
            if _GsqlConfig.check_presence:
                for line in update_commands.split('\n'):
                    if line and line[:2] != '--':
                        sys.stdout.write('CHANGED: %s\n' % (o.name(),))
                        break
            else:
                sys.stdout.write(update_commands)
        # Finish
        if _GsqlConfig.warnings and not _quietly:
            sys.stderr.write("""
Done.
Objects other than tables and sequences not checked.  You can use the
--recreate option to replace them unconditionally, without any loss of data.
References, triggers and special constraints not checked -- there are no easy
introspection means to check them.
Other checks may be missing as well.  Please create a new database and compare
database dumps if you want to be sure about your schema.
""")

    def fix_db(self):
        self.check_db(_quietly=True)
        self.regensql()

    def update_views(self):
        obj = _GsqlConfig.update_views
        connection = self.get_connection()
        depends = []
        todo = [_GsqlConfig.update_views]
        views = []
        while todo != []:
            v = todo.pop()
            if v in depends:
                continue
            else:
                depends.append(v)
            data = connection.query(
               ("select distinct c.relname "
                " from pg_class d, pg_depend a join pg_rewrite b on a.objid=b.oid "
                " join pg_class c on ev_class=c.oid "
                " join pg_views e on e.viewname=c.relname "
                " where refclassid = 'pg_class'::regclass and refobjid = d.oid "
                " and ev_class<>d.oid and d.relname='%s'") %
               v)
            for i in range(data.ntuples):
                todo.append(data.getvalue(i, 0))
        # sys.stdout.write('drop view %s cascade;\n' % obj)        
        def process(o):
            name = self[o].name()
            if name not in depends:
                return
            if isinstance(self[o], (_GsqlView, _GsqlViewNG)):
                sys.stdout.write("DROP VIEW IF EXISTS %s CASCADE;\n\n" % name)
                sys.stdout.write(self[o].reoutput(self._table_keys))
            else:
                sys.stdout.write(self[o].reoutput())
            sys.stdout.write('\n')
        self._process_resolved(process)
        

_gsql_defs = _GsqlDefs()

def _gsql_process(class_, args, kwargs):
    spec = class_(*args, **kwargs)
    _gsql_defs.add(spec)
    return spec

def sqltype(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GsqlType."""
    return _gsql_process(_GsqlType, args, kwargs)

def schema(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GsqlSchema."""
    return _gsql_process(_GsqlSchema, args, kwargs)

def table(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GsqlTable."""
    return _gsql_process(_GsqlTable, args, kwargs)

def view(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GsqlView."""
    return _gsql_process(_GsqlView, args, kwargs)

def viewng(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GsqlViewNG."""
    return _gsql_process(_GsqlViewNG, args, kwargs)


def function(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GsqlFunction.
    """
    return _gsql_process(_GsqlFunction, args, kwargs)

def sequence(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GsqlSequence.
    """
    return _gsql_process(_GsqlSequence, args, kwargs)


def sql_raw(text, name=None, depends=(), **kwargs):
    """Specifikace prost�ch SQL p��kaz�.

    Argumenty:

      text -- string obsahuj�c� k��en� SQL p��kazy
      depends -- stejn� jako v�'_GsqlSpec.__init__()'

    Tato funkce by m�la b�t pou��v�na jen ve v�jime�n�ch p��padech, nebo� v�n�
    obsa�en� k�d je mimo dosah v�ech kontrol.
      
    """
    kwargs = copy.copy(kwargs)
    kwargs.update({'name': name, 'depends': depends})
    return _gsql_process(_GsqlRaw, (text,), kwargs)


def view_sql_raw(*args, **kwargs):
    """Z�hlediska specifikace ekvivalentn� vol�n� konstruktoru '_GviewsqlRaw."""
    return _gsql_process(_GviewsqlRaw, args, kwargs)


def sql_raw_include(file_name, depends=(), **kwargs):
    """Specifikace souboru obsahuj�c�ho SQL p��kazy, kter� m� b�t na�ten.

    Tento soubor je beze zm�ny zahrnut do v�sledn�ch SQL p��kaz�.

    Argumenty:

      file_name -- jm�no k��en�ho SQL souboru; absolutn� nebo relativn� v��i
        aktu�ln�mu adres��i
      depends -- stejn� jako v�'_GsqlSpec.__init__()'

    """
    f = open(file_name)
    sql = f.read()
    f.close()
    kwargs = copy.copy(kwargs)
    kwargs.update({'depends': depends, 'file_name': file_name})
    return _gsql_process(_GsqlRaw, (sql,), kwargs)


def include(file_name):
    """Zpracuj pythonov� soubor 'file_name'.

    Soubor je zpracov�n prost�ednictv�m vol�n� 'execfile()'.
    
    Argumenty:

      file_name -- jm�no k��en�ho pythonov�ho souboru; absolutn� nebo relativn�
        v��i aktu�ln�mu adres��i

    """
    execfile(file_name, copy.copy(globals()))


###############################################################################


class _GsqlConfig:
    
    GENDB = _gsql_defs.gensql
    GEALL = _gsql_defs.gensqlall
    RGNDB = _gsql_defs.regensql
    CHKDB = _gsql_defs.check_db
    FIXDB = _gsql_defs.fix_db
    UPDVW = _gsql_defs.update_views

    request = RGNDB
    warnings = True
    dbname = None
    dbhost = None
    dbuser = None
    dbpassword = None
    check_presence = False


_GSQL_OPTIONS = (
    ('help             ', 'print this help message and exit'),
    ('create           ', 'create all database objects without data (default action)'),
    ('create-all       ', 'create all database objects with data'),
    ('recreate         ', 'recreate all non-data database objects'),
    ('check-db=DATABASE', 'check DATABASE contents against definitions'),
    ('check-presence   ', 'just check for presence rather than generating updates'),
    ('update-views=object', 'update views dependent on specified object (table or view)'),
    ('fix-db=DATABASE  ', 'update DATABASE contents according to definitions'),
    ('no-warn          ', 'suppress warnings when checking/fixing'),
    ('host=HOST        ', 'connect to DATABASE at HOST'),
    ('user=USER        ', 'connect to DATABASE as USER'),
    ('password=PASSWORD', 'use PASSWORD when connecting to DATABASE'),
    ('database=DATABASE', 'DATABASE for connection string'),
    )

def _usage(optexception=None):
    _USAGE = 'usage: %s file\n' % sys.argv[0]
    if optexception:
        sys.stderr.write(optexception.msg)
        sys.stderr.write('\n')
    sys.stderr.write(_USAGE)
    sys.stderr.write('\nOptions:\n')
    for option, description in _GSQL_OPTIONS:
        sys.stderr.write('  --%s %s\n' % (option, description))
    sys.stderr.write('\n')
    sys.exit(_EXIT_USAGE)


def _go(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    def extract_option(odef):
        option = odef[0]
        option = string.strip(option)
        pos = option.find('=')
        if pos >= 0:
            option = option[:pos+1]
        return option
    try:
        opts, args = getopt.getopt(argv, '',
                                   map(extract_option, _GSQL_OPTIONS))
    except getopt.GetoptError, e:
        _usage(e)
    for o, v in opts:
        if o == '--help':
            _usage()
        elif o == '--host':
            _GsqlConfig.dbhost = v
        elif o == '--user':
            _GsqlConfig.dbuser = v
        elif o == '--password':
            _GsqlConfig.dbpassword = v
        elif o == '--database':
            _GsqlConfig.dbname = v
        elif o == '--create':
            _GsqlConfig.request = _GsqlConfig.GENDB
        elif o == '--create-all':
            _GsqlConfig.request = _GsqlConfig.GEALL
        elif o == '--recreate':
            _GsqlConfig.request = _GsqlConfig.RGNDB
        elif o == '--check-db':
            _GsqlConfig.request = _GsqlConfig.CHKDB
            _GsqlConfig.dbname = v
        elif o == '--check-presence':
            _GsqlConfig.check_presence = True
        elif o == '--update-views':
            _GsqlConfig.request = _GsqlConfig.UPDVW
            _GsqlConfig.update_views = v
        elif o == '--fix-db':
            _GsqlConfig.request = _GsqlConfig.FIXDB
            _GsqlConfig.dbname = v
        elif o == '--no-warn':
            _GsqlConfig.warnings = False
        else:
            raise ProgramError('Unrecognized option', o)
    if len(args) != 1:
        _usage()
    execfile(args[0], copy.copy(globals()))
    _GsqlConfig.request()
    

if __name__ == '__main__':
    _go()
    sys.exit(exit_code)
