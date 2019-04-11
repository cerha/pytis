# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 Brailcom, o.p.s.
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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


"""Obecná implementace databázového rozhraní pro PostgreSQL.

Tato část je nezávislá na konkrétní použité knihovně pro přístup k PostgreSQL,
tvoří jen obecné rozhraní skládající například SQL příkazy.  Fyzický přístup
k databázi zajišťují rozhraní dále implementovaná v jiných zdrojových souborech.

"""

import copy
import os
import re
import string
import sys
import thread
import threading
import time
import weakref

import pytis.data
from pytis.data import DBException, DBInsertException, DBLockException, DBRetryException, \
    DBSystemException, DBUserException, NotWithinSelect, DBConnection, DBConnectionPool, DBData, \
    ColumnSpec, DBColumnBinding, Row, Function, dbtable, reversed_sorting, \
    Array, Binary, Boolean, Date, DateTime, Float, FullTextIndex, Inet, Integer, LTree, \
    Macaddr, Number, Range, Serial, String, Time, TimeInterval, ival, sval, \
    Type, Value, Operator, AND, OR, EQ, NE, GT, LT, FORWARD, BACKWARD, ASCENDENT, DESCENDANT
import pytis.util
from pytis.util import ACTION, Counter, DEBUG, ecase, EVENT, is_anystring, is_sequence, \
    log, object_2_5, OPERATIONAL, ProgramError, remove_duplicates, UNDEFINED, \
    with_lock, xtuple
import evaction

try:
    # WeakSet was introduced in Python 2.7 and we need to support Wiking
    # applications on older installations.
    WeakSet = weakref.WeakSet
except AttributeError:
    class WeakSet(weakref.WeakValueDictionary):

        def __init__(self):
            weakref.WeakValueDictionary.__init__(self)
            self._pg_counter = pytis.util.Counter()

        def add(self, item):
            self[self._pg_counter.next()] = item

        def __iter__(self):
            for v in self.values():
                yield v

_ = pytis.util.translations('pytis-data')


# Modifikace tabulek se oznamuje zasláním notifikace `__modif_table', kde `table'
# je jméno modifikované tabulky.


def pg_encoding(enc):
    ENCODING_MAPPING = {'utf-8': 'unicode',
                        'iso-8859-1': 'latin1',
                        'iso-8859-2': 'latin2',
                        'iso-8859-3': 'latin3',
                        'iso-8859-4': 'latin4',
                        'iso-8859-9': 'latin5',
                        'iso-8859-10': 'latin6',
                        'iso-8859-13': 'latin7',
                        'iso-8859-14': 'latin8',
                        'iso-8859-15': 'latin9',
                        'iso-8859-16': 'latin10',
                        'iso-8859-5': 'iso_8859_5',
                        'iso-8859-6': 'iso_8859_6',
                        'iso-8859-7': 'iso_8859_7',
                        'iso-8859-8': 'iso_8859_8',
                        'iso8859-1': 'latin1',
                        'iso8859-2': 'latin2',
                        'iso8859-3': 'latin3',
                        'iso8859-4': 'latin4',
                        'iso8859-9': 'latin5',
                        'iso8859-10': 'latin6',
                        'iso8859-13': 'latin7',
                        'iso8859-14': 'latin8',
                        'iso8859-15': 'latin9',
                        'iso8859-16': 'latin10',
                        'iso8859-5': 'iso_8859_5',
                        'iso8859-6': 'iso_8859_6',
                        'iso8859-7': 'iso_8859_7',
                        'iso8859-8': 'iso_8859_8',
                        'ascii': 'sql_ascii',
                        }
    enc = enc.lower().strip()
    return ENCODING_MAPPING.get(enc, enc)


class _PgValue(object):

    def __init__(self, value):
        if isinstance(value, _PgValue):
            value = value.value()
        self._value = value
        self._pg_value = self._convert_value(value)

    def _convert_value(self, value):
        v = value.value()
        t = value.type()
        if v is None:
            result = None
        elif isinstance(t, Array):
            result = [vv.value() for vv in v]
        elif isinstance(t, Binary):
            result = buffer(v)
            if not str(result):
                result = None
        else:
            result = v
        return result

    def value(self):
        return self._value

    def pg_value(self):
        return self._pg_value

    def query(self):
        template, args = _Query.next_arg(self)
        return _Query(template, args)


class _Query(object):

    _n = 0

    def __init__(self, template, args=None):
        assert isinstance(template, basestring), template
        assert args is None or isinstance(args, dict), args
        if __debug__:
            if args is not None:
                for v in args.values():
                    assert isinstance(v, (_Query, _PgValue, Value,)), v
        self._template = template
        self._args = copy.copy(args) or {}

    def template(self):
        return self._template

    def args(self):
        return self._args

    def __nonzero__(self):
        return not not self._template

    def __add__(self, other):
        args = copy.copy(self._args)
        if isinstance(other, basestring):
            template = self._template + other
        else:
            template = self._template + other._template
            args.update(other._args)
        return _Query(template, args)

    def __mod__(self, args):
        new_args = copy.copy(self._args)
        new_args.update(args)
        return _Query(self._template, new_args)

    def append(self, template, args):
        new_args = copy.copy(self._args)
        new_args.update(args)
        return _Query(self._template + template, new_args)

    def wrap(self, function=''):
        return _Query(function + '(' + self._template + ')', self._args)

    def label(self, name):
        return self.append(' as %s' % (name,), {})

    def cast(self, type_):
        result = self
        if type_:
            result = result + '::' + type_
        return result

    def update(self, update_args):
        args = copy.copy(update_args)
        template, args = self.query(args)
        return self.__class__(template, args)

    def query(self, args=None, expand=None):
        query = self._template
        if expand is None:
            expand = args is None
        if args is None:
            args = {}
        for k, v in self._args.items():
            if k in args:
                continue
            if isinstance(v, Value):
                v = _PgValue(v)
            if isinstance(v, _PgValue):
                args[k] = v.pg_value() if expand else v
            else:
                q, a = v.query(args, expand)
                for kk, vv in a.items():
                    if kk not in args:
                        args[kk] = vv
                format_key = '%%(%s)s' % (k,)
                while True:
                    pos = query.find(format_key)
                    if pos < 0:
                        break
                    query = query[:pos] + q + query[pos + len(format_key):]
        return query, args

    def format(self):
        query, args = self.query()
        for k, v in args.items():
            if isinstance(v, basestring):
                args[k] = "'" + v + "'"
            elif isinstance(v, buffer):
                args[k] = '<binary_data>'
        return query % args

    @classmethod
    def join(class_, queries, separator=', '):
        templates = []
        args = {}
        for q in queries:
            if isinstance(q, Value):
                q = _PgValue(q)
            if isinstance(q, basestring):
                templates.append(q)
            elif isinstance(q, _PgValue):
                t, a = class_.next_arg(q)
                templates.append(t)
                args.update(a)
            else:
                templates.append(q.template())
                args.update(q.args())
        return _Query(string.join(templates, separator), args)

    @classmethod
    def next_arg(class_, value):
        _Query._n += 1
        arg = '__qarg_%d' % (_Query._n,)
        return '%(' + arg + ')s', {arg: value}

    @classmethod
    def next_arg_query(class_, value):
        t, a = class_.next_arg(value)
        return _Query(t, a)


class _QFunction(_Query):

    def __init__(self, name, arguments=None):
        if arguments is None:
            t, a = self.next_arg(None)
            q = _Query(t)
            self._values_arg = a.keys()[0]
        else:
            q = self.__class__.join(arguments)
        q = q.wrap(name)
        super(_QFunction, self).__init__(q.template(), q.args())

    def values_arg(self):
        return self._values_arg

    def values(self, values):
        args = copy.copy(self._args)
        args[self._values_arg] = self.join(values)
        return _Query(self._template, args)


class _QInsert(_Query):

    def __init__(self, table, columns):
        t = self.next_val()[0]
        self._values_arg = t
        super(_QInsert, self).__init__("insert into %s (%s) values (%s)" % (table, t,))

    def values(self, values):
        args = copy.copy(self._args)
        args[self._values_arg] = self.join(values)
        return _Query(self._template, args)


class PostgreSQLResult(object):
    """Na použitém backendu nezávislá reprezentace výsledku SQL příkazu.

    Předpokládá se předefinování této třídy v potomcích PostgreSQLAccessor dle
    potřeb konkrétního použitého backendu.

    """

    def __init__(self, data):
        """

        Argumenty:

          data -- datový objekt odpovídající výsledku; jedná-li se o sekvenci
            sekvencí stringů, fungují standardní metody v této třídě, v opačném
            případě je nutno tyto metody předefinovat

        Příklady standardních hodnot 'data':

          (('1', 'prvni'), ('2', 'druhy'), ('3', 'treti'))
          ()
          [['42']]

        """
        # Poznámka ke specifikaci: Reprezentace dat řetězci se může zdát
        # poněkud nevhodná, protože u některých rozhraní to může znamenat
        # konverzi dat na nějaký typ a pak zpět na řetězec.  To je ovšem cena,
        # kterou rádi zaplatíme za srozumitelnost celé záležitosti.  Problémem
        # není ani mrhání CPU cykly, protože kód poběží na klientech, kteří
        # se stejně vesměs flákají.
        self._data = data

    def __getitem__(self, row):
        """Vrať hodnotu výsledku z řádku 'row'.

        Návratovou hodnotou je reprezentace dat řádku jako indexovatelný objekt
        s hodnotami typu string odpovídajícími jednotlivým sloupcům výsledku.
        """
        return self._data[row]

    def __nonzero__(self):
        """Vrať True právě když objekt obsahuje nějaká data.
        """
        return len(self) > 0

    def __len__(self):
        """Vrať počet řádků dat.
        """
        return len(self._data)


class PostgreSQLAccessor(object_2_5):
    """Třída pro low-level přístup k PostgreSQL.

    Tato třída je zodpovědná za komunikaci s PostgreSQL realizovanou
    prostřednictvím konkrétní backendové knihovny.  Konkrétně má na starosti
    tyto věci: otevírání a uzavírání spojení do databáze, zasílání SQL příkazů
    databázovému stroji, převod výsledků SQL příkazů do obecné na použitém
    backendu nezávislé podoby.

    Přístup k PostgreSQL prostřednictvím konkrétního backendu se realizuje
    poděděním této třídy a předefinováním jejích metod.

    """

    class _postgresql_Connection(object):
        """Spojení do databáze.
        """
        _connection_set = WeakSet()

        def __init__(self, connection, connection_data):
            """

            Argumenty:

              connection -- spojení do databázového stroje
              connection_data -- specifikace parametrů spojení

            """
            self._connection_set.add(self)
            self._connection = connection
            self._connection_data = connection_data
            self._connection_info = {}

        def connection(self):
            return self._connection

        def connection_data(self):
            return self._connection_data

        def connection_info(self, key):
            return self._connection_info.get(key)

        def set_connection_info(self, key, value):
            self._connection_info[key] = value

        @classmethod
        def rollback_connections(class_, callback=None):
            for c in class_._connection_set:
                if callback is not None:
                    callback(c)
                try:
                    c._connection.rollback()
                except Exception:
                    pass

        @classmethod
        def close_idle_connections(class_, timeout):
            limit = time.time() - timeout
            for c in class_._connection_set:
                if c.connection_info('last_activity') <= limit:
                    c._connection.rollback()
                    PostgreSQLAccessor._postgresql_close_connection(c)

    class _postgresql_Result(object):
        """Výsledek SQL příkazu.
        """

        def __init__(self, result):
            """

            Argumenty:

              result -- výsledek SQL příkazu v podobě závislé na použitém
               backendu

            """
            self._result = result

        def result(self):
            return self._result

    def _postgresql_new_connection(self, connection_data):
        """Vytvoř, inicializuj a vrať nové spojení do databáze.

        Návratovou hodnotou je instance '_postgresql_Connection'.

        Argumenty:

          connection_data -- dictionary obsahující přihlašovací údaje jako
            stroj, port, uživatel, heslo, atd.

        """
        connection = self._postgresql_open_connection(connection_data)
        self._postgresql_initialize_connection(connection)
        return connection

    @classmethod
    def _postgresql_open_connection(class_, connection_data):
        """Vytvoř a vrať nové spojení do databáze.

        Návratovou hodnotou je instance '_postgresql_Connection'.

        Argumenty:

          connection_data -- dictionary obsahující přihlašovací údaje jako
            stroj, port, uživatel, heslo, atd.

        Tato metoda musí být předefinována v podtřídě.

        """
        raise ProgramError(_(u"Method not implemented"))

    @classmethod
    def _postgresql_close_connection(class_, connection):
        """Uzavři spojení do databáze.

        Argumenty:

          connection -- spojení, které má být uzavřeno, instance
            '_postgresql_Connection'

        V této třídě metoda nedělá nic.

        """
        pass

    def _postgresql_initialize_connection(self, connection):
        """Proveď potřebné inicializace nového spojení 'connection'.

        Pozor, tato metoda může být volána z jakékoliv instance třídy, nesmí
        tedy zde být nic specifického pro konkrétní instanci.

        """
        self._postgresql_initialize_transactions(connection)
        self._postgresql_initialize_coding(connection)
        self._postgresql_initialize_search_path(connection,
                                                self._pg_connection_data().schemas())
        self._postgresql_initialize_crypto(connection)
        self._postgresql_initialize_session_variables(connection)

    def _postgresql_initialize_transactions(self, connection):
        """Nastav způsob provádění transakcí pro konkrétní backend."""
        pass

    def _postgresql_initialize_coding(self, connection):
        # This queries are intentionally run without _pg_query_lock to avoid
        # deadlock on connection reopening in dbapi.py.
        query = _Query('set client_encoding to "utf-8"')
        self._postgresql_query(connection, query, False)
        query = _Query("select pg_encoding_to_char(encoding) "
                       "from pg_database where datname = current_database()")
        result = self._postgresql_query(connection, query, False)
        coding = result[0].result().fetchone()[0]
        if coding != 'UTF8':
            self._pg_encoding = coding

    def _postgresql_initialize_crypto(self, connection):
        import config
        password = config.dbconnection.crypto_password()
        if not password:
            return
        self._postgresql_query(connection, _Query("savepoint __pytis_init_crypto"), False)
        t, a = _Query.next_arg(sval(password))
        query = _Query("select pytis_crypto_unlock_current_user_passwords(%s)" % (t,), a)
        try:
            self._postgresql_query(connection, query, False)
        except DBUserException:
            self._postgresql_query(connection, _Query("rollback to __pytis_init_crypto"), False)
            # Prevent logging pytis_crypto_unlock_current_user_passwords
            # failures all the time:
            config.dbconnection.set_crypto_password(None)

    def _postgresql_initialize_search_path(self, connection, schemas):
        if schemas:
            search_path = string.join(schemas, ',')
            if connection.connection_info('search_path') != search_path:
                query = _Query("set search_path to " + search_path)
                self._postgresql_query(connection, query, False)
                connection.set_connection_info('search_path', search_path)

    def _postgresql_initialize_session_variables(self, connection):
        import config
        for k in config.session_variables:
            if len(k.split('.')) == 2:
                v = config.session_variables[k]
                if isinstance(v, basestring):
                    query = _Query("select set_config('{}', '{}', False)".format(k, v))
                    self._postgresql_query(connection, query, False)

    def _postgresql_query(self, connection, query, restartable):
        """Perform SQL 'query' and return the result.

        Arguments:

          connection -- '_postgresql_Connection' instance
          query -- '_Query' instance of the SQL command to be performed
          restartable -- iff this is true, the method may try to restart the
            database connection in case of error

        The return value is a pair ('result', 'connection'), where 'result' is
        a '_postgresql_Result' result and 'connection' a
        '_postgresql_Connection' of the connection that returned the result.

        This method is required to be redefined in a subclass.

        """
        raise ProgramError(_(u"Method not implemented"))

    def _postgresql_transform_query_result(self, result):
        """Vrať instanci 'PostgreSQLResult' odpovídající výsledku 'result'.

        Argumenty:

          result -- instance '_postgresql_Result'

        Tato metoda musí být předefinována v podtřídě.

        """
        raise ProgramError(_(u"Method not implemented"))

    def _postgresql_begin_transaction(self):
        self._pg_query(_Query('begin'))

    def _postgresql_commit_transaction(self):
        self._pg_query(_Query('commit'))

    def _postgresql_rollback_transaction(self):
        self._pg_query(_Query('rollback'))

    @classmethod
    def rollback_connections(class_, callback=None):
        """Rollback all database connections.

        Arguments:

          callback -- function of a single argument to be called for each of
            the rollbacked connections with the connection instance as its
            single argument; it's currently useful only for debugging

        """
        class_._postgresql_Connection.rollback_connections(callback)

    @classmethod
    def close_idle_connections(class_, timeout=60):
        """Close all inactive database connections.

        Arguments:

          timeout -- number of seconds defining the interval of connection
            inactivity to consider the connection inactive

        """
        class_._postgresql_Connection.close_idle_connections(timeout)


class PostgreSQLConnector(PostgreSQLAccessor):
    """Třída pro přístup k PostgreSQL na vyšší úrovni.

    Třída rozšiřuje funkce nadtřídy o funkce vyšší úrovně jako jsou správa
    spojení, SQL inicializace při otevírání spojení nebo zpracování výsledků
    SQL příkazů.

    """

    _pg_connection_pool_ = None

    def __init__(self, connection_data, connection_name=None, **kwargs):
        """
        Arguments:

          connection_data -- 'DBConnection' instance
          kwargs -- propagated to superclass constructors

        """
        import config
        self._pg_encoding = None
        # Logování
        if config.dblogtable:
            self._pdbb_logging_command = _QInsert(config.dblogtable, ('command',))
        else:
            self._pdbb_logging_command = None
        # Connection management
        # Connection pool open/close methods are class specific.  This doesn't
        # hurt now as we use only one database access class.  But it's not very
        # elegant anyway.
        if PostgreSQLConnector._pg_connection_pool_ is None:
            PostgreSQLConnector._pg_connection_pool_ = \
                DBConnectionPool(self._postgresql_new_connection,
                                 self._postgresql_close_connection)
        if isinstance(connection_data, DBConnection):
            def connection_data_function(connection_data=connection_data):
                return connection_data
            connection_data = connection_data_function
        assert connection_data is not None
        self._connection_name = connection_name
        self._pg_connection_data_ = connection_data
        self._pg_connections_ = []
        self._pg_query_lock = thread.allocate_lock()
        self._pg_query_counter = 0
        super(PostgreSQLConnector, self).__init__(connection_data=connection_data, **kwargs)

    def _pg_connection_pool(self):
        return PostgreSQLConnector._pg_connection_pool_

    def _pg_connection_data(self):
        return self._pg_connection_data_().select(self._connection_name)

    def _pg_connections(self):
        return self._pg_connections_

    def _pg_get_connection(self, outside_transaction=False):
        connections = self._pg_connections()
        if outside_transaction or not connections:
            pool = self._pg_connection_pool()
            connection = pool.get(self._pg_connection_data()), True
        else:
            connection = connections[-1], False
        return connection

    def _pg_return_connection(self, connection):
        pool = self._pg_connection_pool()
        pool.put_back(connection.connection_data(), connection)

    def _pg_query(self, query, outside_transaction=False, backup=False, transaction=None):
        """Call the SQL 'query' and return the result.

        Arguments:

          query -- '_Query' instance
          outside_transaction -- iff it is true, the query is performed outside
            the current transaction (if there is any)
          backup -- iff it is true, write the completed SQL command into log
          transaction -- transaction object containing the connection to be
            used for performing the query or 'None' (in which case the
            connection is selected automatically); this argument may not be
            used when 'outside_transaction' is true

        The return value is a 'PostgreSQLResult' instance.

        The method must properly handle database exception and in case any is
        caught the corresponding 'DBException' must be raised.

        """
        assert isinstance(query, _Query), query
        assert transaction is None or not outside_transaction, \
            'Connection given to a query to be performed outside transaction'
        borrowed_connection = None
        if transaction is None:
            connection, new = self._pg_get_connection(outside_transaction)
            if new:
                borrowed_connection = connection
        elif not transaction.open():
            raise DBUserException("Can't use closed transaction")
        else:
            connection = transaction._trans_connection()
        self._pg_query_counter += 1
        # Proveď dotaz
        if __debug__:
            log(DEBUG, 'SQL query', query.format())

        def lfunction(connection=connection):
            try:
                self._postgresql_initialize_search_path(connection,
                                                        self._pg_connection_data().schemas())
                result, connection = self._postgresql_query(connection, query, outside_transaction)
            finally:
                # Vrať DB spojení zpět
                if connection is not None and connection is borrowed_connection:
                    self._postgresql_query(connection, "commit", outside_transaction)
                    self._pg_return_connection(connection)
            if backup and self._pdbb_logging_command is not None:
                assert not outside_transaction, \
                    ('Backed up SQL command outside transaction', query.format())
                # Zde nemůže dojít k významné záměně pořadí zalogovaných
                # příkazů, protože všechny DML příkazy jsou uzavřeny
                # v transakcích a ty konfliktní jsou díky serializaci
                # automaticky správně řazeny.
                logging_query = self._pdbb_logging_command.values((query.format(),))
                self._postgresql_query(connection, logging_query, False)
            # Získej a vrať data
            return self._postgresql_transform_query_result(result)
        data = with_lock(self._pg_query_lock, lfunction)
        if __debug__:
            log(DEBUG, 'SQL query result', data)
        return data

    def _pg_flush_connections(self):
        self._pg_connection_pool().flush(self._postgresql_close_connection)

    def reset_crypto_password(self, password):
        """Set crypto password to 'password' in all database connections.

        This method closes all connections as a side effect so use it
        with caution.

        Arguments:

          password -- new crypto password; basestring

        """
        import config
        config.dbconnection.set_crypto_password(password)
        config.dbconnection = config.dbconnection  # mark as changed
        self._pg_flush_connections()


class PostgreSQLUserGroups(PostgreSQLConnector):
    """Třída pro zjišťování skupin uživatele."""

    _access_groups = {}
    _access_groups_data_objects = {}
    _logical_access_groups = UNDEFINED
    DMP_GROUPS_TABLE = 'ev_pytis_user_roles'

    def __init__(self, *args, **kwargs):
        super(PostgreSQLUserGroups, self).__init__(*args, **kwargs)
        key = self._pgg_connection_key(self._pg_connection_data())
        PostgreSQLUserGroups._access_groups_data_objects[key] = self

    def _postgresql_initialize_connection(self, connection):
        superclass = super(PostgreSQLUserGroups, self)
        superclass._postgresql_initialize_connection(connection)
        self._pgg_update_user_groups(connection)

    def _pgg_update_user_groups(self, connection):
        key = self._pgg_connection_key(connection.connection_data())
        try:
            del PostgreSQLUserGroups._access_groups[key]
        except KeyError:
            pass

    def _pgg_connection_key(self, connection_data):
        return (connection_data.user(), connection_data.host(), connection_data.port(),
                connection_data.database(),)

    def _pgg_retrieve_access_groups(self, data):
        if __debug__:
            log(DEBUG, "Retrieving list of user groups")
        q = _Query("select rolname from pg_roles where pg_has_role(rolname, 'member')")
        d = data._pg_query(q, outside_transaction=True)
        groups = [row[0] for row in d]
        if __debug__:
            log(DEBUG, "List of user groups retrieved")
        return groups

    def access_groups(self):
        """Vrať sekvenci jmen skupin, do kterých patří přihlášený uživatel.

        Nejsou-li skupiny uživatele známy, vrať 'None'.

        Argumenty:

          connection_data -- specifikace spojení, jehož skupiny mají být
            vráceny

        Sekvence jmen skupin je updatována při každém vytvoření nového
        spojení.  Jména skupin jsou strings.

        """
        connection_data = self._pg_connection_data()
        if PostgreSQLUserGroups._logical_access_groups is UNDEFINED:
            PostgreSQLUserGroups._logical_access_groups = None
            import config
            if config.use_dmp_roles:
                # Check for ev_pytis_user_roles presence first, to prevent logging
                # error messages in non-DMP applications
                tables = dbtable('information_schema.tables', ('table_name', 'table_schema'),
                                 connection_data, connection_name=self._connection_name)
                roles_data = None
                try:
                    n = tables.select(condition=EQ('table_name',
                                                   Value(String(),
                                                         PostgreSQLUserGroups.DMP_GROUPS_TABLE)))
                    schema_name = None
                    schemas = config.dbschemas
                    if not schemas:
                        schemas = 'public'
                    for i in range(n):
                        s_name = tables.fetchone()['table_schema'].value()
                        for s in schemas.split(','):
                            if s == s_name:
                                schema_name = s
                                break
                        if schema_name:
                            break
                    if schema_name:
                        try:
                            table_name = '{}.{}'.format(schema_name,
                                                        PostgreSQLUserGroups.DMP_GROUPS_TABLE)
                            roles_data = dbtable(table_name, ('roleid',), connection_data)
                        except DBException:
                            pass
                        else:
                            def process(row):
                                return row[0].value()
                            logical_access_groups = roles_data.select_map(process)
                            PostgreSQLUserGroups._logical_access_groups = logical_access_groups
                finally:
                    try:
                        tables.close()
                    except Exception:
                        pass
        if PostgreSQLUserGroups._logical_access_groups is None:
            key = self._pgg_connection_key(connection_data)
            groups = PostgreSQLUserGroups._access_groups.get(key, UNDEFINED)
            if groups is UNDEFINED:
                data = self._access_groups_data_objects[key]
                groups = PostgreSQLUserGroups._access_groups[key] = \
                    self._pgg_retrieve_access_groups(data)
        else:
            groups = PostgreSQLUserGroups._logical_access_groups
        return groups

    # TODO: Temporary compatibility hack:
    def class_access_groups(connection_data):
        import pytis.data
        return pytis.data.default_access_groups(connection_data)
    class_access_groups = staticmethod(class_access_groups)


class PostgreSQLNotifier(PostgreSQLConnector):
    """Class with notification about table contents changes.

    The class runs a thread watching for notification defined in the
    `_pg_notifications' attribute and sets the `_pg_changed' attribute to True
    whenever any of the given object gets changed and calls registered
    callbacks.

    """

    NOTIFIERS = {}

    class _PgNotifier(PostgreSQLConnector):

        # Jsou tu dva zámky -- pozor na uváznutí!

        def __init__(self, connection_data, connection_name=None):
            if __debug__:
                log(DEBUG, 'Notifier creation')
            PostgreSQLConnector.__init__(self, connection_data,
                                         connection_name=connection_name)
            self._notif_data_lock = thread.allocate_lock()
            self._notif_data_objects = weakref.WeakKeyDictionary()
            self._notif_connection_lock = thread.allocate_lock()
            thread.start_new_thread(self._notif_listen, ())

        def _notif_do_registration(self, notification):
            self._pg_query(_Query('listen "%s"' % notification))

        def _notif_register(self, notification):
            # Zamykáme zde kvůli možnosti současného vyvolání této metody
            # z `register' i naslouchacího threadu.
            if __debug__:
                log(DEBUG, 'Registering notification:', notification)

            def lfunction():
                self._notif_init_connection()
                self._notif_do_registration(notification)
            with_lock(self._notif_connection_lock, lfunction)
            if __debug__:
                log(DEBUG, 'Notification registered:', notification)

        def _notif_listen(self):
            if __debug__:
                log(DEBUG, 'Nový listener')
            error_pause = 1
            self._notif_init_connection()
            while True:
                if __debug__:
                    log(DEBUG, 'Listening on new connection')
                notiflist = []
                for d in self._notif_data_objects.values():
                    notiflist = notiflist + d
                if __debug__:
                    log(DEBUG, 'Notifications to register:', notiflist)
                notiflist = []
                for list_ in self._notif_data_objects.values():
                    notiflist += list_
                try:
                    # connection do poolu nikdy nevracíme, takže na něj můžeme
                    # navěsit, co je nám libo.
                    for n in remove_duplicates(notiflist):
                        self._notif_register(n)
                except DBException:
                    time.sleep(error_pause)
                    error_pause = error_pause * 2
                    continue
                self._notif_listen_loop()

        def _notif_listen_loop(self):
            raise Exception("Volána neimplementovaná metoda")

        def _notif_invoke_callbacks(self, notifications):
            if __debug__:
                log(DEBUG, 'Invoking callbacks')

            def lfunction():
                return copy.copy(self._notif_data_objects)
            data_objects = with_lock(self._notif_data_lock, lfunction)
            for d, ns in data_objects.items():
                for n in ns:
                    if n in notifications:
                        if __debug__:
                            log(DEBUG, 'Invoking data object callbacks:', d)
                        d._call_on_change_callbacks()
                        break

        def register_notification(self, data, notification):
            if __debug__:
                log(DEBUG, 'Registering notification:', notification)

            def lfunction():
                try:
                    notifications = self._notif_data_objects[data]
                except KeyError:
                    self._notif_data_objects[data] = notifications = []
                notifications.append(notification)
            with_lock(self._notif_data_lock, lfunction)
            self._notif_register(notification)
            if __debug__:
                log(DEBUG, 'Notification registered')

    def __init__(self, connection_data, **kwargs):
        """
        Argumenty:

          connection_data -- údaje o spojení, stejné jako ve třídě 'PostgreSQLConnector'
          kwargs -- k předání předkovi

        """
        self._pg_notifications = []
        super(PostgreSQLNotifier, self).__init__(connection_data=connection_data,
                                                 **kwargs)
        self._pg_changed = False

    def after_init(self):
        super(PostgreSQLNotifier, self).after_init()
        # Attention, notifications may be registered only after or other
        # initializations are performed.  Be careful about class successors!
        import config
        if config.dblisten:
            self._pg_add_notifications()

    def _call_on_change_callbacks(self):
        self._pg_changed = True
        super(PostgreSQLNotifier, self)._call_on_change_callbacks()

    def _pg_notifier_key(self, connection_data):
        d = connection_data
        return (d.host(), d.port(), d.database())

    def _pg_add_notifications(self):
        notifications = self._pg_notifications
        if not notifications:
            return
        spec = self._pg_connection_data()
        try:
            notifier = PostgreSQLNotifier.NOTIFIERS[spec]
        except KeyError:
            notifier = PostgreSQLNotifier.NOTIFIERS[spec] = \
                self._PgNotifier(spec, connection_name=self._connection_name)
        for n in notifications:
            notifier.register_notification(self, n)


class PostgreSQLStandardBindingHandler(PostgreSQLConnector, DBData):
    """Interpretace sémantiky specifikace napojení do databáze.

    Tato třída řeší problematiku naplnění významu specifikace napojení sloupců
    datové tabulky na data v databázi PostgreSQL.  Nedědí žádnou datovou
    třídu, pouze implementuje metody týkající se interpretace specifikace
    sloupců, je tudíž vhodná k podědění v některém z potomků 'data.DBData'.

    Současná implementace této třídy podporuje sloupcovou specifikační třídu
    'DBColumnBinding' a jen tuto třídu.  Pro bindings navíc platí následující
    pravidla:

    - Musí být specifikováno alespoň jedno binding.

    - Modifikovat (insert, update, delete) lze pouze tabulku klíče.

    - Všechny složky klíče musí být z téže tabulky.

    Poslední pravidlo se může zdát příliš omezující, avšak není tomu tak,
    protože práci s vícenásobnými tabulkami je lepší a jednodušší implementovat
    pomocí rules na serveru, než pomocí tohoto aplikačního rozhraní.  Je pouze
    zapotřebí, aby databázový stroj tuto funkcionalitu podporoval a aby tato
    podpora fungovala.

    **Pozor**: Metody modifikující tabulku se nestarají o obecné udržení
    integrity dat, tj. ohlídání vlastností klíčů nebo referenční integrity je
    ponecháno na databázovém stroji.  Předpokládá se, že v případě porušení
    pravidel definovaných v databázovém stroji je příslušná transakce
    stornována.  Stejně tak metoda 'delete' pracuje na tom principu, že vymaže
    řádek *pouze z tabulky primárního klíče* napojení; předpokládá se, že data
    v ostatních tabulkách budou smazána automaticky databázovým strojem v rámci
    pravidel zachování referenční integrity.

    """
    _PDBB_CURSOR_NAME = 'selection'

    _pdbb_selection_counter = Counter()
    _pdbb_selection_counter_lock = thread.allocate_lock()

    _pdbb_table_column_data = {}

    class _TableColumnData(object):

        def __init__(self, basic, default, unique):
            self._basic = basic
            self._default = default
            self._unique = unique

        def basic(self):
            return self._basic

        def default(self):
            return self._default

        def unique(self):
            return self._unique

    @classmethod
    def _pdbb_next_selection_number(class_):
        def lfunction():
            return class_._pdbb_selection_counter.next()
        return with_lock(class_._pdbb_selection_counter_lock, lfunction)

    def __init__(self, bindings=None, ordering=None, operations=None, column_groups=None,
                 db_spec=None, **kwargs):
        """
        Arguments:

          bindings, ordering, kwargs -- passed to superclass
          operations -- sequence of pairs (OPERATION, COLUMN, NAME), where
            OPERATION is one of 'AGG_*' constants of the class, COLUMN is id of
            the column binding corresponding to the column to be aggregated and
            NAME is the result column name (string) that can be used to refer to
            the aggregate column
          column_groups -- sequence of columns for the GROUP BY clause.  Each
            item is either a string identifier of an existing column or a tuple
            (NAME, TYPE, FUNCTION_NAME, *FUNCTION_ARGS), where NAME is the
            string identifier of the column, TYPE is a 'pytis.data.Type'
            instance of the resulting column type, FUNCTION_NAME is a string
            containing the name of the database function returning the value
            used for grouping, and FUNCTION_ARGS are arguments of the function.
            Each of FUNCTION_ARGS can be either a string denoting another table
            column name or a 'Value' instance denoting particular value of the
            argument.
          db_spec -- corresponding database specification as '_SQLTabular'
            instance or 'None'; if not 'None' then database introspection can
            be omitted for this object

        If 'column_groups' is not 'None' it defines the set of grouped columns
        as in the GROUP BY part of an SQL select statement.  If 'operations' is
        not 'None', it defines a set of columns to be used as given aggregates
        in SQL select statements.

        If any of 'operations' and 'column_groups' is not 'None', all columns
        not specified in 'operations' nor 'column_groups' are excluded from all
        select operations.  This especially concerns the key column(which is
        typically not present when using groupings), without its presence it's
        impossible to perform data manipulation and other operations(in some
        cases you can use the virtual column named '_number' containing the
        current row number).  Additionally note that 'condition' constructor
        argument, unlike 'condition' argument in 'select' calls, applies to the
        base non-aggregated columns of the table thus allowing you to filter
        the underlying data rows before aggregations get applied.

        """
        self._pdbb_db_spec = db_spec
        self._pdbb_table_schemas = {}
        self._pdbb_function_schemas = {}
        super(PostgreSQLStandardBindingHandler, self).__init__(
            bindings=bindings, ordering=ordering, **kwargs)
        self._pdbb_operations = operations
        self._pdbb_column_groups = []
        if column_groups is None:
            self._pdbb_column_groups = None
        else:
            for c in column_groups:
                if isinstance(c, basestring):
                    c = (c, None, None)
                self._pdbb_column_groups.append(c)
        self._pdbb_create_sql_commands()

    def _pdbb_tabcol(self, table_name, column_name, column_id):
        """Vrať zadaný sloupec zformátovaný pro SQL."""
        if table_name:
            result = '%s.%s' % (table_name, column_name)
        else:                           # aggregate or so, alias must be used
            result = column_id
        return result

    def _pdbb_btabcol(self, binding, full_text_handler=None, convert_ltree=False, operations=None,
                      column_groups=None):
        """Vrať sloupec z 'binding' zformátovaný pro SQL."""
        def column_type():
            return self.find_column(binding.id()).type()
        result = None
        if operations is not None:
            for aggregate, id_, name in operations:
                if name == binding.id():
                    result = _QFunction(self._pg_aggregate_name(aggregate), (binding.column(),))
                    result = result.label(name)
                    break
        if column_groups is not None:
            for g in column_groups:
                name = g[0]
                if name == binding.id():
                    function_name = g[2]
                    if function_name is not None:
                        result = self._pdbb_column_group_call(g).label(name)
                    break
        if result is None:
            column_name = binding.column()
            column_id = binding.id()
            if full_text_handler is not None and isinstance(binding.type(), FullTextIndex):
                result = full_text_handler(binding)
            elif convert_ltree and isinstance(column_type(), LTree) and column_type().text():
                result = self._pdbb_tabcol(binding.table(), column_name, column_id) + '::text'
            else:
                result = self._pdbb_tabcol(binding.table(), column_name, column_id)
                crypto_name = binding.crypto_name()
                if crypto_name is not None:
                    btype = binding.type()
                    if btype is None:
                        raise Exception("Unknown type in crypto column binding", binding)
                    # At least when called from Wiking, btype may be a type class
                    # (and not a Type instance).  Hmm.
                    if ((isinstance(btype, String) or
                         isinstance(btype, type) and issubclass(btype, String))):
                        decryption_function = 'pytis_decrypt_text'
                    elif (isinstance(btype, Float) or
                          isinstance(btype, type) and issubclass(btype, Float)):
                        decryption_function = 'pytis_decrypt_float'
                    elif (isinstance(btype, Integer) or
                          isinstance(btype, type) and issubclass(btype, Integer)):
                        decryption_function = 'pytis_decrypt_int'
                    elif (isinstance(btype, Binary) or
                          isinstance(btype, type) and issubclass(btype, Binary)):
                        decryption_function = 'pytis_decrypt_binary'
                    else:
                        raise Exception("Encryption support not available for the type", btype)
                    result = _QFunction(decryption_function, (result, sval(crypto_name),))
        if isinstance(result, basestring):
            result = _Query(result)
        return result

    def _pdbb_coalesce(self, ctype, value):
        if ctype is None or isinstance(ctype, (String, Range,)) or not value:
            cast = ''
        elif isinstance(ctype, Float):
            cast = 'numeric'
        elif isinstance(ctype, Number):
            cast = ''
        elif isinstance(ctype, Time):
            cast = 'time' if ctype.without_timezone() else 'timetz'
        elif isinstance(ctype, Date):
            cast = 'date'
        elif isinstance(ctype, DateTime):
            cast = 'timestamp' if ctype.without_timezone() else 'timestamptz'
        elif isinstance(ctype, Boolean):
            cast = 'bool'
        else:
            cast = ''
        return _PgValue(value).query().cast(cast)

    def _pdbb_split_object_name(self, obj, schema_dict, object_table, object_label):
        items = obj.split('.')
        if len(items) < 2:
            if schema_dict.get(obj) is None:
                table_schema = None
                if table_schema is None:
                    schemas = self._pg_connection_data().schemas()
                    if not schemas:
                        schemas = ['public', 'pg_catalog']
                    if len(schemas) == 1:
                        table_schema = schemas[0]
                    elif self._pdbb_db_spec is not None:
                        for s in self._pdbb_db_spec.object_schemas():
                            if s in schemas:
                                table_schema = s
                                break
                        else:
                            table_schema = 'public'
                    else:
                        for s in schemas:
                            query = (("select %(table)s.%(label)sname from %(table)s, pg_namespace "
                                      "where %(table)s.%(label)snamespace = pg_namespace.oid and "
                                      "%(table)s.%(label)sname='%(name)s' and "
                                      "pg_namespace.nspname='%(namespace)s'") %
                                     dict(table=object_table, label=object_label, name=obj,
                                          namespace=s))
                            if self._pg_query(_Query(query), outside_transaction=True):
                                table_schema = s
                                break
                        else:
                            table_schema = 'public'
                schema_dict[obj] = table_schema
            items.insert(0, schema_dict[obj])
        return items

    def _pdbb_split_table_name(self, table):
        return self._pdbb_split_object_name(table, self._pdbb_table_schemas, 'pg_class', 'rel')

    def _pdbb_split_function_name(self, table):
        return self._pdbb_split_object_name(table, self._pdbb_function_schemas, 'pg_proc', 'pro')

    def _pdbb_unique_table_id(self, table):
        connection_data = self._pg_connection_data()
        return table, connection_data.host(), connection_data.port(), connection_data.database()

    def _pdbb_get_table_column_data(self, table):
        schema, table_name = self._pdbb_split_table_name(table)
        d = self._pg_query(_Query(
            ("select pg_attribute.attname, pg_type.typname, pg_attribute.atttypmod, "
             "pg_attribute.attnotnull "
             "from pg_class, pg_attribute, pg_type, pg_namespace "
             "where pg_class.oid = pg_attribute.attrelid and "
             "pg_class.relnamespace = pg_namespace.oid and "
             "pg_namespace.nspname = '%s' and "
             "pg_class.relname = '%s' and "
             "pg_attribute.atttypid = pg_type.oid and "
             "pg_attribute.attnum > 0") %
            (schema, table_name,)),
            outside_transaction=True)
        d1 = self._pg_query(_Query(
            ("select pg_attribute.attname, pg_attrdef.adsrc "
             "from pg_class, pg_attribute, pg_attrdef, pg_namespace "
             "where pg_class.oid = pg_attrdef.adrelid and "
             "pg_class.relnamespace = pg_namespace.oid and "
             "pg_namespace.nspname = '%s' and "
             "pg_class.oid = pg_attribute.attrelid and "
             "pg_class.relname = '%s' and "
             "pg_attribute.attnum = pg_attrdef.adnum and "
             "pg_attribute.attnum > 0") %
            (schema, table_name,)),
            outside_transaction=True)
        d2 = self._pg_query(_Query(
            ("select attname, conkey "
             "from pg_constraint, pg_namespace, pg_class, pg_attribute "
             "where conrelid = pg_class.oid and attrelid = pg_class.oid and "
             "relnamespace = pg_namespace.oid and attnum = any (conkey) and "
             "nspname = '%s' and relname = '%s' and (contype = 'p' or contype = 'u') and "
             "pg_attribute.attnum > 0") %
            (schema, table_name,)),
            outside_transaction=True)
        table_data = self._TableColumnData(d, d1, d2)
        table_key = self._pdbb_unique_table_id(table)
        PostgreSQLStandardBindingHandler._pdbb_table_column_data[table_key] = table_data
        return table_data

    def _pdbb_get_table_type(self, table, column, noerror=False):
        if self._pdbb_db_spec is not None:
            for b in self._bindings:
                if b.id() == column:
                    return b.type()
        table_key = self._pdbb_unique_table_id(table)
        table_data = PostgreSQLStandardBindingHandler._pdbb_table_column_data.get(table_key)
        if table_data is None:
            table_data = self._pdbb_get_table_column_data(table)

        def lookup_column(data):
            if isinstance(column, int):
                row = data[column]
            else:
                for row in data:
                    if row[0] == column:
                        break
                else:
                    return None
            return row[1:]
        try:
            type_, size_string, not_null = lookup_column(table_data.basic())
        except Exception:
            if noerror:
                return None
            raise DBException("Unknown column '%s' in table '%s'" % (column, table),
                              None, table, column)
        try:
            default = lookup_column(table_data.default())[0]
        except Exception:
            default = ''
        try:
            # TODO: This is a quick hack to ignore multicolumn unique constraints. (TC)
            row = lookup_column(table_data.unique())
            unique = row and len(row[0]) == 1
        except Exception:
            unique = False
        serial = (default[:len('nextval')] == 'nextval')
        return self._pdbb_get_type(type_, size_string, not_null=not_null,
                                   serial=serial, unique=unique)

    def _pdbb_get_type(self, type_, size_string, not_null=False, serial=False, unique=False):
        # Zde lze doplnit další používané standardní typy z PostgreSQL
        TYPE_MAPPING = {'bool': Boolean,
                        'bpchar': String,
                        'char': String,
                        'date': Date,
                        'time': Time,
                        'timetz': Time,
                        'smallint': Integer,
                        'bigint': Integer,
                        'int2': Integer,
                        'int4': Integer,
                        'int4range': pytis.data.IntegerRange,
                        'int8': pytis.data.LargeInteger,
                        'int8range': pytis.data.LargeIntegerRange,
                        'numeric': Float,
                        'float4': Float,
                        'float8': Float,
                        'name': String,
                        'text': String,
                        'timestamp': DateTime,
                        'timestamptz': DateTime,
                        'interval': TimeInterval,
                        'tsrange': pytis.data.DateTimeRange,
                        'tstzrange': pytis.data.DateTimeRange,
                        'daterange': pytis.data.DateRange,
                        'tsvector': FullTextIndex,
                        'varchar': String,
                        'ltree': LTree,
                        'inet': Inet,
                        'macaddr': Macaddr,
                        'bytea': Binary,
                        'oid': pytis.data.Oid,  # for backward compatibility
                        'sql_identifier': String,
                        }
        if type_ and type_[0] == '_':
            array = True
            type_ = type_[1:]
        else:
            array = False
        try:
            db_type_cls = TYPE_MAPPING[type_]
        except KeyError:
            raise pytis.data.DBException('Unhandled database type', None, type_)
        db_type_kwargs = {}
        if not_null in (1, 'T'):
            db_type_kwargs['not_null'] = True
        if unique and db_type_cls != Boolean:
            db_type_kwargs['unique'] = True
        if db_type_cls is String:
            if type_ != 'text':
                try:
                    size = int(size_string) - 4
                except Exception:
                    size = None
                if size < 0:
                    size = None
                db_type_kwargs['maxlen'] = size
        elif db_type_cls is Float:
            if type_ == 'numeric':
                spec = int(size_string)
                precision = (spec & 0xFFFF) - 4
                if precision >= 0 and precision <= 100:
                    db_type_kwargs['precision'] = precision
                else:
                    db_type_kwargs['digits'] = 100
        elif db_type_cls is Integer and serial:
            db_type_cls = Serial
        elif type_ in ('timestamp', 'time', 'tsrange',):
            db_type_kwargs['without_timezone'] = True
        if array:
            db_type_kwargs['inner_type'] = db_type_cls()
            db_type_cls = pytis.data.Array
        return db_type_cls(**db_type_kwargs)

    def _pdbb_apply_type_kwargs(self, ctype, binding):
        btype = binding.type()
        kwargs = binding.kwargs()
        if btype:
            if not isinstance(btype, pytis.data.Type):
                btype = btype()
            if ctype is None or ctype.__class__ == Binary and not isinstance(btype, Binary):
                # Maybe a crypto column
                ctype = btype
            else:
                assert (isinstance(btype, ctype.__class__) or
                        isinstance(btype, TimeInterval) and  # temporary hack
                        ctype.__class__ == Time), \
                    "%s.%s: User type doesn't match DB type: %s, %s" % \
                    (binding.table(), binding.column(), btype, ctype)
                ctype = ctype.clone(btype)
        if ctype and kwargs:
            if isinstance(ctype, pytis.data.Array) and 'inner_type' not in kwargs:
                # This argument is mandatory for Array type.
                kwargs = dict(kwargs, inner_type=ctype.inner_type())
            ctype = ctype.clone(ctype.__class__(**kwargs))
        return ctype

    def _db_bindings_to_column_spec(self, bindings):
        key = []
        columns = []
        do_introspection = (
            # No introspection needed when DB specification is available.
            self._pdbb_db_spec is None and
            # Introspection not possible for table functions.
            self._arguments is None
        )
        for b in bindings:
            if not b.id():              # skrytý sloupec
                continue
            if do_introspection:
                table_type = self._pdbb_get_table_type(b.table(), b.column())
            else:
                table_type = None
            if b.type() is None and self._arguments is not None:
                raise Exception("Column types must be specified for table functions",
                                b.id(), b.table(),)
            t = self._pdbb_apply_type_kwargs(table_type, b)
            colspec = ColumnSpec(b.id(), t)
            columns.append(colspec)
            if b in self._key_binding:
                assert not isinstance(t, Binary), "Binary types may not be used as keys"
                key.append(colspec)
        assert key, DBUserException('data key column not found')
        # Hotovo
        return columns, tuple(key)

    def _pdbb_sql_column_list_from_names(self, column_names, full_text_handler=None,
                                         operations=None, column_groups=None):
        bindings = [self._db_column_binding(name) for name in column_names]
        if column_groups:
            column_groups = [g for g in column_groups if g[0] in column_names]
        return self._pdbb_sql_column_list(bindings, full_text_handler, operations=operations,
                                          column_groups=column_groups)

    def _pdbb_sql_column_list(self, bindings, full_text_handler=None, operations=None,
                              column_groups=None):
        column_names = [self._pdbb_btabcol(b, full_text_handler=full_text_handler,
                                           operations=operations,
                                           column_groups=column_groups)
                        for b in bindings if b is not None and b.id()]
        return _Query.join(column_names)

    def _pdbb_full_text_handler(self, binding):
        indexed_columns = binding.type().columns()
        if indexed_columns:
            indexed_columns_list = []
            for name in indexed_columns:
                sql_name = self._pdbb_btabcol(self._db_column_binding(name))
                indexed_columns_list.append(_QFunction('coalesce', (sql_name, sval(''),)))
            text = _Query.join(indexed_columns_list, "||' * '||")
            query_name = self._pdbb_fulltext_query_name(binding.column())
            result = _QFunction('ts_headline', (text, query_name,))
        else:
            result = _Query("''")
        return result

    def _pdbb_column_group_call(self, group):
        args = _Query.join(group[3:])
        return args.wrap(group[2])

    def _pdbb_create_sql_commands(self):
        """Vytvoř šablony SQL příkazů používané ve veřejných metodách."""
        bindings = self._bindings
        for b in bindings:
            assert isinstance(b, DBColumnBinding), ('Unsupported binding specification', b)
        # Připrav parametry
        operations = (self._pdbb_operations or [])
        aggregate_columns = [o[2] for o in operations]
        group_columns = []
        function_column_groups = []
        for g in (self._pdbb_column_groups or []):
            if g[2] is None:
                group_columns.append(g[0])
            else:
                function_column_groups.append(g)
        if self._pdbb_column_groups is None and self._pdbb_operations is None:
            filtered_bindings = bindings
        else:
            filtered_bindings = []
            for b in bindings:
                if b.id() in group_columns:
                    filtered_bindings.append(b)
            for aggregate, id_, name in operations:
                for b in bindings:
                    if id_ == b.id():
                        assert name not in [b_.id() for b_ in bindings], \
                            ('Duplicate column name', name,)
                        if aggregate == self.AGG_COUNT:
                            type_ = Integer()
                        elif aggregate == self.AGG_AVG:
                            type_ = Float()
                        else:
                            type_ = self.find_column(id_).type()
                            assert type_ is not None
                        cb = DBColumnBinding(name, '', b.column(), type_=type_)
                        self._bindings = bindings = bindings + (cb,)
                        filtered_bindings.append(cb)
                        self._columns = self._columns + (ColumnSpec(name, type_),)
            for g in function_column_groups:
                name, type_ = g[0], g[1]
                self._columns = self._columns + (ColumnSpec(name, type_),)
                cb = DBColumnBinding(name, '', name, type_=type_)
                self._bindings = bindings = bindings + (cb,)
                filtered_bindings.append(cb)
        self._pdbb_filtered_bindings = filtered_bindings
        column_list = self._pdbb_sql_column_list(filtered_bindings,
                                                 full_text_handler=self._pdbb_full_text_handler,
                                                 operations=self._pdbb_operations,
                                                 column_groups=self._pdbb_column_groups)
        assert column_list, ('No columns present', [b.id() for b in bindings], group_columns,)
        if self._pdbb_column_groups:
            groupby_columns = list(self._distinct_on or [])
            for b in bindings:
                if b.id() in group_columns:
                    groupby_columns.append(self._pdbb_btabcol(b))
            for g in self._pdbb_column_groups:
                if g[2] is not None:
                    c = g[0]
                    assert c not in groupby_columns, ("group column duplicate", g,)
                    groupby_columns.append(self._pdbb_column_group_call(g))
            groupby = _Query('group by ') + _Query.join(groupby_columns, ', ')
        else:
            groupby = _Query('')
        table_names = [b.table() for b in bindings if b.table()]
        table_names = remove_duplicates(table_names)
        if self._arguments is not None:
            assert len(table_names) == 1, "Only single tables supported for table functions"
            table_list = _QFunction(table_names[0])
            table_expressions = [table_list]
            self._arguments_arg = table_expressions[0].values_arg()
        else:
            table_expressions = table_names
            table_list = _Query.join(table_expressions)
        if len(table_expressions) <= 1:
            relation = _Query('true')
        else:
            rels = [self._pdbb_btabcol(b) + '=' + self._pdbb_btabcol(b.related_to())
                    for b in bindings if b.related_to()]
            relation = _Query.join(rels, ' and ')
        main_table = self._key_binding[0].table()
        schema, main_table_name = self._pdbb_split_table_name(main_table)
        if self._arguments is None:
            main_table_from = main_table
        else:
            main_table_from = table_expressions[0]
        if isinstance(main_table_from, basestring):
            main_table_from = _Query(main_table_from)
        keytabcols = [self._pdbb_btabcol(b) for b in self._key_binding]
        assert len(keytabcols) == 1, ('Multicolumn keys no longer supported', keytabcols)
        first_key_column = keytabcols[0]
        key_cond = _Query('%(key_column)s=%(key)s', dict(key_column=first_key_column))
        if self._distinct_on:
            distinct_columns_string = self._pdbb_sql_column_list_from_names(self._distinct_on)
            distinct_on = _Query(' DISTINCT ON (') + distinct_columns_string + ')'
            distinct_on_ordering = distinct_columns_string + ', '
        else:
            distinct_on = _Query('')
            distinct_on_ordering = _Query('')
        sort_exclude = aggregate_columns + [g[0] for g in function_column_groups]

        def sortspec(direction):
            items = []
            bindings = [b for b in self._key_binding
                        if b in filtered_bindings and b.id() not in aggregate_columns]
            if not bindings:
                bindings = filtered_bindings
            for b in bindings:
                if b.id() not in sort_exclude:
                    items.append(self._pdbb_btabcol(b, convert_ltree=True) + ' ' + direction)
            for g in function_column_groups:
                items.append(self._pdbb_column_group_call(g) + ' ' + direction)
            # TODO: items may still be empty (if only aggregates are present in the result columns)
            return _Query.join(items)
        ordering = sortspec('ASC')
        rordering = sortspec('DESC')
        condition = key_cond
        relation_and_condition = _Query.join((relation.wrap(), condition.wrap(),), ' and ')
        if self._condition is None:
            filter_condition = _Query('true')
        else:
            filter_condition = self._pdbb_condition2sql(self._condition).wrap()

        def make_lock_command():
            if self._pdbb_db_spec is not None:
                from pytis.data.gensqlalchemy import SQLTable
                if issubclass(self._pdbb_db_spec, SQLTable):
                    return ''
            if self._pdbb_db_spec is None:
                qresult = self._pg_query(_Query(
                    (("select relkind from pg_class join pg_namespace "
                      "on (pg_class.relnamespace = pg_namespace.oid) "
                      "where nspname='%s' and relname='%s'") %
                     (schema, main_table_name,))),
                    outside_transaction=True)
                if qresult[0][0] == 'r':
                    return ''
            qresult = self._pg_query(_Query(
                ("select definition from pg_views where schemaname = '%s' and viewname = '%s'" %
                 (schema, main_table_name,))),
                outside_transaction=True)
            assert len(qresult) == 1, (schema, main_table_name,)
            lock_query = qresult[0][0]
            if lock_query[-1] == ';':
                lock_query = lock_query[:-1]
            lock_query = lock_query.replace('%', '%%')
            lock_query = _Query(lock_query)
            # There are some issues with locking views:
            # - There is a PostgreSQL bug preventing locking views which are
            #   built on top of other views.
            # - It's not possible to lock views using LEFT OUTER JOIN (this is
            #   a PostgreSQL feature).
            # Both the problems can be solved by using FOR UPDATE OF version of
            # the locking clause.  But first we need to know what may be put
            # after OF without causing a database error.
            qresult = self._pg_query(
                _Query("select ev_action from "
                       "pg_rewrite join pg_class on (pg_rewrite.ev_class = pg_class.oid) "
                       "join pg_namespace on (pg_class.relnamespace = pg_namespace.oid) "
                       "where nspname=%(schema)s and relname=%(main_table)s",
                       dict(schema=sval(schema), main_table=sval(main_table))),
                outside_transaction=True)
            ev_action_string = qresult[0][0]
            ev_action = evaction.pg_parse_ev_action(ev_action_string)
            ev_rtable = ev_action[0]['rtable']
            lock_candidates = [table['eref']['aliasname']
                               for table in ev_rtable if table['inFromCl']]

            def check_candidate(candidate_table):
                try:
                    self._pg_query(lock_query + (" for update of %s nowait limit 1" %
                                                 (candidate_table,)),
                                   outside_transaction=True)
                    return True
                except DBLockException:
                    return True
                except DBUserException:
                    return False

            lock_tables = [c for c in lock_candidates if check_candidate(c)]

            def find_real_key():
                keyname = first_key_column.template().split('.')[-1]
                for colspec in ev_action[0]['targetList']:
                    if colspec['resname'] == keyname:
                        break
                else:
                    return None
                table = colspec['resorigtbl']
                column = colspec['resorigcol']
                qresult = self._pg_query(_Query(("select relname, attname from pg_class join "
                                                 "pg_attribute on (attrelid=pg_class.oid) "
                                                 "where pg_class.oid=%s and "
                                                 "pg_attribute.attnum=%s") % (table, column,)),
                                         outside_transaction=True)
                if qresult:
                    relname, attname = qresult[0]
                    if relname not in lock_tables:
                        return None
                else:
                    return None
                return relname, attname
            if lock_tables:
                real_key = find_real_key()
            else:
                real_key = None
            if real_key:
                key_table_name, key_column_name = real_key
                lock_tables_string = string.join(lock_tables, ', ')
                limit_clause = '(%s.%s=%%(key)s)' % (key_table_name, key_column_name,)
                # Stupid and incorrect, but how to make it better?
                t = lock_query.template()
                matches = [m for m in re.finditer(' where ', t, re.I)]
                if matches:
                    match = matches[-1]
                else:
                    match = None
                if match:
                    beg, end = match.span()
                    n = 0
                    for char in t[end:]:
                        if char == '(':
                            n = n - 1
                        elif char == ')':
                            n = n + 1
                    if n > 0:
                        match = None
                if match:
                    lock_query = _Query(t[:beg] + ' where ' + limit_clause + ' and ' + t[end:])
                else:
                    lock_query = lock_query + ' where ' + limit_clause
                result = lock_query + " for update of %s nowait" % (lock_tables_string,)
            else:
                log(EVENT, "Unlockable view, won't be locked:", main_table)
                result = lock_query + ' limit 1'
            return result
        self._pdbb_command_lock = make_lock_command
        # We make all cursors names unique to avoid conflicts with codebooks
        # when using cross-class transactions and additionally to avoid
        # conflicts when using data instance cache.
        cursor_name = '%s_%%(selection)s' % (self._PDBB_CURSOR_NAME,)
        # Vytvoř šablony příkazů
        args = dict(columns=column_list, supplement=_Query(''), condition=_Query('true'),
                    groupby=groupby, relation=relation_and_condition,
                    filter_condition=filter_condition, tables=table_list, ordering=ordering)
        template = ('select %(columns)s from %(tables)s '
                    'where %(relation)s and %(filter_condition)s '
                    '%(groupby)s order by %(ordering)s %(supplement)s')
        self._pdbb_command_row = _Query(template, args)
        self._pdbb_command_distinct = _Query(
            "select distinct %(expression)s from %(tables)s "
            "where %(condition)s and (%(relation)s) and %(filter_condition)s "
            "order by %(sorting)s",
            dict(tables=table_list, relation=relation, filter_condition=filter_condition))
        self._pdbb_command_fetch_last = _Query('fetch last from %s' % (cursor_name,))
        self._pdbb_command_move_to_start = _Query('move absolute 0 from %s' % (cursor_name,))
        query = _Query(('declare %s scroll cursor for '
                        'select __pytis_select.*, row_number() over () as _number from '
                        '(%%(inner_query)s %%(limit)s) __pytis_select') % (cursor_name,))
        args = dict(columns=column_list, relation=relation,
                    filter_condition=filter_condition,
                    table=table_list, std_ordering=ordering, groupby=groupby,
                    distinct=distinct_on, distinct_ordering=distinct_on_ordering)
        if self._pdbb_operations:
            inner_query = _Query(
                ('select * '
                 'from (select%%(distinct)s %%(columns)s from %%(table)s '
                 'where (%%(relation)s) and %%(filter_condition)s %%(groupby)s '
                 'order by %%(distinct_ordering)s%%(ordering)s %%(std_ordering)s) '
                 'as %s %%(fulltext_queries)s where %%(condition)s') % (table_names[0],),
                args)
        elif distinct_on:
            inner_query = _Query(
                ('select %%(columns)s '
                 "from (select%%(distinct)s * from %%(table)s%%(fulltext_queries)s "
                 "where %%(condition)s and (%%(relation)s) and %%(filter_condition)s) "
                 "as %s %%(groupby)s order by %%(ordering)s %%(std_ordering)s") % (table_names[0],),
                args)
        else:
            inner_query = _Query(
                ('select %(columns)s '
                 "from %(table)s%(fulltext_queries)s "
                 "where %(condition)s and (%(relation)s) and %(filter_condition)s %(groupby)s "
                 "order by %(distinct_ordering)s%(ordering)s %(std_ordering)s"),
                args)
        self._pdbb_command_select = query % dict(inner_query=inner_query)
        self._pdbb_command_dummy_select = _Query("declare %s scroll cursor for select 1 where false"
                                                 % (cursor_name,))
        self._pdbb_command_close_select = _Query('close %s' % (cursor_name,))
        args = dict(relation=relation, filter_condition=filter_condition, tables=table_list,
                    inner_columns=column_list, distinct=distinct_on, groupby=groupby)
        if self._pdbb_operations:
            self._pdbb_command_select_agg = _Query(
                ('select%%(distinct)s %%(columns)s from '
                 '(select %%(inner_columns)s from %%(tables)s '
                 'where true and %%(filter_condition)s %%(groupby)s) as %s '
                 'where %%(condition)s and (%%(relation)s)') % (table_names[0],),
                args)
        else:
            self._pdbb_command_select_agg = _Query(
                'select%(distinct)s %(columns)s from %(tables)s '
                'where %(condition)s and (%(relation)s) and %(filter_condition)s',
                args)
        self._pdbb_command_fetch_forward = _Query('fetch forward %%(number)s from %s' %
                                                  (cursor_name,))
        self._pdbb_command_fetch_backward = _Query('fetch backward %%(number)s from %s' %
                                                   (cursor_name,))
        self._pdbb_command_move_forward = _Query('move forward %%(number)s from %s' %
                                                 (cursor_name,))
        self._pdbb_command_move_backward = _Query('move backward %%(number)s from %s' %
                                                  (cursor_name,))
        self._pdbb_command_move_absolute = _Query('move absolute %%(number)s from %s' %
                                                  (cursor_name,))
        query = _Query(('select %(columns)s from %(main_table)s '
                        'where (%(relation)s) and %(filter_condition)s and %(condition)s '
                        '%(groupby)s order by %(ordering)s %(search_ordering)s limit 1'),
                       dict(columns=column_list, relation=relation, groupby=groupby,
                            filter_condition=filter_condition, main_table=main_table_from))
        self._pdbb_command_search_first = query % dict(search_ordering=ordering)
        self._pdbb_command_search_last = query % dict(search_ordering=rordering)
        args = dict(relation=relation, filter_condition=filter_condition, columns=column_list,
                    key_column=first_key_column, main_table=main_table_from, groupby=groupby,
                    distinct=distinct_on, tables=table_list)
        if self._pdbb_operations:
            self._pdbb_command_search_distance = _Query(
                ('select count(*) from '
                 '(select%%(distinct)s * from '
                 '(select %%(columns)s from %%(tables)s '
                 'where (%%(relation)s) and %%(filter_condition)s %%(groupby)s) as %s'
                 ' where %%(condition)s) as __count') % (table_names[0],),
                args)
        elif distinct_on:
            self._pdbb_command_search_distance = _Query(
                ('select count(*) from (select%%(distinct)s * from %%(main_table)s '
                 'where (%%(relation)s) and %%(filter_condition)s and %%(condition)s) as %s') %
                (table_names[0]),
                args)
        else:
            self._pdbb_command_search_distance = _Query(
                'select count(%(key_column)s) from %(main_table)s '
                'where (%(relation)s) and %(filter_condition)s and %(condition)s',
                args)
        self._pdbb_command_insert = _Query(
            'insert into %s (%%(columns)s) values (%%(values)s) returning %%(key_column)s' %
            (main_table,),
            args)
        self._pdbb_command_insert_alternative = _Query(
            'insert into %s (%%(columns)s) values (%%(values)s)' % (main_table,))
        self._pdbb_command_insert_get_last = _Query(
            'select %%(key_column)s from %s order by %%(key_column)s desc limit 1' % (main_table,),
            args)
        if self._ordering:
            ordering = []
            for o in self._ordering:
                for b in self._bindings:
                    if b.id() == o:
                        ordering.append(b.column())
                        break
                else:
                    raise ProgramError('Invalid ordering id', o)
            ocol = ordering[0]
            eqs = []
            for i in range(1, len(ordering)):
                eqs.append('%s=%%(param_%d)s' % (ordering[i], i,))
            if eqs:
                eqstring = ' AND '.join(eqs)
                xeqstring = ' AND ' + eqstring
            else:
                eqstring = xeqstring = ''
            self._pdbb_command_insert_shift = _Query(
                'update %s set %s=%s+1 where %s>=%%(position)s %s' %
                (main_table, ocol, ocol, ocol, xeqstring))
            self._pdbb_command_insert_newpos = _Query(
                'select max(%s) from %s where %s' % (ocol, main_table, eqstring))
        update_from_tables = [t for t in table_names if t != main_table]
        if update_from_tables:
            update_from_clause = (' from ' +
                                  string.join(update_from_tables, ', '))
        else:
            update_from_clause = ''
        args = dict(relation=relation)
        self._pdbb_command_update = _Query(
            'update %s set %%(settings)s%s where (%%(relation)s) and (%%(condition)s)' %
            (main_table, update_from_clause),
            args)
        args['key_column'] = first_key_column
        self._pdbb_command_broken_update_preselect = _Query(
            'select count (%%(key_column)s) from %s where (%%(relation)s) and (%%(condition)s)' %
            (main_table),
            args)
        self._pdbb_command_test_broken_update = _Query(
            ("select 'yes' from pg_class, pg_namespace, pg_rewrite "
             "where pg_rewrite.ev_type = '2' and "
             "pg_rewrite.is_instead = 't' and "
             "pg_class.oid = pg_rewrite.ev_class and "
             "pg_class.relnamespace = pg_namespace.oid and "
             "pg_namespace.nspname = '%s' and "
             "pg_class.relname = '%s'") %
            (schema, main_table_name,))
        self._pdbb_command_delete = _Query('delete from %s where %%(condition)s' % (main_table,))
        self._pdbb_command_refresh = _Query('refresh materialized view %s' % (main_table,))
        self._pdbb_command_isolation = _Query('set transaction isolation level %(isolation)s'
                                              '%(read_only)s')
        self._pdbb_command_notify = _Query('notify "__modif_%s"' % (main_table.lower(),))
        self._pg_notifications = map(lambda t: '__modif_%s' % (t.lower(),), table_names)

    def _pdbb_condition2sql(self, condition):
        if condition is None:
            return _Query('true')
        op_name, op_args, op_kwargs = \
            condition.name(), condition.args(), condition.kwargs()

        def function_call(op_args):
            assert len(op_args) >= 1, ('Invalid number of arguments', op_args)
            function, args = op_args[0], op_args[1:]
            queries = []
            for a in args:
                if isinstance(a, basestring):  # column name
                    queries.append(colarg(a)[0])
                elif isinstance(a, Value):  # direct value
                    queries.append(a)
                else:
                    raise ProgramError("Invalid function condition argument", a)
            return _QFunction(function, queries)

        def colarg(colid):
            if isinstance(colid, Operator) and colid.name() == 'Function':
                return function_call(colid.args()), None
            assert isinstance(colid, basestring), ('Invalid column specification', colid)
            col = self._db_column_binding(colid)
            assert col, ('Invalid column name', colid)
            a = self._pdbb_btabcol(col)
            t = self.find_column(colid).type()
            return a, t

        def relop(rel, args, kwargs):
            assert len(args) == 2, ('Invalid number or arguments', args)
            arg1, arg2 = args
            a1, t1 = colarg(arg1)
            if isinstance(arg2, basestring):
                a2, t2 = colarg(arg2)
                a2null = False
            else:
                assert isinstance(arg2, Value), ('Invalid value type', arg2)
                assert (not isinstance(t1, Binary) or
                        (rel in ('=', '!=') and arg2.value() is None)), \
                    "Binary data can only be compared with NULL values"
                val = arg2
                a2 = self._pdbb_coalesce(t1, val)
                t2 = arg2.type()
                a2null = val.value() is None
            if kwargs.get('ignore_case') and isinstance(t1, String) and isinstance(t2, String):
                def fix_case(x):
                    return x.wrap('lower')
            else:
                def fix_case(x):
                    return x
            if rel in ('=', '!=') and a2null:
                relarg = _Query(' IS' + (' NOT' if rel == '!=' else '') + ' NULL')
            else:
                relarg = _Query(rel + ' ') + fix_case(a2)
            return (fix_case(a1) + ' ' + relarg).wrap()
        operators = {'EQ': '=',
                     'NE': '!=',
                     'LT': '<',
                     'GT': '>',
                     'LE': '<=',
                     'GE': '>=',
                     'LTreeAncestor': '@>',
                     'LTreeDescendant': '<@',
                     'RangeContains': '@>',
                     'RangeContained': '<@',
                     'RangeOverlap': '&&',
                     }
        if op_name in operators:
            expression = relop(operators[op_name], op_args, op_kwargs)
        elif op_name in ('WM', 'NW'):
            cid, spec = op_args[0], op_args[1].value()
            for old, new in (('%', '\\%'), ('_', '\\_')):
                spec = string.replace(spec, old, new)
            for old, new in (('*', '%'), ('?', '_')):
                i = -1
                while True:
                    i = string.find(spec, old, i + 1)
                    if i < 0:
                        break
                    j = i - 1
                    while j >= 0 and spec[j] == '\\':
                        j = j - 1
                    if (i - j) % 2 == 1:
                        spec = spec[:i] + new + spec[i + 1:]
            rel = (op_name == 'NW' and 'NOT ' or '') + 'LIKE'
            expression = relop(rel, (cid, sval(spec),), op_kwargs)
        elif op_name == 'NOT':
            assert len(op_args) == 1, ('Invalid number or arguments', op_args)
            arg = op_args[0]
            assert isinstance(arg, Operator)
            expression = _Query('not ') + self._pdbb_condition2sql(arg)
        elif op_name == 'AND' or op_name == 'OR':
            if not op_args:
                expression = _Query('true' if op_name == 'AND' else 'false')
            else:
                assert not filter(lambda a: a and not isinstance(a, Operator),
                                  op_args), \
                    ('Invalid suboperator', op_args)
                exps = map(self._pdbb_condition2sql, op_args)
                sqlop = (' and ' if op_name == 'AND' else ' or ')
                expression = _Query.join(exps, sqlop)
        elif op_name == 'IN':
            assert len(op_args) == 5, ('Invalid number or arguments', op_args)
            col, data, table_col, cond, arguments = op_args
            table = data._key_binding[0].table()
            if data._condition is not None:
                cond = pytis.data.AND(data._condition, cond)
            condition = data._pdbb_condition2sql(cond)
            if arguments:
                args = []
                for i, b in enumerate(data._arguments):
                    type_ = b.type()
                    if not isinstance(type_, Type):
                        type_ = type_()
                    arg_value = arguments.get(b.id(), type_.default_value())
                    args.append(arg_value)
                table = _QFunction(table, args)
            else:
                table = _Query(table)
            expression = (_Query('%s in (select %s from ' % (col, table_col,)) + table + ' where ' +
                          condition + ')')
        elif op_name == 'FT':
            assert len(op_args) == 3, ('Invalid number of arguments', op_args)
            col, query, query_id = op_args
            expression = _Query('%s @@ %s' % (col, self._pdbb_fulltext_query_name(col),))
        elif op_name == 'LTreeMatch':
            assert len(op_args) == 2, ('Invalid number of arguments', op_args)
            col, query = op_args
            expression = _Query("%s ~ " % (col,)) + _Query.next_arg_query(sval(query))
        elif op_name == 'Function':
            expression = function_call(op_args)
        else:
            raise ProgramError('Unknown operator', op_name)
        return expression.wrap()

    def _pdbb_sort2sql(self, sort):
        function_column_dict = {}
        for g in (self._pdbb_column_groups or []):
            if g[2] is not None:
                function_column_dict[g[0]] = g

        def full_text_handler(binding):
            column_name = self._pdbb_btabcol(binding)
            query = self._pdbb_fulltext_query_name(binding.column())
            return _QFunction('ts_rank_cd', (column_name, query,))

        def item2sql(item, self=self):
            if isinstance(item, tuple):
                id, dirspec = item
                dir = {ASCENDENT: 'ASC', DESCENDANT: 'DESC'}[dirspec]
            else:
                id, dir = item, 'ASC'
            g = function_column_dict.get(id)
            if g:
                colstring = self._pdbb_column_group_call(g)
            else:
                b = self._db_column_binding(id)
                assert b is not None, \
                    "Unknown column '%s' in sorting specification for '%s'" % \
                    (id, self._key_binding[0].table())
                colstring = self._pdbb_btabcol(b, full_text_handler=full_text_handler,
                                               convert_ltree=True)
            return colstring + ' ' + dir
        sort_string = _Query.join([item2sql(item) for item in sort])
        if sort_string:
            sort_string += ','
        return sort_string

    def _pdbb_limit2sql(self, limit):
        return _Query('' if limit is None else 'limit %d' % (limit,))

    def _pdbb_fulltext_query_name(self, column_name):
        return '_pytis_ftq__%s' % (column_name,)

    # Metody související s exportovanými metodami DB operací

    def _pdbb_table_row_lists(self, row):
        table_bindings = filter(lambda b, t=self._key_binding[0].table(): b.table() == t,
                                self._bindings)
        columns = []
        values = []
        for b in table_bindings:
            try:
                value = row[b.id()]
            except KeyError:
                continue
            colid = b.id()
            colspec = self.find_column(colid)
            assert colspec, ('Column not found', colid)
            crypto_name = b.crypto_name()
            ctype = colspec.type()
            if isinstance(ctype, (DateTime, Time,)) and value.value() is not None:
                t = value.type()
                if ctype.without_timezone():
                    if not t.without_timezone():
                        notz_value = value.value()
                        if isinstance(t, DateTime):
                            # There's no way to convert time without date to local time zone
                            notz_value = notz_value.astimezone(DateTime.LOCAL_TZINFO)
                        notz_value = notz_value.replace(tzinfo=None)
                        value = Value(ctype, notz_value)
                else:
                    if t.without_timezone():
                        # There's no way to convert time without date to local time zone
                        if isinstance(t, DateTime):
                            tz_value = value.value().replace(tzinfo=DateTime.LOCAL_TZINFO)
                        else:
                            try:
                                raise DBUserException("Time value without time zone", None,
                                                      (value.value(), b.table(), colid,))
                            except DBUserException as e:
                                try:
                                    import pytis.form
                                except Exception:
                                    raise e
                                else:
                                    if pytis.form.top_window() is not None:
                                        pytis.form.top_level_exception()
                                    else:
                                        raise e
                        value = Value(ctype, tz_value)
            if crypto_name is not None:
                if isinstance(ctype, String):
                    encryption_function = 'pytis_encrypt_text'
                elif isinstance(ctype, Float):
                    encryption_function = 'pytis_encrypt_float'
                elif isinstance(ctype, Integer):
                    encryption_function = 'pytis_encrypt_int'
                elif isinstance(ctype, Binary):
                    encryption_function = 'pytis_encrypt_binary'
                else:
                    raise Exception("Encryption supported not available for the type", ctype)
                if isinstance(ctype, Binary):
                    if value.value() is None:
                        if b.encrypt_empty():
                            value = _QFunction(encryption_function,
                                               (sval(None), sval(crypto_name),))
                    else:
                        value = _QFunction(encryption_function, (value, sval(crypto_name)))
                else:
                    if value.value() is not None or b.encrypt_empty():
                        value = _QFunction(encryption_function, (value, sval(crypto_name)))
            columns.append(b.column())
            values.append(value)
        return columns, values

    def _pg_make_arguments(self, args, arguments):
        if self._arguments is not None and arguments is not self.UNKNOWN_ARGUMENTS:
            call_arguments = []
            for i in range(len(self._arguments)):
                b = self._arguments[i]
                type_ = b.type()
                if not isinstance(type_, Type):
                    type_ = type_()
                arg_value = arguments.get(b.id(), type_.default_value())
                call_arguments.append(arg_value)
            args[self._arguments_arg] = _Query.join(call_arguments)

    def _pg_row(self, key_value, columns, transaction=None, supplement='', arguments={}):
        """Retrieve and return raw data corresponding to 'key_value'."""
        args = dict(key=key_value, supplement=_Query(supplement))
        if columns:
            args['columns'] = self._pdbb_sql_column_list_from_names(
                columns, operations=self._pdbb_operations,
                column_groups=self._pdbb_column_groups)
        self._pg_make_arguments(args, arguments)
        query = self._pdbb_command_row.update(args)
        return self._pg_query(query, transaction=transaction)

    def _pg_search(self, row, condition, direction, transaction=None, arguments={}):
        if transaction is None:
            transaction = self._pg_select_transaction
        sorting = self._pg_last_select_sorting
        if direction == FORWARD:
            pass
        elif direction == BACKWARD:
            sorting = reversed_sorting(sorting)
        else:
            raise ProgramError('Invalid direction', direction)

        def sorting_condition(sorting, forwards, row, mayeq):
            # - forwards je True:
            #   pak je row řádek, na kterém stojíme a hledáme všechny řádky
            #   v směru pohybu vyhledávání
            # - forwards je False:
            #   pak je row řádek vyhledávaný řádek a hledáme všechny řádky,
            #   které jsou v protisměru pohybu vyhledávání.
            if row is None:
                return None
            sdirection = ecase(direction,
                               (FORWARD, ASCENDENT),
                               (BACKWARD, DESCENDANT))
            sorting = (tuple(sorting) +
                       tuple(map(lambda c: (c.id(), sdirection), self.key())))
            processed = []
            conditions = []
            for cid, dir in sorting:
                if cid in processed:
                    continue
                conds = [EQ(c, row[c]) for c in processed]
                if (forwards and dir == ASCENDENT) or (not forwards and dir == DESCENDANT):
                    relop = GT
                else:
                    relop = LT
                if row[cid].value() is None:
                    if relop is LT:
                        conds.append(NE(cid, row[cid]))
                else:
                    neq = relop(cid, row[cid], ignore_case=False)
                    if relop is GT:
                        nullval = Value(row[cid].type(), None)
                        neq = OR(neq, EQ(cid, nullval))
                    conds.append(neq)
                if conds and len(conds) > len(processed):
                    conditions.append(AND(*conds))
                processed.append(cid)
            if mayeq:
                eqs = [EQ(c, row[c], ignore_case=False) for c in processed]
                conditions.append(AND(*eqs))
            return OR(*conditions)
        select_cond = self._pg_last_select_condition
        common_cond = AND(select_cond,
                          sorting_condition(sorting, True, row, False))
        sort_string = self._pdbb_sort2sql(sorting)
        # Najdi první řádek splňující požadovanou podmínku
        search_cond = AND(common_cond, condition)
        cond_string = self._pdbb_condition2sql(search_cond)
        if direction == FORWARD:
            sql_command = self._pdbb_command_search_first
        elif direction == BACKWARD:
            sql_command = self._pdbb_command_search_last
        else:
            raise ProgramError('Unknown direction', direction)
        qargs = {'condition': cond_string, 'ordering': sort_string}
        if self._pdbb_select_column_list:
            qargs['columns'] = self._pdbb_select_column_list
        self._pg_make_arguments(qargs, arguments)
        query = sql_command.update(qargs)
        data_ = self._pg_query(query, transaction=transaction)
        if not data_:
            return 0
        # Zjisti vzdálenost mezi aktuálním a vyhledaným řádkem
        row_found = self._pg_make_row_from_raw_data(
            data_, template=self._pg_make_row_template_limited)
        search_cond = AND(common_cond,
                          sorting_condition(sorting, False,
                                            row_found, True))
        cond_string = self._pdbb_condition2sql(search_cond)
        args = dict(condition=cond_string)
        self._pg_make_arguments(args, arguments)
        data_ = self._pg_query(self._pdbb_command_search_distance.update(args),
                               transaction=transaction)
        try:
            result = int(data_[0][0])
        except Exception:
            raise ProgramError('Unexpected result', data_)
        return result

    class _PgRowCounting(object):

        class _Thread(threading.Thread):
            _PG_INITIAL_STEP = 1000
            _PG_MAX_STEP = 100000
            _PG_DEFAULT_TIMEOUT = 0.1
            _PG_STOP_CHECK_TIMEOUT = 0.1

            def __init__(self, data, initial_count, transaction, selection):
                threading.Thread.__init__(self)
                self._pg_data = data
                self._pg_transaction = transaction
                self._pg_selection = selection
                self._pg_current_count = initial_count
                self._pg_initial_count = initial_count
                self._pg_finished = False
                self._pg_terminate = False
                self._pg_terminate_event = threading.Event()
                self._pg_urgent = False
                self._pg_correction = 0
                self._pg_exception = None

            def run(self):
                try:
                    data = self._pg_data
                    step = min_step = self._PG_INITIAL_STEP
                    max_step = self._PG_MAX_STEP
                    test_count = self._pg_initial_count
                    selection = self._pg_selection
                    transaction = self._pg_transaction
                    args = dict(selection=selection, number=ival(self._pg_initial_count))
                    query = data._pdbb_command_move_absolute.update(args)
                    data._pg_query(query, transaction=transaction)
                    query_counter = data._pg_query_counter
                    while True:
                        if self._pg_dead():
                            self._pg_initial_count = self._pg_current_count
                            args = dict(selection=selection, number=ival(data._pg_dbpointer + 1))
                            query = data._pdbb_command_move_absolute.update(args)
                            if not transaction or transaction.open():
                                # The transaction can still become dead before
                                # the following query gets called, but the
                                # resulting error should be harmless.
                                data._pg_query(query, transaction=transaction)
                            return
                        if data._pg_query_counter > query_counter:
                            step = max(step / 8, min_step)
                        test_count += step
                        args = dict(selection=selection, number=ival(step))
                        query = data._pdbb_command_move_forward.update(args)
                        try:
                            result = data._pg_query(query, transaction=transaction)
                        except DBUserException:
                            log(OPERATIONAL, "Database exception in counting thread",
                                pytis.util.format_traceback())
                            self._pg_exception = sys.exc_info()
                            self._pg_finished = True
                            self._pg_terminate_event.set()
                            return
                        query_counter = data._pg_query_counter
                        self._pg_current_count = self._pg_current_count + result[0][0]
                        if self._pg_current_count < test_count:
                            break
                        if step < max_step:
                            step = min(2 * step, max_step)
                        # Give other (perhaps more urgent) threads on the same
                        # data object opportunity to call database queries.
                        if not self._pg_urgent:
                            time.sleep(0.1)
                    args = dict(selection=selection, number=ival(0))
                    query = data._pdbb_command_move_absolute.update(args)
                    data._pg_query(query, transaction=transaction)
                    self._pg_finished = True
                finally:
                    self._pg_terminate_event.set()

            def _pg_dead(self):
                return (self._pg_terminate or self._pg_exception is not None or
                        (self._pg_transaction and not self._pg_transaction.open()))

            def pg_count(self, min_value=None, timeout=None, corrected=False):
                self._pg_urgent = True
                stop_check = self._pg_data._pg_stop_check
                if self._pg_dead():
                    pass
                elif stop_check is not None:
                    start_time = time.time()
                    if timeout is not None:
                        stop_time = time.time() + timeout
                    while (not self._pg_finished and
                           (min_value is not None or
                            timeout is None or time.time() <= stop_time) and
                           (min_value is None or self._pg_current_count < min_value)):
                        stop_check(start_time)
                        t = self._PG_STOP_CHECK_TIMEOUT
                        if timeout is not None:
                            t = min(t, max(stop_time - time.time(), 0))
                        self._pg_terminate_event.wait(t)
                elif min_value is not None:
                    while self._pg_current_count < min_value and not self._pg_finished:
                        self._pg_terminate_event.wait(timeout or self._PG_DEFAULT_TIMEOUT)
                elif not self._pg_finished:
                    self._pg_terminate_event.wait(timeout)
                count = self._pg_current_count
                if corrected:
                    count += self._pg_correction
                self._pg_urgent = False
                return count, self._pg_finished

            def pg_stop(self):
                if self._pg_dead():
                    return
                self._pg_terminate = True
                self._pg_terminate_event.wait()
                data = self._pg_data
                args = dict(selection=self._pg_selection, number=ival(data._pg_dbpointer + 1))
                query = data._pdbb_command_move_absolute.update(args)
                data._pg_query(query, transaction=self._pg_transaction)

            def pg_restart(self):
                new_thread = self.__class__(self._pg_data, self._pg_current_count,
                                            self._pg_transaction, self._pg_selection)
                new_thread.start()
                return new_thread

            def pg_correct(self, correction):
                self._pg_correction += correction

        def __init__(self, data, transaction, selection):
            self._thread = self._Thread(data, 0, transaction, selection)

        def start(self):
            self._thread.start()

        def count(self, min_value=None, timeout=None, corrected=False):
            result = self._thread.pg_count(min_value, timeout, corrected)
            if self._thread._pg_exception:
                import pytis.form
                pytis.form.top_level_exception(self._thread._pg_exception)
            return result

        def stop(self):
            self._thread.pg_stop()

        def restart(self):
            self._thread = self._thread.pg_restart()

        def __add__(self, correction):
            self._thread.pg_correct(correction)
            return self

    def _pg_start_row_counting_thread(self, transaction, selection):
        t = self._PgRowCounting(self, transaction, selection)
        t.start()
        return t

    def _pg_select(self, condition, sort, columns, arguments={}, transaction=None,
                   async_count=False, stop_check=None, limit=None):
        """Initiate select and return the number of its lines or 'None'.

        Arguments:

          condition -- unprocessed conditional expression or 'None'
          sort -- unprocessed sorting specification or 'None'
          operation -- unprocessed specification of an aggregation function
          columns -- sequence of IDs of columns to select
          arguments -- dictionary of function call arguments
          transaction -- transaction object
          async_count -- if true, count result lines asynchronously and return
            a '_PgRowCounting' instance instead of the number of lines;
            this is useful on large tables where row counting may take
            significant amount of time
          stop_check -- if not 'None' then it is a function to be called
            periodically, during some long taking operations, with the single
            argument passing start time of the long operation as returned by
            'time.time()'.  It is not guaranteed that this function gets
            actually called during any long taking operation.  If it gets, it's
            up to the function what to do, it can e.g. raise some exception to
            stop the operation.
          limit -- maximum number of rows

        """
        cond_string = self._pdbb_condition2sql(condition)
        sort_string = self._pdbb_sort2sql(sort)
        limit_string = self._pdbb_limit2sql(limit)
        args = {'condition': cond_string, 'ordering': sort_string, 'limit': limit_string}
        fulltext_queries = [_Query('')]
        if condition:
            def find_fulltext(op):
                if op.name() == 'FT':
                    index_column = op.args()[0]
                    query = op.args()[1]
                    fulltext_queries[0] += (",to_tsquery('%s') as %s" %
                                            (query,
                                             self._pdbb_fulltext_query_name(index_column),))
                elif op.logical():
                    for a in op.args():
                        if isinstance(a, pytis.data.Operator):
                            find_fulltext(a)
            find_fulltext(condition)
        args['fulltext_queries'] = fulltext_queries[0]
        self._pg_make_arguments(args, arguments)
        connections = self._pg_connections()
        if connections and connections[-1].connection_info('broken') and transaction is None:
            # Current connection is broken, maybe after database server
            # restart.  In such a situation ugly things happen in wx forms and
            # we should try to avoid them.  We are most likely here
            # because_pg_restore_select tries to reopen the select.  We replace
            # the broken connection by a new one, but we may do it only if we
            # are not inside higher level transaction.
            new_connection = self._pg_connection_pool().get(self._pg_connection_data())
            connections[-1] = new_connection
        if columns:
            args['columns'] = self._pdbb_select_column_list = \
                self._pdbb_sql_column_list_from_names(
                    columns,
                    full_text_handler=self._pdbb_full_text_handler,
                    operations=self._pdbb_operations,
                    column_groups=self._pdbb_column_groups)
        else:
            self._pdbb_select_column_list = None
        args['selection'] = self._pdbb_selection_number = ival(self._pdbb_next_selection_number())
        dummy_select = (self._arguments is not None and arguments is self.UNKNOWN_ARGUMENTS)
        command = self._pdbb_command_dummy_select if dummy_select else self._pdbb_command_select
        transaction_ = self._pg_select_transaction if transaction is None else transaction
        self._pg_query(command.update(args), transaction=transaction_)
        if async_count:
            result = self._pg_start_row_counting_thread(transaction_, args['selection'])
        elif stop_check is not None:
            # Allow stop_check even when sync count is requested.
            counting_thread = self._pg_start_row_counting_thread(transaction_, args['selection'])
            result, finished = counting_thread.count()
            assert finished
        else:
            data = self._pg_query(self._pdbb_command_fetch_last.update(args),
                                  transaction=transaction_)
            self._pg_query(self._pdbb_command_move_to_start.update(args), transaction=transaction_)
            if data:
                result = int(data[0][-1])
            else:
                result = 0
        self._pg_number_of_rows = result
        self._pg_dbpointer = -1
        return result

    def _pg_distinct(self, column, prefix, condition, sort, transaction=None,
                     arguments={}):
        cond_string = self._pdbb_condition2sql(condition)
        colspec = self.find_column(column)
        if prefix:
            if isinstance(colspec.type(), String):
                expr = _QFunction('substr', (_Query(column), ival(1), ival(prefix))).label(column)
            else:
                raise ProgramError("Invalid column type for prefix selection")
        else:
            expr = _Query(column)
        dir = {ASCENDENT: 'ASC', DESCENDANT: 'DESC'}[sort]
        sort_string = _Query('%s %s' % (column, dir))
        args = dict(expression=expr, condition=cond_string, sorting=sort_string)
        self._pg_make_arguments(args, arguments)
        query = self._pdbb_command_distinct.update(args)
        data = self._pg_query(query, transaction=transaction)
        tmpl = self._pg_create_make_row_template((colspec,))
        result = [self._pg_make_row_from_raw_data([r], tmpl)[column] for r in data]
        return result

    def _pg_select_aggregate(self, operation, colids, condition, transaction=None, arguments={}):
        if __debug__:
            self._pg_check_arguments(arguments)
            if operation != self.AGG_COUNT:
                if operation in (self.AGG_MIN, self.AGG_MAX):
                    allowed = (Number, DateTime, String)
                else:
                    allowed = Number
                for cid in colids:
                    t = self.find_column(cid).type()
                    assert isinstance(t, allowed), (operation, cid, t, allowed,)
        close_select = False
        if self._pg_select_transaction is None:
            self.select(condition=condition, arguments=arguments, transaction=transaction)
            close_select = True
        if self._arguments is not None and arguments is self.UNKNOWN_ARGUMENTS:
            data = [[None for x in colids]]
        else:
            try:
                data = self._pg_select_aggregate_1(operation, colids, condition,
                                                   transaction=transaction, arguments=arguments)
            except Exception:
                cls, e, tb = sys.exc_info()
                try:
                    if transaction is None:
                        self._pg_select_transaction.rollback()
                except Exception:
                    pass
                self._pg_select_transaction = None
                raise cls, e, tb
            if close_select:
                self.close()
        ti = Integer()
        tf = Float()

        def make_value(cid, dbvalue):
            if operation == self.AGG_COUNT:
                t = ti
            elif operation == self.AGG_AVG:
                t = tf
            else:
                t = self.find_column(cid).type()
            return Value(t, dbvalue)
        result = [make_value(cid, data[0][i]) for i, cid in enumerate(colids)]
        return result

    def _pg_aggregate_name(self, operation):
        FMAPPING = {self.AGG_MIN: 'min',
                    self.AGG_MAX: 'max',
                    self.AGG_COUNT: 'count',
                    self.AGG_SUM: 'sum',
                    self.AGG_AVG: 'avg',
                    }
        try:
            return FMAPPING[operation]
        except KeyError:
            raise ProgramError('Invalid aggregate function identifier',
                               operation)

    def _pg_select_aggregate_1(self, operation, colids, condition, transaction=None, arguments={}):
        cond_string = self._pdbb_condition2sql(condition)
        colnames = [self._pdbb_btabcol(self._db_column_binding(cid)) for cid in colids]
        function = self._pg_aggregate_name(operation)
        function_list = [_QFunction(function, (cname,)) for cname in colnames]
        function_string = _Query.join(function_list)
        args = dict(columns=function_string, condition=cond_string)
        if arguments:
            self._pg_make_arguments(args, arguments)
        query = self._pdbb_command_select_agg.update(args)
        if transaction is None:
            transaction = self._pg_select_transaction
        return self._pg_query(query, transaction=transaction)

    def _pg_fetchmany(self, count, direction, transaction=None):
        """Vrať 'count' řádků selectu jako raw data."""
        args = {'number': ival(count), 'selection': self._pdbb_selection_number}
        if direction == FORWARD:
            query = self._pdbb_command_fetch_forward.update(args)
        elif direction == BACKWARD:
            query = self._pdbb_command_fetch_backward.update(args)
        else:
            raise ProgramError('Invalid direction', direction)
        return self._pg_query(query, transaction=transaction)

    def _pg_skip(self, count, direction, exact_count=False, transaction=None):
        """Přeskoč 'count' řádků v 'direction'."""
        args = dict(number=ival(count), selection=self._pdbb_selection_number)
        if direction == FORWARD:
            self._pg_query(self._pdbb_command_move_forward.update(args),
                           transaction=transaction)
        elif direction == BACKWARD:
            answer = self._pg_query(self._pdbb_command_move_backward.update(args),
                                    transaction=transaction)
            answer_count = answer[0][0]
            if exact_count and answer_count != count:
                log(OPERATIONAL, "Unexpected result of cursor operation MOVE:",
                    (answer_count, count))
        else:
            raise ProgramError('Invalid direction', direction)
        return None

    def _pg_insert(self, row, after=None, before=None, transaction=None):
        """Vlož 'row' a vrať jej jako nová raw data nebo vrať 'None'."""
        ordering = self._ordering
        if ordering:
            ocol = self._ordering[0]
            if after:
                neighbor = after
                n = neighbor[ocol].value() + 1
            elif before:
                neighbor = before
                n = neighbor[ocol].value()
            else:
                neighbor = row
                n = -1
            try:
                args = {}
                for i in range(1, len(self._ordering)):
                    args['param_%d' % (i,)] = neighbor[ordering[i]]
            except KeyError:
                raise ProgramError('Invalid column id in ordering', self._ordering, row)
            if n >= 0:
                args['position'] = ival(n)
                self._pg_query(self._pdbb_command_insert_shift.update(args), backup=True,
                               transaction=transaction)
            else:
                result = self._pg_query(self._pdbb_command_insert_newpos.update(args),
                                        transaction=transaction)
                if result:
                    raw = result[0][0]
                    if raw is None:
                        n = 1
                    else:
                        n = int(raw)
                else:
                    n = 1
            oval = Value(Integer(), n)
            try:
                row[ocol] = oval
            except KeyError:
                row.append(ocol, oval)
        cols, vals = self._pdbb_table_row_lists(row)
        columns = _Query.join(cols)
        values = _Query.join(vals)
        self._pg_query(_Query("savepoint _insert"), transaction=transaction)
        try:
            key_data = self._pg_query(
                self._pdbb_command_insert.update(dict(columns=columns, values=values)),
                backup=True, transaction=transaction)
        except DBInsertException:
            self._pg_query(_Query("rollback to _insert"), transaction=transaction)
            self._pg_query(
                self._pdbb_command_insert_alternative.update(dict(columns=columns, values=values)),
                backup=True, transaction=transaction)
            try:
                key = row[self._key_binding[0].id()]
            except KeyError:
                key = None
                if isinstance(self._key_binding[0].type(), Serial):
                    try:
                        key_data = self._pg_query(self._pdbb_command_insert_get_last,
                                                  transaction=transaction)
                        key_row = self._pg_make_row_from_raw_data(
                            key_data, template=(self._pg_make_row_template[0],))
                        key = key_row[0]
                    except DBException:
                        pass
        else:
            key_row = self._pg_make_row_from_raw_data(
                key_data, template=(self._pg_make_row_template[0],))
            key = key_row[0]
            self._pg_query(_Query("release _insert"), transaction=transaction)
        if key is None:
            result = None
        else:
            result = self.row(key, transaction=transaction)
        return result

    def _pg_update(self, condition, row, transaction=None):
        """Updatuj řádky identifikované 'condition'.

        Vrací: Počet updatovaných řádků.

        """
        # TODO: Při použití RULEs v PostgreSQL UPDATE vrací vždy 0.  Toto
        # chování je sporné, nicméně v tuto chvíli PostgreSQL nenabízí žádné
        # přímé řešení, jak výsledek UPDATE zjistit.  Proto zde aplikujeme
        # jakýsi hack, který nějakým způsobem ošetří alespoň některé situace,
        # aby nebyl signalizován neúspěch UPDATE v případě jeho úspěchu.
        cols, vals = self._pdbb_table_row_lists(row)
        if not cols:
            return 0
        settings = _Query(cols[0] + '=') + _Query.next_arg_query(vals[0])
        for c, v in zip(cols[1:], vals[1:]):
            settings = settings + _Query(', ' + c + '=') + _Query.next_arg_query(v)
        cond_query = self._pdbb_condition2sql(condition)

        def extract_result(d):
            try:
                return int(d[0][0])
            except Exception:
                raise DBSystemException('Unexpected UPDATE result', None, d)
        try:
            broken = self._pdbb_broken_update_result
        except AttributeError:
            broken = self._pdbb_broken_update_result = \
                self._pg_query(self._pdbb_command_test_broken_update, transaction=transaction)
        if broken:
            q = self._pdbb_command_broken_update_preselect.update(dict(condition=cond_query))
            d = self._pg_query(q, transaction=transaction)
            result = extract_result(d)
        d = self._pg_query(self._pdbb_command_update.update(dict(settings=settings,
                                                                 condition=cond_query)),
                           backup=True, transaction=transaction)
        if not broken:
            result = extract_result(d)
        if result >= 0:
            return result
        else:
            raise DBSystemException('Unexpected UPDATE value', None, result)

    def _pg_delete(self, condition, transaction=None):
        """Smaž řádek identifikovaný podmínkou 'condition'.

        Vrací: Počet smazaných řádků.

        """
        sql_condition = self._pdbb_condition2sql(condition)
        d = self._pg_query(self._pdbb_command_delete.update(dict(condition=sql_condition)),
                           backup=True, transaction=transaction)
        try:
            result = int(d[0][0])
        except Exception:
            raise DBSystemException('Unexpected DELETE result', None, d)
        if result >= 0:
            return result
        else:
            raise DBSystemException('Unexpected DELETE value', None, result)

    def _pg_send_notifications(self):
        """Rozešli notifikace o modifikaci tohoto datového objektu."""
        self._pg_query(self._pdbb_command_notify, outside_transaction=True)


class DBDataPostgreSQL(PostgreSQLStandardBindingHandler, PostgreSQLNotifier):
    """Datová tabulka s napojením do PostgreSQL.

    Tato třída překládá požadavky do SQL, není však implementačně závislá na
    konkrétním použitém postgresovém modulu pro Python.

    """
    # TODO: Tato třída je mamut a měla by být rozdělena na několik menších částí

    _PG_LOCK_TABLE = '_rowlocks_real'
    _PG_LOCK_TABLE_LOCK = '_rowlocks_real'
    _PG_LOCK_TIMEOUT = 30         # perioda updatu v sekundách

    class _PgBuffer:

        def __init__(self):
            if __debug__:
                log(DEBUG, 'New buffer')
            self.reset()

        def reset(self):
            """Completely reset the buffer."""
            if __debug__:
                log(DEBUG, 'Resetting buffer')
            self._buffer = []
            # _start ... position of the buffer begining within the database.
            #    The row number of the first buffer item, starting from 0.
            # _pointer ... position of the buffer pointer relative to the
            #    first buffer item.  Points to the last fetched item.  May
            #    point outside the buffer.
            self._start = 0
            self._pointer = -1

        def current(self):
            """Return the current buffer item.

            If the current position points outside buffer data previously
            loaded by 'fill()', returns None.

            """
            buf = self._buffer
            pointer = self._pointer
            if pointer < 0 or pointer >= len(buf):
                row = None
            else:
                row = buf[pointer]
            return row

        def position(self):
            """Return the current buffer position as int.

            The position may point outside buffer data (previously loaded by
            'fill()').

            """
            return self._start + self._pointer

        def fetch(self, direction):
            """Return next buffer item in given direction from the current position.

            If the row is not present in the buffer, 'None' is returned and it
            is assumed that the buffer will be filled by subsequent 'fill()'
            call if necessary.  Buffer position is updated in any case -
            incremented when fetching forward and decremented when fetching
            backwards.

            """
            buf = self._buffer
            pointer = self._pointer
            if direction == FORWARD:
                pointer += 1
            elif direction == BACKWARD:
                pointer -= 1
            else:
                raise ProgramError('Invalid direction', direction)
            if pointer < 0 or pointer >= len(buf):
                if __debug__:
                    log(DEBUG, 'Buffer miss:', pointer)
                result = None
            else:
                if __debug__:
                    log(DEBUG, 'Buffer hit:', pointer)
                result = buf[pointer]
            self._pointer = pointer
            return result

        def skip(self, count, direction):
            """Skip given number of items in given direction.

            Moves buffer position relatively to the current position.

            Arguments:

              count -- number of items to skip
              direction -- One of the constants 'FORWARD'/'BACKWARD'

            """
            if direction == FORWARD:
                self._pointer += count
            elif direction == BACKWARD:
                self._pointer -= count
            else:
                raise ProgramError('Invalid direction', direction)

        def goto(self, position):
            """Set buffer position to given absolute position.

            Arguments:

              position -- Item number starting from zero.

            """
            self._pointer = position - self._start

        def fill(self, position, items):
            """Fill the buffer by given items and update the pointers.

            Arguments:

              position -- position of the first item within the database
                select starting from zero.
              items -- list of items to fill in the buffer.


            """
            if __debug__:
                log(DEBUG, 'Filling buffer:', (position, len(items)))
            n = len(items)
            buf = self._buffer
            buflen = len(buf)
            start = self._start
            if start + buflen == position:
                import config
                retain = max(config.cache_size - n, 0)
                cutoff = max(buflen - retain, 0)
                buf = buf[cutoff:] + items
                start += cutoff
                pointer = position - start - 1
            else:
                buf = items
                start = position
                pointer = -1
            self._buffer = buf
            self._start = start
            self._pointer = pointer

        def __str__(self):
            buffer = self._buffer
            pointer = self._pointer
            max = len(buffer) - 1
            if max < 0:
                bufstr = ''
            elif max == 0:
                bufstr = str(buffer[0])
            elif max == 1:
                bufstr = '%s\n%s' % (buffer[0], buffer[1])
            elif pointer <= 0 or pointer >= max:
                bufstr = '%s\n...\n%s' % (buffer[0], buffer[-1])
            elif max == 2:
                bufstr = '%s\n%s\n%s' % (buffer[0], buffer[1], buffer[2])
            elif pointer == 1:
                bufstr = '%s\n%s\n...\n%s' % (buffer[0], buffer[1], buffer[-1])
            elif pointer == max - 1:
                bufstr = '%s\n...\n%s\n%s' % \
                         (buffer[0], buffer[-2], buffer[-1])
            else:
                bufstr = '%s\n...\n%s\n...\n%s' % \
                         (buffer[0], buffer[pointer], buffer[-1])
            return '<PgBuffer: start=%d, index=%d\n%s>' % (self._start, self._pointer, bufstr)

    def __init__(self, bindings, key, connection_data, ro_select=True, **kwargs):
        """Inicializuj databázovou tabulku dle uvedených specifikací.

        Argumenty:

          bindings -- stejné jako v předkovi
          key -- binding klíčového sloupce datové tabulky, musí být jeden
            z prvků 'bindings' nebo sekvence prvků z 'bindings'
          connection_data -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'
          ro_select -- iff true, make select transactions read-only when
            possible
          kwargs -- předá se předkovi

        """
        if __debug__:
            log(DEBUG, 'Creating data table')
        if is_sequence(key):
            self._key_binding = tuple(key)
        else:
            self._key_binding = (key,)
        self._pg_select_transaction = None
        super(DBDataPostgreSQL, self).__init__(
            bindings=bindings, key=key, connection_data=connection_data,
            **kwargs)
        self._pg_dbpointer = -1  # DB cursor position starting from zero
        self._pg_buffer = self._PgBuffer()
        self._pg_number_of_rows = None
        self._pg_initial_select = False
        self._pg_ro_select = ro_select
        # TODO: Ugly, fix this!
        if hasattr(self, '_pdbb_filtered_bindings'):
            binding_ids = [b.id() for b in self._pdbb_filtered_bindings]
            filtered_columns = [c for c in self._columns if c.id() in binding_ids]
        else:
            filtered_columns = self._columns
        self._pg_make_row_template = \
            self._pg_create_make_row_template(filtered_columns,
                                              column_groups=getattr(self, '_pdbb_column_groups'))
        self._pg_make_row_template_limited = None
        # NASTAVENÍ CACHE
        # Protože pro různé parametry (rychlost linky mezi serverem a klientem,
        # velikost paměti atd.), je vhodné různé nastavení cache,
        # budeme parametry nastavovat z konfiguračního souboru.
        # Pozor, config.cache_size je využíváno přímo v _PgBuffer.
        # Zde tyto hodnoty zapamatujeme jako atributy objektu, protože jsou
        # potřeba v kritických částech kódu a čtení konfigurace přeci jen trvá.
        import config
        self._pg_initial_fetch_size = config.initial_fetch_size
        self._pg_fetch_size = config.fetch_size

    # Metody pro transakce

    def _pg_allocate_connection(self):
        connections = self._pg_connections()
        if __debug__:
            if len(connections) >= 3:
                log(DEBUG, 'Suspicious connection depth:', len(connections))
        connection = self._pg_get_connection(outside_transaction=True)[0]
        connections.append(connection)

    def _pg_deallocate_connection(self):
        self._pg_return_connection(self._pg_connections().pop())

    def _pg_begin_transaction(self, isolation=None, read_only=False):
        if self._pg_select_transaction is not None:
            self.close()
        limit = 10
        while True:
            self._pg_allocate_connection()
            try:
                self._postgresql_begin_transaction()
                if isolation:
                    read_only_string = (" READ ONLY" if read_only else "")
                    args = dict(isolation=_Query(isolation), read_only=_Query(read_only_string))
                    q = self._pdbb_command_isolation.update(args)
                    self._pg_query(q)
                else:
                    self._pg_query(_Query("select null"))
                break
            except DBRetryException:
                # Maybe database connection lost in past, try again
                limit -= 1
                if limit <= 0:
                    raise

    def _pg_commit_transaction(self):
        self._postgresql_commit_transaction()
        self._pg_deallocate_connection()

    def _pg_rollback_transaction(self):
        self._postgresql_rollback_transaction()
        self._pg_deallocate_connection()

    # Pomocné metody

    def _pg_create_make_row_template(self, columns, column_groups=None):
        template = []
        for c in columns:
            id_ = c.id()
            type = c.type()
            if isinstance(type, (String, LTree)):
                typid = 0
            elif isinstance(type, (Time, DateTime,)):
                typid = 2
            elif isinstance(type, TimeInterval):
                typid = 3
            else:
                typid = 99
            template.append((id_, typid, type))
        return template

    def _pg_limited_make_row_template(self, columns):
        template = []
        for c in columns:
            for item in self._pg_make_row_template:
                if item[0] == c:
                    template.append(item)
                    break
            else:
                raise ProgramError("Column not found in template", c)
        return template

    def _pg_make_row_from_raw_data(self, data_, template=None):
        if not data_:
            return None
        if not template:
            template = self._pg_make_row_template
        return Row([(cid, Value(ctype, dbvalue))
                    for dbvalue, (cid, typid, ctype) in zip(data_[0], template)])

    def _pg_already_present(self, row, transaction=None):
        key = []
        for k in self.key():
            try:
                id = k.id()
            except Exception:
                return False
            try:
                key.append(row[id])
            except KeyError:
                return False
        return self.row(key, transaction=transaction)

    def _pg_key_condition(self, key):
        if __debug__:
            log(DEBUG, 'Creating condition from key:', key)
        key = xtuple(key)
        keycols = map(lambda b: b.id(), self._key_binding)
        assert len(keycols) == len(key), ('Invalid key length', key, keycols)
        ands = map(EQ, keycols, key)
        condition = AND(*ands)
        if __debug__:
            log(DEBUG, 'Key condition created:', condition)
        return condition

    def _pg_restore_select(self):
        row_number = self._pg_last_select_row_number
        if row_number is None and self._pg_last_select_transaction is not None:
            # The last select wasn't closed correctly and we are inside a
            # higher level transaction -- we mustn't continue in such a case.
            return False
        if row_number is None:
            row_number = self._pg_buffer.position()
        self.select(condition=self._pg_last_select_condition,
                    sort=self._pg_last_select_sorting,
                    columns=self._pg_last_select_columns,
                    transaction=self._pg_last_select_transaction,
                    arguments=self._pg_last_select_arguments,
                    async_count=self._pg_async_count,
                    stop_check=self._pg_stop_check,
                    limit=self._pg_last_select_limit,
                    timeout_callback=self._pg_timeout_callback)
        if row_number >= 0:
            self.skip(row_number + 1)
        return True

    def _pg_maybe_restore_select(self):
        if self._pg_select_transaction is None:
            if not self._pg_restore_select():
                raise NotWithinSelect()

    def _pg_number_of_rows_(self, min_value=None):
        if isinstance(self._pg_number_of_rows, int):
            number = self._pg_number_of_rows
        else:
            number, finished = self._pg_number_of_rows.count(min_value=min_value)
            if finished:
                self._pg_number_of_rows = number
        return number

    def _pg_check_arguments(self, arguments):
        if arguments is self.UNKNOWN_ARGUMENTS or not arguments:
            return
        assert self._arguments is not None, ("Arguments passed to a non-function", arguments,)
        argument_names = [b.id() for b in self._arguments]
        for k in arguments.keys():
            assert k in argument_names, ("Invalid function argument", k,)

    # Veřejné metody a jimi přímo volané abstraktní metody

    def row(self, key, columns=None, transaction=None, arguments={}):
        if self._arguments is not None and arguments is self.UNKNOWN_ARGUMENTS:
            return None
        if __debug__:
            self._pg_check_arguments(arguments)
        # TODO: Temporary compatibility hack.  The current internal db code
        # uses multikeys, but user code does not anymore.  Before we rewrite
        # the internal parts to use single keys only, we should allow both
        # kinds of keys.
        if not is_sequence(key):
            key = (key,)
        if columns:
            template = self._pg_limited_make_row_template(columns)
        else:
            template = None
        try:
            data = self._pg_row(key[0], columns, transaction=transaction, arguments=arguments)
        except Exception:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except Exception:
                pass
            raise cls, e, tb
        if transaction is None and self._pg_select_transaction is None:
            self._postgresql_commit_transaction()
        result = self._pg_make_row_from_raw_data(data, template=template)
        return result

    def select(self, condition=None, sort=(), reuse=False, columns=None, transaction=None,
               arguments={}, async_count=False, stop_check=None, timeout_callback=None,
               limit=None):
        if __debug__:
            log(DEBUG, 'Select started:', condition)
        if __debug__:
            self._pg_check_arguments(arguments)
        if ((reuse and not self._pg_changed and self._pg_number_of_rows and
             condition == self._pg_last_select_condition and
             sort == self._pg_last_select_sorting and
             columns == self._pg_last_select_columns and
             transaction is self._pg_last_select_transaction and
             limit == self._pg_last_select_limit and
             not async_count)):
            use_cache = True
        else:
            use_cache = False
        if self._pg_select_transaction is not None:
            self.close()
        if transaction is None:
            from pytis.data import DBTransactionDefault
            self._pg_select_transaction = \
                DBTransactionDefault(self._pg_connection_data(),
                                     connection_name=self._connection_name,
                                     isolation=DBPostgreSQLTransaction.REPEATABLE_READ,
                                     timeout_callback=timeout_callback)
            self._pg_select_user_transaction = False
            self._pg_select_set_read_only = self._pg_ro_select
        else:
            self._pg_select_transaction = transaction
            self._pg_select_user_transaction = True
            self._pg_select_set_read_only = False
        self._pg_last_select_condition = condition
        self._pg_last_select_sorting = sort
        self._pg_last_select_columns = columns
        self._pg_last_select_transaction = transaction
        self._pg_last_select_arguments = arguments
        self._pg_last_select_limit = limit
        self._pg_last_select_row_number = None
        self._pg_stop_check = stop_check
        self._pg_async_count = async_count
        self._pg_timeout_callback = timeout_callback
        self._pg_changed = False
        if columns:
            self._pg_make_row_template_limited = \
                self._pg_limited_make_row_template(columns)
        else:
            self._pg_make_row_template_limited = None
        last_number_of_rows = self._pg_number_of_rows
        try:
            row_count = self._pg_select(condition, sort, columns, transaction=transaction,
                                        arguments=arguments, async_count=async_count,
                                        stop_check=stop_check, limit=limit)
        except Exception:
            if isinstance(last_number_of_rows, self._PgRowCounting):
                try:
                    last_number_of_rows.stop()
                except Exception:
                    pass
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_select_transaction.rollback()
            except Exception:
                pass
            self._pg_select_transaction = None
            raise cls, e, tb
        if use_cache and isinstance(row_count, int) and row_count == last_number_of_rows:
            self._pg_buffer.goto(-1)
        else:
            self._pg_buffer.reset()
            self._pg_initial_select = True
        return row_count

    def select_aggregate(self, operation, condition=None, transaction=None, arguments={}):
        return self._pg_select_aggregate(operation[0], (operation[1],),
                                         condition=condition, transaction=transaction,
                                         arguments=arguments)[0]

    def select_and_aggregate(self, operation, condition=None, reuse=False, sort=(),
                             columns=None, transaction=None, arguments={}):
        if columns is None:
            function_columns = [g[0] for g in (self._pdbb_column_groups or ()) if g[2] is not None]
            columns = [c.id() for c in self.columns()
                       if c.id() not in function_columns]
            if self._pdbb_operations:
                allowed_column_ids = [b.id() for b in self._pdbb_filtered_bindings]
                columns = [c for c in columns if c in allowed_column_ids]
        select_result = self.select(condition=condition, reuse=reuse,
                                    sort=sort, columns=columns, transaction=transaction)
        if operation == self.AGG_COUNT:
            number_columns = columns
        else:
            number_columns = [cid for cid in columns
                              if isinstance(self.find_column(cid).type(), Number)]
        aggregate_results = self._pg_select_aggregate(operation, number_columns,
                                                      condition=condition, transaction=transaction,
                                                      arguments=arguments)

        def aggregate_value(cid):
            if number_columns and cid == number_columns[0]:
                del number_columns[0]
                number = aggregate_results[0]
                del aggregate_results[0]
                result = (cid, number,)
            else:
                result = (cid, Value(Type(), None),)
            return result
        aggregates = [aggregate_value(cid) for cid in columns]
        return select_result, Row(aggregates)

    def distinct(self, column, prefix=None, condition=None, sort=ASCENDENT, transaction=None,
                 arguments={}):
        """Vrať sekvenci všech nestejných hodnot daného sloupce.

        Argumenty:

          column -- column identifier
          prefix -- length of a string prefix to work on (integer).  If not 'None', only given
            initial substring of column's value is considered by the query.  Only applicable for
            columns of string types.
          condition -- conditional expression as an Operator instance or 'None'
          sort -- one of 'ASCENDENT', 'DESCENDANT' constants or None
          transaction -- transaction object to be used when running the SQL
            commands
          arguments -- dictionary of function call arguments

        """
        if __debug__:
            self._pg_check_arguments(arguments)
        return self._pg_distinct(column, prefix, condition, sort, transaction=transaction,
                                 arguments=arguments)

    def fetchone(self, direction=FORWARD, transaction=None):
        """Stejné jako v nadtřídě.

        Metoda automaticky nereaguje na notifikace o změně dat souvisejících
        tabulek a pokračuje (nedojde-li k přerušení transakce) v dodávce
        starých dat.  Automatické přenačítání dat při změně by mohlo vést
        k výkonnostním problémům, jeho provádění je tedy ponecháno na uvážení
        aplikace, která se může nechat o změnách informovat registrací
        prostřednictvím metody 'add_callback_on_change()'.

        """
        if __debug__:
            log(DEBUG, 'Fetching row from selection in direction:', direction)
        assert direction in(FORWARD, BACKWARD), ('Invalid direction', direction)
        self._pg_maybe_restore_select()
        buf = self._pg_buffer
        position = buf.position()
        row = buf.fetch(direction)
        if row:
            result = row
        else:
            if transaction is None:
                transaction = self._pg_select_transaction
            # PostgreSQL cursors have a number of problems.  They often fail
            # at the edges and FETCH BACKWARD often doesn't work correctly.
            # The following code tries to work around the known issues.
            if self._pg_initial_select:
                self._pg_initial_select = False
                fetch_size = self._pg_initial_fetch_size
            else:
                fetch_size = self._pg_fetch_size
            max_fetch_size = self._pg_number_of_rows_(fetch_size)
            if direction == FORWARD:
                last_position = min(position + 1, self._pg_number_of_rows_(position + 1))
                number_of_rows = self._pg_number_of_rows_(last_position + fetch_size)
                fetch_size = min(fetch_size, number_of_rows - last_position)
            elif position <= 0:
                fetch_size = 0
            else:
                fetch_size = min(fetch_size, max_fetch_size)
            assert 0 <= fetch_size <= max_fetch_size, (fetch_size, max_fetch_size)
            if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                self._pg_number_of_rows.stop()
            correction = 0
            if fetch_size != 0:
                skip = position - self._pg_dbpointer
                if direction == BACKWARD:
                    if fetch_size < position:
                        skip -= fetch_size + 1
                    else:
                        skip -= position + 1
                        correction = fetch_size - position
                if __debug__:
                    log(DEBUG, 'Determined skip:', skip)
                try:
                    if skip != 0:
                        self._pg_skip(abs(skip), FORWARD if skip > 0 else BACKWARD,
                                      exact_count=True, transaction=transaction)
                    row_data = self._pg_fetchmany(fetch_size, FORWARD, transaction=transaction)
                    if not row_data:
                        # If rows are fetched, the pointer will be further updated.
                        self._pg_dbpointer += skip
                except Exception:
                    cls, e, tb = sys.exc_info()
                    if not self._pg_select_user_transaction:
                        try:
                            self._pg_select_transaction.rollback()
                        except Exception:
                            pass
                    self._pg_select_transaction = None
                    raise cls, e, tb
            else:
                # Don't run an unnecessary SQL command
                row_data = None
            if row_data:
                mkrow, tmpl = self._pg_make_row_from_raw_data, self._pg_make_row_template_limited
                rows = [mkrow([d], template=tmpl) for d in row_data]
                # The last column is '_number' (convert long to int).
                buf.fill(int(row_data[0][-1]) - 1, rows)
                if direction == BACKWARD:
                    buf.goto(buf.position() + len(rows) + 1 - correction)
                self._pg_dbpointer = int(row_data[-1][-1]) - 1
                result = buf.fetch(direction)
            else:
                pos = buf.position()
                buf.goto(min(max(pos, -1), self._pg_number_of_rows_(pos)))
                result = None
            if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                self._pg_number_of_rows.restart()
        if __debug__:
            log(DEBUG, 'Returned row', str(result))
        if self._pg_select_set_read_only:
            self._pg_select_transaction.set_read_only()
            self._pg_select_set_read_only = False
        return result

    def last_row_number(self):
        self._pg_maybe_restore_select()
        return self._pg_buffer.position()

    def last_select_condition(self):
        return self._pg_last_select_condition

    def last_select_condition_sql(self):
        return self._pdbb_condition2sql(self._pg_last_select_condition)

    def skip(self, count, direction=FORWARD):
        if __debug__:
            log(DEBUG, 'Skipping rows:', (direction, count))
        assert isinstance(count, (int, long)) and count >= 0, count
        assert direction in (FORWARD, BACKWARD), direction
        self._pg_maybe_restore_select()
        buf = self._pg_buffer
        position = buf.position()
        if direction == FORWARD:
            count = min(count, self._pg_number_of_rows_(position + count) - position)
        else:
            count = min(count, position + 1)
        buf.skip(count, direction)
        if __debug__:
            log(DEBUG, 'Rows skipped:', count)
        return count

    def rewind(self):
        self._pg_maybe_restore_select()
        pos = self._pg_buffer.position()
        if pos >= 0:
            self.skip(pos + 1, BACKWARD)

    def search(self, condition, direction=FORWARD, transaction=None, arguments={}):
        """Vyhledej ve směru 'direction' první řádek od 'row' dle 'condition'.

        Vrací: Vzdálenost od řádku 'row' jako kladný integer nebo 0, pokud
        takový řádek neexistuje.

        """
        if __debug__:
            log(DEBUG, 'Searching row:', (condition, direction))
        assert direction in (FORWARD, BACKWARD), ('Invalid direction', direction)
        if __debug__:
            self._pg_check_arguments(arguments)
        self._pg_maybe_restore_select()
        if self._arguments is not None and arguments is self.UNKNOWN_ARGUMENTS:
            return 0
        row = self._pg_buffer.current()
        pos = self._pg_buffer.position()
        if not row and pos >= 0 and pos < self._pg_number_of_rows_(pos + 1):
            self.skip(1, BACKWARD)
            row = self.fetchone()
        if not row and (pos < 0 and direction == BACKWARD or pos >= 0 and direction == FORWARD):
            result = 0
        else:
            try:
                result = self._pg_search(row, condition, direction,
                                         transaction=transaction, arguments=arguments)
            except Exception:
                cls, e, tb = sys.exc_info()
                if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                    try:
                        self._pg_number_of_rows.stop()
                    except Exception:
                        pass
                if not self._pg_select_user_transaction:
                    try:
                        self._pg_select_transaction.rollback()
                    except Exception:
                        pass
                self._pg_select_transaction = None
                raise cls, e, tb
        if __debug__:
            log(DEBUG, 'Search result:', result)
        return result

    def close(self):
        if __debug__:
            log(DEBUG, 'Explicitly closing current select')
        if self._pg_select_transaction is not None:
            if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                self._pg_number_of_rows.stop()
            self._pg_last_select_row_number = self._pg_buffer.position()
            args = dict(selection=self._pdbb_selection_number)
        if self._pg_select_transaction is not None and not self._pg_select_user_transaction:
            try:
                self._pg_select_transaction.commit()
            except DBSystemException:  # e.g. after db engine restart
                pass
            self._pg_select_transaction = None
        elif self._pg_select_transaction is not None:  # inside user transaction
            if self._pg_select_transaction.open():
                query = self._pdbb_command_close_select.update(args)
                transaction = self._pg_select_transaction
                try:
                    self._pg_query(query, transaction=transaction)
                except DBRetryException:
                    # Do nothing when the connection is allready closed
                    # (e.g. when db server closed connection because of long inactivity)
                    pass
        self._pg_select_transaction = None
        # Flush cached data
        self._pg_buffer.reset()

    def select_active(self):
        return self._pg_select_transaction is not None

    def insert(self, row, after=None, before=None, transaction=None):
        assert after is None or before is None, 'Both after and before specified'
        log(ACTION, 'Insert row:', (row, after, before))
        if transaction is None:
            self._pg_begin_transaction()
        try:
            # Jestliže je definováno ordering, které je součástí klíče, bude
            # nově vložený řádek nutně unikátní.
            if (((not self._ordering or (self._ordering[0] not in [c.id() for c in self.key()])) and
                 self._pg_already_present(row, transaction=transaction))):
                msg = 'Row with this key already exists'
                result = msg, False
                log(ACTION, msg)
            else:
                positioned = after or before
                if after:
                    neighbor = after = self.row(after)
                elif before:
                    neighbor = before = self.row(before)
                if positioned and (not neighbor):
                    msg = 'Given neighbor row not found'
                    log(ACTION, msg, (after, before))
                    result = msg, False
                else:
                    r = self._pg_insert(row, after=after, before=before, transaction=transaction)
                    result = r, True
        except Exception:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except Exception:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        if result[1]:
            log(ACTION, 'Row inserted:', result)
        return result

    def update(self, key, row, transaction=None):
        key = xtuple(key)
        log(ACTION, 'Update row:', key)
        log(ACTION, 'New data:', str(row))
        if transaction is None:
            self._pg_begin_transaction()
        try:
            origrow = self.row(key, transaction=transaction)
            if origrow:
                ordering = self._ordering
                if ordering:
                    row = copy.copy(row)
                    for id in ordering:
                        try:
                            row[id] = origrow[id]
                        except KeyError:
                            row.append(id, origrow[id])
                keys = map(ColumnSpec.id, self.key())
                new_key = []
                for i in range(len(keys)):
                    try:
                        v = row[keys[i]]
                    except KeyError:
                        v = key[i]
                    new_key.append(v)
                new_key = tuple(new_key)
                if new_key != key and self._pg_already_present(row):
                    msg = 'Row with given key already exists'
                    result = msg, False
                    log(ACTION, msg, key)
                else:
                    n = self._pg_update(self._pg_key_condition(key), row,
                                        transaction=transaction)
                    if n == 0:
                        result = None, False
                    else:
                        new_row = self.row(new_key, transaction=transaction)
                        result = new_row, True
            else:  # not origrow
                msg = 'Row with given key does not exist'
                result = msg, False
                log(ACTION, msg, key)
        except Exception:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except Exception:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        if result[1]:
            log(ACTION, 'Row updated:', result)
        return result

    def update_many(self, condition, row, transaction=None):
        log(ACTION, 'Update rows:', condition)
        log(ACTION, 'New data:', str(row))
        if transaction is None:
            self._pg_begin_transaction()
        try:
            ordering = self._ordering
            if ordering:
                new_row_items = []
                for k, v in row.items():
                    if k not in ordering:
                        new_row_items.append((k, v))
                row = Row(new_row_items)
            result = self._pg_update(condition, row, transaction=transaction)
        except Exception:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except Exception:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        if result:
            log(ACTION, 'Rows updated:', result)
        return result

    def delete(self, key, transaction=None):
        log(ACTION, 'Delete row:', key)
        if transaction is None:
            self._pg_begin_transaction()
        try:
            result = self._pg_delete(self._pg_key_condition(key),
                                     transaction=transaction)
        except Exception:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except Exception:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        log(ACTION, 'Row deleted:', result)
        return result

    def delete_many(self, condition, transaction=None):
        log(ACTION, 'Delete rows:', condition)
        if transaction is None:
            self._pg_begin_transaction()
        try:
            result = self._pg_delete(condition, transaction=transaction)
        except Exception:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except Exception:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        log(ACTION, 'Rows deleted:', result)
        return result

    def refresh(self):
        """Refresh materialized view.

        This method may be invoked only on materialized views.

        """
        self.close()
        self._pg_query(self._pdbb_command_refresh)

    # Locking

    def lock_row(self, key, transaction=None):
        """Lock row with the given key.

        Arguments:

          key -- key of the row to lock
          transaction -- transaction object representing the transaction in
            which the row is locked

        The lock is automatically released when the transaction is closed
        (whether by commit or rollback).

        """
        if is_sequence(key):
            key = key[0]
        log(EVENT, 'Locking row:', str(key))
        self._pg_query(_Query('savepoint _lock'), transaction=transaction)
        try:
            command = self._pdbb_command_lock()
            if command:         # special locking command necessary
                command = command.update(dict(key=key))
                result = self._pg_query(command, transaction=transaction)
            else:
                result = self._pg_row(key, None, transaction=transaction,
                                      supplement='for update nowait')
            self._pg_query(_Query('release _lock'), transaction=transaction)
            if not result:
                return "No such record"
        except DBLockException:
            log(EVENT, 'Row already locked by another process')
            self._pg_query(_Query('rollback to _lock'), transaction=transaction)
            return "Record locked by another process"
        log(EVENT, 'Row locked')
        return None


class DBPostgreSQLCounter(PostgreSQLConnector, Counter):
    """Čítač uložený v PostgreSQL."""

    def __init__(self, name, connection_data, **kwargs):
        """Initialize the instance.

        Arguments:
          name -- identifier of the counter in the database as a string
          connection_data -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'
          kwargs -- passed to 'PostgreSQLConnector' constructor.

        """
        assert is_anystring(name)
        PostgreSQLConnector.__init__(self, connection_data, **kwargs)
        self._name = name
        self._query = _Query("select nextval('%s')" % name)

    def next(self, transaction=None):
        result = self._pg_query(self._query, transaction=transaction)
        try:
            number = int(result[0][0])
        except Exception as e:
            raise pytis.data.DBException(_(u"Invalid database counter value"), e)
        return number


class DBPostgreSQLFunction(Function, DBDataPostgreSQL,
                           PostgreSQLStandardBindingHandler):
    """PostgreSQL implementation of the 'Function' class."""

    def __init__(self, name, connection_data, result_columns=None, **kwargs):
        """
        Arguments:

          name -- name of the function (string) or database specification class
            corresponding to the function
          connection_data -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'
          result_columns -- sequence of 'ColumnSpec' instances describing the result
            rows; if 'None', columns and their types are determined
            automatically but only if it is possible and supported
          kwargs -- forwarded to successors

        """
        from pytis.data.gensqlalchemy import SQLFunctional
        assert isinstance(name, basestring) or issubclass(name, SQLFunctional), name
        db_spec = name
        if isinstance(name, basestring):
            candidates = list(pytis.data.gensqlalchemy.specifications_by_name(name))
            if len(candidates) == 1 and issubclass(candidates[0], SQLFunctional):
                db_spec = candidates[0]
        if isinstance(db_spec, basestring):
            self._name = name
            db_spec = None
        else:
            self._name = db_spec.pytis_name(real=True)
            if result_columns is None:
                result_columns = db_spec.result_type
                if result_columns is None:
                    result_columns = []
                elif not isinstance(result_columns, (tuple, list)):
                    if isinstance(result_columns, Type):
                        result_columns = [ColumnSpec('_result', result_columns)]
                    elif isinstance(result_columns, ColumnSpec):
                        result_columns = [result_columns]
                    elif result_columns == pytis.data.gensqlalchemy.SQLFunctional.RECORD:
                        result_columns = [c for c in db_spec.arguments if c.out()]
                    elif hasattr(result_columns, 'fields'):
                        result_columns = result_columns.fields
                    else:
                        result_columns = None
                        db_spec = None
        self._pdbb_result_columns = result_columns
        bindings = ()
        super(DBPostgreSQLFunction, self).__init__(
            bindings=bindings, key=bindings, connection_data=connection_data, db_spec=db_spec,
            **kwargs)
        if self._pdbb_db_spec is None:
            arg_query = _Query("select proargtypes from pg_proc where proname=%(name)s",
                               dict(name=sval(name)))
            data = self._pg_query(arg_query, outside_transaction=True)
            arguments = string.join(['%%(__farg%d)s' % (i,)
                                     for i in range(len(string.split(data[0][0])))], ', ')
        else:
            def arg_spec(arg):
                return '%%s' if isinstance(arg, Binary) else '%s'
            arguments = string.join(['%%(__farg%d)s' % (i,)
                                     for i in range(len(self._pdbb_db_spec.arguments))
                                     if not self._pdbb_db_spec.arguments[i].out()],
                                    ', ')
        self._pdbb_function_call = 'select * from %s(%s)' % (self._name, arguments)

    def _db_bindings_to_column_spec(self, __bindings):
        if self._pdbb_result_columns is not None:
            return self._pdbb_result_columns, ()
        if self._pdbb_db_spec is not None:
            columns = [ColumnSpec(b.id(), b.type()) for b in self._bindings]
        else:
            schema, name = self._pdbb_split_function_name(self._name)
            type_query = _Query("select proretset, prorettype, proargtypes "
                                "from pg_proc, pg_namespace "
                                "where pg_proc.pronamespace = pg_namespace.oid and "
                                "pg_namespace.nspname = %(schema)s and proname = %(name)s",
                                dict(schema=sval(schema), name=sval(name)))
            self._pg_begin_transaction()
            try:
                data = self._pg_query(type_query)
                assert data, ('No such function', self._name)
                assert len(data) == 1, ('Overloaded functions not supported', self._name)
                r_set, r_type, arg_types = data[0]

                def type_instances(tnum):
                    query = _Query("select typname, nspname, typlen, typtype "
                                   "from pg_type join "
                                   "pg_namespace on typnamespace = pg_namespace.oid "
                                   "where pg_type.oid = %(oid)s",
                                   dict(oid=ival(tnum)))
                    data = self._pg_query(query)
                    type_, type_ns, size_string, t_type = data[0]
                    if t_type == 'b':
                        instances = [self._pdbb_get_type(type_, size_string)]
                    elif t_type == 'c':
                        table = '%s.%s' % (type_ns, type_,)
                        table_data = self._pdbb_get_table_column_data(table)
                        instances = [self._pdbb_get_table_type(table, i)
                                     for i in range(len(table_data.basic()))]
                    elif type_ == 'void':
                        instances = []
                    else:
                        raise Exception(("Unsupported function return type, "
                                         "use explicit column specification:"),
                                        '%s.%s' % (type_ns, type_,))
                    return instances
                r_type_instances = type_instances(r_type)
                columns = [ColumnSpec('column%d' % (i + 1,), r_type_instances[i])
                           for i in range(len(r_type_instances))]
            finally:
                self._pg_commit_transaction()
        return columns, ()

    def _pdbb_create_sql_commands(self):
        self._pg_notifications = []

    def call(self, row, transaction=None):
        log(EVENT, 'Function call:', self._name)
        args = dict([('__farg%d' % (i,), row[i]) for i in range(len(row))])
        if transaction is None:
            outside_transaction = True
        else:
            outside_transaction = False
        data = self._pg_query(_Query(self._pdbb_function_call, args),
                              transaction=transaction,
                              outside_transaction=outside_transaction)
        if transaction is None:
            self._pg_query(_Query('commit'), outside_transaction=outside_transaction)
        result = [self._pg_make_row_from_raw_data([r]) for r in data]
        log(EVENT, 'Function call result:', (self._name, result))
        return result


class DBPostgreSQLTransaction(DBDataPostgreSQL):
    """User transaction.

    By creating an instance of this class new transaction is started.  You can
    tell 'DBDataPostgreSQL' methods to be invoked inside the transaction by
    giving the instance as their 'transaction' argument.

    The transaction is finished by calling one of its 'commit' or 'rollback'
    methods.  After that the transaction may not be used any longer.

    You can also use partial rollbacks by setting transaction points with the
    'set_point' method and calling the 'cut' method to rollback to a previously
    set transaction point.

    """

    REPEATABLE_READ = 'repeatable read'

    _watched_transactions = WeakSet()
    _trans_last_check = {}
    _trans_check_interval = None

    def __init__(self, connection_data, isolation=None, read_only=False, timeout_callback=None,
                 ok_rollback_closed=False, **kwargs):
        """
        Arguments:

          connection_data -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'
          isolation -- transaction isolation level, either 'None' (default
            isolation level, i.e. read commited) or 'REPEATABLE_READ' constant of
            this class (repeatable read isolation level)
          read_only -- whether the transaction is read-only; boolean
          timeout_callback -- function to be called on transaction timeout or 'None';
            the function is called with no arguments
          ok_rollback_closed -- iff true, don't complain about rollbacking
            closed transactions

        """
        super(DBPostgreSQLTransaction, self).__init__(
            bindings=(), key=(), connection_data=connection_data,
            **kwargs)
        if timeout_callback is None:
            import config
            if config.debug:
                def timeout_callback(self=self):
                    log(DEBUG, "Unhandled transaction timeout",
                        self._trans_connection().connection_info('transaction_commands'))
        self._trans_timeout_callback = timeout_callback
        self._trans_notifications = []
        self._pg_begin_transaction(isolation=isolation, read_only=read_only)
        self._open = True
        assert isinstance(ok_rollback_closed, bool), ok_rollback_closed
        self._ok_rollback_closed = ok_rollback_closed
        if timeout_callback is not None:
            self._pid = os.getpid()
            DBPostgreSQLTransaction._watched_transactions.add(self)

    def __del__(self):
        if self._open:
            try:
                self._pg_close_transaction()
            except Exception:
                pass

    def _db_bindings_to_column_spec(self, __bindings):
        return (), ()

    def _pdbb_create_sql_commands(self):
        self._pdbb_command_isolation = _Query('set transaction isolation level %(isolation)s'
                                              '%(read_only)s')

    def _trans_connection(self):
        return self._pg_get_connection()[0]

    def _trans_notify(self, dbdata):
        self._trans_notifications.append(dbdata)

    def commit(self):
        """Commit the transaction."""
        if not self._open:
            log(EVENT, "Attempt to commit closed transaction")
            return
        self._pg_commit_transaction()
        self._open = False
        for dbdata in self._trans_notifications:
            dbdata._pg_send_notifications()

    def rollback(self):
        """Rollback the transaction."""
        if not self._open:
            if not self._ok_rollback_closed:
                log(EVENT, "Attempt to rollback closed transaction")
            return
        self._pg_rollback_transaction()
        self._open = False

    def set_point(self, point):
        """Set transaction point for possible future partial rollback.

        Arguments:

          point -- string containing only lowercase English letters defining
            the transaction point

        """
        assert re.match('^[a-z]+$', point)
        self._pg_query(_Query('savepoint %s' % (point,)), transaction=self)

    def cut(self, point):
        """Rollback the transaction to the given point.

        Arguments:

          point -- string containing only lowercase English letters defining
            the transaction point to which the rollback should be performed

        """
        assert re.match('^[a-z]+$', point)
        self._pg_query(_Query('rollback to %s' % (point,)), transaction=self)
        self._pg_query(_Query('release %s' % (point,)), transaction=self)

    def set_read_only(self):
        """Make the transaction read-only.

        Usually, the transaction should be marked as read-only using
        'read_only' constructor argument.  But in some situations, such as with
        cursors calling functions utilizing temporary tables, this is not
        possible and the transaction can be set as read-only only after the
        database modifying operation, using this method.

        """
        self._pg_query(_Query('set transaction read only'), transaction=self)

    def open(self):
        """Return true iff the transaction is open and hasn't been closed yet."""
        return self._open

    def set_max_age(self, moment):
        """Set maximum transaction age.

        Calling the method has the same effect on transaction age as if an SQL
        command was called at 'moment'.

        Arguments:

          moment -- minimum time to set in the same form as 'time.time()'
            result

        """
        c = self._trans_connection()
        value = max(c.connection_info('last_query_time') or 0, moment)
        c.set_connection_info('last_query_time', value)

    @classmethod
    def close_transactions(class_):
        """Check all transactions for timeout.

        Call timeout callbacks for timeouted transactions.

        """
        T = DBPostgreSQLTransaction
        if T._trans_check_interval is None:
            import config
            T._trans_max_time = config.max_transaction_time
            if T._trans_max_time is None:
                T._trans_max_time = 100000000
            T._trans_max_idle_time = config.max_transaction_idle_time
            if T._trans_max_idle_time is None:
                T._trans_max_idle_time = 100000000
            T._trans_check_interval = \
                max(1, min(T._trans_max_time, T._trans_max_idle_time) / 3)
        now = time.time()
        pid = os.getpid()
        if now - T._trans_last_check.get(pid, 0) >= T._trans_check_interval:
            T._trans_last_check[pid] = now
            for t in set(class_._watched_transactions):
                if t._open and t._pid == pid:
                    callback = t._trans_timeout_callback
                    if callback is None:
                        continue
                    c = t._trans_connection()
                    if ((now - c.connection_info('transaction_start_time') > t._trans_max_time or
                         now - c.connection_info('last_query_time') > t._trans_max_idle_time)):
                        try:
                            callback()
                        except Exception:
                            # The callback may crash if the wx class is already inactive.
                            try:
                                class_._watched_transactions.remove(t)
                            except KeyError:
                                pass
