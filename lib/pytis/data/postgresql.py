# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

import collections
import copy
import operator
import re
import string
import thread
import threading
import time
import weakref

import mx.DateTime

from dbdata import *
from evaction import *
from pytis.data import *
import pytis.data


# Modifikace tabulek se oznamuje zasláním notifikace `MODIF_table', kde `table'
# je jméno modifikované tabulky.


def pg_escape(string_):
    return string_.replace("'", "''").replace('\\', '\\\\')


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
        def __init__(self, connection, connection_data):
            """

            Argumenty:

              connection -- spojení do databázového stroje
              connection_data -- specifikace parametrů spojení
              
            """
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
        raise ProgramError(_("Volána neimplementovaná metoda"))

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
        
    def _postgresql_initialize_transactions(self, connection):
        """Nastav způsob provádění transakcí pro konkrétní backend."""
        pass

    def _postgresql_initialize_coding(self, connection):
        # This queries are intentionally run without _pg_query_lock to avoid
        # deadlock on connection reopening in dbapi.py.
        query = 'set client_encoding to "utf-8"'
        self._postgresql_query(connection, query, False)
        query = ("select pg_encoding_to_char(encoding) "
                 "from pg_database where datname = current_database()")
        result = self._postgresql_query(connection, query, False)
        coding = result[0].result().fetchone()[0]
        if coding != 'UTF8':
            self._pg_encoding = coding

    def _postgresql_initialize_search_path(self, connection, schemas):
        if schemas:
            search_path = string.join(schemas, ',')
            if connection.connection_info('search_path') != search_path:
                query = "set search_path to " + search_path
                self._postgresql_query(connection, query, False)
                connection.set_connection_info('search_path', search_path)
        
    def _postgresql_query(self, connection, query, restartable, query_args=()):
        """Perform SQL 'query' and return the result.

        Arguments:

          connection -- '_postgresql_Connection' instance
          query -- string containing the final form of the SQL command to be
            performed
          restartable -- iff this is true, the method may try to restart the
            database connection in case of error
          query_args -- formatting arguments corresponding to 'query'
            containing '%s' formatting marks; this argument should be only used
            when absolutely necessary (such as when working with binary data
            types)

        The return value is a pair ('result', 'connection'), where 'result' is
        a '_postgresql_Result' result and 'connection' a
        '_postgresql_Connection' of the connection that returned the result.

        This method is required to be redefined in a subclass.

        """
        raise ProgramError(_("Unimplemented method"))

    def _postgresql_transform_query_result(self, result):
        """Vrať instanci 'PostgreSQLResult' odpovídající výsledku 'result'.

        Argumenty:

          result -- instance '_postgresql_Result'

        Tato metoda musí být předefinována v podtřídě.
        
        """
        raise ProgramError(_("Volána neimplementovaná metoda"))

    def _postgresql_begin_transaction(self):
        self._pg_query ('begin')
                
    def _postgresql_commit_transaction(self):
        self._pg_query ('commit')
        
    def _postgresql_rollback_transaction(self):
        self._pg_query ('rollback')


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
            self._pdbb_logging_command = \
                "insert into %s (command) values ('%%s')" % config.dblogtable
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
            connection = pool.get(self._pg_connection_data())
        else:
            connection = connections[-1]
        return connection
        
    def _pg_return_connection(self, connection):
        pool = self._pg_connection_pool()
        pool.put_back(connection.connection_data(), connection)

    def _pg_query(self, query, outside_transaction=False, backup=False,
                  query_args=(), transaction=None):
        """Call the SQL 'query' and return the result.

        Arguments:
        
          query -- PostgreSQL SQL command as a string
          outside_transaction -- iff it is true, the query is performed outside
            the current transaction (if there is any)
          backup -- iff it is true, write the completed SQL command into log
          query_args -- formatting arguments corresponding to 'query'
            containing '%s' formatting marks; this argument should be only used
            when absolutely necessary (such as when working with binary data
            types)
          transaction -- transaction object containing the connection to be
            used for performing the query or 'None' (in which case the
            connection is selected automatically); this argument may not be
            used when 'outside_transaction' is true

        The return value is a 'PostgreSQLResult' instance.

        The method must properly handle database exception and in case any is
        caught the corresponding 'DBException' must be raised.
        
        """
        if self._pg_encoding:
            try:
                query.encode(self._pg_encoding)
            except UnicodeEncodeError:
                raise DBUserException(_("Data obsahují znaky, které nelze reprezentovat v kódování databáze"))
        if type(query) is pytypes.UnicodeType:
            query = query.encode('utf-8')
        assert transaction is None or not outside_transaction, \
            'Connection given to a query to be performed outside transaction'
        if transaction is None:
            connection = self._pg_get_connection(outside_transaction)
        elif not transaction.open():
            raise DBUserException("Can't use closed transaction")
        else:
            connection = transaction._trans_connection()
        self._pg_query_counter += 1
        # Proveď dotaz
        if __debug__:
            log(DEBUG, 'SQL query', query)
        def lfunction(connection=connection):
            try:
                self._postgresql_initialize_search_path(connection,
                                                        self._pg_connection_data().schemas())
                result, connection = self._postgresql_query(connection, query,
                                                            outside_transaction,
                                                            query_args=query_args)
            finally:
                # Vrať DB spojení zpět
                if connection and transaction is None and outside_transaction:
                    self._pg_return_connection(connection)                  
            if backup and self._pdbb_logging_command:
                assert not outside_transaction, \
                    ('Backed up SQL command outside transaction', query)
                # Zde nemůže dojít k významné záměně pořadí zalogovaných
                # příkazů, protože všechny DML příkazy jsou uzavřeny
                # v transakcích a ty konfliktní jsou díky serializaci
                # automaticky správně řazeny.
                self._postgresql_query(connection,
                                       (self._pdbb_logging_command %
                                        (pg_escape(query),)),
                                       False)
            # Získej a vrať data
            return self._postgresql_transform_query_result(result)
        data = with_lock(self._pg_query_lock, lfunction)
        if __debug__:
            log(DEBUG, 'SQL query result', data)
        return data


class PostgreSQLUserGroups(PostgreSQLConnector):
    """Třída pro zjišťování skupin uživatele."""
    
    _access_groups = {}
    _access_groups_data_objects = {}
    _logical_access_groups = UNDEFINED

    def __init__ (self, *args, **kwargs):
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
        return (connection_data.user(), connection_data.password(),
                connection_data.host(), connection_data.port(),
                connection_data.database(),)
        
    def _pgg_retrieve_access_groups(self, data):
        if __debug__:
            log(DEBUG, "Retrieving list of user groups")
        d = data._pg_query("select rolname from pg_roles where pg_has_role(rolname, 'member')",
                           outside_transaction=True)
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
            # Check for ev_pytis_user_roles presence first, to prevent logging
            # error messages in non-DMP applications
            tables = dbtable('pg_catalog.pg_class', ('relname',), connection_data,
                             connection_name=self._connection_name)
            roles_data = None
            try:
                if tables.select(condition=EQ('relname', Value(String(), 'ev_pytis_user_roles'))) > 0:
                    try:
                        roles_data = dbtable('ev_pytis_user_roles', ('roleid',), connection_data)
                    except pytis.data.DBException:
                        pass
                    else:
                        def process(row):
                            return row[0].value()
                        logical_access_groups = roles_data.select_map(process)
                        PostgreSQLUserGroups._logical_access_groups = logical_access_groups
            finally:
                try:
                    tables.close()
                except:
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
                log(DEBUG, 'Vytvoření notifikátoru')
            PostgreSQLConnector.__init__(self, connection_data,
                                         connection_name=connection_name)
            self._notif_data_lock = thread.allocate_lock()
            self._notif_data_objects = weakref.WeakKeyDictionary()
            self._notif_connection_lock = thread.allocate_lock()
            thread.start_new_thread(self._notif_listen, ())

        def _notif_do_registration(self, notification):
            self._pg_query('listen "%s"' % notification)
            
        def _notif_register(self, notification):
            # Zamykáme zde kvůli možnosti současného vyvolání této metody
            # z `register' i naslouchacího threadu.
            if __debug__:
                log(DEBUG, 'Registruji notifikaci:', notification)
            def lfunction():
                self._notif_init_connection()
                self._notif_do_registration(notification)
            with_lock(self._notif_connection_lock, lfunction)
            if __debug__:
                log(DEBUG, 'Notifikace zaregistrována:', notification)

        def _notif_listen(self):
            if __debug__:
                log(DEBUG, 'Nový listener')
            error_pause = 1
            self._notif_init_connection()
            while True:
                if __debug__:
                    log(DEBUG, 'Napichuji se na nové spojení')
                notiflist = []
                for d in self._notif_data_objects.values():
                    notiflist = notiflist + d
                if __debug__:
                    log(DEBUG, 'Notifikace k registraci:', notiflist)                    
                notiflist = reduce(lambda x, y: x + y,
                                   self._notif_data_objects.values(), [])
                try:
                    # connection do poolu nikdy nevracíme, takže na něj můžeme
                    # navěsit, co je nám libo.
                    for n in remove_duplicates(notiflist):
                        self._notif_register(n)
                except pytis.data.DBException, e:
                    time.sleep(error_pause)
                    error_pause = error_pause * 2
                    continue
                self._notif_listen_loop()

        def _notif_listen_loop(self):
            raise Exception("Volána neimplementovaná metoda")

        def _notif_invoke_callbacks(self, notifications):
            if __debug__:
                log(DEBUG, 'Volám callbacky')
            def lfunction():
                return copy.copy(self._notif_data_objects)
            data_objects = with_lock(self._notif_data_lock, lfunction)
            for d, ns in data_objects.items():
                for n in ns:
                    if n in notifications:
                        if __debug__:
                            log(DEBUG, 'Volám callbacky datového objektu:', d)
                        d._call_on_change_callbacks()
                        break

        def register_notification(self, data, notification):
            if __debug__:
                log(DEBUG, 'Registruji notifikaci:', notification)
            notification = notification.lower()
            def lfunction():
                try:
                    notifications = self._notif_data_objects[data]
                except KeyError:
                    self._notif_data_objects[data] = notifications = []
                notifications.append(notification)
            with_lock(self._notif_data_lock, lfunction)
            self._notif_register(notification)
            if __debug__:
                log(DEBUG, 'Notifikace zaregistrována')

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
        key = self._pg_notifier_key(spec)
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

    class _SQLCommandTemplate(object):
        def __init__(self, template, arguments={}):
            self._template = template
            self._arguments = arguments
        def format(self, arguments):
            args = copy.copy(arguments)
            for k, v in self._arguments.items():
                if k not in args:
                    args[k] = v
            if isinstance(self._template, collections.Callable):
                self._template = self._template()
            return self._template % args

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
                 **kwargs):
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
            return self._pdbb_get_table_type(binding.table(), binding.column(), binding.type(),
                                             type_kwargs=binding.kwargs(), noerror=True)
        result = None
        if operations is not None:
            for aggregate, id_, name in operations:
                if name == binding.id():
                    result = '%s(%s) as %s' % (self._pg_aggregate_name(aggregate), binding.column(), name,)
                    break
        if column_groups is not None:
            for g in column_groups:
                name = g[0]
                if name == binding.id():                
                    function_name = g[2]
                    if function_name is not None:
                        call = self._pdbb_column_group_call(g)
                        result = '%s as %s' % (call, name,)
                    break
        if result is None:
            column_name = binding.column()
            column_id = binding.id()
            if full_text_handler is not None and isinstance(binding.type(), FullTextIndex):
                result = full_text_handler(binding)
            elif convert_ltree and isinstance(column_type(), LTree):
                # In order to make Czech sorting working on ltrees, we must do some
                # ugly things...
                result = ("replace(%s::text, '.', chr(160))" %
                          (self._pdbb_tabcol(binding.table(), column_name, column_id),))
                result = self._pdbb_tabcol(binding.table(), column_name, column_id) + '::text'
            else:
                result = self._pdbb_tabcol(binding.table(), column_name, column_id)
        return result
        
    def _pdbb_coalesce(self, ctype, value):
        """Vrať string 'value' zabezpečený pro typ sloupce 'ctype' v SQL."""
        if ctype is None or isinstance(ctype, String) or value == 'NULL':
            default = "''"
            cast = ''
        elif isinstance(ctype, Float):
            default = '0'
            cast = '::numeric'                
        elif isinstance(ctype, Number):
            default = '0'
            cast = ''
        elif isinstance(ctype, Time):
            default = "'00:00:01'"
            cast = '::time'
        elif isinstance(ctype, Date):
            default = "'0000-01-01'"
            cast = '::date'
        elif isinstance(ctype, DateTime):
            default = "'0000-01-01'"
            cast = '::timestamp'
        elif isinstance(ctype, Boolean):
            default = "'F'"
            cast = '::bool'
        else:
            default = "''"
            cast = ''
        #return 'coalesce(%s%s, %s%s)' % (value, cast, default, cast)
        return '%s%s' % (value, cast)

    def _pdbb_split_object_name(self, obj, schema_dict, object_table, object_label):
        items = obj.split('.')
        if len(items) < 2:
            if schema_dict.get(obj) is None:
                schemas = self._pg_connection_data().schemas()
                if not schemas:
                    schemas = ['public', 'pg_catalog']
                if len(schemas) == 1:
                    schema_dict[obj] = schemas[0]
                else:
                    for s in schemas:
                        query = (("select %(table)s.%(label)sname from %(table)s, pg_namespace "
                                  "where %(table)s.%(label)snamespace = pg_namespace.oid and "
                                  "%(table)s.%(label)sname='%(name)s' and "
                                  "pg_namespace.nspname='%(namespace)s'") %
                                 dict(table=object_table, label=object_label, name=obj, namespace=s))
                        if self._pg_query(query, outside_transaction=True):
                            schema_dict[obj] = s
                            break
                    else:
                        schema_dict[obj] = 'public'
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
        d = self._pg_query(
            ("select pg_attribute.attname, pg_type.typname, pg_attribute.atttypmod, "
             "pg_attribute.attnotnull "
             "from pg_class, pg_attribute, pg_type, pg_namespace "
             "where pg_class.oid = pg_attribute.attrelid and "
             "pg_class.relnamespace = pg_namespace.oid and "
             "pg_namespace.nspname = '%s' and "
             "pg_class.relname = '%s' and "
             "pg_attribute.atttypid = pg_type.oid and "
             "pg_attribute.attnum > 0") % \
            (schema, table_name,),
            outside_transaction=True)
        d1 = self._pg_query(
            ("select pg_attribute.attname, pg_attrdef.adsrc "
             "from pg_class, pg_attribute, pg_attrdef, pg_namespace "
             "where pg_class.oid = pg_attrdef.adrelid and "
             "pg_class.relnamespace = pg_namespace.oid and "
             "pg_namespace.nspname = '%s' and "
             "pg_class.oid = pg_attribute.attrelid and "
             "pg_class.relname = '%s' and "
             "pg_attribute.attnum = pg_attrdef.adnum and "
             "pg_attribute.attnum > 0") % \
            (schema, table_name,),
            outside_transaction=True)
        d2 = self._pg_query(
            ("select attname, conkey "
             "from pg_constraint, pg_namespace, pg_class, pg_attribute "
             "where conrelid = pg_class.oid and attrelid = pg_class.oid and relnamespace = pg_namespace.oid and attnum = any (conkey) and "
             "nspname = '%s' and relname = '%s' and (contype = 'p' or contype = 'u') and "
             "pg_attribute.attnum > 0") %
            (schema, table_name,),
            outside_transaction=True)
        table_data = self._TableColumnData(d, d1, d2)
        table_key = self._pdbb_unique_table_id(table)
        PostgreSQLStandardBindingHandler._pdbb_table_column_data[table_key] = table_data
        return table_data
        
    def _pdbb_get_table_type(self, table, column, ctype=None, type_kwargs=None, noerror=False):
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
        except:
            if noerror:
                return None
            raise pytis.data.DBException("Unknown column '%s' in table '%s'" % (column, table),
                                         None, table, column)
        try:
            default = lookup_column(table_data.default())[0]
        except:
            default = ''
        try:
            # TODO: This is a quick hack to ignore multicolumn unique constraints. (TC)
            row = lookup_column(table_data.unique())
            unique = (not not row) and row[0].find(',') == -1
        except:
            unique = False
        serial = (default[:len('nextval')] == 'nextval')
        return self._pdbb_get_type(type_, size_string, not_null, serial,
                                   ctype=ctype, unique=unique, type_kwargs=type_kwargs)
    
    def _pdbb_get_type(self, type_, size_string, not_null, serial,
                       ctype=None, unique=False, type_kwargs=None):
        # Zde lze doplnit další používané standardní typy z PostgreSQL
        TYPE_MAPPING = {'bool': Boolean,
                        'bpchar': String,
                        'char': String,
                        'date': Date,
                        'time': Time,
                        'smallint': Integer,
                        'bigint': Integer,
                        'int2': Integer,
                        'int4': Integer,
                        'int8': Integer,
                        'numeric': Float,
                        'float4': Float,
                        'float8': Float,
                        'name': String,
                        'text': String,
                        'timestamp': DateTime,
                        'timestamptz': DateTime,
                        'interval': TimeInterval,
                        'tsvector': FullTextIndex,
                        'varchar': String,
                        'ltree': LTree,
                        'inet': Inet,
                        'macaddr': Macaddr,
                        'bytea': Binary,
                        'oid': pytis.data.Oid, # for backward compatibility
                        }
        try:
            type_class_ = TYPE_MAPPING[type_]
        except KeyError:
            raise pytis.data.DBException('Unhandled database type', None, type_)
        if ctype is None or type(ctype) == type(pytis.data.Type):
            if type_kwargs is None:
                type_kwargs = {}
            if not_null in (1, 'T') and 'not_null' not in type_kwargs:
                type_kwargs['not_null'] = True
            if 'unique' not in type_kwargs and type_class_ != Boolean:
                type_kwargs['unique'] = unique
            if type_class_ == String and 'maxlen' not in type_kwargs:
                if type_ != 'text':
                    try:
                        size = int(size_string) - 4
                    except:
                        size = None
                    if size < 0:
                        size = None
                    type_kwargs['maxlen'] = size
            elif type_class_ == Float and 'precision' not in type_kwargs:
                spec = int(size_string)
                precision = (spec & 0xFFFF) - 4
                if precision < 0 or precision > 100:
                    precision = None
                type_kwargs['precision'] = precision
            elif type_class_ == Integer and serial:
                type_class_ = Serial
            if ctype:
                assert issubclass(ctype, type_class_), \
                       ("User type doesn't match DB type", ctype, type_class_)
                type_class_ = ctype
            result = type_class_(**type_kwargs)
        else:
            if __debug__:
                if isinstance(ctype, TimeInterval) and type_class_ == Time: # temporary hack
                    pass
                else:
                    assert isinstance(ctype, type_class_), \
                           ("User type doesn't match DB type", ctype, type_class_)
            result = ctype
        return result

    def _db_bindings_to_column_spec(self, bindings):
        key = []
        columns = []
        for b in bindings:
            if not b.id():              # skrytý sloupec
                continue
            if self._arguments is None:
                t = self._pdbb_get_table_type(b.table(), b.column(), b.type(), type_kwargs=b.kwargs())
            else:
                t = b.type()
                assert t is not None, ("Column types must be specified for table functions",
                                       b.id(), b.table(),)
                if type(t) == type(Type):
                    t = t(**b.kwargs())
            colspec = ColumnSpec(b.id(), t)
            columns.append(colspec)
            if b in self._key_binding:
                assert not isinstance(t, Binary), ("Binary types may not be "+
                                                   "used as keys")
                key.append(colspec)
        assert key, DBUserException('data key column not found')
        # Hotovo
        return columns, tuple(key)

    def _pdbb_sql_column_list_from_names(self, column_names, full_text_handler=None, operations=None,
                                         column_groups=None):
        bindings = [self._db_column_binding(name) for name in column_names]
        if column_groups:
            column_groups = [g for g in column_groups if g[0] in column_names]
        return self._pdbb_sql_column_list(bindings, full_text_handler, operations=operations,
                                          column_groups=column_groups)
        
    def _pdbb_sql_column_list(self, bindings, full_text_handler=None, operations=None, column_groups=None):
        column_names = [self._pdbb_btabcol(b, full_text_handler=full_text_handler, operations=operations,
                                           column_groups=column_groups)
                        for b in bindings if b is not None and b.id()]
        return string.join(column_names, ', ')
    
    def _pdbb_full_text_handler(self, binding):
        indexed_columns = binding.type().columns()
        if indexed_columns:
            indexed_columns_list = []
            for name in indexed_columns:
                sql_name = self._pdbb_btabcol(self._db_column_binding(name))
                indexed_columns_list.append("coalesce(%s,'')" % (sql_name,))
            text = string.join(indexed_columns_list, "||' * '||")
            query_name = self._pdbb_fulltext_query_name(binding.column())
            result = 'ts_headline(%s,%s)' % (text, query_name,)
        else:
            result = "''"
        return result
    
    def _pdbb_column_group_call(self, group):
        args = []
        for a in group[3:]:
            if isinstance(a, basestring):
                args.append(a)
            elif isinstance(a, Value):
                args.append(self._pg_value(a))
            else:
                raise ProgramError("Invalid group function argument", group, a)
        return '%s(%s)' % (group[2], string.join(args, ', '),)
        
    def _pdbb_create_sql_commands(self):
        """Vytvoř šablony SQL příkazů používané ve veřejných metodách."""
        bindings = self._bindings
        for b in bindings:
            assert isinstance(b, DBColumnBinding), \
                   ('Unsupported binding specification', b)
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
                        b = DBColumnBinding(name, '', b.column(), type_=type_)
                        self._bindings = bindings = bindings + (b,)
                        filtered_bindings.append(b)
                        self._columns = self._columns + (ColumnSpec(name, type_),)
            for g in function_column_groups:
                name, type_ = g[0], g[1]
                self._columns = self._columns + (ColumnSpec(name, type_),)
                b = DBColumnBinding(name, '', name, type_=type_)
                self._bindings = bindings = bindings + (b,)
                filtered_bindings.append(b)
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
            groupby = 'group by %s' % (string.join(groupby_columns, ','))
        else:
            groupby = ''
        table_names = [b.table() for b in bindings if b.table()]
        table_names = remove_duplicates(table_names)
        if self._arguments is not None:
            assert len(table_names) == 1, "Only single tables supported for table functions"
            table_names[0] = (table_names[0] +
                              ('(%s)' % string.join (['%%(__arg_%d)s' % (i+1,)
                                                      for i in range(len(self._arguments))],
                                                     ', ')))
        table_list = string.join(table_names, ', ')
        if len(table_names) <= 1:
            relation = 'true'
        else:
            rels = ['%s=%s' % (self._pdbb_btabcol(b),
                               self._pdbb_btabcol(b.related_to()))
                    for b in bindings if b.related_to()]
            relation = ' and '.join(rels)
        main_table = self._key_binding[0].table()
        schema, main_table_name = self._pdbb_split_table_name(main_table)
        if self._arguments is None:
            main_table_from = main_table
        else:
            main_table_from = table_names[0]
        keytabcols = [self._pdbb_btabcol(b) for b in self._key_binding]
        assert len (keytabcols) == 1, ('Multicolumn keys no longer supported', keytabcols)
        first_key_column = keytabcols[0]
        key_cond = '%s=%%(key)s' % (first_key_column,)
        if self._distinct_on:
            distinct_columns_string = self._pdbb_sql_column_list_from_names(self._distinct_on)
            distinct_on = " DISTINCT ON (%s)" % (distinct_columns_string,)
            distinct_on_ordering = distinct_columns_string + ', '
        else:
            distinct_on = ''
            distinct_on_ordering = ''
        sort_exclude = aggregate_columns + [g[0] for g in function_column_groups]
        def sortspec(dir):
            items = []
            bindings = [b for b in self._key_binding
                        if b in filtered_bindings and b.id() not in aggregate_columns]
            if not bindings:
                bindings = filtered_bindings
            for b in bindings:
                if b.id() not in sort_exclude:
                    items.append('%s %s' % (self._pdbb_btabcol(b, convert_ltree=True), dir,))
            for g in function_column_groups:
                items.append('%s %s' % (self._pdbb_column_group_call(g), dir,))
            # TODO: items may still be empty (if only aggregates are present in the result columns)
            return string.join(items, ',')
        ordering = sortspec('ASC')
        rordering = sortspec('DESC')
        condition = key_cond
        relation_and_condition = '(%s) and (%s)' % (relation, condition)
        if self._condition is None:
            filter_condition = ''
        else:
            filter_condition = ' and (%s)' % (self._pdbb_condition2sql(self._condition),)
            filter_condition = filter_condition.replace('%', '%%')
        def make_lock_command():
            qresult = self._pg_query(
                "select relkind from pg_class join pg_namespace on (pg_class.relnamespace = pg_namespace.oid) where nspname='%s' and relname='%s'" %
                (schema, main_table_name,),
                outside_transaction=True)
            if qresult[0][0] == 'r':
                return ''
            else:
                qresult = self._pg_query(
                    ("select definition from pg_views where schemaname = '%s' and viewname = '%s'" %
                     (schema, main_table_name,)),
                    outside_transaction=True)
                assert len(qresult) == 1, (schema, main_table_name,)
                lock_query = qresult[0][0]
                if lock_query[-1] == ';':
                    lock_query = lock_query[:-1]
                # There are some issues with locking views:
                # - There is a PostgreSQL bug preventing locking views which are
                #   built on top of other views.
                # - It's not possible to lock views using LEFT OUTER JOIN (this is
                #   a PostgreSQL feature).
                # Both the problems can be solved by using FOR UPDATE OF version of
                # the locking clause.  But first we need to know what may be put
                # after OF without causing a database error.
                qresult = self._pg_query(
                    ("select ev_action from "
                     "pg_rewrite join pg_class on (pg_rewrite.ev_class = pg_class.oid) "
                     "join pg_namespace on (pg_class.relnamespace = pg_namespace.oid) "
                     "where nspname='%s' and relname='%s'") % (schema, main_table,),
                    outside_transaction=True)
                ev_action_string = qresult[0][0]
                ev_action = pg_parse_ev_action(ev_action_string)
                ev_rtable = ev_action[0]['rtable']
                lock_candidates = [table['eref']['aliasname']
                                   for table in ev_rtable if table['inFromCl']]
                def check_candidate(candidate_table):
                    try:
                        self._pg_query("%s for update of %s nowait limit 1" %
                                       (lock_query, candidate_table,),
                                       outside_transaction=True)
                        return True
                    except DBLockException:
                        return True
                    except DBUserException:
                        return False
                lock_tables = [c for c in lock_candidates if check_candidate(c)]
                def find_real_key():
                    keyname = first_key_column.split('.')[-1]
                    for colspec in ev_action[0]['targetList']:
                        if colspec['resname'] == keyname:
                            break
                    else:
                        return None
                    table = colspec['resorigtbl']
                    column = colspec['resorigcol']
                    qresult = self._pg_query(("select relname, attname from pg_class join pg_attribute on (attrelid=pg_class.oid) "+
                                              "where pg_class.oid=%s and pg_attribute.attnum=%s")
                                             % (table, column,),
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
                    matches = [m for m in re.finditer(' where ', lock_query, re.I)]
                    if matches:
                        match = matches[-1]
                    else:
                        match = None 
                    if match:
                        beg, end = match.span()
                        n = 0
                        for char in lock_query[end:]:
                            if char == '(':
                                n = n - 1
                            elif char == ')':
                                n = n + 1
                        if n > 0:
                            match = None
                    if match:
                        lock_query = lock_query[:beg] + ' where ' + limit_clause + ' and ' + lock_query[end:]
                    else:
                        lock_query = lock_query + ' where ' + limit_clause
                    result = ("%s for update of %s nowait" % (lock_query, lock_tables_string,))
                else:
                    log(EVENT, "Unlockable view, won't be locked:", main_table)
                    result = '%s limit 1' % (lock_query,)
                return result
        self._pdbb_command_lock = self._SQLCommandTemplate(make_lock_command)
        # We make all cursors names unique to avoid conflicts with codebooks
        # when using cross-class transactions and additionally to avoid
        # conflicts when using data instance cache.
        cursor_name = '%s_%%(selection)s' % (self._PDBB_CURSOR_NAME,)
        # Vytvoř šablony příkazů
        self._pdbb_command_row = \
          self._SQLCommandTemplate(
            ('select %%(columns)s from %s where %s%s %s order by %s %%(supplement)s' %
             (table_list, relation_and_condition, filter_condition, groupby, ordering,)),
            {'columns': column_list, 'supplement': '', 'condition': 'true'})            
        if distinct_on or self._pdbb_operations is not None:
            if groupby:
                count_column = groupby_columns[0]
            else:
                count_column = first_key_column
            if self._pdbb_operations:
                self._pdbb_command_count = \
                    "select count(*) from (select%s * from (select %s from %s%%(fulltext_queries)s where (%s)%s %s) as %s where %%(condition)s) as __count" % \
                    (distinct_on, column_list, table_list, relation, filter_condition, groupby, table_names[0],)
            else:
                self._pdbb_command_count = \
                    "select count(*) from (select%s %s from %s%%(fulltext_queries)s where %%(condition)s and (%s) %s%s) as %s" % \
                    (distinct_on, count_column, table_list, relation, filter_condition, groupby, table_names[0],)
        else:
            if self._arguments is None:
                count_column = first_key_column
            else:
                count_column = '*'
            self._pdbb_command_count = \
                'select count(%s) from %s%%(fulltext_queries)s where %%(condition)s and (%s)%s' % \
                (count_column, table_list, relation, filter_condition,)
        self._pdbb_command_distinct = \
          'select distinct %%s from %s where %%s and (%s)%s order by %%s' % \
          (table_list, relation, filter_condition,)
        self._pdbb_command_fetch_last = \
            self._SQLCommandTemplate('fetch last from %s' % (cursor_name,))
        self._pdbb_command_move_to_start = \
            self._SQLCommandTemplate('move absolute 0 from %s' % (cursor_name,))
        if self._pdbb_operations:
            self._pdbb_command_select = \
                self._SQLCommandTemplate(
                (('declare %s scroll cursor for select *, '+
                  'row_number() over () as _number '+
                  'from (select%s %%(columns)s from %s where (%s)%s %s order by %s%%(ordering)s %s) as %s '+
                  '%%(fulltext_queries)s where %%(condition)s') %
                 (cursor_name, distinct_on, table_list, relation, filter_condition,
                  groupby, distinct_on_ordering, ordering, table_names[0],)),
                {'columns': column_list})
        elif distinct_on:
            self._pdbb_command_select = \
                self._SQLCommandTemplate(
                (("declare %s scroll cursor for select %%(columns)s, "
                  "row_number() over (order by %s%%(ordering)s %s) as _number "
                  "from (select%s * from %s%%(fulltext_queries)s "
                  "where %%(condition)s and (%s)%s) "
                  "as %s %s order by %s%%(ordering)s %s") %
                 (cursor_name, distinct_on_ordering, ordering, distinct_on, table_list, relation, filter_condition,
                  table_names[0], groupby, distinct_on_ordering, ordering,)),
                {'columns': column_list})
        else:
            self._pdbb_command_select = \
                self._SQLCommandTemplate(
                (("declare %s scroll cursor for select %%(columns)s, "
                  "row_number() over (order by %s%%(ordering)s %s) as _number "
                  "from %s%%(fulltext_queries)s "
                  "where %%(condition)s and (%s)%s %s order by %s%%(ordering)s %s") %
                 (cursor_name, distinct_on_ordering, ordering, table_list, relation, filter_condition, groupby,
                  distinct_on_ordering, ordering,)),
                {'columns': column_list})
        self._pdbb_command_close_select = \
            self._SQLCommandTemplate('close %s' % (cursor_name,))
        if self._pdbb_operations:
            self._pdbb_command_select_agg = \
              ('select%s %%s from (select %s from %s where true%s %s) as %s where %%s and (%s)' %
               (distinct_on, column_list, table_list, filter_condition, groupby, table_names[0], relation,))
        else:
            self._pdbb_command_select_agg = \
              ('select%s %%s from %s where %%s and (%s)%s' %
               (distinct_on, table_list, relation, filter_condition,))
        self._pdbb_command_fetch_forward = \
          self._SQLCommandTemplate('fetch forward %%(number)d from %s' % (cursor_name,))
        self._pdbb_command_fetch_backward = \
          self._SQLCommandTemplate('fetch backward %%(number)d from %s' % (cursor_name,))
        self._pdbb_command_move_forward = \
          self._SQLCommandTemplate('move forward %%(number)d from %s' % (cursor_name,))
        self._pdbb_command_move_backward = \
          self._SQLCommandTemplate('move backward %%(number)d from %s' % (cursor_name,))
        self._pdbb_command_move_absolute = \
          self._SQLCommandTemplate('move absolute %%(number)d from %s' % (cursor_name,))
        self._pdbb_command_search_first = \
          self._SQLCommandTemplate(
            (('select %%(columns)s from %s where (%s)%s and %%(condition)s '+
              '%s order by %%(ordering)s %s limit 1') %
             (main_table_from, relation, filter_condition, groupby, ordering,)),
            {'columns': column_list})
        self._pdbb_command_search_last = \
          self._SQLCommandTemplate(
            (('select %%(columns)s from %s where (%s)%s and %%(condition)s '+
              '%s order by %%(ordering)s %s limit 1') %
             (main_table_from, relation, filter_condition, groupby, rordering,)),
            {'columns': column_list})
        if self._pdbb_operations:
            self._pdbb_command_search_distance = \
                'select count(*) from (select%s * from (select %s from %s where (%s)%s %s) as %s where %%(condition)s) as __count' % \
                (distinct_on, column_list, table_list, relation, filter_condition, groupby, table_names[0],)
        elif distinct_on:
            self._pdbb_command_search_distance = \
                'select count(*) from (select%s * from %s where (%s)%s and %%(condition)s) as %s' % \
                (distinct_on, main_table_from, relation, filter_condition, table_names[0])
        else:
            self._pdbb_command_search_distance = \
                'select count(%s) from %s where (%s)%s and %%(condition)s' % \
                (first_key_column, main_table_from, relation, filter_condition,)
        self._pdbb_command_insert = \
          ('insert into %s (%%s) values (%%s) returning %s' %
           (main_table, first_key_column,))
        self._pdbb_command_insert_alternative = \
          ('insert into %s (%%s) values (%%s)' % (main_table,))
        self._pdbb_command_insert_get_last = \
          ('select %s from %s order by %s desc limit 1' %
           (first_key_column, main_table, first_key_column,))
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
            eqs = ['%s=%%s' % o for o in ordering[1:]]
            if eqs:
                eqstring = ' AND '.join(eqs)
                xeqstring = ' AND ' + eqstring
            else:
                eqstring = xeqstring = ''
            self._pdbb_command_insert_shift = \
              ('update %s set %s=%s+1 where %s>=%%s %s' % \
               (main_table, ocol, ocol, ocol, xeqstring))
            self._pdbb_command_insert_newpos = \
              ('select max(%s) from %s where %s' % \
               (ocol, main_table, eqstring))
        update_from_tables = [t for t in table_names if t != main_table]
        if update_from_tables:
            update_from_clause = (' from ' +
                                  string.join(update_from_tables, ', '))
        else:
            update_from_clause = ''
        self._pdbb_command_update = \
          'update %s set %%s%s where (%s) and (%%s)' % \
          (main_table, update_from_clause, relation)
        self._pdbb_command_broken_update_preselect = \
          'select count (%s) from %s where (%s) and (%%s)' % \
          (first_key_column, main_table, relation)
        self._pdbb_command_test_broken_update = \
          (("select 'yes' from pg_class, pg_namespace, pg_rewrite "
            "where pg_rewrite.ev_type = '2' and "
            "pg_rewrite.is_instead = 't' and "
            "pg_class.oid = pg_rewrite.ev_class and "
            "pg_class.relnamespace = pg_namespace.oid and "
            "pg_namespace.nspname = '%s' and "
            "pg_class.relname = '%s'")
           % (schema, main_table_name,))
        self._pdbb_command_delete = \
          'delete from %s where %%s' % main_table
        self._pdbb_command_isolation = 'set transaction isolation level %s'
        self._pdbb_command_notify = \
          'notify "MODIF_%s"' % main_table
        self._pg_notifications = map(lambda t: 'MODIF_%s' % t, table_names)

    def _pdbb_condition2sql(self, condition):
        if condition == None:
            return 'true'
        op_name, op_args, op_kwargs = \
                 condition.name(), condition.args(), condition.kwargs()
        def function_call(op_args):
            assert len(op_args) >= 1, ('Invalid number of arguments', op_args)
            function, args = op_args[0], op_args[1:]
            string_args = []
            for a in args:
                if isinstance(a, str):
                    string_args.append(colarg(a)[0])
                elif isinstance(a, Value):
                    string_args.append(self._pg_value(a))
                else:
                    raise ProgramError("Invalid function condition argument", a)
            return '%s(%s)' % (function, string.join(string_args, ', '),)
        def colarg(colid):
            if isinstance(colid, Operator) and colid.name() == 'Function':
                return function_call(colid.args()), None
            assert isinstance(colid, str), ('Invalid column specification', colid)
            col = self._db_column_binding(colid)
            assert col, ('Invalid column name', colid)
            a = self._pdbb_btabcol(col)
            t = self.find_column(colid).type()
            return a, t
        def relop(rel, args, kwargs):
            assert len(args) == 2, ('Invalid number or arguments', args)
            arg1, arg2 = args
            a1, t1 = colarg(arg1)
            if isinstance(arg2, str):
                a2, t2 = colarg(arg2)
                a2null = False
            else:
                assert isinstance(arg2, Value), ('Invalid value type', arg2)
                assert (not isinstance(t1, Binary) or
                        (rel in ('=','!=') and arg2.value() is None)), \
                        "Binary data can only be compared with NULL values"
                val = self._pg_value(arg2)
                a2 = self._pdbb_coalesce(t1, val)
                t2 = arg2.type()
                a2null = val is 'NULL' # fuj
            if kwargs.get('ignore_case') and isinstance(t1, String) and isinstance(t2, String):
                fix_case = lambda x: 'lower(%s)' % x
            else:
                fix_case = lambda x: x
            if rel in ('=', '!=') and a2null:
                rel = 'IS' + (rel == '!=' and ' NOT' or '')
            return '(%s %s %s)' % (fix_case(a1), rel, fix_case(a2))
        operators = {'EQ': '=',
                     'NE': '!=',
                     'LT': '<',
                     'GT': '>',
                     'LE': '<=',
                     'GE': '>=',
                     'LTreeAncestor': '@>',
                     'LTreeDescendant': '<@',
                     }
        if op_name in operators:
            expression = relop(operators[op_name], op_args, op_kwargs)
        elif op_name in ('WM', 'NW'):
            cid, spec = op_args[0], op_args[1].value()
            for old, new in (('%', '\%'), ('_', '\_')):
                spec = string.replace(spec, old, new)
            for old, new in (('*', '%'), ('?', '_')):
                i = -1
                while True:
                    i = string.find(spec, old, i+1)
                    if i < 0:
                        break
                    j = i - 1
                    while j >= 0 and spec[j] == '\\':
                        j = j - 1
                    if (i-j) % 2 == 1:
                        spec = spec[:i] + new + spec[i+1:]
            spec = Value(String(), spec)
            rel = (op_name == 'NW' and 'NOT ' or '') + 'LIKE'
            expression = relop(rel, (cid, spec), op_kwargs)
        elif op_name == 'NOT':
            assert len(op_args) == 1, ('Invalid number or arguments', op_args)
            arg = op_args[0]
            assert isinstance(arg, Operator)
            expression = 'not %s' % self._pdbb_condition2sql(arg)
        elif op_name == 'AND' or op_name == 'OR':
            if not op_args:
                expression = (op_name == 'AND' and 'true' or 'false')
            else:
                assert \
                  not filter(lambda a: \
                             a and not isinstance(a, Operator),
                             op_args), \
                             ('Invalid suboperator', op_args)
                exps = map(self._pdbb_condition2sql, op_args)
                sqlop = (' %s ' % (op_name == 'AND' and 'and' or 'or'))
                expression = sqlop.join(exps)
        elif op_name == 'IN':
            assert len(op_args) == 4, ('Invalid number or arguments', op_args)
            col, data, table_col, cond = op_args
            table = data._key_binding[0].table()
            condition = data._pdbb_condition2sql(cond)
            expression = '%s in (select %s from %s where %s)' % \
                         (col, table_col, table, condition)
        elif op_name == 'FT':
            assert len(op_args) == 3, ('Invalid number of arguments', op_args)
            col, query, query_id = op_args
            expression = '%s @@ %s' % (col, self._pdbb_fulltext_query_name(col),)
        elif op_name == 'LTreeMatch':
            assert len(op_args) == 2, ('Invalid number of arguments', op_args)
            col, query = op_args
            expression = "%s ~ '%s'" % (col, query,)
        elif op_name == 'Function':
            expression = function_call(op_args)
        elif op_name == 'Raw':          # TODO: Remove.
            assert len(op_args) == 1, ('Invalid number of arguments', op_args)
            expression = op_args[0]
        else:
            raise ProgramError('Unknown operator', op_name)
        return '(%s)' % expression

    def _pdbb_sort2sql(self, sort):
        function_column_dict = {}
        for g in (self._pdbb_column_groups or []):
            if g[2] is not None:
                function_column_dict[g[0]] = g
        def full_text_handler(binding):
            column_name = self._pdbb_btabcol(binding)
            query = self._pdbb_fulltext_query_name(binding.column())
            result = 'ts_rank_cd(%s,%s)' % (column_name, query)
            return result
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
                colstring = self._pdbb_btabcol(b, full_text_handler=full_text_handler,
                                               convert_ltree=True)
            return '%s %s' % (colstring, dir,)
        sort_string = ','.join([item2sql(item) for item in sort])
        if sort_string:
            sort_string += ','
        return sort_string

    def _pdbb_fulltext_query_name(self, column_name):
        return '_pytis_ftq__%s' % (column_name,)
        
    # Metody související s exportovanými metodami DB operací
    
    def _pdbb_table_row_lists(self, row):
        table_bindings = filter(lambda b, t=self._key_binding[0].table(): \
                                b.table() == t,
                                self._bindings)
        columns = []
        values = []
        for b in table_bindings:
            try:
                value = self._pg_value(row[b.id()])
            except KeyError:
                continue
            colid = b.id()
            colspec = self.find_column(colid)
            assert colspec, ('Column not found', colid)
            columns.append(b.column())
            values.append(value)
        return columns, values

    def _pg_make_arguments(self, args, arguments):
        if self._arguments is not None:
            for i in range(len(self._arguments)):
                b = self._arguments[i]
                arg_value = arguments.get(b.id(), b.type().default_value())
                args['__arg_%d' % (i+1,)] = self._pg_value(arg_value)
    
    def _pg_row (self, key_value, columns, transaction=None, supplement='',
                 arguments={}):
        """Retrieve and return raw data corresponding to 'key_value'."""
        args = {'key': key_value, 'supplement': supplement}
        if columns:
            args['columns'] = self._pdbb_sql_column_list_from_names(columns, operations=self._pdbb_operations,
                                                                    column_groups=self._pdbb_column_groups)
        self._pg_make_arguments(args, arguments)
        query = self._pdbb_command_row.format(args)
        return self._pg_query(query, transaction=transaction)
    
    def _pg_search(self, row, condition, direction, transaction=None, arguments={}):
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
            sorting = tuple(sorting) + \
                      tuple(map(lambda c: (c.id(), sdirection),
                                self.key()))
            processed = []
            conditions = []
            for cid, dir in sorting:
                if cid in processed:
                    continue
                conds = [EQ(c, row[c]) for c in processed]
                if (forwards and dir == ASCENDENT) or \
                       (not forwards and dir == DESCENDANT):
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
                if conds:
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
        query = sql_command.format(qargs)
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
        args = {'condition': cond_string}
        self._pg_make_arguments(args, arguments)
        data_ = self._pg_query(self._pdbb_command_search_distance % args,
                               transaction=transaction)
        try:
            result = int(data_[0][0])
        except:
            raise ProgramError('Unexpected result', data_)
        return result

    class _PgRowCounting(object):
        class _Thread(threading.Thread):
            _PG_INITIAL_STEP = 1000
            _PG_MAX_STEP = 100000
            _PG_DEFAULT_TIMEOUT = 0.1
            def __init__(self, data, initial_count, transaction, selection, position):
                threading.Thread.__init__(self)
                self._pg_data = data
                self._pg_transaction = transaction
                self._pg_selection = selection
                self._pg_position = position
                self._pg_current_count = initial_count
                self._pg_initial_count = initial_count
                self._pg_finished = False
                self._pg_terminate = False
                self._pg_terminate_event = threading.Event()
                self._pg_urgent = False
                self._pg_correction = 0
            def run(self):
                try:
                    data = self._pg_data
                    step = min_step = self._PG_INITIAL_STEP
                    max_step = self._PG_MAX_STEP
                    test_count = self._pg_initial_count
                    selection = self._pg_selection
                    transaction = self._pg_transaction
                    args = dict(selection=selection, number=self._pg_initial_count)
                    query = data._pdbb_command_move_absolute.format(args)
                    data._pg_query(query, transaction=transaction)
                    query_counter = data._pg_query_counter
                    while True:
                        if self._pg_dead():
                            self._pg_initial_count = self._pg_current_count
                            args = dict(selection=selection, number=self._pg_position()+1)
                            query = data._pdbb_command_move_absolute.format(args)
                            if not transaction or transaction.open():
                                # The transaction can still become dead before
                                # the following query gets called, but the
                                # resulting error should be harmless.
                                data._pg_query(query, transaction=transaction)
                            return
                        if data._pg_query_counter > query_counter:
                            step = max(step/8, min_step)
                        test_count += step
                        args = dict(selection=selection, number=step)
                        query = data._pdbb_command_move_forward.format(args)
                        result = data._pg_query(query, transaction=transaction)
                        query_counter = data._pg_query_counter
                        self._pg_current_count = self._pg_current_count + result[0][0]
                        if self._pg_current_count < test_count:
                            break
                        if step < max_step:
                            step = min(2*step, max_step)
                        # Give other (perhaps more urgent) threads on the same
                        # data object opportunity to call database queries.
                        if not self._pg_urgent:
                            time.sleep(0.1)
                    self._pg_finished = True
                finally:
                    self._pg_terminate_event.set()
            def _pg_dead(self):
                return (self._pg_terminate or
                        (self._pg_transaction and not self._pg_transaction.open()))
            def pg_count(self, min_value=None, timeout=None, corrected=False):
                self._pg_urgent = True
                if self._pg_dead():
                    pass
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
                args = dict(selection=self._pg_selection, number=self._pg_position()+1)
                query = data._pdbb_command_move_absolute.format(args)
                data._pg_query(query, transaction=self._pg_transaction)
            def pg_restart(self):
                new_thread = self.__class__(self._pg_data, self._pg_current_count,
                                            self._pg_transaction, self._pg_selection,
                                            self._pg_position)
                new_thread.start()
                return new_thread
            def pg_correct(self, correction):
                self._pg_correction += correction
        def __init__(self, data, transaction, selection, position):
            self._thread = self._Thread(data, 0, transaction, selection, position)
        def start(self):
            self._thread.start()
        def count(self, min_value=None, timeout=None, corrected=False):
            result = self._thread.pg_count(min_value, timeout, corrected)
            return result
        def stop(self):
            self._thread.pg_stop()
        def restart(self):
            self._thread = self._thread.pg_restart()
        def __add__(self, correction):
            self._thread.pg_correct(correction)
            return self
            
    def _pg_start_row_counting_thread(self, transaction, selection):
        t = self._PgRowCounting(self, transaction, selection, self._pg_buffer.dbpointer)
        t.start()
        return t
        
    def _pg_select (self, condition, sort, columns, arguments={}, transaction=None, async_count=False):
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
          
        """
        cond_string = self._pdbb_condition2sql(condition)
        sort_string = self._pdbb_sort2sql(sort)
        args = {'condition': cond_string, 'ordering': sort_string}
        fulltext_queries = ['']
        if condition:
            def find_fulltext(operator):
                if operator.name() == 'FT':
                    index_column = operator.args()[0]
                    query = operator.args()[1]
                    fulltext_queries[0] += (",to_tsquery('%s') as %s" %
                                            (query,
                                             self._pdbb_fulltext_query_name(index_column),))
                elif operator.logical():
                    for a in operator.args():
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
                self._pdbb_sql_column_list_from_names(columns,
                                                      full_text_handler=self._pdbb_full_text_handler,
                                                      operations=self._pdbb_operations,
                                                      column_groups=self._pdbb_column_groups)
        else:
            self._pdbb_select_column_list = None
        args['selection'] = self._pdbb_selection_number = \
            self._pdbb_next_selection_number()
        self._pg_query(self._pdbb_command_select.format(args), transaction=transaction)
        if async_count:
            result = self._pg_start_row_counting_thread(transaction, args['selection'])
        else:
            data = self._pg_query(self._pdbb_command_fetch_last.format(args), transaction=transaction)
            self._pg_query(self._pdbb_command_move_to_start.format(args), transaction=transaction)
            if data:
                result = int(data[0][-1])
            else:
                result = 0
        self._pdbb_select_rows = result
        return result

    def _pg_distinct (self, column, prefix, condition, sort, transaction=None):
        cond_string = self._pdbb_condition2sql(condition)
        colspec = self.find_column(column)
        if prefix:
            if isinstance(colspec.type(), String):
                expr = 'substr(%s, 1, %d) as %s' % (column, prefix, column)
            else:
                raise ProgramError("Invalid column type for prefix selection")
        else:
            expr = column
        dir = {ASCENDENT: 'ASC', DESCENDANT: 'DESC'}[sort]
        sort_string = '%s %s' % (column, dir)
        query = self._pdbb_command_distinct % (expr, cond_string, sort_string)
        data = self._pg_query(query, transaction=transaction)
        tmpl = self._pg_create_make_row_template((colspec,))
        result = [self._pg_make_row_from_raw_data([r], tmpl)[column]
                  for r in data]
        return result
        
    def _pg_select_aggregate(self, operation, colids, condition, transaction=None):
        if __debug__:
            if operation != self.AGG_COUNT:
                if operation in (self.AGG_MIN, self.AGG_MAX):
                    allowed = (Number, DateTime, String)
                else:
                    allowed = Number
                for cid in colids:
                    t = self.find_column(cid).type()
                    assert isinstance(t, allowed)
        close_select = False
        if not self._pg_is_in_select:
            self.select(condition=condition, transaction=transaction)
            close_select = True
        try:
            data = self._pg_select_aggregate_1(operation, colids, condition,
                                               transaction=transaction)
        except:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
                pass
            self._pg_is_in_select = False
            raise cls, e, tb
        if close_select:
            self.close()
        I = Integer()
        F = Float()
        def make_value(cid, dbvalue):
            if operation == self.AGG_COUNT:
                t = I
            elif operation == self.AGG_AVG:
                t = F
            else:
                t = self.find_column(cid).type()
            if isinstance(t, String):
                if dbvalue is None:
                    v = None
                else:
                    v = unicode(dbvalue, 'utf-8')
                value = Value(t, v)
            else:
                if isinstance(t, DateTime):
                    kwargs = dict(format=t.SQL_FORMAT, local=not t.is_utc())
                else:
                    kwargs = dict()
                value, error = t.validate(dbvalue, strict=False, **kwargs)
                assert error is None, (error, t, dbvalue)
            return value
        result = [make_value(cid, data[0][i]) for i, cid in enumerate(colids)]
        return result

    def _pg_aggregate_name(self, operation):
        FMAPPING = {self.AGG_MIN: 'min',
                    self.AGG_MAX: 'max',
                    self.AGG_COUNT: 'count',
                    self.AGG_SUM: 'sum',
                    self.AGG_AVG: 'avg',}
        try:
            return FMAPPING[operation]
        except KeyError:
            raise ProgramError('Invalid aggregate function identifier',
                               operation)

    def _pg_select_aggregate_1(self, operation, colids, condition, transaction=None):
        cond_string = self._pdbb_condition2sql(condition)
        colnames = [self._pdbb_btabcol(self._db_column_binding(cid)) for cid in colids]
        function = self._pg_aggregate_name(operation)
        function_list = ['%s (%s)' % (function, cname,) for cname in colnames]
        function_string = string.join(function_list, ', ')
        return self._pg_query(self._pdbb_command_select_agg %
                              (function_string, cond_string),
                              transaction=transaction)
    
    def _pg_fetchmany (self, count, direction, transaction=None):
        """Vrať 'count' řádků selectu jako raw data."""
        args = {'number': count, 'selection': self._pdbb_selection_number}
        if direction == FORWARD:
            query = self._pdbb_command_fetch_forward.format(args)
        elif direction == BACKWARD:
            query = self._pdbb_command_fetch_backward.format(args)
        else:
            raise ProgramError('Invalid direction', direction)
        return self._pg_query(query, transaction=transaction)

    def _pg_skip(self, count, direction, exact_count=False, transaction=None):
        """Přeskoč 'count' řádků v 'direction' a vrať jejich počet nebo 'None'.
        """
        args = {'number': count, 'selection': self._pdbb_selection_number}
        if direction == FORWARD:
            self._pg_query(self._pdbb_command_move_forward.format(args),
                           transaction=transaction)
        elif direction == BACKWARD:
            answer = self._pg_query(self._pdbb_command_move_backward.format(args),
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
                params = [self._pg_value(neighbor[o])
                          for o in self._ordering[1:]]
            except KeyError:
                raise ProgramError('Invalid column id in ordering',
                                   self._ordering, row)
            if n >= 0:
                params = [str(n)] + params
            params = tuple(params)
            if n >= 0:
                self._pg_query(self._pdbb_command_insert_shift % params,
                               backup=True, transaction=transaction)
            else:
                result = self._pg_query((self._pdbb_command_insert_newpos %
                                         (params,)),
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
        cols, vals_ = self._pdbb_table_row_lists(row)
        vals = []
        query_args = []
        escape = len([v for v in vals_ if isinstance(v, buffer)]) != 0
        for v in vals_:
            if isinstance(v, buffer):
                vals.append('%s')
                query_args.append(v)
            else:
                if escape:
                    # Quick fix by TC.  TODO: Wouldn't it be better to escape always and then
                    # always substitute in `_DBAPIAccessor._postgresql_query.do_query()'?
                    v = v.replace('%', '%%')
                vals.append(v)
                
        self._pg_query("savepoint _insert", transaction=transaction)
        try:
            key_data = self._pg_query(
                (self._pdbb_command_insert %
                 (string.join(cols, ','), string.join(vals, ','),)),
                backup=True, query_args=query_args, transaction=transaction)
            key_row = self._pg_make_row_from_raw_data(
                key_data, template=(self._pg_make_row_template[0],))
            key = key_row[0]
            self._pg_query("release _insert", transaction=transaction)
        except DBInsertException:
            self._pg_query("rollback to _insert", transaction=transaction)
            self._pg_query(
                (self._pdbb_command_insert_alternative %
                 (string.join(cols, ','), string.join(vals, ','),)),
                backup=True, query_args=query_args, transaction=transaction)
            try:
                key = row[self._key_binding[0].id()]
            except KeyError:
                key = None
                if isinstance(self._key_binding[0].type(), Serial):
                    try:
                        key_data = self._pg_query(
                            self._pdbb_command_insert_get_last,
                            transaction=transaction)
                        key_row = self._pg_make_row_from_raw_data(
                            key_data, template=(self._pg_make_row_template[0],))
                        key = key_row[0]
                    except DBException:
                        pass
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
        query_args = []
        s = []
        escape = len([v for v in vals if isinstance(v, buffer)]) != 0
        for c, v in zip(cols, vals):
            if isinstance(v, buffer):
                item = "%s=%%s" % c
                query_args.append(v)
            else:
                if escape:
                    # Quick fix by TC.  TODO: Wouldn't it be better to escape always and then
                    # always substitute in `_DBAPIAccessor._postgresql_query.do_query()'?
                    v = v.replace('%', '%%')
                item = "%s=%s" % (c, v)
            s.append(item)
        settings = ','.join(s)
        cond_string = self._pdbb_condition2sql(condition)
        def extract_result(d):
            try:
                return int(d[0][0])
            except:
                raise DBSystemException('Unexpected UPDATE result', None, d)
        try:
            broken = self._pdbb_broken_update_result
        except AttributeError:
            broken = self._pdbb_broken_update_result = \
                     self._pg_query(self._pdbb_command_test_broken_update,
                                    transaction=transaction)
        if broken:
            d = self._pg_query(self._pdbb_command_broken_update_preselect % \
                               cond_string, transaction=transaction)
            result = extract_result(d)
        d = self._pg_query(self._pdbb_command_update % (settings, cond_string,),
                           backup=True, query_args=query_args,
                           transaction=transaction)
        if not broken:
            result = extract_result(d)
        if result >= 0:
            return result
        else:
            raise DBSystemException('Unexpected UPDATE value', None, result)

    def _pg_delete (self, condition, transaction=None):
        """Smaž řádek identifikovaný podmínkou 'condition'.

        Vrací: Počet smazaných řádků.

        """
        sql_condition = self._pdbb_condition2sql(condition)
        d = self._pg_query(self._pdbb_command_delete % sql_condition,
                           backup=True, transaction=transaction)
        try:
            result = int(d[0][0])
        except:
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
        # Dříve to býval buffer, nyní se přeměňuje na "buffer-cache".

        def __init__(self):
            if __debug__: log(DEBUG, 'Nový buffer')
            self.reset()

        def _number_of_rows(self, row_count_info, min_value=None):
            if isinstance(row_count_info, int):
                number = row_count_info
            else:
                number, finished = row_count_info.count(min_value=min_value)
            return number
            
        def reset(self):
            """Kompletně resetuj buffer."""
            if __debug__:
                log(DEBUG, 'Resetuji buffer')
            self._buffer = []
            # _dbpointer ... pozice ukazovátka kursoru v databázi, na který
            #   prvek kursoru počínaje od 0 ukazuje
            # _dbposition ... pozice začátku bufferu v databázi, číslo prvku
            #   kurzoru počínaje 0, který odpovídá prvnímu prvku bufferu
            # _pointer ... pozice ukazovátka v bufferu, ukazuje na poslední
            #   přečtený prvek, nemusí vždy ukazovat dovnitř bufferu
            self._dbposition = 0
            self._dbpointer = self._pointer = -1

        def current(self):
            """Vrať aktuální řádek a jeho pozici v databázi počínaje od 0.

            Výsledek je vrácen jako dvojice (ROW, POSITION).  Je-li aktuální
            řádek mimo buffer, je ROW 'None'.  Je-li aktuální pozice mimo
            buffer, je POSITION je -1 nebo počet řádků selectu.

            """
            buffer = self._buffer
            pointer = self._pointer
            position = self._dbposition + pointer
            if pointer < 0 or pointer >= len(buffer):
                row = None
            else:
                row = buffer[pointer]
            return row, position

        def fetch(self, direction, row_count_info):
            """Vrať řádek nebo 'None' a updatuj ukazovátka.

            Pokud řádek není v bufferu, je vráceno 'None' a předpokládá se
            následné volání metod 'correction()' a 'fill()'.

            """
            buffer = self._buffer
            pointer = self._pointer
            if direction == FORWARD:
                pointer += 1
            elif direction == BACKWARD:
                pointer -= 1
            else:
                raise ProgramError('Invalid direction', direction)
            if pointer < 0 or pointer >= len(buffer):
                if __debug__: log(DEBUG, 'Buffer miss:', pointer)
                pos = self._dbposition + pointer
                # Interní ukazovátko po obyčejném minutí neupdatujeme, protože
                # přijde fill a pokus o znovuvytažení hodnoty, s novým updatem
                # ukazovátka.  Avšak pokud jsme kompletně mimo rozsah dat, není
                # tato zdrženlivost namístě a je nutno ukazovátko posunout na
                # správnou pozici, tj. mimo rozsah dat.
                if pos < 0:
                    self._pointer = -1 - self._dbposition
                elif pos >= self._number_of_rows(row_count_info, pos+1):
                    self._pointer = self._number_of_rows(row_count_info) - self._dbposition
                return None
            self._pointer = pointer
            result = buffer[pointer]
            if __debug__: log(DEBUG, 'Buffer hit:', pointer) #, str(result))
            return result

        def correction(self, direction, row_count_info):
            """Vrať argument pro DB operaci SKIP před naplněním bufferu.

            Kladná návratová hodnota odpovídá posunu vpřed, záporná posunu
            zpět.
            
            Databázové ukazovátko je updatováno jako kdyby SKIP byl proveden.

            """
            if __debug__: log(DEBUG, 'Žádost o korekci:',
                (self._dbpointer, self._dbposition, self._pointer, direction))
            pointer = self._pointer
            buflen = len(self._buffer)
            pos = self._dbposition + pointer
            if pointer > buflen or pointer < -1:
                # Dostali jsme se daleko za buffer, je nutno provést DB skip.
                # TODO: zde by mohlo být dobré nastavit pozici tak, aby byla
                # načtena ještě nějaká data proti směru bufferu.  Jak to ale
                # udělat čistě?
                if pos >= 0:
                    pos = min(pos, self._number_of_rows(row_count_info, pos))
                else:
                    pos = max(pos, -1)
                correction = pos - self._dbpointer
                self._buffer = []
                self._pointer = -1
                self._dbpointer = self._dbpointer + correction
                self._dbposition = self._dbpointer + 1
            elif (direction == FORWARD and pointer >= buflen - 1) or \
                 (direction == BACKWARD and pointer <= 1):
                # Jsme u hranice bufferu, provedeme DB skip bez mazání bufferu.
                # Rozsah v podmínce je zvolen tak, aby ošetřil i předchozí
                # buffer miss.
                correction = pos - self._dbpointer
                self._dbpointer = self._dbpointer + correction
            else:
                # Jsme uvnitř bufferu, žádný DB skip se nekoná.
                correction = 0
            if __debug__: log(DEBUG, 'Určená korekce:', correction)
            return correction

        def goto(self, position):
            """Updatuj databázovou pozici nastavenou bez vědomí bufferu.

            Argumenty:

              position -- číslo prvku cursoru začínajícího od 0, na který
                ukazovátko databázového kurzoru právě ukazuje

            """
            self._pointer = position - self._dbposition
            self._dbpointer = position
            
        def skip(self, count, direction, row_count_info):
            """Proveď skip.

            Argumenty:

              count -- počet řádků, o kolik se má skok provést
              direction -- jedna ze směrových konstant modulu
              row_count_info -- informace o počtu řádků v aktuálním selectu

            Vrací: Počet skutečně přeskočených řádků ve směru 'direction'.
            
            """
            pointer = self._pointer
            if direction == FORWARD:
                pointer = pointer + count
            elif direction == BACKWARD:
                pointer = pointer - count
            else:
                raise ProgramError('Invalid direction', direction)
            pos = self._dbposition + pointer
            if pos < -1:
                pointer = -1 - self._dbposition
            elif pos > self._number_of_rows(row_count_info, pos):
                pointer = self._number_of_rows(row_count_info) - self._dbposition
            result = pointer - self._pointer
            if direction == BACKWARD:
                result = -result
            self._pointer = pointer
            return result
        
        def fill(self, rows, direction, extra_move=False):
            """Naplň se daty 'rows' a updatuj ukazovátka."""
            # extra_move je tu kvůli tomu, že pokud dojde ve fetchmany
            # k překročení hranic dat ještě před získáním požadovaného počtu
            # řádků, musí být dbpointer přesunut ještě o jednu pozici dál (mimo
            # data).
            if __debug__: log(DEBUG, 'Plním buffer:', direction)            
            n = len(rows)
            buffer = self._buffer
            buflen = len(buffer)
            dbpointer = self._dbpointer
            dbposition = self._dbposition
            pointer = self._pointer + dbposition
            import config
            retain = max(config.cache_size - n, 0)
            cutoff = max(buflen - retain, 0)
            if direction == FORWARD:
                if dbposition + buflen - 1 == dbpointer:
                    buffer = buffer[cutoff:] + rows
                    dbposition = dbposition + cutoff
                else:
                    buffer = rows
                    dbposition = dbpointer + 1
                dbpointer = dbpointer + n
                if extra_move:
                    dbpointer = dbpointer + 1
            elif direction == BACKWARD:
                rows.reverse()
                if dbposition == dbpointer:
                    buffer = rows + buffer[:retain]
                else:
                    buffer = rows
                dbpointer = dbpointer - n
                dbposition = dbpointer
                if extra_move:
                    dbpointer = dbpointer - 1
            else:
                raise ProgramError('Invalid direction', direction)
            self._pointer = pointer - dbposition
            self._dbpointer = dbpointer
            self._dbposition = dbposition
            self._buffer = buffer

        def dbpointer(self):
            """Return database pointer position within the database cursor."""
            return self._dbpointer

        def copy(self):
            """Vrať \"rozumnou\" kopii instance."""
            # Nepoužíváme postupy modulu `copy', protože potřebujeme něco mezi
            # hlubokou a mělkou kopií.
            copy_ = self.__class__()
            copy_._buffer = copy.copy(self._buffer)
            copy_._pointer = self._pointer
            copy_._dpointer = self._dpointer
            copy_._dbposition = self._dbposition
            return copy_

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
            return '<PgBuffer: db=%d, start=%d, index=%d\n%s>' % \
                   (self._dbpointer, self._dbposition, self._pointer, bufstr)

    def __init__(self, bindings, key, connection_data, **kwargs):
        """Inicializuj databázovou tabulku dle uvedených specifikací.

        Argumenty:
        
          bindings -- stejné jako v předkovi
          key -- binding klíčového sloupce datové tabulky, musí být jeden
            z prvků 'bindings' nebo sekvence prvků z 'bindings'
          connection_data -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'
          kwargs -- předá se předkovi
        
        """
        if __debug__:
            log(DEBUG, 'Vytvářím databázovou tabulku')
        if is_sequence(key):
            self._key_binding = tuple(key)
        else:
            self._key_binding = (key,)
        self._pg_is_in_select = False
        super(DBDataPostgreSQL, self).__init__(
            bindings=bindings, key=key, connection_data=connection_data,
            **kwargs)
        self._pg_buffer = self._PgBuffer()
        self._pg_number_of_rows = None
        self._pg_initial_select = False
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
                log(DEBUG, 'Podezřele velká hloubka spojení:', len(connections))
        connection = self._pg_get_connection(outside_transaction=True)
        connections.append(connection)
        
    def _pg_deallocate_connection(self):
        self._pg_return_connection(self._pg_connections().pop())

    def _pg_begin_transaction (self, isolation=None):
        if self._pg_is_in_select:
            self.close()
        self._pg_allocate_connection()
        self._postgresql_begin_transaction()
        if isolation:
            self._pg_query(self._pdbb_command_isolation % (isolation,))
        
    def _pg_commit_transaction (self):
        self._postgresql_commit_transaction()
        self._pg_deallocate_connection()
        if self._pg_is_in_select is True:
            self._pg_is_in_select = False
        
    def _pg_rollback_transaction (self):
        self._postgresql_rollback_transaction()
        self._pg_deallocate_connection()
        if self._pg_is_in_select is True:
            self._pg_is_in_select = False

    # Pomocné metody

    def _pg_create_make_row_template(self, columns, column_groups=None):
        template = []
        for c in columns:
            id_ = c.id()
            type = c.type()
            if isinstance(type, (String, LTree)):
                typid = 0
            elif isinstance(type, (Time, DateTime)):
                typid = 2
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
        row_data = []
        data_0 = data_[0]
        i = 0
        for id, typid, type_ in template:
            dbvalue = data_0[i]
            i += 1
            if typid == 0:              # string
                if dbvalue is None:
                    v = None
                else:
                    #TODO: This belongs elsewhere (if moving, move also from _pg_select_aggregate)
                    v = unicode(dbvalue, 'utf-8')
                value = Value(type_, v)
            elif typid == 2:            # time
                local = not type_.is_utc()
                value, err = type_.validate(dbvalue, strict=False,
                                            format=type_.SQL_FORMAT, local=local)
                assert err is None, (dbvalue, type_, err)
            else:
                value, err = type_.validate(dbvalue, strict=False)
                assert err is None, (dbvalue, type_, err)
            row_data.append((id, value))
        return Row(row_data)

    def _pg_already_present(self, row, transaction=None):
        key = []
        for k in self.key():
            try:
                id = k.id()
            except:
                return False
            try:
                key.append(row[id])
            except KeyError:
                return False
        return self.row(key, transaction=transaction)
    
    _pg_dt = type(mx.DateTime.DateTimeFrom('2001-01-01'))
    def _pg_value(self, value):
        v = value.value()
        if v == None:
            result = 'NULL'
        elif isinstance(value.type(), Boolean):
            result = "'%s'" % value.type().export(v)
        elif is_anystring(v):
            result = "'%s'" % pg_escape(v)
        elif type(v) == self._pg_dt:
            if isinstance(value.type(), Date):
                result = "'%s'" % v.strftime('%Y-%m-%d')
            elif isinstance(value.type(), Time):
                result = "'%s'" % v.strftime('%H:%M:%S')
            else:                       # DateTime
                result = "'%s'" % v.strftime('%Y-%m-%d %H:%M:%S')
        elif isinstance(value.type(), Float):
            result = value.type().export(v, locale_format=False)
        else:
            result = value.type().export(v)
        return result

    def _pg_key_condition(self, key):
        if __debug__: log(DEBUG, 'Vytvářím podmínku z klíče:', key)
        key = xtuple(key)
        keycols = map(lambda b: b.id(), self._key_binding)
        assert len(keycols) == len(key), ('Invalid key length', key, keycols)
        ands = map(EQ, keycols, key)
        condition = AND(*ands)
        if __debug__: log(DEBUG, 'Podmínka z klíče vytvořena:', condition)
        return condition

    def _pg_connection_maker(self):
        def maker():
            self._pg_new_connection(self._pg_connection_data(), self)
        return maker

    def _pg_restore_select(self):
        row_number = self._pg_last_select_row_number
        if row_number is None and self._pg_last_select_transaction is not None:
            # The last select wasn't closed correctly and we are inside a
            # higher level transaction -- we mustn't continue in such a case.
            return False
        if row_number is None:
            row_number = self._pg_buffer.current()[1]
        self.select(condition=self._pg_last_select_condition,
                    sort=self._pg_last_select_sorting,
                    transaction=self._pg_last_select_transaction)
        if row_number >= 0:
            self.skip(row_number)
        return True

    def _pg_number_of_rows_(self, min_value=None):
        if isinstance(self._pg_number_of_rows, int):
            number = self._pg_number_of_rows
        else:
            number, finished = self._pg_number_of_rows.count(min_value=min_value)
            if finished:
                self._pg_number_of_rows = number
        return number

    # Veřejné metody a jimi přímo volané abstraktní metody

    def row(self, key, columns=None, transaction=None, arguments={}):
        #log(EVENT, 'Zjištění obsahu řádku:', key)
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
            data = self._pg_row(self._pg_value(key[0]), columns,
                                transaction=transaction, arguments=arguments)
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        if transaction is None and not self._pg_is_in_select:
            self._postgresql_commit_transaction()
        result = self._pg_make_row_from_raw_data(data, template=template)
        #log(EVENT, 'Vrácený obsah řádku', result)
        return result
        
    def select(self, condition=None, sort=(), reuse=False, columns=None, transaction=None,
               arguments={}, async_count=False):
        if __debug__:
            log(DEBUG, 'Select started:', condition)
        if (reuse and not self._pg_changed and self._pg_number_of_rows and
            condition == self._pg_last_select_condition and
            sort == self._pg_last_select_sorting and
            transaction is self._pg_last_select_transaction and
            not async_count):
            use_cache = True
        else:
            use_cache = False
        if self._pg_is_in_select: 
            self.close()
        if transaction is None:
            self._pg_begin_transaction (isolation='serializable')
        self._pg_is_in_select = transaction or True
        self._pg_last_select_condition = condition
        self._pg_last_select_sorting = sort
        self._pg_last_select_transaction = transaction
        self._pg_last_fetch_row = None
        self._pg_last_select_row_number = None
        self._pg_changed = False
        if columns:
            self._pg_make_row_template_limited = \
                self._pg_limited_make_row_template(columns)
        else:
            self._pg_make_row_template_limited = None        
        try:
            row_count_info = self._pg_select(condition, sort, columns, transaction=transaction,
                                             arguments=arguments, async_count=async_count)
        except:
            if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                try:
                    self._pg_number_of_rows.stop()
                except:
                    pass
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
                pass
            self._pg_is_in_select = False
            raise cls, e, tb
        if (use_cache and
            (not isinstance(row_count_info, int) or row_count_info != self._pg_number_of_rows)):
            use_cache = False
        if use_cache:
            self._pg_buffer.goto(-1)
        else:
            self._pg_buffer.reset()
            self._pg_initial_select = True
        self._pg_number_of_rows = row_count_info
        return row_count_info

    def select_aggregate(self, operation, condition=None, transaction=None):
        return self._pg_select_aggregate(operation[0], (operation[1],),
                                         condition=condition, transaction=transaction)[0]
    
    def select_and_aggregate(self, operation, condition=None, reuse=False, sort=(),
                             columns=None, transaction=None):
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
                                                      condition=condition, transaction=transaction)
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
        
    def distinct(self, column, prefix=None, condition=None, sort=ASCENDENT, transaction=None):
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

        """
        return self._pg_distinct(column, prefix, condition, sort, transaction=transaction)

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
            log(DEBUG, 'Vytažení řádku ze selectu ve směru:', direction)
        assert direction in(FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        if not self._pg_is_in_select:
            if not self._pg_restore_select():
                raise ProgramError('Not within select')
        # Tady začíná opravdové vytažení aktuálních dat
        buffer = self._pg_buffer
        row = buffer.fetch(direction, self._pdbb_select_rows)
        if row:
            result = row
        else:
            if (transaction is None and isinstance(self._pg_is_in_select, DBPostgreSQLTransaction)):
                transaction_ = self._pg_is_in_select
            else:
                transaction_ = transaction
            # Kurzory v PostgreSQL mají spoustu chyb.  Například často
            # kolabují při překročení hranic dat a mnohdy správně nefunguje
            # FETCH BACKWARD.  V následujícím kódu se snažíme některé
            # nejčastější chyby PostgreSQL obejít.
            def stop_counting():
                if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                    self._pg_number_of_rows.stop()
            def start_counting():
                if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                    self._pg_number_of_rows.restart()
            def skip():
                xcount = buffer.correction(FORWARD, self._pg_number_of_rows)
                if xcount < 0:
                    xcount = -xcount
                    skip_direction = BACKWARD
                else:
                    skip_direction = FORWARD
                if xcount > 0:
                    try:
                        result = self._pg_skip(xcount, skip_direction,
                                               exact_count=True,
                                               transaction=transaction_)
                    except:
                        cls, e, tb = sys.exc_info()
                        try:
                            if transaction is None:
                                self._pg_rollback_transaction()
                        except:
                            pass
                        self._pg_is_in_select = False
                        raise cls, e, tb
            stop_counting()
            skip()
            if self._pg_initial_select:
                self._pg_initial_select = False
                std_size = self._pg_initial_fetch_size
            else:
                std_size = self._pg_fetch_size
            current_row_number = buffer.current()[1]
            last_row_number = min(current_row_number + 1,
                                  self._pg_number_of_rows_(current_row_number+1))
            if direction == FORWARD:
                size = min(self._pg_number_of_rows_(last_row_number+std_size)-last_row_number, std_size)
            else:
                if current_row_number <= 0:
                    if current_row_number == 0:
                        skipped = buffer.skip(1, BACKWARD,
                                              self._pg_number_of_rows)
                        assert skipped == 1, skipped
                        skip()
                    return None
                size = min(self._pg_number_of_rows_(std_size), std_size)
            assert size >= 0 and size <= self._pg_number_of_rows_(size)
            if direction == FORWARD:
                xskip = None
            else:
                xskip = buffer.skip(size, BACKWARD, self._pg_number_of_rows)
                skip()
            try:
                if size != 0:
                    data_ = self._pg_fetchmany(size, FORWARD, transaction=transaction_)
                else:
                    # Don't run an unnecessary SQL command
                    data_ = None
            except:
                cls, e, tb = sys.exc_info()
                try:
                    if transaction is None:
                        self._pg_rollback_transaction()
                except:
                    pass
                self._pg_is_in_select = False
                raise cls, e, tb
            if data_:
                row_data = [self._pg_make_row_from_raw_data(
                        [d], template=self._pg_make_row_template_limited)
                            for d in data_]
                buffer.fill(row_data, FORWARD, len(row_data)!=size)
                if xskip:
                    buffer.skip(xskip, FORWARD, self._pg_number_of_rows)
                result = buffer.fetch(direction, self._pdbb_select_rows)
            else:
                result = None
            start_counting()
        self._pg_last_fetch_row = result
        if __debug__: log(DEBUG, 'Vrácený řádek', str(result))
        return result
    
    def last_row_number(self):
        if not self._pg_is_in_select:
            if not self._pg_restore_select():
                raise ProgramError('Not within select')
        return self._pg_buffer.current()[1]

    def last_select_condition(self):
        return self._pg_last_select_condition

    def last_select_condition_sql(self):
        return self._pdbb_condition2sql(self._pg_last_select_condition)

    def skip(self, count, direction=FORWARD):
        if __debug__: log(DEBUG, 'Přeskočení řádků:', (direction, count))
        assert type(count) == type(0) and count >= 0, \
               ('Invalid count', count)
        assert direction in (FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        result = self._pg_buffer.skip(count, direction,
                                      self._pg_number_of_rows)
        if count > 0:
            self._pg_last_fetch_row = None
        if __debug__: log(DEBUG, 'Přeskočeno řádků:', result)
        return result

    def rewind(self):
        if not self._pg_is_in_select:
            if not self._pg_restore_select():
                raise ProgramError('Not within select')                
        __, pos = self._pg_buffer.current()
        if pos >= 0:
            self.skip(pos+1, BACKWARD)
        
    def search(self, condition, direction=FORWARD, transaction=None, arguments={}):
        """Vyhledej ve směru 'direction' první řádek od 'row' dle 'condition'.

        Vrací: Vzdálenost od řádku 'row' jako kladný integer nebo 0, pokud
        takový řádek neexistuje.

        """
        if __debug__: log(DEBUG, 'Hledání řádku:', (condition, direction))
        assert direction in (FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        if not self._pg_is_in_select:
            if not self._pg_restore_select():
                raise ProgramError('Not within select')
        row, pos = self._pg_buffer.current()
        if not row and pos >= 0 and pos < self._pg_number_of_rows_(pos+1):
            self.skip(1, BACKWARD)
            row = self.fetchone()
        if not row and (pos < 0 and direction == BACKWARD or \
                        pos >= 0 and direction == FORWARD):
            result = 0
        else:
            try:
                result = self._pg_search(row, condition, direction,
                                         transaction=transaction, arguments=arguments)
            except:
                cls, e, tb = sys.exc_info()
                if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                    try:
                        self._pg_number_of_rows.stop()
                    except:
                        pass
                try:
                    if transaction is None:
                        self._pg_rollback_transaction()
                except:
                    pass
                self._pg_is_in_select = False
                raise cls, e, tb
        if __debug__: log(DEBUG, 'Výsledek hledání:', result)
        return result

    def close(self):
        if __debug__:
            log(DEBUG, 'Explicitly closing current select')
        if self._pg_is_in_select:
            if isinstance(self._pg_number_of_rows, self._PgRowCounting):
                self._pg_number_of_rows.stop()
            _, self._pg_last_select_row_number = self._pg_buffer.current()
            args = dict(selection=self._pdbb_selection_number)
        if self._pg_is_in_select is True: # no user transaction
            try:
                self._pg_commit_transaction()
            except DBSystemException: # e.g. after db engine restart
                pass
        elif self._pg_is_in_select: # inside user transaction
            query = self._pdbb_command_close_select.format(args)
            transaction = self._pg_is_in_select
            self._pg_query(query, transaction=transaction)
        self._pg_is_in_select = False
        # Flush cached data
        self._pg_buffer.reset()

    def insert(self, row, after=None, before=None, transaction=None):
        assert after is None or before is None, 'Both after and before specified'
        log(ACTION, 'Insert row:', (row, after, before))
        if transaction is None:
            self._pg_begin_transaction ()
        try:
            # Jestliže je definováno ordering, které je součástí klíče, bude
            # nově vložený řádek nutně unikátní.
            if (not self._ordering or (self._ordering[0] not in [c.id() for c in self.key()])) \
                   and self._pg_already_present(row, transaction=transaction):
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
                    r = self._pg_insert (row, after=after, before=before, transaction=transaction)
                    result = r, True
        except:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
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
            self._pg_begin_transaction ()
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
            else: # not origrow
                msg = 'Row with given key does not exist'
                result = msg, False
                log(ACTION, msg, key)
        except:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction ()
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
            self._pg_begin_transaction ()
        try:
            ordering = self._ordering
            if ordering:
                new_row_items = []
                for k, v in row.items():
                    if k not in ordering:
                        new_row_items.append((k, v))
                row = Row(new_row_items)
            result = self._pg_update (condition, row, transaction=transaction)
        except:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction ()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        if result:
            log(ACTION, 'Rows updated:', result)
        return result

    def delete(self, key, transaction=None):
        log(ACTION, 'Delete row:', key)
        if transaction is None:
            self._pg_begin_transaction ()
        try:
            result = self._pg_delete (self._pg_key_condition(key),
                                      transaction=transaction)
        except:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction ()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        log(ACTION, 'Row deleted:', result)
        return result
    
    def delete_many(self, condition, transaction=None):
        log(ACTION, 'Delete rows:', condition)
        if transaction is None:
            self._pg_begin_transaction ()
        try:
            result = self._pg_delete (condition, transaction=transaction)
        except:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        if transaction is None:
            self._pg_commit_transaction ()
            self._pg_send_notifications()
        else:
            transaction._trans_notify(self)
        log(ACTION, 'Rows deleted:', result)
        return result

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
        log(EVENT, 'Locking row:', str (key))
        self._pg_query('savepoint _lock', transaction=transaction)
        try:
            command = self._pdbb_command_lock.format({'key': self._pg_value(key)})
            if command:         # special locking command necessary
                result = self._pg_query(command, transaction=transaction)
            else:
                result = self._pg_row(self._pg_value(key), None,
                                      transaction=transaction,
                                      supplement='for update nowait')
            self._pg_query('release _lock', transaction=transaction)
            if not result:
                return "No such record"
        except DBLockException:
            log(EVENT, 'Row already locked by another process')
            self._pg_query('rollback to _lock', transaction=transaction)
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
        assert is_string(name)
        PostgreSQLConnector.__init__(self, connection_data, **kwargs)
        self._name = name
        self._query = "select nextval('%s')" % name
        
    def next(self, transaction=None):
        result = self._pg_query(self._query, transaction=transaction)
        try:
            number = int(result[0][0])
        except Exception, e:
            raise pytis.data.DBException(_("Chybná hodnota čítače z databáze"), e)
        return number


class DBPostgreSQLFunction(Function, DBDataPostgreSQL,
                           PostgreSQLStandardBindingHandler):
    """PostgreSQL implementation of the 'Function' class."""
    
    def __init__(self, name, connection_data, result_columns=None, **kwargs):
        """
        Arguments:

          name -- jméno funkce jako neprázdný string
          connection_data -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'
          result_columns -- sequence of 'ColumnSpec' instances describing the result
            rows; if 'None', columns and their types are determined
            automatically but only if it is possible and supported
          kwargs -- forwarded to successors

        """
        assert is_string(name)
        self._name = name
        bindings = ()
        self._pdbb_result_columns = result_columns
        super(DBPostgreSQLFunction, self).__init__(
            bindings=bindings, key=bindings, connection_data=connection_data,
            **kwargs)
        arg_query = "select pronargs from pg_proc where proname='%s'" % name
        data = self._pg_query(arg_query, outside_transaction=True)
        narg = int(data[0][0])
        arguments = string.join(('%s',)*narg, ', ')
        self._pdbb_function_call = 'select * from %s(%s)' % (name, arguments)
        
    def _db_bindings_to_column_spec(self, __bindings):
        if self._pdbb_result_columns is not None:
            return self._pdbb_result_columns, ()
        schema, name = self._pdbb_split_function_name(self._name)
        type_query = ("select proretset, prorettype, proargtypes from pg_proc, pg_namespace "
                      "where pg_proc.pronamespace = pg_namespace.oid and "
                      "pg_namespace.nspname = '%s' and proname = '%s'") % (schema, name,)
        self._pg_begin_transaction()
        try:
            data = self._pg_query(type_query)
            assert data, ('No such function', self._name)
            assert len(data) == 1, ('Overloaded functions not supported',
                                    self._name)
            r_set, r_type, arg_types = data[0]
            def type_instances(tnum):
                query = ("select typname, nspname, typlen, typtype "
                         "from pg_type join pg_namespace on typnamespace = pg_namespace.oid "
                         "where pg_type.oid = '%s'") % (tnum,)
                data = self._pg_query(query)
                type_, type_ns, size_string, t_type = data[0]
                if t_type == 'b':
                    instances = [self._pdbb_get_type(type_, size_string, False, False)]
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
            columns = [ColumnSpec('column%d' % (i+1,), r_type_instances[i])
                       for i in range(len(r_type_instances))]
        finally:
            self._pg_commit_transaction()
        return columns, ()

    def _pdbb_create_sql_commands(self):
        self._pg_notifications = []
    
    def call(self, row, transaction=None):
        log(EVENT, 'Function call:', self._name)
        arguments = tuple([self._pg_value(item) for item in row])
        if transaction is None:
            outside_transaction = True
        else:
            outside_transaction = False
        data = self._pg_query(self._pdbb_function_call % arguments,
                              transaction=transaction,
                              outside_transaction=outside_transaction
                              )
        if transaction is None:
            self._pg_query ('commit', outside_transaction=outside_transaction)
        result = [self._pg_make_row_from_raw_data([row]) for row in data]
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

    def __init__(self, connection_data, **kwargs):
        super(DBPostgreSQLTransaction, self).__init__(
            bindings=(), key=(), connection_data=connection_data,
            **kwargs)
        self._trans_notifications = []
        self._pg_begin_transaction()
        self._open = True
        
    def _db_bindings_to_column_spec(self, __bindings):
        return (), ()
    
    def _pdbb_create_sql_commands(self):
        pass

    def _trans_connection(self):
        return self._pg_get_connection()

    def _trans_notify(self, dbdata):
        self._trans_notifications.append(dbdata)

    def commit(self):
        """Commit the transaction."""
        self._pg_commit_transaction()
        self._open = False
        for dbdata in self._trans_notifications:
            dbdata._pg_send_notifications()

    def rollback(self):
        """Rollback the transaction."""
        self._pg_rollback_transaction()
        self._open = False

    def set_point(self, point):
        """Set transaction point for possible future partial rollback.

        Arguments:

          point -- string containing only lowercase English letters defining
            the transaction point
            
        """
        assert re.match('^[a-z]+$', point)
        self._pg_query('savepoint %s' % (point,), transaction=self)

    def cut(self, point):
        """Rollback the transaction to the given point.

        Arguments:

          point -- string containing only lowercase English letters defining
            the transaction point to which the rollback should be performed
            
        """
        assert re.match('^[a-z]+$', point)
        self._pg_query('rollback to %s' % (point,), transaction=self)
        self._pg_query('release %s' % (point,), transaction=self)

    def open(self):
        """Return true iff the transaction is open and hasn't been closed yet."""
        return self._open
