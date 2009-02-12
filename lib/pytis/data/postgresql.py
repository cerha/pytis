# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2009 Brailcom, o.p.s.
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


"""Obecn� implementace datab�zov�ho rozhran� pro PostgreSQL.

Tato ��st je nez�visl� na konkr�tn� pou�it� knihovn� pro p��stup k�PostgreSQL,
tvo�� jen obecn� rozhran� skl�daj�c� nap��klad SQL p��kazy.  Fyzick� p��stup
k�datab�zi zaji��uj� rozhran� d�le implementovan� v�jin�ch zdrojov�ch souborech.

"""

import copy
import operator
import re
import string
import thread
import time
import weakref

import mx.DateTime

from dbdata import *
from evaction import *
from pytis.data import *
import pytis.data


# Modifikace tabulek se oznamuje zasl�n�m notifikace `MODIF_table', kde `table'
# je jm�no modifikovan� tabulky.


def pg_escape(string_):
    return string_.replace("'", "''")


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
    if ENCODING_MAPPING.has_key(enc):
        return ENCODING_MAPPING[enc]
    else:
        return enc


class PostgreSQLResult(object):
    """Na pou�it�m backendu nez�visl� reprezentace v�sledku SQL p��kazu.

    P�edpokl�d� se p�edefinov�n� t�to t��dy v�potomc�ch PostgreSQLAccessor dle
    pot�eb konkr�tn�ho pou�it�ho backendu.

    """
    def __init__(self, data):
        """
        
        Argumenty:

          data -- datov� objekt odpov�daj�c� v�sledku; jedn�-li se o�sekvenci
            sekvenc� string�, funguj� standardn� metody v�t�to t��d�, v�opa�n�m
            p��pad� je nutno tyto metody p�edefinovat
        
        P��klady standardn�ch hodnot 'data':
        
          (('1', 'prvni'), ('2', 'druhy'), ('3', 'treti'))
          ()
          [['42']]

        """
        # Pozn�mka ke specifikaci: Reprezentace dat �et�zci se m��e zd�t
        # pon�kud nevhodn�, proto�e u�n�kter�ch rozhran� to m��e znamenat
        # konverzi dat na n�jak� typ a pak zp�t na �et�zec.  To je ov�em cena,
        # kterou r�di zaplat�me za srozumitelnost cel� z�le�itosti.  Probl�mem
        # nen� ani mrh�n� CPU cykly, proto�e k�d pob�� na klientech, kte��
        # se stejn� vesm�s fl�kaj�.
        self._data = data
        
    def __getitem__(self, row):
        """Vra� hodnotu v�sledku z���dku 'row'.

        N�vratovou hodnotou je reprezentace dat ��dku jako indexovateln� objekt
        s�hodnotami typu string odpov�daj�c�mi jednotliv�m sloupc�m v�sledku.
        """
        return self._data[row]

    def __nonzero__(self):
        """Vra� True pr�v� kdy� objekt obsahuje n�jak� data.
        """
        return len(self) > 0

    def __len__(self):
        """Vra� po�et ��dk� dat.
        """
        return len(self._data)
    

class PostgreSQLAccessor(object):
    """T��da pro low-level p��stup k�PostgreSQL.

    Tato t��da je zodpov�dn� za komunikaci s�PostgreSQL realizovanou
    prost�ednictv�m konkr�tn� backendov� knihovny.  Konkr�tn� m� na starosti
    tyto v�ci: otev�r�n� a uzav�r�n� spojen� do datab�ze, zas�l�n� SQL p��kaz�
    datab�zov�mu stroji, p�evod v�sledk� SQL p��kaz� do obecn� na pou�it�m
    backendu nez�visl� podoby.

    P��stup k�PostgreSQL prost�ednictv�m konkr�tn�ho backendu se realizuje
    pod�d�n�m t�to t��dy a p�edefinov�n�m jej�ch metod.

    """

    class _postgresql_Connection(object):
        """Spojen� do datab�ze.
        """
        def __init__(self, connection, connection_data):
            """

            Argumenty:

              connection -- spojen� do datab�zov�ho stroje
              connection_data -- specifikace parametr� spojen�
              
            """
            self._connection = connection
            self._connection_data = connection_data

        def connection(self):
            return self._connection

        def connection_data(self):
            return self._connection_data

    class _postgresql_Result(object):
        """V�sledek SQL p��kazu.
        """
        def __init__(self, result):
            """

            Argumenty:

              result -- v�sledek SQL p��kazu v�podob� z�visl� na pou�it�m
               backendu
               
            """
            self._result = result

        def result(self):
            return self._result
    
    def _postgresql_new_connection(self, connection_data):
        """Vytvo�, inicializuj a vra� nov� spojen� do datab�ze.

        N�vratovou hodnotou je instance '_postgresql_Connection'.

        Argumenty:

          connection_data -- dictionary obsahuj�c� p�ihla�ovac� �daje jako
            stroj, port, u�ivatel, heslo, atd.
            
        """
        connection = self._postgresql_open_connection(connection_data)
        self._postgresql_initialize_connection(connection)
        return connection

    @classmethod
    def _postgresql_open_connection(class_, connection_data):
        """Vytvo� a vra� nov� spojen� do datab�ze.

        N�vratovou hodnotou je instance '_postgresql_Connection'.

        Argumenty:

          connection_data -- dictionary obsahuj�c� p�ihla�ovac� �daje jako
            stroj, port, u�ivatel, heslo, atd.

        Tato metoda mus� b�t p�edefinov�na v�podt��d�.

        """
        raise ProgramError(_("Vol�na neimplementovan� metoda"))

    @classmethod
    def _postgresql_close_connection(class_, connection):
        """Uzav�i spojen� do datab�ze.

        Argumenty:

          connection -- spojen�, kter� m� b�t uzav�eno, instance
            '_postgresql_Connection'

        V�t�to t��d� metoda ned�l� nic.
        
        """
        pass
    
    def _postgresql_initialize_connection(self, connection):
        """Prove� pot�ebn� inicializace nov�ho spojen� 'connection'.
        """
        self._postgresql_initialize_transactions(connection)
        self._postgresql_initialize_coding(connection)

    def _postgresql_initialize_transactions(self, connection):
        """Nastav zp�sob prov�d�n� transakc� pro konkr�tn� backend."""
        # Nastavujeme serializovan� transakce, abychom v�r�mci jedn� transakce
        # nemohli dostat r�zn� v�sledky pro opakovan� selecty.
        def lfunction():
            self._postgresql_query(connection,
                                   ('set session characteristics as transaction '+
                                    'isolation level serializable'),
                                   False)
        with_lock(self._pg_query_lock, lfunction)

    def _postgresql_initialize_coding(self, connection):
        query = ['set client_encoding to "utf-8"']
        def lfunction():
            return self._postgresql_query(connection, query[0], False)
        with_lock(self._pg_query_lock, lfunction)
        query[0] = ("select pg_encoding_to_char(encoding) "
                    "from pg_database where datname = current_database()")
        result = with_lock(self._pg_query_lock, lfunction)
        coding = result[0].result().fetchone()[0]
        if coding != 'UTF8':
            self._pg_encoding = coding
        
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
        """Vra� instanci 'PostgreSQLResult' odpov�daj�c� v�sledku 'result'.

        Argumenty:

          result -- instance '_postgresql_Result'

        Tato metoda mus� b�t p�edefinov�na v�podt��d�.
        
        """
        raise ProgramError(_("Vol�na neimplementovan� metoda"))

    def _postgresql_begin_transaction(self):
        self._pg_query ('begin')
        
    def _postgresql_commit_transaction(self):
        self._pg_query ('commit')
        
    def _postgresql_rollback_transaction(self):
        self._pg_query ('rollback')


class PostgreSQLConnector(PostgreSQLAccessor):
    """T��da pro p��stup k�PostgreSQL na vy��� �rovni.

    T��da roz�i�uje funkce nadt��dy o�funkce vy��� �rovn� jako jsou spr�va
    spojen�, SQL inicializace p�i otev�r�n� spojen� nebo zpracov�n� v�sledk�
    SQL p��kaz�.

    """

    _pg_connection_pool_ = None
    
    def __init__(self, connection_data, **kwargs):
        """
        Arguments:

          connection_data -- 'DBConnection' instance
          kwargs -- propagated to superclass constructors

        """
        import config
        self._pg_encoding = None
        # Logov�n�
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
            def _lambda(connection_data=connection_data):
                return connection_data
            connection_data = _lambda
        self._pg_connection_data_ = connection_data
        self._pg_connections_ = []
        self._pg_query_lock = thread.allocate_lock()
        super(PostgreSQLConnector, self).__init__(
            connection_data=connection_data, **kwargs)

    def _pg_connection_pool(self):
        return PostgreSQLConnector._pg_connection_pool_

    def _pg_connection_data(self):
        return self._pg_connection_data_()

    def _pg_connections(self):
        return self._pg_connections_
    
    def _pg_get_connection(self, outside_transaction=False):
        connections = self._pg_connections()        
        if outside_transaction or not connections:
            pool = self._pg_connection_pool()
            return pool.get(self._pg_connection_data())
        else:
            return connections[-1]
        
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
                raise DBUserException(_("Data obsahuj� znaky, kter� nelze reprezentovat v k�dov�n� datab�ze"))
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
        # Prove� dotaz
        if __debug__:
            log(DEBUG, 'SQL query', query)
        def lfunction(connection=connection):
            try:
                result, connection = self._postgresql_query(connection, query,
                                                            outside_transaction,
                                                            query_args=query_args)
            finally:
                # Vra� DB spojen� zp�t
                if connection and transaction is None and outside_transaction:
                    self._pg_return_connection(connection)                  
            if backup and self._pdbb_logging_command:
                assert not outside_transaction, \
                    ('Backed up SQL command outside transaction', query)
                # Zde nem��e doj�t k�v�znamn� z�m�n� po�ad� zalogovan�ch
                # p��kaz�, proto�e v�echny DML p��kazy jsou uzav�eny
                # v�transakc�ch a ty konfliktn� jsou d�ky serializaci
                # automaticky spr�vn� �azeny.
                self._postgresql_query(connection,
                                       (self._pdbb_logging_command %
                                        (pg_escape(query),)),
                                       False)
            # Z�skej a vra� data
            return self._postgresql_transform_query_result(result)
        data = with_lock(self._pg_query_lock, lfunction)
        if __debug__:
            log(DEBUG, 'SQL query result', data)
        return data


class PostgreSQLUserGroups(PostgreSQLConnector):
    """T��da pro zji��ov�n� skupin u�ivatele."""
    
    _access_groups = {}
    _access_groups_data_objects = {}

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
        self._access_groups_data_objects[key] = self

    def _pgg_connection_key(self, connection_data):
        return connection_data.user(), connection_data.password()
        
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
        """Vra� sekvenci jmen skupin, do kter�ch pat�� p�ihl�en� u�ivatel.

        Nejsou-li skupiny u�ivatele zn�my, vra� 'None'.

        Argumenty:

          connection_data -- specifikace spojen�, jeho� skupiny maj� b�t
            vr�ceny

        Sekvence jmen skupin je updatov�na p�i ka�d�m vytvo�en� nov�ho
        spojen�.  Jm�na skupin jsou strings.

        """
        connection_data = self._pg_connection_data()
        key = self._pgg_connection_key(connection_data)
        groups = PostgreSQLUserGroups._access_groups.get(key, UNDEFINED)
        if groups is UNDEFINED:
            data = self._access_groups_data_objects[key]
            groups = PostgreSQLUserGroups._access_groups[key] = \
                self._pgg_retrieve_access_groups(data)
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

        # Jsou tu dva z�mky -- pozor na uv�znut�!

        def __init__(self, connection_data):
            if __debug__:
                log(DEBUG, 'Vytvo�en� notifik�toru')
            PostgreSQLConnector.__init__(self, connection_data)
            self._notif_data_lock = thread.allocate_lock()
            self._notif_data_objects = weakref.WeakKeyDictionary()
            self._notif_connection_lock = thread.allocate_lock()
            thread.start_new_thread(self._notif_listen, ())

        def _notif_do_registration(self, notification):
            self._pg_query('listen "%s"' % notification)
            
        def _notif_register(self, notification):
            # Zamyk�me zde kv�li mo�nosti sou�asn�ho vyvol�n� t�to metody
            # z�`register' i�naslouchac�ho threadu.
            if __debug__:
                log(DEBUG, 'Registruji notifikaci:', notification)
            def lfunction():
                self._notif_init_connection()
                self._notif_do_registration(notification)
            with_lock(self._notif_connection_lock, lfunction)
            if __debug__:
                log(DEBUG, 'Notifikace zaregistrov�na:', notification)

        def _notif_listen(self):
            if __debug__:
                log(DEBUG, 'Nov� listener')
            error_pause = 1
            self._notif_init_connection()
            while True:
                if __debug__:
                    log(DEBUG, 'Napichuji se na nov� spojen�')
                notiflist = []
                for d in self._notif_data_objects.values():
                    notiflist = notiflist + d
                if __debug__:
                    log(DEBUG, 'Notifikace k�registraci:', notiflist)                    
                notiflist = reduce(lambda x, y: x + y,
                                   self._notif_data_objects.values(), [])
                try:
                    # connection do poolu nikdy nevrac�me, tak�e na n�j m��eme
                    # nav�sit, co je n�m libo.
                    for n in remove_duplicates(notiflist):
                        self._notif_register(n)
                except pytis.data.DBException, e:
                    time.sleep(error_pause)
                    error_pause = error_pause * 2
                    continue
                self._notif_listen_loop()

        def _notif_listen_loop(self):
            raise Exception("Vol�na neimplementovan� metoda")

        def _notif_invoke_callbacks(self, notifications):
            if __debug__:
                log(DEBUG, 'Vol�m callbacky')
            def lfunction():
                return copy.copy(self._notif_data_objects)
            data_objects = with_lock(self._notif_data_lock, lfunction)
            for d, ns in data_objects.items():
                for n in ns:
                    if n in notifications:
                        if __debug__:
                            log(DEBUG, 'Vol�m callbacky datov�ho objektu:', d)
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
                log(DEBUG, 'Notifikace zaregistrov�na')

    def __init__(self, connection_data, **kwargs):
        """
        Argumenty:

          connection_data -- �daje o�spojen�, stejn� jako ve t��d� 'PostgreSQLConnector'
          kwargs -- k p�ed�n� p�edkovi
        
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
              self._PgNotifier(spec)
        for n in notifications:
            notifier.register_notification(self, n)


class PostgreSQLStandardBindingHandler(PostgreSQLConnector, DBData):
    """Interpretace s�mantiky specifikace napojen� do datab�ze.

    Tato t��da �e�� problematiku napln�n� v�znamu specifikace napojen� sloupc�
    datov� tabulky na data v�datab�zi PostgreSQL.  Ned�d� ��dnou datovou
    t��du, pouze implementuje metody t�kaj�c� se interpretace specifikace
    sloupc�, je tud� vhodn� k�pod�d�n� v�n�kter�m z�potomk� 'data.DBData'.

    Sou�asn� implementace t�to t��dy podporuje sloupcovou specifika�n� t��du
    'DBColumnBinding' a jen tuto t��du.  Pro bindings nav�c plat� n�sleduj�c�
    pravidla:

    - Mus� b�t specifikov�no alespo� jedno binding.

    - Modifikovat (insert, update, delete) lze pouze tabulku kl��e.

    - V�echny slo�ky kl��e mus� b�t z�t�e tabulky.

    Posledn� pravidlo se m��e zd�t p��li� omezuj�c�, av�ak nen� tomu tak,
    proto�e pr�ci s�v�cen�sobn�mi tabulkami je lep�� a jednodu��� implementovat
    pomoc� rules na serveru, ne� pomoc� tohoto aplika�n�ho rozhran�.  Je pouze
    zapot�eb�, aby datab�zov� stroj tuto funkcionalitu podporoval a aby tato
    podpora fungovala.
 
    **Pozor**: Metody modifikuj�c� tabulku se nestaraj� o�obecn� udr�en�
    integrity dat, tj. ohl�d�n� vlastnost� kl��� nebo referen�n� integrity je
    ponech�no na datab�zov�m stroji.  P�edpokl�d� se, �e v�p��pad� poru�en�
    pravidel definovan�ch v�datab�zov�m stroji je p��slu�n� transakce
    stornov�na.  Stejn� tak metoda 'delete' pracuje na tom principu, �e vyma�e
    ��dek *pouze z�tabulky prim�rn�ho kl��e* napojen�; p�edpokl�d� se, �e data
    v�ostatn�ch tabulk�ch budou smaz�na automaticky datab�zov�m strojem v�r�mci
    pravidel zachov�n� referen�n� integrity.
    
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
                if not args.has_key(k):
                    args[k] = v
            if callable(self._template):
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
        
    def __init__(self, bindings=None, ordering=None, **kwargs):
        super(PostgreSQLStandardBindingHandler, self).__init__(
            bindings=bindings, ordering=ordering, **kwargs)
        self._pdbb_create_sql_commands()
        
    def _pdbb_tabcol(self, table_name, column_name):
        """Vra� zadan� sloupec zform�tovan� pro SQL."""
        return '%s.%s' % (table_name, column_name)

    def _pdbb_btabcol(self, binding, full_text_handler=None):
        """Vra� sloupec z�'binding' zform�tovan� pro SQL."""
        if full_text_handler is not None and isinstance(binding.type(), FullTextIndex):
            result = full_text_handler(binding)
        else:
            result = self._pdbb_tabcol(binding.table(), binding.column())
        return result
        
    def _pdbb_coalesce(self, ctype, value):
        """Vra� string 'value' zabezpe�en� pro typ sloupce 'ctype' v�SQL."""
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

    def _pdbb_split_table_name(self, table):
        items = table.split('.')
        if len(items) < 2:
            items.insert(0, 'public')
        return items

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
        
    def _pdbb_get_table_type(self, table, column, ctype=None, type_kwargs=None):
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
            raise pytis.data.DBException(_("Nen� mo�no zjistit typ sloupce"), None,
                                         table, column)
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
        # Zde lze doplnit dal�� pou��van� standardn� typy z�PostgreSQL
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
                        'tsvector': FullTextIndex,
                        'varchar': String,
                        'inet': Inet,
                        'macaddr': Macaddr,
                        'bytea': Binary,
                        'oid': pytis.data.Oid, # for backward compatibility
                        }
        try:
            type_class_ = TYPE_MAPPING[type_]
        except KeyError:
            raise pytis.data.DBException('Unhandled database type', None, type_)
        if ctype is None:
            if type_kwargs is None:
                type_kwargs = {}
            if not_null in (1, 'T') and not type_kwargs.has_key('not_null'):
                type_kwargs['not_null'] = True
            if not type_kwargs.has_key('unique') and type_class_ != Boolean:
                type_kwargs['unique'] = unique
            enumerator = type_kwargs.get('enumerator')
            if enumerator and isinstance(enumerator, DataEnumerator):
                enumerator.set_data_factory_kwargs(connection_data=self._pg_connection_data())
            if type_class_ == String:
                if type_ != 'text':
                    try:
                        size = int(size_string) - 4
                    except:
                        size = None
                    if size < 0:
                        size = None
                    type_kwargs['maxlen'] = size
            elif type_class_ == Float:
                spec = int(size_string)
                precision = (spec & 0xFFFF) - 4
                if precision < 0 or precision > 100:
                    precision = None
                type_kwargs['precision'] = precision
            elif type_class_ == Integer and serial:
                type_class_ = Serial
            result = type_class_(**type_kwargs)
        else:
            assert isinstance(ctype, type_class_), \
                   ("User type doesn't match DB type", ctype, type_class_)
            result = ctype
        return result

    def _db_bindings_to_column_spec(self, bindings):
        key = []
        columns = []
        for b in bindings:
            if not b.id():              # skryt� sloupec
                continue
            t = self._pdbb_get_table_type(b.table(), b.column(), b.type(), type_kwargs=b.kwargs())
            colspec = ColumnSpec(b.id(), t)
            columns.append(colspec)
            if b in self._key_binding:
                assert not isinstance(t, Binary), ("Binary types may not be "+
                                                   "used as keys")
                key.append(colspec)
        assert key, DBUserException('data key column not found')
        # Hotovo
        return columns, tuple(key)

    def _pdbb_sql_column_list_from_names(self, column_names, full_text_handler=None):
        bindings = [self._db_column_binding(name) for name in column_names]
        return self._pdbb_sql_column_list(bindings, full_text_handler)
        
    def _pdbb_sql_column_list(self, bindings, full_text_handler=None):
        column_names = [self._pdbb_btabcol(b, full_text_handler) for b in bindings if b.id()]
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
        
    def _pdbb_create_sql_commands(self):
        """Vytvo� �ablony SQL p��kaz� pou��van� ve ve�ejn�ch metod�ch."""
        bindings = self._bindings
        for b in bindings:
            assert isinstance(b, DBColumnBinding), \
                   ('Unsupported binding specification', b)
        # P�iprav parametry
        column_list = self._pdbb_sql_column_list(bindings,
                                                 full_text_handler=self._pdbb_full_text_handler)
        table_names = map(lambda b: b.table(), bindings)
        table_names = remove_duplicates(table_names)
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
        keytabcols = [self._pdbb_btabcol(b) for b in self._key_binding]
        assert len (keytabcols) == 1, ('Multicolumn keys no longer supported', keytabcols)
        first_key_column = keytabcols[0]
        key_cond = '%s=%%(key)s' % (first_key_column,)
        if self._distinct_on:
            distinct_columns_string = self._pdbb_sql_column_list_from_names(self._distinct_on)
            distinct_on = " DISTINCT ON (%s)" % (distinct_columns_string,)
        else:
            distinct_on = ''
        def sortspec(dir, self=self, keytabcols=keytabcols):
            items = []
            for i in range(len(keytabcols)):
                k = keytabcols[i]
                items.append('%s %s' % (k, dir,))
            spec = string.join(items, ',')
            return spec
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
                    ("select definition from pg_views where viewname = '%s'" %
                     (main_table,)),
                    outside_transaction=True)
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
                    ("select ev_action from "+
                     "pg_rewrite join pg_class on (pg_rewrite.ev_class = pg_class.oid) "+
                     "where relname='%s'") % (main_table,),
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
        # Vytvo� �ablony p��kaz�
        self._pdbb_command_row = \
          self._SQLCommandTemplate(
            ('select %%(columns)s from %s where %s%s order by %s %%(supplement)s' %
             (table_list, relation_and_condition, filter_condition, ordering,)),
            {'columns': column_list, 'supplement': '', 'condition': 'true'})
        if distinct_on:
            self._pdbb_command_count = \
                "select count(*) from (select%s * from %s%%(fulltext_queries)s where %%(condition)s and (%s)%s) as %s" % \
                (distinct_on, table_list, relation, filter_condition, table_names[0])
        else:
            self._pdbb_command_count = \
                'select count(%s) from %s%%(fulltext_queries)s where %%(condition)s and (%s)%s' % \
                (first_key_column, table_list, relation, filter_condition,)
        self._pdbb_command_distinct = \
          'select distinct %%s from %s where %%s and (%s)%s order by %%s' % \
          (table_list, relation, filter_condition,)
        if distinct_on:
            self._pdbb_command_select = \
                self._SQLCommandTemplate(
                (('declare %s scroll cursor for select %%(columns)s from '+
                  '(select %s * from %s%%(fulltext_queries)s '+
                  'where %%(condition)s and (%s)%s) '+
                  'as %s order by %%(ordering)s %s') %
                 (cursor_name, distinct_on, table_list, relation, filter_condition,
                  table_names[0], ordering,)),
                {'columns': column_list})
        else:
            self._pdbb_command_select = \
                self._SQLCommandTemplate(
                (('declare %s scroll cursor for select %%(columns)s from %s%%(fulltext_queries)s '+
                  'where %%(condition)s and (%s)%s order by %%(ordering)s %s') %
                 (cursor_name, table_list, relation, filter_condition, ordering,)),
                {'columns': column_list})
        self._pdbb_command_close_select = \
            self._SQLCommandTemplate('close %s' % (cursor_name,))
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
        self._pdbb_command_search_first = \
          self._SQLCommandTemplate(
            (('select %%(columns)s from %s where (%s)%s and %%(condition)s '+
              'order by %%(ordering)s %s limit 1') %
             (main_table, relation, filter_condition, ordering,)),
            {'columns': column_list})
        self._pdbb_command_search_last = \
          self._SQLCommandTemplate(
            (('select %%(columns)s from %s where (%s)%s and %%(condition)s '+
              'order by %%(ordering)s %s limit 1') %
             (main_table, relation, filter_condition, rordering,)),
            {'columns': column_list})
        if distinct_on:
            self._pdbb_command_search_distance = \
                'select count(*) from (select%s * from %s where (%s)%s and %%s) as %s' % \
                (distinct_on, main_table, relation, filter_condition, table_names[0])
        else:
            self._pdbb_command_search_distance = \
                'select count(%s) from %s where (%s)%s and %%s' % \
                (first_key_column, main_table, relation, filter_condition,)
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
        self._pdbb_command_notify = \
          'notify "MODIF_%s"' % main_table
        self._pg_notifications = map(lambda t: 'MODIF_%s' % t, table_names)

    def _pdbb_condition2sql(self, condition):
        if condition == None:
            return 'true'
        op_name, op_args, op_kwargs = \
                 condition.name(), condition.args(), condition.kwargs()
        def relop(rel, args, kwargs):
            def colarg(colid):
                assert isinstance(colid, str), ('Invalid column name type', colid)
                col = self._db_column_binding(colid)
                assert col, ('Invalid column name', colid)
                a = self._pdbb_btabcol(col)
                t = self.find_column(colid).type()
                return a, t
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
            if kwargs.has_key('ignore_case') and kwargs['ignore_case'] and \
                   isinstance(t1, String) and isinstance(t2, String):
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
                     }
        if operators.has_key(op_name):
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
        else:
            raise ProgramError('Unknown operator', op_name)
        return '(%s)' % expression

    def _pdbb_sort2sql(self, sort):
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
            b = self._db_column_binding(id)
            return '%s %s' % (self._pdbb_btabcol(b, full_text_handler=full_text_handler), dir)
        sort_string = ','.join([item2sql(item) for item in sort])
        if sort_string:
            sort_string += ','
        return sort_string

    def _pdbb_fulltext_query_name(self, column_name):
        return '_pytis_ftq__%s' % (column_name,)
        
    # Metody souvisej�c� s�exportovan�mi metodami DB operac�
    
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

    def _pg_row (self, key_value, columns, transaction=None, supplement=''):
        """Retrieve and return raw data corresponding to 'key_value'."""
        args = {'key': key_value, 'supplement': supplement}
        if columns:
            args['columns'] = self._pdbb_sql_column_list_from_names(columns)
        query = self._pdbb_command_row.format(args)
        return self._pg_query(query, transaction=transaction)
    
    def _pg_search(self, row, condition, direction, transaction=None):
        sorting = self._pg_last_select_sorting
        if direction == FORWARD:
            pass
        elif direction == BACKWARD:
            sorting = reversed_sorting(sorting)
        else:
            raise ProgramError('Invalid direction', direction)
        def sorting_condition(sorting, forwards, row, mayeq):
            # - forwards je True:
            #   pak je row ��dek, na kter�m stoj�me a hled�me v�echny ��dky
            #   v sm�ru pohybu vyhled�v�n�
            # - forwards je False:
            #   pak je row ��dek vyhled�van� ��dek a hled�me v�echny ��dky,
            #   kter� jsou v protism�ru pohybu vyhled�v�n�.
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
                    conditions.append(apply(AND, conds))
                processed.append(cid)
            if mayeq:
                eqs = [EQ(c, row[c], ignore_case=False) for c in processed]
                conditions.append(apply(AND, eqs))
            return apply(OR, conditions)
        select_cond = self._pg_last_select_condition
        common_cond = AND(select_cond,
                          sorting_condition(sorting, True, row, False))
        sort_string = self._pdbb_sort2sql(sorting)
        # Najdi prvn� ��dek spl�uj�c� po�adovanou podm�nku
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
        query = sql_command.format(qargs)
        data_ = self._pg_query(query, transaction=transaction)
        if not data_:
            return 0
        # Zjisti vzd�lenost mezi aktu�ln�m a vyhledan�m ��dkem
        row_found = self._pg_make_row_from_raw_data(
            data_, template=self._pg_make_row_template_limited)
        search_cond = AND(common_cond,
                          sorting_condition(sorting, False,
                                            row_found, True))
        cond_string = self._pdbb_condition2sql(search_cond)
        data_ = self._pg_query((self._pdbb_command_search_distance %
                                (cond_string,)),
                               transaction=transaction)
        try:
            result = int(data_[0][0])
        except:
            raise ProgramError('Unexpected result', data_)
        return result

    def _pg_select (self, condition, sort, columns, transaction=None):
        """Initiate select and return the number of its lines or 'None'.

        Arguments:

          condition -- unprocessed conditional expression or 'None'
          sort -- unprocessed sorting specification or 'None'
          operation -- unprocessed specification of an aggregation function
          columns -- sequence of IDs of columns to select
          transaction -- transaction object
          
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
        data = self._pg_query(self._pdbb_command_count % args, transaction=transaction)
        if columns:
            args['columns'] = self._pdbb_select_column_list = \
                self._pdbb_sql_column_list_from_names(columns,
                                                      full_text_handler=self._pdbb_full_text_handler)
        else:
            self._pdbb_select_column_list = None
        args['selection'] = self._pdbb_selection_number = \
            self._pdbb_next_selection_number()
        query = self._pdbb_command_select.format(args)
        self._pg_query(query, transaction=transaction)
        try:
            result = int(data[0][0])
        except:
            raise DBSystemException('Unexpected SELECT result', data)
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
                for cid in colids:
                    t = self.find_column(cid).type()
                    assert isinstance(t, Number)
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
        def make_value(i):
            cid = colids[i]
            if operation == self.AGG_COUNT:
                t = I
            elif operation == self.AGG_AVG:
                t = F
            else:
                t = self.find_column(cid).type()
            value, error = t.validate(data[0][i])
            assert error is None, error
            return value
        result = [make_value(i) for i in range(len(colids))]
        return result

    def _pg_select_aggregate_1(self, operation, colids, condition, transaction=None):
        cond_string = self._pdbb_condition2sql(condition)
        colnames = [self._pdbb_btabcol(self._db_column_binding(cid)) for cid in colids]
        FMAPPING = {self.AGG_MIN: 'min',
                    self.AGG_MAX: 'max',
                    self.AGG_COUNT: 'count',
                    self.AGG_SUM: 'sum',
                    self.AGG_AVG: 'avg',}
        try:
            function = FMAPPING[operation]
        except KeyError:
            raise ProgramError('Invalid aggregate function identifier',
                               operation)
        function_list = ['%s (%s)' % (function, cname,) for cname in colnames]
        function_string = string.join(function_list, ', ')
        return self._pg_query(self._pdbb_command_select_agg %
                              (function_string, cond_string),
                              transaction=transaction)
    
    def _pg_fetchmany (self, count, direction, transaction=None):
        """Vra� 'count' ��dk� selectu jako raw data."""
        args = {'number': count, 'selection': self._pdbb_selection_number}
        if direction == FORWARD:
            query = self._pdbb_command_fetch_forward.format(args)
        elif direction == BACKWARD:
            query = self._pdbb_command_fetch_backward.format(args)
        else:
            raise ProgramError('Invalid direction', direction)
        return self._pg_query(query, transaction=transaction)

    def _pg_skip(self, count, direction, exact_count=False, transaction=None):
        """P�esko� 'count' ��dk� v�'direction' a vra� jejich po�et nebo 'None'.
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
        """Vlo� 'row' a vra� jej jako nov� raw data nebo vra� 'None'."""
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
        """Updatuj ��dky identifikovan� 'condition'.

        Vrac�: Po�et updatovan�ch ��dk�.

        """
        # TODO: P�i pou�it� RULEs v�PostgreSQL UPDATE vrac� v�dy�0.  Toto
        # chov�n� je sporn�, nicm�n� v�tuto chv�li PostgreSQL nenab�z� ��dn�
        # p��m� �e�en�, jak v�sledek UPDATE zjistit.  Proto zde aplikujeme
        # jak�si hack, kter� n�jak�m zp�sobem o�et�� alespo� n�kter� situace,
        # aby nebyl signalizov�n ne�sp�ch UPDATE v�p��pad� jeho �sp�chu.
        cols, vals = self._pdbb_table_row_lists(row)
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
        """Sma� ��dek identifikovan� podm�nkou 'condition'.

        Vrac�: Po�et smazan�ch ��dk�.

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
        """Roze�li notifikace o�modifikaci tohoto datov�ho objektu."""
        self._pg_query(self._pdbb_command_notify, outside_transaction=True)


class DBDataPostgreSQL(PostgreSQLStandardBindingHandler, PostgreSQLNotifier):
    """Datov� tabulka s�napojen�m do PostgreSQL.

    Tato t��da p�ekl�d� po�adavky do SQL, nen� v�ak implementa�n� z�visl� na
    konkr�tn�m pou�it�m postgresov�m modulu pro Python.
    
    """
    # TODO: Tato t��da je mamut a m�la by b�t rozd�lena na n�kolik men��ch ��st�

    _PG_LOCK_TABLE = '_rowlocks_real'
    _PG_LOCK_TABLE_LOCK = '_rowlocks_real'
    _PG_LOCK_TIMEOUT = 30         # perioda updatu v�sekund�ch

    class _PgBuffer:
        # D��ve to b�val buffer, nyn� se p�em��uje na "buffer-cache".

        def __init__(self):
            if __debug__: log(DEBUG, 'Nov� buffer')
            self.reset()
            
        def reset(self):
            """Kompletn� resetuj buffer."""
            if __debug__: log(DEBUG, 'Resetuji buffer')
            self._buffer = []
            # _dbpointer ... pozice ukazov�tka kursoru v�datab�zi, na kter�
            #   prvek kursoru po��naje od 0 ukazuje
            # _dbposition ... pozice za��tku bufferu v�datab�zi, ��slo prvku
            #   kurzoru po��naje�0, kter� odpov�d� prvn�mu prvku bufferu
            # _pointer ... pozice ukazov�tka v�bufferu, ukazuje na posledn�
            #   p�e�ten� prvek, nemus� v�dy ukazovat dovnit� bufferu
            self._dbposition = 0
            self._dbpointer = self._pointer = -1

        def current(self):
            """Vra� aktu�ln� ��dek a jeho pozici v�datab�zi po��naje od 0.

            V�sledek je vr�cen jako dvojice (ROW, POSITION).  Je-li aktu�ln�
            ��dek mimo buffer, je ROW 'None'.  Je-li aktu�ln� pozice mimo
            buffer, je POSITION je -1 nebo po�et ��dk� selectu.

            """
            buffer = self._buffer
            pointer = self._pointer
            position = self._dbposition + pointer
            if pointer < 0 or pointer >= len(buffer):
                row = None
            else:
                row = buffer[pointer]
            return row, position

        def fetch(self, direction, number_of_rows):
            """Vra� ��dek nebo 'None' a updatuj ukazov�tka.

            Pokud ��dek nen� v�bufferu, je vr�ceno 'None' a p�edpokl�d� se
            n�sledn� vol�n� metod 'correction()' a 'fill()'.

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
                # Intern� ukazov�tko po oby�ejn�m minut� neupdatujeme, proto�e
                # p�ijde fill a pokus o�znovuvyta�en� hodnoty, s�nov�m updatem
                # ukazov�tka.  Av�ak pokud jsme kompletn� mimo rozsah dat, nen�
                # tato zdr�enlivost nam�st� a je nutno ukazov�tko posunout na
                # spr�vnou pozici, tj. mimo rozsah dat.
                if pos < 0:
                    self._pointer = -1 - self._dbposition
                elif pos >= number_of_rows:
                    self._pointer = number_of_rows - self._dbposition
                return None
            self._pointer = pointer
            result = buffer[pointer]
            if __debug__: log(DEBUG, 'Buffer hit:', pointer) #, str(result))
            return result

        def correction(self, direction, number_of_rows):
            """Vra� argument pro DB operaci SKIP p�ed napln�n�m bufferu.

            Kladn� n�vratov� hodnota odpov�d� posunu vp�ed, z�porn� posunu
            zp�t.
            
            Datab�zov� ukazov�tko je updatov�no jako kdyby SKIP byl proveden.

            """
            if __debug__: log(DEBUG, '��dost o�korekci:',
                (self._dbpointer, self._dbposition, self._pointer, direction))
            pointer = self._pointer
            buflen = len(self._buffer)
            pos = self._dbposition + pointer
            if pointer > buflen or pointer < -1:
                # Dostali jsme se daleko za buffer, je nutno prov�st DB skip.
                # TODO: zde by mohlo b�t dobr� nastavit pozici tak, aby byla
                # na�tena je�t� n�jak� data proti sm�ru bufferu.  Jak to ale
                # ud�lat �ist�?
                if pos >= 0:
                    pos = min(pos, number_of_rows)
                else:
                    pos = max(pos, -1)
                correction = pos - self._dbpointer
                self._buffer = []
                self._pointer = -1
                self._dbpointer = self._dbpointer + correction
                self._dbposition = self._dbpointer + 1
            elif (direction == FORWARD and pointer >= buflen - 1) or \
                 (direction == BACKWARD and pointer <= 1):
                # Jsme u�hranice bufferu, provedeme DB skip bez maz�n� bufferu.
                # Rozsah v�podm�nce je zvolen tak, aby o�et�il i�p�edchoz�
                # buffer miss.
                correction = pos - self._dbpointer
                self._dbpointer = self._dbpointer + correction
            else:
                # Jsme uvnit� bufferu, ��dn� DB skip se nekon�.
                correction = 0
            if __debug__: log(DEBUG, 'Ur�en� korekce:', correction)
            return correction

        def goto(self, position):
            """Updatuj datab�zovou pozici nastavenou bez v�dom� bufferu.

            Argumenty:

              position -- ��slo prvku cursoru za��naj�c�ho od�0, na kter�
                ukazov�tko datab�zov�ho kurzoru pr�v� ukazuje

            """
            self._pointer = position - self._dbposition
            self._dbpointer = position
            
        def skip(self, count, direction, number_of_rows):
            """Prove� skip.

            Argumenty:

              count -- po�et ��dk�, o�kolik se m� skok prov�st
              direction -- jedna ze sm�rov�ch konstant modulu
              number_of_rows -- po�et ��dk� v�aktu�ln�m selectu

            Vrac�: Po�et skute�n� p�esko�en�ch ��dk� ve sm�ru 'direction'.
            
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
            elif pos > number_of_rows:
                pointer = number_of_rows - self._dbposition
            result = pointer - self._pointer
            if direction == BACKWARD:
                result = -result
            self._pointer = pointer
            return result
        
        def fill(self, rows, direction, extra_move=False):
            """Napl� se daty 'rows' a updatuj ukazov�tka."""
            # extra_move je tu kv�li tomu, �e pokud dojde ve fetchmany
            # k�p�ekro�en� hranic dat je�t� p�ed z�sk�n�m po�adovan�ho po�tu
            # ��dk�, mus� b�t dbpointer p�esunut je�t� o�jednu pozici d�l (mimo
            # data).
            if __debug__: log(DEBUG, 'Pln�m buffer:', direction)            
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

        def copy(self):
            """Vra� \"rozumnou\" kopii instance."""
            # Nepou��v�me postupy modulu `copy', proto�e pot�ebujeme n�co mezi
            # hlubokou a m�lkou kopi�.
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
        """Inicializuj datab�zovou tabulku dle uveden�ch specifikac�.

        Argumenty:
        
          bindings -- stejn� jako v�p�edkovi
          key -- binding kl��ov�ho sloupce datov� tabulky, mus� b�t jeden
            z�prvk� 'bindings' nebo sekvence prvk� z�'bindings'
          connection_data -- instance t��dy 'DBConnection' definuj�c�
            parametry p�ipojen�, nebo funkce bez argument� vracej�c� takovou
            instanci 'DBConnection'
          ordering -- stejn� jako v�p�edkovi
        
        """
        if __debug__:
            log(DEBUG, 'Vytv���m datab�zovou tabulku')
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
        self._pg_make_row_template = \
            self._pg_create_make_row_template(self._columns)
        self._pg_make_row_template_limited = None
        # NASTAVEN� CACHE
        # Proto�e pro r�zn� parametry (rychlost linky mezi serverem a klientem,
        # velikost pam�ti atd.), je vhodn� r�zn� nastaven� cache,
        # budeme parametry nastavovat z konfigura�n�ho souboru.
        # Pozor, config.cache_size je vyu��v�no p��mo v _PgBuffer.
        # Zde tyto hodnoty zapamatujeme jako atributy objektu, proto�e jsou
        # pot�eba v kritick�ch ��stech k�du a �ten� konfigurace p�eci jen trv�.
        import config
        self._pg_initial_fetch_size = config.initial_fetch_size
        self._pg_fetch_size = config.fetch_size

    # Metody pro transakce

    def _pg_allocate_connection(self):
        connections = self._pg_connections()
        if __debug__:
            if len(connections) >= 3:
                log(DEBUG, 'Podez�ele velk� hloubka spojen�:', len(connections))
        connection = self._pg_get_connection(outside_transaction=True)
        connections.append(connection)
        
    def _pg_deallocate_connection(self):
        self._pg_return_connection(self._pg_connections().pop())

    def _pg_begin_transaction (self):
        if self._pg_is_in_select:
            self.close()
        self._pg_allocate_connection()
        self._postgresql_begin_transaction()
        
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

    # Pomocn� metody

    def _pg_create_make_row_template(self, columns):
        template = []
        for c in columns:
            id = c.id()
            type = c.type()
            if isinstance(type, String):
                typid = 0
            elif isinstance(type, (Time, DateTime)):
                typid = 2
            else:
                typid = 99
            template.append((id, typid, type))
        return template

    def _pg_limited_make_row_template(self, columns):
        return [item for item in self._pg_make_row_template
                if item[0] in columns]
    
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
                    v = unicode(dbvalue, 'utf-8')  #TODO: pat�� jinam
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
        if __debug__: log(DEBUG, 'Vytv���m podm�nku z�kl��e:', key)
        key = xtuple(key)
        keycols = map(lambda b: b.id(), self._key_binding)
        assert len(keycols) == len(key), ('Invalid key length', key, keycols)
        ands = map(EQ, keycols, key)
        condition = apply(AND, ands)
        if __debug__: log(DEBUG, 'Podm�nka z�kl��e vytvo�ena:', condition)
        return condition

    def _pg_connection_maker(self):
        def maker():
            self._pg_new_connection(self._pg_connection_data(), self)
        return maker

    def _pg_restore_select(self):
        row_number = self._pg_last_select_row_number
        if row_number is None:
            return False
        self.select(condition=self._pg_last_select_condition,
                    sort=self._pg_last_select_sorting,
                    transaction=self._pg_last_select_transaction)
        self.skip(row_number)
        return True

    # Ve�ejn� metody a jimi p��mo volan� abstraktn� metody

    def row(self, key, columns=None, transaction=None):
        #log(EVENT, 'Zji�t�n� obsahu ��dku:', key)
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
                                transaction=transaction)
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        result = self._pg_make_row_from_raw_data(data, template=template)
        #log(EVENT, 'Vr�cen� obsah ��dku', result)
        return result
        
    def select(self, condition=None, sort=(), reuse=False, columns=None, transaction=None):
        if __debug__:
            log(DEBUG, 'Select started:', condition)
        if (reuse and not self._pg_changed and self._pg_number_of_rows and
            condition == self._pg_last_select_condition and
            sort == self._pg_last_select_sorting and
            transaction is self._pg_last_select_transaction):
            use_cache = True
        else:
            use_cache = False
        if self._pg_is_in_select: 
            self.close()
        if transaction is None:
            self._pg_begin_transaction ()
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
            number_of_rows = self._pg_select(condition, sort, columns, transaction=transaction)
        except:
            cls, e, tb = sys.exc_info()
            try:
                if transaction is None:
                    self._pg_rollback_transaction()
            except:
                pass
            self._pg_is_in_select = False
            raise cls, e, tb
        if use_cache and number_of_rows != self._pg_number_of_rows:
            use_cache = False
        if use_cache:
            self._pg_buffer.goto(-1)
        else:
            self._pg_buffer.reset()
            self._pg_initial_select = True
        self._pg_number_of_rows = number_of_rows
        return number_of_rows

    def select_aggregate(self, operation, condition=None, transaction=None):
        return self._pg_select_aggregate(operation[0], (operation[1],),
                                         condition=condition, transaction=transaction)[0]
    
    def select_and_aggregate(self, operation, condition=None, reuse=False, sort=(),
                             columns=None, transaction=None):
        if columns is None:
            columns = [c.id() for c in self.columns()]
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
        """Vra� sekvenci v�ech nestejn�ch hodnot dan�ho sloupce.

        Argumenty:

          column -- column identifier
          prefix -- lenght of a string prefix to work on (integer).  If not 'None', only given
            initial substring of column's value is considered by the query.  Only applicable for
            columns of string types.
          condition -- conditional expression as an Operator instance or 'None'
          sort -- one of 'ASCENDENT', 'DESCENDANT' constants or None
          transaction -- transaction object to be used when running the SQL
            commands

        """
        return self._pg_distinct(column, prefix, condition, sort, transaction=transaction)

    def fetchone(self, direction=FORWARD, transaction=None):
        """Stejn� jako v�nadt��d�.

        Metoda automaticky nereaguje na notifikace o�zm�n� dat souvisej�c�ch
        tabulek a pokra�uje (nedojde-li k�p�eru�en� transakce) v�dod�vce
        star�ch dat.  Automatick� p�ena��t�n� dat p�i zm�n� by mohlo v�st
        k�v�konnostn�m probl�m�m, jeho prov�d�n� je tedy ponech�no na uv�en�
        aplikace, kter� se m��e nechat o�zm�n�ch informovat registrac�
        prost�ednictv�m metody 'add_callback_on_change()'.

        """
        if __debug__:
            log(DEBUG, 'Vyta�en� ��dku ze selectu ve sm�ru:', direction)
        assert direction in(FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        if not self._pg_is_in_select:
            if not self._pg_restore_select():
                raise ProgramError('Not within select')
        # Tady za��n� opravdov� vyta�en� aktu�ln�ch dat
        buffer = self._pg_buffer
        row = buffer.fetch(direction, self._pdbb_select_rows)
        if row:
            result = row
        else:
            # Kurzory v�PostgreSQL maj� spoustu chyb.  Nap��klad �asto
            # kolabuj� p�i p�ekro�en� hranic dat a mnohdy spr�vn� nefunguje
            # FETCH BACKWARD.  V n�sleduj�c�m k�du se sna��me n�kter�
            # nej�ast�j�� chyby PostgreSQL obej�t.
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
            skip()
            if self._pg_initial_select:
                self._pg_initial_select = False
                std_size = self._pg_initial_fetch_size
            else:
                std_size = self._pg_fetch_size
            current_row_number = buffer.current()[1]
            last_row_number = min(current_row_number + 1,
                                  self._pg_number_of_rows)
            if direction == FORWARD:
                size = min(self._pg_number_of_rows-last_row_number, std_size)
            else:
                if current_row_number <= 0:
                    if current_row_number == 0:
                        skipped = buffer.skip(1, BACKWARD,
                                              self._pg_number_of_rows)
                        assert skipped == 1, skipped
                        skip()
                    return None
                size = min(self._pg_number_of_rows, std_size)
            assert size >= 0 and size <= self._pg_number_of_rows
            if direction == FORWARD:
                xskip = None
            else:
                xskip = buffer.skip(size, BACKWARD, self._pg_number_of_rows)
                skip()
            try:
                if size != 0:
                    data_ = self._pg_fetchmany(size, FORWARD, transaction=transaction)
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
        self._pg_last_fetch_row = result
        if __debug__: log(DEBUG, 'Vr�cen� ��dek', str(result))
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
        if __debug__: log(DEBUG, 'P�esko�en� ��dk�:', (direction, count))
        assert type(count) == type(0) and count >= 0, \
               ('Invalid count', count)
        assert direction in (FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        result = self._pg_buffer.skip(count, direction,
                                      self._pg_number_of_rows)
        if count > 0:
            self._pg_last_fetch_row = None
        if __debug__: log(DEBUG, 'P�esko�eno ��dk�:', result)
        return result

    def rewind(self):
        if not self._pg_is_in_select:
            if not self._pg_restore_select():
                raise ProgramError('Not within select')                
        __, pos = self._pg_buffer.current()
        if pos >= 0:
            self.skip(pos+1, BACKWARD)
        
    def search(self, condition, direction=FORWARD, transaction=None):
        """Vyhledej ve sm�ru 'direction' prvn� ��dek od 'row' dle 'condition'.

        Vrac�: Vzd�lenost od ��dku 'row' jako kladn� integer nebo 0, pokud
        takov� ��dek neexistuje.

        """
        if __debug__: log(DEBUG, 'Hled�n� ��dku:', (condition, direction))
        assert direction in (FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        if not self._pg_is_in_select:
            if not self._pg_restore_select():
                raise ProgramError('Not within select')
        row, pos = self._pg_buffer.current()
        if not row and pos >= 0 and pos < self._pg_number_of_rows:
            self.skip(1, BACKWARD)
            row = self.fetchone()
        if not row and (pos < 0 and direction == BACKWARD or \
                        pos >= 0 and direction == FORWARD):
            result = 0
        else:
            try:
                result = self._pg_search(row, condition, direction,
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
        if __debug__: log(DEBUG, 'V�sledek hled�n�:', result)
        return result

    def close(self):
        if __debug__:
            log(DEBUG, 'Explicitly closing current select')
        if self._pg_is_in_select:
            _, self._pg_last_select_row_number = self._pg_buffer.current()
        if self._pg_is_in_select is True: # no user transaction
            self._pg_commit_transaction()
        elif self._pg_is_in_select: # inside user transaction
            query = self._pdbb_command_close_select.format(
                {'selection': self._pdbb_selection_number})
            self._pg_query(query, transaction=self._pg_is_in_select)
        self._pg_is_in_select = False
        # Flush cached data
        self._pg_buffer.reset()

    def insert(self, row, after=None, before=None, transaction=None):
        assert after is None or before is None, 'Both after and before specified'
        log(ACTION, 'Insert row:', (row, after, before))
        if transaction is None:
            self._pg_begin_transaction ()
        try:
            # Jestli�e je definov�no ordering, kter� je sou��st� kl��e, bude
            # nov� vlo�en� ��dek nutn� unik�tn�.
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
            if not result:
                return "No such record"
        except DBLockException:
            log(EVENT, 'Row already locked by another process')
            self._pg_query('rollback to _lock', transaction=transaction)
            return "Record locked by another process"
        log(EVENT, 'Row locked')
        return None


class DBPostgreSQLCounter(PostgreSQLConnector, Counter):
    """��ta� ulo�en� v�PostgreSQL."""
    
    def __init__(self, name, connection_data):
        """Inicializuj ��ta�.

        Argumenty:

          name -- identifik�tor ��ta�e v�datab�zi, string
          connection_data -- instance t��dy 'DBConnection' definuj�c�
            parametry p�ipojen�, nebo funkce bez argument� vracej�c� takovou
            instanci 'DBConnection'

        """
        assert is_string(name)
        PostgreSQLConnector.__init__(self, connection_data)
        self._name = name
        self._query = "select nextval('%s')" % name
        
    def next(self):
        result = self._pg_query(self._query)
        try:
            number = int(result[0][0])
        except Exception, e:
            raise pytis.data.DBException(_("Chybn� hodnota ��ta�e z datab�ze"), e)
        return number


class DBPostgreSQLFunction(Function, DBDataPostgreSQL,
                           PostgreSQLStandardBindingHandler):
    """PostgreSQL implementation of the 'Function' class."""
    
    def __init__(self, name, connection_data, result_columns=None, **kwargs):
        """
        Arguments:

          name -- jm�no funkce jako nepr�zdn� string
          connection_data -- instance t��dy 'DBConnection' definuj�c�
            parametry p�ipojen�, nebo funkce bez argument� vracej�c� takovou
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
        type_query = ("select proretset, prorettype, proargtypes from pg_proc"+
                      " where proname = '%s'") % self._name
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
