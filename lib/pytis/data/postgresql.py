# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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

from pytis.data import *
from dbdata import *


# Modifikace tabulek se oznamuje zasl�n�m notifikace `MODIF_table', kde `table'
# je jm�no modifikovan� tabulky.


def pg_escape(string_):
    return string_.replace("\\", "\\\\").replace("'", "\\'")


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
        
    def _postgresql_open_connection(self, connection_data):
        """Vytvo� a vra� nov� spojen� do datab�ze.

        N�vratovou hodnotou je instance '_postgresql_Connection'.

        Argumenty:

          connection_data -- dictionary obsahuj�c� p�ihla�ovac� �daje jako
            stroj, port, u�ivatel, heslo, atd.

        Tato metoda mus� b�t p�edefinov�na v�podt��d�.

        """
        raise ProgramError(_("Vol�na neimplementovan� metoda"))

    def _postgresql_close_connection(self, connection):
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
        self._postgresql_query(connection,
                               ('set session characteristics as transaction '+
                                'isolation level serializable'),
                               False)

    def _postgresql_initialize_coding(self, connection):
        encoding = self._pg_encoding
        query = 'set client_encoding to "%s"' % (encoding,)
        self._postgresql_query(connection, query, False)
        
    def _postgresql_query(self, connection, query, restartable):
        """Prove� SQL p��kaz 'query' a vra� v�sledek.

        Argumenty:

          connection -- instance '_postgresql_Connection'
          query -- string obsahuj�c� fin�ln� podobu SQL p��kazu, kter� se m�
            prov�st
          restartable -- pr�v� kdy� je pravda, je povoleno pokusit se o�restart
            spojen� v�p��pad� chyby

        N�vratovou hodnotou je dvojice ('result', 'connection'), kde 'result'
        je instance '_postgresql_Result' a 'connection' je instance
        '_postgresql_Connection' pou�it�ho spojen�.

        Tato metoda mus� b�t p�edefinov�na v�podt��d�.

        """
        raise ProgramError(_("Vol�na neimplementovan� metoda"))

    def _postgresql_transform_query_result(self, result):
        """Vra� instanci 'PostgreSQLResult' odpov�daj�c� v�sledku 'result'.

        Argumenty:

          result -- instance '_postgresql_Result'

        Tato metoda mus� b�t p�edefinov�na v�podt��d�.
        
        """
        raise ProgramError(_("Vol�na neimplementovan� metoda"))


class PostgreSQLConnector(PostgreSQLAccessor):
    """T��da pro p��stup k�PostgreSQL na vy��� �rovni.

    T��da roz�i�uje funkce nadt��dy o�funkce vy��� �rovn� jako jsou spr�va
    spojen�, SQL inicializace p�i otev�r�n� spojen� nebo zpracov�n� v�sledk�
    SQL p��kaz�.

    """
    
    def __init__(self, connection_data, **kwargs):
        """
        Arguments:

          connection_data -- 'DBConnection' instance
          kwargs -- propagated to superclass constructors

        """
        import config
        # K�dov�n�
        self._pg_encoding = config.db_encoding
        # Logov�n�
        if config.dblogtable:
            self._pdbb_logging_command = \
                "insert into %s (command) values ('%%s')" % config.dblogtable
        else:
            self._pdbb_logging_command = None
        # Spr�va spojen�
        PostgreSQLConnector._pg_connection_pool_ = \
            DBConnectionPool(self._postgresql_new_connection,
                             self._postgresql_close_connection)
        if isinstance(connection_data, DBConnection):
            def _lambda(connection_data=connection_data):
                return connection_data
            connection_data = _lambda
        self._pg_connection_data_ = connection_data
        self._pg_connections_ = []
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

    def _pg_query(self, query, outside_transaction=False, backup=False):
        """Prove� SQL p��kaz 'query' a vra� v�sledek.

        Argumenty:
        
          query -- SQL p��kaz PostgreSQL jako string
          outside_transaction -- pr�v� kdy� je pravda, je query provedeno mimo
            aktu�ln� prov�d�nou transakci, je-li jak�
          backup -- pr�v� kdy� je pravda, zaloguj proveden� SQL p��kaz
          
        N�vratovou hodnotou je instance t��dy 'PostgreSQLResult'.
        
        Metoda mus� ��dn� o�et�ovat v�jimky a v�p��pad� jejich v�skytu nahodit
        v�jimku 'DBException' nebo n�kter�ho jej�ho potomka.
        
        """
        if type(query) is pytypes.UnicodeType:
            query = query.encode(self._pg_encoding)
        connection = self._pg_get_connection(outside_transaction)
        # Prove� dotaz
        if __debug__:
            log(DEBUG, 'SQL dotaz', query)
        try:
            result, connection = self._postgresql_query(connection, query,
                                                        outside_transaction)
        finally:
            # Vra� DB spojen� zp�t
            if connection and outside_transaction:
                self._pg_return_connection(connection)
        if backup and self._pdbb_logging_command:
            assert not outside_transaction, \
                ('Backed up SQL command outside transaction', query)
            # Zde nem��e doj�t k�v�znamn� z�m�n� po�ad� zalogovan�ch
            # p��kaz�, proto�e v�echny DML p��kazy jsou uzav�eny
            # v�transakc�ch a ty konfliktn� jsou d�ky serializaci
            # automaticky spr�vn� �azeny.
            self._pg_query(connection,
                           self._pdbb_logging_command % pg_escape(query),
                           outside_transaction=False, backup=False)
        # Z�skej a vra� data
        data = self._postgresql_transform_query_result(result)
        if __debug__:
            log(DEBUG, 'V�sledek SQL dotazu', data)
        return data


class PostgreSQLUserGroups(PostgreSQLConnector):
    """T��da pro zji��ov�n� skupin u�ivatele."""
    
    _access_groups = {}

    def _postgresql_initialize_connection(self, connection):
        superclass = super(PostgreSQLUserGroups, self)
        superclass._postgresql_initialize_connection(connection)
        self._pgg_update_user_groups(connection)

    def _pgg_update_user_groups(self, connection):
        key = self._pgg_connection_key(connection.connection_data())
        PostgreSQLUserGroups._access_groups[key] = self

    def _pgg_connection_key(self, connection_data):
        return connection_data.user(), connection_data.password()
        
    def _pgg_retrieve_access_groups(self, data):
        if __debug__:
            log(DEBUG, 'Updatuji seznam skupin u�ivatel�')
        d = data._pg_query("select groname, grolist from pg_group",
                           outside_transaction=True)
        regexp = None
        the_user = data._pg_connection_data().user()
        groups = []
        for group, uid_string in d:
            if uid_string is not None and regexp is None:
                if uid_string != '{}':
                    uids = uid_string[1:-1].split(',')
                    for u in uids:
                        d1 = data._pg_query("select pg_get_userbyid(%s)" % u,
                                            outside_transaction=True)
                        user = d1[0][0]
                        if user == the_user:
                            regexp = re.compile('[^0-9]%s[^0-9]' % u)
                            groups.append(group)
                            break
            else:
                if uid_string is not None and regexp.search(uid_string):
                    groups.append(group)
        if __debug__:
            log(DEBUG, 'Seznam skupin u�ivatel� updatov�n')
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
        groups = PostgreSQLUserGroups._access_groups.get(key)
        if isinstance(groups, PostgreSQLConnector):
            groups = PostgreSQLUserGroups._access_groups[key] = \
                self._pgg_retrieve_access_groups(groups)
        return groups

    # TODO: Temporary compatibility hack:
    def class_access_groups(connection_data):
        import pytis.data.pgsql
        class PgUserGroups(pytis.data.pgsql._PgsqlAccessor,
                           PostgreSQLUserGroups):
            pass
        return PgUserGroups(connection_data).access_groups()
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
            self._pg_query('listen %s' % notification)
            
        def _notif_register(self, notification):
            # Zamyk�me zde kv�li mo�nosti sou�asn�ho vyvol�n� t�to metody
            # z�`register' i�naslouchac�ho threadu.
            if __debug__:
                log(DEBUG, 'Registruji notifikaci:', notification)
            lock = self._notif_connection_lock
            lock.acquire()
            try:
                self._notif_do_registration(notification)
            finally:
                lock.release()
            if __debug__:
                log(DEBUG, 'Notifikace zaregistrov�na:', notification)

        def _notif_listen(self):
            if __debug__:
                log(DEBUG, 'Nov� listener')
            error_pause = 1
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
                except DBException, e:
                    time.sleep(error_pause)
                    error_pause = error_pause * 2
                    continue
                self._notif_listen_loop()

        def _notif_listen_loop(self):
            raise Exception("Vol�na neimplementovan� metoda")

        def _notif_invoke_callbacks(self, notifications):
            if __debug__:
                log(DEBUG, 'Vol�m callbacky')
            lock = self._notif_data_lock
            lock.acquire()
            try:
                data_objects = copy.copy(self._notif_data_objects)
            finally:
                lock.release()
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
            lock = self._notif_data_lock
            lock.acquire()
            try:
                try:
                    notifications = self._notif_data_objects[data]
                except KeyError:
                    self._notif_data_objects[data] = notifications = []
                notification = notification.lower()
                notifications.append(notification)
            finally:
                lock.release()
            self._notif_register(notification)
            if __debug__:
                log(DEBUG, 'Notifikace zaregistrov�na')

    def __init__(self, connection_data, **kwargs):
        """
        Argumenty:

          connection_data -- �daje o�spojen�, stejn� jako ve t��d� 'PostgreSQLConnector'
          kwargs -- k p�ed�n� p�edkovi
        
        """
        super(PostgreSQLNotifier, self).__init__(connection_data=connection_data,
                                                 **kwargs)
        self._pg_changed = False
        # Pozor, notifikace sm� b�t registrov�ny a� nakonec po proveden� v�ech
        # inicializac�!  Pozor na potomky!
        self._pg_notifications = []
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

    def __init__(self, bindings=None, ordering=None, **kwargs):
        super(PostgreSQLStandardBindingHandler, self).__init__(
            bindings=bindings, ordering=ordering, **kwargs)
        self._pdbb_create_sql_commands()
        
    def _pdbb_tabcol(self, table_name, column_name):
        """Vra� zadan� sloupec zform�tovan� pro SQL."""
        return '%s.%s' % (table_name, column_name)

    def _pdbb_btabcol(self, binding):
        """Vra� seznam sloupc� z�'binding' zform�tovan�ch pro SQL."""
        cols = xtuple(binding.column())
        return map(lambda c, tc=self._pdbb_tabcol, t=binding.table(): \
                   tc(t, c),
                   cols)

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
        elif isinstance(ctype, DateTime):
            default = "'0000-01-01'"
            cast = '::date'
        elif isinstance(ctype, Boolean):
            default = "'F'"
            cast = '::bool'
        else:
            default = "''"
            cast = ''
        #return 'coalesce(%s%s, %s%s)' % (value, cast, default, cast)
        return '%s%s' % (value, cast)

    def _pdbb_get_table_type(self, table, column, ctype, type_kwargs=None):
        d = self._pg_query(
            ("select pg_type.typname, pg_attribute.atttypmod, "+\
             "pg_attribute.attnotnull "+\
             "from pg_class, pg_attribute, pg_type "+\
             "where pg_class.oid = pg_attribute.attrelid and "+\
             "pg_class.relname = '%s' and "+\
             "pg_attribute.attname = '%s' and "+\
             "pg_attribute.atttypid = pg_type.oid") % \
            (table, column),
            outside_transaction=True)
        d1 = self._pg_query(
            ("select pg_attrdef.adsrc "+\
             "from pg_class, pg_attribute, pg_attrdef "+\
             "where pg_class.oid = pg_attrdef.adrelid and "+\
             "pg_class.oid = pg_attribute.attrelid and "+\
             "pg_class.relname = '%s' and "+\
             "pg_attribute.attname = '%s' and "+\
             "pg_attribute.attnum = pg_attrdef.adnum") % \
            (table, column),
            outside_transaction=True)
        try:
            type_, size_string, not_null = d[0]
        except:
            raise DBException(_("Nen� mo�no zjistit typ sloupce"), None,
                              table, column, d)
        try:
            default = d1[0][0]
        except:
            default = ''
        serial = (default[:len('nextval')] == 'nextval')
        return self._pdbb_get_type(type_, size_string, not_null, serial,
                                   ctype=ctype, type_kwargs=type_kwargs)
    
    def _pdbb_get_type(self, type_, size_string, not_null, serial,
                       ctype=None, type_kwargs=None):
        # Zde lze doplnit dal�� pou��van� standardn� typy z�PostgreSQL
        TYPE_MAPPING = {'bool': Boolean,
                        'bpchar': String,
                        'char': String,
                        'date': Date,
                        'time': Time,
                        'smallint': Integer,                        
                        'int2': Integer,                        
                        'int4': Integer,
                        'int8': Integer,
                        'numeric': Float,
                        'float4': Float,
                        'float8': Float,
                        'oid': Oid,
                        'name': String,
                        'text': String,
                        'timestamp': DateTime,
                        'timestamptz': DateTime,
                        'varchar': String,
                        'inet': Inet,
                        'macaddr': Macaddr
                        }
        try:
            type_class_ = TYPE_MAPPING[type_]
        except KeyError:
            raise DBException('Unhandled database type', None, type_)
        if ctype is None:
            if type_kwargs is None:
                type_kwargs = {}
            if not_null in (1, 'T') and not type_kwargs.has_key('not_null'):
                type_kwargs['not_null'] = True
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
            enumerator, kwargs = b.enumerator(), copy.copy(b.kwargs())
            if enumerator:
                df_kwargs = {'connection_data': self._pg_connection_data()}
                e_kwargs = {'data_factory_kwargs': df_kwargs}
                for a in ('value_column', 'validity_column',
                          'validity_condition'):
                    if kwargs.has_key(a):
                        e_kwargs[a] = kwargs[a]
                        del kwargs[a]
                kwargs['enumerator'] = DataEnumerator(enumerator, **e_kwargs)
                #TODO: Toto je hack kv�li zp�tn� kompatibilit�...
                if not kwargs.has_key('not_null'):
                    kwargs['not_null'] = True
            t = self._pdbb_get_table_type(b.table(), b.column(), b.type(),
                                          type_kwargs=kwargs)
            colspec = ColumnSpec(b.id(), t)
            columns.append(colspec)
            if b in self._key_binding:
                key.append(colspec)
        assert key, DBUserException('data key column not found')
        # P�idej oid
        if not some(lambda c: isinstance(c.type(), Oid), columns):
            columns.append(ColumnSpec('oid', Oid()))
        # Hotovo
        return columns, tuple(key)

    def _pdbb_create_sql_commands(self):
        """Vytvo� �ablony SQL p��kaz� pou��van� ve ve�ejn�ch metod�ch."""
        bindings = self._bindings
        for b in bindings:
            assert isinstance(b, DBColumnBinding), \
                   ('Unsupported binding specification', b)
        # P�iprav parametry
        visible_bindings = filter(lambda b: b.id(), bindings)
        column_names = map(lambda b: self._pdbb_btabcol(b), visible_bindings)
        column_names = reduce(lambda x, y: x + y, column_names, [])
        column_list = string.join(column_names, ', ')
        table_names = map(lambda b: b.table(), bindings)
        table_names = remove_duplicates(table_names)
        table_list = string.join(table_names, ', ')
        if len(table_names) <= 1:
            relation = 'true'
        else:
            rels = []
            for b in bindings:
                related = b.related_to()
                if related:
                    next_rels = map (lambda c, r: '%s=%s' % (c, r),
                                     self._pdbb_btabcol(b),
                                     self._pdbb_btabcol(related))
                    rels = rels + next_rels
            relation = string.join(rels, ' and ')
        main_table = self._key_binding[0].table()
        keytabcols = map(self._pdbb_btabcol, self._key_binding)
        keytabcols = reduce(operator.add, keytabcols, [])
        key_eqs = map(lambda k: '%s=%%s' % k, keytabcols)
        key_cond = string.join(key_eqs, ' and ')
        first_key_column = keytabcols[0]
        def sortspec(dir, self=self, keytabcols=keytabcols):
            items = []
            for i in range(len(keytabcols)):
                k = keytabcols[i]
                items.append('%s %s' % (k, dir))
            return string.join(items, ',')
        ordering = sortspec('ASC')
        rordering = sortspec('DESC')
        condition = key_cond
        relation_and_condition = '(%s) and (%s)' % (relation, condition)
        oids = filter(lambda c: isinstance(c.type(), Oid), self.columns())
        assert oids, ('No oids in the data object columns',
                      map(str, self.columns()))
        oidnames = map(lambda c, m=main_table: '%s.%s' % (m, c.id()), oids)
        oidstrings = string.join(oidnames, ', ')
        # Vytvo� �ablony p��kaz�
        self._pdbb_command_row = \
          'select %s, %s from %s where %s order by %s' % \
          (column_list, oidstrings, table_list, relation_and_condition,
           ordering)
        self._pdbb_command_count = \
          'select count(%s) from %s where %%s and (%s)' % \
          (first_key_column, table_list, relation)
        self._pdbb_command_distinct = \
          'select distinct %%s from %s where %%s and (%s) order by %%s' % \
          (table_list, relation)
        self._pdbb_command_select = \
          ('declare %s scroll cursor for select %s, %s from %s '+\
           'where %%s and (%s) order by %%s %s') % \
          (self._PDBB_CURSOR_NAME, column_list, oidstrings, table_list,
           relation, ordering)
        self._pdbb_command_select_agg = \
          ('select %%s(%%s) from %s where %%s and (%s)' %
           (table_list, relation))
        self._pdbb_command_fetch_forward = \
          'fetch forward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_fetch_backward = \
          'fetch backward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_move_forward = \
          'move forward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_move_backward = \
          'move backward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_search_first = \
          ('select %s, %s from %s where (%s) and %%s order by %%s %s '+\
           'limit 1') % \
           (column_list, oidstrings, main_table, relation, ordering)
        self._pdbb_command_search_last = \
          ('select %s, %s from %s where (%s) and %%s order by %%s %s '+\
           'limit 1') % \
           (column_list, oidstrings, main_table, relation, rordering)
        self._pdbb_command_search_distance = \
          'select count(%s) from %s where (%s) and %%s' % \
          (first_key_column, main_table, relation)
        self._pdbb_command_insert = \
          'insert into %s (%%s) values (%%s)' % main_table
        if self._ordering:
            ordering = ()
            for o in self._ordering:
                for b in self._bindings:
                    if b.id() == o:
                        c = xtuple(b.column())
                        break
                else:
                    raise ProgramError('Invalid ordering id', o)
                ordering = ordering + c
            ocol = ordering[0]
            eqs = map(lambda o: '%s=%%s' % o, ordering[1:])
            if eqs:
                eqstring = string.join(eqs, ' AND ')                
                xeqstring = ' AND ' + eqstring
            else:
                eqstring = xeqstring = ''
            self._pdbb_command_insert_shift = \
              ('update %s set %s=%s+1 where %s>=%%s %s' % \
               (main_table, ocol, ocol, ocol, xeqstring))
            self._pdbb_command_insert_newpos = \
              ('select max(%s) from %s where %s' % \
               (ocol, main_table, eqstring))
        oidordering = string.join(map(lambda o: '%s DESC' % o, oidnames),
                                  ', ')
        self._pdbb_command_get_last = \
          ('select %s, %s from %s where %s order by %s limit 1') % \
           (column_list, oidstrings, table_list, relation, oidordering)
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
          ("select 'yes' from pg_class, pg_rewrite "+\
           "where pg_rewrite.ev_type = 2 and "+\
           "pg_rewrite.is_instead = 't' and "+\
           "pg_class.oid = pg_rewrite.ev_class and "+\
           "pg_class.relname = '%s'") % main_table
        self._pdbb_command_delete = \
          'delete from %s where %%s' % main_table
        self._pdbb_command_notify = \
          'notify MODIF_%s' % main_table
        self._pg_notifications = map(lambda t: 'MODIF_%s' % t, table_names)

    def _pdbb_condition2sql(self, condition):
        if condition == None:
            return 'true'
        op_name, op_args, op_kwargs = \
                 condition.name(), condition.args(), condition.kwargs()
        def relop(rel, args, kwargs):
            assert len(args) == 2, ('Invalid number or arguments', args)
            colid, value = args
            assert is_string(colid), \
                   ('Invalid column name type', colid)
            assert isinstance(value, Value), \
                   ('Invalid value type', value)
            col = self._db_column_binding(colid)
            assert col, ('Invalid column name', colid)
            case_insensitive = kwargs.has_key('ignore_case') and \
                               kwargs['ignore_case'] and \
                               isinstance(value.type(), String)
            t = self.find_column(colid).type()
            btabcols = self._pdbb_btabcol(col)
            val = xtuple(self._pg_value(value))
            items = []
            for i in range(len(btabcols)):
                tabcol = btabcols[i]
                dbvalue = self._pdbb_coalesce(t, val[i])
                if case_insensitive:
                    tabcol = 'lower(%s)' % tabcol
                    dbvalue = 'lower(%s)' % dbvalue
                items.append((tabcol, dbvalue))
            def itemize(item, val):
                colid, dbval = item
                if val is 'NULL':       # fuj
                    op = 'IS'
                else:
                    op = '='
                return '%s %s %s' % (colid, op, dbval)
            if rel == '=':
                eqs = map(itemize, items, val)
                result = '(%s)' % string.join(eqs, ' AND ')
            else:
                eqs = []
                for i in range(len(items)):
                    ii = items[:i+1]
                    minieqs = ['(%s %s %s)' % (ii[-1][0], rel, ii[-1][1])]
                    for j in ii[:-1]:
                        minieqs.append('(%s = %s)' % j)
                    eqs.append('(%s)' % string.join(minieqs, ' AND '))
                result = '(%s)' % string.join(eqs, ' OR ')
            return result
        if op_name == 'EQ':
            expression = relop('=', op_args, op_kwargs)
        elif op_name == 'LT':
            expression = relop('<', op_args, op_kwargs)
        elif op_name == 'WM':
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
            c = self.find_column(cid)
            t = c.type()
            # TODO:
            # prov��it, zda po p�echodu na novou podobu ValueCodebooks, toto
            # ji� opravdu nen� zapot�eb�.
            #
            # if (isinstance(t, pytis.data.xtypes.Codebook) and
            #   t.strict()):
            #    def cformat(data, colid):
            #        binding = data._db_column_binding(colid)
            #        return data._pdbb_btabcol(binding)[0]
            #    cname = cformat(self, cid)
            #    cdata = pytis.data.xtypes._codebook_data(t)
            #    ccid = cdata.key()[0].id()
            #    ccname = cformat(cdata, ccid)
            #    tname = cdata._db_column_binding(ccid).table()
            #    vcname = t.value_column()
            #    cuname = cformat(cdata, vcname)
            #    spec = self._pg_value(spec)
            #    if op_kwargs.get('ignore_case'):
            #       def lower(x):
            #            return "lower(%s)" % x
            #        cuname = lower(cuname)
            #        spec = lower(spec)
            #    expression = ("%s IN (SELECT %s FROM %s WHERE %s LIKE %s)" %
            #                  (cname, ccname, tname, cuname, spec))
            #else:
            #    expression = relop('LIKE', (cid, spec), op_kwargs)
            expression = relop('LIKE', (cid, spec), op_kwargs)
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
                expression = string.join(exps, sqlop)
        elif op_name == 'IN':
            assert len(op_args) == 4, ('Invalid number or arguments', op_args)
            col, data, table_col, cond = op_args
            table = data._key_binding[0].table()
            condition = data._pdbb_condition2sql(cond)
            expression = '%s in (select %s from %s where %s)' % \
                         (col, table_col, table, condition)
        else:
            raise ProgramError('Unknown operator', op_name)
        return '(%s)' % expression

    def _pdbb_sort2sql(self, sort):
        def item2sql(item, self=self):
            if type(item) == type(()):
                id, dirspec = item
                if dirspec == ASCENDENT:
                    dir = 'ASC'
                elif dirspec == DESCENDANT:
                    dir = 'DESC'
                else:
                    raise ProgramError('Invalid sorting direction', dirspec)
            else:
                id, dir = item, 'ASC'
            b = self._db_column_binding(id)
            return map(lambda bt: '%s %s' % (bt, dir), self._pdbb_btabcol(b))
        items = map(item2sql, sort)
        sort_string = string.join(reduce(operator.add, items, []), ',')
        if sort_string:
            sort_string = sort_string + ','
        return sort_string
        
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
            if isinstance(colspec.type(), Oid):
                continue
            column = b.column()
            if is_sequence(column):
                columns = columns + list(column)
            else:                
                columns.append(column)
            if is_sequence(value):
                values = values + list(value)
            else:
                values.append(value)
        return columns, values

    def _pg_row (self, value):
        """Vyt�hni a vra� raw data odpov�daj�c� kl��ov� hodnot� 'value'."""
        return self._pg_query(self._pdbb_command_row % value)
    
    def _pg_search(self, row, condition, direction):
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
        data_ = self._pg_query(sql_command % (cond_string, sort_string))
        if not data_:
            return 0
        # Zjisti vzd�lenost mezi aktu�ln�m a vyhledan�m ��dkem
        row_found = self._pg_make_row_from_raw_data(data_)
        search_cond = AND(common_cond,
                          sorting_condition(sorting, False,
                                            row_found, True))
        cond_string = self._pdbb_condition2sql(search_cond)
        data_ = self._pg_query(self._pdbb_command_search_distance % \
                               cond_string)
        try:
            result = int(data_[0][0])
        except:
            raise ProgramError('Unexpected result', data_)
        return result

    def _pg_select (self, condition, sort):
        """Inicializuj select a vra� po�et ��dk� v�n�m nebo 'None'.

        Argumenty:

          condition -- nezpracovan� podm�nkov� v�raz nebo 'None'
          sort -- nezpracovan� specifikace t��d�n� nebo 'None'
          operation -- nezpracovan� specifikace agrega�n� funkce
          
        """
        cond_string = self._pdbb_condition2sql(condition)
        sort_string = self._pdbb_sort2sql(sort)
        data = self._pg_query(self._pdbb_command_count % cond_string)
        self._pg_query(self._pdbb_command_select %
                       (cond_string, sort_string))
        try:
            result = int(data[0][0])
        except:
            raise DBSystemException('Unexpected SELECT result', data)
        self._pdbb_select_rows = result
        return result

    def _pg_distinct (self, column, condition, sort):
        cond_string = self._pdbb_condition2sql(condition)
        sort_string = self._pdbb_sort2sql(((column, sort),))
        if sort_string.endswith(','):
            sort_string = sort_string[:-1]
        data = self._pg_query(self._pdbb_command_distinct % \
                              (column, cond_string, sort_string))
        tmpl = self._pg_create_make_row_template((self.find_column(column),))
        result = [self._pg_make_row_from_raw_data([r], tmpl)[column]
                  for r in data]
        return result
    
    def _pg_select_aggregate(self, operation, condition):
        cond_string = self._pdbb_condition2sql(condition)
        aggfun, colid = operation
        FMAPPING = {self.AGG_MIN: 'min',
                    self.AGG_MAX: 'max',
                    self.AGG_COUNT: 'count',
                    self.AGG_SUM: 'sum',
                    self.AGG_AVG: 'avg',}
        try:
            function = FMAPPING[aggfun]
        except KeyError:
            raise ProgramError('Invalid aggregate function identifier',
                               operation)
        return self._pg_query(self._pdbb_command_select_agg %
                              (function, colid, cond_string))
    
    def _pg_fetchmany (self, count, direction):
        """Vra� 'count' ��dk� selectu jako raw data."""
        if direction == FORWARD:
            query = self._pdbb_command_fetch_forward % count
        elif direction == BACKWARD:
            query = self._pdbb_command_fetch_backward % count
        else:
            raise ProgramError('Invalid direction', direction)
        return self._pg_query(query)

    def _pg_skip(self, count, direction, exact_count=False):
        """P�esko� 'count' ��dk� v�'direction' a vra� jejich po�et nebo 'None'.
        """
        if direction == FORWARD:
            self._pg_query(self._pdbb_command_move_forward % count)
        elif direction == BACKWARD:
            answer = self._pg_query(self._pdbb_command_move_backward % count)
            answer_count = answer[0][0]
            if exact_count and answer_count != count:
                log(OPERATIONAL, "Ne�ekan� v�sledek kurzorov� operace MOVE:",
                    (answer_count, count))
        else:
            raise ProgramError('Invalid direction', direction)
        return None
        
    def _pg_insert(self, row, after=None, before=None):
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
                params = map(lambda o, s=self, n=neighbor: s._pg_value(n[o]),
                             self._ordering[1:])
            except KeyError:
                raise ProgramError('Invalid column id in ordering',
                                   self._ordering, row)
            if n >= 0:
                params = [str(n)] + params
            params = tuple(params)
            if n >= 0:
                self._pg_query(self._pdbb_command_insert_shift % params,
                               backup=True)
            else:
                result = self._pg_query(self._pdbb_command_insert_newpos % \
                                        params)
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
        self._pg_query(self._pdbb_command_insert % \
                       (string.join(cols, ','), string.join(vals, ',')),
                       backup=True)
        # Pokud data nemaj� kl�� (proto�e je generov�n, nap�. jako SERIAL),
        # nezb�v�, ne� se ��dit oid.  Tento postup pak lze pou��t obecn�, snad
        # v�PostgreSQL funguje.  Je to zalo�eno na p�edpokladu, �e ka�d� nov�
        # vlo�en� z�znam m� nejvy��� oid v�tabulce.
        data = self._pg_query(self._pdbb_command_get_last)
        return self._pg_make_row_from_raw_data(data)
    
    def _pg_update(self, condition, row):
        """Updatuj ��dky identifikovan� 'condition'.

        Vrac�: Po�et updatovan�ch ��dk�.

        """
        # TODO: P�i pou�it� RULEs v�PostgreSQL UPDATE vrac� v�dy�0.  Toto
        # chov�n� je sporn�, nicm�n� v�tuto chv�li PostgreSQL nenab�z� ��dn�
        # p��m� �e�en�, jak v�sledek UPDATE zjistit.  Proto zde aplikujeme
        # jak�si hack, kter� n�jak�m zp�sobem o�et�� alespo� n�kter� situace,
        # aby nebyl signalizov�n ne�sp�ch UPDATE v�p��pad� jeho �sp�chu.
        cols, vals = self._pdbb_table_row_lists(row)
        s = map(lambda c, v: "%s=%s" % (c, v), cols, vals)
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
                     self._pg_query(self._pdbb_command_test_broken_update)
        if broken:
            d = self._pg_query(self._pdbb_command_broken_update_preselect % \
                               cond_string)
            result = extract_result(d)
        d = self._pg_query(self._pdbb_command_update % (settings, cond_string),
                           backup=True)
        if not broken:
            result = extract_result(d)
        if result >= 0:
            return result
        else:
            raise DBSystemException('Unexpected UPDATE value', None, result)

    def _pg_delete (self, condition):
        """Sma� ��dek identifikovan� podm�nkou 'condition'.

        Vrac�: Po�et smazan�ch ��dk�.

        """
        sql_condition = self._pdbb_condition2sql(condition)
        d = self._pg_query(self._pdbb_command_delete % sql_condition,
                           backup=True)
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

    _PG_LOCK_TABLE = '_rowlocks'
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
        super(DBDataPostgreSQL, self).__init__(
            bindings=bindings, key=key, connection_data=connection_data,
            **kwargs)
        self._pg_is_in_select = False
        self._pg_buffer = self._PgBuffer()
        self._pg_number_of_rows = None
        self._pg_initial_select = False
        self._pg_make_row_template = \
            self._pg_create_make_row_template(self._columns)
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
                if __debug__:
                    log(DEBUG, 'Podez�ele velk� hloubka spojen�:',
                        len(connections))
        connection = self._pg_get_connection(outside_transaction=True)
        connections.append(connection)
        
    def _pg_deallocate_connection(self):
        self._pg_return_connection(self._pg_connections().pop())

    def _pg_begin_transaction (self):
        self._pg_allocate_connection()
        self._pg_query ('begin')
        
    def _pg_commit_transaction (self):
        self._pg_query ('commit')
        self._pg_deallocate_connection()
        
    def _pg_rollback_transaction (self):
        self._pg_query ('rollback')
        self._pg_deallocate_connection()

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
    
    def _pg_make_row_from_raw_data(self, data_, template=None):
        if not data_:
            return None
        if not template:
            template = self._pg_make_row_template
        row_data = []
        data_0 = data_[0]
        i = 0
        for id, typid, type in template:            
            dbvalue = data_0[i]
            i += 1
            if typid == 0:              # string
                if dbvalue is None:
                    v = None
                else:
                    v = unicode(dbvalue, self._pg_encoding)  #TODO: pat�� jinam
                value = Value(type, v)
            elif typid == 2:            # time
                value, err = type.validate(dbvalue, strict=False,
                                           format=type.SQL_FORMAT, local=False)
                assert err is None, err
            else:
                value, err = type.validate(dbvalue, strict=False)
                assert err is None, err
            row_data.append((id, value))
        return Row(row_data)

    def _pg_already_present(self, row):
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
        return self.row(key)
    
    _pg_dt = type(mx.DateTime.DateTimeFrom('2001-01-01'))
    def _pg_value(self, value):
        if is_sequence(value):
            return tuple(map(self._pg_value, value))
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

    # Ve�ejn� metody a jimi p��mo volan� abstraktn� metody

    def row(self, key):
        #log(EVENT, 'Zji�t�n� obsahu ��dku:', key)
        try:
            data = self._pg_row (self._pg_value(key))
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        result = self._pg_make_row_from_raw_data(data)
        #log(EVENT, 'Vr�cen� obsah ��dku', result)
        return result
        
    def select(self, condition=None, sort=(), reuse=False):
        if __debug__: log(DEBUG, 'Zah�jen� selectu:', condition)
        if reuse and not self._pg_changed and self._pg_number_of_rows and \
               condition == self._pg_last_select_condition and \
               sort == self._pg_last_select_sorting:
            use_cache = True
        else:
            use_cache = False
        self.close()
        self._pg_begin_transaction ()
        self._pg_is_in_select = True
        self._pg_last_select_condition = condition
        self._pg_last_select_sorting = sort
        self._pg_last_fetch_row = None
        self._pg_changed = False
        try:
            number_of_rows = self._pg_select (condition, sort)
        except:
            cls, e, tb = sys.exc_info()
            try:
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

    def select_aggregate(self, operation, condition=None):
        opid = operation[0]
        t = self.find_column(operation[1]).type()
        if opid == self.AGG_COUNT:
            t = Integer()
        elif opid == self.AGG_AVG:
            if not isinstance(t, Number):
                return None
            t = Float()
        else:
            if not isinstance(t, Number):
                return None
        close_select = False
        if not self._pg_is_in_select:
            self.select(condition=condition)
            close_select = True
        try:
            data = self._pg_select_aggregate(operation, condition)
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            self._pg_is_in_select = False
            raise cls, e, tb
        if close_select:
            self.close()
        result, error = t.validate(data[0][0])
        assert error is None, error
        return result
        
    def distinct(self, column, condition=None, sort=ASCENDENT):
        """Vra� sekvenci v�ech nestejn�ch hodnot dan�ho sloupce.

        Argumenty:

          column -- identifik�tor sloupce.
          condition -- podm�nkov� v�raz nebo 'None'.
          sort -- jedna z konstant  'ASCENDENT', 'DESCENDANT' nebo None.

        """
        return self._pg_distinct(column, condition, sort)

    def fetchone(self, direction=FORWARD):
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
            # Pokusy o�rekonstrukci fetche nevedou k�ni�emu dobr�mu.  P�vodn�
            # tu bylo proveden� nov�ho selectu a hled�n� p�vodn�ho ��dku
            # v�n�m.  Pomineme-li mo�n� s�mantick� probl�my t�to operace,
            # nepom��e n�m toto ani v�konnostn�, proto�e mus�me prohledat cel�
            # data a� po k��en� m�sto -- to u� je ov�em m��eme rovnou znovu
            # na��st.
            #
            # Lep�� je pou��vat explicitn� nov� select s�argumentem `reuse'
            # v�kombinaci s�metodou `skip'.  P�i "pokra�ov�n�" selectu n�m v�ce
            # ne� o�ten sam� ��dek jde sp�e o�tu samou pozici v�datech, co�
            # tento postup podporuje.  P�i vhodn�m mechanismu bufferov�n�
            # odpadnou i�nejz�va�n�j�� v�konnostn� probl�my.
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
                                               exact_count=True)
                    except:
                        cls, e, tb = sys.exc_info()
                        try:
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
                data_ = self._pg_fetchmany(size, FORWARD)
            except:
                cls, e, tb = sys.exc_info()
                try:
                    self._pg_rollback_transaction()
                except:
                    pass
                self._pg_is_in_select = False
                raise cls, e, tb
            if data_:
                row_data = [self._pg_make_row_from_raw_data([d]) for d in data_]
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
            raise ProgramError('Not within select')
        __, pos = self._pg_buffer.current()
        if pos >= 0:
            self.skip(pos+1, BACKWARD)
        
    def search(self, condition, direction=FORWARD):
        """Vyhledej ve sm�ru 'direction' prvn� ��dek od 'row' dle 'condition'.

        Vrac�: Vzd�lenost od ��dku 'row' jako kladn� integer nebo 0, pokud
        takov� ��dek neexistuje.

        """
        if __debug__: log(DEBUG, 'Hled�n� ��dku:', (condition, direction))
        assert direction in (FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        if not self._pg_is_in_select:
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
                result = self._pg_search(row, condition, direction)
            except:
                cls, e, tb = sys.exc_info()
                try:
                    self._pg_rollback_transaction()
                except:
                    pass
                self._pg_is_in_select = False
                raise cls, e, tb
        if __debug__: log(DEBUG, 'V�sledek hled�n�:', result)
        return result

    def close(self):
        if __debug__: log(DEBUG, 'Explicitn� ukon�en� selectu')
        if self._pg_is_in_select:
            self._pg_commit_transaction()
            self._pg_is_in_select = False

    def insert(self, row, after=None, before=None):
        assert after is None or before is None, \
               'Both after and before specified'
        log(ACTION, 'Vlo�en� ��dku', (row, after, before))
        self._pg_begin_transaction ()
        try:
            # Jestli�e je definov�no ordering, kter� je sou��st� kl��e, bude
            # nov� vlo�en� ��dek nutn� unik�tn�.
            if (not self._ordering or \
                (self._ordering[0] not in map(lambda c: c.id(), self.key()))
                ) and \
               self._pg_already_present(row):
                msg = '��dek s�t�mto kl��em ji� existuje'
                result = msg, False
                log(ACTION, msg)
            else:
                positioned = after or before
                if after:
                    neighbor = after = self.row(after)
                elif before:
                    neighbor = before = self.row(before)
                if positioned and (not neighbor):
                    msg = 'Zadan� sousedn� ��dek nenalezen'
                    log(ACTION, msg,
                        (after, before))
                    result = msg, False
                else:
                    r = self._pg_insert (row, after=after, before=before)
                    result = r, True
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        self._pg_commit_transaction()
        self._pg_send_notifications()
        if result[1]:
            log(ACTION, '��dek vlo�en', result)
        return result
    
    def update(self, key, row):
        key = xtuple(key)
        log(ACTION, 'Update ��dku:', key)
        log(ACTION, 'Nov� data', str(row))
        self._pg_begin_transaction ()
        try:
            origrow = self.row(key)
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
                    msg = '��dek s�t�mto kl��em ji� existuje'
                    result = msg, False
                    log(ACTION, msg, key)
                else:
                    n = self._pg_update(self._pg_key_condition(key), row)
                    if n == 0:
                        result = None, False
                    else:
                        new_row = self.row(new_key)
                        result = new_row, True
            else: # not origrow
                msg = '��dek s�dan�m kl��em neexistuje'
                result = msg, False
                log(ACTION, msg, key)
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        if result[1]:
            log(ACTION, '��dek updatov�n', result)
        return result
    
    def update_many(self, condition, row):
        log(ACTION, 'Update ��dk�:', condition)
        log(ACTION, 'Nov� data', str(row))
        self._pg_begin_transaction ()
        try:
            ordering = self._ordering
            if ordering:
                new_row_items = []
                for k, v in row.items():
                    if k not in ordering:
                        new_row_items.append((k, v))
                row = Row(new_row_items)
            result = self._pg_update (condition, row)
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        if result:
            log(ACTION, '��dky updatov�ny:', result)
        return result

    def delete(self, key):
        log(ACTION, 'Maz�n� ��dku:', key)
        self._pg_begin_transaction ()
        try:
            result = self._pg_delete (self._pg_key_condition(key))
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        log(ACTION, '��dek smaz�n', result)
        return result
    
    def delete_many(self, condition):
        log(ACTION, 'Maz�n� ��dk�:', condition)
        self._pg_begin_transaction ()
        try:
            result = self._pg_delete (condition)
        except:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        log(ACTION, '��dky smaz�ny', result)
        return result

    def lock_row(self, key):
        # Pro zamyk�n� je vy�adov�na existence zamykac� tabulky se speci�ln�mi
        # vlastnostmi, kter� je definov�na v�souboru `db.sql'.
        log(EVENT, 'Zamyk�m ��dek:', map(str, xtuple(key)))
        self._pg_begin_transaction();
        try:
            self._pg_query('lock table %s' % self._PG_LOCK_TABLE_LOCK)
            row = self.row(key)
            if not row:
                self._pg_rollback_transaction()
                return 'Z�znam neexistuje'
            oidcols = filter(lambda c: isinstance(c.type(), Oid),
                             self.columns())
            cids = map(lambda c: c.id(), oidcols)
            oids = map(lambda i, row=row: row[i].value(), cids)
            for oid in oids:
                data = self._pg_query('select usename from %s where row = %d'\
                                      % (self._PG_LOCK_TABLE, oid))
                if data:
                    self._pg_rollback_transaction()
                    return 'u�ivatel `%s\'' % data[0][0]
            lock_ids = []
            for oid in oids:
                self._pg_query('insert into %s (row) values (%d)' % \
                               (self._PG_LOCK_TABLE, oid))
                data = self._pg_query('select id from %s where row = %d' % \
                                      (self._PG_LOCK_TABLE, oid))
                lock_ids.append(data[0][0])
            self._pg_lock_ids = lock_ids
            self._pg_commit_transaction()
        except DBException:
            cls, e, tb = sys.exc_info()
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise cls, e, tb
        DBDataPostgreSQL.__bases__[0].lock_row(self, key)
        update_commands = \
          map(lambda id, self=self: 'update %s set id = id where id = %s' % \
              (self._PG_LOCK_TABLE, id),
              lock_ids)
        thread.start_new_thread(self._pg_locking_process,
                                (key, update_commands))
        log(EVENT, '��dek zam�en')
        return None

    def unlock_row(self):
        log(EVENT, 'Odemyk�m ��dek')
        DBDataPostgreSQL.__bases__[0].unlock_row(self)
        for id in self._pg_lock_ids:
            try:
                self._pg_query('delete from %s where id = %s' % \
                               (self._PG_LOCK_TABLE, id),
                               outside_transaction=True)
            except DBException:
                pass
        self._pg_lock_ids = None
        log(EVENT, '��dek odem�en')

    def _pg_locking_process(self, locked_row, update_commands):
        if __debug__: log(DEBUG, 'Nastartov�n zamykac� proces')
        while True:
            time.sleep(self._PG_LOCK_TIMEOUT)
            if self._locked_row != locked_row:
                return
            for command in update_commands:
                try:
                    self._pg_query(command, outside_transaction=True)
                except DBException, e:
                    if __debug__:
                        log(DEBUG, 'Chyba p��kazu obnovy ��dku', (e, command))
            if __debug__: log(DEBUG, 'Z�mek updatov�n')



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
            raise DBException(_("Chybn� hodnota ��ta�e z datab�ze"), e)
        return number


class DBPostgreSQLFunction(Function, DBDataPostgreSQL,
                           PostgreSQLStandardBindingHandler):
    """Implementace t��dy 'Function' pro PostgreSQL.

    Podporov�ny jsou pouze funkce vracej�c� jedinou hodnotu.

    """
    def __init__(self, name, connection_data, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no funkce jako nepr�zdn� string
          connection_data -- instance t��dy 'DBConnection' definuj�c�
            parametry p�ipojen�, nebo funkce bez argument� vracej�c� takovou
            instanci 'DBConnection'
          kwargs -- k p�ed�n� p�edk�m

        """
        assert is_string(name)
        self._name = name
        bindings = ()
        super(DBPostgreSQLFunction, self).__init__(
            bindings=bindings, key=bindings, connection_data=connection_data,
            **kwargs)
        arg_query = "select pronargs from pg_proc where proname='%s'" % name
        data = self._pg_query(arg_query, outside_transaction=True)
        narg = int(data[0][0])
        arguments = string.join(('%s',)*narg, ', ')
        self._pdbb_function_call = 'select %s(%s)' % (name, arguments)
        
    def _db_bindings_to_column_spec(self, __bindings):
        type_query = ("select proretset, prorettype, proargtypes from pg_proc"+
                      " where proname = '%s'") % self._name
        self._pg_begin_transaction()
        try:
            data = self._pg_query(type_query)
            assert data, ('No such function', self._name)
            assert len(data) == 1, ('Overloaded functions not supported',
                                    self._name)
            r_set, r_type, arg_types = data[0]
            assert r_set == 'F', \
                   ('Multiset functions not supported', self._name)
            def type_instance(tnum):
                query = ("select typname, typlen from pg_type "+
                         "where oid = '%s'") % tnum
                data = self._pg_query(query)
                type_, size_string = data[0]
                t = self._pdbb_get_type(type_, size_string, False, False)
                return t
            r_type_instance = type_instance(r_type)
            columns = [ColumnSpec('', r_type_instance)]
        finally:
            self._pg_commit_transaction()
        return columns, ()

    def _pdbb_create_sql_commands(self):
        self._pg_notifications = []
    
    def call(self, row):
        log(EVENT, ('Vol�n� funkce `%s\'' % self._name))
        arguments = tuple(map(self._pg_value, row))
        data = self._pg_query(self._pdbb_function_call % arguments,
                              outside_transaction=True)
        result = self._pg_make_row_from_raw_data(data)
        log(EVENT, ('V�sledek vol�n� funkce `%s\':' % self._name), result)
        return [result]
