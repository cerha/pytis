# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2008 Brailcom, o.p.s.
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

"""PostgreSQL database interface using Python Database API.

The particular Database API implementation is selected using the 'dbapi' import
line.

As Python Database API doesn't define notification interface, it is
implementation specific and has to be implemented separately for each supported
Database API implementation.

"""

import select
import types as pytypes

import psycopg2 as dbapi

from pytis.util import *
from dbdata import *
from postgresql import *


class _DBAPIAccessor(PostgreSQLAccessor):

    @classmethod
    def _postgresql_open_connection(class_, connection_data):
        # Prepare connection data
        kwargs = {}
        for option, accessor in (('user', DBConnection.user),
                                 ('password', DBConnection.password),
                                 ('database', DBConnection.database),
                                 ('host', DBConnection.host),
                                 ('port', DBConnection.port)):
            # Note: 'port' argument is not defined in DB API.
            value = accessor(connection_data)
            if value != None:
                kwargs[option] = value
        # Open the connection
        try:
            connection = dbapi.connect(**kwargs)
        except dbapi.DatabaseError, e:
            # Does this error detection work for dbapi as well?
            if e.args:
                msg = e.args[0].lower()
                if msg.find('password') != -1 or \
                       msg.find('authentication failed') != -1:
                    raise DBLoginException()
            raise DBException(_("Can't connect to database"), e)
        return class_._postgresql_Connection(connection, connection_data)

    @classmethod
    def _postgresql_close_connection(class_, connection):
        connection.connection().close()
    
    def _postgresql_query(self, connection, query, outside_transaction, query_args=()):
        result = None
        def do_query(raw_connection):
            cursor = raw_connection.cursor()
            # query_args shouldn't be used when empty to prevent mistaken
            # '%' processing in `query'
            try:
                if query_args:
                    cursor.execute(query, query_args)
                else:
                    cursor.execute(query)
            finally:
                if outside_transaction:
                    raw_connection.commit()
            return cursor
        try:
            result = do_query(connection.connection())
        except dbapi.InterfaceError, e:
            raise DBUserException(None, e, e.args, query)
        except dbapi.NotSupportedError, e:
            if e.args and e.args[0].find('cannot perform INSERT RETURNING') != -1:
                # This is handled once again below since older dbapi versions report it as
                # ProgrammingError and newer versions as NotSupportedError.
                raise DBInsertException()
            raise DBUserException(None, e, e.args, query)
        except dbapi.ProgrammingError, e:
            if e.args:
                if e.args[0].find('could not obtain lock') != -1:
                    raise DBLockException()
                elif e.args[0].find('cannot perform INSERT RETURNING') != -1:
                    raise DBInsertException()
            raise DBUserException(None, e, e.args, query)
        except dbapi.DataError, e:
            raise DBUserException(None, e, e.args, query)
        except dbapi.OperationalError, e:
            if e.args and e.args[0].find('could not obtain lock') != -1:
                raise DBLockException()
            if not outside_transaction:
                raise DBSystemException(_("Database operational error"),
                                        e, e.args, query)
            cdata = connection.connection_data()
            connection = self._postgresql_new_connection(cdata)
            try:
                result = do_query(connection)
            except Exception, e:
                raise DBSystemException(_("Database operational error"),
                                        e, e.args, query)
        except dbapi.InternalError, e:
            raise DBException(None, e, query)
        except dbapi.IntegrityError, e:
            raise DBUserException(_("Database integrity violation"),
                                  e, e.args, query)
        return self._postgresql_Result(result), connection

    def _postgresql_transform_query_result(self, result):
        cursor = result.result()
        data = []
        if cursor.description:
            for i in range(cursor.rowcount):
                row = cursor.fetchone()
                row_data = []
                for col in row:
                    if col is None or isinstance(col, buffer):
                        coldata = col
                    elif col is True:
                        coldata = 'T'
                    elif col is False:
                        coldata = 'F'
                    else:
                        coldata = str(col)
                    row_data.append(coldata)
                data.append(row_data)
        else:
            data.append([cursor.rowcount])
        return PostgreSQLResult(data)

    def _postgresql_begin_transaction(self):
        # In psycopg2 `begin' is called automatically.
        # By disabling its explicit call we avoid PostgreSQL warnings about
        # transactions in progress.
        pass
    
    def _postgresql_commit_transaction(self):
        connection = self._pg_get_connection().connection()
        connection.commit()
        
    def _postgresql_rollback_transaction(self):
        connection = self._pg_get_connection().connection()
        connection.rollback()
        # For unknown reasons, connection client encoding gets reset after
        # rollback
        cursor = connection.cursor()
        cursor.execute('set client_encoding to "utf-8"')

    
class DBAPICounter(_DBAPIAccessor, DBPostgreSQLCounter):
    pass


class DBAPIFunction(_DBAPIAccessor, DBPostgreSQLFunction):
    pass


class DBAPITransaction(_DBAPIAccessor, DBPostgreSQLTransaction):
    pass


class DBAPIData(_DBAPIAccessor, DBDataPostgreSQL):

    # This part is psycopg specific
    
    class _PgNotifier(_DBAPIAccessor, PostgreSQLNotifier._PgNotifier):

        def __init__(self, connection_data):
            PostgreSQLNotifier._PgNotifier.__init__(self, connection_data)
            self._pgnotif_connection = None

        def _notif_do_registration(self, notification):
            if self._pgnotif_connection is None:
                connection_data = self._pg_connection_data()
                connection = self._postgresql_new_connection(connection_data)
                self._pgnotif_connection = connection
                connection.connection().set_isolation_level(0)
            connection = self._pgnotif_connection
            query = 'listen %s' % (notification,)
            # TODO: Allow reconnection with re-registrations            
            def lfunction():
                return self._postgresql_query(connection, query, False)
            _result, self._pgnotif_connection = \
                with_lock(self._pg_query_lock, lfunction)
        
        def _notif_listen_loop(self):
            connection_ = self._pgnotif_connection
            connection = connection_.connection()
            while True:
                if __debug__:
                    log(DEBUG, 'Hlídám vstup', connection)
                def lfunction():
                    cursor = connection.cursor()
                    fileno = cursor.fileno()
                    return cursor, fileno
                cursor, fileno = with_lock(self._pg_query_lock, lfunction)
                try:
                    select.select([fileno], [], [], None)
                except Exception, e:
                    if __debug__:
                        log(DEBUG, 'Chyba na socketu', e.args)
                    break
                if __debug__:
                    log(DEBUG, 'Přišel vstup')
                def lfunction():
                    notifications = []
                    if cursor.isready():
                        notifies = connection.notifies
                        if notifies:
                            if __debug__:
                                log(DEBUG, 'Zaregistrována změna dat')
                            notifications = []
                            while notifies:
                                notifications.append(notifies.pop()[1])
                    return notifications
                notifications = with_locks((self._notif_connection_lock,
                                            self._pg_query_lock,),
                                           lfunction)
                if __debug__:
                    log(DEBUG, 'Načteny notifikace:', notifications)
                self._notif_invoke_callbacks(notifications)


### Defaults


class DBDataDefaultClass(PostgreSQLUserGroups, RestrictedData, DBAPIData):
    """Datová třída, kterou v našich aplikacích standardně používáme.

    Je utvořena pouhým složením existujících tříd a nezavádí žádnou další novou
    funkcionalitu kromě konstruktoru.

    """    
    def __init__(self, bindings, key, connection_data=None, ordering=None,
                 access_rights=AccessRights((None, (None, Permission.ALL))),
                 dbconnection_spec=None, **kwargs):
        # TODO: Vyřadit dbconnection_spec ze seznamu argumentů po konverzi
        # aplikací.
        if dbconnection_spec is not None:
            if connection_data is not None:
                raise Exception("Programming error: " +
                                "Both connection_data and dbconnection_spec given")
            connection_data = dbconnection_spec
        super(DBDataDefaultClass, self).__init__(
            bindings=bindings, key=key, connection_data=connection_data,
            ordering=ordering, access_rights=access_rights, **kwargs)
        # TODO: Následující hack je tu proto, že ve voláních konstruktorů výše
        # je _pg_add_notifications voláno předčasně, přičemž pořadí volání
        # konstruktorů nelze změnit.  Pro nápravu je potřeba ještě předělat
        # třídy týkající se notifikací.
        import config
        if config.dblisten:
            self._pg_add_notifications()


### Exportované proměnné/třídy


DBDataDefault = DBDataDefaultClass
"""Podtřída 'DBData', kterou používáme pro přístup k databázi."""

DBCounterDefault = DBAPICounter
"""Podtřída třídy 'Counter', která je standardně používána."""

DBFunctionDefault = DBAPIFunction
"""Podtřída třídy 'Function', která je standardně používána."""

DBTransactionDefault = DBAPITransaction
"""Standard transaction class."""

def _postgresql_access_groups(connection_data):
    import pytis.data.dbapi
    class PgUserGroups(pytis.data.dbapi._DBAPIAccessor,
                       PostgreSQLUserGroups):
        pass
    return PgUserGroups(connection_data).access_groups()
default_access_groups = _postgresql_access_groups
"""Funkce vracející seznam skupin uživatele specifikovaného spojení."""
