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
from postgresql import *


class _DBAPIAccessor(PostgreSQLAccessor):

    def _postgresql_open_connection(self, connection_data):
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
        return self._postgresql_Connection(connection, connection_data)
    
    def _postgresql_close_connection(self, connection):
        connection.connection().close()
    
    def _postgresql_query(self, connection, query, restartable):
        result = None
        def do_query(raw_connection):
            try:
                cursor = raw_connection.cursor()
                cursor.execute(query)
                return cursor
            except:
                cls, e, tb = sys.exc_info()
                try:
                    raw_connection.close() # just to be sure
                except:
                    pass
                raise cls, e, tb
        try:
            result = do_query(connection.connection())
        except dbapi.InterfaceError, e:
            raise DBUserException(None, e, query)
        except dbapi.ProgrammingError, e:
            raise DBUserException(None, e, query)
        except dbapi.DataError, e:
            raise DBUserException(None, e, query)
        except dbapi.OperationalError, e:
            if not restartable:
                raise DBSystemException(_("Database operational error"), e,
                                        query)
            cdata = connection.connection_data()
            connection = self._postgresql_new_connection(cdata)
            try:
                result = do_query(connection)
            except Exception, e:
                raise DBSystemException(_("Database operational error"), e,
                                        query)
        except dbapi.InternalError, e:
            raise DBException(None, e, query)
        except dbapi.IntegrityError, e:
            raise DBUserException(_("Database integrity violation"),
                                  e, query)
        return self._postgresql_Result(result), connection

    def _postgresql_transform_query_result(self, result):
        cursor = result.result()
        data = []
        if cursor.description:
            for i in range(cursor.rowcount):
                row = cursor.fetchone()
                row_data = []
                for col in row:
                    if col is True:
                        row_data.append('T')
                    elif col is False:
                        row_data.append('F')
                    else:
                        row_data.append(str(col))
                data.append(row_data)
        else:
            data.append([cursor.rowcount])
        return PostgreSQLResult(data)

    
class DBAPICounter(_DBAPIAccessor, DBPostgreSQLCounter):
    pass


class DBAPIFunction(_DBAPIAccessor, DBPostgreSQLFunction):
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
            _result, self._pgnotif_connection = \
                self._postgresql_query(connection, query, False)
        
        def _notif_listen_loop(self):
            connection_ = self._pgnotif_connection
            connection = connection_.connection()
            while True:
                if __debug__:
                    log(DEBUG, 'Hlídám vstup', connection)
                cursor = connection.cursor()
                try:
                    select.select([cursor], [], [], None)
                except Exception, e:
                    if __debug__:
                        log(DEBUG, 'Chyba na socketu', e.args)
                    break
                if __debug__:
                    log(DEBUG, 'Přišel vstup')
                lock = self._notif_connection_lock
                lock.acquire()
                try:
                    notifications = []
                    if cursor.isready():
                        notifies = connection.notifies
                        if notifies:
                            if __debug__:
                                log(DEBUG, 'Zaregistrována změna dat')
                            notifications = []
                            while notifies:
                                notifications.append(notifies.pop()[1])
                finally:
                    lock.release()
                if __debug__:
                    log(DEBUG, 'Načteny notifikace:', notifications)
                self._notif_invoke_callbacks(notifications)
