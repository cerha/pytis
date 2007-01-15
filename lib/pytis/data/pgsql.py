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

"Implementace datového rozhraní pro PostgreSQL prostřednictvím pyPgSQL."

import select
import types as pytypes

from pyPgSQL import libpq

from pytis.util import *
from postgresql import *


class _PgsqlAccessor(PostgreSQLAccessor):
    
    def _postgresql_open_connection(class_, connection_data):
        # Sestav connection string
        connection_string = ''
        for option, accessor in (('user', DBConnection.user),
                                 ('password', DBConnection.password),
                                 ('dbname', DBConnection.database),
                                 ('host', DBConnection.host),
                                 ('port', DBConnection.port)):
            value = accessor(connection_data)
            if value != None:
                connection_string += " %s='%s'" % (option, pg_escape(str(value)))
        # Otevři spojení
        if __debug__:
            log(DEBUG, 'Připojovací řetězec:', connection_string)
        try:
            connection = libpq.PQconnectdb(connection_string)
        except libpq.DatabaseError, e:
            if e.args:
                msg = e.args[0].lower()
                if msg.find('password') != -1 or \
                       msg.find('authentication failed') != -1:
                    raise DBLoginException()
            raise DBException(_("Nelze se připojit k databázi"), e)
        return class_._postgresql_Connection(connection, connection_data)
    #_postgresql_open_connection = classmethod(_postgresql_open_connection)
    
    def _postgresql_close_connection(class_, connection):
        connection.connection().finish()
    #_postgresql_close_connection = classmethod(_postgresql_close_connection)
    
    def _postgresql_query(class_, connection, query, restartable):
        result = None
        def do_query(connection):
            try:
                return connection.query(query)
            except:
                cls, e, tb = sys.exc_info()
                try:
                    connection.finish()     # pro jistotu
                except:
                    pass
                raise cls, e, tb
        try:
            result = do_query(connection.connection())
        except libpq.InterfaceError, e:
            raise DBUserException(None, e, query)
        except libpq.ProgrammingError, e:
            raise DBUserException(None, e, query)
        except libpq.OperationalError, e:
            if not restartable:
                raise DBSystemException(_("Operační chyba v databázi"), e,
                                        query)
            cdata = connection.connection_data()
            connection = class_._postgresql_new_connection(cdata)
            try:
                result = do_query(connection)
            except Exception, e:
                raise DBSystemException(_("Operační chyba v databázi"), e,
                                        query)
        except libpq.InternalError, e:
            raise DBException(None, e, query)
        except libpq.IntegrityError, e:
            raise DBUserException(_("Pokus o porušení integrity dat"),
                                  e, query)
        return class_._postgresql_Result(result), connection
    #_postgresql_query = classmethod(_postgresql_query)

    def _postgresql_transform_query_result(class_, result_):
        result = result_.result()
        if result.resultType == libpq.RESULT_DML:
            if result.cmdTuples:
                data = [[result.cmdTuples]]
            else:
                data = [['0']]
        elif result.resultType == libpq.RESULT_DDL: # move, begin, etc.
            data = result.cmdStatus
        else:
            data = []
            for row in range(result.ntuples):
                row_data = []
                for col in range(result.nfields):
                    try:
                        value = result.getvalue(row, col)                    
                    except libpq.InterfaceError, e:
                        raise DBUserException(None, e)
                    except libpq.TypeError, e:
                        raise DBUserException(None, e)
                    if value is libpq.PG_True:
                        value = 'T'
                    elif value is libpq.PG_False:
                        value = 'F'
                    elif value is None:
                        pass
                    elif (type(value) in (type(0), type(0L), type(0.0),
                                          type(libpq.PgInt2(0))) \
                          or hasattr(libpq,'PgInt8') \
                          and type(value) == type(libpq.PgInt8(0))):
                        value = str(value)                        
                    else:
                        assert is_string(value), \
                               DBException\
                               ('Unexpected pyPgSQL type received', None,
                                value)
                    row_data.append(value)
                data.append(row_data)
        return PostgreSQLResult(data)
#     _postgresql_transform_query_result = \
#         classmethod(_postgresql_transform_query_result)

    
class DBPyPgCounter(_PgsqlAccessor, DBPostgreSQLCounter):
    pass


class DBPyPgFunction(_PgsqlAccessor, DBPostgreSQLFunction):
    pass


class DBDataPyPgSQL(_PgsqlAccessor, DBDataPostgreSQL, PostgreSQLNotifier):

    class _PgNotifier(_PgsqlAccessor, PostgreSQLNotifier._PgNotifier):

        def __init__(self, connection_data):
            PostgreSQLNotifier._PgNotifier.__init__(self, connection_data)
            self._pgnotif_connection = None

        def _notif_do_registration(self, notification):
            if self._pgnotif_connection is None:
                connection_data = self._pg_connection_data()
                self._pgnotif_connection = \
                    self._postgresql_new_connection(connection_data)
            connection = self._pgnotif_connection
            query = 'listen %s' % (notification,)
            # TODO: Allow reconnection with re-registrations
            self._postgresql_query(connection, query, False)
        
        def _notif_listen_loop(self):
            connection_ = self._pgnotif_connection
            connection = connection_.connection()
            while True:
                if __debug__:
                    log(DEBUG, 'Hlídám vstup', connection)
                try:
                    select.select([connection.socket], [], [], None)
                except Exception, e:
                    if __debug__:
                        log(DEBUG, 'Chyba na socketu', e.args)
                    break
                if __debug__:
                    log(DEBUG, 'Přišel vstup')
                lock = self._notif_connection_lock
                lock.acquire()
                try:
                    try:
                        connection.consumeInput()
                        notice = connection.notifies()
                    except Exception, e:
                        if __debug__:
                            log(DEBUG, 'Databázová chyba', e.args)
                        break
                    notifications = []
                    if notice:
                        if __debug__:
                            log(DEBUG, 'Zaregistrována změna dat')
                    while notice:
                        n = notice.relname.lower()
                        notifications.append(n)
                        notice = connection.notifies()
                finally:
                    lock.release()
                if __debug__:
                    log(DEBUG, 'Načteny notifikace:', notifications)
                self._notif_invoke_callbacks(notifications)

    def __init__(self, bindings, key, dbconnection_spec, ordering=None):
        DBDataPostgreSQL.__init__(self, bindings, key, dbconnection_spec,
                                  ordering)

