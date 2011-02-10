# -*- coding: utf-8 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2011 Brailcom, o.p.s.
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

from pyPgSQL import libpq

from pytis.util import *
from dbdata import *
from postgresql import *


class _PgsqlAccessor(PostgreSQLAccessor):

    @classmethod
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
            raise DBException(_(u"Nelze se připojit k databázi"), e)
        return class_._postgresql_Connection(connection, connection_data)

    @classmethod
    def _postgresql_close_connection(class_, connection):
        connection.connection().finish()
    
    def _postgresql_query(self, connection, query, restartable, query_args=None):
        if query_args:
            raise Exception(_(u"Query arguments unsupported by " +
                              "the pgsql backend"))
        result = None
        def do_query(connection):
            return connection.query(query)
        try:
            result = do_query(connection.connection())
        except libpq.InterfaceError, e:
            raise DBUserException(None, e, query)
        except libpq.ProgrammingError, e:
            raise DBUserException(None, e, query)
        except libpq.OperationalError, e:
            if e.args:
                if e.args[0].find('could not obtain lock') != -1:
                    raise DBLockException()
                elif e.args[0].find('cannot perform INSERT RETURNING') != -1:
                    raise DBInsertException()
            if not restartable:
                raise DBSystemException(_(u"Database operational error"),
                                        e, e.args, query)
            cdata = connection.connection_data()
            connection = self._postgresql_new_connection(cdata)
            try:
                result = do_query(connection.connection())
            except Exception, e:
                raise DBSystemException(_(u"Database operational error"),
                                        e, e.args, query)
        except libpq.InternalError, e:
            raise DBException(None, e, query)
        except libpq.IntegrityError, e:
            raise DBUserException(_(u"Database integrity violation"),
                                  e, e.args, query)
        return self._postgresql_Result(result), connection

    def _postgresql_transform_query_result(self, result_):
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

    
class DBPyPgCounter(_PgsqlAccessor, DBPostgreSQLCounter):
    pass


class DBPyPgFunction(_PgsqlAccessor, DBPostgreSQLFunction):
    pass


class DBPyPgTransaction(_PgsqlAccessor, DBPostgreSQLTransaction):
    pass


class DBDataPyPgSQL(_PgsqlAccessor, DBDataPostgreSQL):

    class _PgNotifier(_PgsqlAccessor, PostgreSQLNotifier._PgNotifier):

        def __init__(self, connection_data, **kwargs):
            super(DBDataPyPgSQL._PgNotifier, self).__init__(
                connection_data=connection_data, **kwargs)
            self._pgnotif_connection = None

        def _notif_init_connection(self):
            if self._pgnotif_connection is None:
                connection_data = self._pg_connection_data()
                self._pgnotif_connection = self._postgresql_new_connection(connection_data)
            
        def _notif_do_registration(self, notification):
            connection = self._pgnotif_connection
            query = 'listen "%s"' % (notification,)
            # TODO: Allow reconnection with re-registrations
            def lfunction():
                self._postgresql_query(connection, query, False)
            with_lock(self._pg_query_lock, lfunction)
        
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
                def lfunction():
                    try:
                        connection.consumeInput()
                        notice = connection.notifies()
                    except Exception, e:
                        if __debug__:
                            log(DEBUG, 'Databázová chyba', e.args)
                        return None
                    notifications = []
                    if notice:
                        if __debug__:
                            log(DEBUG, 'Zaregistrována změna dat')
                    while notice:
                        n = notice.relname.lower()
                        notifications.append(n)
                        notice = connection.notifies()
                    return notifications
                notifications = with_locks((self._notif_connection_lock,
                                            self._pg_query_lock,),
                                           lfunction)
                if notifications is None:
                    break
                if __debug__:
                    log(DEBUG, 'Načteny notifikace:', notifications)
                self._notif_invoke_callbacks(notifications)


### Defaults


class DBDataDefaultClass(PostgreSQLUserGroups, RestrictedData, DBDataPyPgSQL):
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
        self._pg_add_notifications()


### Exportované proměnné/třídy


DBDataDefault = DBDataDefaultClass
"""Podtřída 'DBData', kterou používáme pro přístup k databázi."""

DBCounterDefault = DBPyPgCounter
"""Podtřída třídy 'Counter', která je standardně používána."""

DBFunctionDefault = DBPyPgFunction
"""Podtřída třídy 'Function', která je standardně používána."""

DBTransactionDefault = DBPyPgTransaction
"""Standard transaction class."""

def _postgresql_access_groups(connection_data):
    import pytis.data.pgsql
    class PgUserGroups(pytis.data.pgsql._PgsqlAccessor,
                       PostgreSQLUserGroups):
        pass
    return PgUserGroups(connection_data).access_groups()
default_access_groups = _postgresql_access_groups
"""Funkce vracející seznam skupin uživatele specifikovaného spojení."""
