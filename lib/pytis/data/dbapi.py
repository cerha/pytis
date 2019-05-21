# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2015 Brailcom, o.p.s.
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

import datetime
import inspect
import select
import time

import psycopg2 as dbapi
import psycopg2.extensions
import psycopg2.extras

import pytis
from pytis.util import log, translations, Locked, DEBUG, OPERATIONAL
from pytis.data import AccessRights, Permission, Range, RestrictedData
from .dbdata import (DBConnection, DBException, DBInsertException, DBLockException,
                    DBLoginException, DBRetryException, DBSystemException, DBUserException)
from .postgresql import (DBDataPostgreSQL, DBPostgreSQLCounter, DBPostgreSQLFunction,
                        DBPostgreSQLTransaction, PostgreSQLAccessor, PostgreSQLResult,
                        PostgreSQLNotifier, PostgreSQLUserGroups, PostgreSQLConnector)


_ = translations('pytis-data')


class _DBAPIAccessor(PostgreSQLAccessor):

    _query_callback = (None,)
    _last_server_errors = {}

    @classmethod
    def set_query_callback(class_, callback):
        # We can't simply assign callback, because Python would made an unbound
        # class method from it.  By wrapping the callback by a tuple we prevent
        # that misbehavior.
        class_._query_callback = (callback,)

    @classmethod
    def _postgresql_open_connection(class_, connection_data):
        # Prepare connection data
        kwargs = dict(connect_timeout=10)
        for option, accessor in (('user', DBConnection.user),
                                 ('password', DBConnection.password),
                                 ('database', DBConnection.database),
                                 ('host', DBConnection.host),
                                 ('port', DBConnection.port),
                                 ('sslmode', DBConnection.sslmode),
                                 ):
            # Note: 'port' argument is not defined in DB API.
            value = accessor(connection_data)
            if value is not None:
                kwargs[option] = value
        connection_key = tuple(kwargs.items())
        if class_._last_server_errors.get(connection_key, 0) + 60 > time.time():
            raise DBSystemException(_(u"Waiting for server recovery"))
        # Open the connection
        try:
            connection = dbapi.connect(**kwargs)
        except dbapi.DatabaseError as e:
            # Does this error detection work for dbapi as well?
            if e.args:
                msg = e.args[0].lower()
                if ((msg.find('password') != -1 or
                     msg.find('authentication failed') != -1)):
                    raise DBLoginException()
            class_._last_server_errors[connection_key] = time.time()
            raise DBSystemException(_(u"Can't connect to database"), e)
        connection.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_READ_COMMITTED)
        connection_instance = class_._postgresql_Connection(connection, connection_data)
        class_._postgresql_reset_connection_info(connection_instance)
        connection_instance.set_connection_info('last_activity', time.time())
        cursor = connection.cursor()
        cursor.execute("show standard_conforming_strings")
        result = cursor.fetchone()[0]
        connection_instance.set_connection_info('standard_strings', (result == 'on'))
        cursor.close()
        return connection_instance

    @classmethod
    def _postgresql_close_connection(class_, connection):
        connection.connection().close()

    @classmethod
    def _postgresql_reset_connection_info(class_, connection, commands=None):
        connection.set_connection_info('transaction_commands', (commands or []))
        connection.set_connection_info('search_path', None)
        connection.set_connection_info('transaction_start_time', None)
        connection.set_connection_info('last_query_time', None)
        if __debug__:
            connection.set_connection_info('transaction_start_stack', None)

    def _postgresql_query(self, connection, query, outside_transaction, _retry=True):
        result = None

        def transform_arg(arg):
            if isinstance(arg, Range.Range):
                lower = arg.lower()
                upper = arg.upper()
                test_value = upper if lower is None else lower
                if isinstance(test_value, (int, long, float)):
                    c = psycopg2.extras.NumericRange
                elif isinstance(test_value, datetime.datetime):
                    if test_value.tzinfo is None:
                        c = psycopg2.extras.DateTimeRange
                    else:
                        c = psycopg2.extras.DateTimeTZRange
                elif isinstance(test_value, datetime.date):
                    c = psycopg2.extras.DateRange
                else:
                    raise Exception("Unsupported range type", arg)
                bounds = ('[' if arg.lower_inc() else '(') + (']' if arg.upper_inc() else ')')
                arg = c(lower, upper, bounds=bounds)
            return arg
        if isinstance(query, basestring):
            query_args = {}
        else:
            query, args = query.query()
            query_args = dict([(k, transform_arg(v)) for k, v in args.items()])

        def do_query(connection, query):
            raw_connection = connection.connection()
            standard_strings = connection.connection_info('standard_strings')
            if not standard_strings:
                query = query.replace('\\', '\\\\')
            cursor = raw_connection.cursor()
            callback = self._query_callback[0]
            # The hasattr test is a hack enforced by the fact that constructor
            # calls of pytis.data classes are in very strange state now.
            if hasattr(self, '_sql_logger') and self._sql_logger is not None:
                if query_args:
                    def escape(arg):
                        if isinstance(arg, basestring):
                            result = "'%s'" % (arg.replace('\x00', '\\0').replace("'", "''"),)
                            if not standard_strings:
                                result = result.replace('\\', '\\\\')
                        elif isinstance(arg, buffer):
                            result = '<binary_data>'
                        else:
                            result = arg
                        return result
                    escaped_args = dict([(k, escape(v)) for k, v in query_args.items()])
                    query_string = query % escaped_args
                else:
                    query_string = query
                self._sql_logger.write(query_string + '\n')
            if callback is not None:
                start_time = time.time()
            # query_args shouldn't be used when empty to prevent mistaken
            # '%' processing in `query'
            try:
                if query_args:
                    cursor.execute(query, query_args)
                else:
                    cursor.execute(query)
                connection.connection_info('transaction_commands').append(query)
            finally:
                if outside_transaction:
                    raw_connection.commit()
                    self._postgresql_reset_connection_info(connection, ['commit'])
            if callback is not None:
                callback(query, start_time, time.time())
            return cursor

        def retry(message, exception):
            connection.set_connection_info('broken', True)
            if _retry:
                if not outside_transaction:
                    raise DBRetryException(message, exception, exception.args, query)
                cdata = connection.connection_data()
                search_path = connection.connection_info('search_path')
                new_connection = self._postgresql_new_connection(cdata)
                try:
                    if search_path:
                        do_query(new_connection, 'set search_path to ' + search_path)
                    result = do_query(new_connection, query)
                except Exception as e:
                    raise DBSystemException(message, e, e.args, query)
            else:
                raise DBSystemException(message, exception, exception.args, query)
            return result, new_connection
        try:
            result = do_query(connection, query)
            if query == 'commit':
                self._postgresql_commit_transaction(connection)
            elif query == 'rollback':
                self._postgresql_rollback_transaction(connection)
        except dbapi.InterfaceError as e:
            if e.args and e.args[0].find('connection already closed') != -1:
                # We believe this shouldn't happen as a program error and it
                # may occur as a result of database engine connection crash.
                log(OPERATIONAL, "Access to closed database connection")
                result, connection = retry(_(u"Database interface error"), e)
            else:
                raise DBUserException(None, e, e.args, query)
        except dbapi.NotSupportedError as e:
            if e.args and e.args[0].find('cannot perform INSERT RETURNING') != -1:
                # This is handled once again below since older dbapi versions report it as
                # ProgrammingError and newer versions as NotSupportedError.
                raise DBInsertException()
            raise DBUserException(None, e, e.args, query)
        except dbapi.ProgrammingError as e:
            if e.args:
                position = e.args[0].find
                if position('could not obtain lock') != -1:
                    raise DBLockException()
                elif position('cannot perform INSERT RETURNING') != -1:
                    raise DBInsertException()
                elif (position('error: Connection timed out') != -1 or
                      position('timeout expired') != -1):

                    raise DBSystemException(_(u"Database connection timeout"), e, e.args, query)
                elif position('server closed the connection unexpectedly') != -1:
                    result, connection = retry(_(u"Database connection error"), e)
                else:
                    log(OPERATIONAL, "Transaction commands:",
                        connection.connection_info('transaction_commands'))
                    data = '%s [search_path=%s]' % (query,
                                                    connection.connection_info('search_path'),)
                    raise DBUserException(None, e, e.args, data)
            else:
                raise DBUserException(None, e, e.args, query)
        except dbapi.DataError as e:
            raise DBUserException(None, e, e.args, query)
        except dbapi.OperationalError as e:
            if e.args and e.args[0].find('could not obtain lock') != -1:
                raise DBLockException()
            result, connection = retry(_(u"Database operational error"), e)
        except dbapi.InternalError as e:
            if e.args and e.args[0].find('terminating connection due to '
                                         'idle-in-transaction timeout') != -1:
                # We believe this shouldn't happen as a program error and it
                # may occur as a result of database engine connection crash.
                log(OPERATIONAL, "Access to closed database connection")
                result, connection = retry(_(u"Database internal error"), e)
            else:
                raise DBException(None, e, query)
        except dbapi.IntegrityError as e:
            raise DBUserException(_(u"Database integrity violation"),
                                  e, e.args, query)
        now = time.time()
        connection.set_connection_info('last_query_time', now)
        connection.set_connection_info('last_activity', now)
        if connection.connection_info('transaction_start_time') is None:
            connection.set_connection_info('transaction_start_time', now)
            if __debug__:
                if query not in ('commit', 'rollback',):
                    connection.set_connection_info('transaction_start_stack',
                                                   inspect.currentframe())
        if __debug__:
            if query.startswith('fetch') or query.startswith('skip'):
                extra_info = (query,)
            else:
                self._last_informative_query = query
                extra_info = ()
            info = (time.ctime(now), self._last_informative_query,) + extra_info
            connection.set_connection_info('last_access', info)
        return self._postgresql_Result(result), connection

    def _postgresql_transform_query_result(self, result):
        cursor = result.result()
        data = []
        if cursor.description:
            for i in range(cursor.rowcount):
                row = cursor.fetchone()
                row_data = []
                for col in row:
                    if isinstance(col, psycopg2.extras.Range):
                        col = Range.Range(col.lower, col.upper,
                                          lower_inc=col.lower_inc, upper_inc=col.upper_inc)
                    elif isinstance(col, str):
                        col = unicode(col, 'utf-8')
                    row_data.append(col)
                data.append(row_data)
        else:
            data.append([cursor.rowcount])
        return PostgreSQLResult(data)

    def _postgresql_begin_transaction(self):
        # In psycopg2 `begin' is called automatically.
        # By disabling its explicit call we avoid PostgreSQL warnings about
        # transactions in progress.
        # Not only that, we even close previous transaction here.  This so that
        # contingent connection initialization commands such as coding settings
        # don't prevent to set (non-default) transaction isolation level at the
        # beginning of transaction.
        try:
            self._postgresql_commit_transaction()
        except dbapi.OperationalError as e:
            self._maybe_connection_error(e)

    def _postgresql_commit_transaction(self, connection=None):
        if connection is None:
            connection_, new = self._pg_get_connection()
        else:
            connection_, new = connection, False
        raw_connection = connection_.connection()
        try:
            raw_connection.commit()
            self._postgresql_reset_connection_info(connection_, ['commit'])
        except dbapi.OperationalError as e:
            self._maybe_connection_error(e)
        except dbapi.InterfaceError as e:
            self._maybe_connection_error(e)
        except dbapi.InternalError as e:
            self._maybe_connection_error(e)
        if new:
            self._pg_return_connection(connection_)

    def _postgresql_rollback_transaction(self, connection=None):
        if connection is None:
            connection_, new = self._pg_get_connection()
        else:
            connection_, new = connection, False
        raw_connection = connection_.connection()
        try:
            raw_connection.rollback()
        except dbapi.OperationalError as e:
            self._maybe_connection_error(e)
        except dbapi.InterfaceError as e:
            self._maybe_connection_error(e)
        except dbapi.InternalError as e:
            self._maybe_connection_error(e)
        # For unknown reasons, connection client encoding gets reset after
        # rollback
        self._postgresql_reset_connection_info(connection_, ['rollback'])
        query = 'set client_encoding to "utf-8"'
        try:
            cursor = raw_connection.cursor()
            cursor.execute(query)
            # We commit the transaction immediately to prevent an open
            # (maybe) idle transaction.
            raw_connection.commit()
        except dbapi.OperationalError as e:
            self._maybe_connection_error(e)
        except dbapi.InterfaceError as e:
            self._maybe_connection_error(e)
        except dbapi.InternalError as e:
            self._maybe_connection_error(e)
        if new:
            self._pg_return_connection(connection_)

    def _maybe_connection_error(self, e):
        position = e.args[0].find
        if position('server closed the connection unexpectedly') != -1 or \
           position('terminating connection due to idle-in-transaction timeout') != -1 or \
           position('connection allready closed') != -1:
            raise DBSystemException(_(u"Database connection error"), e, e.args)


class DBAPICounter(_DBAPIAccessor, DBPostgreSQLCounter):
    pass


class DBAPIFunction(_DBAPIAccessor, DBPostgreSQLFunction):

    def __init__(self, name, connection_data, sql_logger=None, **kwargs):
        self._sql_logger = sql_logger
        super(DBAPIFunction, self).__init__(name=name, connection_data=connection_data, **kwargs)


class DBAPITransaction(_DBAPIAccessor, DBPostgreSQLTransaction):
    pass


class DBAPIData(_DBAPIAccessor, DBDataPostgreSQL):

    class _PgNotifier(_DBAPIAccessor, PostgreSQLNotifier._PgNotifier):

        def __init__(self, connection_data, connection_name=None):
            self._sql_logger = None  # difficult to call superclass constructors properly
            PostgreSQLNotifier._PgNotifier.__init__(self, connection_data,
                                                    connection_name=connection_name)

            self._pgnotif_connection = None

        def _notif_init_connection(self):
            if self._pgnotif_connection is None:
                connection_data = self._pg_connection_data()
                connection = self._postgresql_new_connection(connection_data)
                self._pgnotif_connection = connection
                connection.connection().set_isolation_level(
                    psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT)
                self._registered_notifications = []

        def _notif_do_registration(self, notification):
            if notification not in self._registered_notifications:
                self._registered_notifications.append(notification)
            connection = self._pgnotif_connection
            query = 'listen "%s"' % (notification,)
            # TODO: Allow reconnection with re-registrations
            with Locked(self._pg_query_lock):
                _result, self._pgnotif_connection = self._postgresql_query(connection, query, True)

        def _notif_listen_loop(self):
            while True:
                while self._pgnotif_connection is None:
                    time.sleep(10)
                    try:
                        self._notif_init_connection()
                        for notification in self._registered_notifications:
                            self._notif_do_registration(notification)
                    except Exception as e:
                        self._pgnotif_connection = None
                connection_ = self._pgnotif_connection
                connection = connection_.connection()
                if __debug__:
                    log(DEBUG, 'Listening for notifications:', connection)
                with Locked(self._pg_query_lock):
                    cursor = connection.cursor()
                    try:
                        fileno = connection.fileno()
                    except AttributeError:  # older psycopg2 versions
                        fileno = cursor.fileno()
                try:
                    select.select([fileno], [], [], None)
                except Exception as e:
                    if __debug__:
                        log(DEBUG, 'Socket error', e.args)
                    break
                if __debug__:
                    log(DEBUG, 'Input received')
                with Locked(self._notif_connection_lock), Locked(self._pg_query_lock):
                    notifications = []
                    try:
                        connection.poll()
                        ready = True
                    except AttributeError:  # older psycopg2 versions
                        try:
                            ready = cursor.isready()
                        except dbapi.OperationalError:
                            self._pg_notif_connection = None
                            ready = False
                    if ready:
                        notifies = connection.notifies
                        if notifies:
                            if __debug__:
                                log(DEBUG, 'Data change registered')
                            while notifies:
                                notifications.append(notifies.pop()[1])
                if __debug__:
                    log(DEBUG, 'Notifications received:', notifications)
                self._notif_invoke_callbacks(notifications)


# Defaults


class DBDataDefaultClass(PostgreSQLUserGroups, RestrictedData, DBAPIData):
    """Datová třída, kterou v našich aplikacích standardně používáme.

    Je utvořena pouhým složením existujících tříd a nezavádí žádnou další novou
    funkcionalitu kromě konstruktoru.

    """

    def __init__(self, bindings, key, connection_data=None, ordering=None,
                 access_rights=AccessRights((None, (None, Permission.ALL))),
                 dbconnection_spec=None, sql_logger=None, **kwargs):
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
        self._sql_logger = sql_logger
        # TODO: Následující hack je tu proto, že ve voláních konstruktorů výše
        # je _pg_add_notifications voláno předčasně, přičemž pořadí volání
        # konstruktorů nelze změnit.  Pro nápravu je potřeba ještě předělat
        # třídy týkající se notifikací.
        if pytis.config.dblisten:
            self._pg_add_notifications()


# Exportované proměnné/třídy


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


def _reload_session_variables(connection_data):
    PostgreSQLConnector(connection_data)._pg_flush_connections()


reload_session_variables = _reload_session_variables
"""Reload all session variables

This can be done only by closing all existing connections
in the connection pool. So use with caution.
"""
