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

"Implementace datov�ho rozhran� pro PostgreSQL prost�ednictv�m pyPgSQL."

import copy
import select
import string
import thread
import time
import weakref
import types as pytypes

from pyPgSQL import libpq

from pytis.util import *
from postgresql import *


### Spr�va spojen�


def _pypg_new_connection(spec, data):
    # Toto mus� b�t funkce, proto�e nesm� b�t v�z�na na konkr�tn� instanci.
    if not isinstance(spec, DBConnection):
        spec = spec()
    if __debug__: log(DEBUG, 'Vytv���m nov� DB spojen�', spec)
    # Sestav connection string
    connection_string = ''
    for option, accessor in (('user', DBConnection.user),
                             ('password', DBConnection.password),
                             ('dbname', DBConnection.database),
                             ('host', DBConnection.host),
                             ('port', DBConnection.port)):
        value = accessor(spec)
        if value != None:
            connection_string += " %s='%s'" % (option, pg_escape(str(value)))
    # Otev�i spojen�
    if __debug__: log(DEBUG, 'P�ipojovac� �et�zec:', connection_string)
    try:
        connection = libpq.PQconnectdb(connection_string)
        try:
            import config
            encoding = pg_encoding(config.db_encoding)
            connection.query('set client_encoding to %s' % encoding)
        except libpq.DatabaseError, e:
            raise DBException(_("Nelze nastavit client_encoding na %s"  
                                % config.db_encoding), e)
    except libpq.DatabaseError, e:
        if e.args:
            msg = e.args[0].lower()
            if msg.find('password') != -1 or \
                   msg.find('authentication failed') != -1:
                raise DBLoginException()
        raise DBException(_("Nelze se p�ipojit k datab�zi"), e)
    # Nastavujeme serializovan� transakce, abychom v�r�mci jedn� transakce
    # nemohli dostat r�zn� v�sledky pro opakovan� selecty.
    try:
        connection.query('set session characteristics as transaction '+\
                         'isolation level serializable')
    except libpq.DatabaseError, e:
        try:
            connection.finish()         # pro jistotu
        except:
            pass
        raise DBSystemException(None, e)
    if __debug__: log(DEBUG, 'Spojen� otev�eno')
    if data is not None:
        data.update_access_groups(spec)
    # Vra� spojen�
    return connection

_pypg_connection_pool_ = None
def _pypg_connection_pool():
    # Connection pool nelze inicializovat p��mo u�jeho definice kv�li cyklick�m
    # z�vislostem modul� (config -> dbdata -> log -> config).
    global _pypg_connection_pool_
    if _pypg_connection_pool_ is None:
        _pypg_connection_pool_ = DBConnectionPool(_pypg_new_connection)
    return _pypg_connection_pool_

def _pypg_query(connection, query, data=None, outside_transaction=True,
                conmaker=None):
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
        result = do_query(connection)
    except libpq.InterfaceError, e:
        raise DBUserException(None, e, query)
    except libpq.ProgrammingError, e:
        raise DBUserException(None, e, query)
    except libpq.OperationalError, e:
        if not outside_transaction:
            connection = None
            raise DBSystemException(_("Opera�n� chyba v�datab�zi"), e, query)
        if conmaker:
            connection = conmaker()
            try:
                result = do_query(connection)
            except Exception, e:
                connection = None
                raise DBSystemException(_("Opera�n� chyba v�datab�zi"), e,
                                        query)
        else:
            connection = None
            raise DBSystemException(_("Opera�n� chyba v�datab�zi"), e, query)
    except libpq.InternalError, e:
        connection = None
        raise DBException(None, e, query)
    except libpq.IntegrityError, e:
        raise DBUserException(_("Pokus o�poru�en� integrity dat"),
                              e, query)
    return result, connection


### Datov� tabulky


class DBDataPyPgSQL(DBDataPostgreSQL):
    """Implementace postgresov� tabulkov� t��dy prost�ednictv�m pyPgSQL."""

    NOTIFIERS = {}

    class _PgNotifier:

        # Jsou tu dva z�mky -- pozor na uv�znut�!

        def __init__(self, connection_pool, connection_spec):
            if __debug__: log(DEBUG, 'Vytvo�en�')
            self._data_lock = thread.allocate_lock()
            self._data_objects = weakref.WeakKeyDictionary()
            self._connection_lock = thread.allocate_lock()
            self._connection = None
            thread.start_new_thread(self._listen,
                                    (connection_pool, connection_spec))

        def _register(self, notification):
            # Zamyk�me zde kv�li mo�nosti sou�asn�ho vyvol�n� t�to metody
            # z�`register' i�naslouchac�ho threadu.
            if __debug__: log(DEBUG, 'Registruji notifikaci:', notification)
            connection = self._connection
            lock = self._connection_lock
            if connection:
                lock.acquire()
                try:
                    try:
                        connection.query('listen %s' % notification)
                    except Exception, e:
                        try:
                            connection.finish() # pro jistotu
                        except:
                            pass
                        raise DBSystemException(
                            _("Datab�zov� chyba listen"), e)
                finally:
                    lock.release()
            if __debug__: log(DEBUG, 'Notifikace zaregistrov�na:', notification)

        def _listen(self, pool, spec):
            if __debug__: log(DEBUG, 'Nov� listener')
            error_pause = 1
            while True:
                if __debug__: log(DEBUG, 'Napichuji se na nov� spojen�')
                connection = self._connection = pool.get(spec)
                notiflist = reduce(lambda x, y: x + y,
                                   self._data_objects.values(), [])
                if __debug__: log(DEBUG, 'Notifikace k�registraci:', notiflist)
                try:
                    # connection do poolu nikdy nevrac�me, tak�e na n�j m��eme
                    # nav�sit, co je n�m libo.
                    for n in remove_duplicates(notiflist):
                        self._register(n)
                except DBException, e:
                    time.sleep(error_pause)
                    error_pause = error_pause * 2
                    continue
                while True:
                    if __debug__: log(DEBUG, 'Hl�d�m vstup', connection)
                    try:
                        select.select([connection.socket], [], [], None)
                    except Exception, e:
                        if __debug__: log(DEBUG, 'Chyba na socketu', e.args)
                        break
                    if __debug__: log(DEBUG, 'P�i�el vstup')
                    lock = self._connection_lock
                    lock.acquire()
                    try:
                        try:
                            connection.consumeInput()
                            notice = connection.notifies()
                        except Exception, e:
                            if __debug__: log(DEBUG, 'Datab�zov� chyba', e.args)
                            break
                        notifications = []
                        if notice:
                            self._pg_changed = True
                            if __debug__: log(DEBUG, 'Zaregistrov�na zm�na dat')
                        while notice:
                            n = string.lower(notice.relname)
                            notifications.append(n)
                            notice = connection.notifies()
                    finally:
                        lock.release()
                    if __debug__:
                        log(DEBUG, 'Na�teny notifikace:', notifications)
                    self._invoke_callbacks(notifications)

        def _invoke_callbacks(self, notifications):
            if __debug__: log(DEBUG, 'Vol�m callbacky')
            lock = self._data_lock
            lock.acquire()
            try:
                data_objects = copy.copy(self._data_objects)
            finally:
                lock.release()
            for d, ns in data_objects.items():
                for n in ns:
                    if n in notifications:
                        if __debug__:
                            log(DEBUG, 'Vol�m callbacky datov�ho objektu:', d)
                        d._call_on_change_callbacks()
                        break

        def register(self, data, notification):
            if __debug__: log(DEBUG, 'Registruji notifikaci:', notification)
            lock = self._data_lock
            lock.acquire()
            try:
                try:
                    notifications = self._data_objects[data]
                except KeyError:
                    self._data_objects[data] = notifications = []
                notification = string.lower(notification)
                notifications.append(notification)
            finally:
                lock.release()
            self._register(notification)
            if __debug__: log(DEBUG, 'Notifikace zaregistrov�na')

    # Metody hlavn� t��dy

    def __init__(self, bindings, key, dbconnection_spec, ordering=None):
        self._pypg_connection = []
        super_(DBDataPyPgSQL).__init__(self, bindings, key,
                                       dbconnection_spec, ordering)
        
    def _pypg_add_notifications(self):
        notifications = self._pg_notifications
        if not notifications:
            return
        s = self._pg_dbconnection_spec()
        spec = (s.host(), s.port(), s.database())
        try:
            notifier = DBDataPyPgSQL.NOTIFIERS[spec]
        except KeyError:
            notifier = DBDataPyPgSQL.NOTIFIERS[spec] = \
              DBDataPyPgSQL._PgNotifier(_pypg_connection_pool(), s)
        for n in notifications:
            notifier.register(self, n)

    def _pypg_new_connection(self):
        return _pypg_new_connection(self._pg_dbconnection_spec(), self)

    def _pypg_allocate_connection(self):
        if __debug__:
            if len(self._pypg_connection) >= 3:
                if __debug__:
                    log(DEBUG, 'Podez�ele velk� hloubka spojen�:',
                        len(self._pypg_connection))
        connection = self._pypg_get_connection(self, outside_transaction=True)
        self._pypg_connection.append(connection)
        
    def _pypg_deallocate_connection(self):
        self._pypg_return_connection(self._pypg_connection.pop())
        
    def _pypg_get_connection(self, data, outside_transaction=False):
        if outside_transaction or not self._pypg_connection:
            pool = _pypg_connection_pool()
            return pool.get(self._pg_dbconnection_spec(), data)
        else:
            return self._pypg_connection[-1]
        
    def _pypg_return_connection(self, connection):
        pool = _pypg_connection_pool()
        pool.put_back(self._pg_dbconnection_spec(), connection)
        
    def _pg_begin_transaction (self):
        self._pypg_allocate_connection()
        DBDataPyPgSQL.__bases__[0]._pg_begin_transaction(self)
        
    def _pg_commit_transaction (self):
        DBDataPyPgSQL.__bases__[0]._pg_commit_transaction(self)
        self._pypg_deallocate_connection()
        
    def _pg_rollback_transaction (self):
        DBDataPyPgSQL.__bases__[0]._pg_rollback_transaction(self)
        self._pypg_deallocate_connection()

    def _pg_query(self, query, outside_transaction=False, backup=False,
                  group_update=True):
        if type(query) is pytypes.UnicodeType:
            query = query.encode(self._db_encoding)
        if group_update:
            data_arg = self
        else:
            data_arg = None
        # Z�skej DB spojen�
        connection = self._pypg_get_connection(data_arg, outside_transaction)
        # Prove� query
        if __debug__: log(DEBUG, 'SQL dotaz', query)
        try:
            result, connection = \
                    _pypg_query(connection, query, data=data_arg,
                                outside_transaction=outside_transaction,
                                conmaker=self._pypg_new_connection)
        finally:
            # Vra� DB spojen� zp�t
            if connection and outside_transaction:
                self._pypg_return_connection(connection)
        if backup and self._pdbb_logging_command:
            assert not outside_transaction, \
                   ('Backed up SQL command outside transaction', query)
            # Zde nem��e doj�t k�v�znamn� z�m�n� po�ad� zalogovan�ch
            # p��kaz�, proto�e v�echny DML p��kazy jsou uzav�eny
            # v�transakc�ch a ty konfliktn� jsou d�ky serializaci
            # automaticky spr�vn� �azeny.
            _pypg_query(connection,
                        self._pdbb_logging_command % pg_escape(query),
                        data=data_arg, outside_transaction=False)
        # Z�skej a vra� data
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
        if __debug__: log(DEBUG, 'V�sledek SQL dotazu', data)
        return data


### ��ta�e


class DBPyPgCounter(Counter):
    """��ta� ulo�en� v�PostgreSQL, zp��stupn�n� p�es pyPgSQL."""
    
    def __init__(self, name, dbconnection_spec):
        """Inicializuj ��ta�.

        Argumenty:

          name -- identifik�tor ��ta�e v�datab�zi, string

        """
        self._name = name
        self._dbconnection_spec = dbconnection_spec
        self._query = "select nextval('%s')" % name
        
    def next(self):
        pool = _pypg_connection_pool()
        connection = pool.get(self._dbconnection_spec)
        result, connection = _pypg_query(connection, self._query)
        if connection:
            pool.put_back(self._dbconnection_spec, connection)
        return int(result.getvalue(0, 0))


### Datab�zov� funkce a procedury


class DBPyPgFunction(Function, PostgreSQLUserGroups,
                     PostgreSQLStandardBindingHandler, DBDataPyPgSQL):
    # TODO: Opravdu se mus� d�dit to v�echno?
    """Implementace t��dy 'Function' pro PostgreSQL.

    Podporov�ny jsou pouze funkce vracej�c� jedinou hodnotu.

    """
    def __init__(self, name, dbconnection_spec):
        """Inicializuj instanci.

        Argumenty:

          name -- jm�no funkce jako nepr�zdn� string
          dbconnection_spec -- instance t��dy 'DBConnection' definuj�c�
            parametry p�ipojen�, nebo funkce bez argument� vracej�c� takovou
            instanci 'DBConnection'

        """
        assert is_string(name)
        self._name = name
        bindings = ()
        DBDataPyPgSQL.__init__(self, bindings, bindings, dbconnection_spec)
        PostgreSQLStandardBindingHandler.__init__(self)
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
