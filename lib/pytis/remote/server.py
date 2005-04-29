# -*- coding: iso-8859-2 -*-

# Privátní objekty datového serveru
# 
# Copyright (C) 2002, 2004, 2005 Brailcom, o.p.s.
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Objekty datového serveru.

Tyto objekty slouží především serveru zpřístupňujícímu datové a jiné objekty
klientům.

"""

import Bastion
import thread
import time

import Pyro.core, Pyro.errors, Pyro.naming

from pytis.util import *
from pytis.remote import *
import pytis.data
import config


NAME_RESOLVER = 'resolver'
"""Identifikace (jméno) resolveru vystaveného serverem."""
NAME_TYPE_TABLE = 'typetable'
"""Identifikace (jméno) tabulky vzdálených typů vystavené serverem."""

_pyro_daemon = None


def _safe_function(object):
    """Vrať pravdu právě když 'object' je funkce z modulu 'defs'."""
    return callable(object) and object.__module__ == 'defs'


def _bastionize(object, allowed_methods=None):
    # TODO: Aren't the Pyro attributes dangerous?
    # A: Maybe, their changes could confuse other clients?
    if allowed_methods is None:
        allowed_methods = object.ALLOWED_METHODS
    access_list = allowed_methods + ('GUID', 'setDaemon', 'Pyro_dyncall')
    return Bastion.Bastion(object, lambda x: x in access_list)


_orig_data_factory = pytis.data.DataFactory
def wrap_data_factory(*args, **kwargs):
    factory = _orig_data_factory(*args, **kwargs)
    object = _bastionize(_PyroDataFactory(factory))
    resolver_uri = _pyro_daemon.connect(object)
    result = RemoteDataFactory(resolver_uri)
    return result


class _PyroDataFactory(Pyro.core.ObjBase, _orig_data_factory):
    """Serverová data factory s vystavením datových objektů přes Pyro."""

    ALLOWED_METHODS = ('create',)
    
    def __init__(self, factory):
        apply(_orig_data_factory.__init__,
              (self, factory._class_) + factory._args, factory._kwargs)
        Pyro.core.ObjBase.__init__(self)
        self._factory = factory
        
    def create(self, dbconnection_spec=None):
        Pyro.util.Log.msg(self.__class__.__name__, 'create',
                          str(self._class_), deepstr(self._args),
                          str(self._kwargs))
        # Z bezpečnostních důvodů přijímáme z klíčovaných argumentů pouze
        # `dbconnection_spec'.
        if dbconnection_spec is not None and \
           not isinstance(dbconnection_spec, pytis.data.DBConnection) and \
           not _safe_function(dbconnection):
            raise InvalidAccessError('Unsafe dbconnection specification',
                                     dbconnection_spec)
        if dbconnection_spec is not None:
            dbconnection_spec = \
              config.dbconnection.modified(
                user=dbconnection_spec.user(),
                password=dbconnection_spec.password())
        kwargs = {'dbconnection_spec': dbconnection_spec}
        import random
        random.seed()
        password = random.randrange(2**31-1)
        data = _PyroData(self._factory.create(**kwargs), password)
        pyro_data = Pyro.core.ObjBase()
        pyro_data.delegateTo(data)
        pyro_data = Bastion.Bastion(pyro_data)
        uri = _pyro_daemon.connect(pyro_data)
        _PyroData.register_remote_object(pyro_data, data)
        result = RemoteData(uri, password)
        return result


class _PyroData:
    """Serverový datový objekt (wrapper) s hesly.

    Veškeré veřejné metody přijímají jako argument heslo, které je chrání před
    vyvolání neautorizovaným klientem.  Kromě toho je volání některých metod
    obaleného objektu chráněno základním threadovým zámkem; ani takto obalený
    datový objekt však není thread-safe, kvůli vzájemné závislosti metod
    (například metody závislé na předchozím volání metody 'select()').

    """

    _TIMEOUT = 150*60                   # sekund
    _collection_lock = thread.allocate_lock()
    _active_data_objects = []
    
    def __init__(self, data, password):
        self._data = data
        self._password = password
        self._lock = thread.allocate_lock()
        self._timestamp = time.time()

    def _collect(class_):
        lock = class_._collection_lock
        lock.acquire()
        try:
            limit = time.time() - class_._TIMEOUT
            active = []
            for object, data in class_._active_data_objects:
                if data.timestamp() > limit:
                    active.append((object, data))
                else:
                    _pyro_daemon.disconnect(object)
            class_._active_data_objects = active
        finally:
            lock.release()
    _collect = classmethod(_collect)

    def register_remote_object(class_, object, data):
        # Zde musí být Pyro objekt i jeho datový objekt, protože Pyro objekt
        # zpřístupňuje svůj objekt jenom vzdáleně (a nechceme spoléhat na
        # nedokumentované atributy Pyro objektu).
        class_._collect()
        lock = class_._collection_lock
        lock.acquire()
        try:
            class_._active_data_objects.append((object, data))
        finally:
            lock.release()
    register_remote_object = classmethod(register_remote_object)

    def _check_type_integer(self, arg):
        if arg is not None and type(arg) != type(0):
            raise InvalidAccessError('Invalid argument type',
                                     'integer', arg)
        return arg
        
    def _check_type_string(self, arg):
        if arg is not None and type(arg) != type(''):
            raise InvalidAccessError('Invalid argument type', 'string', arg)
        return arg

    def _check_type_value(self, arg):
        if arg is not None:
            if not isinstance(arg, pytis.data.Value):
                # Vnitřek Value je kontrolován ve Value.__setstate__.
                raise InvalidAccessError('Invalid argument type',
                                         'value', arg)
        return arg
        
    def _check_type_key(self, arg):
        if arg is None:
            return None
        arg = xtuple(arg)
        for a in arg:
            self._check_type_value(a)
        return arg
    
    def _check_type_row(self, arg):
        if arg is not None and not isinstance(arg, pytis.data.Row):
            raise InvalidAccessError('Invalid argument type', 'row', arg)
        return arg

    def _check_type_boolean(self, arg):
        return self._check_type_integer(arg)

    def _check_type_condition(self, arg):
        if arg is not None:
            if not isinstance(arg, pytis.data.Operator):
                raise InvalidAccessError('Invalid argument type',
                                         'condition', arg)
            for a in list(arg.args()) + arg.kwargs().values():
                if a is None or isinstance(a, pytis.data.Value):
                    pass
                elif isinstance(a, pytis.data.Operator):
                    self._check_type_condition(a)
                else:
                    t = type(a)
                    if t != types.StringType and t != types.IntType:
                        raise InvalidAccessError(
                            'Invalid condition element', a)
        return arg

    def _check_type_direction(self, arg):
        if arg not in (pytis.data.FORWARD, pytis.data.BACKWARD):
            raise InvalidAccessError('Invalid argument type',
                                     'direction', arg)
        return arg

    def _check_type_sort(self, arg):
        def invalid_elem(elem):
            if type(elem) == type(''):
                return False
            id, dir = elem
            return type(id) != type('') or \
                   dir not in(pytis.data.ASCENDENT, pytis.data.DESCENDANT)
        if not is_sequence(arg) or some(invalid_elem, arg):
            raise InvalidAccessError('Invalid argument type', 'sort', arg)
        return arg
    
    def _call_method(self, password, lockp, name, **kwargs):
        Pyro.util.Log.msg(self.__class__.__name__, name)
        self._check_type_integer(password)
        self._timestamp = time.time()
        method = getattr(self._data, name)
        if lockp:
            self._lock.acquire()
        try:
            return method(**kwargs)
        finally:
            if lockp:
                self._lock.release()
        
    def columns(self, password):
        return self._call_method(password, False, 'columns')

    def find_column(self, password, id):
        return self._call_method(password, False, 'find_column',
                                 id=self._check_type_string(id))
    
    def key(self, password):
        return self._call_method(password, False, 'key')
    
    def row_key(self, password, row):
        return self._call_method(password, False, 'row_key',
                                 row=self._check_type_row(row))
    
    def row(self, password, key):
        return self._call_method(password, True, 'row',
                                 key=self._check_type_key(key))
        
    def select(self, password, condition=None, reuse=False, sort=()):
        return self._call_method(
            password, True, 'select',
            condition=self._check_type_condition(condition),
            reuse=self._check_type_boolean(reuse),
            sort=self._check_type_sort(sort))
        
    def fetchone(self, password, direction=pytis.data.FORWARD):
        return self._call_method(
            password, True, 'fetchone',
            direction=self._check_type_direction(direction))
    
    def skip(self, password, count, direction=pytis.data.FORWARD):
        return self._call_method(
            password, True, 'skip',
            count=self._check_type_integer(count),
            direction=self._check_type_direction(direction))
    
    def rewind(self, password):
        return self._call_method(password, True, 'rewind')
    
    def search(self, password, condition, direction=pytis.data.FORWARD):
        return self._call_method(
            password, True, 'search',
            condition=self._check_type_condition(condition),
            direction=self._check_type_direction(direction))
    
    def search_key(self, password, key, direction=pytis.data.FORWARD):
        return self._call_method(
            password, True, 'search_key',
            key=self._check_type_key(key),
            direction=self._check_type_direction(direction))
    
    def close(self, password):
        return self._call_method(password, True, 'close')
    
    def insert(self, password, row, after=None, before=None):
        return self._call_method(password, True, 'insert',
                                 row=self._check_type_row(row),
                                 after=self._check_type_key(after),
                                 before=self._check_type_key(before))
    
    def update(self, password, key, row):
        return self._call_method(password, True, 'update',
                                 key=self._check_type_key(key),
                                 row=self._check_type_row(row))
    
    def update_many(self, password, condition, row):
        return self._call_method(
            password, True, 'update_many',
            condition=self._check_type_condition(condition),
            row=self._check_type_row(row))
    
    def delete(self, password, key):
        return self._call_method(password, True, 'delete',
                                 key=self._check_type_key(key))
    
    def delete_many(self, password, condition):
        return self._call_method(
            password, True, 'delete_many',
            condition=self._check_type_condition(condition))
    
    def lock_row(self, password, key):
        return self._call_method(password, True, 'lock_row',
                                 key=self._check_type_key(key))
    
    def unlock_row(self, password):
        return self._call_method(password, True, 'unlock_row')
    
    def locked_row(self, password):
        return self._call_method(password, True, 'locked_row')
    
    def change_number(self, password):
        return self._call_method(password, False, 'change_number')

    def timestamp(self):
        """Vrať čas posledního přístupu k hlavním metodám třídy."""
        return self._timestamp
    

class _PyroResolver(Pyro.core.ObjBase, FileResolver):
    """Resolver určený pro vzdálený přístup přes Pyro.

    Resolver je navíc obohacen o speciální zpracování datových specifikací.

    """
    ALLOWED_METHODS = ('get',)
    
    def __init__(self):
        import config
        Pyro.core.ObjBase.__init__(self)
        FileResolver.__init__(self, config.def_dir)
        self._data_specs = {}

    def get(self, file_name, spec_name, assert_class=None):
        Pyro.util.Log.msg(self.__class__.__name__,
                          'get', file_name, spec_name)
        if type(file_name) != type('') or type(spec_name) != type(''):
            raise InvalidAccessError('Invalid argument type', 'resolver',
                                     file_name, spec_name)
        super_ = super(_PyroResolver, self).get
        if spec_name == 'data_spec':
            try:
                result = self._data_specs[file_name]
            except KeyError:
                result = self._data_specs[file_name] = \
                         super_(file_name, spec_name)
            assert isinstance(result, RemoteDataFactory)
        else:
            result = super_(file_name, spec_name)
        return result


def run_server():
    # Start the servers
    global _pyro_daemon
    Pyro.core.initServer()
    host = config.server
    if not host:
        log(OPERATIONAL, "Nespecifikované jméno stroje serveru")
        return
    _pyro_daemon = Pyro.core.Daemon(host=host)
    try:
        # Create and connect the objects
        resolver = _bastionize(_PyroResolver())
        _pyro_daemon.connect(resolver, NAME_RESOLVER)
        type_table = pytis.data.Type.type_table()
        pyro_type_table = Pyro.core.ObjBase()
        pyro_type_table.delegateTo(type_table)
        pyro_type_table = \
          _bastionize(pyro_type_table, allowed_methods=('get',))
        _pyro_daemon.connect(pyro_type_table, NAME_TYPE_TABLE)
        # Run the daemon
        try:
            _pyro_daemon.requestLoop()
        except KeyboardInterrupt:
            pass
    finally:
        # Close the daemon
        try:
            _pyro_daemon.shutdown()
        except:
            pass
