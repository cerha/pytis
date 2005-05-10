# -*- coding: iso-8859-2 -*-

# Klientsk� datov� objekty
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

"""Datov� objekty zp��stup�uj�c� vzd�len� datov� objekty.

Soubor definuje obaly datov�ch objekt�, kter� nep�istupuj� k�datab�zi p��mo,
n�br� k�n� p�istupuj� prost�ednictv�m vzd�len�ch vol�n� pythonov�ho serveru.

"""

import copy
import thread
import time
import weakref

import Pyro.core

from pytis.remote import *
from pytis.util import *


class _DataChangesWatcher:
    # Watcher slou��, krom� sledov�n� zm�n objekt�, i�k�oping�v�n� objekt� na
    # serveru -- server objekty, ke kter�m nikdo nep�istupuje, ma�e.

    _PAUSE_SECONDS = 5
    
    def __init__(self):
        self._callbacks = weakref.WeakKeyDictionary()
        self._stamps = weakref.WeakKeyDictionary()
        self._clones = weakref.WeakKeyDictionary()
        self._lock = thread.allocate_lock()
        thread.start_new_thread(self._watcher, ())
        
    def _watcher(self):
        if __debug__: log(DEBUG, 'Spu�t�n callback watcher pro remote data')
        while True:
            time.sleep(self._PAUSE_SECONDS)
            self._lock.acquire()
            try:
                for data, stamp in self._stamps.items():
                    data_clone = self._clones[data]
                    try:
                        new_stamp = data_clone.change_number()
                    except Pyro.errors.PyroError:
                        # Jinak n�m p�i shozen� serveru za�ne chrlit tracebacky
                        continue
                    assert new_stamp >= stamp
                    if new_stamp > stamp:
                        self._stamps[data] = new_stamp
                        try:
                            callbacks = self._callbacks[data]
                            if __debug__:
                                log(DEBUG, 'Vol�m datov� callbacks',
                                    (data, callbacks))
                            for c in callbacks:
                                c()
                        except Exception, e:
                            if __debug__:
                                log(DEBUG, 'Chyba p�i vol�n� datov�ch callbacks',
                                    e)
            finally:
                self._lock.release()

    def register(self, data):
        self._stamps[data] = data.change_number()
        data_clone = copy.copy(data)
        data_clone.clone_proxy()
        self._clones[data] = data_clone
        
    def register_callback(self, data, callback):
        if __debug__: log(DEBUG, 'Registrace callbacku', (data, callback))
        self._lock.acquire()
        try:
            if self._stamps.has_key(data):
                # Zde bychom spr�vn� m�li updatovat stamp a p��padn� spustit
                # callbacky.  Ale zaji�t�n� toho, �e nov� registrovan� callback
                # nebude napoprv� vol�n zbyte�n�, nen� zase tak nutn�, abychom
                # kv�li tomu zde zav�d�ly komplikace.
                pass
            else:
                self.register(data)
            try:
                callbacks = self._callbacks[data]
            except KeyError:
                callbacks = self._callbacks[data] = []
            callbacks.append(callback)
        finally:
            self._lock.release()

    def unregister_callback(self, data, callback):
        if __debug__: log(DEBUG, 'Odregistrace callbacku', (data, callback))
        self._lock.acquire()
        try:
            try:
                callbacks = self._callbacks[data]
            except KeyError:
                pass
            else:
                try:
                    callbacks.remove(callback)
                except ValueError:
                    pass
        finally:
            self._lock.release()


class _RemoteWrapper(object):

    _CACHEABLE_METHODS = ()
    
    def __init__(self, uri):
        self._uri = uri
        self._proxy = None
        self._cache = {}

    def _cached_method(self, attr, *args):
        key = (attr, args)
        try:
            result = self._cache[key]
        except KeyError:
            result = self._cache[key] = attr(*args)
        return result

    def __getattr__(self, name):
        if name and name[0] != '_':
            if self._proxy is None:
                self._proxy = Pyro.core.getProxyForURI(self._uri)
            attr = getattr(self._proxy, name)
            if name in self._CACHEABLE_METHODS:
                result = lambda *args: self._cached_method(attr, *args)
            else:
                result = attr
        else:
            raise AttributeError(name)
        return result

    def __getstate__(self):
        state = copy.copy(self.__dict__)
        state['_proxy'] = None
        return state

    def clone_proxy(self):
        """Zajisti objektu novou instanci jeho Pyro proxy.

        Vol�n� t�to metody je u�ite�n� po nakop�rov�n� objektu, zejm�na m�-li
        b�t pou�it v�jin�m vl�kn�, tj. Pyro proxy nesm� b�t sd�len� s�jin�m
        objektem.

        """
        self._proxy = None


class RemoteData(_RemoteWrapper):
    """Klientsk� proxy pro p��stup k�dat�m v�Pyro."""

    _CACHEABLE_METHODS = ('key', 'find_column')
    _data_changes_watcher = None

    def __init__(self, uri, password):
        if RemoteData._data_changes_watcher is None:
            RemoteData._data_changes_watcher = _DataChangesWatcher()
        super(RemoteData, self).__init__(uri)
        self._password = password
        RemoteData._data_changes_watcher.register(self)

    def add_callback_on_change(self, function):
        if RemoteData._data_changes_watcher is None:
            # Tento test je nutn� i�zde, proto�e po odpicklov�n� nen� spu�t�no
            # __init__.  A�v�__init__ je to nutn� kv�li p�edkovi.  Hm, na
            # serveru by ale asi k�registraci doch�zet nem�lo?  Tak�e TODO:.
            RemoteData._data_changes_watcher = _DataChangesWatcher()
        RemoteData._data_changes_watcher.register_callback(self, function)
    
    def remove_callback_on_change(self, function):
        RemoteData._data_changes_watcher.unregister_callback(self, function)

    def __getattr__(self, name):
        method = super(RemoteData, self).__getattr__(name)
        return lambda *args, **kwargs: method(self._password, *args, **kwargs)


class RemoteDataFactory(_RemoteWrapper):
    """Klientsk� data factory se z�sk�v�n�m datov�ch objekt� p�es Pyro."""
