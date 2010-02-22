# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2007, 2010 Brailcom, o.p.s.
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

"""Pom�cky pro pr�ci s konfigurac� v Pytis aplikac�ch.

Krom� standardn�ch konfigura�n�ch voleb Pytisu jsou v aplikac�ch �asto pot�eba
hodnoty jednak glob�ln�ch nastaven� (spole�n�ch pro v�echny u�ivatele) a jednak
u�ivatelsk�ch nastaven�.  Tyto hodnoty jsou v�t�inou specifick� pro danou
aplikaci a je t�eba zajistit p��stup k nim i v r�mci datab�zov�ch procedur a
jin�ch datov�ch operac�.  Proto jsou �e�eny pomoc� datab�zov� konfigura�n�
tabulky, kter� vrac� v�dy jeden ��dek obsahuj�c� v�echny dostupn� volby.  N��e
definovan� t��dy a funkce zjednodu�uj� pr�ci s t�mito hodnotami v r�mci
pythonov�ho k�du ve specifikac�ch aplikace.

""" 

from pytis.extensions import *

import thread, pytis.data

import config


class DBConfig(object):
    """Konfigurace spojen� s datov�m objektem.

    Konfigurace vnit�n� pracuje s datov�m objektem vytvo�en�m nad specifikac�
    ur�enou argumentem konstruktoru.  P�edpokl�d� se, �e datov� objekt vrac�
    v�dy jen jeden ��dek (na �rovni SQL omezen� nap� na aktu�ln�ho u�ivatele).
    Hodnotu datov�ho sloupe�ku je potom mo�n� z tohoto objektu z�skat jako ze
    slovn�ku.

    Z�pis hodnoty do slovn�ku vyvol� zaps�n� zm�n�n� hodnoty do datab�ze.
    P��padn� zm�ny dat na �rvni datab�ze nejsou t�mto objektem v sou�asn�
    implementaci reflektov�ny.

    """
    _data_object_cache = {}
    _data_object_lock = thread.allocate_lock()

    def __init__(self, name, callback=None, transaction=None):
        """Inicializuj instanci.

        Argumenty:

          name -- ur�uje n�zev specifikace datov�ho objektu pro resolver.

          callback -- pokud nen� None, bude dan� funkce vol�na p�i ka�d� zm�n�
            v datov�m objektu.  Jde o funkci jednoho argumentu, kter�m je
            (aktualizovan�) instance 'DBConfig'.

        """
        key = (name, transaction)
        try:
            data = DBConfig._data_object_cache[key]
        except KeyError:
            data = data_object(name)
            if data is not None:
                DBConfig._data_object_cache[key] = data
        self._data = data
        self._transaction = transaction
        def lfunction():
            data.select(transaction=transaction)
            self._row = data.fetchone(transaction=transaction)
            data.close()
        with_lock(self._data_object_lock, lfunction)
        self._key = [self._row[c.id()] for c in data.key()]
        if callback:
            self._callback = callback
            self._data.add_callback_on_change(self._on_change)

    def _on_change(self):
        def lfunction():
            self._data.select(transaction=self._transaction)
            self._row = self._data.fetchone(transaction=self._transaction)
            self._data.close()
        with_lock(self._data_object_lock, lfunction)
        self._callback(self)

    def value(self, key):
        """Vra� hodnotu 'key' jako instanci 'pytis.data.Value'."""
        return self._row[key]
        
    def __getitem__(self, key):
        """Vra� hodnotu 'key' jako Pythonovou hodnotu."""
        return self._row[key].value()

    def __setitem__(self, key, value):
        """Nastav hodnotu 'key' jako Pythonovou hodnotu."""
        type = self._row[key].type()
        self._row[key] = pytis.data.Value(type, value)
        self._data.update(self._key, self._row, transaction=self._transaction)

    def has_key(self, key):
        return self._row.has_key(key)

    def keys(self):
        return self._row.keys()

    def items(self):
        return tuple([(key, self[key]) for key in self._row.keys()])


def cfg_param(column, cfgspec='Nastaveni.BvCfg', value_column=None, transaction=None):
    """Vrac� instanci Value pro konfigura�n� parametr.

    Argumenty:

      column -- n�zev sloupce v konfigura�n� tabulce uveden� ve specifikaci
        udan� druh�m parametrem.
      cfgspec -- voliteln� n�zev specifikace s vazbou na konfigura�n� tabulku.
      value_column -- pokud je po�adavan� sloupec Codebook, umo��uje z�skat
        hodnotu u�ivatelsk�ho sloupce.
      transaction -- transakce pro datov� operace  

    """
    dbconfig = DBConfig(cfgspec, transaction=transaction)
    if not dbconfig.has_key(column):
        return pytis.data.Value(None, None)
    value = dbconfig.value(column)
    if value.type().enumerator():
        return cb2colvalue(value, column=value_column, transaction=transaction)
    else:
        assert value_column is None, "Column '%s' has no enumerator!" % column
        return value

    
def saved_config_reader(name, column):
    """Vra� funkci pro na�ten� ulo�en� u�ivatelsk� konfigurace z datab�ze.

    Argumenty:

      name -- n�zev specifikace, ze kter� je vytvo�en datov� objekt.
      column -- n�zev sloupe�ku, ve kter�m je ulo�en� zapicklovan� konfigurace.

    Funkce je ur�ena pro pou�it� ve specifika�n� funkci `read_config()' v
    `application.py'.

    """
    import cPickle as pickle
    def reader():
        value = DBConfig(name)[column]
        try:
            return pickle.loads(str(value))
        except pickle.UnpicklingError, e:
            log(OPERATIONAL, "Couldn't restore saved configuration:", e)
            return ()
    return reader

def saved_config_writer(name, column):
    """Vra� funkci pro ulo�en� u�ivatelsk� konfigurace do datab�ze.

    Argumenty:

      name -- n�zev specifikace, ze kter� je vytvo�en datov� objekt.
      column -- n�zev sloupe�ku, do kter�ho m� b�t konfigurace ulo�ena.

    Funkce je ur�ena pro pou�it� ve specifika�n� funkci `write_config()' v
    `application.py'.

    """
    import cPickle as pickle
    def writer(items):
        DBConfig(name)[column] = pickle.dumps(items)
    return writer

def pytis_config_reader():
    """Vra� funkci pro na�ten� ulo�en� u�ivatelsk� konfigurace z datab�ze.

    Funkce je ur�ena pro pou�it� ve specifika�n� funkci `read_config()' v
    `application.py'.

    """
    import cPickle as pickle
    import binascii,zlib
    def reader():
        try:
            value = dbfunction('read_pytis_config')
            if value:
                pickled = zlib.decompress(binascii.a2b_base64(value))
                return pickle.loads(pickled)
            else:
                return ()
        except pickle.UnpicklingError, e:
            log(OPERATIONAL, "Couldn't restore saved configuration:", e)
            return ()
    return reader

def pytis_config_writer():
    """Vra� funkci pro ulo�en� u�ivatelsk� konfigurace do datab�ze.

    Funkce je ur�ena pro pou�it� ve specifika�n� funkci `write_config()' v
    `application.py'.

    """
    import cPickle as pickle
    import binascii,zlib
    def writer(items):
        pickled = pickle.dumps(items)
        value = binascii.b2a_base64(zlib.compress(pickled))
        try:
            dbfunction('write_pytis_config', ('value', pytis.data.Value(pytis.data.String(), value)))
        except:
            log(OPERATIONAL, "Couldn't save user configuration:", e)
    return writer


def pytis_config_update(oldname, newname):
    """Update saved user configurations after specification name changes.

    Arguments:
      oldname -- original name of the renamed specification as a string
      newname -- new name of the renamed specification as a string

    Saved user configurations refer to the specification name, so if the name changes, saved user
    settings, such as form sorting, displayed columns, saved filters etc. are lost.  This script
    goes through all saved user configurations and fixes them to match the new specification name
    if necessary (if given user has saved config for given specification name).

    This function is designed to be invoked from a shell script.  It may prompt for a database
    password on STDIN and write results to STDOUT or STDERR.

    Limitation: Only form state is currently supported, recent forms and startup forms are
    untouched.
    
    Returns the number of updated records (which contained 'oldname').
    
    """
    import sys, binascii, zlib, cPickle as pickle
    bindings = [pytis.data.DBColumnBinding(column, '_pytis_config', column)
                for column in ('uzivatel', 'config')]
    factory = pytis.data.DataFactory(pytis.data.DBDataDefault, bindings, bindings[0])
    while True:
        try:
            data = factory.create(dbconnection_spec=config.dbconnection)
        except pytis.data.DBLoginException, e:
            if config.dbconnection.password() is None:
                import getpass
                login = config.dbuser
                password = getpass.getpass("Enter database password for %s: " % login)
                config.dbconnection.update_login_data(user=login, password=password)
            else:
                sys.stderr.write("Login failed.\n")
                sys.exit(1)
        else:
            break
    updated = 0
    transaction = pytis.data.DBTransactionDefault(config.dbconnection)
    try:
        data.select(transaction=transaction)
        while True:
            row = data.fetchone(transaction=transaction)
            if row is None:
                break
            saved_config = row['config'].value()
            if saved_config:
                changed = False
                unpacked = dict(pickle.loads(zlib.decompress(binascii.a2b_base64(saved_config))))
                form_state = unpacked.get('form_state')
                if form_state:
                    for key, value in form_state.items():
                        form, name = key.split('/', 1)
                        if name == oldname:
                            del form_state[key]
                            form_state['/'.join((form, newname))] = value
                            changed = True
                if changed:
                    updated += 1
                    v = binascii.b2a_base64(zlib.compress(pickle.dumps(tuple(unpacked.items()))))
                    row['config'] = pytis.data.Value(row['config'].type(), v)
                    error, success = data.update(row[0], row, transaction=transaction)
                    if not success:
                        raise Exception(error)
        data.close()
    except:
        transaction.rollback()
        sys.stderr.write("Transaction ROLLED BACK.\n")
        raise
    else:
        transaction.commit()
    return updated


