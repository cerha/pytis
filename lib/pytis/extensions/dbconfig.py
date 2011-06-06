# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2007, 2010, 2011 Brailcom, o.p.s.
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
tabulky, kter� vrac� v�dy jeden ��dek obsahuj�c� v�echny dostupn� volby.  N�e
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
            self._row = data.fetchone()
            data.close()
        with_lock(self._data_object_lock, lfunction)
        self._key = [self._row[c.id()] for c in data.key()]
        if callback:
            self._callback = callback
            self._data.add_callback_on_change(self._on_change)

    def _on_change(self):
        def lfunction():
            self._data.select(transaction=self._transaction)
            self._row = self._data.fetchone()
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
