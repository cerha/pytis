# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006 Brailcom, o.p.s.
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

import pytis.data

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

    def __init__(self, name, callback=None):
        """Inicializuj instanci.

        Argumenty:

          name -- ur�uje n�zev specifikace datov�ho objektu pro resolver.

          callback -- pokud nen� None, bude dan� funkce vol�na p�i ka�d� zm�n�
            v datov�m objektu.  Jde o funkci jednoho argumentu, kter�m je
            (aktualizovan�) instance 'DBConfig'.

        """
        global data_object_cache
        try:
            cache = data_object_cache
        except NameError:
            cache = data_object_cache = {}
        try:
            data = cache[name]
        except KeyError:
            data = data_object(name)
            if data is not None:
                cache[name] = data
        self._data = data
        self._data.select()
        self._row = self._data.fetchone()
        self._key = [self._row[c.id()] for c in self._data.key()]
        self._data.close()
        if callback:
            self._callback = callback
            self._data.add_callback_on_change(self._on_change)

    def _on_change(self):
        self._data.select()
        self._row = self._data.fetchone()
        self._data.close()
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
        self._data.update(self._key, self._row)

    def has_key(self, key):
        return self._row.has_key(key)

    def keys(self):
        return self._row.keys()

    def items(self):
        return tuple([(key, self[key]) for key in self._row.keys()])


def cfg_param(column, cfgspec='Nastaveni.BvCfg', value_column=None):
    """Vrac� instanci Value pro konfigura�n� parametr.

    Argumenty:

      column -- n�zev sloupce v konfigura�n� tabulce uveden� ve specifikaci
        udan� druh�m parametrem.
      cfgspec -- voliteln� n�zev specifikace s vazbou na konfigura�n� tabulku.
      value_column -- pokud je po�adavan� sloupec Codebook, umo��uje z�skat
        hodnotu u�ivatelsk�ho sloupce.

    """
    dbconfig = DBConfig(cfgspec)
    if not dbconfig.has_key(column):
        return pytis.data.Value(None, None)
    value = dbconfig.value(column)
    if value.type().enumerator():
        return cb2colvalue(value, column=value_column)
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
            log(OPERATIONAL, "Nepoda�ilo se obnovit ulo�enou konfiguraci:", e)
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


