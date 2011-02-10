# -*- coding: utf-8 -*-

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

"""Pomůcky pro práci s konfigurací v Pytis aplikacích.

Kromě standardních konfiguračních voleb Pytisu jsou v aplikacích často potřeba
hodnoty jednak globálních nastavení (společných pro všechny uživatele) a jednak
uživatelských nastavení.  Tyto hodnoty jsou většinou specifické pro danou
aplikaci a je třeba zajistit přístup k nim i v rámci databázových procedur a
jiných datových operací.  Proto jsou řešeny pomocí databázové konfigurační
tabulky, která vrací vždy jeden řádek obsahující všechny dostupné volby.  Níže
definované třídy a funkce zjednodušují práci s těmito hodnotami v rámci
pythonového kódu ve specifikacích aplikace.

""" 

from pytis.extensions import *

import thread, pytis.data

import config


class DBConfig(object):
    """Konfigurace spojená s datovým objektem.

    Konfigurace vnitřně pracuje s datovým objektem vytvořeným nad specifikací
    určenou argumentem konstruktoru.  Předpokládá se, že datový objekt vrací
    vždy jen jeden řádek (na úrovni SQL omezený např na aktuálního uživatele).
    Hodnotu datového sloupečku je potom možné z tohoto objektu získat jako ze
    slovníku.

    Zápis hodnoty do slovníku vyvolá zapsání změněné hodnoty do databáze.
    Případné změny dat na úrvni databáze nejsou tímto objektem v současné
    implementaci reflektovány.

    """
    _data_object_cache = {}
    _data_object_lock = thread.allocate_lock()

    def __init__(self, name, callback=None, transaction=None):
        """Inicializuj instanci.

        Argumenty:

          name -- určuje název specifikace datového objektu pro resolver.

          callback -- pokud není None, bude daná funkce volána při každé změně
            v datovém objektu.  Jde o funkci jednoho argumentu, kterým je
            (aktualizovaná) instance 'DBConfig'.

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
        """Vrať hodnotu 'key' jako instanci 'pytis.data.Value'."""
        return self._row[key]
        
    def __getitem__(self, key):
        """Vrať hodnotu 'key' jako Pythonovou hodnotu."""
        return self._row[key].value()

    def __setitem__(self, key, value):
        """Nastav hodnotu 'key' jako Pythonovou hodnotu."""
        type = self._row[key].type()
        self._row[key] = pytis.data.Value(type, value)
        self._data.update(self._key, self._row, transaction=self._transaction)

    def __contains__(self, key):
        return key in self._row

    def has_key(self, key):
        return self.__contains__(key)
    
    def keys(self):
        return self._row.keys()

    def items(self):
        return tuple([(key, self[key]) for key in self._row.keys()])


def cfg_param(column, cfgspec='Nastaveni.BvCfg', value_column=None, transaction=None):
    """Vrací instanci Value pro konfigurační parametr.

    Argumenty:

      column -- název sloupce v konfigurační tabulce uvedené ve specifikaci
        udané druhým parametrem.
      cfgspec -- volitelný název specifikace s vazbou na konfigurační tabulku.
      value_column -- pokud je požadavaný sloupec Codebook, umožňuje získat
        hodnotu uživatelského sloupce.
      transaction -- transakce pro datové operace  

    """
    dbconfig = DBConfig(cfgspec, transaction=transaction)
    if column not in dbconfig:
        return pytis.data.Value(None, None)
    value = dbconfig.value(column)
    if value.type().enumerator():
        return cb2colvalue(value, column=value_column, transaction=transaction)
    else:
        assert value_column is None, "Column '%s' has no enumerator!" % column
        return value
