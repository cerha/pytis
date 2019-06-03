# -*- coding: utf-8 -*-

# Copyright (C) 2018, 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2013 Brailcom, o.p.s.
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

"""Implementace různých cachí.

Modul nabízí třídy umožňující provádět různé typy cachování.

"""
from future import standard_library

import _thread
import collections

from pytis.util import Locked, Counter, super_

standard_library.install_aliases()  # to get collections.UserDict


class _Cache(collections.UserDict):
    """Bázový objekt pro všechny cache."""

    def __init__(self, provider, validator=None):
        """Inicializuj instanci.

        Argumenty:

          provider -- funkce jednoho argumentu, kterým je klíč, vracející
            hodnotu odpovídající danému klíči
          validator -- funkce jednoho argumentu, kterým je klíč, vracející
            pravdu právě když položka odpovídající klíči je platná; může být
            též 'None', v kterémžto případě jsou všechny položky automaticky
            považovány za platné

        """
        collections.UserDict.__init__(self)
        assert isinstance(provider, collections.Callable)
        self._provider = provider
        self._validator = validator

    def __getitem__(self, key):
        """Vrať hodnotu odpovídající klíči 'key'.

        Pokud hodnota není v cache přítomna, použij pro její získání funkci
        'provider' a ulož ji do cache.  Metoda sama o sobě nevyvolává žádnou
        výjimku.

        """
        try:
            result = super_(_Cache).__getitem__(self, key)
            if self._validator is not None and not self._validator(key):
                raise KeyError()
        except KeyError:
            result = self[key] = self._provider(key)
        return result

    def __setitem__(self, key, value):
        """Ulož 'value' s 'key' do cache."""
        super_(_Cache).__setitem__(self, key, value)

    def reset(self):
        """Kompletně zruš aktuální obsah cache."""
        self.data = {}


class SimpleCache(_Cache):
    """Jednoduchá cache s neomezeným počtem uložených položek."""


class LimitedCache(_Cache):
    """Cache s omezeným počtem položek.

    V konstruktoru je zadán maximální počet položek cache, který není nikdy
    překročen.

    """

    def __init__(self, provider, limit=1000):
        """Inicializuj instanci.

        Argumenty:

          provider -- stejné jako v předkovi
          limit -- nezáporný integer určující maximální povolený počet položek
            cache

        """
        super_(LimitedCache).__init__(self, provider)
        assert isinstance(limit, int), limit
        self._limit = limit
        self._counter = Counter()
        self._lock = _thread.allocate_lock()

    def __getitem__(self, key):
        result = super_(LimitedCache).__getitem__(self, key)
        return result

    def __setitem__(self, key, value):
        if self._limit > 0:
            with Locked(self._lock):
                if self._counter.next() > self._limit:
                    self._collect()
                super_(LimitedCache).__setitem__(self, key, value)

    def reset(self):
        super_(LimitedCache).reset(self)
        self._counter.reset()

    def _collect(self):
        self.reset()


class RangeCache(_Cache):
    """Cache s celočíselnými klíči ukládající souvislé úseky dat.

    Tyto úseky jsou dány souvislými intervaly klíčů, dané velikosti.
    V podstatě se tedy jedná o pole cachovaných hodnot.

    """

    def __init__(self, provider, size=1000):
        """Inicializuj instanci.

        Argumenty:

          provider -- stejné jako v předkovi
          size -- nezáporný integer, maximální velikost cachovaného úseku dat

        """
        _Cache.__init__(self, provider)
        assert isinstance(size, int), size
        self._size = size
