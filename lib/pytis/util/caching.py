# -*- coding: iso-8859-2 -*-

# Cache
# 
# Copyright (C) 2002, 2005, 2006, 2007, 2011 Brailcom, o.p.s.
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

"""Implementace r�zn�ch cach�.

Modul nab�z� t��dy umo��uj�c� prov�d�t r�zn� typy cachov�n�.

"""

import collections
import UserDict

from pytis.util import *


class _Cache(object, UserDict.UserDict):
    """B�zov� objekt pro v�echny cache."""

    def __init__(self, provider, validator=None):
        """Inicializuj instanci.

        Argumenty:

          provider -- funkce jednoho argumentu, kter�m je kl��, vracej�c�
            hodnotu odpov�daj�c� dan�mu kl��i
          validator -- funkce jednoho argumentu, kter�m je kl��, vracej�c�
            pravdu pr�v� kdy� polo�ka odpov�daj�c� kl��i je platn�; m��e b�t
            t� 'None', v�kter�m�to p��pad� jsou v�echny polo�ky automaticky
            pova�ov�ny za platn�
          
        """
        UserDict.UserDict.__init__(self)
        assert isinstance(provider, collections.Callable)
        self._provider = provider
        self._validator = validator

    def __getitem__(self, key):
        """Vra� hodnotu odpov�daj�c� kl��i 'key'.

        Pokud hodnota nen� v�cache p��tomna, pou�ij pro jej� z�sk�n� funkci
        'provider' a ulo� ji do cache.  Metoda sama o�sob� nevyvol�v� ��dnou
        v�jimku.

        """
        try:
            result = super(_Cache, self).__getitem__(key)
            if self._validator is not None and not self._validator(key):
                raise KeyError()
        except KeyError:
            result = self[key] = self._provider(key)
        return result

    def __setitem__(self, key, value):
        """Ulo� 'value' s�'key' do cache."""
        super(_Cache, self).__setitem__(key, value)

    def reset(self):
        """Kompletn� zru� aktu�ln� obsah cache."""
        self.data = {}


class SimpleCache(_Cache):
    """Jednoduch� cache s�neomezen�m po�tem ulo�en�ch polo�ek."""


class LimitedCache(_Cache):
    """Cache s�omezen�m po�tem polo�ek.

    V�konstruktoru je zad�n maxim�ln� po�et polo�ek cache, kter� nen� nikdy
    p�ekro�en.

    """
    def __init__(self, provider, limit=1000):
        """Inicializuj instanci.

        Argumenty:

          provider -- stejn� jako v�p�edkovi
          limit -- nez�porn� integer ur�uj�c� maxim�ln� povolen� po�et polo�ek
            cache
          
        """
        super(LimitedCache, self).__init__(provider)
        assert type(limit) == type(0)
        self._limit = limit
        self._counter = Counter()
        self._lock = thread.allocate_lock()

    def __getitem__(self, key):
        result = super(LimitedCache, self).__getitem__(key)
        return result

    def __setitem__(self, key, value):
        if self._limit > 0:
            def lfunction():
                if self._counter.next() > self._limit:
                    self._collect()
                super(LimitedCache, self).__setitem__(key, value)
            with_lock(self._lock, lfunction)

    def reset(self):
        super(LimitedCache, self).reset()
        self._counter.reset()
        
    def _collect(self):
        self.reset()


class RangeCache(_Cache):
    """Cache s�celo��seln�mi kl��i ukl�daj�c� souvisl� �seky dat.

    Tyto �seky jsou d�ny souvisl�mi intervaly kl���, dan� velikosti.
    V�podstat� se tedy jedn� o�pole cachovan�ch hodnot.

    """
    def __init__(self, provider, size=1000):
        """Inicializuj instanci.

        Argumenty:

          provider -- stejn� jako v�p�edkovi
          size -- nez�porn� integer, maxim�ln� velikost cachovan�ho �seku dat

        """
        super(RangeCache, self).__init__(self, provider)
        assert type(size) == type(0)
        self._size = size
