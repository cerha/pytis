# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2013 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Implementace různých cachí.

Modul nabízí třídy umožňující provádět různé typy cachování.

"""
from __future__ import print_function
from future import standard_library

import _thread

from pytis.util import Locked, Counter

standard_library.install_aliases()  # to get collections.UserDict


class _Cache(dict):
    """Base class for all caches."""

    def __init__(self, provider, validator=None):
        """Initialize the instance.

        Arguments:
          provider -- function of one argument (the cache key) returning the
            value corresponding to given key.
          validator -- function of one argument (the cache key) returning true
            if the value corresponding to given key is valid.  If None, all
            values are considered valid.

        """
        super(_Cache, self).__init__()
        assert callable(provider)
        self._provider = provider
        self._validator = validator

    def __getitem__(self, key):
        """Return the value corresponding to 'key'.

        If the value is not present in the cache, use the 'provider' function
        to retrieve it and save it to the cache.  The method itself doesn't
        raise any exception.

        """
        try:
            result = super(_Cache, self).__getitem__(key)
            if self._validator is not None and not self._validator(key):
                raise KeyError()
        except KeyError:
            result = self[key] = self._provider(key)
        return result

    def __setitem__(self, key, value):
        """Save 'value' with 'key' to the cache."""
        super(_Cache, self).__setitem__(key, value)

    def reset(self):
        """Completely discard all current cache content."""
        self.clear()


class SimpleCache(_Cache):
    """Simple cache with unlimited number of items."""


class LimitedCache(_Cache):
    """Cache with a limited number of items."""

    def __init__(self, provider, limit=1000):
        """Initialize the instance.

        Arguments:

          provider -- same as in parent class
          limit -- maximal number of cached items as a non-negative integer.

        """
        super(LimitedCache, self).__init__(provider)
        assert isinstance(limit, int), limit
        self._limit = limit
        self._counter = Counter()
        self._lock = _thread.allocate_lock()

    def __getitem__(self, key):
        return super(LimitedCache, self).__getitem__(key)

    def __setitem__(self, key, value):
        if self._limit > 0:
            with Locked(self._lock):
                if self._counter.next() > self._limit:
                    self._collect()
                super(LimitedCache, self).__setitem__(key, value)

    def reset(self):
        super(LimitedCache, self).reset()
        self._counter.reset()

    def _collect(self):
        self.reset()


class RangeCache(_Cache):
    """Cache with integer keys saving continuous ranges of data.

    The ranges are given by continuous intervals of key values and their size
    is given.  The result is actually an array of cached values, but values
    outside the given range are discarded.

    """

    def __init__(self, provider, size=1000):
        """Initialize the instance.

        Arguments:

          provider -- same as in parent class.
          size -- maximal cached range size as a non-negative integer.

        """
        super(RangeCache, self).__init__(provider)
        assert isinstance(size, int), size
        self._size = size
