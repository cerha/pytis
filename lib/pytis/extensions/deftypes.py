# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2013 OUI Technology Ltd.
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

"""Definice odvozených datových typů."""

from __future__ import print_function
import pytis.data
from pytis.util import ProgramError
from pytis.presentation import PrettyTreeOrder


class Price(pytis.data.Float):

    def _init(self, not_null=True, precision=2, **kwargs):
        super(Price, self)._init(precision=precision, not_null=not_null, **kwargs)

    def default_value(self):
        value, error = self.validate('0')
        if error is not None:
            raise ProgramError("Can't validate default value")
        return value


class StringNotNull(pytis.data.String):

    def _init(self, **kwargs):
        kwargs['not_null'] = True
        super(StringNotNull, self)._init(**kwargs)


class _TreeOrder(PrettyTreeOrder, pytis.data.String):
    pass
