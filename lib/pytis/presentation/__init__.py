# -*- coding: utf-8 -*-

# Copyright (C) 2002-2017 Brailcom, o.p.s.
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

"""Data transformations and presentation.

This module is intended to solve processing of data which does not belong to
the module 'pytis.data', is generally needed by user interfaces, but is not
specific for any particular user interface.  The typical functionality solved
by this module is computing the values of virtual fields, handling dynamic
changes of editability of input fields, their validation and integrity
checking, dynamic codebook filtering etc.

"""

from spec import *
from field import *
from types_ import PrettyType, PrettyTreeOrder, PrettyFoldable

for file in (spec, field):
    file.__dict__.update(globals())
