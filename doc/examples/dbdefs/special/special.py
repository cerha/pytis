# -*- coding: utf-8 -*-

# Copyright (C) 2013 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
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

"""Demonstration of separate database definitions.

It's possible to process either complete database definitions using

  gsql dbdefs

or just definitions in this module using

  gsql dbdefs.special

Whether definitions from dbdefs.special are included in

  gsql dbdefs

output or not is determined by the presence or absence of 'special' import in
dbdefs/__init__.py.

"""

import pytis.data
import pytis.data.gensqlalchemy as sql

class Special(sql.SQLTable):
    name = 'special'
    fields = (sql.Column('x', pytis.data.Integer()),)
