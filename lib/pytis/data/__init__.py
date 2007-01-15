# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2005, 2006 Brailcom, o.p.s.
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

"""Modul pro pr�ci s�datov�mi zdroji.

��elem modulu je odst�nit v�voj��e aplikace od low-level pr�ce s�datov�mi
zdroji.  Modul zav�d�:

- Typovou abstrakci, viz modul 'types_'.

- Abstrakci datov�ho zdroje, viz modul 'data'.

- Podporu pro pr�ci s�datov�mi zdroji napojen�mi na rela�n� datab�ze, viz modul
  'dbdata'.

"""

from pytis.util import *

from types_ import *
from data import *
from access import *
from dbdata import DBConnection, DBData, DBBinding, DBColumnBinding, DBException, DBSystemException, DBUserException, DBLoginException
from defaults import DBDataDefault, DBCounterDefault, DBFunctionDefault

types_.__dict__.update(globals())
