# -*- coding: utf-8 -*-

# Copyright (C) 2001, 2002, 2005, 2006, 2007, 2013 Brailcom, o.p.s.
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

"""Modul pro práci s datovými zdroji.

Účelem modulu je odstínit vývojáře aplikace od low-level práce s datovými
zdroji.  Modul zavádí:

- Typovou abstrakci, viz modul 'types_'.

- Abstrakci datového zdroje, viz modul 'data'.

- Podporu pro práci s datovými zdroji napojenými na relační databáze, viz modul
  'dbdata'.

"""

from types_ import *
from data import *
from access import *
from dbdata import DBConnection, DBData, DBBinding, DBColumnBinding, DBException, \
    DBSystemException, DBUserException, DBLoginException, DBInsertException, \
    DBLockException, DBRetryException, DBConnectionPool
from defaults import DBDataDefault, DBCounterDefault, DBFunctionDefault, \
    DBTransactionDefault, default_access_groups, reload_session_variables
from deprecated import Oid
import dbdefs

# TODO: The following import serves for backward compatibility of applications
# and should be removed once applications switch to using the
# default_access_groups function.
from postgresql import PostgreSQLUserGroups

types_.__dict__.update(globals())
