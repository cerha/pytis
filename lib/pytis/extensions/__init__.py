# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2005-2011 Brailcom, o.p.s.
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

import sys
if sys.getdefaultencoding() != 'iso-8859-2':
    reload(sys)
    sys.setdefaultencoding('iso-8859-2')

import pytis.data, pytis.form

from dbconfig import *
from dbutils import *
from misc import *
from deftypes import *
from deftypes import _TreeOrder
from spec import *
from defs import *
from dmp import dmp_add_member, dmp_add_form, dmp_commit, dmp_import, dmp_reset_rights, dmp_update_form

for file in (dbconfig, dbutils, misc, types, spec, defs):
    file.__dict__.update(globals())

# Většina defsů počítají s tím, že importem pytis.extensions jsou importovány i
# všechny identifikátory z pytis.form.  Časem by bylo dobré se toho zbavit a
# používat celé názvy `pytis.forms.*'.  Zde je import každopádně až na konci,
# abychom "nezasvinili" jmenný prostor modulů.
from pytis.form import *
