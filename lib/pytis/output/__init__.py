# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2005 Brailcom, o.p.s.
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

"""Formátování výstupů, zejména pro tisk.

Tento modul poskytuje všechny funkce potřebné pro jednoduché sestavení výstupu.
Konkrétně definuje jednoduché značkování textu a provádí jeho formátování.

"""

from pytis.util import *

from exception import *
from resolver import *
from markup import *
from formatter import *
from flibdata import *
from flibutil import *
