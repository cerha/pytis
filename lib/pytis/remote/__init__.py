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

"""Modul s�r�zn�mi pom�ckami pro vzd�len� vol�n� k�du.

Je ur�en p�edev��m pro serverovou i�klientskou ��st serveru zp��stup�uj�c�ho
data prost�ednictv�m vzd�len�ch datov�ch objekt�.

"""

from pytis.util import *

from util import *
from data import *
from server import *

# TODO:
# - security checks of transferred arguments
# Pyro:
# - use SSL communication (can't compile M2Crypto)
# - connection password/certificate authentization
