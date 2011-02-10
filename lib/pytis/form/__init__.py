# -*- coding: utf-8 -*-

# Copyright (C) 2001-2009, 2011 Brailcom, o.p.s.
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

"""Práce s uživatelským rozhraním.

Modul obsahuje vše co souvisí s uživatelským rozhraním a vše co má něco
společného s wxWindows.  Všechny obslužné záležitosti uživatelského rozhraní
jsou implementovány v tomto modulu, mimo tento modul se definují pouze
specifikace jednotlivých prvků rozhraní.

"""

from pytis.util import *
from pytis.presentation import *

if 'WXVER' in os.environ:
    version = os.environ["WXVER"]
else:
    try:
        import wx
    except ImportError:
        version = '2.6'
    else:
        version = None
if version is not None:
    import wxversion
    try:
        ok = wxversion.checkInstalled(version)
    except ValueError:
        ok = False
    if ok:
        wxversion.select(version)
    else:
        sys.stderr.write("Invalid wx version: %s\nInstalled versions are: %s\n" % \
                         (version, ', '.join(wxversion.getInstalled())))

_list = list

from event import *
from command import *
from screen import *
from dialog import *
from search import *
from inputfield import *
from form import *
from list import *
from dualform import *
from output import *
from application import *
from configui import *
from commands_ import *
import _grid

list_ = list
list = _list

# Řešení cyklických závislostí souborů
for file in (event, application, command, dialog, form, dualform,
             inputfield, list_, output, screen, search, _grid):
    file.__dict__.update(globals())

