# -*- coding: iso-8859-2 -*-

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

"""Pr�ce s�u�ivatelsk�m rozhran�m.

Modul obsahuje v�e co souvis� s�u�ivatelsk�m rozhran�m a v�e co m� n�co
spole�n�ho s�wxWindows.  V�echny obslu�n� z�le�itosti u�ivatelsk�ho rozhran�
jsou implementov�ny v�tomto modulu, mimo tento modul se definuj� pouze
specifikace jednotliv�ch prvk� rozhran�.

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

# �e�en� cyklick�ch z�vislost� soubor�
for file in (event, application, command, dialog, form, dualform,
             inputfield, list_, output, screen, search, _grid):
    file.__dict__.update(globals())

