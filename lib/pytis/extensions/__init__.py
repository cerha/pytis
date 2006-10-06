# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2005, 2006 Brailcom, o.p.s.
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

import pytis.data

from dbconfig import *
from dbutils import *
from misc import *
from types import *
from spec import *

for file in (dbconfig, dbutils, misc, types, spec):
    file.__dict__.update(globals())

if hasattr(pytis, 'form'):
    # Star�� defsy po��taj� s t�m, �e importem pytis.extensions jsou
    # importov�ny i v�echny identifik�tory z pytis.form.  Proto�e v�ak
    # nechceme, aby pytis.extensions z�visely na pytis.form a t�m pota�mo na
    # wx, importujeme tyto identifik�tory jen pokud jsou pytis.extensions
    # pou��v�ny z wx aplikace, v kter�m�to p��pad� by modul pytis.form ji� m�l
    # b�t nata�en.  �asem by bylo dobr� to ned�lat v�bec - defsy by na
    # formul���ch nem�ly z�viset.  Pokud z�vis� (interaktivn� obslu�n� rutiny
    # pou��vaj�c� formul��e �i dialogy), nech� si pot�ebn� identifik�tory
    # importuj� vlastnoru�n�.
    from pytis.form import *
    # Modul defs je tak� z�visl� na pytis.form, tak�e jej chceme importovat
    # jen v p��pad� b�hu wx aplikace.
    from defs import *
    defs.__dict__.update(globals())

    
