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
    # Starší defsy počítají s tím, že importem pytis.extensions jsou
    # importovány i všechny identifikátory z pytis.form.  Protože však
    # nechceme, aby pytis.extensions závisely na pytis.form a tím potažmo na
    # wx, importujeme tyto identifikátory jen pokud jsou pytis.extensions
    # používány z wx aplikace, v kterémžto případě by modul pytis.form již měl
    # být natažen.  Časem by bylo dobré to nedělat vůbec - defsy by na
    # formulářích neměly záviset.  Pokud závisí (interaktivní obslužné rutiny
    # používající formuláře či dialogy), nechť si potřebné identifikátory
    # importují vlastnoručně.
    from pytis.form import *
    # Modul defs je také závislý na pytis.form, takže jej chceme importovat
    # jen v případě běhu wx aplikace.
    from defs import *
    defs.__dict__.update(globals())

    
