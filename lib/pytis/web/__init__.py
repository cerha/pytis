# -*- coding: utf-8 -*-

# Copyright (C) 2006, 2007, 2008, 2011, 2013 Brailcom, o.p.s.
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

from pytis.util import translate as _

import pytis.data as pd
from pytis.presentation import *

import lcg
from lcg import concat, log as debug

from request import *
from form import *
from field import *
from dialog import *

_globals = dict([(k,v) for k,v in globals().items() if not k.startswith('_')])
for _file in (form, field, ):
    _file.__dict__.update(_globals)
del _globals
