# -*- coding: utf-8 -*-

# Copyright (C) 2001, 2002, 2006, 2010, 2011, 2012 Brailcom, o.p.s.
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

__version__ = "1.2.1"

import util
config = util.Configuration()
import sys
sys.modules['config'] = config
import data

# dbdefs has to be imported in a very special way.  It can't be direct
# pytis.dbdefs module because such a module can't be imported and used on top
# level inside its module files.  The tricks applied here are apparently
# necessary to make the whole pytis.dbdefs import work.
if False:
    # Import disabled for now as it somewhat slows down import of pytis.
    import os
    sys.path.insert(0, os.path.join(__path__[0], 'db'))
    import dbdefs
    del sys.path[0]
    sys.modules['pytis.dbdefs'] = dbdefs
    del sys.modules['dbdefs']
    
