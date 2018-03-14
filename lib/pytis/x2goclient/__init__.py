# -*- coding: utf-8 -*-

# Copyright (C) 2017, 2018 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Pytis X2Go client and startup application.

This module defines the classes used by bin/pytis2go.py.

We avoid any imports from submodules here because different submodules are
designed to run in different processes.  The separation into processes is
necessary in order to avoid problems with wx vs. gevent.  Importing wx and x2go
is mutually exclusive, because x2go monkey patches system libraries
(gevent.monkey.patch_all) and they stop to work correctly under wx main
loop. Also wx application stops to work when x2go (glib) main loop takes over.
That is why the application spawns the client through a separate process (see
runclient.py and clientprocess.py).

"""

# ATTENTION: This should be updated on each code change.
X2GOCLIENT_VERSION = '2017-07-13 10:26'
X2GOCLIENT_REQUIRED_VERSION = '2017-07-13 10:26'

# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
