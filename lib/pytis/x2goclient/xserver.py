#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018 Brailcom, o.p.s.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Run a X-server in a separate process.

This module is run as 'python -m pytis.x2goclient.xserver' to start a local X11
server.  Use the class 'pytis.x2goclient.clientprocess.XServer' to start the
server in a subprocess from Python.

"""

from pytis.x2goclient.x2goclient import XServer

if __name__ == '__main__':
    XServer()
