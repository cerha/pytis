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
server.  The class 'pytis.x2goclient.clientprocess.XServer' does this for you
so you should not need to run this module directly.

"""

import argparse
from pytis.x2goclient.x2goclient import start_xserver, XServerAlreadyRunning, XServerNotInstalled

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Start X2Go X-server in a separate process',
        add_help=True
    )
    parser.add_argument(
        '--variant', required=True,
        help="Retrieve session profiles via an X2Go Session Broker under the given URL.",
    )
    args = parser.parse_args()

    try:
        print start_xserver(variant=args.variant)
    except XServerAlreadyRunning as e:
        print e.args[1]
    except XServerNotInstalled:
        pass
