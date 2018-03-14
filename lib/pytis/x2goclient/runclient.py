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

import gevent.monkey
gevent.monkey.patch_all() # noqa: E402

import sys
import argparse
import rpyc.utils.server
from .clientprocess import ClientService

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=(
            """Run a Pytis X2Go client in a separate process controlled through an RPyC API.

            This module is run as 'python -m pytis.x2goclient.runclient ...' to
            start an RPyC service server with the interface defined by
            'ClientService'.  A new Pytis X2Go client may be invoked and
            controlled through this service passing it X2Go session parameters
            from another Python process.

            However you don't normally want to run this module and connect its RPyC
            service directly.  Use the Python class 'ClientProcess' to do so through
            its API.

            """),
        add_help=True, argument_default=None
    )
    parser.add_argument(
        '--port',
        default=18861, type=int,
        help="Port number where to start the RPyC server.",
    )
    args = parser.parse_args()

    # Read the service authentication key from STDIN
    key = sys.stdin.readline().strip()

    server = rpyc.utils.server.OneShotServer(
        ClientService, port=args.port,
        protocol_config=dict(allow_public_attrs=True),
        authenticator=ClientService.Authenticator(key),
    )
    server.start()
