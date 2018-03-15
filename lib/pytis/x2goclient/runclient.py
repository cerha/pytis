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

"""Run a Pytis X2Go client in a separate process controlled through an RPyC API.

This module is run as 'python -m pytis.x2goclient.runclient' to start an
RPyC service server with the interface defined by 'ClientService'.  A new Pytis
X2Go client may be invoked and controlled through this service passing it X2Go
session parameters from another Python process.

However you don't normally want to run this module and connect its RPyC service
directly.  Use the Python class 'ClientProcess' to do so through its API.

"""

import gevent.monkey
gevent.monkey.patch_all() # noqa: E402

import sys
import rpyc.utils.server
from .clientprocess import ClientService

DEFAULT_PORT = 18861
MAX_PORT_ATTEMPTS = 1000

if __name__ == '__main__':
    # Read the service authentication key from STDIN
    key = sys.stdin.readline().strip()
    authenticator = ClientService.Authenticator(key)
    min_port, max_port = DEFAULT_PORT, DEFAULT_PORT + MAX_PORT_ATTEMPTS
    for port in xrange(min_port, max_port):
        try:
            server = rpyc.utils.server.OneShotServer(
                ClientService,
                port=port,
                hostname='localhost',
                authenticator=authenticator,
                protocol_config=dict(allow_public_attrs=True),
            )
        except:
            continue
        # Write the service port number to STDOUT
        sys.stdout.write(str(port) + '\n')
        sys.stdout.flush()
        sys.stderr.write("Starting ClientService RPyC server on port %d.\n" % port)
        server.start()
        break
    else:
        raise Exception("No free ClientService RPyC server port found in range %s-%s." %
                        (min_port, max_port))
