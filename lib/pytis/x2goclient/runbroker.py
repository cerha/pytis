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

import io
import sys
import base64
import cPickle as pickle

def die(error):
    sys.stderr.write(error + "\n")
    sys.exit(1)

if __name__ == '__main__':
    """Run a Pytis X2Go Broker in a separate process.
    
    This module is run as 'python -m pytis.x2goclient.runbroker ...' to load
    information from a remote X2Go broker server.  The connection parameters
    are passed in through STDIN and the result is dumped to STDOUT both as
    pickled (serialized) Python objects.
    
    You don't normally want to run this module yourself and pickle/unpickle its
    input/output, but you rather use the Python class 'BrokerProcess' to do so
    through its API.

    """
    stdout = sys.stdout
    sys.stdout = io.BytesIO()
    from .x2goclient import PytisSshProfiles
    try:
        kwargs = pickle.loads(base64.b64decode(sys.stdin.read()))
    except (TypeError, EOFError):
        die("Expected base64 encoded pickled dictionary of PytisSshProfiles arguments on STDIN.")
    try:
        broker = PytisSshProfiles(**kwargs)
    except PytisSshProfiles.ConnectionFailed:
        die("Broker connection failed.")
    stdout.write(base64.b64encode(pickle.dumps((broker.load_profiles()))))
    sys.stdout = stdout
