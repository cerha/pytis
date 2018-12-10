# -*- coding: utf-8 -*-

# Copyright (C) 2018 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2013 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import pdb
import socket
import sys


# Based on the example from http://www.dzone.com/snippets/remote-debugging-python-using
class Rdb(pdb.Pdb):
    """Debugger for remote processes such as Wiking applications.

    Put the following line to the source code to make a remote breakpoint:

      pytis.util.rdb().set_trace()

    Then you can connect to the debugger from a command line like follows:

      telnet localhost 1111

    """
    def __init__(self, port=1111):
        """
        Arguments:

          port -- port to listen on for incoming client connections; integer

        """
        self._rdb_stdout = sys.stdout
        self._rdb_stdin = sys.stdin
        self._rdb_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._rdb_socket.bind(('localhost', port,))
        self._rdb_socket.listen(1)
        client_socket, __address = self._rdb_socket.accept()
        handle = client_socket.makefile('rw')
        pdb.Pdb.__init__(self, stdin=handle, stdout=handle)
        sys.stdout = sys.stdin = handle

    def __del__(self):
        self._rdb_finish()
        super(Rdb, self).__del__()

    def _rdb_finish(self):
        if self._rdb_socket is not None:
            sys.stdout = self._rdb_stdout
            sys.stdin = self._rdb_stdin
            self._rdb_socket.close()
            self._rdb_socket = None

    def do_run(self, arg):
        self._rdb_finish()
        self.set_continue()
        return 1

    do_r = do_run

_rdb = None


def rdb(port=1111):
    "Return the shared 'Rdb' instance."
    global _rdb
    if _rdb is None:
        _rdb = Rdb(port=port)
    return _rdb
