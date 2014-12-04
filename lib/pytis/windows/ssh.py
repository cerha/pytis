#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2014 Brailcom, o.p.s.
# Copyright (C) 2008 Robey Pointer <robeypointer@gmail.com>
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

import getpass
import multiprocessing
import os
import select
import socket
import sys
import threading

import paramiko

from pytis.util import log, EVENT, OPERATIONAL


class ReverseTunnel(multiprocessing.Process):
    """Process running reverse ssh forward connection.

    Parameters are given in the constructor.  The process can be started using
    'start()' method.

    """
    _DEFAULT_SSH_FORWARD_PORT = 10000
    _MAX_SSH_FORWARD_ATTEMPTS = 100

    def __init__(self, ssh_host, forward_port, ssh_port=22, ssh_user=None,
                 ssh_forward_port=None, forward_host='localhost', key_filename=None):
        """
        Arguments:

          ssh_host -- ssh host to connect to; basestring; the starting
            point of the tunnel will be established there
          forward_port -- port to forward the connection to; integer; this is
            the target point of the tunnel
          ssh_port -- ssh port on 'ssh_host' to connect to; integer
          ssh_user -- ssh user on 'ssh_host' to connect to; basestring or
            'None in which case the current user is used
          ssh_forward_port -- forwarding port on the 'ssh_host' to bind to;
            integer or 'None' (in which case an arbitrary free port is
            selected); this is the starting point of the tunnel
          forward_host -- host to forward the connection to; basestring;
            the target point of the tunnel will be there
          key_filename -- name of the file containing the ssh key to use for
            connection to 'ssh_host'; basestring or 'None' in which case
            '~/.ssh/id_rsa' is used

        """
        super(ReverseTunnel, self).__init__()
        self._ssh_host = ssh_host
        self._forward_port = forward_port
        self._ssh_port = ssh_port
        self._ssh_user = ssh_user
        self._ssh_forward_port = ssh_forward_port
        self._forward_host = forward_host
        self._key_filename = key_filename
        self._actual_ssh_forward_port = None

    def forward_port(self):
        """Return the actual target port of the forwarded connection.

        This is the ssh_forward_port specified in the constructor or the actual
        port in case none was specified or 'None' when the tunnel hasn't been
        established yet.
        
        """
        return self._actual_ssh_forward_port
        
    def _handler(self, chan, host, port):
        sock = socket.socket()
        try:
            sock.connect((host, port))
        except Exception as e:
            log(OPERATIONAL, 'Forwarding request to %s:%d failed: %r' % (host, port, e))
            return
        log(EVENT, 'Tunnel open %r -> %r -> %r' % (chan.origin_addr, chan.getpeername(),
                                                  (host, port)))
        while True:
            r, w, x = select.select([sock, chan], [], [])
            if sock in r:
                data = sock.recv(1024)
                if len(data) == 0:
                    break
                chan.send(data)
            if chan in r:
                data = chan.recv(1024)
                if len(data) == 0:
                    break
                sock.send(data)
        chan.close()
        sock.close()
        log(EVENT, 'Tunnel closed from %r' % (chan.origin_addr,))

    def _reverse_forward_tunnel(self, transport):
        forward_host = self._forward_host
        forward_port = self._forward_port
        port = self._ssh_forward_port
        if port is None:
            port = self._DEFAULT_SSH_FORWARD_PORT
        port_limit = port + self._MAX_SSH_FORWARD_ATTEMPTS
        for p in range(port, port_limit):
            try:
                transport.request_port_forward('', p)
                break
            except paramiko.SSHException as e:
                log(EVENT, "Couldn't connect to port %s: %s" % (p, e,))
        else:
            log(OPERATIONAL, "No free port found in the range %s-%s" % (port, port_limit - 1,))
            return
        self._actual_ssh_forward_port = p
        log(EVENT, 'Remote port %d forwarded to %s:%d' %
            (self._actual_ssh_forward_port, forward_host, forward_port,))
        while True:
            chan = transport.accept(1000)
            if chan is None:
                continue
            thread = threading.Thread(target=self._handler, args=(chan, forward_host, forward_port))
            thread.setDaemon(True)
            thread.start()

    def pytis_forward_port(self):
        return self._actual_ssh_forward_port

    def run(self):
        self._actual_ssh_port = None
        # Get parameters
        ssh_host = self._ssh_host
        ssh_port = self._ssh_port
        forward_host = self._forward_host
        forward_port = self._forward_port
        user = self._ssh_user or getpass.getuser()
        key_filename = self._key_filename or os.path.expanduser('~/.ssh/id_rsa')
        # Create client
        client = paramiko.SSHClient()
        client.load_system_host_keys()
        client.set_missing_host_key_policy(paramiko.WarningPolicy())
        # Connect to the ssh host
        log(EVENT, 'Connecting to ssh host %s:%d' % (ssh_host, ssh_port,))
        try:
            client.connect(ssh_host, ssh_port, user, key_filename=key_filename, look_for_keys=True)
        except Exception as e:
            log(OPERATIONAL, 'Failed to connect to %s:%d: %r' % (ssh_host, ssh_port, e,))
            return
        # Forward
        log(EVENT, 'Forwarding remote port %d+ to %s:%d' %
            (self._ssh_forward_port or self._DEFAULT_SSH_FORWARD_PORT, forward_host, forward_port,))
        self._reverse_forward_tunnel(client.get_transport())


# Just for testing:
if __name__ == '__main__':
    args = sys.argv[1:]
    if len(args) < 3 or args[0] != 'rforward':
        sys.stderr.write("usage: " + sys.argv[0] + " rforward SSH-HOST FORWARD-PORT SSH-PORT " +
                         "SSH-USER SSH-FORWARD-PORT FORWARD-HOST KEY-FILENAME\n")
        sys.exit(1)
    args = args[1:]
    def arg(n, default):
        a = args[n:n + 1]
        return a[0] if a else default
    tunnel = ReverseTunnel(args[0], int(args[1]),
                           ssh_port=int(arg(2, '22')),
                           ssh_user=(arg(3, None)),
                           ssh_forward_port=int(arg(4, '10000')),
                           forward_host=(arg(5, 'localhost')),
                           key_filename=(arg(6, None)))
    tunnel.start()
    tunnel.join()
