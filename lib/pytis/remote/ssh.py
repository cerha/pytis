#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2014, 2015, 2017 Brailcom, o.p.s.
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

import copy
import getpass
import os
import select
import socket
import sys
import threading

import Crypto
import gevent
import gevent.event
import gevent.subprocess
import paramiko

from pytis.util import log, EVENT, OPERATIONAL


class AuthHandler(paramiko.auth_handler.AuthHandler):

    def __init__(self, transport):
        super(AuthHandler, self).__init__(transport)
        self._public_key = None
        self.acceptable_public_keys = []

    def check_publickey(self, username, key, event):
        self.transport.lock.acquire()
        try:
            self.auth_event = event
            self.auth_method = 'publickey'
            self.username = username
            self._public_key = key
            self._request_auth()
        finally:
            self.transport.lock.release()

    def _parse_service_accept(self, m):
        service = m.get_text()
        if service != 'ssh-userauth' or self.auth_method != 'publickey' or self._public_key is None:
            super(AuthHandler, self)._parse_service_accept(m)
        else:
            m = paramiko.message.Message()
            m.add_byte(paramiko.common.cMSG_USERAUTH_REQUEST)
            m.add_string(self.username)
            m.add_string('ssh-connection')
            m.add_string(self.auth_method)
            m.add_boolean(False)
            m.add_string(self._public_key.get_name())
            m.add_string(self._public_key.asbytes())
            self.transport._send_message(m)

    def _parse_userauth_pk_ok(self, m):
        self._public_key = None
        algorithm = m.get_string()
        blob = m.get_string()
        self.acceptable_public_keys.append((algorithm, blob,))
        m = paramiko.message.Message()
        m.add_byte(paramiko.common.cMSG_USERAUTH_REQUEST)
        m.add_string(self.username)
        m.add_string('ssh-connection')
        m.add_string('none')
        self.transport._send_message(m)

    _handler_table = copy.copy(paramiko.auth_handler.AuthHandler._handler_table)
    _handler_table[paramiko.common.MSG_SERVICE_ACCEPT] = _parse_service_accept
    _handler_table[paramiko.common.MSG_USERAUTH_PK_OK] = _parse_userauth_pk_ok

class Transport(paramiko.Transport):

    def public_key_acceptable(self, username, key):
        if (not self.active) or (not self.initial_kex_done):
            raise paramiko.ssh_exception.SSHException('No existing session')
        event = threading.Event()
        self.auth_handler = AuthHandler(self)
        self.auth_handler.check_publickey(username, key, event)
        try:
            result = self.auth_handler.wait_for_response(event)
        except paramiko.ssh_exception.AuthenticationException:
            result = (key.get_name(), key.asbytes(),) in self.auth_handler.acceptable_public_keys
        return result

def read_public_key_file(filename):
    data = open(filename, 'r').read()
    if data.startswith('ssh-dss '):
        # This is probably a public OpenSSH key
        import binascii
        import struct
        keystring = binascii.a2b_base64(data.split(' ')[1])
        keyparts = []
        while len(keystring) > 4:
            length = struct.unpack(">I", keystring[:4])[0]
            keyparts.append(keystring[4:4 + length])
            keystring = keystring[4 + length:]
        if keyparts[0] == 'ssh-dss':
            tup = [Crypto.Util.number.bytes_to_long(keyparts[x]) for x in (4, 3, 1, 2)]
            pubk = Crypto.PublicKey.DSA.construct(tup)
            p = pubk.key.p
            q = pubk.key.q
            g = pubk.key.g
            y = pubk.key.y
            vals = (p, q, g, y)
            return paramiko.DSSKey(vals=vals)
        return None
    elif data.startswith('ssh-rsa '):
        key = Crypto.PublicKey.RSA.importKey(data)
        return paramiko.RSAKey(vals=(key.e, key.n))
    else:
        return None

def public_key_acceptable(hostname, username, key_filename, port=22):
    """Return true iff the given public key is accepted by the server.

    Arguments:

      hostname -- ssh server to connect to; string
      username -- user on the server; string
      key_filename -- name of the public key file; string
      port -- ssh server port; integer

    """
    s = socket.socket()
    s.connect((hostname, port,))
    public_key = read_public_key_file(key_filename)
    if public_key is None:
        raise Exception("Couldn't parse public key file", key_filename)
    transport = Transport(s)
    transport.start_client()
    result = transport.public_key_acceptable(username, public_key)
    transport.close()
    s.close()
    return result

def ssh_connect(server, add_to_known_hosts=False, **kwargs):
    """Establish SSH connection according to given parameters.

    Arguments:
      server -- remote server host name (string)
      port -- remote server port (int)
      username -- remote user name (string)
      password -- remote user password or passphrase (if key_filename is not
        None) (string)
      key_filename -- name of the SSH private key file (string)
      allow_agent -- true if SSH agent should be allowed (bool)
      gss_auth -- true if GSS (Kerberos) authenticationexception should be
        allowed (bool)
      add_to_known_hosts -- Iff true, server keys not already present in known
        hosts will be automatically added.

    Returns 'paramiko.SSHClient' instance if successfully connected or None if
    connection fails.

    """
    client = paramiko.SSHClient()
    client.load_system_host_keys()
    if add_to_known_hosts:
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
    try:
        client.connect(server, **kwargs)
    except (paramiko.ssh_exception.AuthenticationException,
            paramiko.ssh_exception.SSHException,  # Happens on GSS auth failure.
            ImportError):  # Happens on GSS auth attempt with GSS libs uninstalled.
        return None
    return client

def ssh_exec(command, hostname):
    """Execute given 'command' on 'hostname' using X11 forwarding.

    Arguments:

      command -- command name and its arguments; list of strings
      hostname -- host to execute the given command on; basestring

    """
    # Paramiko doesn't handle X11 forwarding very well, so it's much easier to
    # use just subprocess here.
    return gevent.subprocess.Popen(['ssh', '-X', hostname] + command)


class ReverseTunnel(gevent.Greenlet):
    """Process running reverse ssh forward connection.

    Parameters are given in the constructor.  The process can be started using
    'start()' method.

    """
    _DEFAULT_SSH_FORWARD_PORT = 10000
    _MAX_SSH_FORWARD_ATTEMPTS = 100

    def __init__(self, ssh_host, forward_port, ssh_port=22, ssh_user=None,
                 ssh_forward_port=None, forward_host='localhost', key_filename=None,
                 ssh_password=None, ssh_forward_port_result=None, gss_auth=False,
                 strict_forward_port=False):
        """
        Arguments:

          ssh_host -- ssh host to connect to; basestring; the starting
            point of the tunnel will be established there
          forward_port -- port to forward the connection to; integer; this is
            the target point of the tunnel
          ssh_port -- ssh port on 'ssh_host' to connect to; integer
          ssh_user -- ssh user on 'ssh_host' to connect to; basestring or
            'None in which case the current user is used
          ssh_password -- ssh password for 'ssh_user' on 'ssh_host'; basestring
            or 'None'
          ssh_forward_port -- forwarding port on the 'ssh_host' to bind to;
            integer (if None or if the value is 0 then an arbitrary free port
            is selected); this is the starting point of the tunnel
          forward_host -- host to forward the connection to; basestring;
            the target point of the tunnel will be there
          key_filename -- name of the file containing the ssh key to use for
            connection to 'ssh_host'; basestring or 'None' in which case
            '~/.ssh/id_rsa' is used
          ssh_forward_port_result -- optional instance providing 'set' method
            to put the actual ssh forward port value (integer) into
          gss_auth -- whether to use GSS-API authentication; boolean
          strict_forward_port -- iff True, use only the given ssh_forward_port
            number; if it is not free then don't attempt to find another port
            and raise 'paramiko.SSHException' instead

        """
        super(ReverseTunnel, self).__init__()
        self._ssh_host = ssh_host
        self._forward_port = forward_port
        self._ssh_port = ssh_port
        self._ssh_user = ssh_user
        self._ssh_password = ssh_password
        self._ssh_forward_port = ssh_forward_port
        self._forward_host = forward_host
        self._key_filename = key_filename
        self._actual_ssh_forward_port = ssh_forward_port_result
        self._gss_auth = gss_auth
        self._strict_forward_port = strict_forward_port
        self._ssh_client = None

    def _handler(self, chan, host, port):
        sock = socket.socket()
        sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        try:
            sock.connect((host, port))
        except Exception as e:
            log(OPERATIONAL, 'Forwarding request to %s:%d failed: %r' % (host, port, e))
            return
        log(EVENT, 'Tunnel open %r -> %r -> %r' % (chan.origin_addr, chan.getpeername(),
                                                   (host, port)))
        while True:
            r, w, x = select.select([sock, chan], [], [])
            try:
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
            except Exception as e:
                log(OPERATIONAL,
                    'Reverse tunnel {} encountered socket error: {}'.format(
                        chan, str(e)))
        chan.close()
        sock.close()
        log(EVENT, 'Tunnel closed from %r' % (chan.origin_addr,))

    def _reverse_forward_tunnel(self, transport):
        forward_host = self._forward_host
        forward_port = self._forward_port
        port = self._ssh_forward_port
        if not port:
            port = self._DEFAULT_SSH_FORWARD_PORT
        n_attempts = 1 if self._strict_forward_port else self._MAX_SSH_FORWARD_ATTEMPTS
        port_limit = port + n_attempts
        for p in range(port, port_limit):
            try:
                transport.request_port_forward('', p)
                break
            except paramiko.SSHException as e:
                log(EVENT, "Couldn't connect to port %s: %s" % (p, e,))
        else:
            message = "No free port found in the range %s-%s" % (port, port_limit - 1,)
            log(OPERATIONAL, message)
            raise paramiko.SSHException(message)
        if self._actual_ssh_forward_port is not None:
            self._actual_ssh_forward_port.set(p)
        log(EVENT, 'Remote port %d forwarded to %s:%d' % (p, forward_host, forward_port,))
        while True:
            chan = transport.accept(1000)
            if chan is None:
                continue
            gevent.spawn(self._handler, chan, forward_host, forward_port)

    def run(self):
        # Get parameters
        ssh_host = self._ssh_host
        ssh_port = self._ssh_port
        forward_host = self._forward_host
        forward_port = self._forward_port
        user = self._ssh_user or getpass.getuser()
        password = self._ssh_password
        key_filename = self._key_filename
        if key_filename is None and not os.getenv('SSH_AGENT_PID'):
            key_filename = os.path.expanduser('~/.ssh/id_rsa')
            if not os.path.exists(key_filename):
                key_filename = None
        # Create client
        self._ssh_client = client = paramiko.SSHClient()
        client.load_system_host_keys()
        client.set_missing_host_key_policy(paramiko.WarningPolicy())
        # Connect to the ssh host
        log(EVENT, 'Connecting to ssh host %s:%d' % (ssh_host, ssh_port,))
        try:
            client.connect(hostname=ssh_host, port=ssh_port, username=user,
                           key_filename=key_filename, password=password, gss_auth=self._gss_auth)
        except Exception as e:
            log(OPERATIONAL, 'Failed to connect to %s@%s:%d: %r' % (user, ssh_host, ssh_port, e,))
            return
        # Forward
        log(EVENT, 'Forwarding remote port %d+ to %s:%d' %
            (self._ssh_forward_port or self._DEFAULT_SSH_FORWARD_PORT, forward_host, forward_port,))
        self._reverse_forward_tunnel(client.get_transport())

    def kill(self):
        if self._ssh_client:
            while True:
                transport = self._ssh_client.get_transport()
                active = transport.is_active()
                if active:
                    transport.cancel_port_forward(self._forward_host, self._forward_port)
                    transport.close()
                else:
                    self._ssh_client.close()
                    self._ssh_client = None
                    break
        super(ReverseTunnel, self).kill()

    def _run(self):
        return self.run()


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
    import gevent.monkey
    gevent.monkey.patch_all()
    forward_port = gevent.event.AsyncResult()
    def forward_port_callback(value):
        print "ssh forward port:", value.get()
    forward_port.rawlink(forward_port_callback)
    tunnel = ReverseTunnel(args[0], int(args[1]),
                           ssh_port=int(arg(2, '22')),
                           ssh_user=(arg(3, None)),
                           ssh_forward_port=int(arg(4, '10000')),
                           forward_host=(arg(5, 'localhost')),
                           key_filename=(arg(6, None)),
                           ssh_forward_port_result=forward_port)
    tunnel.start()
    tunnel.join()
