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
import socket
import threading
import Crypto
import paramiko

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
