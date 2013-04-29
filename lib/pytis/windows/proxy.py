# -*- coding: utf-8 -*-

# Copyright (C) 2011 Brailcom, o.p.s.
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

from pytis.util import translate as _

import logging
import rpyc
import rpyc.utils.server

import pytis.util
import config

logging_level = logging.INFO

class ProxyException(Exception):
    pass

class ProxyService(rpyc.Service):

    def __init__(self, *args, **kwargs):
        rpyc.Service.__init__(self, *args, **kwargs)
        self._connections = {}

    def _new_pytis_connection(self, ip, port):
        pytis.util.log(pytis.util.EVENT, 'New windows connection requested:', (ip, port,))
        connection = rpyc.ssl_connect(ip, port, keyfile=config.rpc_key_file,
                                      certfile=config.rpc_certificate_file)
        pytis.util.log(pytis.util.EVENT, 'New windows connection created:', (ip, port,))
        return connection

    def exposed_request(self, target_ip, user_name, request, *args, **kwargs):
        master_port = config.rpc_remote_port
        master_connection = self._connections.get(target_ip, master_port)
        if user_name is None:
            connection = master_connection
            port = master_port
        else:
            try:
                port = getattr(master_connection.root, 'user_port')(user_name)
            except:
                master_connection = self._connections[master_port] = \
                  self._new_pytis_connection(target_ip, master_port)
                port = getattr(master_connection.root, 'user_port')(user_name)
                if port is None:
                    raise ProxyException("User server unavailable", user_name)
            connection = self._connections.get(port)
        try:
            remote_method = getattr(connection.root, request)
        except:
            connection = self._connections[port] = self._new_pytis_connection(target_ip, port)
            remote_method = getattr(connection.root, request)
        return remote_method(*args, **kwargs)

class ProxyThreadedServer(rpyc.utils.server.ThreadedServer):

    def _get_logger(self):
        logger = super(ProxyThreadedServer, self)._get_logger()
        logger.setLevel(logging_level)
        handler = logging.StreamHandler()
        logger.addHandler(handler)
        return logger

def run_proxy():
    t = ProxyThreadedServer(ProxyService, hostname='localhost', port=config.rpc_local_port,
                            auto_register=False)
    t.start()
