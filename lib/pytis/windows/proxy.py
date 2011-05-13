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

import logging
import rpyc
import rpyc.utils.server

import config

logging_level = logging.INFO

class ProxyException(Exception):
    pass

class ProxyService(rpyc.Service):

    def __init__(self, *args, **kwargs):
        rpyc.Service.__init__(self, *args, **kwargs)
        self._connection = None

    def exposed_request(self, target_ip, user_name, request, *args, **kwargs):
        try:
            getattr(self._connection.root, 'echo')
        except:
            self._connection = rpyc.ssl_connect(target_ip, config.rpc_remote_port,
                                                keyfile=config.rpc_key_file,
                                                certfile=config.rpc_certificate_file)
        if user_name is None:
            port = config.rpc_remote_port
        else:
            port = getattr(self._connection.root, 'user_port')(user_name)
            if port is None:
                raise ProxyException("User server unavailable", user_name)
        return getattr(self._connection.root, request)(*args, **kwargs)

class ProxyThreadedServer(rpyc.utils.server.ThreadedServer):

    def _get_logger(self):
        logger = super(ProxyThreadedServer, self)._get_logger()
        logger.setLevel(logging_level)
        handler = logging.StreamHandler()
        logger.addHandler(handler)
        return logger

def run_proxy():
    t = ProxyThreadedServer(ProxyService, hostname='localhost', port=config.rpc_local_port)
    t.start()
