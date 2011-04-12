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

import rpyc

import config

class ProxyService(rpyc.Service):

    def exposed_request(self, target_ip, request, *args, **kwargs):
        connection = rpyc.ssl_connect(target_ip, config.rpc_remote_port,
                                      keyfile=config.rpc_key_file,
                                      certfile=config.rpc_certificate_file)
        return getattr(connection.root, request)(*args, **kwargs)

def run_proxy():
    from rpyc.utils.server import ThreadedServer
    t = ThreadedServer(ProxyService, hostname='localhost', port=config.rpc_local_port)
    t.start()
