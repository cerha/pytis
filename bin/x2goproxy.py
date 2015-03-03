#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2015 Brailcom, o.p.s.
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

import gevent.monkey
gevent.monkey.patch_all()

import imp
import os
import sys

import gevent
import paramiko

import pytis.remote


class Application(object):

    def __init__(self, config_file):
        self._config_file = config_file
        self._read_application_configuration()

    def _read_application_configuration(self):
        configuration = imp.load_source('_config', self._config_file)
        confdict = configuration.__dict__
        self._application_command = confdict['command']
        self._connection_parameters = dict([c for c in confdict.items() if c[0] in ('hostname',)])
        del sys.modules['_config']

    def _run_application(self):
        session_id = pytis.remote.x2go_session_id(fake=True)
        command = [self._application_command, '--session-id', session_id]
        return pytis.remote.ssh_exec(command, **self._connection_parameters)

    def _run_tunnel(self):
        parameters = self._connection_parameters
        hostname = parameters.get('hostname')
        username = parameters.get('username')
        password = parameters.get('password')
        key_filename = parameters.get('key_filename')
        gss_auth = parameters.get('gss_auth')
        pytis_x2go_file = pytis.remote.pytis_x2go_info_file()
        while True:
            access_data = pytis.remote.read_x2go_info_file(rename=True, use_defaults=False)
            if access_data is None:
                gevent.sleep(1)
            else:
                break
        while True:
            updated_access_data = pytis.remote.read_x2go_info_file(rename=True, use_defaults=False)
            if updated_access_data is not None:
                access_data = updated_access_data
            port = access_data['port']
            try:
                tunnel = pytis.remote.ReverseTunnel(hostname,
                                                    port, ssh_forward_port=port,
                                                    strict_forward_port=True,
                                                    ssh_user=username,
                                                    ssh_password=password,
                                                    key_filename=key_filename,
                                                    gss_auth=gss_auth)
            except paramiko.SSHException:
                gevent.sleep(5)
                continue
            tunnel.start()
            while True:
                gevent.sleep(1)
                if tunnel.ready():
                    break
                if os.path.exists(pytis_x2go_file):
                    tunnel.kill()
                    break
                
    def run(self):
        gevent.spawn(self._run_tunnel)
        application = self._run_application()
        while True:
            gevent.sleep(1)
            if application.poll():
                break

def main():
    if len(sys.argv) != 2:
        sys.stderr.write("usage: %s CONFIG-FILE\n" % (sys.argv[0],))
        sys.exit(1)
    config_file = sys.argv[1]
    application = Application(config_file)
    application.run()

if __name__ == '__main__':
    main()
