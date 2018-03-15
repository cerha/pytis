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

"""Running X2Go services in a subprocess.

This module hides the details of running X2Go client, X2Go broker and XServer
in a subprocess behid simple APIs which are similar to the APIs of the original
classes and feel like they are run in-process.

All these classes are designed to be used from
'pytis.x2goclient.startapp.X2GoStartApp'.

Running in a subprocess is necessary because x2go (and thus
'pytis.x2goclient.x2goclient') can not be imported in the process where the
startup wx application runs.  It causes conflicts between the ws and glib main
loop and gevent monkey patching.

"""

import os
import re
import sys
import rpyc
import time
import base64
import socket
import hashlib
import subprocess
import cPickle as pickle

_pytislib = os.path.normpath(os.path.join(sys.path[0], '..', 'lib'))
_pythonpath = os.environ.get('PYTHONPATH', '')
if not _pythonpath:
    _pythonpath = _pytislib
elif _pytislib not in _pythonpath.split(os.pathsep):
    _pythonpath += os.pathsep + _pytislib

class ClientService(rpyc.Service):
    """RPyC Service exposing the public API of X2GoClient to the parent process.

    This class is not meant to be used directly.  Use 'ClientProcess' instead.

    """
    class Authenticator(object):
        def __init__(self, key):
            self._key = key

        def __call__(self, sock):
            if sock.recv(len(self._key)) != self._key:
                raise rpyc.utils.authenticators.AuthenticationError("wrong magic word")
            return sock, None

        def connect(self, host, port):
            conn = rpyc.connect(host, port, config=dict(allow_pickle=True,
                                                        allow_public_attrs=True))
            if hasattr(socket, 'fromfd'):
                fd = conn.fileno()
                sock = socket.fromfd(fd, socket.AF_INET, socket.SOCK_STREAM)
            else:
                # There is no socket.fromfd in Python 2.x on Windows, so let's use
                # the original hidden socket.
                sock = conn._channel.stream.sock
            sock.send(self._key)
            return conn

    def exposed_connect(self, connection_parameters, on_echo=None):
        from pytis.x2goclient.x2goclient import X2GoClient
        self._client = X2GoClient(connection_parameters,
                                  on_rpyc_echo=rpyc.async(on_echo) if on_echo else None)

    def exposed_list_sessions(self):
        return self._client.list_sessions()

    def exposed_terminate_session(self, session):
        self._client.terminate_session(session)

    def exposed_resume_session(self, session):
        self._client.resume_session(session)

    def exposed_start_new_session(self):
        self._client.start_new_session()

    def exposed_main_loop(self):
        self._client.main_loop()


class ClientProcess(object):
    """Run Pytis X2Go client in a separate process and control it from the current Python process.

    This class invokes a new process and runs a Pytis X2Go client inside it.
    The running 'X2GoClient' instance inside this process may be controlled
    through the public methods of this class's instance.

    This class hides the details of spawning a new Python process, connecting
    and authenticating to its RPyC service and working with its RPyC interface
    behind a simple API.  The 'X2GoClient' run's in another process, but its
    public methods may be called as if the client was running locally.

    """

    def __init__(self, session_parameters, on_echo=None):
        """Start a Python subprocess running 'X2GoClient' and 'ClientService' server.

        Arguments:
          session_parameters -- X2Go session parameters as a dictionary
          on_echo -- callback to run on each 'PytisService' echo call.  A Pytis
            application running on the X2Go server calls echo on startup and
            then every few minutes, so the callback may be used to detect
            a running Pytis application.

        """
        self._process = subprocess.Popen((sys.executable, '-m', 'pytis.x2goclient.runclient'),
                                         env=dict(os.environ.copy(), PYTHONPATH=_pythonpath),
                                         stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        # Generate a secret token and pass it to the subprocess through
        # its STDIN and read the port number from its STDOUT (it will
        # allocate the first available port number and print it.
        key = hashlib.sha256(os.urandom(16)).hexdigest()
        self._process.stdin.write(key + '\n')
        port = int(self._process.stdout.readline())
        # Wait for the RPyC server inside the subprocess to come up.
        time.sleep(3)
        authenticator = ClientService.Authenticator(key)
        self._conn = authenticator.connect('localhost', port=port)
        # Start a new X2GoClient instance inside the subprocess.
        self._conn.root.connect(session_parameters, on_echo=on_echo)

    def list_sessions(self):
        """Return the result of 'X2GoClient.list_sessions()' on the subprocess instance."""
        return self._conn.root.list_sessions()

    def terminate_session(self, session):
        """Call 'X2GoClient.terminate_session()' on the subprocess instance."""
        self._conn.root.terminate_session(session)

    def resume_session(self, session):
        """Call 'X2GoClient.resume_session()' on the subprocess instance."""
        self._conn.root.resume_session(session)

    def start_new_session(self):
        """Call 'X2GoClient.start_new_session()' on the subprocess instance."""
        self._conn.root.start_new_session()

    def main_loop(self):
        """Call 'X2GoClient.main_loop()' on the subprocess instance."""
        self._conn.root.main_loop()

    def terminate(self):
        """Terminate the subprocess and the 'X2GoClient' instance running inside it."""
        self._process.terminate()


class Broker(object):
    """Higher level X2Go broker interface.

    This class queries 'PytisSshProfiles' in a subprocess and provides a few
    additional public methods to simplify working with broker from the startup
    application.

    """
    _DEFAULT_SSH_PORT = 22
    _URL_MATCHER = re.compile('^(?P<protocol>(ssh|http(|s)))://'
                              '(|(?P<username>[a-zA-Z0-9_\.-]+)'
                              '(|:(?P<password>.*))@)'
                              '(?P<server>[a-zA-Z0-9\.-]+)'
                              '(|:(?P<port>[0-9]+))'
                              '($|(?P<path>/.*)$)')

    def __init__(self, url, password=None):
        parameters, path = self._split_url(url)
        self._connection_parameters = parameters
        self._path = path
        self._password = password
        self._upgrade_parameters = None

    def _split_url(self, url):
        match = self._URL_MATCHER.match(url)
        if not match:
            raise Exception("Invalid URL: %s", url)
        parameters = match.groupdict()
        parameters['port'] = int(parameters['port'] or self._DEFAULT_SSH_PORT)
        if parameters.pop('protocol', None) not in (None, 'ssh'):
            raise Exception("Unsupported broker protocol: %s" % url)
        path = parameters.pop('path')
        return parameters, path

    def _list_profiles(self, connection_parameters):
        kwargs = dict(
            connection_parameters=connection_parameters,
            broker_path=self._path,
            broker_password=self._password,
        )
        process = subprocess.Popen((sys.executable, '-m', 'pytis.x2goclient.runbroker'),
                                   env=dict(os.environ.copy(), PYTHONPATH=_pythonpath),
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        # Serialize kwargs and pass them to the subprocess through its STDIN.
        process.stdin.write(base64.b64encode(pickle.dumps(kwargs)) + '\n')
        response = process.stdout.readline().strip()
        if response == 'Connection failed':
            return None
        else:
            profiles, upgrade_parameters = pickle.loads(base64.b64decode(response))
            self._connection_parameters.update(connection_parameters)
            self._upgrade_parameters = upgrade_parameters
            return profiles

    def list_profiles(self, authenticate):
        """Return a list of two-tuples (profile_id, session_parameters).

        The list is sorted by profile name.  'session_parameters' is a
        dictionary of X2Go session parameters.

        """
        return authenticate(self._list_profiles, self._connection_parameters)

    def server(self):
        """Return broker's server hostname as a string."""
        return self._connection_parameters['server']

    def port(self):
        """Return broker's server port number as int."""
        return self._connection_parameters['port']

    def username(self):
        """Return broker authentication username as a string."""
        return self._connection_parameters['username']

    def url(self):
        """Return broker URL as a string."""
        params = self._connection_parameters
        return "ssh://%s%s@%s%s/%s" % (
            params['username'],
            ':' + params['password'] if params['password'] else '',
            params['server'],
            ':' + params['port'] if params['port'] != self._DEFAULT_SSH_PORT else '',
            self._path,
        )

    def upgrade_parameters(self):
        """Return Pytis upgrade parameters as (version, connection_parameters, path).

        'version' is the available upgrade version as a string,
        'connection_parameters' is a dictionary of upgrade server connection parameters,
        'path' is the upgrade server path.

        Must be called after 'list_profiles()' and only if 'list_profiles()'
        doesn't return None.  Otherwise the behavior is undefined.

        """
        version, url = self._upgrade_parameters
        if url:
            connection_parameters, path = self._split_url(url)
            return version, connection_parameters, path
        else:
            return version, None, None


class XServer(object):
    """Run X-server in a separate process.

    The constructor simply runs 'python -m pytis.x2goclient.xserver' to start a
    local X11 server in a subprocess.

    """

    def __init__(self):
        subprocess.Popen((sys.executable, '-m', 'pytis.x2goclient.xserver'),
                         env=dict(os.environ.copy(), PYTHONPATH=_pythonpath))
