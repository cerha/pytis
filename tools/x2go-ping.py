#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Minimal diagnostic tool for testing pytis2go remote communication.

Reads the pytis X2Go info file and tests the connection to the pytis2go
service (RPyC or JSON protocol, auto-detected from the info file).

Run this on the X2Go server inside an active pytis2go session.
"""

from __future__ import print_function

import os
import socket
import sys
import time

try:
    import importlib.util as _importlib_util
    def _load_module(name, path):
        spec = _importlib_util.spec_from_file_location(name, path)
        mod = _importlib_util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        return mod
except ImportError:
    import imp  # Python 2
    def _load_module(name, path):
        return imp.load_source(name, path)

import pytis.remote


def report(message):
    sys.stderr.write(message + '\n')


def _connect_rpyc(password, port):
    import rpyc.utils.authenticators
    connector = pytis.remote.Connector(password)
    try:
        connection = connector.connect('localhost', port)
    except rpyc.utils.authenticators.AuthenticationError:
        report("Authentication failed")
        return None
    except socket.error as e:
        report("Connection error -- tunnel not available? %s" % (e,))
        return None
    except EOFError as e:
        report("Connection error -- RPyC server not connected or invalid password? %s" % (e,))
        return None
    except Exception as e:
        # Catch Python 2/3 RPyC wire-incompatibility (TypeError in vinegar.load)
        report("Connection error -- possible Python version mismatch: %s: %s"
               % (type(e).__name__, e))
        return 'version_mismatch'
    message = "hello client"
    try:
        echoed = connection.root.echo(message)
    except Exception as e:
        report("Echo failed -- possible Python version mismatch: %s: %s"
               % (type(e).__name__, e))
        return 'version_mismatch'
    if echoed != message:
        report("Invalid echo result: %r" % (echoed,))
        return None
    return connection


def _connect_json(password, port):
    client_py = os.path.join(os.path.dirname(os.path.abspath(pytis.remote.__file__)), 'client.py')
    report("Loading JSON client from: %s" % client_py)
    if not os.path.exists(client_py):
        report("ERROR: client.py not found at %s" % client_py)
        report("Make sure the pytis installation on this server includes client.py.")
        return None
    try:
        mod = _load_module('pytis_remote_client', client_py)
    except Exception as e:
        report("ERROR loading client.py: %s: %s" % (type(e).__name__, e))
        return None
    ServiceClient = mod.ServiceClient
    AuthError = mod.AuthError
    RemoteError = mod.RemoteError
    client = ServiceClient(password)
    try:
        client.connect('localhost', port)
    except AuthError as e:
        report("Authentication failed: %s" % e)
        return None
    except socket.error as e:
        report("Connection error -- tunnel not available? %s" % e)
        return None
    try:
        result = client.request('echo', text='hello')
    except RemoteError as e:
        report("Echo request failed: %s" % e)
        return None
    if result != 'hello':
        report("Invalid echo result: %r" % (result,))
        return None
    return client


def _read_access_data(info_file):
    try:
        pytis.remote.keep_x2go_info_file()
        return pytis.remote.parse_x2go_info_file(info_file)
    except pytis.remote.X2GoInfoException as e:
        report("Exception when parsing P2Go info file: %s" % (e.args,))
        return None


def main():
    session_id = pytis.remote.x2go_session_id()
    if not session_id:
        report("X2GO_SESSION environment variable not set.")
        return
    info_file = pytis.remote.pytis_x2go_info_file(session_id)
    if not os.path.exists(info_file):
        report("P2Go info file not present: %s" % info_file)
        return
    if not os.access(info_file, os.R_OK):
        report("P2Go info file not readable: %s" % info_file)
        return
    with open(info_file) as f:
        raw = f.read()
    report("Info file: %s" % info_file)
    report("Info file content (raw): %r" % raw)
    access_data = _read_access_data(info_file)
    if access_data is None:
        return
    protocol = access_data.get('protocol', 'rpyc')
    port = access_data['port']
    password = access_data['password']
    report("Protocol: %s  Port: %s" % (protocol, port))

    if protocol == 'rpyc':
        # Signal our Python version so pytis2go can (re)start the RPyC service
        # with the matching Python executable.
        pytis.remote.write_python_version()
        report("Python version %d written." % sys.version_info[0])

    if protocol == 'json':
        conn = _connect_json(password, port)
    else:
        conn = _connect_rpyc(password, port)
        if conn == 'version_mismatch':
            # The service is running a different Python version.  Pytis2go polls
            # the python-version file every second and will restart the RPyC
            # subprocess.  Wait and then retry with fresh credentials from the
            # info file (the port will have changed after the restart).
            report("Python version mismatch detected.  Waiting up to 30 seconds "
                   "for pytis2go to restart the RPyC service...")
            deadline = time.time() + 30
            conn = None
            while time.time() < deadline:
                time.sleep(2)
                access_data = _read_access_data(info_file)
                if access_data is None:
                    continue
                new_port = access_data['port']
                new_password = access_data['password']
                if new_port == port:
                    continue  # Service not restarted yet
                report("Service restarted on port %s, retrying..." % new_port)
                conn = _connect_rpyc(new_password, new_port)
                if conn != 'version_mismatch' and conn is not None:
                    break
                port = new_port
                password = new_password
            if conn is None or conn == 'version_mismatch':
                report("Timed out waiting for service restart.")
                return

    if conn is not None:
        report("Connection OK!")


if __name__ == '__main__':
    main()
