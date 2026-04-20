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

import pytis.remote


def report(message):
    sys.stderr.write(message + '\n')


def connect():
    try:
        pytis.remote.connect()
    except socket.error as e:
        report("Connection error (tunnel not available?): %s" % e)
        return False
    except EOFError as e:
        report("Connection error (server not connected or invalid password?): %s" % e)
        return False
    except ValueError as e:
        report("Connection failed (possible Python version mismatch?): %s" % e)
        return False
    except Exception as e:
        # Catch rpyc.utils.authenticators.AuthenticationError and others by name (avoid import).
        if type(e).__name__ == 'AuthenticationError':
            report("Authentication failed: %s" % e)
        else:
            report("Connection failed (%s): %s" % (type(e).__name__, e))
        return False
    else:
        return True


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
    pytis.remote.keep_x2go_info_file()
    try:
        access_data = pytis.remote.parse_x2go_info_file(info_file)
    except pytis.remote.X2GoInfoException as e:
        report("Exception when parsing P2Go info file: %s" % (e.args,))
        return
    if not access_data:
        report("Parsing P2Go info file returned an empty result.")
        return
    report("Protocol: %s  Port: %s" % (access_data['protocol'], access_data['port']))
    if connect():
        report("Connection OK!")
        return

    if access_data['protocol'] == 'rpyc':
        # Signal our Python version so pytis2go can (re)start the RPyC service
        # with the matching executable, then wait for the restart.
        pytis.remote.write_python_version()
        report("Python version %d written." % sys.version_info[0])
        # Poll for new credentials (pytis2go writes them when the new service is
        # up) rather than hammering the service on every retry.
        report("Waiting for service restart (up to 30 s)...")
        deadline = time.time() + 30
        while time.time() < deadline:
            time.sleep(2)
            try:
                new_data = pytis.remote.parse_x2go_info_file(info_file)
            except pytis.remote.X2GoInfoException:
                continue
            if not new_data or new_data['password'] == access_data['password']:
                continue
            report("Service restarted on port %s, retrying..." % new_data['port'])
            if connect():
                report("Connection OK!")
                return
        report("Timed out waiting for service restart.")


if __name__ == '__main__':
    main()
