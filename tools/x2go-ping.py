#!/usr/bin/env python

import os
import rpyc.utils.authenticators
import socket
import sys

sys.path.append(os.path.normpath(os.path.join(sys.path[0], '..', 'lib')))

import pytis.remote


def report(message):
    sys.stderr.write(message + '\n')


def main():
    session_id = pytis.remote.x2go_session_id()
    if not session_id:
        report("X2GO_SESSION environment variable not set.")
        return
    info_file = pytis.remote.pytis_x2go_info_file(session_id)
    if not os.path.exists(info_file):
        report("P2Go info file not present: %s" % (info_file,))
        return
    if not os.access(info_file, os.R_OK):
        report("P2Go info file not readable: %s" % (info_file,))
        return
    try:
        access_data = pytis.remote.parse_x2go_info_file(info_file)
    except pytis.remote.X2GoInfoException as e:
        report("Exception when parsing P2Go info file: %s" % (e.args,))
        return
    connector = pytis.remote.Connector(access_data['password'])
    try:
        connection = connector.connect('localhost', access_data['port'])
    except rpyc.utils.authenticators.AuthenticationError:
        report("Authentication failed")
        return
    except socket.error as e:
        report("Connection error -- tunnel not available? %s" % (e,))
        return
    except EOFError as e:
        report("Connection error -- RPyC server not connected or invalid password? %s" % (e,))
        return
    message = "hello client"
    echoed_message = connection.root.echo(message)
    if echoed_message != message:
        report("Invalid echo result: %s" % (echoed_message,))
        return
    report("Connection OK!")

if __name__ == '__main__':
    main()
