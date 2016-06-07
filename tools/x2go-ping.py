#!/usr/bin/env python

import os
import rpyc.utils.authenticators
import socket
import sys

import pytis.remote
import pytis.remote.pytisproc as pytisproc

def report(message):
    sys.stderr.write(message + '\n')
    
def main():
    session_id = pytis.remote.x2go_session_id()
    if not session_id:
        report("X2GO_SESSION environment variable not set.")
        return
    pytis_x2go_file = pytis.remote.pytis_x2go_info_file(session_id)
    if not os.path.exists(pytis_x2go_file):
        report("Pytis configuration file not present: %s" % (pytis_x2go_file,))
        return
    if not os.access(pytis_x2go_file, os.R_OK):
        report("Pytis configuration file not readable: %s" % (pytis_x2go_file,))
        return
    try:
        access_data = pytis.remote.parse_x2go_info_file(pytis_x2go_file)
    except pytis.remote.X2GoInfoException as e:
        report("Exception when parsing pytis info file: %s" % (e.args,))
        return
    port = access_data['port']
    password = access_data['password']
    authenticator = pytisproc.PasswordAuthenticator(password)
    try:
        connection = authenticator.connect('localhost', port)
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
