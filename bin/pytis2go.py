#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011-2018 Brailcom, o.p.s.
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

import os
import sys
import rpyc
import socket
import argparse
import platform
import threading
import rpyc.utils.server

if platform.system() == 'Windows':
    # Windows setup for locales (must precede imports of our libraries containing translations).
    reload(sys)
    sys.setdefaultencoding('cp1250')
    # Set locale language
    import ctypes
    lcid = ctypes.windll.kernel32.GetUserDefaultLCID()
    if not lcid:
        lcid = ctypes.windll.kernel32.GetSystemDefaultLCID()
    import locale
    os.environ['LANGUAGE'] = locale.windows_locale.get(lcid)

pytislib = os.path.normpath(os.path.join(sys.path[0], '..', 'lib'))
if os.path.isdir(pytislib) and pytislib not in sys.path:
    sys.path.append(pytislib)


def main():
    parser = argparse.ArgumentParser(
        description='X2Go Pytis client service.',
        # formatter_class=argparse.RawDescriptionHelpFormatter,
        add_help=True, argument_default=None
    )
    options = (
        (('--version', '-v'), {'default': False, 'action': 'store_true'},
         "Print version number and exit."),
        (('--broker-url', '-b'), {},
         "Retrieve session profiles via an X2Go Session Broker under the given URL."),
        (('--autoload', '-a'), {'default': False, 'action': 'store_true'},
         "Automatically load profiles (connect to broker) on astartup."),
        # (('--list-profiles', '-l'), {'default': False, 'action': 'store_true'},
        #  'List available session profiles and exit.'),
        (('--profile', '-p'), {},
         "Use given profile to start a session."),
        (('--username', '-u'), {},
         "Username for broker and session authentication (default: current user)."),
        # (('--broker-username', '-U'), {},
        #  ("Username for broker authentication if different from session username "
        #  "(default: --username)."),
        # (('--password',), {},
        #  'User password for session authentication'),
        (('--broker-password',), {},
         "Password for authenticating against the X2Go Session Broker"),
        # (('-k', '--ssh-privkey'), {},
        #  ("Use file \'SSH_PRIVKEY\' as private key for the "
        #   "SSH connection (e.g. ~/.ssh/id_rsa)")),
        (('--add-to-known-hosts', '-s'), {'default': False, 'action': 'store_true'},
         ("Add RSA host key fingerprint to ~/.ssh/known_hosts "
          "if authenticity of server can\'t be established (default: not set)")),
        (('--window-title', '-t'), {'default': None},
         "Override startup application progress window title (default: Pytis2Go)"),
        (('--port',), {'default': 56789},
         "Port to check for a running instance/starting this instance's Pytis2Go service)"),
        (('--no-agent-authentication', '-A'), {'action': 'store_true'},
         "Disable trying SSH Agent authentication."),
        (('--no-kerberos-authentication', '-K'), {'action': 'store_true'},
         "Disable trying Kerberos authentication."),
    )
    for args, kwargs, doc in options:
        parser.add_argument(*args, **dict(kwargs, help=doc))

    args = parser.parse_args()

    if args.version:
        import pytis.x2goclient
        sys.stderr.write("%s\n" % (pytis.x2goclient.X2GOCLIENT_VERSION,))
        sys.exit(0)

    from pytis.x2goclient.startapp import Pytis2GoApp

    class Service(rpyc.Service):
        app = None
        def exposed_start_session(self, profile_id):
            Service.app.start_session(profile_id)

    try:
        server = rpyc.utils.server.ThreadedServer(Service, 'localhost', port=args.port)
        threading.Thread(target=server.start).start()
    except socket.error:
        sys.stderr.write("Found a running Pytis2Go instance on port %s, switching to client mode\n"
                         % args.port)
        conn = rpyc.connect('localhost', args.port)
        if args.profile:
            sys.stderr.write("Passing on session startup: %s\n" % args.profile)
            conn.root.start_session(args.profile)
        else:
            sys.stderr.write("Nothing to do.\n")
    else:
        # Run in server mode: Start the application.
        app = Service.app = Pytis2GoApp(args)
        app.run()


if __name__ == '__main__':
    main()
