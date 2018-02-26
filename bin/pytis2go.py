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
import argparse
import platform

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

import pytis.x2goclient  # noqa: E402


def main():
    parser = argparse.ArgumentParser(
        description='X2Go Pytis client service.',
        # formatter_class=argparse.RawDescriptionHelpFormatter,
        add_help=True, argument_default=None
    )
    options = (
        (('--broker-url', '-b'), {},
         "Retrieve session profiles via an X2Go Session Broker under the given URL."),
        (('--autoload', '-a'), {'default': False, 'action': 'store_true'},
         "Automatically load profiles (connect to broker) on astartup."),
        # (('--list-profiles', '-l'), {'default': False, 'action': 'store_true'},
        #  'List available session profiles and exit.'),
        (('--session-profile', '-p'), {},
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
        (('--add-to-known-hosts', '-A'), {'default': False, 'action': 'store_true'},
         ("Add RSA host key fingerprint to ~/.ssh/known_hosts "
          "if authenticity of server can\'t be established (default: not set)")),
        (('--window-title', '-t'), {'default': None},
         "Override startup application progress window title (default: Pytis2Go)"),
        (('--version', '-v'), {'default': False, 'action': 'store_true'},
         "Print version number and exit."),
    )
    for args, kwargs, doc in options:
        parser.add_argument(*args, **dict(kwargs, help=doc))

    args = parser.parse_args()

    if args.version:
        sys.stderr.write("%s\n" % (pytis.x2goclient.X2GOCLIENT_VERSION,))
        sys.exit(0)

    app = pytis.x2goclient.X2GoStartApp(args)
    app.MainLoop()


if __name__ == '__main__':
    main()
