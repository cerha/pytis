#!/usr/bin/env python

# Copyright (C) 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2010-2018 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import sys
import getopt
import pytis.util
import pytis.data
import pytis.help
import config


def usage(msg=None):
    sys.stderr.write("""Update DB help texts for a Pytis application
Usage: %s [options]
   Options are pytis command line options, such as --config or --dbhost and --dbname.
""" % sys.argv[0])
    if msg:
        sys.stderr.write(msg)
        sys.stderr.write('\n')
    sys.exit(1)


def run():
    if '--help' in sys.argv:
        usage()
    try:
        config.add_command_line_options(sys.argv)
    except getopt.GetoptError as e:
        usage(e.msg)
    if len(sys.argv) != 1:
        usage()
    # Disable pytis logging and notification thread (may cause troubles when
    # creating data objects for profile validation).
    config.dblisten = False
    # Disable pytis logging of data operations etc.
    config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT,
                          pytis.util.DEBUG, pytis.util.OPERATIONAL]
    while True:
        try:
            updater = pytis.help.HelpUpdater()
        except pytis.data.DBLoginException as e:
            if config.dbconnection.password() is None:
                import getpass
                login = config.dbuser
                password = getpass.getpass("Enter database password for %s: " % login)
                config.dbconnection.update_login_data(user=login, password=password)
            else:
                sys.stderr.write(e.message())
                sys.exit(1)
        else:
            break
    updater.update()


if __name__ == '__main__':
    run()
