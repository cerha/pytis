#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2010, 2011 Brailcom, o.p.s.
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

import sys, getopt, binascii, zlib, cPickle as pickle
import pytis.util, pytis.data, config
from pytis.form import DBConfigurationStorage

def die(message):
    sys.stderr.write(message + "\n")
    sys.exit(1)

def usage(msg=None):
    message = ("View/update saved Pytis user configurations.\n"
               "Usage: %s [options] command username [option [value]]\n"
               "  options: Pytis command line options to specify database connection (defined by pytis configuration)\n"
               "  command: One of 'set', 'get', 'unset'\n"
               "  username: Name of the pytis user whose configuration is to be operated\n"
               "  option: Configuration option name (optional for the 'get' command)\n"
               "  value: The new option value (only for the 'set' command)") % sys.argv[0]
    if msg:
        message += '\n' + msg
    die(message)

def run():
    # Process command line options and init configuration.
    try:
        config.add_command_line_options(sys.argv)
        command, username = sys.argv[1:3]
        option = len(sys.argv) > 3 and sys.argv[3] or None
        value = len(sys.argv) > 4 and sys.argv[4] or None
    except getopt.GetoptError as e:
        usage(e.msg)
    except ValueError as e:
        usage()
    if command not in ('get', 'set', 'unset'):
        usage("Unknown command '%s'" % command)
    if command != 'get' and option is None:
        usage("Option name is mandatory for command '%s'" % command)
    if command == 'set' and value is None:
        usage("Value must be specified for command '%s'" % command)
    if command != 'set' and value is not None:
        usage("Value mmakes no sense for command '%s'" % command)
    storage = DBConfigurationStorage(config.dbconnection, username=username)
    cfg = dict(storage.read())
    if command == 'get':
        if option is not None:
            if option not in cfg:
                die("Option '%s' not set for user '%s'." % (option, username))
            options = (option,)
        else:
            if not cfg.keys():
                die("No options set for user '%s'." % username)
            options = sorted(cfg.keys())
        for option in options:
            print "%s = %s" % (option, cfg[option])
    else:
        if command == 'unset':
            if option not in cfg:
                die("Option '%s' not set for user '%s'." % (option, username))
            else:
                del cfg[option]
        else: # command == 'set'
            cfg[option] = value
        # Avoid pytis logging during the update.
        config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT, pytis.util.DEBUG, pytis.util.OPERATIONAL]
        # Update the saved configuration.
        storage.write(cfg.items())


if __name__ == '__main__':
    run()
