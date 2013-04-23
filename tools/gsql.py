#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2012, 2013 Brailcom, o.p.s.
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

import optparse
import os
import sys

import pytis.extensions.gensqlalchemy

USAGE = """usage: %prog [ OPTIONS ] SPECIFICATION-MODULE"""

def parse_options():
    parser = optparse.OptionParser(usage=USAGE)
    parser.add_option("--limit", default=None, action="store", dest="regexp",
                      help="output specifications matching and dependent on REGEXP")
    parser.add_option("--no-deps", action="store_true", dest="no_deps",
                      help="do not output dependent objects on --limit")
    parser.add_option("--views", action="store_true", dest="views",
                      help="limit output to views")
    parser.add_option("--functions", action="store_true", dest="functions",
                      help="limit output to functions")
    parser.add_option("--schema", action="store", type="string", dest="schema",
                      help="create all objects in that schema")
    parser.add_option("--names", action="store_true", dest="names_only",
                      help="print only kinds and names of the database objects")
    parser.add_option("--pretty", action="store", type="int", dest="pretty",
                      help="more visually pleasant and possibly less correct output")
    parser.add_option("--source", action="store_true", dest="source",
                      help="print source info")
    parser.add_option("--upgrade", action="store_true", dest="upgrade",
                      help="output SQL commands for upgrade (experimental)")
    parser.add_option("--config", action="store", type="string", dest="config_file",
                      help="pytis configuration file for database access (on upgrade)")
    parser.add_option("--host", default=None, action="store", dest="host")
    parser.add_option("--port", default=None, type="int", action="store", dest="port")
    parser.add_option("--database", default=None, action="store", dest="database")
    parser.add_option("--user", default=None, action="store", dest="user")
    parser.add_option("--password", default=None, action="store", dest="password")
    options, args = parser.parse_args(args=sys.argv[1:])
    if len(args) != 1:
        parser.print_help()
        sys.exit(1)
    return options, args[0]

def update_config(options):
    import config
    if options.config_file is not None:
        for o in config.options():
            o.reset()
        config.config_file = config_file
        config.read_configuration_file(config_file)
    if options.host is not None:
        config.dbhost = options.host
    if options.port is not None:
        config.dbport = options.port
    if options.database is not None:
        config.dbname = options.database
    if options.user is not None:
        config.dbuser = options.user
    if options.password is not None:
        config.dbpass = options.password
    else:
        password = os.getenv('PGPASSWORD')
        if password is not None:
            config.dbpass = password
    
def run():
    options, module = parse_options()
    update_config(options)
    pytis.extensions.gensqlalchemy.gsql_module(module, regexp=options.regexp, no_deps=options.no_deps,
                                               views=options.views, functions=options.functions,
                                               names_only=options.names_only, pretty=options.pretty,
                                               schema=options.schema, source=options.source,
                                               upgrade=options.upgrade)

if __name__ == '__main__':
    run()
