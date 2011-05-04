#!/usr/bin/env python

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

import sys, getopt, pprint, types
import pytis.util, pytis.data, config

from pytis.form import DBFormProfileManager

def usage(msg=None):
    sys.stderr.write("Display saved form profiles.\n"
                     "Usage: config-update [options] username [pattern]\n"
                     "  options: Pytis command line options (defined by pytis configuration)\n"
                     "  username: Name of the database user owning the profiles\n"
                     "  pattern: wildcard pattern to mach form specification names\n"
                     "  and/or profile names (specification name pattern may be\n"
                     "  followed by a colon and profiloe name pattern)\n")
    if msg:
        sys.stderr.write(msg)
        sys.stderr.write('\n')
    sys.exit(1)

def run():
    try:
        config.add_command_line_options(sys.argv)
        if len(sys.argv) == 2:
            username, pattern = sys.argv[1], None
        elif len(sys.argv) == 3:
            username, pattern = sys.argv[1:]
        else:
            usage()
    except getopt.GetoptError, e:
        usage(e.msg)
    # Disable pytis logging.
    if pattern and ':' in pattern:
        import re
        pattern, name_pattern = pattern.split(':', 1)
        name_matcher = re.compile(name_pattern.replace('*', '.*'))
    else:
        name_matcher = None
    config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT, pytis.util.DEBUG, pytis.util.OPERATIONAL]
    while True:
        try:
            manager = DBFormProfileManager(config.dbconnection, username=username)
        except pytis.data.DBLoginException, e:
            if config.dbconnection.password() is None:
                import getpass
                login = config.dbuser
                password = getpass.getpass("Enter database password for %s: " % login)
                config.dbconnection.update_login_data(user=login, password=password)
            else:
                sys.stderr.write("Login failed.\n")
                sys.exit(1)
        else:
            break
    pp = pprint.PrettyPrinter()
    for fullname in manager.list_fullnames(pattern=pattern):
        fullname_printed = False
        for profile_id in manager.list_profile_ids(fullname):
            if profile_id == '__dualform__':
                continue
            profile = manager.load_profile(fullname, profile_id)
            if isinstance(profile, pytis.form.FormProfile):
                name, state_id = profile._state['_name'], profile._state['_id']
            else:
                name, state_id = profile.name(), profile.id()
            if name_matcher is None or name_matcher.match(name):
                if not fullname_printed:
                    print '\n' + fullname
                    fullname_printed = True
                if isinstance(name, types.UnicodeType):
                    name = name.encode('utf-8')
                if isinstance(state_id, types.UnicodeType):
                    state_id = state_id.encode('utf-8')
                print '  * %s (%s):' % (state_id, name)
                if isinstance(profile, pytis.form.FormProfile):
                    state = profile._state
                    for key in ('filter', 'sorting', 'columns', 'grouping', 'folding', 'aggregations',
                                'column_widths', 'group_by_columns', 'aggregation_columns'):
                        value = profile._state['_'+key]
                        indent = '\n        ' + ' ' * len(key)
                        formatted = indent.join(pp.pformat(value).splitlines())
                        print '    - %s: %s' % (key, formatted)
                else:
                    for key, value in profile._settings.items():
                        print '    - %s: %s' % (key, value)

if __name__ == '__main__':
    run()
