#!/usr/bin/env python

# Copyright (C) 2010 Brailcom, o.p.s.
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

import sys, getopt, pytis.util, pytis.extensions, config 

def usage(msg=None):
    sys.stderr.write("Update saved Pytis user configurations after application changes.\n"
                     "Usage: config-update [ options ] old new\n"
                     "  options: Pytis command line options (defined by pytis configuration)\n"
                     "  old: original form specification\n"
                     "  new: new form specification\n"
                     "Form specification is a string <form-type>/<specification-name>, where\n"
                     "form type may be a `*' to match any form type.\n")
    if msg:
        sys.stderr.write(msg)
        sys.stderr.write('\n')
    sys.exit(1)

def run():
    # Process command line options and init configuration.
    try:
        config.add_command_line_options(sys.argv)
        old, new = sys.argv[1:]
    except getopt.GetoptError, e:
        usage(e.msg)
    except ValueError, e:
        usage()
    # Avoid pytis logging during the update.
    config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT, pytis.util.DEBUG, pytis.util.OPERATIONAL]
    # Do the actual update.
    updated = pytis.extensions.pytis_config_update(old, new)
    print "Updated %d records." % updated

if __name__ == '__main__':
    run()
