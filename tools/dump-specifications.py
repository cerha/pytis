#!/usr/bin/env python

# Copyright (C) 2010, 2011, 2015 Brailcom, o.p.s.
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

"""Dump Pytis application specifications to STDOUT.
 
Specifications, thier fields and properties are dumped to standard output in a
way, that allows comparing these outputs to detect changes caused by
modifications of specification files or Pytis API changes (or both).

"""

import sys
import re
import getopt
import argparse

import pytis.extensions
import pytis.util

class NullLogger(pytis.util.Logger):
    def log(self, *args, **kwargs):
        pass

OBJID_REGEX = re.compile(r'(object )?at 0x[0-9a-f]+')

def run():
    parser = argparse.ArgumentParser(
        description="Dump Pytis application specifications to STDOUT",
        epilog=("Additionally, you can pass other valid Pytis command line arguments, "
                "such as --dbhost or --dbname to override certain configuration file "
                "options."),
    )
    parser.add_argument('--wiking', action='store_true',
                        help="Dump a wiking application (Pytis wx application is the default)")
    parser.add_argument('--exclude', nargs='+', metavar='NAME', default=(),
                        help="Name(s) of specification(s) to skip")
    parser.add_argument('--config', required=True, metavar='PATH',
                        help="Configuration file path")

    args, argv = parser.parse_known_args()
    import config
    try:
        config.add_command_line_options([sys.argv[0], '--config', args.config] + argv)
    except getopt.GetoptError as e:
        parser.print_help()
        sys.exit(1)
    config.log_logger = (NullLogger, (), {})
    if args.wiking:
        import wiking
        wiking.cfg.user_config_file = config.config_file
        config.resolver = wiking.cfg.resolver = wiking.WikingResolver(wiking.cfg.modules)
        config.dbconnections = wiking.cfg.connections
        config.dbconnection = config.option('dbconnection').default()
        base_class = wiking.Module
    else:
        base_class = pytis.presentation.Specification
    for name, cls in sorted(config.resolver.walk(cls=base_class)):
        if name not in args.exclude:
            try:
                if not args.wiking:
                    spec = cls()
                    if not spec.public:
                        continue
                    data_spec = spec.data_spec()
                    data = data_spec.create(dbconnection_spec=config.dbconnection)            
                elif hasattr(cls, 'Spec'):
                    spec = cls.Spec(cls)
                    module = cls(name)
                    data = module._data
                else:
                    continue
                #print ' ', spec.table
                view_spec = spec.view_spec()
                record = pytis.presentation.PresentedRow(view_spec.fields(), data, None)
                for fid in sorted(record.keys()):
                    t = record.type(fid)
                    f = view_spec.field(fid)
                    attr = (
                        ('not_null', t.not_null()),
                        ('editable', f.editable()),
                        ('type', t),
                    )
                    attributes = ' '.join('%s=%s' % (k, v) for k, v in attr if v is not None)
                    print OBJID_REGEX.sub('', '%s.%s %s' % (name, fid, attributes))
                print
            except Exception as e:
                print 'ERROR: %s: %s' % (name, e)


if __name__ == '__main__':
    run()
