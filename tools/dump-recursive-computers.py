#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2010-2018 OUI Technology Ltd.
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

"""Dump fields with recursive runtime_filter or runtime_arguments.

All specifications are walked through and fields which define a runtime_filter
or runtime_arguments computer which depend on the field itself are reported to
STDOUT.

"""
from __future__ import print_function

import sys
import getopt
import argparse

import pytis.presentation
import pytis.util


class NullLogger(pytis.util.Logger):

    def log(self, *args, **kwargs):
        pass


def run():
    parser = argparse.ArgumentParser(
        description="Dump fields with recursive runtime_filter or runtime_arguments.",
        epilog=("Additionally, you can pass other valid Pytis command line arguments, "
                "such as --dbhost or --dbname to override certain configuration file "
                "options."),
    )
    parser.add_argument('-w', '--wiking', action='store_true',
                        help="Dump a wiking application (Pytis wx application is the default)")
    parser.add_argument('-e', '--exit-on-error', action='store_true',
                        help=("Print traceback and exit when exception occurs. "
                              "By default, the error message is printed to the "
                              "dump output and the program continues."))
    parser.add_argument('-x', '--exclude', nargs='+', metavar='NAME', default=(),
                        help="Name(s) of specification(s) to skip")
    parser.add_argument('-i', '--include', nargs='+', metavar='NAME', default=(),
                        help="Dump only given specification name(s)")
    parser.add_argument('--config', required=True, metavar='PATH',
                        help="Configuration file path")

    args, argv = parser.parse_known_args()
    try:
        pytis.config.add_command_line_options([sys.argv[0], '--config', args.config] + argv)
    except getopt.GetoptError as e:
        parser.print_help()
        sys.exit(1)
    pytis.config.log_logger = (NullLogger, (), {})
    import lcg
    lcg.log = lambda *args, **kwargs: None
    if args.wiking:
        import wiking
        wiking.cfg.user_config_file = pytis.config.config_file
        pytis.config.resolver = wiking.cfg.resolver = wiking.WikingResolver(wiking.cfg.modules)
        pytis.config.dbconnections = wiking.cfg.connections
        pytis.config.dbconnection = pytis.config.option('dbconnection').default()
    resolver = pytis.config.resolver
    processed, errors = 0, 0
    if args.include:
        names = args.include
    else:
        if args.wiking:
            base_class = wiking.Module
        else:
            base_class = pytis.presentation.Specification
        names = sorted(name for name, cls in resolver.walk(cls=base_class))
    for name in names:
        if name not in args.exclude:
            try:
                if args.wiking:
                    cls = resolver.wiking_module_cls(name)
                    if not hasattr(cls, 'Spec'):
                        continue
                    module = resolver.wiking_module(name)
                    spec = module.Spec(cls)
                else:
                    spec = resolver.specification(name)
                    if not spec.public:
                        continue
                view_spec = spec.view_spec()

                def all_deps(fid):
                    result = set((fid,))
                    computer = view_spec.field(fid).computer()
                    if computer:
                        for k in computer.depends():
                            result.update(all_deps(k))
                    return result

                for f in view_spec.fields():
                    for attr in ('runtime_filter', 'runtime_arguments'):
                        computer = getattr(f, attr)()
                        if computer:
                            for dep in computer.depends():
                                if f.id() in all_deps(dep) and dep not in computer.novalidate():
                                    print("%s.%s: Cyclic dependency in '%s' specification. "
                                          "Add '%s' to 'novalidate' to avoid recursion." %
                                          (name, f.id(), attr, dep))
            except Exception as e:
                if args.exit_on_error:
                    try:
                        import cgitb
                        tb = cgitb.text(sys.exc_info())
                    except Exception:
                        import traceback
                        tb = "".join(traceback.format_exception(*sys.exc_info())) + "\n"
                    sys.stderr.write(tb)
                    sys.stderr.write("Failed on specification: %s\n\n" % name)
                    sys.exit(1)
                else:
                    print('ERROR: %s: %s: %r' % (name, e.__class__.__name__, e))
                    errors += 1
            processed += 1
    sys.stderr.write("Processed %d specifications with %d errors%s.\n" %
                     (processed, errors,
                      " (grep output for '^ERROR:' or use --exit-on-error)" if errors else ''))


if __name__ == '__main__':
    run()
