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

from __future__ import print_function
import sys
import getopt
import argparse
import pytis.util
import pytis.data

from pytis.form import FormProfileManager
from pytis.presentation import Profile

# Cache of ViewSpec instances and data objects.
cache = {}


def run():
    parser = argparse.ArgumentParser(
        description="Validate and optionally fix saved form profiles",
        epilog=("Additionally, you can pass other valid Pytis command line arguments, "
                "such as --dbhost or --dbname to override certain configuration file "
                "options."),
    )
    parser.add_argument('-d', '--delete-column', nargs='+', metavar='SPEC_NAME:COLUMN',
                        default=(), help="Delete COLUMN of SPEC_NAME from all existing profiles")
    parser.add_argument('-r', '--rename-column', nargs='+', metavar='SPEC_NAME:OLD:NEW', default=(),
                        help="Rename column OLD of SPEC_NAME to NEW in all existing profiles")
    parser.add_argument('--config', required=True, metavar='PATH',
                        help="Configuration file path")
    args, argv = parser.parse_known_args()
    try:
        pytis.config.add_command_line_options([sys.argv[0], '--config', args.config] + argv)
        delete_columns = {}
        for spec_name, column in [x.split(':') for x in args.delete_column]:
            delete_columns.setdefault(spec_name, []).append(column)
        rename_columns = {}
        for spec_name, old, new in [x.split(':') for x in args.rename_column]:
            rename_columns.setdefault(spec_name, {})[old] = new
    except (getopt.GetoptError, ValueError):
        parser.print_help()
        sys.exit(1)
    # Disable pytis logging and notification thread (may cause troubles when
    # creating data objects for profile validation).
    pytis.config.dblisten = False
    # Disable pytis logging of data operations etc.
    pytis.config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT,
                                pytis.util.DEBUG, pytis.util.OPERATIONAL]
    while True:
        try:
            data = pytis.data.dbtable('e_pytis_form_profile_base', ('id', 'username'),
                                      pytis.config.dbconnection)
        except pytis.data.DBLoginException as e:
            if pytis.config.dbconnection.password() is None:
                import getpass
                login = pytis.config.dbuser
                password = getpass.getpass("Enter database password for %s: " % login)
                pytis.config.dbconnection.update_login_data(user=login, password=password)
            else:
                sys.stderr.write(e.message())
                sys.exit(1)
        else:
            break
    total_processed = 0
    total_valid = 0
    total_invalid = 0
    skipped = 0
    usernames = [v.value() for v in data.distinct('username')]
    resolver = pytis.util.resolver()
    for username in usernames:
        manager = FormProfileManager(pytis.config.dbconnection, username=username)
        for spec_name in manager.list_spec_names():
            try:
                view_spec, data_object, error = cache[spec_name]
            except KeyError:
                try:
                    view_spec = resolver.get(spec_name, 'view_spec')
                    data_spec = resolver.get(spec_name, 'data_spec')
                    data_object = data_spec.create(dbconnection_spec=pytis.config.dbconnection)
                except Exception as e:
                    print("%s: %s" % (spec_name, e))
                    skipped += 1
                    view_spec, data_object,  error = None, None, e
                else:
                    error = None
                cache[spec_name] = view_spec, data_object, error
            if not error:
                for form_name in manager.list_form_names(spec_name):
                    for profile in manager.load_profiles(
                            spec_name, form_name, view_spec, data_object,
                            Profile('__default_profile__', '-'),
                            delete_columns=delete_columns.get(spec_name, ()),
                            rename_columns=rename_columns.get(spec_name, {}),
                    ):
                        # Update the 'errors' column in the database table.
                        manager.save_profile(spec_name, form_name, profile)
                        for param, error in profile.errors():
                            print(':'.join((username, spec_name, form_name, profile.id(),
                                            ' %s: %s' % (param, error))))
                        if profile.errors():
                            total_invalid += 1
                        else:
                            total_valid += 1
                        total_processed += 1
    print((u"Total %d profiles processed:\n"
           u"  %d valid\n"
           u"  %d invalid") % (total_processed, total_valid, total_invalid))
    if skipped == 1:
        print(u"  1 specification skipped due to errors")
    elif skipped > 1:
        print(u"  %d specifications skipped due to errors" % skipped)


if __name__ == '__main__':
    run()
