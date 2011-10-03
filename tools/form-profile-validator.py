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

import sys, getopt, types
import pytis.util, pytis.data, config

from pytis.form import FormProfileManager
from pytis.presentation import Profile

def usage(msg=None):
    sys.stderr.write("""Validate saved form profiles.
Usage: %s [options]
Options: Pytis command line options, such as --config or --dbhost and --dbname.
""" % sys.argv[0])
    if msg:
        sys.stderr.write(msg)
        sys.stderr.write('\n')
    sys.exit(1)

    
# Cache of ViewSpec instances and data objects.
cache = {}

def run():
    if '--help' in sys.argv:
        usage()
    try:
        config.add_command_line_options(sys.argv)
        if len(sys.argv) > 1:
            usage()
    except getopt.GetoptError as e:
        usage(e.msg)
    # Disable pytis logging and notification thread (may cause troubles when
    # creating data objects for profile validation).
    config.dblisten = False
    # Disable pytis logging of data operations etc.
    config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT, pytis.util.DEBUG, pytis.util.OPERATIONAL]
    while True:
        try:
            data = pytis.data.dbtable('e_pytis_form_profiles', ('id', 'username'),
                                      config.dbconnection)
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
    total_processed = 0
    total_valid = 0
    total_invalid = 0
    skipped = 0
    usernames = [v.value() for v in data.distinct('username')]
    resolver = pytis.util.resolver()
    for username in usernames:
        manager = FormProfileManager(config.dbconnection, username=username)
        for spec_name in manager.list_spec_names():
            try:
                view_spec, data_object, error = cache[spec_name]
            except KeyError:
                try:
                    view_spec = resolver.get(spec_name, 'view_spec')
                    data_spec = resolver.get(spec_name, 'data_spec')
                    data_object = data_spec.create(dbconnection_spec=config.dbconnection)
                except Exception as e:
                    print "%s: %s" % (spec_name, e)
                    skipped += 1
                    view_spec, data_object,  error = None, None, e
                else:
                    error = None
                cache[spec_name] = view_spec, data_object,  error
            if not error:
                for form_name in manager.list_form_names(spec_name):
                    for profile in manager.load_profiles(spec_name, form_name,
                                                         view_spec, data_object,
                                                         Profile('__default_profile__', '-')):
                        # Update the 'errors' column in the database table.
                        manager.save_profile(spec_name, form_name, profile)
                        for param, error in profile.errors():
                            print ':'.join((username, spec_name, form_name, profile.id(), ' %s: %s' % (param, error)))
                        if profile.errors():
                            total_invalid += 1
                        else:
                            total_valid += 1
                        total_processed += 1
    print (u"Total %d profiles processed:\n"
           u"  %d valid\n"
           u"  %d invalid") % (total_processed, total_valid, total_invalid)
    if skipped == 1:
        print u"  1 specification skipped due to errors"
    elif skipped > 1:
        print u"  %d specifications skipped due to errors" % skipped


if __name__ == '__main__':
    run()
