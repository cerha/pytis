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

from pytis.form import DBFormProfileManager

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
                sys.stderr.write("Login failed.\n")
                sys.exit(1)
        else:
            break
    total_processed = 0
    total_valid = 0
    total_invalid = 0
    usernames = [v.value() for v in data.distinct('username')]
    for username in usernames:
        manager = DBFormProfileManager(config.dbconnection, username=username)
        for fullname in manager.list_fullnames():
            specname = fullname.split('/')[2]
            for profile_id in manager.list_profile_ids(fullname):
                if profile_id not in ('__dualform__', '__form_settings__'):
                    # '__dualform__' profiles are no longer used, but they may
                    # remain in the database. once they are deleted from all
                    # projects, the condition may be removed.
                    # '__form_settings__' profiles are not FormProfile
                    # instances and they don't need to be validated.
                    profile = manager.load_profile(fullname, profile_id)
                    try:
                        view_spec, data_object, error = cache[specname]
                    except KeyError:
                        resolver = pytis.util.resolver()
                        try:
                            view_spec = resolver.get(specname, 'view_spec')
                            data_spec = resolver.get(specname, 'data_spec')
                            data_object = data_spec.create(dbconnection_spec=config.dbconnection)
                        except Exception as e:
                            print "%s: %s" % (specname, e)
                            view_spec, data_object,  error = None, None, e
                        else:
                            error = None
                        cache[specname] = view_spec, data_object,  error
                    if not error:
                        errors = profile.validate(view_spec, data_object)
                        # Update the 'errors' column in the database table.
                        manager.save_profile(fullname, profile)
                        for error in errors:
                            print ':'.join((username, fullname, profile_id, ' '+ error))
                        if errors:
                            total_invalid += 1
                        else:
                            total_valid += 1
                    total_processed += 1
    print (u"Total %d profiles processed:\n"
           u"  %d valid\n"
           u"  %d invalid") % (total_processed, total_valid, total_invalid)
    skipped = total_processed - (total_valid + total_invalid)
    if skipped != 0:
        print u"  %d skipped due to errors" % skipped


if __name__ == '__main__':
    run()
