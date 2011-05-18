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
    sys.stderr.write("""Display saved form profiles.
Usage: %s [options] [username [fullname_pattern [profile_name_pattern]]]

  username: Name of the database user owning the profiles.  If omitted, all
    profiles of all users are processed.  Use '*' if you want all users but
    need to specify fullname_pattern or profile_name_pattern.

  fullname_pattern: Wildcard pattern to match the form fullname string.  If
    omitted, printed profiles are not restricted by fullname.  Use '*' if you
    want all fullnames but need to specify profile_name_pattern.

  profile_name_pattern: Wildcard pattern to match the user visible profile name
    string.  If omitted, printed profiles are not restricted by name.

  Options:

    --validate: If supplied, the profiles will be validated.  You may need to
      set up the Python path and pytis defs dir in this case so that the
      specifications may be loaded correctly.

    --invalid:  If supplied, only invalid profiles will printed (implies --validate).

    Other standard Pytis command line options, such as --config or --dbhost and
    --dbname may be used here as well.

""" % sys.argv[0])
    if msg:
        sys.stderr.write(msg)
        sys.stderr.write('\n')
    sys.exit(1)

# State information for print_info().
last_fullname = None
last_username = None
total_printed = 0
    
# Cache of ViewSpec instances and data objects used when validation is on.
cache = {}

def dbop(op, *args, **kwargs):
    while True:
        try:
            return op(*args, **kwargs)
        except pytis.data.DBLoginException as e:
            if config.dbconnection.password() is None:
                import getpass
                login = config.dbuser
                password = getpass.getpass("Enter database password for %s: " % login)
                config.dbconnection.update_login_data(user=login, password=password)
            else:
                sys.stderr.write("Login failed.\n")
                sys.exit(1)

def print_info(username, fullname, info):
    global last_username
    global last_fullname
    global total_printed
    if username != last_username:
        last_username = username
        print "======== %s ========\n" % username
    if fullname != last_fullname:
        last_fullname = fullname
        print fullname + '\n'
    print info
    total_printed += 1

def run():
    if '--help' in sys.argv:
        usage()
    if '--validate' in sys.argv:
        sys.argv.remove('--validate')
        validate = True
    else:
        validate = False
    if '--invalid' in sys.argv:
        sys.argv.remove('--invalid')
        validate = True
        filter_invalid = True
    else:
        filter_invalid = False
    try:
        config.add_command_line_options(sys.argv)
        username = fullname_pattern = profile_name_pattern = None
        if len(sys.argv) > 1:
            username = sys.argv[1]
        if len(sys.argv) > 2:
            fullname_pattern = sys.argv[2]
        if len(sys.argv) > 3:
            profile_name_pattern = sys.argv[3]
        if len(sys.argv) > 4:
            usage()
    except getopt.GetoptError as e:
        usage(e.msg)
    if profile_name_pattern:
        import re
        profile_name_matcher = re.compile(profile_name_pattern.replace('*', '.*'))
    else:
        profile_name_matcher = None
    # Disable pytis logging and notification thread (may cause troubles when
    # creating data objects for profile validation).
    config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT, pytis.util.DEBUG, pytis.util.OPERATIONAL]
    config.dblisten = False
    if username is None or username == '*':
        data = dbop(pytis.data.dbtable, 'e_pytis_form_profiles', ('id', 'username'),
                    config.dbconnection)
        usernames = [v.value() for v in data.distinct('username')]
    else:
        usernames = (username,)
    for username in usernames:
        manager = dbop(DBFormProfileManager, config.dbconnection, username=username)
        for fullname in manager.list_fullnames(pattern=fullname_pattern):
            for profile_id in manager.list_profile_ids(fullname):
                if profile_id == '__dualform__':
                    continue
                profile = manager.load_profile(fullname, profile_id)
                # The profile may be either FormProfile or FormSettings instance!
                if validate and isinstance(profile, pytis.form.FormProfile):
                    specname = fullname.split('/')[2]
                    try:
                        view_spec, data_object, error = cache[specname]
                    except KeyError:
                        resolver = pytis.util.resolver()
                        try:
                            view_spec = resolver.get(specname, 'view_spec')
                            data_spec = resolver.get(specname, 'data_spec')
                            data_object = data_spec.create(dbconnection_spec=config.dbconnection)
                        except Exception as e:
                            view_spec, data_object,  error = None, None, e
                        else:
                            error = None
                        cache[specname] = view_spec, data_object,  error
                    if error:
                        info = "  * %s: Unable to validate profile: %s\n" % (profile_id, error)
                        print_info(username, fullname, info)
                        continue
                    profile.validate(view_spec, data_object)
                if filter_invalid and (isinstance(profile, pytis.form.FormSettings) or profile.valid()):
                    # Ignore FormSettings instances and valid profiles if --invalid was requested.
                    continue
                if profile_name_matcher is None or profile_name_matcher.match(profile.name()):
                    print_info(username, fullname, profile.dump())
    print "Total %d profiles match." % total_printed
                    
if __name__ == '__main__':
    run()
