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

import sys, getopt, binascii, zlib, cPickle as pickle
import pytis.util, pytis.data, config

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

def pytis_config_update(old, new):
    """Update saved user configurations after application changes.

    Arguments:
      old -- original form specification string 
      new -- new form specification string
      
    Saved user configurations refer to the form type and specification name, so
    if one of those changes, saved user settings, such as form sorting,
    displayed columns, saved filters etc. are lost.  This script goes through
    all saved user configurations and fixes them to match the new specification
    if necessary (if given user has saved config for given form type and
    specification name).

    Form specification is a string <form-type>/<specification-name>, where form
    type is a string name of the form class, such as 'BrowseForm'.  A `*' may
    be used in front of the slash to match any form type.  Only specification
    name will be updated in this case leaving the form type unchanged ('new'
    contains only new specification name whithout form type in this case).

    This function is designed to be invoked from a shell script.  It may prompt for a database
    password on STDIN and write results to STDOUT or STDERR.

    Limitation: Only form state is currently supported, recent forms and startup forms are
    untouched.
    
    Returns the number of updated records (which contained 'old').
    
    """
    bindings = [pytis.data.DBColumnBinding(column, '_pytis_config', column)
                for column in ('uzivatel', 'config')]
    factory = pytis.data.DataFactory(pytis.data.DBDataDefault, bindings, bindings[0])
    while True:
        login = config.dbuser
        dbname = config.dbname or login
        try:
            data = factory.create(dbconnection_spec=config.dbconnection)
        except pytis.data.DBLoginException, e:
            if config.dbconnection.password() is None:
                import getpass
                password = getpass.getpass("Enter database password for %s@%s: " % (login, dbname))
                config.dbconnection.update_login_data(user=login, password=password)
            else:
                sys.stderr.write("Logging to database %s failed.\n" % dbname)
                sys.exit(1)
        else:
            break
    updated = 0
    transaction = pytis.data.DBTransactionDefault(config.dbconnection)
    try:
        data.select(transaction=transaction)
        while True:
            row = data.fetchone(transaction=transaction)
            if row is None:
                break
            saved_config = row['config'].value()
            if saved_config:
                changed = False
                unpacked = dict(pickle.loads(zlib.decompress(binascii.a2b_base64(saved_config))))
                form_state = unpacked.get('form_state')
                if form_state:
                    for key, value in form_state.items():
                        new_key = None
                        if old.startswith('*/'):
                            form, name = key.split('/', 1)
                            if name == old[2:]:
                                new_key = '/'.join((form, new))
                        elif old == key:
                            new_key = new
                        if new_key:
                            del form_state[key]
                            form_state[new_key] = value
                            changed = True
                if changed:
                    updated += 1
                    v = binascii.b2a_base64(zlib.compress(pickle.dumps(tuple(unpacked.items()))))
                    row['config'] = pytis.data.Value(row['config'].type(), v)
                    error, success = data.update(row[0], row, transaction=transaction)
                    if not success:
                        raise Exception(error)
        data.close()
    except:
        transaction.rollback()
        sys.stderr.write("Transaction ROLLED BACK.\n")
        raise
    else:
        transaction.commit()

if __name__ == '__main__':
    run()
