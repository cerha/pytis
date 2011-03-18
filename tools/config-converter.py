#!/usr/bin/env python
# -*- coding: iso-8859-2 -*-

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

import sys, getopt, sys, binascii, zlib, cPickle as pickle
import pytis.util, pytis.data, config
from pytis.form import DBConfigurationStorage, DBFormProfileManager, FormProfile


def usage(msg=None):
    sys.stderr.write("Convert saved Pytis user configurations to form profiles.\n"
                     "Usage: config-update [options] [users]\n"
                     "  options: Pytis command line options (defined by pytis configuration)\n"
                     "  users: Name(s) of database users for which the conversion is to be\n"
                     "         done.  If no user names are given, all users are processed.\n\n"
                     "The script may prompt for a database password on STDIN if the database\n"
                     "requires it and it was not passed as --dbpass.  The usernames are printed\n"
                     "to STDOUT as they are processed with the total number of created profiles\n"
                     "for given user.\n")
    if msg:
        sys.stderr.write(msg)
        sys.stderr.write('\n')
    sys.exit(1)

def run():
    # Process command line options and init configuration.
    try:
        config.add_command_line_options(sys.argv)
    except getopt.GetoptError, e:
        usage(e.msg)
    if config.help:
        usage()
    # Avoid pytis logging during the update.
    config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT, pytis.util.DEBUG, pytis.util.OPERATIONAL]
    # Do the actual update.
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
    transaction = pytis.data.DBTransactionDefault(config.dbconnection)
    forms = {'MainForm': pytis.form.MultiBrowseDualForm.MainForm,
             'TabbedBrowseForm': pytis.form.MultiSideForm.TabbedBrowseForm,
             '_SideForm': pytis.form.AggregationDualForm._SideForm,
             'SubForm': None}
    ignored_specifications = []
    if len(sys.argv) > 1:
        condition = pytis.data.OR(
            *[pytis.data.EQ('uzivatel', pytis.data.Value(pytis.data.String(), username))
              for username in sys.argv[1:]])
    else:
        condition = None
    try:
        data.select(condition=condition, transaction=transaction)
        print "Converting user profiles:"
        while True:
            row = data.fetchone(transaction=transaction)
            if row is None:
                break
            saved_config = row['config'].value()
            if not saved_config:
                continue
            print "  *", row['uzivatel'].value(), '...'
            count = 0
            options = dict(pickle.loads(zlib.decompress(binascii.a2b_base64(saved_config))))
            cfg = DBConfigurationStorage(config.dbconnection, username=row['uzivatel'].value())
            manager = DBFormProfileManager(config.dbconnection, username=row['uzivatel'].value())
            for key, state in options.pop('form_state', {}).items():
                if not state:
                    continue
                formname, specname = key.split('/')
                try:
                    form = forms[formname]
                except KeyError:
                    form = getattr(pytis.form, formname)
                if form is None:
                    continue # Ignore obsolete forms mapped to None.
                fullname = 'form/%s.%s/%s//' % (form.__module__, form.__name__, specname)
                resolver = pytis.util.resolver()
                try:
                    view_spec = resolver.get(specname, 'view_spec')
                    data_spec = resolver.get(specname, 'data_spec')
                except pytis.util.ResolverError, e:
                    # Ignore configurations for specifications that no longer exist
                    if specname not in ignored_specifications:
                        ignored_specifications.append((specname, e))
                    continue
                kwargs = dict([(param, state[param])
                               for param in ('sorting', 'grouping', 'columns', 'folding')
                               if state.get(param) is not None])
                try:
                    data_object = data_spec.create(dbconnection_spec=config.dbconnection)
                except Exception, e:
                    if specname not in ignored_specifications:
                        ignored_specifications.append((specname, e))
                    continue
                if state.has_key('column_width'):
                    kwargs['column_widths'] = dict(state['column_width'])
                if kwargs.has_key('sorting'):
                    mapping = {'SORTING_ASCENDENT': pytis.data.ASCENDENT,
                               'SORTING_DESCENDANT': pytis.data.DESCENDANT}
                    kwargs['sorting'] = tuple([(cid, mapping[dir]) for cid, dir in kwargs['sorting']])
                if kwargs:
                    profile = FormProfile('__default_profile__', _("V�choz� profil"), **kwargs)
                    manager.save_profile(fullname, profile, transaction=transaction)
                    count += 1
                    for p in view_spec.profiles():
                        profile = FormProfile(p.id(), p.name(), filter=p.filter(), **kwargs)
                        manager.save_profile(fullname, profile, transaction=transaction)
                        count += 1
                for i, (name, cond) in enumerate(state.pop('conditions', ())):
                    profile = FormProfile('_user_profile_%d' % (i+1), name.strip())
                    profile._state = dict([('_'+k, v) for k,v in kwargs.items()], _filter=cond)
                    if profile.validate(view_spec, data_object):
                        manager.save_profile(fullname, profile, transaction=transaction)
                        count += 1
                    else:
                        print "    - Ignoring invalid saved condition '%s': %s" % (name, cond)
            for option, value in options.pop('application_state', {}).items():
                options[option.replace('startup_forms', 'saved_startup_forms')] = value
            # Some old saved configs may include 'dbconnection' it due to an old bug.
            options.pop('dbconnection', None)
            cfg.write(options.items())
            print "    ... %d profiles created, %d configuration options converted." % (count, len(options))
        data.close()
    except:
        transaction.rollback()
        sys.stderr.write("Transaction ROLLED BACK.\n")
        raise
    else:
        transaction.commit()
    if ignored_specifications:
        print "The following specifications were ignored due to resolver errors:"
        for specname, error in sorted(ignored_specifications):
            print "  - %s: %s" % (specname, error)
            

if __name__ == '__main__':
    run()
