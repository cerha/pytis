#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2019, 2023 Tomáš Cerha <t.cerha@gmail.com>
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

from __future__ import print_function
import argparse
import getopt
import os
import sys

pytisdir = os.path.dirname(os.path.dirname(__file__))
sys.path.append(os.path.join(pytisdir, 'lib'))
sys.path.append(os.path.join(pytisdir, '..', 'lcg', 'lib'))

import pytis.util
import pytis.presentation
import pytis.data as pd
from pytis.form.managers import (
    LegacyApplicationConfigManager, LegacyFormSettingsManager, LegacyFormProfileManager,
    FormProfileManager, FormProfileParamsManager, LegacyAggregatedViewsManager,
)

class NewFormProfileParamsManager(FormProfileParamsManager):
    def save(self, spec_name, form_name, profile_id, params, errors, transaction=None):
        if not all(v is None for v in params.values()):
            super(NewFormProfileParamsManager, self).save(spec_name, form_name, profile_id,
                                                          params, errors, transaction=transaction)

class NewFormProfileManager(FormProfileManager):
    # Rename the classes with New* prefix to avoid Legacy* class usage
    # even if old columns still exist (see UserSetttingsManager.__new__).
    _PARAMS_MANAGER_CLASS = NewFormProfileParamsManager


def run():
    # Process command line options and init configuration.
    parser = argparse.ArgumentParser(
        description="Convert saved Pytis user configurations from pickled objects to JSON.",
        epilog=("Additionally, you can pass other valid Pytis command line arguments, "
                "such as --dbhost or --dbname to override certain configuration file "
                "options."),
    )
    parser.add_argument('--config', required=True, help="Configuration file path")
    parser.add_argument('--table', help="Process only given table and nothing else.")
    parser.add_argument('--user', help="Process only given user.")
    parser.add_argument('--log-users', action='store_true', help="Print processed user names.")
    parser.add_argument('--dry-run', action='store_true',
                        help="Rollback the transaction at the end.")
    args, argv = parser.parse_known_args()
    try:
        pytis.config.add_command_line_options([sys.argv[0], '--config', args.config] + argv)
    except getopt.GetoptError as e:
        parser.print_help()
        sys.exit(1)

    applibdir = os.path.dirname(os.path.dirname(os.path.dirname(pytis.config.print_spec_dir)))
    if os.path.split(applibdir)[-1] == 'lib':
        sys.path.append(applibdir)
    pytis.config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT,
                                pytis.util.DEBUG, pytis.util.OPERATIONAL]
    resolver = pytis.config.resolver
    transaction = pd.transaction()
    default_profile = pytis.presentation.Profile('__default_profile__', '-')
    try:

        if args.table is None or args.table == 'e_pytis_config':
            print("Processing table: e_pytis_config")
            data = pd.dbtable('e_pytis_config', ('id', 'username', 'options'))
            if args.user:
                users = (args.user,)
            else:
                users = [v.value() for v in data.distinct('username', transaction=transaction)]
            for username in users:
                if args.log_users:
                    print("  Processing user: {}".format(username))
                mgr = LegacyApplicationConfigManager(pytis.config.dbconnection, username=username)
                options = mgr.load(transaction=transaction)
                data.update_many(pd.EQ('username', pd.sval(username)),
                                 pd.Row((('options', pd.Value(pd.JSON(), options)),)),
                                 transaction=transaction)

        if args.table is None or args.table == 'e_pytis_form_settings':
            print("Processing table: e_pytis_form_settings")
            data = pd.dbtable('e_pytis_form_settings',
                              ('id', 'username', 'spec_name', 'form_name', 'settings'))
            if args.user:
                users = (args.user,)
            else:
                users = [v.value() for v in data.distinct('username', transaction=transaction)]
            for username in users:
                if args.log_users:
                    print("  Processing user: {}".format(username))
                mgr = LegacyFormSettingsManager(pytis.config.dbconnection, username=username)
                for sname in data.distinct('spec_name',
                                           condition=pd.EQ('username', pd.sval(username)),
                                           transaction=transaction):
                    spec_name = sname.value()
                    for fname in data.distinct('form_name',
                                               condition=pd.AND(
                                                   pd.EQ('username', pd.sval(username)),
                                                   pd.EQ('spec_name', sname),
                                               ), transaction=transaction):
                        form_name = fname.value()
                        row = mgr._row(spec_name=spec_name, form_name=form_name,
                                       transaction=transaction)
                        settings = mgr._unpickle(row['pickle'].value())
                        data.update(row['id'],
                                    pd.Row((('settings', pd.Value(pd.JSON(), settings)),)),
                                    transaction=transaction)

        if args.table is None or args.table == 'e_pytis_form_profile_base':
            print("Processing table: e_pytis_form_profile_base")
            data = pd.dbtable('e_pytis_form_profile_base',
                              ('id', 'username', 'spec_name', 'profile_id', 'title', 'filter'))
            if args.user:
                users = (args.user,)
            else:
                users = [v.value() for v in data.distinct('username', transaction=transaction)]
            for username in users:
                if args.log_users:
                    print("  Processing user: {}".format(username))
                mgr = LegacyFormProfileManager(pytis.config.dbconnection, username=username)
                newmgr = NewFormProfileManager(pytis.config.dbconnection, username=username)
                for spec_name in mgr.list_spec_names(transaction=transaction):
                    try:
                        view_spec = resolver.get(spec_name, 'view_spec')
                        data_spec = resolver.get(spec_name, 'data_spec')
                    except pytis.util.ResolverError as e:
                        sys.stderr.write("Ignoring specification %s: %s\n" % (spec_name, e))
                        continue
                    for form_name in mgr.list_form_names(spec_name, transaction=transaction):
                        data_object = data_spec.create(dbconnection_spec=pytis.config.dbconnection)
                        profiles = mgr.load_profiles(spec_name, form_name, view_spec, data_object,
                                                     default_profile, transaction=transaction,
                                                     only_saved=True)
                        for profile in profiles:
                            newmgr.save_profile(spec_name, form_name, profile,
                                                transaction=transaction)

        if args.table is None or args.table == 'e_pytis_aggregated_views':
            print("Processing table: e_pytis_aggregated_views")
            data = pd.dbtable('e_pytis_aggregated_views',
                              ('id', 'username', 'spec_name', 'aggregated_view_id', 'params'))
            if args.user:
                users = (args.user,)
            else:
                users = [v.value() for v in data.distinct('username', transaction=transaction)]
            for username in users:
                if args.log_users:
                    print("  Processing user: {}".format(username))
                mgr = LegacyAggregatedViewsManager(pytis.config.dbconnection, username=username)
                for sname in data.distinct('spec_name',
                                           condition=pd.EQ('username', pd.sval(username)),
                                           transaction=transaction):
                    spec_name = sname.value()
                    for aggregated_view_id in mgr.list(spec_name, transaction=transaction):
                        row = mgr._row(spec_name=spec_name, aggregated_view_id=aggregated_view_id,
                                       transaction=transaction)
                        av = mgr.load(spec_name, aggregated_view_id)
                        params = dict(
                            name=av.name(),
                            group_by_columns=av.group_by_columns(),
                            aggregation_columns=av.aggregation_columns(),
                        )
                        data.update(row['id'], pd.Row((('params', pd.Value(pd.JSON(), params)),)),
                                    transaction=transaction)

    except Exception:
        transaction.rollback()
        raise
    else:
        if args.dry_run:
            transaction.rollback()
        else:
            transaction.commit()


if __name__ == '__main__':
    run()
