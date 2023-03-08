#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2019, 2022 Tomáš Cerha <t.cerha@gmail.com>
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
import sys
import getopt
import pytis.util
import pytis.presentation
import pytis.data as pd
from pytis.form.managers import (
    LegacyApplicationConfigManager, LegacyFormSettingsManager, LegacyFormProfileManager,
    FormProfileManager, LegacyAggregatedViewsManager,
)

def die(message):
    sys.stderr.write(message + "\n")
    sys.exit(1)


def usage(msg=None):
    message = ("Update saved Pytis user configurations.\n"
               "Usage: %s [options]\n"
               "  options: Pytis command line options to specify database connection"
               " (defined by pytis configuration)\n") % sys.argv[0]
    if msg:
        message += '\n' + msg
    die(message)


def run():
    # Process command line options and init configuration.
    try:
        pytis.config.add_command_line_options(sys.argv)
    except getopt.GetoptError as e:
        usage(e.msg)
    # Avoid pytis logging during the update.
    pytis.config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT,
                                pytis.util.DEBUG, pytis.util.OPERATIONAL]
    resolver = pytis.config.resolver
    transaction = pd.transaction()
    default_profile = pytis.presentation.Profile('__default_profile__', '-')
    try:
        data = pd.dbtable('e_pytis_config', ('id', 'username', 'options'))
        for v in data.distinct('username', transaction=transaction):
            username = v.value()
            mgr = LegacyApplicationConfigManager(pytis.config.dbconnection, username=username)
            options = mgr.load(transaction=transaction)
            data.update_many(pd.EQ('username', pd.sval(username)),
                             pd.Row((('options', pd.Value(pd.JSON(), options)),)),
                             transaction=transaction)
        data = pd.dbtable('e_pytis_form_settings',
                          ('id', 'username', 'spec_name', 'form_name', 'settings'))
        for uname in data.distinct('username', transaction=transaction):
            username = uname.value()
            mgr = LegacyFormSettingsManager(pytis.config.dbconnection, username=username)
            for sname in data.distinct('spec_name', condition=pd.EQ('username', uname),
                                       transaction=transaction):
                spec_name = sname.value()
                for fname in data.distinct('form_name',
                                           condition=pd.AND(pd.EQ('username', uname),
                                                            pd.EQ('spec_name', sname)),
                                           transaction=transaction):
                    form_name = fname.value()
                    row = mgr._row(spec_name=spec_name, form_name=form_name, transaction=transaction)
                    settings = mgr._unpickle(row['pickle'].value())
                    data.update(row['id'], pd.Row((('settings', pd.Value(pd.JSON(), settings)),)),
                                transaction=transaction)
        data = pd.dbtable('e_pytis_form_profile_base',
                          ('id', 'username', 'spec_name', 'profile_id', 'title', 'filter'))
        for uname in data.distinct('username', transaction=transaction):
            username = uname.value()
            mgr = LegacyFormProfileManager(pytis.config.dbconnection, username=username)
            newmgr = FormProfileManager(pytis.config.dbconnection, username=username)
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
                                                 default_profile, transaction=transaction)
                    for profile in profiles:
                        newmgr.save_profile(spec_name, form_name, profile, transaction=transaction)
        data = pd.dbtable('e_pytis_aggregated_views',
                          ('id', 'username', 'spec_name', 'aggregated_view_id', 'params'))
        for uname in data.distinct('username', transaction=transaction):
            username = uname.value()
            mgr = LegacyAggregatedViewsManager(pytis.config.dbconnection, username=username)
            for sname in data.distinct('spec_name', condition=pd.EQ('username', uname),
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
        transaction.commit()


if __name__ == '__main__':
    run()
