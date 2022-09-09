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
import pytis.data as pd
from pytis.form.managers import LegacyApplicationConfigManager


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
    transaction = pd.transaction()
    try:
        data = pd.dbtable('e_pytis_config', ('id', 'username', 'options'))
        for v in data.distinct('username', transaction=transaction):
            username = v.value()
            acm = LegacyApplicationConfigManager(pytis.config.dbconnection, username=username)
            options = acm.load(transaction=transaction)
            data.update_many(pd.EQ('username', pd.sval(username)),
                             pd.Row((('options', pd.Value(pd.JSON(), options)),)),
                             transaction=transaction)
    except Exception:
        transaction.rollback()
        raise
    else:
        transaction.commit()

if __name__ == '__main__':
    run()
