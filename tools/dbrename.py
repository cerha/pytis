#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2013 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from pytis.util import translate as _

from pytis.util import translate as _

import imp
import re
import sys
import psycopg2 as dbapi

def output(text):
    sys.stdout.write(text + '\n')

def open_connection(db):
    return dbapi.connect(database=getattr(db, 'dbname', None),
                         host=getattr(db, 'dbhost', None),
                         user=getattr(db, 'dbuser', None),
                         password=getattr(db, 'dbpass', None),
                         port=getattr(db, 'dbport', None))
    
def run(current_db, sample_db):
    if current_db.endswith('.py'):
        current_db = current_db[:-3]
    if sample_db.endswith('.py'):
        sample_db = sample_db[:-3]
    current_config = imp.load_module('current_db', *imp.find_module(current_db))
    sample_config = imp.load_module('sample_db', *imp.find_module(sample_db))
    current_connection = open_connection(current_config)
    sample_connection = open_connection(sample_config)
    current_c = current_connection.cursor()
    sample_c = sample_connection.cursor()
    # Triggers
    sample_c.execute("select tgname, nspname, relname, tgtype from pg_trigger join pg_class on tgrelid=pg_class.oid join pg_namespace on relnamespace=pg_namespace.oid where not tgisinternal and (tgname like (nspname||'\_\_%'))")
    regexp = re.compile('([a-z_]+)__([a-z_0-9]+)__(log|insert|update|delete)__(before|after)')
    for row in sample_c.fetchall():
        name = row[0]
        match = regexp.match(name)
        if match:
            old_name = match.group(2) + '_' + match.group(3)[:3]
            current_c.execute("select tgname from pg_trigger join pg_class on tgrelid=pg_class.oid join pg_namespace on relnamespace=pg_namespace.oid where not tgisinternal and (tgname=%s or tgname=%s) and nspname=%s and relname=%s and tgtype=%s", (old_name,) + row)
            trigger_names = current_c.fetchall()
            count = len(trigger_names)
            if count == 0:
                output("-- Can't find original trigger: %s (for %s)" % (old_name, name,))
            elif count == 1:
                cur_name = trigger_names[0][0]
                if cur_name != name:
                    output("ALTER TRIGGER %s ON %s.%s RENAME TO %s;" % (cur_name, row[1], row[2], name,))
            else:
                output("-- Multiple occurrences of trigger: %s (for %s)" % (old_name, name,))
        else:
            output("-- Can't determine original trigger name: %s" % (name,))
    # Rules
    sample_c.execute("select pg_rewrite.oid, rulename, nspname, relname, ev_attr, ev_type, is_instead from pg_rewrite join pg_class on ev_class=pg_class.oid join pg_namespace on relnamespace=pg_namespace.oid where rulename like '%\_\_%'")
    regexp = re.compile('([a-z_0-9]+)__(insert|update|delete)_(also|instead)')
    for row in sample_c.fetchall():
        name = row[1]
        match = regexp.match(name)
        if match:
            old_name = match.group(1) + '_' + match.group(2)[:3]
            current_c.execute("select pg_rewrite.oid, rulename from pg_rewrite join pg_class on ev_class=pg_class.oid join pg_namespace on relnamespace=pg_namespace.oid where (rulename=%s or rulename=%s) and nspname=%s and relname=%s and ev_attr=%s and ev_type=%s and is_instead=%s", (old_name,) + row[1:])
            rule_names = current_c.fetchall()
            count = len(rule_names)
            if count == 0:
                output("-- Can't find original rule: %s (for %s)" % (old_name, name,))
            elif count == 1:
                cur_name = rule_names[0][1]
                if cur_name != name:
                    output("UPDATE pg_rewrite SET rulename='%s' WHERE oid=%s;" % (name, rule_names[0][0],))
            else:
                output("-- Multiple occurrences of rule: %s (for %s)" % (old_name, name,))
        else:
            output("-- Can't determine original rule name: %s" % (name,))
    # Indexes
    sample_c.execute("select ci.relname, nspname, c.relname from pg_index join pg_class ci on indexrelid=ci.oid join pg_class c on indrelid=c.oid join pg_namespace on c.relnamespace=pg_namespace.oid where ci.relname like 'ix_%'")
    for row in sample_c.fetchall():
        name = row[0]
        schema = row[1]
        table = row[2]
        prefix = 'ix_%s_%s_' % (schema, table,)
        if name.startswith(prefix):
            old_name = table + '__' + name[len(prefix):] + '__index'
            old_name_2 = table + '_' + name[len(prefix):] + '_idx'
            current_c.execute("select ci.relname from pg_index join pg_class ci on indexrelid=ci.oid join pg_class c on indrelid=c.oid join pg_namespace on c.relnamespace=pg_namespace.oid where (ci.relname=%s or ci.relname=%s or ci.relname=%s) and nspname=%s and c.relname=%s", (old_name, old_name_2,) + row)
            index_names = current_c.fetchall()
            count = len(index_names)
            if count == 0:
                output("-- Can't find original index: %s (for %s)" % (old_name, name,))
            elif count == 1:
                cur_name = index_names[0][0]
                if cur_name != name:
                    output("ALTER INDEX %s.%s RENAME TO %s;" % (schema, cur_name, name,))
            else:
                output("-- Multiple occurrences of index: %s (for %s)" % (old_name, name,))
        else:
            output("-- Can't determine original index name: %s" % (name,))
    

if __name__ == '__main__':
    run(sys.argv[1], sys.argv[2])
