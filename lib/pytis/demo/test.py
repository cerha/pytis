#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2019-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2012 OUI Technology Ltd.
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

# The tests are expected to be performed on a freshly created pytis-demo
# database.

from __future__ import unicode_literals

import copy
import psycopg2 as dbapi

import pytis
import pytis.data
import pytis.extensions
import pytis.form
import pytis.presentation
import pytis.util

_configuration_file = 'pytis-demo-config.py'
_connection_data = {'database': 'pytis-demo'}
_parameters = copy.copy(_connection_data)
_parameters.update(dict(configuration_file=_configuration_file))
_dmp_config = pytis.extensions.DMPConfiguration(configuration_file=_configuration_file)
_resolver = _dmp_config.resolver()


class _TestBase:
    _BACKUP_TABLES = ('c_pytis_menu_actions', 'e_pytis_action_rights', 'e_pytis_menu',)

    def setup(self):
        self._resolver = _resolver
        self._orig_rights = self._read_rights()
        self._orig_menu = self._read_menu()
        for table in self._BACKUP_TABLES:
            try:
                self._query("drop table x%s" % (table,))
            except Exception:
                pass
            self._query("create table x%s as select * from %s" % (table, table,))

    def teardown(self):
        pytis.data.DBDataDefault._pg_connection_pool_.flush(
            pytis.data.DBDataDefault._postgresql_close_connection
        )
        self._query("insert into e_pytis_disabled_dmp_triggers (id) values ('genmenu')")
        try:
            reverse_tables = list(self._BACKUP_TABLES)
            reverse_tables.reverse()
            for table in reverse_tables:
                self._query("truncate %s cascade" % (table,))
            for table in self._BACKUP_TABLES:
                self._query("insert into %s (select * from x%s)" % (table, table,))
                self._query("drop table x%s" % (table,))
        finally:
            self._query("delete from e_pytis_disabled_dmp_triggers")
            self._commit()

    def _query(self, query):
        connection = dbapi.connect(**_connection_data)
        cursor = connection.cursor()
        cursor.execute(query)
        if query.startswith("select "):
            result = cursor.fetchall()
        else:
            result = None
        connection.commit()
        return result

    def _commit(self):
        for f in ('pytis_update_transitive_roles',
                  'pytis_update_actions_structure',
                  'pytis_update_summary_rights',
                  'pytis_update_rights_redundancy',):
            self._query("select %s()" % (f,))

    def _reload_rights(self):
        import pytis.form.application
        pytis.form.application.init_access_rights(pytis.data.DBConnection(**_connection_data))

    def _check_rights(self, expected_rights):
        for shortname, permission, column, permitted in expected_rights:
            assert pytis.form.action_has_access(shortname, permission, column) == permitted
            if shortname[:5] == 'form/':
                rights = self._resolver.specification(shortname[5:]).data_spec().access_rights()
                assert rights.permitted(permission, (pytis.config.dbuser,), column=column) == permitted

    def _read_rights(self):
        import pytis.form.application
        self._reload_rights()
        access_rights = pytis.form.application._access_rights
        spec_rights = pytis.presentation.Specification._access_rights
        return access_rights, spec_rights

    def _read_menu(self):
        data = pytis.data.dbtable('e_pytis_menu',
                                  ('menuid', 'name', 'title', 'position', 'next_position',
                                   'fullname', 'help', 'hotkey', 'locked'),
                                  pytis.data.DBConnection(**_connection_data))

        def process(row):
            return (row['position'].value(), row['name'].value(), row['title'].value(),
                    row['fullname'].value(),)
        return data.select_map(process, sort=('position',))

    def _compare_rights(self, old_rights, new_rights):
        def compare_access(rights_1, rights_2):
            diff = []
            for shortname, columns_permissions_1 in rights_1.items():
                columns_permissions_2 = rights_2.get(shortname)
                if columns_permissions_2 is None:
                    diff.append('A %s' % (shortname,))
                    continue
                for column, permissions_1 in columns_permissions_1.items():
                    permissions_2 = columns_permissions_2.get(column)
                    if permissions_2 is None:
                        diff.append('A %s %s' % (shortname, column,))
                        continue
                    p_diff = set(permissions_1).difference(set(permissions_2))
                    if p_diff:
                        diff.append('A %s %s %s' % (shortname, column, p_diff,))
            return diff

        def compare_spec(rights_1, rights_2):
            diff = []
            for shortname, spec_1 in rights_1.items():
                spec_2 = rights_2.get(shortname)
                if spec_2 is None:
                    diff.append('S %s' % (shortname,))
                    continue
                permissions_1 = spec_1._permission_table
                permissions_2 = spec_2._permission_table
                for permission, column_groups_1 in permissions_1.items():
                    column_groups_2 = permissions_2.get(permission)
                    if column_groups_2 is None:
                        diff.append('S %s %s' % (shortname, permission,))
                        continue
                    for column, groups_1 in column_groups_1.items():
                        groups_2 = column_groups_2.get(column)
                        if groups_2 is None:
                            diff.append('S %s %s %s' % (shortname, permission, column,))
                            continue
                        g_diff = set(groups_1).difference(set(groups_2))
                        if g_diff:
                            diff.append('S %s %s %s %s' % (shortname, permission, column, g_diff,))
            return diff
        return (compare_access(old_rights[0], new_rights[0]) + compare_spec(old_rights[1],
                                                                            new_rights[1]),
                compare_access(new_rights[0], old_rights[0]) + compare_spec(new_rights[1],
                                                                            old_rights[1]),)

    def _compare_menu(self, old_menu, new_menu):
        old = set(old_menu)
        new = set(new_menu)
        return old.difference(new), new.difference(old)

    def _check_no_change(self):
        changes = self._compare_rights(self._orig_rights, self._read_rights())
        assert all([not c for c in changes])
        changes = self._compare_menu(self._orig_menu, self._read_menu())
        assert all([not c for c in changes])


class TestBasic(_TestBase):

    def test_commit(self):
        pytis.extensions.dmp_commit(_parameters, False)
        self._check_no_change()


class TestMenu(_TestBase):

    def test_add(self):
        pytis.extensions.dmp_add_action(_parameters, False,
                                        'form/pytis.form.BrowseForm/misc.YProducts//',
                                        "Products", None)
        self._check_no_change()
        pytis.extensions.dmp_add_action(_parameters, False,
                                        'form/pytis.form.BrowseForm/misc.XProducts//',
                                        "Products", None)
        pytis.extensions.dmp_add_action(_parameters, False,
                                        'form/pytis.form.BrowseForm/misc.XProducts//',
                                        '2.1112.11235', None)
        changes = self._compare_menu(self._orig_menu, self._read_menu())
        assert not changes[0]
        assert len(changes[1]) == 2

    def test_delete(self):
        pytis.extensions.dmp_delete_menu(_parameters, False, '99999')
        self._check_no_change()
        pytis.extensions.dmp_add_action(_parameters, False,
                                        'form/pytis.form.BrowseForm/misc.XProducts//',
                                        '2.1112.11236', None)
        pytis.extensions.dmp_delete_menu(_parameters, False, '2.1112.11236')
        changes = self._compare_menu(self._orig_menu, self._read_menu())
        assert all([not c for c in changes])
        self._check_no_change()


class TestForms(_TestBase):
    pass


class TestRights(_TestBase):

    def test_reset(self):
        dbuser = pytis.config.dbuser
        pytis.extensions.dmp_change_rights(_parameters, False,
                                           (('form/misc.Products', dbuser, 'update',
                                             False, None, False,),
                                            ('form/cb.Countries', dbuser, 'view',
                                             False, None, False,),))
        pytis.extensions.dmp_commit(_parameters, False)
        self._read_rights()
        self._check_rights((('form/misc.Products', pytis.data.Permission.UPDATE, None, False),
                            ('form/cb.Countries', pytis.data.Permission.VIEW, None, False),))
        pytis.extensions.dmp_reset_rights(_parameters, False, 'form/misc.Products')
        pytis.extensions.dmp_reset_rights(_parameters, False, 'form/cb.Countries')
        self._check_no_change()

    def test_system(self):
        pytis.extensions.dmp_convert_system_rights(_parameters, False, 'form/misc.Products')
        self._check_no_change()
        assert self._query("select count(*) from e_pytis_action_rights "
                           "where system and shortname='form/misc.Products'")[0][0] == 0
        assert self._query("select count(*) from e_pytis_action_rights "
                           "where not system and shortname='form/misc.Products'")[0][0] > 0


class TestMix(_TestBase):
    # The idea is to perform a lot of dmp operations randomly selected from a
    # predefined list.  Each operation should be performed reversely sooner or
    # later.  The test should finish in the same state as it started (and
    # without any crash of course).
    pass


# Operations to be tested:
# def dmp_add_member(parameters, fake, member, role):
# def dmp_add_action(parameters, fake, fullname, position, title):
# def dmp_update_form(parameters, fake, specification, new_fullname):
# def dmp_rename_specification(parameters, fake, old_name, new_name):
# def dmp_delete_fullname(parameters, fake, fullname):
# def dmp_delete_shortname(parameters, fake, shortname):
# def dmp_change_rights(parameters, fake, requests):
# def dmp_copy_rights(parameters, fake, from_shortname, to_shortname):
