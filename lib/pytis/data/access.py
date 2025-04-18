# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2017 OUI Technology Ltd.
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

"""Access rights handling.

Everything what is needed to define and handle access permissions to the data
objects is put here.  The set of allowed access rights is defined in the
'Permission' class.  The access rights checking mechanism is implemented in the
class 'AcessRights'.  Access to data objects is regulated in the
'RestrictedData' class.

Access rights violation is signalized exclusively using the
'DataAccessException' class.

"""
from __future__ import print_function

from past.builtins import basestring

import pytis
from pytis.util import EVENT, log, remove_duplicates, translations, xtuple
from .data import Data, EQ, MemData, Row
from .types_ import sval

_ = translations('pytis-data')


class Permission(object):
    """Enumerator of permission constants.

    Not all the constants here make sense for all data objects.  For instance,
    it makes no sense to specify 'DELETE' permission for a table column, or the
    user commands will mostly use 'CALL' permission.

    """

    VIEW = 'VIEW'
    """Right to view the contents."""
    INSERT = 'INSERT'
    """Right to insert new records."""
    UPDATE = 'UPDATE'
    """Right to change already existing data."""
    DELETE = 'DELETE'
    """Right to delete data."""
    CALL = 'CALL'
    """Right to execute."""
    EXPORT = 'EXPORT'
    """Right to make CSV export."""
    PRINT = 'PRINT'
    """Right to print the form.

    The following rules apply when using DMP:

    - Print actions are always permitted or forbidden based on their own PRINT
      permissions, regardless of the related form PRINT permissions.

    - If a form doesn't have PRINT permission, the user can't create and edit
      user output templates.

    - If a side form doesn't have PRINT permission, it is not available in user
      output templates in Binding substitutions.

    """
    ALL = 'ALL'
    """All rights to the given object."""

    @classmethod
    def all_permissions(class_):
        """Return tuple of all the non-general permissions constants."""
        return (class_.VIEW, class_.INSERT, class_.UPDATE, class_.DELETE,
                class_.CALL, class_.EXPORT, class_.PRINT,)


class AccessRights(object):
    """Access rights specification."""

    def __init__(self, *access_rights):
        """
        Arguments:

          access_rights -- each of the arguments is a tuple of tuples of the
            form (COLUMNS, (GROUPS, PERMISSIONS, ...), ...), where COLUMNS is
            a column name or a sequence of column names (as strings) or 'None',
            GROUPS is a group name or a sequence of group names or 'None' and
            PERMISSIONS is a sequence of 'Permission' class constants

        PERMISSIONS makes corresponding actions allowed, anything what is not
        allowed is forbidden.  When COLUMN or GROUP is 'None', it's an implicit
        value that applies to the given column or group respectively, if no
        more specific permission is defined for it.  Implicit permissions are
        added to the explicit permissions given to a column or group, so they
        can be extended but not limited.  If a column name is 'False', it
        applies only to those columns which don't have any own rights defined
        for the given permission.

        If a column or a group within a column is given multiple times, the
        corresponding permissions are added together.

        """
        self._specification = access_rights
        self._permission_table = self._build_permission_table(access_rights)
        self._query_cache = {}

    def _build_permission_table(self, access_rights):
        table = {}
        all_permissions = Permission.all_permissions()
        for p in all_permissions:
            table[p] = {}
        for a in access_rights:
            columns, groupdefs = xtuple(a[0]), a[1:]
            for p in all_permissions:
                table_p = table[p]
                for c in columns:
                    if c not in table_p:
                        table_p[c] = ()
                if True not in table_p:
                    table_p[True] = ()
            for gd in groupdefs:
                groups, permissions = xtuple(gd[0]), gd[1:]
                if Permission.ALL in permissions:
                    permissions = all_permissions
                for p in permissions:
                    table_p = table[p]
                    for c in columns:
                        if None in groups:
                            table_p[c] = (None,)
                            table_p[True] = (None,)
                        elif c in table_p:
                            table_p[c] = table_p[c] + groups
                            table_p[True] = table_p[True] + groups
                        else:
                            table_p[c] = groups
                            table_p[True] = table_p.get(True, ()) + groups
        return table

    def _permitted(self, permission, groups, column):
        if groups is None:
            # If user groups can't be retrieved, allow it
            return True
        ok_groups = self.permitted_groups(permission, column)
        return (None in ok_groups) or any(g in ok_groups for g in groups)

    def permitted(self, permission, groups, column=None):
        """Return true iff any of 'groups' has got 'permission'.

        Arguments:

          permission -- required permission, one of the 'Permission' class
            constants except of 'Permission.ALL'
          groups -- sequence of group names (as strings); permission is valid
            if at least one of the listed groups has got the permission
          column -- name of the column (as a string) to test the permission
            against, or 'None' in which case implicit rights are tested, or
            'True' to check that any of the columns has the permission

        """
        assert isinstance(column, basestring) or column in (None, True, False,), column
        key = (permission, xtuple(groups), column)
        try:
            result = self._query_cache[key]
        except KeyError:
            result = self._query_cache[key] = self._permitted(permission, groups, column)
        return result

    def _permitted_groups(self, permission, column):
        permsets = self._permission_table[permission]
        groups = permsets.get(column, ())
        if isinstance(column, basestring):
            for sets in self._permission_table.values():
                if column in sets:
                    break
            else:
                groups += permsets.get(False, ())
        return groups

    def permitted_groups(self, permission, column):
        """Return list of groups with 'permission' to 'column'.

        Arguments:

          permission -- asked permission, one of the 'Permission' class
            constants except of 'Permission.ALL'
          column -- name of the column (as a string) to test the permission
            against, or 'None' in which case implicit rights are tested

        """
        groups = self._permitted_groups(permission, column)
        if column is None:
            groups += self._permitted_groups(permission, True)
        else:
            groups += self._permitted_groups(permission, None)
        return remove_duplicates(list(groups))

    def specification(self):
        """Return original specification given in the constructor."""
        return self._specification


class DBAccessRights(AccessRights):
    """Access rights retrieved from a database.


    The access rights are read from the database only when initializing the
    instance, they are not checked for later updates.

    """

    def __init__(self, object_name, connection_data=None):
        """
        Arguments:

          object-name -- symbolic identifier (as a string) of the access rights
            in the database
          connection_data -- connection parameters specification

        """
        access_rights = self._build_access_rights(object_name, connection_data)
        super(DBAccessRights, self).__init__(*access_rights)

    def _build_access_rights(self, object_name, connection_data):
        import pytis.data
        access_rights = []
        bindings = [pytis.data.DBColumnBinding(name, 'pytis.access_rights', name)
                    for name in ('id', 'object', 'column_', 'group_', 'permission')]
        key = bindings[0]
        data = pytis.data.DBDataDefault(bindings, key,
                                        connection_data=connection_data)
        try:
            data.select(condition=EQ('object', sval(object_name)))
            while True:
                row = data.fetchone()
                if row is None:
                    break
                access_rights.append((row['column_'].value(),
                                      (row['group_'].value(),
                                       row['permission'].value())))
        finally:
            try:
                data.close()
            except Exception:
                pass
        return access_rights


class RestrictedData(Data):
    """Data object with restricted access to its operations."""

    def __init__(self, access_rights=AccessRights((None, (None, Permission.ALL))), **kwargs):
        """
        Arguments:

          access_rights -- 'AccessRights' instance defining access rights to
            the object

        """
        self._access_rights = access_rights
        super(RestrictedData, self).__init__(access_rights=access_rights,
                                             **kwargs)

    def access_groups(self):
        """Return tuple of the user's groups.

        In this class the method returns an empty tuple.

        """
        return ()

    def _check_access_columns(self, columns, permission):
        groups = self.access_groups()
        for c in columns:
            if not self._access_rights.permitted(permission, groups,
                                                 column=c):
                table = self._bindings[0].table()
                raise DataAccessException(permission, table=table, column=c)

    def _check_access_condition_columns(self, condition):
        # Toto původně byla lokální funkce v `_check_access_condition'.
        # Jenomže je rekurzivní, což vede k tomu, že nemůže být uvolněna (sice
        # nechápu, proč ji neuvolní garbage collector, když hlásí, že nejsou
        # žádné neuvolnitelné objekty, ale prostě je to tak) a dochází
        # k jejímu hromadění bez uvolnění pro každou novou instanci datového
        # objektu.
        if condition is None:
            return []
        elif condition.logical():
            result = []
            for a in condition.args():
                result += self._check_access_condition_columns(a)
            return result
        elif condition.name == 'IN':
            column, data, table_column, table_condition, table_arguments = condition.args()
            # Toto nefunguje pro vzdálený přístup, ale nelze svítit...
            data._check_access_columns(table_column)
            data._check_access_condition(table_condition)
            return [column]
        else:
            return [condition.args()[0]]

    def _check_access_condition(self, condition):
        return True

    def _check_access_sorting(self, sorting):
        return True

    def _check_access_key(self):
        return True

    def _check_access_delete(self):
        self._check_access_columns([None], Permission.DELETE)

    def _access_filter_row(self, row, permission=Permission.VIEW):
        if row is None:
            return row
        groups = self.access_groups()
        rights = self._access_rights
        if permission == Permission.VIEW or rights.permitted(permission, groups):
            return row
        filtered_items = [item for item in row.items()
                          if rights.permitted(permission, groups, column=item[0])]
        return Row(filtered_items)

    def permitted(self, column_id, permission):
        """Return true iff the user may access the given column.

        Arguments:

          column_id -- id (as a string) of the column checked for access; it
            may be also 'None' meaning global column access right is checked
          permission -- the permission to be checked, one of the 'Permission'
            class constants

        """
        return self._access_rights.permitted(permission, self.access_groups(),
                                             column=column_id)

    # Předefinované metody.
    # Je nutno dbát opatrnosti u conditions a dalších argumentů, protože ty
    # umožňují získávat informace o datech nepřímo.

    def row(self, key, **kwargs):
        self._check_access_key()
        row = super(RestrictedData, self).row(key, **kwargs)
        return self._access_filter_row(row)

    def select(self, condition=None, sort=(), check_condition=True, **kwargs):
        if check_condition:
            self._check_access_condition(condition)
        self._check_access_sorting(sort)
        return super(RestrictedData, self).select(condition=condition,
                                                  sort=sort, **kwargs)

    def fetch(self, *args, **kwargs):
        row = super(RestrictedData, self).fetch(*args, **kwargs)
        return self._access_filter_row(row)

    def search(self, condition, **kwargs):
        self._check_access_condition(condition)
        return super(RestrictedData, self).search(condition, **kwargs)

    def insert(self, row, **kwargs):
        row = self._access_filter_row(row, Permission.INSERT)
        resrow, result = super(RestrictedData, self).insert(row, **kwargs)
        return self._access_filter_row(resrow), result

    def update(self, key, row, **kwargs):
        self._check_access_key()
        row = self._access_filter_row(row, Permission.UPDATE)
        resrow, result = super(RestrictedData, self).update(key, row, **kwargs)
        return self._access_filter_row(resrow), result

    def update_many(self, condition, row, **kwargs):
        self._check_access_condition(condition)
        row = self._access_filter_row(row, Permission.UPDATE)
        return super(RestrictedData, self).update_many(condition, row, **kwargs)

    def delete(self, key, **kwargs):
        self._check_access_key()
        self._check_access_delete()
        return super(RestrictedData, self).delete(key, **kwargs)

    def delete_many(self, condition, **kwargs):
        self._check_access_condition(condition)
        self._check_access_delete()
        return super(RestrictedData, self).delete_many(condition, **kwargs)


class RestrictedMemData(RestrictedData, MemData):
    """Memory data class with access rights support.

    The primary purpose of this class is not limiting access rights to memory
    data, but support of the access rights checking protocol that allows to use
    memory data objects in forms (so called virtual forms not bound to a
    database table).

    """

    def __init__(self, columns, **kwargs):
        super(RestrictedMemData, self).__init__(columns=columns, **kwargs)


class DataAccessException(Exception):
    """Exception raised on access rights violation."""

    def __init__(self, permission, table=None, column=None):
        """
        Arguments:

          permission -- the missing permission, one of the 'Permission' class
            constants
          table -- name of the table which couldn't be accessed, string or
            'None'
          column -- name of the column which couldn't be accessed, string or
            'None'

        """
        log(EVENT, 'Access violation attempt:',
            (pytis.config.dbconnection.user(), permission, table, column))
        Exception.__init__(self, _(u"Access denied"), permission, table, column)


def is_in_groups(access_groups):
    """Return true iff the current user is a member of any of the groups.

    Arguments:

      'access_groups' -- sequence of group names (as strings) to test the user
        against, or 'None'; if it is 'None' then true is returned
        unconditionally.

    """
    from pytis.data import default_access_groups
    groups = default_access_groups(pytis.config.dbconnection)
    if ((groups is None or access_groups is None or
         any(g in groups for g in xtuple(access_groups)))):
        return True
    else:
        return False
