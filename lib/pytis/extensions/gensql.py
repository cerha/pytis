# -*- coding: utf-8 -*-

# Copyright (C) 2018 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2012-2013 Brailcom, o.p.s.
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


import copy
import inspect
import os
import re
import string
import sys
import types

import pytis.data
import pytis.util


class SQLException(Exception):
    pass


class SQLFlexibleValue(object):

    def __init__(self, name, default=None, environment=None):
        assert isinstance(name, basestring), name
        assert isinstance(environment, (basestring, types.NoneType,)), environment
        self._name = name
        self._default = default
        self._environment = environment

    def value(self):
        value = None
        name = None
        if self._environment is not None:
            name = os.getenv(self._environment)
        if name is None:
            value = globals().get(self._name)
        if value is None:
            if name is None:
                name = self._default
            value = _name_mapping[name]
            if value is None:
                raise SQLException("Unknown flexible reference", name)
        return value

public_schema = SQLFlexibleValue('default_schema', environment='GENSQL_DEFAULT_SCHEMA',
                                 default='public')
default_user = SQLFlexibleValue('default_user', environment='GENSQL_DEFAULT_USER',
                                default='public')

_name_mapping = {}


class _BasicMetaclass(type):
    def __new__(metaclass_, name, bases, dict_):
        class_ = type.__new__(metaclass_, name, bases, dict_)
        name = class_.name
        if name is None:
            name = pytis.util.camel_case_to_lower(class_.__name__)
        if name in _name_mapping and _name_mapping[name] is not class_:
            raise SQLException("Duplicate object name", (class_, _name_mapping[name],))
        _name_mapping[name] = class_
        return class_


class SQLDatabaseSpecification(object):
    __metaclass__ = _BasicMetaclass

    name = None
    schemas = (public_schema,)
    search_path = (public_schema,)
    old_names = ()

    _SQL_OBJECT = ''

    def __init__(self, cursor):
        super(SQLDatabaseSpecification, self).__init__()
        assert isinstance(self.name, basestring), self.name
        assert isinstance(self.schemas, tuple), self.schemas
        assert isinstance(self.search_path, tuple), self.search_path
        assert all([isinstance(x, (basestring, SQLFlexibleValue,)) for x in self.schemas],
                   self.schemas)
        assert all([isinstance(x, (basestring, SQLFlexibleValue,)) for x in self.search_path],
                   self.search_path)

    def sql_create_all(self):
        commands = ()
        for schema in self.search_path:
            commands = commands + self.sql_create(schema)
        return commands

    def sql_update_all(self, cursor):
        commands = ()
        for schema in self.search_path:
            commands = commands + self.sql_update(schema, cursor)
        return commands

    def sql_create(self, schema):
        return (self._sql_search_path(schema) +
                self._sql_create(schema))

    def sql_update(self, schema, cursor):
        if self.sql_changed(schema, cursor):
            return (self._sql_search_path(schema) +
                    self._sql_remove(schema) +
                    self._sql_create(schema))

    def sql_changed(self, schema):
        return True

    def dependencies(self):
        return set([self._sql_flexible_value(s) for s in self.schemas if s])

    def _sql_flexible_value(self, value):
        if isinstance(value, SQLFlexibleValue):
            value = value.value()
        return value

    def _sql_create(self, schema):
        return ()

    def _sql_remove(self, schema):
        if self._SQL_OBJECT:
            commands = ("DROP %s %s" % (self._SQL_OBJECT, self._pg_escape(self.name),),)
        else:
            commands = ()
        return commands

    def _sql_search_path(self, schema):
        path_list = [self._sql_id_escape(s) for s in (self._schema,) + self.search_path if s]
        path = string.join(path_list, ',')
        return ('SET SEARCH_PATH TO %s' % (path,),)

    def _sql_identification(self, schema):
        return '%s %s' % (self._SQL_OBJECT, self._sql_name(schema),)

    def _sql_name(self, schema, name=None):
        if name is None:
            name = self.name
        name = self._escape(name)
        if schema:
            name = '%s.%s' % (self._escape(schema), name,)
        return name

    def _sql_id_escape(self, identifier):
        identifier = self._sql_flexible_value(identifier)
        return '"%s"' % (identifier.replace('"', '""'),)

    def _sql_value_escape(self, value):
        if isinstance(value, basestring):
            value = "'%s'" % (value.replace("'", "''"),)
            if value.find('\\') >= 0:
                value = 'E%s' % (value.replace('\\', '\\\\'),)
        return value

    def _sql_value(self, type_, value):
        if isinstance(value, basestring):
            if __debug__:
                _value, error = type_.validate(value)
                assert error is not None, (type_, value, error,)
            output = self._sql_value_escape(value)
        elif isinstance(value, SQLFunctional):
            output = value.call(())
        else:
            output = str(value)
        return output


class SQLPredefined(SQLDatabaseSpecification):

    def sql_create(self, schema):
        if self._sql_predefined():
            return ()
        return super(SQLPredefined, self).sql_create(schema)

    def sql_update(self, schema, cursor):
        if self._sql_predefined():
            return ()
        return super(SQLPredefined, self).sql_update(schema, cursor)

    def sql_changed(self, schema):
        if self._sql_predefined():
            return False
        return super(SQLPredefined, self).sql_changed(schema)

    def _sql_predefined(self):
        return True


class SQLDocumented(SQLDatabaseSpecification):

    def sql_create(self, schema):
        return (super(SQLDocumented, self).sql_create(schema) +
                self._sql_documentation(schema))

    def _sql_documentation(self, schema):
        docstring = self.__doc__
        text = self._sql_value_escape(docstring)
        return ("COMMENT ON %s IS '%s'" % (self._sql_identification(schema), text,))


class SQLProprietary(SQLDocumented):

    owner = None
    rights = (('all', (default_user,),),)

    _AVAILABLE_RIGHTS = ('all',)

    def __init__(self):
        super(SQLProprietary, self).__init__()
        assert isinstance(self.owner, (types.NoneType, basestring, SQLFlexibleValue,)), self.owner
        assert isinstance(self.rights, tuple), self.rights
        if __debug__:
            for right_spec in self.rights:
                assert isinstance(right_spec, tuple) and len(right_spec) == 2, right_spec
                assert right_spec[0] in self._AVAILABLE_RIGHTS, right_spec
                users = right_spec[1]
                assert isinstance(users, tuple), right_spec
                for u in users:
                    assert isinstance(u, (basestring, SQLFlexibleValue,)), right_spec

    def sql_create(self, schema):
        return (super(SQLProprietary, self).sql_create(schema) +
                self._sql_create_owner(schema) +
                self._sql_create_grant(schema))

    def _sql_create_owner(self, schema):
        if self.owner is None:
            commands = ()
        else:
            commands = ("ALTER %s SET OWNER TO %s" % (self._sql_identification(schema),
                                                      self._sql_id_escape(self.owner),))
        return commands

    def _sql_create_grant(self, schema):
        commands = ()
        for right, users in self.rights:
            for user in users:
                commands = commands + (("GRANT %s on %s to %s" % (
                    right,
                    self._sql_identification(schema),
                    self._sql_id_escape(user),
                )),)
        return commands


class SQLSchema(SQLProprietary):

    schemas = ('',)

    _SQL_OBJECT = 'SCHEMA'
    _AVAILABLE_RIGHTS = ('all', 'create', 'usage',)

    def _sql_create(self, schema):
        if self.owner is None:
            owner = ''
        else:
            owner = " AUTHORIZATION %s" % (self._pg_escape(self.owner),)
        command = "CREATE SCHEMA %s%s" % (self._pg_escape(self.name), owner,)
        return (command,)


class Public(SQLPredefined, SQLSchema):
    name = 'public'


class SQLSequence(SQLProprietary):

    start = None
    minvalue = None
    maxvalue = None
    increment = None
    cycle = None

    _SQL_OBJECT = 'SEQUENCE'
    _AVAILABLE_RIGHTS = ('all', 'usage', 'select', 'update',)

    def __init__(self):
        super(SQLSequence, self).__init__()
        assert isinstance(self.start, (types.NoneType, int, long,)), self.start
        assert isinstance(self.minvalue, (types.NoneType, int, long,)), self.minvalue
        assert isinstance(self.maxvalue, (types.NoneType, int, long,)), self.maxvalue
        assert isinstance(self.increment, (types.NoneType, int, long,)), self.increment
        assert isinstance(self.cycle, (types.NoneType, bool,)), self.cycle

    def _sql_create(self, schema):
        command = "CREATE SEQUENCE %s" % (self._sql_name(schema),)
        if self.start is not None:
            command += " START %s" % (self.start,)
        if self.minvalue is not None:
            command += " MINVALUE %s" % (self.minvalue,)
        if self.maxvalue is not None:
            command += " MAXVALUE %s" % (self.maxvalue,)
        if self.increment is not None:
            command += " INCREMENT %s" % (self.increment,)
        if self.cycle:
            command += " CYCLE"
        return (command,)


class SQLIndex(SQLDocumented):

    columns = ()
    method = None

    _SQL_OBJECT = 'INDEX'

    def __init__(self):
        super(SQLIndex, self).__init__()
        assert self.method in (None, 'btree', 'hash', 'gist', 'gin',), self.method
        assert self.columns and isinstance(self.columns, tuple), self.columns
        if __debug__:
            for c in self.columns:
                assert isinstance(c, ColumnInfo), (self, c,)
            table = self.columns[0].table()
            for c in self.columns[1:]:
                assert c.table() is table, (self, c,)

    def dependencies(self):
        dependencies = super(SQLIndex, self).dependencies()
        dependencies.union(set([c.table() for c in self.columns]))
        return dependencies

    def _sql_create(self, schema):
        name = self._sql_name(schema)
        table = self.columns[0].table()
        method = self.method or 'btree'
        columns = string.join([c.id() for c in self.columns], ', ')
        command = ("CREATE INDEX %s ON %s USING %s (%s)" %
                   (name, table, method, columns,))
        return command


class Column(pytis.data.ColumnSpec):

    def __init__(self, id, type, doc=None, old_ids=(), name=None, unique=False, check=None,
                 default=None, references=None, key=False, index=False):
        super(Column, self).__init__(id, type)
        assert isinstance(doc, (types.NoneType, basestring,)), doc
        assert isinstance(old_ids, tuple), old_ids
        assert isinstance(name, (types.NoneType, basestring,)), name
        assert isinstance(unique, bool), unique
        assert isinstance(check, (types.NoneType, basestring,)), check
        assert isinstance(default, (types.NoneType, basestring, SQLFunctional,)), default
        if __debug__:
            if isinstance(default, basestring):
                _value, error = self.type().validate(default)
                assert not error, (default, error,)
        assert isinstance(references, (types.NoneType, SQLTable, ColumnInfo, Reference)), references
        assert isinstance(key, bool), key
        assert isinstance(index, (bool, dict,)), index
        if __debug__:
            if isinstance(index, dict):
                for k in index.keys():
                    assert k in ('method',), index
        self._doc = doc
        self._old_ids = old_ids
        if name is None:
            name = id
        self._name = name
        self._unique = unique
        self._check = check
        self._default = default
        if isinstance(references, SQLTable):
            key_column = SQLTable.key_column()
            assert key_column is not None, references
            references = ColumnInfo(id=key_column.id(), table=references)
        if isinstance(references, ColumnInfo):
            references = Reference(column=ColumnInfo)
        self._references = references
        self._key = key
        self._index = index

    def doc(self):
        return self._doc

    def old_ids(self):
        return self._old_ids

    def unique(self):
        return self._unique

    def check(self):
        return self._check

    def default(self):
        return self._default

    def references(self):
        return self._references

    def key(self):
        return self._key

    def index(self):
        return self._index

    def name(self):
        return self._name

    def rename(self, name, key=None):
        assert isinstance(name, basestring), name
        new = copy.copy(self)
        new._name = name
        if key is not None:
            new._key = key
        return new


class PrimaryColumn(Column):

    def __init__(self, id, type, doc=None, old_ids=(), name=None, check=None,
                 default=None, references=None):
        super(PrimaryColumn, self).__init__(self, id, type, doc=None, old_ids=(), name=None,
                                            check=None, default=None, references=None,
                                            key=True, unique=True)


class _AttributeDictionary(dict):
    def __getattr__(self, name):
        try:
            return self[name]
        except KeyError:
            raise AttributeError(name)


class _TabularMetaclass(_BasicMetaclass):
    def __new__(metaclass_, name, bases, dict_):
        class_ = _BasicMetaclass.__new__(metaclass_, name, bases, dict_)
        columns = [(c.id(), ColumnInfo(id=c, table=class_),) for c in class_.columns]
        class_.c = _AttributeDictionary(columns)
        return class_


class SQLTabular(SQLDatabaseSpecification):
    __metaclass__ = _TabularMetaclass

    columns = ()

    _SQL_CREATE_FILLER = 'AS '

    def __init__(self):
        super(SQLTabular, self).__init__()
        assert isinstance(self.columns, tuple), self.columns
        assert self.columns, self.columns
        if __debug__:
            for c in self.columns:
                assert isinstance(c, Column), c
        self._key_column = None
        for c in self.columns:
            if c.key():
                self._key_column = c
                break

    def _sql_create(self, schema):
        return (self._sql_def_intro(schema) +
                self._sql_def_columns() +
                self._sql_def_outro(schema),)

    def _sql_def_intro(self, schema):
        return "CREATE %s %s %s" % (self._SQL_OBJECT, self._sql_name(schema),
                                    self._SQL_CREATE_FILLER,)

    def _sql_def_outro(self, schema):
        return ""

    def _sql_def_columns(self, columns=None, long=True):
        if columns is None:
            columns = self.columns
        formatted = [self._sql_def_format_column(c) for c in columns]
        formatted += self._sql_def_extra_columns()
        separator = ',\n' if long else ', '
        return "(%s)" % (string.join(formatted, separator),)

    def _sql_def_extra_columns(self):
        return []

    def _sql_def_format_column(self, column):
        return self.sql_column(column)

    def _sql_documentation(self, schema):
        commands = ["COMMENT ON COLUMN %s.%s IS '%s'" %
                    (self._sql_name(schema), c.id(), self._sql_value_escape(c.doc()),)
                    for c in self.columns if c.doc()]
        return tuple(commands)

    def sql_table_column(self, column, schema):
        return '%s.%s' % (self._sql_name(schema), self.sql_column(column),)

    def sql_column(self, column):
        return self._sql_id_escape(column.id())

    def sql_column_type(self, column):
        type_ = column.type()
        if type_.__class__ == pytis.data.String:
            minlen = type_.minlen()
            maxlen = type_.maxlen()
            if maxlen is None:
                result = 'text'
            elif maxlen == minlen:
                result = 'char(%d)' % (maxlen,)
            else:
                result = 'varchar(%d)' % (maxlen,)
        elif type_.__class__ == pytis.data.Float:
            digits = type_.digits()
            if digits:
                precision = type_.precision() or 0
                result = 'numeric(%d, %d)' % (digits, precision,)
            else:
                result = 'numeric'
        else:
            MAPPING = {pytis.data.Integer:   'int',
                       pytis.data.Serial:    'serial',
                       pytis.data.Oid:       'oid',
                       pytis.data.Boolean:   'bool',
                       pytis.data.DateTime: 'timestamp(0)',
                       pytis.data.Date:      'date',
                       pytis.data.Time:      'time',
                       pytis.data.Binary:    'bytea',
                       pytis.data.Image:     'bytea',
                       pytis.data.Color:     'varchar(7)',
                       pytis.data.LTree:    'ltree',
                       }
            try:
                result = MAPPING[type_.__class__]
            except KeyError:
                raise SQLException("Unsupported column type", (self.name, column,))
        return result

    @classmethod
    def columns_except(class_, *exceptions):
        return [c for c in class_().columns if c not in exceptions]


class ColumnInfo(pytis.util.Structure):
    _attributes = (pytis.util.Attribute('id', type=basestring),
                   pytis.util.Attribute('table', type=SQLTabular),
                   )


class Reference(pytis.util.Structure):
    _attributes = (pytis.util.Attribute('column', type=ColumnInfo),
                   pytis.util.Attribute('update', type=basestring, default=None),
                   pytis.util.Attribute('delete', type=basestring, default=None),
                   )

    def __init__(self, *args, **kwargs):
        super(Reference, self).__init__(*args, **kwargs)
        assert self._update in (None, 'cascade',), self._update
        assert self._delete in (None, 'cascade',), self._delete


class _TableMetaclass(_TabularMetaclass):

    def __new__(metaclass_, name, bases, dict_):
        class_ = _TabularMetaclass.__new__(metaclass_, name, bases, dict_)
        class_._indexes = []
        for c in class_.columns:
            index = c.index()
            if index is True:
                index = {}
            if isinstance(index, dict):
                class Index(SQLIndex):
                    name = '%s__%s__index' % (class_.name, c.id(),)
                    columns = (ColumnInfo(id=c.id(), table=class_),)
                    method = index.get('method')
                class_._indexes.append(Index)
        return class_


class SQLTable(SQLTabular):
    __metaclass__ = _TableMetaclass

    inherits = ()
    tablespace = None
    init_columns = None
    init_values = ()
    check = ()
    unique = ()
    with_oids = False

    _SQL_OBJECT = 'TABLE'
    _SQL_CREATE_FILLER = ''

    def __init__(self):
        super(SQLTable, self).__init__()
        assert isinstance(self.tablespace, (types.NoneType, basestring, SQLFlexibleValue,)), \
            self.tablespace
        assert isinstance(self.inherits, tuple)
        if __debug__:
            for i in self.inherits:
                assert isinstance(i, SQLTable), (self, i,)
        assert isinstance(self.init_columns, (types.NoneType, tuple,)), self.init_columns
        assert isinstance(self.init_values, tuple), self.init_values
        if __debug__ and self.init_values:
            n = len(self.init_values[0])
            for row in self.init_values:
                assert isinstance(row, tuple), row
                assert len(row) == n, (row, n,)
            if self.init_columns is None:
                assert n == len(self.columns), (n, self.columns,)
            else:
                assert n == len(self.init_columns), (n, self.init_columns,)
                for c in self.init_columns:
                    assert isinstance(c, basestring), c
        if self.init_columns is None:
            self.init_columns = [c.id() for c in self.columns]
        assert isinstance(self.check, tuple), self.check
        if __debug__:
            for c in self.check:
                assert isinstance(c, basestring), c
        assert isinstance(self.unique, tuple), self.unique
        if __debug__:
            column_ids = [c.id() for c in self.columns]
            for unique in self.unique:
                assert unique, self.unique
                for u in unique:
                    assert isinstance(u, basestring), u
                    assert u in column_ids, (u, column_ids,)
        assert isinstance(self.with_oids, bool), self.with_oids

    def dependencies(self):
        dependencies = super(SQLTable, self).dependencies()
        dependencies = dependencies.union(set(self.inherits))
        dependencies = dependencies.union(set([c.references()[0]
                                               for c in self.columns if c.references()]))
        return dependencies

    def key_column(self):
        return self._key_column

    def _sql_create(self, schema):
        commands = self._sql_create(schema)
        for c in self.columns:
            doc = c.doc()
            if c.doc:
                sql_column = self.sql_table_column(c, schema)
                sql_comment = self._sql_value_escape(doc),
                commands.append("COMMENT ON COLUMN %s IS %s" % (sql_column, sql_comment,))
        if self.init_values:
            table_name = self._sql_name(schema)
            columns = string.join([self._sql_id_escape(c) for c in self.init_columns], ', ')
            for row in self.init_values:
                values = [self._sql_value(c.type(), v)
                          for c, v in zip(self.init_columns, self.init_values,)]
                commands.append("INSERT INTO %s (%s) VALUES (%s)" %
                                (table_name, columns, values,))
        return commands

    def _sql_def_outro(self, schema):
        outro = "%s OIDS" % ("WITH" if self.with_oids else "WITHOUT",)
        if self.inherits:
            # how to handle `schema' here?
            table_list = string.join([t._sql_name(None) for t in self.inherits], ', ')
            outro += "\nINHERITS(%s)" % (table_list,)
        if self.tablespace:
            outro += "\nTABLESPACE %s" % (self._sql_id_escape(self.tablespace),)
        return outro

    def _sql_def_format_column(self, column):
        sql = '%s %s' % (self.sql_column(column), self.sql_column_type(column),)
        if column is self._key_column:
            sql += " PRIMARY KEY"
        else:
            if column.type().not_null():
                sql += " NOT NULL"
            if column.unique():
                sql += " UNIQUE"
        if column.check():
            sql += " CHECK (%s)" % (column.check(),)
        if column.default() is not None:
            sql += " DEFAULT %s" % (self._sql_value(column.default()),)
        reference = column.references()
        if reference is not None:
            column_info = reference.column_info()
            # how to handle schema here?
            sql += " REFERENCES %s(%s)" % (column_info.table()._sql_name(), column_info.id(),)
            if reference.update():
                sql += " ON UPDATE %s" % (reference.update(),)
            if reference.delete():
                sql += " ON DELETE %s" % (reference.delete(),)
        return sql

    def _sql_def_extra_columns(self):
        extra = []
        for unique in self.unique:
            extra.append("UNIQUE (%s)" % (string.join(unique, ', '),))
        for check in self.check:
            extra.append("CHECK (%s)" % (check,))
        return extra


class SQLView(SQLTabular):

    # columns = (...) -- table column, view column, function column, direct value
    #  it may be a column directly, or a named column created from something

    # pytis like condition, combined with (outer) joins as "pytis condition macros"
    condition = None

    # how about schemas (of inserted tables etc.)?

    insert = None  # or a sequence of SQLTable's or a sequence of columns
    update = None
    delete = None

    # class ...(SQLRule): ...

    def dependencies(self):
        dependencies = super(SQLView, self).dependencies()
        dependencies = dependencies.union(set(self.inherits))
        dependencies = dependencies.union(set([c.references()[0]
                                               for c in self.columns if c.references()]))
        return dependencies


class SQLSet(SQLTabular):

    kind = 'union'
    tables = ()
    # ...


class SQLType(SQLTabular):

    _SQL_OBJECT = 'TYPE'
    _AVAILABLE_RIGHTS = ()

    def _sql_def_format_column(self, column):
        return '%s %s' % (self.sql_column(column), self.sql_column_type(column),)


class SQLFunctional(SQLTabular):

    arguments = ()
    return_type = None
    multirow = False
    security_definer = False
    stability = 'volatile'
    depends_on = ()

    _LANGUAGE = None

    def __init__(self):
        if issubclass(self.return_type, (SQLType, SQLTable,)):
            self.columns = self.return_type.columns
        super(SQLFunctional, self).__init__()
        assert isinstance(self.setof, bool), self.setof
        assert isinstance(self.security_definer, bool), self.security_definer
        assert self.stability in ('immutable', 'stable', 'volatile',)
        assert (self.return_type is None or
                issubclass(self.return_type, (pytis.data.Type, SQLType, SQLTable,))), \
            self.return_type
        assert isinstance(self.arguments, (types.NoneType, tuple,)), self.arguments
        if __debug__:
            if self.arguments:
                for a in self.arguments:
                    assert isinstance(a, Column), a
        if self.return_type is None and len(self.columns) > 1:
            self._auto_return_type = self._sql_create_return_type()
        else:
            self._auto_return_type = None
        assert isinstance(self.depends_on, tuple), self.depends_on
        if __debug__:
            for d in self.depends_on:
                assert isinstance(d, SQLDatabaseSpecification), (self, d,)

    def dependencies(self):
        dependencies = super(SQLFunctional, self).dependencies()
        if self.return_type is not None:
            dependencies.add(self.return_type)
        dependencies = dependencies.union(self.depends_on)
        return dependencies

    def sql_call(self, arguments=(), schema=None):
        assert isinstance(arguments, tuple), arguments
        assert len(arguments) == len(self.arguments or ()), (arguments, self.arguments,)
        if self.arguments is None:
            argument_list = ''
        else:
            values = [str(a) if isinstance(a, basestring) else self._sql_value_escape(a)
                      for a in arguments]
            argument_list = '(%s)' % (string.join(values, ', '),)
        return "%s%s" % (self._sql_name(schema), argument_list,)

    def sql_create(self, schema):
        commands = ()
        if self._auto_return_type is not None:
            commands = commands + self._auto_return_type.sql_create(schema)
        commands = commands + super(SQLFunctional, self).sql_create(schema)
        return commands

    def _sql_remove(self, schema):
        commands = super(SQLFunctional, self)._sql_remove(schema)
        if self._auto_return_type is not None:
            commands = commands + self._auto_return_type._sql_remove(schema)
        return commands

    def _sql_create_return_type(self):
        class Type(SQLType):
            name = 't_%s' % (self.name,)
            schemas = self.schemas
            search_path = self.search_path
            columns = self.columns
        return Type()

    def _sql_create(self, schema):
        name = self._sql_name(schema)
        arguments = self._sql_def_columns(columns=self.arguments, long=False)
        if self.return_type is None and self._auto_return_type is None:
            if self.columns:
                assert len(self.columns) == 1, self.columns
                return_type = self.sql_column_type(self.columns[0])
            else:
                assert not self.multirow, self
                return_type = "void"
        else:
            return_type = (self.return_type or self._auto_return_type)._sql_name(schema)
        if self.multirow:
            return_type = 'SETOF ' + return_type
        body = self._sql_body()
        command = ("CREATE FUNCTION %s %s RETURNS %s AS $$ %s $$ LANGUAGE %s %s" %
                   (name, arguments, return_type, body, self._LANGUAGE, self.stability,))
        if self.security_definer:
            command += ' SECURITY DEFINER'
        return (command,)

    def _sql_body(self):
        return ''


class SQLSystemFunction(SQLPredefined, SQLFunctional):
    # e.g. current_date, session_user
    arguments = None


class SQLFunctionalRaw(SQLFunctional):
    body = ''

    def __init__(self):
        super(SQLFunctionalRaw, self).__init__()
        assert isinstance(self.body, basestring), self.body
        assert isinstance(self.arguments, tuple), self.arguments

    def _sql_body(self):
        return self.body


class SQLFunction(SQLFunctionalRaw):
    _LANGUAGE = 'sql'


class SQLPlFunction(SQLFunctionalRaw):
    _LANGUAGE = 'plpgsql'


class SQLPyFunction(SQLPredefined, SQLFunctional):
    _LANGUAGE = 'plpythonu'
    _STATICMETHOD_MATCHER = re.compile('( *)@staticmethod\r?\n?', re.MULTILINE)

    def __init__(self):
        super(SQLPyFunction, self).__init__()
        assert not self.name or self.name in dir(self), self
        assert isinstance(self.arguments, tuple), self.arguments

    def _sql_predefined(self):
        return not self.name

    def _sql_body(self):
        arglist = string.join([c.id() for c in self.arguments], ', ')
        lines = ['def %s(%s):' % (self.name, arglist,),
                 '    %s = args' % (arglist,)]  # hard-wired indentation
        main_lines = self._method_source_lines(self.name, 0)
        main_lines = main_lines[1:]
        prefix = 'sub_'
        for name in dir(self):
            if name.startswith(prefix):
                function_lines = self._method_source_lines(name, 4)  # hard-wired forev^h^h now
                first_line = function_lines[0]
                i = first_line.find(prefix)
                j = i + len(prefix)
                function_lines[0] = first_line[:i] + first_line[j:]
                lines += function_lines
        lines += main_lines
        return string.join(lines, '\n')

    def _method_source_lines(self, name, indentation):
        method = getattr(self, name)
        try:
            lines = inspect.getsourcelines(method)[0]
        except Exception as e:
            raise SQLException("Invalid plpythonu method", (self.__class__.__name__, name, e))
        match = self._STATICMETHOD_MATCHER.match(lines[0])
        if not match:
            self.SQLException("@staticmethod decorator not found", (self.__class__.__name__, name))
        indentation = indentation - len(match.group(1))
        if indentation == 0:
            def reindent(line):
                return line
        elif indentation > 0:
            def reindent(line):
                return ' '*indentation + line
        else:
            def reindent(line):
                return line[-indentation:]
        lines = [l.rstrip() for l in lines[1:]]
        return [reindent(l) for l in lines if l.strip()]


class _TableHookMetaclass(_TabularMetaclass):

    def __new__(metaclass_, name, bases, dict_):
        class_ = _TabularMetaclass.__new__(metaclass_, name, bases, dict_)
        table = class_.table
        if table is None:
            name = class_.__name__
            module = sys.modules[class_.__module__]
            for c in module.__dict__.values():
                if getattr(c, name, None) is class_:
                    class_.table = c
                    break
            # else:
            #     raise SQLException("Can't find associated table", (class_,))
        return class_


class SQLEventHandler(SQLFunctional):
    __metaclass__ = _TableHookMetaclass

    table = None

    def __init__(self):
        super(SQLEventHandler, self).__init__()
        assert issubclass(self.table, SQLTabular), self.table
        # This is not required for triggers, but we don't use trigger function
        # arguments in practice and it simplifies things:
        assert len(self.arguments) == 0, self.arguments

    def dependencies(self):
        dependencies = super(SQLEventHandler, self).dependencies()
        dependencies = dependencies.union(self.table)
        return dependencies

    def _sql_events(self):
        return string.join(self.events, ' or ')


class Trigger(pytis.data.Type):
    pass


class SQLTrigger(SQLEventHandler):

    return_type = Trigger()
    position = 'after'
    each_row = True
    events = ('insert', 'update', 'delete', 'truncate',)

    def __init__(self):
        super(SQLTrigger, self).__init__()
        assert isinstance(self.return_type, Trigger), self.return_type
        assert self.position in ('after', 'before',), self.position
        assert isinstance(self.each_row, bool), self.each_row
        assert isinstance(self.events, tuple), self.events
        assert len(self.events) > 0, self.events
        if __debug__:
            for e in self.events:
                assert e in ('insert', 'update', 'deleted', 'truncate',), e

    def _trigger_name(self, schema):
        return self._sql_name(schema, name=self._name+'__trigger')

    def _sql_create_trigger(self, schema):
        trigger_name = self._trigger_name(schema)
        function_name = self._sql_name(schema)
        table_name = self.table._sql_name(schema)
        events = self._sql_events()
        scope = 'ROW' if self.each_row else 'STATEMENT'
        return ("CREATE TRIGGER %s %s %s ON %s FOR EACH %s EXECUTE PROCEDURE %s()" %
                (trigger_name, self.position, events, table_name, scope, function_name,),)

    def _sql_remove_trigger(self, schema):
        trigger_name = self._trigger_name(schema)
        table_name = self.table._sql_name(schema)
        return ("DROP TRIGGER %s ON %s" % (trigger_name, table_name,),)

    def _sql_create(self, schema):
        return super(SQLFunctional, self)._sql_remove(schema) + self._sql_create_trigger(schema)

    def _sql_remove(self, schema):
        return self._sql_remove_trigger(schema) + super(SQLFunctional, self)._sql_remove(schema)


class SQLRule(SQLEventHandler):

    events = ('insert', 'update', 'delete',)
    kind = 'also'

    def __init__(self):
        super(SQLRule, self).__init__()
        assert isinstance(self.events, tuple), self.events
        assert len(self.events) > 0, self.events
        if __debug__:
            for e in self.events:
                assert e in ('insert', 'update', 'deleted',), e
        assert self.kind in ('also', 'instead',), self.kind

    def _sql_create(self, schema):
        name = self._sql_name(schema)
        table_name = self.table._sql_name(schema)
        events = self._sql_events()
        body = self._sql_body()
        return ("CREATE RULE %s AS ON %s TO %s DO %s ( %s )" %
                (name, events, table_name, self.kind, body,),)

    def _sql_remove(self, schema):
        rule_name = self._sql_name(schema)
        table_name = self.table._sql_name(schema)
        return ("DROP RULE %s ON %s" % (rule_name, table_name,),)


class SQLRaw(SQLDatabaseSpecification):

    body = ""
    depends_on = ()

    def __init__(self):
        super(SQLRaw, self).__init__()
        assert isinstance(self.body, basestring), self.body
        assert isinstance(self.depends_on, tuple), self.depends_on
        if __debug__:
            for d in self.depends_on:
                assert isinstance(d, SQLDatabaseSpecification), d

    def dependencies(self):
        dependencies = super(SQLRaw, self).dependencies()
        dependencies = dependencies.union(self.depends_on)
        return dependencies

    def _sql_create(self):
        return self.body
