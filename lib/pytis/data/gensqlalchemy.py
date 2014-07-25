# -*- coding: utf-8 -*-

# Copyright (C) 2012, 2013, 2014 Brailcom, o.p.s.
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

"""Specifications of database objects.

Database objects are specified as classes.  The classes inherit superclasses
specific for particular kinds of database objects.  The classes define database
objects by specifying their properties in class attributes or methods.  The
following main specification classes are available: 'SQLSchema', 'SQLSequence',
'SQLTable', 'SQLView', 'SQLType', 'SQLFunction', 'SQLPlFunction',
'SQLPyFunction', 'SQLTrigger'.

Database specifications are based on SQLAlchemy Python library.  In many places
you can (and should) use SQLAlchemy constructs.

It is important to distinguish between specification classes and their
instances.  All database objects are specified as classes.  This makes
specification inheritence easy.  Also a single specification class may define
multiple database objects, typically a table specification may define the same
table to be created in multiple schemas.  Particular database objects (e.g. a
single table in a given schema) are represented by specification instances.
Those instances are automatically created when a specification class is
defined.

When defining a specification you can refer to other specifications.  Usually
you use specification classes in such places.  But sometimes it is necessary to
use specification instances, in particular, SQLAlchemy can work only with
specification instances.  You may not create specification instances directly.
But they are available through instances of auxiliary classes 'TableLookup',
'ColumnLookup' and 'ReferenceLookup' (available under short names 't', 'c' and
'r').

Specifications are defined in a Python source file.  Nothing else than
specification classes and corresponding utilities should be put into the file.
Names of specification classes may not start with 'SQL' and '_SQL' prefixes,
those prefixes are reserved for base classes.  Specifications whose names start
with underscore or 'Base_' prefix don't emit database objects, they are
typically used as common base classes to be used by real specifications.  You
can put specifications to several files in a Pytho module and then include them
all in the corresponding '__init__.py' file.

SQL code can be generated from the specifications using 'gsql_module()'
function.

An example specification is available in 'pytis/doc/examples/dbdefs/', you can
see all the most important constructs there.

"""

from __future__ import unicode_literals

try:
    import alembic.ddl.base
except ImportError:
    alembic = None

import codecs
import collections
from contextlib import contextmanager
import copy
import cStringIO
import imp
import inspect
import os
import re
import string
import sqlalchemy
from sqlalchemy.ext.compiler import compiles
import sys
import types

import pytis.data
import pytis.util


# SQLAlchemy extensions

G_CONVERT_THIS_FUNCTION_TO_TRIGGER = object()  # hack for gensql conversions

def _function_arguments_seq(function, types_only=False):
    search_path = function.search_path()
    def arg(column):
        a_column = column.sqlalchemy_column(search_path, None, None, None)
        in_out = 'out ' if column.out() and not types_only else ''
        if types_only:
            name = ''
        else:
            name = a_column.name
            if name:
                name = '"%s" ' % (name,)
        return '%s%s%s' % (in_out, name, a_column.type.compile(_engine.dialect),)
    return [arg(c) for c in function.arguments]

def _function_arguments(function, types_only=False):
    return string.join(_function_arguments_seq(function, types_only=types_only), ', ')

def _role_string(role):
    if role is True:
        role_string = 'PUBLIC'
    else:
        role_string = '"%s"' % (role,)
    return role_string
    
class _PytisSchemaHandler(object):

    def _set_search_path(self, search_path):
        path_list = [_sql_id_escape(s) for s in search_path]
        path = string.join(path_list, ',')
        command = 'SET SEARCH_PATH TO %s' % (path,)
        self.connection.execute(command)
        return search_path
    
class _PytisSchemaGenerator(sqlalchemy.engine.ddl.SchemaGenerator, _PytisSchemaHandler):

    def visit_view(self, view, create_ok=False):
        replace = 'OR REPLACE ' if view._REPLACE_ON_CREATE else ''
        command = 'CREATE %s%s "%s"."%s" AS\n' % (replace, view._DB_OBJECT, view.schema, view.name,)
        with _local_search_path(self._set_search_path(view.search_path())):
            query = view.query()
            query.pytis_prefix = command
            self.connection.execute(query)
            view.dispatch.after_create(view, self.connection, checkfirst=self.checkfirst,
                                       _ddl_runner=self)
            if hasattr(view, 'indexes'):
                for index in view.indexes:
                    self.traverse_single(index)

    def visit_materialized_view(self, view, create_ok=False):
        self.visit_view(view, create_ok=create_ok)

    def visit_type(self, type_, create_ok=False):
        with _local_search_path(self._set_search_path(type_.search_path())):
            self.make_type(type_.name, type_.fields)
            type_.dispatch.after_create(type_, self.connection, checkfirst=self.checkfirst,
                                        _ddl_runner=self)

    def make_type(self, type_name, columns):
        sqlalchemy_columns = [c.sqlalchemy_column(None, None, None, None) for c in columns]
        def ctype(c):
            return c.type.compile(_engine.dialect)
        column_string = string.join(['%s %s' % (self.preparer.format_column(c), ctype(c),)
                                     for c in sqlalchemy_columns],
                                    ', ')
        command = 'CREATE TYPE %s AS\n(%s)' % (type_name, column_string,)
        self.connection.execute(command)

    def visit_function(self, function, create_ok=False, result_type=None, suffix=''):
        search_path = function.search_path()
        with _local_search_path(self._set_search_path(search_path)):
            if isinstance(function.result_type, (tuple, list,)):
                self.make_type('t_' + function.pytis_name() + suffix, function.result_type)
            query_prefix, query_suffix = \
                function._pytis_header_footer(result_type=result_type, search_path=search_path,
                                              suffix=suffix)
            cost = function.execution_cost
            if cost is not None:
                assert isinstance(cost, int) and cost > 0, cost
                query_suffix += ' COST %s' % (cost,)
            rows = function.expected_rows
            if rows is not None:
                assert isinstance(rows, int) and rows > 0 and function.multirow, rows
                query_suffix += ' ROWS %s' % (rows,)
            for name, value in function.set_parameters:
                assert isinstance(name, basestring), name
                if value is None:
                    str_value = 'FROM CURRENT'
                elif name.lower() == 'time zone':
                    str_value = str(value)
                elif isinstance(value, (int, long, float, basestring,)):
                    str_value = 'TO %s' % (value,)
                else:
                    raise Exception("Unrecognized function parameter value", value)
                query_suffix += ' SET %s %s' % (name, str_value,)
            body = function.body()
            if isinstance(body, basestring):
                body = body.strip()
                command = query_prefix + body + query_suffix
                self.connection.execute(command)
            elif isinstance(body, (tuple, list,)):
                n = len(body)
                for i in range(n):
                    query = body[i]
                    if i == 0:
                        query.pytis_prefix = query_prefix
                    if i == n - 1:
                        query.pytis_suffix = query_suffix
                    self.connection.execute(query)
            else:
                query = body
                query.pytis_prefix = query_prefix
                query.pytis_suffix = query_suffix
                self.connection.execute(query)
            function.dispatch.after_create(function, self.connection, checkfirst=self.checkfirst,
                                           _ddl_runner=self)

    def visit_aggregate(self, aggregate, create_ok=False):
        search_path = aggregate.search_path()
        with _local_search_path(self._set_search_path(search_path)):
            self.visit_function(aggregate, create_ok=create_ok, suffix='_agg')
            name = aggregate.pytis_name(real=True)
            arguments = _function_arguments_seq(aggregate, types_only=True)
            if len(arguments) < 2:
                arguments.append('*')
            settings = ['sfunc=%s_agg' % (name,),
                        'stype=%s' % (arguments[0],)]
            init_value = aggregate.initial_value
            if init_value is not None:
                if isinstance(init_value, basestring):
                    init_value = "'%s'" % (init_value.replace("'", "''"),)
                settings.append('initcond=%s' % (init_value,))
            command = (('CREATE AGGREGATE "%s"."%s" (%s) (\n'
                        '%s\n)') %
                       (aggregate.schema, name, string.join(arguments[1:], ', '),
                        string.join(settings, ', '),))
            self.connection.execute(command)
            aggregate.dispatch.after_create(aggregate, self.connection, checkfirst=self.checkfirst,
                                            _ddl_runner=self)

    def visit_trigger(self, trigger, create_ok=False):
        for search_path in _expand_schemas(trigger):
            with _local_search_path(self._set_search_path(search_path)):
                if isinstance(trigger, (SQLPlFunction, SQLPyFunction,)):
                    trigger._DB_OBJECT = 'FUNCTION' # hack for comments
                    try:
                        self.visit_function(trigger, create_ok=create_ok, result_type='trigger')
                    finally:
                        trigger._DB_OBJECT = 'TRIGGER'
                if trigger.table is not None and trigger.events:
                    events = string.join(trigger.events, ' OR ')
                    table = object_by_path(trigger.table.name, trigger.search_path())
                    row_or_statement = 'ROW' if trigger.each_row else 'STATEMENT'
                    trigger_call = trigger(*trigger.arguments)
                    command = (('CREATE TRIGGER "%s" %s %s ON "%s"\n'
                                'FOR EACH %s EXECUTE PROCEDURE %s') %
                               (trigger.pytis_name(real=True), trigger.position, events,
                                table.pytis_name(real=True), row_or_statement, trigger_call,))
                    self.connection.execute(command)
                trigger.dispatch.after_create(trigger, self.connection, checkfirst=self.checkfirst,
                                              _ddl_runner=self)

    def visit_raw(self, raw, create_ok=False):
        self._set_search_path(raw.search_path())
        message = """-- Don't use raw constructs anymore.
-- Never, really never, introduce new raw constructs.
-- If there is a missing feature in gensqlalchemy, report it.
-- But don't introduce new raw constructs just because you think it's necessary.
-- If you try to use an existing raw construct, rewrite it to a regular
-- gensqlalchemy construct instead.  If you think the situation is exceptional
-- enough to mandate use of the raw construct, see above.
"""
        if raw.error_level == 0:
            sql = ""
        else:
            sql = message
        if raw.error_level <= 1:
            sql += raw.sql()
            if raw.error_level > 0:
                sql += "\n-- I repeat:\n--\n" + message
        self.connection.execute(sql)

class _PytisSchemaDropper(sqlalchemy.engine.ddl.SchemaGenerator, _PytisSchemaHandler):

    def visit_view(self, view, create_ok=False):
        command = 'DROP %s "%s"."%s"' % (view._DB_OBJECT, view.schema, view.name,)
        self.connection.execute(command)

    def visit_materialized_view(self, view, create_ok=False):
        self.visit_view(view, create_ok=create_ok)
        
    def visit_type(self, type_, create_ok=False):
        command = 'DROP TYPE "%s"."%s"' % (type_.schema, type_.name,)
        self.connection.execute(command)

    def visit_function(self, function, create_ok=False, result_type=None):
        name = function.pytis_name(real=True)
        arguments = _function_arguments(function)
        command = 'DROP FUNCTION "%s"."%s" (%s)' % (function.schema, name, arguments,)
        self.connection.execute(command)

    def visit_aggregate(self, aggregate, checkfirst=False):
        name = aggregate.pytis_name(real=True)
        arguments = _function_arguments(aggregate, types_only=True)[1:]
        command = 'DROP AGGREGATE "%s"."%s" (%s)' % (aggregate.schema, name, arguments,)
        self.connection.execute(command)

    def visit_trigger(self, trigger, checkfirst=False):
        for search_path in _expand_schemas(trigger):
            with _local_search_path(self._set_search_path(search_path)):
                table = trigger.table
                if table is not None:
                    self.connection.execute('DROP TRIGGER "%s" ON "%s"' %
                                            (trigger.pytis_name(real=True),
                                             table.pytis_name(real=True),))
        
class _ObjectComment(sqlalchemy.schema.DDLElement):
    def __init__(self, obj, kind, comment):
        self.object = obj
        self.kind = kind
        self.comment = comment
@compiles(_ObjectComment)
def visit_object_comment(element, compiler, **kw):
    o = element.object
    kind = element.kind
    schema = '\"%s\".' % (o.schema,)
    if o._DB_OBJECT == 'TRIGGER':
        extra = ' ON \"%s\"' % (o.table.pytis_name(real=True),)
        schema = ''
    elif isinstance(o, SQLFunctional):
        extra = '(%s)' % (_function_arguments(o),)
        kind = o._DB_OBJECT
    else:
        extra = ''
    return ("COMMENT ON %s %s\"%s\"%s IS '%s'" %
            (kind, schema, o.pytis_name(real=True), extra,
             element.comment.replace("'", "''"),))
        
class _ColumnComment(sqlalchemy.schema.DDLElement):
    def __init__(self, table, field):
        self.table = table
        self.field = field
@compiles(_ColumnComment)
def visit_column_comment(element, compiler, **kw):
    return ("COMMENT ON COLUMN \"%s\".\"%s\".\"%s\" IS '%s'" %
            (element.table.schema, element.table.name, element.field.id(),
             element.field.doc().replace("'", "''"),))

class _AccessRight(sqlalchemy.schema.DDLElement):
    def __init__(self, obj, right, group):
        self.object = obj
        self.right = right
        self.group = group
@compiles(_AccessRight)
def visit_access_right(element, compiler, **kw):
    o = element.object
    name = o.pytis_name()
    if isinstance(o, SQLSchematicObject):
        schema = '"%s".' % (o.schema,)
    else:
        schema = ''
    if isinstance(o, SQLFunctional):
        extra = '(%s)' % (_function_arguments(o),)
    else:
        extra = ''
    if isinstance(o, SQLView):
        kind = ''                       # PostgreSQL requires this
    else:
        kind = o._DB_OBJECT + ' '
    return ("GRANT %s ON %s%s%s%s TO %s" %
            (element.right, kind, schema, name, extra, _role_string(element.group),))

class _Rule(sqlalchemy.schema.DDLElement):
    def __init__(self, table, action, instead_commands, also_commands):
        self.table = table
        self.action = action
        self.instead_commands = instead_commands
        self.also_commands = also_commands
@compiles(_Rule)
def visit_rule(element, compiler, **kw):
    instead_commands = element.instead_commands()
    also_commands = element.also_commands()
    if instead_commands is None and not also_commands:
        return
    # We have to attach also_commands to instead_commands because
    # also_commands may contain references to NEW and OLD which are not
    # available in ALSO rules.
    kind = 'ALSO' if instead_commands is None and also_commands else 'INSTEAD'
    commands = tuple(instead_commands or ()) + tuple(also_commands or ())
    if commands:
        sql_commands = [_make_sql_command(c) for c in commands]
        if len(sql_commands) == 1:
            sql = sql_commands[0]
        else:
            sql = '(%s)' % (string.join(sql_commands, '; '),)
    else:
        sql = 'NOTHING'
    table = element.table
    rule_name = '%s__%s_%s' % (table.name, element.action.lower(), kind.lower(),)
    return ('CREATE OR REPLACE RULE "%s" AS ON %s TO "%s"."%s"\nDO %s %s' %
            (rule_name, element.action, table.schema, table.name, kind, sql,))

class FullOuterJoin(sqlalchemy.sql.Join):
    __visit_name__ = 'full_outer_join'
    def __init__(self, left, right, onclause=None):
        super(FullOuterJoin, self).__init__(left, right, onclause=onclause, isouter=True)
@compiles(FullOuterJoin)
def visit_full_outer_join(join, compiler, asfrom=False, **kwargs):
    return (
        join.left._compiler_dispatch(compiler, asfrom=True, **kwargs) +
        " FULL OUTER JOIN " +
        join.right._compiler_dispatch(compiler, asfrom=True, **kwargs) +
        " ON " +
        join.onclause._compiler_dispatch(compiler, **kwargs)
    )

# Based on an example from SQLAlchemy manual:
class InsertFromSelect(sqlalchemy.sql.expression.Executable,
                       sqlalchemy.sql.expression.ClauseElement):
    def __init__(self, table, select):
        self.table = table
        self.select = select
@compiles(InsertFromSelect)
def visit_insert_from_select(element, compiler, **kwargs):
    column_list = ['"%s"' % (c.name,) for c in element.select.c]
    return "INSERT INTO %s (%s) (%s)" % (
        compiler.process(element.table, asfrom=True),
        string.join(column_list, ', '),
        compiler.process(element.select)
    )

# Based on PGDialect.get_columns which unfortunately doesn't handle types:
def _get_columns(dialect, connection, relation_name, schema):
    # This is a hack, but we don't want to parse type column types ourselves.
    SQL_COLS = """
        SELECT a.attname,
          pg_catalog.format_type(a.atttypid, a.atttypmod),
          (SELECT substring(pg_catalog.pg_get_expr(d.adbin, d.adrelid)
            for 128)
            FROM pg_catalog.pg_attrdef d
           WHERE d.adrelid = a.attrelid AND d.adnum = a.attnum
           AND a.atthasdef)
          AS DEFAULT,
          a.attnotnull, a.attnum, a.attrelid as table_oid
        FROM pg_catalog.pg_attribute a
        JOIN pg_catalog.pg_class c ON a.attrelid = c.oid
        JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
        WHERE c.relname = :relation_name AND n.nspname = :schema
        AND a.attnum > 0 AND NOT a.attisdropped
        ORDER BY a.attnum
    """
    if getattr(dialect, '_pytis_extra_types_initialized', False) is False:
        for name, class_ in (('ltree', LTreeType),
                             ('oid', OID),
                             ('int4range', INT4RANGE),
                             ('int8range', INT8RANGE),
                             ('numrange', NUMRANGE),
                             ('tsrange', TSRANGE),
                             ('tstzrange', TSTZRANGE),
                             ('daterange', DATERANGE),
                             ('bpchar', sqlalchemy.String), # == char()
                             ):
            if name not in dialect.ischema_names:
                dialect.ischema_names[name] = class_
        dialect._pytis_extra_types_initialized = True
    s = sqlalchemy.text(SQL_COLS,
                        bindparams=[sqlalchemy.bindparam('relation_name', type_=sqlalchemy.String),
                                    sqlalchemy.bindparam('schema', type_=sqlalchemy.String)],
                        typemap={'attname': sqlalchemy.Unicode, 'default': sqlalchemy.Unicode})
    c = connection.execute(s, relation_name=relation_name, schema=schema)
    rows = c.fetchall()
    domains = dialect._load_domains(connection)
    enums = dialect._load_enums(connection)
    columns = []
    for name, format_type, default, notnull, attnum, table_oid in rows:
        column_info = dialect._get_column_info(name, format_type, default, notnull,
                                               domains, enums, schema)
        columns.append(column_info)
    return columns

class _SQLExternal(sqlalchemy.sql.expression.FromClause):

    def __init__(self, name):
        super(_SQLExternal, self).__init__()
        self.name = name

    class _PytisColumn(object):
        def __init__(self, name):
            self.name = name
        def __getattr__(self, name, *args, **kwargs):
            if name.startswith('_'):
                raise AttributeError(name)
            column = '%s.%s' % (self.name, name,)
            return sqlalchemy.literal_column(column)
        def quote(self):
            return True

    def _pytis_column(self):
        return self._PytisColumn(self.name)
    c = property(_pytis_column)

    class _PytisAlias(sqlalchemy.sql.Alias):
        def __init__(self, selectable, name, c):
            sqlalchemy.sql.Alias.__init__(self, selectable, name)
            self._pytis_c = c
        def _pytis_column(self):
            return self._pytis_c
        c = property(_pytis_column)
        def get_children(self):
            return (self.element,)

    def alias(self, name):
        return self._PytisAlias(self, name, self._PytisColumn(name))

class NAME(sqlalchemy.String):
    pass
@compiles(NAME, 'postgresql')
def compile_name(element, compiler, **kwargs):
    return 'NAME'
@compiles(NAME)
def compile_name(element, compiler, **kwargs):
    return 'VARCHAR(64)'

class SERIAL(sqlalchemy.Integer):
    # SQLAlchemy currently doesn't support explicit SERIAL types.
    pass
@compiles(SERIAL)
def compile_serial(element, compiler, **kwargs):
    return 'SERIAL'

class BIGSERIAL(sqlalchemy.BigInteger):
    pass
@compiles(BIGSERIAL)
def compile_bigserial(element, compiler, **kwargs):
    return 'BIGSERIAL'

class OID(sqlalchemy.Integer):
    pass
@compiles(OID)
def compile_oid(element, compiler, **kwargs):
    return 'oid'

@compiles(sqlalchemy.String, 'postgresql')
def compile_string(element, compiler, **kwargs):
    if element.length is None:
        return 'TEXT'
    else:
        return 'VARCHAR(%d)' % (element.length,)

class LTreeType(sqlalchemy.types.UserDefinedType):

    def get_col_spec(self):
        return 'ltree'

    def bind_processor(self, dialect):
        def process(value):
            return value
        return process

    def result_processor(self, dialect, coltype):
        def process(value):
            return value
        return process

class _RangeType(sqlalchemy.types.UserDefinedType):

    def bind_processor(self, dialect):
        def process(value):
            return value
        return process

    def result_processor(self, dialect, coltype):
        def process(value):
            return value
        return process

class INT4RANGE(_RangeType):
    def get_col_spec(self):
        return 'int4range'
        
class INT8RANGE(_RangeType):
    def get_col_spec(self):
        return 'int8range'

class NUMRANGE(_RangeType):
    def get_col_spec(self):
        return 'numrange'

class TSRANGE(_RangeType):
    def get_col_spec(self):
        return 'tsrange'

class TSTZRANGE(_RangeType):
    def get_col_spec(self):
        return 'tstzrange'
        
class DATERANGE(_RangeType):
    def get_col_spec(self):
        return 'daterange'
                
@compiles(sqlalchemy.schema.CreateTable, 'postgresql')
def visit_create_table(element, compiler, **kwargs):
    result = compiler.visit_create_table(element, **kwargs)
    table = element.element
    if table.inherits:
        inherited = [table._table_name(t) for t in table.inherits]
        result = '%s\nINHERITS (%s)\n\n' % (result.rstrip(), string.join(inherited, ', '),)
    return result

_ANY_REGEXP = re.compile('.*')
@compiles(sqlalchemy.sql.expression.Alias)
def visit_alias(element, compiler, **kwargs):
    orig_legal_characters = compiler.preparer.legal_characters
    if element.description.find('(') >= 0:
        # Column aliases may not be quoted
        compiler.preparer.legal_characters = _ANY_REGEXP
        # For older versions of SQLAlchemy:
        element.quote = False
    try:
        return compiler.visit_alias(element, **kwargs)
    finally:
        element.legal_characters = orig_legal_characters

@compiles(sqlalchemy.dialects.postgresql.TIMESTAMP)
def visit_TIMESTAMP(element, compiler, **kwargs):
    # Current SQLAlchemy implementation is buggy: it omits zero precision.
    precision = getattr(element, 'precision', None)
    return "TIMESTAMP%s %s" % ('' if precision is None else "(%d)" % (precision,),
                               (element.timezone and "WITH" or "WITHOUT") + " TIME ZONE",)


# Columns
        
class Column(pytis.data.ColumnSpec):
    """Specification of a database column.

    It can be used anywhere new column specification is needed, typically in
    tables or function arguments.  In many other places, typically in views, it
    is possible to use already defined columns and using already defined
    columns is preferred.

    Unlike standalone database objects such as tables, columns are not
    specified by specification classes but by instances of 'Column' class.
    Column specifications are also not derived from 'sqlalchemy.Column' class,
    they are transformed to 'sqlalchemy.Column' instance inside gensqlalchemy.
    When you later retrieve columns from other objects such as tables or views,
    they are already presented as instances of 'sqlalchemy.Column'.

    Column properties are specified in the class constructor.  Class public
    methods serve for internal purposes of gensqlalchemy and there are not
    useful for specifications.  Instead you can inspect 'sqlalchemy.Column'
    instances when needed in certain places using 'sqlalchemy.Column' public
    methods.
    
    """
    def __init__(self, name, type, label=None, doc=None, unique=False, check=None,
                 default=None, references=None, primary_key=False, index=False, out=False,
                 original_column=None):
        """
        Arguments:

          name -- name of the column; string containing only lower case English
            letters and underscores.  Empty strings are permitted in special
            cases where the column name doesn't matter such as in SQL function
            arguments, however even in such cases it is recommended to use a
            proper column name as a form of documentation.
          type -- column type; instance of 'pytis.data.Type' subclass
          label -- default label to use in user interfaces; basestring or
            'None'.  This is a presentation attribute and is ignored in
            database schemas.
          doc -- documentation of the column; basestring or 'None'
          unique -- if true then the column is marked as unique (if it makes
            sense at the given place); boolean
          check -- if not 'None' then it defines SQL construct to use as a
            check on the given column; basestring
          default -- default column value; the value type must be compatible
            with SQLAlchemy requirements for given column type.  When you want
            to use a database function result as the default value, use
            'sqlalchemy.text' construct.
          references -- if not 'None' then it defines column that should be
            referenced by this 'SQLTable' column.  The value may be the
            referenced column, the referenced table (then its primary key is
            used) or 'ReferenceLookup' instance.  Additional keyword arguments
            from 'sqlalchemy.ForeignKey' constructor (e.g. 'onupdate') may be
            used, in such a case wrap the target object by 'Arguments' class.
          primary_key -- if true then the column is marked as part of the table
            primary key; boolean.  It makes sense only in 'SQLTable'
            definitions.  You can also use 'PrimaryColumn' class instead of
            setting this argument, it is equivalent and you can use either the
            argument or 'PrimaryColumn' class, but it is recommended to be
            consistent within an application.
          index -- if True then a default index should be created for this
            'SQLTable' column; if a dictionary then it contains a single key
            'method' whose value determines indexing method to be used for this
            'SQLTable' column; if False then no index creation is requested by
            the column specification (however the index may still be created
            based on other information or specification)
          out -- when the column is an 'SQLFunction' argument, the value
            defines whether the given function argument is an output argument;
            boolean
          original_column -- original column of this column;
            'sqlalchemy.Column' instance.  Useful in views where the column
            origin must be known for some reason.
          
        """
        assert label is None or isinstance(label, basestring), label
        assert doc is None or isinstance(doc, basestring), doc
        assert isinstance(unique, bool), unique
        assert check is None or isinstance(check, basestring), check
        assert isinstance(primary_key, bool), primary_key
        assert isinstance(index, (bool, dict,)), index
        assert isinstance(out, bool), out
        assert original_column is None or isinstance(original_column, sqlalchemy.Column), \
            original_column
        if (((primary_key or original_column is not None and original_column.primary_key)
             and (not type.not_null() or not type.unique()))):
            type = type.clone(type.__class__(not_null=True, unique=True))
        pytis.data.ColumnSpec.__init__(self, name, type)
        self._label = label
        self._doc = doc
        self._unique = unique
        self._check = check
        self._default = default
        self._references = references
        self._primary_key = primary_key
        self._index = index
        self._out = out
        self._original_column = original_column

    def label(self):
        return self._label

    def doc(self):
        return self._doc

    def default(self):
        return self._default

    def references(self):
        return self._references

    def index(self):
        return self._index

    def primary_key(self):
        return self._primary_key

    def out(self):
        return self._out

    def original_column(self):
        return self._original_column

    def sqlalchemy_column(self, search_path, table_name, key_name, orig_table_name, inherited=False,
                          foreign_constraints=None, check_constraints=None):
        alchemy_type = self.type().sqlalchemy_type()
        args = []
        references = self._references
        if references is not None:
            if not isinstance(references, Arguments):
                references = Arguments(references)
            r_args = references.args()
            kwargs = copy.copy(references.kwargs())
            if isinstance(r_args[0], (ReferenceLookup.Reference, ReferenceLookup.ColumnLookup,
                                      _Reference,)):
                dereference = r_args[0].get()
                if dereference is None:
                    kwargs['use_alter'] = True
                    kwargs['name'] = '%s__r__' % (table_name,)
                    f = _ForwardForeignKey(search_path, self.id(), r_args[0],
                                           r_args[1:], kwargs, None)
                    foreign_constraints.append(f)
                else:
                    r_args = (dereference,) + r_args[1:]
            if not kwargs.get('use_alter'):
                foreign_key = sqlalchemy.ForeignKey(*r_args, **kwargs)
                args.append(foreign_key)
        if self._check:
            constraint = sqlalchemy.CheckConstraint(self._check)
            if check_constraints is not None:
                # In case the constraint contains non-ASCII characters, it has
                # to be created as a table constraint otherwise SQLAlchemy
                # would output incorrectly encoded.
                for c in self._check:
                    if ord(c) >= 128:
                        check_constraints.append(constraint)
                        break
                else:
                    args.append(constraint)
        if self._index and not isinstance(self._index, dict):
            index = True
        else:
            index = False
        default = self._default
        if isinstance(default, (bool, float, int,)):
            default = sqlalchemy.text(repr(default))
        autoincrement = isinstance(alchemy_type, (SERIAL, BIGSERIAL))
        column = sqlalchemy.Column(self.id(), alchemy_type, *args,
                                   server_default=default,
                                   doc=self._doc, index=index,
                                   nullable=(not self.type().not_null()),
                                   primary_key=(self._primary_key and not inherited),
                                   unique=self._unique,
                                   info=dict(inherited=inherited),
                                   autoincrement=autoincrement)
        column.pytis_orig_table = orig_table_name
        column.type.pytis_orig_type = self.type()
        return column

class PrimaryColumn(Column):
    """Specification of a column that is part of the primary key.

    Instances of this class are automatically handled as primary keys or their
    components.  Otherwise there is no difference to 'Column' class.

    """
    def __init__(self, *args, **kwargs):
        kwargs = copy.copy(kwargs)
        kwargs['primary_key'] = True
        super(PrimaryColumn, self).__init__(*args, **kwargs)

class _TabularType(pytis.data.Type):

    class _SqlAlchemyType(sqlalchemy.types.UserDefinedType):

        def __init__(self, tabular):
            super(_TabularType._SqlAlchemyType, self).__init__()
            self._pytis_tabular = tabular

        def get_col_spec(self):
            return self._pytis_tabular.pytis_name(real=True)

        def bind_processor(self, dialect):
            def process(value):
                return value
            return process

        def result_processor(self, dialect, coltype):
            def process(value):
                return value
            return process
    
    def __init__(self, tabular):
        super(_TabularType, self).__init__()
        self._orig_tabular = tabular
        self._tabular = self._SqlAlchemyType(tabular)

    def sqlalchemy_type(self):
        return self._tabular

    def tabular(self):
        return self._orig_tabular

class Argument(Column):
    """Specification of a function argument.

    This is basically the same as 'Column', but it additionaly allows
    specifying '_SQLTabular' subclass as the argument type.
    
    """
    def __init__(self, name, type_, *args, **kwargs):
        if isinstance(type_, type) and issubclass(type_, _SQLTabular):
            type_ = _TabularType(type_)
        super(Argument, self).__init__(name, type_, *args, **kwargs)
    

# Utilities
    
def _error(message, error=True):
    prefix = (u'Error' if error else u'Warning')
    sys.stderr.write(u'%s: %s\n' % (prefix, message,))

def _warn(message):
    _error(message, error=False)

_current_search_path = None
@contextmanager
def _local_search_path(search_path):
    assert isinstance(search_path, (tuple, list,)), search_path
    global _current_search_path
    orig_search_path = _current_search_path
    _current_search_path = search_path
    yield
    _current_search_path = orig_search_path

@contextmanager
def _metadata_connection(metadata):
    connection = metadata.pytis_engine.connect()
    try:
        yield connection
    finally:
        connection.close()
    
def _sql_id_escape(identifier):
    return '"%s"' % (identifier.replace('"', '""'),)

def _sql_value_escape(value):
    if value is None:
        result = 'NULL'
    elif isinstance(value, (int, long, float,)):
        result = value
    elif isinstance(value, basestring):
        result = "'%s'" % (value.replace('\\', '\\\\').replace("'", "''"),)
    else:
        result = unicode(value)
    return result

def _sql_plain_name(name):
    pos = name.rfind('.')
    if pos >= 0:
        name = name[pos + 1:]
    return name

def _is_specification_name(name):
    return (not name.startswith('SQL') and
            not name.startswith('_') and
            not name.startswith('Base_'))

_enforced_schema = None
_enforced_schema_objects = None
def _expand_schemas(cls):
    schemas = cls.schemas
    if isinstance(schemas, SQLFlexibleValue):
        schemas = schemas.value()
    expanded_schemas = []
    for search_path in schemas:
        expanded_path = []
        for s in search_path:
            if isinstance(s, basestring):
                pass
            elif issubclass(s, SQLSchema):
                for o in _PytisSimpleMetaclass.objects:
                    if isinstance(o, s):
                        s = o.pytis_name()
                        break
                else:
                    raise SQLException("Schema instance not found", s)
            else:
                raise SQLException("Invalid schema reference", s)
            expanded_path.append(s)
        if _enforced_schema and cls in _enforced_schema_objects:
            expanded_schemas.append([_enforced_schema] + expanded_path)
            break
        else:
            expanded_schemas.append(expanded_path)
    return expanded_schemas

class SQLFlexibleValue(object):
    """Flexible definition of a value.

    Using instances of this class it is possible to define values which can be
    overridden.  Typical use is in schema definitions or access rights where it
    is useful to use different values in different databases.

    The rules for retrieving the value are defined in constructor.  The current
    value can be retrieved using 'value()' method.

    """
    _values = {}
    
    def __init__(self, name, default=None, environment=None):
        """
        Arguments:

          name -- name of the value; string.  It defines the default name of
            the variable holding the value.
          default -- the default value
          environment -- name of the environment variable containing the name
            of the variable holding the value; string or 'None'

        See also 'value()'.

        """
        assert isinstance(name, basestring), name
        assert isinstance(environment, (basestring, types.NoneType,)), environment
        self._name = name
        self._default = default
        self._environment = environment

    def value(self, globals_=None):
        """Return current object value.

        The value is retrieved as follows:

        - If environment variable given in the constructor exists and contains
          something then its contents defines the name of the variable holding
          the value.  The name is looked up in globals and the corresponding
          value is returned.

        - Otherwise if name given in the constructor exists in globals, return
          its value.

        - Otherwise the default value given in the constructor is returned.

        Arguments:

          globals_ -- dictionary where to look the value for; if 'None' then
            'globals()' is used

        """
        value = None
        name = None
        if self._environment is not None:
            name = os.getenv(self._environment)
        if name is None:
            name = self._name
        value = globals() if globals_ is None else globals_
        value = copy.copy(value)
        value.update(self._values)
        for name_part in name.split('.'):
            if isinstance(value, dict):
                value = value.get(name_part)
            else:
                value = getattr(value, name_part, None)
            if value is None:
                value = self._default
                break
        return value

    @classmethod
    def set_value(class_, name, value):
        """Set flexible value 'name' to 'value'.

        The set value takes priority over other value sources.

        Arguments:

          name -- name of the flexible value; string
          value -- value bound to the name
        
        """
        class_._values[name] = value

_default_schemas = SQLFlexibleValue('default_schemas', environment='GSQL_DEFAULT_SCHEMAS',
                                    default=(('public',),))

_metadata = sqlalchemy.MetaData()

class SQLException(Exception):
    """Exception raised on errors in specification processing."""

class SQLNameException(SQLException):
    def __init__(self, *args):
        super(SQLNameException, self).__init__("Object not found", *args)

class _PytisBaseMetaclass(sqlalchemy.sql.visitors.VisitableType):
    
    _name_mapping = {}
    _class_mapping = {}
    
    def __init__(cls, clsname, bases, clsdict):
        cls._gsql_file, cls._gsql_line = inspect.stack()[2][1:3]
        if cls._is_specification(clsname):
            name = cls.pytis_name()
            name_specs = _PytisBaseMetaclass._name_mapping.get(name)
            if name_specs is None:
                name_specs = _PytisBaseMetaclass._name_mapping[name] = set([cls])
            elif cls not in name_specs:
                cls_schemas = set(cls.object_schemas())
                for spec in name_specs:
                    if set(spec.object_schemas()).intersection(cls_schemas):
                        raise SQLException("Duplicate object name",
                                           (cls, _PytisBaseMetaclass._name_mapping[name],))
                name_specs.add(cls)
            _PytisBaseMetaclass._class_mapping[cls.__name__] = cls
            cls.name = name
        sqlalchemy.sql.visitors.VisitableType.__init__(cls, clsname, bases, clsdict)

    def _is_specification(cls, clsname):
        return _is_specification_name(clsname)

    @classmethod
    def specification_by_class_name(cls, name):
        return cls._class_mapping.get(name)

    @classmethod
    def specifications(cls):
        "Return all registered specifications."
        return set.union(*_PytisBaseMetaclass._name_mapping.values())

    @classmethod
    def clear(cls):
        "Clear all registered data."
        _PytisBaseMetaclass._name_mapping = {}
        _PytisBaseMetaclass._class_mapping = {}

class _PytisSimpleMetaclass(_PytisBaseMetaclass):
    
    objects = []
    
    def __init__(cls, clsname, bases, clsdict):
        _PytisBaseMetaclass.__init__(cls, clsname, bases, clsdict)
        if cls._is_specification(clsname):
            _PytisSimpleMetaclass.objects.append(cls())

class _PytisSchematicMetaclass(_PytisBaseMetaclass):

    objects = []
    init_functions = {}
    init_function_list = []
    init_functions_called = {}

    def __init__(cls, clsname, bases, clsdict):
        _PytisBaseMetaclass.__init__(cls, clsname, bases, clsdict)
        if cls._is_specification(clsname):
            schemas = _expand_schemas(cls)
            def init_function(cls=cls, may_alter_schema=False, schemas=schemas):
                for search_path in schemas:
                    if _debug:
                        sys.stderr.write('*** %s\n' % (cls.__name__,))
                    with _local_search_path(search_path):
                        if may_alter_schema and _enforced_schema:
                            search_path = [_enforced_schema] + search_path
                        if issubclass(cls, SQLSequence):
                            # hack
                            o = cls(cls.pytis_name(), metadata=_metadata, schema=search_path[0],
                                    start=cls.start, increment=cls.increment)
                        else:
                            o = cls(_metadata, search_path)
                    _PytisSchematicMetaclass.objects.append(o)
            # Instance construction takes a lot of time.  In some
            # situations we may be interested just in classes or only in
            # some instances.  In such a case instance creation is delayed
            # using the init_function mechanism.
            # Additionally it is necessary to make all objects available
            # before their instances are created so that all classes and
            # objects are available in _SQLQuery (to get columns and
            # dynamic dependencies) even without explicit dependencies.
            _PytisSchematicMetaclass.init_functions[cls.pytis_name()] = init_function
            _PytisSchematicMetaclass.init_function_list.append((cls, init_function,))

    @classmethod
    def clear(cls):
        _PytisSchematicMetaclass.init_functions = {}
        _PytisSchematicMetaclass.init_function_list = []
        _PytisSchematicMetaclass.init_functions_called = {}

    @classmethod
    def call_init_function(cls, func, **kwargs):
        if func not in _PytisSchematicMetaclass.init_functions_called:
            _PytisSchematicMetaclass.init_functions_called[func] = True
            func(**kwargs)

    @classmethod
    def run_init_functions(cls):
        function_list = _PytisSchematicMetaclass.init_function_list
        while function_list:
            f = function_list.pop()
            cls.call_init_function(f)

class _PytisTriggerMetaclass(_PytisSchematicMetaclass):
    def __init__(cls, clsname, bases, clsdict):
        if ((cls._is_specification(clsname) and
             cls.schemas is None and
             cls.table is not None)):
            cls.schemas = cls.table.schemas
        _PytisSchematicMetaclass.__init__(cls, clsname, bases, clsdict)
    
def object_by_name(name, allow_external=True):
    try:
        o = _metadata.tables[name]
    except KeyError:
        o = None
        basename = name.split('.')[-1]
        init_function = _PytisSchematicMetaclass.init_functions.get(basename)
        if init_function is None:
            if not allow_external:
                raise
        else:
            del _PytisSchematicMetaclass.init_functions[basename]
            _PytisSchematicMetaclass.call_init_function(init_function)
            try:
                o = _metadata.tables[name]
            except KeyError:
                if not allow_external:
                    raise
        if o is None:
            o = _SQLExternal(name)
    return o

def object_by_path(name, search_path=True, allow_external=True):
    if search_path is True:
        search_path = _current_search_path
    for schema in search_path:
        try:
            return object_by_name('%s.%s' % (schema, name,), allow_external=False)
        except KeyError:
            continue
    if not allow_external:
        raise SQLNameException((schema, name,))
    return _SQLExternal(name)

class _Reference(object):
    def __init__(self, name, column):
        self._name = name
        self._column = column
    def get(self, search_path=True):
        try:
            table = object_by_path(self._name, search_path=search_path, allow_external=False)
        except SQLNameException:
            return None
            assert self._column, ('No column name in a forward reference', self._name,)
            return '%s.%s' % (self._name, self._column,)
        if self._column:
            column = self._column
        else:
            column = None
            for c in table.c:
                if c.primary_key:
                    if column is not None:
                        raise SQLException("Can't identify column reference (multikey)", (table,))
                    column = c
            if column is None:
                raise SQLException("Can't identify column reference (no key)", (table,))
            column = column.name
        reference = table.c[column]
        return reference
    def specification_name(self):
        return self._name
def object_by_reference(name):
    name = name.strip()
    pos = name.find('(')
    if pos > 0:
        table = name[:pos].strip()
        column = name[(pos + 1):-1].strip()
    else:
        table = name
        column = None
    return _Reference(table, column)

def object_by_class(class_, search_path=None):
    assert issubclass(class_, SQLObject)
    if search_path is None:
        search_path = _current_search_path
    if _enforced_schema:
        search_path = [_enforced_schema] + search_path
    table_name = class_.pytis_name()
    return object_by_path(table_name, search_path)

def object_by_specification_name(specification_name, search_path=None):
    class_ = _PytisBaseMetaclass.specification_by_class_name(specification_name)
    if class_ is None:
        return None
    return object_by_class(class_, search_path or _current_search_path)

class RawCondition(object):
    def __init__(self, condition):
        self._condition = condition
        self._from_objects = []
    def _compiler_dispatch(self, *args, **kwargs):
        return self._condition
    
class TableLookup(object):
    """Accessor for table instances.

    You can use an instance of this class to access instances of table
    specifications when needed.  The instance makes all defined tables
    available as its attributes by their specification names.

    An instance of this class is available under short name 't'.  So you can
    refer to table instances by expressions such as 't.FooTable'.

    """
    def __getattr__(self, specification):
        return object_by_specification_name(specification)
t = TableLookup()

class ColumnLookup(object):
    """Accessor for table column instances.

    You can use an instance of this class to access instances of columns of
    table specifications when needed.  The instance makes SQLAlchemy column
    accessors available as its attributes named after all defined tables.

    An instance of this class is available under short name 'c'.  So you can
    refer to column instances by expressions such as 'c.FooTable.bar_column'.
    'c.FooTable' returns the same object as regular SQLAlchemy table would
    return with 'FooTable.c'.

    Note the lookup contains only table name and no schema name.  The schema is
    determined dynamically from the context where the column lookup is used
    (such as search path of current table).

    """
    def __getattr__(self, specification):
        return object_by_specification_name(specification).c
c = ColumnLookup()

class ReferenceLookup(object):
    """Representation of specification columns.

    'ReferenceLookup' works similar to 'ColumnLookup' but it returns a
    reference to column instance instead of a direct column instance.  This is
    useful when you want to refer to columns of specifications rather than to
    columns of particular database objects.  Typical use is in foreign key
    constraints where you define a table in multiple schemas by a single
    specification.  If 'ColumnLookup' was used to define the referenced column
    then all the tables would reference the same table in a single given
    schema.  With 'ReferenceLookup' the referenced table is determined
    separately for each of the schemas so the multiple tables can reference
    their corresponding tables in different schemas.

    An instance of this class is available under short name 'r'.  So you can
    refer to column specifications by expressions such as
    'r.FooTable.bar_column'.

    """
    class Reference(object):
        def __init__(self, specification, column):
            self._specification = specification
            self._column = column
        def get(self, search_path=None):
            specification = object_by_specification_name(self._specification, search_path)
            if specification is None or isinstance(specification, _SQLExternal):
                return None
            return specification.c[self._column]
        def specification_name(self):
            return self._specification
    class ColumnLookup(object):
        def __init__(self, specification):
            self._name = specification
            self._column = None
        def get(self, search_path=None):
            specification = object_by_specification_name(self._name, search_path=search_path)
            if specification is None or isinstance(specification, _SQLExternal):
                return None
            for c in specification.c:
                if c.primary_key:
                    self._column = c.name
                    return c
            return None
        def __getattr__(self, column):
            if column == '__clause_element__':
                raise AttributeError(column)
            return ReferenceLookup.Reference(self._name, column)
    def __getattr__(self, specification):
        return self.ColumnLookup(specification)
r = ReferenceLookup()

class Arguments(object):
    """Wrapper for objects accompanied by arguments.

    By making an instance of this class you can add optional arguments to
    objects.  This is currently used only in foreign key constraints.

    This class is also available by short name 'a'.
    
    """
    def __init__(self, *args, **kwargs):
        self._args = args
        self._kwargs = kwargs
    def args(self):
        return self._args
    def kwargs(self):
        return self._kwargs
a = Arguments

def reorder_columns(columns, column_ordering):
    """Return 'columns' in 'column_ordering' order.

    Arguments:

      columns -- columns to reorder, sequence of 'sqlalchemy.Column' instances
      column_ordering -- specification of column ordering; tuple of column
        names (strings)

    This function is useful in union selects when you need to combine different
    tables with different column ordering.

    """
    assert len(columns) == len(column_ordering), (columns, column_ordering,)
    ordered_columns = []
    for o in column_ordering:
        for c in columns:
            if c.name == o:
                ordered_columns.append(c)
                break
        else:
            raise SQLException("Column not found", o, columns)
    return ordered_columns

_forward_foreign_keys = []
_ForwardForeignKey = collections.namedtuple('_ForwardForeignKey',
                                            ('search_path', 'column_name', 'reference',
                                             'args', 'kwargs', 'table',))

class SQLObject(object):
    """Base class common to all specification classes.

    This class is not usable directly but it defines properties common to all
    specification classes.

    Properties:

      access_rights -- definition of access rights to the object; sequence of
        pairs (RIGHT, ROLE); if ROLE is 'True' then the right is granted to
        all users
      owner -- database owner of the object; string
      external -- iff true then do not create the object as it's just a
        declaration of a database object defined outside our specifications,
        to be used by objects of our specifications
      db_name -- actual name of the database object; string.  It is useful only
        with overloaded functions or *different* objects in different schemas
        when the Python object names must be different (as required by
        SQLAlchemy) while the database object names should be the same.  If
        'None' then it defaults to 'name'.

    """
    access_rights = ()
    owner = None
    external = False
    db_name = None
    _DB_OBJECT = None
    _pytis_direct_dependencies = None

    @classmethod
    def pytis_kind(class_):
        return class_._DB_OBJECT
    
    @classmethod
    def pytis_name(class_, real=False):
        if real and class_.db_name:
            return class_.db_name
        return class_.name

    def pytis_exists(self, metadata):
        _warn(u"Can't check existence of an object of %s type: %s." %
              (self.pytis_kind() or 'raw', self.name,))
        return True

    def pytis_changed(self, metadata, strict=True):
        return strict

    def pytis_create(self):
        self.create(_engine, checkfirst=False)

    def pytis_drop(self):
        self.drop(_engine)

    def _pytis_upgrade(self, metadata):
        _warn("Can't upgrade an object of %s type: %s." %
              (self.pytis_kind() or 'raw', self.name,))
        return True
        
    def pytis_upgrade(self, metadata, strict=True):
        if not self.pytis_exists(metadata):
            self.pytis_create()
            changed = True
        elif self.pytis_changed(metadata, strict=strict):
            if not self._pytis_upgrade(metadata):
                _gsql_output(("-- %s `%s' apparently changed, "
                              "but automated upgrade is not provided.") %
                             (self._DB_OBJECT, self.pytis_name(),))
            changed = True
        else:
            changed = False
        if changed:
            metadata.pytis_changed.add(self)

    def pytis_dependencies(self):
        return ()

    def _is_true_specification(self):
        return _is_specification_name(self.__class__.__name__)

    def _add_dependencies(self):
        for o in self.depends_on:
            if o is None:
                _warn("Unresolved dependency in %s: %s" %
                      (self.__class__.__name__, self.depends_on,))
                continue
            if isinstance(o, SQLObject):
                self.add_is_dependent_on(o)
            else:
                assert issubclass(o, SQLObject), ("Invalid dependency", o,)
                # General dependency must include all schema instances
                name = o.pytis_name()
                for search_path in _expand_schemas(o):
                    schema = search_path[0]
                    if _enforced_schema:
                        try:
                            o = object_by_name('%s.%s' % (_enforced_schema, name,),
                                               allow_external=False)
                        except:
                            o = object_by_name('%s.%s' % (schema, name,))
                    else:
                        o = object_by_name('%s.%s' % (schema, name,))
                    self.add_is_dependent_on(o)

    def _create_access_rights(self):
        self._access_right_objects = [_AccessRight(self, right, group)
                                      for right, group in self.access_rights]

class SQLSchematicObject(SQLObject):
    """Specification of a database object with schema.

    Most database objects are put into a schema.  This class defines the
    corresponding property.  The class is not usable directly but is inherited
    by other specification classes.

    Properties:

      schemas -- sequence of search paths to use when creating the object.
        Each search path is a sequence of schema names (strings).  For each of
        the search paths an instance of the database object is created in the
        first schema of the given search path.

    It is usually a good idea not to specify schemas directly, but to define
    one or more schemas as 'SQLFlexibleValue' instances and using those
    instances in 'schemas' property.  This way it is possible to override the
    predefined schemas when generating SQL.

    """
    schemas = _default_schemas

    def __init__(self, *args, **kwargs):
        self._search_path = None
        super(SQLSchematicObject, self).__init__(*args, **kwargs)

    @classmethod
    def object_schemas(class_):
        return [s[0] for s in _expand_schemas(class_)]

    @classmethod
    def default_search_path(class_):
        return _expand_schemas(class_)[0]

    def search_path(self):
        search_path = self._search_path
        if _enforced_schema:
            search_path = [_enforced_schema] + search_path
        return search_path

# gensql abbreviations -- do not use in new code!

def gA(table, **kwargs):
    return Arguments(object_by_reference(table), **kwargs)
gL = sqlalchemy.sql.literal_column
gO = object_by_path
gR = RawCondition

def _alchemy2pytis_type(atype):
    if hasattr(atype, 'pytis_orig_type'):
        return atype.pytis_orig_type
    if isinstance(atype, sqlalchemy.Integer):
        return pytis.data.Integer()
    elif isinstance(atype, sqlalchemy.String):
        return pytis.data.String()
    elif isinstance(atype, sqlalchemy.Boolean):
        return pytis.data.Boolean()
    elif isinstance(atype, sqlalchemy.Date):
        return pytis.data.Date()
    elif isinstance(atype, sqlalchemy.Numeric):
        return pytis.data.Float(digits=atype.precision, precision=atype.scale)
    elif isinstance(atype, sqlalchemy.types.NullType):
        return None
    raise Exception("Unrecognized SQLAlchemy type", atype)
    

# Database objects

class SQLSchema(sqlalchemy.schema.DDLElement, sqlalchemy.schema.SchemaItem, SQLObject):
    """Schema specification.

    Properties:

      name -- name of the schema; string
      
    """
    __metaclass__ = _PytisSimpleMetaclass
    __visit_name__ = 'schema'
    _DB_OBJECT = 'SCHEMA'
    name = None
    
    def __init__(self, *args, **kwargs):
        super(SQLSchema, self).__init__(*args, **kwargs)
        self._create_access_rights()

    def pytis_exists(self, metadata):
        return self.pytis_name(real=True) in metadata.pytis_inspector.get_schema_names()

    def pytis_changed(self, metadata, strict=True):
        return False
        
    def pytis_create(self):
        _engine.execute(self)
        self.after_create(_engine)
        
    def after_create(self, bind):
        for o in self._access_right_objects:
            bind.execute(o)
        if self.owner:
            command = ('ALTER SCHEMA "%s" OWNER TO "%s"' %
                       (self.name, self.owner,))
            bind.execute(command)

@compiles(SQLSchema)
def visit_schema(element, compiler, **kw):
    command = sqlalchemy.schema.CreateSchema(element.name)
    return _make_sql_command(command)

class SQLSequence(sqlalchemy.Sequence, SQLSchematicObject):
    """Sequence specification.

    Properties:

      name -- name of the sequence; string
      start -- initial value of the sequence; integer
      increment -- increment of the sequence; integer

    Note that some sequences are created automatically, e.g. for SERIAL
    columns, you shouldn't define such sequences in specifications.

    """
    __metaclass__ = _PytisSchematicMetaclass
    _DB_OBJECT = 'SEQUENCE'
    name = None
    start = None
    increment = None
    
    def __init__(self, *args, **kwargs):
        super(SQLSequence, self).__init__(*args, **kwargs)
        self._create_access_rights()

    def after_create(self, bind):
        for o in self._access_right_objects:
            bind.execute(o)

    def pytis_exists(self, metadata):
        name = self.pytis_name(real=True)
        with _metadata_connection(metadata) as connection:
            return metadata.pytis_engine.dialect.has_sequence(connection, name, schema=self.schema)

    def pytis_changed(self, metadata, strict=True):
        return False
        
    def pytis_create(self):
        super(SQLSequence, self).pytis_create()
        self.after_create(_engine)
    
class _SQLTabular(sqlalchemy.Table, SQLSchematicObject):
    """Base class for all table-like specification objects.

    It defines properties common to all specifications of objects which can
    look like tables, i.e. real tables, views and functions.  It supports
    especially creation of INSERT, UPDATE and DELETE rules.

    Properties:

      name -- name of the object; string
      depends_on -- sequence of specification classes this class depends on.
        Dependencies are usually implicit (typically by refering to another
        specification class in the specification) and you have to define them
        here only in case of doing something special (e.g. by using an
        otherwise unreferenced object inside raw SQL code).
      insert_order -- sequence of specification classes defining the table
        object and order of insertion on them when INSERT command is performed
        on the object.  If the value is 'None' then default insert action
        (depending on particular tabular object kind) is performed.
      update_order -- the same as 'insert_order' but it applies to updates
      delete_order -- the same as 'insert_order' but it applies to deletes
      no_insert_columns -- sequence of names of columns (strings) to ignore
        when performing INSERT action on the object
      no_update_columns -- the same as 'no_insert_columns' but it applies to
        updates
      special_insert_columns -- sequence of tripples (TABLE, COLUMN_NAME, VALUE);
        each of the tripples defines assignment of special VALUE (SQL
        expression as basestring) to COLUMN_NAME (basestring column name) of
        TABLE (specification class or string table name)
      special_update_columns -- the same as 'special_insert_columns' but it
        applies to updates

    The class also defines default methods for rule handling, see
    'on_insert()', 'on_update()', 'on_delete()', 'on_insert_also()',
    'on_update_also()', 'on_delete_also()'.

    """
    __metaclass__ = _PytisSchematicMetaclass
    _DB_OBJECT = None
    
    name = None
    depends_on = ()
    insert_order = None
    update_order = None
    delete_order = None
    no_insert_columns = ()
    no_update_columns = ()
    special_insert_columns = ()
    special_update_columns = ()

    def _init(self, *args, **kwargs):
        super(_SQLTabular, self)._init(*args, **kwargs)
        self._search_path = _current_search_path
        self._add_dependencies()
        self._create_comments()
        self._create_access_rights()
        self._register_access_rights()
        self._create_rules()

    def pytis_exists(self, metadata):
        name = self.pytis_name(real=True)
        with _metadata_connection(metadata) as connection:
            return metadata.pytis_engine.dialect.has_table(connection, name, schema=self.schema)

    _PYTIS_TYPE_MAPPING = {'VARCHAR': 'TEXT',
                           'BIGSERIAL': 'BIGINT',
                           'SERIAL': 'INTEGER',
                           }

    def _pytis_ctype_changed(self, column_1, column_2):
        if ((column_1.type != column_2.type and
             not isinstance(column_2.type, sqlalchemy.types.NullType))):
            new_type = str(column_2.type)
            new_type = self._PYTIS_TYPE_MAPPING.get(new_type, new_type)
            orig_type = str(column_1.type)
            return orig_type != new_type
        else:
            return False

    _PYTIS_TYPE_CAST_MATCHER = re.compile("^'?(.*[^'])'?::[a-z]+$")
    def _pytis_defaults_changed(self, column_1, column_2):
        default_1 = column_1.server_default
        default_2 = column_2.server_default
        if (((default_1 is None and default_2 is not None) or
             (default_1 is not None and default_2 is None))):
            return True
        if default_1 is None and default_2 is None:
            return False
        lower = (isinstance(column_1.type, sqlalchemy.Boolean) and
                 isinstance(column_2.type, sqlalchemy.Boolean))
        def textify(default):
            for_update = ' FOR UPDATE' if default.for_update else ''
            if not isinstance(default, basestring):
                default = default.arg
            if not isinstance(default, basestring):
                try:
                    default = default.text
                except AttributeError:
                    default = str(default)
            if lower:
                default = default.lower()
            match = self._PYTIS_TYPE_CAST_MATCHER.match(default)
            if match is not None:
                default = match.group(1)
            default += for_update
            return default
        return textify(default_1) != textify(default_2)

    def _pytis_distinct_columns(self, db_column, spec_column):
        if (db_column.name != spec_column.name or
            self._pytis_ctype_changed(db_column, spec_column) or
            (not spec_column.primary_key and
             (db_column.nullable != spec_column.nullable or
              self._pytis_defaults_changed(db_column, spec_column)))):
            return True
        
    def _pytis_columns_changed(self, metadata):
        name = self.pytis_name(real=True)
        schema = self.schema
        dialect = metadata.pytis_engine.dialect
        with _metadata_connection(metadata) as connection:
            columns = _get_columns(dialect, connection, name, schema)
        if len(columns) != len(self.c):
            def_columns = [c for c in self.c]
            if len(def_columns) != 1:
                return True
            def_name = def_columns[0].name
            if def_name == '*' or def_name.endswith('.*'):
                _warn("Can't compare star columns in `%s'." % (self.name,))
                return False
        for c1_kwargs, c2 in zip(columns, self.c):
            for orig, real in (('type', 'type_'), ('default', 'server_default'),):
                c1_kwargs[real] = c1_kwargs[orig]
                del c1_kwargs[orig]
            c1 = sqlalchemy.Column(**c1_kwargs)
            if self._pytis_distinct_columns(c1, c2):
                return True
        return False
        
    def pytis_changed(self, metadata, strict=True):
        if strict:
            return self._pytis_columns_changed(metadata)
        return False

    def _pytis_upgrade(self, metadata):
        # We don't emit DROP here.  They may be dependencies between objects
        # forcing them to be dropped in certain order.  So we drop objects in
        # global gsql processing.
        self.pytis_create()
        return True
        
    def pytis_dependencies(self):
        return self._extra_dependencies

    def _create_comments(self):
        doc = self.__doc__
        if doc:
            sqlalchemy.event.listen(self, 'after_create',
                                    _ObjectComment(self, self._DB_OBJECT, doc))

    def _register_access_rights(self):
        for o in self._access_right_objects:
            sqlalchemy.event.listen(self, 'after_create', o)
        if self.owner:
            command = ('ALTER %s "%s"."%s" OWNER TO "%s"' %
                       (self._DB_OBJECT, self.schema, self.name, self.owner,))
            sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))
        
    def _create_rules(self):
        def make_rule(action, instead_commands, also_commands):
            rule = _Rule(self, action, instead_commands, also_commands)
            sqlalchemy.event.listen(self, 'after_create', rule)
        make_rule('INSERT', self.on_insert, self.on_insert_also)
        make_rule('UPDATE', self.on_update, self.on_update_also)
        make_rule('DELETE', self.on_delete, self.on_delete_also)

    def _original_columns(self):
        return self.c

    def _equivalent_rule_columns(self, column):
        return [column]

    def _hidden_rule_columns(self, tabular):
        return []
    
    def _rule_assignments(self, tabular, excluded, special):
        assignments = {}
        all_columns = self._hidden_rule_columns(tabular)
        for c in self._original_columns():
            all_columns.append(c)
        for c in all_columns:
            if c.name in excluded:
                continue
            table_c = c.element if isinstance(c, sqlalchemy.sql.expression._Label) else c
            if not isinstance(table_c, sqlalchemy.sql.expression.ColumnClause):
                # Probably literal column, we can't handle it.
                continue
            table = table_c.table
            if isinstance(table, sqlalchemy.sql.expression.Alias):
                table = table.element
            if table is tabular:
                name = _sql_plain_name(c.name)
                table_column_name = _sql_plain_name(table_c.name)
                if table_column_name not in assignments:
                    # A column may appear more than once in a view.
                    # Let's use its first version here to be consistent with gensql.
                    for t, n, v in special:
                        tt = t if isinstance(t, basestring) else t.pytis_name()
                        if tabular.pytis_name() == tt and n == table_column_name:
                            value = v
                            break
                    else:
                        value = 'new.' + name
                    assignments[table_column_name] = sqlalchemy.literal_column(value)
        return assignments

    def _rule_condition(self, tabular):
        conditions = []
        for table_c in [c for c in tabular.primary_key.columns] + [c for c in tabular.columns]:
            if table_c.primary_key or (table_c.unique and not table_c.nullable):
                # Try to find `tabular' primary key or one of its equivalents in my
                # columns, perhaps aliased.  If primary key is not found, look
                # at primary like (i.e. UNIQUE NOT NULL) columns.
                equivalent_column_instances = self._equivalent_rule_columns(table_c)
                equivalent_columns = [(c.table, c.name, c.primary_key,)
                                      for c in equivalent_column_instances]
                for c in self._original_columns():
                    tc = c.element if isinstance(c, sqlalchemy.sql.expression._Label) else c
                    if not isinstance(tc, sqlalchemy.Column):
                        continue
                    table, name = tc.table, tc.name
                    if isinstance(table, sqlalchemy.sql.expression.Alias):
                        table = table.element
                    if (table, name, True) in equivalent_columns:
                        break
                    if (table, name, False) in equivalent_columns:
                        primary = [ec for ec in equivalent_column_instances if ec.primary_key]
                        if primary:
                            c = primary[0]
                            break
                else:
                    continue
                name = _sql_plain_name(c.name)
                conditions.append(table_c == sqlalchemy.literal_column('old.' + name))
                break
        if not conditions:
            if False:
                # Let's just print warning until converted specifications are fixed
                raise SQLException("Table key column not found in the view", table_c)
            else:
                _gsql_output(("-- WARNING: Missing table key column, incorrect rule for view "
                              "will be output: %s %s") % (self.name, tabular.name,))
                conditions.append(sqlalchemy.sql.false())
        return sqlalchemy.and_(*conditions)

    def _rule_tables(self, order):
        return [object_by_class(tabular, self.search_path()) for tabular in order]

    def _default_rule_commands(self):
        return None
    
    def on_insert(self):
        """Return sequence of SQL commands to be performed on INSERT.

        The method effectively defines an INSERT rule.  The commands are
        represented by SQLAlchemy insertion objects.  Default implementation
        based on object properties is provided so it is usually not necessary
        to redefine the method unless you want to do something special.

        """
        if self.insert_order is None:
            return self._default_rule_commands()
        commands = []
        for tabular in self._rule_tables(self.insert_order):
            assignments = self._rule_assignments(tabular, self.no_insert_columns,
                                                 self.special_insert_columns)
            c = tabular.insert().values(**assignments)
            commands.append(c)
        return commands

    def on_update(self):
        """Return sequence of SQL commands to be performed on UPDATE.

        This is similar to 'on_insert()' except it handles UPDATE.
        
        """
        if self.update_order is None:
            return self._default_rule_commands()
        commands = []
        for tabular in self._rule_tables(self.update_order):
            assignments = self._rule_assignments(tabular, self.no_update_columns,
                                                 self.special_update_columns)
            if not assignments:
                continue
            condition = self._rule_condition(tabular)
            c = tabular.update().values(**assignments).where(condition)
            commands.append(c)
        return commands

    def on_delete(self):
        """Return sequence of SQL commands to be performed on DELETE.

        This is similar to 'on_insert()' except it handles DELETE.
        
        """
        if self.delete_order is None:
            return self._default_rule_commands()
        commands = []
        for tabular in self._rule_tables(self.delete_order):
            condition = self._rule_condition(tabular)
            c = tabular.delete().where(condition)
            commands.append(c)
        return commands

    def on_insert_also(self):
        """Return sequence of additional SQL commands to perfom on INSERT.

        It effectively defines ON INSERT ALSO rule.  The default implementation
        returns an empty sequence.
        
        """
        return ()

    def on_update_also(self):
        """Return sequence of additional SQL commands to perfom on UPDATE.

        This is similar to 'on_insert_also()' except it handles UPDATE.
        
        """
        return ()

    def on_delete_also(self):
        """Return sequence of additional SQL commands to perfom on DELETE.

        This is similar to 'on_insert_also()' except it handles DELETE.
        
        """
        return ()

class _SQLIndexable(SQLObject):
    
    index_columns = ()

    def _init(self, *args, **kwargs):
        super(_SQLIndexable, self)._init(*args, **kwargs)
        if self._is_true_specification():
            self._create_special_indexes()
    
    def _create_special_indexes(self):
        args = ()
        for index in self.index_columns:
            ikwargs = {}
            if isinstance(index, Arguments):
                index_content = index.args()
                index_kwargs = index.kwargs()
                method = index_kwargs.get('method')
                if method:
                    ikwargs['postgresql_using'] = method
                if index_kwargs.get('unique'):
                    ikwargs['unique'] = True
            else:
                index_content = index
                method = None
            columns = []
            colnames = []
            for i in range(len(index_content)):
                c = index_content[i]
                if isinstance(c, basestring):
                    columns.append(getattr(self.c, c))
                    colnames.append(c)
                else:
                    columns.append(c)
                    colnames.append(str(i))
            index_name = string.join(colnames, '_')
            if method:
                index_name += '_' + method
            index_name = ('%s_%s' % (self.name, index_name,))[:59]
            index = sqlalchemy.Index('%s_idx' % (index_name,), *columns, **ikwargs)
            sqlalchemy.event.listen(self, 'after_create', lambda *args, **kwargs: index)
        return args

    def _pytis_index_signature(self, index):
        return (index.name, index.unique, tuple([c.name for c in index.columns]))

    def _pytis_unique_indexes(self):
        indexes = set()
        for unique in self.unique:
            name = (self.pytis_name(real=True) + '_' + string.join(unique, '_'))[:59] + '_key'
            columns = [getattr(self.c, c) for c in unique]
            indexes.add(sqlalchemy.Index(name, *columns, unique=True))
        return indexes

    def _pytis_upgrade_indexes(self, metadata, db_table):
        changed = False
        db_indexes = dict([(i.name, i,) for i in db_table.indexes])
        new_index_columns = {}
        for i in self.indexes:
            try:
                del db_indexes[i.name]
            except KeyError:
                if len(i.columns) == 1:
                    for c in i.columns:
                        new_index_columns[c.name] = i
                else:
                    i.create(_engine)
                    changed = True
        index_names = set([i.name for i in self.indexes])
        db_index_columns = {}
        for i in db_indexes.values():
            if len(i.columns) == 1:
                for c in i.columns:
                    db_index_columns[c.name] = i
            else:
                i.drop(_engine)
                changed = True
        for c in self.c:
            if c.pytis_orig_table != self.name:
                continue
            if not c.primary_key and not c.unique:
                index = c.index
                if index:
                    try:
                        del db_index_columns[c.name]
                    except KeyError:
                        if isinstance(index, dict):
                            method = ''
                            kwargs = index
                            if 'method' in index:
                                method = index[method] + '_'
                            name = '%s_%s_%sidx' % (self.pytis_name(real=True), c.name, method,)
                        else:
                            kwargs = {}
                            name = 'ix_%s_%s_%s' % (self.schema, self.pytis_name(real=True),
                                                    c.name,)
                        if name not in index_names:
                            sqlalchemy.Index(name, c, **kwargs).create(_engine)
                            changed = True
                    try:
                        del new_index_columns[c.name]
                    except KeyError:
                        pass
        for i in new_index_columns.values():
            i.create(_engine)
            changed = True
        for i in db_index_columns.values():
            i.drop(_engine)
            changed = True
        return changed
        
    def pytis_changed(self, metadata, strict=True):
        # Not everything covered here, but we don't support all features in upgrade
        if super(_SQLIndexable, self).pytis_changed(metadata, strict=strict):
            return True
        db_table = self._pytis_db_table(metadata)
        constraints = self._pytis_simplified_constraints(self.constraints)
        db_constraints = self._pytis_simplified_constraints(db_table.constraints)
        if constraints.keys() != db_constraints.keys():
            return True
        # Unique constraints are represented as indexes in the database
        indexes = copy.copy(self.indexes).union(self._pytis_unique_indexes())
        # SQLAlchemy doesn't compare some objects correctly
        # (e.g. sqlalchemy.String() != sqlalchemy.String()) so we have to make
        # our own index comparison.
        index_list = [self._pytis_index_signature(i) for i in indexes]
        index_list.sort()
        db_index_list = [self._pytis_index_signature(i) for i in db_table.indexes]
        db_index_list.sort()
        if index_list != db_index_list:
            return True
        return False

class SQLTable(_SQLIndexable, _SQLTabular):
    """Regular table specification.

    Properties:

      fields -- tuple of 'Column' instances defining table columns in their
        order, excluding inherited columns
      inherits -- tuple of inherited tables, specification classes
      tablespace -- 'None' or string defining tablespace of the table
      check -- tuple of SQL expressions (basestrings) defining check
        constraints on the table
      unique -- tuple of tuples defining unique constraints; each of the tuples
        contains names of the table columns (strings) creating a unique
        constraint
      foreign_keys -- tuple of multicolumn foreign key constraints.  Each of
        the constraints is 'Arguments' instance with the first argument being a
        tuple of table column names and the second argument a tuple of the same
        length containing corresponding foreign table columns or
        'ReferenceLookup' instances.  Additional keyword arguments from
        'sqlalchemy.ForeignKey' constructor (e.g. 'onupdate') may be given.
        For single column foreign key constraints it is preferable to use
        'references' argument in 'Column' definitions.
      with_oids -- iff True then oids are assigned to the table rows; boolean
      index_columns -- tuple of tuples or 'Arguments' instances.  Each of the
        tuples or 'Arguments' instances contains names of columns or SQLAlchemy
        expressions to include within a single index.  An additional keyword
        argument 'method' may be provided in an 'Arguments' whose value
        determines indexing method to be used for this index.  The optional
        argument 'unique' in 'Arguments' marks the index as unique if the
        argument value is true.  Use 'index_columns' only for multicolumn indexes
        and functional indexes, plain single column indexes should be specified
        directly in the corresponding 'Column' specifications.
      triggers -- tuple of trigger specifications to assign to this table.  You
        can assign a trigger to a table directly in 'SQLTrigger' specification.
        But if the same trigger function is to be used in more than one table
        then you should define a trigger not bound to a particular table
        (i.e. with undefined 'table' property) and assign the trigger to
        corresponding tables in 'triggers'.  Each of 'trigger' elements is
        either a specification class or a tuple of a specification class and
        trigger function arguments.

    It is possible to insert predefined rows into the newly created table.  In
    such a case set the following properties:

      init_columns -- tuple of column names (strings) of 'init_values'
      init_values -- tuple of tuples; each of the tuples represents a row of
        values.  The row contains values of columns as specified by
        'init_columns' in the given count and order and of types required by
        SQLAlchemy for given columns.

    See also '_SQLTabular' properties.

    """
    _DB_OBJECT = 'TABLE'
    
    fields = ()
    inherits = ()
    tablespace = None
    init_columns = None
    init_values = ()
    check = ()
    unique = ()
    foreign_keys = ()
    with_oids = False
    triggers = ()
    
    def __new__(cls, metadata, search_path):
        table_name = cls.pytis_name()
        key = []
        fields = cls.fields
        field_names = [f.id() for f in cls.fields]
        for i in cls.inherits:
            fields = fields + tuple([f for f in i.fields if f.id() not in field_names])
        for c in fields:
            if c.primary_key():
                key.append(c.id())
        if len(key) == 1:
            key_name = '%s.%s.%s' % (search_path[0], table_name, key[0],)
        else:
            key_name = None
        columns = ()
        check_constraints = []
        for i in cls.inherits:
            orig_table_name = i.pytis_name()
            columns = columns + tuple([c.sqlalchemy_column(search_path, table_name, key_name,
                                                           orig_table_name, inherited=True,
                                                           check_constraints=check_constraints)
                                       for c in i.fields if c.id() not in field_names])
        foreign_constraints = []
        columns = columns + tuple([c.sqlalchemy_column(search_path, table_name, key_name,
                                                       table_name,
                                                       foreign_constraints=foreign_constraints,
                                                       check_constraints=check_constraints)
                                   for c in cls.fields])
        args = (table_name, metadata,) + columns + tuple(check_constraints)
        for check in cls.check:
            args += (sqlalchemy.CheckConstraint(check),)
        for unique in cls.unique:
            args += (sqlalchemy.UniqueConstraint(*unique),)
        for foreign_key in cls.foreign_keys:
            columns, refcolumns = foreign_key.args()
            refcolumns = [c.get() if isinstance(c, ReferenceLookup.Reference) else c
                          for c in refcolumns]
            kwargs = foreign_key.kwargs()
            args += (sqlalchemy.ForeignKeyConstraint(columns, refcolumns, **kwargs),)
        obj = sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])
        obj.pytis_key = key
        for f in foreign_constraints:
            _forward_foreign_keys.append(_ForwardForeignKey(*(f[:5] + (obj,))))
        for c in cls.inherits:
            if c.with_oids != cls.with_oids:
                raise SQLException("Can't change oids from inherited table",
                                   (cls.pytis_name(), c.pytis_name(),))
        return obj

    def _init(self, *args, **kwargs):
        self._pytis_create_p = False
        super(SQLTable, self)._init(*args, **kwargs)
        self._create_parameters()
        if self._is_true_specification():
            self._create_triggers()

    @property
    def columns(self):
        columns = super(SQLTable, self).columns
        if self._pytis_create_p:
            all_columns = columns
            columns = sqlalchemy.sql.ColumnCollection()
            for c in all_columns:
                if not c.info['inherited']:
                    columns.add(c)
        return columns

    def _table_name(self, table):
        name = table.name
        schemas = _expand_schemas(table)
        table_schemas = [s[0] for s in schemas]
        for schema in self.search_path():
            if schema in table_schemas:
                break
        else:
            raise SQLException("No matching table schema", (name, self.search_path(),))
        return '"%s"."%s"' % (schema, name,)

    def _add_dependencies(self):
        super(SQLTable, self)._add_dependencies()
        for inherited in self.inherits:
            pytis_name = inherited.pytis_name()
            for s in self.search_path():
                name = '%s.%s' % (s, pytis_name,)
                try:
                    o = object_by_name(name, allow_external=False)
                except:
                    continue
                self.add_is_dependent_on(o)
                break
            else:
                raise SQLException("Unresolved dependency", (pytis_name, self.search_path(),))

    def _pytis_db_table(self, metadata):
        name = self.pytis_name(real=True)
        schema = self.schema
        db_table = metadata.tables.get('%s.%s' % (schema, name,))
        if db_table is None:
            db_table = sqlalchemy.Table(name, metadata, schema=schema)
            metadata.pytis_inspector.reflecttable(db_table, None)
        return db_table

    def _pytis_distinct_columns(self, db_column, spec_column):
        if spec_column.pytis_orig_table != self.name:
            return False
        return super(SQLTable, self)._pytis_distinct_columns(db_column, spec_column)

    def _pytis_simplified_constraints(self, constraints):
        simplified = {}
        def simplify(constraint):
            if isinstance(constraint, sqlalchemy.PrimaryKeyConstraint):
                return
            if isinstance(constraint, sqlalchemy.ForeignKeyConstraint):
                columns = [c.name if isinstance(c, sqlalchemy.Column) else c
                           for c in constraint.columns]
                columns.sort()
                targets = []
                for e in constraint.elements:
                    name = e.target_fullname
                    # Strip schema as it may not be present in the introspected table
                    components = name.split('.')
                    name = string.join(components[-2:], '.')
                    targets.append(name)
                targets.sort()
                # We should add (constraint.onupdate, constraint.ondelete,),
                # but SQLAlchemy introspections apparently don't detect cascades,
                # so it would generate a lot of unnecessary updates.
                s = (tuple(columns), tuple(targets),)
            else:
                # Let's ignore other constraints for now
                return
            simplified[s] = constraint
        for c in constraints:
            simplify(c)
        return simplified
        
    def pytis_changed(self, metadata, strict=True):
        # Not everything covered here, but we don't support all features in upgrade
        if super(SQLTable, self).pytis_changed(metadata, strict=strict):
            return True
        db_table = self._pytis_db_table(metadata)
        constraints = self._pytis_simplified_constraints(self.constraints)
        db_constraints = self._pytis_simplified_constraints(db_table.constraints)
        if constraints.keys() != db_constraints.keys():
            return True
        return False

    def _pytis_upgrade(self, metadata):
        changed = False
        db_table = self._pytis_db_table(metadata)
        table_name = db_table.name
        for c in db_table.c:
            if c.name not in self.c:
                _engine.execute(alembic.ddl.base.DropColumn(table_name, c, self.schema))
        if self._pytis_upgrade_indexes(metadata, db_table):
            changed = True
        for c in self.c:
            if c.pytis_orig_table != self.name:
                continue
            if c.name not in db_table.c:
                _engine.execute(alembic.ddl.base.AddColumn(table_name, c, self.schema))
                changed = True
            elif c != db_table.c[c.name]:
                orig_c = db_table.c[c.name]
                if self._pytis_ctype_changed(orig_c, c):
                    ddl = alembic.ddl.base.ColumnType(table_name, c.name, c.type,
                                                      schema=self.schema,
                                                      existing_type=orig_c.type)
                    _engine.execute(ddl)
                    changed = True
                if not c.primary_key and self._pytis_defaults_changed(orig_c, c):
                    server_default = orig_c.server_default
                    ddl = alembic.ddl.base.ColumnDefault(table_name, c.name, c.server_default,
                                                         schema=self.schema,
                                                         existing_server_default=server_default)
                    _engine.execute(ddl)
                    changed = True
                if ((not orig_c.primary_key and not c.primary_key and
                     orig_c.nullable != c.nullable)):
                    ddl = alembic.ddl.base.ColumnNullable(table_name, c.name, c.nullable,
                                                          schema=self.schema,
                                                          existing_nullable=orig_c.nullable)
                    _engine.execute(ddl)
                    changed = True
        constraints = self._pytis_simplified_constraints(self.constraints)
        db_constraints = self._pytis_simplified_constraints(db_table.constraints)
        for k in constraints.keys():
            try:
                del db_constraints[k]
            except KeyError:
                fdef = sqlalchemy.schema.AddConstraint(constraints[k])
                _engine.execute(fdef)
                changed = True
        for c in db_constraints.values():
            fdef = sqlalchemy.schema.DropConstraint(c)
            _engine.execute(fdef)
            changed = True
        return changed
            
    def _alter_table(self, alteration):
        command = 'ALTER TABLE "%s"."%s" %s' % (self.schema, self.name, alteration,)
        sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))

    def _create_comments(self):
        super(SQLTable, self)._create_comments()
        for c in self.fields:
            doc = c.doc()
            if doc:
                sqlalchemy.event.listen(self, 'after_create', _ColumnComment(self, c))
        
    def _create_parameters(self):
        self._alter_table("SET %s OIDS" % ("WITH" if self.with_oids else "WITHOUT",))
        if self.tablespace:
            self._alter_table('SET TABLESPACE "%s"' % (self.tablespace,))

    def _create_special_indexes(self):
        args = ()
        for f in self.fields:
            index = f.index()
            if isinstance(index, dict):
                assert index.keys() == ['method'], index
                method = index['method']
                column_name = f.id()
                index = sqlalchemy.Index('%s_%s_%s_idx' % (self.name, column_name, method,),
                                         getattr(self.c, column_name), postgresql_using=method)
                sqlalchemy.event.listen(self, 'after_create', lambda *args, **kwargs: index)
        super(SQLTable, self)._create_special_indexes()
        return args

    def _create_triggers(self):
        for t in self.triggers:
            if not isinstance(t, (tuple, list,)):
                t = (t,)
            trigger = t[0]
            class T(trigger):
                __doc__ = trigger.__doc__
                name = '%s__%s' % (self.name, trigger.name,)
                if self.schema:
                    name = '%s__%s' % (self.schema, name,)
                if _current_search_path:
                    schemas = (_current_search_path,)
                table = self.__class__
                arguments = t[1:]
            sqlalchemy.event.listen(self, 'after_create', lambda *args, **kwargs: T)
            if T._pytis_direct_dependencies is None:
                T._pytis_direct_dependencies = []
            T._pytis_direct_dependencies.append(self.__class__)
    
    def _register_access_rights(self):
        super(SQLTable, self)._register_access_rights()
        groups = set([group for right, group in self.access_rights
                      if right.lower() in ('insert', 'all')])
        for c in self.c:
            if isinstance(c.type, (SERIAL, BIGSERIAL,)) and not c.info.get('inherited'):
                for g in groups:
                    cname = c.name
                    command = ('GRANT usage ON "%s"."%s_%s_seq" TO GROUP %s' %
                               (self.schema, self.name, cname, _role_string(g),))
                    sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))

    def create(self, bind=None, checkfirst=False):
        self._pytis_create_p = True
        with _local_search_path(self._set_search_path(bind)):
            try:
                super(SQLTable, self).create(bind=bind, checkfirst=checkfirst)
            finally:
                self._pytis_create_p = False
            self._insert_values(bind)

    def _set_search_path(self, bind):
        search_path = self.search_path()
        path_list = [_sql_id_escape(s) for s in search_path]
        path = string.join(path_list, ',')
        command = 'SET SEARCH_PATH TO %s' % (path,)
        bind.execute(command)
        return search_path
        
    def _insert_values(self, bind):
        if self.init_values:
            for row in self.init_values:
                values = dict(zip(self.init_columns, row))
                insert = self.insert().values(**values)
                bind.execute(insert)

    @classmethod
    def specification_fields(class_):
        return class_.fields

    @classmethod
    def add_init_values(class_, *values):
        """Add given 'values' to init_values.

        This is useful when you need to add init values to an existing table
        from another library or application.

        Arguments:

          values -- each of the values represents a single init row, as
            described in 'init_values' property

        """
        class_.init_values = class_.init_values + values

class _SQLReplaceable(SQLObject):

    def _pytis_definition(self, connection):
        return None

    def _pytis_set_definition_schema(self, connection):
        query = ("set search_path to public")
        connection.execute(sqlalchemy.text(query))

    def pytis_changed(self, metadata, strict=True):
        if super(_SQLReplaceable, self).pytis_changed(metadata, strict=strict):
            return True
        if not strict:
            return False
        with _metadata_connection(metadata) as connection:
            definition = self._pytis_definition(connection)
            if definition is None:
                return True
            transaction = connection.begin()
            try:
                connection._run_visitor(_PytisSchemaGenerator, self, checkfirst=False)
                new_definition = self._pytis_definition(connection)
            except:
                new_definition = None
            finally:
                transaction.rollback()
        return definition != new_definition

class _SQLQuery(SQLObject):

    def _add_dependencies(self):
        super(_SQLQuery, self)._add_dependencies()
        self._pytis_add_dynamic_dependencies()

    def _pytis_add_dynamic_dependencies(self):
        objects = self._pytis_query_objects()
        seen = []
        # We may add some objects multiple times here but that doesn't matter.
        # Trying to prune the list in trivial ways makes gsql many times slower
        # because there may be many comparisons here.
        while objects:
            o = objects.pop()
            if isinstance(o, sqlalchemy.sql.Alias):
                # Some aliases, e.g. sqlalchemy.alias with sqlalchemy.literal inside,
                # can't get children, so prevent crashing on that.
                try:
                    objects += o.get_children()
                except:
                    pass
            elif isinstance(o, sqlalchemy.Table):
                self.add_is_dependent_on(o)
                seen.append(o)
            elif isinstance(o, sqlalchemy.sql.ClauseElement):
                objects += o.get_children()
                seen.append(o)
            elif isinstance(o, (tuple, list,)):
                objects += list(o)
            elif not isinstance(o, (RawCondition, basestring,)):
                raise SQLException("Unknown condition element", o)
        self._pytis_query_dependencies = seen

    def _pytis_query_objects(self):
        return []

    def pytis_dependencies(self):
        return (list(super(_SQLQuery, self).pytis_dependencies()) +
                self._pytis_query_dependencies)

    @classmethod
    def _exclude(cls, tabular, *columns_tables, **kwargs):
        """Return sequence of 'tabular' columns with some exclusions.

        Arguments:

          tabular -- 'SQLTable' instance providing the initial sequence of columns
          columns_tables -- sequence of objects to exclude from columns of
            'tabular'.  The objects can be names of the columns to exclude
            (strings) or tables (or their aliases) in which case all columns
            with names present among those table column names are excluded.
          inherited (in kwargs) -- iff true, exclude all inherited columns;
            boolean

        """
        inherited = kwargs.get('inherited', True)
        columns = []
        tables = []
        for x in columns_tables:
            if isinstance(x, SQLTable):
                tables.append(x)
            elif isinstance(x, sqlalchemy.sql.Alias):
                tables.append(x.original)
            else:
                columns.append(x)
        inherited_columns = {}
        if inherited:
            t = tabular
            if isinstance(t, sqlalchemy.sql.expression.Alias):
                t = t.element
            for c in t.c:
                if isinstance(c, sqlalchemy.Column) and c.info.get('inherited'):
                    inherited_columns[c.name] = True
        included = []
        for c in tabular.c:
            if c in columns:
                continue
            cname = c.name
            if cname in columns:
                continue
            if inherited_columns.get(cname):
                continue
            for t in tables:
                if cname in t.c:
                    break
            else:
                included.append(c)
        return included

class _SQLBaseView(_SQLReplaceable, _SQLQuery, _SQLTabular):
    
    @classmethod
    def query(class_):
        return None

    primary_column = None

    def __new__(cls, metadata, search_path):
        with _local_search_path(search_path):
            columns = tuple([sqlalchemy.Column(c.name, c.type) for c in cls.query().columns])
            primary = cls.primary_column
            if primary is not None:
                for c in columns:
                    if c.name == primary:
                        c.primary_key = True
                        break
            args = (cls.name, metadata,) + columns
            return sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])

    def _pytis_definition(self, connection):
        self._pytis_set_definition_schema(connection)
        name = self.pytis_name(real=True)
        schema = self.schema
        query = ("select pg_get_ruledef(pg_rewrite.oid) from pg_rewrite join "
                 "pg_class on ev_class = pg_class.oid join "
                 "pg_namespace on relnamespace = pg_namespace.oid "
                 "where rulename = '_RETURN' and relname = :name and nspname = :schema")
        result = connection.execute(
            sqlalchemy.text(query,
                            bindparams=[sqlalchemy.bindparam('schema', schema,
                                                             type_=sqlalchemy.String),
                                        sqlalchemy.bindparam('name', name,
                                                             type_=sqlalchemy.String)]))
        row = result.fetchone()
        result.close()
        if row is None:
            raise Exception("Object not identified: %s.%s" % (schema, name,))
        return row[0]

    def _pytis_query_objects(self):
        return [self.query()]

    @classmethod
    def _alias(cls, columns, **aliases):
        raliases = dict([(v, k,) for k, v in aliases.items()])
        aliased = []
        columns = [c for c in columns]
        inherited = {}
        if columns and isinstance(columns[0].table, sqlalchemy.sql.expression.Alias):
            for cc in columns[0].table.element.columns:
                if cc.info.get('inherited'):
                    inherited[cc.name] = True
        for c in columns:
            if c in raliases:
                aliased.append(c.label(raliases[c]))
            elif not inherited.get(c.name):
                aliased.append(c)
        return aliased

    @classmethod
    def _reorder(cls, tabular_1, tabular_2):
        def columns(t):
            if isinstance(t, _SQLTabular):
                columns = t.c
            elif isinstance(t, (tuple, list,)):
                columns = t
            else:
                raise Exception("Program error", t)
            return columns
        columns_1 = columns(tabular_1)
        columns_2 = columns(tabular_2)
        reordered = []
        for c in columns_1:
            for cc in columns_2:
                if c.name == cc.name:
                    reordered.append(cc)
                    break
            else:
                raise SQLException("Missing column", c)
        return reordered
    
    def _original_columns(self):
        return self.query().inner_columns
    
    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)
            
    def drop(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaDropper, self, checkfirst=checkfirst)
        
    @classmethod
    def specification_fields(class_):
        with _local_search_path(class_.default_search_path()):
            query = class_.query()
        def original_column(c):
            result = None
            if isinstance(c.type, SERIAL) and len(c.base_columns) == 1:
                base_column = list(c.base_columns)[0]
                if ((isinstance(base_column, sqlalchemy.Column) and
                     isinstance(base_column.type, SERIAL))):
                    table = base_column.table
                    if isinstance(table, SQLTable):
                        result = base_column
                    elif isinstance(table, SQLView):
                        result = original_column(base_column)
            return result
        def make_column(c):
            return Column(c.name, _alchemy2pytis_type(c.type), original_column=original_column(c))
        fields = [make_column(c) for c in query.c]
        return fields
    
class SQLView(_SQLBaseView):
    """View specification.

    Views are similar to tables in that they have columns.  But unlike tables
    view columns are not defined in the view specification directly, they are
    defined implicitly by included tables and their columns.

    The primary specification element of this class is 'query()' class method
    which returns the expression defining the view.  The method must return an
    'sqlalchemy.ClauseElement' instance (as all the SQLAlchemy expressions do).

    Special specification element is 'join_columns()' class method returning
    tuple of tuples of equivalent columns ('sqlalchemy.Column' instances).  If
    the view is made by joining some relations, duplicate join columns are
    excluded from the view and there are modification rules defined for the
    view then it is necessary to enumerate all equivalent relation columns in
    'join_columns()' tuples in order to make gensqlalchemy generate complete
    and correct rules.

    The expression in 'query()' method is typically constructed using means
    provided by SQLAlchemy.  The class defines a few additional utility class
    methods which may be useful for constructing the expression:
    'SQLView._exclude()', 'SQLView._alias()', 'SQLView._reorder()'.  See their
    documentation strings for more information.

    Properties:

      primary_column -- name (label) of the "key" column; basestring.  It can
        be left 'None' in most cases.  It must be defined only when the view is
        a part of another view which defines update or delete rules modifying
        the view.  The primary column is the view column uniquely identifying
        the view rows.

    """
    _DB_OBJECT = 'VIEW'
    _REPLACE_ON_CREATE = True

    @classmethod
    def join_columns(class_):
        return ()

    __visit_name__ = 'view'
        
    def _equivalent_rule_columns(self, column):
        equivalents = []
        if column is not None:
            equivalents = [column]
        for equivalent_columns in self.join_columns():
            if column in equivalent_columns:
                for c in equivalent_columns:
                    if c is not column:
                        equivalents.append(c)
        return equivalents
    
    def _hidden_rule_columns(self, tabular):
        hidden_columns = []
        original_columns = self._original_columns()
        for equivalent_columns in self.join_columns():
            alias = None
            for c in equivalent_columns:
                if c in original_columns:
                    alias = c.name
                    break
            if alias:
                for c in equivalent_columns:
                    if c.table is tabular and c not in original_columns:
                        hidden_columns.append(c.label(alias))
        return hidden_columns

    def _default_rule_commands(self):
        return ('NOTHING',)

@compiles(SQLView)
def visit_view(element, compiler, **kw):
    return '"%s"."%s"' % (element.schema, element.name,)

class SQLMaterializedView(_SQLIndexable, _SQLBaseView):
    """Materialized view specification.

    Specifications of materialized views are similar to view specifications.
    The primary difference is that materialized views can't have rules so rule
    related facilities are not available.

    Properties:

      index_columns -- tuple of tuples or 'Arguments' instances.  Each of the
        tuples or 'Arguments' instances contains names of columns or SQLAlchemy
        expressions to include within a single index.  An additional keyword
        argument 'method' may be provided in an 'Arguments' whose value
        determines indexing method to be used for this index.  The optional
        argument 'unique' in 'Arguments' marks the index as unique if the
        argument value is true.  Use 'index_columns' only for multicolumn indexes
        and functional indexes, plain single column indexes should be specified
        directly in the corresponding 'Column' specifications.

    """
    _DB_OBJECT = 'MATERIALIZED VIEW'
    _REPLACE_ON_CREATE = False

    __visit_name__ = 'materialized_view'

    def _default_rule_commands(self):
        return None

@compiles(SQLMaterializedView)
def visit_materialized_view(element, compiler, **kw):
    return '"%s"."%s"' % (element.schema, element.name,)

class SQLType(_SQLTabular):
    """Database type specification.

    Subclasses of this class define new types to be stored in the database.
    There is rarely need to do this.  Typical use of database types is to
    define return value types of functions returning multicolumn values which
    is handled automatically in 'SQLFunctional' objects and there is no need to
    define such types manually.

    This class doesn't introduce new properties.  You typically define just the
    fields the type should contain.

    """
    _DB_OBJECT = 'TYPE'

    __visit_name__ = 'type'

    def __new__(cls, metadata, search_path):
        columns = tuple([c.sqlalchemy_column(search_path, None, None, None)
                         for c in cls.fields])
        args = (cls.name, metadata,) + columns
        return sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])

    @classmethod
    def specification_fields(class_):
        return class_.fields

    def pytis_exists(self, metadata):
        name = self.pytis_name(real=True)
        with _metadata_connection(metadata) as connection:
            return metadata.pytis_engine.dialect.has_type(connection, name, schema=self.schema)

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)
            
    def drop(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaDropper, self, checkfirst=checkfirst)
    
class SQLFunctional(_SQLReplaceable, _SQLTabular):
    """Base class of function definitions.

    There are several kinds of supported database functions (SQL, PL/pgSQL,
    PL/Python) but they share most their properties:

      arguments -- tuple of function arguments represented by 'Column'
        instances in the given order.  If there are output arguments, mark them
        using 'out' argument in 'Column' constructor.
      result_type -- return type of the function.  It can be either instance of
        'pytis.data.Type' (simple return value of the given type) or a 'Column'
        instance (simple return value of the given column type) or a tuple of
        'Column' instances (composite return value of the given column types)
        or a tabular specification (composite return value of values
        corresponding to the given object columns) or 'SQLFunctional.RECORD'
        constant (the return value is defined by output function arguments) or
        'None' (there is no return value)
      multirow -- iff true then the function may return multiple results
        (rows); boolean
      security_definer -- iff true then the function runs with permissions of
        its creator rather than the user invoking it; boolean
      stability -- information about stability of the function return values to
        the database query optimizer, the given value (string) is used directly
        in the function definition.  This is useful to set if the function
        returns the same values for the same input data and without any side
        effects.
      execution_cost -- 'cost' parameter of a PostgreSQL function; positive
        integer or 'None'
      expected_rows -- 'rows' parameter of a PostgreSQL function; positive
        integer or 'None'
      set_parameters -- parameters to be saved and possibly set on entering the
        function.  It corresponds to 'set' parameter of PostgreSQL function.
        It's a sequence of pairs (PARAMETER, VALUE) where PARAMETER is the
        parameter name (basestring) and VALUE is its value (number or
        basestring).  VALUE must be properly surrounded by quotes if it should
        be output as a string.  VALUE may be also 'None' in which case
        'FROM CURRENT' is used.
      sql_directory -- name of the directory where SQL files with function body
        definitions are stored; the name is relative to the processed module
        file name

    Function body can be defined in two ways:

    - By putting the function definition in a file named FUNCTION_NAME.sql
      where FUNCTION_NAME is the name of the function as defined by 'name'
      property.  The file must be present in the current working directory when
      specifications are processed for SQL output.

    - By redefining 'body()' function to return the function body as a
      basestring.

    Functions can be used in SQL expressions of other specifications by calling
    their instances with corresponding function argument values (of the types
    required by SQLAlchemy).  You can get the function instance using
    'TableLookup' accessor.

    """
    _DB_OBJECT = 'FUNCTION'

    arguments = ()
    result_type = None
    multirow = False
    security_definer = False
    stability = 'volatile'
    execution_cost = None
    expected_rows = None
    set_parameters = ()
    sql_directory = 'sql'
    function_name = None        # obsolete

    _LANGUAGE = None

    __visit_name__ = 'function'

    RECORD = 'RECORD'

    def __new__(cls, metadata, search_path):
        with _local_search_path(search_path):
            # It would be nice to utilize specification_fields here.
            result_type = cls.result_type
            if result_type is None:
                columns = ()
            elif result_type == cls.RECORD:
                columns = tuple([c.sqlalchemy_column(search_path, None, None, None)
                                 for c in cls.arguments if c.out()])
            elif isinstance(result_type, (tuple, list,)):
                columns = tuple([c.sqlalchemy_column(search_path, None, None, None)
                                 for c in result_type])
            elif isinstance(result_type, Column):
                columns = (result_type.sqlalchemy_column(search_path, None, None, None),)
            elif isinstance(result_type, pytis.data.Type):
                columns = (sqlalchemy.Column('result', result_type.sqlalchemy_type()),)
            elif result_type is G_CONVERT_THIS_FUNCTION_TO_TRIGGER:
                columns = ()
            elif issubclass(result_type, _SQLTabular):
                columns = tuple([sqlalchemy.Column(c.name, c.type)
                                 for c in object_by_class(result_type, search_path).c])
            else:
                raise SQLException("Invalid result type", result_type)
            args = (cls.name, metadata,) + columns
            return sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])

    @classmethod
    def specification_fields(class_):
        result_type = class_.result_type
        if result_type is None:
            columns = ()
        elif result_type == class_.RECORD:
            columns = tuple([c for c in class_.arguments if c.out()])
        elif isinstance(result_type, (tuple, list,)):
            columns = tuple(result_type)
        elif isinstance(result_type, Column):
            columns = (result_type,)
        elif isinstance(result_type, pytis.data.Type):
            columns = (Column('result', result_type.sqlalchemy_type()),)
        elif result_type is G_CONVERT_THIS_FUNCTION_TO_TRIGGER:
            columns = ()
        elif issubclass(result_type, _SQLTabular):
            columns = result_type.specification_fields()
        else:
            raise SQLException("Invalid result type", result_type)
        return columns

    def _add_dependencies(self):
        super(SQLFunctional, self)._add_dependencies()
        result_type = self.result_type
        if ((result_type not in (None, G_CONVERT_THIS_FUNCTION_TO_TRIGGER,) and
             not isinstance(result_type, (tuple, list, Column, pytis.data.Type,)) and
             result_type != SQLFunctional.RECORD)):
            self.add_is_dependent_on(object_by_class(result_type, self._search_path))
        for a in self.arguments:
            if isinstance(a, Argument):
                t = a.type()
                if isinstance(t, _TabularType):
                    o = object_by_class(t.tabular(), self._search_path)
                    self.add_is_dependent_on(o)

    def pytis_exists(self, metadata):
        # This can't distinguish between functions overloaded by argument types
        with _metadata_connection(metadata) as connection:
            return self._pytis_definition(connection) != ''

    def _pytis_columns_changed(self, metadata):
        return False
        
    def _pytis_definition(self, connection):
        # This can't distinguish between functions overloaded by argument types
        self._pytis_set_definition_schema(connection)
        name = self.pytis_name(real=True)
        schema = self.schema
        nargs = len(self.arguments)
        query = ("select pg_catalog.pg_get_functiondef(pg_proc.oid) "
                 "from pg_proc join pg_namespace n on pronamespace = n.oid "
                 "where nspname = :schema and proname = :name and pronargs = :nargs")
        result = connection.execute(
            sqlalchemy.text(query,
                            bindparams=[sqlalchemy.bindparam('schema', schema,
                                                             type_=sqlalchemy.String),
                                        sqlalchemy.bindparam('name', name,
                                                             type_=sqlalchemy.String),
                                        sqlalchemy.bindparam('nargs', nargs,
                                                             type_=sqlalchemy.Integer)]))
        definition = ''
        for row in result:
            definition += row[0]
        result.close()
        return definition
        
    def pytis_changed(self, metadata, strict=True):
        if self._pytis_columns_changed(metadata):
            return True
        if isinstance(self.body, types.MethodType):
            # We don't handle (i.e. we signal change on unchanged functions)
            # the following cases:
            # - overloaded functions
            # - some schema related situations
            with _local_search_path(self.search_path()):
                body = self.body()
            if not isinstance(body, basestring):
                body = unicode(body)
            marker = '$function$'
            header, footer = self._pytis_header_footer(search_path=self.search_path(),
                                                       marker=marker)
            body = header.replace('"', '').lower() + body.strip() + footer
            with _metadata_connection(metadata) as connection:
                db_body = self._pytis_definition(connection)
            pos = db_body.find(marker)
            if pos >= 0:
                db_body = (db_body[:pos].replace('\n', ' ').replace('  ', ' ').lower() +
                           db_body[pos:])
            return body.strip() != db_body.strip()
        return False

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)
            
    def drop(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaDropper, self, checkfirst=checkfirst)

    @classmethod
    def pytis_name(class_, real=False):
        if real and class_.function_name:
            _warn("function_name attribute is obsolete in `%s', use db_name instead" %
                  (class_.name,))
            return class_.function_name
        return super(SQLFunctional, class_).pytis_name(real=real)

    def __call__(self, *arguments):
        """Return 'sqlalchemy.ClauseElement' corresponding to function call.

        Arguments:

          arguments -- tuple of function argument values, of types as required
            by SQLAlchemy
        
        """
        function_name = self.pytis_name(real=True)
        name = '"%s"."%s"' % (self.schema, function_name,)
        # We can't use the standard SQLAlchemy function call here
        # (i.e. getattr(sqlalchemy.sql.expression.func, name)(*arguments))
        # since this puts argument symbols instead of argument values into the
        # argument list.
        argument_list = [unicode(_sql_value_escape(a)) for a in arguments]
        expression = u'%s(%s)' % (name, string.join(argument_list, ', '),)
        return sqlalchemy.sql.expression.TextClause(expression)

    def body(self):
        """Return function body as basestring.

        The default implementation reads the function body from the file named
        FUNCTION_NAME.sql where FUNCTION_NAME is the name of the function as
        defined by 'name' property.

        """
        module_path = os.path.dirname(sys.modules[self.__module__].__file__)
        sql_file = os.path.join(module_path, self.sql_directory, self.name + '.sql')
        return codecs.open(sql_file, encoding='UTF-8').read()

    def _pytis_header_footer(self, result_type=None, search_path=None, marker='$$', suffix=''):
        arguments = _function_arguments(self)
        if result_type is None:
            function_type = self.result_type
            if function_type is None:
                if isinstance(self, SQLTrigger):
                    result_type = 'trigger'
                else:
                    result_type = 'void'
            elif function_type == SQLFunctional.RECORD:
                result_type = 'RECORD'
            elif function_type is G_CONVERT_THIS_FUNCTION_TO_TRIGGER:
                result_type = 'trigger'
            elif isinstance(function_type, (tuple, list,)):
                result_type = 't_' + self.pytis_name()
            elif isinstance(function_type, Column):
                c = function_type.sqlalchemy_column(search_path, None, None, None)
                result_type = c.type.compile(_engine.dialect)
            elif isinstance(function_type, pytis.data.Type):
                result_type = function_type.sqlalchemy_type().compile(_engine.dialect)
            elif issubclass(function_type, _SQLTabular):
                result_type = object_by_class(function_type, search_path).pytis_name()
            else:
                raise SQLException("Invalid result type", (self.__class__.__name__, function_type,))
        result_type_prefix = 'SETOF ' if self.multirow else ''
        name = self.pytis_name(real=True) + suffix
        security = ' SECURITY DEFINER' if self.security_definer else ''
        stability = ' ' + self.stability if self.stability != 'volatile' else ''
        query_prefix = (('CREATE OR REPLACE FUNCTION "%s"."%s"(%s) RETURNS %s%s '
                         'LANGUAGE %s%s%s AS %s\n') %
                        (self.schema, name, arguments, result_type_prefix, result_type,
                         self._LANGUAGE, stability, security, marker,))
        query_suffix = '\n%s' % (marker,)
        return query_prefix, query_suffix
        
class SQLFunction(_SQLQuery, SQLFunctional):
    """SQL function definition.

    This class doesn't define anything new, see its superclass for information
    about function definition.

    In addition to other classes, 'body()' may return
    'SQLAlchemy.sql.ClauseElement' instance or sequence of
    'SQLAlchemy.sql.ClauseElement' instances.

    """
    _LANGUAGE = 'sql'
    
    def _pytis_query_objects(self):
        return [self.body()]
        
class SQLPlFunction(SQLFunctional):
    """PL/pgSQL function definition.

    This class doesn't define anything new, see its superclass for information
    about function definition.

    """
    _LANGUAGE = 'plpgsql'
    
class SQLPyFunction(SQLFunctional):
    """PL/Python function definition.

    Definitions of PL/Python functions are a bit different from other kinds of
    function definitions.  They share the same properties but their body is
    defined in a different way (although nothing prevents you from defining
    your own 'body()' method the same way as in other kinds of functions).

    The function itself is defined as static method of the specification
    class.  The method must have the same name and arguments as the function.
    Therefore there are some limits on the name of the function, e.g. it is not
    a good idea to name a PL/Python function 'body'.

    You can define utility Python functions, methods and attributes for use in
    the PL/Python function.  The utility objects are defined in 'Util' inner
    class, see its documentation for more information.  Objects defined this
    way are available for use in the PL/Python function.
    
    Utility functions and classes are typically defined in a common base class
    inherited by specifications of PL/Python functions or in a common
    'SQLPyFunction.Util' subclass inherited by 'Util' inner classes in
    specifications.

    """
    _LANGUAGE = 'plpythonu'

    _SUBROUTINE_MATCHER = re.compile('( *)(@staticmethod\r?\n?|class )', re.MULTILINE)

    class Util(object):
        """Inner class for utility definitions to be used in the function.

        All static methods, classes, and attributes containing primitive type
        values or class instances providing proper 'repr' output defined in
        this class are available to the PL/Python function with the exception
        of objects whose names start with underscore.

        You can refer from the PL/Python function to the objects defined in
        this class by their fully qualified names.  For example, if you define
        auxiliary function 'bar' in 'Foo' PL/Python function specification, you
        can call the function using the name 'Foo.Util.bar' in the function
        code.

        """
    
    def body(self):
        # The method itself
        main_method_name = self.name
        arguments = inspect.getargspec(getattr(self, main_method_name)).args
        arglist = string.join(arguments, ', ')
        lines = ['#def %s(%s):' % (self.name, arglist,)]
        if arglist:
            l = '    %s = args' % (arglist,) # hard-wired indentation
            if len(self.arguments) == 1:
                l += '[0]'
            lines.append(l)
        def strip_header(lines):
            while lines and not lines[0].rstrip().endswith('):'):
                lines.pop(0)
            lines.pop(0)
        main_lines = self._method_source_lines(main_method_name, getattr(self, main_method_name), 0)
        strip_header(main_lines)
        # Auxiliary objects
        def output_method(name, method, indentation, prefix=None):
            function_lines = self._method_source_lines(name, method, indentation)
            function_lines = function_lines[1:]
            if prefix:
                first_line = function_lines[0]
                i = first_line.find(prefix)
                j = i + len(prefix)
                function_lines[0] = first_line[:i] + first_line[j:]
            else:
                function_lines.insert(0, ' ' * indentation + '@staticmethod')
            lines.extend(function_lines)
        def output_class(name, class_, indentation, prefix=None):
            class_lines = self._method_source_lines(name, class_, indentation)
            if prefix:
                first_line = class_lines[0]
                i = first_line.find(prefix)
                j = i + len(prefix)
                class_lines[0] = first_line[:i] + first_line[j:]
            lines.extend(class_lines)
        def output_attribute(name, value, indentation):
            line = '%s%s = %s' % (' ' * indentation, name, repr(value),)
            lines.append(line)
        names = [n for n in dir(self.Util) if n[0] != '_']
        if names:
            lines.append('    class %s:\n        class Util:' % (self.__class__.__name__,))
            for n in names:
                obj = getattr(self.Util, n)
                obj_type = type(obj)
                if issubclass(obj_type, types.FunctionType):
                    output_method(n, obj, 12)
                elif issubclass(obj_type, types.TypeType):
                    output_class(n, obj, 12)
                else:
                    output_attribute(n, obj, 12)
        # Backward compatibility
        prefix = 'sub_'
        for name in dir(self):
            if name.startswith(prefix):
                output_method(name, getattr(self, name), 4, prefix=prefix)
        prefix = 'Sub_'
        for name in dir(self):
            if name.startswith(prefix):
                output_class(name, getattr(self, name), 4, prefix=prefix)
        # Final result
        lines += main_lines
        return string.join(lines, '\n')

    def _method_source_lines(self, name, method, indentation):
        try:
            lines = inspect.getsourcelines(method)[0]
        except Exception as e:
            raise SQLException("Invalid plpythonu method", (self.__class__.__name__, name, e))
        match = self._SUBROUTINE_MATCHER.match(lines[0])
        if not match:
            raise SQLException("@staticmethod decorator not found", (self.__class__.__name__, name))
        indentation = indentation - len(match.group(1))
        if indentation == 0:
            def reindent(line):
                return line
        elif indentation > 0:
            def reindent(line):
                return ' ' * indentation + line
        else:
            def reindent(line):
                return line[-indentation:]
        lines = [unicode(l.rstrip(), 'utf-8') for l in lines]
        return [reindent(l) for l in lines if l.strip()]

class SQLAggregate(SQLFunctional):
    """Aggregate function definition.

    Aggregate functions can be basically defined like other database
    functions.  Function arguments define the internal state type (the first
    argument) and the input data types (the rest of the arguments).  The
    function definition itself serves as the internal state transition
    function.

    The following optional properties may be defined:

      initial_value -- the initial state value; internal value of the internal
        state pytis type

    """
    _DB_OBJECT = 'AGGREGATE'

    initial_value = None

    __visit_name__ = 'aggregate'

class SQLEventHandler(SQLFunctional):
    """Definition of a function serving as a table event handler.

    This is basically a normal function defining just one additional property:

      table -- specification of the table the function is bound to

    If table is 'None' then the corresponding function (e.g. trigger function)
    is defined while the event handler itself (e.g. trigger) is not.  You can
    then assign the function to any number of table event handlers via
    'SQLTable' properties (such as 'triggers').

    Subclasses may define more additional properties.
    
    """
    table = None

class SQLTrigger(SQLEventHandler):
    """Trigger definition.

    Trigger is defined as a normal function, just note that SQL functions can't
    be used as triggers in PostgreSQL.  The following properties may be defined
    in triggers in addition to 'SQLEventHandler' properties:

      events -- tuple of table events the trigger should be invoked for; each
        of the events must be one of strings 'insert', 'update', 'delete',
        'truncate'.
      position -- whether the trigger should be invoked before or after table
        modification; one of the strings 'before' and 'after'
      each_row -- if true then the trigger should be invoked for each modified
        table row otherwise it should be invoked once per statement
      call_arguments -- if the trigger function has any arguments then this
        property must define their values and in the right order; tuple of
        values of types accepted by SQLAlchemy for given argument types.
    
    """
    __metaclass__ = _PytisTriggerMetaclass
    _DB_OBJECT = 'TRIGGER'
    
    events = ('insert', 'update', 'delete',)
    position = 'after'
    each_row = True
    call_arguments = ()

    __visit_name__ = 'trigger'
        
    def __new__(cls, metadata, search_path):
        if cls.name is None:
            if cls.table is None:
                raise SQLException("Table not set in anonymous trigger", cls)
            name = cls.table.pytis_name(real=True) + '__'
            name += string.join([e[:3] for e in cls.events], '_')
            name += '__' + cls.position
            cls.name = name
        return SQLEventHandler.__new__(cls, metadata, search_path)

    def _add_dependencies(self):
        super(SQLTrigger, self)._add_dependencies()
        search_path = (self.schema,)
        if self.table is not None:
            t = object_by_class(self.table, search_path=search_path)
            assert t is not None, ("Trigger table not found", self)
            self.add_is_dependent_on(t)
        if not isinstance(self.body, types.MethodType):
            self.add_is_dependent_on(object_by_class(self.body, search_path=search_path))

    def pytis_exists(self, metadata):
        if self.table is None:
            return super(SQLTrigger, self).pytis_exists(metadata)
        else:
            query = ("select count(*) from "
                     "pg_trigger join "
                     "pg_class on tgrelid = pg_class.oid "
                     "where tgname = :tgname and relname = :name")
            with _metadata_connection(metadata) as connection:
                result = connection.execute(
                    sqlalchemy.text(
                        query,
                        bindparams=[sqlalchemy.bindparam('tgname', self.pytis_name(real=True),
                                                         type_=sqlalchemy.String),
                                    sqlalchemy.bindparam('name', self.table.pytis_name(real=True),
                                                         type_=sqlalchemy.String)]))
                n = result.fetchone()[0]
                result.close()
            return n > 0
            
    def pytis_changed(self, metadata, strict=True):
        if super(SQLTrigger, self).pytis_changed(metadata, strict=strict):
            return True
        if self.table is not None:
            query = ("select pg_catalog.pg_get_triggerdef(pg_trigger.oid) from "
                     "pg_trigger join "
                     "pg_class on tgrelid = pg_class.oid "
                     "where tgname = :tgname and relname = :name")
            with _metadata_connection(metadata) as connection:
                result = connection.execute(
                    sqlalchemy.text(
                        query,
                        bindparams=[sqlalchemy.bindparam('tgname', self.pytis_name(real=True),
                                                         type_=sqlalchemy.String),
                                    sqlalchemy.bindparam('name', self.table.pytis_name(real=True),
                                                         type_=sqlalchemy.String)]))
                definition = result.fetchone()[0]
                if result.fetchone() is not None:
                    _warn("Multiple definitions of trigger `%s'" % (self.name,))
                result.close()
            match = re.match(("CREATE TRIGGER [^ ]+ (BEFORE|AFTER) (.*) ON [^ ]+ "
                              "FOR EACH (ROW|STATEMENT) EXECUTE PROCEDURE .*[(](.*)[)]"),
                             definition)
            if match is None:
                _warn("Can't match trigger definition: %s" % (self.name,))
                return True
            if match.group(1).lower() != self.position:
                return True
            if match.group(3) == 'ROW' and not self.each_row:
                return True
            db_events = [e.lower() for e in match.group(2).split(' OR ')]
            db_events.sort()
            events = list(self.events)
            events.sort()
            if db_events != events:
                return True
            arguments = repr(self.arguments)[1:-1]
            if arguments and arguments[-1] == ',':
                arguments = arguments[:-1]
            if arguments != match.group(4):
                return True
            return False

    def __call__(self, *arguments):
        if isinstance(self.body, types.MethodType):
            return super(SQLTrigger, self).__call__(*arguments)
        return object_by_class(self.body)(*arguments)

    def _create_comments(self):
        if self.table is not None:
            super(SQLTrigger, self)._create_comments()

class SQLRaw(sqlalchemy.schema.DDLElement, SQLSchematicObject):
    """Raw SQL definition.

    Do not use raw definitions.  If you think you really need one, ask for
    extension of gensqlalchemy to handle your construction.  If this is not
    possible you may define the following properties:

      name -- name of the raw definition, string
      depends_on -- sequence of specification classes this class depends on

    The raw definition must be returned from class method named 'sql'.

    """
    __metaclass__ = _PytisSchematicMetaclass
    
    name = None
    depends_on = ()
    error_level = 0
    
    __visit_name__ = 'raw'
    
    def __init__(self, metadata, search_path):
        self._extra_dependencies = set()
        super(SQLRaw, self).__init__()
        self._search_path = search_path
        self._add_dependencies()
        self.schema = search_path[0] # required by metadata in certain situations
        metadata._add_table(self.pytis_name(), search_path[0], self)
        
    def search_path(self):
        return self._search_path

    def add_is_dependent_on(self, table):
        self._extra_dependencies.add(table)
    
    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

    def pytis_drop(self):
        _warn("Can't drop raw object: %s" % (self.name,))


# Specification processing

def _db_dependencies(metadata):
    connection = metadata.pytis_engine.connect()
    def row_identifier(row):
        schema, name = row
        return '"%s"."%s"' % (schema, name,)
    def load(query, otype, row_identifier=row_identifier):
        dictionary = {}
        for row in connection.execute(sqlalchemy.text(query)):
            oid = row[0]
            dictionary[oid] = otype + ' ' + row_identifier(tuple(row[1:]))
        return dictionary
    pg_class = load(("select pg_class.oid, nspname, relname "
                     "from pg_class join pg_namespace on relnamespace=pg_namespace.oid "
                     "where relkind not in ('c', 'v')"), 'TABLE')
    pg_namespace = load("select oid, nspname from pg_namespace", 'SCHEMA',
                        lambda row: '"%s"' % (row[0],))
    pg_class.update(load(("select pg_class.oid, nspname, relname "
                          "from pg_class join pg_namespace on relnamespace=pg_namespace.oid "
                          "where relkind = 'c'"), 'TYPE'))
    pg_type = load(("select pg_type.oid, nspname, typname "
                    "from pg_type join pg_namespace on typnamespace=pg_namespace.oid"), 'TYPE')
    pg_proc = load(("select pg_proc.oid, nspname, proname, '' "
                    "from pg_proc join pg_namespace on pronamespace=pg_namespace.oid"), 'FUNCTION',
                   lambda row: '"%s"."%s"(%s)' % row)
    pg_trigger = load(("select pg_trigger.oid, tgname, nspname, relname "
                       "from pg_trigger join pg_class on tgrelid = pg_class.oid join "
                       "pg_namespace on relnamespace=pg_namespace.oid"), 'TRIGGER',
                      lambda row: '"%s" ON "%s"."%s"' % row)
    # Views are tricky, they are both tables and rules, with the roles in
    # dependencies mixed.  So we must consider both forms and exclude
    # dependencies on itself (rule on the view) below.
    pg_class.update(load(("select pg_class.oid, nspname, relname "
                          "from pg_class join pg_namespace on relnamespace=pg_namespace.oid "
                          "where relkind = 'v'"), 'VIEW'))
    pg_rewrite = load(("select pg_rewrite.oid, nspname, relname "
                       "from pg_rewrite join pg_class on ev_class = pg_class.oid join "
                       "pg_namespace on relnamespace=pg_namespace.oid"), 'VIEW')
    loc = locals()
    dependencies = []
    query = ("select distinct objid, c.relname, refobjid, refc.relname "
             "from pg_depend join "
             "pg_class c on (classid=c.oid) join pg_namespace n on (c.relnamespace=n.oid) join "
             "pg_class refc on (refclassid=refc.oid) join "
             "pg_namespace refn on (refc.relnamespace=refn.oid) "
             "where n.nspname='pg_catalog' and refn.nspname='pg_catalog' and "
             "c.relname in "
             "('pg_class', 'pg_proc', 'pg_type', 'pg_namespace', 'pg_trigger', 'pg_rewrite') and "
             "refc.relname in "
             "('pg_class', 'pg_proc', 'pg_type', 'pg_namespace', 'pg_trigger', 'pg_rewrite') and "
             "deptype = 'n'")
    result = connection.execute(query)
    # Let's reduce the set and break unimportant circular dependencies:
    regexp = re.compile('[A-Z]+ "(pg_catalog|information_schema)"\.|'
                        'FUNCTION "public"\."(ltree|ltxtq|lquery)_')
    for oid, reltypename, refoid, refreltypename in result:
        try:
            base = loc[refreltypename][refoid]
            dependent = loc[reltypename][oid]
        except KeyError:
            # Some objects may be no longer present in the database
            continue
        if ((regexp.match(base) is None and regexp.match(dependent) is None and
             base != dependent)):
            dependencies.append((base, dependent,))
    result.close()
    connection.close()
    return dependencies

_engine = None
def _make_sql_command(sql, *multiparams, **params):
    if isinstance(sql, str):
        output = unicode(sql)
    elif isinstance(sql, unicode):
        output = sql
    else:
        compiled = sql.compile(dialect=_engine.dialect)
        if isinstance(sql, sqlalchemy.sql.expression.Insert):
            # SQLAlchemy apparently doesn't work so well without a database
            # connection.  We probably have no better choice than to handle some
            # things manually here, despite the corresponding functionality is
            # present in SQLAlchemy.
            parameters = {}
            sql_parameters = sql.parameters
            if set(compiled.binds.keys()) - set(sql_parameters.keys()):
                for k in compiled.binds.keys():
                    if k not in sql_parameters:
                        column = sql.table.columns[k]
                        if column.default is not None:
                            parameters[k] = _sql_value_escape(column.default.arg)
                # Perhaps also default key value
                if isinstance(sql.table, SQLTable):
                    for k in sql.table.pytis_key:
                        if k not in sql_parameters:
                            column = sql.table.columns[k]
                            parameters[k] = ("nextval('%s.%s_%s_seq')" %
                                             (column.table.schema, column.pytis_orig_table,
                                              column.name,))
            for k, v in sql_parameters.items():
                parameters[k] = _sql_value_escape(v)
            output = unicode(compiled) % parameters
        elif isinstance(sql, sqlalchemy.sql.expression.Select):
            output = unicode(compiled) % compiled.params
        else:
            output = unicode(compiled)
        if hasattr(sql, 'pytis_prefix'):
            output = sql.pytis_prefix + output
        if hasattr(sql, 'pytis_suffix'):
            output = output + sql.pytis_suffix
    return output
def _dump_sql_command(sql, *multiparams, **params):
    output = _make_sql_command(sql, *multiparams, **params)
    if not output:
        return
    output_string = output + ';'
    if _pretty:
        output_string += '\n'
    if _pretty > 1:
        output_string = re.sub(' (UNION|EXCEPT|INTERSECT)( ALL|) ', '\n\\1\\2\n', output_string)
    _gsql_output(output_string)

def include(file_name, globals_=None):
    """Include specification file into current specification.

    Arguments:

      file_name -- name of the specification file to include; basestring
      globals_ -- global dictionary to use; most often 'globals()' should be
        given here

    """
    if globals_ is None:
        globals_ = globals()
    file_, pathname, description = imp.find_module(file_name)
    execfile(pathname, globals_)

_output = None
def _gsql_output(output):
    try:
        _output.write(output)
    except UnicodeEncodeError:
        _output.write(output.encode('utf-8'))
    _output.write('\n')

_debug = False
_pretty = 0
def _gsql_process(loader, regexp, no_deps, views, functions, names_only, pretty, schema, source,
                  config_file, upgrade, debug, module_name):
    global _output
    if upgrade:
        if alembic is None:
            _error("`alembic' package missing, can't perform upgrade.")
            return
        _output = cStringIO.StringIO()
    else:
        _output = sys.stdout
        if _output.encoding is None:
            _output = codecs.getwriter('UTF-8')(_output)
    global _debug
    _debug = debug
    global _pretty
    _pretty = pretty
    global _enforced_schema, _enforced_schema_objects
    _enforced_schema = schema
    _enforced_schema_objects = set()
    if upgrade:
        upgrade_metadata = sqlalchemy.MetaData()
        import config
        connection_data = dict(user=config.dbuser,
                               password=(':' + config.dbpass if config.dbpass else ''),
                               host=(config.dbhost or ''),
                               port=(':' + config.dbport if config.dbport else ''),
                               dbname=config.dbname)
        connection_string = ('postgresql://%(user)s%(password)s@%(host)s%(port)s/%(dbname)s' %
                             connection_data)
        upgrade_metadata.pytis_engine = sqlalchemy.create_engine(connection_string)
        upgrade_metadata.pytis_inspector = sqlalchemy.inspect(upgrade_metadata.pytis_engine)
        upgrade_metadata.pytis_changed = set()
    else:
        upgrade_metadata = None
    _gsql_process_1(loader, regexp, no_deps, views, functions, names_only, source,
                    upgrade_metadata, module_name)

def _gsql_process_1(loader, regexp, no_deps, views, functions, names_only, source,
                    upgrade_metadata, module_name):
    if regexp is not None:
        matcher = re.compile(regexp)
    matched = set()
    def matching(o):
        result = True
        if isinstance(o, SQLObject):
            cls = o.__class__
        else:
            cls = o
        if ((module_name is not None and
             not o.__module__ == module_name and
             not o.__module__.startswith(module_name + '.'))):
            return False
        if (views or functions):
            if issubclass(cls, SQLView):
                if not views:
                    result = False
            elif issubclass(cls, SQLFunctional) and not issubclass(cls, SQLTrigger):
                if not functions:
                    result = False
            else:
                result = False
        if cls.external:
            return False
        if regexp is None:
            return result
        for c in ([cls] + (cls._pytis_direct_dependencies or [])):
            if matcher.search(c.__name__):
                matched.add(o)
                return result
        if no_deps:
            return False
        if issubclass(cls, SQLTable):
            return False
        if isinstance(o, _SQLTabular):
            for d in o._extra_dependencies:
                if d in matched:
                    matched.add(o)
                    return result
        return False
    def output_name(obj):
        kind = obj.pytis_kind()
        name = obj.pytis_name(real=True)
        if kind == 'FUNCTION':
            def colstr(c):
                if isinstance(c, basestring):
                    return c
                return '%s::%s' % (c.id(), c.type().sqlalchemy_type(),)
            arguments = string.join([colstr(c) for c in obj.arguments], ',')
            name = '%s(%s)' % (name, arguments,)
        if _enforced_schema:
            schematic_names = ['%s.%s' % (_enforced_schema, name,)]
        elif isinstance(obj, SQLSchematicObject):
            schematic_names = ['%s.%s' % (obj.schema, name,)]
        elif isinstance(obj, SQLObject):
            schematic_names = [name]
        elif issubclass(obj, SQLSchematicObject):
            schematic_names = ['%s.%s' % (s[0], name,) for s in _expand_schemas(obj)]
        else:
            schematic_names = [name]
        if isinstance(obj, SQLObject):
            class_name = obj.__class__.__name__
        elif issubclass(obj, SQLObject):
            class_name = obj.__name__
        else:
            class_name = ''
        for n in schematic_names:
            output = '%s %s' % (kind, n,)
            if source:
                output = '%s:%s: %s %s' % (obj._gsql_file, obj._gsql_line, class_name, output,)
            _gsql_output(output)
        if upgrade_metadata is not None:
            upgrade_metadata.pytis_changed.add(obj)
    # Load the objects
    loader()
    # Mark objects with changed schemas
    if _enforced_schema and not names_only:
        for o, f in _PytisSchematicMetaclass.init_function_list:
            if matching(o):
                _enforced_schema_objects.add(o)
    # Preprocessing
    full_init = (not ((no_deps and names_only) or (no_deps and regexp)) or
                 upgrade_metadata is not None)
    for o, f in _PytisSchematicMetaclass.init_function_list:
        if matching(o):
            if names_only:
                output_name(o)
            else:
                _PytisSchematicMetaclass.call_init_function(f, may_alter_schema=True)
        elif full_init:
            _PytisSchematicMetaclass.call_init_function(f, may_alter_schema=True)
    if not full_init and names_only:
        return
    # Process all available objects
    def obj_identifier(o):
        if isinstance(o, SQLTrigger) and o.table is not None:
            t = o.table
            identifier = '%s "%s" ON "%s"."%s"' % (o._DB_OBJECT, o.pytis_name(real=True),
                                                   t.schema, t.pytis_name(real=True),)
        elif isinstance(o, SQLSchematicObject):
            identifier = '%s "%s"."%s"' % (o._DB_OBJECT, o.schema, o.pytis_name(real=True),)
            if isinstance(o, SQLFunctional):
                identifier += '(' + _function_arguments(o) + ')'
        else:
            identifier = '%s "%s"' % (o._DB_OBJECT, o.pytis_name(real=True),)
        return identifier
    id2obj = {}
    for o in _PytisSimpleMetaclass.objects:
        if matching(o):
            if names_only:
                output_name(o)
            elif upgrade_metadata is not None:
                o.pytis_upgrade(upgrade_metadata)
            else:
                o.pytis_create()
    for sequence in _metadata._sequences.values():
        if matching(sequence):
            if names_only:
                output_name(sequence)
            elif upgrade_metadata is not None:
                sequence.pytis_upgrade(upgrade_metadata)
            else:
                sequence.pytis_create()
    for table in _metadata.sorted_tables:
        id2obj[obj_identifier(table)] = table
        if matching(table):
            if names_only:
                output_name(table)
            elif upgrade_metadata is not None:
                table.pytis_upgrade(upgrade_metadata)
            else:
                table.pytis_create()
    if upgrade_metadata is None:
        for ffk in _forward_foreign_keys:
            if matching(ffk.table) and not names_only:
                target = ffk.reference.get(ffk.search_path)
                if target is None:
                    raise Exception('Invalid foreign reference',
                                    (ffk.table.name, ffk.reference.specification_name(),
                                     ffk.reference._column,))
                kwargs = ffk.kwargs
                kwargs['name'] += ffk.column_name + '__' + target.name.replace('.', '__')
                f = sqlalchemy.ForeignKeyConstraint((ffk.column_name,), (target,), *ffk.args,
                                                    table=ffk.table, **kwargs)
                fdef = sqlalchemy.schema.AddConstraint(f)
                _engine.execute(fdef)
    # Emit DROP commands
    if upgrade_metadata is not None and not names_only:
        global _output
        str_output = _output.getvalue()
        _output = sys.stdout
        if _output.encoding is None:
            _output = codecs.getwriter('UTF-8')(_output)
        dependencies = _db_dependencies(upgrade_metadata)
        all_items = set()
        dep_dict = {}
        for i1, i2 in dependencies:
            all_items.add(i1)
            all_items.add(i2)
            dep_dict[i1] = dep_dict.get(i1, []) + [i2]
        changed = set()
        for o in upgrade_metadata.pytis_changed:
            changed.add(obj_identifier(o))
        drop = []
        try:
            for o in sqlalchemy.util.topological.sort(dependencies, list(all_items)):
                if o in changed:
                    if not o.startswith('TABLE '):
                        drop.append(o)
                    queue = [o]
                    while queue:
                        for oo in dep_dict.get(queue.pop(0), []):
                            if oo not in changed:
                                changed.add(oo)
                                queue.append(oo)
        except sqlalchemy.exc.CircularDependencyError, e:
            _warn("Can't emit DROP commands due to circular dependencies.\n%s" % (e.args[0],))
        drop.reverse()
        for name in drop:
            o = id2obj.get(name)
            if o is not None:
                o.pytis_drop()
        drop_set = set(drop)
        for name in changed:
            if name not in drop_set:
                o = id2obj.get(name)
                if o is not None and not isinstance(o, SQLTable):
                    o.pytis_drop()
        # Emit update commands
        sys.stdout.write(str_output)
        # Recreate all dropped objects
        drop.reverse()
        for name in drop:
            o = id2obj.get(name)
            if o is None:
                _error("Can't recreate object `%s'" % (name,))
            elif o not in upgrade_metadata.pytis_changed:
                o.pytis_create()
            
    
def gsql_file(file_name, regexp=None, no_deps=False, views=False, functions=False,
              names_only=False, pretty=0, schema=None, source=False, config_file=None,
              upgrade=False, debug=False):
    """Generate SQL code from given specification file.

    Arguments:

      file_name -- name of the specification file to process; basestring
      regexp -- if not 'None', generate SQL code only for specifications with
        specification class names matching this regular expression and
        view and function specifications dependent on those specifications;
        basestring
      no_deps -- iff true, don't output dependent objects when 'regexp' is
        specified
      views -- iff true, output just views; boolean
      functions -- iff true, output just functions; boolean
      names_only -- iff true, output only kinds and names of the database
        objects; boolean
      pretty -- pretty output level; non-negative integer
      schema -- if not 'None' then create all objects in that schema; string
      source -- iff true, print source files and lines
      config_file -- name of pytis configuration file
      upgrade -- iff true, generate SQL commands for upgrade rather than creation
      debug -- iff true, print some debugging information to standard error output

    If both 'views' and 'functions' are specified, output both views and
    functions.

    The SQL code is output on standard output.

    """
    def loader(file_name=file_name):
        execfile(file_name, copy.copy(globals()))
    _gsql_process(loader, regexp, no_deps, views, functions, names_only, pretty, schema, source,
                  config_file, upgrade, debug, None)

def gsql_module(module_name, regexp=None, no_deps=False, views=False, functions=False,
                names_only=False, pretty=0, schema=None, source=False, config_file=None,
                upgrade=False, debug=False, limit_module=False):
    """Generate SQL code from given specification module.

    Arguments:

      module_name -- name of the specification module to process; basestring
      regexp -- if not 'None', generate SQL code only for specifications with
        specification class names matching this regular expression and
        view and function specifications dependent on those specifications;
        basestring
      no_deps -- iff true, don't output dependent objects when 'regexp' is
        specified
      views -- iff true, output just views; boolean
      functions -- iff true, output just functions; boolean
      names_only -- iff true, output only kinds and names of the database
        objects; boolean
      pretty -- pretty output level; non-negative integer
      schema -- if not 'None' then create all objects in that schema; string
      source -- iff true, print source files and lines
      config_file -- name of pytis configuration file
      upgrade -- iff true, generate SQL commands for upgrade rather than creation
      debug -- iff true, print some debugging information to standard error output
      limit_module -- iff true, limit output to specifications defined directly
        in the given module

    If both 'views' and 'functions' are specified, output both views and
    functions.

    The SQL code is output on standard output.

    """
    def loader(module_name=module_name):
        pytis.util.load_module(module_name)
    _gsql_process(loader, regexp, no_deps, views, functions, names_only, pretty, schema, source,
                  config_file, upgrade, debug, (module_name if limit_module else None))

def clear():
    "Clear all loaded specifications."
    _PytisBaseMetaclass.clear()
    _PytisSchematicMetaclass.clear()
    global _metadata
    _metadata = sqlalchemy.MetaData()
    global _engine
    _engine = sqlalchemy.create_engine('postgresql://', strategy='mock', executor=_dump_sql_command)

def specifications():
    "Return all loaded specification classes."
    return _PytisBaseMetaclass.specifications()

def specification_instances():
    "Return all instances of loaded specification classes."
    for o, f in _PytisSchematicMetaclass.init_function_list:
        _PytisSchematicMetaclass.call_init_function(f, may_alter_schema=True)
    return _PytisSimpleMetaclass.objects + _PytisSchematicMetaclass.objects

# Make sure _metadata and _engine are initialized to prevent crashes
clear()

# Local Variables:
# flycheck-flake8rc: "../../../tools/flake8.noredef"
# End:
