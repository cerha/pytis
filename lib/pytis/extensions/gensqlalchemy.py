#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2012, 2013 Brailcom, o.p.s.
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

import codecs
import collections
import copy
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


## SQLAlchemy extensions

G_CONVERT_THIS_FUNCTION_TO_TRIGGER = object()  # hack for gensql conversions

def _function_arguments(function):
    search_path = function.search_path()
    def arg(column):
        a_column = column.sqlalchemy_column(search_path, None, None, None)
        in_out = 'out ' if column.out() else ''
        name = a_column.name
        if name:
            name = '"%s" ' % (name,)
        return '%s%s%s' % (in_out, name, a_column.type.compile(_engine.dialect),)
    return string.join([arg(c) for c in function.arguments], ', ')

class _PytisSchemaGenerator(sqlalchemy.engine.ddl.SchemaGenerator):

    def _set_search_path(self, search_path):
        _set_current_search_path(search_path)
        path_list = [_sql_id_escape(s) for s in search_path]
        path = string.join(path_list, ',')
        command = 'SET SEARCH_PATH TO %s' % (path,)
        self.connection.execute(command)

    def visit_view(self, view, create_ok=False):
        self._set_search_path(view.search_path())
        command = 'CREATE OR REPLACE VIEW "%s"."%s" AS ' % (view.schema, view.name,)
        query = view.query()
        query.pytis_prefix = command
        self.connection.execute(query)
        view.dispatch.after_create(view, self.connection, checkfirst=self.checkfirst, _ddl_runner=self)

    def visit_type(self, type_, create_ok=False):
        self._set_search_path(type_.search_path())
        self.make_type(type_.name, type_.fields)
        type_.dispatch.after_create(type_, self.connection, checkfirst=self.checkfirst, _ddl_runner=self)

    def make_type(self, type_name, columns):
        sqlalchemy_columns = [c.sqlalchemy_column(None, None, None, None) for c in columns]
        def ctype(c):
            return c.type.compile(_engine.dialect)
        column_string = string.join(['%s %s' % (c.name, ctype(c),) for c in sqlalchemy_columns], ', ')
        command = 'CREATE TYPE %s AS (%s)' % (type_name, column_string,)
        self.connection.execute(command)

    def visit_function(self, function, create_ok=False, result_type=None):
        search_path = function.search_path()
        self._set_search_path(search_path)
        arguments = _function_arguments(function)
        if result_type is None:
            function_type = function.result_type
            if function_type is None:
                result_type = 'void'
            elif function_type == SQLFunctional.RECORD:
                result_type = 'RECORD'
            elif function_type is G_CONVERT_THIS_FUNCTION_TO_TRIGGER:
                result_type = 'trigger'
            elif isinstance(function_type, (tuple, list,)):
                result_type = 't_' + function.pytis_name()
                self.make_type(result_type, function_type)
            elif isinstance(function_type, Column):
                result_type = function_type.sqlalchemy_column(search_path, None, None, None).type.compile(_engine.dialect)
            elif isinstance(function_type, pytis.data.Type):
                result_type = function_type.sqlalchemy_type().compile(_engine.dialect)
            elif issubclass(function_type, (SQLTable, SQLType,)):
                result_type = object_by_class(function_type, search_path).pytis_name()
            else:
                raise SQLException("Invalid result type", function_type)
        result_type_prefix = 'SETOF ' if function.multirow else ''
        name = function.function_name or function.name
        query_prefix = ('CREATE OR REPLACE FUNCTION "%s"."%s" (%s) RETURNS %s%s AS $$\n' %
                        (function.schema, name, arguments, result_type_prefix, result_type,))
        query_suffix = ('\n$$ LANGUAGE %s %s' % (function._LANGUAGE, function.stability,))
        if function.security_definer:
            query_suffix += ' SECURITY DEFINER'
        body = function.body()
        if isinstance(body, basestring):
            body = body.strip()
            command = query_prefix + body + query_suffix
            self.connection.execute(command)
        else:
            query = body
            query.pytis_prefix = query_prefix
            query.pytis_suffix = query_suffix
            self.connection.execute(query)
        function.dispatch.after_create(function, self.connection, checkfirst=self.checkfirst, _ddl_runner=self)

    def visit_trigger(self, trigger, create_ok=False):
        if isinstance(trigger, (SQLPlFunction, SQLPyFunction,)):
            self.visit_function(trigger, create_ok=create_ok, result_type='trigger')
        if trigger.table and trigger.events:
            events = string.join(trigger.events, ' OR ')
            table = object_by_path(trigger.table.name, trigger.search_path())
            row_or_statement = 'ROW' if trigger.each_row else 'STATEMENT'
            trigger_call = trigger(*trigger.arguments).value
            command = (('CREATE TRIGGER "%s__%s" %s %s ON %s '
                        'FOR EACH %s EXECUTE PROCEDURE %s') %
                       (trigger.name, trigger.position, trigger.position, events, table,
                        row_or_statement, trigger_call,))
            self.connection.execute(command)
        trigger.dispatch.after_create(trigger, self.connection, checkfirst=self.checkfirst, _ddl_runner=self)

    def visit_raw(self, raw, create_ok=False):
        self._set_search_path(raw.search_path())
        self.connection.execute(raw.sql())

class _ObjectComment(sqlalchemy.schema.DDLElement):
    def __init__(self, obj, kind, comment):
        self.object = obj
        self.kind = kind
        self.comment = comment
@compiles(_ObjectComment)
def visit_object_comment(element, compiler, **kw):
    o = element.object
    if isinstance(o, SQLFunctional):
        extra = '(%s)' % (_function_arguments(o),)
    else:
        extra = ''
    return ("COMMENT ON %s \"%s\".\"%s\"%s IS '%s'" %
            (element.kind, o.schema, o.name, extra, element.comment.replace("'", "''"),))

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
    return ("GRANT %s ON %s%s%s%s TO \"%s\"" %
            (element.right, kind, schema, name, extra, element.group,))

class _Rule(sqlalchemy.schema.DDLElement):
    def __init__(self, table, action, kind, commands):
        self.table = table
        self.action = action
        self.kind = kind
        self.commands = commands
@compiles(_Rule)
def visit_rule(element, compiler, **kw):
    commands = element.commands
    if commands:
        sql_commands = [_make_sql_command(c) for c in commands]
        if len(sql_commands) == 1:
            sql = sql_commands[0]
        else:
            sql = '(%s)' % (string.join(sql_commands, '; '),)
    else:
        sql = 'NOTHING'
    table = element.table
    rule_name = '%s__%s_%s' % (table.name, element.action.lower(), element.kind.lower(),)
    return ('CREATE OR REPLACE RULE "%s" AS ON %s TO "%s"."%s" DO %s %s' %
            (rule_name, element.action, table.schema, table.name, element.kind, sql,))

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

class _SQLExternal(sqlalchemy.sql.expression.FromClause):

    def __init__(self, name):
        super(_SQLExternal, self).__init__()
        self.name = name

    class _PytisColumn(object):
        def __init__(self, name):
            self.name = name
        def __getattr__(self, name, *args, **kwargs):
            column = '%s.%s' % (self.name, name,)
            return sqlalchemy.literal_column(column)

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

@compiles(sqlalchemy.String, 'postgresql')
def compile_string(element, compiler, **kwargs):
    if element.length is None:
        return 'TEXT'
    else:
        return 'VARCHAR(%d)' % (element.length,)

@compiles(sqlalchemy.schema.CreateTable, 'postgresql')
def visit_create_table(element, compiler, **kwargs):
    result = compiler.visit_create_table(element, **kwargs)
    table = element.element
    if table.inherits:
        inherited = [table._table_name(t) for t in table.inherits]
        result = '%s\nINHERITS (%s)\n\n' % (result.rstrip(), string.join(inherited, ', '),)
    return result

@compiles(sqlalchemy.sql.expression.Alias)
def visit_alias(element, compiler, **kwargs):
    if element.description.find('(') >= 0:
        # Column aliases may not be quoted
        element.quote = False
    return compiler.visit_alias(element, **kwargs)

@compiles(sqlalchemy.dialects.postgresql.TIMESTAMP)
def visit_TIMESTAMP(element, compiler, **kwargs):
    # Current SQLAlchemy implementation is buggy: it omits zero precision.
    precision = getattr(element, 'precision', None)
    return "TIMESTAMP%s %s" % ('' if precision is None else "(%d)" % (precision,),
                               (element.timezone and "WITH" or "WITHOUT") + " TIME ZONE",)


## Columns
        
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
    def __init__(self, name, type, doc=None, unique=False, check=None,
                 default=None, references=None, primary_key=False, index=False, out=False):
        """
        Arguments:

          name -- name of the column; string containing only lower case English
            letters and underscores.  Empty strings are permitted in special
            cases where the column name doesn't matter such as in SQL function
            arguments, however even in such cases it is recommended to use a
            proper column name as a form of documentation.
          type -- column type; instance of 'pytis.data.Type' subclass
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
          
        """
        pytis.data.ColumnSpec.__init__(self, name, type)
        assert doc is None or isinstance(doc, basestring), doc
        assert isinstance(unique, bool), unique
        assert check is None or isinstance(check, basestring), check
        assert isinstance(primary_key, bool), primary_key
        assert isinstance(index, (bool, dict,)), index
        assert isinstance(out, bool), out
        self._doc = doc
        self._unique = unique
        self._check = check
        self._default = default
        self._references = references
        self._primary_key = primary_key
        self._index = index
        self._out = out

    def doc(self):
        return self._doc

    def index(self):
        return self._index

    def primary_key(self):
        return self._primary_key

    def out(self):
        return self._out

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
            if isinstance(r_args[0], (ReferenceLookup.Reference, _Reference)):
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
        column = sqlalchemy.Column(self.id(), alchemy_type, *args,
                                   server_default=default,
                                   doc=self._doc, index=index,
                                   nullable=(not self.type().not_null()),
                                   primary_key=(self._primary_key and not inherited),
                                   unique=self._unique,
                                   info=dict(inherited=inherited))
        column.pytis_orig_table = orig_table_name
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


## Utilities

_full_init = False
_current_search_path = None

def _set_current_search_path(search_path):
    global _current_search_path
    _current_search_path = search_path
    
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
        name = name[pos+1:]
    return name

def _is_specification_name(name):
    return (not name.startswith('SQL') and
            not name.startswith('_') and
            not name.startswith('Base_'))

def _expand_schemas(schemas):
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
        for name_part in name.split('.'):
            if isinstance(value, dict):
                value = value.get(name_part)
            else:
                value = getattr(value, name_part, None)
            if value is None:
                value = self._default
                break
        return value

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
        if cls._is_specification(clsname):
            name = cls.pytis_name()
            if (name in _PytisBaseMetaclass._name_mapping and 
                _PytisBaseMetaclass._name_mapping[name] is not cls):
                raise SQLException("Duplicate object name", (cls, _PytisBaseMetaclass._name_mapping[name],))
            _PytisBaseMetaclass._name_mapping[name] = cls
            _PytisBaseMetaclass._class_mapping[cls.__name__] = cls
            cls.name = name
        sqlalchemy.sql.visitors.VisitableType.__init__(cls, clsname, bases, clsdict)

    def _is_specification(cls, clsname):
        return _is_specification_name(clsname)

    @classmethod
    def specification_by_name(cls, name):
        return cls._name_mapping.get(name)

    @classmethod
    def specification_by_class_name(cls, name):
        return cls._class_mapping.get(name)

    @classmethod
    def specifications(cls):
        "Return all registered specifications."
        return _PytisBaseMetaclass._name_mapping.values()

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

    def __init__(cls, clsname, bases, clsdict):
        _PytisBaseMetaclass.__init__(cls, clsname, bases, clsdict)
        if cls._is_specification(clsname):
            schemas = _expand_schemas(cls.schemas)
            for search_path in schemas:
                _set_current_search_path(search_path)
                def init_function(cls=cls, search_path=search_path):
                    if issubclass(cls, SQLSequence):
                        # hack
                        o = cls(cls.pytis_name(), metadata=_metadata, schema=search_path[0],
                                start=cls.start, increment=cls.increment)
                    else:
                        o = cls(_metadata, search_path)
                    _PytisSchematicMetaclass.objects.append(o)
                if _full_init:
                    init_function()
                else:
                    # Instance construction takes a lot of time.  In some
                    # situations we may be interested just in classes or only
                    # in some instances.  In such a case instance creation is
                    # delayed using the init_function mechanism.
                    _PytisSchematicMetaclass.init_functions[cls.pytis_name()] = init_function
                    _PytisSchematicMetaclass.init_function_list.append((cls, init_function,))

    @classmethod
    def clear(cls):
        _PytisSchematicMetaclass.init_functions = {}
        _PytisSchematicMetaclass.init_function_list = []

class _PytisTriggerMetaclass(_PytisSchematicMetaclass):
    def __init__(cls, clsname, bases, clsdict):
        if (cls._is_specification(clsname) and
            cls.schemas is None and
            cls.table is not None):
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
            init_function()
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
def object_by_reference(name):
    name = name.strip()
    pos = name.find('(')
    if pos > 0:
        table = name[:pos].strip()
        column = name[pos+1:-1].strip()
    else:
        table = name
        column = None
    return _Reference(table, column)

def object_by_class(class_, search_path=None):
    assert issubclass(class_, SQLObject)
    if search_path is None:
        search_path = _current_search_path
    table_name = class_.pytis_name()
    return object_by_path(table_name, search_path)    

def object_by_specification_name(specification_name, search_path=None):
    class_ = _PytisBaseMetaclass.specification_by_class_name(specification_name)
    if class_ is None:
        return None 
    return object_by_class(class_, search_path or _current_search_path)

def specification_by_name(name):
    return _PytisBaseMetaclass.specification_by_name(name)

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
            if specification is None:
                return None
            return specification.c[self._column]
    class ColumnLookup(object):
        def __init__(self, specification):
            self._specification = specification
        def __getattr__(self, column):
            return ReferenceLookup.Reference(self._specification, column)
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

      access-rights -- definition of access rights to the object; sequence of
        pairs (RIGHT, ROLE)
      owner -- database owner of the object; string
      external -- iff true then do not create the object as it's just a
        declaration of a database object defined outside our specifications,
        to be used by objects of our specifications

    """
    access_rights = ()
    owner = None
    external = False
    _DB_OBJECT = None

    @classmethod
    def pytis_kind(class_):
        return class_._DB_OBJECT
    
    @classmethod
    def pytis_name(class_):
        return class_.name

    def _is_true_specification(self):
        return _is_specification_name(self.__class__.__name__)

    def _add_dependencies(self):
        for o in self.depends_on:
            if o is None:
                sys.stderr.write("Unresolved dependency in %s: %s\n" %
                                 (self.__class__.__name__, self.depends_on,))
                continue
            if isinstance(o, SQLObject):
                self.add_is_dependent_on(o)
            else:
                assert issubclass(o, SQLObject), ("Invalid dependency", o,)
                # General dependency must include all schema instances
                name = o.pytis_name()
                for search_path in _expand_schemas(o.schemas):
                    schema = search_path[0]
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

    def search_path(self):
        return self._search_path

## gensql abbreviations -- do not use in new code!

def gA(table, **kwargs):
    return Arguments(object_by_reference(table), **kwargs)
gL = sqlalchemy.sql.literal_column
gO = object_by_path
gR = RawCondition
    

## Database objects

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

    def _create_comments(self):
        doc = self.__doc__
        if doc:
            sqlalchemy.event.listen(self, 'after_create', _ObjectComment(self, self._DB_OBJECT, doc))

    def _register_access_rights(self):
        for o in self._access_right_objects:
            sqlalchemy.event.listen(self, 'after_create', o)
        if self.owner:
            command = ('ALTER %s "%s"."%s" OWNER TO "%s"' %
                       (self._DB_OBJECT, self.schema, self.name, self.owner,))
            sqlalchemy.event.listen(self, 'after_create', command)
        
    def _create_rules(self):
        def make_rule(action, instead_commands, also_commands):
            if instead_commands is None and not also_commands:
                return
            # We have to attach also_commands to instead_commands because
            # also_commands may contain references to NEW and OLD which are not
            # available in ALSO rules.
            kind = 'ALSO' if instead_commands is None and also_commands else 'INSTEAD'
            commands = tuple(instead_commands or ()) + tuple(also_commands or ())
            rule = _Rule(self, action, kind, commands)
            sqlalchemy.event.listen(self, 'after_create', rule)
        make_rule('INSERT', self.on_insert(), self.on_insert_also())
        make_rule('UPDATE', self.on_update(), self.on_update_also())
        make_rule('DELETE', self.on_delete(), self.on_delete_also())

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
            if (table_c.primary_key or (table_c.unique and not table_c.nullable)):
                # Try to find `tabular' primary key or one of its equivalents in my
                # columns, perhaps aliased.  If primary key is not found, look
                # at primary like (i.e. UNIQUE NOT NULL) columns.
                equivalent_column_instances = self._equivalent_rule_columns(table_c)
                equivalent_columns = [(c.table, c.name, c.primary_key,)
                                      for c in equivalent_column_instances]
                for c in self._original_columns():
                    tc = c.element if isinstance(c, sqlalchemy.sql.expression._Label) else c
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
                conditions.append(table_c == sqlalchemy.literal_column('old.'+name))
                break
        if not conditions:
            if False:
                # Let's just print warning until converted specifications are fixed
                raise SQLException("Table key column not found in the view", table_c)
            else:
                _gsql_output('-- WARNING: Missing table key column, incorrect rule for view will be output: %s %s' %
                             (self.name, tabular.name,))
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
    
class SQLTable(_SQLTabular):
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
        tuples or 'Arguments' instances contains names of columns to include
        within a single index.  An additional keyword argument 'method' may be
        provided in an 'Arguments' whose value determines indexing method to be
        used for this index.  Use this property only for multicolumn indexes,
        specify single column indexes directly in the corresponding 'Column'
        specifications.
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
    index_columns = ()
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
        columns = columns + tuple([c.sqlalchemy_column (search_path, table_name, key_name, table_name,
                                                        foreign_constraints=foreign_constraints,
                                                        check_constraints=check_constraints)
                                   for c in cls.fields])
        args = (table_name, metadata,) + columns + tuple(check_constraints)
        for check in cls.check:
            args += (sqlalchemy.CheckConstraint(check),)
        for unique in cls.unique:
            args += (sqlalchemy.UniqueConstraint (*unique),)
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
        return obj

    def _init(self, *args, **kwargs):
        self._pytis_create_p = False
        super(SQLTable, self)._init(*args, **kwargs)
        self._create_parameters()
        if self._is_true_specification():
            self._create_special_indexes()
            self._create_triggers()

    @property
    def columns(self):
        columns = super(SQLTable, self).columns
        if self._pytis_create_p:
            columns = sqlalchemy.sql.ColumnCollection(*[c for c in columns if not c.info['inherited']])
        return columns

    def _table_name(self, table):
        name = table.name
        schemas = _expand_schemas(table.schemas)
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
        for index in self.index_columns:
            ikwargs = {}
            if isinstance(index, Arguments):
                colnames = index.args()
                method = index.kwargs().get('method')
                if method:
                    ikwargs['postgresql_using'] = method
            else:
                colnames = index
                method = None
            columns = [getattr(self.c, c) for c in colnames]
            index_name = string.join(colnames, '_')
            if method:
                index_name += '_' + method
            index = sqlalchemy.Index('%s_%s_idx' % (self.name, index_name,),
                                     *columns, **ikwargs)
            sqlalchemy.event.listen(self, 'after_create', lambda *args, **kwargs: index)
        return args

    def _create_triggers(self):
        for t in self.triggers:
            if not isinstance(t, (tuple, list,)):
                t = (t,)
            trigger = t[0]
            class T(trigger):
                name = '%s__%s' % (self.name, trigger.name,)
                if self.schema:
                    name = '%s__%s' % (self.schema, name,)
                if _current_search_path:
                    schemas = (_current_search_path,)
                table = self.__class__
                arguments = t[1:]
            sqlalchemy.event.listen(self, 'after_create', lambda *args, **kwargs: T)
    
    def _register_access_rights(self):
        super(SQLTable, self)._register_access_rights()
        groups = set([group for right, group in self.access_rights
                      if right.lower() in ('insert', 'all')])
        for c in self.c:
            if isinstance(c.type, (SERIAL, BIGSERIAL,)) and not c.info.get('inherited'):
                for g in groups:
                    cname = c.name
                    command = ('GRANT usage ON "%s"."%s_%s_seq" TO GROUP "%s"' %
                               (self.schema, self.name, cname, g,))
                    sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))

    def create(self, bind=None, checkfirst=False):
        self._set_search_path(bind)
        self._pytis_create_p = True
        try:
            super(SQLTable, self).create(bind=bind, checkfirst=checkfirst)
        finally:
            self._pytis_create_p = False
        self._insert_values(bind)

    def _set_search_path(self, bind):
        search_path = self.search_path()
        _set_current_search_path(search_path)
        path_list = [_sql_id_escape(s) for s in search_path]
        path = string.join(path_list, ',')
        command = 'SET SEARCH_PATH TO %s' % (path,)
        bind.execute(command)
        
    def _insert_values(self, bind):
        if self.init_values:
            for row in self.init_values:
                values = dict(zip(self.init_columns, row))
                insert = self.insert().values(**values)
                bind.execute(insert)

class SQLView(_SQLTabular):
    """View specification.

    Views are similar to tables in that they have columns.  But unlike tables
    view columns are not defined in the view specification directly, they are
    defined implicitly by included tables and their columns.

    The primary specification element of this class is 'query()' class method
    which returns the expression defining the view.  The method must return an
    'sqlalchemy.ClauseElement' instance (as all the SQLAlchemy expressions do).

    Additional properties defined by this class:

      join_columns -- tuple of tuples of equivalent columns
        ('sqlalchemy.Column' instances).  If the view is made by joining some
        relations, duplicate join columns are excluded from the view and there
        are modification rules defined for the view then it is necessary to
        enumerate all equivalent relation columns in 'join_column' tuples in
        order to make gensqlalchemy generate complete and correct rules.

    The expression in 'query()' method is typically constructed using means
    provided by SQLAlchemy.  The class defines a few additional utility class
    methods which may be useful for constructing the expression:
    'SQLView._exclude()', 'SQLView._alias()', 'SQLView._reorder()'.  See their
    documentation strings for more information.

    """
    _DB_OBJECT = 'VIEW'

    join_columns = ()
    @classmethod
    def query(class_):
        return None

    __visit_name__ = 'view'

    def __new__(cls, metadata, search_path):
        _set_current_search_path(search_path)
        columns = tuple([sqlalchemy.Column(c.name, c.type) for c in cls.query().columns])
        args = (cls.name, metadata,) + columns
        return sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])

    def _add_dependencies(self):
        super(SQLView, self)._add_dependencies()
        objects = [self.query()]
        seen = []
        # We may add some objects multiple times here but that doesn't matter.
        # Trying to prune the list in trivial ways makes gsql many times slower
        # because there may be many comparisons here.
        while objects:
            o = objects.pop()
            if isinstance(o, sqlalchemy.sql.Alias):
                objects += o.get_children()
            elif isinstance(o, sqlalchemy.Table):
                self.add_is_dependent_on(o)
                seen.append(o)
            elif isinstance(o, sqlalchemy.sql.ClauseElement):
                objects += o.get_children()
                seen.append(o)
            elif not isinstance(o, RawCondition):
                raise SQLException("Unknown condition element", o)
            
    def _equivalent_rule_columns(self, column):
        equivalents = []
        if column is not None:
            equivalents = [column]
        for equivalent_columns in self.join_columns:
            if column in equivalent_columns:
                for c in equivalent_columns:
                    if c is not column:
                        equivalents.append(c)
        return equivalents
    
    def _hidden_rule_columns(self, tabular):
        hidden_columns = []
        original_columns = self._original_columns()
        for equivalent_columns in self.join_columns:
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

    def _default_rule_commands(self):
        return ('NOTHING',)
    
    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

@compiles(SQLView)
def visit_view(element, compiler, **kw):
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

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)
    
class SQLFunctional(_SQLTabular):
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
        constant (the return value is defined by output function arguments).
      multirow -- iff true then the function may return multiple results
        (rows); boolean
      security_definer -- iff true then the function runs with permissions of
        its creator rather than the user invoking it; boolean
      stability -- information about stability of the function return values to
        the database query optimizer, the given value (string) is used directly
        in the function definition.  This is useful to set if the function
        returns the same values for the same input data and without any side
        effects.
      sql_directory -- name of the directory where SQL files with function body
        definitions are stored; the name is relative to the processed module
        file name
      function_name -- actual name of the database function; string.  It is
        useful only with overloaded functions when the Python object names must
        be different (as required by SQLAlchemy) while the database object
        names should be the same.  If 'None' then it defaults to 'name'.

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
    sql_directory = 'sql'
    function_name = None

    _LANGUAGE = None

    __visit_name__ = 'function'

    RECORD = 'RECORD'

    def __new__(cls, metadata, search_path):
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

    def _add_dependencies(self):
        super(SQLFunctional, self)._add_dependencies()
        result_type = self.result_type
        if (result_type not in (None, G_CONVERT_THIS_FUNCTION_TO_TRIGGER,) and
            not isinstance(result_type, (tuple, list, Column, pytis.data.Type,)) and
            result_type != SQLFunctional.RECORD):
            self.add_is_dependent_on(object_by_class(result_type, self._search_path))

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

    def __call__(self, *arguments):
        """Return 'sqlalchemy.ClauseElement' corresponding to function call.

        Arguments:

          arguments -- tuple of function argument values, of types as required
            by SQLAlchemy
        
        """
        function_name = self.function_name or self.name
        name = '"%s"."%s"' % (self.schema, function_name,)
        # We can't use the standard SQLAlchemy function call here
        # (i.e. getattr(sqlalchemy.sql.expression.func, name)(*arguments))
        # since this puts argument symbols instead of argument values into the
        # argument list.
        argument_list = [_sql_value_escape(a) for a in arguments]
        expression = u'%s(%s)' % (name, string.join(argument_list, ', '),)
        return sqlalchemy.literal(expression)

    def body(self):
        """Return function body as basestring.

        The default implementation reads the function body from the file named
        FUNCTION_NAME.sql where FUNCTION_NAME is the name of the function as
        defined by 'name' property.

        """
        module_path = os.path.dirname(sys.modules[self.__module__].__file__)
        sql_file = os.path.join(module_path, self.sql_directory, self.name + '.sql')
        return codecs.open(sql_file, encoding='UTF-8').read()

class SQLFunction(SQLFunctional):
    """SQL function definition.

    This class doesn't define anything new, see its superclass for information
    about function definition.

    In addition to other classes, 'body()' may return 'SQLAlchemy.Select' instance.

    """
    _LANGUAGE = 'sql'

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

    You can define utility Python functions for use in the PL/Python function.
    The utility functions are also defined as static methods of the
    specification class.  The methods must be named sub_NAME, where NAME is the
    name of the utility function, and must have the same arguments.  Functions
    defined this way are available for use in the PL/Python function.  You can
    also define utility inner classes in a similar way, they must be named
    Sub_NAME.
    
    Utility functions and classes are typically defined in a common base class
    inherited by specifications of PL/Python functions.
    
    """
    _LANGUAGE = 'plpythonu'

    _SUBROUTINE_MATCHER = re.compile('( *)(@staticmethod\r?\n?|class )', re.MULTILINE)
    
    def body(self):
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
        main_lines = self._method_source_lines(main_method_name, 0)
        strip_header(main_lines)
        prefix = 'sub_'
        for name in dir(self):
            if name.startswith(prefix):
                function_lines = self._method_source_lines(name, 4) # hard-wired forev^h^h now
                function_lines = function_lines[1:]
                first_line = function_lines[0]
                i = first_line.find(prefix)
                j = i + len(prefix)
                function_lines[0] = first_line[:i] + first_line[j:]
                lines += function_lines
        prefix = 'Sub_'
        for name in dir(self):
            if name.startswith(prefix):
                class_lines = self._method_source_lines(name, 4) # hard-wired forev^h^h now
                first_line = class_lines[0]
                i = first_line.find(prefix)
                j = i + len(prefix)
                class_lines[0] = first_line[:i] + first_line[j:]
                lines += class_lines
        lines += main_lines
        return string.join(lines, '\n')

    def _method_source_lines(self, name, indentation):
        method = getattr(self, name)
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
                return ' '*indentation + line
        else:
            def reindent(line):
                return line[-indentation:]
        lines = [unicode(l.rstrip(), 'utf-8') for l in lines]
        return [reindent(l) for l in lines if l.strip()]

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
    
    events = ('insert', 'update', 'delete',)
    position = 'after'
    each_row = True
    call_arguments = ()

    __visit_name__ = 'trigger'
    
    def _add_dependencies(self):
        super(SQLTrigger, self)._add_dependencies()
        search_path = (self.schema,)
        if self.table:
            t = object_by_class(self.table, search_path=search_path)
            assert t is not None, ("Trigger table not found", self)
            self.add_is_dependent_on(t)
        if type(self.body) != types.MethodType:
            self.add_is_dependent_on(object_by_class(self.body, search_path=search_path))

    def __call__(self, *arguments):
        if type(self.body) == types.MethodType:
            return super(SQLTrigger, self).__call__(*arguments)
        return object_by_class(self.body)(*arguments)

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


## Specification processing

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
                for k in sql.table.pytis_key:
                    if k not in sql_parameters:
                        column = sql.table.columns[k]
                        parameters[k] = ("nextval('%s.%s_%s_seq')" %
                                         (column.table.schema, column.pytis_orig_table, column.name,))
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
    _gsql_output(output + ';')

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
    _output.write(output)
    _output.write('\n')

def _gsql_process(loader, regexp, no_deps, views, functions, names_only):
    global _output
    _output = sys.stdout
    if _output.encoding is None:
        _output = codecs.getwriter('UTF-8')(_output)
    global _full_init
    _full_init = not (names_only or (no_deps and regexp))
    try:
        _gsql_process_1(loader, regexp, no_deps, views, functions, names_only)
    finally:
        _full_init = False

def _gsql_process_1(loader, regexp, no_deps, views, functions, names_only):
    if regexp is not None:
        matcher = re.compile(regexp)
    matched = set()
    def matching(o):
        result = True
        if isinstance(o, SQLObject):
            cls = o.__class__
        else:
            cls = o
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
        if matcher.search(cls.__name__):
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
        name = obj.pytis_name()
        if isinstance(obj, SQLSchematicObject):
            name = '%s.%s' % (obj.schema, name,)
        _gsql_output('%s %s' % (kind, name,))
    # Load the objects
    loader()
    # Preprocessing in case of speed optimized limited output
    if not _full_init:
        for o, f in _PytisSchematicMetaclass.init_function_list:
            if matching(o):
                if names_only:
                    output_name(o)
                else:
                    f()
        if names_only:
            return
    # Process all available objects
    for o in _PytisSimpleMetaclass.objects:
        if matching(o):
            if names_only:
                output_name(o)
            else:
                _engine.execute(o)
                o.after_create(_engine)
    for sequence in _metadata._sequences.values():
        if matching(sequence):
            if names_only:
                output_name(sequence)
            else:
                sequence.create(_engine, checkfirst=False)
                sequence.after_create(_engine)
    for table in _metadata.sorted_tables:
        if matching(table):
            if names_only:
                output_name(table)
            else:
                table.create(_engine, checkfirst=False)
    for ffk in _forward_foreign_keys:
        if matching(ffk.table) and not names_only:
            target = ffk.reference.get(ffk.search_path)
            if target is None:
                raise Exception('Invalid foreign reference',
                                (ffk.table.name, ffk.reference._name, ffk.reference._column,))
            kwargs = ffk.kwargs
            kwargs['name'] += target.name.replace('.', '__')
            f = sqlalchemy.ForeignKeyConstraint((ffk.column_name,), (target,), *ffk.args,
                                                table=ffk.table, **kwargs)
            fdef = sqlalchemy.schema.AddConstraint(f)
            _engine.execute(fdef)
    
def gsql_file(file_name, regexp=None, no_deps=False, views=False, functions=False,
              names_only=False):
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

    If both 'views' and 'functions' are specified, output both views and
    functions.

    The SQL code is output on standard output.

    """
    def loader(file_name=file_name):
        execfile(file_name, copy.copy(globals()))
    _gsql_process(loader, regexp, no_deps, views, functions, names_only)

def gsql_module(module_name, regexp=None, no_deps=False, views=False, functions=False,
                names_only=False):
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

    If both 'views' and 'functions' are specified, output both views and
    functions.

    The SQL code is output on standard output.

    """
    def loader(module_name=module_name):
        imp.load_module(module_name, *imp.find_module(module_name))
    _gsql_process(loader, regexp, no_deps, views, functions, names_only)

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

# Make sure _metadata and _engine are initialized to prevent crashes
clear()
