#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2012 Brailcom, o.p.s.
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


import copy
import inspect
import os
import re
import string
import sqlalchemy
from sqlalchemy.ext.compiler import compiles
import types
import pytis.data

## SQLAlchemy extensions

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
        condition = view.condition()
        condition.pytis_prefix = command
        self.connection.execute(condition)

    def visit_function(self, function, create_ok=False):
        search_path = function.search_path()
        self._set_search_path(search_path)
        def arg(column):
            a_column = column.sqlalchemy_column(search_path, None, None, None)
            return '"%s" %s' % (a_column.name, a_column.type,)
        arguments = string.join([arg(c) for c in function.arguments], ', ')
        result_type = function.result_type[0].sqlalchemy_column(search_path, None, None, None).type
        if function.multirow:
            result_type = 'SETOF ' + result_type
        command = ('CREATE OR REPLACE FUNCTION "%s"."%s" (%s) RETURNS %s AS $$\n%s\n$$ LANGUAGE %s %s' %
                   (function.schema, function.name, arguments, result_type, function.body(),
                    function._LANGUAGE, function.stability,))
        if function.security_definer:
            command += ' SECURITY DEFINER'
        self.connection.execute(command)

class _TableComment(sqlalchemy.schema.DDLElement):
    def __init__(self, table, comment):
        self.table = table
        self.comment = comment
@compiles(_TableComment)
def visit_table_comment(element, compiler, **kw):
    return ("COMMENT ON TABLE \"%s\".\"%s\" IS '%s'" %
            (element.table.schema, element.table.name, element.comment.replace("'", "''"),))

class _ColumnComment(sqlalchemy.schema.DDLElement):
    def __init__(self, table, field):
        self.table = table
        self.field = field
@compiles(_ColumnComment)
def visit_column_comment(element, compiler, **kw):
    return ("COMMENT ON COLUMN \"%s\".\"%s\".\"%s\" IS '%s'" %
            (element.table.schema, element.table.name, element.field.id(),
             element.field.doc().replace("'", "''"),))
    
## Columns
        
class Column(pytis.data.ColumnSpec):
    
    def __init__(self, name, type, doc=None, unique=False, check=None,
                 default=None, references=None, primary_key=False, index=False):        
        pytis.data.ColumnSpec.__init__(self, name, type)
        self._doc = doc
        self._unique = unique
        self._check = check
        self._default = default
        self._references = references
        self._primary_key = primary_key
        self._index = index

    def doc(self):
        return self._doc

    def index(self):
        return self._index

    def primary_key(self):
        return self._primary_key

    def sqlalchemy_column(self, search_path, table_name, key_name, orig_table_name):
        alchemy_type = self.type().sqlalchemy_type()
        args = []
        references = self._references
        if references is not None:
            if not isinstance(references, a):
                references = a(references)
            r_args = references.args()
            if isinstance(r_args[0], (ReferenceLookup.Reference, _Reference)):
                r_args = (r_args[0].get(table_name, key_name),) + r_args[1:]
            args.append(sqlalchemy.ForeignKey(*r_args, **references.kwargs()))
        if self._index and not isinstance(self._index, dict):
            index = True
        else:
            index = False
        column = sqlalchemy.Column(self.id(), alchemy_type, *args, default=self._default,
                                   doc=self._doc, index=index,
                                   nullable=(not self.type().not_null()),
                                   primary_key=self._primary_key, unique=self._unique)
        column.pytis_orig_table = orig_table_name
        return column

class PrimaryColumn(Column):
    
    def __init__(self, *args, **kwargs):
        kwargs = copy.copy(kwargs)
        kwargs['primary_key'] = True
        super(PrimaryColumn, self).__init__(*args, **kwargs)

## Utilities

_current_search_path = None

def _set_current_search_path(search_path):
    global _current_search_path
    _current_search_path = search_path
    
def _sql_id_escape(identifier):
    return '"%s"' % (identifier.replace('"', '""'),)

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
            name = self._name
        value = globals().get(name)
        if value is None:
            value = self._default
        return value

_default_schemas = SQLFlexibleValue('default_schemas', environment='GSQL_DEFAULT_SCHEMAS',
                                    default=(('public',),))

_metadata = sqlalchemy.MetaData()

class SQLException(Exception):
    pass

class _PytisTableMetaclass(sqlalchemy.sql.visitors.VisitableType):
    
    _name_mapping = {}
    
    def __init__(cls, clsname, bases, clsdict):
        is_specification = not clsname.startswith('SQL') and not clsname.startswith('_SQL')
        if is_specification:
            name = cls.pytis_name()
            if (name in _PytisTableMetaclass._name_mapping and 
                _PytisTableMetaclass._name_mapping[name] is not cls):
                raise SQLException("Duplicate object name", (cls, _PytisTableMetaclass._name_mapping[name],))
            _PytisTableMetaclass._name_mapping[name] = cls
            cls.name = name
        sqlalchemy.sql.visitors.VisitableType.__init__(cls, clsname, bases, clsdict)
        if is_specification:
            schemas = cls.schemas
            if isinstance(schemas, SQLFlexibleValue):
                schemas = schemas.value()
            for search_path in schemas:
                _set_current_search_path(search_path)
                cls(_metadata, search_path)
    
def object_by_name(name):
    return _metadata.tables[name]

def object_by_path(name, search_path=True):
    if search_path is True:
        search_path = _current_search_path
    for schema in search_path:
        try:
            return object_by_name('%s.%s' % (schema, name,))
        except KeyError:
            continue
    raise SQLException("Object not found", (schema, name,))

class _Reference(object):
    def __init__(self, name, column):
        self._name = name
        self._column = column
    def get(self, table_name, key_name):
        if table_name == self._name:
            reference = key_name
        else:
            table = object_by_path(self._name)
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

def object_by_specification(specification):
    class_ = globals()[specification]
    assert issubclass(class_, _SQLTabular)
    table_name = class_.pytis_name()
    return object_by_path(table_name, _current_search_path)


class RawCondition(object):
    def __init__(self, condition):
        self._condition = condition
        self._from_objects = []
    def _compiler_dispatch(self, *args, **kwargs):
        return self._condition
    
class TableLookup(object):
    def __getattr__(self, specification):
        return object_by_specification(specification)
t = TableLookup()

class ColumnLookup(object):
    def __getattr__(self, specification):
        return object_by_specification(specification).c
c = ColumnLookup()

class ReferenceLookup(object):
    class Reference(object):
        def __init__(self, specification, column):
            self._specification = specification
            self._column = column
        def get(self, table_name, key_name):
            columns = object_by_specification(self._specification).c
            return columns[self._column]
    class ColumnLookup(object):
        def __init__(self, specification):
            self._specification = specification
        def __getattr__(self, column):
            return ReferenceLookup.Reference(self._specification, column)
    def __getattr__(self, specification):
        return self.ColumnLookup(specification)
r = ReferenceLookup()

class a(object):
    def __init__(self, *args, **kwargs):
        self._args = args
        self._kwargs = kwargs
    def args(self):
        return self._args
    def kwargs(self):
        return self._kwargs

## Database objects

class _SQLTabular(sqlalchemy.Table):
    __metaclass__ = _PytisTableMetaclass
    
    name = None
    schemas = _default_schemas
    depends_on = ()

    def _init(self, *args, **kwargs):
        super(_SQLTabular, self)._init(*args, **kwargs)
        self._search_path = _current_search_path
        self._add_dependencies()

    def _add_dependencies(self):
        for o in self.depends_on:
            self.add_is_dependent_on(o)

    def search_path(self):
        return self._search_path

    @classmethod
    def pytis_name(class_):
        name = class_.name
        if name is None:
            name = pytis.util.camel_case_to_lower(class_.__name__)
        return name
    
class SQLTable(_SQLTabular):
    
    fields = ()
    inherits = ()
    tablespace = None
    init_columns = None
    init_values = ()
    check = ()
    unique = ()
    with_oids = False

    def __new__(cls, metadata, search_path):
        table_name = cls.pytis_name()
        key = []
        fields = cls.fields
        for i in cls.inherits:
            fields = fields + i.fields
        for c in fields:
            if c.primary_key():
                key.append(c.id())
        if len(key) == 1:
            key_name = '%s.%s.%s' % (search_path[0], table_name, key[0],)
        else:
            key_name = None
        columns = ()
        for i in cls.inherits:
            orig_table_name = i.pytis_name()
            columns = columns + tuple([c.sqlalchemy_column(search_path, table_name, key_name, orig_table_name)
                                       for c in i.fields])
        columns = columns + tuple([c.sqlalchemy_column(search_path, table_name, key_name, table_name)
                                   for c in cls.fields])
        args = (table_name, metadata,) + columns
        for check in cls.check:
            args += (sqlalchemy.CheckConstraint(check),)
        for unique in cls.unique:
            args += (sqlalchemy.UniqueConstraint (*unique),)
        obj = sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])
        obj.pytis_key = key
        return obj

    def _init(self, *args, **kwargs):
        super(SQLTable, self)._init(*args, **kwargs)
        self._create_parameters()
        self._create_special_indexes()
        self._create_comments()

    def _table_name(self, table):
        name = table.name
        schemas = table.schemas
        if isinstance(schemas, SQLFlexibleValue):
            schemas = schemas.value()
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
            self.add_is_dependent_on(object_by_name('%s.%s' % (self.schema, inherited.pytis_name(),)))

    def _alter_table(self, alteration):
        command = 'ALTER TABLE "%s"."%s" %s' % (self.schema, self.name, alteration,)
        sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))

    def _create_parameters(self):
        self._alter_table("SET %s OIDS" % ("WITH" if self.with_oids else "WITHOUT",))
        if self.tablespace:
            self._alter_table('SET TABLESPACE "%s"' % (self.tablespace,))
        for table in self.inherits:
            self._alter_table('INHERIT %s' % (self._table_name(table),))

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
        return args

    def _create_comments(self):
        doc = self.__doc__
        if doc:
            sqlalchemy.event.listen(self, 'after_create', _TableComment(self, doc))
        for c in self.fields:
            doc = c.doc()
            if doc:
                sqlalchemy.event.listen(self, 'after_create', _ColumnComment(self, c))

    def create(self, bind=None, checkfirst=False):
        super(SQLTable, self).create(bind=bind, checkfirst=checkfirst)
        self._insert_values(bind)
        
    def _insert_values(self, bind):
        if self.init_values:
            for row in self.init_values:
                values = dict(zip(self.init_columns, row))
                insert = self.insert().values(**values)
                bind.execute(insert)

class SQLView(_SQLTabular):

    @classmethod
    def condition(class_):
        return None

    __visit_name__ = 'view'

    def __new__(cls, metadata, search_path):
        columns = tuple([sqlalchemy.Column(c.name, c.type) for c in cls.condition().columns])
        args = (cls.name, metadata,) + columns
        return sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])

    def _add_dependencies(self):
        super(SQLView, self)._add_dependencies()
        objects = [self.condition()]
        seen = []
        while objects:
            o = objects.pop()
            if isinstance(o, sqlalchemy.sql.Alias):
                objects += o.get_children()
            elif o in seen:
                continue
            elif isinstance(o, sqlalchemy.Table):
                self.add_is_dependent_on(o)
                seen.append(o)
            elif isinstance(o, sqlalchemy.sql.ClauseElement):
                objects += o.get_children()
                seen.append(o)

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

@compiles(SQLView)
def visit_view(element, compiler, **kw):
    return '"%s"."%s"' % (element.schema, element.name,)

class SQLFunctional(_SQLTabular):

    arguments = ()
    result_type = None
    multirow = False
    security_definer = False
    stability = 'volatile'

    _LANGUAGE = None

    __visit_name__ = 'function'

    def __new__(cls, metadata, search_path):
        columns = tuple([c.sqlalchemy_column(search_path, None, None, None) for c in cls.result_type])
        args = (cls.name, metadata,) + columns
        return sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

    def __call__(self, *arguments):
        name = '"%s"."%s"' % (self.schema, self.name,)
        return getattr(sqlalchemy.sql.expression.func, name)(*arguments)

    def body(self):
        return open(self.name + '.sql').read()

class SQLFunction(SQLFunctional):
    
    _LANGUAGE = 'sql'

class SQLPlFunction(SQLFunctional):

    _LANGUAGE = 'plpgsql'
    
class SQLPyFunction(SQLFunctional):

    _LANGUAGE = 'plpythonu'

    _STATICMETHOD_MATCHER = re.compile('( *)@staticmethod\r?\n?', re.MULTILINE)
    
    def body(self):
        arglist = string.join([c.id() for c in self.arguments], ', ')
        lines = ['#def %s(%s):' % (self.name, arglist,),
                 '    %s = args' % (arglist,)] # hard-wired indentation
        main_lines = self._method_source_lines(self.name, 0)
        main_lines = main_lines[1:]
        prefix = 'sub_'
        for name in dir(self):
            if name.startswith(prefix):
                function_lines = self._method_source_lines(name, 4) # hard-wired forev^h^h now
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

## Specification processing

engine = None
def _dump_sql_command(sql, *multiparams, **params):
    if isinstance(sql, str):
        output = unicode(sql)
    elif isinstance(sql, unicode):
        output = sql
    else:
        compiled = sql.compile(dialect=engine.dialect)
        if isinstance(sql, sqlalchemy.sql.expression.Insert):
            # SQLAlchemy apparently doesn't work so well without a database
            # connection.  We probably have no better choice than to handle some
            # things manually here, despite the corresponding functionality is
            # present in SQLAlchemy.
            parameters = {}
            sql_parameters = sql.parameters
            if len(sql_parameters) != len(compiled.binds):
                # Perhaps default key value
                for k in sql.table.pytis_key:
                    if k not in sql_parameters:
                        column = sql.table.columns[k]
                        parameters[k] = ("nextval('%s.%s_%s_seq')" %
                                         (column.table.schema, column.pytis_orig_table, column.name,))
            for k, v in sql_parameters.items():
                if v is None:
                    value = 'NULL'
                elif isinstance(v, (int, long, float,)):
                    value = v
                else:
                    value = "'%s'" % (v.replace('\\', '\\\\').replace("'", "''"),)
                parameters[k] = value
            output = unicode(compiled) % parameters
        elif isinstance(sql, sqlalchemy.sql.expression.Select):
            output = unicode(compiled) % compiled.params
        else:
            output = unicode(compiled)
        if hasattr(sql, 'pytis_prefix'):
            output = sql.pytis_prefix + output
    print output + ';'

def gsql_file(file_name):
    global _metadata
    _metadata = sqlalchemy.MetaData()
    execfile(file_name, copy.copy(globals()))
    global engine
    engine = sqlalchemy.create_engine('postgresql://', strategy='mock', executor=_dump_sql_command)
    for table in _metadata.sorted_tables:
        table.create(engine, checkfirst=False)
    
## Sample demo

class Foo(SQLTable):
    """Foo table."""
    name = 'foo'
    fields = (PrimaryColumn('id', pytis.data.Serial()),
              Column('foo', pytis.data.String(), doc='some string', index=dict(method='hash')),
              Column('n', pytis.data.Integer(not_null=True), doc='some number'),
              )
    inherits = ()
    tablespace = None
    init_columns = ('foo', 'n',)
    init_values = (('abc', 10,),
                   ('def', 20,),
                   )
    unique = (('foo', 'n',),)
    with_oids = False

class Foo2(Foo):
    name = 'foofoo'
    inherits = (Foo,)
    fields = (Column('bar', pytis.data.String()),)
    check = ('n > 0',)
    init_columns = ()
    init_values = ()
    with_oids = False

class Bar(SQLTable):
    """Bar table."""
    schemas = (('private', 'public',),)
    fields = (PrimaryColumn('id', pytis.data.Serial()),
              Column('foo_id', pytis.data.Integer(), references=a(r.Foo.id, onupdate='CASCADE')),
              Column('description', pytis.data.String()),
              )
    init_columns = ('foo_id', 'description',)
    init_values = ((1, 'some text'),)

class Baz(SQLView):
    """Baz view."""
    name = 'baz'
    schemas = (('private', 'public',),)
    @classmethod
    def condition(class_):
        return sqlalchemy.union(sqlalchemy.select([c.Foo.id, c.Bar.description],
                                                  from_obj=[t.Foo.join(t.Bar)]),
                                sqlalchemy.select([c.Foo2.id, sqlalchemy.literal_column("'xxx'", sqlalchemy.String)]))

class Baz2(SQLView):
    schemas = (('private', 'public',),)
    @classmethod
    def condition(class_):
        return sqlalchemy.select([c.Baz.id], from_obj=[t.Baz], whereclause=(c.Baz.id > '0'))

class AliasView(SQLView):
    name = 'aliased'
    @classmethod
    def condition(class_):
        foo1 = t.Foo.alias('foo1')
        foo2 = t.Foo.alias('foo2')
        return sqlalchemy.select([foo1], from_obj=[foo1.join(foo2, foo1.c.n<foo2.c.n)])

class Func(SQLFunction):
    name = 'plus'
    arguments = (Column('x', pytis.data.Integer()), Column('y', pytis.data.Integer()),)
    result_type = (Column('z', pytis.data.Integer()),)

    def body(self):
        return 'SELECT $1 + $2'

class FileFunc(SQLFunction):
    name = 'minus'
    arguments = (Column('x', pytis.data.Integer()), Column('y', pytis.data.Integer()),)
    result_type = (Column('z', pytis.data.Integer()),)

class PyFunc(SQLPyFunction):
    name = 'times'
    arguments = (Column('x', pytis.data.Integer()), Column('y', pytis.data.Integer()),)
    result_type = (Column('z', pytis.data.Integer()),)

    @staticmethod
    def times(x, y):
        return pythonic(x, y)

    @staticmethod
    def sub_pythonic(x, y):
        return x * y

def run():
    global engine
    engine = sqlalchemy.create_engine('postgresql://', strategy='mock', executor=_dump_sql_command)
    for table in _metadata.sorted_tables:
        table.create(engine, checkfirst=False)

if __name__ == '__main__':
    run()
