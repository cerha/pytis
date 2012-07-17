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
        view.dispatch.after_create(view, self.connection, checkfirst=self.checkfirst, _ddl_runner=self)

    def visit_type(self, type_name, columns):
        sqlalchemy_columns = [c.sqlalchemy_column(None, None, None, None) for c in columns]
        column_string = string.join(['%s %s' % (c.name, c.type,) for c in sqlalchemy_columns], ', ')
        command = 'CREATE TYPE %s AS (%s)' % (type_name, column_string,)
        self.connection.execute(command)

    def visit_function(self, function, create_ok=False, result_type=None):
        search_path = function.search_path()
        self._set_search_path(search_path)
        def arg(column):
            a_column = column.sqlalchemy_column(search_path, None, None, None)
            return '"%s" %s' % (a_column.name, a_column.type,)
        arguments = string.join([arg(c) for c in function.arguments], ', ')
        if result_type is None:
            function_type = function.result_type
            if function_type is None:
                result_type = 'void'
            elif isinstance(function_type, (tuple, list,)):
                result_type = 't_' + function.pytis_name()
                self.visit_type(result_type, function_type)
            elif isinstance(function_type, Column):
                result_type = function_type.sqlalchemy_column(search_path, None, None, None).type
            elif isinstance(function_type, pytis.data.Type):
                result_type = function_type.sqlalchemy_type()
            elif issubclass(function_type, SQLTable):
                result_type = object_by_class(function_type, search_path).pytis_name()
            else:
                raise Exception("Invalid result type", function_type)
        result_type_prefix = 'SETOF ' if function.multirow else ''
        body = function.body().strip()
        command = ('CREATE OR REPLACE FUNCTION "%s"."%s" (%s) RETURNS %s%s AS $$\n%s\n$$ LANGUAGE %s %s' %
                   (function.schema, function.name, arguments, result_type_prefix, result_type, body,
                    function._LANGUAGE, function.stability,))
        if function.security_definer:
            command += ' SECURITY DEFINER'
        self.connection.execute(command)

    def visit_trigger(self, trigger, create_ok=False):
        if isinstance(trigger, (SQLPlFunction, SQLPyFunction,)):
            self.visit_function(trigger, create_ok=create_ok, result_type='trigger')
        if trigger.events:
            events = string.join(trigger.events, ' OR ')
            table = object_by_path(trigger.table.name, trigger.search_path())
            row_or_statement = 'ROW' if trigger.each_row else 'STATEMENT'
            trigger_call = trigger(*trigger.arguments).value
            command = (('CREATE TRIGGER "%s__%s__%s_trigger" %s %s ON %s '
                        'FOR EACH %s EXECUTE PROCEDURE %s') %
                       (trigger.schema, trigger.name, trigger.position, trigger.position, events, table,
                        row_or_statement, trigger_call,))
            self.connection.execute(command)

    def visit_raw(self, raw, create_ok=False):
        self._set_search_path(raw.search_path())
        return self.connection.execute(raw.sql())

class _ObjectComment(sqlalchemy.schema.DDLElement):
    def __init__(self, obj, kind, comment):
        self.object = obj
        self.kind = kind
        self.comment = comment
@compiles(_ObjectComment)
def visit_object_comment(element, compiler, **kw):
    return ("COMMENT ON %s \"%s\".\"%s\" IS '%s'" %
            (element.kind, element.object.schema, element.object.name, element.comment.replace("'", "''"),))

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
    name = element.object.pytis_name()
    if isinstance(element, SQLSchematicObject):
        schema = '"%s".' % (element.schema,)
    else:
        schema = ''
    return ("GRANT %s ON %s%s TO \"%s\"" % (element.right, schema, name, element.group,))

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
    return ('CREATE OR REPLACE RULE "%s__%s" AS ON %s TO "%s"."%s" DO %s %s' %
            (table.schema, rule_name, element.action, table.schema, table.name, element.kind, sql,))

class _SQLExternal(sqlalchemy.sql.expression.FromClause):

    def __init__(self, name):
        super(_SQLExternal, self).__init__()
        self.name = name
    
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
            kwargs = copy.copy(references.kwargs())
            if isinstance(r_args[0], (ReferenceLookup.Reference, _Reference)):
                dereference = r_args[0].get(table_name, key_name)
                r_args = (dereference,) + r_args[1:]
                if isinstance(dereference, basestring):
                    kwargs['use_alter'] = True
                    kwargs['name'] = '%s__r__%s' % (table_name, dereference.replace('.', '__'),)
            if not isinstance(dereference, basestring):
                # Let's disable forward references for now as they can cause
                # crashes in view definitions.
                args.append(sqlalchemy.ForeignKey(*r_args, **kwargs))
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
                    raise Exception("Schema instance not found", s)
            else:
                raise Exception("Invalid schema reference", s)
            expanded_path.append(s)
        expanded_schemas.append(expanded_path)
    return expanded_schemas

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

class SQLNameException(SQLException):
    def __init__(self, *args):
        super(SQLNameException, self).__init__("Object not found", *args)


class _PytisBaseMetaclass(sqlalchemy.sql.visitors.VisitableType):
    
    _name_mapping = {}
    
    def __init__(cls, clsname, bases, clsdict):
        if cls._is_specification(clsname):
            name = cls.pytis_name()
            if (name in _PytisBaseMetaclass._name_mapping and 
                _PytisBaseMetaclass._name_mapping[name] is not cls):
                raise SQLException("Duplicate object name", (cls, _PytisBaseMetaclass._name_mapping[name],))
            _PytisBaseMetaclass._name_mapping[name] = cls
            cls.name = name
        sqlalchemy.sql.visitors.VisitableType.__init__(cls, clsname, bases, clsdict)

    def _is_specification(cls, clsname):
        return not clsname.startswith('SQL') and not clsname.startswith('_')

    @classmethod
    def specification_by_name(cls, name):
        return cls._name_mapping.get(name)

class _PytisSimpleMetaclass(_PytisBaseMetaclass):
    
    objects = []
    
    def __init__(cls, clsname, bases, clsdict):
        _PytisBaseMetaclass.__init__(cls, clsname, bases, clsdict)
        if cls._is_specification(clsname):
            _PytisSimpleMetaclass.objects.append(cls())

class _PytisSchematicMetaclass(_PytisBaseMetaclass):

    objects = []

    def __init__(cls, clsname, bases, clsdict):
        _PytisBaseMetaclass.__init__(cls, clsname, bases, clsdict)
        if cls._is_specification(clsname):
            schemas = _expand_schemas(cls.schemas)
            for search_path in schemas:
                _set_current_search_path(search_path)
                # hack
                if issubclass(cls, SQLSequence):
                    o = cls(cls.pytis_name(), metadata=_metadata, schema=search_path[0],
                            start=cls.start, increment=cls.increment)
                else:
                    o = cls(_metadata, search_path)
                _PytisSchematicMetaclass.objects.append(o)

class _PytisTriggerMetaclass(_PytisSchematicMetaclass):
    def __init__(cls, clsname, bases, clsdict):
        if cls._is_specification(clsname):
            if cls.table is None:
                if cls.events:
                    raise Exception("Trigger without table", cls)
            else:
                cls.schemas = cls.table.schemas
        _PytisSchematicMetaclass.__init__(cls, clsname, bases, clsdict)
    
def object_by_name(name, allow_external=True):
    try:
        o = _metadata.tables[name]
    except KeyError:
        if not allow_external:
            raise
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
    def get(self, table_name, key_name):
        if table_name == self._name:
            reference = key_name
        else:
            try:
                table = object_by_path(self._name, allow_external=False)
            except SQLNameException:
                return self._name
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

def object_by_specification(specification):
    class_ = globals()[specification]
    return object_by_class(class_, _current_search_path)

def specification_by_name(name):
    return _PytisBaseMetaclass.specification_by_name(name)

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

class SQLObject(object):

    access_rights = ()
    owner = None
    
    @classmethod
    def pytis_name(class_):
        name = class_.name
        if name is None:
            name = pytis.util.camel_case_to_lower(class_.__name__, separator='_')
        return name

    def _add_dependencies(self):
        for o in self.depends_on:
            if not isinstance(o, SQLObject):
                assert issubclass(o, SQLObject), ("Invalid dependency", o,)
                o = object_by_class(o, search_path=self._search_path)
            self.add_is_dependent_on(o)

    def _create_access_rights(self):
        self._access_right_objects = [_AccessRight(self, right, group)
                                      for right, group in self.access_rights]

class SQLSchematicObject(SQLObject):
    
    schemas = _default_schemas

    def __init__(self, *args, **kwargs):
        self._search_path = None
        super(SQLSchematicObject, self).__init__(*args, **kwargs)

    def search_path(self):
        return self._search_path

## Database objects

class SQLSchema(sqlalchemy.schema.DDLElement, sqlalchemy.schema.SchemaItem, SQLObject):
    __metaclass__ = _PytisSimpleMetaclass
    __visit_name__ = 'schema'
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
    __metaclass__ = _PytisSchematicMetaclass
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
    __metaclass__ = _PytisSchematicMetaclass
    _DB_OBJECT = None
    
    name = None
    depends_on = ()
    insert_order = None
    update_order = None
    delete_order = None

    def _init(self, *args, **kwargs):
        super(_SQLTabular, self)._init(*args, **kwargs)
        self._search_path = _current_search_path
        self._add_dependencies()
        self._create_comments()
        self._create_access_rights()
        self._create_rules()

    def _create_comments(self):
        doc = self.__doc__
        if doc:
            sqlalchemy.event.listen(self, 'after_create', _ObjectComment(self, self._DB_OBJECT, doc))

    def _create_access_rights(self):
        super(_SQLTabular, self)._create_access_rights()
        for o in self._access_right_objects:
            sqlalchemy.event.listen(self, 'after_create', o)
        if self.owner:
            command = ('ALTER %s "%s"."%s" OWNER TO "%s"' %
                       (self._DB_OBJECT, self.schema, self.name, self.owner,))
            sqlalchemy.event.listen(self, 'after_create', command)
        
    def _create_rules(self):
        def make_rule(action, kind, commands):
            if commands is None:
                return
            if not commands and kind == 'ALSO':
                return
            rule = _Rule(self, action, kind, commands)
            sqlalchemy.event.listen(self, 'after_create', rule)
        make_rule('INSERT', 'INSTEAD', self.on_insert())
        make_rule('UPDATE', 'INSTEAD', self.on_update())
        make_rule('DELETE', 'INSTEAD', self.on_delete())
        make_rule('INSERT', 'ALSO', self.on_insert_also())
        make_rule('UPDATE', 'ALSO', self.on_update_also())
        make_rule('DELETE', 'ALSO', self.on_delete_also())

    def _original_columns(self):
        return self.c
    
    def _rule_assignments(self, tabular):
        assignments = {}
        for c in self._original_columns():
            table_c = c.element if isinstance(c, sqlalchemy.sql.expression._Label) else c
            if table_c.table is tabular:
                name = _sql_plain_name(c.name)
                table_column_name = _sql_plain_name(table_c.name)
                assignments[table_column_name] = sqlalchemy.literal_column('new.'+name)
        return assignments

    def _rule_condition(self, tabular):
        conditions = []
        for table_c in tabular.primary_key.columns:
            for c in self._original_columns():
                tc = c.element if isinstance(c, sqlalchemy.sql.expression._Label) else c
                if tc is table_c:
                    break
            else:
                raise Exception("Table key column not found in the view", table_c)
            name = _sql_plain_name(c.name)
            conditions.append(table_c == sqlalchemy.literal_column('old.'+name))
        return sqlalchemy.and_(*conditions)

    def _rule_tables(self, order):
        return [object_by_class(tabular, self.search_path()) for tabular in order]

    def on_insert(self):
        if self.insert_order is None:
            return None
        commands = []
        for tabular in self._rule_tables(self.insert_order):
            assignments = self._rule_assignments(tabular)
            c = tabular.insert().values(**assignments)
            commands.append(c)
        return commands

    def on_update(self):
        if self.update_order is None:
            return None
        commands = []
        for tabular in self._rule_tables(self.update_order):
            assignments = self._rule_assignments(tabular)
            condition = self._rule_condition(tabular)
            c = tabular.update().values(**assignments).where(condition)
            commands.append(c)
        return commands

    def on_delete(self):
        if self.delete_order is None:
            return None
        commands = []
        for tabular in self._rule_tables(self.delete_order):
            condition = self._rule_condition(tabular)
            c = tabular.delete().where(condition)
            commands.append(c)
        return commands

    def on_insert_also(self):
        return ()

    def on_update_also(self):
        return ()

    def on_delete_also(self):
        return ()
    
class SQLTable(_SQLTabular):
    _DB_OBJECT = 'TABLE'
    
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
            name = '%s.%s' % (self.schema, inherited.pytis_name(),)
            self.add_is_dependent_on(object_by_name(name, allow_external=False))

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
    _DB_OBJECT = 'VIEW'

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
            else:
                raise Exception("Invalid dependency", o)

    @classmethod
    def _exclude(cls, tabular, *columns_tables, **kwargs):
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
        if inherited:
            the_tabular = tabular.original if isinstance(tabular, sqlalchemy.sql.Alias) else tabular
            if isinstance(the_tabular, SQLTable):
                path = _current_search_path
                try:
                    _set_current_search_path(the_tabular.search_path())
                    tables += [object_by_path(t.pytis_name()) for t in the_tabular.inherits]
                finally:
                    _set_current_search_path(path)
        included = []
        for c in tabular.c:
            if c in columns:
                continue
            cname = c.name
            if cname in columns:
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
        for c in columns:
            if c in raliases:
                aliased.append(c.label(raliases[c]))
            else:
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
                raise Exception("Missing column", c)
        return reordered
    
    def _original_columns(self):
        return self.condition().inner_columns
    
    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

@compiles(SQLView)
def visit_view(element, compiler, **kw):
    return '"%s"."%s"' % (element.schema, element.name,)

class SQLFunctional(_SQLTabular):
    _DB_OBJECT = 'FUNCTION'

    arguments = ()
    result_type = None
    multirow = False
    security_definer = False
    stability = 'volatile'

    _LANGUAGE = None

    __visit_name__ = 'function'

    def __new__(cls, metadata, search_path):
        result_type = cls.result_type
        if result_type is None:
            columns = ()
        elif isinstance(result_type, (tuple, list,)):
            columns = tuple([c.sqlalchemy_column(search_path, None, None, None)
                             for c in result_type])
        elif isinstance(result_type, Column):
            columns = (result_type.sqlalchemy_column(search_path, None, None, None),)
        elif isinstance(result_type, pytis.data.Type):
            columns = (sqlalchemy.Column('result', result_type.sqlalchemy_type()),)
        elif issubclass(result_type, _SQLTabular):
            columns = tuple([sqlalchemy.Column(c.name, c.type)
                             for c in object_by_class(result_type, search_path).c])
        else:
            raise Exception("Invalid result type", result_type)
        args = (cls.name, metadata,) + columns
        return sqlalchemy.Table.__new__(cls, *args, schema=search_path[0])

    def _add_dependencies(self):
        super(SQLFunctional, self)._add_dependencies()
        result_type = self.result_type
        if result_type is not None and not isinstance(result_type, (tuple, list, Column, pytis.data.Type,)):
            self.add_is_dependent_on(object_by_class(result_type, self._search_path))

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

    def __call__(self, *arguments):
        name = '"%s"."%s"' % (self.schema, self.name,)
        # We can't use standard SQLAlchemy function call here
        # (i.e. getattr(sqlalchemy.sql.expression.func, name)(*arguments))
        # since this puts argument symbols instead of argument values into the
        # argument list.
        argument_list = [_sql_value_escape(a) for a in arguments]
        expression = u'%s(%s)' % (name, string.join(argument_list, ', '),)
        return sqlalchemy.literal(expression)

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
        lines = ['#def %s(%s):' % (self.name, arglist,)]
        if arglist:
            l = '    %s = args' % (arglist,) # hard-wired indentation
            if len(self.arguments) == 1:
                l += '[0]'
            lines.append(l)
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
            raise Exception("Invalid plpythonu method", (self.__class__.__name__, name, e))
        match = self._STATICMETHOD_MATCHER.match(lines[0])
        if not match:
            self.Exception("@staticmethod decorator not found", (self.__class__.__name__, name))
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

class SQLEventHandler(SQLFunctional):
    
    table = None

class SQLTrigger(SQLEventHandler):
    __metaclass__ = _PytisTriggerMetaclass
    
    position = 'after'
    events = ('insert', 'update', 'delete',)
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
    __metaclass__ = _PytisSchematicMetaclass
    
    name = None
    depends_on = ()
    
    __visit_name__ = 'raw'
    
    def __init__(self, metadata, search_path):
        self._extra_dependencies = set()
        super(SQLRaw, self).__init__()
        self._search_path = search_path
        self._add_dependencies()
        metadata._add_table(self.pytis_name(), search_path[0], self)
        
    def search_path(self):
        return self._search_path

    def add_is_dependent_on(self, table):
        self._extra_dependencies.add(table)
    
    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)

## Specification processing

engine = None
def _make_sql_command(sql, *multiparams, **params):
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
    return output
def _dump_sql_command(sql, *multiparams, **params):
    output = _make_sql_command(sql, *multiparams, **params)
    print output + ';'

def gsql_file(file_name):
    global _metadata
    _metadata = sqlalchemy.MetaData()
    execfile(file_name, copy.copy(globals()))
    global engine
    engine = sqlalchemy.create_engine('postgresql://', strategy='mock', executor=_dump_sql_command)
    for o in _PytisSimpleMetaclass.objects:
        engine.execute(o)
    for o in _PytisSchematicMetaclass.objects:
        if not isinstance(o, _SQLTabular):
            o.create(engine, checkfirst=False)
    for table in _metadata.sorted_tables:
        table.create(engine, checkfirst=False)
    
## Sample demo

class Private(SQLSchema):
    name = 'private'
    owner = 'postgres'
    access_rights = (('ALL', 'private-users',),)

class Counter(SQLSequence):
    access_rights = (('ALL', 'counter-users',),)

class Foo(SQLTable):
    """Foo table."""
    name = 'foo'
    fields = (PrimaryColumn('id', pytis.data.Serial()),
              Column('foo', pytis.data.String(), doc='some string', index=dict(method='hash')),
              Column('n', pytis.data.Integer(not_null=True), doc='some number'),
              Column('b', pytis.data.Boolean(), default=True),
              Column('description', pytis.data.String()),
              )
    inherits = ()
    tablespace = None
    init_columns = ('foo', 'n',)
    init_values = (('abc', 10,),
                   ('def', 20,),
                   )
    unique = (('foo', 'n',),)
    with_oids = False
    access_rights = (('ALL', 'foo-users',),)

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
    schemas = ((Private, 'public',),)
    fields = (PrimaryColumn('id', pytis.data.Serial()),
              Column('foo_id', pytis.data.Integer(), references=a(r.Foo.id, onupdate='CASCADE')),
              Column('description', pytis.data.String()),
              )
    init_columns = ('foo_id', 'description',)
    init_values = ((1, 'some text'),)
    depends_on = (Foo2,)
    def on_delete(self):
        return ()
    def on_insert_also(self):
        return (object_by_class(Foo2).insert().values(n=sqlalchemy.literal_column('new.id'),
                                                      foo=sqlalchemy.literal_column('new.description')),
                "select 42",)

class BarTrigger(SQLPlFunction, SQLTrigger):
    table = Bar
    events = ('insert',)
    def body(self):
        return """
begin
  insert into foo (n, foo) values (foo_id, description);
end;
"""

class LogTable(SQLTable):
    fields = (PrimaryColumn('id', pytis.data.Serial()),
              Column('table_name', pytis.data.String(),),
              Column('action', pytis.data.String()),
              )

class LogFunction(SQLPyFunction, SQLTrigger):
    events = ()
    arguments = ()
    depends_on = (LogTable,)
    @staticmethod
    def log_function():
        plpy.execute("insert into log_table (table_name, action) values ('%s', '%s')" %
                     (TD['args'][0], TD['event'],))

class _LogTrigger(SQLTrigger):
    events = ('insert', 'update', 'delete',)
    body = LogFunction
    
class LoggingTable(SQLTable):
    fields = (PrimaryColumn('id', pytis.data.Integer()),)
class LoggingTableTrigger(_LogTrigger):
    table = LoggingTable
    arguments = (LoggingTable.pytis_name(),)

class Circular1(SQLTable):
    """Circular REFERENCES, together with Circular2."""
    fields = (PrimaryColumn('id', pytis.data.Integer()),
              Column('x', pytis.data.Integer(), references=a(object_by_reference('public.circular2.id'))),
              )
class Circular2(SQLTable):
    fields = (PrimaryColumn('id', pytis.data.Integer()),
              Column('x', pytis.data.Integer(), references=a(object_by_reference('public.circular1.id'))),
              )

class Baz(SQLView):
    """Baz view."""
    name = 'baz'
    schemas = ((Private, 'public',),)
    @classmethod
    def condition(class_):
        return sqlalchemy.union(sqlalchemy.select([c.Foo.id, c.Bar.description],
                                                  from_obj=[t.Foo.join(t.Bar)]),
                                sqlalchemy.select([c.Foo2.id, sqlalchemy.literal_column("'xxx'", sqlalchemy.String)]))

class Baz2(SQLView):
    schemas = ((Private, 'public',),)
    @classmethod
    def condition(class_):
        return sqlalchemy.select([c.Baz.id], from_obj=[t.Baz], whereclause=(c.Baz.id > 0))

class AliasView(SQLView):
    name = 'aliased'
    @classmethod
    def condition(class_):
        foo1 = t.Foo.alias('foo1')
        foo2 = t.Foo.alias('foo2')
        return sqlalchemy.select([foo1], from_obj=[foo1.join(foo2, foo1.c.n<foo2.c.n)])

class FromSelect(SQLView):
    @classmethod
    def condition(class_):
        foo = t.Foo
        select = sqlalchemy.select([foo], from_obj=[foo]).alias('s')
        return sqlalchemy.select(['s.*'], from_obj=[select], whereclause=('s.n > 0'))

class LimitedView(SQLView):
    @classmethod
    def condition(class_):
        foo = t.Foo
        return sqlalchemy.select(class_._exclude(foo, foo.c.n))

class EditableView(SQLView):
    schemas = ((Private, 'public',),)
    update_order = (Foo, Bar,)
    @classmethod
    def condition(class_):
        return sqlalchemy.select([c.Foo.id, c.Foo.description.label('d1'),
                                  c.Bar.id.label('id2'), c.Bar.description.label('d2')],
                                 from_obj=[t.Foo.join(t.Bar)])

class Func(SQLFunction):
    name = 'plus'
    arguments = (Column('x', pytis.data.Integer()), Column('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    def body(self):
        return 'SELECT $1 + $2'

class FileFunc(SQLFunction):
    name = 'minus'
    arguments = (Column('x', pytis.data.Integer()), Column('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

class PyFunc(SQLPyFunction):
    name = 'times'
    arguments = (Column('x', pytis.data.Integer()), Column('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def times(x, y):
        return pythonic(x, y)

    @staticmethod
    def sub_pythonic(x, y):
        return x * y

class PyFuncSingleArg(SQLPyFunction):
    name = 'single_argument'
    arguments = (Column('x', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def single_argument(x):
        return x + 1

class PyFuncZeroArg(SQLPyFunction):
    name = 'zero_arguments'
    arguments = ()
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def zero_arguments():
        return 42

class SideEffectFunction(SQLPyFunction):
    name = 'foo_insert'
    arguments = (Column('n', pytis.data.Integer()),)

    @staticmethod
    def foo_insert(n):
        plpy.execute("insert into foo (n) values (%s)" % (n,))

class TableSelectFunction(SQLPyFunction):
    name = 'tableselect'
    schemas = (('public', Private,),)
    arguments = (Column('foo', pytis.data.Integer()),)
    result_type = Bar
    multirow = True
    stability = 'stable'

    @staticmethod
    def tableselect(foo):
        return plpy.execute("select * from private.bar where foo_id >= %s" % (foo,)) 
    
class TableFunction(SQLPyFunction):
    name = 'pseudotable'
    arguments = (Column('n', pytis.data.Integer()),)
    result_type = (Column('x', pytis.data.Integer()), Column('y', pytis.data.Integer()), Column('z', pytis.data.Integer()),)
    multirow = True
    stability = 'immutable'

    @staticmethod
    def pseudotable(n):
        result = []
        for i in range(1, n + 1):
            for j in range(1, n + 1):
                result.append([i, j, i * j])
        return result

class NeverUseThis(SQLRaw):
    schemas = ((Private, 'public',),)
    depends_on = (Baz,)                 # just to test dependencies
    @classmethod
    def sql(class_):
        return "select 'never use raw constructs'"

class ReallyNeverUseThis(SQLRaw):
    schemas = ((Private, 'public',),)
    depends_on = (NeverUseThis,)                 # just to test dependencies
    @classmethod
    def sql(class_):
        return "select 'do not use raw constructs anymore'"

def run():
    global engine
    engine = sqlalchemy.create_engine('postgresql://', strategy='mock', executor=_dump_sql_command)
    for o in _PytisSimpleMetaclass.objects:
        engine.execute(o)
        o.after_create(engine)
    for sequence in _metadata._sequences.values():
        sequence.create(engine, checkfirst=False)
        sequence.after_create(engine)
    for table in _metadata.sorted_tables:
        table.create(engine, checkfirst=False)

if __name__ == '__main__':
    run()
