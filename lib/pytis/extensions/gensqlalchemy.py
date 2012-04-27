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
import sqlalchemy
from sqlalchemy.ext.compiler import compiles
import pytis.data

## SQLAlchemy extensions

class _PytisSchemaGenerator(sqlalchemy.engine.ddl.SchemaGenerator):

    def visit_view(self, view, create_ok=False):
        command = 'CREATE OR REPLACE VIEW "%s"."%s" AS %s' % (view.schema, view.name, view.condition,)
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

    def sqlalchemy_column(self):
        alchemy_type = self.type().sqlalchemy_type()
        args = []
        if self._references:
            args.append(sqlalchemy.ForeignKey(self._references))
        return sqlalchemy.Column(self.id(), alchemy_type, *args, default=self._default,
                                 doc=self._doc, index=self._index,
                                 nullable=(not self.type().not_null()),
                                 primary_key=self._primary_key, unique=self._unique)

class PrimaryColumn(Column):
    
    def __init__(self, *args, **kwargs):
        kwargs = copy.copy(kwargs)
        kwargs['primary_key'] = True
        super(PrimaryColumn, self).__init__(*args, **kwargs)

## Utilities

_metadata = sqlalchemy.MetaData()

class SQLException(Exception):
    pass

class _PytisTableMetaclass(sqlalchemy.sql.visitors.VisitableType):
    
    _name_mapping = {}
    
    def __init__(cls, clsname, bases, clsdict):
        is_specification = not clsname.startswith('SQL')
        if is_specification:
            name = cls.name
            if name is None:
                name = pytis.util.camel_case_to_lower(clsname)
            if (name in _PytisTableMetaclass._name_mapping and 
                _PytisTableMetaclass._name_mapping[name] is not cls):
                raise SQLException("Duplicate object name", (cls, _PytisTableMetaclass._name_mapping[name],))
            _PytisTableMetaclass._name_mapping[name] = cls
            cls.name = name
        sqlalchemy.sql.visitors.VisitableType.__init__(cls, clsname, bases, clsdict)
        if is_specification:
            for schema in cls.schemas:
                cls(_metadata, schema)

def t(name):
    return _metadata.tables[name]
        
## Database objects

class SQLTable(sqlalchemy.Table):
    __metaclass__ = _PytisTableMetaclass
    
    name = None
    schemas = ('public',)
    search_path = ('public',)           # TODO: how to handle this in SQLAlchemy?
    fields = ()
    inherits = ()
    tablespace = None
    init_columns = None
    init_values = ()
    check = ()
    unique = ()
    with_oids = False

    def __new__(cls, metadata, schema):
        columns = tuple([c.sqlalchemy_column() for c in cls.fields])
        args = (cls.name, metadata,) + columns
        for check in cls.check:
            args += (sqlalchemy.CheckConstraint(check),)
        for unique in cls.unique:
            args += (sqlalchemy.UniqueConstraint (*unique),)
        return sqlalchemy.Table.__new__(cls, *args, schema=schema)

    def _init(self, *args, **kwargs):
        super(SQLTable, self)._init(*args, **kwargs)
        self._create_parameters()
        self._create_comments()

    def _alter_table(self, alteration):
        command = 'ALTER TABLE "%s"."%s" %s' % (self.schema, self.name, alteration,)
        sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))

    def _create_parameters(self):
        self._alter_table("SET %s OIDS" % ("WITH" if self.with_oids else "WITHOUT",))
        if self.tablespace:
            self._alter_table('SET TABLESPACE "%s"' % (self.tablespace,))
        for table in self.inherits:
            # TODO: How about schemas here?
            self._alter_table('INHERIT "%s"' % (table.name,))

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


class SQLView(sqlalchemy.Table):

    name = None
    schemas = ('public',)
    search_path = ('public',)           # TODO: how to handle this in SQLAlchemy?
    condition = None

    __visit_name__ = 'view'

    def __new__(cls, metadata, schema):
        columns = tuple([sqlalchemy.Column(c.name, c.type) for c in cls.condition.columns])
        args = (cls.name, metadata,) + columns
        return sqlalchemy.Table.__new__(cls, *args, schema=schema)

    def create(self, bind=None, checkfirst=False):
        bind._run_visitor(_PytisSchemaGenerator, self, checkfirst=checkfirst)


## Sample demo


class Foo(SQLTable):
    """Foo table."""
    name = 'foo'
    fields = (PrimaryColumn('id', pytis.data.Serial()),
              Column('foo', pytis.data.String(), doc='some string'),
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
    fields = Foo.fields + (Column('bar', pytis.data.String()),)
    check = ('n > 0',)
    init_columns = ()
    init_values = ()
    with_oids = False
        
class Bar(SQLTable):
    """Bar table."""
    fields = (PrimaryColumn('id', pytis.data.Serial()),
              # TODO: How to set schema in references automatically?
              Column('foo_id', pytis.data.Integer(), references='public.foo.id'),
              Column('description', pytis.data.String()),
              )
    init_columns = ('foo_id', 'description',)
    init_values = ((1, 'some text'),
                   )

class Baz(SQLView):
    """Baz view."""
    name = 'baz'
    condition = sqlalchemy.union(sqlalchemy.select([t('public.foo').c.id, t('public.bar').c.description],
                                                   from_obj=[t('public.foo').join(t('public.bar'))]),
                                 sqlalchemy.select([t('public.foofoo').c.id, sqlalchemy.literal_column("'xxx'", sqlalchemy.String)]))


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
                # Probably default key value
                for k in compiled.binds.keys():
                    if k not in sql_parameters:
                        column = sql.table.columns[k]
                        parameters[k] = "nextval('%s_%s_seq')" % (column.table.name, column.name,)
            for k, v in sql_parameters.items():
                if v is None:
                    value = 'NULL'
                elif isinstance(v, (int, long, float,)):
                    value = v
                else:
                    value = "'%s'" % (v.replace('\\', '\\\\').replace("'", "''"),)
                parameters[k] = value
            output = unicode(compiled) % parameters
        else:
            output = unicode(compiled)
    print output + ';'

if __name__ == '__main__':
    engine = sqlalchemy.create_engine('postgresql://', strategy='mock', executor=_dump_sql_command)
    for cls in (Foo, Foo2, Bar):
        for schema in cls.schemas:
            table = t('%s.%s' % (schema, cls.name,))
            table.create(engine, checkfirst=False)
    view = Baz(_metadata, 'public')
    view.create(engine, checkfirst=False)
