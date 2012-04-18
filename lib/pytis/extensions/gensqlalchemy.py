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
import sqlalchemy.dialects.postgresql
import pytis.data


class Column(sqlalchemy.schema.Column, pytis.data.ColumnSpec):
    
    def __init__(self, name, type, doc=None, unique=False, check=None,
                 default=None, references=None, primary_key=False, index=False):
        alchemy_type = type.sqlalchemy_type()
        args = []
        if references: # sqlalchemy.schema.ForeignKey instance
            args.append(references)
        sqlalchemy.schema.Column.__init__(self, name, alchemy_type, *args, default=default, doc=doc,
                                          index=index, nullable=(not type.not_null()),
                                          primary_key=primary_key, unique=unique)
        pytis.data.ColumnSpec.__init__(self, name, type)

class PrimaryColumn(Column):
    
    def __init__(self, *args, **kwargs):
        kwargs = copy.copy(kwargs)
        kwargs['primary_key'] = True
        super(PrimaryColumn, self).__init__(*args, **kwargs)


class SQLTable(sqlalchemy.schema.Table):
    
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
        columns = cls.fields
        args = (cls.name, metadata,) + columns
        for check in cls.check:
            args += (sqlalchemy.schema.CheckConstraint(check),)
        for unique in cls.unique:
            args += (sqlalchemy.schema.UniqueConstraint (*unique),)
        return sqlalchemy.schema.Table.__new__(cls, *args, schema=schema)

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
            command = ("COMMENT ON TABLE \"%s\".\"%s\" IS '%s'" %
                       (self.schema, self.name, doc.replace("'", "''"),))
            sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))
        for c in self.fields:
            doc = c.doc
            if doc:
                command = ("COMMENT ON COLUMN \"%s\".\"%s\".\"%s\" IS '%s'" %
                           (self.schema, self.name, c.name, doc.replace("'", "''"),))
                sqlalchemy.event.listen(self, 'after_create', sqlalchemy.DDL(command))

    def create(self, bind=None, checkfirst=False):
        super(SQLTable, self).create(bind=bind, checkfirst=checkfirst)
        self._insert_values(bind)
        
    def _insert_values(self, bind):
        if self.init_values:
            # TODO: This performs the SQL statements but doesn't log them.
            connection = bind.connect()
            for row in self.init_values:
                values = dict(zip(self.init_columns, row))
                insert = self.insert().values(**values)
                connection.execute(insert)


## Sample demo


class Foo(SQLTable):
    """Foo table."""
    name = 'foo'
    schemas = ('public',)
    search_path = ('public',)
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


class Bar(Foo):
    """Bar table."""
    name = 'bar'
    inherits = (Foo,)
    fields = copy.deepcopy(Foo.fields) + (Column('bar', pytis.data.String()),)
    check = ('n > 0',)
    init_columns = ()
    init_values = ()
    with_oids = False


if __name__ == '__main__':
    # TODO: How to just output SQL commands without connection to the database?
    # See sqlalchemy.engine.base.Connection; if we can override it and make an
    # engine around it then we could do whatever we want with the SQL commands.
    engine = sqlalchemy.create_engine('postgresql:///test', echo=True)
    metadata = sqlalchemy.MetaData()
    for cls in (Foo, Bar,):
        for schema in cls.schemas:
            table = cls(metadata, schema)
            table.create(engine)

