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
              Column('n', pytis.data.Integer(not_null=True), check='n<1000', doc='some number'),
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
  insert into foo (n, foo) values (new.foo_id, new.description);
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

class SomeType(SQLType):
    fields = (Column('x', pytis.data.Integer()),
              Column('y', pytis.data.Integer()),
              )

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
