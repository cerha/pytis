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

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class Private(sql.SQLSchema):
    name = 'private'
    owner = 'postgres'
    access_rights = (('ALL', 'private-users',),)

class Counter(sql.SQLSequence):
    name = 'counter'
    access_rights = (('ALL', 'counter-users',),)

class Foo(sql.SQLTable):
    """Foo table."""
    name = 'foo'
    fields = (sql.PrimaryColumn('id', pytis.data.LargeSerial()),
              sql.Column('foo', pytis.data.String(), doc='some string', index=dict(method='hash')),
              sql.Column('n', pytis.data.Integer(not_null=True), check='n<1000', doc='some number'),
              sql.Column('b', pytis.data.Boolean(), default=True),
              sql.Column('description', pytis.data.String()),
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
    fields = (sql.Column('bar', pytis.data.String()),)
    check = ('n > 0',)
    init_columns = ()
    init_values = ()
    with_oids = False

class Bar(sql.SQLTable):
    """Bar table."""
    name = 'bar'
    schemas = ((Private, 'public',),)
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('foo_id', pytis.data.Integer(), references=sql.a(sql.r.Foo.id, onupdate='CASCADE')),
              sql.Column('description', pytis.data.String()),
              )
    init_columns = ('foo_id', 'description',)
    init_values = ((1, 'some text'),)
    depends_on = (Foo2,)
    def on_delete(self):
        return ()
    def on_insert_also(self):
        return (sql.object_by_class(Foo2).insert().values(n=sqlalchemy.literal_column('new.id'),
                                                          foo=sqlalchemy.literal_column('new.description')),
                "select 42",)

class BarTrigger(sql.SQLPlFunction, sql.SQLTrigger):
    name = 'bar_trigger'
    table = Bar
    events = ('insert',)
    def body(self):
        return """
begin
  insert into foo (n, foo) values (new.foo_id, new.description);
end;
"""

class Indexed(sql.SQLTable):
    name = 'indexed'
    fields = (sql.Column('x', pytis.data.LTree(), index=dict(method='gist')),
              sql.Column('y', pytis.data.LTree()),
              sql.Column('z', pytis.data.LTree()),
              )
    index_columns = (('x', 'y',),
                     sql.a('x', 'y', 'z', method='gist'),)

class LogTable(sql.SQLTable):
    name = 'log_table'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('table_name', pytis.data.String(),),
              sql.Column('action', pytis.data.String()),
              )
    unique = (('table_name', 'action',),)

class LogFunction(sql.SQLPyFunction, sql.SQLTrigger):
    name = 'log_function'
    events = ()
    arguments = ()
    depends_on = (LogTable,)
    @staticmethod
    def log_function():
        plpy.execute("insert into log_table (table_name, action) values ('%s', '%s')" %
                     (TD['args'][0], TD['event'],))

class LogTrigger(sql.SQLTrigger):
    name = 'log_trigger'
    events = ('insert', 'update', 'delete',)
    body = LogFunction

class _LoggingTable(sql.SQLTable):
    """Demonstration of trigger attachment inheritance.

    'LogTrigger' is attached to all subclass specifications.  Note the
    trick with turning 'trigger' attribute into property.  This works for
    triggers but may not work for other specification properties.
    
    """
    @property
    def triggers(self):
        return ((LogTrigger, self.pytis_name(),),)

class LoggingTable(_LoggingTable):
    name = 'logging_table'
    fields = (sql.PrimaryColumn('id', pytis.data.Integer()),)

class AnotherLoggingTable(_LoggingTable):
    name = 'another_logging_table'
    fields = (sql.PrimaryColumn('x', pytis.data.Integer()),)

class ReferencingTable(sql.SQLTable):
    name = 'referencing_table'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('name', pytis.data.String()),
              sql.Column('action', pytis.data.String()),
              )
    foreign_keys = (sql.a(('name', 'action',), (sql.r.LogTable.table_name, sql.r.LogTable.action,),
                          onupdate='cascade', ondelete='cascade'),)

class Circular1(sql.SQLTable):
    """Circular REFERENCES, together with Circular2."""
    name = 'circular1'
    fields = (sql.PrimaryColumn('id', pytis.data.Integer()),
              sql.Column('x', pytis.data.Integer(), references=sql.r.Circular2.id),
              )
class Circular2(sql.SQLTable):
    name = 'circular2'
    fields = (sql.PrimaryColumn('id', pytis.data.Integer()),
              sql.Column('x', pytis.data.Integer(), references=sql.r.Circular1.id),
              )

class Baz(sql.SQLView):
    """Baz view."""
    name = 'baz'
    schemas = ((Private, 'public',),)
    @classmethod
    def query(class_):
        return sqlalchemy.union(sqlalchemy.select([sql.c.Foo.id, sql.c.Bar.description],
                                                  from_obj=[sql.t.Foo.join(sql.t.Bar)]),
                                sqlalchemy.select([sql.c.Foo2.id, sqlalchemy.literal_column("'xxx'", sqlalchemy.String)]))

class Baz2(sql.SQLView):
    name = 'baz2'
    schemas = ((Private, 'public',),)
    @classmethod
    def query(class_):
        return sqlalchemy.select([sql.c.Baz.id], from_obj=[sql.t.Baz], whereclause=(sql.c.Baz.id > 0))

class AliasView(sql.SQLView):
    name = 'aliased'
    @classmethod
    def query(class_):
        foo1 = sql.t.Foo.alias('foo1')
        foo2 = sql.t.Foo.alias('foo2')
        return sqlalchemy.select([foo1], from_obj=[foo1.join(foo2, foo1.c.n<foo2.c.n)])

class FromSelect(sql.SQLView):
    name = 'from_select'
    @classmethod
    def query(class_):
        foo = sql.t.Foo
        select = sqlalchemy.select([foo], from_obj=[foo]).alias('s(keycol)')
        return sqlalchemy.select(['s.*'], from_obj=[select], whereclause='s.n > 0')

class LimitedView(sql.SQLView):
    name = 'limited_view'
    @classmethod
    def query(class_):
        foo = sql.t.Foo
        return sqlalchemy.select(class_._exclude(foo, foo.c.n))

class EditableView(sql.SQLView):
    name = 'editable_view'
    schemas = ((Private, 'public',),)
    update_order = (Foo, Bar,)
    special_update_columns = ((Foo, 'n', 'length(new.description)',),)
    @classmethod
    def query(class_):
        return sqlalchemy.select([sql.c.Foo.id, sql.c.Foo.description.label('d1'), sql.c.Foo.n,
                                  sql.c.Bar.id.label('id2'), sql.c.Bar.description.label('d2')],
                                 from_obj=[sql.t.Foo.join(sql.t.Bar)])

class SimplifiedEditableView(sql.SQLView):
    """Demonstration how to remove duplicate columns in a join.

    If tables are joined on their columns, it is not necessary to put the join
    column into the view multiple times for each of the view relations.  The
    column can be present in the query only once and this is all needed for
    read-only views.  But for views with modification rules it is necessary to
    tell gensqlalchemy about equivalent join columns using 'join_columns'
    specification property.

    """
    name = 'simplified_editable_view'
    schemas = ((Private, 'public',),)
    insert_order = (Foo, Bar,)
    update_order = (Foo, Bar,)
    delete_order = (Foo, Bar,)
    join_columns = ((sql.c.Foo.id, sql.c.Bar.id,),)
    @classmethod
    def query(class_):
        return sqlalchemy.select([sql.c.Foo.id, sql.c.Foo.description.label('d1'),
                                  sql.c.Bar.description.label('d2')],
                                 from_obj=[sql.t.Foo.join(sql.t.Bar)])

class BogusView(sql.SQLView):
    "One should avoid using outer joins when possible."
    name = 'bogus_view'
    schemas = ((Private, 'public',),)
    @classmethod
    def query(class_):
        foo, bar = sql.t.Foo, sql.t.Bar
        return sqlalchemy.select([foo], from_obj=[sql.FullOuterJoin(foo, bar, foo.c.id==bar.c.id)])

class Func(sql.SQLFunction):
    name = 'plus'
    arguments = (sql.Column('x', pytis.data.Integer()), sql.Column('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    def body(self):
        return 'SELECT $1 + $2'

class FileFunc(sql.SQLFunction):
    name = 'minus'
    arguments = (sql.Column('x', pytis.data.Integer()), sql.Column('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

class FileFuncOverloaded(sql.SQLFunction):
    function_name = 'minus'
    name = 'minus_float'
    arguments = (sql.Column('x', pytis.data.Float()), sql.Column('y', pytis.data.Float()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

class SelectFunc(sql.SQLFunction):
    name = 'foo_increment'
    arguments = (sql.Column('inc', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    multirow = True
    stability = 'stable'

    def body(self):
        return sqlalchemy.select([sql.c.Foo.n + sqlalchemy.literal_column('$1')], from_obj=[sql.t.Foo])

class PyFunc(sql.SQLPyFunction):
    name = 'times'
    arguments = (sql.Column('x', pytis.data.Integer()), sql.Column('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def times(x, y):
        computer = Computer()
        return computer.compute(pythonic(x, y))

    @staticmethod
    def sub_pythonic(x, y):
        return x * y

    class Sub_Computer(object):
        def compute(self, value):
            return value + 1

class PyFuncSingleArg(sql.SQLPyFunction):
    name = 'single_argument'
    arguments = (sql.Column('x', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def single_argument(x):
        return x + 1

class PyFuncZeroArg(sql.SQLPyFunction):
    name = 'zero_arguments'
    arguments = ()
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def zero_arguments():
        return 42

class SideEffectFunction(sql.SQLPyFunction):
    name = 'foo_insert'
    arguments = (sql.Column('n', pytis.data.Integer()),)

    @staticmethod
    def foo_insert(n):
        plpy.execute("insert into foo (n) values (%s)" % (n,))

class TableSelectFunction(sql.SQLPyFunction):
    name = 'tableselect'
    schemas = (('public', Private,),)
    arguments = (sql.Column('foo', pytis.data.Integer()),)
    result_type = Bar
    multirow = True
    stability = 'stable'

    @staticmethod
    def tableselect(foo):
        return plpy.execute("select * from private.bar where foo_id >= %s" % (foo,)) 
    
class TableFunction(sql.SQLPyFunction):
    name = 'pseudotable'
    arguments = (sql.Column('n', pytis.data.Integer()),)
    result_type = (sql.Column('x', pytis.data.Integer()), sql.Column('y', pytis.data.Integer()), sql.Column('z', pytis.data.Integer()),)
    multirow = True
    stability = 'immutable'

    @staticmethod
    def pseudotable(n):
        result = []
        for i in range(1, n + 1):
            for j in range(1, n + 1):
                result.append([i, j, i * j])
        return result

class MultilinePyArguments(sql.SQLPyFunction):
    name = 'multilineargs'
    arguments = tuple([sql.Column(n, pytis.data.Integer()) for n in 'abcdefghi'])
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def multilineargs(a, b, c,
                      d, e, f,
                      g, h, i):
        return a + b + c + d + e + f + g + h + i

class SomeType(sql.SQLType):
    name = 'some_type'
    fields = (sql.Column('x', pytis.data.Integer()),
              sql.Column('y', pytis.data.Integer()),
              )

class NeverUseThis(sql.SQLRaw):
    name = 'never_use_this'
    schemas = ((Private, 'public',),)
    depends_on = (Baz,)                 # just to test dependencies
    @classmethod
    def sql(class_):
        return "select 'never use raw constructs'"

class ReallyNeverUseThis(sql.SQLRaw):
    name = 'really_never_use_this'
    schemas = ((Private, 'public',),)
    depends_on = (NeverUseThis,)                 # just to test dependencies
    @classmethod
    def sql(class_):
        return "select 'do not use raw constructs anymore'"
