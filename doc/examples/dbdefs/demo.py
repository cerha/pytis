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

import sqlalchemy
import pytis.data
import pytis.data.gensqlalchemy as sql
from pytis.data.dbdefs import ival

class Private(sql.SQLSchema):
    name = 'private'
    owner = 'postgres'
    access_rights = (('ALL', 'private-users',),)

class Counter(sql.SQLSequence):
    name = 'counter'
    access_rights = (('ALL', 'counter-users',),)


# Tables

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
    fields = (sql.Column('bar', pytis.data.String()),
              sql.Column('r', pytis.data.IntegerRange()),
              )
    check = ('n > 0',)
    init_columns = ()
    init_values = ()
    with_oids = False
    access_rights = (('ALL', True,),)

class Bar(sql.SQLTable):
    """Bar table."""
    name = 'bar'
    schemas = ((Private, 'public',),)
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('foo_id', pytis.data.Integer(),
                         references=sql.a(sql.r.Foo.id, onupdate='CASCADE')),
              sql.Column('description', pytis.data.String()),
              )
    init_columns = ('foo_id', 'description',)
    init_values = ((1, 'some text'),)
    depends_on = (Foo2,)
    def on_delete(self):
        return ()
    def on_insert_also(self):
        Foo2 = sql.t.Foo2
        n_column = sqlalchemy.literal_column('1').label('n')
        return (Foo2.insert().values(n=sqlalchemy.literal_column('new.id'),
                                     foo=sqlalchemy.literal_column('new.description')),
                sql.InsertFromSelect(Foo2, sqlalchemy.select([n_column])),
                "select 42",
                )
    owner = 'pytis'
    access_rights = (('ALL', True,),)

class BarTrigger(sql.SQLPlFunction, sql.SQLTrigger):
    "Trigger directly defining its function."
    name = 'bar_trigger'
    schemas = ((Private, 'public',),)
    table = Bar
    events = ('insert',)
    def body(self):
        return """
begin
  insert into foo (n, foo) values (new.foo_id, new.description);
  return null;
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

class FunctionallyIndexed(sql.SQLTable):
    name = 'functionally_indexed'
    fields = (sql.Column('x', pytis.data.Integer()),
              sql.Column('y', pytis.data.Integer()),
              sql.Column('z', pytis.data.Integer()),
              )
    @property
    def index_columns(self):
        y = sqlalchemy.func.coalesce(sql.c.FunctionallyIndexed.y,
                                     sqlalchemy.literal_column('-1', type_=sqlalchemy.Integer()))
        return (sql.a('x', y, unique=True),)

class DateTable(sql.SQLTable):
    name = 'date_table'
    schemas = (('public',),
               (Private,),)
    fields = (sql.Column('world_timestamp', pytis.data.DateTime(utc=True)),
              sql.Column('local_timestamp', pytis.data.DateTime(utc=False)),
              sql.Column('ambigous_timestamp', pytis.data.DateTime(without_timezone=True)),
              )

class NonOverlappingRanges(sql.SQLTable):
    name = 'non_overlapping_ranges'
    fields = (sql.Column('d', pytis.data.DateRange()),
              sql.Column('d2', pytis.data.DateRange()),)
    exclude_constraints = ((('d', '&&'),),
                           (('d2', '&&'), dict(deferrable=True)),)

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

class DemoLogTrigger(sql.SQLTrigger):
    "Tables with this trigger automatically log changes."
    name = 'demo_log_trigger'
    events = ('insert', 'update', 'delete',)
    body = LogFunction

class _LoggingTable(sql.SQLTable):
    """Demonstration of trigger attachment inheritance.

    'DemoLogTrigger' is attached to all subclass specifications.  Note the
    trick with turning 'trigger' attribute into property.  This works for
    triggers but may not work for other specification properties.

    """
    @property
    def triggers(self):
        return ((DemoLogTrigger, self.pytis_name(),),)

class LoggingTable(_LoggingTable):
    name = 'logging_table'
    fields = (sql.PrimaryColumn('id', pytis.data.Integer()),)

class AnotherLoggingTable(_LoggingTable):
    name = 'another_logging_table'
    fields = (sql.PrimaryColumn('x', pytis.data.Integer()),)

class AnonymousTrigger(sql.SQLTrigger):
    """There is no explicit name for this trigger.

    The trigger name is generated from its table name, events and position.
    
    """
    body = LogFunction
    table = Bar
    schemas = ((Private, 'public',),)
    
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

class TableWithLongNames(sql.SQLTable):
    "Just a test of name length limits."
    name = 'table_with_long_name_names'
    fields = (sql.Column('column_with_a_long_name', pytis.data.String()),
              sql.Column('another_column_with_a_long_name', pytis.data.String()),
              )
    index_columns = (('column_with_a_long_name', 'another_column_with_a_long_name',),)


# Views

class Baz(sql.SQLView):
    """Baz view."""
    name = 'baz'
    schemas = ((Private, 'public',),)
    @classmethod
    def query(class_):
        return sqlalchemy.union(sqlalchemy.select([sql.c.Foo.id, sql.c.Bar.description],
                                                  from_obj=[sql.t.Foo.join(sql.t.Bar)]),
                                sqlalchemy.select([sql.c.Foo2.id,
                                                   sqlalchemy.literal_column("'xxx'",
                                                                             sqlalchemy.String)]))

class Baz2(sql.SQLView):
    name = 'baz2'
    schemas = ((Private, 'public',),)
    @classmethod
    def query(class_):
        return sqlalchemy.select([sql.c.Baz.id], from_obj=[sql.t.Baz],
                                 whereclause=(sql.c.Baz.id > 0))

class Baz2Dup(sql.SQLView):
    name = 'baz2'
    schemas = (('public', Private,),)
    @classmethod
    def query(class_):
        return sqlalchemy.select([sql.c.Baz.id], from_obj=[sql.t.Baz],
                                 whereclause=(sql.c.Baz.id <= 0))

class MaterializedView(sql.SQLMaterializedView, Baz):
    """Materialized views are mostly just specialized views.

    They may not have rules but their columns may be indexed.

    """
    name = 'mview'
    index_columns = (('description',),)

class AliasView(sql.SQLView):
    name = 'aliased'
    @classmethod
    def query(class_):
        foo1 = sql.t.Foo.alias('foo1')
        foo2 = sql.t.Foo.alias('foo2')
        return sqlalchemy.select([foo1], from_obj=[foo1.join(foo2, foo1.c.n < foo2.c.n)])

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
    special_update_columns = ((Foo, 'n', 'length(new.d1)',),)
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
    primary_column = 'id'
    insert_order = (Foo, Bar,)
    update_order = (Foo, Bar,)
    delete_order = (Foo, Bar,)
    @classmethod
    def join_columns(class_):
        return ((sql.c.Foo.id, sql.c.Bar.id,),)
    @classmethod
    def query(class_):
        return sqlalchemy.select([sql.c.Foo.id, sql.c.Foo.description.label('d1'),
                                  sql.c.Bar.description.label('d2')],
                                 from_obj=[sql.t.Foo.join(sql.t.Bar)])

class ViewOverView(sql.SQLView):
    """This view defines modification rules on SimplifiedEditableView.
    
    For this reason SimplifiedEditableView must define primary_column.
    
    """
    name = 'view_over_view'
    schemas = ((Private, 'public',),)
    insert_order = (SimplifiedEditableView,)
    update_order = (SimplifiedEditableView,)
    delete_order = (SimplifiedEditableView,)
    @classmethod
    def query(class_):
        return sqlalchemy.select([sql.t.SimplifiedEditableView])

class BogusView(sql.SQLView):
    "One should avoid using full outer joins when possible."
    name = 'bogus_view'
    schemas = ((Private, 'public',),)
    @classmethod
    def query(class_):
        foo, bar = sql.t.Foo, sql.t.Bar
        return sqlalchemy.select([foo], from_obj=[sql.FullOuterJoin(foo, bar,
                                                                    foo.c.id == bar.c.id)])


# Functions

class Func(sql.SQLFunction):
    name = 'plus'
    arguments = (sql.Argument('x', pytis.data.Integer()), sql.Argument('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    def body(self):
        return 'SELECT $1 + $2'

class FileFunc(sql.SQLFunction):
    name = 'minus'
    arguments = (sql.Argument('x', pytis.data.Integer()), sql.Argument('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

class FileFuncOverloaded(sql.SQLFunction):
    db_name = 'minus'
    name = 'minus_float'
    arguments = (sql.Argument('x', pytis.data.Float()), sql.Argument('y', pytis.data.Float()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

class SelectFunc(sql.SQLFunction):
    name = 'foo_increment'
    arguments = (sql.Argument('inc', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    multirow = True
    stability = 'stable'

    def body(self):
        return sqlalchemy.select([sql.c.Foo.n + sqlalchemy.literal_column('$1')],
                                 from_obj=[sql.t.Foo])

    depends_on = (Foo,)

class RowSelectFunc(sql.SQLFunction):
    name = 'row_select_func'
    arguments = (sql.Argument('min_n', pytis.data.Integer()),)
    result_type = (sql.Column('foo', pytis.data.String()),
                   sql.Column('n', pytis.data.Integer()),
                   sql.Column('b', pytis.data.Boolean()),
                   )
    multirow = True
    stability = 'stable'
    execution_cost = 10
    expected_rows = 4

    def body(self):
        foo = sql.t.Foo
        return sqlalchemy.select(self._exclude(foo, foo.c.id, foo.c.description),
                                 whereclause=(foo.c.n >= ival('$1')))

    depends_on = (Foo,)

class PrivateDateTable(sql.SQLFunction):
    "Demonstration of 'set_parameters' attribute."
    name = 'private_date_table'
    arguments = ()
    result_type = DateTable
    multirow = True
    stability = 'stable'
    set_parameters = (('TIME ZONE', "'PST8PDT'"),
                      ('search_path', 'private'),)
    def body(class_):
        return "select * from date_table"

class PyFunc(sql.SQLPyFunction):
    name = 'times'
    arguments = (sql.Argument('x', pytis.data.Integer()), sql.Argument('y', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    stability = 'immutable'

    class Util(sql.SQLPyFunction.Util):

        INCREMENT = 1

        @staticmethod
        def pythonic(x, y):
            return x * y

        class Computer(object):
            def compute(self, value):
                return value + PyFunc.Util.INCREMENT

    @staticmethod
    def times(x, y):
        computer = PyFunc.Util.Computer()
        return computer.compute(PyFunc.Util.pythonic(x, y))

class PyFuncSingleArg(sql.SQLPyFunction):
    name = 'single_argument'
    arguments = (sql.Argument('x', pytis.data.Integer()),)
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
    arguments = (sql.Argument('n', pytis.data.Integer()),)

    @staticmethod
    def foo_insert(n):
        plpy.execute("insert into foo (n) values (%s)" % (n,))

class TableSelectFunction(sql.SQLPyFunction):
    name = 'tableselect'
    schemas = (('public', Private,),)
    arguments = (sql.Argument('foo', pytis.data.Integer()),)
    result_type = Bar
    multirow = True
    stability = 'stable'

    @staticmethod
    def tableselect(foo):
        return plpy.execute("select * from private.bar where foo_id >= %s" % (foo,))
    
class TableFunction(sql.SQLPyFunction):
    name = 'pseudotable'
    arguments = (sql.Argument('n', pytis.data.Integer()),)
    result_type = (sql.Column('x', pytis.data.Integer()),
                   sql.Column('y', pytis.data.Integer()),
                   sql.Column('z', pytis.data.Integer()),)
    multirow = True
    stability = 'immutable'

    @staticmethod
    def pseudotable(n):
        result = []
        for i in range(1, n + 1):
            for j in range(1, n + 1):
                result.append([i, j, i * j])
        return result

class TableFunctionView(sql.SQLView):
    name = 'function_view'
    @classmethod
    def query(class_):
        f = sql.t.TableFunction(4)
        return sqlalchemy.select(['*'], from_obj=[f])
    depends_on = (TableFunction,)

class MultilinePyArguments(sql.SQLPyFunction):
    name = 'multilineargs'
    arguments = tuple([sql.Argument(n, pytis.data.Integer()) for n in 'abcdefghi'])
    result_type = pytis.data.Integer()
    stability = 'immutable'

    @staticmethod
    def multilineargs(a, b, c,
                      d, e, f,
                      g, h, i):
        return a + b + c + d + e + f + g + h + i

class RowFunction(sql.SQLPlFunction):
    name = 'row_n'
    arguments = (sql.Argument('row', Foo),)
    result_type = pytis.data.Integer()
    stability = 'immutable'
    def body(self):
        return "begin return row.n; end;"

class CountFunction(sql.SQLFunction, sql.SQLAggregate):
    name = 'count_f'
    arguments = (sql.Column('state', pytis.data.Integer()),)
    result_type = pytis.data.Integer()
    initial_value = 0
    def body(self):
        return 'SELECT $1 + 1'

class SumFunction(sql.SQLFunction, sql.SQLAggregate):
    name = 'sum_f'
    arguments = (sql.Column('state', pytis.data.Integer()),
                 sql.Column('n', pytis.data.Integer()),
                 )
    result_type = pytis.data.Integer()
    initial_value = 0
    def body(self):
        return 'SELECT $1 + $2'


# Miscellaneous

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
