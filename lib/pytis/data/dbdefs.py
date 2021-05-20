# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2013-2015 OUI Technology Ltd.
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

"Miscellaneous utilities for gensqlalchemy specifications."

from past.builtins import basestring

import sqlalchemy

# Simple shorthands

null = sqlalchemy.sql.null()
coalesce = sqlalchemy.sql.functions.coalesce
and_ = sqlalchemy.and_
or_ = sqlalchemy.or_
not_ = sqlalchemy.not_
case = sqlalchemy.case
func = sqlalchemy.func
true = sqlalchemy.sql.true()
false = sqlalchemy.sql.false()
between = sqlalchemy.sql.between
select = sqlalchemy.select
exists = sqlalchemy.exists
distinct = sqlalchemy.distinct


def dval(date):
    """Return literal date value.

    Arguments:

      date -- ISO date string, e.g. "2012-12-31"

    """
    return sqlalchemy.literal_column("'%s'" % (date,), type_=sqlalchemy.Date())


def ival(number):
    """Return literal integer value.

    Arguments:

      number -- integer value

    """
    return sqlalchemy.literal_column(str(number), type_=sqlalchemy.Integer())


def fval(number):
    """Return literal float value.

    Arguments:

      number -- float value

    """
    return sqlalchemy.literal_column(str(number), type_=sqlalchemy.Float())


def itval(interval):
    """Return literal interval value.

    Arguments:

      interval -- PostgreSQL interval string, e.g. "11:59:00" or "1 day"

    """
    return sqlalchemy.literal_column("'%s'" % (interval,), type_=sqlalchemy.Interval())


def sval(text, length=None):
    """Return literal string value.

    Arguments:

      text -- unicode value

    """
    return sqlalchemy.literal_column(
        "'%s'" % (text.replace("'", "''"),), type_=sqlalchemy.String(length=length)
    )



def dtype(expr):
    """Return SQLAlchemy expression casted to date.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.Date())


def ttype(expr, timezone=False):
    """Return SQLAlchemy expression casted to time.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.Time(timezone=False))


def dttype(expr, timezone=False):
    """Return SQLAlchemy expression casted to datetime.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.DateTime(timezone=False))


def itype(expr):
    """Return SQLAlchemy expression casted to integer.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.Integer())


def ftype(expr, precision, scale, asdecimal=False):
    """Return SQLAlchemy expression casted to numeric.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.Numeric(precision=precision, scale=scale,
                                                    asdecimal=asdecimal))


def rtype(expr):
    """Return SQLAlchemy expression casted to float.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.Float())


def ittype(expr):
    """Return SQLAlchemy expression casted to interval.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.Interval())


def stype(expr, **kwargs):
    """Return SQLAlchemy expression casted to string.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.String(**kwargs))


def btype(expr, **kwargs):
    """Return SQLAlchemy expression casted to boolean.

    Arguments:

      expr -- 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return sqlalchemy.cast(expr, sqlalchemy.Boolean(**kwargs))


def is_(column, other):
    """Like SQLAlchemy 'is_()' method, but casted to boolean.

    Arguments:

      column -- SQLAlchemy column instance to compare
      other -- object to compare the column to

    """
    return sqlalchemy.cast(column.is_(other), sqlalchemy.Boolean())


def if_(condition, then_, else_):
    """Return simple conditional expression.

    Arguments:

      condition -- condition, 'sqlalchemy.sql.expression.ClauseElement' instance
      then_ -- then part, 'sqlalchemy.sql.expression.ClauseElement' instance
      else_ -- else part, 'sqlalchemy.sql.expression.ClauseElement' instance

    """
    return case([(condition, then_)], else_=else_)


def _rule_assignments(values):
    assignments = {}
    for v in values:
        if isinstance(v, basestring):
            assignments[v] = sqlalchemy.literal_column('new.%s' % (v,))
        elif isinstance(v, tuple):
            assert len(v) == 2, v
            vv = v[1]
            if isinstance(vv, basestring):
                vv = sqlalchemy.literal_column(vv)
            assignments[v[0]] = vv
        else:
            raise Exception("Invalid rule assignment specifier", v)
    return assignments


def rule_condition(*specifiers):
    """Return condition expression based on 'specifiers'.

    Arguments:

      specifiers -- sequence of condition specifiers.  Each of the specifiers
        can be on of the following:

          basestring -- it names the column to assign and the same column name
            is used from OLD to get the value
          tuple of two elements -- the first element (basestring) names the
            column to assign, the second element defines the value; the value
            may be either a 'sqlalchemy.sql.expression.ClauseElement' instance
            or a basestring to be wrapped by 'sqlalchemy.literal_column'
          'sqlalchemy.sql.expression.ClauseElement' instance -- it is used as
            it is

    The resulting condition is a conjunction of the conditions generated from
    the specifiers.

    """
    conditions = []
    for s in specifiers:
        if isinstance(s, basestring):
            c = sqlalchemy.literal_column(s) == sqlalchemy.literal_column('old.%s' % (s,))
        elif isinstance(s, tuple):
            assert len(s) == 2, s
            sv = s[1]
            if isinstance(sv, basestring):
                sv = sqlalchemy.literal_column(sv)
            c = sqlalchemy.literal_column(s[0]) == sv
        else:
            assert isinstance(s, sqlalchemy.sql.expression.ClauseElement), s
            c = s
        conditions.append(c)
    return and_(*conditions)


def rule_insert(table, values, inline=False):
    """Return typical insert rule statement.

    Arguments:

      table -- 'sqlalchemy.schema.Table' instance to apply the insert action on
      values -- sequence of column assignment specifications; each of the
        element is one of the following forms:

          basestring -- it names the column to assign and the same column name
            is used from NEW to get the value
          tuple of two elements -- the first element (basestring) names the
            column to assign, the second element defines the value; the value
            may be either a 'sqlalchemy.sql.expression.ClauseElement' instance
            or a basestring to be wrapped by 'sqlalchemy.literal_column'

    """
    insert = table.insert(inline=inline)
    return insert.values(**_rule_assignments(values))


def rule_update(table, conditions, values):
    """Return typical update rule statement.

    Arguments:

      table -- 'sqlalchemy.schema.Table' instance to apply the update action on
      conditions -- sequence of specifiers to pass to 'rule_condition()' in
        order to create WHERE clause for the update statement
      values -- sequence of column assignment specifications; each of the
        element is one of the following forms:

          basestring -- it names the column to assign and the same column name
            is used from NEW to get the value
          tuple of two elements -- the first element (basestring) names the
            column to assign, the second element defines the value; the value
            may be either a 'sqlalchemy.sql.expression.ClauseElement' instance
            or a basestring to be wrapped by 'sqlalchemy.literal_column'

    """
    update = table.update()
    if conditions:
        update = update.where(rule_condition(*conditions))
    assignments = _rule_assignments(values)
    result = update.values(**assignments)
    return result
