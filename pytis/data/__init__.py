# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2013 OUI Technology Ltd.
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

"""Data source access abstraction.

The purpose of this module is to hide the low level details of working with
data sources from the application developer.

The module defines:

- Data type abstraction (see the module 'types_').

- Data source abstraction (see the module 'data').

- Support for data sources connected to a relational database (see the
module 'dbdata').

"""
from __future__ import print_function

# Imports use 'X as X' form (PEP 484) to mark names as explicitly re-exported
# without maintaining a separate __all__ list.
from .types_ import (
    Type as Type, Number as Number, Big as Big, Large as Large, Limited as Limited,
    Range as Range, Integer as Integer, IntegerRange as IntegerRange,
    SmallInteger as SmallInteger, LargeInteger as LargeInteger,
    LargeIntegerRange as LargeIntegerRange,
    Serial as Serial, LargeSerial as LargeSerial, Float as Float,
    DoublePrecision as DoublePrecision, Monetary as Monetary,
    String as String, Name as Name, PgName as PgName,
    Password as Password, RegexString as RegexString, Color as Color,
    Inet as Inet, Macaddr as Macaddr, Email as Email,
    TreeOrderBase as TreeOrderBase, TreeOrder as TreeOrder,
    FullTextIndex as FullTextIndex, DateTime as DateTime, LocalDateTime as LocalDateTime,
    DateTimeRange as DateTimeRange, ISODateTime as ISODateTime, Date as Date,
    DateRange as DateRange, Time as Time, LocalTime as LocalTime,
    TimeInterval as TimeInterval, date_and_time as date_and_time,
    add_timedelta as add_timedelta,
    Boolean as Boolean, Uuid as Uuid, Binary as Binary, Image as Image,
    LTree as LTree, Array as Array, JSON as JSON, JSONB as JSONB,
    Enumerator as Enumerator,
    TransactionalEnumerator as TransactionalEnumerator,
    FixedEnumerator as FixedEnumerator, DataEnumerator as DataEnumerator,
    ValidationError as ValidationError,
    Value as Value, WMValue as WMValue, sval as sval, ival as ival, fval as fval,
    bval as bval, dval as dval, dtval as dtval, tval as tval, wmval as wmval,
    binval as binval,
)
from .data import (
    FORWARD as FORWARD, BACKWARD as BACKWARD, ASCENDENT as ASCENDENT,
    DESCENDANT as DESCENDANT,
    Operator as Operator, Data as Data, Counter as Counter, Function as Function,
    MemData as MemData, ColumnSpec as ColumnSpec, Row as Row, FetchBuffer as FetchBuffer,
    DataFactory as DataFactory, EQ as EQ, NE as NE, WM as WM, NW as NW,
    LT as LT, LE as LE, GT as GT, GE as GE, NOT as NOT, AND as AND, OR as OR,
    ANY_OF as ANY_OF, IN as IN, FT as FT,
    LTreeMatch as LTreeMatch, LTreeAncestor as LTreeAncestor,
    LTreeDescendant as LTreeDescendant, RangeContains as RangeContains,
    RangeContained as RangeContained,
    RangeOverlap as RangeOverlap, FunctionCondition as FunctionCondition,
    OpFunction as OpFunction,
    reversed_sorting as reversed_sorting, opposite_direction as opposite_direction,
)
from .access import (
    Permission as Permission, AccessRights as AccessRights,
    DBAccessRights as DBAccessRights, RestrictedData as RestrictedData,
    RestrictedMemData as RestrictedMemData,
    DataAccessException as DataAccessException, is_in_groups as is_in_groups,
)
from .dbdata import (
    DBConnection as DBConnection, DBData as DBData, DBBinding as DBBinding,
    DBColumnBinding as DBColumnBinding, DBException as DBException,
    DBSystemException as DBSystemException, DBUserException as DBUserException,
    DBLoginException as DBLoginException, DBInsertException as DBInsertException,
    DBLockException as DBLockException, DBRetryException as DBRetryException,
    NotWithinSelect as NotWithinSelect, DBConnectionPool as DBConnectionPool,
    dbtable as dbtable, dbfunction as dbfunction, create as create, transaction as transaction,
    REPEATABLE_READ as REPEATABLE_READ, Transaction as Transaction,
)
from .defaults import (
    DBDataDefault as DBDataDefault, DBCounterDefault as DBCounterDefault,
    DBFunctionDefault as DBFunctionDefault, DBTransactionDefault as DBTransactionDefault,
    default_access_groups as default_access_groups,
    reload_session_variables as reload_session_variables,
    reset_crypto_password as reset_crypto_password,
)
from .deprecated import Oid as Oid

from . import dbdefs
