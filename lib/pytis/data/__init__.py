# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
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

- Support for data sources connected to a relational
  database (see the module 'dbdata').

"""
from __future__ import print_function

from .types_ import (  # noqa: F401
    Type, Number, Big, Large, Limited,
    Range, Integer, IntegerRange, SmallInteger, LargeInteger, LargeIntegerRange,
    Serial, LargeSerial, Float, DoublePrecision, Monetary, String, Name, PgName,
    Password, RegexString, Color, Inet, Macaddr, Email, TreeOrderBase, TreeOrder,
    FullTextIndex, DateTime, LocalDateTime, DateTimeRange, ISODateTime, Date,
    DateRange, Time, LocalTime, TimeInterval, date_and_time, add_timedelta,
    Boolean, Uuid, Binary, Image, LTree, Array, JSON, JSONB, Enumerator,
    TransactionalEnumerator, FixedEnumerator, DataEnumerator, ValidationError,
    Value, WMValue, sval, ival, fval, bval, dval, dtval, tval, wmval, binval,
)
from .data import (  # noqa: F401
    FORWARD, BACKWARD, ASCENDENT, DESCENDANT,
    Operator, Data, Counter, Function, MemData, ColumnSpec, Row, FetchBuffer,
    DataFactory, EQ, NE, WM, NW, LT, LE, GT, GE, NOT, AND, OR, ANY_OF, IN, FT,
    LTreeMatch, LTreeAncestor, LTreeDescendant, RangeContains, RangeContained,
    RangeOverlap, FunctionCondition, OpFunction,
    reversed_sorting, opposite_direction,
)
from .access import (  # noqa: F401
    Permission, AccessRights, DBAccessRights, RestrictedData, RestrictedMemData,
    DataAccessException, is_in_groups,
)
from .dbdata import (  # noqa: F401
    DBConnection, DBData, DBBinding, DBColumnBinding, DBException,
    DBSystemException, DBUserException, DBLoginException, DBInsertException,
    DBLockException, DBRetryException, NotWithinSelect, DBConnectionPool,
    dbtable, dbfunction, transaction,
)
from .defaults import (  # noqa: F401
    DBDataDefault, DBCounterDefault, DBFunctionDefault, DBTransactionDefault,
    default_access_groups, reload_session_variables, reset_crypto_password,
)
from .deprecated import Oid  # noqa: F401

from . import dbdefs  # noqa: F401
