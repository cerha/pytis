# -*- coding: utf-8 -*-

# Copyright (C) 2019, 2021 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Data source access abstraction.

The purpose of this module is to hide the low level details of working with
data sources from the application developer.

The module defines:

- Data type abstraction (see the module 'types_').

- Data source abstraction (see the module 'data').

- Support for data sources connected to a relational
  database (see the module 'dbdata').

"""

from .types_ import (  # noqa: F401
    Type, Number, Big, Large, Limited,
    Range, Integer, IntegerRange, SmallInteger, LargeInteger, LargeIntegerRange,
    Serial, LargeSerial, Float, DoublePrecision, Monetary, String, Name,
    Password, RegexString, Color, Inet, Macaddr, Email, TreeOrderBase, TreeOrder,
    FullTextIndex, DateTime, LocalDateTime, DateTimeRange, ISODateTime, Date,
    DateRange, Time, LocalTime, TimeInterval, date_and_time, add_timedelta,
    Boolean, Binary, Image, LTree, Array, Enumerator, TransactionalEnumerator,
    FixedEnumerator, DataEnumerator, ValidationError, Value, WMValue,
    sval, ival, fval, bval, dval, dtval, tval, wmval,
)
from .data import (  # noqa: F401
    FORWARD, BACKWARD, ASCENDENT, DESCENDANT,
    Operator, Data, Counter, Function, MemData, ColumnSpec, Row, FetchBuffer,
    DataFactory, EQ, NE, WM, NW, LT, LE, GT, GE, NOT, AND, OR, ANY_OF, IN, FT,
    LTreeMatch, LTreeAncestor, LTreeDescendant, RangeContains, RangeContained,
    RangeOverlap, FunctionCondition, OpFunction,
    reversed_sorting, opposite_direction, dbtable, dbfunction, transaction
)
from .access import (  # noqa: F401
    Permission, AccessRights, DBAccessRights, RestrictedData, RestrictedMemData,
    DataAccessException, is_in_groups,
)
from .dbdata import (  # noqa: F401
    DBConnection, DBData, DBBinding, DBColumnBinding, DBException,
    DBSystemException, DBUserException, DBLoginException, DBInsertException,
    DBLockException, DBRetryException, NotWithinSelect, DBConnectionPool,
)
from .defaults import (  # noqa: F401
    DBDataDefault, DBCounterDefault, DBFunctionDefault,
    DBTransactionDefault, default_access_groups, reload_session_variables,
)
from .deprecated import Oid  # noqa: F401

from . import dbdefs  # noqa: F401

# TODO: The following import serves for backward compatibility of applications
# and should be removed once applications switch to using the
# default_access_groups function.
from .postgresql import PostgreSQLUserGroups  # noqa: F401
