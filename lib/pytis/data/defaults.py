# -*- coding: utf-8 -*-

# Copyright (C) 2019-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2006 OUI Technology Ltd.
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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


"""Setting default database access classes.

This effectively selects particular database backend to be used by importing
the corresponding source file.

"""
from __future__ import print_function

from .dbapi import (  # noqa: F401
    DBDataDefault, DBCounterDefault, DBFunctionDefault, DBTransactionDefault,
    default_access_groups, reload_session_variables, reset_crypto_password,
)
