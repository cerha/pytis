# -*- coding: utf-8 -*-

# Copyright (c) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (c) 2001-2013 OUI Technology Ltd.
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

from __future__ import print_function

__version__ = "1.3.0.dev3"

# Imports use 'X as X' form (PEP 484) to mark names as explicitly re-exported
# without maintaining a separate __all__ list.
from . import util as util
from . import data as data

config = util.Configuration()
