# -*- coding: utf-8 -*-

# Copyright (C) 2019-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2010 OUI Technology Ltd.
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

from __future__ import unicode_literals
from __future__ import print_function

import lcg


def variables(resolver):
    return dict(love=lcg.TextContent("make love"),
                war=lcg.TextContent("not war"))
