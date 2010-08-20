# -*- coding: utf-8 -*-

# Copyright (C) 2010 Brailcom, o.p.s.
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

"""Gensql definitions for print output formatting."""

db_rights = globals().get('Gall_pytis', None)

_std_table('e_pytis_output_templates',
           (P('id', TSerial),
            C('module', TString, constraints=('not null',)),
            C('specification', TString, constraints=('not null',)),
            C('data', TString),
            ),
           """Storage of print output templates handled by a DatabaseResolver.""",
           sql="unique (module, specification)",
           grant=db_rights,
           depends=())
