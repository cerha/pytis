# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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

"""Definice t��d ur�uj�c�ch konkr�tn� pou�it� p��stupov� metody do datab�ze.

Aplikace by m�ly pro p��stup do datab�ze pou��vat v�hradn� instance zde
definovan�ch t��d.

"""

from dbdata import *
from pgsql import *


class DBDataDefaultClass(PostgreSQLUserGroups, RestrictedData,
                         PostgreSQLStandardBindingHandler,
                         DBDataPyPgSQL):
    """Datov� t��da, kterou v�na�ich aplikac�ch standardn� pou��v�me.

    Je utvo�ena pouh�m slo�en�m existuj�c�ch t��d a nezav�d� ��dnou dal�� novou
    funkcionalitu krom� konstruktoru.

    """    
    def __init__(self, bindings, key, dbconnection_spec, ordering=None,
                 access_rights=AccessRights((None, (None, Permission.ALL)))):
        """Stejn� jako u�'DBDataPyPgSQL', vol� v�ak konstruktory p�edk�."""
        RestrictedData.__init__(self, access_rights)
        DBDataPyPgSQL.__init__(self, bindings, key, dbconnection_spec,
                               ordering)
        PostgreSQLStandardBindingHandler.__init__(self)
        # Registrace notifikac� sem jaksi nepat��, ale kam jinam ji d�t, kdy�
        # m� b�t provedena a� po uskute�n�n� v�ech inicializac�?
        import config
        if config.dblisten:
            self._pypg_add_notifications()


### Exportovan� prom�nn�/t��dy


DBDataDefault = DBDataDefaultClass
"""Podt��da 'DBData', kterou pou��v�me pro p��stup k�datab�zi."""

DBCounterDefault = DBPyPgCounter
"""Podt��da t��dy 'Counter', kter� je standardn� pou��v�na."""

DBFunctionDefault = DBPyPgFunction
"""Podt��da t��dy 'Function', kter� je standardn� pou��v�na."""
