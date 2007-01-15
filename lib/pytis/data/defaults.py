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

"""Definice tříd určujících konkrétní použité přístupové metody do databáze.

Aplikace by měly pro přístup do databáze používat výhradně instance zde
definovaných tříd.

"""

from dbdata import *
from pgsql import *


class DBDataDefaultClass(PostgreSQLUserGroups, RestrictedData, DBDataPyPgSQL):
    """Datová třída, kterou v našich aplikacích standardně používáme.

    Je utvořena pouhým složením existujících tříd a nezavádí žádnou další novou
    funkcionalitu kromě konstruktoru.

    """    
    def __init__(self, bindings, key, connection_data=None, ordering=None,
                 access_rights=AccessRights((None, (None, Permission.ALL))),
                 dbconnection_spec=None, **kwargs):
        # TODO: Vyřadit dbconnection_spec ze seznamu argumentů po konverzi
        # aplikací.
        if dbconnection_spec is not None:
            if connection_data is not None:
                raise Exception("Programming error: " +
                                "Both connection_data and dbconnection_spec given")
            connection_data = dbconnection_spec
        super(DBDataDefaultClass, self).__init__(
            bindings=bindings, key=key, connection_data=connection_data,
            ordering=ordering, access_rights=access_rights, **kwargs)
        # TODO: Následující hack je tu proto, že ve voláních konstruktorů výše
        # je _pg_add_notifications voláno předčasně, přičemž pořadí volání
        # konstruktorů nelze změnit.  Pro nápravu je potřeba ještě předělat
        # třídy týkající se notifikací.
        self._pg_add_notifications()


### Exportované proměnné/třídy


DBDataDefault = DBDataDefaultClass
"""Podtřída 'DBData', kterou používáme pro přístup k databázi."""

DBCounterDefault = DBPyPgCounter
"""Podtřída třídy 'Counter', která je standardně používána."""

DBFunctionDefault = DBPyPgFunction
"""Podtřída třídy 'Function', která je standardně používána."""

def _postgresql_access_groups(connection_data):
    import pytis.data.pgsql
    class PgUserGroups(pytis.data.pgsql._PgsqlAccessor,
                       PostgreSQLUserGroups):
        pass
    return PgUserGroups(connection_data).access_groups()
default_access_groups = _postgresql_access_groups
"""Funkce vracející seznam skupin uživatele specifikovaného spojení."""
