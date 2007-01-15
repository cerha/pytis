# -*- coding: iso-8859-2 -*-

# Přístupová práva
# 
# Copyright (C) 2002, 2004, 2005, 2006 Brailcom, o.p.s.
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

"""Infrastruktura pro přístupová práva.

Je zde definováno vše, co je potřeba pro zajištění přístupových práv k datovým
objektům.  Množina typů přístupových práv je definována třídou 'Permission'.
Logika přístupových práv je implementována ve třídě 'AccessRights'.  Zajištění
přístupu k datovým objektům řízeného přístupovými právy obstarává třída
'RestrictedData'.

Pro signalizaci pokusu o neoprávněný přístup k datům se používá výhradně třída
'DataAccessException'.

"""

import operator

from pytis.data import *
from pytis.util import *


class Permission:
    """Výčtová třída konstant specifikujících povolené přístupové akce.

    Ne všechny zde definované konstanty mají smysl pro všechny datové objekty.
    Například pro sloupec nemá smysl specifikovat 'DELETE' a pro uživatelský
    příkaz je nejdůležitější 'CALL'.

    """
    
    VIEW = 'VIEW'
    """Právo k prohlížení obsahu."""
    INSERT = 'INSERT'
    """Právo vložit nový záznam."""
    UPDATE = 'UPDATE'
    """Právo změnit existující data."""
    DELETE = 'DELETE'
    """Právo smazat existující data."""
    CALL = 'CALL'
    """Právo ke spuštění."""
    EXPORT = 'EXPORT'
    """Právo k exportu do CSV."""
    ALL = 'ALL'
    """Všechna práva k danému objektu."""

    def all_permissions(class_):
        """Vrať tuple všech neuniverzálních konstant třídy."""
        return (class_.VIEW, class_.INSERT, class_.UPDATE, class_.DELETE,
                class_.CALL, class_.EXPORT)
    all_permissions = classmethod(all_permissions)


class AccessRights:
    """Specifikace přístupových práv."""
    
    def __init__(self, *access_rights):
        """Inicializuj instanci.

        Argumenty:

          access_rights -- každý argument má podobu tuple tuples ve tvaru
            (COLUMNS, (GROUPS, PERMISSIONS, ...), ...), kde COLUMNS je jméno
            sloupce nebo sekvence jmen sloupců (strings) nebo 'None', GROUPS je
            jméno skupiny nebo sekvence jmen skupin nebo 'None'' a PERMISSIONS
            je sekvence konstant třídy 'Permission'

        'permissions' povoluje přístup, co není dovoleno, je zakázáno.  Je-li
        na místě COLUMNS, resp. GROUP, 'None', jedná se o implicitní hodnotu,
        platnou pokud pro daný sloupec, resp. skupinu, není řečeno jinak.
        Implicitní práva se sčítají s právy zadanými pro nějaký sloupec,
        resp. skupinu, explicitně, tj. lze je rozšířit, nikoliv však omezit.

        Je-li některé z COLUMNS nebo GROUP v rámci COLUMNS uvedeno vícekrát,
        odpovídající práva se sčítají.

        """
        table = {}
        all_permissions = Permission.all_permissions()
        for p in all_permissions:
            table[p] = {}
        for a in access_rights:
            columns, groupdefs = xtuple(a[0]), a[1:]
            for gd in groupdefs:
                groups, permissions = xtuple(gd[0]), gd[1:]
                if Permission.ALL in permissions:
                    permissions = all_permissions
                for p in permissions:
                    table_p = table[p]
                    for c in columns:
                        if None in groups:
                            table_p[c] = (None,)
                        elif table_p.has_key(c):
                            table_p[c] = table_p[c] + groups
                        else:
                            table_p[c] = groups
        self._permission_table = table
        self._query_cache = {}

    def _permitted(self, permission, groups, column):
        ok_groups = self.permitted_groups(permission, column) + \
                    self.permitted_groups(permission, None) 
        return (None in ok_groups) or some(lambda g: g in ok_groups, groups)
    
    def permitted(self, permission, groups, column=None):
        """Vrať pravdu, právě když 'group' má přístup.

        Argumenty:

          permission -- požadované přístupové právo, jedna z konstant třídy
            'Permission' kromě 'Permission.ALL'
          groups -- sekvence jmen skupin (strings) představující množinu
            oprávnění; přístup je povolen, je-li povolen alespoň pro jednu
            z těchto skupin
          column -- jméno sloupce (string), pro který má být přístupové právo
            testováno, nebo 'None' (pak se testují implicitní práva)

        """
        key = (permission, xtuple(groups), column)
        try:
            result = self._query_cache[key]
        except KeyError:
            result = self._query_cache[key] = \
                     self._permitted(permission, groups, column)
        return result

    def permitted_groups(self, permission, column):
        """Vrať seznam skupin, které mají dané přístupové právo.
        
        Argumenty:
        
          permission -- požadované přístupové právo, jedna z konstant třídy
            'Permission' kromě 'Permission.ALL'
          column -- jméno sloupce (string), pro který má být přístupové právo
            testováno, nebo 'None' (pak se testují implicitní práva)
            
        """
        permsets = self._permission_table[permission]
        return permsets.get(column, ())


class RestrictedData(object):
    """Datový objekt s omezeným přístupem ke svým operacím."""
    
    def __init__(self, access_rights):
        """Inicializuj objekt.

        Argumenty:

          access_rights -- instance třídy 'AccessRights' určující přístupová
            práva k objektu
          
        """
        self._access_rights = access_rights

    def access_groups(self):
        """Vrať tuple skupin, ve kterých je uživatel přítomen.

        V této třídě metoda vrací prázdné tuple.

        """
        return ()

    def _check_access_columns(self, columns, permission):
        groups = self.access_groups()
        for c in columns:
            if not self._access_rights.permitted(permission, groups,
                                                 column=c):
                table = self._bindings[0].table()
                raise DataAccessException(permission, table=table, column=c)
        
    def _check_access_condition_columns(self, condition):
        # Toto původně byla lokální funkce v `_check_access_condition'.
        # Jenomže je rekurzivní, což vede k tomu, že nemůže být uvolněna (sice
        # nechápu, proč ji neuvolní garbage collector, když hlásí, že nejsou
        # žádné neuvolnitelné objekty, ale prostě je to tak) a dochází
        # k jejímu hromadění bez uvolnění pro každou novou instanci datového
        # objektu.
        if condition is None:
            return []
        elif condition.logical():
            return reduce(operator.add,
                          map(self._check_access_condition_columns,
                              condition.args()),
                          [])
        elif condition.name == 'IN':
            column, data, table_column, table_condition = condition.args()
            # Toto nefunguje pro vzdálený přístup, ale nelze svítit...
            data._check_access_columns(table_column)
            data._check_access_condition(table_condition)
            return [column]
        else:
            return [condition.args()[0]]
        
    def _check_access_condition(self, condition):
        self._check_access_columns(
            self._check_access_condition_columns(condition), Permission.VIEW)

    def _check_access_sorting(self, sorting):
        columns = []
        for s in sorting:
            if isinstance(s, str):
                columns.append(s)
            else:
                columns.append(s[0])
        self._check_access_columns(columns, Permission.VIEW)

    def _check_access_key(self):
        key_columns = map(lambda c: c.id(), self.key())
        self._check_access_columns(key_columns, Permission.VIEW)

    def _check_access_delete(self):
        self._check_access_columns([None], Permission.DELETE)

    def _access_filter_row(self, row, permission=Permission.VIEW):
        if row is None:
            return row
        groups = self.access_groups()
        rights = self._access_rights
        if rights.permitted(permission, groups):
            return row
        filtered_items = filter(lambda i: \
                                rights.permitted(permission, groups,
                                                 column=i[0]),
                                row.items())
        if not filtered_items:
            table = self._bindings[0].table()
            raise DataAccessException(permission, table=table)
        return pytis.data.Row(filtered_items)

    def accessible(self, column_id, permission):
        """Vrať pravdu, právě když má uživatel právo přístupu k danému sloupci.

        Argumenty:

          column_id -- id sloupce testovaného na přístup jako string; může být
            též 'None', v kterémžto případě je testováno globální právo
            přístupu ke sloupcům
          permission -- jedna z konstant třídy 'Permission' určující, které
            přístupové právo se má testovat

        """
        return self._access_rights.permitted(permission, self.access_groups(),
                                             column=column_id)
    
    # Předefinované metody.
    # Je nutno dbát opatrnosti u conditions a dalších argumentů, protože ty
    # umožňují získávat informace o datech nepřímo.

    def row(self, key, **kwargs):
        self._check_access_key()
        row = super(RestrictedData, self).row(key, **kwargs)
        return self._access_filter_row(row)

    def select(self, condition=None, sort=(), **kwargs):
        self._check_access_condition(condition)
        self._check_access_sorting(sort)
        return super(RestrictedData, self).select(condition=condition,
                                                  sort=sort, **kwargs)
    
    def fetchone(self, **kwargs):
        row = super(RestrictedData, self).fetchone(**kwargs)
        return self._access_filter_row(row)
        
    def search(self, condition, **kwargs):
        self._check_access_condition(condition)
        return super(RestrictedData, self).search(condition, **kwargs)
        
    def insert(self, row, **kwargs):
        row = self._access_filter_row(row, Permission.INSERT)
        resrow, result = super(RestrictedData, self).insert(row, **kwargs)
        return self._access_filter_row(resrow), result

    def update(self, key, row):
        self._check_access_key()
        row = self._access_filter_row(row, Permission.UPDATE)
        resrow, result = super(RestrictedData, self).update(key, row)
        return self._access_filter_row(resrow), result

    def update_many(self, condition, row):
        self._check_access_condition(condition)
        row = self._access_filter_row(row, Permission.UPDATE)
        return super(RestrictedData, self).update_many(condition, row)

    def delete(self, key):
        self._check_access_key()
        self._check_access_delete()
        return super(RestrictedData, self).delete(key)

    def delete_many(self, condition):
        self._check_access_condition(condition)
        self._check_access_delete()
        return super(RestrictedData, self).delete_many(condition)


class RestrictedMemData(MemData, RestrictedData):
    def __init__(self, columns, access_rights, data=()):
        MemData.__init__(self, columns, data=data)
        RestrictedData.__init__(self, access_rights)
        
    
class DataAccessException(Exception):
    """Výjimka vyvolaná při porušení přístupových práv."""

    def __init__(self, permission, table=None, column=None):
        """Inicializuj výjimku.

        Argumenty:

          permission -- chybějící právo, jedna z konstant třídy 'Permission'
          table -- jméno tabulek, ke které byl odepřen přístup, string nebo
            'None'
          column -- jméno sloupce, ke kterému byl odepřen přístup, string nebo
            'None'
        
        """
        import config
        log(EVENT, 'Pokus o neoprávněný přístup',
            (config.dbconnection.user(), permission, table, column))
        Exception.__init__(self, _("Přístup odmítnut"),
                           permission, table, column)


def is_in_groups(access_groups):
    """Vrať pravdu pokud přihlášený uživatel patří alespoň do jedné ze skupin.
    
    'access_groups' je sekvence názvů skupin, jako řetězců, nebo None, v
    kterémžto případě je vrácena vždy pravda.
    
    """
    import config
    groups = pytis.data.default_access_groups(config.dbconnection)
    if groups is None or access_groups is None\
           or some(lambda g: g in groups, xtuple(access_groups)):
        return True
    else:
        return False
