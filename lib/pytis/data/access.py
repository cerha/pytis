# -*- coding: iso-8859-2 -*-

# P��stupov� pr�va
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

"""Infrastruktura pro p��stupov� pr�va.

Je zde definov�no v�e, co je pot�eba pro zaji�t�n� p��stupov�ch pr�v k�datov�m
objekt�m.  Mno�ina typ� p��stupov�ch pr�v je definov�na t��dou 'Permission'.
Logika p��stupov�ch pr�v je implementov�na ve t��d� 'AccessRights'.  Zaji�t�n�
p��stupu k�datov�m objekt�m ��zen�ho p��stupov�mi pr�vy obstar�v� t��da
'RestrictedData'.

Pro signalizaci pokusu o�neopr�vn�n� p��stup k�dat�m se pou��v� v�hradn� t��da
'DataAccessException'.

"""

import operator

from pytis.data import *
from pytis.util import *


class Permission:
    """V��tov� t��da konstant specifikuj�c�ch povolen� p��stupov� akce.

    Ne v�echny zde definovan� konstanty maj� smysl pro v�echny datov� objekty.
    Nap��klad pro sloupec nem� smysl specifikovat 'DELETE' a pro u�ivatelsk�
    p��kaz je nejd�le�it�j�� 'CALL'.

    """
    
    VIEW = 'VIEW'
    """Pr�vo k�prohl�en� obsahu."""
    INSERT = 'INSERT'
    """Pr�vo vlo�it nov� z�znam."""
    UPDATE = 'UPDATE'
    """Pr�vo zm�nit existuj�c� data."""
    DELETE = 'DELETE'
    """Pr�vo smazat existuj�c� data."""
    CALL = 'CALL'
    """Pr�vo ke spu�t�n�."""
    EXPORT = 'EXPORT'
    """Pr�vo k exportu do CSV."""
    ALL = 'ALL'
    """V�echna pr�va k�dan�mu objektu."""

    def all_permissions(class_):
        """Vra� tuple v�ech neuniverz�ln�ch konstant t��dy."""
        return (class_.VIEW, class_.INSERT, class_.UPDATE, class_.DELETE,
                class_.CALL, class_.EXPORT)
    all_permissions = classmethod(all_permissions)


class AccessRights:
    """Specifikace p��stupov�ch pr�v."""
    
    def __init__(self, *access_rights):
        """Inicializuj instanci.

        Argumenty:

          access_rights -- ka�d� argument m� podobu tuple tuples ve tvaru
            (COLUMNS, (GROUPS, PERMISSIONS, ...), ...), kde COLUMNS je jm�no
            sloupce nebo sekvence jmen sloupc� (strings) nebo 'None', GROUPS je
            jm�no skupiny nebo sekvence jmen skupin nebo 'None'' a PERMISSIONS
            je sekvence konstant t��dy 'Permission'

        'permissions' povoluje p��stup, co nen� dovoleno, je zak�z�no.  Je-li
        na m�st� COLUMNS, resp. GROUP, 'None', jedn� se o�implicitn� hodnotu,
        platnou pokud pro dan� sloupec, resp. skupinu, nen� �e�eno jinak.
        Implicitn� pr�va se s��taj� s�pr�vy zadan�mi pro n�jak� sloupec,
        resp. skupinu, explicitn�, tj. lze je roz���it, nikoliv v�ak omezit.

        Je-li n�kter� z�COLUMNS nebo GROUP v r�mci COLUMNS uvedeno v�cekr�t,
        odpov�daj�c� pr�va se s��taj�.

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
        """Vra� pravdu, pr�v� kdy� 'group' m� p��stup.

        Argumenty:

          permission -- po�adovan� p��stupov� pr�vo, jedna z�konstant t��dy
            'Permission' krom� 'Permission.ALL'
          groups -- sekvence jmen skupin (strings) p�edstavuj�c� mno�inu
            opr�vn�n�; p��stup je povolen, je-li povolen alespo� pro jednu
            z�t�chto skupin
          column -- jm�no sloupce (string), pro kter� m� b�t p��stupov� pr�vo
            testov�no, nebo 'None' (pak se testuj� implicitn� pr�va)

        """
        key = (permission, xtuple(groups), column)
        try:
            result = self._query_cache[key]
        except KeyError:
            result = self._query_cache[key] = \
                     self._permitted(permission, groups, column)
        return result

    def permitted_groups(self, permission, column):
        """Vra� seznam skupin, kter� maj� dan� p��stupov� pr�vo.
        
        Argumenty:
        
          permission -- po�adovan� p��stupov� pr�vo, jedna z�konstant t��dy
            'Permission' krom� 'Permission.ALL'
          column -- jm�no sloupce (string), pro kter� m� b�t p��stupov� pr�vo
            testov�no, nebo 'None' (pak se testuj� implicitn� pr�va)
            
        """
        permsets = self._permission_table[permission]
        return permsets.get(column, ())


class RestrictedData(object):
    """Datov� objekt s�omezen�m p��stupem ke sv�m operac�m."""
    
    def __init__(self, access_rights):
        """Inicializuj objekt.

        Argumenty:

          access_rights -- instance t��dy 'AccessRights' ur�uj�c� p��stupov�
            pr�va k�objektu
          
        """
        self._access_rights = access_rights

    def access_groups(self):
        """Vra� tuple skupin, ve kter�ch je u�ivatel p��tomen.

        V�t�to t��d� metoda vrac� pr�zdn� tuple.

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
        # Toto p�vodn� byla lok�ln� funkce v�`_check_access_condition'.
        # Jenom�e je rekurzivn�, co� vede k�tomu, �e nem��e b�t uvoln�na (sice
        # nech�pu, pro� ji neuvoln� garbage collector, kdy� hl�s�, �e nejsou
        # ��dn� neuvolniteln� objekty, ale prost� je to tak) a doch�z�
        # k�jej�mu hromad�n� bez uvoln�n� pro ka�dou novou instanci datov�ho
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
            # Toto nefunguje pro vzd�len� p��stup, ale nelze sv�tit...
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
        """Vra� pravdu, pr�v� kdy� m� u�ivatel pr�vo p��stupu k�dan�mu sloupci.

        Argumenty:

          column_id -- id sloupce testovan�ho na p��stup jako string; m��e b�t
            t� 'None', v�kter�m�to p��pad� je testov�no glob�ln� pr�vo
            p��stupu ke sloupc�m
          permission -- jedna z�konstant t��dy 'Permission' ur�uj�c�, kter�
            p��stupov� pr�vo se m� testovat

        """
        return self._access_rights.permitted(permission, self.access_groups(),
                                             column=column_id)
    
    # P�edefinovan� metody.
    # Je nutno db�t opatrnosti u�conditions a dal��ch argument�, proto�e ty
    # umo��uj� z�sk�vat informace o�datech nep��mo.

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
    """V�jimka vyvolan� p�i poru�en� p��stupov�ch pr�v."""

    def __init__(self, permission, table=None, column=None):
        """Inicializuj v�jimku.

        Argumenty:

          permission -- chyb�j�c� pr�vo, jedna z�konstant t��dy 'Permission'
          table -- jm�no tabulek, ke kter� byl odep�en p��stup, string nebo
            'None'
          column -- jm�no sloupce, ke kter�mu byl odep�en p��stup, string nebo
            'None'
        
        """
        import config
        log(EVENT, 'Pokus o�neopr�vn�n� p��stup',
            (config.dbconnection.user(), permission, table, column))
        Exception.__init__(self, _("P��stup odm�tnut"),
                           permission, table, column)


def is_in_groups(access_groups):
    """Vra� pravdu pokud p�ihl�en� u�ivatel pat�� alespo� do jedn� ze skupin.
    
    'access_groups' je sekvence n�zv� skupin, jako �et�zc�, nebo None, v
    kter�m�to p��pad� je vr�cena v�dy pravda.
    
    """
    import config
    groups = pytis.data.default_access_groups(config.dbconnection)
    if groups is None or access_groups is None\
           or some(lambda g: g in groups, xtuple(access_groups)):
        return True
    else:
        return False
