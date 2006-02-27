# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006 Brailcom, o.p.s.
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

"""Pom�cky pro operace s datov�mi objekty a daty obecn�.""" 

from pytis.extensions import *

import pytis.form
import pytis.data
import config

def data_object(spec):
    """Vra� sestaven� datov� objekt na z�klad� n�zvu specifikace.

    Argumentem je n�zev specifikace datov�ho objektu nebo p��mo instance t��dy
    'pytis.data.DataFactory'
    
    """
    if isinstance(spec, types.StringType):
        resolver = pytis.form.resolver()
        spec = resolver.get(spec, 'data_spec')
    op = lambda: spec.create(dbconnection_spec=config.dbconnection)
    success, data = pytis.form.db_operation(op)
    #if not success:
    #    raise "Nepoda�ilo se vytvo�it datov� objekt pro %s!" % (spec)
    return data

# Alias
data_create = data_object


def dbselect(spec, condition=None, sort=()):
    """Vra� ��dky dan� db tabulky jako sekvenci.

    Argumenty:

      spec -- n�zev specifikace datov�ho objektu nebo p��mo instance t��dy
        'pytis.data.DataFactory'
      condition, sort -- argumenty vol�n� 'pytis.data.select()'.
        
    Vrac� v�echny ��dky vr�cen� z datab�ze jako list.
    
    """
    data = data_object(spec)
    data.select(condition=condition, sort=sort)
    result = []
    while True:
        row = data.fetchone()
        if row is None:
            data.close()
            break
        result.append(row)
    return result


def dbinsert(spec, row):
    """Provede update nad tabulkou danou specifikac�.

    Argumenty:

      spec -- n�zev specifikace datov�ho objektu nad kter�m m� b�t proveden
        insert.
      row -- sekvence dvouprvkov�ch sekvenc� (id, value) nebo instance
        pytis.data.Row
        
    Vrac� po�et updatovan�ch ��dk�.
    
    """
    assert isinstance(row, pytis.data.Row) or is_sequence(row), \
           _("Argument must be a sequence or Row instance.")
    if is_sequence(row):
        for item in row:
            if not is_sequence(item) or len(item) != 2:
                raise 'Column definition must be (ID, VALUE) pair.'
            k, v = item
            if not is_string(k):
                raise 'Invalid column id %s' % k
            if not isinstance(v, pytis.data.Value):
                raise 'Invalid column value %s' % v
        row = pytis.data.Row(row)
    data = data_object(spec)
    op = lambda: data.insert(row)
    success, result = pytis.form.db_operation(op)
    return result


def dbupdate(row, values=()):
    """Provede update nad p�edan�m ��dkem.

    Argumenty:

      row -- p�edan� instance aktu�ln�ho PresentedRow
      values -- sekvence dvouprvkov�ch sekvenc� ('id', value) ,
        kde 'id' je �et�zcov� identifik�tor pol��ka a value je
        instance, kterou se bude pol��ko aktualizovat
    """
    data = row.data()
    updaterow = row.row()
    key = data.key()
    if is_sequence(key):
        key = key[0]
    for col, val in values:
        updaterow[col] = val
    op = lambda: data.update(row[key.id()], updaterow)
    return pytis.form.db_operation(op)

# Alias
row_update = dbupdate


def dbupdate_many(spec, condition=None, update_row=None):
    """Provede update nad tabulkou danou specifikac�.

    Argumenty:

      spec -- specifikace datov�ho objektu nad kter�m m� b�t proveden
        select; string'
      condition -- podm�nka updatovan�.
      update_row -- ��dek kter�m se provede update, 
        
    Vrac� po�et updatovan�ch ��dk�.
    
    """
    if not isinstance(condition, pytis.data.Operator):
        raise "Nebyla p�ed�na podm�nka pro update_many."
    if not isinstance(update_row, pytis.data.Row):
        raise "Nebyl p�ed�n ��dek pro update_many."
    data = data_object(spec)
    return data.update_many(condition, update_row) 


def dbfunction(name, *args, proceed_with_empty_values=False):
    """Zavolej datab�zovou funkci a vra� v�sledek jako Pythonovou hodnotu.

    Argumenty:

      name -- n�zev funkce.
      args -- argumenty vol�n� funkce; sekvence dvouprvkov�ch tupl�, kde prvn�
        prvek je n�zev argumentu a druh� jeho hodnota jako instance 'Value'.
      proceed_with_empty_values -- pokud je pravdiv�, vol� datab�zovou funkci
        v�dy.  V opa�n�m p��pad� (v�vchoz� chov�n�) testuje, zda v�echny
        argumenty obsahuj� nepr�zdnou hodnotu (jejich vnit�� hodnota nen� None
        ani pr�zdn� �et�zec) a pokud test neprojde, vr�t� None bez vol�n�
        datab�zov� funkce.  To znamen� �sporu pokud je tato funkce pou�ita v
        computeru pol��ka, kter� je z�visl� na jin�ch pol��k�ch, kter� je�t�
        nejsou vypln�na.

    """
    if not proceed_with_empty_values:
        for id, v in args:
            value = v.value()
            if value is None or value == '':
                return None
    op = lambda: pytis.data.DBFunctionDefault(name, config.dbconnection)
    success, function = pytis.form.db_operation(op)
    op = lambda: function.call(pytis.data.Row(args))[0][0]
    success, result = pytis.form.db_operation(op)
    return result.value()



# Pozor, stejn� metoda metoda je definov�na i v pytis.data.access
def is_in_groups(groups):
    if isinstance(groups, types.StringType):
        groups = xtuple(groups)
    from sets import Set
    conn = config.dbconnection
    dbgroups=pytis.data.PostgreSQLUserGroups.class_access_groups(conn)
    if Set(groups) & Set(dbgroups) == Set([]):
        return False
    else:
        return True

