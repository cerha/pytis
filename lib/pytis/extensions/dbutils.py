# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2007, 2010, 2011 Brailcom, o.p.s.
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

import config

def data_object(spec):
    """Vra� sestaven� datov� objekt na z�klad� n�zvu specifikace.

    Argumentem je n�zev specifikace datov�ho objektu nebo p��mo instance t��dy
    'pytis.data.DataFactory'
    
    """
    if isinstance(spec, types.StringTypes):
        spec = pytis.util.resolver().get(spec, 'data_spec')
    def conn_spec():
        return config.dbconnection
    success, data = pytis.form.db_operation(spec.create, dbconnection_spec=conn_spec)
    #if not success:
    #    errmsg = "Nepoda�ilo se vytvo�it datov� objekt pro %s!" % (spec)
    #    raise ProgramError(errmsg)
    return data

# Alias
data_create = data_object


def dbselect(spec, condition=None, sort=(), transaction=None):
    """Vra� ��dky dan� db tabulky jako sekvenci.

    Argumenty:

      spec -- n�zev specifikace datov�ho objektu nebo p��mo instance t��dy
        'pytis.data.DataFactory'
      condition, sort, transaction -- argumenty vol�n�
        'pytis.data.postgresql.select()'.
        
    Vrac� v�echny ��dky vr�cen� z datab�ze jako list.
    
    """
    data = data_object(spec)
    data.select(condition=condition, sort=sort, transaction=transaction)
    result = []
    while True:
        row = data.fetchone()
        if row is None:
            data.close()
            break
        result.append(row)
    return result


def dbinsert(spec, row, transaction=None):
    """Provede insert do tabulky dan� specifikac�.

    Argumenty:

      spec -- n�zev specifikace datov�ho objektu nad kter�m m� b�t proveden
        insert.
      row -- sekvence dvouprvkov�ch sekvenc� (id, value) nebo instance
        pytis.data.Row
      transaction -- instance pytis.data.DBTransactionDefault  
        
    Vrac� po�et vlo�en�ch ��dk�.
    
    """
    assert isinstance(row, pytis.data.Row) or is_sequence(row), \
           _("Argument must be a sequence or Row instance.", row)
    if is_sequence(row):
        for item in row:
            if not is_sequence(item) or len(item) != 2:                
                errmsg = 'Column definition must be (ID, VALUE) pair.'
                raise ProgramError(errmsg)
            k, v = item
            if not is_string(k):
                errmsg =  'Invalid column id %s' % k
                raise ProgramError(errmsg)
            if not isinstance(v, pytis.data.Value):
                errmsg = 'Invalid column value %s' % v
                raise ProgramError(errmsg)
        row = pytis.data.Row(row)
    data = data_object(spec)
    success, result = pytis.form.db_operation(data.insert, row, transaction=transaction)
    return result


def dbupdate(row, values=(), transaction=None):
    """Provede update nad p�edan�m ��dkem.

    Argumenty:

      row -- p�edan� instance aktu�ln�ho PresentedRow
      values -- sekvence dvouprvkov�ch sekvenc� ('id', value) ,
        kde 'id' je �et�zcov� identifik�tor pol��ka a value je
        instance, kterou se bude pol��ko aktualizovat
      transaction -- instance pytis.data.DBTransactionDefault  
    """
    data = row.data()
    updaterow = row.row()
    key = data.key()
    if is_sequence(key):
        key = key[0]
    for col, val in values:
        updaterow[col] = val
    return pytis.form.db_operation(data.update, row[key.id()], updaterow, transaction=transaction)

# Alias
row_update = dbupdate


def dbupdate_many(spec, condition=None, update_row=None,
                  transaction=None):
    """Provede update nad tabulkou danou specifikac�.

    Argumenty:

      spec -- specifikace datov�ho objektu nad kter�m m� b�t proveden
        select; string'
      condition -- podm�nka updatovan�.
      update_row -- ��dek kter�m se provede update,
      transaction -- instance pytis.data.DBTransactionDefault        
        
    Vrac� po�et updatovan�ch ��dk�.
    
    """
    if not isinstance(condition, pytis.data.Operator):
        errmsg = "Nebyla p�ed�na podm�nka pro update_many."
        raise ProgramError(errmsg)        
    if not isinstance(update_row, pytis.data.Row):
        errmsg = "Nebyl p�ed�n ��dek pro update_many."
        raise ProgramError(errmsg)
    data = data_object(spec)
    return data.update_many(condition, update_row, transaction=transaction) 


def dbfunction(name, *args, **kwargs):
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
      transaction -- instance pytis.data.DBTransactionDefault        
    """
    proceed_with_empty_values = kwargs.get('proceed_with_empty_values', False)
    transaction = kwargs.get('transaction')
    if not proceed_with_empty_values:
        for id, v in args:
            value = v.value()
            if value is None or value == '':
                return None
    def conn_spec():
        return config.dbconnection
    success, function = pytis.form.db_operation(pytis.data.DBFunctionDefault, name, conn_spec)
    success, result   = pytis.form.db_operation(function.call, pytis.data.Row(args),
                                                transaction=transaction)
    if len(result) == 1 and len(result[0]) == 1:
        return result[0][0].value()
    return result


def nextval(seq):
    """Vra� funkci pro v�po�et v�choz� hodnoty sloupce z dan� sekvence.

    Argumentem je n�zev sekvence v datab�zi.  Vhodn� pro zjednodu�en�
    specifikace 'default' ve fieldspec.
    
    """
    def conn_spec():
        return config.dbconnection
    counter = pytis.data.DBCounterDefault(seq, conn_spec)
    return lambda transaction=None: counter.next(transaction=transaction)


def enum(name, **kwargs):
    """Vytvo� instanci 'DataEnumerator' nad danou specifikac�.

    Takto vytvo�en� enumer�tor lze pou��t jako argument 'enumerator'
    konstruktoru datov�ho typu.  Argument 'name' je �et�zec ur�uj�c� n�zev
    specifikace, ze kter� bude z�sk�n datov� objekt enumer�toru.
    
    """
    data_spec = pytis.util.resolver().get(name, 'data_spec')
    return pytis.data.DataEnumerator(data_spec, **kwargs)


# Pozor, stejn� metoda metoda je definov�na i v pytis.data.access
def is_in_groups(groups):
    if isinstance(groups, types.StringType):
        groups = xtuple(groups)
    from sets import Set
    def conn_spec():
        return config.dbconnection
    dbgroups = pytis.data.default_access_groups(conn_spec)
    if Set(groups) & Set(dbgroups) == Set([]):
        return False
    else:
        return True

