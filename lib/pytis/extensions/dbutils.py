# -*- coding: utf-8 -*-

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

"""Pomůcky pro operace s datovými objekty a daty obecně.""" 

from pytis.extensions import *

import config

def data_object(spec):
    """Vrať sestavený datový objekt na základě názvu specifikace.

    Argumentem je název specifikace datového objektu nebo přímo instance třídy
    'pytis.data.DataFactory'
    
    """
    if isinstance(spec, types.StringTypes):
        spec = pytis.util.resolver().get(spec, 'data_spec')
    def conn_spec():
        return config.dbconnection
    success, data = pytis.form.db_operation(spec.create, dbconnection_spec=conn_spec)
    #if not success:
    #    errmsg = "Nepodařilo se vytvořit datový objekt pro %s!" % (spec)
    #    raise ProgramError(errmsg)
    return data

# Alias
data_create = data_object


def dbselect(spec, condition=None, sort=(), transaction=None):
    """Vrať řádky dané db tabulky jako sekvenci.

    Argumenty:

      spec -- název specifikace datového objektu nebo přímo instance třídy
        'pytis.data.DataFactory'
      condition, sort, transaction -- argumenty volání
        'pytis.data.postgresql.select()'.
        
    Vrací všechny řádky vrácené z databáze jako list.
    
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
    """Provede insert do tabulky dané specifikací.

    Argumenty:

      spec -- název specifikace datového objektu nad kterým má být proveden
        insert.
      row -- sekvence dvouprvkových sekvencí (id, value) nebo instance
        pytis.data.Row
      transaction -- instance pytis.data.DBTransactionDefault  
        
    Vrací počet vložených řádků.
    
    """
    assert isinstance(row, pytis.data.Row) or is_sequence(row), \
           _(u"Argument must be a sequence or Row instance.", row)
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
    """Provede update nad předaným řádkem.

    Argumenty:

      row -- předaná instance aktuálního PresentedRow
      values -- sekvence dvouprvkových sekvencí ('id', value) ,
        kde 'id' je řetězcový identifikátor políčka a value je
        instance, kterou se bude políčko aktualizovat
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
    """Provede update nad tabulkou danou specifikací.

    Argumenty:

      spec -- specifikace datového objektu nad kterým má být proveden
        select; string'
      condition -- podmínka updatovaní.
      update_row -- řádek kterým se provede update,
      transaction -- instance pytis.data.DBTransactionDefault        
        
    Vrací počet updatovaných řádků.
    
    """
    if not isinstance(condition, pytis.data.Operator):
        errmsg = "Nebyla předána podmínka pro update_many."
        raise ProgramError(errmsg)        
    if not isinstance(update_row, pytis.data.Row):
        errmsg = "Nebyl předán řádek pro update_many."
        raise ProgramError(errmsg)
    data = data_object(spec)
    return data.update_many(condition, update_row, transaction=transaction) 


def dbfunction(name, *args, **kwargs):
    """Zavolej databázovou funkci a vrať výsledek jako Pythonovou hodnotu.

    Argumenty:

      name -- název funkce.
      args -- argumenty volání funkce; sekvence dvouprvkových tuplů, kde první
        prvek je název argumentu a druhý jeho hodnota jako instance 'Value'.
      proceed_with_empty_values -- pokud je pravdivé, volá databázovou funkci
        vždy.  V opačném případě (vývchozí chování) testuje, zda všechny
        argumenty obsahují neprázdnou hodnotu (jejich vnitří hodnota není None
        ani prázdný řetězec) a pokud test neprojde, vrátí None bez volání
        databázové funkce.  To znamená úsporu pokud je tato funkce použita v
        computeru políčka, které je závislé na jiných políčkách, která ještě
        nejsou vyplněna.
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
    """Vrať funkci pro výpočet výchozí hodnoty sloupce z dané sekvence.

    Argumentem je název sekvence v databázi.  Vhodné pro zjednodušení
    specifikace 'default' ve fieldspec.
    
    """
    def conn_spec():
        return config.dbconnection
    counter = pytis.data.DBCounterDefault(seq, conn_spec)
    return lambda transaction=None: counter.next(transaction=transaction)


def enum(name, **kwargs):
    """Vytvoř instanci 'DataEnumerator' nad danou specifikací.

    Takto vytvořený enumerátor lze použít jako argument 'enumerator'
    konstruktoru datového typu.  Argument 'name' je řetězec určující název
    specifikace, ze které bude získán datový objekt enumerátoru.
    
    """
    data_spec = pytis.util.resolver().get(name, 'data_spec')
    return pytis.data.DataEnumerator(data_spec, **kwargs)


# Pozor, stejná metoda metoda je definována i v pytis.data.access
def is_in_groups(groups):
    if isinstance(groups, basestring):
        groups = xtuple(groups)
    def conn_spec():
        return config.dbconnection
    dbgroups = pytis.data.default_access_groups(conn_spec)
    if set(groups) & set(dbgroups) == set([]):
        return False
    else:
        return True

