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

"""Pomůcky pro operace s datovými objekty a daty obecně.""" 

from pytis.extensions import *

import pytis.form
import pytis.data
import config

def data_object(spec):
    """Vrať sestavený datový objekt na základě názvu specifikace.

    Argumentem je název specifikace datového objektu nebo přímo instance třídy
    'pytis.data.DataFactory'
    
    """
    if isinstance(spec, types.StringType):
        resolver = pytis.form.resolver()
        spec = resolver.get(spec, 'data_spec')
    op = lambda: spec.create(dbconnection_spec=config.dbconnection)
    success, data = pytis.form.db_operation(op)
    #if not success:
    #    raise "Nepodařilo se vytvořit datový objekt pro %s!" % (spec)
    return data

# Alias
data_create = data_object


def dbselect(spec, condition=None, sort=()):
    """Vrať řádky dané db tabulky jako sekvenci.

    Argumenty:

      spec -- název specifikace datového objektu nebo přímo instance třídy
        'pytis.data.DataFactory'
      condition, sort -- argumenty volání 'pytis.data.select()'.
        
    Vrací všechny řádky vrácené z databáze jako list.
    
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
    """Provede insert do tabulky dané specifikací.

    Argumenty:

      spec -- název specifikace datového objektu nad kterým má být proveden
        insert.
      row -- sekvence dvouprvkových sekvencí (id, value) nebo instance
        pytis.data.Row
        
    Vrací počet vložených řádků.
    
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
    """Provede update nad předaným řádkem.

    Argumenty:

      row -- předaná instance aktuálního PresentedRow
      values -- sekvence dvouprvkových sekvencí ('id', value) ,
        kde 'id' je řetězcový identifikátor políčka a value je
        instance, kterou se bude políčko aktualizovat
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
    """Provede update nad tabulkou danou specifikací.

    Argumenty:

      spec -- specifikace datového objektu nad kterým má být proveden
        select; string'
      condition -- podmínka updatovaní.
      update_row -- řádek kterým se provede update, 
        
    Vrací počet updatovaných řádků.
    
    """
    if not isinstance(condition, pytis.data.Operator):
        raise "Nebyla předána podmínka pro update_many."
    if not isinstance(update_row, pytis.data.Row):
        raise "Nebyl předán řádek pro update_many."
    data = data_object(spec)
    return data.update_many(condition, update_row) 


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

    """
    def proceed_with_empty_values(proceed_with_empty_values=False):
        return proceed_with_empty_values
    if not proceed_with_empty_values(**kwargs):
        for id, v in args:
            value = v.value()
            if value is None or value == '':
                return None
    op = lambda: pytis.data.DBFunctionDefault(name, config.dbconnection)
    success, function = pytis.form.db_operation(op)
    op = lambda: function.call(pytis.data.Row(args))[0][0]
    success, result = pytis.form.db_operation(op)
    return result.value()



# Pozor, stejná metoda metoda je definována i v pytis.data.access
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

