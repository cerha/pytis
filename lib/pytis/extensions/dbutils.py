# -*- coding: utf-8 -*-

# Copyright (C) 2018-2022 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2017 OUI Technology Ltd.
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

from past.builtins import basestring

import pytis
import pytis.util
import pytis.data as pd
from pytis.api import app
from pytis.util import ProgramError, translations, xtuple

_ = translations('pytis-wx')


def dbselect(spec, condition=None, sort=(), transaction=None, arguments={}):
    """Vrať řádky dané db tabulky jako sekvenci.

    Argumenty:

      spec -- název specifikace datového objektu nebo přímo instance třídy
        'pd.DataFactory'
      condition, sort, transaction -- argumenty volání
        'pd.postgresql.select()'.

    Vrací všechny řádky vrácené z databáze jako list.

    """
    data = pytis.util.data_object(spec)
    data.select(condition=condition, sort=sort, transaction=transaction,
                arguments=arguments)
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
        pd.Row
      transaction -- instance pd.DBTransactionDefault

    Vrací počet vložených řádků.

    """
    assert isinstance(row, (pd.Row, tuple, list)), row
    import pytis.form
    if isinstance(row, (tuple, list)):
        for item in row:
            if not isinstance(item, (tuple, list)) or len(item) != 2:
                errmsg = 'Column definition must be (ID, VALUE) pair.'
                raise ProgramError(errmsg)
            k, v = item
            if not isinstance(k, basestring):
                errmsg = 'Invalid column id %s' % k
                raise ProgramError(errmsg)
            if not isinstance(v, pd.Value):
                errmsg = 'Invalid column value %s' % v
                raise ProgramError(errmsg)
        row = pd.Row(row)
    data = pytis.util.data_object(spec)
    success, result = pytis.form.db_operation(data.insert, row, transaction=transaction)
    return result


def dbupdate(row, values=(), transaction=None):
    """Provede update nad předaným řádkem.

    Argumenty:

      row -- předaná instance aktuálního PresentedRow
      values -- sekvence dvouprvkových sekvencí ('id', value) ,
        kde 'id' je řetězcový identifikátor políčka a value je
        instance, kterou se bude políčko aktualizovat
      transaction -- instance pd.DBTransactionDefault

    """
    import pytis.form
    data = row.data()
    updaterow = row.row()
    key = data.key()
    if isinstance(key, (tuple, list)):
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
      transaction -- instance pd.DBTransactionDefault

    Vrací počet updatovaných řádků.

    """
    if not isinstance(condition, pd.Operator):
        errmsg = "Nebyla předána podmínka pro update_many."
        raise ProgramError(errmsg)
    if not isinstance(update_row, pd.Row):
        errmsg = "Nebyl předán řádek pro update_many."
        raise ProgramError(errmsg)
    data = pytis.util.data_object(spec)
    return data.update_many(condition, update_row, transaction=transaction)


def dbfunction(name, *args, **kwargs):
    """Deprecated.  Use pytis.data.dbfunction instead.

    Nová funkce pytis.data.dbfunction má oproti této původní funkci několik
    odlišností, které je potřeba zohlednit při přepisu starého kódu na použití
    nové funkce.

    1. Tato (původní) funkce vrací None pokud je hodnota některého z argumentů
    None aniž by došlo ke skutečnému vyvolání DB funkce (čemuž lze zabránit
    argumentem proceed_with_empty_values=False).  Nová funkce toto nedělá a
    argument proceed_with_empty_values nemá.

    2. Argumenty nové funkce se předávají vnitřní hodnotou a nikoliv jako
    sekvence dvouprvkových tuplů, ale přímo jako argumenty funkce.

    3. Funkci je možné předat jako DB specifikaci.  V tom případě je prováděna
    typová kontrola argumentů a výsledek může být jak množina řádků (tabulkové
    funkce), tak jednoduchá hodnota dle atributu multirow ve specifikaci.

    4. pd.dbfunction nepoužívá obalení pomocí pf.db_operation(), takže to je
    potřeba případně přidat explicitně, kde je to potřeba (většinou by to
    potřeba být nemělo.).

    """
    # TODO PY3: define keyword arguments in function definition.
    import pytis.form
    proceed_with_empty_values = kwargs.pop('proceed_with_empty_values', False)
    transaction = kwargs.pop('transaction', None)
    assert not kwargs
    if not proceed_with_empty_values and args and any(v.value() in (None, '') for k, v in args):
        return None

    def conn_spec():
        return pytis.config.dbconnection
    success, function = pytis.form.db_operation(pd.DBFunctionDefault, name, conn_spec)
    success, result = pytis.form.db_operation(function.call, pd.Row(args),
                                              transaction=transaction)
    if not success:
        return None
    if len(result) == 1 and len(result[0]) == 1:
        return result[0][0].value()
    return result


def enum(name, **kwargs):
    """Vytvoř instanci 'DataEnumerator' nad danou specifikací.

    Takto vytvořený enumerátor lze použít jako argument 'enumerator'
    konstruktoru datového typu.  Argument 'name' je řetězec určující název
    specifikace, ze které bude získán datový objekt enumerátoru.

    """
    data_spec = pytis.config.resolver.get(name, 'data_spec')
    return pd.DataEnumerator(data_spec, **kwargs)


# Pozor, stejná metoda metoda je definována i v pd.access
def is_in_groups(groups):
    if isinstance(groups, basestring):
        groups = xtuple(groups)

    def conn_spec():
        return pytis.config.dbconnection
    dbgroups = pd.default_access_groups(conn_spec)
    if set(groups) & set(dbgroups) == set([]):
        return False
    else:
        return True


def safe_commit(transaction, msg=None):
    """Commit transaction and handle possible timeout errors.

    Arguments:
      transaction -- transaction to commit
      msg -- message to show, when database error occurs

    Returns True if commit was successful, otherwise returns False.

    """
    import pytis.form
    DEFAULT_MSG = _("The database connection was closed because of long inactivity.")
    try:
        transaction.commit()
        return True
    except pd.DBSystemException:
        app.error(msg or DEFAULT_MSG)
        return False


def safe_rollback(transaction, msg=None):
    """Rollback transaction and handle possible timeout errors.

    Arguments:
      transaction -- transaction to commit
      msg -- message to show, when database error occurs

    Returns True if rollback was successful, otherwise return False.
    """
    import pytis.form
    DEFAULT_MSG = _("The database connection was closed because of long inactivity.")
    try:
        transaction.rollback()
        return True
    except pd.DBSystemException:
        app.error(msg or DEFAULT_MSG)
        return False
