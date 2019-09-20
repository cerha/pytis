# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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

"""Třídy pro zpřístupnění tabulkových dat z různých zdrojů.

Základem přístupu k datům je třída 'Data' definující obecné rozhraní přístupu
k tabulkovým datům.  Předpokládá se, že právě s touto třídou budou schopny
pracovat ostatní moduly programu, které potřebují získávat nebo předávat větší
množství tabulkových dat, zejména z objektů jako jsou databáze.  Specializované
podtřídy umožňují přístup k různým zdrojům dat, viz například třída
'dbdata.DBData'.

Kromě třídy 'Data' modul definuje ještě pomocnou třídu 'Row' reprezentující
jeden řádek dat a pomocnou třídu 'ColumnSpec' používanou pro specifikaci
sloupců instancí třídy 'Data'.

K dispozici jsou také abstraktní třídy pro pokročilejší databázové funkce, viz
například třídy 'Counter' a 'Function'.  Tyto třídy je nutno chápat jako
volitelné, konkrétní databázový backend nemusí nabízet žádnou jejich konkrétní
implementaci.

"""
from past.builtins import basestring, long
from builtins import range
from future.utils import python_2_unicode_compatible

import copy
import datetime
import functools
import operator

import pytis.util
import pytis.data
from pytis.util import (
    EVENT, DEBUG, InvalidAccessError, LimitedCache, NotImplementedException, ProgramError,
    find, log, object_2_5, sameclass, translations, xtuple,
)
from .types_ import DateTime, Number, Type, Value, WMValue

_ = translations('pytis-data')

unistr = type(u'')  # Python 2/3 transition hack.

FORWARD = 'FORWARD'
"""Konstanta pro dopředný posun v 'Data.fetch'."""
BACKWARD = 'BACKWARD'
"""Konstanta pro zpětný posun v 'Data.fetch'."""


def opposite_direction(direction):
    """Vrať směr opačný k 'direction'.

    'direction' i vrácený výsledek jsou některé z konstant 'FORWARD' a
    'BACKWARD'.

    """
    if direction == FORWARD:
        return BACKWARD
    elif direction == BACKWARD:
        return FORWARD
    else:
        raise ProgramError('Invalid direction', direction)


ASCENDENT = 'ASCENDENT'
"""Konstanta pro vzestupné třídění."""
DESCENDANT = 'DESCENDANT'
"""Konstanta pro sestupné třídění."""


# Data classes


@python_2_unicode_compatible
class Operator(object):
    """Conditional operator for the 'select()' method.

    The instances of this class define a data filtering condition.  An operator
    is given by its name, operands (constructor arguments) and parameters
    (keyword arguments).  See the functions 'AND', 'OR', 'NOT', 'EQ', 'NE',
    'LE', 'GT', 'WM' and others for convenient constructors of some well known
    operators.

    The instances of this class are immutable and hashable.

    When the operators are compared or hashed, the data type of all operands
    (arguments passed to the constructor) which are 'Value' instances are
    ignored.  Thus two operators are considered equal if just the inner python
    values of their operands match.  This is because they represent the same
    condition even if the data types of the operands differ in their parameters
    (for example one is String(not_null=True) and the other is
    String(not_null=False)).

    """
    class ComparableDateTime(object):
        """Wrapper for datetime values to enable their comparison to None."""

        def __init__(self, value):
            self._value = value

        def __eq__(self, other):
            if sameclass(self, other):
                return self._value == other._value
            else:
                return NotImplemented

        def __ne__(self, other):
            # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
            return not self == other

        def __lt__(self, other):
            if sameclass(self, other):
                return self._value < other._value
            elif other is None:
                return False
            else:
                return NotImplemented

        def __le__(self, other):
            return self < other or self == other

        def __gt__(self, other):
            return not self == other and not self < other

        def __ge__(self, other):
            return self > other or self == other

        def __hash__(self):
            return hash(self._value)

    def __init__(self, name, *args, **kwargs):
        """Vytvoř operátor 'name' s argumenty 'args' a 'kwargs'.

        Argumenty:

          name -- jméno operátoru jako řetězec
          args -- argumenty operátoru
          translation -- překlad operátoru na primitivní operátory, instance
            'Operator'
          kwargs -- pojmenované (klíčové) argumenty operátoru

        """
        self._name = name
        self._translation = kwargs.pop('translation', None)
        self._args = args
        self._kwargs = kwargs

    def __str__(self):
        def arg(arg):
            if isinstance(arg, (Value, WMValue)):
                value = arg.value()
                if isinstance(arg.type(), DateTime):
                    value = str(value)
                return repr(value)
            else:
                return str(arg)
        args = [arg(a) for a in self.args()]
        return '%s (%s)' % (self.name(), ', '.join(args))

    def __eq__(self, other):
        if sameclass(self, other):
            return (self._name == other._name and
                    self._relaxed_args() == other._relaxed_args() and
                    self._kwargs == other._kwargs)
        else:
            return NotImplemented

    def __ne__(self, other):
        # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
        return not self == other

    def __hash__(self):
        return (hash(self._name) ^
                hash(tuple(self._relaxed_args())) ^
                hash(tuple(self._kwargs.items())))

    def _relaxed_args(self):
        """Return self._args, with Value instances transformed to ignore their type attributes.

        So the attributes of data type instances don't figure in the result.
        Only their class is taken into account.  This is necessary to avoid the
        influence of data types attributes on the results of __eq__ and
        __hash__.

        """
        def relax(arg):
            if isinstance(arg, (Value, WMValue)):
                cls, value = arg.type().__class__, arg.value()
                if isinstance(value, (datetime.datetime, datetime.date, datetime.time)):
                    # datetime values can not be compared to None, so we use
                    # this wrapper class for simplicity.
                    value = self.ComparableDateTime(value)
                return (cls, value)
            elif isinstance(arg, dict):
                return tuple((k, relax(v)) for k, v in arg.items())
            else:
                return arg
        return [relax(arg) for arg in self._args]

    def name(self):
        """Vrať jméno operátoru zadané v konstruktoru."""
        return self._name

    def translation(self):
        """Vrať překlad operátoru zadaný v konstruktoru."""
        return self._translation

    def args(self):
        """Vrať tuple argumentů operátoru zadaných v konstruktoru."""
        return self._args

    def kwargs(self):
        """Vrať dictionary klíčových argumentů zadaných v konstruktoru."""
        return self._kwargs

    def logical(self):
        """Vrať pravdu, právě když se jedná o logický operátor.

        Logické operátory jsou 'AND', 'OR' a 'NOT'.  Ostatní operátory jsou
        relační.

        """
        return self._name in ('AND', 'OR', 'NOT')


class Data(object_2_5):
    """Základní abstrakce přístupu k tabulkovým datům.

    Tabulková data jsou rozdělena do sloupců a řádků.  Třída umožňuje pracovat
    s řádky dat způsobem analogickým práci s daty nad relační databází.
    Specifikace sloupců je fixní vzhledem k dané instanci třídy.  Jeden nebo
    více sloupců třídy jsou definovány jako klíčové, hodnoty těchto sloupců
    v jednotlivých řádcích tyto řádky jednoznačně identifikují.  Obsah řádků je
    dynamický, získávaný ze zdroje dat, počet řádků se může během existence
    instance třídy měnit.

    Třída definuje následující skupiny metod pro přístup k datům:

    - Metodu pro výběr konkrétního řádku: 'row()'.  Řádek je vybrán podle
      zadaného klíče.

    - Metody pro výběr všech řádků: 'select()', 'fetch()', 'fetchone()',
      'skip()' a 'close()'.  Po zavolání metody 'select()' je možno postupně
      získávat jednotlivé řádky pomocí metod 'fetch()', resp. 'fetchone()'.
      Tento způsob umožňuje předávání i většího množství dat z externích zdrojů
      bez nutnosti je všechna najednou držet v paměti.  Metoda 'select'
      umožňuje kdykoliv zpřístupnění dat reinicializovat (\"rewind\").

    - Metody pro modifikaci dat: 'insert()', 'update()' a 'delete()'.

    - Metody pro získání informací o tabulce: 'columns()' a 'key()'.

    - Metody pro nastavení callbacků při modifikaci dat:
      'add_callback_on_change()', 'remove_callback_on_change()'.

    Tato třída má charakter abstraktní třídy a chová se jako prázdná tabulka,
    tj. jako read-only tabulka s definovanými sloupci, avšak žádnými řádky.

    """
    AGG_MIN = 'AGG_MIN'
    """Konstanta získání minimální hodnoty pro metodu 'select()'."""
    AGG_MAX = 'AGG_MAX'
    """Konstanta získání maximální hodnoty pro metodu 'select()'."""
    AGG_COUNT = 'AGG_COUNT'
    """Konstanta získání počtu položek pro metodu 'select()'."""
    AGG_SUM = 'AGG_SUM'
    """Konstanta získání součtu položek pro metodu 'select()'."""
    AGG_AVG = 'AGG_AVG'
    """Konstanta získání průměrné hodnoty položek pro metodu 'select()'."""

    _CACHEABLE = True

    UNKNOWN_ARGUMENTS = 'UNKNOWN_ARGUMENTS'

    class UnsupportedOperation(Exception):
        """Signalizuje, že byla žádána nepodporovaná operace."""

    def __init__(self, columns, key, ordering=None, condition=None, full_init=True, **kwargs):
        """
        Arguments:

          columns -- sequence of 'ColumnSpec' instances, it uniquely identifies
            table columns and their order (by their ordering in 'columns')
          key -- key column or a list of key columns uniquely identifying table
            rows; all those columns must be present in 'columns'
          ordering -- specification of automatically retained sorting, it is a
            column id or a tuple of column ids or 'None'.  If it is not 'None',
            rows are automatically ordered by the given column (or the first of
            the given columns) in 'select*' operations.  This ordering column
            must be of type 'types_.Integer'.  If the argument is a tuple, only
            rows with the same values in the second and following columns of
            the tuple are mutually ordered.
          condition -- condition limiting selection of lines in all 'select*'
            and 'row' operations; 'Operator' instance or 'None'
          full_init -- iff true, call the 'after_init' method in the constructor
          kwargs -- given to the ancestor constructor

        """
        super(Data, self).__init__(columns=columns, key=key, ordering=ordering,
                                   **kwargs)
        assert all(isinstance(c, ColumnSpec) for c in columns), \
            ('Invalid column specification', columns)
        key = xtuple(key)
        if ordering:
            ordering = xtuple(ordering)
        if __debug__:
            for k in key:
                assert k in columns, ('Key not among columns', k, columns)
        self._columns = tuple(columns)
        self._key = key
        self._ordering = ordering
        self._condition = condition
        self._change_number = pytis.util.Counter()
        self._on_change_callbacks = []
        self._select_last_row_number = None
        if full_init:
            self.after_init()

    def after_init(self):
        """Method called after all initializations.

        The purpose of this method is to allow instance completion after it is
        copied from its template in the data object cache.

        """
        pass

    def columns(self):
        """Return 'columns' specification given to constructor."""
        return self._columns

    def condition(self):
        """Return 'condition' specification given to constructor."""
        return self._condition

    def find_column(self, id):
        """Vrať 'ColumnSpec' identifikovanou 'id'.

        Pokud taková specifikace neexistuje, vrať 'None'.

        """
        return find(id, self.columns(), key=ColumnSpec.id)

    def key(self):
        """Vrať klíčové sloupce zadané v konstruktoru, jako tuple."""
        return self._key

    def row_key(self, row):
        """Vrať hodnoty klíče z 'row' jako tuple instancí 'Value'."""
        return row.columns([c.id() for c in self.key()])

    def row(self, key, columns=None, transaction=None, arguments={}):
        """Return row instance 'Row' identified by 'key'.

        If there is no such row, return 'None'.

        Arguments:

          key -- instance or a sequence of instances of the class
            'types_.Value' corresponding to the columns of the key,
            representing key values of the row being looked for
          columns -- sequence of names of the columns to retrieve; if not
            given, all columns are retrieved; if given, all key columns must be
            included
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)
          arguments -- dictionary of table function call arguments, with
            function argument identifiers as keys and 'pytis.data.Value'
            instances as values.  Useful only when the table is actually a row
            returning database function, otherwise ignored.

        Length of 'key' must correspond to the number of key columns.

        The method always returns 'None' in this class.

        """
        return None

    def select(self, condition=None, reuse=False, sort=(), columns=None, transaction=None,
               arguments={}, async_count=False, timeout_callback=None, limit=None):
        """Initialize selection of records from the data source.

        The method itself does not necessarily load any data, the selection is
        only initialized if necessary.  The actual data may be obtained by
        repetitive calls to 'fetch()' (or 'fetchone()') after calling this
        method.  Rows can also be skipped using the 'skip()' method.  The
        number of corresponding rows is returned if possible, but None may be
        returned if this feature is not implemented.

        Repeated calls to this method may not result in the same data if the
        data source changes in the meanwhile.

        Arguments:
          condition -- condition limiting the set of resulting rows as an
            'Operator' instance or 'None'
          reuse -- boolean flag indicating, that the data of the previous
            select may be reused if the condition matches
          sort -- sequence of sorting specifiers.  Each specifier is a column
            identifier or a pair (ID, DIRECTION).  DIRECTION os one of module
            constants 'ASCENDENT' or 'DESCENDANT'.  The default value is
            'ASCENDENT'.
          columns -- sequence of IDs of columns to select; if not given, all
            columns are selected
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)
          async_count -- if true, try to count result lines asynchronously
          timeout_callback -- ignored
          limit -- limit maximum number of selected rows, integer or 'None' (no
            limit)

        Je-li 'condition' různé od 'None', specifikuje podmínku pro výběr
        řádků.  Podtřídy nejsou povinny podmínky implementovat (mohou je
        neimplementovat vůbec nebo mohou implementovat pouze některé podmínky),
        v takovém případě to musí být uvedeno v dokumentaci a při zadání
        nepodporované podmínky musí metoda vyvolat výjimku
        'UnsupportedOperation'.

        Třídění výběru se provádí podle sloupců uvedených v argumentu 'sort',
        s prioritou dle jejich pořadí.  Třídění je taktéž nepovinná operace a
        podtřídy nejsou povinny je implementovat; pokud je neimplementují, musí
        to být uvedeno v jejich dokumentaci.

        The method always returns 0 in this class.

        """
        self._select_last_row_number = -1
        return 0

    def select_map(self, function, transaction=None, **kwargs):
        """Aplikuj 'function' na všechny řádky výběru a vrať výsledky.

        Zavolej metodu 'select()' s argumenty 'kwargs' a na všechny vrácené
        řádky zavolej funkci 'function'.  Výsledky všech volání 'function' vrať
        jako seznam s počtem a pořadím prvků odpovídajících vráceným datovým
        řádkům.

        Argumenty:

          function -- function of one argument -- the 'Row' instance
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)
          kwargs -- remaining arguments passed to 'select()'

        """
        result = []
        try:
            self.select(transaction=transaction, **kwargs)
            while True:
                row = self.fetchone()
                if row is None:
                    break
                result.append(function(row))
        finally:
            try:
                self.close()
            except Exception:
                pass
        return result

    def select_aggregate(self, operation, condition=None, transaction=None):
        """Vrať výslednou hodnotu agregační funkce.

        Metoda provádí select, jehož hodnotou je výsledek agregační funkce
        kompletně vybraných dat.  Je-li vyvolána během neuzavřeného select,
        tento select nepřerušuje a je-li podmínka její a aktuálního select
        shodná, vrací výsledek odpovídající obsahu onoho selectu (například
        zpracováním v jediné transakci).

        Argumenty:

          operation -- dvojice (OPERATION, COLUMN), kde OPERATION je jednou
            z 'AGG_*' konstant třídy a COLUMN je id sloupce, nad kterým má být
            operace provedena.
          condition -- shodné se stejnojmenným argumentem metody 'select()'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Vrací: Instanci 'Value' odpovídající požadované agregační funkci.

        Podtřídy nejsou povinny tuto metodu implementovat (mohou ji
        neimplementovat vůbec nebo mohou implementovat pouze některé podmínky),
        v takovém případě to musí být uvedeno v dokumentaci a při zadání
        nepodporované podmínky musí metoda vyvolat výjimku
        'UnsupportedOperation'.

        V této třídě metoda vždy vyvolává výjimku 'UnsupportedOperation'.

        """
        raise self.UnsupportedOperation()

    def select_and_aggregate(self, operation, condition=None, reuse=False, sort=(),
                             columns=None, transaction=None):
        """Combination of 'select' and 'select_aggregate' methods.

        The method returns a pair (SELECT_RESULT, AGGREGATE_RESULT) where:

        - SELECT_RESULT is a result of `select' call with the same arguments
          (except for 'operation').

        - AGGREGATE_RESULT is a 'Row' instance containing the same columns as
          the corresponding select.  Row values of columns with the type
          corresponding to the type of the 'operation' are results of the
          aggregate function defined by the 'operation' argument that must be
          one of the 'AGG_*' class constants.  Other row values are 'None'.

        If 'select_aggregate' is unsupported in the given class then
        'select_and_aggregate' is unsupported as well.

        """
        if columns is None:
            columns = [c.id() for c in self.columns()]
        try:
            select_result = self.select(condition=condition, reuse=reuse,
                                        sort=sort, columns=columns, transaction=transaction)

            def aggregate_value(cid):
                if ((operation == self.AGG_COUNT or
                     isinstance(self.find_column(cid).type(), Number))):
                    number = self.select_aggregate((operation, cid,), condition=condition,
                                                   transaction=transaction)
                    result = (cid, number,)
                else:
                    result = (cid, Value(Type(), None),)
                return result
            aggregates = [aggregate_value(cid) for cid in columns]
        finally:
            try:
                self.close()
            except Exception:
                pass
        return select_result, Row(aggregates)

    def fetch(self, position):
        """Return data row from given position or next in given direction.

        Arguments:

          position -- either an absolute position as int or a direction (one of
            'FORWARD', 'BACKWARD').  When int, the internal selection pointer
            is moved to given position and the item at given position is
            returned.  If direction, the pointer is incremented (on 'FORWARD')
            or decremented (on 'BACKWARD') and the item from the new position
            is returned (if it exists).

        If there is no data row at the requested position (we got outside the
        rows available within the current selection) 'None' is returned.

        The first call to this method must be preceeded by calling 'select()'.
        Otherwise the behavior of this method is undefined.  Subsequent calls
        to 'select()' resets the buffer so a following 'fetch()' will start
        from the beginning.

        Calls to 'select()', 'fetch()' and 'skip()' should not be interspersed
        by calls to other public methods, otherwise the behavior is undefined.

        After the last call to this method 'close()' should be called.

        In this class always returns None.

        """
        return None

    def fetchone(self):
        """Convenience alias for 'fetch(FORWARD)'."""
        return self.fetch(FORWARD)

    def last_row_number(self):
        """Vrať pořadí řádku posledně vráceného metodou 'fetch()'.

        Řádky jsou číslovány od 0.  Pokud v aktuálním selectu dosud nebyl žádný
        řádek přes 'fetch()' získán, vrať -1.

        """
        return self._select_last_row_number

    def skip(self, count, direction=FORWARD):
        """Přeskoč 'count' řádků ve směru 'direction'.

        Argumenty:

          count -- počet řádků, které mají být přeskočeny; musí to být
            nezáporný integer
          direction -- jedna z konstant 'FORWARD' a 'BACKWARD', určuje směr
            pohybu při přeskakování

        Vrací: Počet skutečně přeskočených řádků (může být nižší, dojde-li
        k překročení hranic dat) nebo 'None', pokud tento údaj není znám.

        Pokud při skákání dojde k překročení hranic dat, nadbytečné
        přeskakování řádků se ignoruje.

        Metodu je možno použít pouze během otevřeného selectu (viz metody
        'select()' a 'close()').

        V této třídě metoda přeskakuje řádky prostřednictvím volání metody
        'fetch()' a vrací počet řádků, pro které tato volání nevrátila
        'None'.

        """
        for i in range(count):
            if self.fetch(direction) is None:
                return i
        return count

    def rewind(self):
        """Vrať se před začátek dat aktuálního selectu.

        Metodu je možno použít pouze během otevřeného selectu (viz metody
        'select()' a 'close()').

        """
        while self.fetch(BACKWARD) is not None:
            pass

    def search(self, condition, direction=FORWARD, transaction=None):
        """Vyhledej nejbližší výskyt řádku splňujícího 'condition'.

        Argumenty:

          condition -- podmínkový výraz, který musí hledaný prvek splňovat,
            instance třídy 'Operator'
          direction -- směr vyhledávání, jedna z konstant 'FORWARD' a
            'BACKWARD'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Vrací: Vzdálenost hledaného řádku od aktuálního řádku v počtu řádků
        jako kladný integer.  Pokud řádek nebyl nalezen, je vrácena 0.

        Aktuální řádek je řádek, na který ukazuje ukazovátko.  Tj. například
        řádek bezprostředně předtím vytažený metodou 'fetch()'.

        Metodu je možno použít pouze během otevřeného selectu (viz metody
        'select()' a 'close()'), vyhledávání se provádí pouze mezi řádky
        tohoto selectu.  Pozice ukazovátka selectu není touto metodou změněna.

        Podtřídy nejsou povinny tuto metodu implementovat (mohou ji
        neimplementovat vůbec nebo mohou implementovat pouze některé podmínky),
        v takovém případě to musí být uvedeno v dokumentaci a při zadání
        nepodporované podmínky musí metoda vyvolat výjimku
        'UnsupportedOperation'.

        V této třídě metoda vždy vyvolává výjimku 'UnsupportedOperation'.

        """
        raise self.UnsupportedOperation()

    def search_key(self, key, direction=FORWARD):
        """Stejné jako 'search()', ale hledá podle 'key' místo podmínky.

        Argumenty:

          key -- sekvence hodnot klíče
          direction -- stejné jako v 'search()'

        """
        eqs = map(lambda c, k: EQ(c.id(), k), self.key(), key)
        condition = AND(*eqs)
        return self.search(condition, direction=direction)

    def close(self):
        """Ukonči aktuální select.

        Tato metoda umožňuje explicitně uzavřít aktuální čtení dat pomocí
        'select()' + 'fetch()' a uvolnit tak již případné dále nepotřebné
        datové struktury nebo prostředky systému.

        Metoda může být volána pouze když je aktivní 'select()' spojení.
        Pokud tomu tak není, je její chování nedefinováno.

        V této třídě tato metoda nedělá nic.

        """
        self._select_last_row_number = None

    def insert(self, row, after=None, before=None, transaction=None):
        """Vlož 'row' do tabulky.

        Argumenty:

          row -- instance třídy 'Row'
          after -- 'None' nebo klíč řádku, za který má být nový řádek vložen
          before -- 'None' nebo klíč řádku, před který má být nový řádek vložen
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Argumenty 'after' a 'before' mají smysl pouze tehdy, pokud byl
        v konstruktoru specifikován argument 'ordering'.  Nesmí být
        specifikovány oba současně.  Pokud řádek určený některým z těchto
        argumentů neexistuje, není nový řádek vložen.  Pokud je v novém řádku
        uvedena hodnota pořadového sloupce, je tato ignorována.  Pokud má
        datový objekt ordering a oba argumenty 'after' a 'before' jsou 'None',
        nový řádek může být vložen na kteroukoliv pozici.

        'row' nemusí obsahovat obsahovat hodnoty pro všechny sloupce tabulky a
        může obsahovat i další sloupce, které tabulka neobsahuje.  Záleží na
        implementaci tabulkové třídy, jak s takovým sloupcem naloží -- může
        například chybějící sloupce doplnit implicitními hodnotami nebo je
        dopočítat nebo také může sloupec odmítnout do tabulky vložit.  V každém
        případě platí, že pořadí sloupců v 'row' je z hlediska této metody
        nepodstatné, sloupce jsou rozeznávány svými názvy.

        Pokud 'row' obsahuje klíč a řádek s takovým klíčem již v tabulce
        existuje, neprováděj nic.  Pakliže 'row' obsahuje pouze některé a ne
        všechny sloupce klíče, záleží na konkrétní implementaci, zda jej vloží
        či nikoliv.

        Není specifikováno, na které místo tabulky má být řádek vložen, obvykle
        je to konec tabulky, není to však vyžadováno.

        Je-li sloupec do tabulky úspěšně vložen, vrať dvojici (ROW, 'True'),
        kde ROW je skutečný nový řádek tabulky, který na základě vložení 'row'
        vzniknul.  Pokud nemůže být obsah nového řádku zjištěn, vrať dvojici
        ('None', 'True').  Pokud řádek nebyl do tabulky úspěšně vložen, vrať
        dvojici ('None', 'False') v případě, že popis chyby není definován,
        příp. dvojici (text, 'False'), kde text je popis příčiny neúspěchu.

        V této třídě metoda vždy pouze vrací ('None', 'False').

        """
        return None, False

    def update(self, key, row, transaction=None):
        """Nahraď řádek identifikovaný klíčem 'key' řádkem 'row'.

        Argumenty:

          key -- instance nebo seznam instancí třídy 'types._Value', musí
            odpovídat všem sloupcům klíče tabulky
          row -- instance třídy 'Row'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        'row' nemusí obsahovat hodnoty pro všechny sloupce tabulky a může
        obsahovat i další sloupce, které tabulka neobsahuje.  Záleží na
        implementaci tabulkové třídy, jak s takovým sloupcem naloží -- může
        například chybějící sloupce doplnit implicitními hodnotami nebo je
        dopočítat nebo také může sloupec odmítnout do tabulky vložit.  V každém
        případě platí, že pořadí sloupců v 'row' je z hlediska této metody
        nepodstatné, sloupce jsou rozeznávány svými názvy.

        Pokud 'key' v tabulce neidentifikuje žádný existující řádek, neprováděj
        nic a vrať 'None'.  Pokud 'row' obsahuje klíč různý od 'key' a řádek
        s takovým klíčem již v tabulce existuje, neprováděj nic a vrať 'None'.
        Pakliže 'row' obsahuje pouze některé a ne všechny sloupce klíče, záleží
        na konkrétní implementaci, jak s tím naloží.

        Byl-li pro datový objekt v konstruktoru specifikován argument
        'ordering', není pořadí řádku updatem změněno.  Případná nově zadaná
        hodnota kteréhokoliv \"ordering\" sloupce je ignorována a je nahrazena
        starou hodnotou.  Z toho plyne, že updatem není možno změnit hodnotu
        žádného sloupce uvedeného v 'ordering'.

        Je-li sloupec úspěšně updatován, vrať dvojici (ROW, 'True'), kde ROW
        je skutečný updatovaný řádek tabulky, který na základě vložení 'row'
        vzniknul.  Pokud takový řádek nelze zjistit, vrať ('None', 'True').
        V opačném případě (tj. pokud řádek s 'key' neexistuje nebo pokud je
        problematický klíč 'row', jak je uvedeno výše) vrať dvojici
        ('None', 'False') v případě, že popis chyby není definován,
        příp. dvojici (text, 'False'), kde text je popis příčiny neúspěchu.

        V této třídě metoda vždy pouze vrací dvojici ('None', 'False').

        """
        return None, False

    def update_many(self, condition, row, transaction=None):
        """Update rows given by 'condition' by data of given 'row'.

        Arguments:

          condition -- condition which limits the rows to be updated;
            'Operator' instance
          row -- 'Row' instance
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        'row' nemusí obsahovat hodnoty pro všechny sloupce tabulky a může
        obsahovat i další sloupce, které tabulka neobsahuje.  Záleží na
        implementaci tabulkové třídy, jak s takovým sloupcem naloží -- může
        například chybějící sloupce doplnit implicitními hodnotami nebo je
        dopočítat nebo také může sloupec odmítnout do tabulky vložit.  V každém
        případě platí, že pořadí sloupců v 'row' je z hlediska této metody
        nepodstatné, sloupce jsou rozeznávány svými názvy.

        Pokud by změna řádků měla mít za následek změnu klíče některého řádku
        na klíč po updatu existující v jiném řádku, je vyvolána výjimka a data
        nejsou změněna.

        Byl-li pro datový objekt v konstruktoru specifikován argument
        'ordering', není pořadí řádku updatem změněno.  Případná nově zadaná
        hodnota kteréhokoliv \"ordering\" sloupce je ignorována.  Z toho plyne,
        že updatem není možno změnit hodnotu žádného sloupce uvedeného
        v 'ordering'.

        Vrací: Počet updatovaných řádků.

        V této třídě metoda vždy pouze vrací '0'.

        """
        return 0

    def delete(self, key, transaction=None):
        """Smaž řádek identifikovaný 'key'.

        Argumenty:

          key -- instance nebo seznam instancí třídy 'types._Value', musí
            odpovídat všem sloupcům klíče tabulky; určuje řádek, který má být
            smazán
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Pokud je řádek smazán, vrať 1.
        Pokud takový řádek nelze smazat (včetně situace, kdy neexistuje),
        nedělej nic a vrať 0.

        V této třídě metoda vždy pouze vrací 0.

        """
        return 0

    def delete_many(self, condition, transaction=None):
        """Smaž řádky identifikované 'condition'.

        Argumenty:

          condition -- podmínka odpovídající řádkům, které mají být
            smazány; instance třídy 'Operator'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Vrací: Počet smazaných řádků.

        V této třídě metoda vždy pouze vrací 0.

        """
        return 0

    def change_number(self):
        """Vrať počet dosud zaregistrovaných změn dat.

        Tento počet je větší nebo roven 0 a každá následující návratová hodnota
        je větší nebo rovna předchozí návratové hodnotě.  Nic jiného
        garantovaného není, zejména ne že počet odpovídá skutečnému počtu změn
        dat, jedná se pouze o orientační hodnotu.  Pro podrobnější diskusi
        spolehlivosti viz dokumentace metody `add_callback_on_change()'.

        """
        return self._change_number.current()

    def add_callback_on_change(self, function):
        """Zaregistruj 'function' do seznamu modifikačních callbacků.

        Dojde-li k oznámení o modifikaci dat, může pak být 'function' zavolána,
        bez parametrů.  Zda je 'function' skutečně zavolána, závisí na
        konkrétní implementaci datové třídy.  Některé implementace mohou toto
        volání garantovat, některé mohou volání provádět bez garance a některé
        nemusí volání provádět nikdy.  Tato zpětná volání jsou tedy pouze
        pomocným mechanismem, který může být pro určité účely (například
        překreslení tabulky na obrazovce) užitečný, nelze na něj však spoléhat.

        **Pozor:** Vzhledem k tomu, že se jedná o callbacky, bude 'function'
        obvykle volána v samostatném threadu, je v ní tedy třeba dbát na
        případné možné kolize s hlavním threadem.

        V této třídě k žádným modifikacím dat nedochází a tudíž se ani
        neprovádí zpětná volání.

        """
        self._on_change_callbacks.append(function)

    def remove_callback_on_change(self, function):
        """Odstraň 'function' ze seznamu modifikačních callbacků.

        Pokud 'function' v seznamu modifikačních callbacků není, nedělej nic.

        """
        try:
            self._on_change_callbacks.remove(function)
        except ValueError:
            pass

    def _call_on_change_callbacks(self):
        self._change_number.next()
        for c in self._on_change_callbacks:
            c()

    @classmethod
    def cacheable(class_):
        """Return whether it is possible to cache instances of this class.

        This information is intended to be used in the 'DataFactory' class.

        """
        return class_._CACHEABLE


class Counter(object):
    """Abstrakce přístupu ke generátoru tickets.

    Jedná se o jednoduchý čítač vracející postupně sekvenci unikátních
    vzestupných čísel, blíže viz metoda 'next()'.

    """

    def next(self):
        """Vrať další hodnotu čítače jako integer.

        V této třídě metoda pouze vyvolává 'NotImplementedException'.

        """
        raise NotImplementedException()


class Function(object):
    """Abstrakce databázových funkcí.

    Podporovány jsou pouze jednoduché funkce, přijímající pevný (ale libovolný)
    počet argumentů daných svojí pozicí a vracející seznam řádků.

    """

    def call(self, row):
        """Zavolej funkci a vrať výsledek.

        Argumenty:

          row -- instance třídy 'Row' odpovídající argumentům funkce, je
            důležité pořadí prvků v 'row'

        Vrací: Sekvenci instancí třídy 'Row' odpovídající výsledku volání
        funkce.

        """
        raise NotImplementedException()


class MemData(Data):
    """Data kept in memory.

    The class providing a data object which holds its data rows in memory.  The
    data is not persistent, but otherwise the data object provides the same
    interface as the database based data objects.  It is mainly useful for
    testing and debugging and special purposes, such as virtual forms.

    The class is not thread-safe.

    Data modification methods don't invoke any callbacks.

    """

    _CACHEABLE = False

    def __init__(self, columns, key=None, data=(), **kwargs):
        """Initialize data source according to given 'columns' specification.

        Arguments:
          'columns' -- same as in parent class.
          'key' -- same as in parent class but may be also None, in which
             case the first columns is used as a key.
          'data' -- may contain initial data as a sequence of 'pytis.data.Row'
             instances or a sequence of tuples of internal python values.  In
             the later case the values appear in the order of 'columns' and
             must match their types ('TypeError' is raised if not).

        """
        super(MemData, self).__init__(columns, key if key is not None else columns[0], **kwargs)
        self._mem_data = []
        self._mem_cursor = -1
        for row in data:
            if isinstance(row, (tuple, list)):
                row = pytis.data.Row([(c.id(), pytis.data.Value(c.type(), v))
                                      for c, v in zip(self._columns, row)])
            self.insert(row)

    def _mem_find_index(self, key):
        if isinstance(key, (tuple, list)):
            key = key[0]
        data = self._mem_data
        k = self.key()[0].id()
        condition = self._condition2pyfunc(self._condition)
        for i in range(len(data)):
            if data[i][k] == key and condition(data[i]):
                return i
        else:
            return None

    def _mem_create_row(self, row, index=None):
        try:
            key = row[self.key()[0].id()]
        except Exception:
            return None
        i = self._mem_find_index(key)
        if index is not None and i is not None and i != index:
            return None
        pairs = []
        for c in self.columns():
            id = c.id()
            try:
                val = row[id]
            except Exception:
                val = None
            pairs.append((id, val))
        return Row(pairs)

    def _restrict_row_columns(self, row, columns):
        if columns is None:
            return row
        else:
            return Row([(cid, row[cid]) for cid in columns])

    def row(self, key, columns=None):
        index = self._mem_find_index(key)
        if index is None:
            return None
        return self._restrict_row_columns(self._mem_data[index], columns)

    def rewind(self):
        self._mem_cursor = -1

    def _condition2pyfunc(self, condition):
        def wildcard_match(string, pattern):
            import re
            regexp = re.escape(pattern).replace('\\*', '.*').replace('\\?', '.') + '$'
            return re.match(regexp, string)
        if condition is None:
            return lambda row: True
        op_name = condition.name()
        relational_operators = {
            'EQ': operator.eq,
            'LT': operator.lt,
            'WM': wildcard_match,
        }
        if op_name in relational_operators.keys():
            def relop(row, op, args, kwargs):
                def arg(a, ignore_case=False):
                    if isinstance(a, basestring):
                        v = row[a].value()
                    else:
                        v = a.value()
                    if ignore_case and isinstance(v, basestring):
                        v = v.lower()
                    return v
                return op(*[arg(a, **kwargs) for a in args])
            return lambda row: relop(row, relational_operators[op_name],
                                     condition.args(), condition.kwargs())
        elif op_name == 'NOT':
            func = self._condition2pyfunc(condition.args()[0])
            return lambda row: not func(row)
        elif op_name == 'AND':
            fctns = [self._condition2pyfunc(c) for c in condition.args()]
            return lambda row: functools.reduce(lambda r, f: r and f(row), fctns, True)
        else:
            t = condition.translation()
            if t is not None:
                return self._condition2pyfunc(t)
            else:
                raise self.UnsupportedOperation(op_name)

    def select(self, condition=None, reuse=False, sort=None, columns=None, transaction=None,
               arguments={}, async_count=False, stop_check=None, timeout_callback=None, limit=None):
        """Inicializace vytahování záznamů.

        Bližší popis viz nadtřída.  Argumenty 'condition', 'sort',
        'transaction', 'arguments', 'async_count', 'stop_check' a 'timeout_callback'
        jsou ignorovány.

        """
        if self._condition is not None:
            if condition is not None:
                condition = AND(condition, self._condition)
            else:
                condition = self._condition
        cond = self._condition2pyfunc(condition)
        self._mem_cursor = -1
        result = []
        for row in self._mem_data:
            if cond(row):
                if limit is not None:
                    if limit <= 0:
                        break
                    limit -= 1
                result.append(self._restrict_row_columns(row, columns))
        self._mem_select = result
        return len(self._mem_select)

    def close(self):
        self._mem_select = []

    def fetch(self, position):
        data = self._mem_select
        cursor = self._mem_cursor
        size = len(data)
        if position == FORWARD:
            cursor += 1
        elif position == BACKWARD:
            cursor += 1
        else:
            assert isinstance(position, int), position
            cursor = position
        if 0 <= cursor < size:
            result = data[cursor]
        else:
            result = None
        self._mem_cursor = max(-1, min(cursor, size))
        return result

    def last_row_number(self):
        return self._mem_cursor

    def insert(self, row, transaction=None):
        """Vlož 'row' do tabulky.

        Pro bližší popis viz nadtřída.

        'row' je vložen na konec tabulky.  Chybějící sloupce jsou nastaveny na
        'None'.

        Argument 'transaction' je ignorován.

        """
        assert isinstance(row, Row)
        new_row = self._mem_create_row(row)
        if new_row is None:
            return None, False
        self._mem_data.append(new_row)
        return new_row, True

    def update(self, key, row, transaction=None):
        """Updatuj 'row' v tabulce.

        Pro bližší popis viz nadtřída.

        'row' je v tabulce vložen na místo řádku identifikovaného 'key''.
        Chybějící sloupce jsou nastaveny na 'None'.

        Argument 'transaction' je ignorován.

        """
        index = self._mem_find_index(key)
        if index is None:
            return None, False
        new_row = self._mem_create_row(row, index)
        if new_row is None:
            return None, False
        self._mem_data[index] = new_row
        return new_row, True

    def delete(self, key, transaction=None):
        """
        Argument 'transaction' is ignored.
        """
        index = self._mem_find_index(key)
        if index is None:
            return 0
        del self._mem_data[index]
        return 1


# Utility functions


def EQ(x, y, ignore_case=False):
    """Podmínkový operátor 'EQ' relace rovnosti '='.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- hodnota sloupce, instance třídy 'types._Value'
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je primitivní.

    """
    return Operator('EQ', x, y, ignore_case=ignore_case)


def NE(x, y, ignore_case=False):
    """Podmínkový operátor nerovnosti.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- hodnota sloupce, instance třídy 'types._Value'
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je vyjádřený pomocí jiných operátorů.

    """
    t = NOT(EQ(x, y, ignore_case=ignore_case))
    return Operator('NE', x, y, ignore_case=ignore_case, translation=t)


def WM(x, y, ignore_case=True):
    """Podmínkový operátor 'WM' (\"wildcard matches\") porovnání dle vzoru.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- instance 'WMValue' definující vzor; její hodnota může jako wildcars
        obsahovat znaky '*' (lze substituovat čímkoliv) a '?' (lze substituovat
        libovolným znakem)
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je primitivní.

    """
    return Operator('WM', x, y, ignore_case=ignore_case)


def NW(x, y, ignore_case=True):
    """Podmínkový operátor negace porovnání dle vzoru.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- string definující vzor; jako wildcars může obsahovat znaky '*' (lze
        substituovat čímkoliv) a '?' (lze substituovat libovolným znakem)
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je vyjádřený pomocí jiných operátorů.

    """
    t = NOT(WM(x, y, ignore_case=ignore_case))
    return Operator('NW', x, y, ignore_case=ignore_case, translation=t)


def LT(x, y, ignore_case=False):
    """Podmínkový operátor 'LT' relace '<'.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- hodnota sloupce, instance třídy 'types._Value'
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je primitivní.

    """
    return Operator('LT', x, y, ignore_case=ignore_case)


def LE(x, y, ignore_case=False):
    """Podmínkový operátor relace '<='.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- hodnota sloupce, instance třídy 'types._Value'
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je vyjádřený pomocí jiných operátorů.

    """
    t = OR(LT(x, y, ignore_case=ignore_case),
           EQ(x, y, ignore_case=ignore_case))
    return Operator('LE', x, y, ignore_case=ignore_case, translation=t)


def GT(x, y, ignore_case=False):
    """Podmínkový operátor relace '>'.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- hodnota sloupce, instance třídy 'types._Value'
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je vyjádřený pomocí jiných operátorů.

    """
    t = AND(NOT(EQ(x, y, ignore_case=ignore_case)),
            NOT(LT(x, y, ignore_case=ignore_case)))
    return Operator('GT', x, y, ignore_case=ignore_case, translation=t)


def GE(x, y, ignore_case=False):
    """Podmínkový operátor relace '>='.

    Argumenty:

      x -- column identifier (basestring), or an 'OpFunction' value
      y -- hodnota sloupce, instance třídy 'types._Value'
      ignore_case -- zda má být ignorována velikost písmen (má-li to pro daný
        typ smysl)

    Tento operátor je vyjádřený pomocí jiných operátorů.

    """
    t = OR(GT(x, y, ignore_case=ignore_case),
           EQ(x, y, ignore_case=ignore_case))
    return Operator('GE', x, y, ignore_case=ignore_case, translation=t)


def NOT(x):
    """Logical operator 'NOT' for logical negation.

    Arguments:

      x -- operand as 'Operator' instance

    This operator is primitive.

    """
    return Operator('NOT', x)


def AND(*args):
    """Logical operator 'AND' for logical conjunction.

    Arguments:

      args -- sequence of operands as 'Operator' instances (may be also empty).
        The operands may also be None in which case they are ignored.

    The operator is commutative so the order of operands doesn't matter.

    This operator is primitive.

    """
    return Operator('AND', *[arg for arg in args if arg is not None])


def OR(*args):
    """Logical operator 'OR' for logical disjunction.

    Arguments:

      args -- sequence of operands as 'Operator' instances (may be also empty).

    The operator is commutative so the order of operands doesn't matter.

    This operator is not primitive - it may be expressed by a combination of
    other operators.

    """
    t = NOT(AND(*map(NOT, args)))
    return Operator('OR', *args, **{'translation': t})


def ANY_OF(column, *args):
    """Matching one of the given values.

    'column' value is equal to any of the 'args' values.

    Arguments:

      column -- name of the table column; string
      args -- 'Value' instances to check for

    This operator is not primitive - it may be expressed by a combination of
    other operators.

    """
    assert isinstance(column, basestring), column
    assert all([isinstance(a, Value) for a in args]), args
    return OR(*[EQ(column, value) for value in args])


def IN(column_id, data, table_column_id, table_condition, table_arguments=None):
    """Operator testing presence in a subquery.

    Arguments:

      column_id -- column id (basestring), the value of which is tested for
        presence in the subquery
      data -- 'Data' instance, to perform the subquery on
      table_column_id -- column id (basestring) in 'data' returning the subquery values
      table_condition -- condition limiting the selection from 'data'
      table_arguments -- arguments passed to 'data' (if 'data' is on a table function).

    This operator is primitive.

    """
    return Operator('IN', column_id, data, table_column_id, table_condition, table_arguments)


_ft_query_id = None


def FT(column_id, query):
    """Full text search.

    Arguments:

      query -- string containing the PostgreSQL full text search expression

    Using this operator is limited only to search conditions and to one
    instance of this operator per one condition.

    This is a primitive operator.

    """
    global _ft_query_id
    if _ft_query_id is None:
        # This is not thread safe, but query id is not used for now anyway
        _ft_query_id = pytis.util.Counter()
    query_id = _ft_query_id.next()
    return Operator('FT', column_id, query, query_id)


def LTreeMatch(column_id, match):
    """ltree matching.

    Arguments:

      column_id -- column identifier, string
      match -- lquery, string

    """
    return Operator('LTreeMatch', column_id, match)


def LTreeAncestor(x, y):
    """ltree comparison: The left an ancestor or equal to the right.

    Arguments:

      x -- column identifier as a string
      y -- column identifier as a string, or a non-NULL ltree value as a
        'types._Value' instance

    """
    return Operator('LTreeAncestor', x, y)


def LTreeDescendant(x, y):
    """ltree comparison: The left a descendant or equal to the right.

    Arguments:

      x -- column identifier as a string
      y -- column identifier as a string, or a non-NULL ltree value as a
        'types._Value' instance

    """
    return Operator('LTreeDescendant', x, y)


def RangeContains(column, value):
    """Range "contains" operator.

    Arguments:

      range_ -- column identifier (string), or an 'OpFunction' value
      value -- column identifier (string) or 'Value' instance (of a range type)

    """
    return Operator('RangeContains', column, value)


def RangeContained(column, value):
    """Range "is contained by" operator.

    Arguments:

      range_ -- column identifier (string), or an 'OpFunction' value
      value -- column identifier (string) or 'Value' instance (of a range type)

    """
    return Operator('RangeContained', column, value)


def RangeOverlap(column, value):
    """Range overlap test.

    Arguments:

      range_ -- column identifier (string), or an 'OpFunction' value
      value -- column identifier (string) or 'Value' instance (of a range type)

    """
    return Operator('RangeOverlap', column, value)


def FunctionCondition(function, *args):
    """Function call.

    Arguments:

      function -- function name as a string
      args -- function arguments; each of the arguments is either a column
        identifier as a string or a 'types_.Value' instance.

    """
    return Operator('Function', function, *args)


def OpFunction(function, *args):
    """Function call to be used in relation operators.

    Arguments:

      function -- function name as a string
      args -- function arguments; each of the arguments is either a column
        identifier as a string or a 'types_.Value' instance.

    """
    return Operator('Function', function, *args)


def reversed_sorting(sorting):
    """Vrať specifikaci třídění reverzní ke specifikaci 'sorting'.

    Prvky setříděné dle vrácené hodnoty mají přesně obrácené pořadí než prvky
    setříděné dle 'sorting'.

    """
    def revspec(spec):
        if isinstance(spec, (tuple, list)):
            id, dir = spec
        else:
            id, dir = spec, ASCENDENT
        if dir == ASCENDENT:
            result = DESCENDANT
        elif dir == DESCENDANT:
            result = ASCENDENT
        else:
            raise ProgramError('Invalid sorting direction', dir)
        return id, result
    reversed = map(revspec, sorting)
    return tuple(reversed)


# Pomocné třídy


@python_2_unicode_compatible
class ColumnSpec(object):
    """Specifikace sloupce tabulkových dat.

    Každý sloupec je identifikován svým názvem (string) a typem (instance třídy
    'types_.Type').  Blíže viz metoda '__init__()'.

    Instance třídy jsou považovány za immutable, tudíž je možno je sdílet.

    """

    def __init__(self, id, type):
        """Inicializuj specifikaci sloupce.

        Argumenty:

          id -- string identifikující sloupec (název sloupce)
          type -- typ sloupce, instance třídy 'types_.Type'

        Názvem sloupce může být libovolný řetězec, nedoporučuje se však
        používat prázdný řetězec jako regulérní název sloupce.

        """
        assert isinstance(id, basestring), id
        assert isinstance(type, Type), (id, type)
        self._id = id
        self._type = type

    def __str__(self):
        return '<Column: id=%s, type=%s>' % (self.id(), self.type())

    def __eq__(self, other):
        if not sameclass(self, other):
            return NotImplemented
        else:
            return (self._id == other._id and self._type == other._type)

    def __ne__(self, other):
        # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
        return not self == other

    def __hash__(self):
        return hash(self._id)

    def id(self):
        """Vrať string identifikující sloupec zadaný v konstruktoru."""
        return self._id

    def type(self):
        """Vrať typ sloupce jako instanci třídy 'types_.Type'."""
        return self._type


@python_2_unicode_compatible
class Row(object):
    """Reprezentace jednoho řádku řádkových dat.

    V podstatě se jedná o uspořádaný seznam sloupců (položek) a jejich hodnot.
    Data jsou identifikována buďto svým pořadím, nebo názvem příslušného
    sloupce.  V prvním případě se na ně lze odkazovat číselným odkazem tak jako
    u položek seznamů, v druhém případě stringy, tak jako u hash tabulek.

    Hodnotami sloupců jsou instance třídy 'types_.Value'.

    Třída emuluje sekvenci i hash tabulku.  Kromě této emulace třída
    neposkytuje žádné další metody.  Iniciální data musí být předána
    v konstruktoru, později lze modifikovat již jen existující sloupce.
    Mazání sloupců není možné.

    """

    def __init__(self, data=()):
        """Inicializuj řádek.

        Argumenty:

          data -- sekvence dvouprvkových sekvencí, každá dvouprvková sekvence
            obsahuje jako svůj první prvek identifikátor sloupce (string) a
            jako druhý prvek jeho hodnotu (instance třídy 'Value'); argument
            není nutno klíčovat

        Příklad obsahu 'data':

          (('poradi', Value(Integer.make(), 1)),
           ('popis', Value(String.make(), 'prvni prvek')))

        """
        if __debug__:
            assert isinstance(data, (tuple, list)), data
            for item in data:
                assert isinstance(item, (tuple, list)) and len(item) == 2, \
                    ('Column definition must be (ID, VALUE) pair', item,)
                k, v = item
                assert isinstance(k, basestring), k
                assert isinstance(v, Value), v
        self._set_data(list(data))

    def _set_data(self, data):
        self._data = data
        self._indexes = dict([(key, i) for i, (key, value) in enumerate(data)])

    def _index(self, key):
        if isinstance(key, basestring):
            return self._indexes[key]
        elif isinstance(key, (int, long)):
            if key < 0:
                result = len(self) + key
                if result < 0:
                    raise IndexError('List index out of range', key)
            else:
                result = key
        else:
            raise KeyError('Illegal key type', key)
        return result

    def __getstate__(self):
        return self._data

    def __setstate__(self, state):
        if not isinstance(state, list):
            raise InvalidAccessError('Invalid row data', state)
        for k, v in state:
            if not isinstance(k, basestring):
                raise InvalidAccessError('Invalid row key', k)
            if not isinstance(v, Value):
                raise InvalidAccessError('Invalid row value', v)
        self._set_data(state)

    def __str__(self):
        items = [self._data[i][0] + '=' + unistr(item) for i, item in enumerate(self)]
        return '<Row: %s>' % ', '.join(items)

    def __hash__(self):
        value = 0
        for k, v in self._data:
            value = value ^ hash(k) ^ hash(v)
        return value

    def __eq__(self, other):
        # Rows are equal if they contain the same keys and values including their order.
        if not sameclass(self, other):
            return NotImplemented
        elif len(self) != len(other):
            return False
        else:
            return all(k1 == k2 and v1 == v2
                       for (k1, v1), (k2, v2) in zip(self._data, other._data))

    def __ne__(self, other):
        # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
        return not self == other

    def __len__(self):
        """Vrať počet sloupců v řádku."""
        return len(self._data)

    def __getitem__(self, key):
        """Return the value(s) of column(s) given by 'key'.

        Arguments:

          key -- must be an existing key (column id as a string or column index
            as integer) or a slice instance to get multiple column values at
            once.

        Returns the column value as a 'Value' instance if 'key' is a string or
        integer or a 'Row' instance containing given subset of values if 'key'is a slice.

        Exception is raised if key refers to unknown column(s).

        """
        if isinstance(key, slice):
            return self.__class__(self._data[key])
        else:
            return self._data[self._index(key)][1]

    def __setitem__(self, key, value):
        """Set the value of given column 'key' to given 'value'.

        Arguments:

          key -- must be an existing key (column id as a string or column index
            as integer) or a slice instance to set multiple columns at once.

          value -- new column value as a 'types_.Value' instance or a sequence
            of such values when 'key' is a slice.

        The behavior is undefined if column/columns given by 'key' do not exist.

        The change does not propagate to instances which are copies of this
        instance.

        """
        self._data = data = self._data[:]
        if isinstance(key, slice):
            sliced = data[key]
            # Cast value to list before passing to len to get more a appropriate
            # "TypeError: ... object is not iterable" rather than
            # "TypeError: object of type ... has no len()".
            if len(list(value)) != len(sliced):
                raise IndexError("Sequence length doesn't match the slice.")
            for (k, old_value), new_value in zip(sliced, value):
                index = self._index(k)
                data[index] = (k, new_value)
        else:
            index = self._index(key)
            data[index] = (data[index][0], value)

    def __contains__(self, key):
        """Vrať pravdu, právě když řádek obsahuje sloupec jména 'key'."""
        return key in self.keys()

    def has_key(self, key):
        return self.__contains__(key)

    def get(self, key, default=None):
        """Return `key' column value.

        In case there is no such column, return `default'.

        """
        try:
            result = self[key]
        except KeyError:
            result = default
        return result

    def keys(self):
        """Vrať seznam názvů všech sloupců jako strings.

        Pořadí položek vráceného seznamu je nedefinováno.

        """
        return self._indexes.keys()

    def items(self):
        """Vrať seznam dvojic [ID, VALUE] odpovídajících všem sloupcům.

        ID je řetězec, VALUE je instance třídy 'types_.Value'.

        Pořadí dvojic vráceného seznamu je nedefinováno.

        """
        return [copy.copy(x) for x in self._data]

    def values(self):
        """Vrať seznam hodnot všech sloupců v jejich pořadí.

        Vrací list hodnot typu 'pytis.data.Value'.

        """
        return [value for key, value in self._data]

    def columns(self, keys):
        """Vrať hodnoty sloupců odpovídající 'keys'.

        Argumenty:

          keys -- sekvence klíčů sloupců, viz též metoda '__getitem__()'

        Vrací: tuple hodnot sloupců, jejichž klíče jsou dány argumentem 'keys',
        v odpovídajícím pořadí.

        """
        return tuple([self[k] for k in keys])

    def append(self, key, value):
        """Připoj na konec řádku sloupec 'key' s hodnotou 'value'.

        Argumenty:

          key -- id sloupce jako string, nesmí být shodné s id žádného sloupce
            v instanci již přítomného
          value -- hodnota sloupce jako instance třídy 'types_.Value'

        """
        assert isinstance(key, basestring)
        assert isinstance(value, Value)
        assert not any(x[0] == key for x in self._data)
        self._data.append((key, value))
        self._indexes[key] = len(self._data) - 1

    def update(self, dict):
        """Updatuj hodnoty sloupců hodnotami z 'dict'.

        Argumenty:

          dict -- slovník klíčovaný názvy sloupců obsahující nové hodnoty pro
            tyto sloupce jako instance třídy 'types_.Value'.

        Sloupcům v 'dict' neobsaženým zůstanou zachovány jejich původní
        hodnoty.

        """
        for k in dict.keys():
            assert isinstance(dict[k], Value), ('Invalid column value', dict[k],)
            if k in self:
                self[k] = dict[k]


@python_2_unicode_compatible
class FetchBuffer(object):
    """Buffer-cache optimizing data object reads.

    Caches continuous sequences of numbered items.  The implementation is
    agnostic about the stored items, but the intended use is for data rows read
    from a data object.  To minimize the communication overhead with the data
    source, the rows are typically read in chunks.

    The items (typically data rows) are retrieved from the buffer using
    'fetch()'.  If the requested item doesn't exist within the currently cached
    range, it is automatically retrieved from the data source behind the scenes
    using the method 'retrieve'

    The maximal total number of cached items is limited by the constructor
    argument 'limit'.  The cache will grow up to this size if fetch requests
    ask for rows which are near the currently cached range (up to 'fetch_size'
    in distance).  If the newly requested item isn't in the reach of the
    currently cached items, the cache is emptied and recreated.

    """

    def __init__(self, retrieve, limit=1000, initial_fetch_size=100, fetch_size=50):
        """Arguments:

        retrieve -- Function retrieving buffer items from data source.  Must be
          a function of two arguments (position, count).  Both arguments are
          integers, where 'position' is the position of the first item to
          retrieve numbered from zero and 'count' is the total number of items
          to retrieve.  The function must return a list of items to store in
          the buffer.  The number of returned items may be lower than requested
          if the data source doesn't have any more data at given positions.
        limit -- The maximal total number of items kept in the cache.
        initial_fetch_size -- Number of items to fetch at once in the first
          request (when the buffer is empty).
        fetch_size -- Number of items to fetch at once in subsequent requests.

        """
        assert limit > fetch_size and limit > initial_fetch_size
        self._retrieve = retrieve
        self._limit = limit
        self._initial_fetch_size = initial_fetch_size
        self._fetch_size = fetch_size
        self.reset()
        if __debug__:
            log(DEBUG, 'New buffer')

    def __str__(self):
        buf = self._buffer
        pointer = self._pointer
        size = len(buf)
        if size <= 2:
            items = buf
        elif pointer <= 0 or pointer > size:
            items = (buf[0], '...', buf[-1])
        elif size == 3:
            items = buf
        elif pointer == 1:
            items = (buf[0], buf[pointer], '...', buf[-1])
        elif pointer == size - 2:
            items = (buf[0], '...', buf[pointer], buf[-1])
        else:
            items = (buf[0], '...', buf[pointer], '...', buf[-1])
        return '<FetchBuffer: limit=%d, size=%d, start=%d, pointer=%d%s>' % \
            (self._limit, len(buf), self._start, self._pointer,
             ('\n' if items else '') + '\n'.join(items))

    def __len__(self):
        return len(self._buffer)

    def reset(self):
        """Completely reset the buffer."""
        if __debug__:
            log(DEBUG, 'Resetting buffer')
        self._buffer = []
        # _start ... absolute position of the first buffer item, starting from 0.
        # _pointer ... position of the last fetched item relative to the first
        #   buffer item.  May also point outside the buffer.
        self._start = 0
        self._pointer = -1

    def rewind(self):
        """Reset buffer pointer to the initial position (in front of the first item)."""
        self._pointer = -1 - self._start

    def position(self):
        """Return the current buffer position as int.

        The position may point outside the currenty buffered data or even
        outside the source data range.

        """
        return self._start + self._pointer

    def skip(self, count, direction):
        """Skip given number of items in given direction.

        Moves buffer position relatively to the current position.

        Arguments:

          count -- number of items to skip
          direction -- One of the constants 'FORWARD'/'BACKWARD'

        """
        if direction == FORWARD:
            self._pointer += count
        elif direction == BACKWARD:
            self._pointer -= count
        else:
            raise ProgramError('Invalid direction', direction)

    def _fill(self, position, items):
        # If the new items overlap or adjoin with the current buffer content, old
        # content is left in the buffer up to 'self._limit'.
        if __debug__:
            log(DEBUG, 'Filling buffer:', (position, len(items)))
        n = len(items)
        if n > self._limit:
            raise ValueError("FetchBuffer size limit %d exceeded: %d" % (self._limit, n))
        buf = self._buffer
        size = len(buf)
        start = self._start
        end = start + size
        retain = max(self._limit - n, 0)  # How many existing items can be left in the buffer.
        if start < position <= end:
            # Append items to the end of buffer (cut off from the start if necessary).
            overlap = end - position
            if overlap:
                del buf[-overlap:]
            cutoff = max(size - retain - overlap, 0)
            if cutoff:
                start += cutoff
                del buf[:cutoff]
            buf += items
            pointer = position - start - 1
        elif position < start <= position + n:
            # Prepend items to the beginning of buffer (cut off from the end if necessary).
            overlap = position + n - start
            if overlap:
                del buf[:overlap]
            cutoff = max(size - retain - overlap, 0)
            if cutoff:
                del buf[:cutoff]
            buf[0:0] = items
            start = position
            pointer = -1
        else:
            # If the new content doesn't adjoin or overlap with the old content,
            # the old content is simply replaced by the new content.
            self._buffer = items
            start = position
            pointer = -1
        self._start = start
        self._pointer = pointer

    def fetch(self, position):
        """Return buffer item from given position.

        Arguments:
          position -- either an absolute position as int or a direction (one of
            'FORWARD', 'BACKWARD').  When int, buffer pointer is moved to given
            position and the item at given position is returned.  If direction,
            buffer pointer is incremented (on 'FORWARD') or decremented (on
            'BACKWARD') and the item from the new position is returned.

        If the item at the requested position is not present in the buffer, it
        is automatically retrieved from the data source and returned.  'None'
        is returned if the data source doesn't contain any data at the
        requested position.

        Buffer position is updated in any case (set to given absolute position
        or incremented/decremented when fetching forward/backward).

        """
        if position == FORWARD:
            pointer = self._pointer + 1
        elif position == BACKWARD:
            pointer = self._pointer - 1
        else:
            assert isinstance(position, int), position
            pointer = position - self._start
        buf = self._buffer
        size = len(buf)
        if pointer >= 0 and pointer < size:
            if __debug__:
                log(DEBUG, 'Buffer hit:', pointer)
            result = buf[pointer]
        else:
            if __debug__:
                log(DEBUG, 'Buffer miss:', pointer)
            start = self._start
            fetch_size = self._initial_fetch_size if size == 0 else self._fetch_size
            if size <= pointer < size + fetch_size:
                # We are past the buffer end, but within the reach of fetch_size.
                fetch_position = start + size
            elif pointer < 0 and -pointer <= fetch_size:
                # We are in front of the buffer start, but within the reach of fetch_size.
                fetch_position = max(0, start - fetch_size)
                fetch_size = min(start, fetch_size)
            else:
                # We are nowhere near (up to fetch_size) to the current buffer
                # content, so let's start from scratch around the requested position.
                fetch_position = max(0, start + pointer - fetch_size // 2)
            if fetch_size:
                items = self._retrieve(fetch_position, fetch_size)
                if items:
                    self._fill(fetch_position, items)
            # If self._start was moved within _fill, update the pointer accordingly.
            pointer += start - self._start
            if pointer >= 0 and pointer < len(self._buffer):
                result = self._buffer[pointer]
            else:
                result = None
        self._pointer = pointer
        return result


@python_2_unicode_compatible
class DataFactory(object):
    """Factory na tvorbu datových objektů dle zadané specifikace.

    V konstruktoru třídy jsou zadány parametry datového objektu, jehož instance
    lze posléze vytvářet pomocí metody 'create()'.  Hlavní místo použití této
    třídy jsou specifikační soubory načítané resolverem.  Resolver získá ze
    statických specifikací instanci factory a s její pomocí pak na vyžádání, po
    případném doplnění dynamických specifikací, vytváří instance datových
    objektů.

    """
    _data_object_cache = None

    @staticmethod
    def _get_data_object(key):
        class_, args, kwargs = key
        kwargs = dict(kwargs)
        kwargs['full_init'] = False
        return class_(*args, **kwargs)

    def __init__(self, class_, *args, **kwargs):
        """Inicializuj instanci

        Argumenty:

          class_ -- třída 'Data' nebo její podtřída
          args -- tuple argumentů, které mají být při vytváření instance
            předány konstruktoru třídy 'class_'
          kwargs -- dictionary klíčovaných argumentů, které mají být při
            vytváření instance předány konstruktoru třídy 'class_'

        """
        assert issubclass(class_, Data)
        self._class_ = class_

        def adjust(arg):
            if isinstance(arg, list):
                arg = tuple(arg)
            return arg
        self._args = tuple([adjust(a) for a in args])
        self._kwargs = kwargs
        # self._kwargs_hashable = tuple(kwargs.items())
        if DataFactory._data_object_cache is None and class_.cacheable():
            DataFactory._data_object_cache = LimitedCache(DataFactory._get_data_object)

    def __str__(self):
        return '<%s>' % (self.__class__.__name__,)

    def class_(self):
        """Vrať třídu datového objektu."""
        return self._class_

    def create(self, **kwargs):
        """Vytvoř a vrať novou instanci datového objektu.

        Instance je vytvořena dle specifikace třídy a argumentů jejího
        konstruktoru zadaných v '__init__()'.

        Argumenty:

          kwargs -- dictionary klíčovaných argumentů, které mají být dodatečně
            předány konstruktoru datové třídy.  Je-li některý argument zadán
            zde i v '__init__()', tak vyšší prioritu má ten zdejší.

        Tato metoda vrací vždy zbrusu novou instanci datové třídy.

        """
        _kwargs = copy.copy(self._kwargs)
        _kwargs.update(kwargs)
        cache = DataFactory._data_object_cache
        cacheable = cache is not None
        if cacheable:
            key = (self._class_, self._args, tuple(_kwargs.items()),)
            try:
                {key: True}
            except TypeError:
                cacheable = False
                log(EVENT, "Non-cacheable data object cache key: %s" % (key,))
        # TODO: Stále ještě máme problém, nyní u validity_condition
        # if cacheable:
        if False:
            data_object = cache[key]
            result = copy.copy(data_object)
            for attr in result.__dict__.keys():
                try:
                    result.__dict__[attr] = copy.copy(result.__dict__[attr])
                except Exception:
                    pass
            result.after_init()
        else:
            result = self._class_(*self._args, **_kwargs)
        return result

    def access_rights(self):
        return self._kwargs.get('access_rights')

    def connection_name(self):
        return self._kwargs.get('connection_name')

    # "Podrobné" porovnávání data factories je příliš náročné na CPU.
    # def __eq__(self, other):
    #     if sameclass(self, other):
    #         return all(getattr(self, attr) == getattr(other, attr) for attr in
    #                    ('_class_', '_args', '_kwargs'))
    #     else:
    #         return NotImplemented
    # def __hash__(self):
    #     return hash_attr(self, ('_class_', '_args', '_kwargs_hashable'))


def dbtable(table, columns, connection_data, arguments=None, connection_name=None, sql_logger=None):
    """Return 'DBDataDefault' instance corresponding to a 'table' with 'columns'.

    Arguments:

      table -- table name, string
      columns -- sequence of column ids (strings); instead of column ids pairs
        of the form (ID, TYPE,) may be used where ID is the column id and TYPE
        is 'Type' instance corresponding to the type of the column,
        specifying types is necessary in case of table functions
      arguments -- optional sequence of table function arguments in case the
        table is actually a database function returning rows; sequence items
        must be 'DBBinding' instances
      sql_logger -- if not 'None' all SQL commands are written to this object
        using its 'write' method (which the object must provide)

    """
    def binding(spec):
        if isinstance(spec, (list, tuple)):
            id, type_ = spec
        else:
            id, type_ = spec, None
        return pytis.data.DBColumnBinding(id, table, id, type_=type_)
    bindings = [binding(spec) for spec in columns]
    factory = pytis.data.DataFactory(pytis.data.DBDataDefault, bindings, bindings[0],
                                     arguments=arguments, sql_logger=sql_logger)
    data = factory.create(connection_data=connection_data, connection_name=connection_name)
    return data


def transaction():
    """Create a new database transaction for current connection and return it.

    Returns a 'DBTransactionDefault' instance.  Aims to simplyfy new
    transaction creation in applications and avoid the need to access
    pytis.config (requiring importing it).

    """
    return pytis.data.DBTransactionDefault(pytis.config.dbconnection)
