# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

"""T��dy pro zp��stupn�n� tabulkov�ch dat z�r�zn�ch zdroj�.

Z�kladem p��stupu k�dat�m je t��da 'Data' definuj�c� obecn� rozhran� p��stupu
k�tabulkov�m dat�m.  P�edpokl�d� se, �e pr�v� s�touto t��dou budou schopny
pracovat ostatn� moduly programu, kter� pot�ebuj� z�sk�vat nebo p�ed�vat v�t��
mno�stv� tabulkov�ch dat, zejm�na z�objekt� jako jsou datab�ze.  Specializovan�
podt��dy umo��uj� p��stup k�r�zn�m zdroj�m dat, viz nap��klad t��da
'dbdata.DBData'.

Krom� t��dy 'Data' modul definuje je�t� pomocnou t��du 'Row' reprezentuj�c�
jeden ��dek dat a pomocnou t��du 'ColumnSpec' pou��vanou pro specifikaci
sloupc� instanc� t��dy 'Data'.

K�dispozici jsou tak� abstraktn� t��dy pro pokro�ilej�� datab�zov� funkce, viz
nap��klad t��dy 'Counter' a 'Function'.  Tyto t��dy je nutno ch�pat jako
voliteln�, konkr�tn� datab�zov� backend nemus� nab�zet ��dnou jejich konkr�tn�
implementaci.

"""

import copy
import string
import types

import pytis.util
from pytis.data import *


FORWARD = 'FORWARD'
"""Konstanta pro dop�edn� posun v�'Data.fetchone'."""
BACKWARD = 'BACKWARD'
"""Konstanta pro zp�tn� posun v�'Data.fetchone'."""

def opposite_direction(direction):
    """Vra� sm�r opa�n� k�'direction'.

    'direction' i�vr�cen� v�sledek jsou n�kter� z�konstant 'FORWARD' a
    'BACKWARD'.

    """
    if direction == FORWARD:
        return BACKWARD
    elif direction == BACKWARD:
        return FORWARD
    else:
        raise ProgramError('Invalid direction', direction)


ASCENDENT = 'ASCENDENT'
"""Konstanta pro vzestupn� t��d�n�."""
DESCENDANT = 'DESCENDANT'
"""Konstanta pro sestupn� t��d�n�."""



### Datov� t��dy


class Operator:
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
    def __init__(self, name, *args, **kwargs):
        """Vytvo� oper�tor 'name' s�argumenty 'args' a 'kwargs'.

        Argumenty:

          name -- jm�no oper�toru jako �et�zec
          args -- argumenty oper�toru
          translation -- p�eklad oper�toru na primitivn� oper�tory, instance
            'Operator'
          kwargs -- pojmenovan� (kl��ov�) argumenty oper�toru

        """
        self._name = name
        self._translation = kwargs.pop('translation', None)
        self._args = args
        self._kwargs = kwargs

    def name(self):
        """Vra� jm�no oper�toru zadan� v�konstruktoru."""
        return self._name

    def translation(self):
        """Vra� p�eklad oper�toru zadan� v�konstruktoru."""
        return self._translation

    def args(self):
        """Vra� tuple argument� oper�toru zadan�ch v�konstruktoru."""
        return self._args

    def kwargs(self):
        """Vra� dictionary kl��ov�ch argument� zadan�ch v�konstruktoru."""
        return self._kwargs

    def logical(self):
        """Vra� pravdu, pr�v� kdy� se jedn� o�logick� oper�tor.

        Logick� oper�tory jsou 'AND', 'OR' a 'NOT'.  Ostatn� oper�tory jsou
        rela�n�.
        
        """
        return self._name in ('AND', 'OR', 'NOT')
    
    def _relaxed_args(self):
        """Return self._args, with Value instances transformed to ignore their type attributes.

        So the attributes of data type instances don't figure in the result.
        Only their class is taken into account.  This is necessary to avoid the
        influence of data types attributes on the results of __cmp__ and
        __hash__.

        """
        def relax(arg):
            if isinstance(arg, (Value, WMValue)):
                return (arg.type().__class__, arg.value())
            else:
                return arg
        return [relax(arg) for arg in self._args]

    def __str__(self):
        args = string.join(map(str, self.args()), ', ')
        return '%s (%s)' % (self.name(), args)

    def __cmp__(self, other):
        if sameclass(self, other):
            return cmp(self._name, other._name) or \
                   cmp(self._relaxed_args(), other._relaxed_args()) or \
                   cmp(self._kwargs, other._kwargs)
        else:
            return compare_objects(self, other)

    def __hash__(self):
        return (hash(self._name) ^
                hash(tuple(self._relaxed_args())) ^
                hash(tuple(self._kwargs.items())))


class Data(object_2_5):
    """Z�kladn� abstrakce p��stupu k�tabulkov�m dat�m.

    Tabulkov� data jsou rozd�lena do sloupc� a ��dk�.  T��da umo��uje pracovat
    s���dky dat zp�sobem analogick�m pr�ci s�daty nad rela�n� datab�z�.
    Specifikace sloupc� je fixn� vzhledem k�dan� instanci t��dy.  Jeden nebo
    v�ce sloupc� t��dy jsou definov�ny jako kl��ov�, hodnoty t�chto sloupc�
    v�jednotliv�ch ��dc�ch tyto ��dky jednozna�n� identifikuj�.  Obsah ��dk� je
    dynamick�, z�sk�van� ze zdroje dat, po�et ��dk� se m��e b�hem existence
    instance t��dy m�nit.

    T��da definuje n�sleduj�c� skupiny metod pro p��stup k�dat�m:

    - Metodu pro v�b�r konkr�tn�ho ��dku: 'row()'.  ��dek je vybr�n podle
      zadan�ho kl��e.

    - Metody pro v�b�r v�ech ��dk�: 'select()', 'fetchone()', 'skip()' a
      'close()'.  Po zavol�n� metody 'select()' je mo�no postupn� z�sk�vat
      jednotliv� ��dky pomoc� metody 'fetchone()'.  Tento zp�sob umo��uje
      p�ed�v�n� i�v�t��ho mno�stv� dat z�extern�ch zdroj� bez nutnosti je
      v�echna najednou dr�et v�pam�ti.  Metoda 'select' umo��uje kdykoliv
      zp��stupn�n� dat reinicializovat (\"rewind\").

    - Metody pro modifikaci dat: 'insert()', 'update()' a 'delete()'.

    - Metody pro z�sk�n� informac� o�tabulce: 'columns()' a 'key()'.

    - Metody pro nastaven� callback� p�i modifikaci dat:
      'add_callback_on_change()', 'remove_callback_on_change()'.

    Tato t��da m� charakter abstraktn� t��dy a chov� se jako pr�zdn� tabulka,
    tj. jako read-only tabulka s�definovan�mi sloupci, av�ak ��dn�mi ��dky.
    
    """
    AGG_MIN = 'AGG_MIN'
    """Konstanta z�sk�n� minim�ln� hodnoty pro metodu 'select()'."""
    AGG_MAX = 'AGG_MAX'
    """Konstanta z�sk�n� maxim�ln� hodnoty pro metodu 'select()'."""
    AGG_COUNT = 'AGG_COUNT'
    """Konstanta z�sk�n� po�tu polo�ek pro metodu 'select()'."""
    AGG_SUM = 'AGG_SUM'
    """Konstanta z�sk�n� sou�tu polo�ek pro metodu 'select()'."""
    AGG_AVG = 'AGG_AVG'
    """Konstanta z�sk�n� pr�m�rn� hodnoty polo�ek pro metodu 'select()'."""

    _CACHEABLE = True
    
    class UnsupportedOperation(Exception):
        """Signalizuje, �e byla ��d�na nepodporovan� operace."""
    
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
        super(Data,self).__init__(columns=columns, key=key, ordering=ordering,
                                  **kwargs)
        assert not filter(lambda c: not isinstance(c, ColumnSpec),
                          columns), \
                          'Invalid column specification'
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
        """Vra� 'ColumnSpec' identifikovanou 'id'.

        Pokud takov� specifikace neexistuje, vra� 'None'.
        
        """
        return find(id, self.columns(), key=ColumnSpec.id)
        
    def key(self):
        """Vra� kl��ov� sloupce zadan� v�konstruktoru, jako tuple."""
        return self._key

    def row_key(self, row):
        """Vra� hodnoty kl��e z�'row' jako tuple instanc� 'Value'."""
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
               arguments={}, async_count=False):
        """Initialize selection of records from the data source.
        
        The method itself does not necessarily load any data, the selection is only initialized if
        necessary.  The actual data may be obtained by repetitive calls to 'fetchone()' after
        calling this method.  Rows can also be skipped using the 'skip()' method.  The number of
        corresponding rows is returned if possible, but None may be returned if this feature is not
        implemented.

        Repeated calls to this method may not result in the same data if the data source changes in
        the meanwhile.

        Arguments:

          condition -- condition limiting the set of resulting rows as an 'Operator' instance or
            'None'
          reuse -- boolean flag indicating, that the data of the previous select may be reused if
            the condition matches
          sort -- sequence of sorting specifiers.  Each specifier is a column identifier or a pair
            (ID, DIRECTION).  DIRECTION os one of module constants 'ASCENDENT' or 'DESCENDANT'.
            The default value is 'ASCENDENT'.
          columns -- sequence of IDs of columns to select; if not given, all
            columns are selected
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)
          async_count -- if true, try to count result lines asynchronously
          
        Je-li 'condition' r�zn� od 'None', specifikuje podm�nku pro v�b�r
        ��dk�.  Podt��dy nejsou povinny podm�nky implementovat (mohou je
        neimplementovat v�bec nebo mohou implementovat pouze n�kter� podm�nky),
        v�takov�m p��pad� to mus� b�t uvedeno v�dokumentaci a p�i zad�n�
        nepodporovan� podm�nky mus� metoda vyvolat v�jimku
        'UnsupportedOperation'.

        T��d�n� v�b�ru se prov�d� podle sloupc� uveden�ch v�argumentu 'sort',
        s�prioritou dle jejich po�ad�.  T��d�n� je takt� nepovinn� operace a
        podt��dy nejsou povinny je implementovat; pokud je neimplementuj�, mus�
        to b�t uvedeno v�jejich dokumentaci.
        
        The method always returns 0 in this class.
        
        """
        self._select_last_row_number = -1
        return 0
    
    def select_map(self, function, transaction=None, **kwargs):
        """Aplikuj 'function' na v�echny ��dky v�b�ru a vra� v�sledky.

        Zavolej metodu 'select()' s�argumenty 'kwargs' a na v�echny vr�cen�
        ��dky zavolej funkci 'function'.  V�sledky v�ech vol�n� 'function' vra�
        jako seznam s�po�tem a po�ad�m prvk� odpov�daj�c�ch vr�cen�m datov�m
        ��dk�m.

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
                row = self.fetchone(transaction=transaction)
                if row is None:
                    break
                result.append(function(row))
        finally:
            try:
                self.close()
            except:
                pass
        return result

    def select_aggregate(self, operation, condition=None, transaction=None):
        """Vra� v�slednou hodnotu agrega�n� funkce.

        Metoda prov�d� select, jeho� hodnotou je v�sledek agrega�n� funkce
        kompletn� vybran�ch dat.  Je-li vyvol�na b�hem neuzav�en�ho select,
        tento select nep�eru�uje a je-li podm�nka jej� a aktu�ln�ho select
        shodn�, vrac� v�sledek odpov�daj�c� obsahu onoho selectu (nap��klad
        zpracov�n�m v�jedin� transakci).

        Argumenty:

          operation -- dvojice (OPERATION, COLUMN), kde OPERATION je jednou
            z�'AGG_*' konstant t��dy a COLUMN je id sloupce, nad kter�m m� b�t
            operace provedena.
          condition -- shodn� se stejnojmenn�m argumentem metody 'select()'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Vrac�: Instanci 'Value' odpov�daj�c� po�adovan� agrega�n� funkci.

        Podt��dy nejsou povinny tuto metodu implementovat (mohou ji
        neimplementovat v�bec nebo mohou implementovat pouze n�kter� podm�nky),
        v�takov�m p��pad� to mus� b�t uvedeno v�dokumentaci a p�i zad�n�
        nepodporovan� podm�nky mus� metoda vyvolat v�jimku
        'UnsupportedOperation'.
        
        V�t�to t��d� metoda v�dy vyvol�v� v�jimku 'UnsupportedOperation'.

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
                if (operation == self.AGG_COUNT or
                    isinstance(self.find_column(cid).type(), Number)):
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
            except:
                pass
        return select_result, Row(aggregates)
        
    def fetchone(self, direction=FORWARD, transaction=None):
        """Vra� dal�� ��dek dat.

        Opakovan�m vol�n�m t�to metody je mo�no z�skat v�echny ��dky tabulky.
        Pokud ji� ��dn� dal�� ��dek dat k�dispozici nen�, vra� 'None'.

        Argumenty:

          direction -- jedna z�konstant 'FORWARD' a 'BACKWARD', ur�uje, zda
            m� b�t vr�cen p�edchoz� nebo n�sleduj�c� ��dek
          transaction -- deprecated, don't use anymore

        Ne v�echny podt��dy jsou povinny implementovat vr�cen� p�edchoz�ho
        ��dku (tj. situaci, kdy 'direction==BACKWARD').  Pokud je
        neimplementuj�, mus� to b�t uvedeno v�dokumentaci a metoda mus�
        v�p��pad� odpov�daj�c�ho po�adavku vyvolat v�jimku
        'UnsupportedOperation'.

        Sm�r je vztahov�n k�posledn� vr�cen�mu ��dku.  M�-li tabulka _n_ ��dk�
        a posledn� vr�cen� ��dek byl _k_-t�, kde 0 < _k_ < n+1, znamen�
        'FORWARD' vr�cen� _k+1_n�ho ��dku (pokud existuje) a 'BACKWARD'
        _k-1_n�ho ��dku (pokud existuje).  Dojde-li k�p�ekro�en� prvn�ho ��dku,
        nejbl�e n�sleduj�c� 'FORWARD' vr�t� prvn� ��dek (pokud existuje),
        a�dojde-li k�p�ekro�en� posledn�ho ��dku, nejbl�e n�sleduj�c�
        'BACKWARD' vr�t� posledn� ��dek (pokud existuje).

        Prvn�mu vol�n� t�to metody mus� p�edch�zet vol�n� metody 'select()'.
        Pokud se tak nestane, je chov�n� metody nespecifikov�no.  N�sleduj�c�
        vol�n� metody 'select()' provede reinicializaci natahov�n� dat,
        tj. n�sledn� vol�n� 'fetchone()' za�ne vracet nov� data, op�t od
        za��tku.

        Ka�d�mu vol�n� 'fetchone()' mus� p�edch�zet vol�n� 'select()',
        'fetchone()' nebo 'skip()'.  Je-li b�hem prov�d�n� v�b�ru vol�na jin�
        ve�ejn� (nebo nesouvisej�c� neve�ejn�) metoda t��dy, je chov�n�
        n�sledn�ho vol�n� metody 'fetchone()' bez bezprost�edn�
        p�edch�zej�c�ho vol�n� 'select()' nedefinov�no.

        Po posledn�m vol�n� t�to metody by m�la b�t zavol�na metoda 'close()'.

        V�t�to t��d� metoda v�dy pouze vrac� 'None'.
        
        """
        return None

    def last_row_number(self):
        """Vra� po�ad� ��dku posledn� vr�cen�ho metodou 'fetchone()'.

        ��dky jsou ��slov�ny od�0.  Pokud v�aktu�ln�m selectu dosud nebyl ��dn�
        ��dek p�es 'fetchone()' z�sk�n, vra� -1.

        """
        return self._select_last_row_number
        
    def skip(self, count, direction=FORWARD):
        """P�esko� 'count' ��dk� ve sm�ru 'direction'.

        Argumenty:

          count -- po�et ��dk�, kter� maj� b�t p�esko�eny; mus� to b�t
            nez�porn� integer
          direction -- jedna z�konstant 'FORWARD' a 'BACKWARD', ur�uje sm�r
            pohybu p�i p�eskakov�n�

        Vrac�: Po�et skute�n� p�esko�en�ch ��dk� (m��e b�t ni���, dojde-li
        k�p�ekro�en� hranic dat) nebo 'None', pokud tento �daj nen� zn�m.

        Pokud p�i sk�k�n� dojde k�p�ekro�en� hranic dat, nadbyte�n�
        p�eskakov�n� ��dk� se ignoruje.

        Metodu je mo�no pou��t pouze b�hem otev�en�ho selectu (viz metody
        'select()' a 'close()').

        V�t�to t��d� metoda p�eskakuje ��dky prost�ednictv�m vol�n� metody
        'fetchone()' a vrac� po�et ��dk�, pro kter� tato vol�n� nevr�tila
        'None'.
        
        """
        for i in range(count):
            if self.fetchone(direction) == None:
                return i
        return count

    def rewind(self):
        """Vra� se p�ed za��tek dat aktu�ln�ho selectu.

        Metodu je mo�no pou��t pouze b�hem otev�en�ho selectu (viz metody
        'select()' a 'close()').

        """
        while self.fetchone(BACKWARD) != None:
            pass

    def search(self, condition, direction=FORWARD, transaction=None):
        """Vyhledej nejbli��� v�skyt ��dku spl�uj�c�ho 'condition'.

        Argumenty:

          condition -- podm�nkov� v�raz, kter� mus� hledan� prvek spl�ovat,
            instance t��dy 'Operator'
          direction -- sm�r vyhled�v�n�, jedna z�konstant 'FORWARD' a
            'BACKWARD'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Vrac�: Vzd�lenost hledan�ho ��dku od aktu�ln�ho ��dku v�po�tu ��dk�
        jako kladn� integer.  Pokud ��dek nebyl nalezen, je vr�cena 0.

        Aktu�ln� ��dek je ��dek, na kter� ukazuje ukazov�tko.  Tj. nap��klad
        ��dek bezprost�edn� p�edt�m vyta�en� metodou 'fetchone()'.

        Metodu je mo�no pou��t pouze b�hem otev�en�ho selectu (viz metody
        'select()' a 'close()'), vyhled�v�n� se prov�d� pouze mezi ��dky
        tohoto selectu.  Pozice ukazov�tka selectu nen� touto metodou zm�n�na.
        
        Podt��dy nejsou povinny tuto metodu implementovat (mohou ji
        neimplementovat v�bec nebo mohou implementovat pouze n�kter� podm�nky),
        v�takov�m p��pad� to mus� b�t uvedeno v�dokumentaci a p�i zad�n�
        nepodporovan� podm�nky mus� metoda vyvolat v�jimku
        'UnsupportedOperation'.
        
        V�t�to t��d� metoda v�dy vyvol�v� v�jimku 'UnsupportedOperation'.

        """
        raise self.UnsupportedOperation()

    def search_key(self, key, direction=FORWARD):
        """Stejn� jako 'search()', ale hled� podle 'key' m�sto podm�nky.

        Argumenty:

          key -- sekvence hodnot kl��e
          direction -- stejn� jako v�'search()'

        """
        eqs = map(lambda c, k: EQ(c.id(), k), self.key(), key)
        condition = apply(AND, eqs)
        return self.search(condition, direction=direction)

    def close(self):
        """Ukon�i aktu�ln� select.

        Tato metoda umo��uje explicitn� uzav��t aktu�ln� �ten� dat pomoc�
        'select()' + 'fetchone()' a uvolnit tak ji� p��padn� d�le nepot�ebn�
        datov� struktury nebo prost�edky syst�mu.

        Metoda m��e b�t vol�na pouze kdy� je aktivn� 'select()' spojen�.
        Pokud tomu tak nen�, je jej� chov�n� nedefinov�no.

        V�t�to t��d� tato metoda ned�l� nic.
        
        """
        self._select_last_row_number = None
    
    def insert(self, row, after=None, before=None, transaction=None):
        """Vlo� 'row' do tabulky.

        Argumenty:
        
          row -- instance t��dy 'Row'
          after -- 'None' nebo kl�� ��dku, za kter� m� b�t nov� ��dek vlo�en
          before -- 'None' nebo kl�� ��dku, p�ed kter� m� b�t nov� ��dek vlo�en
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Argumenty 'after' a 'before' maj� smysl pouze tehdy, pokud byl
        v�konstruktoru specifikov�n argument 'ordering'.  Nesm� b�t
        specifikov�ny oba sou�asn�.  Pokud ��dek ur�en� n�kter�m z�t�chto
        argument� neexistuje, nen� nov� ��dek vlo�en.  Pokud je v�nov�m ��dku
        uvedena hodnota po�adov�ho sloupce, je tato ignorov�na.  Pokud m�
        datov� objekt ordering a oba argumenty 'after' a 'before' jsou 'None',
        nov� ��dek m��e b�t vlo�en na kteroukoliv pozici.
        
        'row' nemus� obsahovat obsahovat hodnoty pro v�echny sloupce tabulky a
        m��e obsahovat i�dal�� sloupce, kter� tabulka neobsahuje.  Z�le�� na
        implementaci tabulkov� t��dy, jak s�takov�m sloupcem nalo�� -- m��e
        nap��klad chyb�j�c� sloupce doplnit implicitn�mi hodnotami nebo je
        dopo��tat nebo tak� m��e sloupec odm�tnout do tabulky vlo�it.  V�ka�d�m
        p��pad� plat�, �e po�ad� sloupc� v�'row' je z�hlediska t�to metody
        nepodstatn�, sloupce jsou rozezn�v�ny sv�mi n�zvy.

        Pokud 'row' obsahuje kl�� a ��dek s�takov�m kl��em ji� v�tabulce
        existuje, neprov�d�j nic.  Pakli�e 'row' obsahuje pouze n�kter� a ne
        v�echny sloupce kl��e, z�le�� na konkr�tn� implementaci, zda jej vlo��
        �i nikoliv.
        
        Nen� specifikov�no, na kter� m�sto tabulky m� b�t ��dek vlo�en, obvykle
        je to konec tabulky, nen� to v�ak vy�adov�no.

        Je-li sloupec do tabulky �sp�n� vlo�en, vra��dvojici (ROW, 'True'),
        kde ROW je skute�n� nov� ��dek tabulky, kter� na z�klad� vlo�en� 'row'
        vzniknul.  Pokud nem��e b�t obsah nov�ho ��dku zji�t�n, vra� dvojici
        ('None', 'True').  Pokud ��dek nebyl do tabulky �sp�n� vlo�en, vra�
        dvojici ('None', 'False') v p��pad�, �e popis chyby nen� definov�n,
        p��p. dvojici (text, 'False'), kde text je popis p���iny ne�sp�chu.

        V�t�to t��d� metoda v�dy pouze vrac� ('None', 'False').
        
        """
        return None, False

    def update(self, key, row, transaction=None):
        """Nahra� ��dek identifikovan� kl��em 'key' ��dkem 'row'.

        Argumenty:
        
          key -- instance nebo seznam instanc� t��dy 'types._Value', mus�
            odpov�dat v�em sloupc�m kl��e tabulky
          row -- instance t��dy 'Row'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        'row' nemus� obsahovat hodnoty pro v�echny sloupce tabulky a m��e
        obsahovat i�dal�� sloupce, kter� tabulka neobsahuje.  Z�le�� na
        implementaci tabulkov� t��dy, jak s�takov�m sloupcem nalo�� -- m��e
        nap��klad chyb�j�c� sloupce doplnit implicitn�mi hodnotami nebo je
        dopo��tat nebo tak� m��e sloupec odm�tnout do tabulky vlo�it.  V�ka�d�m
        p��pad� plat�, �e po�ad� sloupc� v�'row' je z�hlediska t�to metody
        nepodstatn�, sloupce jsou rozezn�v�ny sv�mi n�zvy.

        Pokud 'key' v�tabulce neidentifikuje ��dn� existuj�c� ��dek, neprov�d�j
        nic a vra� 'None'.  Pokud 'row' obsahuje kl�� r�zn� od 'key' a ��dek
        s�takov�m kl��em ji� v�tabulce existuje, neprov�d�j nic a vra� 'None'.
        Pakli�e 'row' obsahuje pouze n�kter� a ne v�echny sloupce kl��e, z�le��
        na konkr�tn� implementaci, jak s�t�m nalo��.

        Byl-li pro datov� objekt v�konstruktoru specifikov�n argument
        'ordering', nen� po�ad� ��dku updatem zm�n�no.  P��padn� nov� zadan�
        hodnota kter�hokoliv \"ordering\" sloupce je ignorov�na a je nahrazena
        starou hodnotou.  Z�toho plyne, �e updatem nen� mo�no zm�nit hodnotu
        ��dn�ho sloupce uveden�ho v�'ordering'.

        Je-li sloupec �sp�n� updatov�n, vra� dvojici (ROW, 'True'), kde ROW
        je�skute�n� updatovan� ��dek tabulky, kter� na z�klad� vlo�en� 'row'
        vzniknul.  Pokud takov� ��dek nelze zjistit, vra� ('None', 'True').
        V�opa�n�m p��pad� (tj. pokud ��dek s�'key' neexistuje nebo pokud je
        problematick� kl�� 'row', jak je uvedeno v��e) vra� dvojici
        ('None', 'False') v p��pad�, �e popis chyby nen� definov�n,
        p��p. dvojici (text, 'False'), kde text je popis p���iny ne�sp�chu.
        
        V�t�to t��d� metoda v�dy pouze vrac� dvojici ('None', 'False').
        
        """
        return None, False

    def update_many(self, condition, row, transaction=None):
        """Nahra� ��dky identifikovan� 'condition' daty 'row'.

        Argumenty:
        
          key -- instance nebo seznam instanc� t��dy 'types._Value', mus�
            odpov�dat v�em sloupc�m kl��e tabulky; ur�uje ��dek, kter� m� b�t
            smaz�n
          row -- instance t��dy 'Row'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        'row' nemus� obsahovat hodnoty pro v�echny sloupce tabulky a m��e
        obsahovat i�dal�� sloupce, kter� tabulka neobsahuje.  Z�le�� na
        implementaci tabulkov� t��dy, jak s�takov�m sloupcem nalo�� -- m��e
        nap��klad chyb�j�c� sloupce doplnit implicitn�mi hodnotami nebo je
        dopo��tat nebo tak� m��e sloupec odm�tnout do tabulky vlo�it.  V�ka�d�m
        p��pad� plat�, �e po�ad� sloupc� v�'row' je z�hlediska t�to metody
        nepodstatn�, sloupce jsou rozezn�v�ny sv�mi n�zvy.

        Pokud by zm�na ��dk� m�la m�t za n�sledek zm�nu kl��e n�kter�ho ��dku
        na kl�� po updatu existuj�c� v�jin�m ��dku, je vyvol�na v�jimka a data
        nejsou zm�n�na.

        Byl-li pro datov� objekt v�konstruktoru specifikov�n argument
        'ordering', nen� po�ad� ��dku updatem zm�n�no.  P��padn� nov� zadan�
        hodnota kter�hokoliv \"ordering\" sloupce je ignorov�na.  Z�toho plyne,
        �e updatem nen� mo�no zm�nit hodnotu ��dn�ho sloupce uveden�ho
        v�'ordering'.

        Vrac�: Po�et updatovan�ch ��dk�.
        
        V�t�to t��d� metoda v�dy pouze vrac� '0'.
        
        """
        return 0
    
    def delete(self, key, transaction=None):
        """Sma� ��dek identifikovan� 'key'.

        Argumenty:
        
          key -- instance nebo seznam instanc� t��dy 'types._Value', mus�
            odpov�dat v�em sloupc�m kl��e tabulky; ur�uje ��dek, kter� m� b�t
            smaz�n
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Pokud je ��dek smaz�n, vra��1.
        Pokud takov� ��dek nelze smazat (v�etn� situace, kdy neexistuje),
        ned�lej nic a vra��0.

        V�t�to t��d� metoda v�dy pouze vrac�0.
        
        """
        return 0

    def delete_many(self, condition, transaction=None):
        """Sma� ��dky identifikovan� 'condition'.
        
        Argumenty:
        
          condition -- podm�nka odpov�daj�c� ��dk�m, kter� maj� b�t
            smaz�ny; instance t��dy 'Operator'
          transaction -- transaction object encapsulating the database
            operation environment or 'None' (meaning default environment)

        Vrac�: Po�et smazan�ch ��dk�.

        V�t�to t��d� metoda v�dy pouze vrac�0.

        """
        return 0

    def change_number(self):
        """Vra� po�et dosud zaregistrovan�ch zm�n dat.

        Tento po�et je v�t�� nebo roven 0 a ka�d� n�sleduj�c� n�vratov� hodnota
        je v�t�� nebo rovna p�edchoz� n�vratov� hodnot�.  Nic jin�ho
        garantovan�ho nen�, zejm�na ne �e po�et odpov�d� skute�n�mu po�tu zm�n
        dat, jedn� se pouze o�orienta�n� hodnotu.  Pro podrobn�j�� diskusi
        spolehlivosti viz dokumentace metody `add_callback_on_change()'.
        
        """
        return self._change_number.current()
    
    def add_callback_on_change(self, function):
        """Zaregistruj 'function' do seznamu modifika�n�ch callback�.

        Dojde-li k�ozn�men� o�modifikaci dat, m��e pak b�t 'function' zavol�na,
        bez parametr�.  Zda je 'function' skute�n� zavol�na, z�vis� na
        konkr�tn� implementaci datov� t��dy.  N�kter� implementace mohou toto
        vol�n� garantovat, n�kter� mohou vol�n� prov�d�t bez garance a n�kter�
        nemus� vol�n� prov�d�t nikdy.  Tato zp�tn� vol�n� jsou tedy pouze
        pomocn�m mechanismem, kter� m��e b�t pro ur�it� ��ely (nap��klad
        p�ekreslen� tabulky na obrazovce) u�ite�n�, nelze na n�j v�ak spol�hat.

        **Pozor:** Vzhledem k�tomu, �e se jedn� o�callbacky, bude 'function'
        obvykle vol�na v�samostatn�m threadu, je v�n� tedy t�eba db�t na
        p��padn� mo�n� kolize s�hlavn�m threadem.

        V�t�to t��d� k���dn�m modifikac�m dat nedoch�z� a tud� se ani
        neprov�d� zp�tn� vol�n�.

        """
        self._on_change_callbacks.append(function)

    def remove_callback_on_change(self, function):
        """Odstra� 'function' ze seznamu modifika�n�ch callback�.

        Pokud 'function' v�seznamu modifika�n�ch callback� nen�, ned�lej nic.
        
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
    """Abstrakce p��stupu ke gener�toru tickets.

    Jedn� se o�jednoduch� ��ta� vracej�c� postupn� sekvenci unik�tn�ch
    vzestupn�ch ��sel, bl�e viz metoda 'next()'.

    """    
    def next(self):
        """Vra� dal�� hodnotu ��ta�e jako integer.

        V�t�to t��d� metoda pouze vyvol�v� 'NotImplementedException'.
        
        """
        raise NotImplementedException()


class Function(object):
    """Abstrakce datab�zov�ch funkc�.

    Podporov�ny jsou pouze jednoduch� funkce, p�ij�maj�c� pevn� (ale libovoln�)
    po�et argument� dan�ch svoj� pozic� a vracej�c� seznam ��dk�.

    """
    def call(self, row):
        """Zavolej funkci a vra� v�sledek.

        Argumenty:

          row -- instance t��dy 'Row' odpov�daj�c� argument�m funkce, je
            d�le�it� po�ad� prvk� v�'row'

        Vrac�: Sekvenci instanc� t��dy 'Row' odpov�daj�c� v�sledku vol�n�
        funkce.

        """
        raise NotImplementedException()



class MemData(Data):
    """Data dr�en� v�pam�ti.

    T��da slou�� jako jednoduch� datov� objekt, kter� ��dky sv�ch dat dr��
    v�pam�ti.  Je ur�ena p�edev��m pro lad�n� a testov�n�.

    T��da nen� thread-safe.

    Modifika�n� metody nevolaj� ��dn� callbacky.
    
    """

    _CACHEABLE = False

    def __init__(self, columns, data=(), **kwargs):
        """Inicializuj datov� zdroj dle specifikace 'columns'.

        'columns' jsou stejn� jako v�p�edkovi.  Kl��em je v�dy prvn� sloupec
        'columns', z��eho� vypl�v�, �e 'columns' nesm� b�t pr�zdn�.

        Argument 'data' m��e obsahovat sekvenci instanc� 'Row', kter�mi m� b�t
        inicializov�n datov� objekt.
        
        """
        super(MemData, self).__init__(columns, columns[0], **kwargs)
        self._mem_data = []
        self._mem_cursor = -1
        for row in data:
            self.insert(row)

    def _mem_find_index(self, key):
        if is_sequence(key):
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
        except:
            return None
        i = self._mem_find_index(key) 
        if index != None and i != None and i != index:
            return None
        pairs = []
        for c in self.columns():
            id = c.id()
            try:
                val = row[id]
            except:
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
        if index == None:
            return None
        return self._restrict_row_columns(self._mem_data[index], columns)

    def rewind(self):
        self._mem_cursor = -1

    def _condition2pyfunc(self, condition):
        if condition is None:
            return lambda row: True
        op_name = condition.name()
        relational_operators = {'EQ': operator.eq,
                                'LT': operator.lt}
        if op_name in relational_operators.keys():
            def relop(row, op, args, kwargs):
                def arg(a, ignore_case=False):
                    if isinstance(a, str):
                        v = row[a].value()
                    else:
                        v = a.value()
                    if ignore_case and isinstance(v, (str, unicode)):
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
            return lambda row: reduce(lambda r, f: r and f(row), fctns, True)
        else:
            t = condition.translation()
            if t is not None:
                return self._condition2pyfunc(t)
            else:
                raise ProgramError("Operator not supported:", op_name)

    def select(self, condition=None, reuse=False, sort=None, columns=None, transaction=None,
               arguments={}, async_count=False):
        """Inicializace vytahov�n� z�znam�.

        Bli��� popis viz nadt��da.  Argumenty 'condition', 'sort',
        'transaction' a 'arguments' jsou ignorov�ny.
        
        """
        if self._condition is not None:
            if condition is not None:
                condition = AND(condition, self._condition)
            else:
                condition = self._condition
        cond = self._condition2pyfunc(condition)
        self._mem_cursor = -1
        self._mem_select = [self._restrict_row_columns(row, columns)
                            for row in self._mem_data if cond(row)]
        return len(self._mem_select)

    def close(self):
        self._mem_select = []
        
    def fetchone(self, direction=FORWARD, transaction=None):
        cursor = self._mem_cursor
        data = self._mem_select
        if direction == FORWARD:
            if cursor >= len(data):
                return None
            elif cursor == len(data) - 1:
                self._mem_cursor = cursor + 1
                return None
            else:
                self._mem_cursor = cursor = cursor + 1
                return data[cursor]
        else:
            if cursor < 0:
                return None
            elif cursor == 0:
                self._mem_cursor = -1
                return None
            else:
                self._mem_cursor = cursor = cursor - 1
                return data[cursor]
            
    def last_row_number(self):
        return self._mem_cursor

    def insert(self, row, transaction=None):
        """Vlo� 'row' do tabulky.

        Pro bli��� popis viz nadt��da.

        'row' je vlo�en na konec tabulky.  Chyb�j�c� sloupce jsou nastaveny na
        'None'.

        Argument 'transaction' je ignorov�n.
        
        """
        assert isinstance(row, Row)
        new_row = self._mem_create_row(row)
        if new_row == None:
            return None, False
        self._mem_data.append(new_row)
        return new_row, True

    def update(self, key, row, transaction=None):
        """Updatuj 'row' v�tabulce.

        Pro bli��� popis viz nadt��da.

        'row' je v�tabulce vlo�en na m�sto ��dku identifikovan�ho 'key''.
        Chyb�j�c� sloupce jsou nastaveny na 'None'.
        
        Argument 'transaction' je ignorov�n.

        """
        index = self._mem_find_index(key)
        if index == None:
            return None, False
        new_row = self._mem_create_row(row, index)
        if new_row == None:
            return None, False
        self._mem_data[index] = new_row
        return new_row, True
        
    def delete(self, key, transaction=None):
        """
        Argument 'transaction' is ignored.
        """
        index = self._mem_find_index(key)
        if index == None:
            return 0
        del self._mem_data[index]
        return 1


### Pomocn� funkce


def EQ(x, y, ignore_case=False):
    """Podm�nkov� oper�tor 'EQ' relace rovnosti '='.

    Argumenty:

      x -- column identifier, string, or an 'OpFunction' value
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je primitivn�.
      
    """
    return Operator('EQ', x, y, ignore_case=ignore_case)

def NE(x, y, ignore_case=False):
    """Podm�nkov� oper�tor nerovnosti.

    Argumenty:

      x -- column identifier, string, or an 'OpFunction' value
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.
      
    """
    t = NOT(EQ(x, y, ignore_case=ignore_case))
    return Operator('NE', x, y, ignore_case=ignore_case, translation=t)
    
def WM(x, y, ignore_case=True):
    """Podm�nkov� oper�tor 'WM' (\"wildcard matches\") porovn�n� dle vzoru.

    Argumenty:

      x -- column identifier, string, or an 'OpFunction' value
      y -- instance 'WMValue' definuj�c� vzor; jej� hodnota m��e jako wildcars
        obsahovat znaky '*' (lze substituovat ��mkoliv) a '?' (lze substituovat
        libovoln�m znakem)
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je primitivn�.
      
    """
    return Operator('WM', x, y, ignore_case=ignore_case)
    
def NW(x, y, ignore_case=True):
    """Podm�nkov� oper�tor negace porovn�n� dle vzoru.

    Argumenty:

      x -- column identifier, string, or an 'OpFunction' value
      y -- string definuj�c� vzor; jako wildcars m��e obsahovat znaky '*' (lze
        substituovat ��mkoliv) a '?' (lze substituovat libovoln�m znakem)
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.
      
    """
    t = NOT(WM(x, y, ignore_case=ignore_case))
    return Operator('NW', x, y, ignore_case=ignore_case, translation=t)

def LT(x, y, ignore_case=False):
    """Podm�nkov� oper�tor 'LT' relace '<'.

    Argumenty:

      x -- column identifier, string, or an 'OpFunction' value
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je primitivn�.
      
    """
    return Operator('LT', x, y, ignore_case=ignore_case)

def LE(x, y, ignore_case=False):
    """Podm�nkov� oper�tor relace '<='.

    Argumenty:

      x -- column identifier, string, or an 'OpFunction' value
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.
      
    """
    t = OR(LT(x, y, ignore_case=ignore_case),
           EQ(x, y, ignore_case=ignore_case))
    return Operator('LE', x, y, ignore_case=ignore_case, translation=t)
    

def GT(x, y, ignore_case=False):
    """Podm�nkov� oper�tor relace '>'.

    Argumenty:

      x -- column identifier, string, or an 'OpFunction' value
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.

    """
    t = AND(NOT(EQ(x, y, ignore_case=ignore_case)),
            NOT(LT(x, y, ignore_case=ignore_case)))
    return Operator('GT', x, y, ignore_case=ignore_case, translation=t)

def GE(x, y, ignore_case=False):
    """Podm�nkov� oper�tor relace '>='.

    Argumenty:
    
      x -- column identifier, string, or an 'OpFunction' value
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.

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
    t = NOT(apply(AND, map(NOT, args)))
    return Operator('OR', *args, **{'translation': t})

def IN(column_id, data, table_column_id, table_condition):
    """Podm�nkov� oper�tor p��slu�nosti.

    Argumenty:

      column_id -- jm�no sloupce (string), kter� m� b�t prvkem mno�iny
      data -- instance t��dy 'Data' odpov�daj�c� tabulce, ze kter� m� b�t
        proveden v�b�r mno�iny
      table_column_id -- jm�no sloupce (string) 'table', ze kter�ho m� b�t
        proveden v�b�r mno�iny
      table_condition -- podm�nka omezuj�c� v�b�r z�'table'

    Tento oper�tor je primitivn�.

    """
    return Operator('IN', column_id, data, table_column_id, table_condition)

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
    """Vra� specifikaci t��d�n� reverzn� ke specifikaci 'sorting'.

    Prvky set��d�n� dle vr�cen� hodnoty maj� p�esn� obr�cen� po�ad� ne� prvky
    set��d�n� dle 'sorting'.

    """
    def revspec(spec):
        if is_sequence(spec):
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



# Pomocn� t��dy


class ColumnSpec:
    """Specifikace sloupce tabulkov�ch dat.

    Ka�d� sloupec je identifikov�n sv�m n�zvem (string) a typem (instance t��dy
    'types_.Type').  Bl�e viz metoda '__init__()'.

    Instance t��dy jsou pova�ov�ny za immutable, tud� je mo�no je sd�let.
    
    """
    def __init__(self, id, type):
        """Inicializuj specifikaci sloupce.

        Argumenty:
        
          id -- string identifikuj�c� sloupec (n�zev sloupce)
          type -- typ sloupce, instance t��dy 'types_.Type'

        N�zvem sloupce m��e b�t libovoln� �et�zec, nedoporu�uje se v�ak
        pou��vat pr�zdn� �et�zec jako regul�rn� n�zev sloupce.

        """
        self._id = id
        self._type = type

    def __str__(self):
        return '<Column: id=%s, type=%s>' % (self.id(), self.type())
        
    def __cmp__(self, other):
        """Vra� 0, pr�v� kdy� 'self' a 'other' jsou shodn�.

        'self' a 'other' jsou shodn� pr�v� kdy� jsou t�e t��dy a maj� stejn�
        id a typ.

        """
        if sameclass(self, other):
            res = compare_objects(self.id(), other.id())
            if res:
                return res
            else:
                return compare_objects(self.type(), other.type())
        else:
            return compare_objects(self, other)

    def __hash__(self):
        return hash(self._id)

    def id(self):
        """Vra� string identifikuj�c� sloupec zadan� v�konstruktoru."""
        return self._id

    def type(self):
        """Vra� typ sloupce jako instanci t��dy 'types_.Type'."""
        return self._type


class Row:
    """Reprezentace jednoho ��dku ��dkov�ch dat.

    V�podstat� se jedn� o�uspo��dan� seznam sloupc� (polo�ek) a jejich hodnot.
    Data jsou identifikov�na bu�to sv�m po�ad�m, nebo n�zvem p��slu�n�ho
    sloupce.  V�prvn�m p��pad� se na n� lze odkazovat ��seln�m odkazem tak jako
    u�polo�ek seznam�, v�druh�m p��pad� stringy, tak jako u�hash tabulek.

    Hodnotami sloupc� jsou instance t��dy 'types_.Value'.
    
    T��da emuluje sekvenci i�hash tabulku.  Krom� t�to emulace t��da
    neposkytuje ��dn� dal�� metody.  Inici�ln� data mus� b�t p�ed�na
    v�konstruktoru, pozd�ji lze modifikovat ji� jen existuj�c� sloupce.
    Maz�n� sloupc� nen� mo�n�.
    
    """
    def __init__(self, data=()):
        """Inicializuj ��dek.

        Argumenty:
        
          data -- sekvence dvouprvkov�ch sekvenc�, ka�d� dvouprvkov� sekvence
            obsahuje jako sv�j prvn� prvek identifik�tor sloupce (string) a
            jako druh� prvek jeho hodnotu (instance t��dy 'Value'); argument
            nen� nutno kl��ovat

        P��klad obsahu 'data':
        
          (('poradi', Value(Integer.make(), 1)),
           ('popis', Value(String.make(), 'prvni prvek')))
          
        """
        if __debug__:
            assert is_sequence(data), ("Argument must be a sequence", data)
            for item in data:
                assert is_sequence(item) and len(item) == 2, \
                       ('Column definition must be (ID, VALUE) pair', item)
                k, v = item
                assert is_string(k), ('Invalid column id', k)
                assert isinstance(v, Value), ('Invalid column value', v)
        self._data = list(data)

    def _index(self, key):
        if is_string(key):
            data = self._data
            for i in range(len(data)):
                if data[i][0] == key:
                    result = i
                    break
            else:
                raise KeyError(key)
        elif type(key) == type(0) or type(key) == type(0L):
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
        if type(state) != types.ListType:
            raise InvalidAccessError('Invalid row data', state)
        for k, v in state:
            if type(k) != types.StringType:
                raise InvalidAccessError('Invalid row key', k)
            if not isinstance(v, Value):
                raise InvalidAccessError('Invalid row value', v)
        self._data = state
        
    def __unicode__(self):
        items = [self._data[i][0] + '==' + unicode(item)
                 for i, item in enumerate(self)]
        return '<Row: %s>' % ', '.join(items)

    def __cmp__(self, other):
        """Vra� 0, pr�v� kdy� 'self' a 'other' obsahuj� stejn� n�zvy a hodnoty.

        Hodnoty a n�zvy mus� b�t stejn� v�etn� sv�ho po�ad�.
        
        """
        if sameclass(self, other):
            l1 = len(self)
            l2 = len(other)
            if l1 < l2:
                return -1
            elif l1 > l2:
                return 1
            else:
                data1 = self._data
                data2 = other._data
                for i in range(l1):
                    k1, v1 = data1[i]
                    k2, v2 = data2[i]
                    x = cmp(k1, k2)
                    if x:
                        return x
                    y = cmp(v1, v2)
                    if y:
                        return y
                else:
                    return 0
        else:
            return compare_objects(self, other)
        
    def __len__(self):
        """Vra� po�et sloupc� v���dku."""
        return len(self._data)

    def __getitem__(self, key):
        """Vra� sloupec dle 'key'.

        Jestli�e 'key' je string odpov�daj�c� n�zvu existuj�c�ho sloupce, vra�
        hodnotu dan�ho sloupce.
        Jestli�e 'key' je integer odpov�daj�c� sloupci analogicky jako
        u�sekvenc�, vra� hodnotu dan�ho sloupce.
        Jinak vyvolej v�jimku.
        
        """
        index = self._index(key)
        return self._data[index][1]

    def __setitem__(self, key, value):
        """Nastav hodnotu existuj�c�ho sloupce 'key' na 'value'.

        Argumenty:
        
          key -- mus� b�t existuj�c� kl��, a� u� string jako n�zev sloupce
            nebo integer jako po�ad� sloupce
          value -- nov� hodnota sloupce, instance t��dy 'types_.Value'

        Jestli�e sloupec identifikovan� 'key' neexistuje, je chov�n� metody
        nedefinov�no.

        Zm�na sloupce se nepropaguje do instanc�, je� jsou kopiemi t�to
        instance.
          
        """
        index = self._index(key)
        self._data = data = copy.copy(self._data)
        data[index] = (data[index][0], value)

    def __getslice__(self, i, j):
        """Vra� po�adovan� slice jako instanci t�to t��dy."""
        return self.__class__(self._data[i:j])

    def __setslice__(self, i, j, data):
        """Nastav po�adovan� slice na 'data'.

        'data' je ve stejn�m form�tu jako argument metody '__init__()'.
        'data' mus� m�t stejnou d�lku jako p�vodn� 'slice', jinak je
        vyvol�na v�jimka.
        
        """
        n = len(data)
        if n != j - i:
            raise IndexError("Sequence length doesn't match")
        for k in range(n):
            self[i+k] = data[k]

    def __contains__(self, key):
        """Vra� pravdu, pr�v� kdy� ��dek obsahuje sloupec jm�na 'key'."""
        return key in self.keys()
    
    def has_key(self, key):
        return self.__contains__(key)

    def keys(self):
        """Vra� seznam n�zv� v�ech sloupc� jako strings.

        Po�ad� polo�ek vr�cen�ho seznamu je nedefinov�no.
        
        """
        return [c[0] for c in self._data]

    def items(self):
        """Vra� seznam dvojic [ID, VALUE] odpov�daj�c�ch v�em sloupc�m.

        ID je �et�zec, VALUE je instance t��dy 'types_.Value'.

        Po�ad� dvojic vr�cen�ho seznamu je nedefinov�no.
        
        """
        return map(copy.copy, self._data)

    def columns(self, keys):
        """Vra� hodnoty sloupc� odpov�daj�c� 'keys'.

        Argumenty:

          keys -- sekvence kl��� sloupc�, viz t� metoda '__getitem__()'

        Vrac�: tuple hodnot sloupc�, jejich� kl��e jsou d�ny argumentem 'keys',
        v�odpov�daj�c�m po�ad�.

        """
        values = map(lambda k, self=self: self[k], keys)
        return tuple(values)

    def append(self, key, value):
        """P�ipoj na konec ��dku sloupec 'key' s�hodnotou 'value'.

        Argumenty:

          key -- id sloupce jako string, nesm� b�t shodn� s id ��dn�ho sloupce
            v�instanci ji� p��tomn�ho
          value -- hodnota sloupce jako instance t��dy 'types_.Value'

        """
        assert is_string(key)
        assert isinstance(value, Value)
        assert not some(lambda x, k=key: x[0] == k, self._data)
        self._data.append((key, value))
        
    def update(self, dict):
        """Updatuj hodnoty sloupc� hodnotami z�'dict'.

        Argumenty:

          dict -- slovn�k kl��ovan� n�zvy sloupc� obsahuj�c� nov� hodnoty pro
            tyto sloupce jako instance t��dy 'types_.Value'.

        Sloupc�m v�'dict' neobsa�en�m z�stanou zachov�ny jejich p�vodn�
        hodnoty.
        
        """
        for k in dict.keys():
            assert isinstance(dict[k], Value), \
                   ('Invalid column value', dict[k])
            if k in self:
                self[k] = dict[k]


class DataFactory(object):
    """Factory na tvorbu datov�ch objekt� dle zadan� specifikace.

    V�konstruktoru t��dy jsou zad�ny parametry datov�ho objektu, jeho� instance
    lze posl�ze vytv��et pomoc� metody 'create()'.  Hlavn� m�sto pou�it� t�to
    t��dy jsou specifika�n� soubory na��tan� resolverem.  Resolver z�sk� ze
    statick�ch specifikac� instanci factory a s�jej� pomoc� pak na vy��d�n�, po
    p��padn�m dopln�n� dynamick�ch specifikac�, vytv��� instance datov�ch
    objekt�.

    """
    _data_object_cache = None

    def __init__(self, class_, *args, **kwargs):
        """Inicializuj instanci

        Argumenty:

          class_ -- t��da 'Data' nebo jej� podt��da
          args -- tuple argument�, kter� maj� b�t p�i vytv��en� instance
            p�ed�ny konstruktoru t��dy 'class_'
          kwargs -- dictionary kl��ovan�ch argument�, kter� maj� b�t p�i
            vytv��en� instance p�ed�ny konstruktoru t��dy 'class_'

        """
        assert issubclass(class_, Data)
        self._class_ = class_
        def adjust(arg):
            if type(arg) == types.ListType:
                arg = tuple(arg)
            return arg
        self._args = tuple([adjust(a) for a in args])
        self._kwargs = kwargs
        self._kwargs_hashable = kwargs.items()
        if DataFactory._data_object_cache is None and class_.cacheable():
            DataFactory._data_object_cache = \
              LimitedCache(DataFactory._get_data_object)

    def class_(self):
        """Vra� t��du datov�ho objektu."""
        return self._class_
            
    def create(self, **kwargs):
        """Vytvo� a vra� novou instanci datov�ho objektu.

        Instance je vytvo�ena dle specifikace t��dy a argument� jej�ho
        konstruktoru zadan�ch v�'__init__()'.

        Argumenty:

          kwargs -- dictionary kl��ovan�ch argument�, kter� maj� b�t dodate�n�
            p�ed�ny konstruktoru datov� t��dy.  Je-li n�kter� argument zad�n
            zde i�v�'__init__()', tak vy��� prioritu m� ten zdej��.

        Tato metoda vrac� v�dy zbrusu novou instanci datov� t��dy.

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
        # TODO: St�le je�t� m�me probl�m, nyn� u validity_condition
        # if cacheable:      
        if False: 
            data_object = cache[key]
            result = copy.copy(data_object)
            ftype = type(identity)
            for attr in result.__dict__.keys():
                try:
                    result.__dict__[attr] = copy.copy(result.__dict__[attr])
                except:
                    pass
            result.after_init()
        else:
            result = self._class_(*self._args, **_kwargs)
        return result
    
    def __str__(self):
        return '<DataFactory: class=%s, args=%s, kwargs=%s>' % \
               (self._class_, deepstr(self._args), self._kwargs)

    def _get_data_object(key):
        class_, args, kwargs = key
        kwargs = dict(kwargs)
        kwargs['full_init'] = False
        return class_(*args, **kwargs)
    _get_data_object = staticmethod(_get_data_object)

    def access_rights(self):
        return self._kwargs.get('access_rights', None)

    # "Podrobn�" porovn�v�n� data factories je p��li� n�ro�n� na CPU.
#     def __cmp__(self, other):
#         return compare_attr(self, other, ('_class_', '_args', '_kwargs'))
#     def __hash__(self):
#         return hash_attr(self, ('_class_', '_args', '_kwargs_hashable'))


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
        if is_sequence(spec):
            id, type_ = spec
        else:
            id, type_ = spec, None
        return pytis.data.DBColumnBinding(id,  table, id, type_=type_)
    bindings = [binding(spec) for spec in columns]
    factory = pytis.data.DataFactory(pytis.data.DBDataDefault, bindings, bindings[0],
                                     arguments=arguments, sql_logger=sql_logger)
    data = factory.create(connection_data=connection_data, connection_name=connection_name)
    return data
