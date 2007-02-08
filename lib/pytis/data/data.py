# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007 Brailcom, o.p.s.
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
    """Podm�nka pro metodu 'select()'.

    T��da m� pouze specifika�n� charakter, jej� instance slou�� pro z�pis
    podm�nek metody.  Ka�d� oper�tor je celkov� d�n sv�m jm�nem, argumenty
    a pojmenovan�mi argumenty.

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
        self._translation = kwargs.get('translation')
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
        
    def __str__(self):
        args = string.join(map(str, self.args()), ', ')
        return '%s (%s)' % (self.name(), args)

    def __cmp__(self, other):
        if sameclass(self, other):
            return cmp(self._name, other._name) or \
                   cmp(self._args, other._args) or \
                   cmp(self._kwargs, other._kwargs)
        else:
            return compare_objects(self, other)


class Data(object):
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

    class UnsupportedOperation(Exception):
        """Signalizuje, �e byla ��d�na nepodporovan� operace."""
    
    def __init__(self, columns, key, ordering=None, **kwargs):
        """Inicializuj datov� zdroj.

        Argumenty:
        
          columns -- sekvence instanc� t��dy 'ColumnSpec', tato sekvence
            jednozna�n� ur�uje sloupce tabulky v�etn� jejich po�ad�, kter� je
            d�no jejich po�ad�m v�'columns'
          key -- kl��ov� sloupec nebo seznam kl��ov�ch sloupc�, jejich�
            hodnoty jednozna�n� identifikuj� ��dky tabulky; v�echny tyto
            sloupce mus� b�t prvky 'columns'
          ordering -- specifikace automaticky udr�ovan�ho t��d�n�, id sloupce
            nebo tuple ids sloupc� nebo 'None'.  Nen�-li 'None', je automaticky
            udr�ov�no po�ad� ��dk� dle dan�ho sloupce (prvn� z�uveden�ch ids),
            kter� mus� b�t typu `types_.Integer'.  Pokud je argumentem tuple,
            je po�ad� udr�ov�no v�dy pouze v�r�mci ��dk�, kter� maj� stejn�
            hodnoty v�druh�m a� posledn�m z�uveden�ch sloupc�.
          kwargs -- p�ed�no p�edkovi
            
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
        self._locked_row = None
        self._change_number = pytis.util.Counter()
        self._on_change_callbacks = []
        self._select_last_row_number = None

    def columns(self):
        """Vra� specifikaci sloupc� zadanou v�konstruktoru, jako tuple."""
        return self._columns

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
        
    def row(self, key, columns=None):
        """Return row instance 'Row' identified by 'key'.

        If there is no such row, return 'None'.

        Arguments:

          key -- instance or a sequence of instances of the class
            'types_.Value' corresponding to the columns of the key,
            representing key values of the row being looked for
          columns -- sequence of names of the columns to retrieve; if not
            given, all columns are retrieved; if given, all key columns must be
            included

        Length of 'key' must correspond to the number of key columns.

        The method always returns 'None' in this class.
        
        """
        return None
    
    def select(self, condition=None, reuse=False, sort=(), columns=None):
        """Inicializuj nata�en� v�ech sloupc� z�datov�ho zdroje.

        Metoda sama nemus� je�t� je�t� ��dn� data natahovat, pouze tento p�enos
        inicializuje, je-li t�eba.  Metoda vrac� po�et ��dk�, kter� budou
        n�sledn� k�dispozici; pokud tato hodnota nen� zn�ma, metoda vr�t�
        'None'.  Po zavol�n� t�to metody je mo�no datov� ��dky z�skat
        opakovan�m vol�n�m metody 'fetchone()'.  P�eskakovat ��dky lze metodu
        'skip()'.

        Opakovan� vol�n� metody nemus� inicializovat zp��stupn�n� shodn�ch dat
        jako v�p�edchoz�m vol�n�, pokud se data u�datov�ho zdroje m�n�.

        Argumenty:

          condition -- podm�nka omezuj�c� v�b�r ��dk�; instance t��dy
            'Operator' nebo 'None'
          reuse -- pr�v� kdy� je pravda, instance sm� pro select pou��t data
            z�p�edchoz�ho dotazu, shoduje-li se jeho podm�nka
          sort -- sekvence specifik�tor� t��d�n�, ka�d� specifik�tor m� podobu
            ID nebo (ID, DIRECTION), kde ID je id t��d�n�ho sloupce a DIRECTION
            je jedna z�konstant modulu 'ASCENDENT' a 'DESCENDANT', implicitn�
            hodnota je 'ASCENDENT'
          columns -- sequence of IDs of columns to select; if not given, all
            columns are selected
          
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

        V�t�to t��d� metoda v�dy pouze vrac��0.
        
        """
        self._select_last_row_number = -1
        return 0
    
    def select_map(self, function, **kwargs):
        """Aplikuj 'function' na v�echny ��dky v�b�ru a vra� v�sledky.

        Zavolej metodu 'select()' s�argumenty 'kwargs' a na v�echny vr�cen�
        ��dky zavolej funkci 'function'.  V�sledky v�ech vol�n� 'function' vra�
        jako seznam s�po�tem a po�ad�m prvk� odpov�daj�c�ch vr�cen�m datov�m
        ��dk�m.

        Argumenty:

          function -- funkce jednoho argumentu, j�m� je instance t��dy 'Row'
          kwargs -- argumenty p�edan� metod� 'select()'
          
        """
        result = []
        self.select(**kwargs)
        while True:
            row = self.fetchone()
            if row is None:
                self.close()
                break
            result.append(function(row))
        return result

    def select_aggregate(self, operation, condition=None):
        """Vra� v�slednou hodnotu agrega�n� funkce.

        Metoda je prov�d� select, jeho� hodnotou je v�sledek agrega�n� funkce
        kompletn� vybran�ch dat.  Je-li vyvol�na b�hem neuzav�en�ho select,
        tento select nep�eru�uje a je-li podm�nka jej� a aktu�ln�ho select
        shodn�, vrac� v�sledek odpov�daj�c� obsahu onoho selectu (nap��klad
        zpracov�n�m v�jedin� transakci).

        Argumenty:

          operation -- dvojice (OPERATION, COLUMN), kde OPERATION je jednou
            z�'AGG_*' konstant t��dy a COLUMN je id sloupce, nad kter�m m� b�t
            operace provedena.
          condition -- shodn� se stejnojmenn�m argumentem metody 'select()'

        Vrac�: Instanci 'Value' odpov�daj�c� po�adovan� agrega�n� funkci.

        Podt��dy nejsou povinny tuto metodu implementovat (mohou ji
        neimplementovat v�bec nebo mohou implementovat pouze n�kter� podm�nky),
        v�takov�m p��pad� to mus� b�t uvedeno v�dokumentaci a p�i zad�n�
        nepodporovan� podm�nky mus� metoda vyvolat v�jimku
        'UnsupportedOperation'.
        
        V�t�to t��d� metoda v�dy vyvol�v� v�jimku 'UnsupportedOperation'.

        """
        raise self.UnsupportedOperation()
        
    def fetchone(self, direction=FORWARD):
        """Vra� dal�� ��dek dat.

        Opakovan�m vol�n�m t�to metody je mo�no z�skat v�echny ��dky tabulky.
        Pokud ji� ��dn� dal�� ��dek dat k�dispozici nen�, vra� 'None'.

        Argumenty:

          direction -- jedna z�konstant 'FORWARD' a 'BACKWARD', ur�uje, zda
            m� b�t vr�cen p�edchoz� nebo n�sleduj�c� ��dek

        Ne v�echny podt��dy jsou povinny implementovat vr�cen� p�edchoz�ho
        ��dku (tj. situaci, kdy 'direction==BACKWARD').  Pokud je
        neimplementuj�, mus� to b�t uvedeno v�dokumentaci a metoda mus�
        v�p��pad� odpov�daj�c�ho po�adavku vyvolat v�jimku
        'UnsupportedOperation'.

        Sm�r je vztahov�n k�posledn� vr�cen�mu ��dku.  M�-li tabulka _n_ ��dk�
        a posledn� vr�cen� ��dek byl _k_-t�, kde 0 < _k_ < n+1, znamen�
        'FORWARD' vr�cen� _k+1_n�ho ��dku (pokud existuje) a 'BACKWARD'
        _k-1_n�ho ��dku (pokud existuje).  Dojde-li k�p�ekro�en� prvn�ho ��dku,
        nejbl��e n�sleduj�c� 'FORWARD' vr�t� prvn� ��dek (pokud existuje),
        a�dojde-li k�p�ekro�en� posledn�ho ��dku, nejbl��e n�sleduj�c�
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

    def search(self, condition, direction=FORWARD):
        """Vyhledej nejbli��� v�skyt ��dku spl�uj�c�ho 'condition'.

        Argumenty:

          condition -- podm�nkov� v�raz, kter� mus� hledan� prvek spl�ovat,
            instance t��dy 'Operator'
          direction -- sm�r vyhled�v�n�, jedna z�konstant 'FORWARD' a
            'BACKWARD'

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
    
    def insert(self, row, after=None, before=None):
        """Vlo� 'row' do tabulky.

        Argumenty:
        
          row -- instance t��dy 'Row'
          after -- 'None' nebo kl�� ��dku, za kter� m� b�t nov� ��dek vlo�en
          before -- 'None' nebo kl�� ��dku, p�ed kter� m� b�t nov� ��dek vlo�en

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

    def update(self, key, row):
        """Nahra� ��dek identifikovan� kl��em 'key' ��dkem 'row'.

        Argumenty:
        
          key -- instance nebo seznam instanc� t��dy 'types._Value', mus�
            odpov�dat v�em sloupc�m kl��e tabulky
          row -- instance t��dy 'Row'

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

    def update_many(self, condition, row):
        """Nahra� ��dky identifikovan� 'condition' daty 'row'.

        Argumenty:
        
          key -- instance nebo seznam instanc� t��dy 'types._Value', mus�
            odpov�dat v�em sloupc�m kl��e tabulky; ur�uje ��dek, kter� m� b�t
            smaz�n
          row -- instance t��dy 'Row'

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
    
    def delete(self, key):
        """Sma� ��dek identifikovan� 'key'.

        Argumenty:
        
          key -- instance nebo seznam instanc� t��dy 'types._Value', mus�
            odpov�dat v�em sloupc�m kl��e tabulky; ur�uje ��dek, kter� m� b�t
            smaz�n

        Pokud je ��dek smaz�n, vra��1.
        Pokud takov� ��dek nelze smazat (v�etn� situace, kdy neexistuje),
        ned�lej nic a vra��0.

        V�t�to t��d� metoda v�dy pouze vrac��0.
        
        """
        return 0

    def delete_many(self, condition):
        """Sma� ��dky identifikovan� 'condition'.
        
        Argumenty:
        
          condition -- podm�nka odpov�daj�c� ��dk�m, kter� maj� b�t
            smaz�ny; instance t��dy 'Operator'

        Vrac�: Po�et smazan�ch ��dk�.

        V�t�to t��d� metoda v�dy pouze vrac��0.

        """
        return 0
        
    def lock_row(self, key):
        """Zam�i ��dek odpov�daj�c� 'key' pro editaci a maz�n�.

        Argumenty:

          key -- kl�� ��dku, kter� m� b�t zam�en

        Jestli�e 'key' neodpov�d� ��dn�mu existuj�c�mu ��dku, je chov�n� metody
        nedefinov�no.

        Zamyk�n� je neblokuj�c�, pokud je ji� po�adovan� ��dek zam�en, metoda
        okam�it� skon��, bez toho ani� by se sama pokou�ela o�jeho dal��
        zamyk�n�.

        Vrac�: 'None', jestli�e byl ��dek zam�en, slovn� popis existuj�c�ho
        z�mku, jestli�e ��dek je ji� zam�en.  Slovn� popis obsahuje opravdu jen
        popis z�mku, ne tedy nap��klad informaci o�tom �e z�znam je zam�en,
        za��n� mal�m p�smenem a nen� ukon�en ��dn�m interpunk�n�m znam�nkem.

        C�lem zamyk�n� je informovat ostatn� klienty, �e p��slu�n� z�znam je
        editov�n a �e by s�n�m nem�li manipulovat.  Zamyk�n� nemus� technicky
        zabra�ovat modifikac�m u�zdroje dat, ke kter�mu se p�istupuje jinak ne�
        instanc� t�to t��dy.  Nen� tak� nutno zaji��ovat o�et�en� p��stupu
        k�dat�m z�v�ce thread�, jestli�e t��da samotn� nen� thread safe.

        V�ka�d� okam�ik m��e b�t zam�en nejv��e jeden ��dek jedn� instance t�to
        t��dy.  P�i pokusu o�zam�en� dal��ho ��dku je chov�n� metody
        nedefinov�no.

        """
        if self._locked_row:
            raise ProgramError('Attempt to lock more than one row')
        self._locked_row = key
        return None

    def unlock_row(self):
        """Odem�i aktu�ln� zam�en� ��dek.

        Jestli�e nen� zam�en ��dn� ��dek, ned�lej nic.

        """
        self._locked_row = None

    def locked_row(self):
        """Vra� kl�� aktu�ln� zam�en�ho ��dku.

        Jestli�e moment�ln� nen� zam�en� ��dn� ��dek, vra� 'None'.

        """
        return self._locked_row

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

        V�t�to t��d� k���dn�m modifikac�m dat nedoch�z� a tud�� se ani
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


class Counter(object):
    """Abstrakce p��stupu ke gener�toru tickets.

    Jedn� se o�jednoduch� ��ta� vracej�c� postupn� sekvenci unik�tn�ch
    vzestupn�ch ��sel, bl��e viz metoda 'next()'.

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
        for i in range(len(data)):
            if data[i][k] == key:
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
        if op_name == 'EQ':
            col, value = condition.args()
            return lambda row: row[col] == value
        elif op_name == 'NOT':
            func = self._condition2pyfunc(condition.args()[0])
            return lambda row: not func(row)
        elif op_name == 'AND':
            fctns = [self._condition2pyfunc(c) for c in condition.args()]
            return lambda row: reduce(lambda r, f: r and f(row), fctns, True)
        else:
            ProgramError("Operator not supported:", condition)

    def select(self, condition=None, reuse=False, sort=None, columns=None):
        """Inicializace vytahov�n� z�znam�.

        Bli��� popis viz nadt��da.  Argumenty 'condition' a 'sort' jsou
        ignorov�ny.
        
        """
        cond = self._condition2pyfunc(condition)
        self._mem_cursor = -1
        self._mem_select = [self._restrict_row_columns(row, columns)
                            for row in self._mem_data if cond(row)]
        return len(self._mem_select)

    def close(self):
        self._mem_select = []
        
    def fetchone(self, direction=FORWARD):
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

    def insert(self, row):
        """Vlo� 'row' do tabulky.

        Pro bli��� popis viz nadt��da.

        'row' je vlo�en na konec tabulky.  Chyb�j�c� sloupce jsou nastaveny na
        'None'.
        
        """
        assert isinstance(row, Row)
        new_row = self._mem_create_row(row)
        if new_row == None:
            return None, False
        self._mem_data.append(new_row)
        return new_row, True

    def update(self, key, row):
        """Updatuj 'row' v�tabulce.

        Pro bli��� popis viz nadt��da.

        'row' je v�tabulce vlo�en na m�sto ��dku identifikovan�ho 'key''.
        Chyb�j�c� sloupce jsou nastaveny na 'None'.
        
        """
        index = self._mem_find_index(key)
        if index == None:
            return None, False
        new_row = self._mem_create_row(row, index)
        if new_row == None:
            return None, False
        self._mem_data[index] = new_row
        return new_row, True
        
    def delete(self, key):
        index = self._mem_find_index(key)
        if index == None:
            return 0
        del self._mem_data[index]
        return 1



### Pomocn� funkce


def EQ(x, y, ignore_case=False):
    """Podm�nkov� oper�tor 'EQ' relace rovnosti '='.

    Argumenty:

      x -- identifik�tor sloupce, string
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je primitivn�.
      
    """
    return Operator('EQ', x, y, ignore_case=ignore_case)

def NE(x, y, ignore_case=False):
    """Podm�nkov� oper�tor nerovnosti.

    Argumenty:

      x -- identifik�tor sloupce, string
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.
      
    """
    return NOT(EQ(x, y, ignore_case=ignore_case))
    
def WM(x, y, ignore_case=True):
    """Podm�nkov� oper�tor 'WM' (\"wildcard matches\") porovn�n� dle vzoru.

    Argumenty:

      x -- identifik�tor sloupce
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

      x -- identifik�tor sloupce typu 'String', string
      y -- string definuj�c� vzor; jako wildcars m��e obsahovat znaky '*' (lze
        substituovat ��mkoliv) a '?' (lze substituovat libovoln�m znakem)
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.
      
    """
    return NOT(WM(x, y, ignore_case=ignore_case))

def LT(x, y, ignore_case=False):
    """Podm�nkov� oper�tor 'LT' relace '<'.

    Argumenty:

      x -- identifik�tor sloupce, string
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je primitivn�.
      
    """
    return Operator('LT', x, y, ignore_case=ignore_case)

def LE(x, y, ignore_case=False):
    """Podm�nkov� oper�tor relace '<='.

    Argumenty:

      x -- identifik�tor sloupce, string
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.
      
    """
    return OR(LT(x, y, ignore_case=ignore_case),
              EQ(x, y, ignore_case=ignore_case))

def GT(x, y, ignore_case=False):
    """Podm�nkov� oper�tor relace '>'.

    Argumenty:

      x -- identifik�tor sloupce, string
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.

    """
    return AND(NOT(EQ(x, y, ignore_case=ignore_case)),
               NOT(LT(x, y, ignore_case=ignore_case)))

def GE(x, y, ignore_case=False):
    """Podm�nkov� oper�tor relace '>='.

    Argumenty:

      x -- identifik�tor sloupce, string
      y -- hodnota sloupce, instance t��dy 'types._Value'
      ignore_case -- zda m� b�t ignorov�na velikost p�smen (m�-li to pro dan�
        typ smysl)

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.

    """
    return OR(GT(x, y, ignore_case=ignore_case),
              EQ(x, y, ignore_case=ignore_case))

def NOT(x):
    """Podm�nkov� oper�tor 'NOT' negace.

    Argumenty:

      x -- oper�tor

    Tento oper�tor je primitivn�.

    """
    return Operator('NOT', x)

def AND(*args):
    """Podm�nkov� oper�tor 'AND' konjunkce.

    Argumenty:

      args -- sekvence oper�tor� (m��e b�t i�pr�zdn�)

    Oper�tor je komutativn�, na po�ad� argument� tedy nez�le��.

    Tento oper�tor je primitivn�.
      
    """
    return Operator(*(('AND',) + tuple(args)))

def OR(*args):
    """Podm�nkov� oper�tor disjunkce.

    Argumenty:

      args -- sekvence oper�tor� (m��e b�t i�pr�zdn�)

    Oper�tor je komutativn�, na po�ad� argument� tedy nez�le��.

    Tento oper�tor je vyj�d�en� pomoc� jin�ch oper�tor�.

    """
    translation = NOT(apply(AND, map(NOT, args)))
    return Operator(*(('OR',) + tuple(args)), **{'translation': translation})

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
    'types_.Type').  Bl��e viz metoda '__init__()'.

    Instance t��dy jsou pova�ov�ny za immutable, tud�� je mo�no je sd�let.
    
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
            assert is_sequence(data), ("Agument must be a sequence", data)
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

    def has_key(self, key):
        """Vra� pravdu, pr�v� kdy� ��dek obsahuje sloupec jm�na 'key'."""
        return some(lambda c, key=key: c[0] == key, self._data)

    def keys(self):
        """Vra� seznam n�zv� v�ech sloupc� jako strings.

        Po�ad� polo�ek vr�cen�ho seznamu je nedefinov�no.
        
        """
        return map (lambda c: c[0], self._data)

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
            if self.has_key(k):
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
        self._args = args
        self._kwargs = kwargs
        self._kwargs_hashable = kwargs.items()
        if DataFactory._data_object_cache is None:
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
        # TODO: Cachovan� datov�ch objekt� zat�m nem��eme pou��t,
        #       proto�e nar��me na probl�m udr�ovan�ch konex�
        # key = (self._class_, self._args, tuple(_kwargs.items()))
        # data_object = DataFactory._data_object_cache[key]
        # return copy.copy(data_object)
        return apply(self._class_, self._args, _kwargs)

    def __str__(self):
        return '<DataFactory: class=%s, args=%s, kwargs=%s>' % \
               (self._class_, deepstr(self._args), self._kwargs)

    def _get_data_object(key):
        c, a, k = key
        return apply(c, a, dict(k))
    _get_data_object = staticmethod(_get_data_object)

    def access_rights(self):
        if self._kwargs.has_key('access_rights'):
            return self._kwargs['access_rights']
        else:
            return None

    # "Podrobn�" porovn�v�n� data factories je p��li� n�ro�n� na CPU.
#     def __cmp__(self, other):
#         return compare_attr(self, other, ('_class_', '_args', '_kwargs'))
#     def __hash__(self):
#         return hash_attr(self, ('_class_', '_args', '_kwargs_hashable'))
