# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2005 Brailcom, o.p.s.
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

"""Třídy pro zjednodušení a zpřehlednění tvorby specifikačních souborů.""" 

from pytis.extensions import *
from pytis.presentation import *


class DataSpec(object):
    """Třída zjednodušující tvorbu datové specifikace.

    Konstruktor této třídy přijímá argumenty ve zjednodušené formě a schovává
    tak některé nízkoúrovňové detaily před tvůrcem specifikace.  Zároveň je
    odstraněna duplicita některých informací, které se při přímém použití
    specifikačních tříd datového rozhraní není možné zcela vyhnout.

    Podrobný popis rozhraní viz. konstruktor třídy.

    Po vytvoření instance této třídy je možné získat odpovídající instanci
    'pytis.data.DataFactory' voláním metody 'make()'.
    
    """
    
    def __init__(self, table, columns, key, access_rights=None,
                 ignore_enumerators=False):
        """Inicializuj specifikaci.

        Argumenty:

          table -- název datové tabulky jako řetězec.
          columns -- sekvence specifikací sloupců jako instancí 'Column'.
            Jedná se vždy o sloupce z tabulky 'table'.
          key -- název klíčového sloupce jako řetězec.  Sloupec s tímto
            identifikátorem musí být přítomný v 'columns'.
          access_rights -- práva jako instance 'pytis.data.AccessRights'.
          ignore_enumerators -- pokud bude předána pravdivá hodnota, budou
            enumerátory všech sloupců ignorovány.

        Pokud 'columns' neobsahují sloupec s identifikátorem 'oid', bude
        automaticky doplněn sloupec 'oid' typu 'pytis.data.Oid'.

        """
        assert isinstance(table, types.StringType)
        assert isinstance(columns, (types.ListType, types.TupleType))
        assert isinstance(key, types.StringType)
        assert isinstance(ignore_enumerators, types.BooleanType)
        assert isinstance(access_rights, pytis.data.AccessRights) \
               or access_rights is None
        assert find(key, columns, key=lambda c: c.id()) is not None
        for c in columns:
            assert isinstance(c, Column)
        self._table = table
        self._key = key
        self._columns = columns
        self._ignore_enumerators = ignore_enumerators
        self._access_rights = access_rights

    def make(self):
        """Vtať instanci 'pytis.data.DataFactory' odpovídající specifikaci."""
        t = self._table
        bindings = []
        for c in self._columns:
            type = c.type()
            kwargs = c.kwargs()
            e = c.enumerator()
            if self._ignore_enumerators:
                e = None
                kwargs = {}
            if e:
                enumerator = pytis.form.resolver().get(e, 'data_spec')
                if not type:
                    kwargs['data_factory_kwargs'] = {'dbconnection_spec':
                                                     config.dbconnection}
                    type = pytis.data.Codebook(enumerator, **kwargs)
                else:
                    assert isinstance(type, pytis.data.Codebook)
                    assert kwargs == {}
            else:
                enumerator = None
                assert kwargs == {}, \
                       "Argumenty jsou zatím podporovány jen pro enumerator."
            bindings.append(pytis.data.DBColumnBinding(c.id(), t, c.column(),
                                                     enumerator=enumerator,
                                                     type_=type))
        if not find('oid', bindings, key=lambda b: b.column()):
            oid = pytis.data.DBColumnBinding('oid', t, 'oid', type_=pytis.data.Oid())
            bindings.append(oid)
        key = find(self._key, bindings, key=lambda b: b.column())
        return pytis.data.DataFactory(pytis.data.DBDataDefault, bindings, key,
                                    access_rights=self._access_rights)
    

class Column(object):
    def __init__(self, id, column=None, enumerator=None, type=None, **kwargs):
        """Inicializuj specifikaci enumerátoru.

        Argumenty:
        
          id -- identifikátor sloupce (řetězec).  Pod tímto identifikátorem
            bude sloubec vystupovat v aplikaci.
          column -- název databázového sloupce (řetězec nebo None).  Implicitně
            je doplněna hodnota 'id', takže pokud se název sloupce
            shoduje s identifikátorem, není jej třeba definovat.
          enumerator -- název specifikace pro resolver (řetězec nebo None).  Z
            této specifikace bude získán datový objekt a použit jako číselník.
            Typ bude v takovém případě automaticky nastaven na
            'pytis.data.Codebook', pokud není určen explicitně (viz. níže).
          type -- explicitní určení datového typu sloupce (instance
            'pytis.data.Type', nebo None).
          **kwargs -- pokud jsou uvedeny jakékoliv další klíčové argumenty,
            budou tyto předány konstruktoru datového typu sloupce.  Momentálně
            jsou však klíčové argumenty podporovány pouze v případě, že je
            specifikován enumerator.  Potom je vytvořen 'Codebook' s danými
            argumenty.  Pokud je třeba předat argumenty jinému typu, je
            prozatím nutno použít argument 'type'.

        """
        assert isinstance(id, types.StringType)
        assert isinstance(column, types.StringType) or column is None
        assert isinstance(enumerator, types.StringType) or enumerator is None
        assert isinstance(type, pytis.data.Type) or type is None
        if isinstance(type, pytis.data.Codebook):
            assert enumerator is not None
        self._id = id
        if column is None:
            column = id
        self._column = column
        self._enumerator = enumerator
        self._type = type
        self._kwargs = kwargs
    
    def id(self):
        return self._id
    
    def column(self):
        return self._column

    def enumerator(self):
        return self._enumerator

    def type(self):
        return self._type
    
    def kwargs(self):
        return self._kwargs


Field = FieldSpec

# Odvozené specializované třídy

class HGroup(GroupSpec):
    """Horizontální seskupení políček."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        GroupSpec.__init__(self, items, **kwargs)

class VGroup(GroupSpec):
    """Vertikální seskupení políček."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.VERTICAL
        GroupSpec.__init__(self, items, **kwargs)
        
class LHGroup(GroupSpec):
    """Horizontální seskupení políček s labelem a orámováním."""
    def __init__(self, label, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        kwargs['label'] = label
        GroupSpec.__init__(self, items, **kwargs)

class LVGroup(GroupSpec):
    """Vertikální seskupení políček s labelem a orámováním."""
    def __init__(self, label, *items, **kwargs):
        kwargs['orientation'] = Orientation.VERTICAL
        kwargs['label'] = label
        GroupSpec.__init__(self, items, **kwargs)
        

class ReusableSpec:
    def __init__(self, resolver):
        self._resolver = resolver
        self._bindings = self._bindings()
        self._fields = self._fields()

    def __getitem__(self, id):
        return find(id, self._fields, key=lambda f: f.id())

    def _bindings(self):
        pass

    def _fields(self):
        pass

    def fields(self, *args):
        """Vrať seznam specifikací sloupců vyjmenovaných sloupců.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'FieldSpec'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() in args, self._fields)

    def bindings(self, *args):
        """Vrať seznam specifikací sloupců vyjmenovaných sloupců.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'pytis.data.DBColumnBinding'.

        """
        if len(args) == 0:
            return self._bindings
        else:
            return filter(lambda b: b.id() in args, self._bindings)


    def fields_complement(self, *args):
        """Vrať seznam specifikací sloupců, které nejsou vyjmenovány.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'FieldSpec'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() not in args, self._fields)


