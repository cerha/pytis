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

"""T��dy pro zjednodu�en� a zp�ehledn�n� tvorby specifika�n�ch soubor�.""" 

from pytis.extensions import *
from pytis.presentation import *


class DataSpec(object):
    """T��da zjednodu�uj�c� tvorbu datov� specifikace.

    Konstruktor t�to t��dy p�ij�m� argumenty ve zjednodu�en� form� a schov�v�
    tak n�kter� n�zko�rov�ov� detaily p�ed tv�rcem specifikace.  Z�rove� je
    odstran�na duplicita n�kter�ch informac�, kter� se p�i p��m�m pou�it�
    specifika�n�ch t��d datov�ho rozhran� nen� mo�n� zcela vyhnout.

    Podrobn� popis rozhran� viz. konstruktor t��dy.

    Po vytvo�en� instance t�to t��dy je mo�n� z�skat odpov�daj�c� instanci
    'pytis.data.DataFactory' vol�n�m metody 'make()'.
    
    """
    
    def __init__(self, table, columns, key, access_rights=None,
                 ignore_enumerators=False):
        """Inicializuj specifikaci.

        Argumenty:

          table -- n�zev datov� tabulky jako �et�zec.
          columns -- sekvence specifikac� sloupc� jako instanc� 'Column'.
            Jedn� se v�dy o sloupce z tabulky 'table'.
          key -- n�zev kl��ov�ho sloupce jako �et�zec.  Sloupec s t�mto
            identifik�torem mus� b�t p��tomn� v 'columns'.
          access_rights -- pr�va jako instance 'pytis.data.AccessRights'.
          ignore_enumerators -- pokud bude p�ed�na pravdiv� hodnota, budou
            enumer�tory v�ech sloupc� ignorov�ny.

        Pokud 'columns' neobsahuj� sloupec s identifik�torem 'oid', bude
        automaticky dopln�n sloupec 'oid' typu 'pytis.data.Oid'.

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
        """Vta� instanci 'pytis.data.DataFactory' odpov�daj�c� specifikaci."""
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
                       "Argumenty jsou zat�m podporov�ny jen pro enumerator."
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
        """Inicializuj specifikaci enumer�toru.

        Argumenty:
        
          id -- identifik�tor sloupce (�et�zec).  Pod t�mto identifik�torem
            bude sloubec vystupovat v aplikaci.
          column -- n�zev datab�zov�ho sloupce (�et�zec nebo None).  Implicitn�
            je dopln�na hodnota 'id', tak�e pokud se n�zev sloupce
            shoduje s identifik�torem, nen� jej t�eba definovat.
          enumerator -- n�zev specifikace pro resolver (�et�zec nebo None).  Z
            t�to specifikace bude z�sk�n datov� objekt a pou�it jako ��seln�k.
            Typ bude v takov�m p��pad� automaticky nastaven na
            'pytis.data.Codebook', pokud nen� ur�en explicitn� (viz. n�e).
          type -- explicitn� ur�en� datov�ho typu sloupce (instance
            'pytis.data.Type', nebo None).
          **kwargs -- pokud jsou uvedeny jak�koliv dal�� kl��ov� argumenty,
            budou tyto p�ed�ny konstruktoru datov�ho typu sloupce.  Moment�ln�
            jsou v�ak kl��ov� argumenty podporov�ny pouze v p��pad�, �e je
            specifikov�n enumerator.  Potom je vytvo�en 'Codebook' s dan�mi
            argumenty.  Pokud je t�eba p�edat argumenty jin�mu typu, je
            prozat�m nutno pou��t argument 'type'.

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

# Odvozen� specializovan� t��dy

class HGroup(GroupSpec):
    """Horizont�ln� seskupen� pol��ek."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        GroupSpec.__init__(self, items, **kwargs)

class VGroup(GroupSpec):
    """Vertik�ln� seskupen� pol��ek."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.VERTICAL
        GroupSpec.__init__(self, items, **kwargs)
        
class LHGroup(GroupSpec):
    """Horizont�ln� seskupen� pol��ek s labelem a or�mov�n�m."""
    def __init__(self, label, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        kwargs['label'] = label
        GroupSpec.__init__(self, items, **kwargs)

class LVGroup(GroupSpec):
    """Vertik�ln� seskupen� pol��ek s labelem a or�mov�n�m."""
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
        """Vra� seznam specifikac� sloupc� vyjmenovan�ch sloupc�.

        Pokud nejsou vyjmenov�ny ��dn� identifik�tory sloupc�, vr�t� seznam
        v�ech sloupc�.  Vrac� sekvenci instanc� 'FieldSpec'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() in args, self._fields)

    def bindings(self, *args):
        """Vra� seznam specifikac� sloupc� vyjmenovan�ch sloupc�.

        Pokud nejsou vyjmenov�ny ��dn� identifik�tory sloupc�, vr�t� seznam
        v�ech sloupc�.  Vrac� sekvenci instanc� 'pytis.data.DBColumnBinding'.

        """
        if len(args) == 0:
            return self._bindings
        else:
            return filter(lambda b: b.id() in args, self._bindings)


    def fields_complement(self, *args):
        """Vra� seznam specifikac� sloupc�, kter� nejsou vyjmenov�ny.

        Pokud nejsou vyjmenov�ny ��dn� identifik�tory sloupc�, vr�t� seznam
        v�ech sloupc�.  Vrac� sekvenci instanc� 'FieldSpec'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() not in args, self._fields)


