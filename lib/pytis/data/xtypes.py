# -*- coding: iso-8859-2 -*-

# Extra datov� typy
#
# Copyright (C) 2001, 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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

"""Speci�ln� datov� typy.

V�tomto modulu jsou um�st�ny typy, kter� nepat�� do mno�iny nejz�kladn�j��ch
typ� a/nebo nemohou b�t p��mo v�'types.py' kv�li cyklick�m z�vislostem modul�.

"""

from pytis.data import *


class Codebook(MutableType, Enumeration):
    """V��tov� typ z�sk�vaj�c� sv� hodnoty z�objektu t��dy 'Data'.

    Hodnoty tohoto typu (vnit�n� i�u�ivatelsk�) odpov�daj� hodnot�m sloupc�
    dan�ho datov�ho objektu.  Vnit�n� hodnotou je typicky pythonov� hodnota
    kl��ov�ho sloupce datov�ho objektu.  U�ivatelskou hodnotou je potom
    exportovan� hodnota ur�en�ho sloupce.  V�b�r sloupc� pou�it�ch pro z�sk�n�
    vnit�n� a u�ivatelsk� hodnoty je mo�n� ovlivnit argumenty konstruktoru.

    """
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    def _make(class_, *args, **kwargs):
        if kwargs.has_key('data_factory_kwargs') and \
               type(kwargs['data_factory_kwargs']) == type({}):
            kwargs = copy.copy(kwargs)
            kwargs['data_factory_kwargs'] = \
              tuple(kwargs['data_factory_kwargs'].items())
        return Enumeration._make(class_, *args, **kwargs)
    _make = staticmethod(_make)
        
    def __init__(self, data_factory, data_factory_kwargs={}, value_column=None,
                 validity_column=None, use_key=False, not_null=True,
                 **kwargs):
        """Inicializuj p�edka dle hodnot poskytnut�ch datov�m objektem 'data'.

        Argumenty:
        
          data_factory -- instance t��dy 'DataFactory', hodnoty ��dk�
            odpov�daj�c� instance t��dy 'Data' v�kl��ov�ch sloupc�ch poskytuj�
            vnit�n� hodnoty enumeratoru ve form� tuples
          data_factory_kwargs -- dictionary kl��ovan�ch argument� pro metodu
            'DataFactory.create()'
          value_column -- id sloupce datov�ho objektu poskytuj�c�ho po exportu
            u�ivatelskou hodnotu enumer�toru.  Je-li None, je u�ivatelsk�
            hodnota z�sk�v�na z�kl��e objektu 'data'.
          use_key -- boolean p��znak ur�uj�c�, zda m� b�t jako vnit�n� hodnota
            ��seln�ku pou��v�na hodnota kl��ov�ho sloupce ��seln�kov� tabulky.
            Pokud je tedy jako 'value_column' pou�it jin�, ne� kl��ov� sloupec
            a 'use_key' je pravdiv� (implicitn� nen�), bude vnit�n� hodnota
            (z�skan� validac�) r�zn� od u�ivatelsk� (validovan�) hodnoty.
          
          **kwargs, not_null -- argumenty p�edan� konstruktoru p�edka
        
        """
        super(Codebook, self).__init__(not_null=not_null, **kwargs)
        assert isinstance(data_factory, DataFactory), data_factory
        assert isinstance(value_column, types.StringType) \
               or value_column is None
        assert isinstance(use_key, types.BooleanType) 
        # Ulo� argumenty
        self._data_factory = data_factory
        if type(data_factory_kwargs) == type(()):
            data_factory_kwargs = dict(data_factory_kwargs)
        self._data_factory_kwargs = data_factory_kwargs
        # Vyt�hni informace z�dat
        data = self._create_data()
        if use_key or value_column is None:
            key = data.key()
            if len(key) != 1:
                ProgramError("Only single-column key is supported by Codebook.")
            internal_column = key[0].id()
        else:
            internal_column = value_column
        if value_column is None:
            value_column = internal_column
        self._value_column = value_column
        self._internal_column = internal_column
        self._columns = columns = data.columns()
        for c in columns:
            cid = c.id()
            if cid == internal_column:
                self._internal_column_type = c.type()
            if cid == value_column:
                self._value_column_type = c.type()
        if validity_column is None:
            self._validity_condition = None
        else:
            c = data.find_column(validity_column)
            assert c, ('Non-existent validity column', validity_column)
            assert isinstance(c.type(), Boolean), \
                   ('Invalid validity column type', c)
            self._validity_condition = EQ(c.id(), Value(Boolean(), True))
        # Initialize the runtime filter.
        self._runtime_filter = None
        self._runtime_filter_dirty = True
        self._runtime_filter_provider = None
        self._runtime_filter_args = None
        
    def _complete(self):
        super(Codebook, self)._complete()
        self._data = self._create_data()        
        self._data_changed = False
        def on_data_change():
            self._data_changed = True
        self._data.add_callback_on_change(on_data_change)
        
    def _create_data(self):
        return apply(self._data_factory.create, (), self._data_factory_kwargs)

    def __getattr__(self, name):
        if name == '_data':
            self._complete()
            return self.__dict__[name]
        else:
            return super(Codebook, self).__getattr__(name)

    # Informa�n� metody
    
    def columns(self):
        """Vra� specifikace v�ech sloupc� datov�ho objektu ��seln�ku.

        Vrac�: Tuple instanc� t��dy 'pytis.data.ColumnSpec'.

        """
        return self._columns

    def internal_column(self):
        """Vra� id sloupce tvo��c�ho vnit�n� hodnotu."""
        return self._internal_column
    
    def value_column(self):
        """Vra� id sloupce tvo��c�ho u�ivatelskou hodnotu."""
        return self._value_column

    def maxlen(self):
        """Vra� maxim�ln�ch d�lku exportovan� hodnoty.

        Vrac� nez�porn� integer ud�vaj�c� maxim�ln� mo�nou d�lku hodnoty,
        nebo 'None' zna��c�, �e d�lka hodnoty nen� omezen�.

        """
        t = self._value_column_type
        return isinstance(t, String) and t.maxlen() or None

    # Run-time filter

    def set_runtime_filter_provider(self, provider, args):
        """Nastav poskytovatele run-time podm�nky filtruj�c� ��dky ��seln�ku.

        Argumenty:
        
          provider -- None, nebo funkce, kter� vrac� instanci t��dy 'Operator'.
            Tato funkce bude vol�na v�dy, kdy� je t�eba zjistit dodate�nou
            filtrovac� podm�nku.  args -- seznam argument� (tuple), kter� maj�
            b�t p�ed�ny t�to funkci.

        Run-time podm�nka umo��uje m�nit mno�inu platn�ch ��dk� ��seln�ku za
        b�hu.  ��seln�k po zm�n� podm�nky automaticky zm�n� svou mno�inu
        platn�ch hodnot tak, aby v�echny ��dky t�to podm�nce odpov�daly.
        Extern� zm�nu je v�ak t�eba ohl�sit vol�n�m metody
        'notify_runtime_filter_change()'.

        """
        assert callable(provider) or provider is None
        self._runtime_filter_provider = provider
        self._runtime_filter_args = args

    def notify_runtime_filter_change(self):
        """Ohlas zm�nu run-time filtrovac� podm�nky.

        Tato metoda by m�la b�t vol�na v�dy, kdy� dojde k extern� zm�n�
        run-time filtrovac� podm�nky.  ��seln�k se tak dozv�, �e si m� v
        p��pad� pot�eby zjistit novou hodnotu podm�nky (viz metoda
        'set_runtime_filter_provider()').
        
        """
        self._runtime_filter_dirty = True
        self._update(force=True)
        
    
    def validity_condition(self):
        """Vra� podm�nku ur�uj�c� platn� ��dky ��seln�ku.

        Podm�nka je vypo�tena za aktu�ln�  p�idan� podm�nky, kterou za b�hu
        poskytuje funkce nastaven� metodou 'set_runtime_filter_provider()'.

        Vrac�: Instanci t��dy 'pytis.data.Operator'.

        """
        f = self._runtime_filter_provider
        if f is not None:
            if self._runtime_filter_dirty:
                self._runtime_filter = apply(f, self._runtime_filter_args)
                assert isinstance(self._runtime_filter, Operator)
                self._runtime_filter_dirty = False
                self._update(force=True)
            condition = self._runtime_filter
        else:
            condition = None
        if self._validity_condition:
            if condition:
                return AND(condition, self._validity_condition)
            else:
                return self._validity_condition
        else:
            return condition
            
    # Validation and export

    def _retrieve(self, column, value):
        data = self._data
        condition = EQ(column, value)
        validity_condition = self.validity_condition()
        if validity_condition is not None:
            condition = AND(condition, validity_condition)
        count = data.select(condition)
        if count > 1:
            raise ProgramError('Insufficient runtime filter for Codebook',
                               condition)
        row = data.fetchone()
        data.close()
        return row

    def _update(self, force=False):
        if force or self._data_changed:
            self._data_changed = False
            self._validation_cache.reset()
            result = True
        else:
            result = False
        return result
            
    def _validate(self, object):
        """Zvaliduje 'object' dle standardn�ch krit�ri�.

        Obecn� informace viz. 'Type.validate()'

        Object je u�ivatelsk� hodnota.  Odpov�daj�c� ��dek bude vyhled�n v
        datov�m objektu a p��slu�n� vnit�n� hodnota bude vr�cena.

        """
        value, error = self._value_column_type.validate(object)
        if value is None:
            result = None, error #self._validation_error(self.VM_INVALID_VALUE)
        else:
            row = self._retrieve(self._value_column, value)
            if row is None:
                result = None, self._validation_error(self.VM_INVALID_VALUE)
            else:
                if self._internal_column == self._value_column:
                    result = Value(self, value.value()), None
                else:
                    v = row[self._internal_column]
                    result = Value(self, v.value()), None
        return result

    def _export(self, value, column=None):
        """Vra� stringovou reprezentaci 'value'.

        Argumenty:

          value -- viz 'Type.export()'

        Pokud vnit�n� hodnota ��seln�ku odpov�d�u�ivatelsk�, bude vr�cena
        exportovan� vnit�n� hodnota i v p��pad�, �e tato nen� v ��seln�ku
        obsa�ena.

        Je nutno m�t na pam�ti, �e vnit�n� hodnoty ��seln�ku se mohou v��ase
        m�nit, vzhledem k�mo�n�m updat�m dat v�datab�zi.

        """
        if value is None:
            result = ''
        else:
            if column is None:
                column = self._value_column
            if column == self._internal_column:
                result = self._internal_column_type.export(value)
            else:
                v = Value(self._internal_column_type, value)
                row = self._retrieve(self._internal_column, v)
                if row is None:
                    result = ''
                else:
                    result = row[column].export()
        return result


    def data_value(self, value, column):
        """Vra� hodnotu dan�ho sloupce z odpov�daj�c�ho ��dku datov�ho objektu.
        
        Argumenty:

          value -- vnit�n� hodnota ��seln�ku
          column -- identifik�tor sloupce, string

        Vrac�: Instanci t��dy 'Value'.  Pokud ��dek odpov�daj�c� dan� hodnot�
          'value' v datov�m objektu nen� nalezen ('value' nen� vnit�n� hodnotou
          ��seln�ku), vra� instanci t��dy 'Value' s hodnotou 'None'.  Pozor,
          vr�cen� hodnota nen� exportovanou hodnotou ��seln�ku, jedn� se
          o�oby�ejnou datovou hodnotu.

        V p��pad� neexistuj�c�ho n�zvu sloupce vyvolej v�jimku 'ProgramError'.

        """
        # TODO: test
        if value is None:
            return pytis.data.Value(self, None)
        v = Value(self._internal_column_type, value)
        row = self._retrieve(self._internal_column, v)
        if row is None:
            return pytis.data.Value(self, None)
        else:
            try:
                result = row[column]
            except KeyError:
                raise ProgramError('Invalid column id', column)
        return result

    

def _codebook_data(codebook):
    # Tato metoda zp��stup�uje datov� objekt ��seln�ku, kter� je nutn� pro
    # introspekci z�le�itost� souvisej�c�ch s�wildcard matching v�dbdata.py.
    # Je to docela o�kliv� hack, ale �ist�� �e�en� se hled� t�ko.    
    return codebook._data



