# -*- coding: iso-8859-2 -*-

# Extra datové typy
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

"""Speciální datové typy.

V tomto modulu jsou umístěny typy, které nepatří do množiny nejzákladnějších
typů a/nebo nemohou být přímo v 'types.py' kvůli cyklickým závislostem modulů.

"""

from pytis.data import *


class Codebook(MutableType, Enumeration):
    """Výčtový typ získávající své hodnoty z objektu třídy 'Data'.

    Hodnoty tohoto typu (vnitřní i uživatelské) odpovídají hodnotám sloupců
    daného datového objektu.  Vnitřní hodnotou je typicky pythonová hodnota
    klíčového sloupce datového objektu.  Uživatelskou hodnotou je potom
    exportovaná hodnota určeného sloupce.  Výběr sloupců použitých pro získání
    vnitřní a uživatelské hodnoty je možné ovlivnit argumenty konstruktoru.

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
        """Inicializuj předka dle hodnot poskytnutých datovým objektem 'data'.

        Argumenty:
        
          data_factory -- instance třídy 'DataFactory', hodnoty řádků
            odpovídající instance třídy 'Data' v klíčových sloupcích poskytují
            vnitřní hodnoty enumeratoru ve formě tuples
          data_factory_kwargs -- dictionary klíčovaných argumentů pro metodu
            'DataFactory.create()'
          value_column -- id sloupce datového objektu poskytujícího po exportu
            uživatelskou hodnotu enumerátoru.  Je-li None, je uživatelská
            hodnota získávána z klíče objektu 'data'.
          use_key -- boolean příznak určující, zda má být jako vnitřní hodnota
            číselníku používána hodnota klíčového sloupce číselníkové tabulky.
            Pokud je tedy jako 'value_column' použit jiný, než klíčový sloupec
            a 'use_key' je pravdivé (implicitně není), bude vnitřní hodnota
            (získaná validací) různá od uživatelské (validované) hodnoty.
          
          **kwargs, not_null -- argumenty předané konstruktoru předka
        
        """
        super(Codebook, self).__init__(not_null=not_null, **kwargs)
        assert isinstance(data_factory, DataFactory), data_factory
        assert isinstance(value_column, types.StringType) \
               or value_column is None
        assert isinstance(use_key, types.BooleanType) 
        # Ulož argumenty
        self._data_factory = data_factory
        if type(data_factory_kwargs) == type(()):
            data_factory_kwargs = dict(data_factory_kwargs)
        self._data_factory_kwargs = data_factory_kwargs
        # Vytáhni informace z dat
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

    # Informační metody
    
    def columns(self):
        """Vrať specifikace všech sloupců datového objektu číselníku.

        Vrací: Tuple instancí třídy 'pytis.data.ColumnSpec'.

        """
        return self._columns

    def internal_column(self):
        """Vrať id sloupce tvořícího vnitřní hodnotu."""
        return self._internal_column
    
    def value_column(self):
        """Vrať id sloupce tvořícího uživatelskou hodnotu."""
        return self._value_column

    def maxlen(self):
        """Vrať maximálních délku exportované hodnoty.

        Vrací nezáporný integer udávající maximální možnou délku hodnoty,
        nebo 'None' značící, že délka hodnoty není omezená.

        """
        t = self._value_column_type
        return isinstance(t, String) and t.maxlen() or None

    # Run-time filter

    def set_runtime_filter_provider(self, provider, args):
        """Nastav poskytovatele run-time podmínky filtrující řádky číselníku.

        Argumenty:
        
          provider -- None, nebo funkce, která vrací instanci třídy 'Operator'.
            Tato funkce bude volána vždy, když je třeba zjistit dodatečnou
            filtrovací podmínku.  args -- seznam argumentů (tuple), které mají
            být předány této funkci.

        Run-time podmínka umožňuje měnit množinu platných řádků číselníku za
        běhu.  Číselník po změně podmínky automaticky změní svou množinu
        platných hodnot tak, aby všechny řádky této podmínce odpovídaly.
        Externí změnu je však třeba ohlásit voláním metody
        'notify_runtime_filter_change()'.

        """
        assert callable(provider) or provider is None
        self._runtime_filter_provider = provider
        self._runtime_filter_args = args

    def notify_runtime_filter_change(self):
        """Ohlas změnu run-time filtrovací podmínky.

        Tato metoda by měla být volána vždy, když dojde k externí změně
        run-time filtrovací podmínky.  Číselník se tak dozví, že si má v
        případě potřeby zjistit novou hodnotu podmínky (viz metoda
        'set_runtime_filter_provider()').
        
        """
        self._runtime_filter_dirty = True
        self._update(force=True)
        
    
    def validity_condition(self):
        """Vrať podmínku určující platné řádky číselníku.

        Podmínka je vypočtena za aktuální  přidané podmínky, kterou za běhu
        poskytuje funkce nastavená metodou 'set_runtime_filter_provider()'.

        Vrací: Instanci třídy 'pytis.data.Operator'.

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
        """Zvaliduje 'object' dle standardních kritérií.

        Obecné informace viz. 'Type.validate()'

        Object je uživatelská hodnota.  Odpovídající řádek bude vyhledán v
        datovém objektu a příslušná vnitřní hodnota bude vrácena.

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
        """Vrať stringovou reprezentaci 'value'.

        Argumenty:

          value -- viz 'Type.export()'

        Pokud vnitřní hodnota číselníku odpovídá uživatelské, bude vrácena
        exportovaná vnitřní hodnota i v případě, že tato není v číselníku
        obsažena.

        Je nutno mít na paměti, že vnitřní hodnoty číselníku se mohou v čase
        měnit, vzhledem k možným updatům dat v databázi.

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
        """Vrať hodnotu daného sloupce z odpovídajícího řádku datového objektu.
        
        Argumenty:

          value -- vnitřní hodnota číselníku
          column -- identifikátor sloupce, string

        Vrací: Instanci třídy 'Value'.  Pokud řádek odpovídající dané hodnotě
          'value' v datovém objektu není nalezen ('value' není vnitřní hodnotou
          číselníku), vrať instanci třídy 'Value' s hodnotou 'None'.  Pozor,
          vrácená hodnota není exportovanou hodnotou číselníku, jedná se
          o obyčejnou datovou hodnotu.

        V případě neexistujícího názvu sloupce vyvolej výjimku 'ProgramError'.

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
    # Tato metoda zpřístupňuje datový objekt číselníku, který je nutný pro
    # introspekci záležitostí souvisejících s wildcard matching v dbdata.py.
    # Je to docela ošklivý hack, ale čistší řešení se hledá těžko.    
    return codebook._data



