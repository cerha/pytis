# -*- coding: iso-8859-2 -*-

# Prezentace dat v políčkách.
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 Brailcom, o.p.s.
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

"""Prezentace dat v políčkách.

_Políčkem_ se zde rozumí abstraktní element uživatelského rozhraní přijímající
textová data, nikoliv konkrétní forma zobrazení dat.

"""

import copy

import pytis.data

from pytis.presentation import *
from pytis.util import *


class PresentedRow(object):
    """Řádek prezentovaných dat.

    Třída je mezičlánkem mezi datovým řádkem a jeho finální prezentací.  Na
    rozdíl od datového řádku obsahuje všechna políčka dané specifikacemi
    políček 'FieldSpec'.  Na druhou stranu ale již neřeší konkrétní prezentaci
    dat přesahující jejich zformátování do stringu.

    """
    class _Column:
        def __init__(self, f, data):
            self.id = f.id()
            self.type = f.type(data)
            self.computer = f.computer()
            self.line_separator = f.line_separator()
            self.default = f.default()
            self.editable = f.editable()
            self.display = f.display()
            self.codebook = f.codebook(data)
            self.codebook_runtime_filter = f.codebook_runtime_filter()
            
    def __init__(self, fieldspec, data, row, prefill=None, singleline=False,
                 change_callback=None, editability_change_callback=None,
                 new=False, resolver=None):
        """Inicializuj prezentaci řádku.
        
        Argumenty:

          fieldspec -- sekvence specifikací políček, instancí třídy
            'FieldSpec'
            
          data -- odpovídající datový objekt, instance třídy 'pytis.data.Data'
          
          row -- data řádku, viz níže
          
          prefill -- slovník hodnot pro inicializaci řádku namísto výchozích
            hodnot.  Slovník je klíčovaný přes textový identifikátor sloupce.
            Hodnotami jsou instance třídy Value, nebo přímo vnitřní hodnoty.
            Takto předvyplněné hodnoty mají přednost nejen před výchozími
            hodnotami určenými specifikací 'default' příslušného políčka, ale
            také před hodnotami dopočtenými pomocí jeho dopočítávací funkce
            ('computer').
            
          singleline -- právě když je pravdivé, stringové hodnoty všech políček
            budou zformátovány jako jednořádkové
            
          change_callback -- funkce jednoho argumentu (id políčka) volaná při
            nepřímé změně políčka (tj. při přepočítávání hodnot), která
            oznamuje \"nečekané\" změny políček v prezentovaném row; je-li
            'None', není žádná taková funkce volána
            
          editability_change_callback -- funkce dvou argumentů (id políčka,
            příznak editovatelnosti) volaná při nepřímé změně editovatelnosti
            políčka.  Voláním této funkce řádek oznamuje, že v důsledku změny v
            jiných políčkách se dané políčko stalo editovatelným (druhý
            argument je pravdivý), či naopak (druhý argument je nepravdivý);
            je-li 'None', není změna editovatelnosti oznamována.
            
          new -- flag určující, zda se jedná o nově vytvářený záznam (nikoliv
            editaci záznamu již existujícího)
            
          resolver -- instance 'Resolver', která má být používána k načítání
            specifikací.  Pokud není určen, je použit globální resolver získaný
            pomocí funkce 'pytis.util.resolver()'.  Globální resolver je
            použitelný v samostatně běžící aplikaci, ale např. v prostředí
            webového serveru je třeba pracovat s více resolvery současně a ty
            je potom nutné předávat jako argument.

        Prezentační podoba je vytvořena z dat specifikovaných argumentem 'row',
        který může mít některou z následujících hodnot:

          None -- bude vytvořen zbrusu nový řádek odpovídající 'fieldspec'
          instance 'PresentedRow' -- bude vytvořena kopie zadaného řádku, oba
            musí mít shodnou specifikaci políček
          instance 'pytis.data.Row' -- bude vytvořena prezentace z daného
            datového řádku

        Ve všech případech je rozhodující podoba 'row' v okamžiku volání tohoto
        konstruktoru, pozdější případné destruktivní změny 'row' nemají na nově
        vytvořenou instanci třídy 'PresentedRow' vliv.

        """
        assert is_sequence(fieldspec)
        # TODO: pytis.remote vyžaduje inicializaci Pyro, což není vždy to pravé
        # ořechové.  `data' by stejně mělo být jednotného typu, je třeba to
        # nějak promyslet.
        #assert isinstance(data, pytis.data.Data) or \
        #       isinstance(data, pytis.remote.RemoteData)
        assert row is None or isinstance(row, (PresentedRow, pytis.data.Row))
        assert change_callback is None or callable(change_callback)
        assert editability_change_callback is None or \
               callable(editability_change_callback)
        assert prefill is None or isinstance(prefill, dict)
        assert isinstance(singleline, bool)
        assert isinstance(new, bool)
        assert resolver is None or isinstance(resolver, Resolver)
        self._fieldspec = fieldspec
        self._data = data
        self._singleline = singleline
        self._change_callback = change_callback
        self._editability_change_callback = editability_change_callback
        self._new = new
        self._cache = {}
        self._resolver = resolver or pytis.util.resolver()
        self._columns = columns = dict([(f.id(), self._Column(f, data))
                                        for f in self._fieldspec])
        self._init_dependencies()
        if prefill:
            V = pytis.data.Value
            prefill = dict([(k, V(columns[k].type,
                                  isinstance(v, V) and v.value() or v))
                            for k, v in prefill.items()])
        self._set_row(row, prefill=prefill)
        self._virtual = dict([(k, self._default(k, prefill=prefill))
                              for k in columns.keys()
                              if data.find_column(k) is None])

    def _set_row(self, row, reset=True, prefill=None):
        self._row = self._init_row(row, prefill=prefill)
        if reset:
            self._original_row = copy.copy(self._row)
            self._original_row_empty = row is None
        self._resolve_dependencies()

    def _all_deps(self, depends):
        all = []
        for key in depends:
            all.append(key)
            computer = self._columns[key].computer
            if computer:
                all.extend(self._all_deps(computer.depends()))
        return all
        
    def _init_dependencies(self):
        # Pro každé políčko si zapamatuji seznam počítaných políček, která na
        # něm závisí (obrácené mapování než ve specifikacích).
        self._dependent = {}
        self._editability_dependent = {}
        self._codebook_runtime_filter_dependent = {}
        # Pro všechna počítaná políčka si pamatuji, zda potřebují přepočítat,
        # či nikoliv (po přepočítání je políčko čisté, po změně políčka na
        # kterém závisí jiná políčka, nastavím závislým políčkům příznak
        # dirty).  Přepočítávání potom mohu provádět až při skutečném požadavku
        # na získání hodnoty políčka.
        self._dirty = {}
        self._editability_dirty = {}
        self._editable = {}
        for key, c in self._columns.items():
            if c.computer is not None:
                self._dirty[key] = True
                for dep in c.computer.depends():
                    if self._dependent.has_key(dep):
                        self._dependent[dep].append(key)
                    else:
                        self._dependent[dep] = [key]
            if isinstance(c.editable, Computer):
                self._editable[key] = True
                self._editability_dirty[key] = True
                for dep in self._all_deps(c.editable.depends()):
                    if self._editability_dependent.has_key(dep):
                        self._editability_dependent[dep].append(key)
                    else:
                        self._editability_dependent[dep] = [key]
            if c.codebook_runtime_filter is not None:
                for dep in self._all_deps(c.codebook_runtime_filter.depends()):
                    if self._codebook_runtime_filter_dependent.has_key(dep):
                        self._codebook_runtime_filter_dependent[dep].append(key)
                    else:
                        self._codebook_runtime_filter_dependent[dep] = [key]
                provider = c.codebook_runtime_filter.function()
                e = c.type.enumerator()
                e.set_runtime_filter_provider(provider, (self,))
                
    def _init_row(self, row, prefill=None):
        self._cache = {}
        if row is None:
            for key in self._dirty.keys():
                self._dirty[key] = True
            row_data = [(c.id(), self._default(c.id(), prefill=prefill))
                        for c in self._data.columns()]
            row = pytis.data.Row(row_data)
        else:
            if isinstance(row, pytis.data.Row):
                row = copy.copy(row)
            elif isinstance(row, PresentedRow):
                row = copy.copy(row._row)
            else:
                raise Exception('Invalid argument row:', row)
            if prefill:
                row.update(prefill)
            for key in self._dirty.keys():
                self._dirty[key] = not row.has_key(key)
        return row

    def _default(self, key, prefill=None):
        if prefill and prefill.has_key(key):
            value = prefill[key]
            if self._dirty.has_key(key):
                # Prefill má přednost před computerem, protože někdy
                # chceme v procedurách mít možnost ve formuláři za
                # nějakých okolností přednastavit jinou hodnotu, než
                # jaká by byla computerem normálně vypočtena.
                self._dirty[key] = False
        elif self._columns.has_key(key):
            col = self._columns[key]
            default = col.default
            if self._new and default is not None:
                if callable(default):
                    default = default()
                value = pytis.data.Value(col.type, default)
                if self._dirty.has_key(key):
                    self._dirty[key] = False
            else:
                value = col.type.default_value()
        else:
            value = self._data.find_column(key).type().default_value()
        return value

    def __getitem__(self, key):
        """Vrať hodnotu políčka 'key' jako instanci třídy 'pytis.data.Value'.
        
        'key' je id políčka (řetězec) identifikující existující políčko, jinak
        je chování metody nedefinováno.
        
        """
        if self._row.has_key(key):
            value = self._row[key]
        else:
            value = self._virtual[key]
        if self._dirty.has_key(key) and self._dirty[key]:
            column = self._columns[key]
            # Nastavením dirty na False už zde zamezíme rekurzi v případě, že
            # se kód computeru ptá na vlastní hodnotu a umožníme mu tak zjistit
            # původní hodnotu (před přepočítáním).
            self._dirty[key] = False
            func = column.computer.function()
            new_value = pytis.data.Value(column.type, func(self))
            if new_value.value() != value.value():
                value = new_value
                if self._row.has_key(key):
                    self._row[key] = value
                else:
                    self._virtual[key] = value
                if self._change_callback is not None:
                    self._change_callback(key)
        return value

    def __setitem__(self, key, value):
        assert isinstance(value, pytis.data.Value)
        column = self._columns[key]
        assert value.type() == column.type, \
               "Invalid type for '%s': %s (expected %s)" % \
               (key, value.type(), column.type)
        self._cache = {}
        if self._row.has_key(key) and self._row[key] != value:
            self._row[key] = value
        elif self._virtual.has_key(key) and self._virtual[key] != value:
            self._virtual[key] = value
        else:
            return
        self._resolve_dependencies(key)
                
    def __str__(self):
        if hasattr(self, '_row'):
            items = []
            for spec in self._fieldspec:
                items.append(spec.id() + '=' + str(self[spec.id()]))
            return '<PresentedRow: %s>' % string.join(items, ', ')
        else:
            return super(PresentedRow, self).__str__()

    def _mark_dependent_dirty(self, key):
        # Rekurzivně označ závislá políčka.
        # Vrať pravdu, pokud k označení nějakých políček došlo.
        if self._dependent.has_key(key):
            for k in self._dependent[key]:
                self._dirty[k] = True
                self._mark_dependent_dirty(k)
            return True
        else:
            return False
    
    def _resolve_dependencies(self, key=None):
        # Recompute dependencies for all fields when key is None or recompute
        # just fields depending on a given field (after its change).
        # TODO: Musí se to dělat vždy?  Např. i při set_row z BrowseFormu?
        if key is None:
            invoke_callbacks = False
        else:
            invoke_callbacks = self._mark_dependent_dirty(key)
        self._notify_runtime_filter_change(key)
        self._recompute_editability(key)
        if invoke_callbacks and self._change_callback is not None:
            # Zavolej 'chage_callback' pro všechna zbylá "dirty" políčka.
            # Políčka, která byla označena jako "dirty" již buďto byla
            # přepočítána a callback byl zavolán během přepočítávání
            # editovatelnosti a runtime codebooků, nebo zůstala "dirty" a
            # musíme tedy jejich callback zavolat teď.
            dirty = [k for k in self._dirty.keys() if self._dirty[k]]
            for k in dirty:
                self._change_callback(k)
    
    def _recompute_editability(self, key=None):
        if key is None:
            keys = self._editable.keys()
        elif self._editability_dependent.has_key(key):
            keys = self._editability_dependent[key]
        else:
            return
        if self._editability_change_callback:
            for k in keys:
                old = self._editable[k]
                new = self._compute_editability(k)
                if old != new:
                    self._editability_change_callback(k, new)
        else:
            for k in keys:
                self._editability_dirty[k] = True

    def _compute_editability(self, key):
        # Vypočti editovatelnost políčka a vrať výsledek (jako boolean).
        func = self._columns[key].editable.function()
        self._editable[key] = result = func(self, key)
        self._editability_dirty[key] = False
        return result
    
    def _notify_runtime_filter_change(self, key=None):
        if key is None:
            columns = [c for c in self._columns.values()
                       if c.codebook_runtime_filter is not None]
        elif self._codebook_runtime_filter_dependent.has_key(key):
            columns = [self._columns[k]
                       for k in self._codebook_runtime_filter_dependent[key]]
        else:
            return
        for c in columns:
            c.type.enumerator().notify_runtime_filter_change()
 
    def row(self):
        """Vrať aktuální datový řádek, jako instanci 'pytis.data.Row'.

        Typy sloupců takto vráceného řádku jsou shodné s typy z datového
        objektu, pro sloupce v datovém objektu přítomné.

        """
        data = self._data
        row_data = []
        for key, value in self._row.items():
            c = data.find_column(key)
            if c is not None:
                if self._dirty.has_key(key) and self._dirty[key]:
                    value = self[key]
                value = pytis.data.Value(c.type(), value.value())
            row_data.append((key, value))
        return pytis.data.Row(row_data)

    def data(self):
        """Vrať odpovídající datový objekt řádku."""
        return self._data

    def format(self, key, **kwargs):
        """Vrať stringovou hodnotu políčka 'key'.

        Argumenty:

          'key' -- id políčka (řetězec) identifikující existující políčko,
            jinak je chování metody nedefinováno.
          'kwargs' -- klíčové argumenty které budou použity při volání metody
            'export()' pro získání řetězcové reprezentace hodnoty.
        
        """
        try:
            return self._cache[key]
        except KeyError:
            pass
        try:
            value = self[key]
        except KeyError:
            # Může nastat například v případě, kdy k danému sloupci nejsou
            # přístupová práva.
            svalue = ''
        else:
            svalue = value.export(**kwargs)
        column = self._columns[key]
        if self._singleline and column.line_separator is not None:
            svalue = string.join(svalue.splitlines(), column.line_separator)
        self._cache[key] = svalue
        return svalue

    def set_row(self, row, reset=False):
        """Nastav aktuální data na 'row'.

        'row' má stejný význam jako stejnojmenný argument metody '__init__()'.

        Pravdivá hodnota argumentu 'reset' způsobí to, že tato nová hodnota
        řádku bude nadále považována za původní, což má vliv na funkci metod
        'changed()' a 'original_row()'.

        Tuto metodu je vhodné využívat pro koncepci aktuálního řádku v tabulce
        s neměnnými sloupci a datovým objektem.  Ušetří se tak chroustání
        specifikací uvnitř této třídy.
        
        """
        self._set_row(row, reset=reset)

    def fields(self):
        """Vrať seznam všech políček."""
        return self._fieldspec
        
    def has_key(self, key):
        """Vrať pravdu, pokud je políčko daného klíče v řádku obsaženo."""
        return self._columns.has_key(key)
        
    def keys(self):
        """Vrať seznam identifikátorů všech políček obsažených v tomto řádku."""
        return self._columns.keys()
        
    def original_row(self, empty_as_none=False):
        """Vrať řádek obsahující původní hodnoty řádku před případnými změnami.

        Vrácená hodnota je instance 'pytis.data.Row', ne nutně totožná (ve
        smyslu 'id()') s řádkem zadaným v konstruktoru.

        Původními hodnotami jsou myšleny hodnoty řádku předaného konstruktoru,
        nebo poslednímu volání metody 'set_row()', s pravdivým argumentem
        'reset'.
        
        """
        if empty_as_none and self._original_row_empty:
            return None
        else:
            return self._original_row

    def changed(self):
        """Vrať pravdu, právě když byl řádek změněn.

        Řádek se považuje za změněný, není-li shodný s řádkem vytvořeným z dat
        zadaných v konstruktoru, ve smyslu operátoru `='.

        """
        return self._row != self._original_row

    def field_changed(self, key):
        """Vrať pravdu, právě když bylo políčko dané 'key' změněno.

        """
        return self._row.has_key(key) and \
               self._row[key] != self._original_row[key]

    def new(self):
        """Vrať pravdu, právě když se jedná o nový záznam."""
        return self._new
    
    def editable(self, key):
        """Vrať pravdu, právě když je políčko dané 'key' editovatelné.

        Význam argumentu 'key' je stejný jako v metodě '__getitem__'.

        """
        if self._editable.has_key(key):
            if self._editability_dirty[key]:
                return self._compute_editability(key)
            else:
                return self._editable[key]
        else:
            editable = self._columns[key].editable
            return editable == Editable.ALWAYS or \
                   (editable == Editable.ONCE and self._new)

    # Nakonec to není nikde potřeba, ale kdyby, stačí odkomentovat a dopsat
    # test...
    #def accessible(self, key, permission):
    #    """Vrať pravdu, právě když má uživatel právo přístupu k danému políčku.
    #
    #    Argumenty:
    #    
    #      'key' -- stejně jako v metodě '__getitem__'.
    #      'permission' -- jedna z konstant třídy 'pytis.data.Permission'
    #        určující, které přístupové právo se má testovat.
    #
    #    Pokud dané políčko není součástí datového řádku (jde o virtuální
    #    políčko), vrací vždy None.
    #        
    #    """
    #    if self._row.has_key(key):
    #        return self._data.accessible(key, permission)
    #    else:
    #        return None


        
    
    def _display_func(self, column):
        def getval(enum, value, col, func=None):
            if value is None:
                return ''
            try:
                v = enum.get(value, col)
            except pytis.data.DataAccessException:
                return ''
            if not v:
                return ''
            elif func:
                return f(v.value())
            else:
                return v.export()
        display = column.display
        if not display and column.codebook:
            try:
                cb_spec = self._resolver.get(column.codebook, 'cb_spec')
            except ResolverError, e:
                pass
            else:
                display = cb_spec.display()
        if not display:
            return None
        elif callable(display):
            return display
        enum = column.type.enumerator()
        if isinstance(display, tuple):
            f, col = display
            return lambda v: getval(enum, v, col, f)
        else:
            return lambda v: getval(enum, v, display)

    def display(self, key):
        """Vrať hodnotu displeje číselníku daného políčka.

        Pokud dané políčko není číselníkem, nebo tento číselník nemá určen
        displej, nebo aktuální hodnota políčka není v číselníku nalezena, nebo
        k nejsou dostatečná práva k jejímu načtení, bude vrácen prázdný
        řetězec.
        
        """
        column = self._columns[key]
        display = self._display_func(column)
        if not display:
            computer = column.computer
            if computer and isinstance(computer, CbComputer):
                column = self._columns[computer.field()]
                display = self._display_func(column)
        if display:
            return display(self[column.id].value())
        else:
            return ''
    
    def enumerate(self, key):
        """Vrať výčet hodnot číselníku daného políčka jako seznam dvojic.

        Vrácený seznam obsahuje vždy vnitřní Pythonovou hodnotu číselníku a k
        ní odpovídající uživatelskou hodnotu jako řetězec.  Uživatelská hodnota
        je určena specifikací `display'.
        
        Vyvolání této metody pro políčko, které není číselníkové je považováno
        za chybu.
       
        """
        column = self._columns[key]
        display = self._display_func(column)
        if display is None:
            display = lambda v: pytis.data.Value(column.type, v).export()
        return [(v, display(v)) for v in column.type.enumerator().values()]



    
