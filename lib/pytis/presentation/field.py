# -*- coding: iso-8859-2 -*-

# Prezentace dat v políčkách.
# 
# Copyright (C) 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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


class PresentedRow:
    """Řádek prezentovaných dat.

    Třída je mezičlánkem mezi datovým řádkem a jeho finální prezentací.  Na
    rozdíl od datového řádku obsahuje všechna políčka dané specifikacemi
    políček 'FieldSpec'.  Na druhou stranu ale již neřeší konkrétní prezentaci
    dat přesahující jejich zformátování do stringu.

    """
    class _Column:
        def __init__(self, id_, type_, computer, separator, line_separator,
                     default, editable, check, codebook_runtime_filter):
            self.id = id_
            self.type = type_
            self.computer = computer
            self.separator = separator
            self.line_separator = line_separator
            self.default = default
            self.editable = editable
            self.check = check
            self.codebook_runtime_filter = codebook_runtime_filter
            
    def __init__(self, fieldspec, data, row, prefill=None, singleline=False,
                 change_callback=None, enable_field_callback=None,
                 disable_field_callback=None, new=False):
        """Inicializuj prezentaci řádku.
        
        Argumenty:

          fieldspec -- sekvence specifikací políček, instancí třídy
            'FieldSpec'
          data -- odpovídající datový objekt, instance třídy 'pytis.data.Data'
          row -- data řádku, viz níže
          prefill -- slovník hodnot pro inicializaci řádku namísto výchozích
            hodnot. Slovník je klíčovaný přes textový identifikátor sloupce.
            Hodnotami jsou instance třídy Value, v případě nového záznamu
            (argument 'row' je 'None') mohou být použity i uživatelské hodnoty.
            Takto lze předvyplňit pouze políčka, k nimž existuje odpovídající
            sloupec v datovém objektu -- plně virtuální políčka mají hodnotu
            vždy určenou pomocí computeru (viz. argument 'computer'
            konstruktoru třídy 'FieldSpec').
          singleline -- právě když je pravdivé, stringové hodnoty všech políček
            budou zformátovány jako jednořádkové
          change_callback -- funkce jednoho argumentu (id políčka) volaná při
            nepřímé změně políčka (tj. při přepočítávání hodnot), která
            oznamuje \"nečekané\" změny políček v prezentovaném row; je-li
            'None', není žádná taková funkce volána
          enable_field_callback -- funkce jednoho argumentu (id políčka) volaná
            při nepřímé změně editovatelnosti políčka.  Tato funkce oznamuje,
            že v důsledku změny v políčkách, na kterých editovatelnost daného
            políčka závisí se dané políčko stalo editovatelným; je-li 'None',
            není žádná taková funkce volána
          disable_field_callback -- funkce jednoho argumentu (id políčka)
            volaná, když se políčko stává needitovatelným - obdoba
            'enable_field_callback'
          new -- flag určující, zda se jedná o nově vytvářený záznam (nikoliv
            editaci záznamu již existujícího)

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
#         assert isinstance(data, pytis.data.Data) or \
#                isinstance(data, pytis.remote.RemoteData)
        self._fieldspec = fieldspec
        self._data = data
        self._singleline = singleline
        self._change_callback = change_callback
        self._enable_field_callback = enable_field_callback
        self._disable_field_callback = disable_field_callback
        self._process_fieldspec()
        self._row = self._init_row(row, prefill=prefill)
        self._original_row = copy.copy(self._row)
        self._new = new
        self._cache = {}
        self._invoke_callbacks()
        self._recompute_dependencies()

    def _process_fieldspec(self):
        data = self._data
        # Pro každé políčko si zapamatuji seznam počítaných políček, která na
        # něm závisí (obrácené mapování než ve specifikacích).
        self._dependent = {}
        self._editability_dependent = {}
        self._codebook_runtime_filter_dependent = {}
        # Pro všechna počítaná políčka si pamatuji, zda potřebují přepočítat,
        # či nikoliv (po přepočítání je políčko čisté, po změně políčka na
        # kterém závisí jiná políčka nastavím závislým políčkům příznak dirty).
        # Přepočítávání potom mohu provádět až při skutečném požadavku na
        # získání hodnoty políčka.
        self._dirty = {}
        self._editability_dirty = {}
        self._editable = {}
        self._columns = {}
        self._refvalues = {}
        for f in self._fieldspec:
            key = f.id()
            c = self._Column(key, f.type(data), f.computer(), f.separator(),
                             f.line_separator(), f.default(), f.editable(),
                             f.check(), f.codebook_runtime_filter())
            self._columns[key] = c
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
                for dep in c.editable.depends():
                    if self._editability_dependent.has_key(dep):
                        self._editability_dependent[dep].append(key)
                    else:
                        self._editability_dependent[dep] = [key]
            if c.codebook_runtime_filter is not None:
                for dep in c.codebook_runtime_filter.depends():
                    if self._codebook_runtime_filter_dependent.has_key(dep):
                        self._codebook_runtime_filter_dependent[dep].append(key)
                    else:
                        self._codebook_runtime_filter_dependent[dep] = [key]
                provider = c.codebook_runtime_filter.function()
                c.type.set_runtime_filter_provider(provider, (self,))
                        
    def _init_row(self, row, prefill=None):
        self._cache = {}
        if row is None:
            def genval(c):
                id = c.id()
                if prefill is not None and prefill.has_key(id):
                    value = prefill[id]
                    if not isinstance(value, pytis.data.Value):
                        value = pytis.data.Value(c.type(), value)
                    else:
                        # Pro Codebooky raději taky vytvoříme novou
                        # instanci
                        value = pytis.data.Value(c.type(), value.value())
                    if self._dirty.has_key(id):
                        self._dirty[id] = False
                else:
                    if self._columns.has_key(id):
                        field = self._columns[id]
                        default = field.default
                        t = field.type
                        if default is None:
                            value = t.default_value()
                        else:
                            value = pytis.data.Value(t, default())
                            if self._dirty.has_key(id):
                                self._dirty[id] = False
                    else:
                        value = c.type().default_value()
                return id, value
            for id in self._dirty.keys():
                self._dirty[id] = True
            row_data = map(genval, self._data.columns())
            row = pytis.data.Row(row_data)
        else:
            if isinstance(row, pytis.data.Row):
                row = copy.copy(row)
            elif isinstance(row, PresentedRow):
                row = copy.copy(row._row)
            else:
                raise Exception('Invalid argument row:', row)
            if prefill is not None:
                row.update(prefill)
            for id in self._dirty.keys():
                self._dirty[id] = not row.has_key(id)
        return row

    def __getitem__(self, key):
        """Vrať hodnotu políčka 'key' jako instanci třídy 'pytis.data.Value'.
        
        'key' je id políčka (řetězec) identifikující existující políčko, jinak
        je chování metody nedefinováno.
        
        """
        column = self._columns[key]
        if column.computer and self._needs_recomputation(column.id):
            value = pytis.data.Value(column.type, self._compute(column))
        else:
            value = self._row[column.id]
        return value

    def __setitem__(self, key, value):
        assert isinstance(value, pytis.data.Value)
        self._cache = {}
        # Pokus o nastavení virtuálních políček tiše ignorujeme...
        if self._row.has_key(key) and self._row[key] != value:
            self._row[key] = value
            if self._mark_dependent_dirty(key):
                self._invoke_callbacks()
            self._recompute_dependencies(key)
                
    def __str__(self):
        items = []
        for spec in self._fieldspec:
            items.append(spec.id() + '=' + str(self[spec.id()]))
        return '<PresentedRow: %s>' % string.join(items, ', ')

    def _mark_dependent_dirty(self, key):
        # Rekurzivně označ závislá políčka.
        # Vrať pravdu, pokud k označení nějakých políček došlo.
        if self._dependent.has_key(key):
            for id in self._dependent[key]:
                self._dirty[id] = True
                self._mark_dependent_dirty(id)
            return True
        else:
            return False
    
    def _invoke_callbacks(self):
        # Zavolej `chage_callback' pro všechna ``dirty'' políčka.
        if self._change_callback is not None:
            keys = filter(lambda key: self._dirty[key], self._dirty.keys())
            for key in keys:
                self._change_callback(key)

    def _needs_recomputation(self, id):
        # Vrať pravdu, pokud jde o počítané políčko, které je třeba vypočítat.
        return not self._row.has_key(id) or self._dirty[id]

    def _compute(self, column):
        # Vypočti a vrať aktuální hodnotu políčka (jako Pythonovou hodnotu).
        id = column.id
        value = column.computer.function()(self)
        self._dirty[id] = False
        if self._row.has_key(id):
            self._row[id] = pytis.data.Value(column.type, value)
        return value

    def _recompute_dependencies(self, key=None):
        # recompute dependencies for all fields when key is None or recompute
        # just fields depending on a field specified by key (after its change).
        self._recompute_editability(key)
        self._recompute_codebook_runtime_filter(key)
    
    def _recompute_editability(self, key=None):
        if key is None:
            ids = self._editable.keys()
        elif self._editability_dependent.has_key(key):
            ids = self._editability_dependent[key]
        else:
            return
        for id in ids:
            if self._enable_field_callback or self._disable_field_callback:
                old = self._editable[id]
                self._editable[id] = new = self._compute_editability(id)
                self._editability_dirty[id] = False
                if not old and new and self._enable_field_callback:
                    self._enable_field_callback(id)
                if old and not new and self._disable_field_callback:
                    self._disable_field_callback(id)
            else:
                self._editability_dirty[id] = True

    def _compute_editability(self, key):
        # Vypočti editovatelnost políčka a vrať výsledek (jako boolean).
        func = self._columns[key].editable.function()
        return func(self, key)
    
    def _recompute_codebook_runtime_filter(self, key=None):
        if key is None:
            ids = [id for id in self._columns.keys()
                   if self._columns[id].codebook_runtime_filter is not None]
        elif self._codebook_runtime_filter_dependent.has_key(key):
            ids = self._codebook_runtime_filter_dependent[key]
        else:
            return
        for id in ids:
            c = self._columns[id]
            c.type.notify_runtime_filter_change()
 

    def row(self):
        """Vrať aktuální datový řádek, jako instanci 'pytis.data.Row'.

        Typy sloupců takto vráceného řádku jsou shodné s typy z datového
        objektu, pro sloupce v datovém objektu přítomné.

        """
        data = self._data
        row_data = []
        for id, value in self._row.items():
            c = data.find_column(id)
            if c is None:
                ok_value = value
            elif self._dirty.has_key(id) and self._dirty[id]:
                ok_value = pytis.data.Value(c.type(),
                                          self._compute(self._columns[id]))
            else:
                ok_value = pytis.data.Value(c.type(), value.value())
            row_data.append((id, ok_value))
        result = pytis.data.Row(row_data)
        return result

    def data(self):
        """Vrať odpovídající datový objekt řádku."""
        return self._data

    def format(self, key, **kwargs):
        """Vrať stringovou hodnotu políčka 'key'.

        Argumenty:

          'key' -- id políčka (řetězec) identifikující existující políčko,
            jinak je chování metody nedefinováno.
          'kwargs' -- klíčové argumenty které budou použity při volání metody
            'export()' pro získáné řetězcové reprezentace hodnoty.
        
        """
        try:
            return self._cache[key]
        except KeyError:
            pass
        column = self._columns[key]
        if column.computer and self._needs_recomputation(column.id):
            value = self._compute(column)
            try:
                svalue = column.type.export(value, **kwargs)
            except Exception, e:
                raise ProgramError("Computer returned an incopatible value:",
                                   key, column.computer.function(),
                                   value, type(value))
        else:
            try:
                value = self._row[column.id]
            except KeyError:
                # Může nastat například v případě, kdy k danému sloupci nejsou
                # přístupová práva.
                svalue = None
            else:
                svalue = value.export(**kwargs)
        if svalue is None:
            svalue = ''
        if self._singleline and column.line_separator is not None:
            svalue = string.join(svalue.splitlines(), column.line_separator)
        self._cache[key] = svalue
        assert not is_sequence(svalue)
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
        self._row = self._init_row(row)
        if reset:
            self._original_row = copy.copy(self._row)
        self._invoke_callbacks()
        self._recompute_dependencies()

    def fields(self):
        """Vrať seznam všech políček."""
        return self._fieldspec
        
    def has_key(self, key):
        """Vrať pravdu, pokud je políčko daného klíče v řádku obsaženo."""
        return self._columns.has_key(key)
        
    def keys(self):
        """Vrať seznam identifikátorů všech políček obsažených v tomto řádku."""
        return self._columns.keys()
        
    def original_row(self):
        """Vrať řádek obsahující původní hodnoty řádku před případnými změnami.

        Vrácená hodnota je instance 'pytis.data.Row' nebo 'None', ne nutně
        totožná (ve smyslu 'id()') s řádkem zadaným v konstruktoru.

        Pozor, pokud byl řádek předaný konstruktoru `None', metoda sice vrátí
        inicializovaný prázdný řádek, ale hodnoty počítaných políček v něm
        nebudou být vypočteny.
        
        """
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
        if not self._row.has_key(key):
            return False
        return self._row[key] != self._original_row[key]

    def new(self):
        """Vrať pravdu, právě když se jedná o nový záznam."""
        return self._new
    
    def editable(self, key):
        """Vrať pravdu, právě když je políčko dané 'key' editovatelné.

        Význam argumentu 'key' je stejný jako v metodě '__getitem__'.

        """
        editable = self._columns[key].editable
        if self._editable.has_key(key):
            if self._editability_dirty[key]:
                self._editable[key] = self._compute_editability(key)
                self._editability_dirty[key] = False
            return self._editable[key]
        else:
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

    def check(self):
        """Proveď kontrolu vzájemné integrity dat řádku.
        
        Metoda provede všechny existující 'check' funkce definované ve
        'FieldSpec' obsažených políček.  Při neúspěchu kontroly některého
        políčka není prováděna žádná akce, pouze je vráceno id tohoto políčka a
        provedeno zalogování.  Očekává se, že případná interakce s uživatelem
        je prováděna v rámci check funkce.

        Vrací: Id políčka, pokud některá kontrolní funkce neprojde, nebo None v
        případě, že je vše v pořádku.

        """
        # TODO: Tato metoda bude zrušena, hned jak se přestanou používat funkce
        # 'check' ve 'FieldSpec', kteréžto mají být nahrazeny stejnojmennou
        # funkcí ve 'ViewSpec', ke které zde však není přístup, takže se volá
        # na úrovni formuláře.
        for spec in self._fieldspec:
            c = self._columns[spec.id()]
            if c.check is not None and not c.check(self):
                log(EVENT, 'Kontrola integrity selhala:', (c.id, self))
                return c.id
        return None

    def listfield_choose(self, key, value):
        """Ošetři výběr položky u ListField políčka."""

        self._refvalues[key] = value
        self._dirty[key] = True
        if self._mark_dependent_dirty(key):
            self._invoke_callbacks()
        
    def refvalue(self, key):
        """Vrátí vybranou hodnotu z ListField.

        Typickým použitím je zjištění vybrané hodnoty v computerech,
        které jsou závislé na ListFieldu.
        """
        
        if self._refvalues.has_key(key):
            return self._refvalues[key]
        return None
