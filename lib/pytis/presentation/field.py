# -*- coding: iso-8859-2 -*-

# Prezentace dat v�pol��k�ch.
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

"""Prezentace dat v�pol��k�ch.

_Pol��kem_ se zde rozum� abstraktn� element u�ivatelsk�ho rozhran� p�ij�maj�c�
textov� data, nikoliv konkr�tn� forma zobrazen� dat.

"""

import copy

import pytis.data

from pytis.presentation import *
from pytis.util import *


class PresentedRow(object):
    """��dek prezentovan�ch dat.

    T��da je mezi�l�nkem mezi datov�m ��dkem a jeho fin�ln� prezentac�.  Na
    rozd�l od datov�ho ��dku obsahuje v�echna pol��ka dan� specifikacemi
    pol��ek 'FieldSpec'.  Na druhou stranu ale ji� ne�e�� konkr�tn� prezentaci
    dat p�esahuj�c� jejich zform�tov�n� do stringu.

    """
    class _Column:
        def __init__(self, id_, type_, computer, line_separator,
                     default, editable, check, codebook_runtime_filter):
            self.id = id_
            self.type = type_
            self.computer = computer
            self.line_separator = line_separator
            self.default = default
            self.editable = editable
            self.check = check
            self.codebook_runtime_filter = codebook_runtime_filter
            
    def __init__(self, fieldspec, data, row, prefill=None, singleline=False,
                 change_callback=None, editability_change_callback=None,
                 new=False):
        """Inicializuj prezentaci ��dku.
        
        Argumenty:

          fieldspec -- sekvence specifikac� pol��ek, instanc� t��dy
            'FieldSpec'
          data -- odpov�daj�c� datov� objekt, instance t��dy 'pytis.data.Data'
          row -- data ��dku, viz n�e
          prefill -- slovn�k hodnot pro inicializaci ��dku nam�sto v�choz�ch
            hodnot. Slovn�k je kl��ovan� p�es textov� identifik�tor sloupce.
            Hodnotami jsou instance t��dy Value, v p��pad� nov�ho z�znamu
            (argument 'row' je 'None') mohou b�t pou�ity i u�ivatelsk� hodnoty.
            Takto lze p�edvypl�it pouze pol��ka, k nim� existuje odpov�daj�c�
            sloupec v datov�m objektu -- pln� virtu�ln� pol��ka maj� hodnotu
            v�dy ur�enou pomoc� computeru (viz. argument 'computer'
            konstruktoru t��dy 'FieldSpec').
          singleline -- pr�v� kdy� je pravdiv�, stringov� hodnoty v�ech pol��ek
            budou zform�tov�ny jako jedno��dkov�
          change_callback -- funkce jednoho argumentu (id pol��ka) volan� p�i
            nep��m� zm�n� pol��ka (tj. p�i p�epo��t�v�n� hodnot), kter�
            oznamuje \"ne�ekan�\" zm�ny pol��ek v prezentovan�m row; je-li
            'None', nen� ��dn� takov� funkce vol�na
          editability_change_callback -- funkce dvou argument� (id pol��ka,
            p��znak editovatelnosti) volan� p�i nep��m� zm�n� editovatelnosti
            pol��ka.  Vol�n�m t�to funkce ��dek oznamuje, �e v d�sledku zm�ny v
            jin�ch pol��k�ch se dan� pol��ko stalo editovateln�m (druh�
            argument je pravdiv�), �i naopak (druh� argument je nepravdiv�);
            je-li 'None', nen� zm�na editovatelnosti oznamov�na.
          new -- flag ur�uj�c�, zda se jedn� o�nov� vytv��en� z�znam (nikoliv
            editaci z�znamu ji� existuj�c�ho)

        Prezenta�n� podoba je vytvo�ena z�dat specifikovan�ch argumentem 'row',
        kter� m��e m�t n�kterou z�n�sleduj�c�ch hodnot:

          None -- bude vytvo�en zbrusu nov� ��dek odpov�daj�c� 'fieldspec'
          instance 'PresentedRow' -- bude vytvo�ena kopie zadan�ho ��dku, oba
            mus� m�t shodnou specifikaci pol��ek
          instance 'pytis.data.Row' -- bude vytvo�ena prezentace z�dan�ho
            datov�ho ��dku

        Ve v�ech p��padech je rozhoduj�c� podoba 'row' v�okam�iku vol�n� tohoto
        konstruktoru, pozd�j�� p��padn� destruktivn� zm�ny 'row' nemaj� na nov�
        vytvo�enou instanci t��dy 'PresentedRow' vliv.

        """
        assert is_sequence(fieldspec)
        # TODO: pytis.remote vy�aduje inicializaci Pyro, co� nen� v�dy to prav�
        # o�echov�.  `data' by stejn� m�lo b�t jednotn�ho typu, je t�eba to
        # n�jak promyslet.
        #assert isinstance(data, pytis.data.Data) or \
        #       isinstance(data, pytis.remote.RemoteData)
        assert row is None or isinstance(row, (PresentedRow, pytis.data.Row))
        assert change_callback is None or callable(change_callback)
        assert editability_change_callback is None or \
               callable(editability_change_callback)
        assert prefill is None or isinstance(prefill, types.DictType)
        assert isinstance(singleline, types.BooleanType)
        assert isinstance(new, types.BooleanType)
        self._fieldspec = fieldspec
        self._data = data
        self._singleline = singleline
        self._change_callback = change_callback
        self._editability_change_callback = editability_change_callback
        self._process_fieldspec()
        self._virtual = {}
        self._new = new
        self._cache = {}
        self._set_row(row, prefill=prefill)

    def _set_row(self, row, reset=True, prefill=None):
        self._row = self._init_row(row, prefill=prefill)
        if reset:
            self._original_row = copy.copy(self._row)
        self._resolve_dependencies()
        
    def _process_fieldspec(self):
        data = self._data
        # Pro ka�d� pol��ko si zapamatuji seznam po��tan�ch pol��ek, kter� na
        # n�m z�vis� (obr�cen� mapov�n� ne� ve specifikac�ch).
        self._dependent = {}
        self._editability_dependent = {}
        self._codebook_runtime_filter_dependent = {}
        # Pro v�echna po��tan� pol��ka si pamatuji, zda pot�ebuj� p�epo��tat,
        # �i nikoliv (po p�epo��t�n� je pol��ko �ist�, po zm�n� pol��ka na
        # kter�m z�vis� jin� pol��ka nastav�m z�visl�m pol��k�m p��znak dirty).
        # P�epo��t�v�n� potom mohu prov�d�t a� p�i skute�n�m po�adavku na
        # z�sk�n� hodnoty pol��ka.
        self._dirty = {}
        self._editability_dirty = {}
        self._editable = {}
        self._columns = {}
        for f in self._fieldspec:
            key = f.id()
            c = self._Column(key, f.type(data), f.computer(),
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
                e = c.type.enumerator()
                e.set_runtime_filter_provider(provider, (self,))
                        
    def _init_row(self, row, prefill=None):
        self._cache = {}
        if row is None:
            def genval(c):
                key = c.id()
                if prefill is not None and prefill.has_key(key):
                    value = prefill[key]
                    # Pro Codebooky rad�ji taky vytvo��me novou instanci
                    if not isinstance(value, pytis.data.Value):
                        value = pytis.data.Value(c.type(), value)
                    else:
                        value = pytis.data.Value(c.type(), value.value())
                    if self._dirty.has_key(key):
                        self._dirty[key] = False
                else:
                    if self._columns.has_key(key):
                        field = self._columns[key]
                        default = field.default
                        t = field.type
                        if default is None:
                            value = t.default_value()
                        else:
                            value = pytis.data.Value(t, default())
                            if self._dirty.has_key(key):
                                self._dirty[key] = False
                    else:
                        value = c.type().default_value()
                return key, value
            for key in self._dirty.keys():
                self._dirty[key] = True
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
            for key in self._dirty.keys():
                self._dirty[key] = not row.has_key(key)
        return row

    def __getitem__(self, key):
        """Vra� hodnotu pol��ka 'key' jako instanci t��dy 'pytis.data.Value'.
        
        'key' je id pol��ka (�et�zec) identifikuj�c� existuj�c� pol��ko, jinak
        je chov�n� metody nedefinov�no.
        
        """
        if self._row.has_key(key):
            value = self._row[key]
        else:
            value = self._virtual.get(key)
        if value is None or self._dirty.has_key(key) and self._dirty[key]:
            column = self._columns[key]
            func = column.computer.function()
            new_value = pytis.data.Value(column.type, func(self))
            self._dirty[key] = False
            if value is None or new_value.value() != value.value():
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
        # Pokus o nastaven� virtu�ln�ch pol��ek ti�e ignorujeme...
        if self._row.has_key(key) and self._row[key] != value:
            self._row[key] = value
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
        # Rekurzivn� ozna� z�visl� pol��ka.
        # Vra� pravdu, pokud k ozna�en� n�jak�ch pol��ek do�lo.
        if self._dependent.has_key(key):
            for k in self._dependent[key]:
                self._dirty[k] = True
                self._mark_dependent_dirty(k)
            return True
        else:
            return False
    
    def _resolve_dependencies(self, key=None):
        # Recompute dependencies for all fields when key is None or recompute
        # just fields depending on a field specified by key (after its change).
        # TODO: Mus� se to d�lat v�dy?  Nap�. i p�i set_row z BrowseFormu?
        invoke_callbacks = False
        if key:
            invoke_callbacks = self._mark_dependent_dirty(key)
        self._notify_runtime_filter_change(key)
        self._recompute_editability(key)
        if invoke_callbacks and self._change_callback is not None:
            # Zavolej 'chage_callback' pro v�echna zbyl� "dirty" pol��ka.
            # Pol��ka, kter� byla ozna�ena jako "dirty" ji� bu�to byla
            # p�epo��t�na a callback byl zavol�n b�hem p�epo��t�v�n�
            # editovatelnosti a runtime codebook�, nebo z�stala "dirty" a mus�me
            # tedy jejich callback zavolat te�.
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
        # Vypo�ti editovatelnost pol��ka a vra� v�sledek (jako boolean).
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
        """Vra� aktu�ln� datov� ��dek, jako instanci 'pytis.data.Row'.

        Typy sloupc� takto vr�cen�ho ��dku jsou shodn� s�typy z�datov�ho
        objektu, pro sloupce v�datov�m objektu p��tomn�.

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
        """Vra� odpov�daj�c� datov� objekt ��dku."""
        return self._data

    def format(self, key, **kwargs):
        """Vra� stringovou hodnotu pol��ka 'key'.

        Argumenty:

          'key' -- id pol��ka (�et�zec) identifikuj�c� existuj�c� pol��ko,
            jinak je chov�n� metody nedefinov�no.
          'kwargs' -- kl��ov� argumenty kter� budou pou�ity p�i vol�n� metody
            'export()' pro z�sk�n� �et�zcov� reprezentace hodnoty.
        
        """
        try:
            return self._cache[key]
        except KeyError:
            pass
        try:
            value = self[key]
        except KeyError:
            # M��e nastat nap��klad v�p��pad�, kdy k�dan�mu sloupci nejsou
            # p��stupov� pr�va.
            svalue = ''
        else:
            svalue = value.export(**kwargs)
        column = self._columns[key]
        if self._singleline and column.line_separator is not None:
            svalue = string.join(svalue.splitlines(), column.line_separator)
        self._cache[key] = svalue
        return svalue

    def set_row(self, row, reset=False):
        """Nastav aktu�ln� data na 'row'.

        'row' m� stejn� v�znam jako stejnojmenn� argument metody '__init__()'.

        Pravdiv� hodnota argumentu 'reset' zp�sob� to, �e tato nov� hodnota
        ��dku bude nad�le pova�ov�na za p�vodn�, co� m� vliv na funkci metod
        'changed()' a 'original_row()'.

        Tuto metodu je vhodn� vyu��vat pro koncepci aktu�ln�ho ��dku v�tabulce
        s�nem�nn�mi sloupci a datov�m objektem.  U�et�� se tak chroust�n�
        specifikac� uvnit� t�to t��dy.
        
        """
        self._set_row(row, reset=reset)

    def fields(self):
        """Vra� seznam v�ech pol��ek."""
        return self._fieldspec
        
    def has_key(self, key):
        """Vra� pravdu, pokud je pol��ko dan�ho kl��e v ��dku obsa�eno."""
        return self._columns.has_key(key)
        
    def keys(self):
        """Vra� seznam identifik�tor� v�ech pol��ek obsa�en�ch v tomto ��dku."""
        return self._columns.keys()
        
    def original_row(self):
        """Vra� ��dek obsahuj�c� p�vodn� hodnoty ��dku p�ed p��padn�mi zm�nami.

        Vr�cen� hodnota je instance 'pytis.data.Row' nebo 'None', ne nutn�
        toto�n� (ve smyslu 'id()') s���dkem zadan�m v�konstruktoru.

        Pozor, pokud byl ��dek p�edan� konstruktoru `None', metoda sice vr�t�
        inicializovan� pr�zdn� ��dek, ale hodnoty po��tan�ch pol��ek v n�m
        nebudou vypo�teny.
        
        """
        return self._original_row

    def changed(self):
        """Vra� pravdu, pr�v� kdy� byl ��dek zm�n�n.

        ��dek se pova�uje za zm�n�n�, nen�-li shodn� s���dkem vytvo�en�m z�dat
        zadan�ch v�konstruktoru, ve smyslu oper�toru `='.

        """
        return self._row != self._original_row

    def field_changed(self, key):
        """Vra� pravdu, pr�v� kdy� bylo pol��ko dan� 'key' zm�n�no.

        """
        return self._row.has_key(key) and \
               self._row[key] != self._original_row[key]

    def new(self):
        """Vra� pravdu, pr�v� kdy� se jedn� o�nov� z�znam."""
        return self._new
    
    def editable(self, key):
        """Vra� pravdu, pr�v� kdy� je pol��ko dan� 'key' editovateln�.

        V�znam argumentu 'key' je stejn� jako v�metod� '__getitem__'.

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

    # Nakonec to nen� nikde pot�eba, ale kdyby, sta�� odkomentovat a dopsat
    # test...
    #def accessible(self, key, permission):
    #    """Vra� pravdu, pr�v� kdy� m� u�ivatel pr�vo p��stupu k�dan�mu pol��ku.
    #
    #    Argumenty:
    #    
    #      'key' -- stejn� jako v�metod� '__getitem__'.
    #      'permission' -- jedna z�konstant t��dy 'pytis.data.Permission'
    #        ur�uj�c�, kter� p��stupov� pr�vo se m� testovat.
    #
    #    Pokud dan� pol��ko nen� sou��st� datov�ho ��dku (jde o virtu�ln�
    #    pol��ko), vrac� v�dy None.
    #        
    #    """
    #    if self._row.has_key(key):
    #        return self._data.accessible(key, permission)
    #    else:
    #        return None

    def check(self):
        """Prove� kontrolu vz�jemn� integrity dat ��dku.
        
        Metoda provede v�echny existuj�c� 'check' funkce definovan� ve
        'FieldSpec' obsa�en�ch pol��ek.  P�i ne�sp�chu kontroly n�kter�ho
        pol��ka nen� prov�d�na ��dn� akce, pouze je vr�ceno id tohoto pol��ka a
        provedeno zalogov�n�.  O�ek�v� se, �e p��padn� interakce s u�ivatelem
        je prov�d�na v r�mci check funkce.

        Vrac�: Id pol��ka, pokud n�kter� kontroln� funkce neprojde, nebo None v
        p��pad�, �e je v�e v po��dku.

        """
        # TODO: Tato metoda bude zru�ena, hned jak se p�estanou pou��vat funkce
        # 'check' ve 'FieldSpec', kter�to maj� b�t nahrazeny stejnojmennou
        # funkc� ve 'ViewSpec', ke kter� zde v�ak nen� p��stup, tak�e se vol�
        # na �rovni formul��e.
        for spec in self._fieldspec:
            c = self._columns[spec.id()]
            if c.check is not None and not c.check(self):
                log(EVENT, 'Kontrola integrity selhala:', (c.id, self))
                return c.id
        return None

        
    def refvalue(self, key):
        """Metoda existuje jen kv�li zp�tn� kompatibilit�, ale vrac� v�dy None.

        Brzy bude zru�ena, tak�e je t�eba v�echna jej� pou�it� odstranit.
        
        """
        log(EVENT, "Pou�ita potla�en� metoda 'PresentedRow.refvalue()'!")
        # TODO: Zru�it!!!
        return None
