# -*- coding: iso-8859-2 -*-

# Prezentace dat v�pol��k�ch.
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
        """Inicializuj prezentaci ��dku.
        
        Argumenty:

          fieldspec -- sekvence specifikac� pol��ek, instanc� t��dy
            'FieldSpec'
            
          data -- odpov�daj�c� datov� objekt, instance t��dy 'pytis.data.Data'
          
          row -- data ��dku, viz n�e
          
          prefill -- slovn�k hodnot pro inicializaci ��dku nam�sto v�choz�ch
            hodnot.  Slovn�k je kl��ovan� p�es textov� identifik�tor sloupce.
            Hodnotami jsou instance t��dy Value, nebo p��mo vnit�n� hodnoty.
            Takto p�edvypln�n� hodnoty maj� p�ednost nejen p�ed v�choz�mi
            hodnotami ur�en�mi specifikac� 'default' p��slu�n�ho pol��ka, ale
            tak� p�ed hodnotami dopo�ten�mi pomoc� jeho dopo��t�vac� funkce
            ('computer').
            
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
            
          resolver -- instance 'Resolver', kter� m� b�t pou��v�na k na��t�n�
            specifikac�.  Pokud nen� ur�en, je pou�it glob�ln� resolver z�skan�
            pomoc� funkce 'pytis.util.resolver()'.  Glob�ln� resolver je
            pou�iteln� v samostatn� b��c� aplikaci, ale nap�. v prost�ed�
            webov�ho serveru je t�eba pracovat s v�ce resolvery sou�asn� a ty
            je potom nutn� p�ed�vat jako argument.

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
        # Pro ka�d� pol��ko si zapamatuji seznam po��tan�ch pol��ek, kter� na
        # n�m z�vis� (obr�cen� mapov�n� ne� ve specifikac�ch).
        self._dependent = {}
        self._editability_dependent = {}
        self._codebook_runtime_filter_dependent = {}
        # Pro v�echna po��tan� pol��ka si pamatuji, zda pot�ebuj� p�epo��tat,
        # �i nikoliv (po p�epo��t�n� je pol��ko �ist�, po zm�n� pol��ka na
        # kter�m z�vis� jin� pol��ka, nastav�m z�visl�m pol��k�m p��znak
        # dirty).  P�epo��t�v�n� potom mohu prov�d�t a� p�i skute�n�m po�adavku
        # na z�sk�n� hodnoty pol��ka.
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
                # Prefill m� p�ednost p�ed computerem, proto�e n�kdy
                # chceme v procedur�ch m�t mo�nost ve formul��i za
                # n�jak�ch okolnost� p�ednastavit jinou hodnotu, ne�
                # jak� by byla computerem norm�ln� vypo�tena.
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
        """Vra� hodnotu pol��ka 'key' jako instanci t��dy 'pytis.data.Value'.
        
        'key' je id pol��ka (�et�zec) identifikuj�c� existuj�c� pol��ko, jinak
        je chov�n� metody nedefinov�no.
        
        """
        if self._row.has_key(key):
            value = self._row[key]
        else:
            value = self._virtual[key]
        if self._dirty.has_key(key) and self._dirty[key]:
            column = self._columns[key]
            # Nastaven�m dirty na False u� zde zamez�me rekurzi v p��pad�, �e
            # se k�d computeru pt� na vlastn� hodnotu a umo�n�me mu tak zjistit
            # p�vodn� hodnotu (p�ed p�epo��t�n�m).
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
        # just fields depending on a given field (after its change).
        # TODO: Mus� se to d�lat v�dy?  Nap�. i p�i set_row z BrowseFormu?
        if key is None:
            invoke_callbacks = False
        else:
            invoke_callbacks = self._mark_dependent_dirty(key)
        self._notify_runtime_filter_change(key)
        self._recompute_editability(key)
        if invoke_callbacks and self._change_callback is not None:
            # Zavolej 'chage_callback' pro v�echna zbyl� "dirty" pol��ka.
            # Pol��ka, kter� byla ozna�ena jako "dirty" ji� bu�to byla
            # p�epo��t�na a callback byl zavol�n b�hem p�epo��t�v�n�
            # editovatelnosti a runtime codebook�, nebo z�stala "dirty" a
            # mus�me tedy jejich callback zavolat te�.
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
        
    def original_row(self, empty_as_none=False):
        """Vra� ��dek obsahuj�c� p�vodn� hodnoty ��dku p�ed p��padn�mi zm�nami.

        Vr�cen� hodnota je instance 'pytis.data.Row', ne nutn� toto�n� (ve
        smyslu 'id()') s���dkem zadan�m v�konstruktoru.

        P�vodn�mi hodnotami jsou my�leny hodnoty ��dku p�edan�ho konstruktoru,
        nebo posledn�mu vol�n� metody 'set_row()', s pravdiv�m argumentem
        'reset'.
        
        """
        if empty_as_none and self._original_row_empty:
            return None
        else:
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
        """Vra� hodnotu displeje ��seln�ku dan�ho pol��ka.

        Pokud dan� pol��ko nen� ��seln�kem, nebo tento ��seln�k nem� ur�en
        displej, nebo aktu�ln� hodnota pol��ka nen� v ��seln�ku nalezena, nebo
        k nejsou dostate�n� pr�va k jej�mu na�ten�, bude vr�cen pr�zdn�
        �et�zec.
        
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
        """Vra� v��et hodnot ��seln�ku dan�ho pol��ka jako seznam dvojic.

        Vr�cen� seznam obsahuje v�dy vnit�n� Pythonovou hodnotu ��seln�ku a k
        n� odpov�daj�c� u�ivatelskou hodnotu jako �et�zec.  U�ivatelsk� hodnota
        je ur�ena specifikac� `display'.
        
        Vyvol�n� t�to metody pro pol��ko, kter� nen� ��seln�kov� je pova�ov�no
        za chybu.
       
        """
        column = self._columns[key]
        display = self._display_func(column)
        if display is None:
            display = lambda v: pytis.data.Value(column.type, v).export()
        return [(v, display(v)) for v in column.type.enumerator().values()]



    
