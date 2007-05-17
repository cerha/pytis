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

    CALL_CHANGE = 'CALL_CHANGE'
    """Callback called on indirect field change (when the field value changes due to its computer
    dependency on another field)."""
            
    CALL_EDITABILITY_CHANGE = 'CALL_EDITABILITY_CHANGE'
    """Callback called on field editability change (when the field editability changes due to its
    editability dependency on another field)."""
    
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
            
    def __init__(self, fieldspec, data, row, prefill=None, singleline=False, new=False,
                 resolver=None, transaction=None):
        """Inicializuj prezentaci ��dku.
        
        Argumenty:

          fieldspec -- sekvence specifikac� pol��ek, instanc� t��dy 'FieldSpec'
            
          data -- odpov�daj�c� datov� objekt, instance t��dy 'pytis.data.Data'
          
          transaction -- current transaction to use for data operations.
          
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
            
          new -- flag ur�uj�c�, zda se jedn� o�nov� vytv��en� z�znam (nikoliv
            editaci z�znamu ji� existuj�c�ho)
            
          resolver -- instance 'Resolver', kter� m� b�t pou��v�na k na��t�n�
            specifikac�.  Pokud nen� ur�en, je pou�it glob�ln� resolver z�skan�
            pomoc� funkce 'pytis.util.resolver()'.  Glob�ln� resolver je
            pou�iteln� v samostatn� b��c� aplikaci, ale nap�. v prost�ed�
            webov�ho serveru je t�eba pracovat s v�ce resolvery sou�asn� a ty
            je potom nutn� p�ed�vat jako argument.

        Initial field values are determined depending on the argument 'row', which can have one of
        the following values:

          None -- default values will be generated according to field specifications.
          'PresentedRow' instance -- field values are taken from this instance.
          'pytis.data.Row' instance -- field values are taken from this data row.

        In any case only the state of the 'row' in the time of this constructor call matters.  Any
        later changes to it have no effect on the newly created instance.

        """
        assert isinstance(fieldspec, (tuple, list))
        # TODO: pytis.remote vy�aduje inicializaci Pyro, co� nen� v�dy to prav� o�echov�.  `data'
        # by stejn� m�lo b�t jednotn�ho typu, je t�eba to n�jak promyslet.
        #assert isinstance(data, pytis.data.Data) or isinstance(data, pytis.remote.RemoteData)
        assert row is None or isinstance(row, (PresentedRow, pytis.data.Row))
        assert prefill is None or isinstance(prefill, dict)
        assert isinstance(singleline, bool)
        assert isinstance(new, bool)
        assert resolver is None or isinstance(resolver, Resolver)
        self._fieldspec = fieldspec
        self._data = data
        self._singleline = singleline
        self._callbacks = {}
        self._new = new
        self._cache = {}
        self._invalid = {}
        self._transaction = transaction
        self._resolver = resolver or pytis.util.resolver()
        self._columns = columns = dict([(f.id(), self._Column(f, data)) for f in self._fieldspec])
        self._init_dependencies()
        if prefill:
            def value(v):
                if isinstance(v, pytis.data.Value):
                    return v.value()
                else:
                    return v
            prefill = dict([(k, pytis.data.Value(columns[k].type, value(v)))
                            for k, v in prefill.items()])
        self._virtual = dict([(k, self._default(k, prefill=prefill))
                              for k in columns.keys()
                              if data.find_column(k) is None])
        self._set_row(row, prefill=prefill)

    def _set_row(self, row, reset=True, prefill=None):
        self._row = self._init_row(row, prefill=prefill)
        if reset:
            self._original_row_empty = row is None
            # We need to compute all dirty fields in the saved original row, but since the
            # computers may use the original row as well, we must create one before running them.
            self._original_row = copy.copy(self._row)
            [self[k] for k in self._dirty.keys() if self._row.has_key(k)]
            self._original_row = copy.copy(self._row)
        self._resolve_dependencies()
        self._run_callback(self.CALL_CHANGE, None)

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
        row_ = row
        if row is None:
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
            # Prefill and default take precedence before the computer
            self._dirty[key] = not (row_ is not None and row_.has_key(key) or \
                                    prefill is not None and prefill.has_key(key) or \
                                    self._new and self._columns[key].default is not None)
        return row

    def _default(self, key, prefill=None):
        if prefill and prefill.has_key(key):
            value = prefill[key]
        elif self._columns.has_key(key):
            col = self._columns[key]
            default = col.default
            if self._new and default is not None:
                if callable(default):
                    default = default()
                value = pytis.data.Value(col.type, default)
            else:
                value = col.type.default_value()
        else:
            value = self._data.find_column(key).type().default_value()
        return value

    def __getitem__(self, key, lazy=False):
        """Vra� hodnotu pol��ka 'key' jako instanci t��dy 'pytis.data.Value'.
        
        'key' je id pol��ka (�et�zec) identifikuj�c� existuj�c� pol��ko, jinak
        je chov�n� metody nedefinov�no.
        
        """
        if self._row.has_key(key):
            value = self._row[key]
        else:
            value = self._virtual[key]
        if not lazy and self._dirty.has_key(key) and self._dirty[key]:
            column = self._columns[key]
            # Reset the dirty flag before calling the computer to allow the computer to retrieve
            # the original value without recursion.
            self._dirty[key] = False
            func = column.computer.function()
            new_value = pytis.data.Value(column.type, func(self))
            if new_value.value() != value.value():
                value = new_value
                if self._row.has_key(key):
                    self._row[key] = value
                else:
                    self._virtual[key] = value
                self._run_callback(self.CALL_CHANGE, key)
        return value

    def __setitem__(self, key, value):
        assert isinstance(value, pytis.data.Value)
        column = self._columns[key]
        assert value.type() == column.type, \
               "Invalid type for '%s': %s (expected %s)" % (key, value.type(), column.type)
        if self._row.has_key(key):
            row = self._row
        elif self._virtual.has_key(key):
            row = self._virtual
        else:
            return
        if row[key] != value:
            row[key] = value
            self._cache = {}
            self._resolve_dependencies(key)
            self._run_callback(self.CALL_CHANGE, key)
                
    def __str__(self):
        if hasattr(self, '_row'):
            items = []
            for spec in self._fieldspec:
                items.append(spec.id() + '=' + str(self[spec.id()].value()))
            return '<PresentedRow: %s>' % string.join(items, ', ')
        else:
            return super(PresentedRow, self).__str__()

    def _run_callback(self, kind, key=None):
        try:
            callbacks = self._callbacks[kind]
        except KeyError:
            pass
        else:
            if key is None:
                for callback in callbacks.values():
                    callback()
            else:
                try:
                    callback = callbacks[key]
                except KeyError:
                    pass
                else:
                    callback()
            
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
        # Recompute dependencies for all fields when the key is None or recompute
        # just fields depending on given field (after its change).
        if key is not None:
            marked = self._mark_dependent_dirty(key)
        # TODO: Do we need to do that always?  Eg. on set_row in BrowseForm?
        self._notify_runtime_filter_change(key)
        self._recompute_editability(key)
        if self._callbacks:
            if key is not None and marked:
                # Call 'chage_callback' for all remaining dirty fields.  Some fields may already
                # have been recomputed during the editability and runtime filter recomputations.
                # The callbacks for those fields have already been generated, but here we neen to
                # handle the rest.
                for key, dirty in self._dirty.items():
                    if dirty:
                        self._run_callback(self.CALL_CHANGE, key)
    
    def _recompute_editability(self, key=None):
        if key is None:
            keys = self._editable.keys()
        elif self._editability_dependent.has_key(key):
            keys = self._editability_dependent[key]
        else:
            return
        if self._callbacks:
            for k in keys:
                old = self._editable[k]
                new = self._compute_editability(k)
                if old != new:
                    self._run_callback(self.CALL_EDITABILITY_CHANGE, k)
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

    def get(self, key, default=None, lazy=False):
        """Return the value for the KEY if it exists or the DEFAULT otherwise.

          Arguments:
            default -- the default value returned when the key does not exist.
            lazy -- if true, the value will not be computed even if it should
              be.  This may result in returning an invalid value, but prevents
              the computer from being invoked.  Does nothing for fields without
              a computer.
          
        """
        try:
            return self.__getitem__(key, lazy=lazy)
        except KeyError:
            return default

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

    def transaction(self):
        """Return the current transaction for data operations."""
        return self._transaction

    def set_transaction(self, transaction):
        """Set the current transaction for data operations."""
        self._transaction = transaction
    
    def format(self, key, **kwargs):
        """Return the string representation of the field value.

        Arguments:

          'key' -- field identifier (string).
          'kwargs' -- keyword arguments passed to the 'export()' method of the field's
            'Value' instance.
        
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
        """Return true if a field of given key is contained within the row."""
        return self._columns.has_key(key)
        
    def keys(self):
        """Vra� seznam identifik�tor� v�ech pol��ek obsa�en�ch v tomto ��dku."""
        return self._columns.keys()
        
    def new(self):
        """Return true if the row represents a new (inserted) record."""
        return self._new
    
    def original_row(self, empty_as_none=False):
        """Vra� *datov�* ��dek obsahuj�c� p�vodn� hodnoty p�ed p��padn�mi zm�nami.

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
        """Return true if the data row has been changed.

        The row is considered changed if the underlying data row is not equal to the original row
        passed to (or created in) the constructor in the sense of the `=' operator.  Changes in the
        virtual fields (not present in the underlying data row) are ignored.

        """
        return self._row != self._original_row or self._invalid

    def field_changed(self, key):
        """Return true if the field 'key' was changed compared to its original value.

        Warning: False is always returned for fully virtual fields (with no underlying data
        column).  For all computed fields the result may not be accurate because the recomputation
        may not have happened yet.  

        """
        return not self._row.has_key(key) or \
               self._row[key] != self._original_row[key] or \
               self._invalid.has_key(key)

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
            return editable == Editable.ALWAYS or editable == Editable.ONCE and self._new
        
    def validate(self, key, string):
        """Validate user input and propagate the value to the row if the string is valid.

        Arguments:

          key -- identifier of the validated field
          string -- string value representing user input

        If the string is not valid, it is saved (can be retrieved later by the 'invalid_string()'
        method) and this state is also reflected by the 'changed()' and 'field_changed()' methods.
        This state is updated after each validation attempt.
        
        Returns: 'ValidationError' instance if an error occurs or None if the string is valid.
        
        """
        column = self._columns[key]
        value, error = column.type.validate(string, transaction=self._transaction)
        if not error:
            if self._invalid.has_key(key):
                del self._invalid[key]
            if string != self.format(key):
                self[key] = value
        else:
            # TODO: perform non-strict validation?
            if string != self.format(key):
                self._invalid[key] = string
            elif self._invalid.has_key(key):
                del self._invalid[key]
        return error

    def invalid_string(self, key):
        """Return the last invalid user input string.

        Returns a string passed to the last call to 'validate()' if this last input string was
        invalid.  None is returned if the last validation was successful.
        
        """
        return self._invalid.get(key)
    
    def register_callback(self, kind, key, function):
        assert kind[:5] == 'CALL_' and hasattr(self, kind), ('Invalid callback kind', kind)
        assert function is None or callable(function), ('Invalid callback function', function)
        try:
            callbacks = self._callbacks[kind]
        except KeyError:
            callbacks = self._callbacks[kind] = {}
        if callbacks.has_key(key):
            raise ProgramError("Callback already registered:", kind, key, callbacks[key])
        callbacks[key] = function

    # Nakonec to nen� nikde pot�eba, ale kdyby, sta�� odkomentovat a dopsat
    # test...
    #def permitted(self, key, permission):
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
    #        return self._data.permitted(key, permission)
    #    else:
    #        return None

    def _display_func(self, column):
        def getval(enum, value, col, func=None):
            if value is None:
                return ''
            try:
                v = enum.get(value, col, transaction=self._transaction)
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


    
