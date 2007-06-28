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

"""Row data presentation layer.

A _field_ in this context refers to an abstract element of the user intercace, not its concrete
representation.

"""

import copy

import pytis.data

from pytis.presentation import *
from pytis.util import *


class PresentedRow(object):
    """A record of presented data.

    The class is an intermediate layer between a data row and its final presentation.  As oposed to
    the data row, it contains all fields present in field specifications (including virtual
    fields).  On the other hand, it doesn't solve a concrete presentation beyond string formatting.

    """

    CALL_CHANGE = 'CALL_CHANGE'
    """Callback called on indirect field change.

    Invoked when the field value changes due to its computer dependency on other field(s)."""
            
    CALL_EDITABILITY_CHANGE = 'CALL_EDITABILITY_CHANGE'
    """Callback called on field editability change.

    Invoked when the field editability changes due to its dependency on another field's
    value(s)."""
    
    CALL_ENUMERATION_CHANGE = 'CALL_ENUMERATION_CHANGE'

    """Callback called on field enumeration change.

    Invoked when the enumaration filter changes due to its dependency on another field's value(s).
    The enumaration is the list of valid field values provided by data type enumarator."""
    
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
            self.data_column = data.find_column(f.id())
            self.virtual = self.data_column is None
            
    def __init__(self, fields, data, row, prefill=None, singleline=False, new=False,
                 resolver=None, transaction=None):
        """Inicialize the instance.
        
        Arguments:

          fields -- a sequence of field specifications as 'FieldSpec' instances.
          data -- the underlying data object as a 'pytis.data.Data' instance.
          transaction -- current transaction for data operations.
          row -- initial row data (see below).
          prefill -- a dictionary of values for row initialization.  The dictionary is keyed by
            field identifiers and the values can be either 'pytis.data.Value' instances or the
            corresponding Python internal values (matching the field type).  These values take
            precedence before default values, the values contained within the passed 'row' as well
            as the computed values (the computers for prefilled values are not invoked.
          singleline -- a boolean flag indicating, that the exported values of all fields will be
            formatted to single line (influences the 'format()' method behavior).
          new -- boolean flag determining, whether the row represents a new record for insertion or
            an existing row for select or update.
          resolver -- a 'Resolver' instance for specification retrieval.  If not used, the global
            resolver returned by 'pytis.util.resolver()' will be used.

        Initial field values are determined depending on the argument 'row', which can have one of
        the following values:

          None -- default values will be generated according to field specifications.
          'PresentedRow' instance -- field values are taken from this instance.
          'pytis.data.Row' instance -- field values are taken from this data row.

        In any case only the state of the 'row' in the time of this constructor call matters.  Any
        later changes to it have no effect on the newly created instance.

        """
        assert isinstance(fields, (tuple, list))
        # TODO: pytis.remote vyžaduje inicializaci Pyro, což není vždy to pravé ořechové.  `data'
        # by stejně mělo být jednotného typu, je třeba to nějak promyslet.
        #assert isinstance(data, pytis.data.Data) or isinstance(data, pytis.remote.RemoteData)
        assert row is None or isinstance(row, (PresentedRow, pytis.data.Row))
        assert prefill is None or isinstance(prefill, dict)
        assert isinstance(singleline, bool)
        assert isinstance(new, bool)
        assert resolver is None or isinstance(resolver, Resolver)
        self._fields = fields
        self._data = data
        self._singleline = singleline
        self._callbacks = {}
        self._new = new
        self._cache = {}
        self._invalid = {}
        self._transaction = transaction
        self._resolver = resolver or pytis.util.resolver()
        self._columns = columns = tuple([self._Column(f, data) for f in fields])
        self._coldict = dict([(c.id, c) for c in columns])
        key = data.key()[0].id()
        if not self._coldict.has_key(key):
            # TODO: This is a temporary hack for old applications which have data columns not
            # present in field specifications.  This should not be supported in future and should
            # be removed.
            self._columns += (self._Column(FieldSpec(key), data),)
        self._init_dependencies()
        self._set_row(row, reset=True, prefill=prefill)

    def _set_row(self, row, reset=False, prefill=None):
        if prefill:
            def value(v):
                if isinstance(v, pytis.data.Value):
                    return v.value()
                else:
                    return v
            prefill = dict([(k, pytis.data.Value(self._coldict[k].type, value(v)))
                            for k, v in prefill.items()])
        self._cache = {}
        def genval(key):
            if row is None or not row.has_key(key):
                if prefill and prefill.has_key(key):
                    value = prefill[key]
                elif self._coldict.has_key(key):
                    col = self._coldict[key]
                    default = col.default
                    if self._new and default is not None:
                        if callable(default):
                            default = default()
                        value = pytis.data.Value(col.type, default)
                    else:
                        value = col.type.default_value()
                else:
                    value = self._data.find_column(key).type().default_value()
            elif prefill and prefill.has_key(key):
                value = prefill[key]
            else:
                value = row[key]
            return value
        row_data = [(c.id, genval(c.id)) for c in self._columns if not c.virtual]
        virtual = [(c.id, genval(c.id)) for c in self._columns if c.virtual]
        for key in self._dirty.keys():
            # Prefill and default take precedence over the computer
            self._dirty[key] = not (not self._new and row is None or \
                                    row is not None and row.has_key(key) or \
                                    prefill is not None and prefill.has_key(key) or \
                                    self._new and self._coldict[key].default is not None)
        self._row = pytis.data.Row(row_data)
        self._virtual = dict(virtual)
        if reset:
            self._original_row_empty = row is None
            if not hasattr(self, '_original_row'):
                # Calling row() may invoke dirty column computations.  The computers may use the
                # original row as well, so we must create one before.
                self._original_row = copy.copy(self._row)
            self._original_row = self.row()
        self._resolve_dependencies()
        self._run_callback(self.CALL_CHANGE, None)

    def _all_deps(self, computer):
        all = []
        for key in computer.depends():
            all.append(key)
            computer = self._coldict[key].computer
            if computer:
                all.extend(self._all_deps(computer))
        return all
        
    def _init_dependencies(self):
        # Pro každé políčko si zapamatuji seznam počítaných políček, která na
        # něm závisí (obrácené mapování než ve specifikacích).
        self._dependent = {}
        self._editability_dependent = {}
        self._runtime_filter_dependent = {}
        # Pro všechna počítaná políčka si pamatuji, zda potřebují přepočítat,
        # či nikoliv (po přepočítání je políčko čisté, po změně políčka na
        # kterém závisí jiná políčka, nastavím závislým políčkům příznak
        # dirty).  Přepočítávání potom mohu provádět až při skutečném požadavku
        # na získání hodnoty políčka.
        self._dirty = {}
        self._editability_dirty = {}
        self._editable = {}
        self._runtime_filter_dirty = {}
        self._runtime_filter = {}
        for c in self._columns:
            key = c.id
            if c.computer is not None:
                self._dirty[key] = True
                for dep in self._all_deps(c.computer):
                    if self._dependent.has_key(dep):
                        self._dependent[dep].append(key)
                    else:
                        self._dependent[dep] = [key]
            if isinstance(c.editable, Computer):
                self._editable[key] = True
                self._editability_dirty[key] = True
                for dep in self._all_deps(c.editable):
                    if self._editability_dependent.has_key(dep):
                        self._editability_dependent[dep].append(key)
                    else:
                        self._editability_dependent[dep] = [key]
            if c.codebook_runtime_filter is not None:
                self._runtime_filter[key] = None
                self._runtime_filter_dirty[key] = True
                for dep in self._all_deps(c.codebook_runtime_filter):
                    if self._runtime_filter_dependent.has_key(dep):
                        self._runtime_filter_dependent[dep].append(key)
                    else:
                        self._runtime_filter_dependent[dep] = [key]

    def __getitem__(self, key, lazy=False):
        """Vrať hodnotu políčka 'key' jako instanci třídy 'pytis.data.Value'.
        
        'key' je id políčka (řetězec) identifikující existující políčko, jinak
        je chování metody nedefinováno.
        
        """
        if self._row.has_key(key):
            value = self._row[key]
        else:
            value = self._virtual[key]
        if not lazy and self._dirty.has_key(key) and self._dirty[key]:
            column = self._coldict[key]
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
                # TODO: This invokes the callback again when called within a callback handler.
                self._run_callback(self.CALL_CHANGE, key)
        return value

    def __setitem__(self, key, value, run_callback=True):
        assert isinstance(value, pytis.data.Value)
        column = self._coldict[key]
        assert value.type() == column.type, \
               "Invalid type for '%s': %s (expected %s)" % (key, value.type(), column.type)
        if self._row.has_key(key):
            row = self._row
        elif self._virtual.has_key(key):
            row = self._virtual
        else:
            return
        if row[key].value() != value.value():
            row[key] = value
            self._cache = {}
            self._resolve_dependencies(key)
            if run_callback:
                self._run_callback(self.CALL_CHANGE, key)
                
    def __str__(self):
        if hasattr(self, '_row'):
            items = [c.id + '=' + str(self[c.id].value()) for c in self._columns]
            return '<PresentedRow: %s>' % string.join(items, ', ')
        else:
            return super(PresentedRow, self).__str__()

    def _run_callback(self, kind, key=None):
        callbacks = self._callbacks.get(kind, {})
        if key is None:
            for callback in callbacks.values():
                callback()
        else:
            callback = callbacks.get(key)
            if callback:
                callback()
            
    def _resolve_dependencies(self, key=None):
        # Recompute dependencies for all fields when the key is None or recompute
        # just fields depending on given field (after its change).
        if key is not None and self._dependent.has_key(key):
            for k in self._dependent[key]:
                self._dirty[k] = True
        # TODO: Do we need to do that always?  Eg. on set_row in BrowseForm?
        self._recompute_editability(key)
        self._notify_runtime_filter_change(key)
        if self._callbacks and key is not None and self._dependent.has_key(key):
            # Call 'chage_callback' for all remaining dirty fields.  Some fields may already have
            # been recomputed during the editability and runtime filter recomputations.  The
            # callbacks for those fields have already been generated, but here we neen to handle
            # the rest.
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
        # Vypočti editovatelnost políčka a vrať výsledek (jako boolean).
        func = self._coldict[key].editable.function()
        self._editable[key] = result = func(self, key)
        self._editability_dirty[key] = False
        return result
    
    def _notify_runtime_filter_change(self, key=None):
        if key is None:
            keys = self._runtime_filter_dirty.keys()
        else:
            keys = self._runtime_filter_dependent.get(key, ())
        for k in keys:
            self._runtime_filter_dirty[k] = True
            self._run_callback(self.CALL_ENUMERATION_CHANGE, k)

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

    def cb_value(self, key, column):
        """Return the value of another column in the data object of given field's enumerator.

        Arguments:

          key -- field identifier (string).  This field must have an enumerator of type
            'DataEnumerator'.
          column -- identifier of another column in the enumerator's data object.

        This method is in fact just a convenience wrapper for 'pytis.data.DataEnumerator.get()'.

        Returns a 'pytis.data.Value' instance or None when the enumerator doesn't contain the
        current value of the field 'key' (the field value is not valid).
            
        """
        value = self[key]
        c = self.runtime_filter(key)
        e = value.type().enumerator()
        return e.get(value.value(), column=column, transaction=self._transaction, condition=c)
        
    def row(self):
        """Return the current *data* row as a 'pytis.data.Row' instance."""
        row_data = [(c.id, pytis.data.Value(c.data_column.type(), self[c.id].value()))
                    for c in self._columns if not c.virtual]
        return pytis.data.Row(row_data)

    def data(self):
        """Return the data object associated with the row."""
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

          key -- field identifier (string).
          kwargs -- keyword arguments passed to the 'export()' method of the field's
            'Value' instance.
        
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
        column = self._coldict[key]
        if self._singleline and column.line_separator is not None:
            svalue = string.join(svalue.splitlines(), column.line_separator)
        self._cache[key] = svalue
        return svalue

    def set_row(self, row, reset=False, prefill=None):
        """Set the current row data according to 'row'.

        Arguments:
           row -- has the same meaning as the constructor argument of the same name.
           reset -- a boolean flag indicating, that the new row data will now be considered as
             original.  This influences the behavior of the methods 'changed()' and
             'original_row()'.
           prefill -- has the same meaning as the constructor argument of the same name.

        This method is meant to support the concept of current row in a form with fixed
        fields/columns and data object.  It saves the specification processing in the constructor.
        
        """
        self._set_row(row, reset=reset, prefill=prefill)

    def fields(self):
        """Vrať seznam všech políček."""
        return self._fields
        
    def has_key(self, key):
        """Return true if a field of given key is contained within the row."""
        return self._coldict.has_key(key)
        
    def keys(self):
        """Vrať seznam identifikátorů všech políček obsažených v tomto řádku."""
        return [c.id for c in self._columns]
        
    def new(self):
        """Return true if the row represents a new (inserted) record."""
        return self._new
    
    def original_row(self, empty_as_none=False):
        """Vrať *datový* řádek obsahující původní hodnoty před případnými změnami.

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
        """Vrať pravdu, právě když je políčko dané 'key' editovatelné.

        Význam argumentu 'key' je stejný jako v metodě '__getitem__'.

        """
        if self._editable.has_key(key):
            if self._editability_dirty[key]:
                return self._compute_editability(key)
            else:
                return self._editable[key]
        else:
            editable = self._coldict[key].editable
            return editable == Editable.ALWAYS or editable == Editable.ONCE and self._new
        
    def validate(self, key, string, **kwargs):
        """Validate user input and propagate the value to the row if the string is valid.

        Arguments:

          key -- identifier of the validated field
          string -- string value representing user input

        If the string is not valid, it is saved (can be retrieved later by the 'invalid_string()'
        method) and this state is also reflected by the 'changed()' and 'field_changed()' methods.
        This state is updated after each validation attempt.
        
        Returns: 'ValidationError' instance if an error occurs or None if the string is valid.
        
        """
        column = self._coldict[key]
        if column.codebook_runtime_filter is not None:
            kwargs = dict(kwargs, condition=self.runtime_filter(key))
        value, error = column.type.validate(string, transaction=self._transaction, **kwargs)
        if not error and column.type.unique() and not column.virtual and \
               (self._new or value != self._original_row[key]):
            count = self._data.select(condition=pytis.data.EQ(column.id, value),
                                      transaction=self._transaction)
            self._data.close()
            if count != 0:
                error = pytis.data.ValidationError(_("Taková hodnota již existuje."))
        result = error
        if error:
            value, error = column.type.validate(string, transaction=self._transaction,
                                                strict=False, **kwargs)
        if not error:
            if self._invalid.has_key(key):
                del self._invalid[key]
            if string != self.format(key):
                self.__setitem__(key, value, run_callback=False)
        else:
            if string != self.format(key):
                self._invalid[key] = string
            elif self._invalid.has_key(key):
                del self._invalid[key]
        return result

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

    # Nakonec to není nikde potřeba, ale kdyby, stačí odkomentovat a dopsat
    # test...
    #def permitted(self, key, permission):
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
    #        return self._data.permitted(key, permission)
    #    else:
    #        return None

    def _display_func(self, column):
        def getval(enum, value, col, func=None):
            if value is None:
                return ''
            if self._transaction and not self._transaction.open():
                return ''
            try:
                v = enum.get(value, col, condition=self.runtime_filter(column.id),
                             transaction=self._transaction)
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
        """Return enumerator `display' value for given field as a string.

        If the field has no enumerator or no display was specified, an empty string is returned.

        Empty string is also returned if current field value doesn't belong to the enumeration (is
        invalid) or if it is not possible to retrieve the displayed value (isufficient access
        rights, current transaction aborted etc.)
        
        """
        column = self._coldict[key]
        display = self._display_func(column)
        if not display:
            computer = column.computer
            if computer and isinstance(computer, CbComputer):
                column = self._coldict[computer.field()]
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
        column = self._coldict[key]
        display = self._display_func(column)
        if display is None:
            display = lambda v: column.type.export(v)
        enumerator = column.type.enumerator()
        if isinstance(enumerator, pytis.data.DataEnumerator):
            kwargs = dict(condition=self.runtime_filter(column))
        else:
            kwargs = {}
        return [(v, display(v)) for v in enumerator.values(**kwargs)]

    def runtime_filter(self, key):
        """Return the current run-time filter condition for an enumerator of field KEY.

        Returns a 'pytis.data.Operator' instance when a filter is active or None if the field has
        no enumerator or if the enumerator is not filtered.

        """
        try:
            dirty = self._runtime_filter_dirty[key]
        except KeyError:
            return None
        if dirty:
            column = self._coldict[key]
            function = column.codebook_runtime_filter.function()
            self._runtime_filter_dirty[key] = False
            condition = self._runtime_filter[key] = function(self)
        else:
            condition = self._runtime_filter[key]
        return condition
