# -*- coding: utf-8 -*-

# Copyright (C) 2002-2011 Brailcom, o.p.s.
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

A _field_ in this context refers to a logical element of the user intercace,
not its concrete representation (input widget).

"""

import collections
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
        def __init__(self, f, type, data):
            self.id = f.id()
            self.type = type
            self.computer = f.computer()
            self.line_separator = f.line_separator()
            self.default = f.default()
            self.editable = f.editable()
            self.display = f.display()
            self.null_display = f.null_display()
            self.prefer_display = f.prefer_display()
            self.codebook = f.codebook()
            self.completer = f.completer
            self.runtime_filter = f.runtime_filter()
            self.runtime_arguments = f.runtime_arguments()
            self.data_column = data.find_column(self.id)
            self.virtual = f.virtual()
            self.secret_computer = False # Set dynamically during initialization.
        def __str__(self):
            return "<_Column id='%s' type='%s' virtual='%s'>" % (self.id, self.type, self.virtual)
    
    def __init__(self, fields, data, row, prefill=None, singleline=False, new=False,
                 resolver=None, transaction=None):
        """Inicialize the instance.
        
        Arguments:

          fields -- a sequence of field specifications as 'Field' instances.
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
        assert row is None or isinstance(row, (PresentedRow, pytis.data.Row))
        assert prefill is None or isinstance(prefill, dict)
        assert isinstance(singleline, bool)
        assert isinstance(new, bool)
        assert resolver is None or isinstance(resolver, Resolver)
        assert data.key()[0].id() in [f.id() for f in fields]
        self._fields = fields
        self._data = data
        self._singleline = singleline
        self._callbacks = {}
        self._new = new
        self._cache = {}
        self._invalid = {}
        self._transaction = transaction
        self._resolver = resolver or pytis.util.resolver()
        self._columns = columns = tuple([self._Column(f, self._type(f), data) for f in fields])
        self._coldict = dict([(c.id, c) for c in columns])
        self._cb_spec_cache = {}
        self._completer_cache = {}
        self._init_dependencies()
        self._set_row(row, reset=True, prefill=prefill)

    def _secret_column(self, column):
        if column.virtual:
            return column.secret_computer
        else:
            return not self.permitted(column.id, pytis.data.Permission.VIEW)

    def _type(self, fspec):
        """Return the final 'pytis.data.Type' instance for given field specification."""
        column = self._data.find_column(fspec.id())
        if column:
            # Actually, the type taken from the data object always takes precedence, since it
            # should already respect type and its arguments from field specification -- they are
            # passed to column binding constructors when the data object is created.
            type_ = column.type()
        else:
            type_ = fspec.type()
            computer = fspec.computer()
            if not type_ and isinstance(computer, CbComputer):
                # If a virtual field as a CbComputer, we can take the data type from the related
                # columnin the enumerator's data object.
                cb_column = self._data.find_column(computer.field())
                type_ = cb_column.type().enumerator().type(computer.column())
                assert type_ is not None, \
                       "Invalid enumerator column '%s' in CbComputer for '%s'." % \
                       (computer.column(), fspec.id())
            else:
                kwargs = fspec.type_kwargs(self._resolver)
                if not type_:
                    # String is the default type of virtual columns.
                    type_ = pytis.data.String(**kwargs)
                elif type(type_) == type(pytis.data.Type):
                    type_ = type_(**kwargs)
        return type_

    def _set_row(self, row, reset=False, prefill=None):
        if prefill:
            def value(v):
                if isinstance(v, pytis.data.Value):
                    return v
                else:
                    return pytis.data.Value(pytis.data.Type(), v)
            prefill = dict([(k, value(v).retype(self._coldict[k].type))
                            for k, v in prefill.items()])
        self._cache = {}
        def genval(key, virtual):
            if row is None or key not in row:
                if prefill and key in prefill:
                    value = prefill[key]
                elif key in self._coldict:
                    col = self._coldict[key]
                    default = col.default
                    if self._new and default is not None:
                        if isinstance(default, collections.Callable):
                            default = default()
                        value = pytis.data.Value(col.type, default)
                    else:
                        value = col.type.default_value()
                else:
                    value = self._data.find_column(key).type().default_value()
            elif prefill and key in prefill:
                value = prefill[key]
            else:
                if key in self._coldict:
                    value = row[key].retype(self._coldict[key].type)
                else:
                    value = row[key]
            return value
        row_data = [(c.id, genval(c.id, False)) for c in self._columns if not c.virtual]
        virtual = [(c.id, genval(c.id, True)) for c in self._columns if c.virtual]
        for key in self._dirty.keys():
            self._dirty[key] = not (not self._new and row is None or
                                    # If the value is contained in the data row, don't compute it.
                                    row is not None and key in row or
                                    # If the value is contained in the prefill, don't compute it.
                                    prefill is not None and key in prefill or
                                    # If the row is new and the field has a
                                    # default value, use the default value
                                    # rather than the computer.
                                    self._new and self._coldict[key].default is not None)
        self._row = pytis.data.Row(row_data)
        self._virtual = dict(virtual)
        self._invalid = {}
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
        self._runtime_arguments_dependent = {}
        # Pro všechna počítaná políčka si pamatuji, zda potřebují přepočítat,
        # či nikoliv (po přepočítání je políčko čisté, po změně políčka na
        # kterém závisí jiná políčka nastavím závislým políčkům příznak
        # dirty).  Přepočítávání potom mohu provádět až při skutečném požadavku
        # na získání hodnoty políčka.
        self._dirty = {}
        self._editability_dirty = {}
        self._editable = {}
        self._runtime_filter_dirty = {}
        self._runtime_filter = {}
        self._runtime_arguments_dirty = {}
        self._runtime_arguments = {}
        def make_deps(column, value_dict, dirty_dict, dependency_dict, computer):
            key = column.id
            if value_dict is not None:
                value_dict[key] = None
            dirty_dict[key] = True
            for dep in self._all_deps(computer):
                if dep in dependency_dict:
                    dependency_dict[dep].append(key)
                else:
                    dependency_dict[dep] = [key]
        for c in self._columns:
            if c.computer is not None:
                make_deps(c, None, self._dirty, self._dependent, c.computer)
            if isinstance(c.editable, Computer):
                make_deps(c, self._editable, self._editability_dirty, self._editability_dependent,
                          c.editable)
            if c.runtime_filter is not None:
                make_deps(c, self._runtime_filter, self._runtime_filter_dirty,
                          self._runtime_filter_dependent, c.runtime_filter)
            if c.runtime_arguments is not None:
                make_deps(c, self._runtime_arguments, self._runtime_arguments_dirty,
                          self._runtime_arguments_dependent, c.runtime_arguments)
        def add_secret(column):
            for key in self._dependent.get(column.id, []):
                column = self._coldict[key]
                if not column.secret_computer:
                    column.secret_computer = True
                    add_secret(column)
        for column in self._columns:
            if not self.permitted(column.id, pytis.data.Permission.VIEW):
                add_secret(column)

    def __getitem__(self, key, lazy=False):
        """Vrať hodnotu políčka 'key' jako instanci třídy 'pytis.data.Value'.
        
        'key' je id políčka (řetězec) identifikující existující políčko, jinak
        je chování metody nedefinováno.
        
        """
        if key in self._row:
            value = self._row[key]
        else:
            value = self._virtual[key]
        if not lazy and self._dirty.get(key):
            column = self._coldict[key]
            # Reset the dirty flag before calling the computer to allow the computer to retrieve
            # the original value without recursion.
            self._dirty[key] = False
            func = column.computer.function()
            new_value = pytis.data.Value(column.type, func(self))
            if new_value.value() != value.value():
                value = new_value
                if key in self._row:
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
        if key in self._row:
            row = self._row
        else:
            row = self._virtual
        if key in self._invalid:
            del self._invalid[key]
        if row[key].value() != value.value():
            row[key] = value
            self._cache = {}
            if self._dirty.get(key):
                self._dirty[key] = False
            self._resolve_dependencies(key)
            if run_callback:
                self._run_callback(self.CALL_CHANGE, key)
                
    def __unicode__(self):
        if hasattr(self, '_row'):
            info = ', '.join([c.id + '=' + unicode(self[c.id].value()) for c in self._columns])
        else:
            info = '%x' % positive_id(self)
        return "<%s: %s>" % (self.__class__.__name__, info)

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
        if key is not None and key in self._dependent:
            for k in self._dependent[key]:
                self._dirty[k] = True
        # TODO: Do we need to do that always?  Eg. on set_row in BrowseForm?
        self._recompute_editability(key)
        self._notify_runtime_filter_change(key)
        if self._callbacks and key is not None and key in self._dependent:
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
        elif key in self._editability_dependent:
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
        self._editable[key] = result = func(self)
        self._editability_dirty[key] = False
        return result
    
    def _notify_runtime_filter_change(self, key=None):
        if key is None:
            keys = (self._runtime_filter_dirty.keys() +
                    self._runtime_arguments_dirty.keys())
        else:
            keys = (self._runtime_filter_dependent.get(key, []) +
                    self._runtime_arguments_dependent.get(key, []))
        for k in remove_duplicates(keys):
            self._runtime_filter_dirty[k] = True
            self._runtime_arguments_dirty[k] = True
            self._run_callback(self.CALL_ENUMERATION_CHANGE, k)

    def get(self, key, default=None, lazy=False, secure=False):
        """Return the value for the KEY if it exists or the DEFAULT otherwise.

          Arguments:
          
            default -- the default value returned when the key does not exist.
            lazy -- if true, the value will not be computed even if it should
              be.  This may result in returning an invalid value, but prevents
              the computer from being invoked.  Does nothing for fields without
              a computer.
            secure -- if 'False', the value is formatted in a common way; if
              'True', the value is replaced by type secret value replacement if
              its column is secret.
          
        """
        if secure and not self.permitted(key, pytis.data.Permission.VIEW):
            return default
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

        This method is in fact just a convenience wrapper for 'pytis.data.DataEnumerator.row()'.

        Returns a 'pytis.data.Value' instance or None when the enumerator doesn't contain the
        current value of the field 'key' (the field value is not valid).
            
        """
        value = self[key]
        row = value.type().enumerator().row(value.value(), transaction=self._transaction,
                                            condition=self.runtime_filter(key),
                                            arguments=self.runtime_arguments(key))
        if row is not None:
            return row[column]
        else:
            return None
        
    def row(self):
        """Return the current *data* row as a 'pytis.data.Row' instance."""
        row_data = [(c.id, self[c.id].retype(c.data_column.type()),)
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
    
    def resolver(self):
        """Return the 'resolver' passed to the constructor."""
        return self._resolver

    def format(self, key, pretty=False, form=None, secure=False, **kwargs):
        """Return the string representation of the field value.

        Arguments:

          key -- field identifier (string).
          pretty -- boolean flag indicating whether pretty export should be
            used to format the value.
          form -- 'Form' instance of the row's form
          secure -- if 'False', the value is formatted in a common way; if
            'True', the value is replaced by type secret value replacement if
            its column is secret; if a basestring, secret values are replaced by
            the string (this is useful for editable secret fields, to display an
            empty string there)
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
            return ''
        if secure is False or self.permitted(key, permission=pytis.data.Permission.VIEW):
            value_type = value.type()
            if pretty and isinstance(value_type, PrettyType):
                svalue = value_type.pretty_export(value.value(), row=self, form=form, **kwargs)
            else:
                svalue = value.export(**kwargs)
        else:
            if secure is True:
                svalue = value.type().secret_export()
            else:
                svalue = secure
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
        """Return the list of all field specifications as 'Field' instances."""
        return self._fields
        
    def __contains__(self, key):
        """Return true if a field of given key is contained within the row."""
        return key in self._coldict
        
    def has_key(self, key):
        return self.__contains__(key)
    
    def keys(self):
        """Return the list of identifiers of all fields contained within the row."""
        return [c.id for c in self._columns]
        
    def key(self):
        """Return the data key for this row as a tuple of key column 'Value' instances."""
        return tuple([self[c.id()] for c in self._data.key()])
        
    def new(self):
        """Return true if the row represents a new (inserted) record."""
        return self._new
    
    def original_row(self, empty_as_none=False):
        """Return a *data* row containing the values before any changes.

        The returned row is a 'pytis.data.Row' instance, not necasarilly identical with the row
        passed to the constructor.

        The original values are values after row initialization or after the last call to
        'set_row()' with 'reset' se to true.
        
        """
        if empty_as_none and self._original_row_empty:
            return None
        else:
            return self._original_row

    def original_presented_row(self):
        """Return 'PresentedRow' instance containing the original row values.

        The original values are values set in row initialization or after the
        last call to 'set_row()' with 'reset' se to true.
        
        """
        original_row = self.original_row()
        original_record = copy.copy(self)
        original_record.set_row(original_row)
        return original_record

    def changed(self):
        """Return true if the *data* row has been changed.

        The row is considered changed if the underlying data row is not equal to the original row
        passed to (or created in) the constructor in the sense of the `=' operator.  Changes in the
        virtual fields (not present in the underlying data row) are ignored.

        """
        for key in self._row.keys():
            if self.field_changed(key):
                return True
        return False

    def field_changed(self, key):
        """Return true if given field was changed compared to its original value.

        Warning: True is always returned for virtual fields (with no underlying data column).  For
        all computed fields the result may not be accurate because the recomputation may not have
        happened yet.

        """
        return key not in self._row or \
               self._row[key].value() != self._original_row[key].value() or \
               key in self._invalid

    def editable(self, key):
        """Vrať pravdu, právě když je políčko dané 'key' editovatelné.

        Význam argumentu 'key' je stejný jako v metodě '__getitem__'.

        """
        if not self.permitted(key, permission=True):
            return False
        if self.hidden_codebook(key):
            return False
        if key in self._editable:
            if self._editability_dirty[key]:
                result = self._compute_editability(key)
            else:
                result = self._editable[key]
        else:
            editable = self._coldict[key].editable
            result = (editable == Editable.ALWAYS or editable == Editable.ONCE and self._new)
        return result
    
    def type(self, key):
        """Return the data type of field identified by 'key'."""
        return self._coldict[key].type

    def hidden_codebook(self, key):
        """Return true iff field identified by 'key' is bound to a non-readable codebook."""
        column = self._coldict[key]
        enumerator = column.type.enumerator()
        return isinstance(enumerator, pytis.data.DataEnumerator) and not enumerator.permitted()
        
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
        if column.runtime_filter is not None:
            kwargs = dict(kwargs, condition=self.runtime_filter(key))
        if column.runtime_arguments:
            kwargs = dict(kwargs, arguments=self.runtime_arguments(key))
        value, error = column.type.validate(string, transaction=self._transaction, **kwargs)
        if not error and column.type.unique() and not column.virtual and \
               (self._new or value != self._original_row[key]) and value.value() is not None:
            if isinstance(self._data, pytis.data.RestrictedData):
                select_kwargs = dict(check_condition=False)
            else:
                select_kwargs = dict()
            count = self._data.select(condition=pytis.data.EQ(column.id, value),
                                      transaction=self._transaction, **select_kwargs)
            self._data.close()
            if count != 0:
                error = pytis.data.ValidationError(_(u"Taková hodnota již existuje."))
        result = error
        if error:
            value, error = column.type.validate(string, transaction=self._transaction,
                                                strict=False, **kwargs)
        if not error:
            if key in self._invalid:
                del self._invalid[key]
            if string != self.format(key):
                self.__setitem__(key, value, run_callback=False)
        else:
            if string != self.format(key):
                self._invalid[key] = string
            elif key in self._invalid:
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
        assert function is None or isinstance(function, collections.Callable), ('Invalid callback function', function)
        try:
            callbacks = self._callbacks[kind]
        except KeyError:
            callbacks = self._callbacks[kind] = {}
        if key in callbacks:
            raise ProgramError("Callback already registered:", kind, key, callbacks[key])
        callbacks[key] = function

    def permitted(self, key, permission):
        """Return true if the user has permissons for given field.
        
        Arguments:
        
          key -- field identifier (string).
          permisson -- one of 'pytis.data.Permission' constants determinint the perission to be
            checked or 'True' in which case corresponding editing permission
            (insert or update) is checked.
          
        Permission checking of virtual fields is limited to the VIEW
        permission, based on the field dependencies.  VIEW of a virtual column
        is permitted if all the fields from dependencies are allowed to VIEW.
        The result is undefined for other permissions of virtual fields.

        """
        if permission is True:
            if self._new:
                permission = pytis.data.Permission.INSERT
            else:
                permission = pytis.data.Permission.UPDATE
        column = self._coldict[key]
        if column.virtual:
            permitted = not column.secret_computer
        elif isinstance(self._data, pytis.data.RestrictedData):
            permitted = self._data.permitted(key, permission)
        else:
            permitted = True
        return permitted

    def _cb_spec(self, column):
        try:
            cb_spec = self._cb_spec_cache[column.id]
        except KeyError:
            codebook = column.codebook
            if codebook:
                try:
                    cb_spec = self._resolver.get(codebook, 'cb_spec')
                except ResolverError, e:
                    cb_spec = CodebookSpec()
            else:
                cb_spec = CodebookSpec()
            self._cb_spec_cache[column.id] = cb_spec
        return cb_spec

    def _completer(self, column):
        try:
            completer = self._completer_cache[column.id]
        except KeyError:
            completer = column.completer(self._resolver)
            if not completer and column.type.enumerator() \
                    and isinstance(column.type, pytis.data.String):
                cb_spec = self._cb_spec(column)
                if not cb_spec or cb_spec.enable_autocompletion():
                    completer = column.type.enumerator()
            self._completer_cache[column.id] = completer
        return completer

    def _display(self, column):
        # Returns a display function to apply to an enumeration value."
        if self._secret_column(column):
            hidden_value = column.type.secret_export()
            display = lambda v: hidden_value
        else:
            display = column.display or self._cb_spec(column).display()
            if display:
                if isinstance(display, str):
                    display_column = display
                    row_function = lambda row: row[display_column].export()
                elif argument_names(display) == ('row',):
                    row_function = display
                else:
                    row_function = None
                if row_function:
                    enumerator = column.type.enumerator()
                    def display(value):
                        if value is None or self._transaction and not self._transaction.open():
                            return ''
                        try:
                            row = enumerator.row(value, transaction=self._transaction,
                                                  condition=self.runtime_filter(column.id),
                                                  arguments=self.runtime_arguments(column.id))
                        except pytis.data.DataAccessException:
                            return ''
                        if row:
                            return row_function(row)
                        else:
                            return ''
        return display

    def _display_as_row_function(self, column):
        # Same as '_display()', but returns a function of a data row.  It would be possible to use
        # '_display()' everywhere, but that causes major inefficiency in 'enumerate()' (separate
        # select for each row of the select).
        if self._secret_column(column):
            hidden_value = column.type.secret_export()
            display = lambda row: hidden_value
        else:
            display = column.display or self._cb_spec(column).display()
            if display is None:
                value_column = column.type.enumerator().value_column()
                display = lambda row: row[value_column].export()
            elif isinstance(display, str):
                display_column = display
                display = lambda row: row[display_column].export()
            elif argument_names(display) != ('row',):
                value_column = column.type.enumerator().value_column()
                display_function = display
                display = lambda row: display_function(row[value_column].value())
        return display
    
    def codebook(self, key):
        """Return the name of given field's codebook specification for resolver."""
        return self._coldict[key].codebook

    def prefer_display(self, key):
        column = self._coldict[key]
        if column.prefer_display is not None:
            return column.prefer_display
        else:
            return self._cb_spec(column).prefer_display()
        
    def display(self, key):
        """Return enumerator `display' value for given field as a string.

        If the field has no enumerator or no display was specified, an empty
        string is returned.

        Empty string is also returned if the current field value doesn't belong
        to the enumeration (is invalid) or if it is not possible to retrieve
        the displayed value (isufficient access rights, current transaction
        aborted etc.)
        
        """
        column = self._coldict[key]
        if self._secret_column(column):
            return ''
        display = self._display(column)
        if not display:
            computer = column.computer
            if computer and isinstance(computer, CbComputer):
                column = self._coldict[computer.field()]
                display = self._display(column)
        value = self[column.id].value()
        if value is None:
            return column.null_display or ''
        elif display:
            return display(value)
        else:
            return ''
    
    def enumerate(self, key):
        """Return the list of valid values of an enumeration field.

        Returns a list of pairs (VALUE, DISPLAY), where VALUE is the internal
        python value and DISPLAY is the corresponding user visible string
        (unicode) for that value (as defined by the `display' specification).

        If the field given by 'key' has no enumerator, None is returned.  The
        inner_type's enumerator is used automatically for fields of type
        'pytis.data.Array'.
       
        """
        column = self._coldict[key]
        if self._secret_column(column):
            return []
        ctype = column.type
        if isinstance(ctype, pytis.data.Array):
            ctype = ctype.inner_type()
        enumerator = ctype.enumerator()
        if enumerator is None:
            return None
        elif isinstance(enumerator, pytis.data.DataEnumerator):
            sorting = None
            cb_spec = self._cb_spec(column)
            if cb_spec:
                sorting = cb_spec.sorting()
            if sorting is None and column.codebook is not None:
                sorting = self._resolver.get(column.codebook, 'view_spec').sorting()
            value_column = enumerator.value_column()
            display = self._display_as_row_function(column)
            return [(row[value_column].value(), display(row))
                    for row in enumerator.rows(transaction=self._transaction,
                                               condition=self.runtime_filter(key),
                                               arguments=self.runtime_arguments(key),
                                               sort=sorting or ())]
        else:
            display = self._display(column)
            if display is None:
                display = lambda v: column.type.export(v)
            return [(v, display(v)) for v in enumerator.values()]

    def _runtime_limit(self, key, dirty_dict, value_dict, column_attribute):
        try:
            dirty = dirty_dict[key]
        except KeyError:
            return None
        if dirty:
            column = self._coldict[key]
            computer = getattr(column, column_attribute)
            if computer is None:
                result = value_dict[key] = None
            else:
                function = computer.function()
                result = value_dict[key] = function(self)
            dirty_dict[key] = False
        else:
            result = value_dict[key]
        return result
        
    def runtime_filter(self, key):
        """Return the current run-time filter condition for an enumerator of field KEY.

        Returns a 'pytis.data.Operator' instance when a filter is active or None if the field has
        no enumerator or if the enumerator is not filtered.

        """
        return self._runtime_limit(key, self._runtime_filter_dirty, self._runtime_filter,
                                   'runtime_filter')

    def runtime_arguments(self, key):
        """Return the current run-time arguments for a table function based codebook of field KEY.

        Returns an arguments dictionary (possibly empty), or 'None' if the
        field has no table function based codebook.

        """
        return self._runtime_limit(key, self._runtime_arguments_dirty, self._runtime_arguments,
                                   'runtime_arguments')

    def has_completer(self, key, static=False):
        """Return true if given field has a completer.

        Arguments:
          key -- field identifier as a string
          static -- if true, true is returned only if the completer is defined by a static set of
            values (i.e. it is not bound to a data object).

        """
        column = self._coldict[key]
        completer = self._completer(column)
        if completer is None:
            return False
        elif static:
            return not isinstance(completer, pytis.data.DataEnumerator)
        else:
            return True

    def completions(self, key, prefix=None):
        """Return the list of available completions for given prefix.

        Arguments:
          key -- field identifier as a string
          prefix -- prefix value as a (unicode) string or None.  If specified, the list of
            completions is filtered for values with given prefix.  Prefix matching is case
            insensitive.

        The returned list contains available completions provided by the underlying completer of
        given field.  The completer is determined either by the 'completer' argument in field
        specification or (if not defined) the enumerator of the field's data type.  If the field is
        not associated with any completer, the method always returns an empty list.  The method
        'has_completer()' may be used to find out, whether the field has a completer.
        
        """
        column = self._coldict[key]
        completer = self._completer(column)
        if completer is not None:
            if prefix:
                prefix = prefix.lower()
            if isinstance(completer, pytis.data.DataEnumerator):
                condition = self.runtime_filter(key)
                if prefix:
                    wmvalue = pytis.data.WMValue(pytis.data.String(), prefix+'*')
                    prefix_condition = pytis.data.WM(completer.value_column(), wmvalue)
                    if condition:
                        condition = pytis.data.AND(condition, prefix_condition)
                    else:
                        condition = prefix_condition
                arguments = self.runtime_arguments(key)
                choices = completer.values(condition=condition, arguments=arguments, max=40) or []
            else:
                # TODO: runtime filter doesn't apply here.  We would need to use MemData object to
                # apply filtering by a pytis operator (which would also have the advantage of
                # common handling of both static and data object based completers).
                choices = completer.values()
                if prefix:
                    choices = [x for x in choices if x.lower().startswith(prefix)]
                else:
                    choices = list(choices)
                import locale
                choices.sort(key=lambda x: locale.strxfrm(x).lower())
            if not (len(choices) == 1 and prefix and choices[0].lower() == prefix):
                return choices
        return []

    def depends(self, key, keys):
        """Return True iff any of the columns in 'keys' depend on column 'key'.

        Arguments:
          key -- field identifier as a string
          keys -- sequence of field identifiers as strings

        Dependencies are established through 'computer', 'editability',
        'runtime_filter' and 'runtime_arguments' specifications of 'Field'.

        """
        for deps in (self._dependent,
                     self._editability_dependent,
                     self._runtime_filter_dependent,
                     self._runtime_arguments_dependent):
            if key in deps:
                for k in deps[key]:
                    if k in keys:
                        return True
        return False
