# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2017 OUI Technology Ltd.
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
from past.builtins import basestring
from future.utils import python_2_unicode_compatible

import copy

import pytis.data
from pytis.util import (
    UNDEFINED, ProgramError, Resolver, argument_names,
    remove_duplicates, translations, format_byte_size,
)

from .spec import CbComputer, CodebookSpec, Computer
from .types_ import PrettyType

_ = translations('pytis-data')

unistr = type(u'')  # Python 2/3 transition hack.


@python_2_unicode_compatible
class PresentedRow(object):
    """A record of presented data.

    The class is an intermediate layer between a data row and its final
    presentation.  As oposed to the data row, it contains all fields present in
    field specifications (including virtual fields).  On the other hand, it
    doesn't solve a concrete presentation beyond string formatting.

    """
    CALL_CHANGE = 'CALL_CHANGE'
    """Callback called on indirect field change.

    Invoked when the field value changes due to its computer dependency on
    other field(s).

    """
    CALL_EDITABILITY_CHANGE = 'CALL_EDITABILITY_CHANGE'
    """Callback called on field editability change.

    Invoked when the field editability changes due to its dependency on another
    field's value(s).

    """
    CALL_VISIBILITY_CHANGE = 'CALL_VISIBILITY_CHANGE'
    """Callback called on field visibility change.

    Invoked when the field visibility changes due to its dependency on another
    field's value(s).

    """
    CALL_ENUMERATION_CHANGE = 'CALL_ENUMERATION_CHANGE'
    """Callback called on field enumeration change.

    Invoked when the enumaration filter changes due to its dependency on
    another field's value(s).  The enumaration is the list of valid field
    values provided by data type enumarator.

    """
    CALL_CHECK = 'CALL_CHECK'
    """Callback called when the field's check function needs to be re-evaluated.

    The field's check function needs to be re-evaluated whenever the values of
    columns on which it depends change.

    """
    class ProtectionError(Exception):
        """Exception raised on column protection violations."""

    @python_2_unicode_compatible
    class _Column(object):

        class ComputedValue(object):

            def __init__(self, column, computer, callback):
                self.column = column
                self.callback = callback
                self.function = computer.function()
                self.depends = computer.depends()
                self.fallback = computer.fallback()
                self.validate = computer.validate()
                self.dirty = True
                self.value = None

        def __init__(self, fspec, data, resolver):
            self.id = fspec.id()
            self.data_column = data.find_column(self.id)
            computer = fspec.computer()
            if self.data_column:
                # Actually, the type taken from the data object always takes precedence, since it
                # should already respect type and its arguments from field specification -- they are
                # passed to column binding constructors when the data object is created.
                ctype = self.data_column.type()
            elif fspec.type():
                ctype = fspec.type()
                if not isinstance(ctype, pytis.data.Type):
                    ctype = ctype(**fspec.type_kwargs())
            elif computer and isinstance(computer, CbComputer):
                # If a virtual field is a CbComputer, we can take the data type from
                # the related column enumerator's data object.
                enumerator = data.find_column(computer.field()).type().enumerator()
                ctype = enumerator.type(computer.column())
                assert ctype is not None, \
                    ("Invalid enumerator column '%s' in CbComputer for '%s'." %
                     (computer.column(), self.id))
            else:
                # String is the default type of virtual columns.
                ctype = pytis.data.String(**fspec.type_kwargs())
            self.type = ctype
            self.line_separator = fspec.line_separator()
            self.formatter = fspec.formatter()
            self.default = fspec.default()
            self.codebook = codebook = fspec.codebook()
            if codebook:
                try:
                    cbspec = resolver.get(codebook, 'cb_spec')
                except pytis.util.ResolverError:
                    cbspec = CodebookSpec()
            else:
                cbspec = None
            self.cbspec = cbspec
            self.display = fspec.display() or cbspec and cbspec.display()
            self.null_display = fspec.null_display()
            self.inline_display = fspec.inline_display()
            prefer_display = fspec.prefer_display()
            if prefer_display is None:
                if cbspec is not None:
                    prefer_display = cbspec.prefer_display()
                else:
                    prefer_display = False
            self.prefer_display = prefer_display
            self.completer = fspec.completer
            self.virtual = fspec.virtual()
            self.secret_computer = False  # Set dynamically during initialization.
            self.attachment_storage = fspec.attachment_storage()
            self.filename = fspec.filename()
            self.is_range = isinstance(ctype, pytis.data.Range)
            self.last_validated_string = None
            self.last_validation_error = None
            self.check_constraints_cache = (None, None)
            self.dependent = []  # Will be initialized later by PresentedRow.__init__().

            def cval(computer, callback):
                if computer is None:
                    return None
                else:
                    if not isinstance(computer, Computer):
                        value = computer
                        computer = Computer(lambda r: value, depends=())
                return self.ComputedValue(self, computer, callback)

            self.computer = cval(computer, PresentedRow.CALL_CHANGE)
            self.editable = cval(fspec.editable(), PresentedRow.CALL_EDITABILITY_CHANGE)
            self.visible = cval(fspec.visible(), PresentedRow.CALL_VISIBILITY_CHANGE)
            self.check = cval(fspec.check(), PresentedRow.CALL_CHECK)
            self.runtime_filter = cval(fspec.runtime_filter(),
                                       PresentedRow.CALL_ENUMERATION_CHANGE)
            self.runtime_arguments = cval(fspec.runtime_arguments(),
                                          PresentedRow.CALL_ENUMERATION_CHANGE)

        def __str__(self):
            return "<_Column id='%s' type='%s' virtual='%s'>" % (self.id, self.type, self.virtual)

    def __init__(self, fields, data, row, prefill=None, singleline=False, new=False,
                 resolver=None, transaction=None):
        """Initialize the instance.

        Arguments:

          fields -- a sequence of field specifications as 'Field' instances.
          data -- the underlying data object as a 'pytis.data.Data' instance.
          transaction -- current transaction for data operations.
          row -- initial row data (see below).
          prefill -- a dictionary of values for row initialization.  The
            dictionary is keyed by field identifiers and the values can be
            either 'pytis.data.Value' instances or the corresponding Python
            internal values (matching the field type).  These values take
            precedence before default values, the values contained within the
            passed 'row' as well as the computed values (the computers for
            prefilled values are not invoked).
          singleline -- a boolean flag indicating that the exported values of
            all fields will be formatted to single line (influences the
            'format()' method behavior).
          new -- boolean flag determining whether the row represents a new
            record for insertion or an existing row for select or update.
          resolver -- a 'Resolver' instance for specification retrieval.  If
            not used, the global resolver returned by 'pytis.util.resolver()'
            will be used.

        Initial field values are determined depending on the argument 'row',
        which can have one of the following values:

          * None -- default values will be generated according to field
            specifications.
          * 'PresentedRow' instance -- field values are taken from this instance.
          * 'pytis.data.Row' instance -- field values are taken from this data
            row.

        In any case only the state of the 'row' in the time of this constructor
        call matters.  Any later changes to it have no effect on the newly
        created instance.

        """
        def all_deps(cval):
            if cval:
                deps = list(cval.depends)
                for key in cval.depends:
                    deps.extend(all_deps(self._coldict[key].computer))
                return remove_duplicates(deps)
            else:
                return []

        def add_secret(column):
            for cval in column.dependent:
                if not cval.column.secret_computer:
                    column.secret_computer = True
                    add_secret(cval.column)

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
        self._new = new
        self._transaction = transaction
        self._resolver = resolver or pytis.util.resolver()
        self._callbacks = {}
        self._formatted_value_cache = {}
        self._completer_cache = {}
        self._protected = False
        self._columns = columns = tuple([self._Column(f, data, self._resolver) for f in fields])
        self._coldict = coldict = dict([(c.id, c) for c in columns])
        for column in columns:
            # Remember which computed values depend on each field (this the opposite
            # direction than what is defined in specifications).  column.dependent
            # will include a list of all ComputedValue instances which depend on
            # given column's value.
            for cval in (column.computer, column.editable, column.visible, column.check,
                         column.runtime_filter, column.runtime_arguments):
                for cid in all_deps(cval):
                    coldict[cid].dependent.append(cval)
        for column in columns:
            if not self.permitted(column.id, pytis.data.Permission.VIEW):
                add_secret(column)
        self._set_row(row, reset=True, prefill=prefill)

    def __str__(self):
        if hasattr(self, '_row'):
            def strval(column):
                if isinstance(column.type, pytis.data.Password):
                    return "***"
                elif (isinstance(column.type, pytis.data.Big) and
                      self[column.id].value() is not None):
                    return '<%s %s>' % (column.type.__class__.__name__,
                                        format_byte_size(len(self[column.id].value())))
                else:
                    return unistr(self[column.id].value())
            info = ', '.join([c.id + '=' + strval(c) for c in self._columns])
        else:
            info = '%x' % id(self)
        return "<%s: %s>" % (self.__class__.__name__, info)

    def __getitem__(self, key, lazy=False):
        """Return the value of given field  as a 'pytis.data.Value' instance.

        'key' key is a string identifier of a one of the fields present in the
        row.  If not present, 'KeyError' is raised.

        """
        column = self._coldict[key]
        if self._protected and self._secret_column(column):
            raise self.ProtectionError(key)
        if key in self._row:
            row = self._row
        else:
            row = self._virtual
        value = row[key]
        computer = column.computer
        if not lazy and computer and computer.dirty:
            old_value = value.value()
            new_value = self._computed_value(computer, old_value=old_value)
            if new_value != old_value:
                value = row[key] = pytis.data.Value(column.type, new_value)
        return value

    def __setitem__(self, key, value, run_callback=True):
        """Set given field's value to given 'pytis.data.Value' instance.

        Arguments:
          key -- key is a string identifier of a one of the fields present in
            the row.  If no such field exists, 'KeyError' is raised.
          value -- Field value either as a 'pytis.data.Value' instance of a
            matching type or a Python representation of the field's inner value
            as accepted by 'Type.adjust_value()' of the field's type.  In both
            cases TypeError is raised if the types don't match (value is a
            'pytis.data.Value' instance of a different type or an inner value
            not acceptable by the type).

        The argument 'run_callback' is not public as the method is only
        supposed to be used as dictionary assignment (not called directly).

        """
        column = self._coldict[key]
        ctype = column.type
        if not isinstance(value, pytis.data.Value):
            value = pytis.data.Value(ctype, value)
        elif value.type() != ctype:
            raise TypeError("Invalid type for '%s': %s (expected %s)" % (key, value.type(), ctype))
        if key in self._row:
            row = self._row
        else:
            row = self._virtual
        column.last_validated_string = None
        column.last_validation_error = None
        if row[key].value() != value.value():
            row[key] = value
            self._formatted_value_cache = {}
            if column.computer:
                column.computer.dirty = False
            self._resolve_dependencies(column.dependent)
            if run_callback:
                self._run_callback(self.CALL_CHANGE, key)

    def __contains__(self, key):
        """Return true if a field of given key is contained within the row."""
        return key in self._coldict

    def _set_row(self, row, reset=False, prefill=None):
        self._virtual = {}
        row_data = []
        computed_values = []
        prefill = copy.copy(prefill)
        for column in self._columns:
            key = column.id
            dirty = False
            if prefill and key in prefill:
                val = prefill.pop(key)
                if isinstance(val, pytis.data.Value):
                    value = val.retype(column.type)
                else:
                    value = pytis.data.Value(column.type, val)
            elif row and key in row:
                value = row[key].retype(column.type)
            elif self._new and column.default is not None:
                default = column.default
                if callable(default):
                    try:
                        default = default(transaction=self._transaction)
                    except TypeError:
                        default = default()
                value = pytis.data.Value(column.type, default)
            else:
                value = column.type.default_value()
                # It would seem logical to set dirty = True here, but historically,
                # the bahavior was this.  The computer is not trigered when new is
                # False and row is None.  But this is quite an uncommon combination
                # so we may not need to keep this behavior?
                dirty = self._new or row is not None
            if column.virtual:
                self._virtual[key] = value
            else:
                row_data.append((key, value))
            if column.computer:
                column.computer.dirty = dirty
            column.last_validated_string = None
            column.last_validation_error = None
            column.check_constraints_cache = (None, None)
            computed_values += [c for c in (column.editable, column.visible, column.check,
                                            column.runtime_filter, column.runtime_arguments) if c]
        if prefill:
            raise KeyError(list(prefill.keys())[0])
        if row:
            # Preserve any extra row columns - they may include inline_display values.
            keys = [x[0] for x in row_data]
            row_data.extend([(k, row[k]) for k in row.keys() if k not in keys])
        self._row = pytis.data.Row(row_data)
        self._formatted_value_cache = {}
        if reset:
            self._original_row = copy.copy(row)
            if not hasattr(self, '_initialized_original_row'):
                # Calling row() may invoke dirty column computations.  The computers may use the
                # original row as well, so we must create one before.
                self._initialized_original_row = copy.copy(self._row)
            self._initialized_original_row = self.row()
        self._resolve_dependencies(computed_values)
        self._run_callback(self.CALL_CHANGE, None)

    def _resolve_dependencies(self, computed_values):
        # Handle recomputations for given list of ComputedValue instances.
        for cval in computed_values:
            # First mark all depending values as dirty and only then start
            # invoking callbacks.  Otherwise the callbacks would se an
            # inconsistent state.
            cval.dirty = True
        changed_enumerations = []
        for cval in computed_values:
            callback = cval.callback
            if callback in self._callbacks:
                key = cval.column.id
                if callback == self.CALL_ENUMERATION_CHANGE:
                    # Avoid calling twice (shared by runtime_filter and runtime_arguments).
                    if key in changed_enumerations:
                        continue
                    changed_enumerations.append(key)
                self._run_callback(callback, key)

    def _computed_value(self, cval, old_value=UNDEFINED):
        if not cval:
            result = None
        elif cval.dirty and all(self._check_constraints(self._coldict[key], self[key]) is None
                                for key in cval.validate):
            # Only invoke the computer if all validated input fields are valid.
            cval.dirty = False
            # Reset the dirty flag before calling the computer function to allow
            # the computer function to retrieve the original value without recursion.
            result = cval.value = cval.function(self)
        elif cval.fallback is not UNDEFINED:
            # If inputs are not valid return the fallback value (if defined).
            result = cval.fallback
        elif old_value is not UNDEFINED:
            # And if no fallback is defined, return the previous value.  Previous
            # value may be passed as argument when kept outside the ComputedValue
            # instance (this is the case for the actual field value, which is in
            # self._row or self._virtual).
            result = old_value
        else:
            # In all other cases, the previous value is kept inside the
            # ComputedValue instance.
            result = cval.value
        return result

    def _check_constraints(self, column, value, bypass_cache=False):
        if not bypass_cache:
            cached_value, cached_result = column.check_constraints_cache
            if cached_value and cached_value.value() == value.value():
                return cached_result
        kwargs = dict(transaction=self._transaction)
        if column.runtime_filter is not None:
            kwargs['condition'] = self.runtime_filter(column.id)
        if column.runtime_arguments:
            kwargs['arguments'] = self.runtime_arguments(column.id)
        error = column.type.check_constraints(value.value(), **kwargs)
        if ((not error and column.type.unique() and not column.virtual and
             (self._new or value != self._initialized_original_row[column.id]) and
             value.value() is not None)):
            select_kwargs = {}
            if isinstance(self._data, pytis.data.RestrictedData):
                select_kwargs['check_condition'] = False
            count = self._data.select(condition=pytis.data.EQ(column.id, value),
                                      transaction=self._transaction, **select_kwargs)
            self._data.close()
            if count != 0:
                error = pytis.data.ValidationError(_("Such value already exists."))
        column.check_constraints_cache = (value, error)
        return error

    def _run_callback(self, kind, key=None):
        callbacks = self._callbacks.get(kind, {})
        if key is None:
            for callback in callbacks.values():
                callback()
        else:
            callback = callbacks.get(key)
            if callback:
                callback()

    def _secret_column(self, column):
        if column.virtual:
            return column.secret_computer
        else:
            return not self.permitted(column.id, pytis.data.Permission.VIEW)

    def _completer(self, column):
        try:
            completer = self._completer_cache[column.id]
        except KeyError:
            completer = column.completer()
            if not completer and column.type.enumerator() \
                    and isinstance(column.type, pytis.data.String):
                if column.cbspec is None or column.cbspec.enable_autocompletion():
                    completer = column.type.enumerator()
            self._completer_cache[column.id] = completer
        return completer

    def _display(self, column, export=None):
        # Returns a display function to apply to an enumeration value."
        if self._secret_column(column):
            hidden_value = column.type.secret_export()

            def display(v):
                return hidden_value
        else:
            display = column.display
            if display:
                if isinstance(display, basestring):
                    display_column = display
                    # Note, we can't use format() to handle multiline values
                    # (we don't have PresentedRow, just the data Row) so we
                    # simply join the lines using semicolons.  It would be
                    # better to respect the line_separator of the display
                    # field, but we don't have simple access to it from here
                    # and the semicolons are fine in most cases.

                    def row_function(row):
                        if export:
                            exported = export(row[display_column])
                        else:
                            exported = row[display_column].export()
                        if isinstance(exported, tuple):
                            # Handle range fields (avoid join to protect localizable strings).
                            return exported[0] + ' – ' + exported[1]
                        else:
                            return '; '.join(exported.splitlines())
                elif argument_names(display) == ('row',):
                    row_function = display
                else:
                    row_function = None
                if row_function:
                    if isinstance(column.type, pytis.data.Array):
                        enumerator = column.type.inner_type().enumerator()
                    else:
                        enumerator = column.type.enumerator()
                    if isinstance(enumerator, pytis.data.DataEnumerator):
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

    def _display_as_row_function(self, column, export=None):
        # Same as '_display()', but returns a function of a data row.  It would be possible to use
        # '_display()' everywhere, but that causes major inefficiency in 'enumerate()' (separate
        # select for each row of the select).
        def check_result(result):
            assert isinstance(result, basestring), \
                "Invalid result of display function for column '%s': %r" % (column.id, result)
            return result

        if self._secret_column(column):
            hidden_value = column.type.secret_export()

            def display_function(row):
                return hidden_value
        else:
            display = column.display
            if display is None:
                display = column.type.enumerator().value_column()

            if isinstance(display, basestring):
                if not export:
                    def export(value):
                        return value.export()

                def display_function(row):
                    result = export(row[display])
                    if isinstance(result, tuple):
                        # Handle range fields (avoid join to protect localizable strings).
                        result = result[0] + ' – ' + result[1]
                    return result
            elif argument_names(display) != ('row',):
                value_column = column.type.enumerator().value_column()

                def display_function(row):
                    return check_result(display(row[value_column].value()))
            else:
                def display_function(row):
                    return check_result(display(row))

        return display_function

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
        """Return the value of another column in given field's enumerator data object.

        Arguments:

          key -- field identifier (string).  This field must have an enumerator
            of type 'DataEnumerator'.
          column -- identifier of another column in the enumerator's data
            object.

        This method is in fact just a convenience wrapper for
        'pytis.data.DataEnumerator.row()'.

        Returns a 'pytis.data.Value' instance or None when the enumerator
        doesn't contain the current value of the field 'key' (the field value
        is not valid).

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

    def format(self, key, pretty=False, form=None, secure=False, export=None, single=True,
               **kwargs):
        """Return the string representation of the field value.

        Arguments:

          key -- field identifier (string).
          pretty -- boolean flag indicating whether pretty export should be
            used to format the value.
          form -- 'Form' instance of the row's form
          secure -- if 'False', the value is formatted in a common way; if
            'True', the value is replaced by type secret value replacement if
            its column is secret; if a basestring, secret values are replaced
            by the string (this is useful for editable secret fields, to
            display an empty string there)
          single -- always export as a single string (bool).  If true, the
            method returns a single string also for Range types.  Otherwise
            range values are returned as a tuple of two separately formatted
            values.
          export -- custom export function to use instead of 'Value.export()'
            (callable of one argument, the internal field value)
          kwargs -- keyword arguments passed to the 'export()' method of the
            field's 'Value' instance.

        """
        cache_key = (key, single, secure, pretty, export)
        try:
            return self._formatted_value_cache[cache_key]
        except KeyError:
            pass
        try:
            value = self[key]
        except KeyError:
            # Může nastat například v případě, kdy k danému sloupci nejsou
            # přístupová práva.
            return ''
        column = self._coldict[key]
        if secure is False or self.permitted(key, permission=pytis.data.Permission.VIEW):
            if column.formatter is not None:
                svalue = column.formatter(value.value())
            else:
                value_type = value.type()
                if pretty and isinstance(value_type, PrettyType):
                    svalue = value_type.pretty_export(value.value(), row=self, form=form, **kwargs)
                else:
                    if export is not None:
                        svalue = export(value, **kwargs)
                    else:
                        svalue = value.export(**kwargs)
        else:
            if secure is True:
                svalue = value.type().secret_export()
            else:
                svalue = secure
        if self._singleline and column.line_separator is not None:
            if svalue is None:
                svalue = ''
            elif column.is_range:
                svalue = tuple(column.line_separator.join(s.splitlines()) for s in svalue)
            else:
                svalue = column.line_separator.join(svalue.splitlines())
        if single and column.is_range:
            if svalue == ('', ''):
                svalue = ''
            else:
                svalue = u' — '.join(x or _("unlimited") for x in svalue)
        self._formatted_value_cache[cache_key] = svalue
        return svalue

    def set_row(self, row, reset=False, prefill=None):
        """Set the current row data according to 'row'.

        Arguments:

           row -- has the same meaning as the constructor argument of the same
             name.
           reset -- a boolean flag indicating, that the new row data will now
             be considered as original.  This influences the behavior of the
             methods 'changed()' and 'original_row()'.
           prefill -- has the same meaning as the constructor argument of the
             same name.

        This method is meant to support the concept of current row in a form
        with fixed fields/columns and data object.  It saves the specification
        processing in the constructor.

        """
        self._set_row(row, reset=reset, prefill=prefill)

    def fields(self):
        """Return the list of all field specifications as 'Field' instances."""
        return self._fields

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

    def original_row(self, initialized=True):
        """Return a *data* row containing the values before changes.

        Arguemnts:

          initialized -- if True (default), return the row with initialized
            values according to prefill, default and computer initializations.
            If False, return the 'row' prior to all internal initializations.
            A higher level explanation of the same is that with
            initialized=False you get the original data row, while with
            initialized=True you get the row values before any user changes.

        In both cases (initialized=True/False) the returned row corresponds to
        the 'row' passed to the constructor or the last call to 'set_row()'
        with 'reset' set to true.  If 'initialized' is False, the returned
        value may be None if the 'row' argument was None.  Otherwise it is a
        'pytis.data.Row' instance.

        """
        if initialized:
            return self._initialized_original_row
        else:
            return self._original_row

    def changed(self):
        """Return true if the *data* row has been changed.

        The row is considered changed if the underlying data row is not equal
        to the original row passed to (or created in) the constructor in the
        sense of the `=' operator.  Changes in the virtual fields (not present
        in the underlying data row) are ignored.

        """
        for key in self._row.keys():
            if self.field_changed(key):
                return True
        return False

    def field_changed(self, key):
        """Return true if given field was changed compared to its original value.

        Warning: True is always returned for virtual fields (with no underlying
        data column).  For all computed fields the result may not be accurate
        because the recomputation may not have happened yet.

        """
        if key not in self._row:
            return True
        if self._row[key].value() != self._initialized_original_row[key].value():
            return True
        column = self._coldict[key]
        if column.last_validation_error:
            # If the last validation attempt didn't succeed (the validated string
            # didn't propagate to self._row), the field should also appear as changed
            # unless the last validated string matches the string representation of the
            # original value (which may have been invalid for some reason).  This
            # is important for reasonable behavior when leaving a form (warning
            # about unsaved changes).
            return column.last_validated_string != self.format(key)
        return False

    def editable(self, key):
        """Return True if given field is currently editable."""
        if not self.permitted(key, permission=True):
            return False
        if self.hidden_codebook(key):
            return False
        return bool(self._computed_value(self._coldict[key].editable))

    def visible(self, key):
        """Return True if given field is currently visible."""
        return bool(self._computed_value(self._coldict[key].visible))

    def check(self, key):
        """Return the result of 'check' for field identified by 'key'."""
        return self._computed_value(self._coldict[key].check)

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

        If the string is not valid, it is saved (can be retrieved later by the
        'invalid_string()' method) and this state is also reflected by the
        'changed()' and 'field_changed()' methods.  This state is updated after
        each validation attempt.

        Returns: 'pytis.data.ValidationError' instance if an error occurs or
        None if the string is valid.

        """
        column = self._coldict[key]
        value, error = column.type.validate(string, strict=False, **kwargs)
        if not error:
            error = self._check_constraints(column, value, bypass_cache=True)
        if value and string != self.format(key):
            self.__setitem__(key, value, run_callback=False)
        column.last_validated_string = string
        column.last_validation_error = error
        return error

    def invalid_string(self, key):
        """Return the last invalid user input string for given field.

        Returns a string passed to the last call to 'validate()' since the last
        'set_row()' or '__setitem__(key)' call if this last input string was
        invalid.  None is returned if the last validation was successful or if
        the field has not been validated yet.

        """
        column = self._coldict[key]
        if column.last_validation_error:
            return column.last_validated_string
        else:
            return None

    def validation_error(self, key):
        """Return the last validation error for given field.

        Returns the 'pytis.data.ValidationError' instance returned by the last
        call to 'validate()' since the last 'set_row()' or '__setitem__()'
        call.  None is returned if the last validation was successful or if the
        field has not been validated yet.

        """
        return self._coldict[key].last_validation_error

    def register_callback(self, kind, key, function):
        assert kind[:5] == 'CALL_' and hasattr(self, kind), kind
        assert function is None or callable(function), function
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
          permission -- one of 'pytis.data.Permission' constants determining the permission to be
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

    def codebook(self, key):
        """Return the name of given field's codebook specification for resolver."""
        return self._coldict[key].codebook

    def prefer_display(self, key):
        column = self._coldict[key]
        return column.prefer_display

    def display(self, key, export=None, single=True):
        """Return enumerator `display' value for given field as a string.

        Arguments:

          export -- function used to export inline_display field value.  If not
            defined, the default export method of inline_display data type is
            used; Relevant only for fields with 'inline_display' defined.
            Unused otherwise (when a display function is called).
          single -- always return a single string (bool).  If true, the method
            returns a single string also for Array types.  Otherwise array
            values are returned as a tuple of separately formatted display
            values.

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
        inline_display = column.inline_display
        if inline_display and inline_display in self._row and column.last_validated_string is None:
            # The row doesn't contain inline_display when it was created in
            # _set_row (not passed from the data interface) and inline_display
            # field is not explicitly present in fields.  The test of
            # column.last_validated_string is important during row changes,
            # where the current inline display value doesn't match the changed
            # field value.  But beware!  We can not use .field_changed() for
            # this purpose, because it would prevent using inline display in
            # browse form (which is tha main purpose of inline display) because
            # we cycle through rows using set_row() without reset=True.  To
            # make the story even longer, we don't want to call set_row() with
            # reset=True in BrowseForm, because it would invoke unnecessary
            # computers.
            value = self._row[inline_display]
            if value.value() is None:
                return column.null_display or ''
            else:
                if export:
                    return export(value)
                else:
                    return value.export()
        display = self._display(column, export=export)
        value = self[column.id].value()
        if value is None:
            return column.null_display or ''
        elif display:
            def check_result(f, *args, **kwargs):
                result = f(*args, **kwargs)
                assert isinstance(result, basestring), \
                    "Invalid result of display function for column '%s': %r" % (column.id, result)
                return result
            if isinstance(column.type, pytis.data.Array):
                result = [check_result(display, v.value()) for v in value]
                if single:
                    result = ', '.join(result)
            else:
                result = check_result(display, value)
            return result
        else:
            return ''

    def enumerate(self, key, export=None):
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
            if column.cbspec:
                sorting = column.cbspec.sorting()
            if sorting is None and column.codebook is not None:
                sorting = self._resolver.get(column.codebook, 'view_spec').sorting()
            value_column = enumerator.value_column()
            display = self._display_as_row_function(column, export=export)

            return [(row[value_column].value(), display(row))
                    for row in enumerator.rows(transaction=self._transaction,
                                               condition=self.runtime_filter(key),
                                               arguments=self.runtime_arguments(key),
                                               sort=sorting or ())]
        else:
            display = self._display(column)
            if display is None:
                def display(v):
                    return column.type.export(v)
            runtime_filter = self.runtime_filter(key)
            kwargs = (self.runtime_arguments(key) or {})
            if isinstance(enumerator, pytis.data.TransactionalEnumerator):
                kwargs['transaction'] = self._transaction
            return [(v, display(v)) for v in enumerator.values(**kwargs)
                    if runtime_filter is None or runtime_filter(v)]

    def runtime_filter(self, key):
        """Return the current run-time filter for an enumerator of field KEY.

        Returns a 'pytis.data.Operator' instance when a filter is active or
        None if the field has no enumerator or if the enumerator is not
        filtered.

        """
        return self._computed_value(self._coldict[key].runtime_filter)

    def runtime_arguments(self, key):
        """Return the current run-time arguments for a table function based codebook of field KEY.

        Returns an arguments dictionary (possibly empty), or 'None' if the
        field has no table function based codebook.

        """
        return self._computed_value(self._coldict[key].runtime_arguments)

    def has_completer(self, key, static=False):
        """Return true if given field has a completer.

        Arguments:
          key -- field identifier as a string
          static -- if true, true is returned only if the completer is defined
            by a static set of values (i.e. it is not bound to a data object).

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
          prefix -- prefix value as a (unicode) string or None.  If specified,
            the list of completions is filtered for values with given prefix.
            Prefix matching is case insensitive.

        The returned list contains available completions provided by the
        underlying completer of given field.  The completer is determined
        either by the 'completer' argument in field specification or (if not
        defined) the enumerator of the field's data type.  If the field is not
        associated with any completer, the method always returns an empty list.
        The method 'has_completer()' may be used to find out, whether the field
        has a completer.

        """
        column = self._coldict[key]
        completer = self._completer(column)
        if completer is not None:
            if prefix:
                prefix = prefix.lower()
            if isinstance(completer, pytis.data.DataEnumerator):
                condition = self.runtime_filter(key)
                if prefix:
                    wmvalue = pytis.data.WMValue(pytis.data.String(), prefix + '*')
                    prefix_condition = pytis.data.WM(completer.value_column(), wmvalue)
                    if condition:
                        condition = pytis.data.AND(condition, prefix_condition)
                    else:
                        condition = prefix_condition
                arguments = self.runtime_arguments(key)
                # TODO: completer.values() returns tuple, which is inconsistent with the rest.
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

        Dependencies are established through 'computer', 'visible', 'editabile', 'check',
        'runtime_filter' and 'runtime_arguments' specifications of 'Field'.

        """
        for cval in self._coldict[key].dependent:
            if cval.column.id in keys:
                return True
        return False

    def protected(self):
        """Return protected copy of the row.

        The protected copy makes raises 'PresentedRow.ProtectionError' on some
        operations on columns without view permission.

        """
        row = copy.copy(self)
        row._protected = True
        return row

    def attachment_storage(self, key):
        """Return the 'AttachmentStorage' instance for given field or None.

        The result depends on the 'attachment_storage' specification of the
        field given by 'key'.  If the attachment storage is defined as a
        callable object, it is automatically called and the result is returned.

        """
        column = self._coldict[key]
        storage = column.attachment_storage
        if callable(storage):
            storage = storage(self)
        return storage

    def filename(self, key):
        """Return the file name for given field or None.

        Returns a string denoting the file name for saving the field value or
        None is the field value is not to be saved as a file.

        The result depends on the 'filename' specification of the field given
        by 'key'.  If the filename is defined as a callable object, it is
        automatically called and the result is returned.  If defined as a field
        id, the exported value of given field is returned.

        """
        column = self._coldict[key]
        filename_spec = column.filename
        if filename_spec:
            if callable(filename_spec):
                return filename_spec(self)
            else:
                return self[filename_spec].export()
        else:
            return None
