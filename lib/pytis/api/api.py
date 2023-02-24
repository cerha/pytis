# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020, 2022, 2023 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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

"""Pytis Application API definition."""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

import inspect
import sys

import pytis.data as pd
import pytis.presentation
import pytis.util


def implements(api_class, incomplete=False):
    """Decorator for marking a class which implements a particular API.

    The argument is the API definition class.  Particular API definition
    classes are defined below.  The implementing class then must define all
    public methods and properties defined by the definition class.

    """
    def provider(self):
        if not hasattr(self, '_api_provider'):
            self._api_provider = APIProvider(self)
        return self._api_provider

    def wrapper(cls):
        signature = inspect.signature if hasattr(inspect, 'signature') else inspect.getargspec
        for name in dir(cls):
            if name.startswith('api_'):
                implementation = getattr(cls, name)
                name = name[4:]
                try:
                    definition = getattr(api_class, name)
                except AttributeError:
                    raise AttributeError("'{}' does not define public API member '{}'"
                                         .format(api_class, name))
                if callable(definition) and not callable(implementation):
                    raise TypeError("'{}.{}' is defined (but not implemented) as a method"
                                    .format(api_class.__name__, name))
                elif callable(definition) and signature(implementation) != signature(definition):
                    raise TypeError("Method signature does not match the definition of '{}.{}'"
                                    .format(api_class.__name__, name))
                elif isinstance(definition, property) and not isinstance(implementation, property):
                    raise TypeError("'{}.{}' is defined (but not implemented) as a property"
                                    .format(api_class.__name__, name))
        cls._api_attributes = [name for name in dir(api_class) if not name.startswith('_')]
        if not incomplete:
            for name in cls._api_attributes:
                if not hasattr(cls, 'api_' + name):
                    raise TypeError("{} does not implement '{}.{}'"
                                    .format(cls.__name__, api_class.__name__, name))
        cls.provider = provider
        return cls
    return wrapper


class APIProvider(object):
    """Public API provider proxying access to the public API of the wrapped instance.

    This class checks for correct API implementation of the wrapped instance
    and restricts access to its attributes to only those which belong to the
    public API.

    Marking a class by the 'pytis.api.implements()' decorator adds a new public
    method 'provider()' which returnes a (cached) 'APIProvider' instance for
    the implementation class instance on which it is called.

    """
    def __init__(self, instance=None):
        self._init(instance)

    def _init(self, instance):
        if not hasattr(instance, '_api_attributes'):
            raise TypeError("{} does not implement API (use pytis.api.implements decorator)"
                            .format(instance.__class__))
        self._instance = instance

    def __str__(self):
        return '<pytis.api.APIProvider for {}>'.format(self._instance)

    def __repr__(self):
        return str(self)

    def __getattr__(self, name):
        if name not in self._instance._api_attributes:
            raise AttributeError("'{}' object has no public API member '{}'"
                                 .format(self._instance.__class__.__name__, name))
        return getattr(self._instance, 'api_' + name)

    def __setattr__(self, name, value):
        if name != '_instance' and name in self._instance._api_attributes:
            return setattr(self._instance, 'api_' + name, value)
        else:
            return super(APIProvider, self).__setattr__(name, value)


class ApplicationAPIProvider(APIProvider):
    """Special case of API provider for the top level 'pytis.api.app' instance.

    We need an initially uninitialized instance in 'pytis.api.app' to allow
    'from pytis.api import app' in the top level of application modules.  This
    instance will be initialized as soon as the real Application API
    implementing instance is ready.

    Morover we want initialize a limited application in scripts where a real
    application does not actually run.  When 'app.param' is accessed when
    'init()' has not been called (yet), 'BaseApplication' instance is created
    automatically.

    """
    def __init__(self):
        self._instance = None

    def __getattr__(self, name):
        if name in ('param', 'has_access') and self._instance is None:
            BaseApplication()  # Will call app.init() automatically.
        return super(ApplicationAPIProvider, self).__getattr__(name)

    def init(self, instance):
        """Start providing the API implemented by given application instance."""
        self._init(instance)

    def release(self):
        """Stop providing the API implemented by the current application instance."""
        self._instance = None


class API:
    """Common base class for definition of public API methods and attributes.

    API definition classes are derived from this base class.  Classes
    implementing a particular API are decorated using 'implements()'.

    """
    pass


class Form(API):
    """Public API representation of the current form."""

    name = property()
    """The specification name as a string.

    Returns None if the form is not bound to any specification (web form).

    """

    field = property()
    """Access to input fields through the attributes of this object.

    Has one attribute named by field id for each field present in the form.
    Each field attribute represents a 'pytis.api.Field' API.  Is None in forms
    which have no input fields (such as browse forms).

    May also be called passing the field id as a string.  Thus
    'form.field.field_id' or 'form.field("field_id")' should both
    give the same result.

    """

    condition = property()
    """Current filtering condition as a 'pytis.data.Operator' instance or None."""

    arguments = property()
    """Current arguemnts as a dictionary of 'pytis.data.Value' instances or None."""

    sorting = property()
    """Current sorting as in 'pytis.data.Data.select()' or None."""

    profile = property()
    """Current form profile as 'pytis.presentation.Profile' instance or None."""

    query_fields = property()
    """The form's query fields panel API as 'pytis.api.QueryFields' instance.

    Returns None if the form has no query fields panel.

    """
    row = property()
    """The current form row as 'pytis.presentation.PresentedRow' instance.

    The current row is the currently active (focused) row of the form.  It
    represents the currently displayed record or the focused row in a multirow
    (tabular) form.  Returns None if the form currently has no active row.

    """
    selection = property()
    """Iterator over all currently selected rows.

    The iterator returns all rows present in the current selection as
    'PresentedRow' instances in the order of their presence in the form.

    """
    main_form = property()
    """The main form of a dual form.

    Returns None if the form is not dual form.

    """
    side_form = property()
    """The current side form of a dual form.

    Returns None if the form is not dual form.

    """

    def refresh(self):
        """Refresh the form UI, typically reload data from DB if applicable."""
        pass

    def clear_selection(self):
        """Unselect all rows that are currently selected (if any)."""
        pass


class StatusField(API):
    """Public API representation of a status bar field.

    Status bar fields are defined by 'pytis.presentation.StatusField' instances
    returned by 'pytis.presentation.Application.status_fields()'.

    """

    def update(self, text=None, icon=None, tooltip=None):
        """Update the text and/or icon displayed in the field.

        Same as setting the corresponding field propertiess separately.

        """
        pass

    def refresh(self):
        """Call the field's refresh function manually.

        Makes extra call to the field's refresh function outside its regular
        refresh interval (defined by field specification).  Mostly useful for
        fields with refresh interval set to zero (no periodic refresh).

        """
        pass

    text = property()
    """Get/set the current status field text as a string."""

    icon = property()
    """Get/set the current status field icon as a string or None.

    The icon is determined by a string identifier accepted by
    'pytis.form.get_icon()'.  The position of the icon is determined by the
    field specification ('icon_position' passed to 'StatusField' constructor).

    """

    tooltip = property()
    """Get/set the current status field tooltip as a string or None.

    May also be a function with no arguments returning a string.  In this case
    the function is called when the tooltip is really needed at the moment when
    the user hovers above the field.  Note that the function is called only
    once and its result is cached until the tooltip is set next time.  Note
    that field label (defined by field specification) is displeyed in the
    tooltip by default as a heading above the actual tooltip content.

    """


class Field(API):
    """Public API representation of a form input field."""

    def refresh(self):
        """Refresh field UI, typically reload enumeration if applicable."""
        pass

    def write(self, text):
        """Insert given text into the field in the current cursor position."""
        pass

    def on_list_change(self, callback):
        """Add callback on change of the list of available values in a LIST field.

        Raises Exception when called on a field which is not
        selection_type=LIST.

        """
        pass


class QueryFields(API):
    """Public API representation of the form's query fields panel."""

    row = property()
    """The query field values as 'pytis.presentation.PresentedRow' instance."""


class Application(API):
    """Public API representation of the currently running application.

    Accessed through 'pytis.api.app'.

    """
    title = property()
    """Get/set the main application frame title as a string."""

    param = property()
    """Access to shared parameters.

    Shared parameters provide a simple way to share values between the database
    and the application code.  Each 'pytis.presentation.SharedParams' instance
    defined in 'Application.params()' leads to creation of one attribute
    'app.param.<name>', where <name> corresponds to the name defined by the
    'SharedParams' instance.  Shared parameter values can be accessed as
    'app.param.<name>.<param>', where <param> corresponds to the name of the
    column present in the data object defined by the SharedParams instance.
    When read, the attributes return the internal Python value of the column.
    When assigned, they update the value in the database.

    """

    form = property()
    """The current form API as 'pytis.api.Form' instance."""

    status = property()
    """Access to status bar fields.

    The returned object has one attribute named by field id for each field
    present in the status bar.  If the field id contains a hyphen, it is
    replaced by an underscore in the attribute name.  Each field implements the
    'pytis.api.StatusField' API.

    May also be called passing the field id as a string.  Thus
    'app.status.field_id' or 'app.status("field_id")' should both give the same
    result.

    """

    def echo(self, message, kind='info'):
        """Display a non-interactive message to the user.

        Arguments:

          message -- the text to be displayed.
          kind -- message kind as a string.  One of 'info', 'warning', 'error'.
            The message kind may be indicated in the UI using colors, icons or
            sounds (beeping for 'warning' and 'error' etc.).

        """
        pass

    def message(self, message=None, title=None, content=None):
        """Display given message in an interactive dialog.

        The user needs to confirm the dialog before continuing.

        Arguments:

          message -- the text to be displayed.
          title -- dialog window title as a string.
          content -- additional content to be displayed under the message in a
            possibly scrollable view.  May be passed as 'lcg.Content' instance
            which is exported and displayed in an HTML component or a string
            which is displayed as plain text.

        """
        pass

    def warning(self, message=None, title=None, content=None):
        """Display warning in an interactive dialog.

        The user needs to confirm the dialog before continuing.

        Arguments:

          message -- the warning text to be displayed.
          title -- dialog window title as a string.
          content -- additional content to be displayed under the message in a
            possibly scrollable view.  May be passed as 'lcg.Content' instance
            which is exported and displayed in an HTML component or a string
            which is displayed as plain text.

        """
        pass

    def error(self, message=None, title=None, content=None):
        """Display given error message in an interactive dialog.

        The user needs to confirm the dialog before continuing.

        Arguments:

          message -- the error text to be displayed.
          title -- dialog window title as a string.
          content -- additional content to be displayed under the message in a
            possibly scrollable view.  May be passed as 'lcg.Content' instance
            which is exported and displayed in an HTML component or a string
            which is displayed as plain text.

        """
        pass

    def question(self, message, answers=None, default=None, title=None, content=None,
                 timeout=None):
        """Display given question in an interactive dialog.

        The user needs to answer by pressing one of the available buttons.
        Default buttons are 'Yes' and 'No' and the method returns True when
        'Yes' was selected, False when 'No' was pressed and None when canceled.
        Custom answers can be passed using the 'answers' argument.

        Arguments:

          message -- the question to be displayed.
          answers -- available answers.  When None, the default answers are
            'Yes' and 'No' and True/False is returned on pressing Yes/No.
            Custom answers may be specified as a sequence of button label
            strings.  Pressing one of the buttons results in returning given
            label.
          default -- the default selected answer (pressing Enter submits it).
            If no answers are given (the answers are Yes/No), the value is a
            boolean - True for 'Yes', False for 'No'.  Otherwise the value must
            be a string matching one of the given answers.
          title -- dialog window title as a string.
          content -- additional content to be displayed under the message in a
            possibly scrollable view.  May be passed as 'lcg.Content' instance
            which is exported and displayed in an HTML component or a string
            which is displayed as plain text.
          timeout -- dialog timeout in seconds; integer.  If not None, the
            dialog is automatically closed after given timeout returning 'None'
            as the answer.

        """
        pass

    def delete_record_question(self, message=None):
        """Display 'question()' dialog asking for record deletion.

        Returns True if the user confirms deletion or False otherwise.

        """
        pass

    def input_text(self, title, label, default=None, not_null=False, width=20, height=1,
                   descr=None, noselect=False):
        """Display a form for entering a single textual value and return this value.

        Arguments:

          title -- input form main title as a string.
          label -- field label as a string.
          default -- initial field value as a string.
          not_null -- iff True, it will not be possible to submit the form without
            entering a value.
          width -- input field width (number of characters).
          height -- input field height (number of characters).
          descr -- field description displayed in a tooltip of a blue icon right
            from the field.
          noselect -- the initial input field value is by default initially
            selected, which results in overwriting the whole value when the
            user starts typing.  Passing True here avoids this initial
            selection.

        Returns the value entered into the field as a string or None if the
        form was escaped or the value was empty (only possible when not_null is
        False).

        """
        pass

    def input_date(self, title, label, default=None, not_null=True, descr=None, noselect=False):
        """Display a form for entering a date and return this value.

        Arguments:

          title -- input form main title as a string.
          label -- field label as a string.
          default -- initial field value as 'datetime.date' or None.
          not_null -- iff True, it will not be possible to submit the form without
            entering a value.
          descr -- field description displayed in a tooltip of a blue icon right
            from the field.
          noselect -- the initial input field value is by default initially
            selected, which results in overwriting the whole value when the
            user starts typing.  Passing True here avoids this initial
            selection.

        Returns the value entered into the field as a 'datetime.date' instance or
        None if the form was escaped or the value was empty (only possible when
        not_null is False).

        """
        pass

    def input_number(self, title, label, default=None, not_null=True, width=14, precision=None,
                     minimum=None, maximum=None, descr=None, noselect=False):
        """Display a form for entering a single numeric value and return this value.

        Arguments:

          title -- input form main title as a string.
          label -- field label as a string.
          default -- initial field value as int or float (float when
            precision is given).
          not_null -- iff True, it will not be possible to submit the form without
            entering a value.
          width -- total input field width (number of characters).
          precision -- number of digits after decimal point or None for an integer field.
          minimum -- minimal value; 'None' denotes no limit.
          maximum -- maximal value; 'None' denotes no limit.
          descr -- field description displayed in a tooltip of a blue icon right
            from the field.
          noselect -- the initial input field value is by default initially
            selected, which results in overwriting the whole value when the
            user starts typing.  Passing True here avoids this initial
            selection.

        Returns the value entered into the field as int or float (float when
        precision was given) or None if the form was escaped or the value was empty
        (only possible when not_null is False).

        """
        pass

    def input_form(self, title, fields, prefill=None, layout=None, check=None, noselect=False):
        """Display modal form to collect user input from user defined fields.

        Arguments:

          title -- window title as a string.
          fields -- sequence of form field specifications as
            'pytis.presentation.Field' instances.
          prefill -- initial field values as a dictionary or None.  The
            dictionary keys are field identifiers and values are either the
            corresponding internal Python values valid for the fields's data
            type or 'pytis.data.Value' instances directly.
          layout -- form layout as in 'pytis.presentation.ViewSpec'
            constructor.
          check -- form check function as in 'pytis.presentation.ViewSpec'
            constructor.
          noselect -- the initial input field values are by default initially
            selected when the field is entered, which results in overwriting
            the whole value when the user starts typing.  Passing True here
            avoids this initial selection.

        Returns a 'pp.PresentedRow' instance containing field values or None if
        the user cancels the form.

        """
        pass

    def new_record(self, name, prefill=None, inserted_data=None, multi_insert=True,
                   copied_row=None, set_values=None, block_on_new_record=False,
                   # spec_kwargs is here temporarily just for backwards compatibility...
                   spec_kwargs={}, transaction=None):
        """Insert a new record using a modal form.

        Arguments:

          name -- specification name for resolver.
          prefill -- a dictionary of values to be prefilled in the form.  Keys
            are field identifiers and values are either 'pytis.data.Value'
            instances or the corresponding Python internal values directly.
          inserted_data -- allows to pass a sequence of 'pytis.data.Row'
            instances to be inserted.  The form is then gradually prefilled by
            values of these rows and the user can individually accept or skip
            each row.
          multi_insert -- boolean flag indicating whether inserting multiple
            values is permitted.  False value will disable this feature and the
            `Next' button will not be present on the form.
          copied_row -- row to copy as a 'pytis.data.Row' instance.  If not
            None, the new row will be a copy of given row.  Field values are
            copied except for fields with 'nocopy' set to True.  Also
            'on_copy_record' is used insted of 'on_new_record' if defined.
          set_values -- dictionary of row values to change in the openened form
            (or None).  The dictionary keys are field identifiers and values
            are either the corresponding internal Python values valid for the
            fields's data type or 'pytis.data.Value' instances directly.  These
            values will not affect the initial row state and thus will appear
            as changed to the user.
          block_on_new_record -- if true, the 'on_new_record' procedure from
            specification  will be blocked.  This makes it possible to call
            'new_record' from within the 'on_new_record' procedure without
            recursion..
          transaction -- transaction for DB operations.

        Runs 'on_new_record' instead of the default insertion form if the
        specification defines it.

        """
        pass

    def edit_record(self, name, row, set_values=None, block_on_edit_record=False,
                    transaction=None):
        """Edit an existing record in a modal form.

        Arguments:

          name -- specification name as a string.
          row -- edited record as a 'pytis.data.Row' or
            'pytis.presentation.PresentedRow' instance or record key as a
            'pytis.data.Value' instance.
          set_values -- dictionary of row values to change in the openened form
            (or None).  The dictionary keys are field identifiers and values
            are either the corresponding internal Python values valid for the
            fields's data type or 'pytis.data.Value' instances directly.  These
            values will not affect the initial row state and thus will appear
            as changed to the user.
          block_on_edit_record -- if true, the 'on_edit_record' procedure from
            specification  will be blocked.  This makes it possible to call
            'edit_record' from within the 'on_new_record' procedure without
            recursion..
          transaction -- transaction for DB operations.

        Runs 'on_edit_record' instead of the default edit form if the
        specification defines it.

        Returns a 'pp.PresentedRow' instance of the updated record or None if
        the user cancels the form.  If 'on_edit_record' is defined, returns
        whatever 'on_edit_record()' returned.

        """
        pass

    def delete_record(self, name, row, question=None, transaction=None):
        """Delete an existing record after user's confirmation.

        Arguments:

          name -- specification name as a string.
          row -- deleted record as a 'pytis.data.Row' or
            'pytis.presentation.PresentedRow' instance or record key as a
            'pytis.data.Value' instance.
          question -- user defined deletion confirmation question or None to
            used the default (generic) question "Are you sure to delete the
            record permanently?".
          transaction -- transaction for DB operations.

        Runs 'on_delete_record' instead of the default deletion if the
        specification defines it.

        Returns True if the record was actually deleted and False if not
        (the user didn't confirm deletion).

        """
        pass

    def run_form(self, name, select_row=None, multi=True, sorting=None, filter=None,
                 condition=None, profile=None, binding=None):
        """Display given form in the main application frame.

        Arguments:

          name -- specification name as a string.
          select_row -- the row to be intially selected within the opened form.
            It may be a 'pytis.data.Value' instance (the row key), their tuple
            (a multi-column key), a non-negative integer (the ordinal position
            of the row to be selected starting from zero), a dictionary of
            'pytis.data.Value' instances keyed by column ids (to select the
            first row with matching values), 'pytis.data.Row' instance (its key
            columns will be used to get the row key) or None to perform no
            initial row selection.
          multi -- iff True, prefer multiform over plain browse form.  This
            means that a multiform (incuding side forms) will be used when the
            specification defines 'bindings'.  If there are no bindings or
            'multi' is set to False, just a simple form without side forms will
            be opened.
          sorting -- specification of initial form sorting in the same format
            as the argument 'sorting' of the 'Profile' constructor.  If not
            None, overrides the sorting of the default form profile.
          filter -- initial filter condition as a 'pytis.data.Operator'
            instance.  This filter is indicated to the user and can be modified
            as any other user-defined filter (as opposed to 'condition').  If
            not None, overrides the filter of the default form profile.
          condition -- 'pytis.data.Operator' instance filtering the rows of the
            underlying data object.  This condition is not indicated to the user
            and it is not possible to turn it of from the UI.
          profile -- id of the initial profile to be loaded.  If not None, it
            must be one of the available system profiles (defined in
            specification) and the arguments 'filter' and 'sorting' must be
            None (they are determined by the profile).
          binding -- id of the side form binding to be initially selected.

        The form type is selected automatically.  If name contains '::' (the
        legacy dual form separator), the form is a dual form.  If the form
        specification defines 'bindings', the form is a multiform with side
        form tabs.  Otherwise the form is a plain browse form (simple table).

        """
        pass

    def codebook(self, name, select_row=0, columns=None, sorting=None, filter=None,
                 condition=None, multirow=False, begin_search=None, transaction=None):
        """Display a modal codebook selection form and return the selected row.

        Arguments:

          name -- specification name as a string.
          select_row -- the row to be intially selected within the opened form.
            It may be a 'pytis.data.Value' instance (the row key), their tuple
            (a multi-column key), a non-negative integer (the ordinal position
            of the row to be selected starting from zero), a dictionary of
            'pytis.data.Value' instances keyed by column ids (to select the
            first row with matching values), 'pytis.data.Row' instance (its key
            columns will be used to get the row key) or None to perform no
            initial row selection.
          columns -- sequence of column identifiels to override the 'columns'
            defined by specification.
          sorting -- specification of initial form sorting in the same format
            as the argument 'sorting' of the 'Profile' constructor.  If not
            None, overrides the sorting of the default form profile.
          filter -- initial filter condition as a 'pytis.data.Operator'
            instance.  This filter is indicated to the user and can be modified
            as any other user-defined filter (as opposed to 'condition').  If
            not None, overrides the filter of the default form profile.
          condition -- 'pytis.data.Operator' instance filtering the rows of the
            underlying data object.  This condition is not indicated to the user
            and it is not possible to turn it of from the UI.
          multirow -- allow selection of multiple rows.
          begin_search -- id of the column on which to start incremental seach.
          transaction -- transaction for DB operations.

        Returns the selected row as 'pytis.data.Row' instance or a tuple of
        such rows when 'multirow' is True.  Returns None when the user cancels
        the form.

        """

    def run_procedure(self, spec_name, proc_name, *args, **kwargs):
        """Run application defined procedure.

        Arguments:

          spec_name -- specification name as a string.
          proc_name -- name of the procedure within the specification.

        All other arguments, including keyword arguments, are passed on to the
        invoked procedure, except for the keyword argument 'block_refresh'.
        When 'block_refresh' is passed and is True, automatic refreshing of all
        currently open forms will be blocked until the procedure returns.

        Returns the value returned by the procedure.

        """
        pass

    def run(self, function, args=(), kwargs={}, over=None, title=None, message=None,
            progress=True, maximum=None, elapsed_time=False, estimated_time=False,
            remaining_time=False, can_abort=False, new_thread=False):
        """Execute a long running operation showing a progress dialog.

        The dialog is displayed until the operation is finished and may inform
        the user about the progress of the operation visually (by a progress
        bar) and textually (by a progress message).

        Arguments:

          function -- the Python function to be executed.
          args -- tuple of arguments to be passed to 'function'.
          kwargs -- dictionary of keyword arguments to be passed to 'function'.
          over -- sequence of arguments to call 'function' repeatedly with each
            of them.  If not None, the function will be called with each item
            as an argument preceeding 'args'.  Additionally, if 'function'
            accepts an argument named 'n', the ordinal number of the item
            within the sequence (starting from zero) will be passed as
            additional keyword argument 'n'.  The progress will be updated
            automatically to reflect the curent position within the sequence.
            You can still call 'update' to update the progress message, but you
            should avoid passing it 'progress'.  When passing an iterable
            object with unknown lenght (such as a generator), you need to pass
            its expected length as the 'maximum' arument.
          title -- dialog window title as a string.
          message -- the initial progress message to be displayed within the
            dialog.  The message can be further updated to indicate the
            operation progress (see "Progress updates" below).
          progress -- if false, progress updates are not supported.  All
            remaining arguments are ignored, the dialog just statically
            displays the 'message' and waits until the operation is finished.
          maximum -- value determining the range in which the progress is
            updated (see Progress updates below).  The default value is 100 if
            'over' is None.
          elapsed_time -- if true, the dialog will display the time elapsed
            from the start.
          estimated_time -- if true, the dialog will display the estimated
            total execution time.
          remaining_time -- if true, the dialog will display the estimated
            remaining time to the end.
          can_abort -- if true, the executed function may be aborted if it is
            written properly (see Progress updates below).
          new_thread -- experimantal support for invoking function in a
            separate thread.

        Progress updates

        The progress updates must be directly supported by the called function
        (if the function doesn't support progress updates, pass false to
        'progress' and the remaining rules will not apply).  The function must
        (in addition to 'args' and 'kwargs') accept the first positional
        argument 'update' and call it as a function periodically to update the
        progress.  The 'update' function accepts two keyword arguments (both
        optional) 'progress' and 'message'.  The argument 'progress' can also
        be passed as the first positional argument.  If not None, it is a
        number between zero and the 'maximum' passed to 'app.run()' (see
        above).  It will move the visual progress indicator proportionally to
        given position between zero and the maximum.  The argument 'message'
        (if not None) updates the progress message displayed within the dialog.
        If 'can_abort' was true, the caller should also check the return value
        of the 'update' function and abort the operation if the return value is
        false.

        Returns the value returned by 'function' when finished.

        """
        pass

    def launch_file(self, path=None, data=None, suffix=None, decrypt=False):
        """Launch a viewer for given local file or data.

        Arguments:

          path -- full path to the file in the local (server side) filesystem.
            If not None, 'data' and 'suffix' must be None.
          data -- the file data as 'bytes' or a file like object.  This is the
            contents of the file to be viewed.  If not None, 'path' must be
            None.
          suffix -- the filename suffix including the leading dot.  Only
            relevant if 'data' is not None.
          decrypt -- if True then decrypt 'data' before saving on remote
            client's machine.

        The viewer will be launched on the client machine (remotely) if remote
        client connection exists.  The file will be temporarily copied to the
        client machine in this case and cleaned up automatically afterwards.

        The client's side file associations will take effect in case of remote
        invocation.  If the viewer is run locally (on the application server),
        the viewer is determined through Mailcap.  For PDF files, the viewer
        set through the configuration option 'postscript_viewer' (if set) takes
        precedence.

        This method is non-blocking.  It returns immediately and the viewer is
        run in the background in all cases.

        """
        pass

    def launch_url(self, url):
        """Open given URL in a web browser.

        The browser will be launched on the client machine (remotely) if remote
        client connection exists.

        This function is non-blocking.  It returns immediately and the browser is
        run in the background in all cases.

        """
        pass

    def splitpath(self, path):
        """Split the path obtained from 'Application.select_file()' and similar methods.

        'Application.select_file()' returns a path name, but the caller doesn't
        know whether it comes from a local filesystem or a remote (client)
        machine.  The same applies for 'f.name' of a file object returned by
        'Application.open_selected_file()'.  It is not clear which path
        separator applies for such paths.  This method should be used to split
        such paths correctly.

        Returns a pair of strings (dirname, filename).

        Arguments:

          path -- path to a local or remote file returned by 'select_file()' or
            similar method.

        """
        pass

    def select_file(self, filename=None, filetypes=None, directory=None, context='default'):
        """Return a filename selected by the user in a GUI dialog.

        Returns None if the user cancels the dialog.  If remote client connection
        exists, the returned filename belongs to the client's filesystem.

        Arguments:

          filename -- initial (default) filename or None
          filetypes -- sequence of allowed filename extensions as case insensitive
            strings.  If present (not None), only files of given types will be
            available for selection.  The exact behavior depends on the Client OS.
            Non-matching files are grayed out on Mac OS and completely hidden on
            Linux and Windows.
          directory -- the initial directory or None
          context -- selector of the memory for keeping track of the most recently
            used directory.  If not None, the initial dialog directory will be
            loaded from given memory and the memory will be updated by the path of
            the selected file when the dialog is closed.  The selector is an
            arbitrary string - for each unique string the most recently used
            directory is stored separately.

        """
        pass

    def select_files(self, directory=None, filetypes=None, context='default'):
        """Return a tuple of filenames selected by the user in a GUI dialog.

        Returns empty tuple if the user cancels the dialog.  If remote client
        connection exists, the returned filenames belong to the client's
        filesystem.

        Arguments:

          directory -- the initial directory or None
          filetypes -- see 'Application.select_file()'
          context -- see 'Application.select_file()' - unused if 'directory' not None.

        """
        pass

    def select_directory(self, directory=None, context='default'):
        """Return a directory selected by the user in a GUI dialog.

        Arguments:

          directory -- the initial directory or None
          context -- see 'Application.select_file()'

        Returns None if the user cancels the dialog.  If remote client connection
        exists, the returned directory belongs to the client's filesystem.

        """
        pass

    def make_selected_file(self, filename, mode='w', encoding=None, filetypes=None,
                           directory=None, context='default'):
        """Return a write-only 'file' like object of a user selected file.

        The file is selected by the user using a GUI dialog.  Returns None if
        the user cancels the dialog.  If remote client connection exists, the
        returned file is created in the client's filesystem (the returned
        object is an 'ExposedFileWrapper' instance).

        Arguments:

          filename -- default filename or None
          mode -- default mode for opening the file
          encoding -- output encoding, string or None
          filetypes -- see 'Application.select_file()'
          directory -- the initial directory or None
          context -- see 'Application.select_file()'

        """
        pass

    def write_selected_file(self, data, filename, mode='w', encoding=None, filetypes=None,
                            context='default'):
        """Write 'data' to a file selected by the user using a GUI dialog.

        The file is selected by the user using a GUI dialog.  Returns 'True' if the
        file was created and written succesfully or 'False' if the user cancels the
        dialog.  If remote client connection exists, the file is created in the
        client's filesystem.

        Arguments:

          data -- the file data as a string or bytes
          filename -- default filename or None
          mode -- default mode for opening the file
          encoding -- output encoding, string or None
          filetypes -- see 'Application.select_file()'
          context -- see 'Application.select_file()'

        """
        pass

    def open_selected_file(self, directory=None, filetypes=None, encrypt=None, context='default'):
        """Return a read-only 'file' like object of a user selected file.

        The file is selected by the user using a GUI dialog.  Returns None if
        the user cancels the dialog.  If remote client connection exists, the
        returned file is created in the client's filesystem (the returned
        object is an 'ExposedFileWrapper' instance).

        The file is always open in binary mode. (TODO: add 'mode' argument)

        Arguments:

          directory -- the initial directory or None
          filetypes -- see 'Application.select_file()'
          encrypt -- list of encryption keys to use to encrypt the file; if the
            list is empty then let the user select the keys; if 'None' then
            don't encrypt the file
          context -- see 'Application.select_file()'

        """
        pass

    def open_file(self, filename, mode='w'):
        """Return a read-only 'file' like object of the given file.

        Arguments:

          filename -- name of the file to open, basestring
          mode -- mode for opening the file

        """
        pass

    def write_file(self, data, filename, mode='w'):
        """Write given 'data' to given file.

        Arguments:

          data -- the (possibly binary) data as a basestring
          filename -- name of the file to write to, basestring
          mode -- mode for opening the file

        """
        pass

    def has_access(self, name, perm=pd.Permission.VIEW, column=None):
        """Return true if the current user has given permission for given form specification.

        Arguments:

          name -- specification name as a string.  May also be a dual name
            (containing '::').  In such a case, the permission is checked for both
            names and 'column=None' is assumed regardless of the actual 'column'
            value.
          perm -- access permission as one of 'pytis.data.Permission' constants.
          column -- string identifier of the column to check or 'None' (no specific
            column checked)

        Raises 'pytis.util.ResolverError' if given specification name cannot be found.

        """
        pass

    def decrypted_areas(self):
        """Return set of names of encryption areas the user has access to."""
        pass

    def printout(self, spec_name, template_id, row=None,
                 parameters=None, output_file=None, language=None, form=None):
        """Print given template to PDF and display the result in a viewer.

        Arguments:
          spec_name -- name of the specification for print resolver
          template_id -- id of the output template, string
          row -- current row data for print resolver as 'pytis.data.Row' instance or None
          parameters -- dictionary of extra user-defined parameters passed
            to the print specification (available in the print specification
            through self._parameter(key))
          output_file -- file to write output PDF data to, open file-like object; if
            'None' then show the output in an external PDF viewer
          language -- language code to pass to the exporter context

          form -- current form; 'Form' instance or None

        """
        pass

    def refresh(self):
        """Refresh visible application components."""
        pass

    def exit(self):
        """Exit the application."""
        pass


@implements(Application, incomplete=True)
class BaseApplication(object):
    """Base class for classes implementing the 'Application' API.

    This class only implements access to shared parameters using 'app.param'
    and access checking using 'app.has_access()'.

    It may be used as a base class for other classes implementing the
    'Application' API or directly in scripts which don't need anything else
    than 'app.param' and 'app.has_access()'.  Without them specifications
    simply can't be instantiated and loaded through the resolver because
    'app.param' and 'app.has_access()' are often used in specification
    construction methods.

    """
    class _Param:  # Access items as attributes.
        def __init__(self, items):
            self.__dict__ = dict(items)

    def __init__(self):
        import pytis
        import pytis.api
        self._specification = pytis.config.resolver.specification('Application')
        # Create DBParams instances for all SharedParams specifications.
        self._param = self._Param(
            (item.name(), pytis.util.DBParams(item.spec_name(), item.condition()))
            for item in self._specification.params()
        )
        pytis.api.app.init(self)
        self._access_rights = None
        self._access_rights_initialized = False
        self._user_roles = ()
        super(BaseApplication, self).__init__()

    def _init_access_rights(self):
        """Read application access rights from the database."""
        # Must be called very early after start of an application.
        self._access_rights_initialized = True
        if not pytis.config.use_dmp_roles:
            return
        try:
            roles_data = pd.dbtable('ev_pytis_user_roles', ('roleid',), pytis.config.dbconnection)
            roles = [row[0].value() for row in roles_data.select_map(pytis.util.identity)]
        except pd.DBException:
            return
        if not roles:
            self._access_rights = 'nonuser'
            return
        self._user_roles = roles
        if not pytis.config.use_dmp_rights:
            return
        self._access_rights = {}
        # Prefill self._access_rights so that default access by specification rights in
        # has_action_access is possible only for shortnames without any rights
        # defined in DMP.
        actions_data = pd.dbtable('e_pytis_action_rights', ('shortname', 'status',),
                                  pytis.config.dbconnection)
        condition = pd.LE('status', pd.ival(0))
        for value in actions_data.distinct('shortname', condition=condition):
            self._access_rights[value.value()] = {}
        rights_data = pd.dbtable('pytis_view_user_rights', (('shortname', pd.String()),
                                                            ('rights', pd.String()),
                                                            ('columns', pd.String())),
                                 pytis.config.dbconnection, arguments=())

        def process(row):
            shortname, rights_string, columns_string = [row[i].value() for i in (0, 1, 2)]
            if columns_string:
                columns = columns_string.split(' ')
            else:
                columns = [None]
            rights = [r.upper() for r in rights_string.split(' ') if r != 'show']
            action_rights = self._access_rights[shortname] = self._access_rights.get(shortname, {})
            relaxed_action_rights = action_rights.get(True)
            if relaxed_action_rights is None:
                # Relaxed access rights are access rights to the action as a whole.
                # The action is accessible if it is accessible itself or if any of
                # its columns is accessible.
                action_rights[True] = relaxed_action_rights = []
            for c in columns:
                action_rights[c] = rights
                for r in rights:
                    if r not in relaxed_action_rights:
                        relaxed_action_rights.append(r)
        rights_data.select_map(process)
        pytis.presentation.Specification._init_access_rights(pytis.config.dbconnection)
        pytis.config.resolver.clear()
        if pytis.config.debug:
            self._dump_rights()

    def _dump_rights(self):
        import pytis.extensions
        registered_shortnames = set()
        if self._access_rights not in (None, 'nonuser',):
            registered_shortnames = registered_shortnames.union(self._access_rights.keys())
        if pytis.presentation.Specification._access_rights not in (None, 'nonuser'):
            registered_shortnames = registered_shortnames.union(
                pytis.presentation.Specification._access_rights.keys()
            )
        resolver = pytis.config.resolver
        output = sys.stderr
        output.write("--- BEGIN list of registered rights ---\n")
        output.write("# source shortname right column permitted\n")

        def find_columns(spec_name):
            try:
                specification = resolver.specification(spec_name)
            except pytis.util.ResolverError:
                specification = None
            if specification is None:
                columns = []
            else:
                columns = [f.id() for f in specification.view_spec().fields()]
            return columns
        all_permissions = pd.Permission.all_permissions()
        for shortname in registered_shortnames:
            if shortname.startswith('form/'):
                columns = find_columns(shortname[5:])
            else:
                columns = []
            for permission in all_permissions:
                permitted = self.action_has_access(shortname, permission)
                output.write('actions %s %s None %s\n' % (shortname, permission, permitted,))
                for c in columns:
                    permitted = self.action_has_access(shortname, permission, c)
                    output.write('actions %s %s %s %s\n' % (shortname, permission, c, permitted,))
        specification_names = pytis.extensions.get_form_defs()
        for spec_name in specification_names:
            columns = find_columns(spec_name)
            for permission in all_permissions:
                permitted = self.api_has_access(spec_name, permission)
                output.write('specifications %s %s None %s\n' % (spec_name, permission, permitted))
                for c in columns:
                    permitted = self.api_has_access(spec_name, permission, c)
                    output.write('specifications %s %s %s %s\n' %
                                 (spec_name, permission, c, permitted,))
        output.write("--- END list of registered rights ---\n")

    @property
    def api_param(self):
        return self._param

    def api_has_access(self, name, perm=pd.Permission.VIEW, column=None):
        if not self.action_has_access('form/' + name, perm=perm, column=column):
            return False
        try:
            main, side = name.split('::')
        except ValueError:
            dual = False
        else:
            dual = True
        if dual:
            return self.api_has_access(main, perm=perm) and self.api_has_access(side, perm=perm)
        else:
            try:
                rights = pytis.config.resolver.get(name, 'data_spec').access_rights()
            except pytis.util.ResolverError:
                rights = None
            if rights:
                if not self._access_rights_initialized:
                    self._init_access_rights()
                groups = pd.default_access_groups(pytis.config.dbconnection)
                if not rights.permitted(perm, groups, column=column):
                    return False
        return self.action_has_access('form/' + name, perm=perm, column=column)

        return True

    def action_has_access(self, action, perm=pd.Permission.CALL, column=None):
        """Return true iff 'action' has 'perm' permission.

        Arguments:

          action -- action identifier, string
          perm -- access permission as one of 'pytis.data.Permission' constants
          column -- string identifier of the column to check or 'None' (no specific
            column checked)

        """
        if self._access_rights == 'nonuser':
            return False
        if self._access_rights is None:
            result = True
        else:
            rights = self._access_rights.get(action)
            if rights is None:
                # No action rights defined => only system rights apply
                # (this function is *action* rights check).
                result = True
                access_rights = pytis.presentation.Specification.data_access_rights(action)
                if access_rights is not None:
                    result = access_rights.permitted(perm, self._user_roles, column=column)
            else:
                if column is None:
                    permissions = rights.get(True, ())
                else:
                    permissions = rights.get(column, None)
                    if permissions is None:
                        permissions = rights.get(None, ())
                result = perm in permissions
        return result


def test_api_definition():
    import pytest
    import pytis.api

    @implements(pytis.api.Form)
    class MyForm:

        @property
        def api_name(self):
            return 'form name'

        @property
        def api_field(self):
            return None

        @property
        def api_condition(self):
            return None

        @property
        def api_arguments(self):
            return {'a': 'A'}

        @property
        def api_sorting(self):
            return None

        @property
        def api_query_fields(self):
            return 'the query fields'

        @property
        def api_row(self):
            return None

        @property
        def api_selection(self):
            return ()

        @property
        def api_main_form(self):
            return MyForm().provider()

        @property
        def api_side_form(self):
            return MyForm().provider()

        def api_clear_selection(self):
            return 'rows unselected'

        def api_refresh(self):
            pass

    @implements(pytis.api.Application, incomplete=True)
    class MyApp:

        non_api_attribute = 'non-API attribute'

        @property
        def api_param(self):
            return 'the param attribute'

        @property
        def api_form(self):
            return MyForm().provider()

        def api_echo(self, message, kind='info'):
            return ':{}: {}'.format(kind, message)

        def echo(self, message):
            return 'echo: {}'.format(message)

    app_instance = MyApp()
    app = app_instance.provider()

    assert app.param == 'the param attribute'
    assert isinstance(app.form, APIProvider)
    assert app.form.name == 'form name'
    assert app.form.arguments['a'] == 'A'
    assert app.form.query_fields == 'the query fields'
    assert app.echo('foo') == ':info: foo'

    app_instance.non_api_attribute == 'non-API attribute'
    with pytest.raises(AttributeError) as e:
        app.non_api_attribute
    assert str(e.value) == "'MyApp' object has no public API member 'non_api_attribute'"


def test_api_definition_errors():
    import pytest

    with pytest.raises(TypeError) as e:
        @implements(Form)
        class DefinePropertyAsMethod:
            def api_query_fields(self):
                pass
    assert str(e.value) == "'Form.query_fields' is defined (but not implemented) as a property"

    with pytest.raises(TypeError) as e:
        @implements(Application)
        class DefineMethodAsProperty:
            @property
            def api_echo(self, message):
                pass
    assert str(e.value) == "'Application.echo' is defined (but not implemented) as a method"

    with pytest.raises(TypeError) as e:
        @implements(Application)
        class InvalidMethodSignature:
            def api_echo(self, massage):
                pass
    assert str(e.value) == "Method signature does not match the definition of 'Application.echo'"

    with pytest.raises(TypeError) as e:
        @implements(Application)
        class IncompleteApplication:
            pass
    assert str(e.value).startswith("IncompleteApplication does not implement 'Application.")
