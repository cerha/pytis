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
        if name == 'param' and self._instance is None:
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
    """Current filtering condition as a pytis.data.Operator instance or None."""

    arguments = property()
    """Current arguemnts as a dictionary of 'pytis.data.Value' instances or None."""

    sorting = property()
    """Current sorting as in pytis.data.Data.select() or None."""

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

    def echo(self, message, kind='info'):
        """Display a non-interactive message to the user.

        Arguments:

          message -- the text to be displayed.
          kind -- message kind as a string.  One of 'info', 'warning', 'error'.
            The message kind may be indicated in the UI using colors, icons or
            sounds (beeping for 'warning' and 'error' etc.).

        """
        pass

    def message(self, message, title=None, content=None):
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

    def warning(self, message, title=None, content=None):
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

    def error(self, message, title=None, content=None):
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
          prefill -- initial field values as a dictionary keyed by field id
            with values of the corresponding inner Python type (same as in
            'pytis.presentation.PresentedRow' constructor).
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

    def launch_file(self, path):
        """Launch a viewer for given local file.

        path -- Full path to the file in the local (server side) filesystem.

        The viewer will be launched on the client machine (remotely) if remote
        client connection exists.  The file will be temporarily copied to the
        client machine in this case and cleaned up automatically afterwards.

        If the viewer is run remotely on a client machine (such as on Windows),
        client side file associations will take effect.  If the viewer is run
        locally (on the application server), the viewer is determined through
        Mailcap.  For PDF files, the viewer set through the configuration option
        'postscript_viewer' (if set) takes precedence.

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

    def refresh(self):
        """Refresh visible application components."""
        pass

    def exit(self):
        """Exit the application."""
        pass


@implements(Application, incomplete=True)
class BaseApplication(object):

    """Base class for classes implementing the 'Application' API.

    This class only implements access to shared parameters using 'app.param'.
    It may be used as a base class for other classes implementing the
    'Application' API or directly in scripts which don't need anything else
    than 'app.param'.

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
        super(BaseApplication, self).__init__()

    @property
    def api_param(self):
        return self._param


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
