# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020, 2022 Tomáš Cerha <t.cerha@gmail.com>
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

    def message(self, message):
        """Display given message in an interactive dialog.

        The user needs to confirm the dialog before continuing.

        Arguments:

          message -- the text to be displayed.

        """
        pass

    def warning(self, message):
        """Display warning in an interactive dialog.

        The user needs to confirm the dialog before continuing.

        Arguments:

          message -- the warning text to be displayed.

        """
        pass

    def error(self, message):
        """Display given error message in an interactive dialog.

        The user needs to confirm the dialog before continuing.

        Arguments:

          message -- the error text to be displayed.

        """
        pass

    def question(self, message, default=True):
        """Display given question in an interactive dialog.

        The user needs to answer Yes or No to given question before continuing.
        Returns True when Yes was selected, False when No and None when
        canceled otherwise.

        Arguments:

          message -- the question to be displayed.
          default -- the default selected answer ('Yes' when True, 'No' when False).

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
