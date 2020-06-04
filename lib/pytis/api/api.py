# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
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


def implements(api_class):
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
        for name in cls._api_attributes:
            if not hasattr(cls, 'api_' + name):
                raise TypeError("{} does not implement '{}.{}'"
                                .format(cls.__name__, api_class.__name__, name))
        cls.provider = provider
        return cls
    return wrapper


class APIProvider:
    """Public API provider proxying access to the public API of the wrapped instance.

    This class checks for correct API implementation of the wrapped instance
    and restricts access to its attributes to only those which belong to the
    public API.

    Marking a class by the 'pytis.api.implements()' decorator adds a new public
    method 'provider()' which returnes a (cached) 'APIProvider' instance for
    the implementation class instance on which it is called.

    """
    def __init__(self, instance=None):
        self._instance = instance
        if instance:
            self.provide(instance)

    def provide(self, instance):
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

    query_fields = property()
    """The form's query fields panel API as 'pytis.api.QueryFields' instance.

    Returns None if the form has no query fields panel.

    """

    def clear_selection(self):
        """Unselect all rows that are currently selected (if any)."""
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

    def message(self, message, kind='info'):
        """Display a non-interactive message to the user.

        Arguments:

          message -- the text to be displayed.
          kind -- message kind as a string.  One of 'info', 'warning', 'error'.
            The message kind may be indicated in the UI using colors, icons or
            sounds (beeping for 'warning' and 'error' etc.).

        """
        pass


def test_api_definition():
    import pytest
    import pytis.api

    @implements(pytis.api.Form)
    class MyForm:

        @property
        def api_name(self):
            return 'form name'

        @property
        def api_query_fields(self):
            return 'the query fields'

        def api_clear_selection(self):
            return 'rows unselected'

    @implements(pytis.api.Application)
    class MyApp:

        non_api_attribute = 'non-API attribute'

        @property
        def api_param(self):
            return 'the param attribute'

        @property
        def api_form(self):
            return MyForm().provider()

        def api_message(self, message, kind='info'):
            return self.message('-' + message + '-')

        def message(self, message):
            return 'Message: {}'.format(message)

    app_instance = MyApp()
    app = app_instance.provider()

    assert app.param == 'the param attribute'
    assert isinstance(app.form, APIProvider)
    assert app.form.name == 'form name'
    assert app.form.query_fields == 'the query fields'
    assert app.message('foo') == 'Message: -foo-'

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
            def api_message(self, condition):
                pass
    assert str(e.value) == "'Application.message' is defined (but not implemented) as a method"

    with pytest.raises(TypeError) as e:
        @implements(Application)
        class InvalidMethodSignature:
            def api_message(self, massage):
                pass
    assert str(e.value) == "Method signature does not match the definition of 'Application.message'"

    with pytest.raises(TypeError) as e:
        @implements(Application)
        class IncompleteApplication:
            pass
    assert str(e.value).startswith("IncompleteApplication does not implement 'Application.")
