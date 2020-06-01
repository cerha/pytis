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


class API:
    """Common base class for classes providing public API methods and attributes.

    This common base class provides just the common infrastructure.  Classes
    derived from this class define the public API (API definition classes) and
    classes derived from the API definition classes implement this API.

    """
    class Proxy:
        """Wrapper class proxying access to the public API of the wrapped instance.

        This class checks for correct API implementation of the wrapped
        instance and restricts access to its attributes to only those which
        belong to the public API.

        """
        def __init__(self, instance=None):
            self._instance = instance
            self._api_attributes = ()
            if instance:
                self.wrap(instance)

        def wrap(self, instance):
            def find_api_class(cls):
                api_bases = [c for c in cls.__bases__ if issubclass(c, API)]
                if api_bases:
                    base_class = api_bases[0]
                    if base_class.__bases__ == (API,):
                        return base_class
                    else:
                        return find_api_class(base_class)
            api_class = find_api_class(instance.__class__)
            if not api_class:
                raise TypeError("{} is not a {} subclass".format(instance.__class__, API))
            api_attributes = [name for name in dir(api_class)
                              if not name.startswith('_') and name not in dir(API)]
            for name in api_attributes:
                if getattr(api_class, name) is getattr(instance.__class__, name):
                    raise TypeError("{} does not implement '{}.{}'"
                                    .format(instance.__class__.__name__, api_class.__name__, name))
            self._instance = instance
            self._api_attributes = api_attributes

        def __str__(self):
            return '<pytis.api.API.Proxy for {}>'.format(self._instance)

        def __repr__(self):
            return str(self)

        def __getattr__(self, name):
            if name not in self._api_attributes:
                raise AttributeError("'{}' object has no public API member '{}'"
                                     .format(self._instance.__class__.__name__, name))
            return getattr(self._instance, name)

    @classmethod
    def _definition(cls, name):
        try:
            return getattr(cls, name)
        except AttributeError:
            raise AttributeError("'{}' does not define public API member '{}'"
                                 .format(cls.__name__, name))

    @classmethod
    def property(cls, f, *args):
        """Decorator marking a property accessed through the public API."""
        if not isinstance(cls._definition(f.__name__), property):
            raise TypeError("'{}.{}' is not defined as a property".format(cls.__name__, f.__name__))
        return property(f, *args)

    @classmethod
    def method(cls, f):
        """Decorator marking a method accessed through the public API."""
        definition = cls._definition(f.__name__)
        if not callable(cls._definition(f.__name__)):
            raise TypeError("'{}.{}' is not defined as a method".format(cls.__name__, f.__name__))
        signature = inspect.signature if hasattr(inspect, 'signature') else inspect.getargspec
        if signature(f) != signature(definition):
            raise TypeError("Signature does not match the definition of '{}.{}'"
                            .format(cls.__name__, f.__name__))
        return f


class Form(API):
    """Public API representation of the current form."""

    query_fields = property()
    """The forms query fields panel API as 'pytis.api.QueryFields' instance.

    Returns None if the form has no query fields panel.

    """


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

    def message(self, message):
        """Display a non-interactive message to the user.

        Arguments:

          message -- the text to be displayed.

        """
        pass


def test_api_definition():
    import pytest
    import pytis.api
    from pytis.api import app

    class MyForm(Form):

        @pytis.api.Form.property
        def query_fields(self):
            return 'the query fields'

        def non_api_method(self):
            pass

    class MyApp(Application):

        non_api_attribute = 'non-API attribute'

        @pytis.api.Application.property
        def param(self):
            return 'the param attribute'

        @pytis.api.Application.property
        def form(self):
            return self.Proxy(MyForm())

        @pytis.api.Application.method
        def message(self, message):
            return 'Message: {}'.format(message)

    app.wrap(MyApp())

    assert app.param == 'the param attribute'
    assert isinstance(app.form, API.Proxy)
    assert app.form.query_fields == 'the query fields'
    assert app.message('foo') == 'Message: foo'
    with pytest.raises(AttributeError) as e:
        app.non_api_attribute
    assert str(e.value) == "'MyApp' object has no public API member 'non_api_attribute'"


def test_api_definition_errors():
    import pytest
    import pytis.api
    with pytest.raises(TypeError) as e:
        class DefinePropertyAsMethod(Form):
            @pytis.api.Form.method
            def query_fields(self):
                pass
    assert str(e.value) == "'Form.query_fields' is not defined as a method"

    with pytest.raises(TypeError) as e:
        class DefineMethodAsProperty(Application):
            @pytis.api.Application.property
            def message(self, condition):
                pass
    assert str(e.value) == "'Application.message' is not defined as a property"

    with pytest.raises(TypeError) as e:
        class InvalidMethodSignature(Form):
            @pytis.api.Application.method
            def message(self, massage):
                pass
    assert str(e.value) == "Signature does not match the definition of 'Application.message'"

    class IncompleteApplication(Application):
        pass
    with pytest.raises(TypeError) as e:
        API.Proxy(IncompleteApplication())
    assert str(e.value).startswith("IncompleteApplication does not implement 'Application.")
