# -*- coding: utf-8 -*-

# Copyright (C) 2001, 2002, 2005, 2006, 2008, 2009, 2011 Brailcom, o.p.s.
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

"""Name resolution services.

The motivation for using resolvers is the capability of flexible symbolic
linking between various components of the application (form specifications)
without the need to access the linked instances directly.  Instances of all
components are created transparently by the resolver in the moment of their
actual use.

"""

import imp
import sys

from pytis.util import *

def resolver():
    """Deprecated: Use config.resolver instead."""
    import config
    return config.resolver


class ResolverError(Exception):
    """Specification name resolution error."""


class Resolver(object):
    """Specification name resolver.

    The resolver is responsible for resolution of specification names.  It
    automatically creates specification instances from classes defined by
    specification modules.  It then calls methods on those instances and
    returns their results.  The specification instances as well as the results
    of their method calls are cached transparently.  The only public method is
    'get()' which returns the result of calling a given method on given
    specification (both given by name).

    The specifications are located by their name in python modules available in
    the current python path.  The resolver can be configured in two modes using
    the constructor argument `search'.  The names are either fully qualified
    names including the top level module name or relative to modules named in
    the search list (see the constructor documentation for more details).

    The configuration option `search_modules' is normally passed as the
    `search' constructor argument by pytis to the resolver instances which it
    creates.

    """

    def __init__(self, search=()):
        """Arguments:

          search -- if not used, the names are fully qualified names of python
           classes located in python modules available in the current python
           path.  Thus a name `myapp.people.Users' will refer to a class
           `Users' in python module `myapp' and its submodule `people'.  If
           `search' is used, it must be a sequence of strings.  These strings
           are used as prefixes to specification names passed to 'get()'.  Thus
           a name `people.Users' will be searched in pyhon modules `myapp' and
           `myextensions' when search is set to ('myapp', 'myextensions').

        """
        self._search = search
        self._specification_cache = SimpleCache(self._get_specification)
        self._method_result_cache = SimpleCache(self._get_method_result)

    def _get_module(self, name):
        for prefix in self._search and [prefix+'.' for prefix in self._search] or ('',):
            try:
                module = __import__(prefix+name)
            except ImportError as e:
                continue
            else:
                components = (prefix+name).split('.')
                for comp in components[1:]:
                    module = getattr(module, comp)
                return module
        search_info = self._search and (' (searching in %s)' % ', '.join(self._search)) or ''
        raise ResolverError("Resolver error loading module '%s'%s: %s" % (name, search_info, e))

    def _get_specification(self, key):
        name, kwargs = key
        if '.' not in name:
            # TODO: This is a backwards compatibility hack to make top level
            # specification files, such as application.py, work.  Application
            # specification should be turned into a specification class instead
            # of a module with functions.  It is impossible to include
            # specifications directly in the top level namespace of the
            # specification module as long as this hack is active.
            return self._get_module(name)
        module_name, spec_name = name.rsplit('.', 1)
        module = self._get_module(module_name)
        # Note: module.__name__ may not be the same as module_name when self._search is used!
        try:
            specification = getattr(module, spec_name)
        except AttributeError as e:
            raise ResolverError("Resolver error loading specification '%s.%s': %s" %
                                (module.__name__, spec_name, e))
        import pytis.presentation
        if type(specification) != type(object) or \
                not issubclass(specification, pytis.presentation.Specification):
            raise ResolverError("Resolver error loading specification '%s.%s': Not a "
                                "pytis.presentation.Specification subclass." %
                                (module.__name__, spec_name))
        return specification(self, **dict(kwargs))

    def _get_method_result(self, key):
        name, kwargs, method_name = key
        specification = self._specification_cache[(name, kwargs)]
        try:
            method = getattr(specification, method_name)
        except AttributeError, e:
            raise ResolverError("Resolver error loading specification '%s.%s': %s" %
                                (name, method_name, e))
        return method(self)

    def get(self, name, method_name, **kwargs):
        """Return the result of calling 'method_name' on specification instance 'name'.

        Arguments:
          name -- string name of the specification
          method_name -- string name of the method to call
          kwargs -- optional keyword arguments to be passed to the
            specification instance constructor

        """
        return self._method_result_cache[(name, tuple(kwargs.items()), method_name)]

