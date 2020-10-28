# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 OUI Technology Ltd.
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

from builtins import range
import sys
import pytis.util


def resolver():
    """Deprecated: Use config.resolver instead."""
    return pytis.config.resolver


def _issubclass(c1, c2):
    try:
        return issubclass(c1, c2)
    except TypeError:
        return False


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
        self._specification_cache = pytis.util.SimpleCache(self._get_specification)
        self._method_result_cache = pytis.util.SimpleCache(self._get_method_result)

    def _import_module(self, name):
        components = name.split('.')
        try:
            module = __import__(name)
        except ImportError as e:
            if sys.version_info[0] == 2:
                template = "No module named %s"
            else:
                template = "No module named '%s'"
            for i in range(len(components) + 1):
                if str(e) == template % '.'.join(components[i:]):
                    # Raise resolver error only if the import error actually
                    # related to importing the named module itself and not to some
                    # nested import within this module.
                    raise ResolverError("Resolver error loading specification '%s': %s" % (name, e))
            # The error inside the imported module (typically the imported
            # module attempts to import a python module which is not
            # installed) must raise the original exception so that we can
            # detect such errors normally.
            raise
        for component in components[1:]:
            try:
                module = getattr(module, component)
            except AttributeError as e:
                raise ResolverError("Resolver error loading specification '%s': %s" % (name, e))
        return module

    def _get_object_by_name(self, name):
        # The returned object is normally a Specification class, but may also
        # be a Wiking module class in Wiking resolver (derived from this class).
        if '.' in name:
            module_name, spec_name = name.rsplit('.', 1)
            allow_search = True not in [(module_name + '.').startswith(prefix + '.')
                                        for prefix in self._search]
        else:
            module_name, spec_name = None, name
            allow_search = True
        if self._search and allow_search:
            for prefix in self._search:
                if module_name is not None:
                    search_module_name = prefix + '.' + module_name
                else:
                    search_module_name = prefix
                try:
                    module = self._import_module(search_module_name)
                except ResolverError:
                    # Try another search path prefix.
                    continue
                try:
                    return getattr(module, spec_name)
                except AttributeError:
                    # Try another search path prefix.
                    continue
            raise ResolverError("Resolver error loading specification '%s': " % name +
                                "Not found within %s." % ', '.join(self._search))
        else:
            if module_name is None:
                raise ResolverError("Resolver error loading specification '%s': " % name +
                                    "Top level name can not be resolved when search is not set.")
            module = self._import_module(module_name)
            try:
                return getattr(module, spec_name)
            except AttributeError as e:
                raise ResolverError("Resolver error loading specification '%s': %s" % (name, e))

    def _get_specification(self, key):
        name, kwargs = key
        # This method is split into two parts (_get_specification and
        # _get_object_by_name) to allow overriding the resolver logic in
        # Wiking.  Note, that Wiking overrides this method without calling the
        # super class method (only _get_object_by_name is used in Wiking
        # resolver).
        specification = self._get_object_by_name(name)
        from types import ModuleType
        if isinstance(specification, ModuleType):
            return specification
        import pytis.presentation
        if not _issubclass(specification, pytis.presentation.SpecificationBase):
            raise ResolverError(("Resolver error loading specification '%s': %s is not a "
                                 "pytis.presentation.Specification subclass.") %
                                (name, specification,))
        if pytis.util.argument_names(specification.__init__):
            # TODO: Remove this temporary hack for backwards compatibility.
            # Now it is necessary for example because some specifications in
            # applications may override the constructor and expect the resolver
            # argument (typically when they define kwargs).
            args = (self,)
        else:
            args = ()
        return specification(*args, **dict(kwargs))

    def _get_method_result(self, key):
        name, kwargs, method_name = key
        specification = self._specification_cache[(name, kwargs)]
        try:
            method = getattr(specification, method_name)
        except AttributeError as e:
            raise ResolverError("Resolver error loading specification '%s.%s': %s" %
                                (name, method_name, e))
        return method()

    def specification(self, name, **kwargs):
        """Return the specification instance of given 'name'.

        Arguments:
          name -- string name of the specification
          kwargs -- optional keyword arguments to be passed to the
            specification instance constructor

        """
        specification = self._specification_cache[(name, tuple(kwargs.items()))]
        import pytis.presentation
        if not isinstance(specification, pytis.presentation.SpecificationBase):
            # We need to avoid returning specification modules here (see
            # _get_specification for its possible return values).
            raise ResolverError(("Resolver error loading specification '%s': %s is not a "
                                 "pytis.presentation.Specification instance.") %
                                (name, specification,))
        return specification

    def get(self, name, method_name, **kwargs):
        """Return the result of calling 'method_name' on specification instance 'name'.

        Arguments:

          name -- string name of the specification
          method_name -- string name of the method to call
          kwargs -- optional keyword arguments to be passed to the
            specification instance constructor

        """
        return self._method_result_cache[(name, tuple(kwargs.items()), method_name)]

    def get_object(self, spec_name, object_name):
        """Return given object from specification.

        Arguments:

          spec_name -- name of the specification; string
          object_name -- name of the object in the specification; string

        """
        specification = self._get_specification((spec_name, (),))
        try:
            object = getattr(specification, object_name)
        except AttributeError:
            raise ResolverError("No attribute %s in specification %s" % (object_name, spec_name,))
        return object

    def walk(self, cls=None):
        """Return all 'cls' subclasses defined in current search path.

        Returns a list of pairs (name, class), where name is the string name of
        the class and class is a subclass of 'cls'.  Each class is returned
        only once and the name is always the shortest name of given class when
        it is first found within the search path (the same class may be
        imported into several modules, so it may be available under several
        different names).

        This method only works when the search path is non-empty.

        """
        if cls is None:
            import pytis.presentation
            cls = pytis.presentation.Specification
        searched_modules = []
        classes = []
        names = {}

        def search(module):
            # Search cls subclasses in given module recursively.
            submodules = []
            for name, value in module.__dict__.items():
                if _issubclass(value, cls) and value != cls \
                        and value not in classes and name not in names:
                    classes.append(value)
                    names[module.__name__ + '.' + name] = value
                elif type(value) == type(module) and value != module \
                        and value.__name__.startswith(module.__name__):
                    submodules.append(value)
            for submodule in submodules:
                # Search submodules at the end (after all classes) to find
                # classes in the top-most module (shortest name).
                if submodule not in searched_modules:
                    searched_modules.append(submodule)
                    search(submodule)
        for module_name in self._search:
            module = self._import_module(module_name)
            search(module)
        name = dict([(class_, name) for name, class_ in names.items()])
        return [(name[class_], class_) for class_ in classes]

    def clear(self):
        """Clear all resolver caches."""
        self._specification_cache.clear()
        self._method_result_cache.clear()

    def reload(self):
        """Reload all specification modules and clear all caches."""
        # TODO: It only works when search path is set!
        self.clear()
        for name in sys.modules:
            for prefix in self._search:
                if ((not prefix.startswith('pytis.') and
                     (name == prefix or name.startswith(prefix + '.')) and
                     sys.modules[name] is not None)):
                    reload(sys.modules[name])
