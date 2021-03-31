# -*- coding: utf-8 -*-

# Copyright (C) 2019-2021 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2014 OUI Technology Ltd.
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

"""Resolver pro specifikace výstupu.

Tyto resolvery byly původně odvozeny od pytisových resolverů.  Nyní jsou to
zcela samostatné třídy využívané výhradně pro sestavování tiskových výstupů.

"""
from past.builtins import basestring

import codecs
import imp
import os

import pytis.util
from pytis.util import SimpleCache, identity, super_, xlist
import pytis.output

_ = pytis.util.translations('pytis-wx')


class ResolverModuleError(pytis.util.ResolverError):
    """Výjimka vyvolávaná při nedostupnosti žádaného specifikačního modulu."""

    def __init__(self, module_name, *args):
        """Inicializuj výjimku.

        Argumenty:

          module_name -- jméno nenalezeného specifikačního modulu, string
          args -- další argumenty předané konstruktoru předka

        """
        msg = 'Specification module not found: %s, %s' % (module_name, args)
        super_(ResolverModuleError).__init__(self, msg)


class ResolverFileError(pytis.util.ResolverError):
    """Výjimka vyvolávaná nelze-li načíst žádaný specifikační soubor."""

    def __init__(self, file_name, path, exception):
        """Inicializuj výjimku.

        Argumenty:

          file_name -- jméno nenalezeného specifikačního souboru, string
          path -- cesta ke specifikačním souborům, string
          exception -- výjimka, která problém signalizovala, instance třídy
            'Exception' nebo 'None'

        """
        msg = 'Error importing specification file %s: %s %s' % (file_name, exception, path)
        super_(ResolverFileError).__init__(self, msg)


class ResolverSpecError(pytis.util.ResolverError):
    """Výjimka vyvolaná není-li ve specifikačním modulu žádaná funkce."""

    def __init__(self, module_name, spec_name):
        """Inicializuj výjimku.

        Argumenty:

          module_name -- jméno specifikačního modulu, string
          spec_name -- jméno specfikační funkce, string

        """
        msg = 'Specification not found: %s, %s' % (module_name, spec_name)
        super_(ResolverSpecError).__init__(self, msg)


class Resolver(object):
    """Resolver umožňuje získat specifikační objekt na základě modulu a jména.

    Modulem se rozumí objekt, může a nemusí jím být pythonový modul,
    poskytující specifikace prostřednictvím funkcí vracejících instance
    specifikačních objektů.  Specifikační jména odpovídají jménům callable
    objektů modulu, resolver je schopen vracet přímo tyto objekty (metoda
    'get_object()') nebo jimi vytvořené instance (metoda 'get()').

    """

    def __init__(self):
        """Inicializuj resolver."""
        self._module_cache = SimpleCache(self._get_module)
        self._object_cache = SimpleCache(self._get_object)
        self._spec_cache = SimpleCache(self._get_spec)
        self._instance_cache = SimpleCache(self._get_instance)

    def _get_module(self, module_name):
        raise ResolverModuleError(module_name)

    def _get_object(self, key):
        module_name, spec_name = key
        module = self._module_cache[module_name]
        try:
            obj = getattr(module, spec_name)
        except AttributeError:
            raise ResolverSpecError(module_name, spec_name)
        return obj

    def _get_spec(self, key):
        module_name, spec_name, kwargs_items = key
        kwargs = dict(kwargs_items)
        if module_name.find('.') != -1:
            parts = module_name.split('.')
            module_name = os.path.join(*parts[:-1])
            class_name = parts[-1]
            instance = self.get_instance(module_name, class_name, **kwargs)
            try:
                method = getattr(instance, spec_name)
            except AttributeError:
                raise ResolverSpecError(module_name, spec_name)
            return method()
        else:
            obj = self.get_object(module_name, spec_name)
            return obj(self, **kwargs)

    def _get_instance(self, key):
        module_name, spec_name, args, kwargs_items = key
        class_ = self.get_object(module_name, spec_name)
        return class_(*args, **dict(kwargs_items))

    def get_object(self, module_name, spec_name):
        """Vrať požadovaný objekt z daného specifikačního modulu.

        Argumenty:

          module_name -- jméno specifikačního modulu
          spec_name -- jméno objektu ze specifikačního modulu, neprázdný
            string

        Není-li modul 'module_name' nalezen, je vyvolána výjimka
        'ResolverModuleError'.  Je-li modul nalezen, avšak není v něm
        nalezena třída daná 'spec_name' nebo pokud 'spec_name' začíná
        podtržítkem, je vyvolána výjimka 'ResolverSpecError'.

        """
        if not spec_name or spec_name[0] == '_':
            raise ResolverSpecError(module_name, spec_name)
        key = (module_name, spec_name)
        return self._object_cache[key]

    def get_module(self, module_name):
        """Vrať požadovaný modul.

        Argumenty:

          module_name -- jméno specifikačního modulu

        Není-li modul 'module_name' nalezen, je vyvolána výjimka
        'ResolverModuleError'.

        """
        return self._get_module(module_name)

    def get_instance(self, module_name, spec_name, *args, **kwargs):
        # Nestačí nám pouhé 'get_object', protože třída jako taková obsahuje
        # svůj modul, což může činit potíže při vzdáleném přístupu přes Pyro.
        """Vrať instanci požadované třídy z daného specifikačního modulu.

        Argumenty:

          module_name -- jméno specifikačního modulu
          spec_name -- jméno veřejné třídy ze specifikačního modulu, neprázdný
            string

        Instance třídy je vytvořena voláním jejího konstruktoru s argumenty
        'args' a 'kwargs'.

        Není-li modul 'module_name' nalezen, je vyvolána výjimka
        'ResolverModuleError'.  Je-li modul nalezen, avšak není v něm
        nalezena třída daná 'spec_name' nebo pokud 'spec_name' začíná
        podtržítkem, je vyvolána výjimka 'ResolverSpecError'.

        """
        key = (module_name, spec_name, tuple(args), tuple(kwargs.items()))
        return self._instance_cache[key]

    def get(self, module_name, spec_name, **kwargs):
        """Vrať specifikaci 'spec_name' ze specifikačního modulu 'module_name'.

        Argumenty:

          module_name -- jméno specifikačního modulu.
          spec_name -- jméno specifikační funkce/metoda.
          kwargs -- klíčové argumenty specifikační funkce/metody.

        Pokud 'module_name' neobsahuje tečky, jde přímo o jméno modulu.  V
        tomto modulu je vyhledána funkce 'spec_name', ta je spuštěna s instancí
        resolveru jako prvním pozičním argumentem a danými klíčovými argumenty
        a výsledek je vrácen.

        Pokud 'module_name' obsahuje tečky, jde o název modulu a třídy v něm
        obsažené.  Název modulu může v tomto případě také obsahovat názvy
        adresářů (oddělené rovněž tečkami).  Například název
        'ucetnictvi.denik.UcetniDenik' znamená, že v adresáři 'ucetnictvi' bude
        hledán soubor 'denik.py' a v něm třída 'UcetniDenik'.  Pokud je třída
        nalezena, je vytvořena její instance (konstruktoru je předána instance
        resolveru jako první poziční argument) a nad ní zavolána metoda
        'spec_name', té jsou předány dané klíčové argumenty a výsledek je
        vrácen.

        Není-li modul 'module_name' nalezen, je vyvolána výjimka
        'ResolverModuleError'.  Je-li modul nalezen, avšak není v něm
        nalezena specifikace 'spec_name', je vyvolána výjimka
        'ResolverSpecError'.

        """
        key = (module_name, spec_name, tuple(kwargs.items()))
        return self._spec_cache[key]


class FileResolver(Resolver):
    """Resolver natahující moduly ze specifikačních souborů.

    Specifikační soubory jsou hledány v adresáři zadaném v konstruktoru
    resolveru.  Jména specifikačních souborů musí začínat velkým písmenem.

    """

    def __init__(self, path):
        """Inicializuj resolver.

        Argumenty:

          path -- cesta ke specifikačním souborům; string, nebo sekvence
            stringů

        """
        super(FileResolver, self).__init__()
        self._path = xlist(path)

    def _get_module(self, name, path=None):
        if path is None:
            path = self._path
        name_list = name.split('/')
        top_name = name_list[0]
        try:
            file, pathname, descr = imp.find_module(top_name, path)
        except ImportError as e:
            raise ResolverFileError(top_name, path, e)
        else:
            try:
                module = imp.load_module(top_name, file, pathname, descr)
            finally:
                if file is not None:
                    file.close()
        if len(name_list) > 1:
            module = self._get_module('/'.join(name_list[1:]), path=module.__path__)
        return module


class DatabaseResolver(Resolver):
    """Resolver taking its objects from a database.

    It looks up objects in the database table specified in the constructor.
    This table is expected to contain (at least) the following columns:

      module -- text column containing module names
      specification -- text column containing specification names
      data -- column of any type containing the object data; alternatively other
        column names can be used as specified in the constructor

    Each (module, specification) value should be unique within the table.

    For given module and specification the resolver returns data Python value
    from the corresponding row.  If there is no corresponding row,
    ResolverError is raised.

    This resolver can handle only complete module + specification requests, it
    doesn't return anything reasonable for module only requests.

    """

    def __init__(self, table, columns=('data',), specs=('body',)):
        """
        Arguments:

          table -- name of the database table storing the resolved data
          columns -- sequence of column names to include in the result
          specs -- sequence of specification names in the order matching
            the order of 'columns'.  Possible specification names are
            'body', 'row', 'page_header', 'first_page_header', 'page_footer'
            and 'style'.


        """
        super(DatabaseResolver, self).__init__()
        assert isinstance(table, basestring), table
        assert isinstance(columns, (tuple, list)), columns
        assert isinstance(specs, (tuple, list)), specs
        self._columns = columns
        self._data = pytis.data.dbtable(table, ('module', 'specification') + columns)
        self._specs = specs

    def _get_module(self, name):
        return name

    def _get_object(self, key):
        module_name, spec_name = key
        condition = pytis.data.AND(pytis.data.EQ('module', pytis.data.sval(module_name)),
                                   pytis.data.EQ('specification', pytis.data.sval(spec_name)))
        rows = self._data.select_map(identity, condition=condition)
        if not rows:
            raise ResolverSpecError(module_name, spec_name)
        return [rows[0][c].value() for c in self._columns]

    def _get_spec(self, key):
        return self._get_object(key[:2])

    def _get_instance(self, key):
        return self._get_object(key[:2])

    def get(self, module_name, spec_name, **kwargs):
        try:
            result_index = self._specs.index(spec_name)
        except ValueError:
            raise pytis.util.ResolverError(module_name, spec_name)
        module_parts = module_name.split('/')
        if module_parts[0] == 'output':
            del module_parts[0]
        if len(module_parts) > 1:
            module_name = '/'.join(module_parts[:-1])
            last_spec_name = module_parts[-1]
        else:
            module_name = '/'.join(module_parts)
            last_spec_name = ''
        result = super(DatabaseResolver, self).get(module_name, last_spec_name,
                                                   **kwargs)[result_index]
        if result and isinstance(result, basestring) and spec_name != 'style':
            result = pytis.output.StructuredText(result)
        return result


class OutputResolver(Resolver):
    """Resolver passed to print output specifications.

    Resolves both standard specifications and print specifications.

    """
    def __init__(self, print_spec_dir, specification_resolver):
        """
        Arguments:

          print_spec_dir -- print specification directory for 'pytis.output.FileResolver'.
          specification_resolver -- instance 'pytis.util.Resolver'.

        """
        super(OutputResolver, self).__init__()
        self._print_resolver = FileResolver(print_spec_dir)
        self._specification_resolver = specification_resolver

    def _get_module(self, module_name):
        return self._print_resolver._get_module(module_name)

    def get(self, module_name, spec_name, **kwargs):
        try:
            return super(OutputResolver, self).get(module_name, spec_name, **kwargs)
        except pytis.util.ResolverError:
            return self._specification_resolver.get(module_name, spec_name, **kwargs)
