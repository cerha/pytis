# -*- coding: utf-8 -*-

# Resolver pro specifikace výstupu
# 
# Copyright (C) 2002, 2005, 2011 Brailcom, o.p.s.
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

from pytis.output import *


class ResolverModuleError(ResolverError):
    """Výjimka vyvolávaná při nedostupnosti žádaného specifikačního modulu."""

    def __init__(self, module_name, *args):
        """Inicializuj výjimku.

        Argumenty:

          module_name -- jméno nenalezeného specifikačního modulu, string
          args -- další argumenty předané konstruktoru předka

        """
        msg = 'Specification module not found: %s, %s' % (module_name, args)
        super_(ResolverModuleError).__init__(self, msg)


class ResolverFileError(ResolverError):
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


class ResolverSpecError(ResolverError):
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
            instance = self.get_instance(module_name, class_name, self, **kwargs)
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
        
    def _get_module(self, name):
        module = sys.modules.get(name)
        if module is None:
            file = None
            try:
                try:
                    file, pathname, descr = imp.find_module(name, self._path)
                except ImportError as e:
                    raise ResolverFileError(name, self._path, e)
                module = imp.load_module(name, file, pathname, descr)
            finally:
                if file is not None:
                    file.close()
        return module


class PlainFileResolver(Resolver):
    """Resolver returning content of a file.

    It returns the content of the file named MODULE-SPEC.EXTENSION, where
    MODULE is the requested module name, SPEC name of the requested
    specification and EXTENSION contingent extension given in the constructor.
    The file content is assumed to be text-like and in UTF-8 encoding.

    If the resulting file can't be read, 'None' is returned (so you can safely
    test for presence of the resolved file).

    Files are looked for in the directory or directories given in the
    resolver's constructor.

    """
    def __init__(self, path, extension=None):
        """
        Arguments:

          path -- path to the specification files; string or sequence of strings
          extension -- if not 'None', it is a string to apppend to the
            constructed final file name together with a preceding dot

        """
        super(PlainFileResolver, self).__init__()
        self._path = xlist(path)
        self._extension = extension
        
    def _get_module(self, name):
        return [os.path.join(path, name) for path in self._path]

    def _get_object(self, key):
        module_name, spec_name = key
        modules = self._module_cache[module_name]
        for module in modules:
            file_name = module + '-' + spec_name
            if self._extension:
                file_name = file_name + '.' + self._extension
            if not os.access(file_name, os.F_OK):
                continue
            try:
                obj = codecs.open(file_name, 'r', 'utf-8').read()
                break
            except:
                raise ResolverSpecError(module_name, spec_name)
        else:
            raise ResolverSpecError(module_name, spec_name)
        return obj

    def _get_spec(self, key):
        return self._get_object(key[:2])

    def _get_instance(self, key):
        return self._get_object(key[:2])


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
    def __init__(self, table, result_columns=('data',)):
        """
        Arguments:

          table -- name of the database table storing the resolved data
          result_columns -- sequence of column names to include in the result

        """
        super(DatabaseResolver, self).__init__()
        assert isinstance(table, basestring), table
        assert is_sequence(result_columns), result_columns
        self._result_columns = result_columns
        import config
        self._data = pytis.data.dbtable(table, ('module', 'specification') + result_columns,
                                        config.dbconnection)
        
    def _get_module(self, name):
        return name

    def _get_object(self, key):
        module_name, spec_name = key
        condition = pytis.data.AND(pytis.data.EQ('module', pytis.data.sval(module_name)),
                                   pytis.data.EQ('specification', pytis.data.sval(spec_name)))
        rows = self._data.select_map(identity, condition=condition)
        if not rows:
            raise ResolverSpecError(module_name, spec_name)
        obj = [rows[0][c].value() for c in self._result_columns]
        if len(obj) == 1:
            obj = obj[0]
        return obj

    def _get_spec(self, key):
        return self._get_object(key[:2])

    def _get_instance(self, key):
        return self._get_object(key[:2])    


class OutputResolver(Resolver):
    """Resolver předávaný specifikacím výstupu.

    Tento resolver jednak poskytuje standardní specifikace, jednak tiskové specifikace a jednak
    zpřístupňuje šablonám parametry, prostřednictvím metody
    'output_parameter()' nebo jejího aliasu 'p()'.
    
    """
    OUTPUT_PARAMETERS = 'output-parameters'
    """Jméno modulu parametrů výstupu."""

    def __init__(self, print_resolver, specification_resolver, parameters={}):
        """
        Arguments:

          print_resolver -- instance 'pytis.output.FileResolver'
          specification_resolver -- instance 'pytis.util.Resolver'
          parameters -- dictionary of output parameters, keys must be non-empty
            strings, values may be arbitrary objects

        """
        super(OutputResolver, self).__init__()
        class P(dict):
            def __getattr__(self, name):
                try:
                    p = self[name]
                except KeyError:
                    raise AttributeError(name)
                return lambda resolver: p
        self._print_resolver = print_resolver
        self._specification_resolver = specification_resolver
        self._parameters = P(parameters)

    def _get_module(self, module_name):
        if module_name == self.OUTPUT_PARAMETERS:
            return self._parameters
        else:
            return self._print_resolver._get_module(module_name)

    def get(self, module_name, spec_name, **kwargs):
        try:
            return super(OutputResolver, self).get(module_name, spec_name, **kwargs)
        except ResolverError:
            return self._specification_resolver.get(module_name, spec_name, **kwargs)
    
    def output_parameter(self, name, **kwargs):
        """Vrať hodnotu parametru výstupu 'name'.

        Argumenty:

          name -- identifikátor parametru, neprázdný string nebo tuple strings
            dávajících po spojení jednoznačný string

        """
        if is_sequence(name):
            name = string.join(name, '/')
        return self.get(self.OUTPUT_PARAMETERS, name, **kwargs)

    p = output_parameter

    def add_output_parameters(self, parameters):
        """Add parameters to the output parameters of the resolver.

        Arguments:

          parameters -- dictionary of output parameters to be added, keys must
            be non-empty strings, values may be arbitrary objects
            
        """
        p = self._parameters
        for k, v in parameters.items():
            if p.has_key(k):
                raise Exception("Key already present in output parameters", k)
            p[k] = v
