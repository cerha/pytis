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

"""Resolvery jmenných odkazů.

Motivací pro zavedení resolveru je možnost vzájemného provázání jednotlivých
formulářů, specifikací a prvků uživatelského rozhraní (menu, tlačítka apod.) na
základě jmen bez nutnosti vytvářet instance všech odkazovaných objektů při
vytváření objektu na ně se odkazujícího. Instance odkazovaných objektů jsou
vytvořeny až při jejich skutečné potřebě pouze na základě znalosti jména.

"""

import imp
import sys

from pytis.util import *

global _current_resolver
_current_resolver = None

def resolver():
    """Vrať resolver pro získání specifikačních instancí na základě jména."""
    global _current_resolver
    if _current_resolver is None:
        import config
        _current_resolver = FileResolver(config.def_dir)
    return _current_resolver

def set_resolver(resolver):
    """Nastav výchozí resolver vracený funkcí 'resolver()'.

    Výchozí resolver je vytvořen automaticky jako instance 'FileResolver' nad
    adresářem daným konfigurační volbou 'def_dir'.  Pokud chceme použít jiný
    resolver, je třeba jej nastavit touto funkcí před inicializací aplikace.

    """
    assert isinstance(resolver, Resolver)
    global _current_resolver
    _current_resolver = resolver


class ResolverError(Exception):
    """Chyba resolvace.

    Tato třída je společným základem všech tříd signalizujících chybu
    resolvace.
    
    """


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
                    module = imp.load_module(name, file, pathname, descr)
                except ImportError, e:
                    raise ResolverFileError(name, self._path, e)
            finally:
                if file is not None:
                    file.close()
        return module


class ProxyResolver(Resolver):
    """Resolver využívající pro získávání dat jiný resolver.

    Tento resolver umožňuje \"řetězit\" instance resolverů.  V konstruktoru je
    zadána instance jiného resolveru a proxy resolver z ní vytahuje všechny
    specifikace.

    Hlavní smysl této třídy je umožnit dynamicky rozšířit existující instanci
    resolveru a další specifikace.

    """
    def __init__(self, resolver, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          resolver -- resolver, ze kterého jsou specifikace získávány, instance
            třídy 'Resolver'
          kwargs -- předává se konstruktoru předka

        """
        super(ProxyResolver, self).__init__(**kwargs)
        self._resolver = resolver

    def _get_module(self, module_name):
        return self._resolver._get_module(module_name)
