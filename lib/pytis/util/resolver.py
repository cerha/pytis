# -*- coding: iso-8859-2 -*-

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

"""Resolvery jmenn�ch odkaz�.

Motivac� pro zaveden� resolveru je mo�nost vz�jemn�ho prov�z�n� jednotliv�ch
formul���, specifikac� a prvk� u�ivatelsk�ho rozhran� (menu, tla��tka apod.) na
z�klad� jmen bez nutnosti vytv��et instance v�ech odkazovan�ch objekt� p�i
vytv��en� objektu na n� se odkazuj�c�ho. Instance odkazovan�ch objekt� jsou
vytvo�eny a� p�i jejich skute�n� pot�eb� pouze na z�klad� znalosti jm�na.

"""

import imp
import sys

from pytis.util import *

global _current_resolver
_current_resolver = None

def resolver():
    """Vra� resolver pro z�sk�n� specifika�n�ch instanc� na z�klad� jm�na."""
    global _current_resolver
    _current_resolver
    if _current_resolver is None:
        import config
        _current_resolver = FileResolver(config.def_dir)
    return _current_resolver

def set_resolver(resolver):
    """Nastav v�choz� resolver vracen� funkc� 'resolver()'.

    V�choz� resolver je vytvo�en automaticky jako instance 'FileResolver' nad
    adres��em dan�m konfigura�n� volbou 'def_dir'.  Pokud chceme pou��t jin�
    resolver, je t�eba jej nastavit touto funkc� p�ed inicializac� aplikace.

    """
    assert isinstance(resolver, Resolver)
    global _current_resolver
    _current_resolver = resolver


class ResolverError(Exception):
    """Chyba resolvace.

    Tato t��da je spole�n�m z�kladem v�ech t��d signalizuj�c�ch chybu
    resolvace.
    
    """


class ResolverModuleError(ResolverError):
    """V�jimka vyvol�van� p�i nedostupnosti ��dan�ho specifika�n�ho modulu."""

    def __init__(self, module_name, *args):
        """Inicializuj v�jimku.

        Argumenty:

          module_name -- jm�no nenalezen�ho specifika�n�ho modulu, string
          args -- dal�� argumenty p�edan� konstruktoru p�edka

        """
        msg = 'Specification module not found: %s, %s' % (module_name, args)
        super_(ResolverModuleError).__init__(self, msg)
    

class ResolverFileError(ResolverError):
    """V�jimka vyvol�van� nelze-li na��st ��dan� specifika�n� soubor."""
    
    def __init__(self, file_name, path, exception):
        """Inicializuj v�jimku.

        Argumenty:

          file_name -- jm�no nenalezen�ho specifika�n�ho souboru, string
          path -- cesta ke specifika�n�m soubor�m, string
          exception -- v�jimka, kter� probl�m signalizovala, instance t��dy
            'Exception' nebo 'None'
          
        """
        msg = 'Error importing specification file %s: %s %s' % (file_name, exception, path)
        super_(ResolverFileError).__init__(self, msg)


class ResolverSpecError(ResolverError):
    """V�jimka vyvolan� nen�-li ve specifika�n�m modulu ��dan� funkce."""
    
    def __init__(self, module_name, spec_name):
        """Inicializuj v�jimku.

        Argumenty:

          module_name -- jm�no specifika�n�ho modulu, string
          spec_name -- jm�no specfika�n� funkce, string
          
        """
        msg = 'Specification not found: %s, %s' % (module_name, spec_name)
        super_(ResolverSpecError).__init__(self, msg)


class Resolver(object):
    """Resolver umo��uje z�skat specifika�n� objekt na z�klad� modulu a jm�na.

    Modulem se rozum� objekt, m��e a nemus� j�m b�t pythonov� modul,
    poskytuj�c� specifikace prost�ednictv�m funkc� vracej�c�ch instance
    specifika�n�ch objekt�.  Specifika�n� jm�na odpov�daj� jm�n�m callable
    objekt� modulu, resolver je schopen vracet p��mo tyto objekty (metoda
    'get_object()') nebo jimi vytvo�en� instance (metoda 'get()').

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
        """Vra� po�adovan� objekt z dan�ho specifika�n�ho modulu.

        Argumenty:

          module_name -- jm�no specifika�n�ho modulu
          spec_name -- jm�no objektu ze specifika�n�ho modulu, nepr�zdn�
            string
        
        Nen�-li modul 'module_name' nalezen, je vyvol�na v�jimka
        'ResolverModuleError'.  Je-li modul nalezen, av�ak nen� v�n�m
        nalezena t��da dan� 'spec_name' nebo pokud 'spec_name' za��n�
        podtr��tkem, je vyvol�na v�jimka 'ResolverSpecError'.

        """
        if not spec_name or spec_name[0] == '_':
            raise ResolverSpecError(module_name, spec_name)
        key = (module_name, spec_name)
        return self._object_cache[key]

    def get_module(self, module_name):
        """Vra� po�adovan� modul.

        Argumenty:

          module_name -- jm�no specifika�n�ho modulu
        
        Nen�-li modul 'module_name' nalezen, je vyvol�na v�jimka
        'ResolverModuleError'.

        """
        return self._get_module(module_name)

    def get_instance(self, module_name, spec_name, *args, **kwargs):
        # Nesta�� n�m pouh� 'get_object', proto�e t��da jako takov� obsahuje
        # sv�j modul, co� m��e �init pot�e p�i vzd�len�m p��stupu p�es Pyro.
        """Vra� instanci po�adovan� t��dy z dan�ho specifika�n�ho modulu.

        Argumenty:

          module_name -- jm�no specifika�n�ho modulu
          spec_name -- jm�no ve�ejn� t��dy ze specifika�n�ho modulu, nepr�zdn�
            string

        Instance t��dy je vytvo�ena vol�n�m jej�ho konstruktoru s�argumenty
        'args' a 'kwargs'.
        
        Nen�-li modul 'module_name' nalezen, je vyvol�na v�jimka
        'ResolverModuleError'.  Je-li modul nalezen, av�ak nen� v�n�m
        nalezena t��da dan� 'spec_name' nebo pokud 'spec_name' za��n�
        podtr��tkem, je vyvol�na v�jimka 'ResolverSpecError'.

        """
        key = (module_name, spec_name, tuple(args), tuple(kwargs.items()))
        return self._instance_cache[key]

    def get(self, module_name, spec_name, **kwargs):
        """Vra� specifikaci 'spec_name' ze specifika�n�ho modulu 'module_name'.

        Argumenty:

          module_name -- jm�no specifika�n�ho modulu.          
          spec_name -- jm�no specifika�n� funkce/metoda.
          kwargs -- kl��ov� argumenty specifika�n� funkce/metody.

        Pokud 'module_name' neobsahuje te�ky, jde p��mo o jm�no modulu.  V
        tomto modulu je vyhled�na funkce 'spec_name', ta je spu�t�na s instanc�
        resolveru jako prvn�m pozi�n�m argumentem a dan�mi kl��ov�mi argumenty
        a v�sledek je vr�cen.

        Pokud 'module_name' obsahuje te�ky, jde o n�zev modulu a t��dy v n�m
        obsa�en�.  N�zev modulu m��e v tomto p��pad� tak� obsahovat n�zvy
        adres��� (odd�len� rovn� te�kami).  Nap��klad n�zev
        'ucetnictvi.denik.UcetniDenik' znamen�, �e v adres��i 'ucetnictvi' bude
        hled�n soubor 'denik.py' a v n�m t��da 'UcetniDenik'.  Pokud je t��da
        nalezena, je vytvo�ena jej� instance (konstruktoru je p�ed�na instance
        resolveru jako prvn� pozi�n� argument) a nad n� zavol�na metoda
        'spec_name', t� jsou p�ed�ny dan� kl��ov� argumenty a v�sledek je
        vr�cen.
          
        Nen�-li modul 'module_name' nalezen, je vyvol�na v�jimka
        'ResolverModuleError'.  Je-li modul nalezen, av�ak nen� v�n�m
        nalezena specifikace 'spec_name', je vyvol�na v�jimka
        'ResolverSpecError'.
        
        """        
        key = (module_name, spec_name, tuple(kwargs.items()))
        return self._spec_cache[key]
        

class FileResolver(Resolver):
    """Resolver natahuj�c� moduly ze specifika�n�ch soubor�.

    Specifika�n� soubory jsou hled�ny v adres��i zadan�m v konstruktoru
    resolveru.  Jm�na specifika�n�ch soubor� mus� za��nat velk�m p�smenem.

    """
    def __init__(self, path):
        """Inicializuj resolver.

        Argumenty:

          path -- cesta ke specifika�n�m soubor�m; string, nebo sekvence
            string�

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
    """Resolver vyu��vaj�c� pro z�sk�v�n� dat jin� resolver.

    Tento resolver umo��uje \"�et�zit\" instance resolver�.  V�konstruktoru je
    zad�na instance jin�ho resolveru a proxy resolver z�n� vytahuje v�echny
    specifikace.

    Hlavn� smysl t�to t��dy je umo�nit dynamicky roz���it existuj�c� instanci
    resolveru a dal�� specifikace.

    """
    def __init__(self, resolver, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          resolver -- resolver, ze kter�ho jsou specifikace z�sk�v�ny, instance
            t��dy 'Resolver'
          kwargs -- p�ed�v� se konstruktoru p�edka

        """
        super(ProxyResolver, self).__init__(**kwargs)
        self._resolver = resolver

    def _get_module(self, module_name):
        return self._resolver._get_module(module_name)
