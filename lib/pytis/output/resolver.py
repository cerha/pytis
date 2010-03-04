# -*- coding: iso-8859-2 -*-

# Resolver pro specifikace v�stupu
# 
# Copyright (C) 2002, 2005 Brailcom, o.p.s.
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

"""Resolver pro specifikace v�stupu.

"""

from pytis.output import *


class OutputResolver(ProxyResolver):
    """Resolver p�ed�van� specifikac�m v�stupu.

    Tento resolver jednak poskytuje standardn� specifikace a jednak
    zp��stup�uje �ablon�m parametry, prost�ednictv�m metody
    'output_parameter()' nebo jej�ho aliasu 'p()'.
    
    """
    OUTPUT_PARAMETERS = 'output-parameters'
    """Jm�no modulu parametr� v�stupu."""

    def __init__(self, resolver, parameters={}):
        """Inicializuj instanci.

        Argumenty:

          resolver -- standardn� resolver specifikac�, p�edan� konstruktoru
            p�edka
          parameters -- dictionary parametr� v�stupu, kl��e mus� b�t nepr�zdn�
            strings, hodnoty mohou b�t libovoln� objekty

        """
        super(OutputResolver, self).__init__(resolver)
        class P:
            def __init__(self, parameters):
                self.__parameters = parameters
            def __getattr__(self, name):
                try:
                    p = self.__parameters[name]
                except KeyError:
                    raise AttributeError(name)
                return lambda resolver: p
        self._parameters = P(parameters)

    def _get_module(self, module_name):
        if module_name == self.OUTPUT_PARAMETERS:
            result = self._parameters
        else:
            result = super(OutputResolver, self)._get_module(module_name)
        return result

    def get(self, module_name, spec_name, **kwargs):
        colon = module_name.find(':')
        if colon != -1:
            kwargs['variant'] = module_name[colon+1:]
            module_name = module_name[:colon]
        return super(OutputResolver, self).get(module_name, spec_name, **kwargs)
    
    def output_parameter(self, name, **kwargs):
        """Vra� hodnotu parametru v�stupu 'name'.

        Argumenty:

          name -- identifik�tor parametru, nepr�zdn� string nebo tuple strings
            d�vaj�c�ch po spojen� jednozna�n� string

        """
        if is_sequence(name):
            name = string.join(name, '/')
        return self.get(self.OUTPUT_PARAMETERS, name, **kwargs)

    p = output_parameter
