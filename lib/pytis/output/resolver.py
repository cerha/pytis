# -*- coding: utf-8 -*-

# Resolver pro specifikace výstupu
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

"""Resolver pro specifikace výstupu.

"""

from pytis.output import *


class OutputResolver(ProxyResolver):
    """Resolver předávaný specifikacím výstupu.

    Tento resolver jednak poskytuje standardní specifikace a jednak
    zpřístupňuje šablonám parametry, prostřednictvím metody
    'output_parameter()' nebo jejího aliasu 'p()'.
    
    """
    OUTPUT_PARAMETERS = 'output-parameters'
    """Jméno modulu parametrů výstupu."""

    def __init__(self, resolver, parameters={}):
        """Inicializuj instanci.

        Argumenty:

          resolver -- standardní resolver specifikací, předaný konstruktoru
            předka
          parameters -- dictionary parametrů výstupu, klíče musí být neprázdné
            strings, hodnoty mohou být libovolné objekty

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
        """Vrať hodnotu parametru výstupu 'name'.

        Argumenty:

          name -- identifikátor parametru, neprázdný string nebo tuple strings
            dávajících po spojení jednoznačný string

        """
        if is_sequence(name):
            name = string.join(name, '/')
        return self.get(self.OUTPUT_PARAMETERS, name, **kwargs)

    p = output_parameter
