# -*- coding: iso-8859-2 -*-
# Copyright (C) 2001, 2002, 2003, 2005 Brailcom, o.p.s.
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

"""Definice odvozených datových typů.""" 

from pytis.extensions import *



class CodebookNull(pytis.data.Codebook):
    """Codebook rozšířený o možnost ponechat hodnotu None."""

    def __init__(self, *args, **kwargs):
        kwargs['not_null'] = False
        super(CodebookNull, self).__init__(*args, **kwargs)

        
class ValueColumnCodebook(pytis.data.Codebook):
    """Codebook umožňující specifikovat value_column"""
    
    def __init__(self, data_factory, value_columns, strict=True,
                 null_value=False):
        assert is_sequence(value_columns) and len(value_columns) == 1
        df_kwargs = {'dbconnection_spec': config.dbconnection}
        super(ValueColumnCodebook, self).__init__(data_factory,
                                                  data_factory_kwargs=df_kwargs,
                                                  value_column=value_columns[0],
                                                  not_null=not null_value)

class ValidityColumnCodebook(pytis.data.Codebook):
    """Codebook umožňující specifikovat validity_column"""
    
    def __init__(self, data_factory, validity_column, null_value=False):         
        df_kwargs = {'dbconnection_spec': config.dbconnection}
        super(ValidityColumnCodebook, self).__init__(data_factory,
                                                     data_factory_kwargs=df_kwargs,
                                                     validity_column=validity_column,
                                                     not_null= not null_value)
        
class Price(pytis.data.Float):
    def __init__(self, not_null=True):
        super(Price, self).__init__(precision=2, not_null=not_null)
    def default_value(self):
        value, error = self.validate('0')
        if error is not None:
            raise ProgramError("Can't validate default value")
        return value
        
class StringNotNull(pytis.data.String):
    def __init__(self, **kwargs):
        kwargs['not_null'] = True
        super(StringNotNull, self).__init__(**kwargs)
