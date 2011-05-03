# -*- coding: utf-8 -*-

# Copyright (C) 2006, 2007, 2008, 2011 Brailcom, o.p.s.
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

import pytis.data as pd
from pytis.presentation import *

import lcg
from lcg import concat, log as debug

from request import *
from form import *
from field import *
from dialog import *

_globals = dict([(k,v) for k,v in globals().items() if not k.startswith('_')])
for _file in (form, field, ):
    _file.__dict__.update(_globals)
del _globals

_ = lcg.TranslatableTextFactory('pytis')

# TODO: This hack is necessary as long as pytis default language is Czech.
pd = pytis.data
# Translators: All the messages below are validation errors.
pd.Type._VM_NULL_VALUE_MSG = _(u"Empty value")
pd.Type._VM_INVALID_VALUE_MSG = _(u"Invalid value")
pd.Number._VM_MINIMUM_MSG = _(u"Minimal value is %(minimum)s")
pd.Number._VM_MAXIMUM_MSG = _(u"Maximal value is %(maximum)s")
pd.Limited._VM_MINLEN_MSG = _(u"Minimal length %(minlen)s characters not satisfied")
pd.Limited._VM_MAXLEN_MSG = _(u"Maximal length %(maxlen)s characters exceeded")
pd.Integer._VM_NONINTEGER_MSG = _(u"Not an integer")
pd.Float._VM_INVALID_NUMBER_MSG = _(u"Invalid number")
pd.String._VM_MINLEN_MSG = _(u"Minimal length %(minlen)s characters not satisfied")
pd.String._VM_MAXLEN_MSG = _(u"String exceeds max length %(maxlen)s characters")
pd.Password._VM_PASSWORD_MSG = _(u"Enter the password twice to eliminate typos")
pd.Password._VM_PASSWORD_VERIFY_MSG = _(u"Passwords don't match")
pd.Password._VM_MIX_CHARACTERS_MSG = _(u"Please use mix of letters and non-letters in your password")
pd.RegexString._VM_FORMAT_MSG = _(u"Invalid format")
pd.Color._VM_FORMAT_MSG = _(u"Invalid color format ('#RGB' or '#RRGGBB')")
pd.DateTime._VM_DT_FORMAT_MSG = _(u"Invalid date or time format")
pd.DateTime._VM_DT_VALUE_MSG = _(u"Invalid date or time")
pd.DateTime._VM_DT_AGE_MSG = _(u"Date outside the allowed range")
pd.Binary._VM_MINLEN_MSG = _(u"Minimal size %(minlen)s not satisfied")
pd.Binary._VM_MAXLEN_MSG = _(u"Maximal size %(maxlen)s exceeded")
pd.Image._VM_MAXSIZE_MSG = _(u"Maximal pixel size %(maxsize)s exceeded")
pd.Image._VM_MINSIZE_MSG = _(u"Minimal pixel size %(minsize)s exceeded")
# Translators: This is the last validation error message.
pd.Image._VM_FORMAT_MSG = _(u"Unsupported format %(format)s; valid formats: %(formats)s")
