# -*- coding: utf-8 -*-

# Copyright (C) 2019-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2013 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Formátování výstupů, zejména pro tisk.

Tento modul poskytuje všechny funkce potřebné pro jednoduché sestavení výstupu.
Konkrétně definuje jednoduché značkování textu a provádí jeho formátování.

"""
from __future__ import print_function

from .exception import (  # noqa: F401
    FormattingException, TemplateException
)

from .resolver import (  # noqa: F401
    ResolverModuleError, ResolverFileError, ResolverSpecError, Resolver,
    FileResolver, DatabaseResolver, OutputResolver,
)

from .markup import (  # noqa: F401
    Null, Nbsp, Euro, Pound, Center, AlignLeft, AlignRight, VCenter,
    VSpace, HSpace, HLine, Paragraph, List, NewPage, PageNumber, Bold,
    Italic, Roman, Subscript, Superscript, FontSize, FontFamily, HGroup,
    VGroup, Group, Document, Table, LongTable, Image, StructuredText,
    LEFT, RIGHT, CENTER, TOP, BOTTOM, MIDDLE,
)

from .flibdata import (  # noqa: F401
    P_NAME, P_KEY, P_ROW, P_CONDITION, P_ARGUMENTS, P_SORTING, P_DATA,
    P_LANGUAGE, data_table, data_item,
)

from .flibutil import (  # noqa: F401
    f_larger, f_smaller, f_table,
)

from .formatter import (  # noqa: F401
    PAGE_WIDTH, PAGE_HEIGHT, PAGE_TOP_MARGIN, PAGE_BOTTOM_MARGIN,
    PAGE_LEFT_MARGIN, PAGE_RIGHT_MARGIN, PAGE_LANDSCAPE_MODE,
    AbortOutput, HashableDict, Formatter, PrintSpecification,
)

# This import allows application developers to forget about the distinction
# which identifiers are defined in pytis.output and which in lcg as they are
# closely related from their perspective.  UMm is also necessary for backwards
# compatibility with applications which historically imported it from pytis.output.
from lcg import (  # noqa: F401
    UMm, UPercent
)
