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

Tento modul poskytuje všechny funkce potřebné pro jednoduché sestavení
výstupu. Konkrétně definuje jednoduché značkování textu a provádí jeho
formátování.

"""
from __future__ import print_function

# Imports use 'X as X' form (PEP 484) to mark names as explicitly re-exported
# without maintaining a separate __all__ list.
from .exception import (
    FormattingException as FormattingException, TemplateException as TemplateException,
)

from .resolver import (
    ResolverModuleError as ResolverModuleError, ResolverFileError as ResolverFileError,
    ResolverSpecError as ResolverSpecError, Resolver as Resolver,
    FileResolver as FileResolver, DatabaseResolver as DatabaseResolver,
    OutputResolver as OutputResolver,
)

from .markup import (
    Null as Null, Nbsp as Nbsp, Euro as Euro, Pound as Pound,
    Center as Center, AlignLeft as AlignLeft, AlignRight as AlignRight,
    VCenter as VCenter,
    VSpace as VSpace, HSpace as HSpace, HLine as HLine, Paragraph as Paragraph,
    List as List, NewPage as NewPage, PageNumber as PageNumber, Bold as Bold,
    Italic as Italic, Roman as Roman, Subscript as Subscript, Superscript as Superscript,
    FontSize as FontSize, FontFamily as FontFamily, HGroup as HGroup,
    VGroup as VGroup, Group as Group, Document as Document, Table as Table,
    LongTable as LongTable, Image as Image, StructuredText as StructuredText,
    LEFT as LEFT, RIGHT as RIGHT, CENTER as CENTER, TOP as TOP, BOTTOM as BOTTOM,
    MIDDLE as MIDDLE,
)

from .flibdata import (
    P_NAME as P_NAME, P_KEY as P_KEY, P_ROW as P_ROW, P_CONDITION as P_CONDITION,
    P_ARGUMENTS as P_ARGUMENTS, P_SORTING as P_SORTING, P_DATA as P_DATA,
    P_LANGUAGE as P_LANGUAGE, data_table as data_table, data_item as data_item,
)

from .flibutil import (
    f_larger as f_larger, f_smaller as f_smaller, f_table as f_table,
)

from .formatter import (
    PAGE_WIDTH as PAGE_WIDTH, PAGE_HEIGHT as PAGE_HEIGHT,
    PAGE_TOP_MARGIN as PAGE_TOP_MARGIN, PAGE_BOTTOM_MARGIN as PAGE_BOTTOM_MARGIN,
    PAGE_LEFT_MARGIN as PAGE_LEFT_MARGIN, PAGE_RIGHT_MARGIN as PAGE_RIGHT_MARGIN,
    PAGE_LANDSCAPE_MODE as PAGE_LANDSCAPE_MODE,
    AbortOutput as AbortOutput, HashableDict as HashableDict,
    Formatter as Formatter, PrintSpecification as PrintSpecification,
)

# This import allows application developers to forget about the distinction
# which identifiers are defined in pytis.output and which in lcg as they are
# closely related from their perspective.  UMm is also necessary for backwards
# compatibility with applications which historically imported it from pytis.output.
from lcg import (
    UMm as UMm, UPercent as UPercent,
)
