# -*- coding: utf-8 -*-

# Copyright (C) 2002, 2005, 2010, 2013 Brailcom, o.p.s.
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

"""Formátování výstupů, zejména pro tisk.

Tento modul poskytuje všechny funkce potřebné pro jednoduché sestavení výstupu.
Konkrétně definuje jednoduché značkování textu a provádí jeho formátování.

"""

from .exception import (  # noqa: F401
    FormattingException, TemplateException
)

from .resolver import (  # noqa: F401
    ResolverModuleError, ResolverFileError, ResolverSpecError, Resolver,
    FileResolver, PlainFileResolver, DatabaseResolver, OutputResolver,
)

from .markup import (  # noqa: F401
    Null, Nbsp, Euro, Pound, Center, AlignLeft, AlignRight, VCenter,
    VSpace, HSpace, HLine, Paragraph, List, NewPage, PageNumber, Bold,
    Italic, Roman, FontSize, FontFamily, HGroup, VGroup, Group, Document,
    Table, LongTable, Image, StructuredText,
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
    AbortOutput, HashableDict, LCGFormatter, Formatter,
    PrintSpecification,
)

# This import allows application developers to forget about the distinction
# which identifiers are defined in pytis.output and which in lcg as they are
# closely related from their perspective.  UMm is also necessary for backwards
# compatibility with applications which historically imported it from pytis.output.
from lcg import (  # noqa: F401
    UMm, UPercent
)
