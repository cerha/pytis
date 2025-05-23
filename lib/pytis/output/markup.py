# -*- coding: utf-8 -*-

# Copyright (C) 2019-2025 Tomáš Cerha <t.cerha@gmail.com>

# Copyright (C) 2002-2017 OUI Technology Ltd.
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

"""Definice formátovacích značek.

Tento soubor definuje prvky sestavující výsledný tiskový dokument.  Prvky
jednak definují formátování a jednak umožňují vkládat externí data do
dokumentu.  Jsou používány v definičních souborech výstupních sestav.

Každá veřejná třída modulu odpovídá formátovací značce použitelné v definici
dokumentu.  Kromě těchto tříd je ještě možno používat obyčejné řetězce, které
budou vloženy tak, jak jsou, včetně mezer a odřádkování, a sekvence obsahující
elementy ke spojení dohromady.

"""
from __future__ import print_function

from past.builtins import basestring, long

import copy
import os
import re
import io
import sys

import lcg
from lcg import Unit, UPoint, Color, UPercent
import pytis.output
import pytis.util
from functools import reduce

_ = pytis.util.translations('pytis-wx')

LEFT = lcg.HorizontalAlignment.LEFT
RIGHT = lcg.HorizontalAlignment.RIGHT
CENTER = lcg.HorizontalAlignment.CENTER
TOP = lcg.VerticalAlignment.TOP
BOTTOM = lcg.VerticalAlignment.BOTTOM
MIDDLE = lcg.VerticalAlignment.CENTER
HORIZONTAL = lcg.Orientation.HORIZONTAL
VERTICAL = lcg.Orientation.VERTICAL

def _something_to_lcg(something):
    if isinstance(something, basestring):
        result = lcg.TextContent(something)
    elif isinstance(something, (list, tuple)):
        result = lcg.Container([_something_to_lcg(s) for s in something], orientation=HORIZONTAL)
    elif isinstance(something, lcg.Content):
        result = something
    else:
        result = something.lcg()
    return result


def _color(color):
    if isinstance(color, (tuple, list)):
        color = lcg.Color(*color)
    elif isinstance(color, basestring):
        color = lcg.Color(color)
    return color


class _Mark(object):

    def __init__(self):
        self._lcg_result = None

    def lcg(self):
        """Return LCG content corresponding to the mark and its content.
        The return value is an 'lcg.Content' instance.
        """
        if self._lcg_result is None:
            self._lcg_result = self._lcg()
        # The result is later modified by LCG processing.
        # So we have to copy it to prevent reparent errors.
        return copy.copy(self._lcg_result)

    def _lcg(self):
        return lcg.Content()

    def _dimension(self, x):
        return x if x is None or isinstance(x, lcg.Unit) else lcg.UMm(x)


class _Container(_Mark):

    KWARGS = {}

    def __init__(self, *contents, **kwargs):
        """Definuj odstavec.

        Argumenty:

          contents -- obsah odstavce, značky a řetězce

        """
        super(_Container, self).__init__()
        self._contents = contents
        for k, v in self.KWARGS.items():
            try:
                v = kwargs[k]
                del kwargs[k]
            except KeyError:
                pass
            self.__dict__['arg_' + k] = v
        if kwargs:
            raise pytis.output.TemplateException(_("Invalid item arguments"),
                                                 self.__class__, tuple(kwargs.keys()))

    def contents(self):
        """Vrať obsah odstavce zadaný v konstruktoru."""
        return self._contents

    def _lcg_contents(self):
        return [_something_to_lcg(c) for c in self._contents]

    def _lcg(self):
        return lcg.Container(self._lcg_contents(), orientation=HORIZONTAL)


class Null(_Mark):
    """Nic.

    Použití této značky se předpokládá v místech, kde nemá být nic, tj. ani
    prázdný řetězec.  Příkladem takového místa, kde použití takové značky dává
    smysl, je patička stránky.

    """


class Nbsp(_Mark):
    """Značka reprezentující znak nezalomitelné mezery.

    Tento znak lze zapsat i v obyčejném stringovém zápisu textu jako ' '.
    Značka slouží jako alternativní forma zápisu.

    """

    def _lcg(self):
        return lcg.TextContent(u" ")


class Euro(_Mark):
    """Značka reprezentující znak měny Euro."""

    def _lcg(self):
        return lcg.TextContent(u"€")


class Pound(_Mark):
    """Značka reprezentující znak libry."""

    def _lcg(self):
        return lcg.TextContent(u"£")


class _HAlignContainer(_Container):
    _ALIGNMENT = None

    def _lcg(self):
        contents = self._lcg_contents()
        if len(contents) > 1:
            contents = [lcg.Container(contents, orientation=HORIZONTAL)]
        return lcg.Container(contents, halign=self._ALIGNMENT, orientation=VERTICAL)


class Center(_HAlignContainer):
    """Značka horizontálního vycentrování svého obsahu."""
    _ALIGNMENT = CENTER


class AlignLeft(_HAlignContainer):
    """Značka zarovnání svého obsahu vlevo."""
    _ALIGNMENT = LEFT


class AlignRight(_HAlignContainer):
    """Značka zarovnání svého obsahu vpravo."""
    _ALIGNMENT = RIGHT


class VCenter(_Container):
    """Značka vertikálního vycentrování svého obsahu."""

    def _lcg(self):
        return lcg.Container(self._lcg_contents(), valign=MIDDLE, orientation=HORIZONTAL)


class _Space(_Mark):

    # For backwards compatibility.  Use global constants directly.
    VERTICAL = VERTICAL
    HORIZONTAL = HORIZONTAL

    def __init__(self, size, orientation):
        super(_Space, self).__init__()
        self._size = size
        self._orientation = orientation

    def size(self):
        return self._size

    def orientation(self):
        return self._orientation

    def _lcg(self):
        if self._orientation == VERTICAL:
            mark = lcg.VSpace
        elif self._orientation == HORIZONTAL:
            mark = lcg.HSpace
        else:
            raise Exception('Unexpected orientation', self._orientation)
        size = self._dimension(self._size) or lcg.UAny(1)
        return mark(size)


class VSpace(_Space):
    """Značka prázdného vertikálního objektu dané šířky."""

    def __init__(self, height):
        """Inicializuj instanci.

        Argumenty:

          height -- požadovaná výška objektu v milimetrech, nezáporné číslo;
            může být i instance 'lcg.Unit';
            může být též 'None', v kterémžto případě objekt bude mít největší
            rozumnou výšku, přičemž \"největší rozumná výška\" není nijak
            exaktně definována

        """
        super(VSpace, self).__init__(height, VERTICAL)


class HSpace(_Space):
    """Značka prázdného horizontálního objektu dané šířky."""

    def __init__(self, width):
        """Inicializuj instanci.

        Argumenty:

          width -- požadovaná šířka objektu v milimetrech, nezáporné číslo;
            může být i instance 'lcg.Unit';
            může být též 'None', v kterémžto případě objekt bude mít největší
            rozumnou šířku, přičemž \"největší rozumná šířka\" není nijak
            exaktně definována

        """
        super(HSpace, self).__init__(width, HORIZONTAL)


class HLine(_Mark):
    """Značka horizontální čáry vyplňující celý dostupný prostor."""

    def __init__(self, thickness=None, color=None, **kwargs):
        """Arguments:

        thickness -- Thickness of the line as 'lcg.Unit' or None for the
          default thickness.  May be also passed directly as int or float which
          will be automatically converted to 'UMm' (milimeters).
        color -- line color as 'lcg.Color' or None for the (media dependent)
          default color.  May be also passed directly as HTML string or a tuple
          of ints or floats which will be automatically converted to 'Color'
          instance with given constructor argument(s).

        """
        self._thickness = self._dimension(thickness)
        self._color = _color(color)
        super(HLine, self).__init__()

    def _lcg(self):
        return lcg.HorizontalSeparator(thickness=self._thickness, color=self._color)


class Paragraph(_Container):
    """Značka odstavce."""
    KWARGS = {'noindent': False}

    def _lcg(self):
        return lcg.Paragraph(self._lcg_contents(), noindent=self.arg_noindent)


class List(_Container):
    """Seznam.

    Položky seznamu jsou uvozeny značkou specifikovanou klíčovým argumentem
    konstruktoru 'mark'.  Není-li tento argument zadán, položky nemají žádné
    zvláštní uvození.

    """
    BULLET_MARK = 'BULLET_MARK'
    """Položky seznamu uvozené puntíkem."""
    NUMBER_MARK = 'NUMBER_MARK'
    """Položky seznamu uvozené vzestupnými arabskými číslicemi od 1."""
    ALPHA_MARK = 'ALPHA_MARK'
    """Položky seznamu uvozené písmeny a), b), c), ..."""
    UPPER_ALPHA_MARK = 'UPPER_ALPHA_MARK'
    """Položky seznamu uvozené písmeny A), B), C), ..."""


    KWARGS = {'mark': None}

    def _lcg(self):
        if self.arg_mark == self.BULLET_MARK:
            order = None
        elif self.arg_mark == self.NUMBER_MARK:
            order = lcg.ItemizedList.NUMERIC
        elif self.arg_mark == self.ALPHA_MARK:
            order = lcg.ItemizedList.LOWER_ALPHA
        elif self.arg_mark == self.UPPER_ALPHA_MARK:
            order = lcg.ItemizedList.UPPER_ALPHA
        else:
            raise Exception('Unexpected list type', self.arg_mark)
        return lcg.ItemizedList(self._lcg_contents(), order=order)


class NewPage(_Mark):
    """Značka nové stránky."""

    def _lcg(self):
        return lcg.NewPage()


class PageNumber(_Mark):
    """Značka generující číslo aktuální stránky.

    Vygenerované číslo je pořadové číslo počínaje od 1, zapsané arabskými
    číslicemi.

    Tato značka smí být použita jen uvnitř hlaviček a patiček stránek.

    """

    def __init__(self, total=False):
        """Inicializuj instanci.

        Argumenty:

          total -- právě když je pravdivé, uveď kromě aktuální stránky
            i celkový počet stran

        """
        super(PageNumber, self).__init__()
        self._total = total

    def total(self):
        """Vrať hodnotu argumentu 'total' z konstruktoru."""
        return self._total

    def _lcg(self):
        if self._total:
            result = lcg.PageNumber(total=True, separator='/')
        else:
            result = lcg.PageNumber(total=False)
        return result


class Bold(_Container):
    """Tučně sázený text."""

    def _lcg(self):
        return lcg.Container(self._lcg_contents(), presentation=lcg.Presentation(bold=True))


class Italic(_Container):
    """Text sázený kurzívou."""

    def _lcg(self):
        return lcg.Container(self._lcg_contents(), presentation=lcg.Presentation(italic=True))


class Roman(_Container):
    """Standardně sázený text."""

    def _lcg(self):
        return lcg.Container(self._lcg_contents(), presentation=lcg.Presentation(italic=False))


class Subscript(_Container):
    """Dolní index."""

    def _lcg(self):
        return lcg.Container(lcg.Subscript(self._lcg_contents()))


class Superscript(_Container):
    """Horní index."""

    def _lcg(self):
        return lcg.Container(lcg.Superscript(self._lcg_contents()))


class FontSize(_Container):
    """Standardně sázený text."""

    def __init__(self, size, *contents):
        """Definuj text.

        Argumenty:

          size -- float určující relativní velikost fontu vzhledem
            k aktuálnímu, na daném místě použitému fontu; 1.0 značí shodnou
            velikost
          contents -- značky, na které je změna velikosti fontu aplikována

        """
        super(FontSize, self).__init__(*contents)
        self._size = size

    def size(self):
        """Vrať velikost zadanou v konstruktoru."""
        return self._size

    def _lcg(self):
        return lcg.Container(self._lcg_contents(),
                             presentation=lcg.Presentation(font_size=self._size))


class FontFamily(_Container):
    """Text sázený zadanou rodinou fontu.

    Tato značka zároveň nastaví základní řez fontu.

    """

    PROPORTIONAL = 'PROPORTIONAL'
    """Standardní proporcionální font (například Times)."""
    SANS_SERIF = 'SANS_SERIF'
    """Standardní bezpatkový proporcionální font(například Helvetica)."""
    FIXED_WIDTH = 'FIXED_WIDTH'
    """Standardní neproporcionální font (například Courier)."""

    def __init__(self, family, *contents):
        """Definuj text.

        Argumenty:

          family -- jedna z konstant této třídy určující rodinu použitého fontu
          contents -- značky, na které je změna velikosti fontu aplikována

        """
        super(FontFamily, self).__init__(*contents)
        self._family = family

    def family(self):
        """Vrať rodinu fontu zadanou v konstruktoru."""
        return self._family

    def _lcg(self):
        if self._family == self.PROPORTIONAL:
            family = lcg.FontFamily.PROPORTIONAL
        elif self._family == self.SANS_SERIF:
            family = lcg.FontFamily.SANS_SERIF
        elif self._family == self.FIXED_WIDTH:
            family = lcg.FontFamily.FIXED_WIDTH
        return lcg.Container(self._lcg_contents(),
                             presentation=lcg.Presentation(font_family=family))


class _Group(_Container):
    KWARGS = dict(
        boxed=False,
        box_margin=None,
        box_radius=None,
        box_width=None,
        box_color=None,
        box_mask=None,
        padding=None,
        padding_top=None,
        padding_bottom=None,
        padding_left=None,
        padding_right=None,
        spacing=None,
        width=None,
        height=None,
        balance=None,
        halign=LEFT,
        valign=TOP,
        color=None,
        background=None,
    )

    def _orientation(self):
        pass

    def _lcg(self):
        def coalesce(a, b):
            return a if a is not None else b
        padding = self.arg_padding
        if padding is not None:
            if not isinstance(padding, (tuple, list)):
                padding = (padding, padding, padding, padding)
            elif len(padding) == 2:
                padding = (padding[0], padding[1], padding[0], padding[1])
            padding = [self._dimension(coalesce(x, y))
                       for x, y in zip((self.arg_padding_top,
                                        self.arg_padding_right,
                                        self.arg_padding_bottom,
                                        self.arg_padding_left),
                                       padding)]
        presentation = dict(
            font_color=_color(self.arg_color),
            background_color=_color(self.arg_background),
        )
        if self.arg_boxed:
            presentation.update(
                boxed=True,
                box_margin=self._dimension(self.arg_box_margin),
                box_radius=self._dimension(self.arg_box_radius),
                box_width=self._dimension(self.arg_box_width),
                box_color=_color(self.arg_box_color),
                box_mask=self.arg_box_mask,
            )
        contents = self._lcg_contents()
        orientation = self._orientation()
        if self.arg_spacing:
            if orientation == VERTICAL:
                space = [VSpace(self.arg_spacing).lcg()]
            else:
                space = [HSpace(self.arg_spacing).lcg()]
            contents = reduce(lambda a, b: a + space + [b], contents[1:], contents[0:1])
        return lcg.Container(contents, orientation=orientation,
                             width=self._dimension(self.arg_width),
                             height=self._dimension(self.arg_height),
                             padding=padding,
                             presentation=lcg.Presentation(**presentation),
                             halign=self.arg_halign,
                             valign=self.arg_valign)


class HGroup(_Group):
    """Group of horizontally arranged items.

    Unlike when multiple items are grouped simply into a tuple, this element
    allows specification of various parameters how the items are arranged:

      boxed -- iff true, the group will have a box around
      box_margin -- space between the box and the content as 'Unit' instance or None
      box_width -- box line width as 'Unit' instance or None
      box_color -- box line color as 'Color' instance.  May be also passed
        directly as HTML string or a tuple of ints or floats which will be
        automatically converted to 'Color' instance with given constructor
        argument(s).
      box_radius -- if not None and not 0, box corners will be rounded
        with given radius as 'Unit' instance
      box_mask -- Mask of visible box sides as a sequence of 4 bools (top,
        right, bottom, left) or None for all sides visible.
      width -- explicit output width as 'Unit' instance or None for default
        sizing.  Applies to the outer dimension of the whole box including any
        padding and/or box_margin.
      height -- explicit output width as 'Unit' instance or None for default
        sizing.  Applies to the outer dimension of the whole box including any
        padding and/or box_margin.
      padding -- space around group contents.  This space is added to
        box_margin for a boxed group, but there are major differences between
        box_margin and padding.  Box margin only applies to boxed groups and is
        ignored for non boxed groups.  Padding applies to both.  Box margin is
        inherited to any inner boxed groups which don't explicitly override it.
        Padding only applies to the group for which it is defined.  Morover,
        padding can be set differently for each side using 'padding_top',
        'padding_bottom', 'padding_left', 'padding_right'.  None or 'Unit'
        instance.
      padding_top -- overrides 'padding' for the top side.
      padding_bottom -- overrides 'padding' for the bottom side.
      padding_left -- overrides 'padding' for the left side.
      padding_right -- overrides 'padding' for the right side.
      spacing -- space between the individual group items. None or 'Unit'
        instance.
      balance -- není-li 'None', jedná se o tuple o počtu prvků shodném
        s počtem prvků skupiny, udávající vzájemný poměr velikostí pořadím
        odpovídajících prvků.  Velikost prvků ve směru orientace skupiny
        (horizontální nebo vertikální) bude patřičně upravena, velikost prvků
        s udaným poměrem 0 zůstane nezměněna.  V LCG tisku není tento argument
        podporován.
      halign -- horizontal alignment of group items.  One of LEFT, RIGHT, CENTER.
      valign -- vertical alignment of group items.  One of TOP, BOTTOM, MIDDLE.
      color -- text color as a 'Color' instance.  May be also passed directly
        as HTML string or a tuple of ints or floats which will be automatically
        converted to 'Color' instance with given constructor argument(s).
      background -- background color as a 'Color' instance, string or tuple (as
        for 'color').

    All argumentrs which accept a Unit instance may be also given directly as
    int or float which will be automatically converted to Umm (given dimension
    in mimimeters).

    """

    def _orientation(self):
        return HORIZONTAL


class VGroup(_Group):
    """Group of vertically arranged items.

    All arguments of HGroup may be also used for VGroup.  Only the direction in
    which the items are arranged is vertical.

    """

    def _orientation(self):
        return VERTICAL


class Group(_Group):

    """Depracated: Use HGroup or VGroup instead."""
    KWARGS = dict(_Group.KWARGS, vertical=False)

    def _orientation(self):
        return VERTICAL if self.arg_vertical else HORIZONTAL


class Document(_Container):
    """Samostatná část dokumentu se samostatně číslovanými stránkami.

    Dokument je uvozen, resp. ukončen, obvyklou hlavičkou, resp. patičkou.
    Stránky uvnitř dokumentu mají své vlastní číslování počínaje od 1.  Pomocí
    této značky lze vytvořit \"několik dokumentů v jednom\".

    Tato značka smí být použita pouze jako značka obalující celý obsah
    dokumentu nebo jako značka všech prvků sekvence definující kompletní obsah
    dokumentu.  Není-li značka použita vůbec, je jí celý dokument obalen
    automaticky.

    Značce lze předat následující argumenty:

      page_header -- značky specifikující hlavičku každé stránky nebo 'None'
        indikující implicitní hlavičku stránky danou specifikační funkcí
        'page_header'
      page_footer -- značky specifikující patičku každé stránky nebo 'None'
        indikující implicitní patičku stránky danou specifikační funkcí
        'page_footer'
      first_page_header -- značky specifikující hlavičku první stránky nebo
        'None' indikující hlavičku stránky danou specifikační funkcí
        'first_page_header'

    """
    KWARGS = {'page_header': None,
              'page_footer': None,
              'first_page_header': None,
              'page_background': None,
              'presentation': None}
    _counter = pytis.util.Counter()

    def lcg_document(self, **kwargs):
        """Return the document(s) as an 'lcg.ContentNode' instance.

        Arguments:

          kwargs -- kwargs to pass to 'lcg.ContentNode' constructor

        """
        def arg(definition):
            if definition is None or isinstance(definition, lcg.Content):
                value = definition
            else:
                value = definition.lcg()
            return value
        content_id = 'pytismarkup%d' % (self._counter.next(),)
        return lcg.ContentNode(id=content_id, title=' ',  # Avoid printing the id.
                               # Avoid the extra container generated by the parent class.
                               content=self.lcg().content(),
                               page_header=arg(self.arg_page_header),
                               page_footer=arg(self.arg_page_footer),
                               first_page_header=arg(self.arg_first_page_header),
                               page_background=arg(self.arg_page_background),
                               presentation=self.arg_presentation,
                               **kwargs)


class Table(_Mark):
    """Nejvýše jednostránková tabulka s předpřipravenými daty.

    Pro formátování větších tabulek lze použít značku 'LongTable'.

    """
    class Column(object):
        """Specifikační třída sloupce tabulky."""

        ALIGN_LEFT = 'ALIGN_LEFT'
        """Zarovnání obsahu sloupce nalevo."""
        ALIGN_CENTER = 'ALIGN_CENTER'
        """Centrování obsahu sloupce."""
        ALIGN_RIGHT = 'ALIGN_RIGHT'
        """Zarovnání obsahu sloupce napravo."""

        def __init__(self, label=None, width=None, alignment=ALIGN_LEFT,
                     label_alignment=ALIGN_CENTER):
            """Inicializuj instanci.

            Argumenty:

              label -- záhlaví sloupce, formátovací element; smí být též
                'None', v kterémžto případě sloupec nemá žádné záhlaví a
                nemá-li žádný sloupec záhlaví, nebude záhlaví vůbec generováno
              width -- šířka sloupce ve znacích, přirozené číslo nebo 'None'
                (v kterémžto případě je šířka sloupce určena automaticky)
              alignment -- způsob zarovnání obsahu sloupce, jedna z 'ALIGN_*'
                konstant třídy
              label_alignment -- způsob zarovnání hlavičky sloupce, jedna z 'ALIGN_*'
                konstant třídy

            """
            self.label = label
            self.width = width
            self.alignment = alignment
            self.label_alignment = label_alignment

    def __init__(self, columns, *data, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          columns -- sekvence instancí třídy 'Table.Column', musí mít
            nejméně 1 a nejvýše 26 prvků
          data -- neprázdná sekvence neprázdných sekvencí, odpovídá řádkům
            (vnější sekvence) formátovaným do sloupců (vnitřní sekvence);
            všechny vnitřní sekvence musí mít délku shodnou s délkou argumentu
            'columns'.  Místo vnitřní sekvence smí být kdekoliv 'None', na
            takovém místě bude místo řádku tabulky vygenerována horizontální
            čára.
          vmargin (jako klíčovaný argument) -- je-li 'None', je mezi řádky
            tabulky ponechána implicitní mezera; je-li 0, není mezi řádky
            tabulky žádná mezera; jiné hodnoty argumentu nejsou povoleny
          compact (jako klíčovaný argument) -- If True, suppress default
            padding in all table cells (including headings).  Use containers to
            add custom padding.

        """
        vmargin, compact = kwargs.pop('vmargin', None), kwargs.pop('compact', False)
        super(Table, self).__init__(**kwargs)
        assert isinstance(columns, (tuple, list))
        assert vmargin in (None, 0), vmargin
        assert isinstance(compact, bool), compact
        if len(columns) > 26:
            raise pytis.output.TemplateException(_("More than 26 columns in a table"))
        self._columns = columns
        self._data = data
        self._vmargin = vmargin
        self._compact = compact

    def columns(self):
        """Vrať specifikaci sloupců tabulky zadanou v konstruktoru."""
        return self._columns

    def data(self):
        """Vrať obsah argumentu 'data' zadaného v '__init__()'."""
        return self._data

    def vmargin(self):
        """Vrať hodnotu argumentu 'vmargin' zadaného v '__init__()'."""
        return self._vmargin

    def compact(self):
        """Vrať hodnotu argumentu 'compact' zadanou v '__init__()'."""
        return self._compact

    def _lcg_presentation(self):
        if self._vmargin == 0:
            margin = lcg.UMm(0)
        else:
            margin = lcg.UFont(0.2)
        return lcg.Presentation(separator_margin=margin)

    def _lcg_table_data(self):
        return self._data

    def _lcg_table(self, table_rows, column_widths):
        return lcg.Table(table_rows, column_widths=column_widths,
                         presentation=self._lcg_presentation(),
                         halign=LEFT, compact=self._compact)

    def _lcg(self):
        table_rows = []
        if any([c.label is not None for c in self._columns]):
            cells = []
            for column in self._columns:
                label = _something_to_lcg(column.label)
                if column.label_alignment == column.ALIGN_LEFT:
                    alignment = LEFT
                elif column.label_alignment == column.ALIGN_CENTER:
                    alignment = CENTER
                elif column.label_alignment == column.ALIGN_RIGHT:
                    alignment = RIGHT
                else:
                    raise Exception('Unknown label alignment', column.label_alignment)
                cells.append(lcg.TableHeading(label, align=alignment))
            table_rows.append(lcg.TableRow(cells))
        alignments = []
        for column in self._columns:
            if column.alignment == column.ALIGN_LEFT:
                alignment = lcg.TableCell.LEFT
            elif column.alignment == column.ALIGN_CENTER:
                alignment = lcg.TableCell.CENTER
            elif column.alignment == column.ALIGN_RIGHT:
                alignment = lcg.TableCell.RIGHT
            else:
                raise Exception('Unknown label alignment', column.alignment)
            alignments.append(alignment)
        for data_row in self._lcg_table_data():
            if data_row is None:
                table_rows.append(lcg.HorizontalSeparator())
            else:
                cells = []
                for column, cell_data, alignment in zip(self._columns, data_row, alignments):
                    cell_data = _something_to_lcg(cell_data)
                    cells.append(lcg.TableCell(cell_data, align=alignment))
                table_rows.append(lcg.TableRow(cells))
        column_widths = []
        for column in self._columns:
            if column.width is None:
                width = None
            elif isinstance(column.width, lcg.Unit):
                width = column.width
            else:
                width = lcg.USpace(column.width)
            column_widths.append(width)
        return self._lcg_table(table_rows, column_widths)


class LongTable(Table):
    """Tabulka, potenciálně vícestránková.

    Tabulka je dána svými sloupci a funkcí generující její řádky.  Délka
    tabulky není omezena, překročí-li tabulka hranice stránky, pokračuje na
    stránkách další, přičemž na každé další stránce je uvozena stejnou
    hlavičkou jako na svém začátku.

    Pro krátké tabulky s víceméně fixním počtem řádků lze použít značku
    'Table'.

    """
    class Column(Table.Column):
        """Specifikační třída sloupce dlouhé tabulky."""

        def __init__(self, label, width, **kwargs):
            """Stejné jako v předkovi až na argumenty 'label' a 'width'.

            Argumenty 'label' a 'width' jsou povinné a nesmí být 'None'.

            """
            Table.Column.__init__(self, label, width, **kwargs)

    def __init__(self, columns, row_generator, row_generator_init=None,
                 separator_height=0, line_separator_height=0,
                 separator_margin=0, line_separator_margin=0,
                 compact=False):
        """Inicializuj instanci.

        Argumenty:

          columns -- sekvence instancí třídy 'LongTable.Column', musí mít
            nejméně 1 a nejvýše 26 prvků
          row_generator -- funkce generující řádky datové tabulky.  Funkce je
            opakovaně volána, bez argumentů, a musí vždy vrátit nový řádek
            tabulky, v podobě sekvencí formátovacích elementů odpovídajících
            sloupcům tabulky, včetně pořadí.  Pokud funkce vrátí 'None', je
            tabulka ukončena.
          row_generator_init -- funkce volaná bez argumentů před prvním voláním
            'row_generator', nebo 'None'
          separator_height -- tloušťka oddělovací čáry mezi záhlavím tabulky a
            jejími řádky v bodech, jako float nebo 'Unit'
          line_separator_height -- tloušťka oddělovací čáry za každým řádkem
            tabulky v bodech, jako float nebo 'Unit'
          separator_margin -- vzdálenost oddělovací čáry záhlaví tabulky od
            záhlaví v bodech, jako float nebo 'Unit'
          line_separator_margin -- vzdálenost oddělovací čáry od každého řádku
            tabulky v bodech, jako float nebo 'Unit'

        """
        super(LongTable, self).__init__(columns, compact=compact)
        assert callable(row_generator), row_generator
        assert row_generator_init is None or callable(row_generator_init), row_generator_init
        assert isinstance(separator_height, (float, int, long, Unit)), separator_height
        assert isinstance(line_separator_height, (float, int, long, Unit)), line_separator_height
        assert isinstance(separator_margin, (float, int, long, Unit)), separator_margin
        assert isinstance(line_separator_margin, (float, int, long, Unit)), line_separator_margin
        self._row_generator = row_generator
        self._row_generator_init = row_generator_init

        def convert(size):
            if not isinstance(size, Unit):
                size = UPoint(size)
            return size
        self._separator_height = convert(separator_height)
        self._line_separator_height = convert(line_separator_height)
        self._separator_margin = convert(separator_margin)
        self._line_separator_margin = convert(line_separator_margin)

    def row_generator(self):
        """Vrať funkci generující řádku tabulky zadanou v konstruktoru."""
        return self._row_generator

    def row_generator_init(self):
        """Vrať argument 'row_generator_init' metody '__init__()'."""
        return self._row_generator_init

    def separator_height(self):
        """Vrať tloušťku oddělovací čáry záhlaví zadanou v konstruktoru."""
        return self._separator_height

    def line_separator_height(self):
        """Vrať tloušťku oddělovací čáry řádků zadanou v konstruktoru."""
        return self._line_separator_height

    def separator_margin(self):
        """Vrať vzdálenost oddělovací čáry záhlaví zadanou v konstruktoru."""
        return self._separator_margin

    def line_separator_margin(self):
        """Vrať vzdálenost oddělovací čáry řádků zadanou v konstruktoru."""
        return self._line_separator_margin

    def _lcg_presentation(self):
        presentation = super(LongTable, self)._lcg_presentation()
        presentation.separator_height = self._line_separator_height
        presentation.separator_margin = self._line_separator_margin
        presentation.header_separator_height = self._separator_height
        presentation.header_separator_margin = self._separator_margin
        return presentation

    def _lcg_table_data(self):
        if self._row_generator_init is not None:
            self._row_generator_init()
        data = []
        while True:
            row = self._row_generator()
            if row is None:
                break
            data.append(row)
        return data

    def _lcg_table(self, table_rows, column_widths):
        return lcg.Table(table_rows, column_widths=column_widths, long=True,
                         presentation=self._lcg_presentation(), halign=LEFT,
                         compact=self._compact)


class Image(_Mark):
    """EPS image."""

    def __init__(self, file_or_data, standalone=True, width=None, height=None):
        """Arguments:

          file_or_data -- image file name relative to the directory given by
            configuration option 'print_spec_dir' or image as memory data
            (bytes).
          width -- explicit output image width as lcg.Unit subclass instance.
            If only one of width/height is specified (not None), the other
            dimension is computed to maintain the original image proportions.
          height -- output image height as lcg.Unit subclass instance.  See
            also 'width'.
          standalone -- if True, the image is drawn separately.  If False, the
            image is embedded within the surrounding paragraph text (if any).
            When inside paragraph (standalone is False) and size is not given
            explicitly through 'width' and/or 'height' the output image may be
            resized automatically to fit inside the current context.

        """
        if sys.version_info[0] == 2:
            assert isinstance(file_or_data, (basestring, buffer, bytes)), type(file_or_data)
        else:
            assert isinstance(file_or_data, (str, bytes)), type(file_or_data)
        super(Image, self).__init__()
        self._standalone = standalone
        self._width = width
        self._height = height
        if sys.version_info[0] == 2 and isinstance(file_or_data, buffer):
            self._file_name = '_mem_image'
            self._data = bytes(file_or_data)
        elif (sys.version_info[0] == 2 and isinstance(file_or_data, unicode) or
              sys.version_info[0] > 2 and isinstance(file_or_data, str)):
            self._file_name = file_or_data
            self._data = None
        else:
            self._file_name = '_mem_image'
            self._data = file_or_data

    def file_name(self):
        """Vrať jméno souboru zadané v konstruktoru."""
        return self._file_name

    def _lcg(self):
        if self._data:
            class InlineImage(lcg.InlineImage):

                def __init__(self, data, **kwargs):
                    self._data = data
                    super(InlineImage, self).__init__("_mem_image", **kwargs)

                def image(self, context):
                    # We need to return the image as a stream object (necessary
                    # for reportlab in PDFExporter) but we also need to satisfy
                    # the assertion in the contructor of 'lcg.export.pdf.Image'.
                    class Image(io.BytesIO, lcg.Image):

                        def src_file(self):
                            return self

                    return Image(self._data)
            return InlineImage(self._data,
                               width=self._dimension(self._width),
                               height=self._dimension(self._height),
                               standalone=self._standalone)
        else:
            file_name = os.path.join(pytis.config.print_spec_dir, self._file_name)
            return lcg.InlineImage(lcg.Image(file_name, src_file=file_name),
                                   width=self._dimension(self._width),
                                   height=self._dimension(self._height),
                                   standalone=self._standalone)


class StructuredText(_Mark):
    """LCG structured text."""

    _ITERATOR_MATCHER = re.compile(r'\|([^|]*\${(?:data\.|[^}]+\.(?:data|codebook)\.))')

    def __init__(self, text):
        """Arguments:

          text -- the structured text; basestring

        """
        super(StructuredText, self).__init__()
        text = self._ITERATOR_MATCHER.sub(r'| @iterate\1', text)
        self._text = text
        self._parameters = {}

    def _lcg(self):
        parser = lcg.Parser()
        parameters = {}
        result = lcg.Container(parser.parse(self._text, parameters))
        self._parameters = {}
        for k, v in parameters.items():
            self._parameters[k] = v
        if 'first_page_header' not in self._parameters and 'page_header' in self._parameters:
            self._parameters['first_page_header'] = self._parameters['page_header']
        return result

    def parameters(self):
        """Return parameters defined in the structured text."""
        return self._parameters
