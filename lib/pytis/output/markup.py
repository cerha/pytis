# -*- coding: utf-8 -*-

# Formátovací prvky
#
# Copyright (C) 2002-2015 Brailcom, o.p.s.
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

"""Definice formátovacích značek.

Tento soubor definuje prvky sestavující výsledný tiskový dokument.  Prvky
jednak definují formátování a jednak umožňují vkládat externí data do
dokumentu.  Jsou používány v definičních souborech výstupních sestav.

Každá veřejná třída modulu odpovídá formátovací značce použitelné v definici
dokumentu.  Kromě těchto tříd je ještě možno používat obyčejné řetězce, které
budou vloženy tak, jak jsou, včetně mezer a odřádkování, a sekvence obsahující
elementy ke spojení dohromady.

"""

import collections
import copy
import os
import re

import lcg
from lcg import Unit, UPoint, Color
import pytis.output
import pytis.util
from pytis.util import some, super_

_ = pytis.util.translations('pytis-wx')


def _something_to_lcg(something):
    if isinstance(something, basestring):
        result = lcg.TextContent(something)
    elif isinstance(something, (list, tuple,)):
        result = lcg.Container([_something_to_lcg(s) for s in something],
                               orientation=lcg.Orientation.HORIZONTAL)
    elif isinstance(something, lcg.Content):
        result = something
    else:
        result = something.lcg()
    return result

def _color(color):
    if isinstance(color, (tuple, list)):
        color = lcg.Color(*color)
    if isinstance(color, basestring):
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
                                                 self.__class__, kwargs.keys())

    def contents(self):
        """Vrať obsah odstavce zadaný v konstruktoru."""
        return self._contents

    def _lcg_contents(self):
        return [_something_to_lcg(c) for c in self._contents]

    def _lcg(self):
        return lcg.Container(self._lcg_contents(), orientation=lcg.Orientation.HORIZONTAL)


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
            contents = [lcg.Container(contents, orientation=lcg.Orientation.HORIZONTAL)]
        return lcg.Container(contents, halign=self._ALIGNMENT, orientation=lcg.Orientation.VERTICAL)

class Center(_HAlignContainer):
    """Značka horizontálního vycentrování svého obsahu."""
    _ALIGNMENT = lcg.HorizontalAlignment.CENTER

class AlignLeft(_HAlignContainer):
    """Značka zarovnání svého obsahu vlevo."""
    _ALIGNMENT = lcg.HorizontalAlignment.LEFT

class AlignRight(_HAlignContainer):
    """Značka zarovnání svého obsahu vpravo."""
    _ALIGNMENT = lcg.HorizontalAlignment.RIGHT

class VCenter(_Container):
    """Značka vertikálního vycentrování svého obsahu."""
    def _lcg(self):
        return lcg.Container(self._lcg_contents(),
                             valign=lcg.VerticalAlignment.CENTER,
                             orientation=lcg.Orientation.HORIZONTAL)


class _Space(_Mark):

    VERTICAL = 'VERTICAL'
    HORIZONTAL = 'HORIZONTAL'

    def __init__(self, size, orientation):
        super(_Space, self).__init__()
        self._size = size
        self._orientation = orientation

    def size(self):
        return self._size

    def orientation(self):
        return self._orientation

    def _lcg(self):
        if self._orientation == self.VERTICAL:
            mark = lcg.VSpace
        elif self._orientation == self.HORIZONTAL:
            mark = lcg.HSpace
        else:
            raise Exception('Unexpected orientation', self._orientation)
        size = self._size
        if size is None:
            size = lcg.UAny(1)
        elif not isinstance(size, lcg.Unit):
            size = lcg.UMm(self._size)
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
        super(VSpace, self).__init__(height, self.VERTICAL)

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
        super(HSpace, self).__init__(width, self.HORIZONTAL)


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
        if thickness is not None and not isinstance(thickness, lcg.Unit):
            thickness = lcg.UMm(thickness)
        self._thickness = thickness
        self._color = _color(color)
        super(HLine, self).__init__()

    def _lcg(self):
        return lcg.HorizontalSeparator(thickness=self._thickness, color=self._color)

class Paragraph(_Container):
    """Značka odstavce."""
    def _lcg(self):
        return lcg.Paragraph(self._lcg_contents())

class List(_Container):
    """Seznam.

    Položky seznamu jsou uvozeny značkou specifikovanou klíčovým argumentem
    konstruktoru 'mark'.  Není-li tento argument zadán, položky nemají žádné
    zvláštní uvození.

    """
    NUMBER_MARK = 'NUMBER_MARK'
    """Položky seznamu uvozené vzestupnými arabskými číslicemi od 1."""
    BULLET_MARK = 'BULLET_MARK'
    """Položky seznamu uvozené puntíkem."""

    KWARGS = {'mark': None}

    def _lcg(self):
        if self.arg_mark == self.BULLET_MARK:
            order = None
        elif self.arg_mark == self.NUMBER_MARK:
            order = lcg.ItemizedList.NUMERIC
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
        presentation = lcg.Presentation()
        presentation.bold = True
        return lcg.Container(self._lcg_contents(), presentation=presentation)

class Italic(_Container):
    """Text sázený kurzívou."""
    def _lcg(self):
        presentation = lcg.Presentation()
        presentation.italic = True
        return lcg.Container(self._lcg_contents(), presentation=presentation)

class Roman(_Container):
    """Standardně sázený text."""
    def _lcg(self):
        presentation = lcg.Presentation()
        presentation.bold = presentation.italic = False
        return lcg.Container(self._lcg_contents(), presentation=presentation)

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
        super_(FontSize).__init__(self, *contents)
        self._size = size

    def size(self):
        """Vrať velikost zadanou v konstruktoru."""
        return self._size

    def _lcg(self):
        presentation = lcg.Presentation()
        presentation.font_size = self._size
        return lcg.Container(self._lcg_contents(), presentation=presentation)

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
        super_(FontFamily).__init__(self, *contents)
        self._family = family

    def family(self):
        """Vrať rodinu fontu zadanou v konstruktoru."""
        return self._family

    def _lcg(self):
        presentation = lcg.Presentation()
        if self._family == self.PROPORTIONAL:
            family = lcg.FontFamily.PROPORTIONAL
        elif self._family == self.SANS_SERIF:
            family = lcg.FontFamily.SANS_SERIF
        elif self._family == self.FIXED_WIDTH:
            family = lcg.FontFamily.FIXED_WIDTH
        presentation.font_family = family
        return lcg.Container(self._lcg_contents(), presentation=presentation)

class _Group(_Container):
    KWARGS = dict(boxed=False,
                  box_margin=None,
                  box_radius=None,
                  box_width=None,
                  box_color=None,
                  padding=None,
                  padding_top=None,
                  padding_bottom=None,
                  padding_left=None,
                  padding_right=None,
                  spacing=None,
                  balance=None)

    def _orientation(self):
        pass

    def _lcg(self):
        def coalesce(a, b):
            return a if a is not None else b
        def unit(x):
            if x is not None and not isinstance(x, lcg.Unit):
                x = lcg.UMm(x)
            return x
        presentation = lcg.Presentation()
        padding_top = coalesce(self.arg_padding_top, self.arg_padding)
        padding_bottom = coalesce(self.arg_padding_bottom, self.arg_padding)
        padding_left = coalesce(self.arg_padding_left, self.arg_padding)
        padding_right = coalesce(self.arg_padding_right, self.arg_padding)
        if self.arg_boxed:
            presentation.boxed = True
            presentation.box_margin = unit(self.arg_box_margin)
            presentation.box_radius = unit(self.arg_box_radius)
            presentation.box_width = unit(self.arg_box_width)
            presentation.box_color = _color(self.arg_box_color)
        contents = self._lcg_contents()
        orientation = self._orientation()
        if orientation == lcg.Orientation.VERTICAL:
            if self.arg_spacing:
                space = [VSpace(self.arg_spacing).lcg()]
                contents = reduce(lambda a, b: a + space + [b], contents[1:], contents[0:1])
            if padding_top:
                contents.insert(0, VSpace(padding_top).lcg())
            if padding_bottom:
                contents.append(VSpace(padding_bottom).lcg())
            if padding_left or padding_right:
                group = HGroup(contents,
                               padding_left=padding_left,
                               padding_right=padding_right)
                contents = group.lcg()
        else:
            if self.arg_spacing:
                space = [HSpace(self.arg_spacing).lcg()]
                contents = reduce(lambda a, b: a + space + [b], contents[1:], contents[0:1])
            if padding_left:
                contents.insert(0, HSpace(padding_left).lcg())
            if padding_right:
                contents.append(HSpace(padding_right).lcg())
            if padding_top or padding_bottom:
                group = VGroup(contents,
                              padding_top=padding_top,
                              padding_bottom=padding_bottom)
                contents = group.lcg()
        return lcg.Container(contents, orientation=orientation,
                             presentation=presentation,
                             halign=lcg.HorizontalAlignment.LEFT,
                             valign=lcg.VerticalAlignment.TOP)


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
      padding -- space around group contents.  This space is added to
        box_margin for a boxed group, but there are major differences between
        box_margin and padding.  Box margin only applies to boxed groups and is
        ignored for non boxed groups.  Padding applies to both.  Box margin is
        inherited to any inner boxed groups which don't explicitly override it.
        Padding only applies to the group for which it is defined.  Morover,
        padding can be set differently for each side using 'padding_top',
        'padding_bottom', 'padding_left', 'padding_right'.  None means or
        'Unit' instance.
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

    All argumentrs which accept a Unit instance may be also given directly as
    int or float which will be automatically converted to Umm (given dimension
    in mimimeters).

    """
    def _orientation(self):
        return lcg.Orientation.HORIZONTAL


class VGroup(_Group):
    """Group of vertically arranged items.

    All arguments of HGroup may be also used for VGroup.  Only the direction in
    which the items are arranged is vertical.

    """
    def _orientation(self):
        return lcg.Orientation.VERTICAL


class Group(_Group):

    """Depracated: Use HGroup or VGroup instead."""
    KWARGS = dict(_Group.KWARGS, vertical=False)

    def _orientation(self):
        return lcg.Orientation.VERTICAL if self.arg_vertical else lcg.Orientation.HORIZONTAL


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
        return lcg.ContentNode(id=content_id, title=' ', # let's avoid printing the id
                               content=self.lcg(),
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
    class Column:
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

        """
        super(Table, self).__init__()
        assert not some(lambda c: c not in ('vmargin',), kwargs.keys())
        assert isinstance(columns, (tuple, list,))
        vmargin = kwargs.get('vmargin')
        assert vmargin in (None, 0)
        if len(columns) > 26:
            raise pytis.output.TemplateException(_("More than 26 columns in a table"))
        self._columns = columns
        self._data = data
        self._vmargin = vmargin

    def columns(self):
        """Vrať specifikaci sloupců tabulky zadanou v konstruktoru."""
        return self._columns

    def data(self):
        """Vrať obsah argumentu 'data' zadaného v '__init__()'."""
        return self._data

    def vmargin(self):
        """Vrať hodnotu argumentu 'vmargin' zadaného v '__init__()'."""
        return self._vmargin

    def _lcg_presentation(self):
        presentation = lcg.Presentation()
        if self._vmargin == 0:
            presentation.separator_margin = lcg.UMm(0)
        else:
            presentation.separator_margin = lcg.UFont(0.2)
        return presentation

    def _lcg_table_data(self):
        return self._data

    def _lcg_table(self, table_rows, column_widths):
        return lcg.Table(table_rows, column_widths=column_widths,
                         presentation=self._lcg_presentation(),
                         halign=lcg.HorizontalAlignment.LEFT)

    def _lcg(self):
        table_rows = []
        if any([c.label is not None for c in self._columns]):
            cells = []
            for column in self._columns:
                label = _something_to_lcg(column.label)
                if column.label_alignment == column.ALIGN_LEFT:
                    alignment = lcg.HorizontalAlignment.LEFT
                elif column.label_alignment == column.ALIGN_CENTER:
                    alignment = lcg.HorizontalAlignment.CENTER
                elif column.label_alignment == column.ALIGN_RIGHT:
                    alignment = lcg.HorizontalAlignment.RIGHT
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
                 separator_margin=0, line_separator_margin=0):
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
        super(LongTable, self).__init__(columns)
        assert isinstance(row_generator, collections.Callable), row_generator
        assert row_generator_init is None or callable(row_generator_init), row_generator_init
        assert isinstance(separator_height, (float, int, long, Unit,)), separator_height
        assert isinstance(line_separator_height, (float, int, long, Unit,)), line_separator_height
        assert isinstance(separator_margin, (float, int, long, Unit,)), separator_margin
        assert isinstance(line_separator_margin, (float, int, long, Unit,)), line_separator_margin
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
                         presentation=self._lcg_presentation(),
                         halign=lcg.HorizontalAlignment.LEFT)

class Image(_Mark):
    """EPS obrázek."""

    def __init__(self, file_name, standalone=True):
        """Inicializuj instanci.

        Argumenty:

          file_name -- jméno souboru obrázku, relativní k adresáři definovanému
            konfigurační volbou 'print_spec_dir'.
          standalone -- určuje, zda obrázek má být vykreslen jako samostatný
            (True) nebo jako součást textu (False).  Pokud je obrázek vykreslen
            jako součást textu, může být na výstupu upravena jeho velikost
            (větší obrázky budou automaticky zmenšeny).

        """
        super(Image, self).__init__()
        self._file_name = file_name
        self._standalone = standalone

    def file_name(self):
        """Vrať jméno souboru zadané v konstruktoru."""
        return self._file_name

    def _lcg(self):
        import config
        file_name = os.path.join(config.print_spec_dir, self._file_name)
        image = lcg.Image(file_name, src_file=file_name)
        return lcg.InlineImage(image, standalone=self._standalone)

class StructuredText(_Mark):
    """LCG structured text."""

    _ITERATOR_MATCHER = re.compile(r'\|([^|]*\${(?:data\.|[^}]+\.(?:data|codebook)\.))')

    def __init__(self, text):
        """
        Arguments:

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
