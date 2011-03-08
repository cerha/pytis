# -*- coding: utf-8 -*-

# Formátování výstupu
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2011 Brailcom, o.p.s.
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

from __future__ import unicode_literals

"""Formátování výstupu.

Modul má na starost formátování textu a dat pro výstup dle zadaných šablon.
Zpracovává specifikace v podobě značek definovaných v modulu 'markup'.  Šablony
mají podobu pythonových souborů, obsahujících následující funkce vracející
odpovídající specifikace:

  coding -- vrací kódování dokumentu, jedna z konstant třídy 'Coding';
    implicitní je 'Coding.ASCII'
  doc_header -- hlavička dokumentu; implicitně prázdná
  doc_footer -- zakončení dokumentu; implicitně prázdné
  page_header -- hlavička stránky; implicitně prázdná
  page_footer -- patička stránky; implicitně číslo stránky
  first_page_header -- hlavička první stránky; implicitně shodná
    s 'page_header'
  page_layout -- dictionary parametrů určujících velikosti stránky a jejích
    okrajů; klíči jsou 'PAGE_*' konstanty modulu, hodnoty jsou popsány
    v dokumentaci těchto konstant
  background -- pozadí stránky; implicitně prázdné bílé
  body -- obsah dokumentu samotného; tato funkce musí být povinně přítomna

Hlavní třídou modulu je třída 'Formatter'.  Ta zajišťuje načtení a zpracování
šablon a doručení zformátovaných dat.

"""

import cStringIO
import codecs
import config
import functools
import operator
import os
import re
import string
import thread
import UserList

import pytis.data
from pytis.output import *
import pytis.presentation


class Coding:
    """Výčtová třída podporovaných kódování."""
    ASCII = 'ASCII'
    LATIN2 = 'LATIN2'


PAGE_WIDTH = 'pwidth'
"""Šířka stránky, instance třídy 'Unit'."""
PAGE_HEIGHT = 'pheight'
"""Výška stránky, instance třídy 'Unit'."""
PAGE_TOP_MARGIN = 'top_margin'
"""Velikost horního okraje stránky, instance třídy 'Unit'."""
PAGE_BOTTOM_MARGIN = 'bottom_margin'
"""Velikost dolního okraje stránky, instance třídy 'Unit'."""
PAGE_LEFT_MARGIN = 'left_margin'
"""Velikot levého okraje stránky, instance třídy 'Unit'."""
PAGE_RIGHT_MARGIN = 'right_margin'
"""Velikost pravého okraje stránky, instance třídy 'Unit'."""
PAGE_LANDSCAPE_MODE = 'landscape_mode'
"""Právě když je pravdivé, bude zaměněna výška a šířka stránky."""


# Toto je mimo LoutFormatter jednak pro přehlednost, a hlavně pak aby nebyl
# maten emacsový mód.
_INTRO = '''
### Basic includes to get the most important things defined

@SysInclude { langdefs }
@SysInclude { bsf } @Use { @BasicSetup }
@SysDatabase @FontDef { fontdefs }
@SysInclude { tbl }

### Our utility definitions

macro " " { ~ }

def @smallmargin { 0.3f }

def @margined
  named margin { @smallmargin }
  right y
{
  { //margin ||margin y ||margin //margin }
}

def @gmargined
  named margin { @smallmargin }
  right y
{
  { ||margin { //margin y /margin } ||margin }
}

def @box   # standard box doesn\'t work well with @HScale inside
  named margin { @smallmargin }
  right y
{
  {
    0 0 moveto
    0 ysize lineto
    xsize ysize lineto
    xsize 0 lineto
    closepath
    stroke
  }
  @Graphic @margined y
}

def @vline
  named placement { left }
  import @PSLengths named linewidth { 0.5p }
  named margin { 0p }
  right y
{
  macro @stroke { linewidth setlinewidth stroke }
  macro @leftline { 0 0 moveto 0 ysize lineto @stroke }
  macro @rightline { xsize 0 moveto xsize ysize lineto @stroke }
  
  placement @Case
  {
    right @Yield
      { { @rightline } @Graphic { y ||margin } ||margin }
    both @Yield
      { ||margin { @leftline
                   @rightline
                 } @Graphic { ||margin y ||margin}
        ||margin
      }
    else @Yield { ||margin { @leftline } @Graphic { ||margin y } }
  }
}

def @hline
  named placement { top }
  import @PSLengths named linewidth { 0.5p }
  named margin { 0p }
  right y
{
  macro @stroke { linewidth setlinewidth stroke }
  macro @topline { 0 ysize moveto xsize ysize lineto @stroke }
  macro @bottomline { 0 0 moveto xsize 0 lineto @stroke }

  placement @Case
  {
    bottom @Yield
      { { @bottomline } @Graphic { y //margin } || //margin }
    both @Yield
      { //margin
        {
          @bottomline
          @topline
        } @Graphic { //margin y //margin } ||
        //margin
      }
    else @Yield
      { //margin { @topline } @Graphic { //margin y } || }
  }
}

def @gvline right y { @vline @gmargined y }
def @ghline right y { @hline @gmargined y }

def @vgroup
  named boxed { no }
  right y
{
  boxed @Case {
    yes @Yield @hline placement { bottom } { @vline placement { both } { y } }
    else @Yield y
  }
}
def @hgroup
  named boxed { no }
  right y
{
  boxed @Case {
    yes @Yield @vline placement { right } { @hline placement { both } { y } }
    else @Yield y
  }
}

macro @vfill { // 1.0f @High //0io @VExpand {} // }
macro @hfill { || @HExpand {} || }

def @cell
  left width
  named header { no }
  named indent { left }
  right y
{
  def @emphasis
    right y
  {
    header @Case
    {
      yes @Yield { @B { y } }
      else @Yield y
    }
  }
  def @indentation
    right y
  {
    indent @Case
    {
      right  @Yield { ||1rt   rragged @Break { y } }
      center @Yield { ||0.5rt cragged @Break { y } }
      else   @Yield { ||       ragged @Break { y } }
    }
  }
  { width } @Wide { ||0.6f @indentation @emphasis { y } ||0.6f }
}

def @lcell
  left width
  named header { no }
  right y
{
  { width } @cell header { header } y
}

def @ccell
  left width
  named header { no }
  right y
{
  { width } @cell header { header } indent { center } { y }
}

def @rcell
  left width
  named header { no }
  right y
{
  { width } @cell header { header } indent { right } { y }
}

def @alignLeft
  right y
{
  { ragged } @Break y
}

def @alignRight
  right y
{
  { rragged } @Break y
}

macro @lineSkip { 1.10fx }

def @font
  left x
  right y
{
  x @Font { @lineSkip } @Break y
}

### Our output layout and formatting

def @textPlace { @Galley }

def @pageSet
  right y
{
  "LoutPageSet" @Graphic y
}

export @pageNumber
def @page
  named @Tag {}
  named @pageHeader {}
  named @pageFooter {}
  named @pageBackground {}
  named @topMargin { %(top_margin)s }
  named @bottomMargin { %(bottom_margin)s }
  named @leftMargin { %(left_margin)s }
  named @rightMargin { %(right_margin)s }
  named @width { %(pwidth)s }
  named @height { %(pheight)s }
  named @landscapeMode { %(landscape_mode)s }
  right @pageNumber
{
  macro @headerSep { 2vx }

  def @pageContents
  {
    @pageHeader
    //@headerSep
    @textPlace
    //@headerSep {} //1rt
    @pageFooter
  }

  def @body
  { //@topMargin ||@leftMargin
   { // || @pageBackground } @Background
   @HExpand { // @VExpand @pageContents }
   ||@rightMargin //@bottomMargin
  }

  @pageSet
  { @landscapeMode @Case { yes @Yield { @height @Wide @width @High @body }
                          else @Yield { @width @Wide @height @High @body }
                         }
  } 
}

export @pageNumber
def @text force into { @textPlace&&preceding }
  named @Tag {}
  right y
{
  def @pageNumber { @page&&preceding @Open { @pageNumber } }

  y
  //
  @page&&preceding @Tagged @Tag
}

export @numOfPages
def @document
  named @firstPageHeader right @pageNumber { %(fpheader)s }
  named @pageHeader right @pageNumber { %(pheader)s }
  named @pageFooter right @pageNumber { %(pfooter)s }
  named @Tag {}
{  
  def @numOfPages { { @page&&@Tag } @Open { @pageNumber } }

  def @pageBackground { %(pbackground)s }
  def @fontFamily { %(font)s }

  def @pageList
    right @pageNumber
  {
    @page
        @pageHeader { @pageHeader @pageNumber }
        @pageFooter { @pageFooter @pageNumber }
        @pageBackground { @pageBackground }
      @pageNumber
    //
    @pageList @Next @pageNumber
  }

  { @fontFamily Base 12p } @Font
  { ragged @lineSkip nohyphen } @Break
  { English } @Language
  { 0.0 0.0 0.0 setrgbcolor } @SetColor
  {
    @page
        @pageHeader { @firstPageHeader 1 }
        @pageFooter { @pageFooter 1 }
        @pageBackground { @pageBackground }
      1
    //
    @pageList 2
  }
  //
  @page&&preceding @Tagged @Tag
}

#def @pageNumber { @text&&preceding @Open { @pageNumber } }
def @numberOfPages { { @document&&preceding } @Open { @numOfPages } }

### The document

'''

class LoutFormatter(Tmpdir):
    """Formátovač textu pracující s využitím Lout."""

    # Pravidla formátování kódu pro Lout:
    #
    # - Generovaný text musí být přehledný, aby se v něm bylo možno vyznat a
    #   diagnostikovat případné chyby.
    # - Odsazovací krok je dvě mezery.
    # - Uvnitř každých složených závorek, pokud nejsou jednořádkové, je zvýšeno
    #   odsazení textu.  Závorky samy jsou odsazeny stejně jako okolní řádky.
    # - Složené závorky jsou obvykle obklopeny z každé strany práve jednou
    #   mezerou nebo znakem nového řádku.
    # - Funkce jsou obvykle na samostatném řádku, nikoliv stejném řádku jako
    #   okolní text.
    # - Levý argument funkce je obvykle na stejném řádku jako jméno funkce,
    #   pokud není moc dlouhý.
    # - Pravý argument funkce je obvykle až na dalších řádcích, pokud není
    #   evidentně krátký.
    # - Pojmenované argumenty funkce jsou obvykle na stejném řádku jako jméno
    #   funkce.  Jsou-li příliš dlouhé, jsou na dalším řádku se zvýšeným
    #   odsazením, a to buď všechny na jednom řádku, nebo každý na samostatném.
    # - Operátory horizontálního spojení jsou, je-li to možné, na stejném řádku
    #   jako okolní text.
    # - Operátory vertikálního spojení jsou obvykle na samostatném řádku.
    #   Jsou-li uvnitř složených závorek, mohou mít stejnou úroveň odsazení
    #   jako ty závorky, tj. doleva oproti okolnímu textu.
    #
    # Koordinace formátování při generování kódu:
    #
    # - Každý vygenerovaný kus kódu musí končit buď mezerou, nebo novým řádkem
    #   a odpovídajícím odsazením.
    # - Končí-li vygenerovaný kus kódu mezerou, znamená to možnost připojení
    #   dalšího textu na stejném řádku, typicky takto plynule naváže operátor
    #   horizontálního spojení.
    # - Končí-li vygenerovaný kus kódu novým řádkem a odsazením, vylučuje to
    #   možnost připojení dalšího textu na stejném řádku, a to včetně
    #   horizontálních operátorů.  Pokud navazuje volání funkce, lze
    #   předpokládat její umístění na samostatný řádek, tj. ve výsledném efektu
    #   její oddělení prázdným řádkem.
    # - Generuje-li se text na stejném řádku, tak se na začátek nevkládá nic a
    #   na konec se vloží mezery a nové řádky dle pravidel výše.
    # - Generuje-li se text na samostatném řádku, tak se na začátek vloží znak
    #   nového řádku plus patřičné odsazení a na konec se vloží mezery a nové
    #   řádky dle pravidel výše.

    INTRO = _INTRO

    _MARK_PROCESSORS = {Null:        '_format_null',
                        Nbsp:        '_format_nbsp',
                        Euro:        '_format_euro',
                        Pound:       '_format_pound',
                        Center:      '_format_center',
                        AlignLeft:  '_format_align_left',
                        AlignRight: '_format_align_right',
                        VCenter:    '_format_vcenter',
                        VSpace:     '_format_space',
                        HSpace:     '_format_space',
                        HLine:      '_format_hline',
                        Paragraph:   '_format_paragraph',
                        List:        '_format_list',
                        NewPage:    '_format_new_page',
                        PageNumber: '_format_page_number',
                        Bold:        '_format_bold',
                        Italic:      '_format_italic',
                        Roman:       '_format_roman',
                        FontSize:   '_format_font_size',
                        FontFamily: '_format_font_family',
                        Group:       '_format_group',
                        Document:    '_format_document',
                        Table:       '_format_table',
                        LongTable:  '_format_long_table',
                        Image:       '_format_image',
                        }
    _SPECIAL_LOUT_CHARS = {'\\': '\\\\', '"': '"\\""', '@': '"@"', '|': '"|"',
                           '/': '"/"', '#': '"#"', '{': '"{"', '}': '"}"',
                           '&': '"&"', '^': '"^"', '~': '"~"', '\n': ' @LLP\n'
                           }

    def __init__(self, resolvers, template_id):
        """Inicializuj instanci.

        Argumenty:

          resolvers -- resolver jmen šablon a datových objektů; smí být
            i neprázdná sekvence resolverů, v kterémžto případě bude použit
            první resolver nevyvolávající výjimku 'ResolverFileError' při
            přístupu k šabloně
          template_id -- id šablony výstupu, string
            
        """
        super(LoutFormatter, self).__init__(prefix='pytislout')
        self._resolvers = resolvers
        self._formatted_document = None
        self._lout_run_lock = thread.allocate_lock()
        processors = {}
        for k, v in self._MARK_PROCESSORS.items():
            processors[k] = getattr(self, v)
        self._mark_processors = processors
        chars = string.join(self._SPECIAL_LOUT_CHARS.keys(), '')
        for old, new in (('\\', '\\\\'), ('^', '\\^')):
            chars = string.replace(chars, old, new)
        self._special_lout_chars_regexp = re.compile('[%s]' % chars)
        # Resolvery
        self._output_resolver = resolver = \
          self._ok_resolver(template_id, 'body', mandatory=True)
        self._generic_resolver = None
        # Specifikace
        self._coding = self._resolve(resolver, template_id, 'coding')
        self._doc_header = \
          self._resolve(resolver, template_id, 'doc_header')
        self._doc_footer = \
          self._resolve(resolver, template_id, 'doc_footer')
        self._page_header = \
          self._resolve(resolver, template_id, 'page_header', default=Null())
        self._first_page_header = \
          self._resolve(resolver, template_id, 'first_page_header',
                        default=self._page_header)
        self._page_footer = \
          self._resolve(resolver, template_id, 'page_footer',
                        default=Center('Strana ', PageNumber()))
        self._page_background = \
          self._resolve(resolver, template_id, 'background', default=None)
        self._page_layout = \
          self._resolve(resolver, template_id, 'page_layout', default={})
        body = self._resolve(resolver, template_id, 'body')
        if not isinstance(body, Document) and \
           not (is_sequence(body) and body and
                isinstance(body[0], Document)):
            body = Document(body)
        self._body = body
    
    def _ok_resolver(self, template_id, element, mandatory=False):
        for resolver in xtuple(self._resolvers):
            try:
                resolver.get_object(template_id, element)
            except ResolverError:
                continue
            break
        else:
            if mandatory:
                raise TemplateException(_(u"Chybí použitelný resolver"))
            else:
                resolver = None
        return resolver

    def _get_generic_resolver(self, id_):
        if self._generic_resolver is None:
            self._generic_resolver = self._ok_resolver(id_, 'view_spec')
        return self._generic_resolver
        
    def _resolve(self, resolver, template_id, element, default=''):
        try:
            result = resolver.get(template_id, element)
        except ResolverSpecError as e:
            if __debug__: log(DEBUG, 'Specifikace nenalezena:', e.args)
            result = default
        return result

    # Pomocné formátovací funkce

    def _indent(self, indentation):
        return indentation + '  '
    
    def _font(self, family):
        if family == FontFamily.PROPORTIONAL:
            font = 'Times'
        elif family == FontFamily.SANS_SERIF:
            font = 'Helvetica'
        elif family == FontFamily.FIXED_WIDTH:
            font = 'Courier'
        else:
            raise TemplateException(_(u"Neznámá specifikace fontu"), family)
        if self._coding == Coding.LATIN2:
            font = font + 'CE'
        return font

    def _f_wrap(self, function, template, stream, indent):
        stream.write('\n%s%s ' % (indent, function))
        self._format_conc(template.contents(), stream, indent)
        stream.write(' ')

    def _unit(self, length, default_unit):
        if not isinstance(length, Unit):
            length = default_unit(length)
        size = length.size()
        if length.__class__ is UMm:
            size = size/10.0
        _UNIT_LETTERS = {UMm: 'c', UPoint: 'p', UFont: 'f', USpace: 's'}
        try:
            letter = _UNIT_LETTERS[length.__class__]
        except KeyError:
            raise FormattingException(_(u"Neznámá jednotka"))
        result = '%f%s' % (size, letter)
        # a hack to avoid locale dependent float formatting
        return result.replace(',','.')

    # Formátovače pro jednotlivé značky a elementy

    def _format_text(self, template, stream, indent):
        dict = self._SPECIAL_LOUT_CHARS
        reg = self._special_lout_chars_regexp
        formatted = ''
        i = 0
        while True:
            r = reg.search(template, i)
            if r is None:
                formatted = formatted + template[i:]
                break
            pos = r.start()
            formatted = formatted + template[i:pos] + dict[r.group()]
            i = pos + 1
        flen = len(formatted)
        for i in range(flen):
            if formatted[i] != ' ':
                break
        if i > 0:
            formatted = '"' + formatted[:i] + '"' + formatted[i:]
        for i in nreverse(range(flen)):
            if formatted[i] != ' ':
                break
        if i < flen - 1:
            formatted = formatted[:i+1] + '"' + formatted[i+1:] + '"'
        # TODO: quick solution for problem with ' & ' at the end of wrapped elements
        #       such as @B or @II
        # formatted = string.join(formatted.split('\n'), '\n'+indent) + ' & '
        formatted = string.join(formatted.split('\n'), '\n'+indent)
        if stream is None:
            return formatted
        else:
            stream.write(formatted)

    def _format_conc(self, elements, stream, indent):
        nindent = self._indent(indent)
        stream.write('\n%s{ ' % (indent))
        # TODO: quick solution for problem with ' & ' at the end of wrapped elements
        #       such as @B or @II
        # see also _format_text
        for i, e in enumerate(elements):
            self._format(e, stream, nindent)
            if i < len(elements) - 1:
                stream.write(' & ')
        stream.write('} ')

    def _format_null(self, template, stream, indent):
        stream.write('@Null ')

    def _format_nbsp(self, template, stream, indent):
        stream.write('~')

    def _format_euro(self, template, stream, indent):
        stream.write('@Euro ')

    def _format_pound(self, template, stream, indent):
        stream.write('@Sterling ')

    def _format_center(self, template, stream, indent):
        self._f_wrap('@Center', template, stream, indent)

    def _format_align_left(self, template, stream, indent):
        self._f_wrap('@alignLeft', template, stream, indent)

    def _format_align_right(self, template, stream, indent):
        self._f_wrap('@alignRight', template, stream, indent)

    def _format_vcenter(self, template, stream, indent):
        self._f_wrap('//0.5rt', template, stream, indent)

    def _format_space(self, template, stream, indent):
        orientation, size = template.orientation(), template.size()
        if orientation == template.VERTICAL:
            operator = '//'
            function = '@vfill'
        elif orientation == template.HORIZONTAL:
            operator = '||'
            function = '@hfill'
        else:
            raise ProgramError('Unknown orientation', orientation, template)
        if size is None:
            stream.write(' %s ' % (function,))
        else:
            stream.write(' { {} %s%s {} } ' %
                         (operator, self._unit(size, UMm)))

    def _format_hline(self, template, stream, indent):
        stream.write('%s@FullWidthRule\n' % indent)
        
    def _format_paragraph(self, template, stream, indent):
        stream.write('\n%s//{@ParaGap} { ' % indent)
        self._format_conc(template.contents(), stream, indent)
        stream.write('\n%s} //\n' % indent)

    def _format_list(self, template, stream, indent):
        nindent = self._indent(indent)
        mark2name = {None: '@LeftList',
                     List.NUMBER_MARK: '@List',
                     List.BULLET_MARK: '@BulletList'}
        try:
            list_name = mark2name[template.arg_mark]
        except KeyError:
            raise TemplateException(_(u"Neznámá značka uvození seznamu"),
                                    template.arg_mark, template)
        stream.write('\n%s%s' % (indent, list_name))
        for e in template.contents():
            stream.write('\n%s@ListItem {\n' % indent)
            self._format(e, stream, nindent)
            stream.write('\n%s}\n' % indent)
        stream.write('\n@EndList\n')

    def _format_new_page(self, template, stream, indent):
        stream.write('\n%s@NP\n%s' % (indent, indent))
    
    def _format_page_number(self, template, stream, indent):
        stream.write(template.total() and '@pageNumber"/"@numberOfPages '
                                        or '@pageNumber ')

    def _format_bold(self, template, stream, indent):
        self._f_wrap('@B', template, stream, indent)

    def _format_italic(self, template, stream, indent):
        self._f_wrap('@II', template, stream, indent)

    def _format_roman(self, template, stream, indent):
        self._f_wrap('@R', template, stream, indent)

    def _format_font_size(self, template, stream, indent):
        function = '{ %ff } @font' % template.size()
        self._f_wrap(function, template, stream, indent)

    def _format_font_family(self, template, stream, indent):
        family = template.family()
        function = '{ %s Base } @font' % self._font(family)
        self._f_wrap(function, template, stream, indent)

    def _format_group(self, template, stream, indent):
        vertical = template.arg_vertical
        boxed = template.arg_boxed
        separator = vertical and '//' or '||'
        function = vertical and '@vgroup' or '@hgroup'
        if boxed:
            function = function + ' boxed { yes }'
        elements = template.contents()
        if boxed:
            line = (vertical and '@ghline' or '@gvline')
        else:
            line = ''
        nindent = self._indent(indent)
        nnindent = self._indent(nindent)
        MARGIN = '@smallmargin'
        estart = '%s\n%s{ ' % (line, nindent)
        egstart = '%s{ ' % (line)
        eend = '\n%s}\n%s%s%s {} %s%%s\n%s' % \
               (nindent, indent, separator, MARGIN, separator, nindent)
        egend = '/ {}\n%s}\n%s%s%%s\n%s' % \
                (nindent, indent, separator, nindent)
        # Rozměry
        balance = template.arg_balance
        nelements = len(elements)
        if balance:
            if len(balance) != nelements:
                raise TemplateException(_(u"Balance neodpovídá počtu prvků"),
                                    balance, template)
            sum = functools.reduce(operator.__add__, balance)
            if sum <= 0 or some(lambda x: x < 0, balance):
                raise TemplateException(_(u"Chybné číslo v balanci"),
                                    balance, template)
            bk = 1.0 / sum
            balance = map(lambda b, bk=bk: bk*b, balance)
        elif balance is not None:
            bk = 1.0 / nelements
            balance = map(lambda __, bk=bk: bk, range(nelements))
        if balance is None:
            gaps = map(lambda __: '', range(nelements))
        else:
            gaps = []
            sum = 0
            for b in balance:
                sum = sum + b
                gaps.append('%.2fbt' % sum)
        # Hurá do toho
        stream.write('\n%s%s\n%s{\n%s' % (indent, function, indent, nindent))
        for e, g in zip(elements, gaps):
            egroup = isinstance(e, Group)
            stream.write(egroup and egstart or estart)
            self._format(e, stream, nnindent)
            stream.write((egroup and egend or eend) % g)
        stream.write('@Null\n%s}\n%s' % (indent, indent))
        
    def _format_document(self, template, stream, indent):
        stream.write('@document\n')
        def header(name, header):
            if header is not None:
                stream.write('  @%s { ' % name)
                self._format(header, stream)
                stream.write(' } \n')
        first_page_header = template.arg_first_page_header
        if first_page_header is None:
            first_page_header = template.arg_page_header
        header ('firstPageHeader', first_page_header)
        header('pageHeader', template.arg_page_header)
        header('pageFooter', template.arg_page_footer)
        stream.write ('//\n@text @Begin\n')
        if self._doc_header:
            self._format(self._doc_header, stream)
            stream.write('\n//\n')
        self._format(template.contents(), stream, indent)
        if self._doc_footer:
            stream.write('\n//\n')
            self._format(self._doc_footer, stream)
        stream.write('\n@End @text\n')

    def _format_table(self, template, stream, indent):
        nindent = self._indent(indent)
        ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        columns = []
        labels = []
        format_spec = []
        lformat_spec = []
        template_columns = template.columns()
        header_present = False
        if template.vmargin() is None:
            vmargin = ''
        else:
            vmargin = 'mv { 0f } '
        def cell_alignment(alignment):
            if alignment == Table.Column.ALIGN_LEFT:
                break_ = ''
            elif alignment == Table.Column.ALIGN_CENTER:
                break_ = 'indent { ctr } '
            elif alignment == Table.Column.ALIGN_RIGHT:
                break_ = 'indent { right } '
            else:
                raise TemplateException(_(u"Neznámý druh zarovnání sloupce"),
                                        c.alignment)
            return break_
        for i in range(len(template_columns)):
            cid = ALPHABET[i]
            columns.append(cid)
            c = template_columns[i]
            label = c.label
            if label is None:
                label = ''
            else:
                header_present = True
            labels.append(label)
            break_ = cell_alignment(c.alignment)
            lbreak_ = cell_alignment(c.label_alignment)
            if c.width is None:
                width = ''
            else:
                width = '%s @Wide ' % self._unit(c.width, USpace)
            cell_format_spec = '@Cell %s%%s%s%s' % (vmargin, width, cid)
            format_spec.append(cell_format_spec % break_)
            lformat_spec.append(cell_format_spec % lbreak_)
        row_format = string.join(format_spec, ' | ')
        header_format = string.join(lformat_spec, ' | ')
        stream.write('%s@Tbl aformat { %s } bformat { %s }\n%s{\n' %
                     (indent, header_format, row_format, indent))
        if header_present:
            stream.write('%s@Rowa ' % nindent)
            for col, cell in zip(columns, labels):
                fcell = self._format(cell, stream, nindent)
                stream.write('%s { @B { %s } }' % (col, fcell))
            stream.write('\n')            
        rule = False
        for row in template.data():
            if row is None:
                rule = True
            else:
                stream.write('%s@Rowb ' % nindent)
                if rule:
                    stream.write('ruleabove { yes } ')
                    rule = False
                for col, cell in zip(columns, row):
                    stream.write('%s { ' % (col,))
                    self._format(cell, stream, nindent)
                    stream.write(' } ')
                stream.write('\n')
            # TODO: rule pod posledním řádkem
        stream.write('%s}\n' % (nindent,))
        
    def _format_long_table(self, template, stream, indent):
        # Formátování
        columns = template.columns()
        hcells = []
        cell_templates = []
        for c in columns:
            w = c.width
            assert w
            pswidth = w + 2
            all_width = self._unit(pswidth, USpace)
            def cell_function(alignment):
                if alignment == LongTable.Column.ALIGN_LEFT:
                    cell_fun = '@lcell'
                elif alignment == LongTable.Column.ALIGN_CENTER:
                    cell_fun = '@ccell'
                elif alignment == LongTable.Column.ALIGN_RIGHT:
                    cell_fun = '@rcell'
                else:
                    raise ProgramError('Unknown alignment', alignment)
                return cell_fun
            # Hlavičková buňka
            if c.label is not None:
                label_stream = cStringIO.StringIO()
                self._format(c.label, label_stream)
                label = label_stream.getvalue()
            else:
                label = ''
            cell_fun = cell_function(c.label_alignment)
            hcell = '{ %s } %s header { yes } { %s }' % (all_width, cell_fun, label)
            hcells.append(hcell)            
            # Netabulková buňka
            cell_fun = cell_function(c.alignment)
            cell_beg = '{ %s } %s { ' % (all_width, cell_fun)
            cell_end = ' }'
            cell_templates.append((cell_beg, cell_end))
        s = stream
        # Hlavička
        nindent = self._indent(indent)
        if [c for c in columns if c.label is not None]:
            labels = string.join(hcells, ' | ')
            hsep_raw = template.separator_height()
            hsep = self._unit(hsep_raw, UPoint)
            hmargin = self._unit(template.separator_margin(), UPoint)
            if hsep_raw:
                labels = (('@hline placement { bottom } linewidth { %s } ' +
                           'margin { %s } { %s }') % (hsep, hmargin, labels))
            else:
                labels = '%s //%s //%s' % (labels, hmargin, hmargin)
            header = '{\n%s%s\n%s}\n%s' % (nindent, labels, nindent, indent)
            s.write('\n%s@LLP\n%s@BeginHeaderComponent {\n%s%s}\n%s//\n' % \
                        (indent, indent, nindent, header, indent))
            s.write(indent + header + '\n')
        # Řádky dat
        sep_raw = template.line_separator_height()
        sep = self._unit(sep_raw, UPoint)
        margin = self._unit(template.line_separator_margin(), UPoint)
        if sep_raw:
            sepline = (('@hline placement { bottom } linewidth { %s } ' +
                        'margin { %s } {\n') % (sep, margin))
        else:
            sepline = '//%s //%s\n' % (margin, margin)
        row_generator_init = template.row_generator_init()
        if row_generator_init is not None:
            row_generator_init()
        row_generator = template.row_generator()
        while True:
            row = row_generator()
            if row is None:
                break
            s.write(sepline)
            s.write('%s{\n%s' % (indent, nindent))
            for i in range(len(columns)):
                cell_beg, cell_end = cell_templates[i]
                s.write(cell_beg)
                self._format(row[i], stream, nindent)
                s.write(cell_end)
                s.write(' | ')
            s.write('@Null\n%s}' % indent)
            if sep_raw:
                s.write('\n%s}' % indent)
            s.write('\n')
        # Zakončení
        s.write('%s//1vx\n%s' % (indent, indent))
        s.write('@EndHeaderComponent\n%s' % (indent))
        
    def _format_image(self, template, stream, indent):
        tfile_name = os.path.normpath(template.file_name())
        if tfile_name == '..' or starts_with(tfile_name, '../'):
            raise TemplateException(_(u"Nikoliv bezpečné jméno souboru"),
                                    template.file_name())
        import config
        file_name = os.path.join(config.def_dir, tfile_name)
        file_name.replace('"', '\\"')
        stream.write('@IncludeGraphic "%s"\n' % file_name)

    # Obecné formátování
    
    def _format(self, template, stream, indent=''):
        if is_anystring(template):
            processor = self._format_text
        elif is_sequence(template):
            processor = self._format_conc
        else:
            try:
                tclass = template.__class__
            except AttributeError:
                raise TemplateException(_(u"Chybný element"), template)
            try:
                processor = self._mark_processors[tclass]
            except KeyError:
                raise TemplateException(_(u"Neznámý element"), template)
        result = processor(template, stream, indent)
        return result

    def _process(self, stream):
        if self._coding == Coding.LATIN2:
            stream.write('@SysInclude { latin2 }\n')
        #font = self._font(FontFamily.SANS_SERIF)
        font = self._font(FontFamily.FIXED_WIDTH)
        def process_special(contents):
            s = cStringIO.StringIO()
            self._format(contents, s)
            return s.getvalue()
        first_page_header = process_special(self._first_page_header)
        page_header = process_special(self._page_header)
        page_footer = process_special(self._page_footer)
        if self._page_background is None:
            background = ''
        else:
            background = process_special(self._page_background)
        def l(id, default):
            return self._unit(self._page_layout.get(id, default), None)
        parameters = {'font': font,
                      'fpheader': first_page_header,
                      'pheader': page_header,
                      'pfooter': page_footer,
                      'pbackground': background,
                      'top_margin': l(PAGE_TOP_MARGIN, UMm(10)),
                      'bottom_margin': l(PAGE_BOTTOM_MARGIN, UMm(10)),
                      'left_margin': l(PAGE_LEFT_MARGIN, UMm(10)),
                      'right_margin': l(PAGE_RIGHT_MARGIN, UMm(10)),
                      'landscape_mode':
                      (self._page_layout.get(PAGE_LANDSCAPE_MODE) and 'yes'
                       or 'no'),
                      'pwidth': l(PAGE_WIDTH, UPoint(595)),   # A4
                      'pheight': l(PAGE_HEIGHT, UPoint(842)), # A4
                      }
        stream.write(self.INTRO % parameters)
        if isinstance(self._body, Document):
            self._format(self._body, stream)
        else:
            for b in self._body:
                self._format(b, stream)
                stream.write('\n//\n')

    def _lout(self, lout_args, stream):
        command = '%s %s -' % (config.lout_command, string.join(lout_args, ' '),)
        def lfunction():
            log(EVENT, 'Start Lout')
            process = Popen(command, from_child=stream,
                            directory=self._tmpdir)
            to_lout = process.to_child()
            try:
                if self._formatted_document is None:
                    cc = [to_lout]
                    if True:
                    # if config.debug:
                        tmpfile = os.path.join(config.tmp_dir, 'pytis.lout')
                        try:
                            f = open(tmpfile, 'w')
                            cc.append(f)
                        except:
                            pass
                    in_stream = Pipe(cc=cc, encoder=codecs.getencoder('iso-8859-2'))
                    if __debug__: log(DEBUG, 'Start zpracování specifikace')
                    try:
                        self._process(in_stream)
                        in_stream.close()
                        self._formatted_document = in_stream.read()
                    except ValueError:  # zápis do uzavřeného streamu
                        log(EVENT, ('Zpracování specifikace přerušeno '
                                    'uzavřením streamu'))
                        in_stream.close()
                    if __debug__: log(DEBUG, 'Konec zpracování specifikace')
                else:
                    if __debug__: log(DEBUG, 'Zdroják Lout je nacachován')
                    import StringIO
                    # Zde by mělo být použito cStringIO, ale z nějakého
                    # záhadného důvodu to s ním nefunguje.  Vypadá to na
                    # nějaký problém s konverzí konců řádků...
                    s = StringIO.StringIO(self._formatted_document)
                    copy_stream(s, to_lout, close=True)
            finally:
                try:
                    to_lout.close()
                    process.wait()
                except:
                    pass
        with_lock(self._lout_run_lock, lfunction)
        errors = ''
        log(EVENT, 'Konec formátování přes Lout')
        return errors

    def preview(self, stream):
        """Vrať zformátovaný dokument jako čistý text.
        
        Argumenty:

          stream -- stream otevřený pro zápis, disponující metodou 'write'.

        Metoda po skončení zápisu uzavírá 'stream'.

        """
        errors = self._lout(('-P',), stream)

    def printout(self, stream):
        """Pošli dokument jako PostScript na 'stream'.

        Argumenty:

          stream -- stream otevřený pro zápis, disponující metodou 'write'.

        Metoda po skončení zápisu uzavírá 'stream'.

        """
        errors = self._lout((), stream)

    def printdirect(self):
        """Pošli dokument jako PostScript na vstup 'printing_command'."""      
        stream = dev_null_stream('w')
        errors = self._lout((), stream)
        process = Popen(config.printing_command,
                        from_child=dev_null_stream('w'))
        stream = process.to_child()
        thread.start_new_thread(self.printout, (stream,))

    def close(self):
        """Explicitně proveď ukončovací akce třídy.

        Tato metoda odpovídá akcím metody '__del__()', avšak umožňuje své
        explicitní volání.  V této třídě totiž metoda '__del__()' z neznámých
        důvodů volána není.
        
        """
        self._cleanup ()


Formatter = LoutFormatter
"""Implicitní formátovač."""
