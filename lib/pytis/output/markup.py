# -*- coding: utf-8 -*-

# Formátovací prvky
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2011 Brailcom, o.p.s.
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

from pytis.output import *


class Unit(object):
    """Velikost explicitně vyjádřená v určitých jednotkách.

    Instance potomků této třídy mohou být použity kdekoliv, kde je možno předat
    absolutní délkové jednotky, místo implicitně uvažovaných jednotek.

    Tato třída je bázová, k přímému použití jsou určeni až její potomci,
    odpovídající konkrétním jednotkám.

    """
    def __init__(self, size):
        """Inicializuj instanci.

        Argumenty:

          size -- velikost v daných jednotkách, float

        """
        self._size = size

    def __nonzero__(self):
        """Vrať pravdu, právě když velikost je nenulová."""
        return self._size != 0
    
    def __add__(self, other):
        """Vrať instanci stejné třídy s velikostí zvětšenou o 'other'.

        Argumenty:

          other -- float nebo int udávající velikost, o kterou má být v nové
            instanci zvětšena velikost aktuální instance

        """
        assert isinstance(other, float) or isinstance(other, int)
        return self.__class__(self._size + other)

    def __mul__(self, other):
        """Vrať instanci stejné třídy s velikostí vynásobenou 'other'.

        Argumenty:

          other -- float nebo int udávající násobek, kterým má být pro novou
            instanci vynásobena velikost aktuální instance

        """
        assert isinstance(other, float) or isinstance(other, int)
        return self.__class__(self._size * other)        

    def size(self):
        """Vrať velikost zadanou v konstruktoru."""
        return self._size

class UMm(Unit):
    """Milimetry."""

class UPoint(Unit):
    """Tiskové body (1/72 palce)."""
    
class UFont(Unit):
    """Jednotky odpovídající velikosti aktuálního fontu."""
    
class USpace(Unit):
    """Jednotky odpovídající preferované mezislovní mezeře aktuálního fontu."""
    

class _Mark(object):
    pass


class _Container(_Mark):

    KWARGS = {}
    
    def __init__(self, *contents, **kwargs):
        """Definuj odstavec.

        Argumenty:

          contents -- obsah odstavce, značky a řetězce

        """
        self._contents = contents
        for k, v in self.KWARGS.items():
            try:
                v = kwargs[k]
                del kwargs[k]
            except KeyError:
                pass
            self.__dict__['arg_' + k] = v
        if kwargs:
            raise TemplateException(_(u"Chybné argumenty prvku"),
                                    self.__class__, kwargs.keys())

    def contents(self):
        """Vrať obsah odstavce zadaný v konstruktoru."""
        return self._contents


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

class Euro(_Mark):
    """Značka reprezentující znak měny Euro."""

class Pound(_Mark):
    """Značka reprezentující znak libry."""

class Center(_Container):
    """Značka horizontálního vycentrování svého obsahu."""

class AlignLeft(_Container):
    """Značka zarovnání svého obsahu vlevo."""

class AlignRight(_Container):
    """Značka zarovnání svého obsahu vpravo."""
    

class VCenter(_Container):
    """Značka horizontálního vycentrování svého obsahu."""


class _Space(_Mark):

    VERTICAL = 'VERTICAL'
    HORIZONTAL = 'HORIZONTAL'
    
    def __init__(self, size, orientation):
        self._size = size
        self._orientation = orientation

    def size(self):
        return self._size

    def orientation(self):
        return self._orientation

class VSpace(_Space):
    """Značka prázdného vertikálního objektu dané šířky."""
    
    def __init__(self, height):
        """Inicializuj instanci.

        Argumenty:

          height -- požadovaná výška objektu v milimetrech, nezáporné číslo;
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
            může být též 'None', v kterémžto případě objekt bude mít největší
            rozumnou šířku, přičemž \"největší rozumná šířka\" není nijak
            exaktně definována

        """
        super(HSpace, self).__init__(width, self.HORIZONTAL)


class HLine(_Mark):
    """Značka horizontální čáry vyplňující celý dostupný prostor."""


class Paragraph(_Container):
    """Značka odstavce."""


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
    

class NewPage(_Mark):
    """Značka nové stránky."""


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


class Bold(_Container):
    """Tučně sázený text."""


class Italic(_Container):
    """Text sázený kurzívou."""


class Roman(_Container):
    """Standardně sázený text."""


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


class Group(_Container):
    """Spojení obsahu do skupiny.

    Na rozdíl od prostého užití tuple, tato značka umožňuje specifikovat různé
    parametry tohoto spojení pomocí předaných klíčovaných argumentů.  Těmito
    argumenty mohou být:

      vertical -- je-li pravdivé, budou prvky spojeny vertikálně, jinak budou
        spojeny horizontálně
      boxed -- právě když je pravdivé, budou prvky skupiny orámovány
      balance -- není-li 'None', jedná se o tuple o počtu prvků shodném
        s počtem prvků skupiny, udávající vzájemný poměr velikostí pořadím
        odpovídajících prvků.  Velikost prvků ve směru orientace skupiny (dle
        argumentu 'vertical') bude patřičně upravena, velikost prvků s udaným
        poměrem 0 zůstane nezměněna.
      
    """
    KWARGS = {'vertical': False,
              'boxed': False,
              'balance': None}


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
              'first_page_header': None}


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
              alignment -- způsob zarovnání hlavičky sloupce, jedna z 'ALIGN_*'
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
        assert is_sequence(columns)
        vmargin = kwargs.get('vmargin')
        assert vmargin in (None, 0)
        if len(columns) > 26:
            raise TemplateException(_(u"Více než 26 sloupců v tabulce"))
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
            jejími řádky v bodech, jako float
          line_separator_height -- tloušťka oddělovací čáry za každým řádkem
            tabulky v bodech, jako float
          separator_margin -- vzdálenost oddělovací čáry záhlaví tabulky od
            záhlaví v bodech, jako float
          line_separator_margin -- vzdálenost oddělovací čáry od každého řádku
            tabulky v bodech, jako float

        """
        super(LongTable, self).__init__(columns)
        assert isinstance(row_generator, collections.Callable)
        self._row_generator = row_generator
        self._row_generator_init = row_generator_init
        self._separator_height = separator_height
        self._line_separator_height = line_separator_height
        self._separator_margin = separator_margin
        self._line_separator_margin = line_separator_margin

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


class Image(_Mark):
    """EPS obrázek."""
    
    def __init__(self, file_name):
        """Inicializuj instanci.

        Argumenty:

          file_name -- jméno souboru obrázku, relativní k adresáři definovanému
            konfigurační volbou 'def_dir'.

        """
        self._file_name = file_name

    def file_name(self):
        """Vrať jméno souboru zadané v konstruktoru."""
        return self._file_name
