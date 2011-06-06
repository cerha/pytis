# -*- coding: iso-8859-2 -*-

# Form�tovac� prvky
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

"""Definice form�tovac�ch zna�ek.

Tento soubor definuje prvky sestavuj�c� v�sledn� tiskov� dokument.  Prvky
jednak definuj� form�tov�n� a jednak umo��uj� vkl�dat extern� data do
dokumentu.  Jsou pou��v�ny v�defini�n�ch souborech v�stupn�ch sestav.

Ka�d� ve�ejn� t��da modulu odpov�d� form�tovac� zna�ce pou�iteln� v�definici
dokumentu.  Krom� t�chto t��d je je�t� mo�no pou��vat oby�ejn� �et�zce, kter�
budou vlo�eny tak, jak jsou, v�etn� mezer a od��dkov�n�, a�sekvence obsahuj�c�
elementy ke spojen� dohromady.

"""

import collections

from pytis.output import *


class Unit(object):
    """Velikost explicitn� vyj�d�en� v�ur�it�ch jednotk�ch.

    Instance potomk� t�to t��dy mohou b�t pou�ity kdekoliv, kde je mo�no p�edat
    absolutn� d�lkov� jednotky, m�sto implicitn� uva�ovan�ch jednotek.

    Tato t��da je b�zov�, k�p��m�mu pou�it� jsou ur�eni a� jej� potomci,
    odpov�daj�c� konkr�tn�m jednotk�m.

    """
    def __init__(self, size):
        """Inicializuj instanci.

        Argumenty:

          size -- velikost v�dan�ch jednotk�ch, float

        """
        self._size = size

    def __nonzero__(self):
        """Vra� pravdu, pr�v� kdy� velikost je nenulov�."""
        return self._size != 0
    
    def __add__(self, other):
        """Vra� instanci stejn� t��dy s�velikost� zv�t�enou o�'other'.

        Argumenty:

          other -- float nebo int ud�vaj�c� velikost, o�kterou m� b�t v�nov�
            instanci zv�t�ena velikost aktu�ln� instance

        """
        assert isinstance(other, float) or isinstance(other, int)
        return self.__class__(self._size + other)

    def __mul__(self, other):
        """Vra� instanci stejn� t��dy s�velikost� vyn�sobenou 'other'.

        Argumenty:

          other -- float nebo int ud�vaj�c� n�sobek, kter�m m� b�t pro novou
            instanci vyn�sobena velikost aktu�ln� instance

        """
        assert isinstance(other, float) or isinstance(other, int)
        return self.__class__(self._size * other)        

    def size(self):
        """Vra� velikost zadanou v�konstruktoru."""
        return self._size

class UMm(Unit):
    """Milimetry."""

class UPoint(Unit):
    """Tiskov� body (1/72 palce)."""
    
class UFont(Unit):
    """Jednotky odpov�daj�c� velikosti aktu�ln�ho fontu."""
    
class USpace(Unit):
    """Jednotky odpov�daj�c� preferovan� mezislovn� meze�e aktu�ln�ho fontu."""
    

class _Mark(object):
    pass


class _Container(_Mark):

    KWARGS = {}
    
    def __init__(self, *contents, **kwargs):
        """Definuj odstavec.

        Argumenty:

          contents -- obsah odstavce, zna�ky a �et�zce

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
            raise TemplateException(_("Chybn� argumenty prvku"),
                                    self.__class__, kwargs.keys())

    def contents(self):
        """Vra� obsah odstavce zadan� v�konstruktoru."""
        return self._contents


class Null(_Mark):
    """Nic.

    Pou�it� t�to zna�ky se p�edpokl�d� v�m�stech, kde nem� b�t nic, tj. ani
    pr�zdn� �et�zec.  P��kladem takov�ho m�sta, kde pou�it� takov� zna�ky d�v�
    smysl, je pati�ka str�nky.

    """

class Nbsp(_Mark):
    """Zna�ka reprezentuj�c� znak nezalomiteln� mezery.

    Tento znak lze zapsat i�v�oby�ejn�m stringov�m z�pisu textu jako '�'.
    Zna�ka slou�� jako alternativn� forma z�pisu.

    """

class Euro(_Mark):
    """Zna�ka reprezentuj�c� znak m�ny Euro."""

class Pound(_Mark):
    """Zna�ka reprezentuj�c� znak libry."""

class Center(_Container):
    """Zna�ka horizont�ln�ho vycentrov�n� sv�ho obsahu."""

class AlignLeft(_Container):
    """Zna�ka zarovn�n� sv�ho obsahu vlevo."""

class AlignRight(_Container):
    """Zna�ka zarovn�n� sv�ho obsahu vpravo."""
    

class VCenter(_Container):
    """Zna�ka horizont�ln�ho vycentrov�n� sv�ho obsahu."""


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
    """Zna�ka pr�zdn�ho vertik�ln�ho objektu dan� ���ky."""
    
    def __init__(self, height):
        """Inicializuj instanci.

        Argumenty:

          height -- po�adovan� v��ka objektu v�milimetrech, nez�porn� ��slo;
            m��e b�t t� 'None', v�kter�m�to p��pad� objekt bude m�t nejv�t��
            rozumnou v��ku, p�i�em� \"nejv�t�� rozumn� v��ka\" nen� nijak
            exaktn� definov�na

        """
        super(VSpace, self).__init__(height, self.VERTICAL)

class HSpace(_Space):
    """Zna�ka pr�zdn�ho horizont�ln�ho objektu dan� ���ky."""
    
    def __init__(self, width):
        """Inicializuj instanci.

        Argumenty:

          width -- po�adovan� ���ka objektu v�milimetrech, nez�porn� ��slo;
            m��e b�t t� 'None', v�kter�m�to p��pad� objekt bude m�t nejv�t��
            rozumnou ���ku, p�i�em� \"nejv�t�� rozumn� ���ka\" nen� nijak
            exaktn� definov�na

        """
        super(HSpace, self).__init__(width, self.HORIZONTAL)


class HLine(_Mark):
    """Zna�ka horizont�ln� ��ry vypl�uj�c� cel� dostupn� prostor."""


class Paragraph(_Container):
    """Zna�ka odstavce."""


class List(_Container):
    """Seznam.

    Polo�ky seznamu jsou uvozeny zna�kou specifikovanou kl��ov�m argumentem
    konstruktoru 'mark'.  Nen�-li tento argument zad�n, polo�ky nemaj� ��dn�
    zvl�tn� uvozen�.
    
    """
    NUMBER_MARK = 'NUMBER_MARK'
    """Polo�ky seznamu uvozen� vzestupn�mi arabsk�mi ��slicemi od�1."""
    BULLET_MARK = 'BULLET_MARK'
    """Polo�ky seznamu uvozen� punt�kem."""
    
    KWARGS = {'mark': None}
    

class NewPage(_Mark):
    """Zna�ka nov� str�nky."""


class PageNumber(_Mark):
    """Zna�ka generuj�c� ��slo aktu�ln� str�nky.

    Vygenerovan� ��slo je po�adov� ��slo po��naje od�1, zapsan� arabsk�mi
    ��slicemi.

    Tato zna�ka sm� b�t pou�ita jen uvnit� hlavi�ek a pati�ek str�nek.

    """
    def __init__(self, total=False):
        """Inicializuj instanci.

        Argumenty:

          total -- pr�v� kdy� je pravdiv�, uve� krom� aktu�ln� str�nky
            i�celkov� po�et stran

        """
        super(PageNumber, self).__init__()
        self._total = total

    def total(self):
        """Vra� hodnotu argumentu 'total' z�konstruktoru."""
        return self._total


class Bold(_Container):
    """Tu�n� s�zen� text."""


class Italic(_Container):
    """Text s�zen� kurz�vou."""


class Roman(_Container):
    """Standardn� s�zen� text."""


class FontSize(_Container):
    """Standardn� s�zen� text."""
    
    def __init__(self, size, *contents):
        """Definuj text.

        Argumenty:

          size -- float ur�uj�c� relativn� velikost fontu vzhledem
            k�aktu�ln�mu, na dan�m m�st� pou�it�mu fontu; 1.0 zna�� shodnou
            velikost
          contents -- zna�ky, na kter� je zm�na velikosti fontu aplikov�na

        """
        super_(FontSize).__init__(self, *contents)
        self._size = size
        
    def size(self):
        """Vra� velikost zadanou v�konstruktoru."""
        return self._size


class FontFamily(_Container):
    """Text s�zen� zadanou rodinou fontu.

    Tato zna�ka z�rove� nastav� z�kladn� �ez fontu.

    """
    
    PROPORTIONAL = 'PROPORTIONAL'
    """Standardn� proporcion�ln� font (nap��klad Times)."""
    SANS_SERIF = 'SANS_SERIF'
    """Standardn� bezpatkov� proporcion�ln� font(nap��klad Helvetica)."""
    FIXED_WIDTH = 'FIXED_WIDTH'
    """Standardn� neproporcion�ln� font (nap��klad Courier)."""

    def __init__(self, family, *contents):
        """Definuj text.

        Argumenty:

          family -- jedna z�konstant t�to t��dy ur�uj�c� rodinu pou�it�ho fontu
          contents -- zna�ky, na kter� je zm�na velikosti fontu aplikov�na

        """
        super_(FontFamily).__init__(self, *contents)
        self._family = family
        
    def family(self):
        """Vra� rodinu fontu zadanou v�konstruktoru."""
        return self._family


class Group(_Container):
    """Spojen� obsahu do skupiny.

    Na rozd�l od prost�ho u�it� tuple, tato zna�ka umo��uje specifikovat r�zn�
    parametry tohoto spojen� pomoc� p�edan�ch kl��ovan�ch argument�.  T�mito
    argumenty mohou b�t:

      vertical -- je-li pravdiv�, budou prvky spojeny vertik�ln�, jinak budou
        spojeny horizont�ln�
      boxed -- pr�v� kdy� je pravdiv�, budou prvky skupiny or�mov�ny
      balance -- nen�-li 'None', jedn� se o�tuple o�po�tu prvk� shodn�m
        s�po�tem prvk� skupiny, ud�vaj�c� vz�jemn� pom�r velikost� po�ad�m
        odpov�daj�c�ch prvk�.  Velikost prvk� ve sm�ru orientace skupiny (dle
        argumentu 'vertical') bude pat�i�n� upravena, velikost prvk� s�udan�m
        pom�rem 0 z�stane nezm�n�na.
      
    """
    KWARGS = {'vertical': False,
              'boxed': False,
              'balance': None}


class Document(_Container):
    """Samostatn� ��st dokumentu se samostatn� ��slovan�mi str�nkami.

    Dokument je uvozen, resp. ukon�en, obvyklou hlavi�kou, resp. pati�kou.
    Str�nky uvnit� dokumentu maj� sv� vlastn� ��slov�n� po��naje od�1.  Pomoc�
    t�to zna�ky lze vytvo�it \"n�kolik dokument� v�jednom\".

    Tato zna�ka sm� b�t pou�ita pouze jako zna�ka obaluj�c� cel� obsah
    dokumentu nebo jako zna�ka v�ech prvk� sekvence definuj�c� kompletn� obsah
    dokumentu.  Nen�-li zna�ka pou�ita v�bec, je j� cel� dokument obalen
    automaticky.

    Zna�ce lze p�edat n�sleduj�c� argumenty:

      page_header -- zna�ky specifikuj�c� hlavi�ku ka�d� str�nky nebo 'None'
        indikuj�c� implicitn� hlavi�ku str�nky danou specifika�n� funkc�
        'page_header'
      page_footer -- zna�ky specifikuj�c� pati�ku ka�d� str�nky nebo 'None'
        indikuj�c� implicitn� pati�ku str�nky danou specifika�n� funkc�
        'page_footer'
      first_page_header -- zna�ky specifikuj�c� hlavi�ku prvn� str�nky nebo
        'None' indikuj�c� hlavi�ku str�nky danou specifika�n� funkc�
        'first_page_header'

    """
    KWARGS = {'page_header': None,
              'page_footer': None,
              'first_page_header': None}


class Table(_Mark):
    """Nejv��e jednostr�nkov� tabulka s�p�edp�ipraven�mi daty.

    Pro form�tov�n� v�t��ch tabulek lze pou��t zna�ku 'LongTable'.

    """
    class Column:
        """Specifika�n� t��da sloupce tabulky."""
        
        ALIGN_LEFT = 'ALIGN_LEFT'
        """Zarovn�n� obsahu sloupce nalevo."""
        ALIGN_CENTER = 'ALIGN_CENTER'
        """Centrov�n� obsahu sloupce."""
        ALIGN_RIGHT = 'ALIGN_RIGHT'
        """Zarovn�n� obsahu sloupce napravo."""
        
        def __init__(self, label=None, width=None, alignment=ALIGN_LEFT,
                     label_alignment=ALIGN_CENTER):
            """Inicializuj instanci.

            Argumenty:

              label -- z�hlav� sloupce, form�tovac� element; sm� b�t t�
                'None', v�kter�m�to p��pad� sloupec nem� ��dn� z�hlav� a
                nem�-li ��dn� sloupec z�hlav�, nebude z�hlav� v�bec generov�no
              width -- ���ka sloupce ve znac�ch, p�irozen� ��slo nebo 'None'
                (v�kter�m�to p��pad� je ���ka sloupce ur�ena automaticky)
              alignment -- zp�sob zarovn�n� obsahu sloupce, jedna z�'ALIGN_*'
                konstant t��dy
              alignment -- zp�sob zarovn�n� hlavi�ky sloupce, jedna z�'ALIGN_*'
                konstant t��dy

            """
            self.label = label
            self.width = width
            self.alignment = alignment
            self.label_alignment = label_alignment
            
    def __init__(self, columns, *data, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          columns -- sekvence instanc� t��dy 'Table.Column', mus� m�t
            nejm�n� 1 a nejv��e 26�prvk�
          data -- nepr�zdn� sekvence nepr�zdn�ch sekvenc�, odpov�d� ��dk�m
            (vn�j�� sekvence) form�tovan�m do sloupc� (vnit�n� sekvence);
            v�echny vnit�n� sekvence mus� m�t d�lku shodnou s�d�lkou argumentu
            'columns'.  M�sto vnit�n� sekvence sm� b�t kdekoliv 'None', na
            takov�m m�st� bude m�sto ��dku tabulky vygenerov�na horizont�ln�
            ��ra.
          vmargin (jako kl��ovan� argument) -- je-li 'None', je mezi ��dky
            tabulky ponech�na implicitn� mezera; je-li 0, nen� mezi ��dky
            tabulky ��dn� mezera; jin� hodnoty argumentu nejsou povoleny

        """
        super(Table, self).__init__()
        assert not some(lambda c: c not in ('vmargin',), kwargs.keys())
        assert is_sequence(columns)
        vmargin = kwargs.get('vmargin')
        assert vmargin in (None, 0)
        if len(columns) > 26:
            raise TemplateException(_("V�ce ne� 26 sloupc� v�tabulce"))
        self._columns = columns
        self._data = data
        self._vmargin = vmargin
        
    def columns(self):
        """Vra� specifikaci sloupc� tabulky zadanou v�konstruktoru."""
        return self._columns

    def data(self):
        """Vra� obsah argumentu 'data' zadan�ho v�'__init__()'."""
        return self._data

    def vmargin(self):
        """Vra� hodnotu argumentu 'vmargin' zadan�ho v�'__init__()'."""
        return self._vmargin
    

class LongTable(Table):
    """Tabulka, potenci�ln� v�cestr�nkov�.

    Tabulka je d�na sv�mi sloupci a funkc� generuj�c� jej� ��dky.  D�lka
    tabulky nen� omezena, p�ekro��-li tabulka hranice str�nky, pokra�uje na
    str�nk�ch dal��, p�i�em� na ka�d� dal�� str�nce je uvozena stejnou
    hlavi�kou jako na sv�m za��tku.

    Pro kr�tk� tabulky s�v�cem�n� fixn�m po�tem ��dk� lze pou��t zna�ku
    'Table'.

    """
    class Column(Table.Column):
        """Specifika�n� t��da sloupce dlouh� tabulky."""
        
        def __init__(self, label, width, **kwargs):
            """Stejn� jako v�p�edkovi a� na argumenty 'label' a 'width'.

            Argumenty 'label' a 'width' jsou povinn� a nesm� b�t 'None'.

            """
            Table.Column.__init__(self, label, width, **kwargs)

    def __init__(self, columns, row_generator, row_generator_init=None,
                 separator_height=0, line_separator_height=0,
                 separator_margin=0, line_separator_margin=0):
        """Inicializuj instanci.

        Argumenty:

          columns -- sekvence instanc� t��dy 'LongTable.Column', mus� m�t
            nejm�n� 1 a nejv��e 26�prvk�
          row_generator -- funkce generuj�c� ��dky datov� tabulky.  Funkce je
            opakovan� vol�na, bez argument�, a�mus� v�dy vr�tit nov� ��dek
            tabulky, v�podob� sekvenc� form�tovac�ch element� odpov�daj�c�ch
            sloupc�m tabulky, v�etn� po�ad�.  Pokud funkce vr�t� 'None', je
            tabulka ukon�ena.
          row_generator_init -- funkce volan� bez argument� p�ed prvn�m vol�n�m
            'row_generator', nebo 'None'
          separator_height -- tlou��ka odd�lovac� ��ry mezi z�hlav�m tabulky a
            jej�mi ��dky v�bodech, jako float
          line_separator_height -- tlou��ka odd�lovac� ��ry za ka�d�m ��dkem
            tabulky v�bodech, jako float
          separator_margin -- vzd�lenost odd�lovac� ��ry z�hlav� tabulky od
            z�hlav� v�bodech, jako float
          line_separator_margin -- vzd�lenost odd�lovac� ��ry od ka�d�ho ��dku
            tabulky v�bodech, jako float

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
        """Vra� funkci generuj�c� ��dku tabulky zadanou v�konstruktoru."""
        return self._row_generator

    def row_generator_init(self):
        """Vra� argument 'row_generator_init' metody '__init__()'."""
        return self._row_generator_init

    def separator_height(self):
        """Vra� tlou��ku odd�lovac� ��ry z�hlav� zadanou v�konstruktoru."""
        return self._separator_height
    
    def line_separator_height(self):
        """Vra� tlou��ku odd�lovac� ��ry ��dk� zadanou v�konstruktoru."""
        return self._line_separator_height

    def separator_margin(self):
        """Vra� vzd�lenost odd�lovac� ��ry z�hlav� zadanou v�konstruktoru."""
        return self._separator_margin
    
    def line_separator_margin(self):
        """Vra� vzd�lenost odd�lovac� ��ry ��dk� zadanou v�konstruktoru."""
        return self._line_separator_margin


class Image(_Mark):
    """EPS obr�zek."""
    
    def __init__(self, file_name):
        """Inicializuj instanci.

        Argumenty:

          file_name -- jm�no souboru obr�zku, relativn� k�adres��i definovan�mu
            konfigura�n� volbou 'def_dir'.

        """
        self._file_name = file_name

    def file_name(self):
        """Vra� jm�no souboru zadan� v�konstruktoru."""
        return self._file_name
