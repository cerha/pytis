# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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

"""T��dy pro specifikaci prezenta�n� vlastnost� formul���.

T��da 'ViewSpec' zast�e�uje ostatn� specifika�n� t��dy definovan� t�mto
modulem ('FieldSpec', 'GroupSpec', 'LayoutSpec').

Vytvo�en� instance formul��ov� t��dy je potom v podstat� interpretac�
p��slu�n�ch specifikac�.

V�echny t��dy tohoto modulu maj� specifika�n� charakter a jejich instance jsou
pova�ov�ny za immutable, tud� mohou b�t libovoln� sd�leny.

"""

import pytis.data

from pytis.util import *
from pytis.presentation import *

class TextFormat(object):
    """Konstanty pro definici vstupn�ho form�tu textu."""
    PLAIN = 'PLAIN'
    HTML = 'HTML'
    WIKI = 'WIKI'

class BorderStyle(object):
    """V��tov� t��da definuj�c� konstanty pro styl or�mov�n�."""
    ALL    = 'ALL'
    """Mezera je kolem dokola."""
    TOP    = 'TOP'
    """Mezera je jen naho�e."""
    BOTTOM = 'BOTTOM'
    """Mezera je jen dole."""
    LEFT   = 'LEFT'
    """Mezera je jen vpravo."""
    RIGHT  = 'RIGHT'
    """Mezera je jen vlevo."""

    
class Color(object):
    """Na GUI toolkitu nez�visl� konstanty pro n�kter� barvy."""
    WHITE = 'WHITE'
    BLACK = 'BLACK'
    RED = 'RED'
    RED20 = 'RED20'
    GREEN = 'GREEN'
    BLUE = 'BLUE'
    YELLOW = 'YELLOW'
    GRAY   = 'GRAY'
    GRAY10 = 'GRAY10'
    GRAY20 = 'GRAY20'
    GRAY30 = 'GRAY30'
    GRAY40 = 'GRAY40'
    GRAY50 = 'GRAY50'
    GRAY60 = 'GRAY60'
    GRAY70 = 'GRAY70'
    GRAY80 = 'GRAY80'
    GRAY90 = 'GRAY90'
    BLANCHETALMOND = 'BLANCHETALMOND'
    LIGHTYELLOW = 'LIGHTYELLOW'
    PEACHPUFF2 = 'PEACHPUFF2'
    SLATEGRAY2 = 'SLATEGRAY2'
    LIGHTSALMON = 'LIGHTSALMON'

    
class FieldStyle(object):
    """Specifika�n� t��da definuj�c� podobu vnit�ku pol��ka s�hodnotou."""

    def __init__(self, foreground=Color.BLACK, background=Color.WHITE,
                 bold=False, slanted=False):
        """Inicializuj instanci.

        Argumenty:

          foreground -- barva textu pol��ka, jedna z�konstant t��dy 'Color'
          background -- barva pozad� pol��ka, jedna z�konstant t��dy 'Color'
          bold -- p��znak ur�uj�c�, zda m� b�t text pol��ka tu�n�
          slanted -- p��znak ur�uj�c�, zda m� b�t text pol��ka sklon�n�
          
        """
        self._foreground = foreground
        self._background = background
        self._bold = bold
        self._slanted = slanted

    def foreground(self):
        """Vra� barvu textu zadanou v�konstruktoru."""
        return self._foreground

    def background(self):
        """Vra� barvu pozad� zadanou v�konstruktoru."""
        return self._background

    def bold(self):
        """Vra� pravdu, pr�v� kdy� m� text blikat."""
        return self._bold

    def slanted(self):
        """Vra� pravdu, pr�v� kdy� m� b�t text tu�n�."""
        return self._slanted


FIELD_STYLE_DEFAULT = FieldStyle()
FIELD_STYLE_EMPHASIS = FieldStyle(bold=True)
FIELD_STYLE_WARNING = FieldStyle(foreground=Color.RED)


class Orientation(object):
    """V��tov� t��da definuj�c� konstanty pro sm�rovou orientaci."""
    HORIZONTAL = 'HORIZONTAL'
    """Horizont�ln� orientace."""
    VERTICAL = 'VERTICAL'
    """Vertik�ln� orientace."""

    
class Button(object):
    """Specifikace tla��tka nav�zan�ho na proceduru pro pou�it� ve formul���ch.

    Takto lze do formul��e um�stit tla��tka, jejich� stisk vyvol� libovolnou
    u�ivatelem definovanou akci.  P�i spu�t�n� akce p�itom lze p�istupovat k
    aktu�ln�m hodnot�m pol��ek formul��e (viz konstruktor).

    Tla��tko lze um�stit do LayoutSpec jako jednu z polo�ek (v�ude tam, kde
    b�n� uv�d�me id pol��ka pro um�st�n� vstupn�ho pol��ka, nebo vno�enou
    LayoutSpec).  Pokud ve specifikaci nen� 'LayoutSpec' definov�na explicitn�,
    nelze tla��tko pou��t.

    """
    
    def __init__(self, label, handler, width=None, tooltip=None,
                 active_in_popup_form=True):
        """Inicializuj specifika�n� instanci.

        Argumenty:

          label -- n�pis tla��tka jako string.
          
          handler -- funkce jednoho argumentu, kter�m je instance
            'PresentedRow' obsahuj�c� aktu�ln� hodnoty pol��ek formul��e.  Tato
            funkce je vyvol�na p�i stisku tla��tka.
            
          width -- ���ka (po�et znak�).  Implicitn� je ���ka nastavena
            automaticky podle ���ky n�pisu ('label'), ale pokud je tento
            argument specifikov�n, bude ���ka nastavena podle dan� celo��seln�
            hodnoty.
            
          tooltip -- text, kter� se zobraz� jako bublinov� n�pov�da pro toto
            tla��tko.
            
          active_in_popup_form -- Pokud je zde specifikov�na pravdiv� hodnota,
            nebude tla��tko aktivn� v popup (mod�ln�ch) formul���ch.  To je
            ur�eno zejm�na pro tla��tka, kter� maj� vyvolat otev�en� nov�ho
            formul��e na z�sobn�ku oken aplikace, co� nen� pr�v� v dob� pr�ce s
            mod�ln�m formul��em mo�n�.
        
        """
        assert isinstance(label, types.StringTypes)
        assert callable(handler)
        assert width is None or isinstance(width, types.IntType)
        assert tooltip is None or isinstance(tooltip, types.StringTypes)
        assert isinstance(active_in_popup_form, types.BooleanType)
        self._label = label
        self._handler = handler
        self._width = width
        self._tooltip = tooltip
        self._active_in_popup_form = active_in_popup_form
        
    def label(self):
        return self._label
    
    def handler(self):
        return self._handler
    
    def width(self):
        return self._width
    
    def tooltip(self):
        return self._tooltip
    
    def active_in_popup_form(self):
        return self._active_in_popup_form

class ActionContext(object):
    """V��tov� t��da definuj�c� konstanty pro ur�en� kontextu akce."""
    
    CURRENT_ROW = 'CURRENT_ROW'

    """Akce je prov�d�na nad aktu�ln�m ��dkem tabulky.  Ten bude p�ed�n
    handleru akce jako pozi�n� argument v podob� instance PresentedRow."""
    
    SELECTION = 'SELECTION'

    """Akce je prov�d�na nad aktu�ln�m v�b�rem, tedy nad v�emi vybran�mi ��dky
    tabulky.  V�b�r bude p�ed�n handleru akce jako pozi�n� argument v podob�
    iter�toru, kter� vrac� jednotliv� ��dky jako instance PresentedRow."""

    # TODO: Zde by je�t� mohla b�t jedna hodnota, kter� by umo�nila definovat
    # univerz�ln� akce, kter� pracuj� implicitn� s aktu�ln�m ��dkem, ale pokud
    # existuje v�b�r, tak s v�b�rem.
    

class _ActionItem(object):
    
    def __init__(self, title):
        assert isinstance(title, types.StringTypes)
        self._title = title

    def title(self, raw=False):
        """Vra� n�zev akce."""
        title = self._title
        if not raw:
            title = title.replace("&", "")
        return title
            
    
class Action(_ActionItem):
    """Definice kontextov� z�visl� akce.

    Tato definice akce slou�� pro pou�it� ve specifik�toru 'actions' t��dy
    'ViewSpec'.  Ka�d� akce je o�et�ena vlastn� obslu�nou funkc�, co� umo��uje
    implementovat libovolnou vlastn� funkcionalitu.  Pro ka�dou akci lze
    definovat tak� kontext, kter� ur�uje kdy m� akce smysl a jak� argumenty
    budou handleru akce p�ed�ny.  T�m je nap��klad mo�n�, aby akce pracovala s
    aktu�ln�m ��dkem tabulky apod.  V�ce viz argumenty konstruktoru.
    
    """
    def __init__(self, title, handler, context=ActionContext.CURRENT_ROW,
                 secondary_context=None, enabled=True, access_groups=None,
                 descr=None, hotkey=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          title -- titulek akce zobrazen� v u�ivatelsk�m rozhran�.

          handler -- callable objekt o�et�uj�c� danou akci.  Handleru jsou p�i
            vyvol�n� akce p�ed�ny argumenty odpov�daj�c� dan�mu kontextu.
            Pokud argument 'context' nen� None, bude p�ed�n prvn� pozi�n�
            argument.  Je to bu�to instance 'PresentedRow' odpov�daj�c�
            aktu�ln�mu ��dku, nebo sekvence vybran�ch ��dk�, v z�vislosti na
            hodnot� argumentu 'context'.  Pokud je definov�n tak� argument
            'secondary_context', bude p�ed�n tak� druh� pozi�n� argument
            odpov�daj�c� kontextu ve druh�m formul��i du�ln�ho formul��e.  D�le
            jsou handleru p�ed�ny tak� ve�ker� zbyl� kl��ov� argumenty.
        
          context -- Instance 'ActionContext' ur�uj�c� v jak�m kontextu m��e
            b�t akce vyvol�na.  Tato hodnota ovliv�uje argumenty, kter� jsou
            handleru akce p�ed�ny p�i jej�m vyvol�n�.  M��e b�t tak� None, v
            kter�m�to p��pad� nejsou handleru p�ad�v�ny ��dn� argumenty.
        
          secondary_context -- Instance 'ActionContext', nebo None.  N�kter�
            akce mohou v du�ln�m formul��i pracovat i s kontextovou informac� z
            druh�ho formul��e.  T�mto argumentem, podobn� jako argumentem
            'context' ur�ujeme s ��m se pracuje.  Specifikace ovlivn� druh�
            pozi�n� argument p�edan� handleru akce.  Pokud je None, s ��dn�m
            dal��m kontextem se nepracuje a druh� pozi�n� argument se handleru
            nep�ed�v�.

          enabled -- funkce, vracej�c� pravdu, pokud je akce aktivn� a nepravdu
            v opa�n�m p��pad�.  Funkci jsou p�ad�ny stejn� argumenty, jako
            handleru.  Nen�-li uvedeno, je akce aktivn� v z�vislosti na
            'access_groups'.  Nam�sto funkce m��e b�t p�ed�na t� p��mo boolean
            hodnota, kter� dostupnost akce ur�uje staticky.

          access_groups -- seznam u�ivatelsk�ch skupin, kter� maj� pr�vo akci
            vyvolat.  Akce se pro ostatn� u�ivatele stane automaticky
            neaktivn�.  Teprve pokud u�ivatel pat�� do jedn� z vyjmenovan�ch
            skupin, je dostupnost akce zji�t�na pomoc� funkce 'enabled'.
              
          descr -- textov� popis akce, kter� m��e b�t pou�it jak k vytvo�en�
            n�pov�dy, tak k zobrazen� v u�ivatelsk�m rozhran�.

          hotkey -- p��padn� kl�vesov� zkratka, kter� akci vyvol�.

          V�echny ostatn� kl��ov� argumenty budou p�i vyvol�n� akce p�ed�ny
          handleru jako kl��ov� argmenty.  Takto nap��klad lze jeden handler
          pou��t pro v�ce podobn�ch akc�.
        
        """
        assert callable(handler)
        assert context in (None,) + public_attributes(ActionContext)
        assert secondary_context in (None,) + public_attributes(ActionContext)
        assert callable(enabled) or isinstance(enabled, types.BooleanType)
        assert access_groups is None or \
               isinstance(access_groups,
                          (types.StringType, types.TupleType, types.ListType))
        assert descr is None or isinstance(descr, types.StringTypes)
        assert hotkey is None or isinstance(hotkey, (types.StringType,
                                                     types.TupleType))
        self._handler = handler
        self._context = context
        self._secondary_context = secondary_context
        self._enabled = enabled
        self._access_groups = access_groups
        self._descr = descr
        self._hotkey = hotkey
        self._kwargs = kwargs
        super(Action, self).__init__(title)
        
    def handler(self):
        """Vra� obslu�nou funkci akce."""
        return self._handler

    def context(self):
        """Vra� kontext akce jako instanci 'ActionContext'."""
        return self._context
    
    def secondary_context(self):
        """Vra� p��davn� kontext akce, pokud je definov�n, nebo None."""
        return self._secondary_context

    def enabled(self):
        """Vra� funkci k zji�t�n� dostupnosti akce, nebo p��mo bool hodnotu."""
        return self._enabled
        
    def access_groups(self):
        """Vra� seznam u�iv. skupin kter� maj� pr�vo akci vyvolat.""" 
        return self._access_groups
        
    def descr(self):
        """Vra� popis akce.""" 
        return self._descr
        
    def hotkey(self):
        """Vra� kl�vesovou zkratku akce.""" 
        return self._hotkey
    
    def kwargs(self):
        """Vra� kl��ov� argumenty pro handler akce."""
        return self._kwargs
    

class ActionGroup(_ActionItem):
    """Definice pojmenovan� logick� skupiny akc�.

    Skupiny akc� slou�� k logick�mu seskupen� souvisej�c�ch akc�.  V
    u�ivatelsk�m rozhran� se takto definovan� akce nap��klad zobraz� jako
    samostatn� podmenu v menu akc�.

    """
    def __init__(self, title, *actions):
        """Inicializuj instanci.

        Argumenty:
        
          title -- n�zev skupiny jako �et�zec

          actions -- obsah t�to skupiny.  Zde plat� rekurz�vn� stejn� pravidla
            jako pro stejnojmennn� argument konstruktoru ViesSpec.

        """
        assert isinstance(actions, (types.ListType, types.TupleType))
        if __debug__:
            for x in actions:
                if isinstance(x, (types.TupleType, types.ListType)):
                    for y in x:
                        assert isinstance(y, (Action, ActionGroup))
                else:
                    assert isinstance(x, (Action, ActionGroup))
        self._actions = actions
        super(ActionGroup, self).__init__(title)
        
    def actions(self):
        """Vra� seznam akc� jako tuple."""
        return self._actions
    
    
class GroupSpec(object):
    """Definice skupiny vstupn�ch pol� edita�n�ho formul��e.

    Tato specifikace se nestar� o vzhled jednotliv�ch vstupn�ch pol�, pouze
    definuje jejich rozlo�en� ve formul��i.

    Skupina m��e obsa�en� prvky skl�dat horizont�ln�, nebo vertik�ln� a skupiny
    se mohou libovoln� vno�ovat (prvkem je bu�to p��mo vstupn� pol��ko, nebo
    jin� skupina -- viz argument 'items' konstruktoru).

    Dal��mi argumenty konstruktoru lze ur�it rozestupy pol��ek, mezeru kolem
    cel� skupiny, styl or�mov�n�, nadpis skupiny apod.

    Postup skl�d�n� pol��ek a skupin ve formul��i je n�sledovn�:

    Podle orientace skupiny jsou obsa�en� celky skl�d�ny bu�to horizont�ln�
    (vedle sebe), nebo vertik�ln� (nad sebe).  Obsa�en�mi celky se rozum�
    sekvence za sebou n�sleduj�c�ch pol��ek a vno�en�ch skupin.  V�echna za
    sebou n�sleduj�c� pol��ka jsou skl�d�na pod sebe do m��ky (nehled� na to,
    jde-li o vertik�ln�, nebo horizont�ln� skupinu).  Teprve celky takto
    seskupen�ch pol��ek a vno�en�ch skupin jsou skl�d�ny podle orientace
    skupiny.  Samostatn� vedle sebe um�st�n� pol��ka je mo�no vytvo�it jejich
    um�st�n�m do samstatn�ch vno�en�ch podskupin (jednoprvkov�ch).
    
    """
    def __init__(self, items, orientation=Orientation.HORIZONTAL, label=None,
                 gap=2, space=1, border=3, border_style=BorderStyle.ALL):
        """Inicializuj instanci.

        Argumenty:

          items -- obsah t�to skupiny jako sekvence vno�en�ch skupin (instanc�
            'GroupSpec'), nebo p��mo vstupn�ch pol��ek ur�en�ch sv�m
            identifik�torem (�et�zec).
            
          orientation -- orientace skl�d�n� obsa�en�ch prvk�; konstanta
            t��dy 'Orientation'.
            
          label -- n�zev skupiny uveden� v z�hlav� r�me�ku - pokud nen� None,
            skupina bude or�mov�na; string;
            
          gap -- velikost vertik�ln� mezery mezi jednotliv�mi pol��ky
            v dialog units; integer; 1 du = 1/4 ���ky b�n�ho znaku.
            Relevantn� pouze pokud 'items' obsahuje p��mo pol��ka.
            
          space -- velikost mezery mezi pol��kem a jeho labelem v du; integer;
            Relevantn� pouze pokud 'items' obsahuje p��mo pol��ka.
            
          border -- velikost mezery kolem cel� skupiny v du; integer;
          
          border_style -- styl or�mov�n�; mezera je implicitn� ze v�ech stran,
            m��e v�ak b�t pouze vpravo, vlevo, naho�e, nebo dole; Konstanta
            t��dy 'BorderStyle'.

        'label' je v�dy pova�ov�n za jazykov� z�visl� text a tud� automaticky
        podl�h� jazykov� konverzi.

        """
        assert is_sequence(items)
        assert label is None or is_anystring(label)
        assert type(gap) == type(0)
        assert gap >= 0
        assert orientation in public_attributes(Orientation)
        assert border_style in public_attributes(BorderStyle)
        for item in items:
            # nen� t�eba kontrolovat rekurzivn�, proto�e kontrola prob�hne pro
            # ka�dou instanci na jej� �rovni...
            assert isinstance(item, GroupSpec) or isinstance(item, Button) \
                   or is_anystring(item), (item, label)
        self._items = items
        self._label = label
        self._orientation = orientation
        self._gap = gap
        self._space = space
        self._border = border
        self._border_style = border_style

    def items(self):
        """Vra� prvky skupiny jako tuple."""
        return tuple(self._items)

    def label(self):
        """Vra� n�zev skupiny."""
        return self._label

    def orientation(self):
        """Vra� orientaci skl�d�n� prvk�; konstanta t��dy 'Orientation'."""
        return self._orientation

    def gap(self):
        """Vra� ���ku mezery vertik�ln� mezi pol��ky v du."""
        return self._gap

    def space(self):
        """Vra� ���ku mezery mezi pol��kem a jeho labelem v du."""
        return self._space

    def border(self):
        """Vra� ���ku mezery kolem cel� skupiny v du."""
        return self._border

    def border_style(self):
        """Vra� styl mezery kolem skupiny jako konstantu 'BorderStyle'."""
        return self._border_style

    
class HGroup(GroupSpec):
    """Horizont�ln� seskupen� pol��ek.

    Tato t��da je pouze pohodln�j��m rozhran�m k t��d� 'GroupSpec'.

    """
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        super(HGroup, self).__init__(items, **kwargs)

        
class VGroup(GroupSpec):
    """Vertik�ln� seskupen� pol��ek.

    Tato t��da je pouze pohodln�j��m rozhran�m k t��d� 'GroupSpec'.

    """
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.VERTICAL
        super(VGroup, self).__init__(items, **kwargs)

        
class LHGroup(HGroup):
    """Horizont�ln� seskupen� pol��ek s labelem a or�mov�n�m.

    Tato t��da je pouze pohodln�j��m rozhran�m k t��d� 'GroupSpec'.

    """
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LHGroup, self).__init__(*items, **kwargs)

        
class LVGroup(VGroup):
    """Vertik�ln� seskupen� pol��ek s labelem a or�mov�n�m.

    Tato t��da je pouze pohodln�j��m rozhran�m k t��d� 'GroupSpec'.

    """
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LVGroup, self).__init__(*items, **kwargs)
    

class LayoutSpec(object):
    """Specifikace rozm�st�n� vstupn�ch pol� edita�n�ho formul��e.

    Edita�n� formul�� pro jeden z�znam tabulky (na �rovni u�ivatelsk�ho
    rozhran�) se sest�v� z n�kolika edita�n�ch pol� - jedno pro ka�dou polo�ku
    z�znamu.  Tato pole mohou b�t vizu�ln� seskupena do skupin.  Skupina je
    specifikov�na instanc� t��dy 'GroupSpec'.  Zp�sob rozlo�en� pol� ve skupin�
    je pops�n v dokumentaci t��dy 'GroupSpec'.

    """
    def __init__(self, caption, group, order=None):
        """Inicializace a dopln�n� defaultn�ch hodnot atribut�.

        Argumenty:
        
          caption -- nadpis edita�n�ho formul��e jednoho z�znamu
          
          group -- specifikace skupiny pol��ek nejv���� �rovn�; instance
            'GroupSpec'. Tato skupina m��e obsahovat dal�� vno�en� skupiny
            (viz dokumentace t��dy 'GroupSpec').
            
          order -- specifikace po�ad� proch�zen� mezi pol��ky jako sekvence
            �at�zc� - identifik�tor� pol��ek.  Pokud nen� None, je po�ad�
            proch�zen� pol��ek ur�eno po�ad�m jejich identifik�tor� v t�to
            sekvenci.  V takov�m p��pad� mus� sekvence obsahovat identifik�tory
            v�ech pol��ek obsa�en�ch v 'group'.  Pokud je ponech�na v�choz�
            hodnota 'None', je po�ad� proch�zen� d�no po�ad�m pol��ek v
            'group' p�i proch�zen� stromu do hloubky.  Tento v�choz� zp�sob
            ur�en� po�ad� v naprost� v�t�in� p��pad� vyhovuje a je z pohledu
            u�ivatele nejp�irozen�j��, proto se pou��t� tohoto argumentu
            doporu�uje jen v nevyhnuteln�ch p��padech!  Prioritn�m �e�en�m by
            v�dy m�la b�t reorganizace skupin formul��e.

        'caption' je v�dy pova�ov�n za jazykov� z�visl� text a tud� automaticky
        podl�h� jazykov� konverzi.

        """
        assert caption is None or is_anystring(caption)
        assert isinstance(group, GroupSpec)
        assert order is None or is_sequence(order)
        self._caption = caption
        self._group = group
        def find_fields(group):
            # Extract field ids from group by recursing it.
            fields = []
            for item in group.items():
                if isinstance(item, GroupSpec):
                    fields += find_fields(item)
                elif not isinstance(item, Button):
                    fields.append(item)
            return fields
        if order is None:
            order = find_fields(group)
        elif __debug__:
            found = find_fields(group)
            for id in order:
                assert is_string(id)
                assert id in found, \
                       (_("Invalid field id in 'order' specification:"), id)
            for id in found:
                assert id in order, \
                       (_("Field id missing in 'order' specification:"), id)
            assert len(found) == len(order), \
                   _("Duplicate field id in 'order' spcification.")
        self._order = order

    def caption(self):
        """Vra� nadpis pro edita�n� formul�� jednoho z�znamu."""
        return self._caption

    def group(self):
        """Vra� skupinu pol��ek nejv���� �rovn�; instance 'GroupSpec'."""
        return self._group
    
    def order(self):
        """Vra� tuple id v�ech pol��ek edita�n�ho formul��e v po�ad� proch�zen�.
        
        Pokud nebylo po�ad� v konstruktoru ur�eno, odpov�d� po�ad� ve skupin�ch.

        """
        return self._order


class ViewSpec(object):
    """Kompletuj�c� specifikace prezenta�n�ch vlastnoost� pro formul��e.

    Instance t�to t��dy zn� ve�ker� prezenta�n� vlasnosti ur�it� entity
    (tabulky z pohledu aplikace).  T��da definuje API pro p��stup k t�mto
    vlastnostem.  Toto API je vyu��v�no formul��ov�mi t��dami.

    Ka�d� instance t�to t��dy definuje vlastnosti pro v�echny zp�soby
    zobrazen� (edita�n� formul��, edita�n� seznam, apod.).

    Ka�d� typ formul��e z�potom vyu��v� ze specifikace pouze tu ��st, kter� je
    pro n�j relevantn�.

    """
    
    def __init__(self, title, fields, singular=None, layout=None, columns=None,
                 actions=(), sorting=None, grouping=None, redirect=None,
                 check=None, cleanup=None, on_new_record=None,
                 on_edit_record=None, on_delete_record=None,
                 on_line_commit=None, focus_field=None, description=None,
                 help=None, row_style=FIELD_STYLE_DEFAULT):
        
        """Inicializuj instanci.

        Argumenty:

          title -- n�zev n�hledu (�et�zec).  N�zev je pou��v�n jako titulek
            z�hlav� seznamov�ch formul��� a na dal��ch m�stech, kde je
            odkazov�no na n�hled jako celek, tedy celou mno�ninu z�znam�, proto
            by m�lo b�t pou�ito mno�n� ��slo, nap�. Faktury.
            
          singular -- n�zev n�hledu jedn� polo�ky (�et�zec).  Tento n�zev je
            pou��v�n v�ude tam, kde jde o jednu polo�ku n�hledu (nap� z�hlav�
            edita�n�ho fotmul��e), proto by m�l b�t v jednotn�m ��sle,
            nap�. Faktura.  Pokud je None, bude pou�it n�zev dan� argumentem
            'title'.
            
          fields -- specifikace pol��ek jako sekvence instanc� t��dy
            'FieldSpec'.
            
          layout -- specifikace rozlo�en� pol��ek v edita�n�m formul��i,
            instance t��dy 'GroupSpec'.  Je mo�n� p�edat tak� sekvenci
            identifik�tor� pol��ek -- v tom p��pad� bude vytvo�ena horizont�ln�
            skupina obsahuj�c� vyjmenovan� pol��ka.  Pokud je None, bude
            v�choz� rozlo�en� sestaveno poskl�d�n�m v�ech pol��ek definovan�ch
            ve fields.  Pro zp�tnou kompatibilitu je mo�n� pou��t tak�
            'LayoutSpec', ale tento zp�sob definice je pova�ov�n za nevhodn� a
            v budoucnu nebude podporov�n.
            
          columns -- specifikace sloupc� tabulkov�ho formul��e, sekvence
            indentifik�tor� pol��ek z 'fields'.  Pokud nen� ur�eno, bude
            v�choz� seznam sloupc� obsahovat v�echna pol��ka z fields, kter�
            nemaj� 'column_width' nastaveno na nulu.
            
          actions -- specifikace dostupn�ch u�ivatelsk�ch akc� jako sekvence
            instanc� 'Action', vno�en�ch sekvenc�, nebo instanc� 'ActionGroup'.
            V nejjednodu���m p��pad� jde o prost� seznam instanc� 'Action'.
            Pokud chceme ovlivnit reprezentaci seznamu dostupn�ch akc� v
            u�ivatelsk�m rozhran�, je mo�n� akce seskupit do vno�en�ch tupl� �i
            list�.  Takto vytvo�en� skupiny akc� budou odd�leny separ�torem.
            D�le je mo�n� vytvo�it vno�enou pojmenovanou skupinu
            (reprezentovanou jako samostatn� podmenu) pou�it�m instance
            'ActionGroup'.  Prvky v r�mci ka�d� 'ActionGroup' lze d�le
            seskupovat stejn�m zp�sobem.
                        
          sorting -- v�choz� se�azen� tabulky.  Specifikace �azen� ve form�tu
            odpov�daj�c�m argumentu 'sort' metody 'pytis.data.select()', nebo
            None.  Potom je v�choz� se�azen� tabulky podle kl��ov�ho sloupce
            datov�ho objektu vzestupn�.
            
          grouping -- v�choz� vizu�ln� seskupov�n� tabulky.  M��e b�t None,
            idendifik�tor sloupce, nebo tuple idendifik�tor�.  Vizu�ln�
            seskupov�n� umo��uje graficky odli�it skupiny ��dk�, kter�
            n�sleduj� bezprost�edn� po sob� a p�itom maj� stejnou hodnotu v�ech
            seskupovac�ch sloupc�.  To m� v�znam pouze u sloupc�, podle kter�ch
            je z�rove� �azeno.
            
          redirect -- p�esm�rov�n� formul�e pro zobrazen�/editaci jednoho
            z�znamu.  Jedn� se o funkci jednoho argumentu, j�m� je instance
            'PresentedRow' reprezentuj�c� ��dek dat, pro kter� je
            p�esm�rov�n� po�adov�no.  Vr�cenou hodnotou mus� b�t n�zev
            specifikace, nad kterou bude vytv��en� formul�� sestaven.  Pokud
            funkce vr�t� None, nebo nen� ��dn� funkce specifikov�na, k ��dn�mu
            p�esm�rov�n� nedojde.
            
          check -- funkce pro ov��en� integrity dat cel�ho z�znamu.  Jedn� se o
            funkci jednoho argumentu, j�m� je instance t��dy `PresentedRow',
            reprezentuj�c� aktu�ln� hodnoty v�ech pol��ek formul��e.  Na rozd�l
            od validace hodnot pol��ek, kter� z�vis� na datov�m typu a m� k
            dispozici pouze vlastn� obsah pol��ka, m� tato funkce k dispozici i
            hodnoty ostatn�ch pol��ek, tak�e je vhodn� pro ov��en� vz�jemn�
            slu�itelnosti t�chto hodnot.  Tato funkce vrac� None, pokud je
            v�e v po��dku a formul�� m��e b�t v tomto stavu odesl�n, nebo
            id pol��ka, jeho� hodnota zp�sobila neplatnost z�znamu.  Formul��
            by potom m�l u�ivatele vr�tit do editace dan�ho pol�ka.
            
          cleanup -- funkce prov�d�j�c� z�v�re�n� akce p�i uzav�en� formul��e.
            Jedn� se o funkci dvou argument�.  Prvn�m je v�sledn� ulo�en� ��dek
            odpov�daj�c� kone�n�mu stavu datab�ze a druh�m je p�vodn� ��dek z
            formul��e p�ed ulo�en�m (na �rovni datab�ze mohou b�t n�kter�
            hodnoty zm�n�ny �i dopln�ny ``default'' hodnoty apod.).  Druh�
            ��dek obsahuje hodnoty po editaci u�ivatelem, ale pomoc� jeho
            metody `original_row' je mo�n� tak� z�skat p�vodn� hodnoty p�ed
            editac�.  Oba argumenty jsou instance 'PresentedRow'.  Funkce je
            spou�t�na v�dy p�i p�i uzav�en� edita�n�ho formul��e tla��tkem
            ``Ok'' (potvrzen�m) a to i v p��pad�, �e ��dn� data nebyla zm�n�na.
            
          on_new_record -- akce vlo�en� nov�ho z�znamu.  Pokud je None, bude
            provedena v�choz� akce (otev�en� PopupEditForm nad danou
            specifikac�).  P�ed�n�m funkce lze p�edefinovat p�id�n� nov�ho
            z�znamu v dan�m n�hledu libovolnou vlastn� funkcionalitou.  Funkce
            mus� akceptovat kl��ov� argument 'prefill' (viz.
            'pytis.form.new_record()').
            
          on_edit_record -- akce editace z�znamu.  Pokud je None, bude
            provedena v�choz� akce (otev�en� PopupEditForm nad danou
            specifikac�).  P�ed�n�m funkce jednoho kl��ov�ho argumentu,
            j�m� je instance 'PresentedRow', lze p�edefinovat editaci z�znamu
            libovolnou vlastn� funkcionalitou.
            
          on_delete_record -- akce vymaz�n� z�znamu.  Pokud je None, bude
            provedena v�choz� akce (vymaz�n� z�znamu).  P�ed�n�m funkce
            jednoho kl��ov�ho argumentu, j�m� je instance 'PresentedRow', lze
            p�edefinovat vymaz�n� z�znamu libovolnou vlastn�
            funkcionalitou. Pokud tato funkce vrac� None, nedojde k ��dn�m
            dal��m akc�m, pokud vrac� instanc� 'pytis.data.Operator', bude
            provedeno 'pytis.data.delete_many()' s p��slu�nou podm�nkou.
            
          on_line_commit -- akce volan� po ulo�en� ��dku v inline editaci.
            P�ed�n�m funkce jednoho argumentu, j�m� je instance
            `PresentedRow', lze vyvolat dopl�uj�c� akce po editaci inline
            z�znamu.
             
          focus_field -- �et�zcov� hodnota identifik�toru pol��ka ur�uj�c�,
            kter� pol��ko m� po otev�en� formul��e fokus, nebo funkce jednoho
            argumentu, kter�m je PresentedRow pro otev�ran� formul��, a kter�
            vrac� p��slu�n� identifik�tor pol��ka.
            
          description -- popis formul��e.  Kr�tk� text rozsahu jedn� a� dvou
            v�t.  V�ce tak� viz pozn�mka n�e.
          
          help -- podrobn�j�� n�pov�da formul��e form�tovan� jako strukturovan�
            text (wiki).  V�ce tak� viz pozn�mka n�e.

          row_style -- instance t��dy 'FieldStyle' ur�uj�c� vizu�ln� styl
            spole�n� pro v�echna pol��ka, nebo funkce jednoho argumentu
            (instance 'PresentedRow') vracej�c� instanci t��dy 'FieldStyle'.
           
        Pokud nen� argument 'layout' nebo 'columns' uveden, bude vygenerov�n
        implicitn� layout a seznam sloupc�, odpov�daj�c� po�ad� pol��ek ve
        'fields'.
        
        Kl��ov� atributy 'layout' a 'columns' mohou b�t uv�d�ny bez
        identifik�toru a tud� by m�lo b�t zaru�eno, �e budou v budoucnu
        zachov�ny v�etn� po�ad�.

	Argument `help' nech� je vyu��v�n pro rozs�hlej�� popis formul��e,
	kter� vy�aduje form�tov�n�.  Jednoduch� popis v rozsahu jedn� a� dvou
	v�t nech� je uv�d�n jako `description'.  Proto�e se oba popisy
	pou��vaj� v jin�ch situac�ch, nen� pravda, �e sta�� uv�st jeden z nich.
	Description by m�l b�t uveden prakticky v�dy.  Help slou�� pro
	generov�n� n�pov�dy a nam�sto n�ho je mo�n� vytvo�it odpov�daj�c�
	soubor ve zdrojov�m adres��i n�pov�dy (viz tutori�l Help).

        """
        assert isinstance(title, (str, unicode))
        if singular is None:
            if isinstance(layout, LayoutSpec):
                singular = layout.caption()
            else:
                singular = title
        else:
            assert isinstance(singular, (str, unicode))
        assert is_sequence(fields)
        self._field_dict = dict([(f.id(), f) for f in fields])
        self._fields = tuple(fields)
        # Initialize the layout
        if layout is None:
            layout = LayoutSpec(singular,
                                GroupSpec([f.id() for f in self._fields],
                                          orientation=Orientation.VERTICAL))
        elif isinstance(layout, GroupSpec):
            layout = LayoutSpec(singular, layout)
        elif isinstance(layout, (list, tuple)):
            layout = LayoutSpec(singular,
                                GroupSpec(layout,
                                          orientation=Orientation.VERTICAL))
        if __debug__:
            assert isinstance(layout, LayoutSpec)
            def recourse_group(group):
                for item in group.items():
                    if isinstance(item, GroupSpec):
                        recourse_group(item)
                    elif not isinstance(item, Button):
                        assert self._field_dict.has_key(item), \
                               (_("Unknown field id in 'layout' spec.:"), item)
                        if self._field_dict[item].width() == 0:
                            log(OPERATIONAL, "Zero width field in layout:",item)
            recourse_group(layout.group())
            for f in fields:
                assert isinstance(f, FieldSpec)
                assert not isinstance(f.computer(), CbComputer) \
                       or f.computer().field() in self._field_dict.keys()
                for (s, c) in (('computer', f.computer()),
                               ('editable', f.editable())):
                    if isinstance(c, Computer):
                        for dep in c.depends():
                            assert self._field_dict.has_key(dep), \
                              ("Unknown field id '%s' in dependencies for " + \
                               "'%s' specification of '%s'.") % (dep, s, f.id())
        # Initialize `columns' specification parameter
        if columns is None:
            columns = tuple([f.id() for f in self._fields if f.column_width()])
        else:
            if __debug__:
                assert is_sequence(columns)
                for id in columns:
                    assert is_string(id)
                    assert self._field_dict.has_key(id), \
                       (_("Unknown column id in 'columns' specification:"), id)
        # Initialize other specification parameters
        if __debug__:
            for x in actions:
                if isinstance(x, (types.TupleType, types.ListType)):
                    for y in x:
                        assert isinstance(y, (Action, ActionGroup))
                else:
                    assert isinstance(x, (Action, ActionGroup))
        if sorting is not None:
            assert is_sequence(sorting)
            if __debug__:
                for id, dir in sorting:
                    assert self.field(id) is not None
                    assert dir in (pytis.data.ASCENDENT,
                                   pytis.data.DESCENDANT)
        if grouping is None:
            grouping = ()
        else:
            grouping = xtuple(grouping)
            if __debug__:
                for id in grouping:
                    assert self.field(id) is not None
        assert redirect is None or callable(redirect)
        assert check is None or callable(check)
        assert cleanup is None or callable(cleanup)
        assert on_new_record is None or callable(on_new_record)
        assert on_edit_record is None or callable(on_edit_record)
        assert on_delete_record is None or callable(on_delete_record)
        assert on_line_commit is None or callable(on_line_commit)
        assert focus_field is None or callable(focus_field) or \
               isinstance(focus_field, types.StringTypes)
        assert isinstance(row_style, FieldStyle) or callable(row_style)
        assert description is None or isinstance(description, types.StringTypes)
        assert help is None or isinstance(help, types.StringTypes)
        self._title = title
        self._singular = singular
        self._columns = columns
        self._layout = layout
        self._actions = actions
        self._sorting = sorting
        self._grouping = grouping
        self._redirect = redirect
        self._check = check
        self._cleanup = cleanup
        self._on_new_record = on_new_record
        self._on_edit_record = on_edit_record
        self._on_delete_record = on_delete_record
        self._on_line_commit = on_line_commit
        self._focus_field = focus_field
        self._description = description
        self._help = help
        self._row_style = row_style

    def title(self):
        """Vra� n�zev n�hledu jako �et�zec."""
        return self._title

    def singular(self):
        """Vra� n�zev pro jednu polo�ku n�hledu jako �et�zec."""
        return self._singular

    def fields(self):
        """Vra� tuple specifikac� v�ech pol��ek v layoutu."""
        return self._fields
        
    def field(self, id):
        """Vra� specifikaci pol��ka dan�ho 'id' jako instanci 'FieldSpec'.

        Pokud takov� pol��ko neexistuje, vra� 'None'.
        
        """
        return self._field_dict.get(id)
        
    def layout(self):
        """Vra� specifikaci rozvr�en� edita�n�ho formul��e."""
        return self._layout

    def columns(self):
        """Vra� tuple identifik�tor� sloupc� pro tabulkov� formul��."""
        return self._columns

    def actions(self, linear=False):
        """Vra� specifikaci akc�."""
        def linearize(spec):
            actions = []
            for x in spec:
                if isinstance(x, Action):
                    actions.append(x)
                elif isinstance(x, ActionGroup):
                    actions.extend(linearize(x.actions()))
                elif isinstance(x, (types.TupleType, types.ListType)):
                    actions.extend(linearize(x))
                else:
                    raise ProgramError("Invalid action specification: %s" % x)
            return actions
        if linear:
            return linearize(self._actions)
        else:
            return self._actions

    def sorting(self):
        """Vra� specifikaci v�choz�ho �azen�."""
        return self._sorting

    def grouping(self):
        """Vra� tuple id sloupc� v�choz�ho vizu�ln�ho seskupov�n�."""
        return self._grouping

    def redirect(self):
        """Vra� funkci zaji��uj�c� p�esm�rov�n� na jin� n�zev specifikace."""
        return self._redirect
        
    def cleanup(self):
        """Vra� funkci prov�d�j�c� akce p�i uzav�en� formul��e."""
        return self._cleanup

    def check(self):
        """Vra� funkci prov�d�j�c� kontrolu integrity z�znamu."""
        return self._check

    def on_new_record(self):
        """Vra� funkci prov�d�j�c� vlo�en� nov�ho z�znamu, nebo None."""
        return self._on_new_record

    def on_edit_record(self):
        """Vra� funkci prov�d�j�c� editaci z�znamu, nebo None."""
        return self._on_edit_record

    def on_delete_record(self):
        """Vra� funkci prov�d�j�c� maz�n� z�znamu, nebo None."""
        return self._on_delete_record

    def on_line_commit(self):
        """Vra� funkci volanou po ulo�en� inline ��dku."""
        return self._on_line_commit

    def focus_field(self):
        """Vra� �et�zec nebo funkci, ur�uj�c� pol��ko formul��e s fokusem."""
        return self._focus_field

    def description(self):
        """Vra� stru�n� popis n�hledu."""
        return self._description

    def help(self):
        """Vra� podrobnou n�pov�du."""
        return self._help
    
    def row_style(self):
        """Vra� v�choz� styl ��dku, nebo funkci, kter� jej vypo�te."""
        return self._row_style

    
class BindingSpec(object):
    """Specifikace vazby dvou n�hled� p�i propojen� do du�ln�ho formul��e.

    Tato specifikace definuje vlastnosti napojen� dvou formul��� p�i jejich
    spojen� do du�ln�ho formul��e.  Definov�na je jak datov� vazba, tak n�kter�
    prezenta�n� vlastnosti spojen�.

    Pou�it� je n�sleduj�c�:

    Funkce 'binding_spec' ve specifikaci libovoln�ho n�hledu vrac� slovn�k
    v�ech mo�n�ch spojen� tohoto n�hledu s dal��mi jin�mi n�hledy.  Slovn�k je
    kl��ov�n n�zvy specifikac� a hodnotou je pr�v� instance 'BindingSpec'.

    Kdy� je tedy nap��klad vytv��en du�ln� formul�� 'A::B', bude ve specifikaci
    n�hledu 'A' (v roli hlavn�ho formul��e) z�sk�na z 'binding_spec' polo�ka
    pro n�hled 'B' (v roli vedlej��ho formul��e).
    

    """
    
    def __init__(self, title, binding_column, side_binding_column=None,
                 hide_binding_column=True, description=None,
                 append_condition=None, sash_ratio=0.5,
                 orientation=Orientation.HORIZONTAL):
        
        """Inicializuj instanci.

        Argumenty:

          title -- titulek tohoto du�ln�ho spojen� formul��� jako �et�zec.
                        
          binding_column -- identifik�tor vazebn�ho sloupce.  Tento sloupec
            bude pou�it pro filtrov�n� vedlej��ho formul��e p�i pohybu po
            z�znamech v hlavn�m formul��i.  Filtrovac� podm�nka je implicitn�
            rovnost hodnot zvolen�ch sloupc� hlavn�ho a vedlej��ho formul��e.

          side_binding_column -- identifik�tor vazebn�ho sloupce ve vedlej��m
            formul��i, pokud je jin�, ne� `binding_column'.  V�choz� hodnota
            `None' znamen�, �e n�zev vazebn�ho sloupce je ve vedlej��m
            formul��i stejn�, jako v hlavn�m formul��i.
            
          hide_binding_column -- vazebn� sloupec m��e b�t (a implicitn� je)
            ve vedlej��m formul��i vypu�t�n (jeho hodnota je pro v�echny
            vyfiltrovan� z�znamy shodn� -- odpov�d� hodnot� z hlavn�ho
            formul��e).

          description -- textov� popis dan�ho du�ln�ho spojen� formul���.
            
          append_condition -- None nebo funkce jednoho argumentu, kter�m je
            aktu�ln� ��dek hlavn�ho formul��e. V tomto p��pad� mus� funkce
            vr�tit instanci Operator, kter� se p�ipoj� k implicitn�
            podm�nce provazuj�c� vazebn� sloupce.

          sash_ratio -- pom�r rozd�len� plochy formul��� jako desetinn� ��slo v
            rozsahu od nuly do jedn�.  V�choz� hodnota 0.5 znamen�, �e
            rozd�len� bude p�esn� v polovin� a ob�ma formul���m tedy p�ipadne
            stejn� plocha.  Men�i hodnota znamen� men�� horn� formul��, v�t��
            naopak.  Uplatn� se pouze p�i zobrazen� dvou ��dkov�ch formul��� v
            horizont�ln�m rozd�len�.  P�i p��tomnosti edita�n�ho formul��e je
            poloha odd�lova�e ��zena jeho ���kou/v��kou.
            
          orientation -- v�choz� orientace du�ln�ho formul��e jako konstanta
            'Orientation'.  V horizon�ln�m rozd�len� jsou formul��e nad sebou,
            ve vertik�ln�m vedle sebe.

        """
        assert isinstance(title, types.StringTypes)
        assert isinstance(binding_column, types.StringTypes)
        assert description is None or isinstance(description, types.StringTypes)
        assert side_binding_column is None or \
               isinstance(side_binding_column, types.StringTypes)
        assert isinstance(hide_binding_column, types.BooleanType)
        assert append_condition is None or callable(append_condition)
        assert orientation in public_attributes(Orientation)
        assert isinstance(sash_ratio, types.FloatType) and 0 < sash_ratio < 1
        self._title = title
        self._binding_column = binding_column
        if side_binding_column is None:
            side_binding_column = binding_column
        self._side_binding_column = side_binding_column
        self._hide_binding_column = hide_binding_column
        self._description = description
        self._append_condition = append_condition
        self._sash_ratio = sash_ratio
        self._orientation = orientation
        

        
    def title(self):
        """Vra� titulek du�ln�ho formul��e jako �et�zec."""
        return self._title

    def description(self):
        """Vra� n�pov�du pro formul��."""
        return self._description
    
    def binding_column(self):
        """Vra� id vazebn�ho sloupce hlavn�ho formul��e jako �et�zec."""
        return self._binding_column

    def side_binding_column(self):
        """Vra� id vazebn�ho sloupce vedlej��ho formul��e jako �et�zec."""
        return self._side_binding_column

    def hide_binding_column(self):
        """Vra� pravdu, pokud m� b�t vazebn� sloupec skryt ve vedlej��m fm."""
        return self._hide_binding_column

    def append_condition(self):
        """Vra� dopl�uj�c� podm�nku."""
        return self._append_condition
    
    def sash_ratio(self):
        return self._sash_ratio
    
    def orientation(self):
        return self._orientation

    
class DualSpec(BindingSpec):
    """Specifikace du�ln�ho formul��e.
    
    POZOR: Tato t��da by ji� nem�la b�t pou��v�na.  Nam�sto specifika�n�
    funkce 'dual_spec' samostatn� du�ln� specifikaci nech� je nyn�
    pou��v�na funkce 'binding_spec' ve specifikaci hlavn�ho formul��e.
    V�ce tak� viz 'BindingSpec'.
    
    """
    def __init__(self, main_name, side_name, binding_column, title="",
                 side_title=None, side_columns=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          main_name -- jm�no specifikace hlavn�ho formul��e; �et�zec.

          side_name -- jm�no specifikace vedlej��ho formul��e; �et�zec.

          side_title -- titulek vedlej��ho formul��e jako �et�zec.  Pokud nen�
            None, bude v du�ln�m formul�i pou�it tento titulek, nam�sto titulku
            ze specifikace vedlej��ho formul��e.

          side_columns -- sekvence identifik�tor� sloupc� vedlej��ho formul��e.
            Pokud je None, budou ve vedlej��m formul��i zobrazeny v�echny
            sloupce dan� jeho specifikac�.
            
        V�echny ostatn� argumenty jsou shodn� jako u 'BindingSpec', pouze
        argument 'title' zde nen� povinn�.

        """
        assert is_anystring(main_name)
        assert is_anystring(side_name)
        assert side_title is None or is_anystring(side_title)
        assert side_columns is None or is_sequence(side_columns)
        self._main_name = main_name
        self._side_name = side_name
        self._side_title = side_title
        self._side_columns = side_columns
        super(DualSpec, self).__init__(title, binding_column, **kwargs)

    def main_name(self):
        """Vra� n�zev specifikace hlavn�ho formul��e jako �et�zec."""
        return self._main_name
        
    def side_name(self):
        """Vra� n�zev specifikace vedlej��ho formul��e jako �et�zec."""
        return self._side_name
        
    def side_title(self):
        """Vra� titulek vedlej��ho formul��e jako �et�zec."""
        return self._side_title

    def side_columns(self):
        """Vra� seznam id sloupc�, vedlej��ho formul��e."""
        return self._side_columns


    

class Editable(object):
    """V��tov� t��da definuj�c� konstanty ur�uj�c� editovatelnost pol��ka."""
    ALWAYS = 'ALWAYS'
    """Pol��ko je editovateln� v�dy."""
    ONCE = 'ONCE'
    """Pol��ko je editovateln� pouze jednou, p�i vytv��en� nov�ho z�znamu."""
    NEVER = 'NEVER'
    """Pol��ko nen� editovateln� nikdy."""

    
class SelectionType(object):
    """V��tov� t��da definuj�c� konstanty zp�sobu v�b�ru z mno�iny hodnot."""
    CHOICE = 'CHOICE'
    """V�b�r z menu.  Viditeln� je jen pr�v� vybran� hodnota."""
    RADIO_BOX = 'RADIO_BOX'
    """Pro ka�dou hodnotu je zobrazeno za�krt�vac� pol��ko."""
    LIST_BOX = 'LIST_BOX'
    """Viditeln� je vybran� hodnota a \"n�kolik\" kolem."""
    CODEBOOK = 'CODEBOOK'
    """Je mo�n� p��m� z�pis hodnoty nebo vyvol�n� ��seln�kov�ho formul��e."""
    LIST = 'LIST'
    """V�cesloupcov�/v�ce��dkov� v�b�rov� pol��ko pro ��seln�ky."""

   
class PostProcess(object):
    "V��tov� t��da definuj�c� konstanty pro zp�sob zpracov�n� u�iv. vstupu."
    UPPER = 'UPPER'
    """P�eve� ve�ker� p�smena na velk�."""
    LOWER = 'LOWER'
    """P�eve� ve�ker� p�smena na mal�."""

    
class TextFilter(object):
    """V��tov� t��da definuj�c� konstanty pro zp�sob filtrov�n� u�iv. vstupu.
    """
    ASCII = 'ASCII'
    """Non-ASCII characters are filtered out."""
    ALPHA = 'ALPHA'
    """Non-alpha characters are filtered out."""
    FLOAT = 'FLOAT'
    """Non-alpha characters exclude '.' are filtered out."""    
    ALPHANUMERIC = 'ALPHANUMERIC'
    """Non-alphanumeric characters are filtered out."""
    NUMERIC = 'NUMERIC'
    """Non-numeric characters are filtered out."""
    INCLUDE_LIST = 'INCLUDE_LIST'
    """Use an include list.

    The validator checks if the user input is on the list, complaining if
    not.
    """
    EXCLUDE_LIST = 'EXCLUDE_LIST'
    """Use an exclude list.

    The validator checks if the user input is on the list, complaining if it
    is.
    """


class Computer(object):
    """Specifikace funkce pro dopo��t�n� hodnoty sloupce."""
    
    def __init__(self, function, depends=None):
        """Inicializuj specifikaci.

        Argumenty:

          function -- libovoln� funkce vracej�c� hodnotu kompatibiln� s vnit�n�
            hodnotou datov�ho typu odpov�daj�c�ho sloupci, pro kter� je
            pou�ita.
            
          depends -- seznam sloupc�, na kter�ch dan� po��tan� pol��ko z�vis�.
            M�l by obsahovat v�echny sloupce, kter� po��tac� funkce pou��v� pro
            ur�en� v�sledn� hodnoty.  Hodnota potom bude p�epo��t�na pouze
            p�i zm�n� v uveden�ch pol��k�ch. Pokud je uveden pr�zdn� seznam,
            nebude hodnota p�epo��t�na nikdy (st�le v�ak bude vypo��t�na p�i
            inicializaci formul��e). Jedn� se o seznam identifik�tor� sloupc�
            jako �et�zc�.

        """
        import re
        assert callable(function)
        self._function = function
        if depends is None:
            raise ProgramError("Computer has no dependency specification!")
        assert is_sequence(depends)
        self._depends = depends

    def __call__(self, *args, **kwargs):
        return apply(self._function, args, kwargs)

    def function(self):
        """Vra� funkci zadanou v�konstruktoru."""
        return self._function

    def depends(self):
        """Vra� seznam id sloupc�, ne kter�ch po��tan� hodnota z�vis�."""
        return self._depends    


class CbComputer(Computer):
    """Specializovan� computer, kter� z�sk�v� hodnotu z ��seln�ku.
    
    Tento computer automaticky poskytuje dopo��t�vac� funkci, kter� z�sk�v�
    hodnotu z n�kter�ho sloupce ��seln�ku nav�zan�ho na jin� pol��ko stejn�ho
    n�hledu.  Toho lze vyu��t u pol��ek, kter� ve skute�nosti jen zobrazuj�
    dopl�uj�c� informace z ��seln�ku.

    Stejn�ho efektu by sice �lo dos�hnout i pou�it�m standardn�ho computeru s
    p��slu�nou dopo��t�vac� funkc�, ale t�m by se u�ivatelsk� rozhran�
    nedozv�d�lo nic o vazb� dopo��t�van�ho pol��ka na pol��ko s enumer�torem a
    nebylo by schopno poskytnout dal�� u�ite�n� funkce, jako nap��klad otev�en�
    n�hledu ��seln�ku jako akce kontextov�ho menu bu�ky, zobrazen� kl��e
    ��seln�ku p�i aktivaci bu�ky, automatick� ur�en� datov�ho typu virtu�ln�ho
    pol��ka apod.

    """
    def __init__(self, field, column, default=None):
        """Inicializuj instanci.

        Argumenty:
        
          field -- identifik�tor pol��ka ve stejn�m n�hledu, kter� je spojeno s
            ��seln�kem (jeho datov� typ m� enumer�tor typu DataEnumerator).

          column -- sloupe�ek datov�ho objektu enumer�toru, kter� ud�v�
            v�slednou hodnotu dopo��t�vac� funkce.
        
        """
        assert isinstance(field, types.StringType)
        assert column is None or isinstance(column, types.StringType)
        self._field = field
        self._column = column
        self._default = default
        super(CbComputer, self).__init__(self._compute, depends=(field,))
        
    def _compute(self, row):
        cbvalue = row[self._field]
        if cbvalue.value() is not None:
            e = cbvalue.type().enumerator()
            assert e is not None, \
                   "CbComputer refers to '%s', which has no enumerator." \
                   % self._field
            value = e.get(cbvalue.value(), self._column)
            if value:
                return value.value()
        return self._default

    def field(self):
        """Vra� id pol��ka, jeho� enumer�tor je pou�it."""
        return self._field
    
    def column(self):
        """Vra� id sloupce datov�ho objektu enumer�toru, kter� ud�v� hodnotu."""
        return self._column
    
    
class CodebookSpec(object):
    """Specifikace vlastnost� n�hledu pro jeho pou�it� jako ��seln�ku.

    Nepovinn� specifika�n� funkce 'cb_spec' m��e pomoc� instance t�to t��dy
    up�esnit vlastnosti dan�ho n�hledu pro jeho pou�it� v kontextu ��seln�ku.

    """
    def __init__(self, columns=None, sorting=None, display=None,
                 display_size=20, begin_search=None):
        
        """Inicializace a dopln�n� v�choz�ch hodnot atribut�.

        Argumenty:
        
          columns -- sekvence identifik�tor� sloupc�, kter� maj� b�t zobrazeny
            v ��seln�kov�m formul��i (t��da 'CodebookForm').  Pokud je 'None',
            bude ��seln�k zobrazovat v�echny sloupce ze specifikace dan�
            tabulky.
            
          sorting -- sekvence identifik�tor� sloupc�, podle kter�ch maj� b�t
            z�znamy ��seln�kov�ho formul��e set��d�ny.  Pokud je 'None',
            bude pou�ito t��d�n� z ViewSpec.
            
          display -- pokud nen� 'None', ur�uje zp�sob z�st�n� u�ivatelsk�
            hodnoty ��seln�ku (v�ce o jej�m vyu�it� viz n�e).  Hodnotou m��e
            b�t bu�to identifik�tor sloupe�ku v datov�m objektu enumer�toru
            (bude zobrazena hodnota tohoto sloupe�ku), nebo funkce jednoho
            argumentu (vnit�n� Pythonov� hodnota enumer�toru), kter� vrac�
            u�ivatelskou hodnotu (�et�zec).  M��e b�t p�d�na tak� dvojice
            (funkce, identifik�tor sloupe�ku).  V tom p��pad� bude argumentem
            funkce hodnota dan�ho sloupce, nam�sto sloupce vnit�n� hodnoty.

          display_size -- ���ka pol��ka displeje ve znac�ch.  Lze tak�
            p�edefinovat stejnojmen�m argumentem 'FieldSpec' pro konkr�tn�
            ��seln�kov� pol��ko.
          
          begin_search -- None nebo identifik�tor sloupce, nad n�m� se m�
            spustit automatick� inkrement�ln� vyhled�v�n�.

        U�ivatelsk� hodnota ��seln�ku je vyu��v�na v n�kolika situac�ch.  U
        b�n�ho ��seln�kov�ho pl��ka typu `SelectionType.CODEBOOK' je pro jej�
        zobrazen� vytvo�en displej.  U ostatn�ch ��seln�kov�ch pol��ek
        (nap�. CHOICE, RADIO apod.)  jsou u�ivatelsk� hodnoty zobrazeny p��mo
        ve v�b�ru.  U�ivatel v tomto p��pad� vnit�n� hodnotu v�bec nevid�, ta
        je pou��v�na pouze intern�.  Dal��m pou�it�m u�ivatelsk� hodnoty je
        zobrazen� souvisej�c� ��seln�kov� hodnoty ve stavov� ��dce gridu (p�i
        aktivaci bu�ky jej� hodnota poch�z� z ��seln�ku).
                    
        """
        assert columns is None or is_sequence(columns)
        assert sorting is None or is_sequence(sorting)
        assert display is None or isinstance(display, str) \
               or callable(display) or isinstance(display, tuple) \
               and callable(display[0]) and isinstance(display[1], str)
        assert display_size is None or isinstance(display_size, types.IntType)
        assert begin_search is None or isinstance(begin_search,types.StringType)
        self._columns = columns
        self._sorting = sorting
        self._display = display
        self._display_size = display_size
        self._begin_search = begin_search

    def columns(self):
        """Vra� seznam id sloupc�, zobrazen�ch ve v�b�rov�m formul��i."""
        return self._columns
        
    def sorting(self):
        """Vra� seznam id sloupc�, podle kter�ch m� b�t ��seln�k set��d�n."""
        return self._sorting
        
    def display(self):
        """Vra� id sloupce zobrazovan�ho v displeji."""
        return self._display
        
    def display_size(self):
        """Vra� velikost displeje (po�et znak�)."""
        return self._display_size
        
    def begin_search(self):
        """Vra� identifik�tor sloupce pro inkrement�ln� vyhled�v�n�."""
        return self._begin_search


class FormType(object):
    """Specifikace abstraktn�ho typu formul��e podle ��elu jeho otev�en�.

    Tyto konstanty slou�� k ur�en� zp�sobu otev�en� n�hledu.  D�ky t�to
    abstrakci je specifikace nez�visl� na pou�it�ch t��d�ch u�ivatelsk�ho
    rozhran�.  Definujeme pouze ��el, za kter�m je formul�� otev�r�n a nech�me
    u�ivatelsk� rozhran� rozhodnout, kter� konkr�tn� formul�� je v dan� situaci
    nejvhodn�j��.

    """
    
    BROWSE = 'BROWSE'
    """Otev�en� ��dkov�ho n�hledu v podob� tabulky."""
    
    VIEW = 'VIEW'
    """Otev�en� needitovateln�ho n�hledu jednoho z�znamu."""
    
    EDIT = 'EDIT'
    """Otev�en� edita�n�ho formul��e jednoho z�znamu."""
    
    INSERT = 'INSERT'
    """Otev�en� edita�n�ho formul��e pro vlo�en� nov�ho z�znamu."""


class Link(object):
    """Specifikace odkazu pol��ka do jin�ho n�hledu.

    Pou��v� se jako hodnota argumentu 'link' ve 'FieldSpec'.

    """
    
    def __init__(self, name, column, type=FormType.BROWSE, label=None):
        """Inicializuj instanci.

        Argumenty:

          name -- n�zev specifikace odkazovan�ho n�hledu jako �et�zec.

          column -- identifik�tor sloupce v odkazovan�m n�hledu.  Slou�� k
            vyhled�n� z�znamu v odkazovan�m n�hledu, kter� odpov�d� aktu�ln�
            hodnot� odkazuj�c�ho pol��ka.

          type -- typ formul��e, ve kter�m bude odkazovan� n�hled otev�en.
            Jedna z konstant 'FormType'.  V�choz�m typem je 'FormType.BROWSE'.

          label -- titulek odkazu v menu.  Pokud nen� uveden, bude odkaz
            pojmenov�n automaticky a za�azen mezi automaticky generovan�
            odkazy.  Pokud je titulek uveden, bude v u�ivatelsk�m rozhran�
            odkaz uveden samostatn� p�ed v�emi automaticky generovan�mi odkazy.
            
        """
        assert isinstance(name, types.StringType)
        assert isinstance(column, types.StringType)
        assert type in public_attributes(FormType)
        assert label is None or isinstance(label, types.StringTypes)
        self._name = name
        self._column = column
        self._type = type
        self._label = label
                
    def name(self):
        """Vra� n�zev specifikace odkazovan�ho n�hledu."""
        return self._name

    def column(self):
        """Vra� id odpov�daj�c�ho sloupce v odkazovan�m n�hledu."""
        return self._column

    def type(self):
        """Vra� konstantu typu formul��e, kter� m� b�t otev�en."""
        return self._type

    def label(self):
        """Vra� typ formul��e, kter� m� b�t otev�en."""
        return self._label

    
class FieldSpec(object):
    """Specifikace abstraktn�ho pol��ka zobrazuj�c�ho datovou hodnotu.

    Tato specifikace je pou�iteln� pro v�echny druhy pr�ce s�pol��ky
    zobrazuj�c�mi hodnoty, zejm�na v�obrazovkov�ch formul���ch, ��dkov�ch
    formul���ch a v�stupn�ch sestav�ch.

    Ka�d� modul pracuj�c� s�pol��ky si z�t�to pon�kud komplexn� specifikace
    vyb�r� pouze pro n�j relevantn� informace.  P�esn� zp�sob interpretace
    t�chto specifikac� z�vis� na t��d�ch implemenuj�c�ch prvky u�ivatelsk�
    rozhran�.  Detailn� popis je proto v�p��pad� t�to t��dy t�eba hledat v
    dokumentaci t��d 'EditForm', 'ListForm', 'InputField' apod.

    """
    def __init__(self, id, label='', column_label=None, descr=None,
                 virtual=False, dbcolumn=None, type=None, type_=None,
                 width=None, column_width=None, fixed=False, height=None,
                 editable=None, compact=False, default=None,
                 computer=None, line_separator='; ', codebook=None,
                 display=None, display_size=None, allow_codebook_insert=False,
                 codebook_insert_spec=None, codebook_runtime_filter=None,
                 selection_type=None, orientation=Orientation.VERTICAL,
                 post_process=None, filter=None, filter_list=None, style=None,
                 link=(), **kwargs):
        """Inicializace a dopln�n� v�choz�ch hodnot atribut�.

        Argumenty:

          id -- textov� identifik�tor pole; nepr�zdn� �et�zec.  Pod t�mto
            identifik�torem je potom pole p��stupn� ve v�ech operac�ch.
          
          label -- text n�pisu u vstupn�ho pole jako �et�zec.  Sm� b�t uv�d�n
            t� jako pozi�n� argument.  Po�ad� je zaru�eno.
          
          column_label -- nadpis sloupce, je-li pol��ko ve sloupci, jako
            string.  Je-li 'None', je pou�ita hodnota 'label'.
            
          descr -- podrobn�j�� popis v rozsahu cca jedn� v�ty vhodn� nap��klad
            pro zobrazen� bublinov� n�pov�dy.

          virtual -- boolovsk� p��znak.  Pokud je pravdiv�, jde o virtu�ln�
            pol��ko bez vazby na datov� objekt.  Hodnota virtu�ln�ho pol��ka je
            nej�ast�ji vypo�tena pomoc� computeru (viz specifika�n� parametr
            'computer'.  Vzhledem k tomu, �e datov� typ virtu�ln�ho pol��ka
            nelze ur�it automaticky (z datov�ho objektu), je v�choz� typ
            stanoven napevno na 'pytis.data.String()'.  Pokud to nevyhovuje, je
            t�eba typ ur�it explicitn� (viz specifika�n� parametr 'type').

          dbcolumn -- n�zev p��slu�n�ho datab�zov�ho sloupce.  Pokud nen�
            ur�en, je n�zev datab�zov�ho sloupce shodn� s identifik�torem
            pol��ka.  Toho je tak� doporu�ov�no vyu��vat pokud nen� n�jak�
            z�va�n� d�vod, aby byl n�zev sloupce jin�, ne� identifik�or
            pol��ka.
          
          type -- explicitn� ur�en� typu hodnoty, se kterou pracuje toto
            pol��ko; instance 'pytis.data.Type'.  V�choz� datov� typ je ur�en
            podle odpov�daj�c�ho sloupe�ku datov�ho objektu.  Pokud je v�ak
            pol��ko virtu�ln� (viz n�e), je nutn� typ ur�it explicitn� (s
            v�jimkou virtu�ln�ch pol��ek pou��vaj�c�ch 'CbComputer').  Typ
            m��eme tak� explicitn� p�edefinovat, pokud chceme pro prezentaci
            hodnot pou��t jin� typ, ne� v�choz� typ datov�ho rozhran� (ten v�ak
            *mus�* b�t kompatibiln� s typem datov�ho rozhran�).  Viz tak�
            metoda 'type()'.
            
          width -- ���ka pole ve znac�ch; kladn� cel� ��slo, nebo�0,
            v�kter�m�to p��pad� je pole skryt�.  Je-li 'None', bude pou�ita
            implicitn� ���ka.  U n�kter�ch typ� vstupn�ch pol��ek m��e m�t
            speci�ln� v�znam (viz jejich dokumentace).
            
          height -- v��ka pole ve znac�ch, kladn� re�ln� ��slo.  U n�kter�ch
            typ� vstupn�ch pol��ek m��e m�t speci�ln� v�znam (viz jejich
            dokumentace).
          
          column_width -- ���ka sloupce v�tabulce ve znac�ch, kladn� cel�
            ��slo.  Je-li 'None', je pou�ita hodnota 'width'.
            
          fixed -- pokud bude p�ad�na pravdiv� hodnota, nebude ���ka sloupce
            automaticky p�epo��t�v�na p�i zm�n� velikosti tabulkov�ho
            formul��e.  Implicitn� jsou sloupce automaticky
            roztahov�ny/zu�ov�ny tak, aby byla rovnom�rn� vyu�ita plocha
            formul��e.  Hodnota 'width/column_width' tak slou�� pouze jako
            v�choz� hodnota.  Pro 'fixed' sloupce v�ak bude v�dy dodr�ov�na.
            
          editable -- instance Computer nebo jedna z konstant t��dy 'Editable',
            ur�uj�c� za jak�ch okolnost� je pol��ko editovateln�.  Je-li 'None',
            bude pou�ita implicitn� hodnota, kterou je obvykle
            'Editable.ALWAYS', ale pro n�kter� kombinace ostatn�ch parametr�
            (nap�. 'computer') m��e b�t implicitn� hodnota jin�.
            Pokud je editable instanc� t��dy `Computer', budou jeho funkci
            p�ed�ny dva argumenty: instance PresentedRow a identifik�tor
            pol��ka.
            
          compact -- pravdiv� hodnota znamen�, �e bude textov� popisek pol��ka
            v edita�n�m formul��i p�imknut k horn�mu okraji vstupn�ho prvku
            (bude tedy nad pol��kem).  V opa�n�m p��pad� (v�choz� chov�n�) je
            popisek vlevo od pol��ka.
            
          default -- funkce pro v�po�et v�choz� hodnoty pol��ka.  Callable
            object vracej�c� hodnotu kompatibiln� s vnit�n� hodnotou
            odpov�daj�c�ho datov�ho typu (viz argument 'type').
            
          computer -- 'instance t��dy 'Computer', nebo None.  Specifikuje
            dopo��t�van� pol��ko (viz. tak� n�e).
            
          line_separator -- odd�lova� ��dk� v�jedno��dkov�m zobrazen�
            v�ce��dkov� hodnoty.  Tento argument sm� b�t vyu��v�n pouze pro
            read-only pol��ka.
            
          codebook -- n�zev specifikace ��seln�ku (�et�zec), nebo None.  N�zev
            specifikace ��seln�ku je norm�ln� p�eb�r�n ze specifikace
            enumer�toru datov�ho typu odpov�daj�c�ho sloupce v 'DataSpec'.
            Pokud v�ak z n�jak�ho d�vodu datov� specifikace nen� definov�na
            pomoc� t��dy DataSpec, ale p��mo pomoc� t��d datov�ho rozhran�,
            nen� tato informace aplikaci dostupn�.  Potom je nutn� n�zev
            ��seln�ku ur�it zde.
            
          display -- umo��uje definovat vlastn� hodnotu displeje pro konkr�tn�
            pou�it� ��seln�ku.  Pokud je None, bude pou�ita hodnota z 'cb_spec'
            ve specifikaci ��seln�ku (co� by m�lo b�t tak� up�ednost�ov�no).
            Pokud je pou�ito, je v�znam stejn� jako u stejnojmenn�ho argumentu
            `CodebookSpec'.  Relevantn� jen pro pol��ka v��tov�ch typ� (datov�
            typ m� definov�n enumer�tor).

          display_size -- velikost displeje ��seln�ku ve znac�ch.  Relevantn�
            jen pro ��seln�kov� pol��ka.  Pokud je None, bude pou�ita hodnota z
            'cb_spec' ve specifikaci ��seln�ku.

          allow_codebook_insert -- Pravdiv� hodnota povol� zobrazen� tla��tka
            pro p�id�n� nov� hodnoty do ��seln�ku.  Relevantn� jen pro
            ��seln�kov� pol��ka.
            
          codebook_insert_spec -- N�zev specifikace, kter� m� b�t pou�ita pro
            vkl�d�n� nov�ch z�znam� (viz 'allow_codebook_insert').  Pokud je
            None, bude pou�ita hodnota 'codebook', nebo jej� v�choz� hodnota.
            Relevantn� jen pro ��seln�kov� pol��ka, kde 'allow_codebook_insert'
            je pravdiv�.
            
          codebook_runtime_filter -- dopo��t�va� run-time filtrovac� podm�nky
            ��seln�ku; instance `Computer'.  T�m je umo�n�no m�nit mno�inu
            hodnot nav�zan�ho ��seln�ku za b�hu.  Nav�zan� dopo��t�vac� funkce
            dostane jako argument aktu�ln� data formul��e jako instanci
            'PresentedRow' a vrac� filtrovac� podm�nku typu
            'pytis.data.Operator'.  ��seln�k bude po zm�n� z�visl�ch pol��ek
            aktualizov�n tak, aby obsahoval pouze ��dku vyhovuj�c� dan�
            podm�nce.
            
          selection_type -- zp�sob v�b�ru z mno�iny hodnot, jedna z�konstant
            t��dy 'SelectionType'.  Relevantn� jen pro vstupn� pole v��tov�ch
            typ� (datov� typ m� ur�en enumer�tor).
            
          orientation -- orientace pol��ka, jedna z�konstant t��dy
            'Orientation'; relevantn� jen u n�kter�ch typ� vstupn�ch pol�, jako
            nap�. 'inputfield.RadioBoxInputField'.
            
          post_process -- funkce upravuj�c� vkl�dan� text b�hem psan�.  Jedn�
            se o funkci jednoho argumentu, kter�m je �et�zcov� hodnota pol��ka.
            Vr�cen� hodnota je potom nastavena jako nov� hodnota pol��ka.  Tato
            funkce je vol�na p�i ka�d� zm�n� hodnoty textov�ho pol��ka.
            P��kladem postprocessingu m��e b�t zm�na velikosti p�smen, pokud
            chceme, aby textov� pol��ko mohlo obsahovat jen velk� p�smena.
            Hodnotou tohoto argumentu m��e b�t tak� n�kter� z�konstant t��dy
            'PostProcess', ��m� je u�et�eno psan� n�kter�ch �asto pou��van�ch
            funkc�.
            
          filter -- specifikace jednoho z p�ednastaven�ch filtr� znak�
            propou�t�n�ch do textov�ho pol��ka z u�ivatelsk�ho vstupu.  Jedna
            z�konstant t��dy 'TextFilter'.
            
          filter_list -- sekvence povolen�ch, nebo zak�zan�ch znak�.
            Relevantn� jen pro 'filter' typu 'INCLUDE_LIST' nebo
            'EXCLUDE_LIST'.
            
          style -- instance t��dy 'FieldStyle' ur�uj�c� vizu�ln� styl pol��ka
            nebo funkce dvou argument� vracej�c� instanci t��dy 'FieldStyle'.
            Jedn�-li se o�funkci, jsou jej�mi argumenty id sloupce jako string
            a aktu�ln� datov� ��dek jako instance 'PresentedRow'.  Pokud je
            None, bude pou�it v�choz� styl ��dku (viz. argument 'row_style'
            konstruktoru 'ViewSpec').

          link -- specifikace odkazu/odkaz� do jin�ho n�hledu souvisej�c�ho s
            hodnotou pol��ka.  Instance 'Link' nebo jejich sekvence.  V
            kontextov�m menu ��dku bude pro ka�d� odkaz vytvo�ena jedna polo�ka
            umo��uj�c� odskok do odkazovan�ho n�hledu s vyhled�n�m z�znamu
            odpov�daj�c�ho aktu�ln� hodnot� pol��ka.

        V�echny dal�� argumenty, kter� budou konstruktoru p�ed�ny jsou
        pova�ov�ny za argumenty konstruktoru datov�ho typu.  P�ed�n� argument�
        konstruktoru t�mto zp�sobem je preferov�no p�ed p�ed�n�m instance typu.
        N�kter� argumenty takto ani p�edat nelze, jako nap��klad `enumerator'
        kter� je vytv��en automaticky podle argumentu `codebook'.  To v�ak
        plat� jen p�i pou�it� t��dy `Specification' pro sestaven� datov�
        specifikace.
            
        Je-li specifikov�n argument 'computer' a jeho hodnota nen� 'None', pak
        hodnota sloupce, pokud ji nelze p�evz�t z�datov�ho objektu, je
        po��t�na.  Takov� sloupec m��e b�t pln� \"virtu�ln�\", tj. nen�
        p��tomen v�datov�m objektu a jeho hodnota je v�dy po��t�na, nebo m��e
        b�t v�datov�m objektu, av�ak hodnota je dopo��t�v�na v pr�b�hu editace
        (i nov� vytvo�en�ho) z�znamu.  Pou�it� pln� virtu�ln�ch sloupc� nen�
        doporu�ov�no z d�vodu v�konnostn�ch probl�m� v rozs�hlej��ch
        tabulkov�ch n�hledech.  U pln� virtu�ln�ch pol��ek je tak� nutn� ur�it
        explicitn� datov� typ pomoc� specifik�toru 'type', proto�e nen� mo�n�
        jej p�evz�t automaticky z datov�ho objektu.  Jedinou v�jimkou jsou
        dopo��t�van� virtu�ln� pol��ka typu 'CbComputer', kde je typ p�evzat z
        datov�ho objektu enumer�toru.

        Dopo��t�v�n� pomoc� 'computer' nelze zam��ovat s v�po�tem v�choz�
        hodnoty (specifik�tor 'default').  V�po�et v�choz� hodnoty je proveden
        pouze jednou p�i vytv��en� nov�ho ��dku.  Funkce pro v�po�et v�choz�
        hodnoty nezn� hodnotu ostatn�ch pol��ek a v pr�b�hu editace se ji�
        neuplat�uje.  Computer naproti tomu p�epo��t�v� hodnotu pol��ka v�dy,
        kdy� dojde ke zm�n� hodnoty pol��ka, na kter�m je z�visl� (viz
        dokumentace t��dy 'Computer').

        Z�vislosti po��tan�ch pol��ek mohou b�t i tranzitivn� (po��tan� pol��ka
        mohou z�viset na jin�ch po��tan�ch pol��k�ch), ale graf z�vislost� mus�
        tvo�it strom (nesm� vzniknout cyklus).
        
        V�ka�d�m p��pad� je po��tan� sloupec implicitn� needitovateln�
        ('Editable.NEVER'), pokud nen� explicitn� nastaven jako editovateln�
        pomoc� specifik�toru 'editable'.

        """
        assert isinstance(id, str)
        assert dbcolumn is None or isinstance(dbcolumn, str)
        self._id = id
        self._dbcolumn = dbcolumn or id
        if type_ is not None:
            assert type is None
            type = type_
        assert label is None or is_anystring(label)
        assert descr is None or is_anystring(descr)
        assert type is None or isinstance(type, pytis.data.Type)
        assert isinstance(virtual, bool)
        assert isinstance(fixed, bool)
        assert default is None or callable(default)
        assert computer is None or isinstance(computer, Computer)
        assert codebook is None or isinstance(codebook, str)
        assert display is None or isinstance(display, str) \
               or callable(display) or isinstance(display, tuple) \
               and len(display) == 2 and callable(display[0]) \
               and isinstance(display[1], str)
        assert display_size is None or isinstance(display_size, int)
        assert isinstance(allow_codebook_insert, bool)
        assert codebook_insert_spec is None \
               or isinstance(codebook_insert_spec, str)
        assert width is None or isinstance(width, int)
        assert codebook_runtime_filter is None \
               or isinstance(codebook_runtime_filter, Computer)
        assert selection_type is None \
               or selection_type in public_attributes(SelectionType)
        assert orientation in public_attributes(Orientation)
        assert post_process is None or callable(post_process) \
               or post_process in public_attributes(PostProcess)
        assert filter is None or filter in public_attributes(TextFilter)
        assert filter not in ('INCLUDE_LIST','EXCLUDE_LIST') \
               or is_sequence(filter_list)
        if editable is None:
            if width == 0 or computer: editable = Editable.NEVER
            else: editable = Editable.ALWAYS
        assert editable in public_attributes(Editable) \
               or isinstance(editable, Computer)
        assert style is None or isinstance(style, FieldStyle) \
               or callable(style), ('Invalid field style', id, style)
        links = xtuple(link)
        if __debug__:
            for lnk in links:
                assert isinstance(lnk, Link)
            for arg in kwargs.keys():
                assert arg in ('not_null', 'value_column', 'validity_column',
                               'validity_condition', 'constraints',
                               'validation_messages', 'precision', 'maxlen',
                               'format', 'mindate', 'maxdate'), arg
        self._label = label
        self._descr = descr
        self._width = width
        if column_width is None:
            column_width = width
        self._column_width = column_width
        self._column_label = column_label
        if virtual and type is None:
            type = pytis.data.String()
        self._virtual = virtual
        self._fixed = fixed
        self._type = type
        self._compact = compact
        self._default = default
        self._computer = computer
        self._height = height
        self._editable = editable
        self._line_separator = line_separator
        self._codebook = codebook
        self._display = display
        self._display_size = display_size
        self._allow_codebook_insert = allow_codebook_insert
        self._codebook_insert_spec = codebook_insert_spec
        self._codebook_runtime_filter = codebook_runtime_filter
        self._orientation = orientation
        self._selection_type = selection_type
        self._post_process = post_process
        self._filter = filter
        self._filter_list = filter_list
        self._style = style
        self._links = links
        self._type_kwargs = kwargs
        
    def __str__(self):
        return "<FieldSpec for '%s'>" % self.id()
        
    def id(self):
        """Vra� id pole zadan� v�konstruktoru jako string."""
        return self._id

    def dbcolumn(self):
        return self._dbcolumn
    
    def type(self, data=None):
        """Vra� datov� typ ze specifikace, nebo z datov�ho sloupce.

        Pokud byl typ explicitn� ur�en v konstruktoru, bude vr�cen tento typ,
        jinak bude vr�cen typ ur�en� sloupe�kem datov�ho objektu p�edan�ho jako
        argument.
        
        """
        type = self._type
        if data:
            column = data.find_column(self.id())
            if type is not None:
                assert column is None or \
                       isinstance(type, column.type().__class__)
            elif column is not None:
                type = column.type()
            elif isinstance(self._computer, CbComputer):
                cb_column = data.find_column(self._computer.field())
                enumerator = cb_column.type().enumerator()
                type = enumerator.type(self._computer.column())
                assert type is not None, \
                     "Invalid enumerator column '%s' in CbComputer for '%s'." \
                     % (self._computer.column(), self.id())
                    
            else:
                raise ProgramError("Data type not specified "
                                   "for virtual column '%s'." % self.id())
        return type

    def virtual(self):
        return self._virtual
    
    def label(self):
        """Vra� textov� popisek tohoto pole jako string."""
        return self._label

    def column_label(self):
        """Vra� textov� popisek pro nadpis sloupce v tabulkov�m zobrazen�.

        Pokud nebyl nadpis sloupce (`column_width') v konstruktoru
        specifikov�n, bude vr�cen popisek pol��ka (metoda `label()').
            
        """
        if self._column_label is None:
            return self.label()
        else:
            return self._column_label

    def descr(self):
        """Vra� podrobn�j�� popis (n�pov�du) tohoto pole jako string."""
        return self._descr

    def width(self, default=12):
        """Vra� ���ku pole ve znac�ch; kladn� cel� ��slo.

        Argumenty:

          default -- hodnota, kter� m� b�t dopln�na v p��pad�, �e ���ka nebyla
            v konstruktoru specifikov�na; integer.

        """
        if self._width is None:
            return default
        else:
            return self._width

    def column_width(self, default=10):
        """Vra� ���ku sloupce ve znac�ch; kladn� cel� ��slo.

        Argumenty:

          default -- hodnota, kter� m� b�t dopln�na v p��pad�, �e v
            konstruktoru nebyla specifikov�na ani ���ka sloupce, ani ���ka
            pol��ka.

        Pokud nebyla ���ka sloupce (`column_width') v konstruktoru
        specifikov�na, bude vr�cena obecn� ���ka pol��ka (v�sledek metody
        `width()').
            
        """
        if self._column_width is None:
            return self.width(default)
        else:
            return self._column_width

    def fixed(self):
        """Vra� pravdu, pokud jde o sloupec s fixn� ���kou."""
        return self._fixed
        
    def height(self, default=1):
        """Vra� v��ku pol��ka ve znac�ch

        Argumenty:

          default -- hodnota, kter� m� b�t dopln�na v p��pad�, �e v��ka nebyla
            v konstruktoru specifikov�na; integer.
            
        """
        if self._height is None:
            return default
        else:
            return self._height

    def editable(self):
        """Vra� jednu z�konstant 'Editable' dle editovatelnosti pol��ka."""
        return self._editable

    def compact(self):
        """Vra� pravdu, m� li b�t popisek p�imknut k horn�mu okraji pol��ka."""
        return self._compact

    def default(self):
        """Vra� funkci pro v�po�et v�choz� hodnoty."""
        return self._default

    def computer(self):
        """Vra� instanci 'Computer' pro dopo��t�v�n� hodnoty."""
        return self._computer

    def line_separator(self):
        """Vra� oddd�lova� ��dk� zadan� v�konstruktoru."""
        return self._line_separator
    
    def codebook(self, data=None):
        """Vra� n�zev specifikace nav�zan�ho ��seln�ku."""
        if data is not None:
            enumerator = self.type(data).enumerator()
            if isinstance(enumerator, pytis.data.DataEnumerator) and \
                   isinstance(enumerator.data_factory(), DataSpec):
                return enumerator.data_factory().origin() or self._codebook
        return self._codebook

    def display_size(self):
        """Vra� velikost displeje ��seln�ku (po�et znak�)."""
        return self._display_size

    def display(self):
        """Vra� hodnotu `display' zadanou v konstruktoru."""
        return self._display

    def allow_codebook_insert(self):
        """Vra� pravdu, m�-li b�t  zobrazeno tla��tko p�id�n� do ��seln�ku."""
        return self._allow_codebook_insert
    
    def codebook_insert_spec(self):
        """Vra� n�zev specifikace pro vkl�d�n� do ��seln�ku, nebo None."""
        return self._codebook_insert_spec
    
    def codebook_runtime_filter(self):
        """Vra� specifikaci computeru run-time podm�nky pro ��seln�k."""
        return self._codebook_runtime_filter

    def selection_type(self):
        """Vra� zp�sob v�b�ru z mno�iny hodnot jako konstantu 'SelectionType'.
        """
        return self._selection_type

    def orientation(self):
        """Vra� orientaci pol��ka jako konstantu 'Orientation'."""
        return self._orientation

    def post_process(self):
        """Vra� funkci zpracov�vaj�c� u�ivatelsk� vstup."""
        return self._post_process

    def filter(self):
        """Vra� typ filtru jako konstantu t��dy TextFilter."""
        return self._filter

    def filter_list(self):
        """Vra� seznam povolen�ch/zak�zan�ch znak� pro filter."""
        return self._filter_list

    def style(self):
        """Vra� specifikaci stylu pol��ka zadanou v�konstruktoru."""
        return self._style

    def links(self):
        """Vra� specifikaci odkazu zadanou v�konstruktoru."""
        return self._links

    def type_kwargs(self):
        return self._type_kwargs



class DataSpec(pytis.data.DataFactory):
    """T��da zjednodu�uj�c� tvorbu datov� specifikace.

    Konstruktor t�to t��dy p�ij�m� argumenty ve zjednodu�en� form� a schov�v�
    tak n�kter� n�zko�rov�ov� detaily p�ed tv�rcem specifikace.  Oproti
    rodi�ovsk� t��d� je podstatn� omezena obecnost, ale v typick�m p��pad�
    pou�it� datov�ho rozhran� v Pytis aplikaci je specifikace p�i pou�it� t�to
    t��dy nejen p�ehledn�j��, ale tak� flexibiln�j��.

    Podrobn� popis rozhran� viz. konstruktor t��dy.

    POZOR: Nam�sto t�to t��dy je vhodn�j�� pou��vat t��du 'Specification' n�e.
    Ta zajist� sestaven� datov� specifikace zcela automaticky, tak�e samostatn�
    udr�ov�n� datov�ch specifikac� ji� nen� pot�eba.  Pokud se t��da
    'Specification' osv�d��, je mo�n� �e tato t��da bude v budouvnu zru�ena.

    """
    
    def __init__(self, table, columns, key, oid=None, access_rights=None,
                 data_class_=pytis.data.DBDataDefault):
        """Inicializuj specifikaci.

        Argumenty:

          table -- n�zev datov� tabulky jako �et�zec.
          
          columns -- sekvence specifikac� sloupc� jako instanc� 'Column'.
            Jedn� se v�dy o sloupce z tabulky 'table'.
            
          key -- n�zev kl��ov�ho sloupce jako �et�zec.  Sloupec s t�mto
            identifik�torem mus� b�t p��tomn� v 'columns'.
            
          oid -- seznam n�zv� OID sloupc� (tuple).  Pokud je None (v�choz�
            hodnota), bude dopln�n jeden sloupec s n�zvem 'oid'.  Pro v�echny
            uveden� sloupce budou automaticky p�id�ny p��slu�n� vazby.  Pokud
            tabulka nem� ��dn� m�t ��dn� oid sloupec, uvedeme pr�zdn� seznam.
            Pokud je sloupec jen jeden, nen� nutno jej obalovat do tuplu.
            
          access_rights -- pr�va jako instance 'pytis.data.AccessRights' nebo
            None, pokud maj� b�t pr�va neomezen�.
            
          data_class_ -- t��da datov�ho objektu, odvozen� od `Data'.
            
        Pokud 'columns' neobsahuj� sloupec s identifik�torem 'oid', bude
        automaticky dopln�n sloupec 'oid' typu 'pytis.data.Oid'.

        """
        assert isinstance(table, types.StringType)
        assert isinstance(columns, (types.ListType, types.TupleType))
        assert isinstance(key, types.StringType)
        assert isinstance(key, (types.StringType, types.ListType,
                                types.TupleType)) or oid is None
        assert isinstance(access_rights, pytis.data.AccessRights) \
               or access_rights is None
        assert find(key, columns, key=lambda c: c.id()) is not None
        if __debug__:
            for c in columns:
                assert isinstance(c, Column)
        if oid is None:
            if find('oid', columns, key=lambda c: c.id()):
                oid = ()
            else:    
                oid = ('oid',)
        else:
            oid = xtuple(oid)
            for c in oid:
                assert isinstance(c, types.StringType)
        if access_rights is None:
            perm = pytis.data.Permission.ALL
            access_rights = pytis.data.AccessRights((None, (None, perm)))
        columns += tuple([Column(c, type=pytis.data.Oid()) for c in oid])
        bindings = []
        for c in columns:
            e = c.enumerator()
            if e:
                enumerator = resolver().get(e, 'data_spec')
                if isinstance(enumerator, DataSpec):
                    enumerator.set_origin(e)
            else:
                enumerator = None
            bindings.append(pytis.data.DBColumnBinding(c.id(), table,
                                                       c.column(),
                                                       enumerator=enumerator,
                                                       type_=c.type(),
                                                       **c.kwargs()))
        key = find(key, bindings, key=lambda b: b.column())
        super(DataSpec, self).__init__(data_class_, bindings, key,
                                       access_rights=access_rights)
        self._origin = None
        
    def set_origin(self, name):
        """Nastav p�vodce t�to specifikace.

          Argumentem je n�zev specifikace pro resolver.

        Instance si takto m��e pamatovat ze kter� specifikace poch�z� a tato
        infomace m��e b�t v aplikaci d�le vyu�ita.  Je to trochu hack, ale
        umo�n� to velk� zjednodu�en�
        
        """
        # TODO: Je to trochu hack, ale umo��uje to velk� zjednodu�en� ve
        # specifikac�ch.  Pokud by �el n�zev specifikace zjistit n�jak �ist�ji,
        # tak by to ur�it� nebylo na �kodu.  Takto jsme omezeni na pou�it� t�to
        # t��dy (s DataFactory tuto informaci nem�me).  Mo�n� n�jak� roz���en�
        # na �rovni resloveru?
        self._origin = name
        
    def origin(self):
        """Vra� n�zev specifikace, ze kter� tato instance poch�z�.

        Pokud je p�vod zn�m, je vr�cen n�zev pro resolver, jinak None.
 
        """
        return self._origin

    
class Column(object):
    """Specifikace sloupce pro datovou specifikaci 'DataSpec'."""
    
    def __init__(self, id, column=None, enumerator=None, type=None, **kwargs):
        """Inicializuj specifikaci.

        Argumenty:
        
          id -- identifik�tor sloupce (�et�zec).  Pod t�mto identifik�torem
            bude sloubec vystupovat v aplikaci.
            
          column -- n�zev datab�zov�ho sloupce (�et�zec nebo None).  Implicitn�
            je dopln�na hodnota 'id', tak�e pokud se n�zev sloupce
            shoduje s identifik�torem, nen� jej t�eba definovat.
            
          enumerator -- n�zev specifikace pro resolver (�et�zec nebo None).  Z
            t�to specifikace bude z�sk�n datov� objekt a pou�it jako enumer�tor
            hodnot datov�ho typu.
            
          type -- explicitn� ur�en� datov�ho typu sloupce (instance
            'pytis.data.Type', nebo None).  Tento argument by m�l b�t pou�it
            pouze pokud chceme ur�it vlastn� (odvozen�) datov� typ, nikoliv
            pokud chceme m�nit parametry standardn�ch typ�.  Ty je mo�no
            nastavit p�ed�n�m kl��ov�ch argument� (viz n�e).
            
          **kwargs -- pokud jsou uvedeny jak�koliv dal�� kl��ov� argumenty,
            budou tyto p�ed�ny konstruktoru datov�ho typu sloupce.  Tento
            postup by m�l b�t preferov�n p�ed explicitn� definic� instance typu
            argumentem 'type', pokud je to mo�n�.

        """
        assert isinstance(id, types.StringType), \
               "Invalid value for argument 'id': %s" % id
        assert isinstance(column, types.StringType) or column is None, \
               "Invalid value for argument 'column': %s" % column
        assert isinstance(enumerator, types.StringType) or enumerator is None, \
               "Invalid value for argument 'enumerator': %s" % enumerator
        assert isinstance(type, pytis.data.Type) or type is None, \
               "Invalid value for argument 'type': %s" % type
        assert enumerator is None or type is None \
               or isinstance(type, pytis.data.Codebook), \
               "Invalid codebook type: %s" % type
        assert type is None or kwargs == {}, \
               "When the 'type' is defined explicitly, " + \
               "using kwargs makes no sense: %s" % kwargs
        self._id = id
        if column is None:
            column = id
        self._column = column
        self._enumerator = enumerator
        self._type = type
        self._kwargs = kwargs
    
    def id(self):
        """Vra� identifik�tor sloupce jako �et�zec."""
        return self._id
    
    def column(self):
        """Vra� n�zev sloupce v datov�m zdroji jako �et�zec."""
        return self._column

    def enumerator(self):
        """Vra� n�zev specifikace enumer�toru jako �et�zec nebo None."""
        return self._enumerator

    def type(self):
        """Vra� datov� typ sloupce jako instanci 'pytis.data.Type' nebo None."""
        return self._type
    
    def kwargs(self):
        """Vra� slovn�k kl��ov�ch argument� konstruktoru datov�ho typu."""
        return self._kwargs


class Specification(object):
    """Souhrnn� specifika�n� t��da sestavuj�c� specifikace automaticky.

    Tato t��da zjednodu�uje vytv��en� specifikac� t�m, �e definuje vlastn�
    pravidla pro sestavov�n� jak prezenta�n� tak datov� specifikace pouze na
    z�klad� jedin� specifikace pol��ek a n�kter�ch dal��ch vlastnost�.

    Pou�it�: Specifikaci vytvo��me odvozen�m specifika�n� t��dy n�hledu od t�to
    t��dy a p�edefinov�n�m jejich ve�ejn�ch atribut�.  To uleh�uje tvorbu
    variant n�hled� s vyu�it�m d�di�nosti.

    V�znam atribut�: N�kter� atrubuty jsou definov�ny p��mo touto t��dou --
    jejich v�znam je zdokumentov�n v r�mci jejich dokumenta�n�ch �et�zc�.
    V�echny ostatn� ve�ejn� atributy, kter� odvozen� t��da definuje budou
    p�ed�ny jako stejnojmenn� argumenty konstruktoru 'ViewSpec'.

    """
    
    table = None
    """N�zev datov� tabulky jako �et�zec.

    Pokud n�zev nen� ur�en, bude odvozen automaticky z n�zvu specifika�n�
    t��dy.  Kapit�lky jsou p�evedeny na slova odd�len� podtr��tkem, tak�e
    nap�. pro specifika�n� t��du 'UcetniOsnova' bude n�zev tabulky
    'ucetni_osnova'.  Z hlediska p�ehlednosti je doporu�ov�no volit toto jmenn�
    sch�ma a vyhnout se tak explicitn�mu ur�ov�n� n�zv� tabulek.

    """

    key = None
    """Identifik�tor kl��ov�ho sloupce jako �et�zec, nebo jejich sekvence.

    Pokud m� tabulka v�cen�sobn� kl��, ud�me sekvenci identifik�tor�
    p��slu�n�ch sloupc�.  Vyjmenovan� sloupce se mus� nach�zet ve specifikaci
    'fields'.  Pokud kl�� nen� definov�n, bude automaticky za kl��ov� pova�ov�n
    prvn� sloupec z 'fields'.

    """

    oid = None
    """Specifikace n�zv� OID sloupc� (tuple).

    Pokud je None (v�choz� hodnota), bude dopln�n jeden sloupec s n�zvem 'oid'.
    Pro v�echny uveden� sloupce budou automaticky p�id�ny p��slu�n� vazby.
    Pokud tabulka nem� ��dn� m�t ��dn� oid sloupec, uvedeme pr�zdn� seznam.
    Pokud je sloupec jen jeden, nen� nutno jej obalovat do tuplu.
    """

    access_rights = None
    """P��stupov� pr�va n�hledu jako instance 'AccessRights'."""

    data_cls = pytis.data.DBDataDefault
    """Datov� t��da pou�it� pro vytvo�en� datov�ho objektu."""

    fields = ()
    """Specifikace pol��ek jako sekvence instanc� 'FieldSpec'.
    
    Pokud nejde o sekvenci, ale o metodu, je tato metoda v okam�ik sestavov�n�
    specifikace zavol�na a sekvence pol���ek je o�ek�v�na jako jej� n�vratov�
    hodnota.
    
    """
    
    bindings = {}
    """Specifikace vazeb pro pou�it� v du�ln�ch formul���ch.
    
    Slovn�k, kde kl��em je n�zev specifikace vedlej��ho formul��e a hodnotou je
    instance 'BindingSpec' ur�uj�c� jak se tento n�hled v�e s dan�m vedlej��m
    n�hledem.
    
    """
    
    cb = CodebookSpec()
    """Specifikace vlastnost� n�hledu p�i jeho pou�it� jako ��selk�ku.
    
    Instance CodebookSpec.
    """
    
    prints = None
    """Specifikace tiskov�ch n�hled�.
    
    Sekvence dvojic (titulek, n�zev tiskov� specifikace).
    
    """
    
    def __init__(self, resolver):
        self._resolver = resolver
        if callable(self.fields):
            self.fields = self.fields()
        assert self.fields, 'No fields defined for %s.' % str(self)
        assert isinstance(self.fields, (list, tuple))
        self._view_spec_kwargs = {}
        for attr in dir(self):
            if not (attr.startswith('_') or attr.endswith('_spec') or \
                    attr in ('table', 'key', 'access_rights', 'oid',
                             'data_cls', 'bindings', 'cb', 'prints')):
                self._view_spec_kwargs[attr] = getattr(self, attr)
        for arg in ('layout', 'actions'):
            try:
                value = self._view_spec_kwargs[arg]
            except:
                continue
            if callable(value):
                self._view_spec_kwargs[arg] = value()

    def _create_data_spec(self):
        def e(name):
            return name and self._resolver.get(name, 'data_spec')
        if issubclass(self.data_cls, pytis.data.DBData):
            table = self.table or \
                    camel_case_to_lower(self.__class__.__name__, '_')
            bindings = [pytis.data.DBColumnBinding(f.id(), table, f.dbcolumn(),
                                                   enumerator=e(f.codebook()),
                                                   type_=f.type(),
                                                   **f.type_kwargs())
                        for f in self.fields if not f.virtual()]
            if self.oid is None:
                oid = ('oid',)
            else:
                oid = xtuple(oid)
                for c in oid:
                    assert isinstance(c, types.StringType)
            for o in oid:
                oidcol = pytis.data.DBColumnBinding(o, table, o,
                                                    type_=pytis.data.Oid()
                                                    )
                bindings.append(oidcol)
            if self.key:
                bdict = dict([(b.column(), b) for b in bindings])
                key = [bdict[k] for k in self.key]
            else:
                key = bindings[0]
            args = (bindings, key,)
        else:
            # TODO: P�evod datov�ch typ� m� mnoh� omezen�, ale za�n�me
            # n���m jednodu���m a dod�lejme co bude pot�eba a� se uk�e,
            # �e je to skute�n� pot�eba...
            columns = []
            for f in self.fields:
                if not f.virtual():
                    type = f.type() or pytis.data.String()
                    kwargs = copy.copy(f.type_kwargs())
                    enum = e(f.codebook())
                    assert f.type() is None or not kwargs and not enum, \
                           ("Nelze ur�it z�rove� typ a jeho argumenty.",
                            f.id(), kwargs, enum)
                    if enum:
                        df_kwargs = {'dbconnection_spec': config.dbconnection}
                        e_kwargs = {'data_factory_kwargs': df_kwargs}
                        for a in ('value_column', 'validity_column',
                                  'validity_condition'):
                            if kwargs.has_key(a):
                                e_kwargs[a] = kwargs[a]
                                del kwargs[a]
                        enumerator = pytis.data.DataEnumerator(enum, **e_kwargs)
                        kwargs['enumerator'] = enumerator
                    if kwargs:
                        type = type.__class__(**kwargs)
                    columns.append(pytis.data.ColumnSpec(f.id(), type))
            args = (columns,)
        access_rights = self.access_rights
        if access_rights is None:
            perm = pytis.data.Permission.ALL
            access_rights = pytis.data.AccessRights((None, (None, perm)))
        return pytis.data.DataFactory(self.data_cls, *args, 
                                      **dict(access_rights=access_rights))

    def _create_view_spec(self, title=None, **kwargs):
        if not title:
            title = ' '.join(split_camel_case(self.__name__))
        return ViewSpec(title, **kwargs)

    def view_spec(self):
        """Vra� prezenta�n� specifikaci jako instanci 'ViewSpec'."""
        try:
            spec = self._view_spec
        except AttributeError:
            kwargs = self._view_spec_kwargs
            spec = self._view_spec = self._create_view_spec(**kwargs)
        return spec
        
    def data_spec(self):
        """Vra� datovou specifikaci jako instanci datov� t��dy."""
        try:
            spec = self._data_spec
        except AttributeError:
            spec = self._data_spec = self._create_data_spec()
        return spec
        
    def cb_spec(self):
        """Vra� specifikaci ��seln�ku jako instanci 'CodebookSpec'."""
        return self.cb
    
    def binding_spec(self):
        """Vra� specifikaci nav�z�n� v du�ln�m formul��i jako slovn�k."""
        return self.bindings

    def print_spec(self):
        """Vra� sekvenci specifikac� tiskov�ch n�hled�."""
        return self.prints


