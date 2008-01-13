# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Brailcom, o.p.s.
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
pova�ov�ny za immutable, tud�� mohou b�t libovoln� sd�leny.

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
    """Independent definition of generic named colors to be used within 'Style' specifications."""
    WHITE  = (255, 255, 255)
    BLACK  = (  0,   0,   0)
    RED    = (255,   0,   0)
    RED20  = (255, 200, 200)
    GREEN  = (  0, 255,   0)
    BLUE   = (  0,   0, 255)
    YELLOW = (255, 255, 160)
    GRAY   = ( 50,  50,  50)
    GRAY90 = ( 25,  25,  25)
    GRAY80 = ( 50,  50,  50)
    GRAY70 = ( 75,  75,  75)
    GRAY60 = (100, 100, 100)
    GRAY50 = (125, 125, 125)
    GRAY40 = (150, 150, 150)
    GRAY30 = (175, 175, 175)
    GRAY20 = (200, 200, 200)
    GRAY10 = (225, 225, 225)
    BLANCHETALMOND = (255, 235, 205)
    LIGHTYELLOW    = (255, 255, 224)
    PEACHPUFF2     = (238, 203, 173)
    SLATEGRAY2     = (185, 211, 238)
    LIGHTSALMON    = (255, 160, 122)

    
class Style(object):
    """Text style specification.

    Style instance is returned by the 'style' attribute of 'FieldSpec' or 'row_style' attribute of
    'ViewSpec'.  Both specifiers may be functions and compute the style based on the values of the
    current row.  They may also return None to indicate the default style.  Field style has a
    higher precedence, so all properties not defined by field style default to those defined by row
    style and if they are not defined by row style, global defaults are used.

    """
    _COLOR_RE = re.compile('^\#[0-9a-fA-F]{3,3}([0-9a-fA-F]{3,3})?$')

    def __init__(self, foreground=None, background=None, bold=None, slanted=None,
                 overstrike=None, underline=None, name=None):
        """Initialize the instance.

        Arguments:

          foreground -- background color as one of 'Color' constants, a tuple of three integers
            (RGB), or a hexadecimal string representation (such as '#ff0000')
          background -- background color in the same format as the foreground color
          bold -- flag indicating bold text
          slanted -- flag indicating slanted (italics) text 
          overstrike -- flag indicating that the text should be stroked through
          underline -- flag indicating that the text should be underlined
          name -- style name (string) refering to a common style definition in a stylesheet
          
        """
        self._foreground = foreground
        self._background = background
        self._bold = bold
        self._slanted = slanted
        self._overstrike = overstrike
        self._underline = underline
        self._name = name

    def foreground(self):
        return self._foreground

    def background(self):
        return self._background

    def bold(self):
        return self._bold

    def slanted(self):
        return self._slanted

    def overstrike(self):
        return self._overstrike
    
    def underline(self):
        return self._underline

    def name(self):
        return self._name

    def __str__(self):
        items = [k[1:]+'='+repr(v) for k,v in self.__dict__.items()
                 if k.startswith('_') and v is not None]
        return "<%s %s>" % (self.__class__.__name__, ', '.join(items))

    def __radd__(self, other):
        if other is None:
            return self
        else:
            return other + self
        
    def __add__(self, other):
        def coalesce(x, y):
            if x is not None:
                return x
            return y
        if other is None:
            return self
        else:
            return Style(foreground=coalesce(self._foreground, other._foreground),
                         background=coalesce(self._background, other._background),
                         bold=coalesce(self._bold, other._bold),
                         slanted=coalesce(self._slanted, other._slanted),
                         overstrike=coalesce(self._overstrike, other._overstrike),
                         underline=coalesce(self._underline, other._underline),
                         name=coalesce(self._name, other._name))


class Orientation(object):
    """Definition of constants for orientation specification."""
    HORIZONTAL = 'HORIZONTAL'
    """Horizontal orientation."""
    VERTICAL = 'VERTICAL'
    """Vertical orientation."""

    
class Button(object):
    """Specification of a button for use within a form layout.

    This allows to place buttons which can invoke an action or a user defined function within form
    layout.  See `GroupSpec' for more information about where buttons can be used.

    """
    
    def __init__(self, label=None, handler=None, enabled=None, action=None, width=None,
                 tooltip=None, active_in_popup_form=True, active_in_readonly_form=False):
        """Initialize the instance.
        
        Arguments:

          label -- button label as a string.  Can be None when 'action' is specified -- in this
            case the action title is used.
          
          handler -- function of one argument -- the 'PresentedRow' instance representing the
            current state of the form.  If None, 'action' must be sepecified.

          enabled -- function of one argument -- the 'PresentedRow' instance representing the
            current state of the form -- which returns True if the button is enabled or False
            otherwise.  This function will be called periodically on each user interface update, so
            the result may change during form editation.  Only relevant if 'handler' is used.  For
            'action' the button state depends on the refered action and its 'enabled' parameter.

          action -- name of an 'Action' specification as a string.  This allows to handle the
            button press by invoking one of 'actions' defined in the same 'ViewSpec' instead of
            passing the 'handler' function directly.  If used, 'handler' must be None, if None,
            'handler' must be specified.
            
          width -- button with (number of characters).  The default width is set automatically to
            fit the label, but this argument may override this default.
            
          tooltip -- button's tooltip text as a string.
            
          active_in_popup_form -- False value deactivate the button in popup (modal) forms.  This
            may be particularly usefull when the button opens a new form, which is impossible in
            modal forms.
            
          active_in_readonly_form -- buttons are inactive in read-only forms by default, since they
            often modify form data.  True value will activate the button in even for readonly
            forms.  This may be particularly usefull when the button performs some action, which
            doesnt't modify the data.
            
        """
        if action is None:
            assert isinstance(label, (str, unicode)), label
            assert callable(handler), handler
            assert enabled is None or callable(enabled), enabled
        else:
            assert isinstance(action, str), action
            assert label is None or isinstance(label, (str, unicode)), label
            assert handler is None, handler
            assert enabled is None, enabled
        assert width is None or isinstance(width, int)
        assert tooltip is None or isinstance(tooltip, (str, unicode)), tooltip
        assert isinstance(active_in_popup_form, bool), active_in_popup_form
        assert isinstance(active_in_readonly_form, bool), active_in_readonly_form
        self._label = label
        self._handler = handler
        self._enabled = enabled
        self._action = action
        self._width = width
        self._tooltip = tooltip
        self._active_in_popup_form = active_in_popup_form
        self._active_in_readonly_form = active_in_readonly_form
        
    def label(self):
        return self._label
    
    def handler(self):
        return self._handler
    
    def enabled(self):
        return self._enabled
    
    def action(self):
        return self._action
    
    def width(self):
        return self._width
    
    def tooltip(self):
        return self._tooltip
    
    def active_in_popup_form(self):
        return self._active_in_popup_form

    def active_in_readonly_form(self):
        return self._active_in_readonly_form

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
        assert isinstance(title, (str, unicode))
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
    
    def __init__(self, title, handler, context=ActionContext.CURRENT_ROW, secondary_context=None,
                 name=None, enabled=True, access_groups=None, descr=None, hotkey=None, **kwargs):
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

          name -- action name as a string.  This name will identify the action for use with a form
            'Button'.  May be None if the action is not refered by any button.

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
        assert callable(enabled) or isinstance(enabled, bool)
        assert access_groups is None or isinstance(access_groups, (str, tuple, list))
        assert descr is None or isinstance(descr, (str, unicode))
        assert hotkey is None or isinstance(hotkey, (str, tuple))
        self._handler = handler
        self._context = context
        self._secondary_context = secondary_context
        self._name = name
        self._enabled = enabled
        self._access_groups = access_groups
        self._descr = descr
        self._hotkey = hotkey
        self._kwargs = kwargs
        super(Action, self).__init__(title)
        
    def handler(self):
        return self._handler

    def context(self):
        return self._context
    
    def secondary_context(self):
        return self._secondary_context

    def name(self):
        return self._name
    
    def enabled(self):
        return self._enabled
        
    def access_groups(self):
        return self._access_groups
        
    def descr(self):
        return self._descr
        
    def hotkey(self):
        return self._hotkey
    
    def kwargs(self):
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
        assert isinstance(actions, (list, tuple))
        if __debug__:
            for x in actions:
                if isinstance(x, (tuple, list)):
                    for y in x:
                        assert isinstance(y, (Action, ActionGroup))
                else:
                    assert isinstance(x, (Action, ActionGroup))
        self._actions = actions
        super(ActionGroup, self).__init__(title)
        
    def actions(self):
        """Vra� seznam akc� jako tuple."""
        return self._actions

    
class Condition(object):
    """Saved searching/filtering condition specification."""
    def __init__(self, name, condition, fixed=True):
        """Initialize condition specification.

        Arguments:
          name -- condition name as a string
          condition -- condition as a 'pytis.data.Operator' instance.  The
            condition must be displayable in the serach/filtering dialog, thus
            certain restrictions apply.  It is not possible to use the logical
            operator 'NOT', logical operators 'AND' and 'OR' must always have
            exactly two operands and only the first of the two may be a nested
            logical operator.  For example instead of AND(a, b, c) use
            AND(AND(a, b), c), insteda of NOT(EQ(x, y)) use NE(x, y), etc.
          fixed -- indicates, whether the user is allowed to manipulate the
            condition.  

        """
        def check(cond):
            assert isinstance(cond, pytis.data.Operator), \
               ('Not an Operator instance:', str(cond))
            assert len(cond.args()) == 2, \
               ('Operator must have just two arguments:', str(cond.args()))
            arg1, arg2 = cond.args()
            if cond.logical():
                check(arg1)
                check(arg1)
                assert not arg2.logical(), \
                   ('Nested conditions only allowed as first operand:',
                    str(arg2))
            else:
                assert isinstance(arg1, str), \
                   ('First operand must be column id:', str(arg1))
                assert isinstance(arg2, (str, pytis.data.Value,
                                         pytis.data.WMValue)), \
                   ('Second operand must be column id or Value instance:',
                    str(arg2))
        if __debug__:
            assert isinstance(name, (str, unicode)), name
            if condition is not None:
                check(condition)
        self._name = name
        self._condition = condition
        self._fixed = fixed
        
    def name(self):
        """Return the name passed to the constructor."""
        return self._name

    def condition(self):
        """Return the condition passed to the constructor."""
        return self._condition
    
    def fixed(self):
        """Return True if the user is allowed to manipulate this condition."""
        return self._fixed
    
    
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
    sebou n�sleduj�c� pol��ka jsou skl�d�na pod sebe do m���ky (nehled� na to,
    jde-li o vertik�ln�, nebo horizont�ln� skupinu).  Teprve celky takto
    seskupen�ch pol��ek a vno�en�ch skupin jsou skl�d�ny podle orientace
    skupiny.  Samostatn� vedle sebe um�st�n� pol��ka je mo�no vytvo�it jejich
    um�st�n�m do samstatn�ch vno�en�ch podskupin (jednoprvkov�ch).
    
    """
    def __init__(self, items, orientation=Orientation.HORIZONTAL, label=None,
                 gap=2, space=1, border=3, border_style=BorderStyle.ALL):
        """Initialize the instance.

        Arguments:

          items -- contents of the group as a sequence of field identifiers (strings), button
            specifications ('Button' instances) or nested groups ('GroupSpec' instances).
            
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

        """
        assert is_sequence(items)
        assert label is None or isinstance(label, (str, unicode))
        assert type(gap) == type(0)
        assert gap >= 0
        assert orientation in public_attributes(Orientation)
        assert border_style in public_attributes(BorderStyle)
        for i, item in enumerate(items):
            if isinstance(item, (tuple, list)):
                if isinstance(items, tuple):
                    items = list(items)
                items[i] = GroupSpec(item, orientation=Orientation.VERTICAL)
            else:
                # No need for recursion, since the check is performed for each group on its level.
                assert isinstance(item, (GroupSpec, Button, str, unicode)), item
        self._items = tuple(items)
        self._label = label
        self._orientation = orientation
        self._gap = gap
        self._space = space
        self._border = border
        self._border_style = border_style

    def items(self):
        """Return the group contents as a tuple."""
        return tuple(self._items)

    def order(self):
        """Return the identifiers of all fields in this group and all subgroups."""
        fields = []
        for item in self._items:
            if isinstance(item, GroupSpec):
                fields.extend(item.order())
            elif not isinstance(item, Button):
                fields.append(item)
        return fields

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

    
class TabGroup(GroupSpec):
    """Tabbed layout specification."""
    
    def __init__(self, *tabs):
        """Initialize the instance.

        Arguments:

          tabs -- tab specifications as (LABEL, GROUP) pairs, where LABEL is the tab label and
            GROUP is the tab contents as a 'GroupSpec' instance.  If GROUP is a sequence, it will
            be automatically turned into a vertical group.

        """
        items = [GroupSpec(xtuple(group), label=label, orientation=Orientation.VERTICAL)
                 for label, group in tabs]
        super(TabGroup, self).__init__(items, orientation=Orientation.VERTICAL)
        

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
    """Horizont�ln� seskupen� pol��ek s nadpisem a or�mov�n�m.

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

        'caption' je v�dy pova�ov�n za jazykov� z�visl� text a tud�� automaticky
        podl�h� jazykov� konverzi.

        """
        assert caption is None or isinstance(caption, (str, unicode))
        assert isinstance(group, GroupSpec)
        assert order is None or is_sequence(order)
        self._caption = caption
        self._group = group
        if order is None:
            order = group.order()
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
        self._order = tuple(order)

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
    
    def __init__(self, title, fields, singular=None, layout=None, list_layout=None, columns=None,
                 actions=(), sorting=None, grouping=None, check=(), cleanup=None,
                 on_new_record=None, on_edit_record=None, on_delete_record=None,
                 redirect=None, focus_field=None, description=None, help=None,
                 row_style=None, conditions=(), aggregations=()):
        
        """Inicializuj instanci.

        Argumenty:

          title -- the title of this view as a (unicode) string.  The title is used in browse form
            headings and in other contexts, where the entity is refered as whole (all the records).
            Thus the title should mostly be in plural (for example 'Invoices').
            
          singular -- the title of a single item (one record) of the entity as a (unicode) string.
            If None, 'title' is used in both contexts.
            
          layout -- specifikace rozlo�en� pol��ek v edita�n�m formul��i,
            instance t��dy 'GroupSpec'.  Je mo�n� p�edat tak� sekvenci
            identifik�tor� pol��ek -- v tom p��pad� bude vytvo�ena horizont�ln�
            skupina obsahuj�c� vyjmenovan� pol��ka.  Pokud je None, bude
            v�choz� rozlo�en� sestaveno poskl�d�n�m v�ech pol��ek definovan�ch
            ve fields.  Pro zp�tnou kompatibilitu je mo�n� pou��t tak�
            'LayoutSpec', ale tento zp�sob definice je pova�ov�n za nevhodn� a
            v budoucnu nebude podporov�n.

          list_layout -- specification of list layout as a 'ListLayout' instance or None.
          
          columns -- specifikace sloupc� tabulkov�ho formul��e, sekvence
            indentifik�tor� pol��ek z 'fields'.  Pokud nen� ur�eno, bude
            v�choz� seznam sloupc� obsahovat v�echna pol��ka z fields, kter�
            nemaj� 'column_width' nastaveno na nulu nebo 'disable_column' na
            True.
            
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
            
          check -- funkce pro ov��en� integrity dat cel�ho z�znamu.  Jedn� se o
            funkci jednoho argumentu, j�m� je instance t��dy `PresentedRow',
            reprezentuj�c� aktu�ln� hodnoty v�ech pol��ek formul��e.  Nam�sto
            jedin� funkce lze p�edat tak� seznam takov�ch funkc� -- v tom
            p��pad� budou funkce vol�ny v po�ad�, ve kter�m jsou uvedeny.  Na
            rozd�l od validace hodnot pol��ek, kter� z�vis� na datov�m typu a
            m� k dispozici pouze vlastn� obsah pol��ka, m� tato funkce k
            dispozici i hodnoty ostatn�ch pol��ek, tak�e je vhodn� pro ov��en�
            vz�jemn� slu�itelnosti t�chto hodnot.  Tato funkce vrac� None,
            pokud je v�e v po��dku a formul�� m��e b�t v tomto stavu odesl�n,
            nebo id pol��ka, jeho� hodnota zp�sobila neplatnost z�znamu.
            Formul�� by potom m�l u�ivatele vr�tit do editace dan�ho pol�ka.
            Je mo�n� vr�tit tak� dvojici (ID, MESSAGE), kde MESSAGE je chybov�
            zpr�va, kter� m� b�t zobrazena u�ivateli.
            
          cleanup -- a function for final actions after inserting/updating a record.  The function
            must accept two arguemnts -- the first one is the row after performing the database
            operation (insert/update) and the second is the edited/inserted row before the database
            operation (row values may be changed by the operation -- default values may be supplied
            and/or triggers/rules may modify the data).  Both arguments are `PresentedRow'
            instances.  Note, that you can also access the values before any user changes throught
            the 'original_row()' method on the second argument.  The cleanup function is run after
            comitting the edit/insert form (using the ``Ok'' button) or after committing inline
            editation/insert regardless whether the record has been changed or not.  Note, that
            unlike 'check', 'cleanup' is called after the database operation, but you can still
            abort the operation by rollback of the transaction (if the underlying database engine
            supports it).
            
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
            
          redirect -- redirection for single record view/editation specified as a callable object
            (function) of one argument - the 'PresentedRow' instance.  The function should return
            the name of the specification to use for given record.  If the function is not defined
            or it returns None, no redirection takes place.  Redirection may be useful when a form
            lists records of different types (displaying only the fields which are common for all
            the subtypes) and you want to be able to open each particular record in a form which
            matches its subtype.  Note, that the same effect would be possible by defining the
            'on_edit_record' function.
            
          focus_field -- identifier of the field, which should automatically gain focus when a form
            is open.  You may also pass a function of one argument ('PresentedRow' instance), which
            returns the field identifier.
            
          description -- brief description of the view.  A short text (one or two sentences)
            without formatting.  Use the 'help' argument below to supply a detailed description.
            But even if 'help' is present, this short description should still be defined.
          
          help -- detailed description of the view as a formatted text in the LCG Structured Text
            format.  This text is used for generating the on-line help and it is also possible to
            supply it in a separate file.  See the Help tutorial for more information.

          row_style -- a 'Style' instance determining the base style for all fields or a function
            of one argument (the 'PresentedRow' instance) returning the 'Style' for one row (based
            on its values).

          conditions -- a sequence of named conditions ('Condition' instances), which should be
            available to the user for filtering/searching records in this view.

          aggregations -- a sequence aggregation functions which should be turned on automatically
            for this view (in forms which support that).  The items are 'AGG_*' constants of
            'pytis.data.Data'.
            
        The arguments 'layout' and 'columns' may be omitted.  Default layout
        and column list will be generated automatically based on the order of
        the field specifications in 'fields'.
        
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
            assert isinstance(actions, (tuple, list)), actions
            assert isinstance(layout, LayoutSpec), layout
            action_names = [action.name() for action in self._linearize_actions(actions)]
            def recourse_group(group):
                for item in group.items():
                    if isinstance(item, GroupSpec):
                        recourse_group(item)
                    elif isinstance(item, Button):
                        assert item.action() is None or item.action() in action_names, \
                               "Unknown button action in layout: %s" % item.action()
                    else:
                        assert self._field_dict.has_key(item), \
                               "Unknown field in layout: %r" % item
                        if self._field_dict[item].width() == 0:
                            log(OPERATIONAL,
                                "Zero width field in layout:", item)
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
                                   _("Unknown field id '%s' in dependencies "
                                     "for '%s' specification of '%s'.") % \
                                     (dep, s, f.id())
        # Initialize `columns' specification parameter
        if columns is None:
            columns = tuple([f.id() for f in self._fields
                             if f.column_width() and not f.disable_column()])
        else:
            if __debug__:
                assert is_sequence(columns)
                for c in columns:
                    assert isinstance(c, str) and self._field_dict.has_key(c),\
                      _("Unknown column id in 'columns' specification: %r") % c
                    f = self._field_dict[c]
                    assert not f.disable_column(), \
                           _("Disabled column in columns: %s") % c
        # Initialize other specification parameters
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
        assert callable(check) or isinstance(check, (list, tuple))
        check = xtuple(check)
        assert isinstance(conditions, (tuple, list))
        if __debug__:
            for f in check:
                assert callable(f)
            for c in conditions:
                assert isinstance(c, Condition)
                assert c.fixed()
        assert isinstance(aggregations, (tuple, list))
        if __debug__:
            for agg in aggregations:
                assert agg in [getattr(pytis.data.Data, attr)
                               for attr in public_attributes(pytis.data.Data)
                               if attr.startswith('AGG_')]
        assert cleanup is None or callable(cleanup)
        assert on_new_record is None or callable(on_new_record)
        assert on_edit_record is None or callable(on_edit_record)
        assert on_delete_record is None or callable(on_delete_record)
        assert redirect is None or callable(redirect)
        assert focus_field is None or callable(focus_field) or \
               isinstance(focus_field, (str, unicode))
        assert row_style is None or isinstance(row_style, Style) or callable(row_style)
        assert description is None or isinstance(description, (str, unicode))
        assert help is None or isinstance(help, (str, unicode))
        self._title = title
        self._singular = singular
        self._columns = columns
        self._layout = layout
        self._list_layout = list_layout
        self._actions = actions
        self._sorting = sorting
        self._grouping = grouping
        self._check = check
        self._cleanup = cleanup
        self._on_new_record = on_new_record
        self._on_edit_record = on_edit_record
        self._on_delete_record = on_delete_record
        self._redirect = redirect
        self._focus_field = focus_field
        self._description = description
        self._help = help
        self._row_style = row_style
        self._conditions = tuple(conditions)
        self._aggregations = tuple(aggregations)
        
    def _linearize_actions(self, spec):
        actions = []
        for x in spec:
            if isinstance(x, Action):
                actions.append(x)
            elif isinstance(x, ActionGroup):
                actions.extend(self._linearize_actions(x.actions()))
            elif isinstance(x, (tuple, list)):
                actions.extend(self._linearize_actions(x))
            else:
                raise ProgramError("Invalid action specification: %s" % x)
        return actions
    
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

    def list_layout(self):
        return self._list_layout
    
    def columns(self):
        """Vra� tuple identifik�tor� sloupc� pro tabulkov� formul��."""
        return self._columns

    def actions(self, linear=False):
        """Vra� specifikaci akc�."""
        if linear:
            return self._linearize_actions(self._actions)
        else:
            return self._actions

    def sorting(self):
        """Vra� specifikaci v�choz�ho �azen�."""
        return self._sorting

    def grouping(self):
        """Vra� tuple id sloupc� v�choz�ho vizu�ln�ho seskupov�n�."""
        return self._grouping

    def cleanup(self):
        """Vra� funkci prov�d�j�c� akce p�i uzav�en� formul��e."""
        return self._cleanup

    def check(self):
        """Vra� tuple funkc� prov�d�j�c�ch kontrolu integrity z�znamu."""
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

    def redirect(self):
        """Vra� funkci zaji��uj�c� p�esm�rov�n� na jin� n�zev specifikace."""
        return self._redirect
        
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

    def conditions(self):
        """Return predefined filtering/serach conditions as a tuple of 'Condition' instances."""
        return self._conditions

    def aggregations(self):
        """Return default aggregation functions as a tuple."""
        return self._aggregations

    
class BindingSpec(object):
    """Specifikace vazby dvou n�hled� p�i propojen� do du�ln�ho formul��e.

    Definuje vlastnosti napojen� dvou formul��� p�i jejich spojen� do du�ln�ho
    formul��e.  Definov�na je jak datov� vazba, tak n�kter� prezenta�n�
    vlastnosti spojen�.

    Pou�it� je n�sleduj�c�:

    Funkce 'binding_spec' ve specifikaci libovoln�ho n�hledu vrac� slovn�k
    v�ech mo�n�ch spojen� tohoto n�hledu s dal��mi jin�mi n�hledy.  Slovn�k je
    kl��ov�n n�zvy specifikac� a hodnotou je pr�v� instance 'BindingSpec'.

    Kdy� je tedy nap��klad vytv��en du�ln� formul�� 'A::B', bude ve specifikaci
    n�hledu 'A' (v roli hlavn�ho formul��e) z�sk�na z 'binding_spec' polo�ka
    pro n�hled 'B' (v roli vedlej��ho formul��e).
    

    """
    
    def __init__(self, title=None, binding_column=None, side_binding_column=None,
                 hide_binding_column=True, append_condition=None, condition=None, description=None, 
                 help=None, sash_ratio=0.5, orientation=Orientation.HORIZONTAL):
        """Inicialize the specification.

        Arguments:

          title -- The title of the dual form as a string.  If omitted, the title will be
            provided automatically as a concatenation of the titles of both involved forms. 
                        
          binding_column -- the identifier of the binding column (string).  The binding column
            determines filtering of the side form depending on the current main form row.  If
            specified, side form will be filtered to contain only the rows, which have the same
            values in the binding column as the current main form row.  Use the argument
            'side_binding_column' if the identifier of the binding column is different in the side
            form.  Using the binding column is optional and if omitted, the filtering depends
            solely on the 'condition' argument.
            
          side_binding_column -- identifier of the binding column in the side form.  The default
            value None means to use the same identifier as given by 'binding_column'.
            
          hide_binding_column -- the binding column is hidden by default in the side form.  Pass a
            false value to this argument to unhide it.

          condition -- a function of one argument (the PresentedRow instance) returning the current
            side form condition (a 'pytis.data.Operator' instance) for given main form row.  If
            used together with the binding column, the condition will be used in conjunction with
            the binding column condition.  If 'binding_column' is None, this condition will be used
            solely.

          description -- a brief description of this dual connection of the forms.
            
          help -- detailed description of this dual connection as a formatted text in the LCG
            Structured Text format.  This text is used for generating the on-line help and it is
            also possible to supply it in a separate file.  See the Help tutorial for more
            information.
            
          sash_ratio -- the relative proportion of the size of the two forms.  The value is a
            decimal number in the range from zero to one.  The default value of 0.5 denotes equal
            space for both forms.  Lower value means less space for the main form.  The ratio only
            has effect when displaying two table forms separated horizontally.  Layout forms are
            sized according to their width/height.
            
          orientation -- the default orientation of the dual form separator as one of 'Orientation'
            constants.  In horizontal orientation, the main form is above the side form, in
            vertical it is on the left side.

        """
        assert title is None or isinstance(title, (str, unicode)), title
        assert binding_column is None or isinstance(binding_column, (str, unicode)), binding_column
        assert side_binding_column is None or isinstance(side_binding_column, (str, unicode))
        assert isinstance(hide_binding_column, bool), hide_binding_column
        if append_condition is not None:
            # Backwards compatibility
            assert condition is None, "Can't use 'append_condition' (deprecated) with 'condition'."
            condition = append_condition
        assert condition is None or callable(condition), condition
        assert condition is not None or binding_column is not None, \
               "You must specify at least one of 'condition', 'binding_column'."
        assert description is None or isinstance(description, (str, unicode)), description
        assert help is None or isinstance(help, (str, unicode)), help
        assert orientation in public_attributes(Orientation), orientation
        assert isinstance(sash_ratio, float) and 0 < sash_ratio < 1, sash_ratio
        self._title = title
        self._binding_column = binding_column
        if side_binding_column is None:
            side_binding_column = binding_column
        self._side_binding_column = side_binding_column
        self._hide_binding_column = hide_binding_column
        self._description = description
        self._help = help
        self._condition = condition
        self._sash_ratio = sash_ratio
        self._orientation = orientation
        
    def title(self):
        return self._title

    def description(self):
        return self._description
    
    def help(self):
        return self._help
    
    def binding_column(self):
        return self._binding_column

    def side_binding_column(self):
        return self._side_binding_column

    def hide_binding_column(self):
        return self._hide_binding_column

    def condition(self):
        return self._condition
    
    def sash_ratio(self):
        return self._sash_ratio
    
    def orientation(self):
        return self._orientation


class Editable(object):
    """Definition of available constants for field editability specification."""
    ALWAYS = 'ALWAYS'
    """The field is always editable."""
    ONCE = 'ONCE'
    """The field is only editable when a new record is being created."""
    NEVER = 'NEVER'
    """The field is never ediatble."""

    
class SelectionType(object):
    """Definition of available selection types for enumeration fields."""
    CHOICE = 'CHOICE'
    """Pull-down menu selection."""
    RADIO = 'RADIO'
    """Radio box with a radio button for each selection value."""
    LIST_BOX = 'LIST_BOX'
    """Scrollable list selection."""
    CODEBOOK = 'CODEBOOK'
    """Hand-editable code entry field with a Codebook form invocation button."""
    LIST = 'LIST'
    """Scrollable list with all codebook columns."""
    # Backwards compatibility options
    RADIO_BOX = RADIO
    """Depricated."""
    
   
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
        """Inicialize the instance.

        Arguments:
        
          field -- identifier of a field in the same specification, which provides codebook
            values.  This field must have an enumerator of type 'DataEnumerator' (must be a
            codebook field).

          column -- identifier of a column within the enumerator data object.  The computer will
            return the value of this column as its result.
          
        
        """
        assert isinstance(field, str)
        assert column is None or isinstance(column, str)
        self._field = field
        self._column = column
        self._default = default
        super(CbComputer, self).__init__(self._compute, depends=(field,))
        
    def _compute(self, row):
        cbvalue = row[self._field]
        if cbvalue.value() is not None:
            e = cbvalue.type().enumerator()
            assert e is not None, "CbComputer field '%s' has no enumerator." % self._field
            row = e.row(cbvalue.value(), transaction=row.transaction(),
                        condition=row.runtime_filter(self._field))
            if row:
                value = row[self._column].value()
                if value is None:
                    return self._default
                return value
        return self._default

    def field(self):
        """Vra� id pol��ka, jeho� enumer�tor je pou�it."""
        return self._field
    
    def column(self):
        """Vra� id sloupce datov�ho objektu enumer�toru, kter� ud�v� hodnotu."""
        return self._column
    
    
class CodebookSpec(object):
    """Specification of codebook properties of given view.

    The specification of any view may define the properties of the view, when used as a codebook
    (see the 'codebook' argument of 'FieldSpec' for more information about codebooks).

    'CodebookSpec' may be defined as the 'cb' attribute of a 'Specification'.
    
    """
    def __init__(self, columns=None, sorting=None, display=None, prefer_display=False,
                 display_size=20, enable_autocompletion=True, begin_search=None):
        """Initialize the instance.

        Arguments:
        
          display -- defines the method of retrieving the user visible value of the codebook item
            (see below for more information about user visible values).  None (the default value)
            means to use the codebook code as the user visible value (no conversion).  A string
            value refers to a column in the *data* object of the codebook.  The user visible value
            is retrieved from given column.  You may also pass a function (callable object).  The
            user visible value is then computed by invoking the function, passing it the code
            (internal Python value of the codebook field) as an argument.  The returned value must
            be a string.  If a function is passed and this function has just one argument named
            'row', the function will recieve the data row of the corresponding codebook data object
            as an argument (instead of just the internal codebook value).

          prefer_display -- If true, the user interface will show the display value instead of the
            codebook internal value wherever possible.  For example the browse from will show the
            display value instead of the code (the display is normally only shown in the status
            line or tooltip).  See below for more details

          display_size -- width of the codebook display field in characters.  It is possible to
            override this value by the argument of the same name within the field specification
            (for particular field).
          
          begin_search -- None or an identifier of a column, where incremental search whould be
            automatically started when a codebook form is invoked (GUI only).

        The user visible value of the codebook is used in several situations.  The codebook field
        ('SelectionType.CODEBOOK') will show it in a display next to the form control for entering
        the codebook value.  Other fields (such as 'CHOICE' or 'RADIO') will directly shoe the user
        value (id 'display' was defined) and the internal value is not visible.  Another usage is
        in browse forms (tables), where the value of the display is shown in the application status
        line (GUI) or in a tooltip (web).  If 'prefer_display' is true, however, the display value
        will be used in browse forms directly.

        Arguments 'columns' and 'sorting' are deprecated.  Use a derived specification if you need
        to modify the parameters of the underlying view.

        """
        assert columns is None or is_sequence(columns)
        assert sorting is None or is_sequence(sorting)
        assert display is None or isinstance(display, str) or callable(display) and len(argument_names(display)) == 1
        assert isinstance(prefer_display, bool)
        assert display_size is None or isinstance(display_size, int)
        assert begin_search is None or isinstance(begin_search, str)
        assert isinstance(enable_autocompletion, bool)
        self._columns = columns
        self._sorting = sorting
        self._display = display
        self._prefer_display = prefer_display
        self._display_size = display_size
        self._begin_search = begin_search
        self._enable_autocompletion = enable_autocompletion
        
    def columns(self):
        return self._columns
        
    def sorting(self):
        return self._sorting
        
    def display(self):
        """Return the 'display' specification as passed to the constructor."""
        return self._display
    
    def prefer_display(self):
        """Return true if the display should be prefered over the internal codebook value."""
        return self._prefer_display
        
    def display_size(self):
        """Return the display size in characters."""
        return self._display_size
        
    def begin_search(self):
        """Return the identifier of the column where incremental search should be started."""
        return self._begin_search

    def enable_autocompletion(self):
        return self._enable_autocompletion
    

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
    
    def __init__(self, name, column, type=FormType.BROWSE, label=None,
                 enabled=True):
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
            
          enabled -- funkce, vracej�c� pravdu, pokud m� b�t odkaz aktivn� a
            nepravdu v opa�n�m p��pad�.  Funkci je p�ed�n jeden argument --
            instance `PresentedRow' aktu�ln�ho ��dku.  Nam�sto funkce m��e b�t
            p�ed�na t� p��mo boolean hodnota, kter� dostupnost akce ur�uje
            staticky.
            
        """
        assert isinstance(name, str)
        assert isinstance(column, str)
        assert type in public_attributes(FormType)
        assert label is None or isinstance(label, (str, unicode))
        assert callable(enabled) or isinstance(enabled, bool)
        self._name = name
        self._column = column
        self._type = type
        self._label = label
        self._enabled = enabled
                
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

    def enabled(self):
        """Vra� funkci k zji�t�n� dostupnosti akce, nebo p��mo bool hodnotu."""
        return self._enabled
    

class ListLayout(object):
    """Specification of list layout.

    Currently only implemented in web forms.

    This layout defines an alternative presentation of lists of records.  The records are not
    presented as a table, but as sections, where each record has its own heading, meta information
    and text (description, annotation, message...).
    
    """
    def __init__(self, title, meta=(), layout=None, content=None, image=None, anchor=None,
                 meta_labels=False):
        """Initialize the instance.

        Arguemnts:

          title -- identifier of a field which will be used as a title for each item in the list (a
            string).

          meta -- a sequence of field identifiers (strings) which will be printed underneath each
            item's title as records meta information.

          layout -- GroupSpec instance describing the layout of a fields within each item's
            section.  If used (not None), the fields will be displayed for each record in a manner
            simillar to a show form.  Similarly as for the 'layout' argument in 'ViewSpec', it is
            also possible to pass a sequence of fields (or 'GroupFpec' instances) which will be
            turned into a vertical group automatically.

          content -- identifier of a field which provides a textual content for this item.  The
            text of the field value will be formatted as WIKI text.  If None, no text content will
            be printed.

          image -- identifier of a field which provides an image to be displayed along with each
            record.

          meta_labels -- boolean flag indicating, whether 'meta' fields should be labeled.  If a
            sequence is passed, only meta fields with identifiers contained within the sequence
            will be babeled.

        """
        if isinstance(layout, (list, tuple)):
            layout = GroupSpec(layout, orientation=Orientation.VERTICAL)
        else:
            assert layout is None or isinstance(layout, GroupSpec)
        if isinstance(meta_labels, (bool)):
            meta_labels = meta_labels and meta or ()
        else:
            assert isinstance(meta_labels, (bool, tuple, list))
        self._title = title
        self._meta = meta
        self._content = content
        self._layout = layout
        self._image = image
        self._anchor = anchor
        self._meta_labels = meta_labels
        
    def title(self):
        return self._title
    
    def meta(self):
        return self._meta
    
    def content(self):
        return self._content

    def layout(self):
        return self._layout

    def image(self):
        return self._image
        
    def anchor(self):
        return self._anchor
    
    def meta_labels(self):
        return self._meta_labels
    
    
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

    def __init__(self, id=None, label=None, column_label=None, inherit=None, **kwargs):
        """Initialize field specification.

        Arguments:

          id -- field identifier as a string.  This identifier is used to refer to the field within
            all pytis operations.  The identifier is also used as the name of the related column in
            the underlying data object by default, but this may be overriden by the 'dbcolumn'
            argument.
          
          label -- user visible field label as a plain or unicode string.  This argument (unlike
            the remaining arguments) may also be passed as positional.

          inherit -- may be used to iherit from other field specification.  If a 'FieldSpec'
            instance is passed in this argument, all constructor arguments not overriden in the
            current constructor call will be inheritred from that instance.
          
          column_label -- optional field label in the column view.  The column label is the same as
            'label' by default, but may be overriden by passing a plain or unicode string value.
            
          descr -- brief field description in the extent of approx. one sentence, suitable for
            example for tooltip text.

          virtual -- boolean flag indicating that the field is not bound to the underlying data
            object.  The value of a virtual field will most often be computed on the fly by a
            'Computer'.  See the argument 'computer' for more information.  Since the data type of
            a virtual field can not be obtained form the data object, the hardcoded default type of
            virtual fields is 'pytis.data.String'.  Use the 'type' argument to override it.

          dbcolumn -- name of the related column in the underlying data object.  The name is the
            same as the field identifier by default.  It is not recommended to use different column
            name than the field identifier unless there is a serious reason for it.

          type -- explicit data type as a 'pytis.data.Type' instance.  The data type is normally
            determined from the underlying data object, but you may need to define the type
            explicitly to improve field presentation or pass additional validation constraints.
            Given type, however, must be compatible with the type used by the data object.
            
          width -- field width in characters (integer).  Default width is determined automatically
            if not specified here.  Certain types of input fields may interpret the value
            differently (eg. as a number of columns) when number of characters doesn't make sense.
            
          height -- field height in characters (integer).  Certain types of input fields may
            interpret the value differently (eg. as a number of rows) when number of characters
            doesn't make sense.
          
          column_width -- table column width in characters (integer).  If not defined, defaults to
            'width'.

          disable_column -- If true, it will not be possible to display the field as a table
            column.  The field will not appear in the selection of columns to display and presence
            of such field in default columns ('ViewSpec' argument 'columns') will be announced as
            an error.

          fixed -- passing True value will disable automatic scaling of column width when the table
            size is changed.  The default behavaior is to accommodate column widths to new form
            size, so that the available space is used evenly between all columns.  Fixed columns,
            however will be left out during these recomputations and will keep their prevoius
            widths.
            
          editable -- one of 'Editable' constants or a 'Computer' instance.  The constants
            determine field editability statically, the computer may be used to compute editability
            dynamically (current row will be passed to the computer function as a 'PresentedRow'
            instance).  The default value is 'Editable.ALWAYS', but certain combinations of other
            specification parameters may lead to another default value (for example if 'computer'
            is used, the field is not editable by default).
            
          compact -- pravdiv� hodnota znamen�, �e bude textov� popisek pol��ka
            v edita�n�m formul��i p�imknut k horn�mu okraji vstupn�ho prvku
            (bude tedy nad pol��kem).  V opa�n�m p��pad� (v�choz� chov�n�) je
            popisek vlevo od pol��ka.

          nocopy -- p��znak umo��uj�c� zak�zat kop�rov�n� hodnoty pol��ka p�i
            kop�rov�n� z�znamu.  Standardn� nejsou kop�rov�ny kl��ov� sloupce a
            dopo��t�van� pol��ka na nich z�visej�c�.  N�kdy je v�ak t�eba
            zamezit tak� kop�rov�n� n�kter�ch dal��ch hodnot.  V tom p��pad� je
            nutno p�edat pravdivou hodnotu tomuto argumentu.
            
          default -- default value or a function for computing the default value.  The default
            value is used when a new record is initialized.  Please note, that if computer is
            defined, it has higher precedence than the default value.  You may pass a value
            directly or a callable object.  The callable object will be called with no arguments
            when the default value is needed and its returned value will be used.  In any case, the
            default value must be compatible with the internal Python representation for the data
            type of the field.  If not defined, the default value is determined by the data type
            (usually None).
            
          computer -- a 'Computer' instance for computing the field value based on the values of
            other fields of the same record.  See below for more details about computed fields.
            
          line_separator -- odd�lova� ��dk� v�jedno��dkov�m zobrazen�
            v�ce��dkov� hodnoty.  Tento argument sm� b�t vyu��v�n pouze pro
            read-only pol��ka.
            
          codebook -- name of the specification which acts as a codebook for this field.  If None,
            the field may still have an 'enumerator' on tha data level, but the user interface will
            not be able to determine which specification it is, so displaying the codebook in a
            separate form will not be possible.  If 'codebook' is defined, the default value of
            'selection_type' is 'SelectionType.CODEBOOK'.  Also the default 'enumerator' for the
            field's data type will be automatically set to a 'DataEnumerator' bound to given
            specification.

          display -- overrides this option for particular field.  If not defined, the value
            defaults to the value defined by the related codebook.  See 'CodebookSpec' for more
            information.  Only relevant if the option 'codebook' (above) was deifned.

          prefer_display -- overrides this option for particular field.  If not defined, the value
            defaults to the value defined by the related codebook.  See 'CodebookSpec' for more
            information.

          display_size -- overrides this option for particular field.  If not defined, the value
            defaults to the value defined by the related codebook.  See 'CodebookSpec' for more
            information.

          allow_codebook_insert -- true value enables a button for codebook new record insertion.
            This button will be displayed next to the codebook field.
            
          codebook_insert_spec -- N�zev specifikace, kter� m� b�t pou�ita pro
            vkl�d�n� nov�ch z�znam� (viz 'allow_codebook_insert').  Pokud je
            None, bude pou�ita hodnota 'codebook', nebo jej� v�choz� hodnota.
            Relevantn� jen pro ��seln�kov� pol��ka, kde 'allow_codebook_insert'
            je pravdiv�.
            
          runtime_filter -- provider of enumeration runtime filter as a 'Computer' instance.  The
            computer function receives a 'PresentedRow' instance and generates a filter condition
            based on the current row data and returns it as a 'pytis.data.Operator' instance.  This
            condition is the used to filter out enumerator data for codebook fields as well as
            available completions when autocompletion is enabled.  This is mostly useful for
            modification of available codebook values based on the current values of other fields
            within the form.

          completer -- enumerator used for automatic completion.  The available completions are
            taken from an enumerator object.  If the field has an enumerator (defined by
            'enumerator' or 'codebook'), it will be used for completions automatically (unless
            autocompletion is disabled by the relevant 'CodebookSpec').  This argument, however,
            makes it possible to specify a completer even for fields, which don't have an
            enumerator (the validation constants imposed by enumerator are not desirable).  The
            value of this argument may be an enumerator instance directly
            (e.g. 'pytis.data.FixedEnumerator') or a name of the specification used to create a
            'pytis.data.DataEnumerator'.  Also a sequens (list or tuple) is accepted and converted
            to a 'FixedEnumerator' instance.
            
          selection_type -- one of 'SelectionType' constants defining the type of user interface
            element used to present the related enumeration.  Only relevant for fields with an
            enumerator (specified either by 'codebook' or 'enumerator').
            
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
            
          style -- instance t��dy 'Style' ur�uj�c� vizu�ln� styl pol��ka
            nebo funkce dvou argument� vracej�c� instanci t��dy 'Style'.
            Jedn�-li se o�funkci, jsou jej�mi argumenty id sloupce jako string
            a aktu�ln� datov� ��dek jako instance 'PresentedRow'.  Pokud je
            None, bude pou�it v�choz� styl ��dku (viz. argument 'row_style'
            konstruktoru 'ViewSpec').

          link -- specifikace odkazu/odkaz� do jin�ho n�hledu souvisej�c�ho s
            hodnotou pol��ka.  Instance 'Link' nebo jejich sekvence.  V
            kontextov�m menu ��dku bude pro ka�d� odkaz vytvo�ena jedna polo�ka
            umo��uj�c� odskok do odkazovan�ho n�hledu s vyhled�n�m z�znamu
            odpov�daj�c�ho aktu�ln� hodnot� pol��ka.

          filename -- identifier of the field, which provides the filename for downloading/saving
            the value of this field into a file.  If not None, the user interface should offer
            downloading/saving the content of the field into a file.  This may be relevant for
            binary fields, as well as for ordinary string data.
            

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
        for key, value in (('id', id), ('label', label) ,('column_label', column_label)):
            if value is not None:
                kwargs[key] = value
        if inherit:
            assert isinstance(inherit, FieldSpec), inherit
            kwargs = dict(inherit._kwargs, **kwargs)
        self._kwargs = kwargs
        self._init(**kwargs)
                 
    def _init(self, id, label=None, column_label=None, descr=None, virtual=False, dbcolumn=None,
              type=None, type_=None, width=None, column_width=None, disable_column=False,
              fixed=False, height=None, editable=None, compact=False, nocopy=False, default=None,
              computer=None, line_separator=';', codebook=None, display=None, prefer_display=None,
              display_size=None, allow_codebook_insert=False, codebook_insert_spec=None,
              codebook_runtime_filter=None, runtime_filter=None, selection_type=None,
              completer=None, orientation=Orientation.VERTICAL, post_process=None, filter=None,
              filter_list=None, style=None, link=(), filename=None, **kwargs):
        assert isinstance(id, str)
        assert dbcolumn is None or isinstance(dbcolumn, str)
        self._id = id
        self._dbcolumn = dbcolumn or id
        if type_ is not None:
            assert type is None
            type = type_
        assert label is None or isinstance(label, (str, unicode))
        assert descr is None or isinstance(descr, (str, unicode))
        assert type is None or isinstance(type, pytis.data.Type)
        assert isinstance(virtual, bool)
        assert isinstance(disable_column, bool)
        assert isinstance(fixed, bool)
        assert isinstance(compact, bool)
        assert isinstance(nocopy, bool)
        assert computer is None or isinstance(computer, Computer)
        assert codebook is None or isinstance(codebook, str)
        assert display is None or isinstance(display, str) or callable(display)
        assert completer is None or isinstance(completer, (str, list,tuple, pytis.data.Enumerator))
        assert prefer_display is None or isinstance(prefer_display, bool)
        assert display_size is None or isinstance(display_size, int)
        assert isinstance(allow_codebook_insert, bool)
        assert codebook_insert_spec is None or isinstance(codebook_insert_spec, str)
        assert width is None or isinstance(width, int)
        if codebook_runtime_filter is not None:
            assert runtime_filter is None
            runtime_filter = codebook_runtime_filter
        assert runtime_filter is None or isinstance(runtime_filter, Computer)
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
        assert editable in public_attributes(Editable) or isinstance(editable, Computer)
        assert style is None or isinstance(style, Style) \
               or callable(style), ('Invalid field style', id, style)
        assert filename is None or isinstance(filename, str)
        links = xtuple(link)
        enumerator_kwargs = {}
        for k in ('value_column', 'validity_column', 'validity_condition'):
            if kwargs.has_key(k):
                enumerator_kwargs[k] = kwargs.pop(k)
        if __debug__:
            for lnk in links:
                assert isinstance(lnk, Link)
            for k in kwargs.keys():
                assert k in ('not_null', 'unique', 'constraints', 'enumerator', 'minlen', 'maxlen',
                             'precision', 'format', 'mindate', 'maxdate', 'validation_messages'), \
                             "Invalid FieldSpec argument for field '%s': %r" % (id, k)
        if label is None:
            label = id
        self._label = label
        if column_label is None:
            column_label = label
        self._column_label = column_label
        self._descr = descr
        self._width = width
        if column_width is None and width != 0:
            column_width = width
        self._column_width = column_width
        if virtual and type is None:
            type = pytis.data.String()
        self._virtual = virtual
        self._fixed = fixed
        self._disable_column = disable_column
        self._type = type
        self._compact = compact
        self._nocopy = nocopy
        self._default = default
        self._computer = computer
        self._height = height
        if isinstance(editable, Computer):
            e_func = editable.function()
            if len(argument_names(e_func)) == 2:
                # For backwards compatibility
                editable = Computer(lambda r: e_func(r, id), depends=editable.depends())
        self._editable = editable
        self._line_separator = line_separator
        self._codebook = codebook
        self._display = display
        self._prefer_display = prefer_display
        self._display_size = display_size
        self._allow_codebook_insert = allow_codebook_insert
        self._codebook_insert_spec = codebook_insert_spec
        self._runtime_filter = runtime_filter
        self._selection_type = selection_type
        self._completer = completer
        self._orientation = orientation
        self._post_process = post_process
        self._filter = filter
        self._filter_list = filter_list
        if callable(style):
            s_func = style
            if len(argument_names(s_func)) == 2:
                # For backwards compatibility
                style = lambda r: s_func(id, r)
        self._style = style
        self._links = links
        self._filename = filename
        self._type_kwargs = kwargs
        self._enumerator_kwargs = enumerator_kwargs
        
    def __str__(self):
        return "<FieldSpec for '%s'>" % self.id()
        
    def id(self):
        return self._id

    def dbcolumn(self):
        return self._dbcolumn
    
    def type(self, data=None):
        """Return the specified data type or take it from data object if not defined explicitly."""
        type = self._type
        if data:
            column = data.find_column(self.id())
            if type is not None:
                assert column is None or isinstance(type, column.type().__class__)
            elif column is not None:
                type = column.type()
            elif isinstance(self._computer, CbComputer):
                cb_column = data.find_column(self._computer.field())
                type = cb_column.type().enumerator().type(self._computer.column())
                assert type is not None, "Invalid enumerator column '%s' in CbComputer for '%s'." \
                       % (self._computer.column(), self.id())
            else:
                raise ProgramError("Data type not specified for virtual column '%s'." % self.id())
        return type

    def virtual(self):
        return self._virtual
    
    def label(self):
        return self._label

    def column_label(self):
        return self._column_label

    def descr(self):
        return self._descr

    def width(self, default=12):
        """Return the width specified in constructor or 'default' if specified width was None."""
        if self._width is None:
            return default
        else:
            return self._width

    def column_width(self, default=10):
        """Return the specified 'column_width', 'width' or 'default' whichever is not None."""
        if self._column_width is None:
            return self.width(default)
        else:
            return self._column_width

    def disable_column(self):
        return self._disable_column
        
    def fixed(self):
        return self._fixed
        
    def height(self, default=1):
        """Return the height specified in constructor or 'default' if specified height was None."""
        if self._height is None:
            return default
        else:
            return self._height

    def editable(self):
        return self._editable

    def compact(self):
        return self._compact

    def nocopy(self):
        return self._nocopy

    def default(self):
        return self._default

    def computer(self):
        return self._computer

    def line_separator(self):
        return self._line_separator
    
    def codebook(self, data=None):
        if data is not None:
            enumerator = self.type(data).enumerator()
            if isinstance(enumerator, pytis.data.DataEnumerator) and \
                   isinstance(enumerator.data_factory(), _DataFactoryWithOrigin):
                return enumerator.data_factory().origin() or self._codebook
        return self._codebook

    def display(self):
        return self._display

    def prefer_display(self):
        return self._prefer_display

    def display_size(self):
        return self._display_size

    def allow_codebook_insert(self):
        return self._allow_codebook_insert
    
    def codebook_insert_spec(self):
        return self._codebook_insert_spec
    
    def runtime_filter(self):
        return self._runtime_filter

    def selection_type(self):
        return self._selection_type

    def completer(self):
        return self._completer
    
    def orientation(self):
        return self._orientation

    def post_process(self):
        return self._post_process

    def filter(self):
        return self._filter

    def filter_list(self):
        return self._filter_list

    def style(self):
        return self._style

    def links(self):
        return self._links

    def filename(self):
        return self._filename

    def type_kwargs(self):
        return self._type_kwargs

    def enumerator_kwargs(self):
        return self._enumerator_kwargs


class Fields(object):
    """Field specification container for convenient field specification inheritance.

    Example usage:

      def fields():
          inherited = Fields(super(SpecName, self).fields())
          overriden = (
              Field(inherit=inherited['field3'], editable=NEVER, default='09'),
              Field(inherit=inherited['field5'], editable=NEVER),
              Field(inherit=inherited['field6'], maxlen=8))
          return inherited.fields(override=overriden, exclude=('field8', 'field9'))

    """

    def __init__(self, fields):
        """Initialize the instance.

        The argument is a sequence of field specifications as 'FieldSpec' instances.
        
        """
        self._fields = tuple(fields)
        self._dict = dict([(f.id(), f) for f in fields])

    def __getitem__(self, key):
        return self._dict[key]
    
    def __len__(self, key):
        return len(self._fields)

    def keys(self):
        return self._dict.keys()

    def fields(self, override=(), exclude=()):
        """Return the list of fields in their original order with given fields excluded/overriden.

        Arguments:

          override -- sequence of 'FieldSpec' instances that should replace the items of the same
            id within the original list.

          exclude -- sequence of field identifiers to be excluded from the resulting list.

        """
        #exclude = [isinstance(x, FieldSpec) and x.id() or x for x in exclude]
        override = dict([(f.id(), f) for f in override])
        return [override.get(f.id(), f) for f in self._fields if f.id() not in exclude]
    

class _DataFactoryWithOrigin(pytis.data.DataFactory):
    """Factory na tvorbu datov�ch objekt� dle zadan� specifikace.
    
    Cel� t��da je velk� hack, kter� umo��uje zjednodu�en� ve specifikac�ch.
    Pokud datov� objekt sestav�me pomoc� t�to t��dy a vyu�ijeme jej pro
    specifikaci enumer�toru, je potom mo�n� v u�ivatelsk�m rozhran� zjistit
    n�zev specifikace tohoto enumer�toru a pou��t ji pro zobrazen� ��seln�ku.

    Jinak se t��da chov� zcela shodn� jako jej� rodi�ovsk� t��da.

    """

    
    def set_origin(self, name):
        """Nastav p�vodce t�to specifikace.

        Argumentem je n�zev specifikace pro resolver.

        Instance si takto m��e pamatovat ze kter� specifikace poch�z� a tato
        infomace m��e b�t v aplikaci d�le vyu�ita.
        
        """
        self._origin = name
        
    def origin(self):
        """Vra� n�zev specifikace, ze kter� tato instance poch�z�.

        Pokud je p�vod zn�m, je vr�cen n�zev pro resolver, jinak None.
 
        """
        try:
            return self._origin
        except AttributeError:
            return None

    
class DataSpec(_DataFactoryWithOrigin):
    """T��da zjednodu�uj�c� tvorbu datov� specifikace (deprecated).

    Konstruktor t�to t��dy p�ij�m� argumenty ve zjednodu�en� form� a schov�v� tak n�kter�
    n�zko�rov�ov� detaily p�ed tv�rcem specifikace.  Oproti rodi�ovsk� t��d� je podstatn� omezena
    obecnost, ale v typick�m p��pad� pou�it� datov�ho rozhran� v Pytis aplikaci je specifikace p�i
    pou�it� t�to t��dy nejen p�ehledn�j��, ale tak� flexibiln�j��.

    Podrobn� popis rozhran� viz. konstruktor t��dy.

    POZOR: Tato t��da je ur�ena k z�niku.  Nam�sto n� nech� je pou��v�na t��da 'Specification'
    n��e.  Ta zajist� sestaven� datov� specifikace zcela automaticky, tak�e samostatn� udr�ov�n�
    datov�ch specifikac� ji� nen� pot�eba.

    """
    
    def __init__(self, table, columns, key, access_rights=None, condition=None,
                 data_class_=pytis.data.DBDataDefault,
                 oid=() # temporary backward compatibility argument, ignored
                 ):
        """Inicializuj specifikaci.

        Argumenty:

          table -- n�zev datov� tabulky jako �et�zec.
          
          columns -- sekvence specifikac� sloupc� jako instanc� 'Column'.  Jedn� se v�dy o sloupce
            z tabulky 'table'.
            
          key -- n�zev kl��ov�ho sloupce jako �et�zec.  Sloupec s t�mto identifik�torem mus� b�t
            p��tomn� v 'columns'.
            
          access_rights -- pr�va jako instance 'pytis.data.AccessRights' nebo None, pokud maj� b�t
            pr�va neomezen�.
            
          condition -- A hardcoded condition filtering data of the underlying data object.  This
            condition is used permanently and the user is not able to switch it off or know that it
            exists.  It has the same effect as implementing the condition in the underlying data
            source.  The value is a 'pytis.data.Operator' instance.
        
          data_class_ -- t��da datov�ho objektu, odvozen� od `Data'.

        """
        assert isinstance(table, str)
        assert isinstance(columns, (list, tuple))
        assert isinstance(key, str)
        assert isinstance(access_rights, pytis.data.AccessRights) or access_rights is None
        assert find(key, columns, key=lambda c: c.id()) is not None
        if __debug__:
            for c in columns:
                assert isinstance(c, Column)
        if access_rights is None:
            perm = pytis.data.Permission.ALL
            access_rights = pytis.data.AccessRights((None, (None, perm)))
        bindings = []
        B = pytis.data.DBColumnBinding
        for c in columns:
            kwargs = c.kwargs()
            e = c.enumerator()
            if e:
                enumerator = resolver().get(e, 'data_spec')
                if isinstance(enumerator, _DataFactoryWithOrigin):
                    enumerator.set_origin(e)
            else:
                enumerator = None
            if isinstance(enumerator, pytis.data.DataFactory):
                enumerator = pytis.data.DataEnumerator(enumerator, **c.enumerator_kwargs())
            if enumerator is not None:
                if not kwargs.has_key('not_null'):
                    kwargs['not_null'] = True
                kwargs = dict(kwargs, enumerator=enumerator)
            bindings.append(B(c.id(), table, c.column(), type_=c.type(), **kwargs))
        key = find(key, bindings, key=lambda b: b.column())
        super(DataSpec, self).__init__(data_class_, bindings, key, access_rights=access_rights,
                                       condition=condition)
        self._origin = None

    
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
            nastavit p�ed�n�m kl��ov�ch argument� (viz n��e).
            
          **kwargs -- pokud jsou uvedeny jak�koliv dal�� kl��ov� argumenty,
            budou tyto p�ed�ny konstruktoru datov�ho typu sloupce.  Tento
            postup by m�l b�t preferov�n p�ed explicitn� definic� instance typu
            argumentem 'type', pokud je to mo�n�.

        """
        assert isinstance(id, str), \
               "Invalid value for argument 'id': %s" % id
        assert isinstance(column, str) or column is None, \
               "Invalid value for argument 'column': %s" % column
        assert isinstance(enumerator, str) or enumerator is None, \
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
        self._enumerator_kwargs = {}
        for k in ('value_column', 'validity_column', 'validity_condition'):
            if kwargs.has_key(k):
                self._enumerator_kwargs[k] = kwargs.pop(k)
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

    def enumerator_kwargs(self):
        """Vra� n�zev specifikace enumer�toru jako �et�zec nebo None."""
        return self._enumerator_kwargs

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
    p�ed�ny jako stejnojmenn� argumenty konstruktoru 'ViewSpec'.  V�choz�
    hodnotou argumentu 'help' pro 'ViewSpec' je dokumenta�n� �et�zec
    specifika�n� t��dy.

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

    fields = ()
    """Specification of all fields as a sequence of 'FieldSpec' instances.
    
    May be also defined as a method of the same name."""
    
    access_rights = None
    """Access rights for the view as an 'AccessRights' instance.

    May be also defined as a method of the same name."""

    condition = None
    """A hardcoded condition filtering data of the underlying data object.

    This condition is used permanently and the user is not able to switch it
    off or know that it exists.  It has the same effect as implementing the
    condition in the underlying data source.  The value is a
    'pytis.data.Operator' instance."""
    
    data_cls = pytis.data.DBDataDefault
    """Datov� t��da pou�it� pro vytvo�en� datov�ho objektu."""

    bindings = {}
    """Specification of bindings for use in dual forms.
    
    A dictionary, where the key is the name of the side form specification and the value is a
    'BindingSpec' instance which determines how the current view interconnects with given side form
    in a dual form.
    
    May be also defined as a method of the same name."""
    
    cb = CodebookSpec()
    """'CodebookSpec' instance defining properties of the view when used as a codebook.

    May be also defined as a method of the same name."""
    
    sorting = None
    """Default sorting specification.
    
    May be also defined as a method of the same name."""
    
    prints = None
    """A sequence of print specifications as pairs (TITLE, NAME)."""
    
    def __init__(self, resolver):
        self._resolver = resolver
        for attr in ('fields', 'access_rights', 'condition', 'bindings', 'cb', 'sorting'):
            value = getattr(self, attr)
            if callable(value):
                setattr(self, attr, value())
        assert self.fields, 'No fields defined for %s.' % str(self)
        assert isinstance(self.fields, (list, tuple))
        self._view_spec_kwargs = {'help': self.__class__.__doc__}
        for attr in dir(self):
            if not (attr.startswith('_') or attr.endswith('_spec') or \
                    attr in ('table', 'key', 'access_rights', 'condition',
                             'data_cls', 'bindings', 'cb', 'prints',
                             'oid', # for backward compatibility 
                             )):
                self._view_spec_kwargs[attr] = getattr(self, attr)
        for arg in ('layout', 'actions', 'columns'):
            try:
                value = self._view_spec_kwargs[arg]
            except:
                continue
            if callable(value):
                self._view_spec_kwargs[arg] = value()
        #if self.__class__.__doc__:
            #parts = re.split('\n\s*\n', self.__class__.__doc__, maxsplit=2)
            #if not self._view_spec_kwargs.has_key('description'):
            #    self._view_spec_kwargs['description'] = parts[0]
            #if not self._view_spec_kwargs.has_key('help') and len(parts) > 1:
            #    self._view_spec_kwargs['help'] = parts[1]

    def _create_data_spec(self):
        def type_kwargs(f):
            kwargs = copy.copy(f.type_kwargs())
            assert f.type() is None or not kwargs, \
                   ("Can't define type and its arguemtns at the same time.", f.id(), kwargs)
            enumerator = kwargs.get('enumerator')
            if enumerator is None and f.codebook():
                enumerator = f.codebook()
            if isinstance(enumerator, str):
                enumerator = self._resolver.get(enumerator, 'data_spec')
            if isinstance(enumerator, pytis.data.DataFactory):
                enumerator = pytis.data.DataEnumerator(enumerator, **f.enumerator_kwargs())
            if enumerator is not None:
                assert isinstance(enumerator, pytis.data.Enumerator)
                kwargs['enumerator'] = enumerator
                if not kwargs.has_key('not_null'):
                    kwargs['not_null'] = True
            return kwargs
        if issubclass(self.data_cls, pytis.data.DBData):
            B = pytis.data.DBColumnBinding
            table = self.table or camel_case_to_lower(self.__class__.__name__, '_')
            bindings = [B(f.id(), table, f.dbcolumn(), type_=f.type(), **type_kwargs(f))
                        for f in self.fields if not f.virtual()]
            if self.key:
                keyid = self.key
                if isinstance(keyid, (list, tuple)):
                    assert len(keyid) == 1, ("Multicolumn keys no longer supported:", keyid)
                    keyid = keyid[0]
                key = find(keyid, bindings, key=lambda b: b.id())
                assert key is not None, ("Invalid key column:", keyid)
            else:
                key = bindings[0]
            args = (bindings, key,)
        else:
            columns = []
            for f in self.fields:
                if not f.virtual():
                    type = f.type() or pytis.data.String()
                    kwargs = type_kwargs(f)
                    if kwargs:
                        type = type.__class__(**kwargs)
                    columns.append(pytis.data.ColumnSpec(f.id(), type))
            args = (columns,)
        access_rights = self.access_rights
        assert access_rights is None or not issubclass(self.data_cls, pytis.data.MemData), \
               "Cannot set `access_rights' for a MemData data object."
        if access_rights is None:
            perm = pytis.data.Permission.ALL
            access_rights = pytis.data.AccessRights((None, (None, perm)))
        return _DataFactoryWithOrigin(self.data_cls, *args, **dict(access_rights=access_rights,
                                                                   condition=self.condition))

    def _create_view_spec(self, title=None, **kwargs):
        if not title:
            title = ' '.join(split_camel_case(self.__class__.__name__))
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


