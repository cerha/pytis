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

"""Třídy pro specifikaci prezentační vlastností formulářů.

Třída 'ViewSpec' zastřešuje ostatní specifikační třídy definované tímto
modulem ('FieldSpec', 'GroupSpec', 'LayoutSpec').

Vytvoření instance formulářové třídy je potom v podstatě interpretací
příslušných specifikací.

Všechny třídy tohoto modulu mají specifikační charakter a jejich instance jsou
považovány za immutable, tudíž mohou být libovolně sdíleny.

"""

import pytis.data

from pytis.util import *
from pytis.presentation import *

class TextFormat(object):
    """Konstanty pro definici vstupního formátu textu."""
    PLAIN = 'PLAIN'
    HTML = 'HTML'
    WIKI = 'WIKI'

class BorderStyle(object):
    """Výčtová třída definující konstanty pro styl orámování."""
    ALL    = 'ALL'
    """Mezera je kolem dokola."""
    TOP    = 'TOP'
    """Mezera je jen nahoře."""
    BOTTOM = 'BOTTOM'
    """Mezera je jen dole."""
    LEFT   = 'LEFT'
    """Mezera je jen vpravo."""
    RIGHT  = 'RIGHT'
    """Mezera je jen vlevo."""

    
class Color(object):
    """Na GUI toolkitu nezávislé konstanty pro některé barvy."""
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
    """Specifikační třída definující podobu vnitřku políčka s hodnotou."""

    def __init__(self, foreground=Color.BLACK, background=Color.WHITE,
                 bold=False, slanted=False):
        """Inicializuj instanci.

        Argumenty:

          foreground -- barva textu políčka, jedna z konstant třídy 'Color'
          background -- barva pozadí políčka, jedna z konstant třídy 'Color'
          bold -- příznak určující, zda má být text políčka tučný
          slanted -- příznak určující, zda má být text políčka skloněný
          
        """
        self._foreground = foreground
        self._background = background
        self._bold = bold
        self._slanted = slanted

    def foreground(self):
        """Vrať barvu textu zadanou v konstruktoru."""
        return self._foreground

    def background(self):
        """Vrať barvu pozadí zadanou v konstruktoru."""
        return self._background

    def bold(self):
        """Vrať pravdu, právě když má text blikat."""
        return self._bold

    def slanted(self):
        """Vrať pravdu, právě když má být text tučný."""
        return self._slanted


FIELD_STYLE_DEFAULT = FieldStyle()
FIELD_STYLE_EMPHASIS = FieldStyle(bold=True)
FIELD_STYLE_WARNING = FieldStyle(foreground=Color.RED)


class Orientation(object):
    """Výčtová třída definující konstanty pro směrovou orientaci."""
    HORIZONTAL = 'HORIZONTAL'
    """Horizontální orientace."""
    VERTICAL = 'VERTICAL'
    """Vertikální orientace."""

    
class Button(object):
    """Specifikace tlačítka navázaného na proceduru pro použití ve formulářích.

    Takto lze do formuláře umístit tlačítka, jejichž stisk vyvolá libovolnou
    uživatelem definovanou akci.  Při spuštění akce přitom lze přistupovat k
    aktuálním hodnotám políček formuláře (viz konstruktor).

    Tlačítko lze umístit do LayoutSpec jako jednu z položek (všude tam, kde
    běžně uvádíme id políčka pro umístění vstupního políčka, nebo vnořenou
    LayoutSpec).  Pokud ve specifikaci není 'LayoutSpec' definována explicitně,
    nelze tlačítko použít.

    """
    
    def __init__(self, label, handler, width=None, tooltip=None,
                 active_in_popup_form=True):
        """Inicializuj specifikační instanci.

        Argumenty:

          label -- nápis tlačítka jako string.
          
          handler -- funkce jednoho argumentu, kterým je instance
            'PresentedRow' obsahující aktuální hodnoty políček formuláře.  Tato
            funkce je vyvolána při stisku tlačítka.
            
          width -- šířka (počet znaků).  Implicitně je šířka nastavena
            automaticky podle šířky nápisu ('label'), ale pokud je tento
            argument specifikován, bude šířka nastavena podle dané celočíselné
            hodnoty.
            
          tooltip -- text, který se zobrazí jako bublinová nápověda pro toto
            tlačítko.
            
          active_in_popup_form -- Pokud je zde specifikována pravdivá hodnota,
            nebude tlačítko aktivní v popup (modálních) formulářích.  To je
            určeno zejména pro tlačítka, která mají vyvolat otevření nového
            formuláře na zásobníku oken aplikace, což není právě v době práce s
            modálním formulářem možné.
        
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
    """Výčtová třída definující konstanty pro určení kontextu akce."""
    
    CURRENT_ROW = 'CURRENT_ROW'

    """Akce je prováděna nad aktuálním řádkem tabulky.  Ten bude předán
    handleru akce jako poziční argument v podobě instance PresentedRow."""
    
    SELECTION = 'SELECTION'

    """Akce je prováděna nad aktuálním výběrem, tedy nad všemi vybranými řádky
    tabulky.  Výběr bude předán handleru akce jako poziční argument v podobě
    iterátoru, který vrací jednotlivé řádky jako instance PresentedRow."""

    # TODO: Zde by ještě mohla být jedna hodnota, která by umožnila definovat
    # univerzální akce, které pracují implicitně s aktuálním řádkem, ale pokud
    # existuje výběr, tak s výběrem.
    

class _ActionItem(object):
    
    def __init__(self, title):
        assert isinstance(title, types.StringTypes)
        self._title = title

    def title(self, raw=False):
        """Vrať název akce."""
        title = self._title
        if not raw:
            title = title.replace("&", "")
        return title
            
    
class Action(_ActionItem):
    """Definice kontextově závislé akce.

    Tato definice akce slouží pro použití ve specifikátoru 'actions' třídy
    'ViewSpec'.  Každá akce je ošetřena vlastní obslužnou funkcí, což umožňuje
    implementovat libovolnou vlastní funkcionalitu.  Pro každou akci lze
    definovat také kontext, který určuje kdy má akce smysl a jaké argumenty
    budou handleru akce předány.  Tím je například možné, aby akce pracovala s
    aktuálním řádkem tabulky apod.  Více viz argumenty konstruktoru.
    
    """
    def __init__(self, title, handler, context=ActionContext.CURRENT_ROW,
                 secondary_context=None, enabled=True, access_groups=None,
                 descr=None, hotkey=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          title -- titulek akce zobrazený v uživatelském rozhraní.

          handler -- callable objekt ošetřující danou akci.  Handleru jsou při
            vyvolání akce předány argumenty odpovídající danému kontextu.
            Pokud argument 'context' není None, bude předán první poziční
            argument.  Je to buďto instance 'PresentedRow' odpovídající
            aktuálnímu řádku, nebo sekvence vybraných řádků, v závislosti na
            hodnotě argumentu 'context'.  Pokud je definován také argument
            'secondary_context', bude předán také druhý poziční argument
            odpovídající kontextu ve druhém formuláři duálního formuláře.  Dále
            jsou handleru předány také veškeré zbylé klíčové argumenty.
        
          context -- Instance 'ActionContext' určující v jakém kontextu může
            být akce vyvolána.  Tato hodnota ovlivňuje argumenty, které jsou
            handleru akce předány při jejím vyvolání.  Může být také None, v
            kterémžto případě nejsou handleru přadávány žádné argumenty.
        
          secondary_context -- Instance 'ActionContext', nebo None.  Některé
            akce mohou v duálním formuláři pracovat i s kontextovou informací z
            druhého formuláře.  Tímto argumentem, podobně jako argumentem
            'context' určujeme s čím se pracuje.  Specifikace ovlivní druhý
            poziční argument předaný handleru akce.  Pokud je None, s žádným
            dalším kontextem se nepracuje a druhý poziční argument se handleru
            nepředává.

          enabled -- funkce, vracející pravdu, pokud je akce aktivní a nepravdu
            v opačném případě.  Funkci jsou přadány stejné argumenty, jako
            handleru.  Není-li uvedeno, je akce aktivní v závislosti na
            'access_groups'.  Namísto funkce může být předána též přímo boolean
            hodnota, které dostupnost akce určuje staticky.

          access_groups -- seznam uživatelských skupin, které mají právo akci
            vyvolat.  Akce se pro ostatní uživatele stane automaticky
            neaktivní.  Teprve pokud uživatel patří do jedné z vyjmenovaných
            skupin, je dostupnost akce zjištěna pomocí funkce 'enabled'.
              
          descr -- textový popis akce, který může být použit jak k vytvoření
            nápovědy, tak k zobrazení v uživatelském rozhraní.

          hotkey -- případná klávesová zkratka, která akci vyvolá.

          Všechny ostatní klíčové argumenty budou při vyvolání akce předány
          handleru jako klíčové argmenty.  Takto například lze jeden handler
          použít pro více podobných akcí.
        
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
        """Vrať obslužnou funkci akce."""
        return self._handler

    def context(self):
        """Vrať kontext akce jako instanci 'ActionContext'."""
        return self._context
    
    def secondary_context(self):
        """Vrať přídavný kontext akce, pokud je definován, nebo None."""
        return self._secondary_context

    def enabled(self):
        """Vrať funkci k zjištění dostupnosti akce, nebo přímo bool hodnotu."""
        return self._enabled
        
    def access_groups(self):
        """Vrať seznam uživ. skupin které mají právo akci vyvolat.""" 
        return self._access_groups
        
    def descr(self):
        """Vrať popis akce.""" 
        return self._descr
        
    def hotkey(self):
        """Vrať klávesovou zkratku akce.""" 
        return self._hotkey
    
    def kwargs(self):
        """Vrať klíčové argumenty pro handler akce."""
        return self._kwargs
    

class ActionGroup(_ActionItem):
    """Definice pojmenované logické skupiny akcí.

    Skupiny akcí slouží k logickému seskupení souvisejících akcí.  V
    uživatelském rozhraní se takto definované akce například zobrazí jako
    samostatné podmenu v menu akcí.

    """
    def __init__(self, title, *actions):
        """Inicializuj instanci.

        Argumenty:
        
          title -- název skupiny jako řetězec

          actions -- obsah této skupiny.  Zde platí rekurzívně stejná pravidla
            jako pro stejnojmennný argument konstruktoru ViesSpec.

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
        """Vrať seznam akcí jako tuple."""
        return self._actions
    
    
class GroupSpec(object):
    """Definice skupiny vstupních polí editačního formuláře.

    Tato specifikace se nestará o vzhled jednotlivých vstupních polí, pouze
    definuje jejich rozložení ve formuláři.

    Skupina může obsažené prvky skládat horizontálně, nebo vertikálně a skupiny
    se mohou libovolně vnořovat (prvkem je buďto přímo vstupní políčko, nebo
    jiná skupina -- viz argument 'items' konstruktoru).

    Dalšími argumenty konstruktoru lze určit rozestupy políček, mezeru kolem
    celé skupiny, styl orámování, nadpis skupiny apod.

    Postup skládání políček a skupin ve formuláři je následovný:

    Podle orientace skupiny jsou obsažené celky skládány buďto horizontálně
    (vedle sebe), nebo vertikálně (nad sebe).  Obsaženými celky se rozumí
    sekvence za sebou následujících políček a vnořených skupin.  Všechna za
    sebou následující políčka jsou skládána pod sebe do mřížky (nehledě na to,
    jde-li o vertikální, nebo horizontální skupinu).  Teprve celky takto
    seskupených políček a vnořených skupin jsou skládány podle orientace
    skupiny.  Samostatná vedle sebe umístěná políčka je možno vytvořit jejich
    umístěním do samstatných vnořených podskupin (jednoprvkových).
    
    """
    def __init__(self, items, orientation=Orientation.HORIZONTAL, label=None,
                 gap=2, space=1, border=3, border_style=BorderStyle.ALL):
        """Inicializuj instanci.

        Argumenty:

          items -- obsah této skupiny jako sekvence vnořených skupin (instancí
            'GroupSpec'), nebo přímo vstupních políček určených svým
            identifikátorem (řetězec).
            
          orientation -- orientace skládání obsažených prvků; konstanta
            třídy 'Orientation'.
            
          label -- název skupiny uvedený v záhlaví rámečku - pokud není None,
            skupina bude orámována; string;
            
          gap -- velikost vertikální mezery mezi jednotlivými políčky
            v dialog units; integer; 1 du = 1/4 šířky běžného znaku.
            Relevantní pouze pokud 'items' obsahuje přímo políčka.
            
          space -- velikost mezery mezi políčkem a jeho labelem v du; integer;
            Relevantní pouze pokud 'items' obsahuje přímo políčka.
            
          border -- velikost mezery kolem celé skupiny v du; integer;
          
          border_style -- styl orámování; mezera je implicitně ze všech stran,
            může však být pouze vpravo, vlevo, nahoře, nebo dole; Konstanta
            třídy 'BorderStyle'.

        'label' je vždy považován za jazykově závislý text a tudíž automaticky
        podléhá jazykové konverzi.

        """
        assert is_sequence(items)
        assert label is None or is_anystring(label)
        assert type(gap) == type(0)
        assert gap >= 0
        assert orientation in public_attributes(Orientation)
        assert border_style in public_attributes(BorderStyle)
        for item in items:
            # není třeba kontrolovat rekurzivně, protože kontrola proběhne pro
            # každou instanci na její úrovni...
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
        """Vrať prvky skupiny jako tuple."""
        return tuple(self._items)

    def label(self):
        """Vrať název skupiny."""
        return self._label

    def orientation(self):
        """Vrať orientaci skládání prvků; konstanta třídy 'Orientation'."""
        return self._orientation

    def gap(self):
        """Vrať šířku mezery vertikální mezi políčky v du."""
        return self._gap

    def space(self):
        """Vrať šířku mezery mezi políčkem a jeho labelem v du."""
        return self._space

    def border(self):
        """Vrať šířku mezery kolem celé skupiny v du."""
        return self._border

    def border_style(self):
        """Vrať styl mezery kolem skupiny jako konstantu 'BorderStyle'."""
        return self._border_style

    
class HGroup(GroupSpec):
    """Horizontální seskupení políček.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        super(HGroup, self).__init__(items, **kwargs)

        
class VGroup(GroupSpec):
    """Vertikální seskupení políček.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.VERTICAL
        super(VGroup, self).__init__(items, **kwargs)

        
class LHGroup(HGroup):
    """Horizontální seskupení políček s labelem a orámováním.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LHGroup, self).__init__(*items, **kwargs)

        
class LVGroup(VGroup):
    """Vertikální seskupení políček s labelem a orámováním.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LVGroup, self).__init__(*items, **kwargs)
    

class LayoutSpec(object):
    """Specifikace rozmístění vstupních polí editačního formuláře.

    Editační formulář pro jeden záznam tabulky (na úrovni uživatelského
    rozhraní) se sestává z několika editačních polí - jedno pro každou položku
    záznamu.  Tato pole mohou být vizuálně seskupena do skupin.  Skupina je
    specifikována instancí třídy 'GroupSpec'.  Způsob rozložení polí ve skupině
    je popsán v dokumentaci třídy 'GroupSpec'.

    """
    def __init__(self, caption, group, order=None):
        """Inicializace a doplnění defaultních hodnot atributů.

        Argumenty:
        
          caption -- nadpis editačního formuláře jednoho záznamu
          
          group -- specifikace skupiny políček nejvýšší úrovně; instance
            'GroupSpec'. Tato skupina může obsahovat další vnořené skupiny
            (viz dokumentace třídy 'GroupSpec').
            
          order -- specifikace pořadí procházení mezi políčky jako sekvence
            řatězců - identifikátorů políček.  Pokud není None, je pořadí
            procházení políček určeno pořadím jejich identifikátorů v této
            sekvenci.  V takovém případě musí sekvence obsahovat identifikátory
            všech políček obsažených v 'group'.  Pokud je ponechána výchozí
            hodnota 'None', je pořadí procházení dáno pořadím políček v
            'group' při procházení stromu do hloubky.  Tento výchozí způsob
            určení pořadí v naprosté většině případú vyhovuje a je z pohledu
            uživatele nejpřirozenější, proto se použítí tohoto argumentu
            doporučuje jen v nevyhnutelných případech!  Prioritním řešením by
            vždy měla být reorganizace skupin formuláře.

        'caption' je vždy považován za jazykově závislý text a tudíž automaticky
        podléhá jazykové konverzi.

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
        """Vrať nadpis pro editační formulář jednoho záznamu."""
        return self._caption

    def group(self):
        """Vrať skupinu políček nejvýšší úrovně; instance 'GroupSpec'."""
        return self._group
    
    def order(self):
        """Vrať tuple id všech políček editačního formuláře v pořadí procházení.
        
        Pokud nebylo pořadí v konstruktoru určeno, odpovídá pořadí ve skupinách.

        """
        return self._order


class ViewSpec(object):
    """Kompletující specifikace prezentačních vlastnoostí pro formuláře.

    Instance této třídy zná veškeré prezentační vlasnosti určité entity
    (tabulky z pohledu aplikace).  Třída definuje API pro přístup k těmto
    vlastnostem.  Toto API je využíváno formulářovými třídami.

    Každá instance této třídy definuje vlastnosti pro všechny způsoby
    zobrazení (editační formulář, editační seznam, apod.).

    Každý typ formuláře z potom využívá ze specifikace pouze tu část, která je
    pro něj relevantní.

    """
    
    def __init__(self, title, fields, singular=None, layout=None, columns=None,
                 actions=(), sorting=None, grouping=None, redirect=None,
                 check=None, cleanup=None, on_new_record=None,
                 on_edit_record=None, on_delete_record=None,
                 on_line_commit=None, focus_field=None, description=None,
                 help=None, row_style=FIELD_STYLE_DEFAULT):
        
        """Inicializuj instanci.

        Argumenty:

          title -- název náhledu (řetězec).  Název je používán jako titulek
            záhlaví seznamových formulářů a na dalších místech, kde je
            odkazováno na náhled jako celek, tedy celou množninu záznamů, proto
            by mělo být použito množné číslo, např. Faktury.
            
          singular -- název náhledu jedné položky (řetězec).  Tento název je
            používán všude tam, kde jde o jednu položku náhledu (např záhlaví
            editačního fotmuláře), proto by měl být v jednotném čísle,
            např. Faktura.  Pokud je None, bude použit název daná argumentem
            'title'.
            
          fields -- specifikace políček jako sekvence instancí třídy
            'FieldSpec'.
            
          layout -- specifikace rozložení políček v editačním formuláři,
            instance třídy 'GroupSpec'.  Je možné předat také sekvenci
            identifikátorů políček -- v tom případě bude vytvořena horizontální
            skupina obsahující vyjmenovaná políčka.  Pokud je None, bude
            výchozí rozložení sestaveno poskládáním všech políček definovaných
            ve fields.  Pro zpětnou kompatibilitu je možné použít také
            'LayoutSpec', ale tento způsob definice je považován za nevhodný a
            v budoucnu nebude podporován.
            
          columns -- specifikace sloupců tabulkového formuláře, sekvence
            indentifikátorů políček z 'fields'.  Pokud není určeno, bude
            výchozí seznam sloupců obsahovat všechna políčka z fields, která
            nemají 'column_width' nastaveno na nulu.
            
          actions -- specifikace dostupných uživatelských akcí jako sekvence
            instancí 'Action', vnořených sekvencí, nebo instancí 'ActionGroup'.
            V nejjednodušším případě jde o prostý seznam instancí 'Action'.
            Pokud chceme ovlivnit reprezentaci seznamu dostupných akcí v
            uživatelském rozhraní, je možné akce seskupit do vnořenách tuplů či
            listů.  Takto vytvořené skupiny akcí budou odděleny separátorem.
            Dále je možné vytvořit vnořenou pojmenovanou skupinu
            (reprezentovanou jako samostatné podmenu) použitím instance
            'ActionGroup'.  Prvky v rámci každé 'ActionGroup' lze dále
            seskupovat stejným způsobem.
                        
          sorting -- výchozí seřazení tabulky.  Specifikace řazení ve formátu
            odpovídajícím argumentu 'sort' metody 'pytis.data.select()', nebo
            None.  Potom je výchozí seřazení tabulky podle klíčového sloupce
            datového objektu vzestupně.
            
          grouping -- výchozí vizuální seskupování tabulky.  Může být None,
            idendifikátor sloupce, nebo tuple idendifikátorů.  Vizuální
            seskupování umožňuje graficky odlišit skupiny řádků, které
            následují bezprostředně po sobě a přitom mají stejnou hodnotu všech
            seskupovacích sloupců.  To má význam pouze u sloupců, podle kterých
            je zároveň řazeno.
            
          redirect -- přesměrování formulře pro zobrazení/editaci jednoho
            záznamu.  Jedná se o funkci jednoho argumentu, jímž je instance
            'PresentedRow' reprezentující řádek dat, pro který je
            přesměrování požadováno.  Vrácenou hodnotou musí být název
            specifikace, nad kterou bude vytvářený formulář sestaven.  Pokud
            funkce vrátí None, nebo není žádná funkce specifikována, k žádnému
            přesměrování nedojde.
            
          check -- funkce pro ověření integrity dat celého záznamu.  Jedná se o
            funkci jednoho argumentu, jímž je instance třídy `PresentedRow',
            reprezentující aktuální hodnoty všech políček formuláře.  Na rozdíl
            od validace hodnot políček, která závisí na datovém typu a má k
            dispozici pouze vlastní obsah políčka, má tato funkce k dispozici i
            hodnoty ostatních políček, takže je vhodná pro ověření vzájemné
            slučitelnosti těchto hodnot.  Tato funkce vrací None, pokud je
            vše v pořádku a formulář může být v tomto stavu odeslán, nebo
            id políčka, jehož hodnota způsobila neplatnost záznamu.  Formulář
            by potom měl uživatele vrátit do editace daného polčka.
            
          cleanup -- funkce provádějící závěrečné akce při uzavření formuláře.
            Jedná se o funkci dvou argumentů.  Prvním je výsledný uložený řádek
            odpovídající konečnému stavu databáze a druhým je původní řádek z
            formuláře před uložením (na úrovni databáze mohou být některé
            hodnoty změněny či doplněny ``default'' hodnoty apod.).  Druhý
            řádek obsahuje hodnoty po editaci uživatelem, ale pomocí jeho
            metody `original_row' je možné také získat původní hodnoty před
            editací.  Oba argumenty jsou instance 'PresentedRow'.  Funkce je
            spouštěna vždy při při uzavření editačního formuláře tlačítkem
            ``Ok'' (potvrzením) a to i v případě, že žádná data nebyla změněna.
            
          on_new_record -- akce vložení nového záznamu.  Pokud je None, bude
            provedena výchozí akce (otevření PopupEditForm nad danou
            specifikací).  Předáním funkce lze předefinovat přidání nového
            záznamu v daném náhledu libovolnou vlastní funkcionalitou.  Funkce
            musí akceptovat klíčový argument 'prefill' (viz.
            'pytis.form.new_record()').
            
          on_edit_record -- akce editace záznamu.  Pokud je None, bude
            provedena výchozí akce (otevření PopupEditForm nad danou
            specifikací).  Předáním funkce jednoho klíčového argumentu,
            jímž je instance 'PresentedRow', lze předefinovat editaci záznamu
            libovolnou vlastní funkcionalitou.
            
          on_delete_record -- akce vymazání záznamu.  Pokud je None, bude
            provedena výchozí akce (vymazání záznamu).  Předáním funkce
            jednoho klíčového argumentu, jímž je instance 'PresentedRow', lze
            předefinovat vymazání záznamu libovolnou vlastní
            funkcionalitou. Pokud tato funkce vrací None, nedojde k žádným
            dalším akcím, pokud vrací instancí 'pytis.data.Operator', bude
            provedeno 'pytis.data.delete_many()' s příslušnou podmínkou.
            
          on_line_commit -- akce volaná po uložení řádku v inline editaci.
            Předáním funkce jednoho argumentu, jímž je instance
            `PresentedRow', lze vyvolat doplňující akce po editaci inline
            záznamu.
             
          focus_field -- řetězcová hodnota identifikátoru políčka určující,
            které políčko má po otevření formuláře fokus, nebo funkce jednoho
            argumentu, kterým je PresentedRow pro otevíraný formulář, a která
            vrací příslušný identifikátor políčka.
            
          description -- popis formuláře.  Krátký text rozsahu jedné až dvou
            vět.  Více také viz poznámka níže.
          
          help -- podrobnější nápověda formuláře formátovaná jako strukturovaný
            text (wiki).  Více také viz poznámka níže.

          row_style -- instance třídy 'FieldStyle' určující vizuální styl
            společný pro všechna políčka, nebo funkce jednoho argumentu
            (instance 'PresentedRow') vracející instanci třídy 'FieldStyle'.
           
        Pokud není argument 'layout' nebo 'columns' uveden, bude vygenerován
        implicitní layout a seznam sloupců, odpovídající pořadí políček ve
        'fields'.
        
        Klíčové atributy 'layout' a 'columns' mohou být uváděny bez
        identifikátoru a tudíž by mělo být zaručeno, že budou v budoucnu
        zachovány včetně pořadí.

	Argument `help' nechť je využíván pro rozsáhlejší popis formuláře,
	který vyžaduje formátování.  Jednoduchý popis v rozsahu jedné až dvou
	vět nechť je uváděn jako `description'.  Protože se oba popisy
	používají v jiných situacích, není pravda, že stačí uvést jeden z nich.
	Description by měl být uveden prakticky vždy.  Help slouží pro
	generování nápovědy a namísto něho je možné vytvořit odpovídající
	soubor ve zdrojovém adresáři nápovědy (viz tutoriál Help).

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
        """Vrať název náhledu jako řetězec."""
        return self._title

    def singular(self):
        """Vrať název pro jednu položku náhledu jako řetězec."""
        return self._singular

    def fields(self):
        """Vrať tuple specifikací všech políček v layoutu."""
        return self._fields
        
    def field(self, id):
        """Vrať specifikaci políčka daného 'id' jako instanci 'FieldSpec'.

        Pokud takové políčko neexistuje, vrať 'None'.
        
        """
        return self._field_dict.get(id)
        
    def layout(self):
        """Vrať specifikaci rozvržení editačního formuláře."""
        return self._layout

    def columns(self):
        """Vrať tuple identifikátorů sloupců pro tabulkový formulář."""
        return self._columns

    def actions(self, linear=False):
        """Vrať specifikaci akcí."""
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
        """Vrať specifikaci výchozího řazení."""
        return self._sorting

    def grouping(self):
        """Vrať tuple id sloupců výchozího vizuálního seskupování."""
        return self._grouping

    def redirect(self):
        """Vrať funkci zajišťující přesměrování na jiný název specifikace."""
        return self._redirect
        
    def cleanup(self):
        """Vrať funkci provádějící akce při uzavření formuláře."""
        return self._cleanup

    def check(self):
        """Vrať funkci provádějící kontrolu integrity záznamu."""
        return self._check

    def on_new_record(self):
        """Vrať funkci provádějící vložení nového záznamu, nebo None."""
        return self._on_new_record

    def on_edit_record(self):
        """Vrať funkci provádějící editaci záznamu, nebo None."""
        return self._on_edit_record

    def on_delete_record(self):
        """Vrať funkci provádějící mazání záznamu, nebo None."""
        return self._on_delete_record

    def on_line_commit(self):
        """Vrať funkci volanou po uložení inline řádku."""
        return self._on_line_commit

    def focus_field(self):
        """Vrať řetězec nebo funkci, určující políčko formuláře s fokusem."""
        return self._focus_field

    def description(self):
        """Vrať stručný popis náhledu."""
        return self._description

    def help(self):
        """Vrať podrobnou nápovědu."""
        return self._help
    
    def row_style(self):
        """Vrať výchozí styl řádku, nebo funkci, která jej vypočte."""
        return self._row_style

    
class BindingSpec(object):
    """Specifikace vazby dvou náhledů při propojení do duálního formuláře.

    Tato specifikace definuje vlastnosti napojení dvou formulářů při jejich
    spojení do duálního formuláře.  Definována je jak datová vazba, tak některé
    prezentační vlastnosti spojení.

    Použití je následující:

    Funkce 'binding_spec' ve specifikaci libovolného náhledu vrací slovník
    všech možných spojení tohoto náhledu s dalšími jinými náhledy.  Slovník je
    klíčován názvy specifikací a hodnotou je právě instance 'BindingSpec'.

    Když je tedy například vytvářen duální formulář 'A::B', bude ve specifikaci
    náhledu 'A' (v roli hlavního formuláře) získána z 'binding_spec' položka
    pro náhled 'B' (v roli vedlejšího formuláře).
    

    """
    
    def __init__(self, title, binding_column, side_binding_column=None,
                 hide_binding_column=True, description=None,
                 append_condition=None, sash_ratio=0.5,
                 orientation=Orientation.HORIZONTAL):
        
        """Inicializuj instanci.

        Argumenty:

          title -- titulek tohoto duálního spojení formulářů jako řetězec.
                        
          binding_column -- identifikátor vazebního sloupce.  Tento sloupec
            bude použit pro filtrování vedlejšího formuláře při pohybu po
            záznamech v hlavním formuláři.  Filtrovací podmínka je implicitně
            rovnost hodnot zvolených sloupců hlavního a vedlejšího formuláře.

          side_binding_column -- identifikátor vazebního sloupce ve vedlejším
            formuláři, pokud je jiný, než `binding_column'.  Výchozí hodnota
            `None' znamená, že název vazebního sloupce je ve vedlejším
            formuláři stejný, jako v hlavním formuláři.
            
          hide_binding_column -- vazební sloupec může být (a implicitně je)
            ve vedlejším formuláři vypuštěn (jeho hodnota je pro všechny
            vyfiltrované záznamy shodná -- odpovídá hodnotě z hlavního
            formuláře).

          description -- textový popis daného duálního spojení formulářů.
            
          append_condition -- None nebo funkce jednoho argumentu, kterým je
            aktuální řádek hlavního formuláře. V tomto případě musí funkce
            vrátit instanci Operator, která se připojí k implicitní
            podmínce provazující vazební sloupce.

          sash_ratio -- poměr rozdělení plochy formulářů jako desetinné číslo v
            rozsahu od nuly do jedné.  Výchozí hodnota 0.5 znamená, že
            rozdělení bude přesně v polovině a oběma formulářům tedy připadne
            stejná plocha.  Menši hodnota znamená menší horní formulář, větší
            naopak.  Uplatní se pouze při zobrazení dvou řádkových formulářů v
            horizontálním rozdělení.  Při přítomnosti editačního formuláře je
            poloha oddělovače řízena jeho šířkou/výškou.
            
          orientation -- výchozí orientace duálního formuláře jako konstanta
            'Orientation'.  V horizonálním rozdělení jsou formuláře nad sebou,
            ve vertikálním vedle sebe.

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
        """Vrať titulek duálního formuláře jako řetězec."""
        return self._title

    def description(self):
        """Vrať nápovědu pro formulář."""
        return self._description
    
    def binding_column(self):
        """Vrať id vazebního sloupce hlavního formuláře jako řetězec."""
        return self._binding_column

    def side_binding_column(self):
        """Vrať id vazebního sloupce vedlejšího formuláře jako řetězec."""
        return self._side_binding_column

    def hide_binding_column(self):
        """Vrať pravdu, pokud má být vazební sloupec skryt ve vedlejším fm."""
        return self._hide_binding_column

    def append_condition(self):
        """Vrať doplňující podmínku."""
        return self._append_condition
    
    def sash_ratio(self):
        return self._sash_ratio
    
    def orientation(self):
        return self._orientation

    
class DualSpec(BindingSpec):
    """Specifikace duálního formuláře.
    
    POZOR: Tato třída by již neměla být používána.  Namísto specifikační
    funkce 'dual_spec' samostatné duální specifikaci nechť je nyní
    používána funkce 'binding_spec' ve specifikaci hlavního formuláře.
    Více také viz 'BindingSpec'.
    
    """
    def __init__(self, main_name, side_name, binding_column, title="",
                 side_title=None, side_columns=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          main_name -- jméno specifikace hlavního formuláře; řetězec.

          side_name -- jméno specifikace vedlejšího formuláře; řetězec.

          side_title -- titulek vedlejšího formuláře jako řetězec.  Pokud není
            None, bude v duálním formulři použit tento titulek, namísto titulku
            ze specifikace vedlejšího formuláře.

          side_columns -- sekvence identifikátorů sloupců vedlejšího formuláře.
            Pokud je None, budou ve vedlejším formuláři zobrazeny všechny
            sloupce dané jeho specifikací.
            
        Všechny ostatní argumenty jsou shodné jako u 'BindingSpec', pouze
        argument 'title' zde není povinný.

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
        """Vrať název specifikace hlavního formuláře jako řetězec."""
        return self._main_name
        
    def side_name(self):
        """Vrať název specifikace vedlejšího formuláře jako řetězec."""
        return self._side_name
        
    def side_title(self):
        """Vrať titulek vedlejšího formuláře jako řetězec."""
        return self._side_title

    def side_columns(self):
        """Vrať seznam id sloupců, vedlejšího formuláře."""
        return self._side_columns


    

class Editable(object):
    """Výčtová třída definující konstanty určující editovatelnost políčka."""
    ALWAYS = 'ALWAYS'
    """Políčko je editovatelné vždy."""
    ONCE = 'ONCE'
    """Políčko je editovatelné pouze jednou, při vytváření nového záznamu."""
    NEVER = 'NEVER'
    """Políčko není editovatelné nikdy."""

    
class SelectionType(object):
    """Výčtová třída definující konstanty způsobu výběru z množiny hodnot."""
    CHOICE = 'CHOICE'
    """Výběr z menu.  Viditelná je jen právě vybraná hodnota."""
    RADIO_BOX = 'RADIO_BOX'
    """Pro každou hodnotu je zobrazeno zaškrtávací políčko."""
    LIST_BOX = 'LIST_BOX'
    """Viditelná je vybraná hodnota a \"několik\" kolem."""
    CODEBOOK = 'CODEBOOK'
    """Je možný přímý zápis hodnoty nebo vyvolání číselníkového formuláře."""
    LIST = 'LIST'
    """Vícesloupcové/víceřádkové výběrové políčko pro číselníky."""

   
class PostProcess(object):
    "Výčtová třída definující konstanty pro způsob zpracování uživ. vstupu."
    UPPER = 'UPPER'
    """Převeď veškerá písmena na velká."""
    LOWER = 'LOWER'
    """Převeď veškerá písmena na malá."""

    
class TextFilter(object):
    """Výčtová třída definující konstanty pro způsob filtrování uživ. vstupu.
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
    """Specifikace funkce pro dopočítání hodnoty sloupce."""
    
    def __init__(self, function, depends=None):
        """Inicializuj specifikaci.

        Argumenty:

          function -- libovolná funkce vracející hodnotu kompatibilní s vnitřní
            hodnotou datového typu odpovídajícího sloupci, pro který je
            použita.
            
          depends -- seznam sloupců, na kterých dané počítané políčko závisí.
            Měl by obsahovat všechny sloupce, které počítací funkce používá pro
            určení výsledné hodnoty.  Hodnota potom bude přepočítána pouze
            při změně v uvedených políčkách. Pokud je uveden prázdný seznam,
            nebude hodnota přepočítána nikdy (stále však bude vypočítána při
            inicializaci formuláře). Jedná se o seznam identifikátorů sloupců
            jako řetězců.

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
        """Vrať funkci zadanou v konstruktoru."""
        return self._function

    def depends(self):
        """Vrať seznam id sloupců, ne kterých počítaná hodnota závisí."""
        return self._depends    


class CbComputer(Computer):
    """Specializovaný computer, který získává hodnotu z číselníku.
    
    Tento computer automaticky poskytuje dopočítávací funkci, která získává
    hodnotu z některého sloupce číselníku navázaného na jiné políčko stejného
    náhledu.  Toho lze využít u políček, která ve skutečnosti jen zobrazují
    doplňující informace z číselníku.

    Stejného efektu by sice šlo dosáhnout i použitím standardního computeru s
    příslušnou dopočítávací funkcí, ale tím by se uživatelské rozhraní
    nedozvědělo nic o vazbě dopočítávaného políčka na políčko s enumerátorem a
    nebylo by schopno poskytnout další užitečné funkce, jako například otevření
    náhledu číselníku jako akce kontextového menu buňky, zobrazení klíče
    číselníku při aktivaci buňky, automatické určení datového typu virtuálního
    políčka apod.

    """
    def __init__(self, field, column, default=None):
        """Inicializuj instanci.

        Argumenty:
        
          field -- identifikátor políčka ve stejném náhledu, které je spojeno s
            číselníkem (jeho datový typ má enumerátor typu DataEnumerator).

          column -- sloupeček datového objektu enumerátoru, který udává
            výslednou hodnotu dopočítávací funkce.
        
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
        """Vrať id políčka, jehož enumerátor je použit."""
        return self._field
    
    def column(self):
        """Vrať id sloupce datového objektu enumerátoru, který udává hodnotu."""
        return self._column
    
    
class CodebookSpec(object):
    """Specifikace vlastností náhledu pro jeho použití jako číselníku.

    Nepovinná specifikační funkce 'cb_spec' může pomocí instance této třídy
    upřesnit vlastnosti daného náhledu pro jeho použití v kontextu číselníku.

    """
    def __init__(self, columns=None, sorting=None, display=None,
                 display_size=20, begin_search=None):
        
        """Inicializace a doplnění výchozích hodnot atributů.

        Argumenty:
        
          columns -- sekvence identifikátorů sloupců, které mají být zobrazeny
            v číselníkovém formuláři (třída 'CodebookForm').  Pokud je 'None',
            bude číselník zobrazovat všechny sloupce ze specifikace dané
            tabulky.
            
          sorting -- sekvence identifikátorů sloupců, podle kterých mají být
            záznamy číselníkového formuláře setříděny.  Pokud je 'None',
            bude použito třídění z ViewSpec.
            
          display -- pokud není 'None', určuje způsob zístání uživatelské
            hodnoty číselníku (více o jejím využití viz níže).  Hodnotou může
            být buďto identifikátor sloupečku v datovém objektu enumerátoru
            (bude zobrazena hodnota tohoto sloupečku), nebo funkce jednoho
            argumentu (vnitřní Pythonová hodnota enumerátoru), která vrací
            uživatelskou hodnotu (řetězec).  Může být přdána také dvojice
            (funkce, identifikátor sloupečku).  V tom případě bude argumentem
            funkce hodnota daného sloupce, namísto sloupce vnitřní hodnoty.

          display_size -- šířka políčka displeje ve znacích.  Lze také
            předefinovat stejnojmeným argumentem 'FieldSpec' pro konkrétní
            číselníkové políčko.
          
          begin_search -- None nebo identifikátor sloupce, nad nímž se má
            spustit automatické inkrementální vyhledávání.

        Uživatelská hodnota číselníku je využívána v několika situacích.  U
        běžného číselníkového plíčka typu `SelectionType.CODEBOOK' je pro její
        zobrazení vytvořen displej.  U ostatních číselníkových políček
        (např. CHOICE, RADIO apod.)  jsou uživatelské hodnoty zobrazeny přímo
        ve výběru.  Uživatel v tomto případě vnitřní hodnotu vůbec nevidí, ta
        je používána pouze interně.  Dalším použitím uživatelské hodnoty je
        zobrazení související číselníkové hodnoty ve stavové řádce gridu (při
        aktivaci buňky jejíž hodnota pochází z číselníku).
                    
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
        """Vrať seznam id sloupců, zobrazených ve výběrovém formuláři."""
        return self._columns
        
    def sorting(self):
        """Vrať seznam id sloupců, podle kterých má být číselník setříděn."""
        return self._sorting
        
    def display(self):
        """Vrať id sloupce zobrazovaného v displeji."""
        return self._display
        
    def display_size(self):
        """Vrať velikost displeje (počet znaků)."""
        return self._display_size
        
    def begin_search(self):
        """Vrať identifikátor sloupce pro inkrementální vyhledávání."""
        return self._begin_search


class FormType(object):
    """Specifikace abstraktního typu formuláře podle účelu jeho otevření.

    Tyto konstanty slouží k určení způsobu otevření náhledu.  Díky této
    abstrakci je specifikace nezávislá na použitých třídách uživatelského
    rozhraní.  Definujeme pouze účel, za kterým je formulář otevírán a necháme
    uživatelské rozhraní rozhodnout, který konkrétní formulář je v dané situaci
    nejvhodnější.

    """
    
    BROWSE = 'BROWSE'
    """Otevření řádkového náhledu v podobě tabulky."""
    
    VIEW = 'VIEW'
    """Otevření needitovatelného náhledu jednoho záznamu."""
    
    EDIT = 'EDIT'
    """Otevření editačního formuláře jednoho záznamu."""
    
    INSERT = 'INSERT'
    """Otevření editačního formuláře pro vložení nového záznamu."""


class Link(object):
    """Specifikace odkazu políčka do jiného náhledu.

    Používá se jako hodnota argumentu 'link' ve 'FieldSpec'.

    """
    
    def __init__(self, name, column, type=FormType.BROWSE, label=None):
        """Inicializuj instanci.

        Argumenty:

          name -- název specifikace odkazovaného náhledu jako řetězec.

          column -- identifikátor sloupce v odkazovaném náhledu.  Slouží k
            vyhledání záznamu v odkazovaném náhledu, který odpovídá aktuální
            hodnotě odkazujícího políčka.

          type -- typ formuláře, ve kterám bude odkazovaný náhled otevřen.
            Jedna z konstant 'FormType'.  Výchozím typem je 'FormType.BROWSE'.

          label -- titulek odkazu v menu.  Pokud není uveden, bude odkaz
            pojmenován automaticky a zařazen mezi automaticky generované
            odkazy.  Pokud je titulek uveden, bude v uživatelském rozhraní
            odkaz uveden samostatně před všemi automaticky generovanými odkazy.
            
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
        """Vrať název specifikace odkazovaného náhledu."""
        return self._name

    def column(self):
        """Vrať id odpovídajícího sloupce v odkazovaném náhledu."""
        return self._column

    def type(self):
        """Vrať konstantu typu formuláře, který má být otevřen."""
        return self._type

    def label(self):
        """Vrať typ formuláře, který má být otevřen."""
        return self._label

    
class FieldSpec(object):
    """Specifikace abstraktního políčka zobrazujícího datovou hodnotu.

    Tato specifikace je použitelná pro všechny druhy práce s políčky
    zobrazujícími hodnoty, zejména v obrazovkových formulářích, řádkových
    formulářích a výstupních sestavách.

    Každý modul pracující s políčky si z této poněkud komplexní specifikace
    vybírá pouze pro něj relevantní informace.  Přesný způsob interpretace
    těchto specifikací závisí na třídách implemenujících prvky uživatelské
    rozhraní.  Detailní popis je proto v případě této třídy třeba hledat v
    dokumentaci tříd 'EditForm', 'ListForm', 'InputField' apod.

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
        """Inicializace a doplnění výchozích hodnot atributů.

        Argumenty:

          id -- textový identifikátor pole; neprázdný řetězec.  Pod tímto
            identifikátorem je potom pole přístupné ve všech operacích.
          
          label -- text nápisu u vstupního pole jako řetězec.  Smí být uváděn
            též jako poziční argument.  Pořadí je zaručeno.
          
          column_label -- nadpis sloupce, je-li políčko ve sloupci, jako
            string.  Je-li 'None', je použita hodnota 'label'.
            
          descr -- podrobnější popis v rozsahu cca jedné věty vhodný například
            pro zobrazení bublinové nápovědy.

          virtual -- boolovský příznak.  Pokud je pravdivý, jde o virtuální
            políčko bez vazby na datový objekt.  Hodnota virtuálního políčka je
            nejčastěji vypočtena pomocí computeru (viz specifikační parametr
            'computer'.  Vzhledem k tomu, že datový typ virtuálního políčka
            nelze určit automaticky (z datového objektu), je výchozí typ
            stanoven napevno na 'pytis.data.String()'.  Pokud to nevyhovuje, je
            třeba typ určit explicitně (viz specifikační parametr 'type').

          dbcolumn -- název příslušného databázového sloupce.  Pokud není
            určen, je název databázového sloupce shodný s identifikátorem
            políčka.  Toho je také doporučováno využívat pokud není nějaký
            závažný důvod, aby byl název sloupce jiný, než identifikáor
            políčka.
          
          type -- explicitní určení typu hodnoty, se kterou pracuje toto
            políčko; instance 'pytis.data.Type'.  Výchozí datový typ je určen
            podle odpovídajícího sloupečku datového objektu.  Pokud je však
            políčko virtuální (viz níže), je nutné typ určit explicitně (s
            výjimkou virtuálních políček používajících 'CbComputer').  Typ
            můžeme také explicitně předefinovat, pokud chceme pro prezentaci
            hodnot použít jiný typ, než výchozí typ datového rozhraní (ten však
            *musí* být kompatibilní s typem datového rozhraní).  Viz také
            metoda 'type()'.
            
          width -- šířka pole ve znacích; kladné celé číslo, nebo 0,
            v kterémžto případě je pole skryté.  Je-li 'None', bude použita
            implicitní šířka.  U některých typů vstupních políček může mít
            speciální význam (viz jejich dokumentace).
            
          height -- výška pole ve znacích, kladné reálné číslo.  U některých
            typů vstupních políček může mít speciální význam (viz jejich
            dokumentace).
          
          column_width -- šířka sloupce v tabulce ve znacích, kladné celé
            číslo.  Je-li 'None', je použita hodnota 'width'.
            
          fixed -- pokud bude přadána pravdivá hodnota, nebude šířka sloupce
            automaticky přepočítávána při změně velikosti tabulkového
            formuláře.  Implicitně jsou sloupce automaticky
            roztahovány/zužovány tak, aby byla rovnoměrně využita plocha
            formuláře.  Hodnota 'width/column_width' tak slouží pouze jako
            výchozí hodnota.  Pro 'fixed' sloupce však bude vždy dodržována.
            
          editable -- instance Computer nebo jedna z konstant třídy 'Editable',
            určující za jakých okolností je políčko editovatelné.  Je-li 'None',
            bude použita implicitní hodnota, kterou je obvykle
            'Editable.ALWAYS', ale pro některé kombinace ostatních parametrů
            (např. 'computer') může být implicitní hodnota jiná.
            Pokud je editable instancí třídy `Computer', budou jeho funkci
            předány dva argumenty: instance PresentedRow a identifikátor
            políčka.
            
          compact -- pravdivá hodnota znamená, že bude textový popisek políčka
            v editačním formuláři přimknut k hornímu okraji vstupního prvku
            (bude tedy nad políčkem).  V opačném případě (výchozí chování) je
            popisek vlevo od políčka.
            
          default -- funkce pro výpočet výchozí hodnoty políčka.  Callable
            object vracející hodnotu kompatibilní s vnitřní hodnotou
            odpovídajícího datového typu (viz argument 'type').
            
          computer -- 'instance třídy 'Computer', nebo None.  Specifikuje
            dopočítávané políčko (viz. také níže).
            
          line_separator -- oddělovač řádků v jednořádkovém zobrazení
            víceřádkové hodnoty.  Tento argument smí být využíván pouze pro
            read-only políčka.
            
          codebook -- název specifikace číselníku (řetězec), nebo None.  Název
            specifikace číselníku je normálně přebírán ze specifikace
            enumerátoru datového typu odpovídajícího sloupce v 'DataSpec'.
            Pokud však z nějakého důvodu datová specifikace není definována
            pomocí třídy DataSpec, ale přímo pomocí tříd datového rozhraní,
            není tato informace aplikaci dostupná.  Potom je nutné název
            číselníku určit zde.
            
          display -- umožňuje definovat vlastní hodnotu displeje pro konkrétní
            použití číselníku.  Pokud je None, bude použita hodnota z 'cb_spec'
            ve specifikaci číselníku (což by mělo být také upřednostňováno).
            Pokud je použito, je význam stejný jako u stejnojmenného argumentu
            `CodebookSpec'.  Relevantní jen pro políčka výčtových typů (datový
            typ má definován enumerátor).

          display_size -- velikost displeje číselníku ve znacích.  Relevantní
            jen pro číselníková políčka.  Pokud je None, bude použita hodnota z
            'cb_spec' ve specifikaci číselníku.

          allow_codebook_insert -- Pravdivá hodnota povolí zobrazení tlačítka
            pro přidání nové hodnoty do číselníku.  Relevantní jen pro
            číselníková políčka.
            
          codebook_insert_spec -- Název specifikace, která má být použita pro
            vkládání nových záznamů (viz 'allow_codebook_insert').  Pokud je
            None, bude použita hodnota 'codebook', nebo její výchozí hodnota.
            Relevantní jen pro číselníková políčka, kde 'allow_codebook_insert'
            je pravdivé.
            
          codebook_runtime_filter -- dopočítávač run-time filtrovací podmínky
            číselníku; instance `Computer'.  Tím je umožněno měnit množinu
            hodnot navázaného číselníku za běhu.  Navázaná dopočítávací funkce
            dostane jako argument aktuální data formuláře jako instanci
            'PresentedRow' a vrací filtrovací podmínku typu
            'pytis.data.Operator'.  Číselník bude po změně závislých políček
            aktualizován tak, aby obsahoval pouze řádku vyhovující dané
            podmínce.
            
          selection_type -- způsob výběru z množiny hodnot, jedna z konstant
            třídy 'SelectionType'.  Relevantní jen pro vstupní pole výčtových
            typů (datový typ má určen enumerátor).
            
          orientation -- orientace políčka, jedna z konstant třídy
            'Orientation'; relevantní jen u některých typů vstupních polí, jako
            např. 'inputfield.RadioBoxInputField'.
            
          post_process -- funkce upravující vkládaný text během psaní.  Jedná
            se o funkci jednoho argumentu, kterým je řetězcová hodnota políčka.
            Vrácená hodnota je potom nastavena jako nová hodnota políčka.  Tato
            funkce je volána při každé změně hodnoty textového políčka.
            Příkladem postprocessingu může být změna velikosti písmen, pokud
            chceme, aby textové políčko mohlo obsahovat jen velká písmena.
            Hodnotou tohoto argumentu může být také některá z konstant třídy
            'PostProcess', čímž je ušetřeno psaní některých často používaných
            funkcí.
            
          filter -- specifikace jednoho z přednastavených filtrů znaků
            propouštěných do textového políčka z uživatelského vstupu.  Jedna
            z konstant třídy 'TextFilter'.
            
          filter_list -- sekvence povolených, nebo zakázaných znaků.
            Relevantní jen pro 'filter' typu 'INCLUDE_LIST' nebo
            'EXCLUDE_LIST'.
            
          style -- instance třídy 'FieldStyle' určující vizuální styl políčka
            nebo funkce dvou argumentů vracející instanci třídy 'FieldStyle'.
            Jedná-li se o funkci, jsou jejími argumenty id sloupce jako string
            a aktuální datový řádek jako instance 'PresentedRow'.  Pokud je
            None, bude použit výchozí styl řádku (viz. argument 'row_style'
            konstruktoru 'ViewSpec').

          link -- specifikace odkazu/odkazů do jiného náhledu souvisejícího s
            hodnotou políčka.  Instance 'Link' nebo jejich sekvence.  V
            kontextovém menu řádku bude pro každý odkaz vytvořena jedna položka
            umožňující odskok do odkazovaného náhledu s vyhledáním záznamu
            odpovídajícího aktuální hodnotě políčka.

        Všechny další argumenty, které budou konstruktoru předány jsou
        považovány za argumenty konstruktoru datového typu.  Předání argumentů
        konstruktoru tímto způsobem je preferováno před předáním instance typu.
        Některé argumenty takto ani předat nelze, jako například `enumerator'
        který je vytvářen automaticky podle argumentu `codebook'.  To však
        platí jen při použití třídy `Specification' pro sestavení datové
        specifikace.
            
        Je-li specifikován argument 'computer' a jeho hodnota není 'None', pak
        hodnota sloupce, pokud ji nelze převzít z datového objektu, je
        počítána.  Takový sloupec může být plně \"virtuální\", tj. není
        přítomen v datovém objektu a jeho hodnota je vždy počítána, nebo může
        být v datovém objektu, avšak hodnota je dopočítávána v průběhu editace
        (i nově vytvořeného) záznamu.  Použití plně virtuálních sloupců není
        doporučováno z důvodu výkonnostních problémů v rozsáhlejších
        tabulkových náhledech.  U plně virtuálních políček je také nutné určit
        explicitně datový typ pomocí specifikátoru 'type', protože není možné
        jej převzít automaticky z datového objektu.  Jedinou výjimkou jsou
        dopočítávaná virtuální políčka typu 'CbComputer', kde je typ převzat z
        datového objektu enumerátoru.

        Dopočítávání pomocí 'computer' nelze zaměňovat s výpočtem výchozí
        hodnoty (specifikátor 'default').  Výpočet výchozí hodnoty je proveden
        pouze jednou při vytváření nového řádku.  Funkce pro výpočet výchozí
        hodnoty nezná hodnotu ostatních políček a v průběhu editace se již
        neuplatňuje.  Computer naproti tomu přepočítává hodnotu políčka vždy,
        když dojde ke změně hodnoty políčka, na kterém je závislý (viz
        dokumentace třídy 'Computer').

        Závislosti počítaných políček mohou být i tranzitivní (počítaná políčka
        mohou záviset na jiných počítaných políčkách), ale graf závislostí musí
        tvořit strom (nesmí vzniknout cyklus).
        
        V každém případě je počítaný sloupec implicitně needitovatelný
        ('Editable.NEVER'), pokud není explicitně nastaven jako editovatelný
        pomocí specifikátoru 'editable'.

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
        """Vrať id pole zadané v konstruktoru jako string."""
        return self._id

    def dbcolumn(self):
        return self._dbcolumn
    
    def type(self, data=None):
        """Vrať datový typ ze specifikace, nebo z datového sloupce.

        Pokud byl typ explicitně určen v konstruktoru, bude vrácen tento typ,
        jinak bude vrácen typ určený sloupečkem datového objektu předaného jako
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
        """Vrať textový popisek tohoto pole jako string."""
        return self._label

    def column_label(self):
        """Vrať textový popisek pro nadpis sloupce v tabulkovém zobrazení.

        Pokud nebyl nadpis sloupce (`column_width') v konstruktoru
        specifikován, bude vrácen popisek políčka (metoda `label()').
            
        """
        if self._column_label is None:
            return self.label()
        else:
            return self._column_label

    def descr(self):
        """Vrať podrobnější popis (nápovědu) tohoto pole jako string."""
        return self._descr

    def width(self, default=12):
        """Vrať šířku pole ve znacích; kladné celé číslo.

        Argumenty:

          default -- hodnota, která má být doplněna v případě, že šířka nebyla
            v konstruktoru specifikována; integer.

        """
        if self._width is None:
            return default
        else:
            return self._width

    def column_width(self, default=10):
        """Vrať šířku sloupce ve znacích; kladné celé číslo.

        Argumenty:

          default -- hodnota, která má být doplněna v případě, že v
            konstruktoru nebyla specifikována ani šířka sloupce, ani šířka
            políčka.

        Pokud nebyla šířka sloupce (`column_width') v konstruktoru
        specifikována, bude vrácena obecná šířka políčka (výsledek metody
        `width()').
            
        """
        if self._column_width is None:
            return self.width(default)
        else:
            return self._column_width

    def fixed(self):
        """Vrať pravdu, pokud jde o sloupec s fixní šířkou."""
        return self._fixed
        
    def height(self, default=1):
        """Vrať výšku políčka ve znacích

        Argumenty:

          default -- hodnota, která má být doplněna v případě, že výška nebyla
            v konstruktoru specifikována; integer.
            
        """
        if self._height is None:
            return default
        else:
            return self._height

    def editable(self):
        """Vrať jednu z konstant 'Editable' dle editovatelnosti políčka."""
        return self._editable

    def compact(self):
        """Vrať pravdu, má li být popisek přimknut k hornímu okraji políčka."""
        return self._compact

    def default(self):
        """Vrať funkci pro výpočet výchozí hodnoty."""
        return self._default

    def computer(self):
        """Vrať instanci 'Computer' pro dopočítávání hodnoty."""
        return self._computer

    def line_separator(self):
        """Vrať odddělovač řádků zadaný v konstruktoru."""
        return self._line_separator
    
    def codebook(self, data=None):
        """Vrať název specifikace navázaného číselníku."""
        if data is not None:
            enumerator = self.type(data).enumerator()
            if isinstance(enumerator, pytis.data.DataEnumerator) and \
                   isinstance(enumerator.data_factory(), DataSpec):
                return enumerator.data_factory().origin() or self._codebook
        return self._codebook

    def display_size(self):
        """Vrať velikost displeje číselníku (počet znaků)."""
        return self._display_size

    def display(self):
        """Vrať hodnotu `display' zadanou v konstruktoru."""
        return self._display

    def allow_codebook_insert(self):
        """Vrať pravdu, má-li být  zobrazeno tlačítko přidání do číselníku."""
        return self._allow_codebook_insert
    
    def codebook_insert_spec(self):
        """Vrať název specifikace pro vkládání do číselníku, nebo None."""
        return self._codebook_insert_spec
    
    def codebook_runtime_filter(self):
        """Vrať specifikaci computeru run-time podmínky pro číselník."""
        return self._codebook_runtime_filter

    def selection_type(self):
        """Vrať způsob výběru z množiny hodnot jako konstantu 'SelectionType'.
        """
        return self._selection_type

    def orientation(self):
        """Vrať orientaci políčka jako konstantu 'Orientation'."""
        return self._orientation

    def post_process(self):
        """Vrať funkci zpracovávající uživatelský vstup."""
        return self._post_process

    def filter(self):
        """Vrať typ filtru jako konstantu třídy TextFilter."""
        return self._filter

    def filter_list(self):
        """Vrať seznam povolených/zakázaných znaků pro filter."""
        return self._filter_list

    def style(self):
        """Vrať specifikaci stylu políčka zadanou v konstruktoru."""
        return self._style

    def links(self):
        """Vrať specifikaci odkazu zadanou v konstruktoru."""
        return self._links

    def type_kwargs(self):
        return self._type_kwargs



class DataSpec(pytis.data.DataFactory):
    """Třída zjednodušující tvorbu datové specifikace.

    Konstruktor této třídy přijímá argumenty ve zjednodušené formě a schovává
    tak některé nízkoúrovňové detaily před tvůrcem specifikace.  Oproti
    rodičovské třídě je podstatně omezena obecnost, ale v typickém případě
    použití datového rozhraní v Pytis aplikaci je specifikace při použití této
    třídy nejen přehlednější, ale také flexibilnější.

    Podrobný popis rozhraní viz. konstruktor třídy.

    POZOR: Namísto této třídy je vhodnější používat třídu 'Specification' níže.
    Ta zajistí sestavení datové specifikace zcela automaticky, takže samostatné
    udržování datových specifikací již není potřeba.  Pokud se třída
    'Specification' osvědčí, je možné že tato třída bude v budouvnu zrušena.

    """
    
    def __init__(self, table, columns, key, oid=None, access_rights=None,
                 data_class_=pytis.data.DBDataDefault):
        """Inicializuj specifikaci.

        Argumenty:

          table -- název datové tabulky jako řetězec.
          
          columns -- sekvence specifikací sloupců jako instancí 'Column'.
            Jedná se vždy o sloupce z tabulky 'table'.
            
          key -- název klíčového sloupce jako řetězec.  Sloupec s tímto
            identifikátorem musí být přítomný v 'columns'.
            
          oid -- seznam názvů OID sloupců (tuple).  Pokud je None (výchozí
            hodnota), bude doplněn jeden sloupec s názvem 'oid'.  Pro všechny
            uvedené sloupce budou automaticky přidány příslušné vazby.  Pokud
            tabulka nemá žádný mít žádný oid sloupec, uvedeme prázdný seznam.
            Pokud je sloupec jen jeden, není nutno jej obalovat do tuplu.
            
          access_rights -- práva jako instance 'pytis.data.AccessRights' nebo
            None, pokud mají být práva neomezená.
            
          data_class_ -- třída datového objektu, odvozená od `Data'.
            
        Pokud 'columns' neobsahují sloupec s identifikátorem 'oid', bude
        automaticky doplněn sloupec 'oid' typu 'pytis.data.Oid'.

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
        """Nastav původce této specifikace.

          Argumentem je název specifikace pro resolver.

        Instance si takto může pamatovat ze které specifikace pochází a tato
        infomace může být v aplikaci dále využita.  Je to trochu hack, ale
        umožní to velké zjednodušení
        
        """
        # TODO: Je to trochu hack, ale umožňuje to velké zjednodušení ve
        # specifikacích.  Pokud by šel název specifikace zjistit nějak čistěji,
        # tak by to určitě nebylo na škodu.  Takto jsme omezeni na použití této
        # třídy (s DataFactory tuto informaci nemáme).  Možná nějaké rozšíření
        # na úrovni resloveru?
        self._origin = name
        
    def origin(self):
        """Vrať název specifikace, ze které tato instance pochází.

        Pokud je původ znám, je vrácen název pro resolver, jinak None.
 
        """
        return self._origin

    
class Column(object):
    """Specifikace sloupce pro datovou specifikaci 'DataSpec'."""
    
    def __init__(self, id, column=None, enumerator=None, type=None, **kwargs):
        """Inicializuj specifikaci.

        Argumenty:
        
          id -- identifikátor sloupce (řetězec).  Pod tímto identifikátorem
            bude sloubec vystupovat v aplikaci.
            
          column -- název databázového sloupce (řetězec nebo None).  Implicitně
            je doplněna hodnota 'id', takže pokud se název sloupce
            shoduje s identifikátorem, není jej třeba definovat.
            
          enumerator -- název specifikace pro resolver (řetězec nebo None).  Z
            této specifikace bude získán datový objekt a použit jako enumerátor
            hodnot datového typu.
            
          type -- explicitní určení datového typu sloupce (instance
            'pytis.data.Type', nebo None).  Tento argument by měl být použit
            pouze pokud chceme určit vlastní (odvozený) datový typ, nikoliv
            pokud chceme měnit parametry standardních typů.  Ty je možno
            nastavit předáním klíčovách argumentů (viz níže).
            
          **kwargs -- pokud jsou uvedeny jakékoliv další klíčové argumenty,
            budou tyto předány konstruktoru datového typu sloupce.  Tento
            postup by měl být preferován před explicitní definicí instance typu
            argumentem 'type', pokud je to možné.

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
        """Vrať identifikátor sloupce jako řetězec."""
        return self._id
    
    def column(self):
        """Vrať název sloupce v datovém zdroji jako řetězec."""
        return self._column

    def enumerator(self):
        """Vrať název specifikace enumerátoru jako řetězec nebo None."""
        return self._enumerator

    def type(self):
        """Vrať datový typ sloupce jako instanci 'pytis.data.Type' nebo None."""
        return self._type
    
    def kwargs(self):
        """Vrať slovník klíčových argumentů konstruktoru datového typu."""
        return self._kwargs


class Specification(object):
    """Souhrnná specifikační třída sestavující specifikace automaticky.

    Tato třída zjednodušuje vytváření specifikací tím, že definuje vlastní
    pravidla pro sestavování jak prezentační tak datové specifikace pouze na
    základě jediné specifikace políček a některých dalších vlastností.

    Použití: Specifikaci vytvoříme odvozením specifikační třídy náhledu od této
    třídy a předefinováním jejich veřejných atributů.  To ulehčuje tvorbu
    variant náhledů s využitím dědičnosti.

    Význam atributů: Některé atrubuty jsou definovány přímo touto třídou --
    jejich význam je zdokumentován v rámci jejich dokumentačních řetězců.
    Všechny ostatní veřejné atributy, které odvozená třída definuje budou
    předány jako stejnojmenné argumenty konstruktoru 'ViewSpec'.

    """
    
    table = None
    """Název datové tabulky jako řetězec.

    Pokud název není určen, bude odvozen automaticky z názvu specifikační
    třídy.  Kapitálky jsou převedeny na slova oddělená podtržítkem, takže
    např. pro specifikační třídu 'UcetniOsnova' bude název tabulky
    'ucetni_osnova'.  Z hlediska přehlednosti je doporučováno volit toto jmenné
    schéma a vyhnout se tak explicitnímu určování názvů tabulek.

    """

    key = None
    """Identifikátor klíčového sloupce jako řetězec, nebo jejich sekvence.

    Pokud má tabulka vícenásobný klíč, udáme sekvenci identifikátorů
    příslušných sloupců.  Vyjmenované sloupce se musí nacházet ve specifikaci
    'fields'.  Pokud klíč není definován, bude automaticky za klíčový považován
    první sloupec z 'fields'.

    """

    oid = None
    """Specifikace názvů OID sloupců (tuple).

    Pokud je None (výchozí hodnota), bude doplněn jeden sloupec s názvem 'oid'.
    Pro všechny uvedené sloupce budou automaticky přidány příslušné vazby.
    Pokud tabulka nemá žádný mít žádný oid sloupec, uvedeme prázdný seznam.
    Pokud je sloupec jen jeden, není nutno jej obalovat do tuplu.
    """

    access_rights = None
    """Přístupová práva náhledu jako instance 'AccessRights'."""

    data_cls = pytis.data.DBDataDefault
    """Datová třída použitá pro vytvoření datového objektu."""

    fields = ()
    """Specifikace políček jako sekvence instancí 'FieldSpec'.
    
    Pokud nejde o sekvenci, ale o metodu, je tato metoda v okamžik sestavování
    specifikace zavolána a sekvence polířček je očekávána jako její návratová
    hodnota.
    
    """
    
    bindings = {}
    """Specifikace vazeb pro použití v duálních formulářích.
    
    Slovník, kde klíčem je název specifikace vedlejšího formuláře a hodnotou je
    instance 'BindingSpec' určující jak se tento náhled váže s danám vedlejším
    náhledem.
    
    """
    
    cb = CodebookSpec()
    """Specifikace vlastností náhledu při jeho použití jako číselkíku.
    
    Instance CodebookSpec.
    """
    
    prints = None
    """Specifikace tiskových náhledů.
    
    Sekvence dvojic (titulek, název tiskové specifikace).
    
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
            # TODO: Převod datových typů má mnohá omezení, ale začněme
            # něčím jednodušším a dodělejme co bude potřeba až se ukáže,
            # že je to skutečně potřeba...
            columns = []
            for f in self.fields:
                if not f.virtual():
                    type = f.type() or pytis.data.String()
                    kwargs = copy.copy(f.type_kwargs())
                    enum = e(f.codebook())
                    assert f.type() is None or not kwargs and not enum, \
                           ("Nelze určit zároveň typ a jeho argumenty.",
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
        """Vrať prezentační specifikaci jako instanci 'ViewSpec'."""
        try:
            spec = self._view_spec
        except AttributeError:
            kwargs = self._view_spec_kwargs
            spec = self._view_spec = self._create_view_spec(**kwargs)
        return spec
        
    def data_spec(self):
        """Vrať datovou specifikaci jako instanci datové třídy."""
        try:
            spec = self._data_spec
        except AttributeError:
            spec = self._data_spec = self._create_data_spec()
        return spec
        
    def cb_spec(self):
        """Vrať specifikaci číselníku jako instanci 'CodebookSpec'."""
        return self.cb
    
    def binding_spec(self):
        """Vrať specifikaci navázání v duálním formuláři jako slovník."""
        return self.bindings

    def print_spec(self):
        """Vrať sekvenci specifikací tiskových náhledů."""
        return self.prints


