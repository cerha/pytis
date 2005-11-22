# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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

import pytis.form
import pytis.data

from pytis.util import *
from pytis.presentation import *


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
        self._label = gettext_(label)
        self._handler = handler
        self._width = width
        self._tooltip = gettext_(tooltip)
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
        """Inicializace a dopln�n� v�choz�ch hodnot atribut�.

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
        self._label = gettext_(label)
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
        self._caption = gettext_(caption)
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
    def __init__(self, title, fields, layout=None, columns=None,
                 popup_menu=None, sorting=None, grouping=None, redirect=None,
                 check=None, cleanup=None, on_new_record=None,
                 on_edit_record=None, on_delete_record=None,
                 enable_inline_insert=True, on_line_commit=None,
                 focus_field=None, description=None):
        """Inicializuj instanci.

        Argumenty:

          title -- titulek z�hlav� seznamov�ch formul��� jako �et�zec; m��e
            b�t t� 'None', v�kter�m�to p��pad� formul�� ��dn� z�hlav� nem�.
          fields -- specifikace pol��ek jednoho z�znamu jako sekvence instanc�
            t��dy 'FieldSpec'.  
          layout -- specifikace rozlo�en� pol��ek v edita�n�m formul��i,
            instance t��dy 'LayoutSpec'.
          columns -- specifikace sloupc� tabulkov�ho formul��e, sekvence
            indentifik�tor� pol��ek z 'fields'.
          popup_menu -- specifikace polo�ek kontextov�ho menu pro jeden ��dek
            tabulky.  Tato pol��ka budou p�id�na do kontextov�ho popup menu
            vyvolan�ho prav�m tla��tkem my�i nad jedn�m z�znamem v seznamov�m
            formul��i.  Jde o sekvenci instanc� 'pytis.form.MItem'.
          sorting -- v�choz� se�azen� tabulky.  Specifikace �azen� ve form�tu
            odpov�daj�c�m argumentu 'sort' metody 'pytis.data.select()', nebo
            None.  Potom je v�choz� se�azen� tabulky podle kl��ov�ho sloupce
            datov�ho objektu vzestupn�.
          grouping -- v�choz� vizu�ln� seskupov�n� tabulky.  Idendifik�tor
            sloupce, podle kter�ho maj� b�t ��dky seskupeny, nebo None.
            Vizu�ln� seskupov�n� umo��uje graficky odli�it skupiny ��dk�,
            kter� n�sleduj� bezprost�edn� po sob� a p�itom maj� stejnou
            hodnotu seskupovac�ho sloupce.  To m� v�znam pouze u sloupc�,
            podle kter�ch je z�rove� �azeno.
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
          cleanup -- funkce prov�d�j�c� z�v�re�n� akce p�i uzav�en�
            formul��e.  Jedn� se o funkci jednoho argumentu, j�m� je instance
            'PresentedRow' obsahuj�c� aktu�ln� data formul��e.  Funkce je
            spou�t�na v�dy p�i p�i uzav�en� edita�n�ho formul��e tla��tkem
            \"Ok\" (potvrzen�m) a to i v p��pad�, �e ��dn� data nebyla
            zm�n�na.
          on_new_record -- akce vlo�en� nov�ho z�znamu.  Pokud je None, bude
            provedena v�choz� akce (otev�en� PopupEditForm nad danou
            specifikac�).  P�ed�n�m funkce dvou kl��ov�ch argument� ('key' a
            'prefill', viz 'pytis.form.new_record()') lze p�edefinovat p�id�n�
            nov�ho z�znamu libovolnou vlastn� funkcionalitou.
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
             
           enable_inline_insert -- umo��uje zak�zat vkl�d�n� z�znam� v re�imu
             inline editace (v ��dkov�m formul��i).  Typicky je to nutn� v
             p��pad�, kdy ��dkov� formul�� neobsahuje v�echny sloupce nutn� k
             �sp�n�mu vlo�en� nov�ho z�znamu do datab�ze.  Pokud je pravdiv�,
             bude u�ivteli pokus o vlo�en� z�znamu odm�tnut s p��slu�nou
             zpr�vou.  Vkl�d�n� pomoc� edita�n�ho formul��e je p�itom dostupn�
             v�dy.
           focus_field -- �et�zcov� hodnota identifik�toru pol��ka ur�uj�c�,
             kter� pol��ko m� po otev�en� formul��e fokus, nebo funkce jednoho
             argumentu, kter�m je PresentedRow pro otev�ran� formul��, a kter�
             vrac� p��slu�n� identifik�tor pol��ka.
           description -- popis formul��e pro bublinkov� help.

        Pokud nen� argument 'layout' nebo 'columns' uveden, bude vygenerov�n
        implicitn� layout a seznam sloupc�, odpov�daj�c� po�ad� pol��ek ve
        'fields'.
        
        Kl��ov� atributy 'layout' a 'columns' mohou b�t uv�d�ny bez
        identifik�toru a tud� by m�lo b�t zaru�eno, �e budou v budoucnu
        zachov�ny v�etn� po�ad�.

        """
        assert is_anystring(title)
        assert is_sequence(fields)
        # Initialize field dictionary
        self._field_dict = {}
        for f in fields:
            assert isinstance(f, FieldSpec)
            self._field_dict[f.id()] = f
        self._fields = tuple(fields)
        # Initialize `layout' specification parameter
        if layout is None:
            ids = tuple(map(lambda f: f.id(), self._fields))
            layout = LayoutSpec(title, GroupSpec(ids))
        elif __debug__:
            assert isinstance(layout, LayoutSpec)
            def recourse_group(group):
                for item in group.items():
                    if isinstance(item, GroupSpec):
                        recourse_group(item)
                    elif not isinstance(item, Button):
                        assert self._field_dict.has_key(item), \
                               (_("Unknown field id in 'layout' spec.:"), item)
            recourse_group(layout.group())
            for f in fields:
                for (s, c) in (('computer', f.computer()),
                               ('editable', f.editable())):
                    if isinstance(c, Computer):
                        for dep in c.depends():
                            assert self._field_dict.has_key(dep), \
                              ("Unknown field id '%s' in dependencies for " + \
                               "'%s' specification of '%s'.") % (dep, s, f.id())
        # Initialize `columns' specification parameter
        if columns is None:
            columns=tuple(map(lambda f: f.id(), self._fields))
        elif __debug__:
            assert is_sequence(columns)
            for id in columns:
                assert is_string(id)
                assert self._field_dict.has_key(id), \
                       (_("Unknown column id in 'columns' specification:"), id)
        # Initialize other specification parameters
        if popup_menu is not None:
            assert is_sequence(popup_menu)
            for item in popup_menu:
                assert isinstance(item, (pytis.form.MItem,
                                         pytis.form.MSeparator,
                                         pytis.form.Menu))
        if sorting is not None:
            assert is_sequence(sorting)
            for item in sorting:
                assert is_sequence(item)
        assert grouping is None or self._field_dict.has_key(grouping)
        assert redirect is None or callable(redirect)
        assert check is None or callable(check)
        assert cleanup is None or callable(cleanup)
        assert on_new_record is None or callable(on_new_record)
        assert on_edit_record is None or callable(on_edit_record)
        assert on_delete_record is None or callable(on_delete_record)
        assert on_line_commit is None or callable(on_line_commit)
        assert focus_field is None or is_anystring(focus_field) \
               or callable(focus_field)
        self._title = gettext_(title)
        self._columns = columns
        self._layout = layout
        self._popup_menu = popup_menu
        self._sorting = sorting
        self._grouping = grouping
        self._redirect = redirect
        self._check = check
        self._cleanup = cleanup
        self._on_new_record = on_new_record
        self._on_edit_record = on_edit_record
        self._on_delete_record = on_delete_record
        self._on_line_commit = on_line_commit
        self._enable_inline_insert = enable_inline_insert
        self._focus_field = focus_field
        self._description = description
                
    def fields(self):
        """Vra� tuple specifikac� v�ech pol��ek v layoutu."""
        return self._fields
        
    def field(self, id):
        """Vra� specifikaci pol��ka dan�ho 'id' jako instanci 'FieldSpec'."""
        return self._field_dict.get(id)
        
    def layout(self):
        """Vra� specifikaci rozvr�en� edita�n�ho formul��e."""
        return self._layout

    def columns(self):
        """Vra� tuple identifik�tor� sloupc� pro tabulkov� formul��."""
        return self._columns

    def title(self):        
        """Vra� titulek tabulkov�ho formul��e jako string, nebo None."""
        return self._title

    def popup_menu(self):        
        """Vra� specifikaci polo�ek kontextov�ho menu pro z�znam v tabulce."""
        return self._popup_menu

    def sorting(self):
        """Vra� specifikaci v�choz�ho �azen�."""
        return self._sorting

    def grouping(self):
        """Vra� id sloupce v�choz�ho vizu�ln�ho seskupov�n�, nebo None."""
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

    def enable_inline_insert(self):
        """Vra� pravdu, je li povoleno vkl�d�n� ��dk� v in-line re�imu."""
        return self._enable_inline_insert

    def focus_field(self):
        """Vra� �et�zec nebo funkci, ur�uj�c� pol��ko formul��e s fokusem."""
        return self._focus_field

    def description(self):
        """Vra� �et�zec nebo funkci, ur�uj�c� pol��ko formul��e s fokusem."""
        return self._description

    
class DualSpec(object):
    """Specifikace du�ln�ho formul��e.


    """
    def __init__(self, main_name, side_name, binding_column,
                 side_binding_column=None, side_columns=None,
                 hide_binding_column=True, append_condition=None,
                 title=None, side_title=None, description=None,
                 sash_ratio=0.5):
        """Inicializuj instanci.

        Argumenty:

          main_name -- jm�no specifikace hlavn�ho formul��e; �et�zec.
          side_name -- jm�no specifikace vedlej��ho formul��e; �et�zec.
          binding_column -- identifik�tor vazebn�ho sloupce.  Tento sloupec
            bude pou�it pro filtrov�n� vedlej��ho formul��e p�i pohybu po
            z�znamech v hlavn�m formul��i.  Filtrovac� podm�nka je implicitn�
            rovnost hodnot zvolen�ch sloupc� hlavn�ho a vedlej��ho formul��e.
          side_binding_column -- identifik�tor vazebn�ho sloupce ve vedlej��m
            formul��i, pokud je jin�, ne� `binding_column'.  V�choz� hodnota
            `None' znamen�, �e n�zev vazebn�ho sloupce je ve vedlej��m
            formul��i stejn�, jako v hlavn�m formul��i.
          side_columns -- sekvence identifik�tor� sloupc� vedlej��ho formul��e.
            Pokud je None, budou ve vedlej��m formul��i zobrazeny v�echny
            sloupce dan� jeho specifikac�.
          hide_binding_column -- vazebn� sloupec m��e b�t (a implicitn� je)
            ve vedlej��m formul��i vypu�t�n (jeho hodnota je pro v�echny
            vyfiltrovan� z�znamy shodn� -- odpov�d� hodnot� z hlavn�ho
            formul��e).
          append_condition -- None nebo funkce jednoho argumentu, kter�m je
            aktu�ln� ��dek hlavn�ho formul��e. V tomto p��pad� mus� funkce
            vr�tit instanci Operator, kter� se p�ipoj� k implicitn�
            podm�nce provazuj�c� vazebn� sloupce.
          title -- titulek hlavn�ho formul��e jako �et�zec.  Pokud nen�
            None, bude v du�ln�m formul�i pou�it tento titulek, nam�sto titulku
            ze specifikace hlavn�ho formul��e.
          side_title -- titulek vedlej��ho formul��e jako �et�zec.  Pokud nen�
            None, bude v du�ln�m formul�i pou�it tento titulek, nam�sto titulku
            ze specifikace vedlej��ho formul��e.
            
        """
        assert is_anystring(main_name)
        assert is_anystring(side_name)
        assert is_anystring(binding_column)
        assert is_anystring(title) or title is None
        assert is_anystring(side_title) or side_title is None
        assert append_condition is None or callable(append_condition)
        assert is_anystring(side_binding_column)
        assert side_columns is None or is_sequence(side_columns)
        self._main_name = main_name
        self._side_name = side_name
        self._binding_column = binding_column
        if side_binding_column is None:
            side_binding_column = binding_column
        self._side_columns = side_columns
        self._side_binding_column = side_binding_column
        self._hide_binding_column = hide_binding_column
        self._append_condition = append_condition
        self._title = title
        self._side_title = side_title
        self._sash_ratio = sash_ratio

    def main_name(self):
        """Vra� n�zev specifikace hlavn�ho formul��e jako �et�zec."""
        return self._main_name
        
    def side_name(self):
        """Vra� n�zev specifikace vedlej��ho formul��e jako �et�zec."""
        return self._side_name
        
    def binding_column(self):
        """Vra� id vazebn�ho sloupce hlavn�ho formul��e jako �et�zec."""
        return self._binding_column

    def side_binding_column(self):
        """Vra� id vazebn�ho sloupce vedlej��ho formul��e jako �et�zec."""
        return self._side_binding_column

    def side_columns(self):
        """Vra� seznam id sloupc�, vedlej��ho formul��e."""
        return self._side_columns

    def hide_binding_column(self):
        """Vra� pravdu, pokud m� b�t vazebn� sloupec skryt ve vedlej��m fm."""
        return self._hide_binding_column

    def append_condition(self):
        """Vra� dopl�uj�c� podm�nku."""
        return self._append_condition
    
    def title(self):
        """Vra� titulek hlavn�ho formul��e jako �et�zec."""
        return self._title

    def side_title(self):
        """Vra� titulek vedlej��ho formul��e jako �et�zec."""
        return self._side_title

    def sash_ratio(self):
        return self._sash_ratio

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


class TextFormat(object):
    """Konstanty pro definici vstupn�ho form�tu textu."""
    PLAIN = 'PLAIN'
    HTML = 'HTML'
    WIKI = 'WIKI'

    
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


class CodebookSpec(object):
    """Specifikace ��seln�kov�ho pol��ka.

    Specifikace pro argument 'codebook' konstruktoru t��dy 'FieldSpec'

    """
    def __init__(self, columns=None, display=None,
                 display_size=20, insert_unknown_values=False,
                 begin_search=None):
        
        """Inicializace a dopln�n� v�choz�ch hodnot atribut�.

        Argumenty:
        
          columns -- sekvence identifik�tor� sloupc�, kter� maj� b�t zobrazeny
            v ��seln�kov�m formul��i (t��da 'CodebookForm').  Pokud je 'None',
            bude ��seln�k zobrazovat v�echny sloupce ze specifikace dan�
            tabulky.
          display -- pokud nen� 'None', bude ��seln�kov� pol��ko vybaveno
            displejem, (viz 'CodebookField').  Hodnotou je identifik�tor
            sloupce obsahuj�c�ho hodnotu k�zobrazen� v�displeji (tento sloupec
            mus� b�t obsa�en v datov� specifikaci ��seln�ku).
          display_size -- ���ka pol��ka displeje ve znac�ch
          insert_unknown_values -- Potla�en� argument.  �asem bude zru�en.
          begin_search -- None nebo identifik�tor sloupce, nad n�m� se m�
            spustit automatick� inkrement�ln� vyhled�v�n�.
          
        """
        assert columns is None or is_sequence(columns)
        assert display is None or isinstance(display, types.StringType)
        assert display_size is None or isinstance(display_size, types.IntType)
        assert begin_search is None or isinstance(begin_search,types.StringType)
        if insert_unknown_values:
            log(EVENT, "Pou�it potla�en� argument 'insert_unknown_values'!")
        self._columns = columns
        self._display = display
        self._display_size = display_size
        self._begin_search = begin_search

    def columns(self):
        """Vra� seznam id sloupc�, zobrazen�ch ve v�b�rov�m formul��i."""
        return self._columns
        
    def display(self):
        """Vra� id sloupce zobrazovan�ho v displeji."""
        return self._display
        
    def display_size(self):
        """Vra� velikost displeje (po�et znak�)."""
        return self._display_size
        
    def begin_search(self):
        """Vra� identifik�tor sloupce pro inkrement�ln� vyhled�v�n�."""
        return self._begin_search


class RefSpec(object):
    """Zachov�no pouze pro zp�tnou kompatibilitu."""
    def __init__(self, name, key, columns, sorting=(), returned_columns=None):
        pass


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
                 width=None, column_width=None, fixed=False, height=None, 
                 editable=None, compact=False, type_=None, 
                 default=None, computer=None,
                 line_separator='; ',
                 codebook=None, display_size=None,
                 allow_codebook_insert=False, codebook_insert_spec=None,
                 codebook_runtime_filter=None, 
                 selection_type=None,
                 orientation=Orientation.VERTICAL,
                 references=None,
                 post_process=None, filter=None, filter_list=None,
                 check=None, style=FIELD_STYLE_DEFAULT):
        """Inicializace a dopln�n� v�choz�ch hodnot atribut�.

        Argumenty:

          id  -- textov� identifik�tor pole; nepr�zdn� string.
          label -- text n�pisu u vstupn�ho pole; string
          column_label -- nadpis sloupce, je-li pol��ko ve sloupci, jako
            string.  Je-li 'None', je pou�ita hodnota 'label'.
          descr -- podrobn�j�� popis v rozsahu cca jedn� v�ty vhodn� nap��klad
            pro zobrazen� bublinov� n�pov�dy.
          width -- ���ka pole ve znac�ch; kladn� cel� ��slo, nebo�0,
            v�kter�m�to p��pad� je pole skryt�.  Je-li 'None', bude pou�ita
            implicitn� ���ka.
          height -- v��ka pole ve znac�ch, kladn� re�ln� ��slo.
          column_width -- ���ka sloupce v�tabulce ve znac�ch, kladn� cel�
            ��slo.  Je-li 'None', je pou�ita hodnota 'width'.
          fixed -- pokud bude p�ad�na pravdiv� hodnota, nebude ���ka sloupce
            automaticky p�epo��t�v�na p�i zm�n� valikosti tabulkov�ho
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
          type_ -- explicitn� ur�en� typu hodnoty, se kterou pracuje toto
            pol��ko; instance 'pytis.data.Type'.  Typ m��e b�t v�t�inou ur�en
            podle nav�zan�ho sloupe�ku datov�ho objektu.  N�kter�
            (nap�. dopo��t�van�) pol��ka v�ak nemus� b�t nav�z�na na konkr�tn�
            datov� sloupec, nebo lze z n�jak�ho d�vodu cht�t pro prezentaci
            hodnot pou��t jin� typ (ten v�ak *mus�* b�t instanc� typu sloupce
            z�datov�ho objektu, pokud je pol��ko na n�jak� nav�z�no).  Viz tak�
            metoda 'type()'.  Nen�-li zad�no, je pou�it typ z�datov�ho
            objektu.
          default -- funkce pro v�po�et v�choz� hodnoty pol��ka.  Callable
            object vracej�c� hodnotu kompatibiln� s vnit�n� hodnotou
            odpov�daj�c�ho datov�ho typu (viz argument 'type_').
          computer -- 'None' nebo instance t��dy 'Computer', specifikuje
            dopo��t�van� pol��ko (viz. tak� n�e).  
          line_separator -- odd�lova� ��dk� v�jedno��dkov�m zobrazen�
            v�ce��dkov� hodnoty.  Tento argument sm� b�t vyu��v�n pouze pro
            read-only pol��ka.
          codebook -- n�zev specifikace ��seln�ku (�et�zec), pokud je pol��ko
            na n�jak� v�z�no.
          display_size -- velikost displeje ��seln�ku ve znac�ch.  Relevantn�
            jen pokud je definov�n 'codebook'.  Pokud je None, bude pou�ita
            hodnota z 'cb_spec' ve specifikaci ��seln�ku.
          allow_codebook_insert -- .  Povol zobrazen� tla��tka pro p�id�n� nov�
            hodnoty do ��seln�ku.  Relevantn� jen pokud je definov�n
            'codebook'.
          codebook_insert_spec -- N�zev specifikace, kter� m� b�t pou�ita pro
            vkl�d�n� nov�ch z�znam� (viz 'allow_codebook_insert').  Pokud je
            None, bude pou�ita specifikace z 'codebook'.  Relevantn� jen pokud
            je definov�n 'codebook' a 'allow_codebook_insert' je pravdiv�.
          codebook_runtime_filter -- dopo��t�va� run-time filtrovac�
            podm�nky ��seln�ku; instance `Computer'.  T�m je umo�n�no m�nit
            mno�inu hodnot nav�zan�ho ��seln�ku za b�hu.  Nav�zan� dopo��t�vac�
            funkce dostane jako argument aktu�ln� data formul��e jako instanci
            'PresentedRow' a vrac� filtrovac� podm�nku typu
            'pytis.data.Operator'.  ��seln�k bude po zm�n� z�visl�ch pol��ek
            aktualizov�n tak, aby obsahoval pouze ��dku vyhovuj�c� dan� podm�nce.
          selection_type -- zp�sob v�b�ru z mno�iny hodnot, jedna z�konstant
            t��dy 'SelectionType'.  Relevantn� jen pro vstupn� pole v��tov�ch
            typ�.  Pokud je ur�en ��seln�k argumentem 'codebook', je v�choz�
            hodnotou 'SelectionType.CODEBOOK'.  Jinak je to
            'SelectionType.CHOICE'.
          orientation -- orientace pol��ka, jedna z�konstant t��dy
            'Orientation'; relevantn� jen u n�kter�ch typ� vstupn�ch pol�, jako
            nap�. 'inputfield.RadioBoxInputField'.
          post_process -- funkce upravuj�c� n�jak�m zp�sobem vkl�dan� text.
            Jedn� se o funkci jednoho argumentu, kter�m je hodnota pol��ka
            z�skan� metodou 'InputField.get_value()'.  Vr�cen� hodnota je
            potom nastavena jako nov� hodnota pol��ka (mus� to tedy b�t hodnota
            akceptovateln� metodou 'InputField.set_value()'). Tato funkce je
            vol�na p�i ka�d� zm�n� hodnoty textov�ho pol��ka.  P��kladem
            postprocessingu m��e b�t zm�na velikosti p�smen, pokud chceme, aby
            textov� pol��ko mohlo obsahovat jen velk� p�smena.  Hodnotou tohoto
            argumentu m��e b�t tak� n�kter� z�konstant t��dy 'PostProcess',
            ��m� je u�et�eno psan� n�kter�ch �asto pou��van�ch funkc�.
          filter -- specifikace jednoho z p�ednastaven�ch filtr� znak�
            propou�t�n�ch do textov�ho pol��ka z u�ivatelsk�ho vstupu.  Jedna
            z�konstant t��dy 'TextFilter'.
          filter_list -- sekvence povolen�ch, nebo zak�zan�ch znak�.
            Relevantn� jen pro 'filter' typu 'INCLUDE_LIST' nebo
            'EXCLUDE_LIST'.
          check -- funkce pro ov��en� integrity dat formul��e.  Jedn� se o
            funkci jednoho argumentu, j�m� je instance t��dy `PresentedRow',
            reprezentuj�c� aktu�ln� hodnoty v�ech pol��ek formul��e.  Na rozd�l
            od validace hodnot pol��ek, kter� z�vis� na datov�m typu a m� k
            dispozici pouze vlastn� obsah pol��ka, m� tato funkce k dispozici i
            hodnoty ostatn�ch pol��ek, tak�e je vhodn� pro ov��en� vz�jemn�
            slu�itelnosti t�chto hodnot.  Tato funkce vrac� pravdu, pokud je
            v�e v po��dku a formul�� m��e b�t v tomto stavu odesl�n, nebo
            nepravdu, pokud je nutn� hodnotu pol��ka upravit.
            POZOR: Tato fnkce by nem�la b�t nad�le vyu��v�na.  Nam�sto n�,
            nech� je vyu��v�na stejnojmenn� funkce specifikovan� ve `ViewSpec'.
          style -- instance t��dy 'FieldStyle' ur�uj�c� vizu�ln� styl pol��ka
            nebo funkce dvou argument� vracej�c� instanci t��dy 'FieldStyle'.
            Jedn�-li se o�funkci, jsou jej�mi argumenty id sloupce jako string
            a aktu�ln� datov� ��dek jako instance 'PresentedRow' nebo
            'pytis.data.Row', v�tomto po�ad�.
            
        Nejd�le�it�j��m parametrem vstupn�ho pole je 'id'. To specifikuje jeho
        vazbu do datov�ho zdroje.

        Atributy 'width' a 'height' mohou m�t u n�kter�ch typ� vstupn�ch pol�
        speci�ln� v�znam (viz dokumentace vstupn�ch pol�).

        Argumenty `label' a `width' sm� b�t uv�d�ny t� jako pozi�n� (bez
        kl��e), tak�e jejich po�ad� by m�lo b�t zaru�eno.

        Je-li specifikov�n argument 'computer' a jeho hodnota nen� 'None', pak
        hodnota sloupce, pokud ji nelze p�evz�t z�datov�ho objektu, je
        po��t�na.  Takov� sloupec m��e b�t pln� \"virtu�ln�\", tj. nen�
        p��tomen v�datov�m objektu a jeho hodnota je v�dy po��t�na, nebo m��e
        b�t v�datov�m objektu, av�ak hodnota je dopo��t�v�na v pr�b�hu editace
        (i nov� vytvo�en�ho) z�znamu.

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
        assert is_string(id)
        assert label is None or is_anystring(label)
        assert descr is None or is_anystring(descr)
        assert type_ is None or isinstance(type_, pytis.data.Type)
        assert isinstance(fixed, types.BooleanType)
        assert default is None or callable(default)
        assert computer is None or isinstance(computer, Computer)
        assert codebook is None or isinstance(codebook, types.StringType)
        assert display_size is None or isinstance(display_size, types.IntType)
        assert isinstance(allow_codebook_insert, types.BooleanType)
        assert codebook_insert_spec is None or \
               isinstance(codebook_insert_spec, types.StringType)
        assert width is None or isinstance(width, types.IntType)
        assert codebook_runtime_filter is None or \
               isinstance(codebook_runtime_filter, Computer)
        assert selection_type is None or \
               selection_type in public_attributes(SelectionType)
        assert post_process is None or callable(post_process) or \
               post_process in public_attributes(PostProcess)
        assert filter is None or filter in public_attributes(TextFilter)
        assert filter not in ('INCLUDE_LIST','EXCLUDE_LIST') or \
               is_sequence(filter_list)
        if editable is None:
            if width == 0 or computer: editable = Editable.NEVER
            else: editable = Editable.ALWAYS
        assert editable in public_attributes(Editable) or \
               isinstance(editable, Computer)
        assert check is None or callable(check)
        if check is not None:
            log(EVENT, "Pou�ita potla�en� funkce 'check' t��dy 'FieldSpec'!")
        if references is not None:
            log(EVENT, "Pou�it potla�en� argument 'references' t��dy 'FieldSpec'!")
        self._id = id
        self._label = gettext_(label)
        self._descr = gettext_(descr)
        self._width = width
        if column_width is None:
            column_width = width
        self._column_width = column_width
        self._column_label = column_label
        self._fixed = fixed
        self._type = type_
        self._compact = compact
        self._default = default
        self._computer = computer
        self._height = height
        self._editable = editable
        self._line_separator = line_separator
        self._codebook = codebook
        self._display_size = display_size
        self._allow_codebook_insert = allow_codebook_insert
        self._codebook_insert_spec = codebook_insert_spec
        self._codebook_runtime_filter = codebook_runtime_filter
        self._orientation = orientation
        if selection_type is None:
            if codebook is not None:
                selection_type = SelectionType.CODEBOOK
            else:
                selection_type = SelectionType.CHOICE
 
        cbtypes = (SelectionType.CODEBOOK, SelectionType.LIST)
        assert selection_type not in cbtypes or codebook is not None, \
               "SelectionType.%s vy�aduje argument 'codebook'!" % selection_type
        self._selection_type = selection_type
        self._post_process = post_process
        self._filter = filter
        self._filter_list = filter_list
        self._check = check
        self._style = style

    def __str__(self):
        return "<FieldSpec: id='%s'; label='%s'>" % \
               (self.id(), self.label())
        
    def id(self):
        """Vra� id pole zadan� v�konstruktoru jako string."""
        return self._id

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
        
    def type(self, data):
        """Vra� datov� typ ze specifikace, nebo z datov�ho sloupce.

        Pokud byl typ explicitn� ur�en v konstruktoru, bude vr�cen tento typ,
        jinak bude vr�cen typ ur�en� sloupe�kem datov�ho objektu p�edan�ho jako
        argument.
        
        """
        column = data.find_column(self.id())
        if self._type is not None:
            type = self._type
            assert column is None or \
                   isinstance(type, column.type().__class__)
        else:
            assert column != None, \
                   ('Data type not specified for virtual column ' + \
                    '(column not found in data object is supposed virtual).',
                    self.id())
            type = column.type()
        return type
        
    def default(self):
        """Vra� funkci pro v�po�et v�choz� hodnoty."""
        return self._default

    def computer(self):
        """Vra� instanci 'Computer' pro dopo��t�v�n� hodnoty."""
        return self._computer

    def line_separator(self):
        """Vra� oddd�lova� ��dk� zadan� v�konstruktoru."""
        return self._line_separator
    
    def codebook(self):
        """Vra� specifikaci nav�zan�ho ��seln�ku."""
        return self._codebook

    def display_size(self):
        """Vra� velikost displeje ��seln�ku (po�et znak�)."""
        return self._display_size
    
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

    def check(self):
        """Vra� funkci pro ov��en� integrity dat formul��e."""
        return self._check

    def style(self):
        """Vra� specifikaci stylu pol��ka zadanou v�konstruktoru."""
        return self._style
