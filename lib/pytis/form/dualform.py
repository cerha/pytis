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

"""Duální formuláře.

Duální formuláře rozdělují okno na dvě části, z nichž každá obsahuje jeden
jednoduchý formulář, přičemž data horního a dolního formuláře jsou nějakým
způsobem závislá.  Blíže viz dokumentace jednotlivých tříd.

"""

import pytis.data
import pytis.output
from pytis.form import *
import wx


class DualForm(Form):
    """Formulář složený ze dvou spolupracujících formulářů.

    Duální formulář je rozdělen na dvě části umístěné nad sebou.  V horní části
    je hlavní (řídící) formulář, v dolní části je vedlejší (podřízený)
    formulář.  Kromě vzájemné spolupráce jsou formuláře nezávislé, mají vlastní
    aktivační kategorii a sadu tlačítek.  Mezi formuláři lze libovolně
    přecházet.

    Duální formulář funguje jako celek, nerealizuje obecné dělení okna aplikace
    dvě samostatné části.  Pokud některý podformulářú duálního formuláře vyvolá
    nový formulář, který má být vložen do okna aplikace,  je tento nový
    formulář vložen do stejného okna, ve kterém je uložen duální formulář.

    Hlavní a vedlejší formuláře o své přítomnosti v duálním formuláři ani o své
    spolupráci neví.  Duální formulář je z jejich pohledu zcela transparentní.

    Tato třída je základem všech konfigurací dvojice formulářů.  Předpokládá se
    využití následujících konfigurací:

    - Hlavní formulář je 'BrowseForm', vedlejší formulář je 'EditForm'.
      Příkladem této konfigurace je listování seznamem položek (hlavní
      formulář) s editací aktuálně vybrané položky (vedlejší formulář).

    - Hlavní formulář je 'EditForm', vedlejší formulář je 'BrowseForm'.
      Příkladem této konfigurace je editace faktury, kde celkové údaje
      o faktuře jsou v hlavním formuláři, zatímco seznam fakturovaných položek
      je ve vedlejším formuláři.

    - Oba formuláře jsou typu 'BrowseForm'.  Příkladem této konfigurace je
      editace seznamu faktur (v hlavním formuláři) kombinovaná s editací
      položek (ve vedlejším formuláři) aktuálně vybrané faktury.

    Tyto konkrétní konfigurace jsou realizovány potomky této třídy.

    """
    _DESCR = _("duální formulář")
    
    def get_command_handler_instance(cls, application):
        return application.top_window()
    get_command_handler_instance = classmethod(get_command_handler_instance)
    
    def __init__(self, *args, **kwargs):
        """Inicializuj duální formulář.

        Argumenty jsou stejné jako v předkovi, specifikují však hlavní formulář
        duálního formuláře.
        
        """
        super_(DualForm).__init__(self, *args, **kwargs)
        wx_callback(wx.EVT_SET_FOCUS, self, self._on_focus)
        wx_callback(wx.EVT_SIZE, self, self._on_size)

    def _init_attributes(self, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Všechny klíčové argumenty jsou posléze předány konstruktoru hlavního
        formuláře.
        
        """
        super_(DualForm)._init_attributes(self)
        self._unprocessed_kwargs = kwargs
        self._active_form = None        
        self._sash_ratio = self._initial_sash_ratio()

    def _initial_sash_ratio(self):
        return self._view.sash_ratio()
        
    def _create_view_spec(self):
        spec = self._resolver.get(self._name, 'dual_spec')
        assert isinstance(spec, DualSpec) 
        return spec

    def _create_data_object(self):
        # Hlavní i vedlejší formulář mají svůj datový objekt.
        return None

    def _create_form(self):
        # Vytvoř rozdělené okno
        self._splitter = splitter = wx.SplitterWindow(self._parent, -1)
        wx_callback(wx.EVT_SPLITTER_DOUBLECLICKED, splitter,
                    splitter.GetId(), lambda e: True)
        wx_callback(wx.EVT_SPLITTER_SASH_POS_CHANGED, splitter,
                    splitter.GetId(), self._on_sash_changed)
        # Vytvoř formuláře
        main_form_kwargs = self._unprocessed_kwargs
        self._main_form = self._create_main_form(splitter, **main_form_kwargs)
        self._side_form = self._create_side_form(splitter)
        splitter.SplitHorizontally(self._main_form, self._side_form)
        self._select_form(self._main_form)
        self._set_main_form_callbacks()
        self._set_side_form_callbacks()
    
    def _create_main_form(self, parent, **kwargs):
        return None

    def _create_side_form(self, parent):
        return None

    def _set_main_form_callbacks(self):
        pass
    
    def _set_side_form_callbacks(self):
        pass
    
    def _other_form(self, form):
        if form is self._main_form:
            other_form = self._side_form
        else:
            other_form = self._main_form
        return other_form

    def _select_form(self, form, force=False):
        if form is None or (form is self._active_form and not force):
            return
        form.focus()
        self._active_form = form
        

    def title(self):
        """Vrať název formuláře jako řetězec."""
        return self._main_form.title()

    def select_row(self, *args, **kwargs):
        if hasattr(self._main_form, 'select_row'):
            self._main_form.select_row(*args, **kwargs)
        else:
            log(EVENT, "Hlavní formulář nepodporuje metodu `select_row()'!")
        
    def show_popup_menu(self):
        self._active_form.show_popup_menu()

    def on_command(self, command, **kwargs):
        if command == DualForm.COMMAND_OTHER_FORM:
            self._select_form(self._other_form(self._active_form))
            return True
        elif command == Form.COMMAND_PRINT and \
                 kwargs.get('form') in (self._main_form, self._side_form):
            target = kwargs['form']
        else:
            target = self._active_form
        if command.handler() is not None:
            kwargs['mainform'] = self._main_form
            kwargs['sideform'] = self._side_form
            kwargs['norefresh'] = True
            # TODO: To je odporný hack!!!
            result = target._on_handled_command(command, **kwargs)
            self.refresh()
            target.SetFocus()
            return result
        if isinstance(target, KeyHandler):
            return target.on_command(command, **kwargs)
        else:
            return False

    def active_form(self):
        return self._active_form

    def show(self):
        # Musíme volat show obou podformulářů, protože splitter je nevolá a
        # přitom v nich mohou být inicializační či ukončovací akce.
        self._side_form.show()
        self._main_form.show()
        self._splitter.Enable(True)
        self._splitter.Show(True)

    def hide(self):
        self._side_form.hide()
        self._main_form.hide()
        self._splitter.Show(False)
        self._splitter.Enable(False)

    def close(self):
        self._main_form.close()
        self._side_form.close()
        self._main_form = None
        self._side_form = None
        self._active_form = None
        self._splitter.Show(False)
        self._splitter.Close()
        self._splitter.Destroy()               
        self.Close()
        self.Destroy()               

    def _sash_position(self, size):
        return size.height * self._sash_ratio
            
    def _on_sash_changed(self, event):
        size = self._splitter.GetSize()
        self._sash_ratio = event.GetSashPosition() / float(size.height)
        
    def _on_size(self, event):
        size = event.GetSize()
        self._splitter.SetSize(size)
        self._splitter.SetSashPosition(self._sash_position(size))
        event.Skip()
        
    def _on_focus(self, event):
        active = self._active_form
        if active:
            active.focus()


class ImmediateSelectionDualForm(DualForm):
    """Duální formulář s okamžitou obnovou vedlejšího formuláře."""
    
    def __init__(self, *args, **kwargs):
        super_(ImmediateSelectionDualForm).__init__(self, *args, **kwargs)
        self._selection_data = None

    def _on_main_selection(self, row):
        r = row.row()
        if r != self._selection_data:
            self._side_form.Show(False)
            if self._do_selection(row):
                self._selection_data = r

    def _do_selection(self, row):
        return True
    
    
class PostponedSelectionDualForm(ImmediateSelectionDualForm):
    """Duální formulář se zpožděnou obnovou vedlejšího formuláře."""
    
    _SELECTION_TICK = 2

    def __init__(self, *args, **kwargs):
        super_(PostponedSelectionDualForm).__init__(self, *args, **kwargs)
        self._selection_candidate = None
        wx_callback(wx.EVT_IDLE, self, self._on_idle)        

    def _on_idle(self, event):
        if self._side_form is None or self._selection_candidate is None:
            pass
        elif self._selection_tick > 0:
            self._selection_tick = self._selection_tick - 1
            microsleep(100)
            event.RequestMore()
        else:
            row = self._selection_candidate
            self._selection_candidate = None
            if self._do_selection(row):
                self._selection_data = row.row()
            else:
                self._selection_candidate = row
                microsleep(100)
                event.RequestMore()
                
    def _on_main_selection(self, row):
        if row.row() != self._selection_data:
            self._side_form.Show(False)
            self._selection_candidate = copy.copy(row)
            self._selection_tick = self._SELECTION_TICK

    
class SideBrowseDualForm(PostponedSelectionDualForm):
    """Duální formulář s vedlejším formulářem 'SideBrowseForm'."""
        
    def title(self):
        """Vrať název formuláře jako řetězec."""
        return self._main_form.title() + " :: " + self._side_form.title()

    def _create_side_form(self, parent):
        view = self._view
        self._binding_column = bcol = view.binding_column()
        self._side_binding_column = sbcol = view.side_binding_column()
        f = SideBrowseForm(parent, self._resolver, view.side_name(),
                           sibling_name=view.main_name(),
                           sibling_row=lambda : self._selection_data,
                           sibling_binding_column=bcol,
                           binding_column=sbcol,
                           hide_binding_column=view.hide_binding_column(),
                           append_condition=view.append_condition(),
                           title=view.side_title(),
                           columns=view.side_columns(),
                           guardian=self)
        self._sbcol_type = f._data.find_column(sbcol).type()
        return f

    def _set_side_form_callbacks(self):
        f = self._side_form
        if isinstance(self._main_form, Refreshable):
            f.set_callback(ListForm.CALL_MODIFICATION,
                           self._main_form.refresh)
        f.set_callback(ListForm.CALL_USER_INTERACTION,
                       lambda : self._select_form(self._side_form))

    def _do_selection(self, row):
        focused = wx_focused_window()
        import _grid
        if isinstance(focused, wx.TextCtrl) and \
               focused.GetName() == _grid.IncrementalSearch.TEXT_CONTROL_NAME:
            # Ošetřovat to speciálním způsobem musíme proto, že je třeba
            # za všech okolností zabránit odskočení z widgetu inkrementálního
            # vyhledávání.  Ten způsob je trochu hloupý, protože vedlejší
            # formulář se nezobrazí, dokud není aplikace opět zaostřena, ale
            # zná někdo lepší řešení?
            return False
        try:
            v, e = self._sbcol_type.validate(row[self._binding_column].export())
            f = self._side_form
            f.set_prefill({self._side_binding_column: v})
            f.filter(data=row.row())
            f.Show(True)
            # Tento _select_form zde byl neznámo proč.  Protože se tak necht2n2
            # přesune focus na horní formulář např. po editaci dolního
            # formuláře, bylo nutné řádek zakomentovat.  Pokud to mělo nějaký
            # význam, bude třeba najít jiné řešení, respektující oba problémy.
            # Pokud se po nějakou dobu na žádný problém nepřijde, je možné to
            # smazat včetně tohoto komentáře...  TC, 22.8.2005
            #self._select_form(self._main_form, force=True)
        finally:
            if focused:
                focused.SetFocus()
        return True

    def close(self):
        self._side_form.set_callback(ListForm.CALL_MODIFICATION, None)
        super_(SideBrowseDualForm).close(self)


class BrowseDualForm(SideBrowseDualForm, Refreshable):
    """Duální formulář s hlavním formulářem 'BrowseForm'.
    
    Hlavním formulářem je instance třídy 'BrowseForm', vedlejším formulářem je
    instance třídy 'SideBrowseForm'.  Formuláře jsou vzájemně propojeny
    prostřednictvím vazebních sloupců daných specifikací `DualSpec'.
    
    """
    def _create_main_form(self, parent, **kwargs):
        dualform = self
        class _MainBrowseForm(BrowseForm):
            def title(self):
                title = dualform._view.title()
                if title is not None:
                    return title
                return super_(_MainBrowseForm).title(self)
        return _MainBrowseForm(parent, self._resolver, self._view.main_name(),
                               guardian=self, **kwargs)


    def _set_main_form_callbacks(self):
        f = self._main_form
        f.set_callback(ListForm.CALL_USER_INTERACTION,
                       lambda : self._select_form(self._main_form))
        f.set_callback(ListForm.CALL_SELECTION, self._on_main_selection)
        f.set_callback(ListForm.CALL_ACTIVATION, self._on_show_record)
        f.set_callback(BrowseForm.CALL_NEW_RECORD, self._on_new_record)
    
    def _on_new_record(self, copy=False):
        result = self._main_form._on_new_record(copy=copy)
        if result:
            self._main_form.select_row(result.row())
            self._side_form.refresh(when=ListForm.DOIT_IMMEDIATELY)
            self._select_form(self._side_form)
            invoke_command(ListForm.COMMAND_NEW_LINE_AFTER)
        return result
    
    def _on_show_record(self, key):
        run_form(ShowDualForm, self._name, select_row=key)

    def _refresh(self, when=None):
        self._main_form.refresh()
        # Refresh sideformu by zde teoreticky být nemusel.  Ten by měl být
        # proveden automaticky po refreshi mainformu.  Někdy k tomu však z
        # neznámých důvodů nedojde, takže jej zde pro jistotu přidáme
        # natvrdo... :-(  Problém je pravděpodobně někde ve zpracování idle
        # eventů ve wx.  Projevuje se to dokonce i tak, že to např. v jednom
        # formuláři funguje a v jiném ne, nebo dokonce stejný formulář na
        # jednom počítači funguje a na jiném ne...
        self._side_form.refresh()

        
class ShowDualForm(SideBrowseDualForm, Refreshable):
    """Duální formulář s hlavním formulářem 'BrowsableShowForm'.

    """
    def __init__(self, *args, **kwargs):
        super_(ShowDualForm).__init__(self, *args, **kwargs)
        self._initialization_done = False

    def _on_idle(self, event):
        super(ShowDualForm, self)._on_idle(event)
        if not self._initialization_done:
            self._initialization_done = True
            self._select_form(self._main_form, force=True)
        
    def _create_main_form(self, parent, **kwargs):
        return BrowsableShowForm(parent, self._resolver,
                                 self._view.main_name(),
                                 guardian=self, **kwargs)

    def _set_main_form_callbacks(self):
        self._main_form.set_callback(BrowsableShowForm.CALL_SELECTION,
                                     self._on_main_selection)

    def _refresh(self, when=None):
        self._side_form.refresh()

    def _sash_position(self, size):
        return min(self._main_form.size().height, size.height - 200)


class BrowseShowDualForm(ImmediateSelectionDualForm, Refreshable):
    """Duální formulář s řádkovým seznamem nahoře a náhledem dole.

    Tento formulář slouží k současnému zobrazení přehledu položek a formuláře s
    rozšiřujícími informacemi.  Podle specifikace vazby a dolního formuláře
    může jít jak o detaily k aktuálnímu záznamu, tak o souhrnné informace
    (např. výsledky agregací nad daty horního formuláře atd.).

    """
    _DESCR = _("duální náhled")
    
    def _create_main_form(self, parent, **kwargs):
        return BrowseForm(parent, self._resolver, self._name, guardian=self,
                          **kwargs)

    def _set_main_form_callbacks(self):
        f = self._main_form
        f.set_callback(ListForm.CALL_USER_INTERACTION,
                       lambda : self._select_form(self._main_form))
        f.set_callback(ListForm.CALL_SELECTION, self._on_main_selection)

    def _create_side_form(self, parent):
        name = self._view.side_name()
        return ShowForm(parent, self._resolver, name)

    def _do_selection(self, row):
        if self._side_form is not None:
            view = self._view
            bcol, sbcol = (view.binding_column(), view.side_binding_column())
            self._side_form.select_row({sbcol: row[bcol]})
            self._side_form.Show(True)
            self._select_form(self._main_form, force=True)
        return True
    
    def _sash_position(self, size):
        return max(size.height - self._side_form.size().height, 200)
    
    def _refresh(self, when=None):
        self._main_form.refresh()

        
class DescriptiveDualForm(BrowseShowDualForm):
    """Duální formulář s řádkovým seznamem nahoře a náhledem dole.

    Tento formulář slouží k současnému zobrazení přehledu položek a podrobnému
    zobrazení aktuální položky.  Náhled není určen k editaci této položky.  Jde
    vlastně o speciální případ formuláře rodičovské řídy, kdy náhled v dolním
    formuláři je dán stejnou specifikací, jako horní formulář.

    """
    
    def _init_attributes(self, **kwargs):
        self._in_mainform_selection = False
        super_(DescriptiveDualForm)._init_attributes(self, **kwargs)
        
    def _create_view_spec(self):
        return None

    def _initial_sash_ratio(self):
        return None # V této třídě se nepoužívá

    def _create_side_form(self, parent):
        return ShowForm(parent, self._resolver, self._name)

    def _set_side_form_callbacks(self):
        self._side_form.set_callback(ShowForm.CALL_SELECTION,
                                     self._on_side_selection)
    
    def _do_selection(self, row):
        if self._side_form is not None:
            self._in_mainform_selection = True
            self._side_form.select_row(row.row())
            self._side_form.Show(True)
            self._select_form(self._main_form, force=True)
            self._in_mainform_selection = False
        return True

    def _on_side_selection(self, row):
        if self._main_form is not None and not self._in_mainform_selection:
            self._main_form.select_row(row.row())
