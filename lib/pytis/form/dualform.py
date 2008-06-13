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

"""Duální formuláře.

Duální formuláře rozdělují okno na dvě části, z nichž každá obsahuje jeden
jednoduchý formulář, přičemž data horního a dolního formuláře jsou nějakým
způsobem závislá.  Blíže viz dokumentace jednotlivých tříd.

"""

import pytis.data
import pytis.output
from pytis.form import *
import wx


class DualForm(Form, Refreshable):
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
    DESCR = _("duální formulář")
    
    def __init__(self, *args, **kwargs):
        """Inicializuj duální formulář.

        Argumenty jsou stejné jako v předkovi, specifikují však hlavní formulář
        duálního formuláře.
        
        """
        super(DualForm, self).__init__(*args, **kwargs)
        wx_callback(wx.EVT_SET_FOCUS, self, lambda e: self.focus())
        wx_callback(wx.EVT_SIZE, self, self._on_size)

    def _init_attributes(self, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Všechny klíčové argumenty jsou posléze předány konstruktoru hlavního
        formuláře.
        
        """
        super(DualForm, self)._init_attributes()
        self._unprocessed_kwargs = kwargs
        self._active_form = None
        self._orientation = self._initial_orientation()
        self._splitter_position_initialized = False

    def _initial_orientation(self):
        #return Orientation.VERTICAL
        return self._view.orientation()

    def _create_view_spec(self):
        self._main_name, self._side_name = main, side = self._name.split('::')
        return self._resolver.get(main, 'binding_spec')[side]

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
        self._main_form = self._create_main_form(splitter, **self._unprocessed_kwargs)
        self._side_form = self._create_side_form(splitter)
        if self._orientation == Orientation.HORIZONTAL:
            splitter.SplitHorizontally(self._main_form, self._side_form)
        else:
            splitter.SplitVertically(self._main_form, self._side_form)
        if isinstance(self._main_form, EditForm):
            gravity = 0
        elif isinstance(self._side_form, EditForm):
            gravity = 1
        elif self._orientation == Orientation.HORIZONTAL:
            gravity = 0.5
        else:
            gravity = 0
        splitter.SetSashGravity(gravity)
        splitter.SetMinimumPaneSize(50)
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
        return self._view.title() or ' / '.join((self._main_form.title(), self._side_form.title()))

    def select_row(self, *args, **kwargs):
        if hasattr(self._main_form, 'select_row'):
            return self._main_form.select_row(*args, **kwargs)
        else:
            log(EVENT, "Hlavní formulář nepodporuje metodu `select_row()'!")
        
    def _cmd_other_form(self):
        self._select_form(self._other_form(self._active_form))

    def active_form(self):
        """Vrať aktivní formulář tohoto duálního formuláře."""
        return self._active_form

    def inactive_form(self):
        """Vrať neaktivní formulář tohoto duálního formuláře."""
        return self._other_form(self._active_form)

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

    def save(self):
        self._main_form.save()
        self._side_form.save()

    def restore(self):        
        self._main_form.restore()
        self._side_form.restore()

    def _exit_check(self):
        return self._main_form._exit_check() and self._side_form._exit_check()
            
    def _cleanup(self):
        self._main_form.close(force=True)
        self._side_form.close(force=True)
        self._side_form = None
        self._main_form = None
        self._side_form = None
        self._active_form = None
        self._splitter.Show(False)
        self._splitter.Close()
        self._splitter.Destroy()
        
    def focus(self):
        active = self._active_form
        if active:
            active.focus()

    def _initial_sash_position(self, mode, size):
        def dim(size, mode):
            if mode == wx.SPLIT_HORIZONTAL:
                return size.height
            else:
                return size.width
        self._splitter_position_initialized = True
        saved_position = self._get_state_param('sash_position', -1, int)
        if saved_position > 0:
            return min(saved_position, dim(size, mode))
        if isinstance(self._main_form, EditForm):
            return min(dim(self._main_form.size(), mode), dim(size, mode) - 200)
        elif isinstance(self._side_form, EditForm):
            return max(dim(size, mode) - dim(self._side_form.size(), mode), 200)
        elif mode == wx.SPLIT_HORIZONTAL:
            return size.height * self._view.sash_ratio()
        else:
            return min(self._main_form.size().width, size.width - 200)

    def _on_sash_changed(self, event):
        position = event.GetSashPosition()
        self._set_state_param('sash_position', position)
        # Sometimes the form is not redrawn correctly...
        self._main_form.Refresh()
        self._active_form.focus()
        event.Skip()

    def _on_size(self, event):
        size = event.GetSize()
        mode = self._splitter.GetSplitMode()
        self._splitter.SetSize(size)
        if not self._splitter_position_initialized:
            position = self._initial_sash_position(mode, size)
            self._splitter.SetSashPosition(position)
        event.Skip()

    def _refresh(self, when=None):
        if isinstance(self._main_form, Refreshable):
            self._main_form.refresh()
        if isinstance(self._side_form, Refreshable):
            self._side_form.refresh()
        
class ImmediateSelectionDualForm(DualForm):
    """Duální formulář s okamžitou obnovou vedlejšího formuláře."""
    
    def __init__(self, *args, **kwargs):
        self._selection_data = None
        super(ImmediateSelectionDualForm, self).__init__(*args, **kwargs)

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
        super(PostponedSelectionDualForm, self).__init__(*args, **kwargs)
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
        r = row and row.row()
        if r != self._selection_data:
            self._side_form.Show(False)
            self._selection_candidate = copy.copy(row)
            self._selection_tick = self._SELECTION_TICK

    
class SideBrowseDualForm(PostponedSelectionDualForm):
    """Duální formulář s vedlejším formulářem 'SideBrowseForm'."""
        
    def _create_side_form(self, parent):
        return SideBrowseForm(parent, self._resolver, self._side_name, guardian=self,
                              main_form=self._main_form, binding=self._view)

    def _set_side_form_callbacks(self):
        f = self._side_form
        if isinstance(self._main_form, Refreshable):
            f.set_callback(ListForm.CALL_MODIFICATION, self._main_form.refresh)
        f.set_callback(ListForm.CALL_USER_INTERACTION, lambda : self._select_form(self._side_form))

    def _do_selection(self, row):
        form = self._side_form
        focused = wx_focused_window()
        try:
            form.on_selection(row)
            form.Show(True)
        finally:
            if focused:
                focused.SetFocus()
        return True

    def _cleanup(self):
        self._side_form.set_callback(ListForm.CALL_MODIFICATION, None)
        super(SideBrowseDualForm, self)._cleanup()


class BrowseDualForm(SideBrowseDualForm):
    """Duální formulář s hlavním formulářem 'BrowseForm'.
    
    Hlavním formulářem je instance třídy 'BrowseForm', vedlejším formulářem je
    instance třídy 'SideBrowseForm'.  Formuláře jsou vzájemně propojeny
    prostřednictvím vazebních sloupců daných specifikací `BindingSpec'.
    
    """

    def _create_main_form(self, parent, **kwargs):
        #dualform = self
        #class _MainBrowseForm(BrowseForm):
        #    def title(self):
        #        title = dualform._view.title()
        #        if not title:
        #            title = super(_MainBrowseForm, self).title()
        #        return title
        return BrowseForm(parent, self._resolver, self._main_name, guardian=self, **kwargs)

    def _set_main_form_callbacks(self):
        f = self._main_form
        f.set_callback(f.CALL_USER_INTERACTION, lambda : self._select_form(self._main_form))
        f.set_callback(f.CALL_SELECTION, self._on_main_selection)
        f.set_callback(f.CALL_ACTIVATION, self._on_main_activation)
    
    def _on_main_activation(self, alternate=False):
        if alternate:
            form, name = (DescriptiveDualForm, self._main_form.name())
        else:
            form, name = (ShowDualForm, self._name)
        run_form(form, name, select_row=self._main_form.current_key())

        
class ShowDualForm(SideBrowseDualForm):
    """Duální formulář s hlavním formulářem 'BrowsableShowForm'.

    """
    def __init__(self, *args, **kwargs):
        super(ShowDualForm, self).__init__(*args, **kwargs)
        self._initialization_done = False

    def _on_idle(self, event):
        super(ShowDualForm, self)._on_idle(event)
        if not self._initialization_done:
            self._initialization_done = True
            self._select_form(self._main_form, force=True)
        
    def _create_main_form(self, parent, **kwargs):
        return BrowsableShowForm(parent, self._resolver, self._main_name, guardian=self, **kwargs)

    def _set_main_form_callbacks(self):
        self._main_form.set_callback(BrowsableShowForm.CALL_SELECTION, self._on_main_selection)


class BrowseShowDualForm(ImmediateSelectionDualForm):
    """Duální formulář s řádkovým seznamem nahoře a náhledem dole.

    Tento formulář slouží k současnému zobrazení přehledu položek a formuláře s rozšiřujícími
    informacemi.  Podle specifikace vazby a dolního formuláře může jít jak o detaily k aktuálnímu
    záznamu, tak o souhrnné informace (např. výsledky agregací nad daty horního formuláře atd.).

    """
    DESCR = _("duální náhled")
    
    def _create_main_form(self, parent, **kwargs):
        return BrowseForm(parent, self._resolver, self._name, guardian=self, **kwargs)

    def _set_main_form_callbacks(self):
        f = self._main_form
        f.set_callback(ListForm.CALL_USER_INTERACTION, lambda : self._select_form(self._main_form))
        f.set_callback(ListForm.CALL_SELECTION, self._on_main_selection)

    def _create_side_form(self, parent):
        return ShowForm(parent, self._resolver, self._side_name)

    def _do_selection(self, row):
        if self._side_form is not None:
            view = self._view
            bcol, sbcol = (view.binding_column(), view.side_binding_column())
            assert bcol is not None
            self._side_form.select_row({sbcol: row[bcol]})
            self._side_form.Show(True)
            self._select_form(self._main_form, force=True)
        return True

        
class DescriptiveDualForm(BrowseShowDualForm):
    """Duální formulář s řádkovým seznamem nahoře a náhledem dole.

    Tento formulář slouží k současnému zobrazení přehledu položek a podrobnému
    zobrazení aktuální položky.  Náhled není určen k editaci této položky.  Jde
    vlastně o speciální případ formuláře rodičovské řídy, kdy náhled v dolním
    formuláři je dán stejnou specifikací, jako horní formulář.

    """
    class _SideForm(ShowForm):
        def _filter_menu(self):
            return None
    
    def _init_attributes(self, orientation=Orientation.HORIZONTAL, **kwargs):
        self._in_mainform_selection = False
        self._orientation = orientation
        super(DescriptiveDualForm, self)._init_attributes(**kwargs)
        
    def _initial_orientation(self):
        return self._orientation
        
    def _create_view_spec(self):
        return None

    def _create_side_form(self, parent):
        return self._SideForm(parent, self._resolver, self._name)

    def _set_side_form_callbacks(self):
        self._side_form.set_callback(ShowForm.CALL_SELECTION, self._on_side_selection)
    
    def _do_selection(self, row):
        if self._side_form is not None:
            self._in_mainform_selection = True
            self._side_form.select_row(row.key())
            self._side_form.Show(True)
            self._select_form(self._main_form, force=True)
            self._in_mainform_selection = False
        return True

    def _on_side_selection(self, row):
        if self._main_form is not None and not self._in_mainform_selection:
            self._main_form.select_row(row.row())

    def title(self):
        return self._main_form.title()

            
class MultiForm(Form, Refreshable):
    """Form container showing multiple inner forms in separate notebook tabs.

    The form has no data itself -- it just acts as a proxy to the currently visible inner form.

    """
    def __init__(self, *args, **kwargs):
        super(MultiForm, self).__init__(*args, **kwargs)
        wx_callback(wx.EVT_SET_FOCUS, self, lambda e: self.focus())
        wx_callback(wx.EVT_SIZE, self, self._on_size)

    def _create_view_spec(self):
        return None
    
    def _create_data_object(self):
        return None
    
    def _create_forms(self, parent):
        pass
        
    def _create_form(self):
        self._notebook = nb = wx.Notebook(self)
        self._forms = forms = self._create_forms(nb)
        for form in forms:
            nb.AddPage(form, form.title())
        self._last_selection = None
        wx_callback(wx.EVT_NOTEBOOK_PAGE_CHANGED, nb, nb.GetId(), self._on_page_changed)
    
    def _on_page_changed(self, event):
        selected_form_index = event.GetSelection()
        if selected_form_index != -1:
            form = self._forms[selected_form_index]
            row = self._last_selection
            if row is not None:
                form.on_selection(row)

    #def _cmd_change_form(self, forward=True):
    #    self._notebook.AdvanceSelection(forward=forward)

    def _exit_check(self):
        for form in self._forms:
            if not form._exit_check():
                return False
        return True
            
    def _cleanup(self):
        nb = self._notebook
        nb.Show(False)
        for form in self._forms:
            form.Reparent(self)
            nb.RemovePage(0)
            form.close(force=True)
        self._forms = None
        nb.Close()
        nb.Destroy()
        
    def _on_size(self, event):
        size = event.GetSize()
        self._notebook.SetSize(size)
        
    def show(self):
        # Call sub-form show/hide methods, since they may contain initialization/cleanup actions.
        for form in self._forms:
            form.show()
        self._notebook.Enable(True)
        self._notebook.Show(True)

    def hide(self):
        for form in self._forms:
            form.hide()
        self._notebook.Show(False)
        self._notebook.Enable(False)

    def set_callback(self, kind, function):
        for form in self._forms:
            form.set_callback(kind, function)

    def active_form(self):
        """Return the currently active form of this form group."""
        selection = self._notebook.GetSelection()
        if selection != -1:
            form = self._forms[selection]
        else:
            form = None
        return form

    def on_selection(self, row):
        self._last_selection = row
        active = self.active_form()
        if active:
            active.on_selection(row)

    def save(self):
        active = self.active_form()
        if active:
            active.save()

    def restore(self):        
        active = self.active_form()
        if active:
            active.restore()

    def focus(self):
        active = self.active_form()
        if active:
            active.focus()

    def _refresh(self, when=None):
        active = self.active_form()
        if active and isinstance(active, Refreshable):
            active.refresh()
            

class MultiSideBrowseForm(MultiForm):
        
    class SubForm(SideBrowseForm):
        _ALLOW_TITLE_BAR = False
        def _init_attributes(self, binding, **kwargs):
            # Hack: Transform 'Binding' to 'BindingSpec'.
            sbcol = binding.colname()
            bcol = self._data.find_column(sbcol).type().enumerator().value_column()
            bs = BindingSpec(binding_column=bcol, side_binding_column=sbcol,
                             condition=binding.condition())
            self._title = binding.title()
            super(MultiSideBrowseForm.SubForm, self)._init_attributes(binding=bs, **kwargs)
        def title(self):
            return self._title
            
    def _init_attributes(self, main_form, **kwargs):
        assert isinstance(main_form, Form), main_form
        self._main_form = main_form
        super(MultiSideBrowseForm, self)._init_attributes(**kwargs)
        
    def _create_forms(self, parent):
        bindings = self._resolver.get(self._main_form.name(), 'binding_spec')
        return [self.SubForm(parent, self._resolver, binding.name(), guardian=self,
                             main_form=self._main_form, binding=binding)
                for binding in bindings]
    
            
class MultiBrowseDualForm(BrowseDualForm):
    """Dual form with a 'BrowseForm' up and multiple side browse forms."""
    DESCR = _("vícenásobný duální formulář")
    
    def _create_view_spec(self):
        return None

    def _create_data_object(self):
        return None
    
    def _initial_orientation(self):
        return Orientation.HORIZONTAL
        
    def _initial_sash_position(self, mode, size):
        return size.height / 2 

        
    def _create_main_form(self, parent, **kwargs):
        return BrowseForm(parent, self._resolver, self._name, guardian=self, **kwargs)
    
    def _create_side_form(self, parent, **kwargs):
        return MultiSideBrowseForm(parent, self._resolver, self._name, guardian=self,
                                   main_form=self._main_form, **kwargs)

    def title(self):
        return self._main_form.title()
    
