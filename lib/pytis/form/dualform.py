# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

class DualFormProfile(FormProfile):
    """Special profile for storing dual form specific parameters.
    
    Dual forms actually don't have any user visible profiles, that the user
    could switch etc.  They just store the internal state automatically
    using profile manager.
    
    """
    def __init__(self, id, sash_position):
        assert isinstance(sash_position, int)
        super(DualFormProfile, self).__init__(id, id)
        self._sash_position = sash_position

    def sash_position(self):
        return self._sash_position


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

    @classmethod
    def add_toolbar_ctrl(cls, toolbar, uicmd):
        cmd = uicmd.command()
        if cmd == DualForm.COMMAND_OTHER_FORM:
            args = uicmd.args()
            # This is a total hack to allow bitmap swithing on the COMMAND_OTHER_FORM toolbar
            # button.  SetBitmap1 doesn't seem to work on tools created using AddTool...
            cls._form_switcher_bitmaps = (get_icon('dual-form-active-up', type=wx.ART_TOOLBAR),
                                           get_icon('dual-form-active-down', type=wx.ART_TOOLBAR))
            button = wx_button(toolbar, 'x', icon='dual-form-active-up', size=(30, 30),
                               tooltip=uicmd.title(), noborder=True,
                               callback=lambda e: cmd.invoke(**args))
            cls._last_form_switch_icon = 0
            def update(event):
                enabled = cmd.enabled(**args)
                event.Enable(enabled)
                if enabled:
                    form = cls._get_command_handler_instance()
                    i = form._active_form != form._main_form and 1 or 0
                    if i != cls._last_form_switch_icon:
                        cls._last_form_switch_icon = i
                        button.SetBitmapLabel(cls._form_switcher_bitmaps[i])
                        toolbar.Realize()
            frame = toolbar.GetParent()
            wx_callback(wx.EVT_UPDATE_UI, frame, button.GetId(), update)
            tool = toolbar.AddControl(button)
            toolbar.SetToolLongHelp(tool.GetId(), uicmd.descr()) # Doesn't work...
        else:
            Form.add_toolbar_ctrl(toolbar, uicmd)
    
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
        form.focus()
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
        self._splitter.Enable(True)
        self._splitter.Show(True)
        self._side_form.show()
        self._main_form.show()

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
        try:
            self._main_form.close(force=True)
        except:
            pass
        try:
            self._side_form.close(force=True)
        except:
            pass
        self._side_form = None
        self._main_form = None
        self._side_form = None
        self._active_form = None
        try:
            self._splitter.Show(False)
            self._splitter.Close()
            self._splitter.Destroy()
        except:
            pass
        
    def focus(self):
        active = self._active_form
        if active:
            active.focus()

    def _initial_sash_position(self, total_size):
        def dimension(size):
            if self._splitter.GetSplitMode() == wx.SPLIT_HORIZONTAL:
                return size.height
            else:
                return size.width
        profile = profile_manager().load_profile(self._fullname(), '__dualform__')
        if profile and profile.sash_position():
            return min(profile.sash_position(), dimension(total_size))
        elif isinstance(self._main_form, EditForm):
            return min(dimension(self._main_form.size()), dimension(total_size) - 200)
        elif isinstance(self._side_form, EditForm):
            return max(dimension(total_size) - dimension(self._side_form.size()), 200)
        else:
            r = self._view.sash_ratio()
            return dimension(wx.Size(total_size.width * r, total_size.height * r))
            
    def _on_sash_changed(self, event):
        profile = DualFormProfile('__dualform__', sash_position=event.GetSashPosition())
        profile_manager().save_profile(self._fullname(), profile)
        # Sometimes the form is not redrawn correctly...
        self._main_form.Refresh()
        self._active_form.focus()
        event.Skip()

    def _on_size(self, event):
        size = event.GetSize()
        self._splitter.SetSize(size)
        if not self._splitter_position_initialized:
            self._splitter_position_initialized = True
            position = self._initial_sash_position(size)
            self._splitter.SetSashPosition(position)
        event.Skip()

    def _refresh(self, when=None):
        if isinstance(self._main_form, Refreshable):
            self._main_form.refresh()
        if isinstance(self._side_form, Refreshable):
            self._side_form.refresh()

    def close(self, force=False):
        # Prevent certain actions to happen in the side form when the form is
        # being closed.
        self._side_form._leave_form_requested = True
        return super(DualForm, self).close(force=force)

        
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

    def _on_idle(self, event):
        if super(PostponedSelectionDualForm, self)._on_idle(event):
            return True
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
                              main_form=self._main_form,
                              binding_column=self._view.binding_column(),
                              side_binding_column=self._view.side_binding_column(),
                              hide_binding_column=self._view.hide_binding_column(),
                              condition=self._view.condition(),
                              arguments=self._view.arguments())

    def _set_side_form_callbacks(self):
        f = self._side_form
        if isinstance(self._main_form, Refreshable):
            f.set_callback(ListForm.CALL_MODIFICATION, self._main_form.refresh)
        f.set_callback(f.CALL_USER_INTERACTION, lambda : self._select_form(f))

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
        try:
            self._side_form.set_callback(ListForm.CALL_MODIFICATION, None)
        except:
            pass
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
        f.set_callback(f.CALL_USER_INTERACTION, lambda : self._select_form(f))
        f.set_callback(f.CALL_SELECTION, self._on_main_selection)
        f.set_callback(f.CALL_ACTIVATION, self._on_main_activation)
    
    def _on_main_activation(self, alternate=False):
        if alternate:
            form, name = (DescriptiveDualForm, self._main_form.name())
        else:
            form, name = (ShowDualForm, self._name)
        run_form(form, name, select_row=self._main_form.current_key())

class AggregationDualForm(PostponedSelectionDualForm):
    """Dual form with 'AggregationForm' main form and 'BrowseForm' sideform.
    
    The side form shows all records corresponding to the currently selected
    aggregation form row.  This means records having the values of all "group
    by" columns same as the selected row.
    
    """
    class _SideForm(SideBrowseForm):
        def _default_columns(self):
            return tuple([c for c in super(AggregationDualForm._SideForm, self)._default_columns()
                          if c not in self._main_form.group_by_columns()])

    def _create_view_spec(self):
        return None
    
    def _initial_orientation(self):
        return Orientation.HORIZONTAL
    
    def _initial_sash_position(self, size):
        return size.height / 2 
        
    def _create_main_form(self, parent, **kwargs):
        return AggregationForm(parent, self._resolver, self._name, guardian=self, **kwargs)

    def _create_side_form(self, parent):
        return self._SideForm(parent, self._resolver, self._name, guardian=self,
                              main_form=self._main_form,
                              condition=self._main_form.side_form_condition)
    
    def _set_main_form_callbacks(self):
        f = self._main_form
        f.set_callback(f.CALL_USER_INTERACTION, lambda : self._select_form(f))
        f.set_callback(f.CALL_SELECTION, self._on_main_selection)
    
    def _set_side_form_callbacks(self):
        f = self._side_form
        f.set_callback(f.CALL_MODIFICATION, self._main_form.refresh)
        f.set_callback(f.CALL_USER_INTERACTION, lambda : self._select_form(f))

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
        try:
            self._side_form.set_callback(ListForm.CALL_MODIFICATION, None)
        except:
            pass
        super(AggregationDualForm, self)._cleanup()

    def title(self):
        return self._main_form.title()

        
class ShowDualForm(SideBrowseDualForm):
    """Duální formulář s hlavním formulářem 'BrowsableShowForm'.

    """
    def __init__(self, *args, **kwargs):
        super(ShowDualForm, self).__init__(*args, **kwargs)
        self._initialization_done = False

    def _on_idle(self, event):
        if super(ShowDualForm, self)._on_idle(event):
            return True
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
        f.set_callback(f.CALL_USER_INTERACTION, lambda : self._select_form(f))
        f.set_callback(f.CALL_SELECTION, self._on_main_selection)

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
    @classmethod
    def _get_command_handler_instance(cls):
        form = current_form(inner=False)
        if isinstance(form, DualForm):
            form = form.active_form()
        return form

    def _create_view_spec(self):
        return None
    
    def _create_data_object(self):
        return None
    
    def _create_forms(self, parent):
        # To be overridden in derived classes.
        pass
        
    def _create_form(self):
        self._notebook = nb = wx.Notebook(self)
        #nb.SetPadding((0,0))
        self._forms = forms = []
        for title, form in self._create_forms(nb):
            forms.append(form)
            if form is None:
                form = wx.Panel(nb)
            nb.AddPage(form, title)
        # Select the first available form.
        for i, form in enumerate(forms):
            if form:
                self._notebook.SetSelection(i)
                break
        self._last_selection = None
        wx_callback(wx.EVT_NOTEBOOK_PAGE_CHANGING, nb, nb.GetId(), self._on_page_change)
        wx_callback(wx.EVT_LEFT_DOWN, nb, self._on_mouse)
        wx_callback(wx.EVT_RIGHT_DOWN, nb, self._on_mouse)
        wx_callback(wx.EVT_SET_FOCUS, self, lambda e: self.focus())
        wx_callback(wx.EVT_SIZE, self, self._on_size)
        #self.set_callback(self.CALL_USER_INTERACTION, lambda : self._select_form(self))

    def _on_mouse(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        event.Skip()
    
    def _on_page_change(self, event=None):
        if self._leave_form_requested:
            # Prevent (possibly expensive) database queries on NotebookEvent
            # called when the form is being closed asynchronously from _on_idle
            # method.
            return
        if event:
            #event.Skip()
            selection = event.GetSelection()
        else:
            selection = self._notebook.GetSelection()
        if selection != -1:
            form = self._forms[selection]
            if form:
                row = self._last_selection
                if row is not None:
                    form.on_selection(row)
                form.focus()
                old_selection = event.GetOldSelection()
                if old_selection != -1:
                    old_form = self._forms[old_selection]
                    if old_form and old_form is not form:
                        old_form._release_data()
            elif event:
                message(_("Formulář není dostupný"), beep_=True)
                event.Veto()
                old_selection = event.GetOldSelection()
                if old_selection != -1:
                    form = self._forms[old_selection]
                    if form:
                        form.focus()
                        form.SetFocus()

    def _exit_check(self):
        for form in self._forms:
            if form and not form._exit_check():
                return False
        return True
            
    def _cleanup(self):
        try:
            nb = self._notebook
            nb.Show(False)
        except:
            pass
        for form in self._forms:
            if form:
                form.Reparent(self)
                try:
                    nb.RemovePage(0)
                except:
                    pass
                form.close(force=True)
            else:
                try:
                    nb.DeletePage(0)
                except:
                    pass
        self._forms = None
        try:
            nb.Close()
            nb.Destroy()
        except:
            pass
        super(MultiForm, self)._cleanup()
        
    def _on_size(self, event):
        size = event.GetSize()
        self._notebook.SetSize(size)

    def _cmd_next_form(self, back=False):
        d = back and -1 or 1
        i = self._notebook.GetSelection() + d
        while i >= 0 and i < len(self._forms):
            form = self._forms[i]
            if form:
                self._notebook.SetSelection(i)
                self._on_page_change()
                return
            i += d
        msg = back and _("Žádný předchozí aktivní formulář") or _("Žádný další aktivní formulář")
        message(msg, beep_=True)
        
    def show(self):
        # Call sub-form show/hide methods, since they may contain initialization/cleanup actions.
        for form in self._forms:
            if form:
                form.show()
                form._release_data()
        self._notebook.Enable(True)
        self._notebook.Show(True)

    def hide(self):
        for form in self._forms:
            if form:
                form.hide()
                form._release_data()
        self._notebook.Show(False)
        self._notebook.Enable(False)

    def set_callback(self, kind, function):
        if kind != ListForm.CALL_MODIFICATION:
            super(MultiForm, self).set_callback(kind, function)
        for form in self._forms:
            if form and hasattr(form, kind):
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
        self._saved_active_form = active = self.active_form()
        if active:
            active.save()

    def restore(self):        
        active = self._saved_active_form
        if active:
            self._notebook.SetSelection(self._forms.index(active))
            active.restore()

    def focus(self):
        active = self.active_form()
        if active:
            active.focus()

    def defocus(self):
        active = self.active_form()
        if active:
            active.defocus()

    def _refresh(self, when=None):
        active = self.active_form()
        if active and isinstance(active, Refreshable):
            active.refresh()
            

class MultiSideForm(MultiForm):
        
    class TabbedForm(object):
        def _init_attributes(self, binding, **kwargs):
            self._binding = binding
            super(MultiSideForm.TabbedForm, self)._init_attributes(**kwargs)
        def binding(self):
            return self._binding
        
    class TabbedBrowseForm(TabbedForm, SideBrowseForm):
        _ALLOW_TITLE_BAR = False
        def _init_attributes(self, binding, **kwargs):
            sbcol = binding.binding_column()
            if sbcol:
                bcol = self._data.find_column(sbcol).type().enumerator().value_column()
            else:
                bcol = None
            kwargs = dict(kwargs, binding_column=bcol, side_binding_column=sbcol,
                          condition=binding.condition(), arguments=binding.arguments(),
                          prefill=binding.prefill())
            super(MultiSideForm.TabbedBrowseForm, self)._init_attributes(binding=binding, **kwargs)
    class TabbedShowForm(TabbedForm, ShowForm):
        def _init_attributes(self, binding, main_form, **kwargs):
            self._bcol = bcol = binding.binding_column()
            self._sbcol = main_form.data().find_column(bcol).type().enumerator().value_column()
            super(MultiSideForm.TabbedShowForm, self)._init_attributes(binding=binding, **kwargs)
        def on_selection(self, row):
            self.select_row({self._sbcol: row[self._bcol]})
            
    def _init_attributes(self, main_form, **kwargs):
        assert isinstance(main_form, Form), main_form
        self._main_form = main_form
        super(MultiSideForm, self)._init_attributes(**kwargs)

    def _create_subform(self, parent, binding):
        if not has_access(binding.name()):
            return None
        kwargs = dict(guardian=self, binding=binding, main_form=self._main_form)
        if binding.single():
            form = self.TabbedShowForm
        else:
            form = self.TabbedBrowseForm
        form_instance = form(parent, self._resolver, binding.name(), **kwargs)
        form_instance._release_data()
        return form_instance

    def _create_forms(self, parent):
        return [(binding.title(), self._create_subform(parent, binding))
                for binding in self._main_form.bindings()
                # TODO: Remove this condition to include inactive tabs in multi form.
                # The wx.Notebook doesn't support inactive tabs and the workaround doesn't
                # work correctly, so we rather exclude disabled tabs here for now.
                if has_access(binding.name())]
    
    def select_binding(self, id):
        """Raise the side form tab corresponfing to the binding of given identifier.

        The argument 'id' is a string identifier of a 'Binding' instance which must appear in the
        main form 'bindings' specification.  If there is no binding of given id, an 'AssertError'
        is raised.  If the corresponding form is not active (e.g. the user has no access rights for
        the form), False is returned and an error message is displayed.  Otherwise the form is
        raised and 'True' is returned.
        
        """
        assert id in [b.id() for b in self._main_form.bindings()]
        for i, form in enumerate(self._forms):
            if form and form.binding().id() == id:
                self._notebook.SetSelection(i)
                return True
        message(_("Požadovaný vedlejší formulář není dostupný"), beep_=True)
        return False
    

class MultiBrowseDualForm(BrowseDualForm):
    """Dual form with a 'BrowseForm' up and multiple side forms.

    Specific constructor arguments:
    
      binding -- has the same effect as calling the method 'select_binding' with
        given argument after the form is created.

    """
    DESCR = _("vícenásobný duální formulář")
    class MainForm(BrowseForm):
        def bindings(self):
            return self._view.bindings()
    
    def _create_view_spec(self):
        return None

    def _create_data_object(self):
        return None
    
    def _initial_orientation(self):
        return Orientation.HORIZONTAL
        
    def _initial_sash_position(self, size):
        return size.height / 2 

    def _create_main_form(self, parent, binding=None, **kwargs):
        self._selected_binding = binding
        return self.MainForm(parent, self._resolver, self._name, guardian=self, **kwargs)
    
    def _create_side_form(self, parent):
        form = MultiSideForm(parent, self._resolver, self._name, guardian=self,
                             main_form=self._main_form)
        if self._selected_binding:
            form.select_binding(self._selected_binding)
        return form
        
    def _on_main_activation(self, alternate=False):
        if alternate:
            form, name = (DescriptiveDualForm, self._name)
        else:
            form, name = (BrowsableShowForm, self._name)
        run_form(form, name, select_row=self._main_form.current_key())
        
    def title(self):
        return self._main_form.title()
    
    def select_binding(self, id):
        return self._side_form.select_binding(id)

    def filter(self, condition):
        return self._main_form.filter(condition)
        
