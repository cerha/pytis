# -*- coding: utf-8 -*-

# Copyright (C) 2001-2014, 2016 Brailcom, o.p.s.
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

import copy
import re
import wx

import pytis.data
from pytis.presentation import Orientation
from pytis.util import EVENT, log, translations, ProgramError
from dialog import MultiQuestion
from event import wx_callback
from form import BrowsableShowForm, EditForm, Form, Refreshable, ShowForm, WebForm
from list import AggregationForm, BrowseForm, ListForm, SideBrowseForm
from screen import CheckItem, Menu, MItem, \
    busy_cursor, is_busy_cursor, microsleep, popup_menu, wx_focused_window
from application import current_form, has_access, message, run_dialog, run_form

_ = translations('pytis-wx')

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
    DESCR = _("dual form")

    def _full_init(self, *args, **kwargs):
        """Inicializuj duální formulář.

        Argumenty jsou stejné jako v předkovi, specifikují však hlavní formulář
        duálního formuláře.

        """
        super(DualForm, self)._full_init(*args, **kwargs)
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
        self._splitter_position_initialized = False

    def _initial_orientation(self):
        saved_orientation = self._get_saved_setting('sash_orientation')
        if saved_orientation is not None:
            return saved_orientation
        else:
            return self._default_orientation()

    def _default_orientation(self):
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
        orientation = self._initial_orientation()
        if orientation == Orientation.HORIZONTAL:
            splitter.SplitHorizontally(self._main_form, self._side_form)
        elif orientation == Orientation.VERTICAL:
            splitter.SplitVertically(self._main_form, self._side_form)
        else:
            raise ProgramError("Invalid dual form orientation: %r" % orientation)
        if isinstance(self._main_form, EditForm):
            gravity = 0
        elif isinstance(self._side_form, EditForm):
            gravity = 1
        elif orientation == Orientation.HORIZONTAL:
            gravity = 0.5
        else:
            gravity = 0
        splitter.SetSashGravity(gravity)
        splitter.SetMinimumPaneSize(80)
        # Setting a minimal size here is a hack to avoid wx/gtk hanging when
        # the splitter size becomes too small.  It is unknown what actually
        # causes the hang, but 180x180 seems to be the lowest minimal size
        # which avoids it.
        splitter.SetMinSize((180, 180))
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
            log(EVENT, "Main form doesn't support `select_row()'!")

    def apply_profile(self, *args, **kwargs):
        if hasattr(self._main_form, 'apply_profile'):
            return self._main_form.apply_profile(*args, **kwargs)
        else:
            log(EVENT, "Main form doesn't support `apply_profile()'!")

    def _cmd_other_form(self):
        self._select_form(self._other_form(self._active_form))

    def _cmd_resplit(self):
        is_vertical = self.is_vertical()
        self._splitter.Unsplit()
        if is_vertical:
            self._splitter.SplitHorizontally(self._main_form, self._side_form)
            new_orientation = Orientation.HORIZONTAL
        else:
            self._splitter.SplitVertically(self._main_form, self._side_form)
            new_orientation = Orientation.VERTICAL
        self._set_saved_setting('sash_orientation', new_orientation)
        self._select_form(self._active_form)

    def main_form(self):
        """Return the instance of the upper (main) form."""
        return self._main_form

    def side_form(self):
        """Return the instance of the lower (side) form."""
        return self._side_form

    def active_form(self):
        """Vrať aktivní formulář tohoto duálního formuláře."""
        return self._active_form

    def inactive_form(self):
        """Vrať neaktivní formulář tohoto duálního formuláře."""
        return self._other_form(self._active_form)

    def is_vertical(self):
        """Return True if the dual form is split vertically, False if horizontally."""
        return self._splitter.GetSplitMode() == 2

    def show(self):
        # Musíme volat show obou podformulářů, protože splitter je nevolá a
        # přitom v nich mohou být inicializační či ukončovací akce.
        self._splitter.Enable(True)
        self._splitter.Show(True)
        self._side_form.show()
        self._main_form.show()

    def hide(self):
        orig_hide_form_requested = self._hide_form_requested
        self._hide_form_requested = True
        try:
            self._side_form.hide()
            self._main_form.hide()
            self._splitter.Show(False)
            self._splitter.Enable(False)
        finally:
            self._hide_form_requested = orig_hide_form_requested

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
            self._side_form.close(force=True)
        except:
            pass
        self._side_form = None
        self._active_form = None
        try:
            self._main_form.close(force=True)
        except:
            pass
        self._main_form = None
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
        saved_position = self._get_saved_setting('sash_position')
        if saved_position:
            if self._splitter.GetSplitMode() == wx.SPLIT_HORIZONTAL:
                maximum = total_size.height
            else:
                maximum = total_size.width
            return min(saved_position, maximum)
        else:
            return self._default_sash_position(total_size)

    def _default_sash_ratio(self):
        return 0.5

    def _default_sash_position(self, total_size):
        def dimension(size):
            if self._splitter.GetSplitMode() == wx.SPLIT_HORIZONTAL:
                return size.height
            else:
                return size.width
        if isinstance(self._main_form, EditForm):
            return min(dimension(wx.Size(*self._main_form.size())), dimension(total_size) - 200)
        elif isinstance(self._side_form, EditForm):
            return max(dimension(total_size) - dimension(wx.Size(*self._side_form.size())), 200)
        else:
            r = self._default_sash_ratio()
            return dimension(wx.Size(total_size.width * r, total_size.height * r))

    def _on_sash_changed(self, event):
        self._set_saved_setting('sash_position', event.GetSashPosition())
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

    def _refresh(self, when=None, interactive=False):
        if isinstance(self._main_form, Refreshable):
            self._main_form.refresh(interactive=interactive)
        if isinstance(self._side_form, Refreshable):
            self._side_form.refresh(interactive=interactive)

    def close(self, force=False):
        # Prevent certain actions to happen in the side form when the form is
        # being closed.
        self._side_form._leave_form_requested = True
        return super(DualForm, self).close(force=force)

    def _print_form_kwargs(self):
        return dict(form_bindings=self._main_form.bindings())


class ImmediateSelectionDualForm(DualForm):
    """Duální formulář s okamžitou obnovou vedlejšího formuláře."""

    def _full_init(self, *args, **kwargs):
        self._selection_data = None
        super(ImmediateSelectionDualForm, self)._full_init(*args, **kwargs)

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

    def _full_init(self, *args, **kwargs):
        super(PostponedSelectionDualForm, self)._full_init(*args, **kwargs)
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
        f.set_callback(f.CALL_USER_INTERACTION, lambda: self._select_form(f))

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
        return BrowseForm(parent, self._resolver, self._main_name, guardian=self, **kwargs)

    def _set_main_form_callbacks(self):
        f = self._main_form
        f.set_callback(f.CALL_USER_INTERACTION, lambda: self._select_form(f))
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
        def _create_query_fields_panel(self, sizer):
            self._query_fields_form = None
            self._query_fields_apply_button = None
            self._unapplied_query_field_changes = False
        def _current_arguments(self):
            return self._main_form.side_form_arguments()

    def _create_view_spec(self):
        return None

    def _default_orientation(self):
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
        f.set_callback(f.CALL_USER_INTERACTION, lambda: self._select_form(f))
        f.set_callback(f.CALL_SELECTION, self._on_main_selection)

    def _set_side_form_callbacks(self):
        f = self._side_form
        f.set_callback(f.CALL_MODIFICATION, self._main_form.refresh)
        f.set_callback(f.CALL_USER_INTERACTION, lambda: self._select_form(f))

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
    def _full_init(self, *args, **kwargs):
        super(ShowDualForm, self)._full_init(*args, **kwargs)
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
    DESCR = _("dual view")

    def _create_main_form(self, parent, **kwargs):
        return BrowseForm(parent, self._resolver, self._name, guardian=self, **kwargs)

    def _set_main_form_callbacks(self):
        f = self._main_form
        f.set_callback(f.CALL_USER_INTERACTION, lambda: self._select_form(f))
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
        def __init__(self, *args, **kwargs):
            self._main_form = kwargs.pop('main_form', None)
            super(DescriptiveDualForm._SideForm, self).__init__(*args, **kwargs)
        def _filter_menu(self):
            return None
        def _apply_profile(self, profile, **kwargs):
            # It makes little sense to apply profiles in this form.
            # What is more important: Disabling this method fixes the problem
            # of choosing wrong row in the side form when
            # _apply_initial_profile gets called in _on_idle after a row has
            # been selected in the main form but still before the other row
            # gets replaced in the side form.
            pass
        def _query_fields_row(self):
            return self._main_form._query_fields_row()

    def _init_attributes(self, orientation=Orientation.HORIZONTAL, **kwargs):
        self._in_mainform_selection = False
        self._orientation = orientation
        super(DescriptiveDualForm, self)._init_attributes(**kwargs)

    def _default_orientation(self):
        return self._orientation

    def _create_view_spec(self):
        return None

    def _create_side_form(self, parent):
        return self._SideForm(parent, self._resolver, self._name, main_form=self._main_form)

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

    def _full_init(self, *args, **kwargs):
        self._block_on_page_change = False
        self._form_callbacks_args = []
        self._old_notebook_selection = -1
        self._moved_notebook_tab = None
        super(MultiForm, self)._full_init(*args, **kwargs)

    def _create_view_spec(self):
        return None

    def _create_data_object(self):
        return None

    def _create_forms(self, parent):
        # To be overridden in derived classes.
        pass

    def _create_form(self):
        import wx.aui
        self._notebook = nb = wx.aui.AuiNotebook(self, style=(wx.aui.AUI_NB_TOP |
                                                              wx.aui.AUI_NB_TAB_MOVE |
                                                              wx.aui.AUI_NB_SCROLL_BUTTONS |
                                                              wx.aui.AUI_NB_WINDOWLIST_BUTTON))
        self._forms = forms = []
        for title, form in self._create_forms(nb):
            forms.append(form)
            if form is None:
                form = wx.Panel(nb)
            nb.AddPage(form, title)
        # Select the first available form.
        self._last_selection = None
        for i, form in enumerate(forms):
            if form:
                self._set_notebook_selection(i)
                break
        # Keep a list of tab indexes corresponding to their visual order in the
        # notebook tab list.  This order is updated on each tab move.  This
        # gives us information about the order of tabs, because the indexes
        # used by wx.aui.AuiNotebook don't change when the tabs are moved
        # around by dragging.  The list includes notebook page indexes (which
        # are also self._forms indexes) in the order of visual appearance of
        # the corresponding notebook tabs.
        self._tab_order = range(len(forms))
        tabctrl = [child for child in nb.GetChildren() if isinstance(child, wx.aui.AuiTabCtrl)][0]
        wx_callback(wx.EVT_LEFT_DOWN, tabctrl, self._on_mouse_left)
        wx_callback(wx.aui.EVT_AUINOTEBOOK_PAGE_CHANGING, nb, nb.GetId(), self._on_page_change)
        wx_callback(wx.aui.EVT_AUINOTEBOOK_BEGIN_DRAG, tabctrl, tabctrl.GetId(),
                    self._on_tab_move_started)
        wx_callback(wx.aui.EVT_AUINOTEBOOK_DRAG_DONE, nb, nb.GetId(), self._on_tab_move_done)
        try:
            tab_right_down = wx.aui.EVT_AUINOTEBOOK_TAB_RIGHT_DOWN
        except:
            tab_right_down = wx.aui.EVT__AUINOTEBOOK_TAB_RIGHT_DOWN
        wx_callback(tab_right_down, nb, nb.GetId(), self._on_tab_mouse_right)
        wx_callback(wx.EVT_RIGHT_DOWN, nb, self._on_notebook_mouse_right)
        wx_callback(wx.EVT_SET_FOCUS, self, lambda e: self.focus())
        wx_callback(wx.EVT_SIZE, self, self._on_size)

    def _on_tab_mouse_right(self, event):
        event.Skip()

    def _on_notebook_mouse_right(self, event):
        event.Skip()

    def _on_mouse_left(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        event.Skip()

    def _init_subform(self, form):
        if form.initialized():
            return
        busy = is_busy_cursor()
        if not busy:
            busy_cursor(True)
        try:
            if form.initialized():
                # This check is not redundant!  Even when the form is reported
                # as unitialized in the first call, it may be already
                # initialized here.  Don't ask me why, I suspect busy_cursor
                # may invoke some event that causes form initialization, but
                # who knows...
                return
            form.full_init()
            form._release_data()
            for kind, function in self._form_callbacks_args:
                if hasattr(form, kind):
                    form.set_callback(kind, function)
        finally:
            if not busy:
                busy_cursor(False)

    def _set_notebook_selection(self, i):
        self._old_notebook_selection = self._notebook.GetSelection()
        self._notebook.SetSelection(i)
        if self._old_notebook_selection != self._notebook.GetSelection():
            self._on_page_change()

    def _on_page_change(self, event=None):
        if self._leave_form_requested or self._hide_form_requested or not self.IsShown():
            # Prevent (possibly expensive) database queries on NotebookEvent
            # called when the form is being closed asynchronously from _on_idle
            # method.
            return
        if self._block_on_page_change:
            return
        if event:
            selection = event.GetSelection()
            old_selection = event.GetOldSelection()
        else:
            selection = self._notebook.GetSelection()
            old_selection = self._old_notebook_selection
        if selection != -1:
            form = self._forms[selection]
            if form:
                # The SetSelection call below is necessary here to make
                # WebForm work in sideforms, as they need the panel to be shown
                # before the webkit widget can be embedded into it.  And the
                # embedding is done within _init_subform(), so we must ensure
                # the page is changed before.
                self._block_on_page_change = True
                try:
                    self._notebook.SetSelection(selection)
                finally:
                    self._block_on_page_change = False
                self._init_subform(form)
                row = self._last_selection
                if row is not None:
                    form.on_selection(row)
                form.focus()
                if old_selection != -1:
                    old_form = self._forms[old_selection]
                    if old_form and old_form is not form and old_form.initialized():
                        old_form._release_data()
            elif event:
                message(_("Form disabled"), beep_=True)
                event.Veto()
                old_selection = event.GetOldSelection()
                if old_selection != -1:
                    form = self._forms[old_selection]
                    if form:
                        form._init_subform(form)
                        form.focus()
                        form.SetFocus()

    def _on_tab_move_started(self, event):
        self._moved_notebook_tab = event.GetSelection()
        event.Skip()

    def _on_tab_move_done(self, event):
        old_position = self._moved_notebook_tab
        if old_position is not None:
            new_position = event.GetSelection()
            self._moved_notebook_tab = None
            self._on_tab_move(old_position, new_position)
        event.Skip()

    def _on_tab_move(self, old_position, new_position):
        tab_order = self._tab_order
        index = tab_order[old_position]
        del tab_order[old_position]
        tab_order.insert(new_position, index)

    def _exit_check(self):
        for form in self._forms:
            if form and form.initialized() and not form._exit_check():
                return False
        return True

    def _cleanup(self):
        self._leave_form_requested = True
        try:
            nb = self._notebook
            nb.Show(False)
        except:
            pass
        for form in self._forms:
            if form and form.initialized():
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
        old_order = self._tab_order.index(self._notebook.GetSelection())
        new_order = (old_order + d) % len(self._tab_order)
        i = self._tab_order[new_order]
        self._init_subform(self._forms[i])
        self._set_notebook_selection(i)

    def forms(self):
        return self._forms

    def show(self):
        # Call sub-form show/hide methods, since they may contain initialization/cleanup actions.
        active = self.active_form()
        for form in self._forms:
            if form and form.initialized():
                form.show()
                form._release_data()
                if form is not active:
                    form.Show(False)
        self._notebook.Enable(True)
        self._notebook.Show(True)

    def hide(self):
        orig_hide_form_requested = self._hide_form_requested
        self._hide_form_requested = True
        try:
            for form in self._forms:
                if form and form.initialized():
                    form.hide()
                    form._release_data()
            self._notebook.Show(False)
            self._notebook.Enable(False)
        finally:
            self._hide_form_requested = orig_hide_form_requested

    def set_callback(self, kind, function):
        if kind != ListForm.CALL_MODIFICATION:
            super(MultiForm, self).set_callback(kind, function)
        self._form_callbacks_args.append((kind, function,))
        for form in self._forms:
            if form and hasattr(form, kind) and form.initialized():
                form.set_callback(kind, function)

    def active_form(self):
        """Return the currently active form of this form group."""
        selection = self._notebook.GetSelection()
        if selection != -1:
            form = self._forms[selection]
            self._init_subform(form)
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
            self._init_subform(active)
            self._set_notebook_selection(self._forms.index(active))
            active.restore()

    def focus(self):
        active = self.active_form()
        if active:
            active.focus()

    def defocus(self):
        active = self.active_form()
        if active:
            active.defocus()

    def _refresh(self, when=None, interactive=False):
        active = self.active_form()
        if active and isinstance(active, Refreshable):
            active.refresh(interactive=interactive)


class MultiSideForm(MultiForm):
    CALL_BINDING_SELECTED = 'CALL_BINDING_SELECTED'

    class TabbedForm(object):
        def __init__(self, *args, **kwargs):
            self._binding = kwargs['binding']
            super(MultiSideForm.TabbedForm, self).__init__(*args, **kwargs)
        def _init_attributes(self, binding, **kwargs):
            self._binding = binding
            super(MultiSideForm.TabbedForm, self)._init_attributes(**kwargs)
        def binding(self):
            return self._binding
        def focus(self):
            nb = self._parent
            if nb.GetPageIndex(self) == nb.GetSelection():
                # Only perform focus if the form is currently selected in the notebook.
                super(MultiSideForm.TabbedForm, self).focus()

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
                          prefill=binding.prefill(), search=binding.search())
            super(MultiSideForm.TabbedBrowseForm, self)._init_attributes(binding=binding, **kwargs)

    class TabbedShowForm(TabbedForm, ShowForm):
        def _init_attributes(self, binding, main_form, **kwargs):
            self._bcol = bcol = binding.binding_column()
            self._sbcol = main_form.data(init_select=False).find_column(bcol).type().\
                enumerator().value_column()
            super(MultiSideForm.TabbedShowForm, self)._init_attributes(binding=binding, **kwargs)
        def on_selection(self, row):
            self.select_row({self._sbcol: row[self._bcol]})

    class TabbedWebForm(TabbedForm, WebForm):
        def _init_attributes(self, binding, main_form, **kwargs):
            if binding.uri():
                get_uri = binding.uri()
                def load_content(row):
                    uri = get_uri(row)
                    restrict_navigation = re.sub(r'^(https?://[a-z0-9][a-z0-9\.-]*).*',
                                                 lambda m: m.group(1), uri)
                    self._browser.load_uri(uri, restrict_navigation=restrict_navigation)
            else:
                get_content = binding.content()
                def load_content(row):
                    self.load_content(get_content(row))
            self._load_content = load_content
            super(MultiSideForm.TabbedWebForm, self)._init_attributes(binding=binding, **kwargs)
        def on_selection(self, row):
            self._load_content(row)

    def _init_attributes(self, main_form, **kwargs):
        assert isinstance(main_form, Form), main_form
        self._main_form = main_form
        super(MultiSideForm, self)._init_attributes(**kwargs)

    def _create_subform(self, parent, binding):
        if binding.name() and not has_access(binding.name()):
            return None
        kwargs = dict(guardian=self, binding=binding, main_form=self._main_form)
        if binding.name() is None:
            form = self.TabbedWebForm
        elif binding.single():
            form = self.TabbedShowForm
        else:
            form = self.TabbedBrowseForm
        form_instance = form(parent, self._resolver, binding.name(), full_init=False, **kwargs)
        return form_instance

    def _create_forms(self, parent):
        bindings = [binding for binding in self._main_form.bindings()
                    # TODO: Remove this condition to include inactive tabs in multi form.
                    # The wx.Notebook doesn't support inactive tabs and the workaround doesn't
                    # work correctly, so we rather exclude disabled tabs here for now.
                    # binding.name() is None for web forms.
                    # UPDATE: inactive tabs might work now with AuiNotebook.
                    if binding.name() is None or has_access(binding.name())]
        saved_order = self._get_saved_setting('binding_order')
        if saved_order:
            bdict = dict([(b.id(), b) for b in bindings])
            bindings = [bdict[binding_id] for binding_id in saved_order if binding_id in bdict]
        return [(binding.title(), self._create_subform(parent, binding)) for binding in bindings]

    def _displayed_forms_menu(self):
        return Menu(_("Available forms"),
                    [CheckItem(b.title(), help='',
                               command=self.COMMAND_TOGGLE_SIDEFORM(binding=b,
                                                                    _command_handler=self),
                               state=lambda b=b: b.id() in [f.binding().id() for f in self._forms])
                     for b in sorted(self._main_form.bindings(), key=lambda b: b.title())
                     if b.name() is None or has_access(b.name())])

    def _on_tab_mouse_right(self, event):
        selection = event.GetSelection()
        menu = (MItem(_("Close this form"), help=_("Close this form"),
                      command=self.COMMAND_TOGGLE_SIDEFORM(binding=self._forms[selection].binding(),
                                                           _command_handler=self)),
                MItem(_("Filter the main form for non-empty side form"),
                      help=_("Show only those rows of the main form, which have "
                             "at least one row in this side form."),
                      command=self.COMMAND_FILTER_BY_SIDEFORM(index=selection,
                                                              _command_handler=self)),
                MItem(_("Filter the main form for empty side form"),
                      help=_("Show only those rows of the main form, which have "
                             "no rows in this side form."),
                      command=self.COMMAND_FILTER_BY_SIDEFORM(index=selection, not_in=True,
                                                              _command_handler=self)),
                self._displayed_forms_menu(),
                )
        popup_menu(self._notebook, menu, self._get_keymap())
        event.Skip()

    def _on_notebook_mouse_right(self, event):
        popup_menu(self._notebook, (self._displayed_forms_menu(),), self._get_keymap())
        event.Skip()

    def _on_page_change(self, event=None):
        if self._leave_form_requested or self._block_on_page_change:
            return
        super(MultiSideForm, self)._on_page_change(event=event)
        binding = self._forms[self._notebook.GetSelection()].binding()
        self._run_callback(self.CALL_BINDING_SELECTED, binding)

    def _save_binding_order(self):
        binding_order = [self._forms[i].binding().id() for i in self._tab_order]
        self._set_saved_setting('binding_order', tuple(binding_order))

    def _on_tab_move(self, old_position, new_position):
        super(MultiSideForm, self)._on_tab_move(old_position, new_position)
        self._save_binding_order()

    def _cmd_toggle_sideform(self, binding):
        nb = self._notebook
        try:
            index = [f.binding().id() for f in self._forms].index(binding.id())
        except ValueError:
            # Add form to the last position.  Note, that attempts to insert a
            # page to a given position using nb.InsertPage() failed as the page
            # indexes seemed to change unreliably.
            form = self._create_subform(nb, binding)
            form.full_init()
            nb.AddPage(form, binding.title(), select=True)
            self._forms = [nb.GetPage(i) for i in range(nb.GetPageCount())]
            self._tab_order.append(self._forms.index(form))
            form.show()
            row = self._last_selection
            if row is not None:
                form.on_selection(row)
            form.focus()
        else:
            # Remove form of given 'index'.
            form = self._forms[index]
            order = self._tab_order.index(index)
            if index != nb.GetPageIndex(form):
                return
            if index == self._notebook.GetSelection():
                self._cmd_next_form(back=(order != 0))
            if form.initialized():
                form.hide()
                form._release_data()
            del self._forms[index]
            del self._tab_order[order]
            for i, j in enumerate(self._tab_order):
                if j > index:
                    self._tab_order[i] -= 1
            nb.DeletePage(index)
        self._save_binding_order()

    def _can_filter_by_sideform(self, index, not_in=False):
        form = self._forms[index]
        return isinstance(form, SideBrowseForm) and form.side_form_in_condition() is not None

    def _cmd_filter_by_sideform(self, index, not_in=False):
        form = self._forms[index]
        if form.COMMAND_UPDATE_PROFILE.enabled():
            msg = _("Can't filter when the current profile is not saved!")
            bsave, bquit = _("Save"), _("Cancel")
            if run_dialog(MultiQuestion, msg, buttons=(bsave, bquit), default=bsave) != bsave:
                return
            form.COMMAND_UPDATE_PROFILE.invoke()
        condition = form.side_form_in_condition()
        if not_in:
            condition = pytis.data.NOT(condition)
        self._main_form.filter(condition, append=True)

    def select_binding(self, id):
        """Raise the side form tab corresponfing to the binding of given identifier.

        The argument 'id' is a string identifier of a 'Binding' instance which must appear in the
        main form 'bindings' specification.  If there is no binding of given id, an 'AssertError'
        is raised.  If the corresponding form is not active (e.g. the user has no access rights for
        the form), False is returned and an error message is displayed.  Otherwise the form is
        raised and 'True' is returned.

        """
        for i, form in enumerate(self._forms):
            if form and form.binding().id() == id:
                self._init_subform(form)
                self._set_notebook_selection(i)
                return True
        message(_("The requested side form is not available."), beep_=True)
        return False


class MultiBrowseDualForm(BrowseDualForm):
    """Dual form with a 'BrowseForm' up and multiple side forms.

    Specific constructor arguments:

      binding -- has the same effect as calling the method 'select_binding' with
        given argument after the form is created.

    """
    DESCR = _("tabbed dual form")
    class MainForm(BrowseForm):
        def bindings(self):
            return self._view.bindings()
        def orientation(self):
            return self._view.orientation()
        def _print_form_kwargs(self):
            return dict(form_bindings=self.bindings())
        def filter(self, *args, **kwargs):
            BrowseForm.filter(self, *args, **kwargs)
            row = self.current_row()
            if row is None:
                # How to clear the side form?
                pass
            else:
                self._guardian._do_selection(row)

    def _create_view_spec(self):
        return None

    def _create_data_object(self):
        return None

    def _default_orientation(self):
        return self._main_form.orientation()

    def _default_sash_position(self, total_size):
        return total_size.height / 2

    def _create_main_form(self, parent, binding=None, **kwargs):
        self._selected_binding = binding
        return self.MainForm(parent, self._resolver, self._name, guardian=self, **kwargs)

    def _create_side_form(self, parent):
        form = MultiSideForm(parent, self._resolver, self._name, guardian=self,
                             main_form=self._main_form)
        form.set_callback(MultiSideForm.CALL_BINDING_SELECTED, self._on_binding_selection)
        selected_binding = self._selected_binding
        if selected_binding is None:
            saved_binding = self._get_saved_setting('binding')
            if saved_binding in [b.id() for b in self._main_form.bindings()]:
                selected_binding = saved_binding
        if selected_binding:
            form.select_binding(selected_binding)
        return form

    def _on_binding_selection(self, binding):
        self._set_saved_setting('binding', binding.id())

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
