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

"""Du�ln� formul��e.

Du�ln� formul��e rozd�luj� okno na dv� ��sti, z nich� ka�d� obsahuje jeden
jednoduch� formul��, p�i�em� data horn�ho a doln�ho formul��e jsou n�jak�m
zp�sobem z�visl�.  Bl�e viz dokumentace jednotliv�ch t��d.

"""

import pytis.data
import pytis.output
from pytis.form import *
import wx


class DualForm(Form):
    """Formul�� slo�en� ze dvou spolupracuj�c�ch formul���.

    Du�ln� formul�� je rozd�len na dv� ��sti um�st�n� nad sebou.  V�horn� ��sti
    je hlavn� (��d�c�) formul��, v�doln� ��sti je vedlej�� (pod��zen�)
    formul��.  Krom� vz�jemn� spolupr�ce jsou formul��e nez�visl�, maj� vlastn�
    aktiva�n� kategorii a sadu tla��tek.  Mezi formul��i lze libovoln�
    p�ech�zet.

    Du�ln� formul�� funguje jako celek, nerealizuje obecn� d�len� okna aplikace
    dv� samostatn� ��sti.  Pokud n�kter� podformul��� du�ln�ho formul��e vyvol�
    nov� formul��, kter� m� b�t vlo�en do okna aplikace,� je tento nov�
    formul�� vlo�en do stejn�ho okna, ve kter�m je ulo�en du�ln� formul��.

    Hlavn� a vedlej�� formul��e o�sv� p��tomnosti v�du�ln�m formul��i ani o�sv�
    spolupr�ci nev�.  Du�ln� formul�� je z�jejich pohledu zcela transparentn�.

    Tato t��da je z�kladem v�ech konfigurac� dvojice formul���.  P�edpokl�d� se
    vyu�it� n�sleduj�c�ch konfigurac�:

    - Hlavn� formul�� je 'BrowseForm', vedlej�� formul�� je 'EditForm'.
      P��kladem t�to konfigurace je listov�n� seznamem polo�ek (hlavn�
      formul��) s�editac� aktu�ln� vybran� polo�ky (vedlej�� formul��).

    - Hlavn� formul�� je 'EditForm', vedlej�� formul�� je 'BrowseForm'.
      P��kladem t�to konfigurace je editace faktury, kde celkov� �daje
      o�faktu�e jsou v�hlavn�m formul��i, zat�mco seznam fakturovan�ch polo�ek
      je ve vedlej��m formul��i.

    - Oba formul��e jsou typu 'BrowseForm'.  P��kladem t�to konfigurace je
      editace seznamu faktur (v�hlavn�m formul��i) kombinovan� s�editac�
      polo�ek (ve vedlej��m formul��i) aktu�ln� vybran� faktury.

    Tyto konkr�tn� konfigurace jsou realizov�ny potomky t�to t��dy.

    """
    _DESCR = _("du�ln� formul��")
    
    def get_command_handler_instance(cls, application):
        return application.top_window()
    get_command_handler_instance = classmethod(get_command_handler_instance)
    
    def __init__(self, *args, **kwargs):
        """Inicializuj du�ln� formul��.

        Argumenty jsou stejn� jako v�p�edkovi, specifikuj� v�ak hlavn� formul��
        du�ln�ho formul��e.
        
        """
        super_(DualForm).__init__(self, *args, **kwargs)
        wx_callback(wx.EVT_SET_FOCUS, self, self._on_focus)
        wx_callback(wx.EVT_SIZE, self, self._on_size)

    def _init_attributes(self, **kwargs):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.

        V�echny kl��ov� argumenty jsou posl�ze p�ed�ny konstruktoru hlavn�ho
        formul��e.
        
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
        # Hlavn� i vedlej�� formul�� maj� sv�j datov� objekt.
        return None

    def _create_form(self):
        # Vytvo� rozd�len� okno
        self._splitter = splitter = wx.SplitterWindow(self._parent, -1)
        wx_callback(wx.EVT_SPLITTER_DOUBLECLICKED, splitter,
                    splitter.GetId(), lambda e: True)
        wx_callback(wx.EVT_SPLITTER_SASH_POS_CHANGED, splitter,
                    splitter.GetId(), self._on_sash_changed)
        # Vytvo� formul��e
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
        """Vra� n�zev formul��e jako �et�zec."""
        return self._main_form.title()

    def select_row(self, *args, **kwargs):
        if hasattr(self._main_form, 'select_row'):
            self._main_form.select_row(*args, **kwargs)
        else:
            log(EVENT, "Hlavn� formul�� nepodporuje metodu `select_row()'!")
        
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
            # TODO: To je odporn� hack!!!
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
        # Mus�me volat show obou podformul���, proto�e splitter je nevol� a
        # p�itom v�nich mohou b�t inicializa�n� �i ukon�ovac� akce.
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
    """Du�ln� formul�� s okam�itou obnovou vedlej��ho formul��e."""
    
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
    """Du�ln� formul�� se zpo�d�nou obnovou vedlej��ho formul��e."""
    
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
    """Du�ln� formul�� s�vedlej��m formul��em 'SideBrowseForm'."""
        
    def title(self):
        """Vra� n�zev formul��e jako �et�zec."""
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
            # O�et�ovat to speci�ln�m zp�sobem mus�me proto, �e je t�eba
            # za v�ech okolnost� zabr�nit odsko�en� z�widgetu inkrement�ln�ho
            # vyhled�v�n�.  Ten zp�sob je trochu hloup�, proto�e vedlej��
            # formul�� se nezobraz�, dokud nen� aplikace op�t zaost�ena, ale
            # zn� n�kdo lep�� �e�en�?
            return False
        try:
            v, e = self._sbcol_type.validate(row[self._binding_column].export())
            f = self._side_form
            f.set_prefill({self._side_binding_column: v})
            f.filter(data=row.row())
            f.Show(True)
            # Tento _select_form zde byl nezn�mo pro�.  Proto�e se tak necht2n2
            # p�esune focus na horn� formul�� nap�. po editaci doln�ho
            # formul��e, bylo nutn� ��dek zakomentovat.  Pokud to m�lo n�jak�
            # v�znam, bude t�eba naj�t jin� �e�en�, respektuj�c� oba probl�my.
            # Pokud se po n�jakou dobu na ��dn� probl�m nep�ijde, je mo�n� to
            # smazat v�etn� tohoto koment��e...  TC, 22.8.2005
            #self._select_form(self._main_form, force=True)
        finally:
            if focused:
                focused.SetFocus()
        return True

    def close(self):
        self._side_form.set_callback(ListForm.CALL_MODIFICATION, None)
        super_(SideBrowseDualForm).close(self)


class BrowseDualForm(SideBrowseDualForm, Refreshable):
    """Du�ln� formul�� s�hlavn�m formul��em 'BrowseForm'.
    
    Hlavn�m formul��em je instance t��dy 'BrowseForm', vedlej��m formul��em je
    instance t��dy 'SideBrowseForm'.  Formul��e jsou vz�jemn� propojeny
    prost�ednictv�m vazebn�ch sloupc� dan�ch specifikac� `DualSpec'.
    
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
        # Refresh sideformu by zde teoreticky b�t nemusel.  Ten by m�l b�t
        # proveden automaticky po refreshi mainformu.  N�kdy k tomu v�ak z
        # nezn�m�ch d�vod� nedojde, tak�e jej zde pro jistotu p�id�me
        # natvrdo... :-(  Probl�m je pravd�podobn� n�kde ve zpracov�n� idle
        # event� ve wx.  Projevuje se to dokonce i tak, �e to nap�. v jednom
        # formul��i funguje a v jin�m ne, nebo dokonce stejn� formul�� na
        # jednom po��ta�i funguje a na jin�m ne...
        self._side_form.refresh()

        
class ShowDualForm(SideBrowseDualForm, Refreshable):
    """Du�ln� formul�� s�hlavn�m formul��em 'BrowsableShowForm'.

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
    """Du�ln� formul�� s���dkov�m seznamem naho�e a n�hledem dole.

    Tento formul�� slou�� k�sou�asn�mu zobrazen� p�ehledu polo�ek a formul��e s
    roz�i�uj�c�mi informacemi.  Podle specifikace vazby a doln�ho formul��e
    m��e j�t jak o detaily k aktu�ln�mu z�znamu, tak o souhrnn� informace
    (nap�. v�sledky agregac� nad daty horn�ho formul��e atd.).

    """
    _DESCR = _("du�ln� n�hled")
    
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
    """Du�ln� formul�� s���dkov�m seznamem naho�e a n�hledem dole.

    Tento formul�� slou�� k�sou�asn�mu zobrazen� p�ehledu polo�ek a podrobn�mu
    zobrazen� aktu�ln� polo�ky.  N�hled nen� ur�en k�editaci t�to polo�ky.  Jde
    vlastn� o speci�ln� p��pad formul��e rodi�ovsk� ��dy, kdy n�hled v doln�m
    formul��i je d�n stejnou specifikac�, jako horn� formul��.

    """
    
    def _init_attributes(self, **kwargs):
        self._in_mainform_selection = False
        super_(DescriptiveDualForm)._init_attributes(self, **kwargs)
        
    def _create_view_spec(self):
        return None

    def _initial_sash_ratio(self):
        return None # V t�to t��d� se nepou��v�

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
