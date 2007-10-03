# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007 Brailcom, o.p.s.
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

"""Interaktivn� formul��e pro pr�ci s�daty.

Z�kladem v�ech formul��� je t��da 'Form'.  D�le tento modul obsahuje jej�
potomky pro konkr�tn� pou�it� -- jednoduch� edita�n� formul��e (pro zobrazen� a
editaci jednoho z�znamu).  Jednoduch� seznamov� formul��e a du�ln� formul��e
jsou v odd�len�ch modulech 'list' a 'dualform'.  Bl�e viz dokumentace
jednotliv�ch t��d.

"""

import time
import pytis.data
import pytis.output
from pytis.presentation import PresentedRow
from pytis.form import *
import wx

class Form(Window, KeyHandler, CallbackHandler, CommandHandler):
    """Spole�n� nadt��da formul���.

    Formul�� si podle jm�na specifikace p�edan�ho konstruktoru vy��d� od
    resolveru p��slu�nou datovou a prezenta�n� specifikaci.  Z datov�
    specifikace vytvo�� datov� objekt (instance t��dy odvozen� z
    'pytis.data.Data').  Datov� objekt a prezenta�n� specifikace jsou potom
    ulo�eny ve form� atribut� instance formul��e ('self._view' a 'self._data')

    Instance t��d odvozen�ch z t�to t��dy jsou potom vytv��eny na z�klad�
    interpretace prezenta�n� specifikace a pracuj� s daty s pomoc� datov�ho
    objektu a jeho API (kter� je nez�visl� na konkr�tn�m zdroji dat).

    Form je potomkem 'Window', d�ky �emu� je mo�n� jej ukl�dat na z�sobn�k oken
    aplikace a prov�d�t dal�� operace, jako zaost�ov�n�, skr�v�n�, zobrazov�n�
    apod.

    Pou��van� specifika�n� funkce:

      print_spec -- sekvence dvojic (POPIS, SOUBOR), kde POPIS je string se
        stru�n�m slovn�m popisem specifikace (vyu��van�m nap��klad jako titulek
        polo�ky menu) a SOUBOR je string ud�vaj�c� jm�no souboru se
        specifikac�, relativn� k�adres��i s�defini�n�mi soubory, bez p��pony

    """

    CALL_USER_INTERACTION = 'CALL_USER_INTERACTION'
    """Konstanta callbacku interakce u�ivatele."""


    _STATUS_FIELDS = ()
    _PERSISTENT_FORM_PARAMS = ()
    DESCR = None

    def _get_command_handler_instance(cls):
        return current_form(inner=False)
    _get_command_handler_instance = classmethod(_get_command_handler_instance)

    def __init__(self, parent, resolver, name, guardian=None, transaction=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:
        
          parent -- instance 'wxFrame', do kter�ho formul�� pat��
          resolver -- resolver jmenn�ch odkaz�, instance 'pytis.util.Resolver' 
          name -- jm�no specifika�n�ho souboru pro resolver; string
          guardian -- formul�� (instance libovoln� t��dy), ve kter�m je
            formul�� vlo�en z�hlediska struktury aplikace; nen�-li zad�n, je
            pou�it 'parent'.  Tento parametr je vyu��v�n nap��klad p�i zas�l�n�
            kl�vesov�ch ud�lost� \"nahoru\".  Typicky je to formul��, kter�
            tuto instanci vytv���.
          transaction -- transaction to use when manipulating data
          kwargs -- viz n�e.

        Resolver je pou�it k z�sk�n� datov� a prezenta�n� specifikace a
        n�sledn�mu vytvo�en� datov�ho objektu. Ten je potom spole�n� s
        prezenta�n� specifikac� ulo�en v podob� atribut� vytv��en� instance.

        Odkaz na resolver samotn� je tak� zapamatov�n pro pozd�j�� pou�it�
        (vytv��en� dal��ch formul���).

          
        Inicializace je rozd�lena do n�kolika krok�.  Nejprve jsou zpracov�n�
        v�echny argumenty spole�n� v�em formu��ov�m t��d�m.  Ty zpracov�v�
        konstruktor b�zov� t��dy 'Form'.  Jejich zpracov�n� by nem�lo b�t
        p�edefinov�v�no v odvozen�ch t��d�ch a ani ��dn� dal�� argumenty by
        nem�ly b�t p�id�v�ny.  Konstruktor je mo�no p�edefinovat a prov�d�t
        n�jak� dopl�uj�c� akce, ale argumenty by nem�ly b�t m�n�ny.

        Po zpracov�n� spole�n�ch argumwent� jsou na�teny specifikace a vytvo�en
        datov� objekt.

        Pot� jsou zpracov�v�ny kl��ov� argumenty.  Ka�d� odvozen� t��da m��e
        definovat sv� vlastn� kl��ov� argumenty.  Ty potom zpracuje
        p�edefinov�n�m metody '_init_attributes()'.  Ta ji� m��e vyu��vat
        inicializovan�ho datov�ho objetu a specifikac� a p��padn� initializovat
        dal�� atributy t��dy.  Metoda '_init_attributes()' by m�la v�dy
        zpracov�vat pouze kl��ov� argumenty, kter� jsou specifick� pro danou
        t��du.  Zbyl� p�ed� metod� rodi�ovsk� t��dy konstrukc� **kwargs.  Takto
        by m�lo b�t zaru�eno, �e dojde postupn� ke zpracov�n� v�ech argument�.
        Pokud n�jak� zbydou, vyvol� b�zov� t��da v�jimku 'AssertionError'.

        Teprve po zpravov�n� argument� konstruktoru a inicializaci atribut� je
        vytv��en vlastn� obsah formul��e (viz. '_create_form()').  Toto by m�lo
        b�t dodr�ov�no i v odvozen�ch t��d�ch.
        
        """
        start_time = time.time()
        self._parent = parent
        self._resolver = resolver
        self._name = name
        self._guardian = guardian or parent
        self._governing_transaction = transaction
        self._transaction = transaction or self._default_transaction()
        Window.__init__(self, parent)
        KeyHandler.__init__(self)
        CallbackHandler.__init__(self)
        try:
            self._view = self._create_view_spec()
            self._data = self._create_data_object()
        except ResolverError:
            log(OPERATIONAL, 'Chyba z�resolveru', format_traceback())
            throw('form-init-error')
        self._init_attributes(**kwargs)
        self._result = None
        start_time = time.time()
        self._create_form()
        log(EVENT, 'Form created in %.3fs:' % (time.time() - start_time), self)

    def _init_attributes(self):
        """Process constructor keyword arguments and initialize the attributes.

        This method is called in the initial phase of form construction before any UI widget
        creation but after the initialization of specifications and the data object.  The derived
        classes should primarily process all their specific constructor arguments and initialize
        the attributes of the instance.  See also the constructor documentation for more details.

        """
        key = self._form_state_key()
        self._form_state = config.form_state.get(key)
        if not isinstance(self._form_state, dict):
            self._form_state = config.form_state[key] = {}
        self._initial_form_state = copy.copy(self._form_state)

    def _create_view_spec(self):
        t = time.time()
        spec = self._resolver.get(self._name, 'view_spec')
        log(EVENT, 'Specification read in %.3fs:' % (time.time() - t), spec)
        assert isinstance(spec, ViewSpec)
        return spec        

    def _create_data_object(self):
        factory = self._resolver.get(self._name, 'data_spec')
        import config
        if __debug__ and config.server:
            import pytis.remote
        else:    
            import pytis.data    
        assert isinstance(factory, pytis.data.DataFactory) or \
               isinstance(factory, pytis.remote.RemoteDataFactory)
        if issubclass(factory.class_(), pytis.data.DBData):
            kwargs = dict(connection_data=config.dbconnection)
        else:
            kwargs = {}
        t = time.time()
        op = lambda : factory.create(**kwargs)
        success, data_object = db_operation(op)
        if not success:
            throw('form-init-error')
        log(EVENT, 'Data object created in %.3fs:' % (time.time() - t), data_object)
        return data_object

    def _create_form(self):
        # Build the form from parts
        self._top_level_sizer = sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_form_parts(sizer)
        self.SetSizer(sizer)
        sizer.Fit(self) # Set the size of window `self' to size of the sizer.

    def _default_transaction(self):
        return None
        
    def _create_form_parts(self, sizer):
        pass

    def __str__(self):
        return '<%s for "%s">' % (self.__class__.__name__, self._name)

    def __repr__(self):
        return str(self)

    def _form_state_key(self):
        return self.__class__.__name__+'/'+self._name
    
    def _get_state_param(self, name, default=None, cls=None, item_cls=None):
        try:
            param = self._form_state[name]
        except KeyError:
            return default
        if cls is not None and not isinstance(param, cls):
            log(OPERATIONAL, "Invalid saved form attribute value:", name)
            return default
        if item_cls is not None:
            assert cls is tuple
            for item in param:
                if not isinstance(item, item_cls):
                    log(OPERATIONAL, "Invalid saved form attribute value:", name)
                    return default
        return param

    def _set_state_param(self, name, value):
        self._form_state[name] = value

    def _unset_state_param(self, name):
        if self._form_state.has_key(name):
            del self._form_state[name]

    def _on_form_state_change(self):
        pass

    def _persistent_form_params(self):
        state, keys = self._form_state, self._PERSISTENT_FORM_PARAMS
        return dict([(k, state[k]) for k in keys if state.has_key(k)])

    def _release_data(self):
        if self._data is not None:
            self._data.sleep()

    # Zpracov�n� p��kaz�
   
    def _can_reload_form_state(self):
        def nonp(state):
            return dict([(k,v) for k,v in state.items()
                         if k not in self._PERSISTENT_FORM_PARAMS])
        return nonp(self._form_state) != nonp(self._initial_form_state)
    
    def _cmd_reload_form_state(self):
        persistent = self._persistent_form_params()
        self._form_state = copy.copy(self._initial_form_state)
        self._form_state.update(persistent)
        config.form_state[self._form_state_key()] = self._form_state
        self._on_form_state_change()
        if isinstance(self, Refreshable):
            self.refresh()

    def _can_reset_form_state(self):
        persistent = self._PERSISTENT_FORM_PARAMS
        return [k for k in self._form_state.keys() if k not in persistent]
        
    def _cmd_reset_form_state(self):
        self._form_state = self._persistent_form_params()
        config.form_state[self._form_state_key()] = self._form_state
        self._on_form_state_change()
        if isinstance(self, Refreshable):
            self.refresh()
        
    def _cmd_help(self):
        help(self._name.replace(':','-'))

    def _cmd_leave_form(self):
        return self.close()

    # Ve�ejn� metody
    
    def name(self):
        """Vra� n�zev specifikace formul��e."""
        return self._name

    def descr(self):
        """Vra� textov� popis typu formul��e jako �et�zec."""
        if self.DESCR is not None:
            return self.DESCR
        else:
            return self.__class__.__name__
        
    def title(self):
        """Vra� titulek ze specifikace formul��e jako �et�zec."""
        return self._view.title()

    def guardian(self):
        """Vra� guardian zadan� v�konstruktoru (nebo parent)."""
        return self._guardian

    def check_permission(self, perm, quiet=True):
        """Vra� pravdu, pokud m� u�ivatel dan� pr�va k datov�mu objektu.

        Argumentem je konstanta  t��dy 'pytis.data.Permission'.

        """
        VIEW   = pytis.data.Permission.VIEW
        INSERT = pytis.data.Permission.INSERT
        UPDATE = pytis.data.Permission.UPDATE
        DELETE = pytis.data.Permission.DELETE
        EXPORT = pytis.data.Permission.EXPORT
        if perm == DELETE:
            result = self._data.permitted(None, perm)
        else:
            for col in self._data.columns():
                if self._data.permitted(col.id(), perm):
                    result = True
                    break
            else:
                result = False
        if not result and not quiet:
            msg = {
                VIEW:   "Nem�te pr�vo k�zobrazen� formul��e.",
                INSERT: "Nem�te pr�vo vlo�it nov� z�znam.",
                UPDATE: "Nem�te pr�vo zm�nit existuj�c� z�znam.",
                DELETE: "Nem�te pr�vo smazat existuj�c� z�znam.",
                EXPORT: "Nem�te pr�vo k exportu do CSV.",
                }[perm]
            message(msg, beep_=True)
        return result

    def set_status(self, field, message):
        """Zobraz zpr�vu `message' v poli `id' stavov� ��dky formul��e.

        M�-li formul�� vlastn� stavovou ��dku a v n� pole `id' zobraz v n�m
        danou zpr�vu a vra� pravdu.  V opa�n�m p��pad� vra� nepravdu.

        """
        return False

    def save(self):
        self._saved_state = map(lambda id: (id, get_status(id)), self._STATUS_FIELDS)
        self._release_data()

    def restore(self):
        for id, message in self._saved_state:
            set_status(id, message, log_=False)

    def data(self):
        """Return a new instance of the data object used by the form."""
        return self._create_data_object()
        
    def _cleanup(self):
        super(Form, self)._cleanup()
        for id in self._STATUS_FIELDS:
            set_status(id, '')
    

class InnerForm(Form):
    """Formul�, kter� zpracuje p��kazy samostatn� i unvit� du�ln�ho formul��e.

    Tato formul��ov� t��da je zde p�edev��m kv�li definici a zpracov�n�
    p��kaz�.  Pokud je aktu�ln�m formul��em jednoduch� formul��, je zpracov�n�
    p��kazu p�ed�no tomuto formul��i.  Pokud je v�ak aktu�ln�m formul��em
    du�ln� formul��, je t�eba rozhodnout, zda bude p��kaz zpracov�n p��mo
    du�ln�m formul��em, nebo jeho aktivn�m podformul��em.  P��kazy t��dy 'Form'
    jsou zpracov�v�ny v�dy formul��em nejvy��� �rovn� (du�ln�m formul��em
    samotn�m, pokud je aktu�ln�m formul��em du�ln� formul��).

    P��kazy definovan� touto t��dou a t��dami z n� odvozen�mi jsou v�ak v�dy
    p�ed�v�ny aktivn�mu vnit�n�mu formul��i.
    
    """
    def _get_command_handler_instance(cls):
        return current_form()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    

class Refreshable:
    """T��da zaji��uj�c� existenci metody 'refresh()' s�dan�m v�znamem.

    Tuto t��du by m�ly d�dit v�echny formul��e, kter� maj� b�t obnoveny p�i
    zm�n� dat (typicky zp�soben� jin�m formul��em v��e na z�sobn�ku r�mc�).
    
    """

    DOIT_IMMEDIATELY = 'DOIT_IMMEDIATELY'
    """Konstanta pro 'refresh()' pro okam�it� update.

    Nen�-li seznam pr�v� editov�n, je update proveden okam�it�.  Jinak je
    u�ivatel dot�z�n, zda m� b�t update proveden ihned; odpov�-li u�ivatel
    negativn�, je update proveden a� po ukon�en� editace.

    """
    DOIT_AFTEREDIT = 'DOIT_AFTEREDIT'
    """Konstanta pro 'refresh()' pro update po skon�en� editace.

    Nen�-li seznam pr�v� editov�n, je update proveden okam�it�.  Jinak je
    proveden a� po ukon�en� editace.
    
    """
    DOIT_IFNEEDED = 'DOIT_IFNEEDED'
    """Konstanta pro 'refresh()' pro podm�n�n� update.

    Update je proveden pouze tehdy, je-li zn�mo, �e do�lo ke zm�n� dat.
    V�takov�m p��pad� je proveden okam�it� pouze tehdy, jestli�e seznam nen�
    pr�ve editov�n a v�posledn� dob� nebyl proveden ��dn� jin� update;
    v�opa�n�m p��pad� je update odlo�en \"a� na vhodn�j�� chv�li\" (nicm�n�
    proveden bude).

    """
    _block_refresh = 0

    def block_refresh(cls, function, *args, **kwargs):
        """Zablokuj ve�ker� refresh po dobu prov�d�n� funkce 'function'.

        V�echny argumenty jsou p�ed�ny volan� funkci.
        
        Vrac�: v�sledek vr�cen� volanou funkc�.

        Refresh je zablokov�n glob�ln�, pro v�echny existuj�c� formul��e.
        
        """
        Refreshable._block_refresh += 1
        try:
            result = function(*args, **kwargs)
        finally:
            Refreshable._block_refresh -= 1
        return result
    block_refresh = classmethod(block_refresh)
    
    def refresh(self, when=None):
        """Aktualizuj data formul��e z�datov�ho zdroje.

        P�ekresli data ve formul��i v�okam�iku dan�m argumentem 'when'.

        Argumenty:

          when -- ur�uje, zda a kdy m� b�t aktualizace provedena, mus� to b�t
            jedna z�'DOIT_*' konstant t��dy.  Implicitn� hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrac�: Pravdu, pr�v� kdy� byla aktualizace provedena.

        """
        level = Refreshable._block_refresh
        if level == 0:
            self._refresh(when=when)
        elif level > 0:
            log(OPERATIONAL, "Refresh neproveden kv�li blokaci:", level)
        else:
            raise ProgramError("Nep��pustn� hodnota _block_refresh:", level)

    def _refresh(self, when=None):
        """Prove� vlastn� refresh.

        Tuto metodu nech� p�edefinuj� odvozen� t��dy.

        """
        pass


class PopupForm:
    """Formul�� nach�zej�c� se v�samostatn�m framu.

    Tato t��da je ur�ena k�vlo�en� mezi p�edky t��dy, jej� instance maj� b�t
    vytv��eny v�samostatn�ch framech.  Pro z�sk�n� framu slou�� metoda
    '_popup_frame'.

    """
    def _popup_frame(self, parent):
        """Vra� frame instance.

        Pokud frame je�t� neexistuje, vytvo� jej.

        Argumenty:
        
          parent -- rodi�ovsk� okno, instance 'wx.Window'

        """
        try:
            frame = self._popup_frame_
        except AttributeError:
            style = wx.DIALOG_MODAL|wx.DEFAULT_DIALOG_STYLE #|wx.RESIZE_BORDER
            frame = wx.Dialog(parent, style=style)
            self._popup_frame_ = frame
            wx_callback(wx.EVT_CLOSE, frame, self._on_frame_close)
        return frame    

    def _on_frame_close(self, event):
        if self:
            if self._exit_check():
                self.defocus()
                event.Skip()
            else:
                event.Veto()

    def close(self, force=False):
        # T�m se zavol� _on_frame_close() a tam provedeme zbytek.
        return self._popup_frame_.Close(force=force)
        
    def run(self, lock_key=None):
        """Show the form as a modal dialog.

        Arguments:

          lock_key -- lock the row with the given key

        """
        if lock_key is not None and not isinstance(self._data, pytis.data.DBDataDefault):
            lock_key = None
        try:
            if lock_key is not None:
                if not self._lock_record(lock_key):
                    return None
            unlock_callbacks()
            frame = self._parent
            frame.SetTitle(self.title())
            frame.SetClientSize(self.GetSize())
            frame.ShowModal()
        finally:
            if self._governing_transaction is None and self._transaction is not None \
                   and self._result is None:
                def rollback():
                    self._transaction.rollback()
                db_operation(rollback)
            self._governing_transaction = None
            self._transaction = None
        result = self._result
        self._close(force=True)
        return result


class TitledForm:
    """Mix-in t��da pro formul��e s�titulkem.
    
    Lze vyu��t bu�to pouze metodu '_create_caption()', kter� vytv��� samotn�
    text titulku, nebo metodu '_create_title_bar()', kter� p�id�v� 3d panel.

    """    
    _TITLE_BORDER_WIDTH = 3
    
    def _create_caption(self, parent=None, size=None):
        # Create the title text as 'wxStaticText' instance.
        text = self.title()
        if parent is None:
            parent = self
        caption = wx.StaticText(parent, -1, text, style=wx.ALIGN_CENTER)
        if size is None: 
            size = caption.GetFont().GetPointSize()
        font = wx.Font(size, wx.DEFAULT, wx.NORMAL, wx.BOLD,
                       encoding=wx.FONTENCODING_DEFAULT)
        caption.SetFont(font)
        width, height, d, e = self.GetFullTextExtent(text, font)
        caption.SetSize(wx.Size(width, height))
        return caption

    def _on_show_description(self, event):
        title = _("Popis n�hledu %s") % self._view.title()
        text = "= "+ self._view.title() +" =\n\n" + (self._view.help() or self._view.description())
        InfoWindow(title, text=text, format=TextFormat.WIKI)

    def _print_menu(self):
        # Vra� seznam polo�ek tiskov�ho menu.
        name = self._name
        try:
            print_spec = self._resolver.get(name, 'print_spec')
        except ResolverSpecError:
            print_spec = None
        if not print_spec:
            print_spec = ((_("V�choz�"), os.path.join('output', name)),)
        return [MItem(title,
                      command=InnerForm.COMMAND_PRINT(print_spec_path=path))
                for title, path in print_spec]

    def _filter_menu(self):
        return None

    def _aggregation_menu(self):
        return None
    
    def _on_menu_button(self, event, items):
        self._run_callback(self.CALL_USER_INTERACTION)
        popup_menu(event.GetEventObject(), items, self._get_keymap())

    def _create_title_bar(self):
        """Vytvo� 3d panel s nadpisem formul��e."""
        panel = wx.Panel(self, -1, style=wx.RAISED_BORDER)
        caption = self._create_caption(panel)
        print_menu = self._print_menu()
        description = self._view.help() or self._view.description()
        buttons = (
            wx_button(panel, icon='filter', label=_("Filtr"),
                      tooltip=_("Zobrazit menu filtrace"), noborder=True,
                      callback=lambda e: self._on_menu_button(e, self._filter_menu()),
                      enabled=self._filter_menu() is not None),
            wx_button(panel, icon='aggregate', label=_("f(x)"),
                      tooltip=_("Zobrazit menu agrega�n�ch funkc�"), noborder=True,
                      callback=lambda e: self._on_menu_button(e, self._aggregation_menu()),
                      enabled=self._aggregation_menu() is not None),
            wx_button(panel, icon=wx.ART_PRINT, noborder=True,
                      tooltip=_("Zobrazit tiskov� menu"),
                      callback=lambda e: self._on_menu_button(e, print_menu)),
            wx_button(panel, '?', icon='describe', noborder=True,
                      tooltip=_("Zobrazit popis n�hledu"),
                      callback=self._on_show_description,
                      enabled=description is not None))
        box = wx.BoxSizer()
        box.Add(caption, 1, wx.EXPAND|wx.ALL, self._TITLE_BORDER_WIDTH)
        for b in buttons:
            box.Add(b)
        panel.SetSizer(box)
        panel.SetAutoLayout(True)        
        box.Fit(panel)
        return panel


class LookupForm(InnerForm):
    """Formul�� s�vyhled�v�n�m a t��d�n�m."""
    
    SORTING_NONE = 'SORTING_NONE'
    """Konstanta pro argument direction p��kazu 'COMMAND_SORT'."""
    SORTING_ASCENDENT = 'SORTING_ASCENDENT'
    """Konstanta pro argument direction p��kazu 'COMMAND_SORT'."""
    SORTING_DESCENDANT = 'SORTING_DESCENDANT'
    """Konstanta pro argument direction p��kazu 'COMMAND_SORT'."""

    _PERSISTENT_FORM_PARAMS = InnerForm._PERSISTENT_FORM_PARAMS + \
                              ('conditions', 'filter', 'search')
    
    def _init_attributes(self, sorting=None, filter=None, condition=None, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          sorting -- specification of initial sorting, same as the argument
            'sort' of 'pytis.data.Data.select()'.
          filter -- initial filter condition as a 'pytis.data.Operator'
            instance.  This filter is indicated to the user and can be modified
            as any other user-defined filter.
          condition -- 'pytis.data.Operator' instance filtering the rows of the
            underlying data object.  This filter is not indicated to the user
            nor is there a chance to turn it off.
          kwargs -- arguments passed to the parent class
        
        """
        super_(LookupForm)._init_attributes(self, **kwargs)
        self._lf_select_count = None
        self._init_sorting(sorting)
        self._lf_initial_sorting = self._lf_sorting
        # _lf_condition reprezentuje statickou podm�nku danou argumentem
        # konstruktoru, naproti tomu _lf_filter reprezentuje aktu�ln� podm�nku
        # u�ivatelsk�ho filtru.
        self._lf_condition = condition
        if filter:
            # Make sure the condition is well formed.
            Condition("", filter)
        self._lf_filter = filter
        self._lf_last_filter = filter or self._load_condition('filter')
        self._lf_search_condition = self._load_condition('search')
        self._user_conditions = self._load_conditions('conditions')
        self._init_select()
        
    def _new_form_kwargs(self):
        return dict(condition=self._lf_condition, sorting=self._lf_sorting)

    def _init_sorting(self, sorting=None):
        if sorting is None:
            sorting = self._get_state_param('sorting', None, tuple)
        if sorting is not None:
            for id, direction in sorting:
                if self._data.find_column(id) is None or direction not in \
                       (self.SORTING_ASCENDENT, self.SORTING_DESCENDANT):
                    sorting = None
                    break
        if sorting is None:
            mapping = {pytis.data.ASCENDENT: self.SORTING_ASCENDENT,
                       pytis.data.DESCENDANT: self.SORTING_DESCENDANT}
            sorting =  tuple([(cid, mapping[dir])
                              for cid, dir in self._default_sorting()])
        self._lf_sorting = sorting

    def _pack_condition(self, condition):
        # We prefer saving the condition in our custom format, since its safer
        # to pickle Python builtin types than instances of application defined
        # classes.
        def pack(something):
            if isinstance(something, pytis.data.Operator):
                args = tuple([pack(arg) for arg in something.args()])
                return (something.name(), args, something.kwargs())
            elif isinstance(something, pytis.data.Value):
                return [something.export()]
            elif isinstance(something, pytis.data.WMValue):
                return [something.value()]
            elif isinstance(something, str):
                return something
            else:
                raise ProgramError("Invalid object to pack:", something)
        assert isinstance(condition, pytis.data.Operator)
        return pack(condition)

    def _unpack_condition(self, packed):
        # NOT is not allowed!
        OPERATORS = ('AND','OR','EQ','NE','WM','NW','LT','LE','GT','GE')
        def unpack(packed):
            name, packed_args, kwargs = packed
            assert name in OPERATORS, name
            assert len(packed_args) == 2, len(packed_args)
            op = getattr(pytis.data, name)
            if name in ('AND', 'OR'):
                args = [unpack(arg) for arg in packed_args]
            elif isinstance(packed_args[1], list):
                col, val = packed_args[0], packed_args[1][0]
                type = self._data.find_column(col).type()
                if name in ('WM', 'NW'):
                    value, err = type.wm_validate(val)
                else:
                    value, err = type.validate(val, strict=False)
                if err is not None:
                    raise ProgramError("Invalid operand value:", err)
                args = col, value
            else:
                args = packed_args
                assert isinstance(args[0], str)
                assert isinstance(args[1], str)
            return op(*args, **kwargs)
        try:
            return unpack(packed)
        except Exception, e:
            log(OPERATIONAL, "Unable to restore packed condition:",
                (packed, str(e)))
            return None
        
    def _load_condition(self, key):
        packed = self._get_state_param(key, None, tuple)
        return packed and self._unpack_condition(packed)
    
    def _save_condition(self, key, condition):
        self._set_state_param(key, self._pack_condition(condition))
        
    def _load_conditions(self, key):
        packed = self._get_state_param(key, None, tuple, tuple)
        if packed:
            unpacked = [(n, self._unpack_condition(c)) for n,c in packed]
            return tuple([Condition(name, cond, fixed=False)
                          for name, cond in unpacked if cond])
        else:
            return ()

    def _save_conditions(self, key, conditions):
        packed = [(c.name(), self._pack_condition(c.condition()))
                  for c in conditions]
        self._set_state_param(key, tuple(packed))
    
    def _default_sorting(self):
        sorting = self._view.sorting()
        if sorting is None:
            sorting = tuple([(k.id(), pytis.data.DESCENDANT)
                             for k in self._data.key()
                             if self._view.field(k.id()) is not None])
        return sorting

    def _current_condition(self, filter=None):
        conditions = (self._lf_condition, filter or self._lf_filter)
        conditions = [c for c in conditions if c is not None]
        if len(conditions) == 0:
            return None
        elif len(conditions) == 1:
            return conditions[0]
        else:
            return pytis.data.AND(*conditions)

    def _init_data_select(self, data):
        return data.select(condition=self._current_condition(),
                           columns=self._select_columns(),
                           sort=self._data_sorting(),
                           transaction=self._transaction, reuse=False)
    
    def _init_select(self):
        def op():
            return self._init_data_select(self._data)
        success, self._lf_select_count = db_operation(op)
        if not success:
            log(EVENT, 'Selh�n� datab�zov� operace')
            throw('form-init-error')
        return self._lf_select_count

    def _data_sorting(self):
        mapping = {self.SORTING_ASCENDENT:  pytis.data.ASCENDENT,
                   self.SORTING_DESCENDANT: pytis.data.DESCENDANT}
        return tuple([(cid, mapping[dir]) for cid, dir in self._lf_sorting])

    def _sorting_columns(self):
        return [cid for cid, direction in self._lf_sorting]
        
    def _sorting_position(self, cid):
        try:
            return self._sorting_columns().index(cid)
        except ValueError:
            return None
        
    def _sorting_direction(self, cid):
        pos = self._sorting_position(cid)
        if pos is not None:
            return self._lf_sorting[pos][1]
        else:
            return None
        
    def _lf_sfs_columns(self):
        return sfs_columns(self._view.fields(), self._data)

    def _search(self, condition, direction, row_number=None, report_failure=True):
        self._search_adjust_data_position(row_number)
        data = self._data
        skip = data.search(condition, direction=direction, transaction=self._transaction)
        if skip == 0:
            log(EVENT, 'Z�znam nenalezen')
            if report_failure:
                message(_("Z�znam nenalezen"), beep_=True)
            result = None
        else:
            result = skip
            log(EVENT, 'Z�znam nalezen:', skip)
            self._search_skip(result, direction)
        return result

    def _search_adjust_data_position(self, row_number):
        pass

    def _search_skip(self, skip, direction):
        data = self._data
        data.skip(skip-1, direction=direction)
        row = data.fetchone(direction=direction, transaction=self._transaction)
        self._select_row(row)

    def _cmd_jump(self):
        if self._lf_select_count > 0:
            prompt = _("Z�znam ��slo (1-%s):") % (self._lf_select_count)
            while True:
                result = run_dialog(InputDialog, message=_("Skok na z�znam"),
                                    prompt=prompt)
                if result is None:
                    break
                elif result.isdigit():
                    self.select_row(int(result)-1)
                    break

    def _cmd_first_record(self):
        self.select_row(0)
        
    def _cmd_last_record(self):
        self.select_row(self._lf_select_count-1)
        
    def _cmd_search(self, next=False, back=False):
        condition = self._lf_search_condition
        if condition is not None and next:
            direction = back and pytis.data.BACKWARD or pytis.data.FORWARD
        else:
            direction, condition, conditions = block_refresh(lambda:
                 run_dialog(SearchDialog, self._lf_sfs_columns(),
                            self.current_row(), col=self._current_column_id(),
                            condition=self._lf_search_condition))
        if direction is not None:
            self._lf_search_condition = condition
            self._search(condition, direction)
            self._save_condition('search', condition)

    def _on_form_state_change(self):
        super(LookupForm, self)._on_form_state_change()
        self._init_sorting()

    def _compute_aggregate(self, operation, column_id, condition):
        condition = self._current_condition(filter=condition)
        return self._data.select_aggregate((operation, column_id), condition,
                                           transaction=self._transaction)

    def _filtered_columns(self):
        columns = []
        def analyze(operator):
            for a in operator.args():
                if isinstance(a, pytis.data.Operator):
                    analyze(a)
                elif isinstance(a, str) and a not in columns:
                    columns.append(a)
        if self._lf_filter is not None:
            analyze(self._lf_filter)
        return columns
        
    def filter(self, condition):
        """Apply given filtering condition."""
        Condition("", condition) # Make sure the condition is well formed.
        self._lf_filter = condition
        if condition is not None:
            self._lf_last_filter = condition
            self._save_condition('filter', condition)
        self._filter_refresh()
        
    def _filter_refresh(self):
        self._init_select()
        self.select_row(self._current_key())

    def _filter_conditions(self):
        return (Condition(_("Posledn� aplikovan� filtr"),
                          self._lf_last_filter),
                ) + self._view.conditions() + self._user_conditions

    def _filter_menu(self):
        # Vra� seznam polo�ek filtra�n�ho menu.
        items = [MItem(_("Otev��t filtra�n� formul��"), command=self.COMMAND_FILTER),
                 MItem(_("Zru�it filtr"),               command=self.COMMAND_UNFILTER),
                 MItem(_("Posledn� aplikovan� filtr"),  command=self.COMMAND_FILTER(last=True))]
        conditions = self._filter_conditions()[1:]
        for i, c in enumerate(conditions):
            if i==0 or i > 0 and  c.fixed() != conditions[i-1].fixed():
                items.append(MSeparator())
            items.append(MItem(c.name(), command=self.COMMAND_FILTER(condition=c.condition()),
                               icon='filter'))
        return items

    def _can_filter(self, condition=None, last=False):
        return not last or self._lf_last_filter is not None
        
    def _cmd_filter(self, condition=None, last=False):
        if last:
            condition = self._lf_last_filter
        if condition:
            perform = True
        else:
            perform, condition, conditions = \
                     run_dialog(FilterDialog, self._lf_sfs_columns(),
                                self.current_row(), self._compute_aggregate,
                                col=self._current_column_id(),
                                condition=self._lf_filter,
                                conditions=self._filter_conditions())
            self._user_conditions = tuple([c for c in conditions
                                           if not c.fixed()])
            self._save_conditions('conditions', self._user_conditions)
        if perform and condition != self._lf_filter:
            self.filter(condition)

    def _can_unfilter(self):
        return self._lf_filter is not None
        
    def _cmd_unfilter(self):
        self.filter(None)

    def _cmd_filter_by_value(self, column_id, value):
        if column_id not in [c.id() for c in self._lf_sfs_columns()]:
            message(_("Podle tohoto sloupce nelze filtrovat."), beep_=True)
        condition = pytis.data.EQ(column_id, value)
        if self._lf_filter is not None:
            condition = pytis.data.AND(self._lf_filter, condition)
        self.COMMAND_FILTER.invoke(condition=condition)
        
    def _cmd_sort(self, col=None, direction=None, primary=False):
        """Zm�� t��d�n�.

        Argumenty:

          col -- id sloupce, podle kter�ho m� b�t seznam set��d�n, nebo
            'None' pro glob�ln� zm�ny (nap��klad vypnut� ve�ker�ho t��d�n�)
          direction -- sm�r t��d�n� (sestupn�/vzestupn�/v�bec).  Hodnota dan�
            konstantou 'LookupForm.SORTING_NONE' zna�� po�adavek na zru�en�
            t��d�n�.  Jinak je o�ek�v�na jedna z konstant
            'LookupForm.SORTING_ASCENDENT' (pro sestupn� t��d�n�), nebo
            'LookupForm.SORTING_DESCENDANT' (pro vzestupn� t��d�n�).
          primary -- pr�v� kdy� je pravdiv�, bude dan� sloupec zvolen jako
            prim�rn� a *jedin�* t��d�c� sloupec.  V opa�n�m p��pad� bude pouze
            p�id�n na konec st�vaj�c�ho seznamu t��d�c�ch sloupc�.
        
        P�i nejednozna�n� kombinaci argument� 'col' a 'direction' je
        automaticky vyvol�n dialog pro v�b�r t��d�c�ch krit�ri�.
        
        """
        if col is None and direction == self.SORTING_NONE:
            sorting = ()
        elif col is None or direction is None:
            columns = self._lf_sfs_columns()
            if col is None and self._lf_sorting: 
                col = self._sorting_columns()[0]
            if direction is not None:
                mapping = {self.SORTING_ASCENDENT:  pytis.data.ASCENDENT,
                           self.SORTING_DESCENDANT: pytis.data.DESCENDANT}
                direction = mapping[direction]
            sorting = run_dialog(SortingDialog, columns, self._data_sorting(),
                                 col=col, direction=direction)
            if sorting is None:
                return None
            elif sorting is ():
                sorting = self._lf_initial_sorting
            else:
                mapping = {pytis.data.ASCENDENT:  self.SORTING_ASCENDENT,
                           pytis.data.DESCENDANT: self.SORTING_DESCENDANT}
                sorting = tuple([(cid, mapping[dir]) for cid, dir in sorting])
        elif col is not None:
            if not self._data.find_column(col):
                message(_("Podle tohoto sloupce nelze t��dit"),
                        beep_=True)
                return None
            pos = self._sorting_position(col)
            sorting = xlist(self._lf_sorting)
            if direction == self.SORTING_NONE:
                del sorting[pos]
            else:
                assert direction in (self.SORTING_ASCENDENT,
                                     self.SORTING_DESCENDANT)
                new_sort_spec = (col, direction)
                if primary and pos !=0:
                    sorting = (new_sort_spec,)
                elif pos is None:
                    sorting.append(new_sort_spec)
                else:
                    sorting[pos] = new_sort_spec
            sorting = tuple(sorting)
        else:
            raise ProgramError("Invalid sorting arguments:", (col, direction))
        if sorting is not None and sorting != self._lf_sorting:
            self._lf_sorting = sorting
            self._set_state_param('sorting', sorting)
            self.select_row(self._current_key())
        return sorting
    
    def _can_sort(self, col=None, direction=None, primary=False):
        # `col' je zde identifik�tor sloupce.
        sorting_columns = tuple(self._sorting_columns())
        if direction == self.SORTING_NONE:
            return sorting_columns and (col is None or col in sorting_columns)
        elif direction is not None and col is not None:
            pos = self._sorting_position(col)
            dir = self._sorting_direction(col)
            if primary:
                return pos != 0 or direction != dir
            else:
                return pos != 0 and direction != dir and sorting_columns
        else:
            return True
        
    # Ve�ejn� metody

    def data(self):
        """Return a new instance of the data object used by the form.

        The instance will have the data select initialized with the current filter condition and
        all its attributes, such as sorting etc.  This is often practical within application
        defined procedures, which retrieve this data object through the 'RecordForm.Record.data()'
        method.

        """
        data = super(LookupForm, self).data()
        self._init_data_select(data)
        return data
            
    def condition(self):
        """Vra� specifikaci aktu�ln� podm�nky v�b�ru dat.

        Podm�nka je vr�cena v�podob� po�adovan� argumentem 'condition'
        metody 'pytis.data.Data.select()'.

        """
        return self._current_condition()

    def sorting(self):
        """Return the current sorting specification."""
        return self._lf_sorting
    
    
class RecordForm(LookupForm):
    """Formul�� schopn� n�jak�m zp�sobem zobrazit aktu�ln� z�znam."""

    CALL_SELECTION = 'CALL_SELECTION'
    """Konstanta callbacku v�b�ru (zm�ny aktu�ln�ho) z�znamu.

    Argumentem callbackov� funkce je nov� vybran� z�znam jako instance
    'RecordForm.Record'.
    
    """
    class Record(PresentedRow):
        # Experimental PresentedRow extension aware of its parent form.  This might allow
        # application specific procedures more reliable access to the current form, from which the
        # row comes.
        def __init__(self, form, *args, **kwargs):
            self._form = form
            super(RecordForm.Record, self).__init__(*args, **kwargs)

        def form(self):
            return self._form

        def data(self):
            # Return a new instance rather than giving the internally used data object.
            # Moreover this instance will have the select initialized in LookupForm.
            return self._form.data()
    
    def _init_attributes(self, prefill=None, select_row=None, _new=False, _singleline=False,
                         **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          prefill -- a dictionary of values used to prefill the current form row.  The meaning is
            the same as for the same 'PresentedRow' constructor argument.

          select_row -- The initially selected row -- the value is the same as the argument of the
            'select_row()' method.

          kwargs -- arguments passed to the parent class

        """
        super_(RecordForm)._init_attributes(self, **kwargs)
        assert prefill is None or isinstance(prefill, dict)
        self._prefill = prefill
        self._row = self.record(self._data_row(select_row), prefill=prefill, new=_new,
                                singleline=_singleline)

    def _signal_update(self):
        pass

    def _select_columns(self):
        return None

    def _find_row_by_number(self, row_number):
        # row_number starts with�0
        data = self._data
        def dbop():
            data.rewind()
            data.skip(row_number)
            return data.fetchone(transaction=self._transaction)
        success, row = db_operation(dbop)
        self._init_select()
        if not success or not row:
            return None
        else:
            return row
    
    def _find_row_by_values(self, cols, values):
        """Vra� datov� ��dek odpov�daj�c� dan�m hodnot�m.

        Arguemnty:

          cols -- sekvence n�zv� sloupc�, kter� maj� b�t prohled�v�ny.
          values -- sekvence hodnot sloupc� jako instanc� 'pytis.data.Value' v
            po�ad� odpov�daj�c�m 'cols'.

        Pro ob� sekvence plat�, �e pokud jsou jednoprvkov�, mohou b�t hodnoty
        p�ed�ny i p��mo, bez obalen� do sekven�n�ho typu.

        """
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        cond = pytis.data.AND(*[pytis.data.EQ(c,v) for c,v in zip(cols, values)])
        condition = pytis.data.AND(cond, self._current_condition())
        data = self._data
        def dbop(condition):            
            data.rewind()
            n = data.select(condition, columns=self._select_columns(),
                            transaction=self._transaction)
            return data.fetchone(transaction=self._transaction)
        success, row = db_operation((dbop, (condition,)))
        self._init_select()
        return row
        
    def _find_row_by_key(self, key):
        cols = self._select_columns()
        def dbop():
            return self._data.row(key, columns=cols, transaction=self._transaction)
        success, row = db_operation(dbop)
        if success and row:
            return row
        else:
            return None
    
    def _get_row_number(self, row):
        """Vra� ��slo ��dku odpov�daj�c� dan� instanci 'pytis.data.Row'.

        Pokud odpov�da�c� ��dek nen� nalezen, vra� None.

        """
        data = self._data
        key = data.key()[0].id()
        def dbop():
            data.rewind()
            return data.search(pytis.data.EQ(key, row[key]), transaction=self._transaction)
        success, result = db_operation(dbop)
        if not success or result == 0:
            return None
        else:
            return result - 1

    def _data_row(self, position):
        # Return the *data* row instance corresponding to given 'select_row()' argument.
        if position is None or isinstance(position, pytis.data.Row):
            row = position
        elif isinstance(position, int):
            row = self._find_row_by_number(position)
        elif isinstance(position, (tuple, pytis.data.Value)):
            row = self._find_row_by_key(xtuple(position))
        elif isinstance(position, dict):
            row = self._find_row_by_values(position.keys(), position.values())
        else:            
            raise ProgramError("Invalid 'position':", position)
        return row

    def _select_row(self, row, quiet=False):
        # Set the form data according to given *data* row.
        self._row.set_row(row)
        self._run_callback(self.CALL_SELECTION, self._row)
        return True

    def _current_key(self):        
        the_row = self.current_row()
        if the_row is not None:
            data_row = the_row.original_row(empty_as_none=True)
            if data_row is None:
                data_row = the_row.row()
            return data_row.columns([c.id() for c in self._data.key()])
        return None
    
    def _current_column_id(self):
        return None
    
    def _new_form_kwargs(self):
        return {}

    def _lock_record(self, key):
        def dbop():
            return self._data.lock_row(key, transaction=self._transaction)
        success, locked = db_operation(dbop)
        if success and locked != None:
            log(EVENT, 'Record is locked')
            run_dialog(Message, _("Z�znam je zam�en"))
            return False
        else:
            return True

    def _check_record(self, row):
        # Prove� kontrolu integrity dan� instance PresentedRow.
        for check in self._view.check():
            result = check(row)
            if result is not None:
                if is_sequence(result):
                    failed_id, msg = result
                    message(msg)
                else:
                    failed_id = result
                    # TODO: T�m bychom p�epsali zpr�vu nastavenou uvnit�
                    # 'check()'.  Pokud ale ��dn� zpr�va nebyla nastavena,
                    # u�ivatel netu��...
                    #message(_("Kontrola integrity selhala!"))
                log(EVENT, 'Kontrola integrity selhala:', failed_id)
                return failed_id
        return None

    def _record_data(self, row):
        rdata = [(f.id(), row[f.id()]) for f in row.fields()
                 if self._data.find_column(f.id()) is not None]
        return pytis.data.Row(rdata)
    
    def _row_copy_prefill(self, the_row):
        # Create a copy of the row, but exclude key columns and computed
        # columns which depend on key columns.
        if the_row:
            keys = [c.id() for c in self._data.key()]
            prefill = {}
            for cid in the_row.keys():
                fspec = self._view.field(cid)
                if cid in keys or fspec.nocopy():
                    continue
                computer = fspec.computer()
                if computer:
                    skip = False
                    for dep in computer.depends():
                        if dep in keys:
                            skip = True
                            break
                    if skip:
                        continue
                prefill[cid] = the_row[cid]
        else:
            prefill = {}
        return prefill

    # Zpracov�n� p��kaz�.
    
    def _cmd_new_record(self, copy=False):
        if not self.check_permission(pytis.data.Permission.INSERT, quiet=False):
            return False
        import copy as copy_
        prefill = self._prefill and copy_.copy(self._prefill) or {}
        if copy:
            prefill.update(self._row_copy_prefill(self.current_row()))
        result = new_record(self._name, prefill=prefill)
        if result:
            if not self.select_row(result.row(), quiet=True):
                msg = _("Vlo�en� z�znam se neobjevil v aktu�ln�m n�hledu.")
                run_dialog(Warning, msg)
    
    def _can_edit_record(self):
        return self._current_key() is not None

    def _cmd_edit_record(self):
        if not self.check_permission(pytis.data.Permission.UPDATE, quiet=False):
            return
        row = self.current_row()
        on_edit_record = self._view.on_edit_record()
        if on_edit_record is not None:
            on_edit_record(row=row)
            # TODO: _signal_update vyvol� refresh.  To je tu jen pro p��pad, �e
            # byla u�ivatelsk� procedura o�et�ena jinak ne� vyvol�n�m
            # formul��e.  Proto�e to samo u� je hack, tak a� si rad�ji tak�
            # tv�rce prov�d� refresh s�m, proto�e tady je vol�n ve v�ech
            # ostatn�ch p��padech zbyte�n� a zdr�uje.
            self._signal_update()
        else:
            name = self._name
            redirect = self._view.redirect()
            if redirect is not None:
                redirected_name = redirect(row)
                if redirected_name is not None:
                    assert isinstance(redirected_name, str)
                    name = redirected_name
            kwargs = self._new_form_kwargs()
            key = self._current_key()
            run_form(PopupEditForm, name, select_row=key, **kwargs)

    def _can_delete_record(self):
        return self._current_key() is not None

    def _cmd_delete_record(self):
        # The return value is used in derived classes!
        if not self.check_permission(pytis.data.Permission.DELETE, quiet=False):
            return False
        # O�et�en� u�ivatelsk� funkce pro maz�n�
        on_delete_record = self._view.on_delete_record()
        if on_delete_record is not None:
            condition = on_delete_record(row=self.current_row())
            if condition is None:
                return False
            assert isinstance(condition, pytis.data.Operator)
            op = lambda : self._data.delete_many(condition, transaction=self._transaction)
            log(EVENT, 'Maz�n� z�znamu:', condition)
        else:
            msg = _("Opravdu chcete z�znam zcela vymazat?")        
            if not run_dialog(Question, msg):
                log(EVENT, 'Maz�n� ��dku u�ivatelem zam�tnuto.')
                return False
            key = self._current_key()
            op = lambda : self._data.delete(key, transaction=self._transaction)
            log(EVENT, 'Maz�n� z�znamu:', key)
        success, result = db_operation(op)
        if success:
            self._signal_update()
            log(ACTION, 'Z�znam smaz�n.')
            return True
        else:
            return False

    def _cmd_import_interactive(self):
        if not self._data.permitted(None, pytis.data.Permission.INSERT):
            msg = _("Nem�te pr�va pro vkl�d�n� z�znam� do t�to tabulky.")
            message(msg, beep_=True)
            return False
        msg = _("Nejprve vyberte soubor obsahuj�c� importovan� data. "
                "Pot� budete moci zkontrolovat a potvrdit ka�d� z�znam.\n\n"
                "*Form�t vstupn�ho souboru:*\n\n"
                "Ka�d� ��dek obsahuje seznam hodnot odd�len�ch zvolen�m "
                "znakem, nebo skupinou znak� (vypl�te n�e). "
                "Tabel�tor zapi�te jako ='\\t'=.\n\n"
                "Prvn� ��dek obsahuje identifik�tory sloupc� a ur�uje tedy "
                "v�znam a po�ad� hodnot v n�sleduj�c�ch (datov�ch) ��dc�ch.\n\n"
                "Identifik�tory jednotliv�ch sloupc� jsou n�sleduj�c�:\n\n" + \
                "\n".join(["|*%s*|=%s=|" % (c.column_label(), c.id()) for c in
                           [self._view.field(id)
                            for id in self._view.layout().order()]]))
        separator = run_dialog(InputDialog, 
                               title=_("Hromadn� vkl�d�n� dat"),
                               report=msg, report_format=TextFormat.WIKI,
                               prompt="Odd�lova�", value='|')
        if not separator:
            if separator is not None:
                message(_("Nebyl zad�n odd�lova�."), beep_=True)
            return False
        separator = separator.replace('\\t', '\t')
        while 1:
            filename = run_dialog(FileDialog)
            if filename is None:
                message(_("Nebyl zad�n soubor. Proces ukon�en."), beep_=True)
                return False
            try:
                fh = open(filename)
            except IOError, e:
                msg = _("Nepoda�ilo se otev��t soubor '%s': %s")
                run_dialog(Error, msg % (filename, str(e)))
                continue
            break
        try:
            columns = [str(id.strip()) for id in fh.readline().split(separator)]
            for id in columns:
                if not self._row.has_key(id):
                    run_dialog(Error, _("Nezn�m� sloupec: %s") % id)
                    return False
            types = [self._row[id].type() for id in columns]
            line_number = 1
            data = []
            for line in fh:
                line_number += 1
                values = line.rstrip('\r\n').split(separator)
                if len(values) != len(columns):
                    msg = _("Chyba dat na ��dku %d:\n"
                            "Po�et hodnot neodpov�d� po�tu sloupc�.")
                    run_dialog(Error, msg % line_number)
                    return False
                row_data = []
                for id, type, val in zip(columns, types, values):
                    value, error = type.validate(val, transaction=self._transaction)
                    assert value.type() == type, (value.type(), type)
                    if error:
                        msg = _("Chyba dat na ��dku %d, sloupec '%s':\n%s") % \
                              (line_number, id, error.message())
                        run_dialog(Error, msg)
                        return False
                    row_data.append((id, value))
                data.append(pytis.data.Row(row_data))
        finally:
            fh.close()
        new_record(self._name, prefill=self._prefill, inserted_data=data)
            
    # Ve�ejn� metody

    def record(self, row, **kwargs):
        """Create a new `RecordForm.Record' instance bound to this form."""
        fields = self._view.fields()
        data = self._create_data_object()
        return self.Record(self, fields, data, row, transaction=self._transaction, **kwargs)
    
    def select_row(self, position, quiet=False):
        """Vyber ��dek dle 'position'.

        Argument 'position' m��e m�t n�kterou z�n�sleduj�c�ch hodnot:
        
          None -- nebude vybr�n ��dn� ��dek.
          Nez�porn� integer -- bude vybr�n ��dek p��slu�n�ho po�ad�, p�i�em�
            ��dky jsou ��slov�ny od�0.
          Datov� kl�� -- bude vybr�n ��dek s�t�mto kl��em, kter�m je instance
            t��dy 'pytis.data.Value' nebo jejich tuple.
          Slovn�k hodnot -- bude vybr�n prvn� nalezen� ��dek obsahuj�c�
            hodnoty slovn�ku (instance 'pytis.data.Value') v sloupc�ch ur�en�ch
            kl��i slovn�ku.
          Instance t��dy 'pytis.data.Row' -- bude p�eveden na datov� kl�� a
            zobrazen odpov�daj�c� ��dek.  Instance mus� b�t kompatibiln�
            s�datov�m objektem formul��e.
        
        Pokud takov� z�znam neexistuje, zobraz chybov� dialog.  Argumentem
        'quiet' lze zobrazen� chybov�ho dialogu potla�it.  T�m lze nenalezen�
        ��dku ti�e ignorovat, nebo o�et�it vlastn�m zp�sobem na z�klad�
        n�vratov� hodnoty.

        V�b�rem je my�lena akce relevantn� pro dan� typ formul��e (odvozen�
        t��dy).  Tedy nap��klad vysv�cen� ��dku v tabulce, zobrazen� z�znamu v
        n�hledov�m formul��i apod.

        Vrac�: Pravdu, pokud byl z�znam �sp�n� nalezen a vybr�n, nepravdu v
        opa�n�m p��pad�.
        
        """
        row = self._data_row(position)
        if not quiet and position is not None and row is None:
            run_dialog(Warning, _("Z�znam nenalezen"))
            return False
        return self._select_row(row, quiet=quiet)

    def current_row(self):
        """Vra� instanci PresentedRow pr�v� aktivn�ho ��dku.

        Nen�-li vybr�n ��dn� ��dek, vra� 'None'.

        """
        return self._row

    def current_key(self):
        """Vra� kl�� aktu�ln� vybran�ho ��dku.

        Vrac�: Sekvenci instanc� t��dy 'pytis.data.Value' nebo 'None', pokud
        nen� vybr�n ��dn� ��dek.

        """
        return self._current_key()

    def prefill(self):
        """Vra� data pro p�edvypln�n� nov�ho z�znamu."""
        return self._prefill
    

### Edita�n� formul��


class EditForm(RecordForm, TitledForm, Refreshable):
    """Formul�� pro editaci v�ech vlastnost� jednoho z�znamu.

    Formul�� je vytvo�en poskl�d�n�m jednotliv�ch vstupn�ch pol��ek dan�ch
    specifikac� do m��ky.  Pole mohou b�t r�zn� seskupov�na a jejich rozlo�en�
    je ur�eno specifika�n� t��dou 'LayoutSpec' resp. 'GroupSpec'.

    Ka�d� vstupn� pole je reprezentov�no objektem t��dy 'InputField'.  To se
    star� o interakci s u�ivatelem, validaci vstupn�ch dat apod.

    Formul�� m��e slou�it jak k�prohl�en� �i editaci st�vaj�c�ch dat, tak
    i�k�vytv��en� nov�ch z�znam� (viz argument konstruktoru 'mode').
 
    """

    MODE_INSERT = 'MODE_INSERT'
    """M�d formul��e pro vkl�d�n� nov�ch z�znam�."""
    MODE_EDIT = 'MODE_EDIT'
    """M�d formul��e pro editaci st�vaj�c�ch z�znam�."""
    MODE_VIEW = 'MODE_VIEW'
    """M�d formul��e pro zobrazen� z�znam� bez mo�nosti editace."""
    
    def __init__(self, *args, **kwargs):
        super(EditForm, self).__init__(*args, **kwargs)
        # Remember the original size.
        self._size = self.GetSizer().GetMinSize() + wx.Size(2, 2)
        if isinstance(self._parent, wx.Dialog):
            wx_callback(wx.EVT_INIT_DIALOG, self._parent, self._set_focus_field)
        else:
            self._set_focus_field()

    def _init_attributes(self, mode=MODE_EDIT, focus_field=None, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          mode -- one of the 'MODE_*' constants.  Determines whether the form is primarily for
            viewing, editation or creation of records.
          focus_field -- identifier of the field which should be activated for user input on form
            startup.  If None, the first field is the default.  It is also possible to pass a
            function of one argument -- the PresentedRow instance representing the current record.
            This function must return a field identifier or None.
          kwargs -- arguments passed to the parent class
         
        """
        assert mode in (self.MODE_EDIT, self.MODE_INSERT, self.MODE_VIEW)
        new = mode == self.MODE_INSERT
        super_(EditForm)._init_attributes(self, _new=new, **kwargs)
        self._mode = mode
        self._focus_field = focus_field or self._view.focus_field()
        # Other attributes
        self._fields = []

    def _set_focus_field(self, event=None):
        """Inicalizuj dialog nastaven�m hodnot pol��ek."""
        if self._focus_field:
            if callable(self._focus_field):
                focused = self._focus_field(self._row)
            else:
                focused = self._focus_field
            if find(focused, self._fields, key=lambda f: f.id()):                
                f = self._field(focused)
        else:
            f = find(True, self._fields, key=lambda f: f.enabled())
            if f is None:
                f = self._fields[0]
        f.set_focus()

    def _create_form_parts(self, sizer):
        # Create all parts and add them to top-level sizer.
        sizer.Add(self._create_title_bar(), 0, wx.EXPAND)
        sizer.Add(self._create_form_controls(), 1, wx.EXPAND)

    def _create_form_controls(self):
        # Create the actual form controls according to the layout.
        panel = wx.ScrolledWindow(self, style=wx.TAB_TRAVERSAL)
        self._fields = [InputField.create(panel, self._row, id, guardian=self,
                                          readonly=self._mode == self.MODE_VIEW)
                        for id in self._view.layout().order()
                        if self._view.field(id).width() != 0]
        # Now create the layout groups.
        group = self._create_group(panel, self._view.layout().group())
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(group, 0, wx.ALIGN_CENTER|wx.LEFT|wx.RIGHT, 8)
        panel.SetScrollRate(20, 20)
        panel.SetSizer(sizer)
        sizer.Fit(panel)
        return panel

    def _field(self, id):
        f = find(id, self._fields, key=lambda f: f.id())
        assert f is not None, (_("Unknown field:"), id)
        return f

    def _create_button(self, parent, item):
        b = wx.Button(parent, -1, item.label())
        b.Enable(item.active_in_popup_form() \
                 or not isinstance(self, PopupForm))
        if item.width() is not None:
            width = dlg2px(b, 4*item.width())
            height = b.GetSize().GetHeight()
            b.SetMinSize((width, height))
        if item.tooltip() is not None:
            b.SetToolTipString(item.tooltip())
        def create_handler(handler):
            def _handler(event):
                handler(self._row)
                busy_cursor(False)
            return _handler
        wx_callback(wx.EVT_BUTTON, self, b.GetId(),
                    create_handler(item.handler()))
        return b
        
    def _create_group(self, parent, group):
        """Vytvo� skupinu vstupn�ch pol��ek podle specifikace.

        Argumenty:

          group -- instance 'GroupSpec', kter� m� b�t zpracov�na.

        Ka�dou posloupnost za sebou n�sleduj�c�ch pol��ek seskup� pod sebe
        a pro ka�dou vno�enou skupinu pol��ek zavol� sebe sama rekurzivn�.
        V�sledek potom poskl�d� do instance 'wx.BoxSizer', kterou vytvo��.

        Specifikace skupiny ovliv�uje zp�sob seskupen�:
        horizont�ln�/vertik�ln�, mezery mezi pol��ky, skupinami
        atd. Viz. dokuewntace t��dy 'GroupSpec'

        Vrac�: 'wx.BoxSizer' napln�n� pol��ky a vno�en�mi skupinami.

        """
        orientation = orientation2wx(group.orientation())
        if group.label() is not None:
            box = wx.StaticBox(parent, -1, group.label())
            sizer = wx.StaticBoxSizer(box, orientation)
        else:
            sizer = wx.BoxSizer(orientation)
        # ka�d� souvisl� sled pol��ek ukl�d�m do pole a teprve nakonec je
        # poskl�d�m metodou self._pack_fields() a vlo��m do sizeru t�to
        # skupiny
        pack = []
        space = dlg2px(parent, group.space())
        gap = dlg2px(parent, group.gap())
        border = dlg2px(parent, group.border())
        border_style = border_style2wx(group.border_style())
        for item in group.items():
            if is_string(item):
                if self._view.field(item).width() == 0:
                    continue
                item = self._field(item)
            if group.orientation() == Orientation.VERTICAL \
                   and (isinstance(item, InputField)
                        and not item.spec().compact() \
                        or isinstance(item, Button)):
                # This field will become a part of current pack.
                pack.append(item)
                continue
            if len(pack) != 0:
                # Add the latest pack into the sizer (if there was one).
                sizer.Add(self._pack_fields(parent, pack, space, gap),
                          0, wx.ALIGN_TOP|border_style, border)
                pack = []
            if isinstance(item, GroupSpec):
                x = self._create_group(parent, item)
            elif isinstance(item, InputField):
                if item.spec().compact():
                    # This is a compact field (not a part of the pack).
                    x = wx.BoxSizer(wx.VERTICAL)
                    x.Add(item.label(), 0, wx.ALIGN_LEFT)
                    x.Add(item.widget())
                else:
                    # This only happens in a HORIZONTAL group.
                    x = self._pack_fields(parent, (item,), space, gap)
            else:
                x = self._create_button(parent, item)
            sizer.Add(x, 0, wx.ALIGN_TOP|border_style, border)
        if len(pack) != 0:
            # p�idej zbyl� sled pol��ek (pokud n�jak� byl)
            sizer.Add(self._pack_fields(parent, pack, space, gap),
                      0, wx.ALIGN_TOP|border_style, border)
        # pokud m� skupina or�mov�n�, p�id�me ji je�t� do sizeru s horn�m
        # odsazen�m, jinak je horn� odsazen� p��li� mal�.
        if group.label() is not None:
            s = wx.BoxSizer(orientation)
            s.Add(sizer, 0, wx.TOP, 2)
            sizer = s
        return sizer

    def _pack_fields(self, parent, items, space, gap):
        """Sestav skupinu pod sebou um�st�n�ch pol��ek/tla��tek do gridu.

        Argumenty:

          items -- sekvence identifik�tor� pol��ek nebo instanc� Button.
          space -- mezera mezi ovl�dac�m prvkem a labelem pol��ka v dlg units;
            integer
          gap -- mezera mezi jednotliv�mi pol��ky v dlg units; integer

        Pro ka�d� prvek skupiny vytvo�� tla��tko nebo pol��ko
        'inputfield.InputField' a p�id� jeho label a widget do vytvo�en�
        instance 'wx.FlexGridSizer'.

        Vrac�: instanci 'wx.FlexGridSizer' napln�nou pol��ky a tla��tky.

        """
        grid = wx.FlexGridSizer(len(items), 2, gap, space)
        for item in items:
            if isinstance(item, Button):
                button = self._create_button(parent, item)
                style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                label = wx.StaticText(parent, -1, "",
                                      style=wx.ALIGN_RIGHT)
                grid.Add(label, 0, style, 2)
                grid.Add(button)                
            else:    
                if item.height() > 1:
                    style = wx.ALIGN_RIGHT|wx.ALIGN_TOP|wx.TOP
                else:
                    style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                grid.Add(item.label(), 0, style, 2)
                grid.Add(item.widget())
        return grid

    def _signal_update(self):
        f = current_form()
        if isinstance(f, Refreshable):
            f.refresh()

    def _refresh(self, when=None):
        self.Refresh()

    def _commit_form(self, close=True):
        # Re-validate all fields.
        for f in self._fields:
            if self._mode == self.MODE_INSERT or self._row.field_changed(f.id()):
                if not f.validate():
                    f.set_focus()
                    return False
        # Ov��en� integrity z�znamu (funkce check).
        failed_id = self._check_record(self._row)
        if failed_id:
            f = self._field(failed_id)
            if f:
                f.set_focus()
            else:
                log(OPERATIONAL, "Unknown field returned by check():", failed_id)
            return False
        transaction = self._transaction
        # Vytvo�en� datov�ho ��dku.
        rdata = self._record_data(self._row)
        if self._mode == self.MODE_INSERT:
            log(ACTION, 'Inserting record...')
            def op():
                return self._data.insert(rdata, transaction=transaction)
        elif self._mode == self.MODE_EDIT:
            log(ACTION, 'Updating record...')
            def op():
                return self._data.update(self._current_key(), rdata, transaction=transaction)
        else:
            raise ProgramError("Can't commit in this mode:", self._mode)
        # Proveden� operace
        def set_point_op():
            transaction.set_point('commitform')
        def cut_op():
            transaction.cut('commitform')
        if transaction is not None:
            success, result = db_operation(set_point_op)
        else:
            success = True
        if success:
            success, result = db_operation(op)
        if success and result[1]:
            new_row = result[0]
            original_row = copy.copy(self._row)
            if new_row is None:
                new_row = self._row.row()
            self._row.set_row(new_row, reset=True)
            self._signal_update()
            if self._mode == self.MODE_INSERT:
                log(ACTION, 'Record inserted')
            else:
                log(ACTION, 'Record updated')
            cleanup = self._view.cleanup()
            if cleanup is not None:
                cleanup(self._row, original_row)
            if close:
                self._result = self._row
                self.close()
            if self._governing_transaction is None and self._transaction is not None:
                def commit():
                    self._transaction.commit()
                db_operation(commit)
                if close:
                    self._transaction = None
                else:
                    self._transaction = self._default_transaction()
                self._row.set_transaction(self._transaction)
            return True
        else:
            if transaction is not None:
                success, __ = db_operation(cut_op)
            else:
                success = True
            if success:
                msg = _("Ulo�en� z�znamu se nezda�ilo")
            else:
                msg = _("Transakce p�eru�ena, nelze pokra�ovat")
            if type(result) == type(()) and \
               isinstance(result[0], (str, unicode)):
                msg = "%s\n\n%s" % (result[0], msg)
            run_dialog(Error, msg)
            return False

    def title(self):
        """Vra� n�zev formul��e jako �et�zec."""        
        return self._view.layout().caption()

    def size(self):
        """Vra� skute�nou velikost formul��e (bez ohledu na aktu�ln� velikost).

        Vr�cen� hodnota reprezentuje minim�ln� velikost formul��e, tak aby byly
        v�echny jeho prvky viditeln�.  Skute�n� velikost m��e b�t men��, nebo
        v�t�� v z�voslosti na velikost okna, ve kter�m je formul�� zobrazen.
        
        """
        return self._size

    def changed(self):
        """Vra� pravdu, pokud byla data zm�n�na od posledn�ho ulo�en�."""
        return self._row.changed()

    def _exit_check(self):
        if self.changed():
            q = _("Data byla zm�n�na a nebyla ulo�ena!") + "\n" + \
                _("Opravdu chcete uzav��t formul��?")
            if not run_dialog(Question, q):
                return False
        return True

    def _can_commit_record(self):
        return self._mode != self.MODE_VIEW
    
    def _cmd_commit_record(self):
        return self._commit_form()

    def _cmd_navigate(self, back=False):
        if self._mode != self.MODE_VIEW:
            # Vygeneruj ud�lost navigace mezi pol��ky.
            w = wx_focused_window()
            if not w:
                self._fields[0].set_focus()
                w = wx_focused_window()
            if w:
                flags = not back and wx.NavigationKeyEvent.IsForward or 0
                w.Navigate(flags=flags)

    
class PopupEditForm(PopupForm, EditForm):
    """Stejn� jako 'EditForm', av�ak v�popup podob�."""

    DESCR = _("edita�n� formul��")
    
    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        EditForm.__init__(self, parent, *args, **kwargs)
        if self._inserted_data is not None:
            self._load_next_row()
        size = copy.copy(self.size())
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 50))
        self.SetSize(size)
        p = parent
        while not p.GetTitle() and p.GetParent():
            p = p.GetParent()
        parent.SetTitle('%s: %s' % (p.GetTitle(), self.title()))

    def _default_transaction(self):
        return pytis.data.DBTransactionDefault(config.dbconnection)
        
    def _init_attributes(self, inserted_data=None, multi_insert=True, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          inserted_data -- allows to pass a sequence of 'pytis.data.Row' instances to be inserted.
            The form is then gradually prefilled by values of these rows and the user can
            individually accept or skip each row.

          multi_insert -- boolean flag indicating whether inserting multiple values is permitted.
            This option is only relevant in insert mode.  False value will disable this feature and
            the `Next' button will not be present on the form.

          kwargs -- arguments passed to the parent class
            
        """
        EditForm._init_attributes(self, **kwargs)
        assert inserted_data is None or self._mode == self.MODE_INSERT, (inserted_data, self._mode)
        assert isinstance(multi_insert, bool), multi_insert
        assert multi_insert or inserted_data is None, (multi_insert, inserted_data)
        self._inserted_data = inserted_data
        self._inserted_data_pointer = 0
        self._multi_insert = multi_insert

    def _create_form_parts(self, sizer):
        # Create all parts and add them to top-level sizer.
        caption = self._create_caption(self, size=18)
        panel = self._create_form_controls()
        buttons = self._create_buttons()
        status_bar = self._create_status_bar()
        # Add parts to the sizer.
        sizer.Add(caption, 0, wx.ALIGN_CENTER|wx.ALL, 8)
        sizer.Add(panel, 1, wx.EXPAND)
        sizer.Add(buttons, 0, wx.ALIGN_CENTER)
        sizer.Add(status_bar, 0, wx.EXPAND)

    def _create_status_bar(self):
        # We use our own statusbar implementation
        spec = (('message', None, _("Oznamovac� oblast")),)
        if self._inserted_data is not None:
            spec += (('progress', 9, _("Ukazatel pozice hromadn�ho vkl�d�n�")),)
        box = wx.BoxSizer()
        self._status_fields = dict(
            [(id, self._create_status_bar_field(box, width, descr))
             for id, width, descr in spec])
        return box

    def _create_status_bar_field(self, sizer, width, descr):
        panel = wx.Panel(self, -1, style=wx.SUNKEN_BORDER)
        panel.SetToolTipString(descr)
        box = wx.BoxSizer()
        panel.SetSizer(box)
        panel.SetAutoLayout(True)
        field = wx.StaticText(panel, -1, '', style=wx.ALIGN_LEFT)
        box.Add(field, 1, wx.EXPAND|wx.ALL, 2)
        box.Fit(panel)
        if width is not None:
            width = dlg2px(field, 4*width)
            height = field.GetSize().GetHeight()
            field.SetMinSize((width, height))
            expansion = 0
        else:
            expansion = 1
        sizer.Add(panel, expansion, wx.EXPAND)
        return field

    def _load_next_row(self):
        data = self._inserted_data
        if data is None or self._inserted_data_pointer > 0:
            self._row.set_row(None, reset=True, prefill=self._prefill)
        if data is not None:
            i = self._inserted_data_pointer
            self._select_row(None)
            if i < len(data):
                self.set_status('progress', "%d/%d" % (i+1, len(data)))
                self._inserted_data_pointer += 1
                ok_button = wx.FindWindowById(wx.ID_OK, self._parent)
                ok_button.Enable(i == len(data)-1)
                for id, value in data[i].items():
                    self._row[id] = pytis.data.Value(self._row[id].type(), value.value())
            else:
                self.set_status('progress', '')
                run_dialog(Message, _("V�echny z�znamy byly zpracov�ny."))
                self._inserted_data = None
        self._set_focus_field()

    def _exit_check(self):
        i = self._inserted_data_pointer
        data = self._inserted_data
        if data is not None and i <= len(data):
            msg = _("Je�t� nebyly zpracov�ny v�echny ��dky "
                    "vstupn�ch dat.\n"
                    "Chcete opravdu ukon�it vkl�d�n�?")
            if not run_dialog(Question, msg, default=False):
                return False
        return super(PopupEditForm, self)._exit_check()

    def _on_next_button(self, event):
        result = self._commit_form(close=False)
        if result:
            message(_("Z�znam ulo�en"))
            refresh()
            self._load_next_row()

    def _on_skip_button(self, event):
        i = self._inserted_data_pointer
        if self._inserted_data is None:
            message(_("Nen� dal�� z�znam"), beep_=True)
        else:
            message(_("Z�znam %d/%d p�esko�en") % (i, len(self._inserted_data)))
            self._load_next_row()
    
    def _buttons(self):
        buttons = ({'id': wx.ID_OK,
                    'toottip': _("Ulo�it z�znam a uzav��t formul��"),
                    'handler': lambda e: self._commit_form(),
                    'default': True},
                   {'id': wx.ID_CANCEL,
                    'toottip': _("Uzav��t formul�� bez ulo�en� dat"),
                    'handler': lambda e: self.close()})
        if self._mode == self.MODE_INSERT and self._multi_insert:
            buttons += ({'id': wx.ID_FORWARD,
                         'label': _("Dal��"),
                         'toottip': _("Ulo�it z�znam a reinicializovat formul��"
                                      " pro vlo�en� dal��ho z�znamu"),
                         'handler': self._on_next_button},)
        if self._inserted_data is not None:
            buttons += ({'label': _("P�esko�it"),
                         'toottip': _("P�esko�it tento z�znam bez ulo�en�"),
                         'handler': self._on_skip_button},)
        return buttons
        
    def _create_buttons(self):
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        for b in self._buttons():
            button = wx.Button(self, b.get('id', -1), b.get('label', ""))
            wx_callback(wx.EVT_BUTTON, self, button.GetId(), b['handler'])
            button.SetToolTipString(b.get('toottip'))
            if b.get('default'):
                button.SetDefault()
            sizer.Add(button, 0, wx.ALL, 20)
        return sizer

    def _cleanup(self):
        super(PopupEditForm, self)._cleanup()

    def can_command(self, command, **kwargs):
        if command.handler() in (LookupForm, RecordForm):
            return False
        return super(PopupEditForm, self).can_command(command, **kwargs)
        
    def run(self):
        if self._mode == self.MODE_EDIT:
            key = self._current_key()
        else:
            key = None
        return PopupForm.run(self, lock_key=key)

    def set_status(self, field, message):
        if self._status_fields.has_key(field):
            self._status_fields[field].SetLabel(unicode(message or ''))
            return True
        else:
            return False

    def set_row(self, row):        
        if self._transaction is None:
             self._transaction = pytis.data.DBTransactionDefault(config.dbconnection) 
        super(PopupEditForm, self).set_row(row)
        

class PopupInsertForm(PopupEditForm):
    
    DESCR = _("vkl�dac� formul��")
    
    def _init_attributes(self, **kwargs):
        super_(PopupInsertForm)._init_attributes(self, mode=EditForm.MODE_INSERT, **kwargs)
        
        
class ShowForm(EditForm):
    """Formul�� pro zobrazen� n�hledu.

    Layout je stejn� jako u edita�n�ho formul��e (resp. 'EditForm'),
    pouze titulek m� stejn� vzhled, jako titulek formul��� typu 'ListForm'.
    Ur�en pro zobrazen� v du�ln�m formul��i.

    """

    DESCR = _("n�hledov� formul��")

    def _init_attributes(self, mode=EditForm.MODE_VIEW, select_row=0,**kwargs):
        super_(ShowForm)._init_attributes(self, mode=mode, select_row=select_row, **kwargs)
        
    def changed(self):
        # Since the row is not reset when the current record changes, it would report a change...
        return False

        
class BrowsableShowForm(ShowForm):
    """Listovac� formul�� pro zobrazen� n�hledu.

    Formul�� je needitovateln�, ale umo��uje pohyb po z�znamech tabulky, nad
    kterou je vytvo�en, vyhled�v�n� atd.  Z u�ivatelsk�ho hlediska jde v
    podstat� o redukci prohl�ec�ch mo�nost� formul��e typu 'BrowseForm' na
    jeden z�znam zobrazen� v Layoutu edita�n�ho formul��e.
    
    """

    def _cmd_next_record(self, back=False):
        current_row = self.current_row()
        if current_row:
            row_number = self._get_row_number(current_row.row())
        else:
            row_number = 0
        if not back:
            row_number += 1
            if row_number == self._lf_select_count:
                message(_("Posledn� z�znam"), beep_=True)
                return
        else:
            if row_number == 0:
                message(_("Prvn� z�znam"), beep_=True)
                return
            row_number -= 1
        self._select_row(self._find_row_by_number(row_number))

    def _select_row(self, row, quiet=False):
        result = super(BrowsableShowForm, self)._select_row(row, quiet=quiet)
        current_row = self.current_row()
        total = self._lf_select_count
        if current_row and total:
            n = self._get_row_number(current_row)
            position = "%d/%d" % (n is not None and n+1 or 0, total)
            set_status('list-position', position)
        return result
                     

