# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
s#
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

"""Interaktivní formuláře pro práci s daty.

Základem všech formulářů je třída 'Form'.  Dále tento modul obsahuje její
potomky pro konkrétní použití -- jednoduché editační formuláře (pro zobrazení a
editaci jednoho záznamu).  Jednoduché seznamové formuláře a duální formuláře
jsou v oddělených modulech 'list' a 'dualform'.  Blíže viz dokumentace
jednotlivých tříd.

"""

import time
import pytis.data
import pytis.output
from pytis.presentation import PresentedRow
from pytis.form import *
import wx

class Form(Window, KeyHandler, CallbackHandler, CommandHandler):
    """Společná nadtřída formulářů.

    Formulář si podle jména specifikace předaného konstruktoru vyžádá od
    resolveru příslušnou datovou a prezentační specifikaci.  Z datové
    specifikace vytvoří datový objekt (instance třídy odvozené z
    'pytis.data.Data').  Datový objekt a prezentační specifikace jsou potom
    uloženy ve formě atributů instance formuláře ('self._view' a 'self._data')

    Instance tříd odvozených z této třídy jsou potom vytvářeny na základě
    interpretace prezentační specifikace a pracují s daty s pomocí datového
    objektu a jeho API (které je nezávislé na konkrétním zdroji dat).

    Form je potomkem 'Window', díky čemuž je možné jej ukládat na zásobník oken
    aplikace a provádět další operace, jako zaostřování, skrývání, zobrazování
    apod.

    Používané specifikační funkce:

      print_spec -- sekvence dvojic (POPIS, SOUBOR), kde POPIS je string se
        stručným slovním popisem specifikace (využívaným například jako titulek
        položky menu) a SOUBOR je string udávající jméno souboru se
        specifikací, relativní k adresáři s definičními soubory, bez přípony

    """
    _STATUS_FIELDS = ()
    DESCR = None

    def _get_command_handler_instance(cls):
        return current_form(inner=False)
    _get_command_handler_instance = classmethod(_get_command_handler_instance)

    def __init__(self, parent, resolver, name, guardian=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:
        
          parent -- instance 'wxFrame', do kterého formulář patří
          resolver -- resolver jmenných odkazů, instance 'pytis.util.Resolver' 
          name -- jméno specifikačního souboru pro resolver; string
          guardian -- formulář (instance libovolné třídy), ve kterém je
            formulář vložen z hlediska struktury aplikace; není-li zadán, je
            použit 'parent'.  Tento parametr je využíván například při zasílání
            klávesových událostí \"nahoru\".  Typicky je to formulář, který
            tuto instanci vytváří.
          kwargs -- viz níže.

        Resolver je použit k získání datové a prezentační specifikace a
        následnému vytvoření datového objektu. Ten je potom společně s
        prezentační specifikací uložen v podobě atributů vytvářené instance.

        Odkaz na resolver samotný je také zapamatován pro pozdější použití
        (vytváření dalších formulářů).

          
        Inicializace je rozdělena do několika kroků.  Nejprve jsou zpracováný
        všechny argumenty společné všem formuářovým třídám.  Ty zpracovává
        konstruktor bázové třídy 'Form'.  Jejich zpracování by nemělo být
        předefinováváno v odvozených třídách a ani žádné další argumenty by
        neměly být přidávány.  Konstruktor je možno předefinovat a provádět
        nějaké doplňující akce, ale argumenty by neměly být měněny.

        Po zpracování společných argumwentů jsou načteny specifikace a vytvořen
        datový objekt.

        Poté jsou zpracovávány klíčové argumenty.  Každá odvozená třída může
        definovat své vlastní klíčové argumenty.  Ty potom zpracuje
        předefinováním metody '_init_attributes()'.  Ta již může využívat
        inicializovaného datového objetu a specifikací a případně initializovat
        další atributy třídy.  Metoda '_init_attributes()' by měla vždy
        zpracovávat pouze klíčové argumenty, které jsou specifické pro danou
        třídu.  Zbylé předá metodě rodičovské třídy konstrukcí **kwargs.  Takto
        by mělo být zaručeno, že dojde postupně ke zpracování všech argumentů.
        Pokud nějaké zbydou, vyvolá bázová třída výjimku 'AssertionError'.

        Teprve po zpravování argumentů konstruktoru a inicializaci atributů je
        vytvářen vlastní obsah formuláře (viz. '_create_form()').  Toto by mělo
        být dodržováno i v odvozených třídách.
        
        """
        self._parent = parent
        self._resolver = resolver
        self._name = name
        self._guardian = guardian or parent
        Window.__init__(self, parent)
        KeyHandler.__init__(self)
        CallbackHandler.__init__(self)
        start_time = time.time()
        spec_args = kwargs.get('spec_args', {})
        try:
            self._view = self._create_view_spec(**spec_args)
            self._data = self._create_data_object(**spec_args)
        except ResolverError:
            log(OPERATIONAL, 'Chyba z resolveru', format_traceback())
            throw('form-init-error')
        log(EVENT, 'Specifikace načteny za %.3fs' % (time.time() - start_time)) 
        self._init_attributes(**kwargs)
        self._result = None
        start_time = time.time()
        self._create_form()
        log(EVENT, 'Formulář sestaven za %.3fs' % (time.time() - start_time))

    def _init_attributes(self, spec_args={}):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.
        
        Argumenty:
        
          kwargs -- klíčové argumenty konstruktoru (viz dokumentace metody
            '__init__()').

        
        Tato metoda je volána po základní inicializaci instance (především
        načtení specifikace a inicializaci datového objektu.  Metody
        vytvářející konkrétní prvky uživatelského rozhraní formuláře (například
        '_create_form()'), jsou však volány až poté.  Zde by měly být především
        zpracovány všechny klíčové argumenty konstruktoru (viz dokumentace
        metody '__init__()' a inicializovány atributy instance.

        """
        key = self._form_state_key()
        self._form_state = config.form_state.get(key)
        if not isinstance(self._form_state, types.DictType):
            self._form_state = config.form_state[key] = {}
        self._initial_form_state = copy.copy(self._form_state)

    def _create_view_spec(self, **kwargs):
        spec = self._resolver.get(self._name, 'view_spec', **kwargs)
        assert isinstance(spec, ViewSpec)
        return spec        

    def _create_data_object(self, **kwargs):
        name = self._name
        data_spec = self._resolver.get(name, 'data_spec', **kwargs)
        import config
        if __debug__ and config.server:
            import pytis.remote
        else:    
            import pytis.data    
        assert isinstance(data_spec, pytis.data.DataFactory)
        assert isinstance(data_spec, pytis.data.DataFactory) or \
               isinstance(data_spec, pytis.remote.RemoteDataFactory)
        op = lambda : data_spec.create(dbconnection_spec=config.dbconnection)
        success, data_object = db_operation(op)
        if not success:
            throw('form-init-error')
        return data_object

    def _create_form(self):
        # Build the form from parts
        sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_form_parts(sizer)
        self.SetSizer(sizer)
        sizer.Fit(self) # Set the size of window `self' to size of the sizer.

    def _create_form_parts(self, sizer):
        pass

    def __str__(self):
        return '<%s for "%s">' % (self.__class__.__name__, self._name)

    def __repr__(self):
        return str(self)

    def _form_state_key(self):
        return self.__class__.__name__+'/'+self._name
    
    def _get_state_param(self, name, default=None, cls=None):
        param = self._form_state.get(name, default)
        if cls is not None and not isinstance(param, cls):
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

    # Zpracování příkazů

    def _can_reload_form_state(self):
        return self._form_state != self._initial_form_state
    
    def _cmd_reload_form_state(self):
        self._form_state = copy.copy(self._initial_form_state)
        config.form_state[self._form_state_key()] = self._form_state
        self._on_form_state_change()
        if isinstance(self, Refreshable):
            self.refresh()

    def _can_reset_form_state(self):
        return self._form_state != {}
        
    def _cmd_reset_form_state(self):
        self._form_state = {}
        config.form_state[self._form_state_key()] = self._form_state
        self._on_form_state_change()
        if isinstance(self, Refreshable):
            self.refresh()
        
    def _cmd_help(self):
        help(self.help_name())

    def _cmd_leave_form(self):
        return self.close()

    # Veřejné metody
    
    def name(self):
        """Vrať název specifikace formuláře."""
        return self._name

    def help_name(self):
        return self._name.replace(':','-')

    def descr(self):
        """Vrať textový popis typu formuláře jako řetězec."""
        if self.DESCR is not None:
            return self.DESCR
        else:
            return self.__class__.__name__
        
    def title(self):
        """Vrať titulek ze specifikace formuláře jako řetězec."""
        return self._view.title()

    def guardian(self):
        """Vrať guardian zadané v konstruktoru (nebo parent)."""
        return self._guardian

    def check_permission(self, perm, quiet=True):
        """Vrať pravdu, pokud má uživatel daná práva k datovému objektu.

        Argumentem je konstanta  třídy 'pytis.data.Permission::'.

        """
        VIEW   = pytis.data.Permission.VIEW
        INSERT = pytis.data.Permission.INSERT
        UPDATE = pytis.data.Permission.UPDATE
        DELETE = pytis.data.Permission.DELETE
        EXPORT = pytis.data.Permission.EXPORT
        if perm == DELETE:
            result = self._data.accessible(None, perm)
        else:
            for col in self._data.columns():
                if self._data.accessible(col.id(), perm):
                    result = True
                    break
            else:
                result = False
        if not result and not quiet:
            msg = {
                VIEW:   "Nemáte právo k zobrazení formuláře.",
                INSERT: "Nemáte právo vložit nový záznam.",
                UPDATE: "Nemáte právo změnit existující záznam.",
                DELETE: "Nemáte právo smazat existující záznam.",
                EXPORT: "Nemáte právo k exportu do CSV.",
                }[perm]
            message(msg, beep_=True)
        return result

    def set_status(self, field, message):
        """Zobraz zprávu `message' v poli `id' stavové řádky formuláře.

        Má-li formulář vlastní stavovou řádku a v ní pole `id' zobraz v něm
        danou zprávu a vrať pravdu.  V opačném případě vrať nepravdu.

        """
        return False

    def save(self):
        self._saved_state = map(lambda id: (id, get_status(id)),
                                self._STATUS_FIELDS)

    def restore(self):
        for id, message in self._saved_state:
            set_status(id, message, log_=False)

    def _cleanup(self):
        super(Form, self)._cleanup()
        for id in self._STATUS_FIELDS:
            set_status(id, '')
    

class InnerForm(Form):
    """Formulř, který zpracuje příkazy samostatně i unvitř duálního formuláře.

    Tato formulářová třída je zde především kvůli definici a zpracování
    příkazů.  Pokud je aktuálním formulářem jednoduchý formulář, je zpracování
    příkazu předáno tomuto formuláři.  Pokud je však aktuálním formulářem
    duální formulář, je třeba rozhodnout, zda bude příkaz zpracován přímo
    duálním formulářem, nebo jeho aktivním podformulářem.  Příkazy třídy 'Form'
    jsou zpracovávány vždy formulářem nejvyšší úrovně (duálním formulářem
    samotným, pokud je aktuálním formulářem duální formulář).

    Příkazy definované touto třídou a třídami z ní odvozenými jsou však vždy
    předávány aktivnímu vnitřnímu formuláři.
    
    """
    def _get_command_handler_instance(cls):
        return current_form()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    

class Refreshable:
    """Třída zajišťující existenci metody 'refresh()' s daným významem.

    Tuto třídu by měly dědit všechny formuláře, které mají být obnoveny při
    změně dat (typicky způsobené jiným formulářem výše na zásobníku rámců).
    
    """

    DOIT_IMMEDIATELY = 'DOIT_IMMEDIATELY'
    """Konstanta pro 'refresh()' pro okamžitý update.

    Není-li seznam právě editován, je update proveden okamžitě.  Jinak je
    uživatel dotázán, zda má být update proveden ihned; odpoví-li uživatel
    negativně, je update proveden až po ukončení editace.

    """
    DOIT_AFTEREDIT = 'DOIT_AFTEREDIT'
    """Konstanta pro 'refresh()' pro update po skončení editace.

    Není-li seznam právě editován, je update proveden okamžitě.  Jinak je
    proveden až po ukončení editace.
    
    """
    DOIT_IFNEEDED = 'DOIT_IFNEEDED'
    """Konstanta pro 'refresh()' pro podmíněný update.

    Update je proveden pouze tehdy, je-li známo, že došlo ke změně dat.
    V takovém případě je proveden okamžitě pouze tehdy, jestliže seznam není
    práve editován a v poslední době nebyl proveden žádný jiný update;
    v opačném případě je update odložen \"až na vhodnější chvíli\" (nicméně
    proveden bude).

    """
    _block_refresh = 0

    def block_refresh(cls, function, *args, **kwargs):
        """Zablokuj veškerý refresh po dobu provádění funkce 'function'.

        Všechny argumenty jsou předány volané funkci.
        
        Vrací: výsledek vrácený volanou funkcí.

        Refresh je zablokován globálně, pro všechny existující formuláře.
        
        """
        Refreshable._block_refresh += 1
        try:
            result = function(*args, **kwargs)
        finally:
            Refreshable._block_refresh -= 1
        return result
    block_refresh = classmethod(block_refresh)
    
    def refresh(self, when=None):
        """Aktualizuj data formuláře z datového zdroje.

        Překresli data ve formuláři v okamžiku daném argumentem 'when'.

        Argumenty:

          when -- určuje, zda a kdy má být aktualizace provedena, musí to být
            jedna z 'DOIT_*' konstant třídy.  Implicitní hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrací: Pravdu, právě když byla aktualizace provedena.

        """
        level = Refreshable._block_refresh
        if level == 0:
            self._refresh(when=when)
        elif level > 0:
            log(OPERATIONAL, "Refresh neproveden kvůli blokaci:", level)
        else:
            raise ProgramError("Nepřípustná hodnota _block_refresh:", level)

    def _refresh(self, when=None):
        """Proveď vlastní refresh.

        Tuto metodu nechť předefinují odvozené třídy.

        """
        pass


class PopupForm:
    """Formulář nacházející se v samostatném framu.

    Tato třída je určena k vložení mezi předky třídy, jejíž instance mají být
    vytvářeny v samostatných framech.  Pro získání framu slouží metoda
    '_popup_frame'.

    """
    def _popup_frame(self, parent):
        """Vrať frame instance.

        Pokud frame ještě neexistuje, vytvoř jej.

        Argumenty:
        
          parent -- rodičovské okno, instance 'wx.Window'

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
        if self._exit_check():
            self.defocus()
            event.Skip()
        else:
            event.Veto()

    def close(self, force=False):
        # Tím se zavolá _on_frame_close() a tam provedeme zbytek.
        return self._popup_frame_.Close(force=force)
        
    def run(self):
        """Zobraz formulář jako modální dialog."""
        unlock_callbacks()
        frame = self._parent
        frame.SetTitle(self.title())
        frame.SetClientSize(self.GetSize())
        frame.ShowModal()
        result = self._result
        self._close(force=True)
        return result


class TitledForm:
    """Mix-in třída pro formuláře s titulkem.
    
    Lze využít buďto pouze metodu '_create_caption()', která vytváří samotný
    text titulku, nebo metodu '_create_title_bar()', která přidává 3d panel.

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
        return InfoWindow(_("Nápověda pro %s") % self._view.title(),
                          text=self._view.description(),
                          format=TextFormat.WIKI)

    def _on_print_menu(self, event):
        button = event.GetEventObject()
        menu = Menu('', self._print_menu).create(button, self._get_keymap())
        button.PopupMenu(menu, (0, button.GetSize().y))
        menu.Destroy()
        
    def _create_print_menu(self):
        # Vrať seznam položek tiskového menu.
        name = self._name
        try:
            print_spec = self._resolver.get(name, 'print_spec')
        except ResolverSpecError:
            print_spec = None
        if not print_spec:
            print_spec = ((_("Výchozí"), os.path.join('output', name)),)
        self._print_menu = [MItem(title, command=InnerForm.COMMAND_PRINT,
                                  args=dict(print_spec_path=path,
                                            _command_handler=self))
                            for title, path in print_spec]

    def _create_title_bar(self, description=None):
        """Vytvoř 3d panel s nadpisem formuláře."""
        panel = wx.Panel(self, -1, style=wx.RAISED_BORDER)
        caption = self._create_caption(panel)
        bmp = wx.ArtProvider_GetBitmap(wx.ART_PRINT, wx.ART_TOOLBAR, (16,16))
        button = wx.BitmapButton(panel, -1, bmp, style=wx.NO_BORDER)
        wx_callback(wx.EVT_BUTTON, button, button.GetId(), self._on_print_menu)
        self._create_print_menu()
        box = wx.BoxSizer()
        box.Add(caption, 1, wx.EXPAND|wx.ALL, self._TITLE_BORDER_WIDTH)
        box.Add(button)
        if description:
            descbmp = wx.ArtProvider_GetBitmap(wx.ART_HELP_BOOK, wx.ART_TOOLBAR,
                                               (16,16))
            descbutton = wx.BitmapButton(panel, -1, descbmp, style=wx.NO_BORDER)
            wx_callback(wx.EVT_BUTTON, descbutton, descbutton.GetId(),
                        self._on_show_description)
            descbutton.SetToolTipString(description)
            box.Add(descbutton)
        panel.SetSizer(box)
        panel.SetAutoLayout(True)        
        box.Fit(panel)
        return panel


class RecordForm(InnerForm):
    """Formulář schopný nějakým způsobem zobrazit aktuální záznam."""

    CALL_SELECTION = 'CALL_SELECTION'
    """Konstanta callbacku výběru (změny aktuálního) záznamu.

    Argumentem callbackové funkce je nově vybraný záznam jako instance
    'PresentedRow'.
    
    """
    CALL_NEW_RECORD = 'CALL_NEW_RECORD'
    """Voláno po vložení nového záznamu.
    
    Argumentem callbackové funkce je nový záznam jako instance 'PresentedRow'.
    
    """

    def _init_attributes(self, prefill=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          prefill -- slovník řetězcových (uživatelských) hodnot, které mají být
            předvyplněny při inicializaci formuláře.
            
          kwargs -- argumenty předané předkovi

        """
        super_(RecordForm)._init_attributes(self, **kwargs)
        assert prefill is None or is_dictionary(prefill)
        self._prefill = prefill
        self._row = None

    def _on_field_change(self, field_id, value=None):
        # Signalizace změny hodnoty políčka z _row
        pass

    def _on_editability_change(self, field_id, editable):
        # Callback změny editovatelnosti políčka
        pass

    def _signal_update(self):
        pass

    def _find_row_by_number(self, row_number):
        # row_number začíná od 0
        def get_it():
            data = self._data
            data.rewind()
            data.skip(row_number)
            return data.fetchone()
        success, row = db_operation(get_it)
        if not success or not row:
            return None
        else:
            return row
    
    def _find_row_by_values(self, cols, values):
        """Vrať datový řádek odpovídající daným hodnotám.

        Arguemnty:

          cols -- sekvence názvů sloupců, které mají být prohledávány.
          values -- sekvence hodnot sloupců jako instancí 'pytis.data.Value' v
            pořadí odpovídajícím 'cols'.

        Pro obě sekvence platí, že pokud jsou jednoprvkové, mohou být hodnoty
        předány i přímo, bez obalení do sekvenčního typu.

        """
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        condition = apply(pytis.data.AND, map(pytis.data.EQ, cols, values))
        data = self._data
        def find_row(condition):
            n = data.select(condition)
            return data.fetchone()
        success, result = db_operation((find_row, (condition,)))
        return result

    def _find_row_by_key(self, key):
        """Vrať datový řádek odpovídající danému datovému klíči."""
        if key is None:
            return None
        success, row = db_operation(lambda : self._data.row(xtuple(key)))
        if success and row:
            return row
        else:
            return None
    
    def _get_row_number(self, row):
        """Vrať číslo řádku odpovídající dané instanci 'pytis.data.Row'.

        Pokud odpovídaící řádek není nalezen, vrať None.

        """
        eqs = [pytis.data.EQ(c.id(), row[c.id()]) for c in self._data.key()]
        condition = pytis.data.AND(*eqs)
        data = self._data
        data.rewind()
        success, result = db_operation(lambda: data.search(condition))
        if not success:
            return None
        elif result == 0:
            return None
        else:
            return result - 1
        
    def _select_row(self, row, quiet=False):
        # Naplň formulář daty z daného *datového* řádku
        raise ProgrammError("This method must be overridden.")

    def _current_key(self):        
        the_row = self.current_row()
        if the_row is not None:
            data_row = the_row.original_row(empty_as_none=True)
            if data_row is None:
                data_row = the_row.row()
            return data_row.columns([c.id() for c in self._data.key()])
        return None

    def _redirected_name(self, key):
        redirect = self._view.redirect()
        if redirect is not None:
            success, row = db_operation(lambda : self._data.row(key))
            if not success:
                raise ProgramError('Row read failure')
            name = redirect(row)
            if name is not None:
                assert isinstance(name, types.StringType)
                return name
        return None
    
    def _run_form(self, form, key):
        name = self._redirected_name(key) or self._name
        kwargs = self._new_form_kwargs()
        run_form(form, name, select_row=key, **kwargs)

    def _new_form_kwargs(self):
        return {}

    def _lock_record(self, key):
        success, locked = db_operation(lambda : self._data.lock_row(key),
                                       quiet=True)
        if success and locked != None:
            log(EVENT, 'Záznam je zamčen', locked)
            run_dialog(Message, _("Záznam je zamčen: %s") % locked)
            return False
        else:
            return True

    def _unlock_record(self):
        if self._data.locked_row():
            db_operation(lambda : self._data.unlock_row(), quiet=True)

    def _check_record(self, row):
        # Proveď kontrolu integrity dané instance PresentedRow.
        check = self._view.check()
        if check is not None:
            result = check(row)
            if result is not None:
                if is_sequence(result):
                    failed_id, msg = result
                    message(msg)
                else:
                    failed_id = result
                    # TODO: Tím bychom přepsali zprávu nastavenou uvnitř
                    # 'check()'.  Pokud ale žádná zpráva nebyla nastavena,
                    # uživatel netuší...
                    #message(_("Kontrola integrity selhala!"))
                log(EVENT, 'Kontrola integrity selhala:', failed_id)
                return failed_id
        return None

    def _record_data(self, row):
        rdata = [(f.id(), row[f.id()]) for f in row.fields()
                 if self._data.find_column(f.id()) is not None]
        return pytis.data.Row(rdata)

    def _row_copy_prefill(self, the_row):
        # Jde o to vytvořit kopii řádku, ale klíč nekopírovat.
        if the_row:
            keys = [c.id() for c in the_row.data().key()]
            prefill = [(k, the_row[k]) for k in the_row.keys() if k not in keys]
        else:
            prefill = {}
        return dict(prefill)

    # Zpracování příkazů.
    
    def _on_new_record(self, copy=False):
        if not self.check_permission(pytis.data.Permission.INSERT, quiet=False):
            return False
        import copy as copy_
        prefill = self._prefill and copy_.copy(self._prefill) or {}
        if copy:
            prefill.update(self._row_copy_prefill(self.current_row()))
        result = new_record(self._name, prefill=prefill)
        if result:
            self.select_row(result.row())
            self._run_callback(self.CALL_NEW_RECORD, result)
    
    def _can_edit_record(self):
        return self._current_key() is not None \
               and self.check_permission(pytis.data.Permission.UPDATE)

    def _on_edit_record(self):
        if not self.check_permission(pytis.data.Permission.UPDATE, quiet=False):
            return
        on_edit_record = self._view.on_edit_record()
        if on_edit_record is not None:
            on_edit_record(row=self.current_row())
            # TODO: _signal_update vyvolá refresh.  To je tu jen pro případ, že
            # byla uživatelská procedura ošetřena jinak než vyvoláním
            # formuláře.  Protože to samo už je hack, tak ať si raději také
            # tvůrce provádí refresh sám, protože tady je volán ve všech
            # ostatních případech zbytečně a zdržuje.
            self._signal_update()
        else:
            self._run_form(PopupEditForm, self._current_key())

    def _can_delete_record(self):
        return self.check_permission(pytis.data.Permission.DELETE)

    def _on_delete_record(self):
        if not self.check_permission(pytis.data.Permission.DELETE, quiet=False):
            return False
        # Ošetření uživatelské funkce pro mazání
        on_delete_record = self._view.on_delete_record()
        if on_delete_record is not None:
            condition = on_delete_record(row=self.current_row())
            if condition is None:
                return True
            assert isinstance(condition, pytis.data.Operator)
            op = lambda : self._data.delete_many(condition)
            log(EVENT, 'Mazání záznamu:', condition)
        else:
            msg = _("Opravdu chcete záznam zcela vymazat?")        
            if not run_dialog(Question, msg):
                log(EVENT, 'Mazání řádku uživatelem zamítnuto.')
                return False
            key = self._current_key()
            op = lambda : self._data.delete(key)
            log(EVENT, 'Mazání záznamu:', key)
        success, result = db_operation(op)
        if success:
            self._signal_update()
            log(ACTION, 'Záznam smazán.')
            return True
        else:
            return False

    def _on_import_interactive(self):
        if not self._data.accessible(None, pytis.data.Permission.INSERT):
            msg = _("Nemáte práva pro vkládání záznamů do této tabulky.")
            message(msg, beep_=True)
            return False
        msg = _("Nejprve vyberte soubor obsahující importovaná data. "
                "Poté budete moci zkontrolovat a potvrdit každý záznam.\n\n"
                "*Formát vstupního souboru:*\n\n"
                "Každý řádek obsahuje seznam hodnot oddělených zvoleným "
                "znakem, nebo skupinou znaků (vyplňte níže). "
                "Tabelátor zapište jako ='\\t'=.\n\n"
                "První řádek obsahuje identifikátory sloupců a určuje tedy "
                "význam a pořadí hodnot v následujících (datových) řádcích.\n\n"
                "Identifikátory jednotlivých sloupců jsou následující:\n\n" + \
                "\n".join(["|*%s*|=%s=|" % (c.column_label(), c.id()) for c in
                           [self._view.field(id)
                            for id in self._view.layout().order()]]))
        separator = run_dialog(InputDialog, 
                               title=_("Hromadné vkládání dat"),
                               report=msg, report_format=TextFormat.WIKI,
                               prompt="Oddělovač", value='|')
        if not separator:
            if separator is not None:
                message(_("Nebyl zadán oddělovač."), beep_=True)
            return False
        separator = separator.replace('\\t', '\t')
        while 1:
            filename = run_dialog(FileDialog)
            if filename is None:
                message(_("Nebyl zadán soubor. Proces ukončen."), beep_=True)
                return False
            try:
                fh = open(filename)
            except IOError, e:
                msg = _("Nepodařilo se otevřít soubor '%s': %s")
                run_dialog(Error, msg % (filename, str(e)))
                continue
            break
        try:
            columns = [str(id.strip()) for id in fh.readline().split(separator)]
            fields = [self._view.field(id) for id in columns]
            if None in fields:
                msg = _("Chybný identifikátor sloupce: %s")
                run_dialog(Error, msg % columns[fields.index(None)])
                return False
            types = [f.type(self._data) for f in fields]
            line_number = 1
            data = []
            for line in fh:
                line_number += 1
                values = line.rstrip('\r\n').split(separator)
                if len(values) != len(columns):
                    msg = _("Chyba dat na řádku %d:\n"
                            "Počet hodnot neodpovídá počtu sloupců.")
                    run_dialog(Error, msg % line_number)
                    return False
                row_data = []
                for id, type, val in zip(columns, types, values):
                    value, error = type.validate(val)
                    if error:
                        msg = _("Chyba dat na řádku %d:\n"
                                "Nevalidní hodnota sloupce '%s': %s") % \
                                (line_number, id, error.message())
                        run_dialog(Error, msg)
                        return False
                    row_data.append((id, value))
                data.append(pytis.data.Row(row_data))
        finally:
            fh.close()
        new_record(self._name, prefill=self._prefill, inserted_data=data)
            
    # Veřejné metody
    
    def select_row(self, position, quiet=False):
        """Vyber řádek dle 'position'.

        Argument 'position' může mít některou z následujících hodnot:
        
          None -- nebude vybrán žádný řádek.
          Nezáporný integer -- bude vybrán řádek příslušného pořadí, přičemž
            řádky jsou číslovány od 0.
          Datový klíč -- bude vybrán řádek s tímto klíčem, kterým je instance
            třídy 'pytis.data.Value' nebo jejich tuple.
          Slovník hodnot -- bude vybrán první nalezený řádek obsahující
            hodnoty slovníku (instance 'pytis.data.Value') v sloupcích určených
            klíči slovníku.
          Instance třídy 'pytis.data.Row' -- bude převeden na datový klíč a
            zobrazen odpovídající řádek.  Instance musí být kompatibilní
            s datovým objektem formuláře.
        
        Pokud takový záznam neexistuje, zobraz chybový dialog a jinak nic.
        Argumentem 'quiet' lze zobrazení chybového dialogu potlačit, takže
        nenalezení řádku je tiše ignorováno.

        Výběrem je myšlena akce relevantní pro daný typ formuláře (odvozené
        třídy).  Tedy například vysvícení řádku v tabulce, zobrazení záznamu v
        náhledovém formuláři apod.
        
        """
        if position is None or isinstance(position, pytis.data.Row):
            row = position
        elif isinstance(position, types.IntType):
            row = self._find_row_by_number(position)
        elif isinstance(position, (types.TupleType, pytis.data.Value)):
            row = self._find_row_by_key(position)
        elif isinstance(position, types.DictType):
            row = self._find_row_by_values(position.keys(), position.values())
        else:            
            raise ProgramError("Invalid 'position':", position)
        if not quiet and position is not None and row is None:
            run_dialog(Warning, _("Záznam nenalezen"))
            return
        self._select_row(row, quiet=quiet)

    def set_row(self, row):
        """Nastav aktuální záznam formuláře daty z instance 'PresentedRow'."""
        self._row = row
        self._run_callback(self.CALL_SELECTION, row)
        
    def current_row(self):
        """Vrať instanci PresentedRow právě aktivního řádku.

        Není-li vybrán žádný řádek, vrať 'None'.

        """
        return self._row

    def current_key(self):
        """Vrať klíč aktuálně vybraného řádku.

        Vrací: Sekvenci instancí třídy 'pytis.data.Value' nebo 'None', pokud
        není vybrán žádný řádek.

        """
        return self._current_key()

    def current_field(self):
        """Vrať identifikátor aktuálně vybraného políčka/sloupečku."""
        return None

    def prefill(self):
        """Vrať data pro předvyplnění nového záznamu."""
        return self._prefill
    
    def set_prefill(self, data):
        """Nastav data pro předvyplnění nového záznamu.

        List si může zapamatovat hodnoty, které mají být automaticky použity
        pro předvyplnění nového záznamu při operacích vložení řádku nad tímto
        listem.  Pro argument 'data' zde platí stejné podmínky, jako pro
        argument 'prefill' konstruktoru třídy 'PresentedRow'.

        """
        self._prefill = data

    def on_command(self, command, **kwargs):
        if command == RecordForm.COMMAND_DELETE_RECORD:
            self._on_delete_record(**kwargs)
        elif command == RecordForm.COMMAND_NEW_RECORD:
            self._on_new_record(**kwargs)
        elif command == RecordForm.COMMAND_IMPORT_INTERACTIVE:
            self._on_import_interactive()
        elif command == RecordForm.COMMAND_EDIT_RECORD:
            self._on_edit_record(**kwargs)
        else:
            return super(RecordForm, self).on_command(command, **kwargs)
        return True

        
class LookupForm(RecordForm):
    """Formulář s vyhledáváním a tříděním."""
    
    SORTING_NONE = 'SORTING_NONE'
    """Konstanta pro argument direction příkazu 'COMMAND_SORT'."""
    SORTING_ASCENDENT = 'SORTING_ASCENDENT'
    """Konstanta pro argument direction příkazu 'COMMAND_SORT'."""
    SORTING_DESCENDANT = 'SORTING_DESCENDANT'
    """Konstanta pro argument direction příkazu 'COMMAND_SORT'."""

    
    def _init_attributes(self, sorting=None, condition=None,
                         indicate_filter=False, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          sorting -- specifikace počátečního třídění formuláře, viz argument
            'sort' metody 'pytis.data.Data.select()'
          condition -- podmínka výběru dat, viz argument 'condition' metody
            'pytis.data.Data.select()'
          indicate_filter -- ???
          kwargs -- argumenty předané konstruktoru předka
        
        """
        super_(LookupForm)._init_attributes(self, **kwargs)
        self._init_sorting(sorting)
        self._lf_initial_sorting = self._lf_sorting
        self._lf_condition = condition
        self._lf_initial_condition = self._lf_condition
        self._lf_indicate_filter = indicate_filter
        self._lf_search_dialog = None
        self._lf_filter_dialog = None
        self._lf_select_count = None
        self._lf_filter = None

    def _new_form_kwargs(self):
        return dict(condition=self._lf_condition, sorting=self._lf_sorting)

    def _init_sorting(self, sorting=None):
        if sorting is None:
            sorting = self._get_state_param('sorting', None, types.TupleType)
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
        
    def _default_sorting(self):
        sorting = self._view.sorting()
        if sorting is None:
            sorting = tuple([(k.id(), pytis.data.DESCENDANT)
                             for k in self._data.key()
                             if self._view.field(k.id()) is not None])
        return sorting

    def _init_select(self):
        data = self._data
        if self._lf_condition and self._lf_filter:
            condition = pytis.data.AND(self._lf_condition, self._lf_filter)
        else:
            condition = self._lf_condition or self._lf_filter
        if self._lf_initial_condition:
            condition = pytis.data.AND(condition, self._lf_initial_condition)
        op = lambda : data.select(condition=condition,
                                  sort=self._data_sorting(), reuse=False)
        success, self._lf_select_count = db_operation(op)
        if not success:
            log(EVENT, 'Selhání databázové operace')
            throw('form-init-error')
        return self._lf_select_count

    def _cleanup(self):
        super(LookupForm, self)._cleanup()
        if self._lf_search_dialog:
            self._lf_search_dialog = None
        if self._lf_filter_dialog:
            self._lf_filter_dialog = None
    
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
    
    def _lf_sf_dialog(self, attr, class_):
        dialog = getattr(self, attr)
        if not dialog:
            dialog = class_(self._parent, self._lf_sfs_columns())
            setattr(self, attr, dialog)
        return dialog
        
    def _search(self, condition, direction, row_number=None,
                report_failure=True):
        self._search_adjust_data_position(row_number)
        data = self._data
        skip = data.search(condition, direction=direction)
        if skip == 0:
            log(EVENT, 'Záznam nenalezen')
            if report_failure:
                run_dialog(Warning, _("Záznam nenalezen"))
            result = None
        else:
            result = skip
            log(EVENT, 'Záznam nalezen:', skip)
            self._search_skip(result, direction)
        return result

    def _search_adjust_data_position(self, row_number):
        pass

    def _search_skip(self, skip, direction):
        data = self._data
        data.skip(skip-1, direction=direction)
        row = data.fetchone(direction=direction)
        self._select_row(row)

    def _cmd_jump(self):
        if self._lf_select_count > 0:
            prompt = _("Záznam číslo (1-%s):") % (self._lf_select_count)
            while True:
                result = run_dialog(InputDialog, message=_("Skok na záznam"),
                                    prompt=prompt)
                if result is None:
                    break
                elif result.isdigit():
                    self.select_row(int(result)-1)
                    break

    def _cmd_search(self, next=False, back=False):
        dlg = self._lf_sf_dialog('_lf_search_dialog', SearchDialog)
        condition = dlg.condition()
        if next and condition is not None:
            if back:
                direction = pytis.data.BACKWARD
            else:
                direction = pytis.data.FORWARD
        else:
            condition, direction = \
                block_refresh(lambda: run_dialog(dlg, self.current_row(),
                                                 self.current_field()))
        if condition is not None:
            self._search(condition, direction)

    def _on_form_state_change(self):
        super(LookupForm, self)._on_form_state_change()
        self._init_sorting()

    def _is_searching(self):
        sd = self._lf_search_dialog
        return bool(sd and sd._condition)
            
    def _can_search_next(self, **kwargs):
        return self._is_searching()

    def _can_search_previous(self, **kwargs):
        return self._is_searching()
            
    def _filter(self, condition):
        self._init_select()
        self.select_row(self._current_key())

    def _cmd_filter(self, show_dialog=True):
        sf_dialog = self._lf_sf_dialog('_lf_filter_dialog', FilterDialog)
        if show_dialog:
            perform, filter = run_dialog(sf_dialog, self._data,
                                         self._lf_initial_condition,
                                         self.current_row(),
                                         self.current_field())
        else:
            perform, filter = (True, sf_dialog.condition())
        if perform and filter != self._lf_filter:
            self._lf_filter = filter
            self._filter(filter)

    def _can_unfilter(self):
        return self._lf_filter is not None
        
    def _cmd_unfilter(self):
        self._lf_sf_dialog('_lf_filter_dialog', FilterDialog).reset_condition()
        self._lf_filter = None
        self._filter(None)

    def _cmd_sort(self, col=None, direction=None, primary=False):
        """Změň třídění.

        Argumenty:

          col -- id sloupce, podle kterého má být seznam setříděn, nebo
            'None' pro globální změny (například vypnutí veškerého třídění)
          direction -- směr třídění (sestupně/vzestupně/vůbec).  Hodnota daná
            konstantou 'LookupForm.SORTING_NONE' značí požadavek na zrušení
            třídění.  Jinak je očekávána jedna z konstant
            'LookupForm.SORTING_ASCENDENT' (pro sestupné třídění), nebo
            'LookupForm.SORTING_DESCENDANT' (pro vzestupné třídění).
          primary -- právě když je pravdivé, bude daný sloupec zvolen jako
            primární a *jediný* třídící sloupec.  V opačném případě bude pouze
            přidán na konec stávajícího seznamu třídících sloupců.
        
        Při nejednoznačné kombinaci argumentů 'col' a 'direction' je
        automaticky vyvolán dialog pro výběr třídících kritérií.
        
        """
        if col is None and direction == self.SORTING_NONE:
            sorting = ()
        elif col is None or direction is None:
            columns = self._lf_sfs_columns()
            if col is None and self._lf_sorting: 
                col = self._sorting_columns()[0]
            d = SortingDialog(self._parent, columns, self._lf_sorting,
                              col=col, direction=direction)
            sorting = run_dialog(d)
            if sorting is None:
                return None
            elif sorting is ():
                sorting = self._lf_initial_sorting
        elif col is not None:
            if not self._data.find_column(col):
                message(_("Podle tohoto sloupce nelze třídit"),
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
        # `col' je zde identifikátor sloupce.
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
        
    # Veřejné metody

    def condition(self):
        """Vrať specifikaci aktuální podmínky výběru dat.

        Podmínka je vrácena v podobě požadované argumentem 'condition'
        metody 'pytis.data.Data.select()'.

        """
        return self._lf_condition
    
    def sorting(self):
        """Vrať specifikaci aktuálního třídění seznamu.

        Podmínka je vrácena v podobě požadované argumentem 'sort'
        metody 'pytis.data.Data.select()'.

        """
        return self._lf_sorting


### Editační formulář


class EditForm(LookupForm, TitledForm, Refreshable):
    """Formulář pro editaci všech vlastností jednoho záznamu.

    Formulář je vytvořen poskládáním jednotlivých vstupních políček daných
    specifikací do mřížky.  Pole mohou být různě seskupována a jejich rozložení
    je určeno specifikační třídou 'LayoutSpec' resp. 'GroupSpec'.

    Každé vstupní pole je reprezentováno objektem třídy 'InputField'.  To se
    stará o interakci s uživatelem, validaci vstupních dat apod.

    Formulář může sloužit jak k prohlížení či editaci stávajících dat, tak
    i k vytváření nových záznamů (viz argument konstruktoru 'mode').
 
    """

    MODE_INSERT = 'MODE_INSERT'
    """Mód formuláře pro vkládání nových záznamů."""
    MODE_EDIT = 'MODE_EDIT'
    """Mód formuláře pro editaci stávajících záznamů."""
    MODE_VIEW = 'MODE_VIEW'
    """Mód formuláře pro zobrazení záznamů bez možnosti editace."""
    
    def __init__(self, *args, **kwargs):
        super(EditForm, self).__init__(*args, **kwargs)
        # Remember the original size.
        self._size = self.GetSizer().GetMinSize() + wx.Size(2, 2)
        for f in self._fields:
            if self._mode == self.MODE_VIEW:
                f.disable(change_appearance=False)
            else:
                f.enable()
        if self._mode == self.MODE_INSERT:
            # Inicializuji prázdný záznam.
            self._init_inserted_row()
        if isinstance(self._parent, wx.Dialog):
            wx_callback(wx.EVT_INIT_DIALOG, self._parent, self._set_focus_field)
        else:
            self._set_focus_field()
            

    def _init_attributes(self, mode=MODE_EDIT, focus_field=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          mode -- jedna z 'MODE_*' konstant třídy.  Určuje, zda formulář slouží
            k prohlížení, editaci či vytváření záznamů.

          focus_field -- id políčka, které má být vybráno jako aktivní pro
            uživatelský vstup, případně funkce jednoho argumentu, kterým je
            aktuální PresentedRow, která vrací id políčka pro uživatelský
            vstup.

          
          kwargs -- argumenty předané konstruktoru předka.

        """
        super_(EditForm)._init_attributes(self, **kwargs)
        assert mode in (self.MODE_EDIT, self.MODE_INSERT, self.MODE_VIEW)
        #assert focus_field in [f.id() for f in self._view.fields()]
        self._mode = mode
        self._focus_field = focus_field or self._view.focus_field()
        # Other attributes
        self._fields = []

    def _init_inserted_row(self):
        self._select_row(None)
        
    def _set_focus_field(self, event=None):
        """Inicalizuj dialog nastavením hodnot políček."""
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
        if self._mode == self.MODE_INSERT:
            permission = pytis.data.Permission.INSERT
        elif self._mode == self.MODE_EDIT:
            permission = pytis.data.Permission.UPDATE
        else:
            permission = pytis.data.Permission.VIEW
        data_columns = [c.id() for c in self._data.columns()]
        for id in self._view.layout().order():
            spec = self._view.field(id)
            if spec.width() != 0:
                if id in data_columns:
                    acc = self._data.accessible(id, permission)
                else:
                    acc = True
                f = InputField.create(panel, spec, self._data, guardian=self,
                                      accessible=acc)
                f.set_callback(InputField.CALL_FIELD_CHANGE,self._on_field_edit)
                self._fields.append(f)
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
                self.set_row(self._row)
            return _handler
        wx_callback(wx.EVT_BUTTON, self, b.GetId(),
                    create_handler(item.handler()))
        return b
        
    def _create_group(self, parent, group):
        """Vytvoř skupinu vstupních políček podle specifikace.

        Argumenty:

          group -- instance 'GroupSpec', která má být zpracována.

        Každou posloupnost za sebou následujících políček seskupí pod sebe
        a pro každou vnořenou skupinu políček zavolá sebe sama rekurzivně.
        Výsledek potom poskládá do instance 'wx.BoxSizer', kterou vytvoří.

        Specifikace skupiny ovlivňuje způsob seskupení:
        horizontální/vertikální, mezery mezi políčky, skupinami
        atd. Viz. dokuewntace třídy 'GroupSpec'

        Vrací: 'wx.BoxSizer' naplněný políčky a vnořenými skupinami.

        """
        orientation = orientation2wx(group.orientation())
        if group.label() is not None:
            box = wx.StaticBox(parent, -1, group.label())
            sizer = wx.StaticBoxSizer(box, orientation)
        else:
            sizer = wx.BoxSizer(orientation)
        # každý souvislý sled políček ukládám do pole a teprve nakonec je
        # poskládám metodou self._pack_fields() a vložím do sizeru této
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
                if  item.spec().compact():
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
            # přidej zbylý sled políček (pokud nějaký byl)
            sizer.Add(self._pack_fields(parent, pack, space, gap),
                      0, wx.ALIGN_TOP|border_style, border)
        # pokud má skupina orámování, přidáme ji ještě do sizeru s horním
        # odsazením, jinak je horní odsazení příliš malé.
        if group.label() is not None:
            s = wx.BoxSizer(orientation)
            s.Add(sizer, 0, wx.TOP, 2)
            sizer = s
        return sizer

    def _pack_fields(self, parent, items, space, gap):
        """Sestav skupinu pod sebou umístěných políček/tlačítek do gridu.

        Argumenty:

          items -- sekvence identifikátorů políček nebo instancí Button.
          space -- mezera mezi ovládacím prvkem a labelem políčka v dlg units;
            integer
          gap -- mezera mezi jednotlivými políčky v dlg units; integer

        Pro každý prvek skupiny vytvoří tlačítko nebo políčko
        'inputfield.InputField' a přidá jeho label a widget do vytvořené
        instance 'wx.FlexGridSizer'.

        Vrací: instanci 'wx.FlexGridSizer' naplněnou políčky a tlačítky.

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

    def _validate_fields(self):
        # Postupná validace všech políček.
        for f in self._fields:
            if self._mode == self.MODE_INSERT or f.is_modified():
                value, error = f.validate()
                if error:
                    log(EVENT, 'Validace selhala:', (f.id(), f.get_value()))
                    f.set_focus()
                    return False
        return True
            
    def _commit_form(self, close=True):
        # Validace všech políček.
        if not self._validate_fields():
            return False
        # Ověření integrity záznamu (funkce check).
        failed_id = self._check_record(self._row)
        if failed_id:
            self._field(failed_id).set_focus()
            return False
        # Vytvoření datového řádku.
        rdata = self._record_data(self._row)
        if self._mode == self.MODE_INSERT:
            log(ACTION, 'Vložení řádku')
            op = (self._data.insert, (rdata,))
        elif self._mode == self.MODE_EDIT:
            log(ACTION, 'Update řádku')
            op = (self._data.update, (self._current_key(), rdata))
        else:
            raise ProgramError("Can't commit in this mode:", self._mode)
        # Provedení operace
        success, result = db_operation(op)
        if success and result[1]:
            new_row = result[0]
            original_row = copy.copy(self._row)
            if new_row is not None:
                self._row.set_row(new_row, reset=True)
                self.set_row(self._row)
            else:
                # TODO: Lze provést něco chytřejšího?
                pass
            self._signal_update()
            if self._mode == self.MODE_INSERT:
                log(ACTION, 'Záznam vložen')
            else:
                log(ACTION, 'Záznam updatován')
            for field in self._fields:
                # Políčka se tímto trikem budou tvářit jako nezmněněná.
                field.init(field.get_value())
            cleanup = self._view.cleanup()
            if cleanup is not None:
                cleanup(self._row, original_row)
            if close:    
                self._result = self._row
                self.close()
            return True
        else:
            msg = _("Uložení záznamu se nezdařilo")
            if type(result) == type(()) and \
               isinstance(result[0], types.StringTypes):
                msg = "%s\n\n%s" % (result[0], msg)
            run_dialog(Error, msg)
            return False

    def _select_row(self, row, quiet=False):
        prow = PresentedRow(self._view.fields(), self._data, row,
                            prefill=self._prefill,
                            new=self._mode == self.MODE_INSERT,
                            change_callback=self._on_field_change,
                        editability_change_callback=self._on_editability_change)
        self.set_row(prow)

    def set_row(self, row):
        """Naplň formulář daty z daného řádku (instance 'PresentedRow')."""
        super_(EditForm).set_row(self, row)
        for f in self._fields:
            f.init(row[f.id()].export())
            if self._mode != self.MODE_VIEW:
                if row.editable(f.id()):
                    f.enable()
                else:
                    f.disable()
        
    def title(self):
        """Vrať název formuláře jako řetězec."""        
        return self._view.layout().caption()

    def size(self):
        """Vrať skutečnou velikost formuláře (bez ohledu na aktuální velikost).

        Vrácená hodnota reprezentuje minimální velikost formuláře, tak aby byly
        všechny jeho prvky viditelné.  Skutečná velikost může být menší, nebo
        větší v závoslosti na velikost okna, ve kterém je formulář zobrazen.
        
        """
        return self._size
    
    def changed(self):
        """Vrať pravdu, pokud byla data změněna od posledního uložení."""
        field = find(True, self._fields, key=lambda f: f.is_modified())
        return field is not None

    def _on_field_edit(self, id, value):
        # Signalizace změny políčka z InputField
        self._row[id] = value

    def _on_field_change(self, id):
        # Signalizace změny políčka z PresentedRow
        field = find(id, self._fields, key=lambda f: f.id())
        if field is not None and self._row is not None:
            value = self._row.format(id)
            if field.initialized() and field.get_value() != value:
                field.set_value(value)
            
    def _on_editability_change(self, id, editable):
        if id in self._view.layout().order():
            if editable:
                self._field(id).enable()
            else:                
                self._field(id).disable()
                
    def _exit_check(self):
        if self.changed():
            q = _("Data byla změněna a nebyla uložena!") + "\n" + \
                _("Opravdu chcete uzavřít formulář?")
            if not run_dialog(Question, q):
                return False
        return True

    def _can_commit_record(self):
        return self._mode != self.MODE_VIEW
    
    def _cmd_commit_record(self):
        return self._commit_form()

    def _cmd_navigate(self, back=False):
        if self._mode != self.MODE_VIEW:
            # Vygeneruj událost navigace mezi políčky.
            w = wx_focused_window()
            if not w:
                self._fields[0].set_focus()
                w = wx_focused_window()
            if w:
                flags = not back and wx.NavigationKeyEvent.IsForward or 0
                w.Navigate(flags=flags)

    
class PopupEditForm(PopupForm, EditForm):
    """Stejné jako 'EditForm', avšak v popup podobě."""

    DESCR = _("editační formulář")
    
    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        EditForm.__init__(self, parent, *args, **kwargs)
        size = copy.copy(self.size())
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 50))
        self.SetSize(size)
        p = parent
        while not p.GetTitle() and p.GetParent():
            p = p.GetParent()
        parent.SetTitle('%s: %s' % (p.GetTitle(), self.title()))

    def _init_attributes(self, inserted_data=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          inserted_data -- umožňuje předat libovolnou sekvenci datových řádků
            (instancí pytis.data.Row).  Formulář je potom postupně
            předvyplňován těmito řádky a tlačítkem ``Další'' je každý záznam
            uložen a formulář naplněn dalším řádkem.  Takto je možné jednoduše
            využít formulář k hromadnému vkládání řádků načtených z libovolného
            zdroje.

          kwargs -- argumenty předané konstruktoru předka.
            
        """
        EditForm._init_attributes(self, **kwargs)
        assert inserted_data is None or self._mode == self.MODE_INSERT
        self._inserted_data = inserted_data
        self._inserted_data_pointer = 0

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
        spec = (('message', None, _("Oznamovací oblast")),)
        if self._inserted_data is not None:
            spec += (('progress', 9, _("Ukazatel pozice hromadného vkládání")),)
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

    def _init_inserted_row(self):
        super(PopupEditForm, self)._init_inserted_row()
        data = self._inserted_data
        if data is not None:
            i = self._inserted_data_pointer
            if i < len(data):
                self.set_status('progress', "%d/%d" % (i+1, len(data)))
                self._inserted_data_pointer += 1
                ok_button = wx.FindWindowById(wx.ID_OK, self._parent)
                ok_button.Enable(i == len(data)-1)
                for id, value in data[i].items():
                    self._field(id).set_value(value.export())
            else:
                self.set_status('progress', '')
                run_dialog(Message, _("Všechny záznamy byly zpracovány."))
                self._inserted_data = None

    def _exit_check(self):
        i = self._inserted_data_pointer
        data = self._inserted_data
        if data is not None and i <= len(data):
            msg = _("Ještě nebyly zpracovány všechny řádky "
                    "vstupních dat.\n"
                    "Chcete opravdu ukončit vkládání?")
            if not run_dialog(Question, msg, default=False):
                return False
        return super(PopupEditForm, self)._exit_check()

    def _on_next_button(self, event):
        result = self._commit_form(close=False)
        if result:
            message(_("Záznam uložen"))
            refresh()
            self._init_inserted_row()

    def _on_skip_button(self, event):
        i = self._inserted_data_pointer
        message(_("Záznam %d/%d přeskočen") % (i, len(self._inserted_data)))
        self._init_inserted_row()

    
    def _buttons(self):
        buttons = ({'id': wx.ID_OK,
                    'toottip': _("Uložit záznam a uzavřít formulář"),
                    'handler': lambda e: self._commit_form(),
                    'default': True},
                   {'id': wx.ID_CANCEL,
                    'toottip': _("Uzavřít formulář bez uložení dat"),
                    'handler': lambda e: self.close()})
        if self._mode == self.MODE_INSERT:
            buttons += ({'id': wx.ID_FORWARD,
                         'label': _("Další"),
                         'toottip': _("Uložit záznam a reinicializovat formulář"
                                      " pro vložení dalšího záznamu"),
                         'handler': self._on_next_button},)
        if self._inserted_data is not None:
            buttons += ({'label': _("Přeskočit"),
                         'toottip': _("Přeskočit tento záznam bez uložení"),
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
        self._unlock_record()
        super(PopupEditForm, self)._cleanup()

    def can_command(self, command, **kwargs):
        if command.handler() in (LookupForm, RecordForm):
            return False
        return super(PopupEditForm, self).can_command(command, **kwargs)
        
    def run(self):
        key = self._current_key()
        if self._mode == self.MODE_EDIT and key and not self._lock_record(key):
            return None
        return PopupForm.run(self)

    def set_status(self, field, message):
        if self._status_fields.has_key(field):
            self._status_fields[field].SetLabel(unicode(message or ''))
            return True
        else:
            return False
       

class ShowForm(EditForm):
    """Formulář pro zobrazení náhledu.

    Layout je stejný jako u editačního formuláře (resp. 'EditForm'),
    pouze titulek má stejný vzhled, jako titulek formulářů typu 'ListForm'.
    Určen pro zobrazení v duálním formuláři.

    """

    DESCR = _("náhledový formulář")

    def _init_attributes(self, mode=EditForm.MODE_VIEW, **kwargs):
        super_(ShowForm)._init_attributes(self, mode=mode, **kwargs)
        

class BrowsableShowForm(ShowForm):
    """Listovací formulář pro zobrazení náhledu.

    Formulář je needitovatelný, ale umožňuje pohyb po záznamech tabulky, nad
    kterou je vytvořen, vyhledávání atd.  Z uživatelského hlediska jde v
    podstatě o redukci prohlížecích možností formuláře typu 'BrowseForm' na
    jeden záznam zobrazený v Layoutu editačního formuláře.
    
    """
        
    def __init__(self, *args, **kwargs):
        super_(BrowsableShowForm).__init__(self, *args, **kwargs)
        self._init_select()
        
    def _cmd_next_record(self, back=False):
        current_row = self.current_row()
        if current_row:
            row_number = self._get_row_number(current_row.row())
        else:
            row_number = 0
        if not back:
            row_number += 1
            if row_number == self._lf_select_count:
                message(_("Poslední záznam"), beep_=True)
                return
        else:
            if row_number == 0:
                message(_("První záznam"), beep_=True)
                return
            row_number -= 1
        self._select_row(self._find_row_by_number(row_number))

    def _cmd_last_record(self, back=False):
        if back:
            row = 0
        else:
            row = self._lf_select_count - 1
        self.select_row(row)

    def _select_row(self, row, quiet=False):
        super(BrowsableShowForm, self)._select_row(row, quiet=quiet)
        current_row = self.current_row()
        total = self._lf_select_count
        if current_row and total:
            position = "%d/%d" % (self._get_row_number(current_row) + 1, total)
            set_status('list-position', position)
                     

