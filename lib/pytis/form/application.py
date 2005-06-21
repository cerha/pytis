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

"""Hlavní aplikační okno.

Tento modul definuje třídu 'Application', která představuje hlavní okno
aplikace a zajišťuje základní služby s ním související.  Modul se týká pouze
uživatelského rozhraní, neřeší obecně start a zastavení aplikace.

"""

import os.path
import sys
import time

import config
import pytis.form
if config.server:
    try:
        import Pyro.util
    except ImportError:
        pass
from pytis.form import *
import wx


_application = None


def run_application(resolver=None):
    """Vytvoř instanci třídy 'defs.application.Application'.

    Zavolej její metodu 'run()'.

    Argumenty:

      resolver -- instance třídy 'FileResolver', která má být použita jako
        resolver; může být též 'None', v kterémžto případě bude vytvořen
        implicitní resolver

    """
    if resolver is None:
        resolver = FileResolver(config.def_dir)
    Application(resolver).run()


class Application(wx.App, KeyHandler):
    """Hlavní okno aplikace.

    Aplikační okno sestává jednak ze statických prvků a jednak z vyměnitelného
    vnitřku okna.  Statickými prvky jsou pull-down menu a stavový řádek.  Jsou
    vytvořeny při vzniku aplikačního okna a dále se již nemění, kromě typu
    aktivace položek menu při výměnách vnitřku.  Vyměnitelný vnitřek může být
    libovolná instance třídy 'wx.Window'.

    Statické prvky jsou parametrizovány specifikačním souborem aplikace.  Tím
    je soubor 'application.py' v adresáři resolveru (určeném konfigurační
    volbou 'def_dir').  Použitelné specifikační funkce jsou:

      title -- titulek okna aplikace jako string.
      menu -- specifikace pull-down menu je ve formátu specifikačního
        argumentu konstruktoru třídy 'pytis.form.screen.MenuBar'.
      status_fields -- specifikace polí stavové řádky ve formátu
        specifikačního argumentu konstruktoru třídy
        'pytis.form.screen.StatusBar'.
      command_keys -- specifikace přiřazení kláves příkazům jako sekvence
        dvojic (COMMAND, KEY), kde COMMAND je instance třídy 'Command' a KEY je
        jemu příslušná klávesa.
      default_font_encoding -- implicitní kódování fontů jako odpovídající wx
        konstanta.
      
    Dynamický vnitřek lze nastavit metodami 'push()', 'pop()' a 'replace()'.
    Celá aplikace funguje jako zásobník vnitřních oken a s nimi souvisejících
    stavů statických prvků.  Vnitřní okna lze na zásobník přidávat, ze
    zásobníku odstraňovat nebo nahrazovat horní element zásobníku.

    Start uživatelského spočívá ve vytvoření instance této třídy (resp. třídy z
    ní odvozené) a volání její metody 'run()'.
    
    """
    _menubar_forms = {}

    _WINDOW_MENU_TITLE = _("Okn&a")

    def __init__(self, resolver):
        """Inicializuj aplikaci.

        Zde se pouze volají konstruktory předků, většina inicializací se ve
        skutečnosti provádí až v metodě 'OnInit'.

        Argumenty:

          resolver -- resolver, který má aplikace používat; instance třídy
            'pytis.util.Resolver'

        """
        self._resolver = resolver
        wx.App.__init__(self)
        self._window_menu_item = {}
    
    def OnInit(self):
        init_colors()
        title = config.application_name
        if __debug__:
            title += ' (wxWidgets %d.%d.%d)' % \
                     (wx.MAJOR_VERSION, wx.MINOR_VERSION, wx.RELEASE_NUMBER)
        frame = self._frame = wx.Frame(None, -1, title, 
                                       pos=(0,0), size=(800, 600),
                                       style=wx.DEFAULT_FRAME_STYLE)
        # Tento panel slouží pouze pro odchytávání klávesových událostí,
        # protože na frame se nedá navěsit EVT_KEY_DOWN.
        self._panel = wx.Panel(self._frame, -1)
        KeyHandler.__init__(self, self._panel)
        self._logo = None
        logo_file = self._spec('logo')
        if logo_file is not None:
            if os.access(logo_file, os.R_OK):
                logo = wx.Image(logo_file, type=wx.BITMAP_TYPE_BMP)
                self._logo = wx.StaticBitmap(self._frame, -1,
                                             logo.ConvertToBitmap())
                self._logo.Show(False)
            else:
                log(OPERATIONAL, "Unable to read logo:", logo_file)
        global _application
        _application = self
        keymap = self.keymap = Keymap()
        custom_keys = self._spec('command_keys', ())
        assert is_sequence(custom_keys), "Specifikace klávesových zkratek " + \
               "'command_keys' musí vracet sekvenci dvojic (COMMAND, KEY)."
        for cmd, key in command.DEFAULT_COMMAND_KEYS + custom_keys:
            keymap.define_key(key, cmd)
        self._statusbar = StatusBar(self._frame, self._spec('status_fields',()))
        self._windows = XStack()
        self._modals = Stack()
        wm = Menu(self._WINDOW_MENU_TITLE, (), activation=(Window.ACT_WINDOW))
        menus = self._spec('menu') + (wm,)
        self._menubar = mb = MenuBar(self._frame, menus, self)

        default_font_encoding = self._spec('default_font_encoding')
        if default_font_encoding is not None:
            wx.Font.SetDefaultEncoding(default_font_encoding)
        wx_callback(wx.EVT_SIZE, self._frame, self._on_frame_size)
        self.SetTopWindow(self._frame)
        self._frame.Show(True)
        self._spec('init')
        self._panel.SetFocus()
        return True


    def _spec(self, name, default_value=None):
        try:
            result = self._resolver.get('application', name)
        except ResolverError, e:
            log(OPERATIONAL, str(e))
            result = default_value
        return result

    # Ostatní metody

    def _update_window_menu(self, recreate=True):
        mb = self._menubar
        menu = mb.GetMenu(mb.FindMenu(self._WINDOW_MENU_TITLE))
        if menu is None:
            return
        if recreate:
            for form, item in self._window_menu_item.items():
                menu.Remove(item.GetId())
                item.Destroy()
                del self._window_menu_item[form]
            for i, form in enumerate(self._windows.items()):
                title = "&%d. %s" % (i+1, form.title())
                if form.__class__ != BrowseForm:
                    title += " (%s)" % form.descr()
                item = RadioItem(title,
                                 help=_('Vyzvednout okno formuláře "%s" %s') % \
                                 (form.title(), str(form)),
                                 command=Application.COMMAND_RAISE_FORM,
                                 args={'form': form}).create(self._frame, menu)
                self._window_menu_item[form] = item
                menu.AppendItem(item)
        for item in menu.GetMenuItems():
            if item.IsCheckable():
                item.Check(False)
        if not self._windows.empty():
            self._window_menu_item[self._windows.active()].Check(True)

    def _raise_form(self, form):
        if form is not None:
            if form not in self._frame.GetChildren():
                log(EVENT, "Reparent -- možná je to tu opravdu potřeba...")
                form.Reparent(self._frame)
            old = self._windows.active()
            if form is not old:
                self.save()
                old.hide()
                self._windows.activate(form)
                self.restore()

    def _activate(self, activations, form):
        self._menubar.activate(activations, form)
            
    def _set_form_state(self, form, select_row=None):
        if select_row:
            form.select_row(select_row)

    # Ostatní veřejné metody

    def run_form(self, form_class, name, *args, **kwargs):
        """Vytvoř formulář a spusť jej.

        Argumenty:

          form_class -- třída vytvářeného formuláře (libovolná třída odvozená
            od třídy `Form')
          name -- jméno specifikace pro resolver

        Další argumenty budou předány konstruktoru formuláře, tak jak následují
        za argumentem `name'.  Argumenty `parent' a `resolver'
        budou doplněny automaticky.

        Vytvořený formulář bude zobrazen v okně aplikace, nebo v novém modálním
        okně, pokud jde o modální formulář odvozený od třídy 'PopupForm'.
        Modální formulář je spuštěn metodou 'run()' a její výsledek je
        návratovou hodnotou volání této metody.  V tomto případě je však návrat
        z této metody proveden až po ukončení formuláře (uzavření jeho okna).
        Pro nemodální formuláře se metoda vrací ihned po zobrazení okna s
        návratovou hodnotou None.  Nemodální formulář je potom nutné z aplikace
        odstranit metodou 'leave_form()'.
          
        """
        result = None
        state_kwargs = {}
        for arg in ('select_row',):
            if kwargs.has_key(arg):
                state_kwargs[arg] = kwargs[arg]
                del kwargs[arg]
        try:
            if callable(name):
                name = name()
                if name is None:
                    return None
            log(ACTION, 'Vytvářím nový formulář:',
                (form_class, name, args, kwargs))
            message(_("Spouštím formulář..."), root=True)
            wx_yield_()
            assert issubclass(form_class, Form)
            assert is_anystring(name)
            result = None
            self.save()
            form = find((form_class, name), self._windows.items(),
                        key=lambda f: (f.__class__, f.name()))
            if form is not None:
                self._raise_form(form)
                message(_('Formulář "%s" nalezen na zásobníku oken.') % \
                        form.title())
                self._set_form_state(form, **state_kwargs)
                return result
            if issubclass(form_class, PopupForm):
                parent = self._modals.top() or self._frame
                kwargs['guardian'] = self._modals.top() or self
            else:
                assert self._modals.empty()
                kwargs['guardian'] = self
                parent = self._frame
            args = (parent, self.resolver(), name) + args
            form = catch('form-init-error', form_class, *args, **kwargs)
            if form is None:
                self.run_dialog(Error, _("Formulář se nepodařilo vytvořit"))
            else:
                self._set_form_state(form, **state_kwargs)
                if isinstance(form, PopupForm):
                    log(EVENT, "Zobrazuji modální formulář:", form)
                    self._modals.push(form)
                    message('', root=True)
                    form.show()
                    try:
                        result = form.run()
                        log(EVENT, "Modální formulář byl uzavřen:", form)
                        log(EVENT, "Návratová hodnota:", result)
                    finally:
                        self._modals.pop()
                        form.close()
                        busy_cursor(False)
                    top = self.top_window()
                    if top is not None:
                        if isinstance(top, Refreshable):
                            top.refresh()
                        top.focus()
                    else:
                        self._panel.SetFocus()
                else:
                    log(EVENT, "Zobrazuji nemodální formulář:", form)
                    old = self._windows.active()
                    if old is not None:
                        old.hide()
                    self._windows.push(form)
                    message('', root=True)
                    form.show()
                    self._activate(form.ACTIVATIONS, form)
                    self._update_window_menu()
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def run_procedure(self, spec_name, proc_name, **kwargs):
        """Spusť proceduru.

        Argumenty:

          spec_name -- jméno specifikace pro resolver.
          proc_name -- jméno procedury, která má být spuštěna.  Jde o klíč do
            slovníku, který je vracen specifikační funkcí 'proc_spec'.

        Klíčové argumenty budou předány spouštěné proceduře.

        Návratová hodnota procedury je návratovou hodnotou volání této metody.         

        """
        result = None
        try:
            log(ACTION, 'Spouštím proceduru:', (spec_name, proc_name, kwargs))
            message(_("Spouštím proceduru..."), root=True, timeout=2)
            # Kvůli wx.SafeYield() se ztrácí focus, takže
            # si ho uložíme a pak zase obnovíme.
            focused = wx_focused_window()            
            wx_yield_()
            spec = self._resolver.get(spec_name, 'proc_spec')
            assert is_dictionary(spec), \
                   _("Specifikace procedur 'proc_spec' musí vracet slovník!")
            assert spec.has_key(proc_name), \
                  _("Specifikace procedur neobsahuje definici '%s'") % proc_name
            proc = spec[proc_name]
            result = proc(**kwargs)
            log(ACTION, "Návratová hodnota procedury:", result)
            if focused:
                focused.SetFocus()
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def new_record(self, name, key=None, prefill=None):
        """Spusť interaktivní akci přidání nového záznamu.
        
        Argumenty:
        
          name -- jméno specifikace pro resolver.
          key -- klíč kopírovaného záznamu, nebo None.
          prefill -- slovník řetězcových (uživatelských) hodnot, které mají být
            předvyplněny při inicializaci formuláře.

        """
        view = self._resolver.get(name, 'view_spec')
        on_new_record = view.on_new_record()
        if on_new_record is not None:
            result = on_new_record(key=key, prefill=prefill)
            top = self.current_form()
            if isinstance(top, Refreshable):
                top.refresh()
        else:
            result = run_form(PopupEditForm, name, select_row=key, new=True,
                              prefill=prefill)
        return result    
            
    def run_dialog(self, dialog_or_class_, *args, **kwargs):
        """Zobraz dialog určené třídy s hlavním oknem aplikace jako rodičem.

        Argumenty:
          dialog_or_class_ -- třída dialogu (odvozená od třídy 'Dialog'), nebo
            přímo instance.  Pokud jde o třídu, bude vytvořena nová instance a
            ta bude následně spuštěna.
          
        Jako první argument konstruktoru dialogové třídy ('parent') bude
        doplněno aktuální (vrchní) okno aplikace.  Ostatní argumenty jsou
        předány tak, jak jsou.  Více o dialogových třídách a jejich argumentech
        konstruktoru v modulu 'pytis.form.dialog'.

        Pokud je argumentem instance, jsou argumenty předány metodě 'run()'.

        Dialog je spuštěn (metodou 'run()') a jeho návratová hodnota je také
        návratovou hodnotou této metody.
        
        """

        
        if not isinstance(dialog_or_class_, Dialog):
            class_ = dialog_or_class_
            assert issubclass(class_, Dialog)
            parent = self._frame
            if not self._modals.empty() and \
                   isinstance(self._modals.top(), wx.Window):
                parent = self._modals.top()
            dialog = class_(parent, *args, **kwargs)
            args, kwargs = (), {}
        else:
            dialog = dialog_or_class_
        self._modals.push(dialog)
        try:
            unlock_callbacks()
            result = dialog.run(*args, **kwargs)
        finally:
            self._modals.pop()
            busy_cursor(False)
        top = self.top_window()
        if top is not None:
            top.focus()
        else:
            self._panel.SetFocus()
        top = self
        return result

    def leave_form(self):
        """Uzavři aktuální formulář otevřený v okně aplikace.

        Pokud není otevřen žádný formulář, zaloguj a jinak nedělej nic.
        
        """
        form = self._windows.active()
        if form:
            log(EVENT, "Zavírám okno nemodálního formuláře:", form)
            form.defocus()
            self._windows.remove(form)
            self._update_window_menu()
            form.close()
            self.restore()
        else:
            log(EVENT, "Není otevřen žádný formulář.")

    def exit(self, quietly=False):
        """Ukonči uživatelské rozhraní aplikace.

        Argumenty:

          quietly -- právě když je pravdivé, nejsou při ukončení kladeny
            uživateli žádné dotazy

        """
        # Zde ignorujeme všemožné výjimky, aby i při poměrně značně havarijní
        # situaci bylo možno aplikaci ukončit.
        try:
            log(ACTION, 'Voláno ukončení aplikace')
        except:
            pass
        try:
            if not self._modals.empty():
                log(EVENT, "Není možno zavřít aplikaci s modálním oknem:",
                    self._modals.top())
                return False
            if not quietly and not self._windows.empty():
                q = _("Aplikace obsahuje otevřené formuláře\n" + \
                      "Opravdu chcete ukončit aplikaci?")
                if not self.run_dialog(Question, q):
                    return False
        except:
            pass
        while not self._windows.empty():
            try:
                self.leave_form()
            except:
                break
        try:
            self._frame.Close()
        except:
            pass
        global _application
        _application = None

    def run(self):
        """Spusť běh uživatelského rozhraní.

        Nevracej se dříve, než je běh uživatelského rozhraní definitivně
        ukončen.

        """
        COMPLETELY_BROKEN = False
        THREADING_BROKEN = False
        if COMPLETELY_BROKEN or THREADING_BROKEN:
            interrupt_init()
        else:
            interrupt_watcher()
        TIME_SLICE = 0.2
        timeout = [time.time() + TIME_SLICE]
        pid = os.getpid()
        def log_wrapper():
            if THREADING_BROKEN:
                if pid == os.getpid() and time.time() > timeout[0]:
                    wx.Yield()
                    timeout[0] = time.time() + TIME_SLICE
            yield_()
        if not COMPLETELY_BROKEN:
            log.add_hook(log_wrapper)
        self.MainLoop()

    def top_window(self):
        """Vrať momentálně aktivní okno aplikace.
        
        """
        if not self._modals.empty():
            return self._modals.top()
        else:
            return self._windows.active()

    def refresh(self):
        """Aktualizuj zobrazení viditelných oken aplikace, pokud je to třeba."""
        for stack in (self._modals, self._windows):
            if not stack.empty():
                top = stack.top()
                if isinstance(top, Refreshable):
                    top.refresh()
        
    def set_status(self, id, message, timeout=None, root=False):
        """Nastav v poli stavové řádky daného 'id' zprávu 'message'.
        
        Argumenty:
        
          id -- identifikátor pole stavové řádky.
          message -- string, který má být zobrazen, nebo 'None'; je-li 'None',
            bude předchozí hlášení smazáno.
          timeout -- není-li 'None', zpráva zmizí po zadaném počtu sekund.
          root -- je-li pravdivé, bude zpráva zobrazena vždy v hlavním okně
            aplikace.  Pokud ne, je zpráva zobrazena ve stavové řádce hlavního
            okna aplikace až v případě, že není otevřeno žádné modální okno,
            nebo se zobrazení zprávy v modálním okně nepodařilo.

        Zobrazení není garantováno, nemusí se zobrazit například v případě, kdy
        stavový řádek neobsahuje odpovídající pole.

        """
        modal = self._modals.top()
        if root or not isinstance(modal, Form) \
               or not modal.set_status(id, message):
            return self._statusbar.message(id, message, timeout=timeout)
            
    def get_status(self, id):
        """Vrať text pole 'id' stavového řádku hlavního okna aplikace.

        Pokud stavový řádek dané pole neobsahuje, vrať None.
        
        """
        return self._statusbar.get_message(id)

    def resolver(self):
        """Vrať resolver instancí podle jména; instance 'pytis.util.Resolver'."""
        return self._resolver

    def wx_frame(self):
        """Vrať instancí 'wx.Frame' hlavního okna aplikace."""
        return self._frame

    def save(self):
        """Ulož stav aplikace."""
        form = self._windows.active()
        if form:
            form.save()
            
    def restore(self):
        """Obnov stav aplikace."""
        form = self._windows.active()
        if form is not None:
            form.resize()
            if isinstance(form, Refreshable):
                form.refresh()
            form.show()
            form.restore()
            self._activate(form.ACTIVATIONS, form)
            if Window.ACT_WINDOW in form.ACTIVATIONS:
                self._update_window_menu(recreate=False)
            form.focus()    
        else:
            self._activate((), None)
            self._panel.SetFocus()

    def add_menu(self, menu, form=None):
        """Volá stejnojmennou metodu instance třídy 'MenuBar' v aplikaci."""
        self._menubar.add_menu(menu, form=form)
        
    # Callbacky

    def _on_frame_size(self, event):
        size = event.GetSize()
        self._frame.SetSize(size)
        top = self._windows.active()
        if top is not None:
            top.resize()
        if self._logo is not None:
            logo = self._logo.GetBitmap()
            logo_posx = max((size.GetWidth()-logo.GetWidth()) / 2, 0)
            logo_posy = max((size.GetHeight()-logo.GetHeight()-50) / 2, 0)
            self._logo.SetPosition((logo_posx,logo_posy))
            if top is None:
                self._logo.Show(True)
        return True

    def on_key_down(self, event, dont_skip=False):
        # Toto je záchranný odchytávač.  Věřte tomu nebo ne, ale pokud tady ta
        # metoda není, wxWindows se při více příležitostech po stisku klávesy
        # zhroutí.
        return KeyHandler.on_key_down(self, event)

    def on_command(self, command, **kwargs):
        """Pošli 'command' s 'kwargs' aktuálnímu oknu.

        Není-li žádné aktuální okno nebo pokud toto okno nemá metodu
        'on_command()', nedělej nic a vrať nepravdu.

        """
        log(command.log_kind(), 'Vyvolán příkaz:', (command, kwargs))
        try:
            try:
                busy_cursor(True)
                if command == Application.COMMAND_SHOW_POPUP_MENU:
                    top = self.top_window()
                    if hasattr(top, 'show_popup_menu'):
                        top.show_popup_menu()
                elif not self._modals.empty():
                    return self._modals.top().on_command(command, **kwargs)
                elif command == Application.COMMAND_EXIT:
                    self.exit()
                elif command == Application.COMMAND_BREAK:
                    message(_("Stop"), beep_=True)
                elif command == Application.COMMAND_RUN_FORM:
                    self.run_form(**kwargs)
                elif command == Application.COMMAND_RUN_PROCEDURE:
                    self.run_procedure(**kwargs)
                elif command == Application.COMMAND_NEW_RECORD:
                    self.new_record(**kwargs)
                elif command == Application.COMMAND_LEAVE_FORM:
                    self.leave_form()
                elif command == Application.COMMAND_RAISE_FORM:
                    self._raise_form(kwargs['form'])
                elif command == Application.COMMAND_NEXT_FORM:
                    self._raise_form(self._windows.next())
                elif command == Application.COMMAND_PREV_FORM:
                    self._raise_form(self._windows.prev())
                elif command == Application.COMMAND_REFRESH:
                    self.refresh()
                elif __debug__ and command == Application.COMMAND_CUSTOM_DEBUG:
                    config.custom_debug()
                else:
                    top = self._windows.active()
                    if top is not None and top.on_command(command, **kwargs):
                        return True
                    if command.handler() is not None:
                        command.handler()(**kwargs)
                        return True
                    return False
            finally:
                busy_cursor(False)
            return True
        except UserBreakException:
            pass
        except:
            top_level_exception()

    def window_count(self):
        """Vrať počet právě otevřených oken na zásobníku."""
        return len(self._windows.items())

    def current_form(self):
        """Vrať právě aktivní formulář aplikace, pokud existuje.
        
        Pokud není otevřen žádný formulář, nebo aktivním oknem není formulář,
        vrací None.  Pokud je otevřeným formulářem duální formulář, bude vrácen
        jeho právě aktivní podformulář.
        
        """
        top = self.top_window()
        if isinstance(top, Form):
            if isinstance(top, DualForm):
                return top.active_form()
            else:
                return top
        else:
            return None
                

# Funkce

def message(message, kind=EVENT, data=None, beep_=False, timeout=None,
            root=False):
    """Zaloguj a zobraz neinteraktivní 'message' v okně aplikace.

    Argumenty:

      message -- řetězec, který má být zobrazen; obsahuje-li jako poslední znak
        dvojtečku, není tato v okně aplikace zobrazena
      kind -- druh zprávy, jedna z konstant modulu 'log'
      data -- doplňující data pro logování, stejné jako v 'log.log'
      beep_ -- právě když je pravdivé, bude hlášení doprovázeno pípnutím
      timeout -- pokud je zadáno, zpráva zmizí po zadaném počtu sekund
      root -- je-li pravdivé, bude zpráva zobrazena vždy v hlavním okně
        aplikace.  Pokud ne, je zpráva zobrazena ve stavové řádce hlavního okna
        aplikace až v případě, že není otevřeno žádné modální okno, nebo se
        zobrazení zprávy v modálním okně nepodařilo.
        
    Pro zobrazení zprávy ve stavové řádce platí stejná pravidla, jako v případě
    metody 'Application.set_status()'.  Zalogováno je však v každém případě.

    """
    if beep_:
        beep()
    if message or data:
        log(kind, message, data=data)
    if _application:
        if message and message[-1] == ':':
            message = message[:-1]
        _application.set_status('message', message, timeout=timeout,
                                root=root)

def set_status(id, message, log_=True):
    """Nastav pole 'id' stavové řádky na 'message'.

    Argumenty:

      id -- identifikátor pole stavové řádky.
      message -- řetězec, který má být zobrazen.
      log_ -- pokud je pravda, bude událost zalogována.

    Pro zobrazení zprávy ve stavové řádce platí stejná omezení, jako v případě
    metody 'Application.set_status()'.  Zalogováno je však v každém případě.

    """
    if __debug__:
        if log_: log(DEBUG, "Nastavení pole stavové řádky:", data=(id, message))
    return _application.set_status(id, message)

def get_status(id):
    """Vrať text pole 'id' stavové řádky. (viz 'Application.get_status()')"""
    return _application.get_status(id)

def run_dialog(*args, **kwargs):
    """Zobraz dialog v okně aplikace (viz 'Application.run_dialog()')."""
    return _application.run_dialog(*args, **kwargs)

def run_form(*args, **kwargs):
    """Zobraz formulář v okně aplikace (viz 'Application.run_form()')."""
    return _application.run_form(*args, **kwargs)

def run_procedure(*args, **kwargs):
    """Spusť proceduru (viz 'Application.run_procedure()')."""
    return _application.run_procedure(*args, **kwargs)

def new_record(*args, **kwargs):
    """Spusť akci přidání nového záznamu (viz 'Application.new_record()')."""
    return _application.new_record(*args, **kwargs)

def leave_form():
    """Odstraň aktuální okno formuláře z aplikace."""
    return _application.leave_form()

def current_form():
    """Vrať právě zobrazený formulář aktuální aplikace, pokud existuje."""
    return _application.current_form()

def resolver():
    """Vrať resolver aplikace získaný přes 'Application.resolver()'."""
    return _application.resolver()

def wx_frame():
    """Vrať instancí 'wx.Frame' hlavního okna aplikace."""
    return _application.wx_frame()

def add_menu(menu, form=None):
    """Zavolej 'Application.add_menu()' aktuální aplikace."""
    return _application.add_menu(menu, form=form)

def exit(**kwargs):
    """Zavolej 'Application.exit() aktuální aplikace a předej argumenty."""
    return _application.exit(**kwargs)

def global_keymap():
    """Vrať klávesovou mapu aplikace jako instanci třídy 'Keymap'."""
    try:
        return _application.keymap
    except AttributeError:
        return Keymap()

def wx_yield_(full=False):
    """Zpracuj wx messages ve frontě.

    Argumenty:

      full -- právě když je pravdivé, zpracuj i uživatelské události

    """
    if full:
        if _application is not None:
            _application.Yield()
    else:
        wx.SafeYield()

def refresh():
    """Aktualizuj zobrazení viditelných oken aplikace, pokud je to třeba."""
    _application.refresh()

