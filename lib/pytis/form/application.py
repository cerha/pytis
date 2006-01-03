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

"""Hlavn� aplika�n� okno.

Tento modul definuje t��du 'Application', kter� p�edstavuje hlavn� okno
aplikace a zaji��uje z�kladn� slu�by s�n�m souvisej�c�.  Modul se t�k� pouze
u�ivatelsk�ho rozhran�, ne�e�� obecn� start a zastaven� aplikace.

"""

import os.path
import sys
import time
import cPickle as pickle

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
    """Vytvo� instanci t��dy 'defs.application.Application'.

    Zavolej jej� metodu 'run()'.

    Argumenty:

      resolver -- instance t��dy 'FileResolver', kter� m� b�t pou�ita jako
        resolver; m��e b�t t� 'None', v�kter�m�to p��pad� bude vytvo�en
        implicitn� resolver

    """
    if resolver is None:
        resolver = FileResolver(config.def_dir)
    Application(resolver).run()


class Application(wx.App, KeyHandler, CommandHandler):
    """Aplikace syst�mu Pytis.

    Pro ka�dou aplikaci syst�mu Pytis existuje po celou dobu jej�ho b�hu jedno
    hlavn� aplika�n� okno.  To se sest�v� jednak ze statick�ch prvk� a jednak z
    vym�niteln�ho vnit�ku okna (vlastn�ch formul���).  Statick�mi prvky jsou
    pull-down menu a stavov� ��dek.

    Start aplikace a vytvo�en� statick�ch prvk� je mo�n� parametrizovat
    specifika�n�m souborem aplikace.  T�m je soubor 'application.py' v adres��i
    resolveru (ur�en�m konfigura�n� volbou 'def_dir').  Pou�iteln� specifika�n�
    funkce jsou:

      read_config -- vrac� odkaz na funkci, kter� bude spu�t�na v pr�b�hu
        inicializace aplikace.  Pokud je definov�na, je o�ek�v�no, �e funkce
        na�te konfigura�n� volby z extern�ho zdroje (nap�. z datab�ze) a p�ed�
        je jako svou n�vratovou hodnotu ve form� sekvence dvojic (N�ZEV,
        HODNOTA), kde N�ZEV je v�dy platn�m n�zvem konfigura�n� volby a HODNOTA
        je jej� ulo�en� hodnota.  V�echny takto na�ten� volby budou automaticky
        nastaveny v glob�ln� prom�nn� `config' a vyu��v�ny aplikac�.  Pokud
        funkce nen� definov�na, pokus� se aplikace volby na��st vlastn�m
        mechanismem.

      write_config -- vrac� odkaz na funkci jednoho argumentu, kter� bude
        spu�t�na v pr�b�hu ukon�en� aplikace.  Argumentem je sekvence dvojic,
        stejn� jako v p��pad� n�vratov� hodnoty funkce 'read_config'.  Od
        funkce je o�ek�v�no, �e takto p�edan� volby ulo�� do extern�ho �lo�n�ho
        prostoru, aby mohly b�t obnoveny funkc� 'read_config' p�i p��t�m
        spu�t�n� aplikace.  Pokud funkce nen� definov�na, pokus� se aplikace
        volby ulo�it vlastn�m mechanismem.  Ukl�dan� volby obsahuj� pouze ty
        polo�ky konfigurace, kter� byly zm�n�ny za b�hu aplikace oproti sv�m
        po��te�n�m hodnot�m na�ten�m z p��kazov� ��dky a konfigura�n�ho
        souboru.
      
      menu -- specifikace hlavn�ho menu aplikace ve form�tu specifika�n�ho
        argumentu konstruktoru t��dy 'pytis.form.screen.MenuBar'.
        
      status_fields -- specifikace pol� stavov� ��dky aplikace ve form�tu
        specifika�n�ho argumentu konstruktoru t��dy
        'pytis.form.screen.StatusBar'.
        
      command_keys -- specifikace p�i�azen� kl�ves p��kaz�m jako sekvence
        dvojic (COMMAND, KEY), kde COMMAND je instance t��dy 'Command' a KEY je
        jemu p��slu�n� kl�vesa.

      init -- Tato funkce m��e prov�d�t libovoln�, bl�e neur�en�,
        inicializa�n� akce aplikace.  Je spu�t�na a� po sestaven� hlavn�ho
        aplika�n�ho okna a na�ten� konfigurace, tak�e zde m��eme pracovat i s
        u�ivatelsk�m rozhran�m.

      TODO: N�sleduj�c� volby by bylo vhodn�j�� p�esunout do konfigurace.
        
      default_font_encoding -- implicitn� k�dov�n� font� jako odpov�daj�c� wx
        konstanta.
        
      logo -- N�zev souboru loga zobraen�ho na pozad� aplika�n�ho okna.

    Ka�d� z t�chto funkc� 
        
    Start u�ivatelsk�ho rozhran� spo��v� ve vytvo�en� instance t�to t��dy a
    vol�n� jej� metody 'run()'.
    
    """
    _menubar_forms = {}

    _WINDOW_MENU_TITLE = _("Okn&a")
    _RECENT_FORMS_MENU_TITLE = _("Posledn� otev�en� formul��e")

    def get_command_handler_instance(cls, application):
        return application
    get_command_handler_instance = classmethod(get_command_handler_instance)
    
    def __init__(self, resolver):
        """Inicializuj aplikaci.

        Zde se pouze volaj� konstruktory p�edk�, v�t�ina inicializac� se ve
        skute�nosti prov�d� a� v�metod� 'OnInit'.

        Argumenty:

          resolver -- resolver, kter� m� aplikace pou��vat; instance t��dy
            'pytis.util.Resolver'

        """
        self._resolver = resolver
        wx.App.__init__(self)
    
    def OnInit(self):
        init_colors()
        # Create the main application window.
        title = config.application_name
        if __debug__:
            title += ' (wxWidgets %d.%d.%d)' % \
                     (wx.MAJOR_VERSION, wx.MINOR_VERSION, wx.RELEASE_NUMBER)
        frame = self._frame = wx.Frame(None, -1, title, 
                                       pos=(0,0), size=(800, 600),
                                       style=wx.DEFAULT_FRAME_STYLE)
        wx_callback(wx.EVT_CLOSE, frame, self._on_frame_close)
        # Tento panel slou�� pouze pro odchyt�v�n� kl�vesov�ch ud�lost�,
        # proto�e na frame se ned� nav�sit EVT_KEY_DOWN.
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
        self._windows = XStack()
        self._modals = Stack()
        self._statusbar = StatusBar(self._frame, self._spec('status_fields',()))
        global _application
        _application = self
        # Read the stored configuration.
        read_config = self._spec('read_config', self._read_config)
        items = read_config()
        for option, value in items:
            if option != 'dbconnection':
                setattr(config, option, value)
        log(OPERATIONAL, "Konfigurace na�tena: %d polo�ek" % len(items))
        # Initialize the application.
        keymap = self.keymap = Keymap()
        custom_keys = self._spec('command_keys', ())
        assert is_sequence(custom_keys), "Specifikace kl�vesov�ch zkratek " + \
               "'command_keys' mus� vracet sekvenci dvojic (COMMAND, KEY)."
        for cmd, key in command.DEFAULT_COMMAND_KEYS + custom_keys:
            keymap.define_key(key, cmd)
        command_menu_items = []
        for group in FORM_COMMAND_MENU:
            if command_menu_items:
                command_menu_items.append(MSeparator())
            for title, cmd in group:
                command_menu_items.append(MItem(title, command=cmd))
        menus = self._spec('menu') + (
            Menu(self._WINDOW_MENU_TITLE, ()),
            Menu(_("P��kazy"), command_menu_items))
        self._menubar = mb = MenuBar(self._frame, menus, self)
        # Try to find the recent forms menu.
        menu_id = mb.FindMenu(self._RECENT_FORMS_MENU_TITLE)
        if menu_id != -1:
            menu = mb.GetMenu(menu_id)
        else:
            for i in range(mb.GetMenuCount()):
                m = mb.GetMenu(i)
                menu_id = m.FindItem(self._RECENT_FORMS_MENU_TITLE)
                if menu_id != -1:
                    menu = m.FindItemById(menu_id).GetSubMenu()
                    break
            else:
                menu = None
        self._recent_forms_menu = menu
        default_font_encoding = self._spec('default_font_encoding')
        if default_font_encoding is not None:
            wx.Font.SetDefaultEncoding(default_font_encoding)
        wx_callback(wx.EVT_SIZE, self._frame, self._on_frame_size)
        self.SetTopWindow(self._frame)
        self._frame.Show(True)
        # Run application specific initialization.
        self._spec('init')
        self._panel.SetFocus()
        # Open the startup forms.
        if config.startup_forms:
            for name in config.startup_forms.split(','):
                separator_position = name.find('/')
                if separator_position != -1:
                    cls_name = name[:separator_position].strip()
                    name = name[separator_position+1:]
                    try:
                        form_cls = getattr(pytis.form, cls_name)
                        if not issubclass(form_cls, Form):
                            raise AttributeError
                    except AttributeError:
                        msg = _("Neplatn� formul��ov� t��da v 'startup_forms':")
                        self.run_dialog(Error, msg +' '+ cls_name)
                        continue
                else:
                    form_cls = BrowseForm
                self.run_form(form_cls, name.strip())
        return True

    def _spec(self, name, default=None, **kwargs):
        try:
            result = self._resolver.get('application', name, **kwargs)
        except ResolverError, e:
            log(OPERATIONAL, str(e))
            result = default
        return result

    
    # Ostatn� metody

    def _check_perm(self, perm, name):
        try:
            data_spec = self._resolver.get(name, 'data_spec')
        except ResolverError:
            return True
        rights = data_spec.access_rights()
        if not rights:
            return True
        connection = config.dbconnection
        groups = pytis.data.DBDataDefault.class_access_groups(connection)
        result = rights.permitted(perm, groups)
        return result

    def _form_menu_item_title(self, form):
        title = form.title()
        if form.__class__ != BrowseForm:
            title += " (%s)" % form.descr()
        return title
    
    def _update_window_menu(self, recreate=True):
        def wmitem(i, form):
            return RadioItem("&%d. %s" % (i, self._form_menu_item_title(form)),
                             help=_('Vyzvednout okno formul��e "%s" (%s/%s)') %\
                             (form.title(),form.__class__.__name__,form.name()),
                             command=Application.COMMAND_RAISE_FORM,
                             state=lambda a: a.top_window() is form,
                             args={'form': form})
        mb = self._menubar
        menu = mb.GetMenu(mb.FindMenu(self._WINDOW_MENU_TITLE))
        if menu is None:
            return 
        if recreate:
            for item in menu.GetMenuItems():
                menu.Remove(item.GetId())
                item.Destroy()
            for i, form in enumerate(self._windows.items()):
                menu.AppendItem(wmitem(i+1, form).create(self._frame, menu))

    def _update_recent_forms(self, item=None):
        menu = self._recent_forms_menu
        if menu is not None:
            recent = config.form_state.get('recent_forms', [])
            if item is not None:
                try:
                    recent.remove(item)
                except ValueError:
                    pass
                recent.insert(0, item)
                config.form_state['recent_forms'] = recent[:10]
            for item in menu.GetMenuItems():
                menu.Remove(item.GetId())
                item.Destroy()
            for item in self._recent_forms_menu_items():
                if isinstance(item, MSeparator):
                    menu.AppendSeparator()
                else:
                    menu.AppendItem(item.create(self._frame, menu))
                
    def _recent_forms_menu_items(self):
        recent = config.form_state.get('recent_forms', [])
        items = [MItem(title,
                       help=_('Otev��t formul�� "%s" (%s/%s)') %
                            (title, args['form_class'].__name__, args['name']),
                       command=Application.COMMAND_RUN_FORM, args=args)
                 for title, args in recent]
        items.append(MSeparator())
        items.append(MItem(_("Vy�istit"),
                           help=_("Vymazat menu posledn� otev�en�ch formul���"),
                           command=Application.COMMAND_CLEAR_RECENT_FORMS))
        return items
        
        
    def _raise_form(self, form):
        if form is not None:
            if form not in self._frame.GetChildren():
                log(EVENT, "Reparent -- mo�n� je to tu opravdu pot�eba...")
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

    def _config_name(self):
        # Return a name for saving/restoring the configuration.
        # This name must be usable as a filename and should be
        # application-specific, so we will derive it from the application name
        # avoiding any non-alphanumeric characters.
        import re
        import unicodedata
        def safe_char(match):
            base_name = re.sub(" WITH .*", '', unicodedata.name(match.group(0)))
            base_char = unicodedata.lookup(base_name)
            return base_char.isalnum() and base_char or '-'
        name = config.application_name.lower()
        return str(re.sub("[^a-zA-Z0-9-]", safe_char, unicode(name)))

    def _stored_options(self, wxconfig):
        options = []
        cont, key, index = wxconfig.GetFirstEntry()
        while cont:
            options.append(key)
            cont, key, index = wxconfig.GetNextEntry(index)
        return options

    def _read_config(self):
        name = self._config_name()
        log(OPERATIONAL, "Na��t�m konfiguraci v�choz� metodou:", name)
        wxconfig = wx.Config(name)
        mapping = ((pytis.data.String,  wxconfig.Read),
                   (pytis.data.Integer, wxconfig.ReadInt),
                   (pytis.data.Boolean, wxconfig.ReadBool))
        items = []
        for option in self._stored_options(wxconfig):
            t = config.type(option)
            for type, read in mapping:
                if isinstance(t, type):
                    value = read(option)
            else:
                value = pickle.loads(str(wxconfig.Read(option)))
            items.append((option, value))
        return tuple(items)
            
    def _write_config(self, items):
        name = self._config_name()
        log(OPERATIONAL, "Ukl�d�m konfiguraci v�choz� metodou:", name)
        wxconfig = wx.Config(name)
        mapping = ((pytis.data.String,  wxconfig.Write),
                   (pytis.data.Integer, wxconfig.WriteInt),
                   (pytis.data.Boolean, wxconfig.WriteBool))
        for option in self._stored_options(wxconfig):
            wxconfig.DeleteEntry(option)
        for option, value in items:
            t = config.type(option)
            for type, write in mapping:
                #TODO: Co None hodnoty???
                if isinstance(t, type):
                    write(option, value)
            else:
                wxconfig.Write(option, pickle.dumps(value))

    def _on_frame_close(self, event):
        if not self._cleanup():
            event.Veto()
        else:
            event.Skip()
            global _application
            _application = None


    def _cleanup(self, quietly=False):
        # Zde ignorujeme v�emo�n� v�jimky, aby i�p�i pom�rn� zna�n� havarijn�
        # situaci bylo mo�no aplikaci ukon�it.
        try:
            log(ACTION, 'Vol�no ukon�en� aplikace')
        except:
            pass
        try:
            if not self._modals.empty():
                log(EVENT, "Nen� mo�no zav��t aplikaci s mod�ln�m oknem:",
                    self._modals.top())
                return False
            if not quietly and not self._windows.empty():
                q = _("Aplikace obsahuje otev�en� formul��e\n" + \
                      "Opravdu chcete ukon�it aplikaci?")
                if not self.run_dialog(Question, q):
                    return False
        except:
            pass
        try:
            while not self._windows.empty():
                try:
                    self.leave_form()
                except:
                    break
        except:
            pass
        try:
            items = tuple([(o, getattr(config, o))
                           for o in config.options()
                           if config.changed(o) and o != 'dbconnection'])
            write_config = self._spec('write_config', self._write_config)
            write_config(items)
            log(OPERATIONAL, "Konfigurace ulo�ena: %d polo�ek" % len(items))
        except Exception, e:
            try:
                log(EVENT, "Saving changed configuration failed:", str(e))
            except:
                pass
        return True
        
    # Ostatn� ve�ejn� metody

    def run_form(self, form_class, name, *args, **kwargs):
        """Vytvo� formul�� a spus� jej.

        Argumenty:

          form_class -- t��da vytv��en�ho formul��e (libovoln� t��da odvozen�
            od t��dy `Form')
          name -- jm�no specifikace pro resolver

        Dal�� argumenty budou p�ed�ny konstruktoru formul��e, tak jak n�sleduj�
        za argumentem `name'.  Argumenty `parent' a `resolver'
        budou dopln�ny automaticky.

        Vytvo�en� formul�� bude zobrazen v okn� aplikace, nebo v nov�m mod�ln�m
        okn�, pokud jde o mod�ln� formul�� odvozen� od t��dy 'PopupForm'.
        Mod�ln� formul�� je spu�t�n metodou 'run()' a jej� v�sledek je
        n�vratovou hodnotou vol�n� t�to metody.  V tomto p��pad� je v�ak n�vrat
        z t�to metody proveden a� po ukon�en� formul��e (uzav�en� jeho okna).
        Pro nemod�ln� formul��e se metoda vrac� ihned po zobrazen� okna s
        n�vratovou hodnotou None.  Nemod�ln� formul�� je potom nutn� z aplikace
        odstranit metodou 'leave_form()'.
          
        """
        # TODO: Toto je jen kv�li zp�tn� kompatibilit�.  Argument 'key' je
        # t�eba ve v�ech projektech p�ejmenovat na 'select_row' a n�sleduj�c�
        # ��dky �asem zru�it.
        if kwargs.has_key('key'):
            kwargs['select_row'] = kwargs['key']
            del kwargs['key']
        if issubclass(form_class, EditForm):
            if kwargs.has_key('new'):
                if kwargs['new']:
                    kwargs['mode'] = EditForm.MODE_INSERT
                del kwargs['new']
            if not kwargs.get('select_row') and not kwargs.has_key('mode'):
                kwargs['mode'] = EditForm.MODE_INSERT
        # konec do�asn�ho hacku    
        result = None
        state_kwargs = {}
        for arg in ('select_row', ):
            if kwargs.has_key(arg):
                state_kwargs[arg] = kwargs[arg]
                del kwargs[arg]
        try:
            if callable(name):
                name = name()
                if name is None:
                    return None
            log(ACTION, 'Vytv���m nov� formul��:',
                (form_class, name, args, kwargs))
            message(_("Spou�t�m formul��..."), root=True)
            wx_yield_()
            assert issubclass(form_class, Form)
            assert is_anystring(name)
            result = None
            self.save()
            form = find((form_class, name), self._windows.items(),
                        key=lambda f: (f.__class__, f.name()))
            if form is not None:
                self._raise_form(form)
                message(_('Formul�� "%s" nalezen na z�sobn�ku oken.') % \
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
                self.run_dialog(Error, _("Formul�� se nepoda�ilo vytvo�it"))
            else:
                self._set_form_state(form, **state_kwargs)
                if isinstance(form, PopupForm):
                    log(EVENT, "Zobrazuji mod�ln� formul��:", form)
                    self._modals.push(form)
                    message('', root=True)
                    form.show()
                    try:
                        result = form.run()
                        log(EVENT, "Mod�ln� formul�� byl uzav�en:", form)
                        log(EVENT, "N�vratov� hodnota:", result)
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
                    log(EVENT, "Zobrazuji nemod�ln� formul��:", form)
                    old = self._windows.active()
                    if old is not None:
                        old.hide()
                    self._windows.push(form)
                    message('', root=True)
                    form.show()
                    self._activate(form.ACTIVATIONS, form)
                    self._update_window_menu()
                    item = (self._form_menu_item_title(form),
                            dict(form_class=form_class, name=name))
                    self._update_recent_forms(item)
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def can_run_form(self, form_class, name, *args, **kwargs):
        perm = pytis.data.Permission.VIEW
        if issubclass(form_class, pytis.form.DualForm):
            try:
                dual_spec = resolver().get(name, 'dual_spec')
            except ResolverError:
                return True
            result = self._check_perm(perm, dual_spec.main_name()) and \
                     self._check_perm(perm, dual_spec.side_name())
        else:
            result = self._check_perm(perm, name)
        return result

    def run_procedure(self, spec_name, proc_name, block_refresh_=False,
                      **kwargs):
        """Spus� proceduru.

        Argumenty:

          spec_name -- jm�no specifikace pro resolver.
          proc_name -- jm�no procedury, kter� m� b�t spu�t�na.  Jde o kl�� do
            slovn�ku, kter� je vracen specifika�n� funkc� 'proc_spec'.

        Kl��ov� argumenty budou p�ed�ny spou�t�n� procedu�e.

        N�vratov� hodnota procedury je n�vratovou hodnotou vol�n� t�to metody.         

        """
        result = None
        try:
            log(ACTION, 'Spou�t�m proceduru:', (spec_name, proc_name, kwargs))
            message(_("Spou�t�m proceduru..."), root=True, timeout=2)
            # Kv�li wx.SafeYield() se ztr�c� focus, tak�e
            # si ho ulo��me a pak zase obnov�me.
            focused = wx_focused_window()
            wx_yield_()
            spec = self._resolver.get(spec_name, 'proc_spec')
            assert is_dictionary(spec), \
                   _("Specifikace procedur 'proc_spec' mus� vracet slovn�k!")
            assert spec.has_key(proc_name), \
                  _("Specifikace procedur neobsahuje definici '%s'") % proc_name
            proc = spec[proc_name]
            if block_refresh_:
                result = block_refresh(proc, **kwargs)
            else:
                result = proc(**kwargs)
            log(ACTION, "N�vratov� hodnota procedury:", result)
            if focused:
                focused.SetFocus()
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def new_record(self, name, key=None, prefill=None):
        """Spus� interaktivn� akci p�id�n� nov�ho z�znamu.
        
        Argumenty:
        
          name -- jm�no specifikace pro resolver.
          key -- kl�� kop�rovan�ho z�znamu, nebo None.
          prefill -- slovn�k �et�zcov�ch (u�ivatelsk�ch) hodnot, kter� maj� b�t
            p�edvypln�ny p�i inicializaci formul��e.

        """
        view = self._resolver.get(name, 'view_spec')
        on_new_record = view.on_new_record()
        if on_new_record is not None:
            result = on_new_record(key=key, prefill=prefill)
            top = self.current_form()
            if isinstance(top, Refreshable):
                top.refresh()
        else:
            result = run_form(PopupEditForm, name, select_row=key,
                              mode=EditForm.MODE_INSERT, prefill=prefill)
        return result

    def can_new_record(self, name, key=None, prefill=None):
        return self._check_perm(pytis.data.Permission.INSERT, name)
    
    def run_dialog(self, dialog_or_class_, *args, **kwargs):
        """Zobraz dialog ur�en� t��dy s hlavn�m oknem aplikace jako rodi�em.

        Argumenty:
          dialog_or_class_ -- t��da dialogu (odvozen� od t��dy 'Dialog'), nebo
            p��mo instance.  Pokud jde o t��du, bude vytvo�ena nov� instance a
            ta bude n�sledn� spu�t�na.
          
        Jako prvn� argument konstruktoru dialogov� t��dy ('parent') bude
        dopln�no aktu�ln� (vrchn�) okno aplikace.  Ostatn� argumenty jsou
        p�ed�ny tak, jak jsou.  V�ce o dialogov�ch t��d�ch a jejich argumentech
        konstruktoru v modulu 'pytis.form.dialog'.

        Pokud je argumentem instance, jsou argumenty p�ed�ny metod� 'run()'.

        Dialog je spu�t�n (metodou 'run()') a jeho n�vratov� hodnota je tak�
        n�vratovou hodnotou t�to metody.
        
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
        """Uzav�i aktu�ln� formul�� otev�en� v okn� aplikace.

        Pokud nen� otev�en ��dn� formul��, zaloguj a jinak ned�lej nic.
        
        """
        form = self._windows.active()
        if form:
            log(EVENT, "Zav�r�m okno nemod�ln�ho formul��e:", form)
            form.defocus()
            self._windows.remove(form)
            self._update_window_menu()
            form.close()
            self.restore()
        else:
            log(EVENT, "Nen� otev�en ��dn� formul��.")
                
    def exit(self, quietly=False):
        """Ukon�i u�ivatelsk� rozhran� aplikace.

        Argumenty:

          quietly -- pr�v� kdy� je pravdiv�, nejsou p�i ukon�en� kladeny
            u�ivateli ��dn� dotazy

        """
        self._frame.Close()

    def run(self):
        """Spus� b�h u�ivatelsk�ho rozhran�.

        Nevracej se d��ve, ne� je b�h u�ivatelsk�ho rozhran� definitivn�
        ukon�en.

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
        """Vra� moment�ln� aktivn� okno aplikace.
        
        """
        if not self._modals.empty():
            return self._modals.top()
        else:
            return self._windows.active()

    def current_form(self):
        """Vra� pr�v� aktivn� formul�� aplikace, pokud existuje.
        
        Pokud nen� otev�en ��dn� formul��, nebo aktivn�m oknem nen� formul��,
        vrac� None.  Pokud je otev�en�m formul��em du�ln� formul��, bude vr�cen
        jeho pr�v� aktivn� podformul��.
        
        """
        top = self.top_window()
        if isinstance(top, Form):
            if isinstance(top, DualForm):
                return top.active_form()
            else:
                return top
        else:
            return None
        
    def refresh(self):
        """Aktualizuj zobrazen� viditeln�ch oken aplikace, pokud je to t�eba."""
        for w in (self._modals.top(), self._windows.active()):
            if isinstance(w, Refreshable):
                w.refresh()
        
    def set_status(self, id, message, timeout=None, root=False):
        """Nastav v poli stavov� ��dky dan�ho 'id' zpr�vu 'message'.
        
        Argumenty:
        
          id -- identifik�tor pole stavov� ��dky.
          message -- string, kter� m� b�t zobrazen, nebo 'None'; je-li 'None',
            bude p�edchoz� hl�en� smaz�no.
          timeout -- nen�-li 'None', zpr�va zmiz� po zadan�m po�tu sekund.
          root -- je-li pravdiv�, bude zpr�va zobrazena v�dy v hlavn�m okn�
            aplikace.  Pokud ne, je zpr�va zobrazena ve stavov� ��dce hlavn�ho
            okna aplikace a� v p��pad�, �e nen� otev�eno ��dn� mod�ln� okno,
            nebo se zobrazen� zpr�vy v mod�ln�m okn� nepoda�ilo.

        Zobrazen� nen� garantov�no, nemus� se zobrazit nap��klad v�p��pad�, kdy
        stavov� ��dek neobsahuje odpov�daj�c� pole.

        """
        modal = self._modals.top()
        if root or not isinstance(modal, Form) \
               or not modal.set_status(id, message):
            return self._statusbar.message(id, message, timeout=timeout)
            
    def get_status(self, id):
        """Vra� text pole 'id' stavov�ho ��dku hlavn�ho okna aplikace.

        Pokud stavov� ��dek dan� pole neobsahuje, vra� None.
        
        """
        return self._statusbar.get_message(id)

    def resolver(self):
        """Vra� resolver instanc� podle jm�na; instance 'pytis.util.Resolver'."""
        return self._resolver

    def wx_frame(self):
        """Vra� instanc� 'wx.Frame' hlavn�ho okna aplikace."""
        return self._frame

    def save(self):
        """Ulo� stav aplikace."""
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

    def recent_forms_menu(self):
        """Vra� menu posledn� otev�en�ch formul��� jako instanci 'Menu'."""
        return Menu(self._RECENT_FORMS_MENU_TITLE,
                    self._recent_forms_menu_items())

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
        # Toto je z�chrann� odchyt�va�.  V��te tomu nebo ne, ale pokud tady ta
        # metoda nen�, wxWindows se p�i v�ce p��le�itostech po stisku kl�vesy
        # zhrout�.
        return KeyHandler.on_key_down(self, event)

    def on_command(self, command, **kwargs):
        """Po�li 'command' s�'kwargs' aktu�ln�mu oknu.

        Nen�-li ��dn� aktu�ln� okno nebo pokud toto okno nem� metodu
        'on_command()', ned�lej nic a vra� nepravdu.

        """
        log(command.log_kind(), 'Vyvol�n p��kaz:', (command, kwargs))
        try:
            try:
                busy_cursor(True)
                if issubclass(command.cls(), Application) \
                       and command.handler() is not None:
                    command.handler()(**kwargs)
                elif command == Application.COMMAND_SHOW_POPUP_MENU:
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
                elif command == Application.COMMAND_CLEAR_RECENT_FORMS:
                    config.form_state['recent_forms'] = []
                    self._update_recent_forms()
                elif __debug__ and command == Application.COMMAND_CUSTOM_DEBUG:
                    config.custom_debug()
                else:
                    top = self._windows.active()
                    if top is not None and top.on_command(command, **kwargs):
                        return True
                    return False
            finally:
                busy_cursor(False)
            return True
        except UserBreakException:
            pass
        except:
            top_level_exception()

    def can_next_form(self):
        return len(self._windows.items()) > 1

    def can_prev_form(self):
        return len(self._windows.items()) > 1

    def can_leave_form(self):
        return self.current_form() is not None

    def can_clear_recent_forms(self):
        return len(config.form_state.get('recent_forms', ())) > 0



# Funkce

def message(message, kind=EVENT, data=None, beep_=False, timeout=None,
            root=False):
    """Zaloguj a zobraz neinteraktivn� 'message' v�okn� aplikace.

    Argumenty:

      message -- �et�zec, kter� m� b�t zobrazen; obsahuje-li jako posledn� znak
        dvojte�ku, nen� tato v�okn� aplikace zobrazena
      kind -- druh zpr�vy, jedna z�konstant modulu 'log'
      data -- dopl�uj�c� data pro logov�n�, stejn� jako v�'log.log'
      beep_ -- pr�v� kdy� je pravdiv�, bude hl�en� doprov�zeno p�pnut�m
      timeout -- pokud je zad�no, zpr�va zmiz� po zadan�m po�tu sekund
      root -- je-li pravdiv�, bude zpr�va zobrazena v�dy v hlavn�m okn�
        aplikace.  Pokud ne, je zpr�va zobrazena ve stavov� ��dce hlavn�ho okna
        aplikace a� v p��pad�, �e nen� otev�eno ��dn� mod�ln� okno, nebo se
        zobrazen� zpr�vy v mod�ln�m okn� nepoda�ilo.
        
    Pro zobrazen� zpr�vy ve stavov� ��dce plat� stejn� pravidla, jako v p��pad�
    metody 'Application.set_status()'.  Zalogov�no je v�ak v�ka�d�m p��pad�.

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
    """Nastav pole 'id' stavov� ��dky na 'message'.

    Argumenty:

      id -- identifik�tor pole stavov� ��dky.
      message -- �et�zec, kter� m� b�t zobrazen.
      log_ -- pokud je pravda, bude ud�lost zalogov�na.

    Pro zobrazen� zpr�vy ve stavov� ��dce plat� stejn� omezen�, jako v p��pad�
    metody 'Application.set_status()'.  Zalogov�no je v�ak v�ka�d�m p��pad�.

    """
    if __debug__:
        if log_: log(DEBUG, "Nastaven� pole stavov� ��dky:", data=(id, message))
    return _application.set_status(id, message)

def get_status(id):
    """Vra� text pole 'id' stavov� ��dky. (viz 'Application.get_status()')"""
    return _application.get_status(id)

def run_dialog(*args, **kwargs):
    """Zobraz dialog v okn� aplikace (viz 'Application.run_dialog()')."""
    return _application.run_dialog(*args, **kwargs)

def run_form(*args, **kwargs):
    """Zobraz formul�� v okn� aplikace (viz 'Application.run_form()')."""
    return _application.run_form(*args, **kwargs)

def run_procedure(*args, **kwargs):
    """Spus� proceduru (viz 'Application.run_procedure()')."""
    return _application.run_procedure(*args, **kwargs)

def new_record(*args, **kwargs):
    """Spus� akci p�id�n� nov�ho z�znamu (viz 'Application.new_record()')."""
    return _application.new_record(*args, **kwargs)

def leave_form():
    """Odstra� aktu�ln� okno formul��e z aplikace."""
    return _application.leave_form()

def current_form():
    """Vra� pr�v� zobrazen� formul�� aktu�ln� aplikace, pokud existuje."""
    return _application.current_form()

def resolver():
    """Vra� resolver aplikace z�skan� p�es 'Application.resolver()'."""
    return _application.resolver()

def wx_frame():
    """Vra� instanc� 'wx.Frame' hlavn�ho okna aplikace."""
    return _application.wx_frame()

def exit(**kwargs):
    """Zavolej 'Application.exit() aktu�ln� aplikace a p�edej argumenty."""
    return _application.exit(**kwargs)

def global_keymap():
    """Vra� kl�vesovou mapu aplikace jako instanci t��dy 'Keymap'."""
    try:
        return _application.keymap
    except AttributeError:
        return Keymap()

def wx_yield_(full=False):
    """Zpracuj wx messages ve front�.

    Argumenty:

      full -- pr�v� kdy� je pravdiv�, zpracuj i�u�ivatelsk� ud�losti

    """
    if full:
        if _application is not None:
            _application.Yield()
    else:
        wx.SafeYield()

def refresh():
    """Aktualizuj zobrazen� viditeln�ch oken aplikace, pokud je to t�eba."""
    _application.refresh()


def block_refresh(function, *args, **kwargs):
    """Zablokuj ve�ker� refresh po dobu prov�d�n� funkce 'function'.
    
    Vrac�: v�sledek vr�cen� volanou funkc�.
    
    """
    return Refreshable.block_refresh(function, *args, **kwargs)

def recent_forms_menu():
    """Vra� menu posledn� otev�en�ch formul��� jako instanci 'pytis.form.Menu'.

    Tato funkce je ur�ena pro vyu�it� p�i definici menu aplikace.  Pokud ji
    pou�ijeme, bude aplikace toto menu d�le obhospoda�ovat.
        
    """
    return _application.recent_forms_menu()
