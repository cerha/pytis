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
import wx.html

_application = None

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
        
      keymap -- specifikace p�i�azen� kl�ves p��kaz�m jako sekvence trojic
        (KEY, COMMAND, ARGS), kde KEY je definice kl�vesov� zkratky, COMMAND je
        instance t��dy 'Command' a ARGS je slovn�k argument�, kter� maj� b�t
        p��kazu p�ed�ny.

      init -- Tato funkce m��e prov�d�t libovoln�, bl�e neur�en�,
        inicializa�n� akce aplikace.  Je spu�t�na a� po sestaven� hlavn�ho
        aplika�n�ho okna a na�ten� konfigurace, tak�e zde m��eme pracovat i s
        u�ivatelsk�m rozhran�m.

      TODO: N�sleduj�c� volbu by bylo vhodn�j�� p�esunout do konfigurace.
        
      default_font_encoding -- implicitn� k�dov�n� font� jako odpov�daj�c� wx
        konstanta.
        
    Start u�ivatelsk�ho rozhran� spo��v� ve vytvo�en� instance t�to t��dy a
    vol�n� jej� metody 'run()'.
    
    """
    _menubar_forms = {}

    _WINDOW_MENU_TITLE = _("Okn&a")
    _RECENT_FORMS_MENU_TITLE = _("Posledn� otev�en� formul��e")

    def _get_command_handler_instance(cls):
        global _application
        return _application
    _get_command_handler_instance = classmethod(_get_command_handler_instance)

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
        logo_file = config.logo
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
        self._help_controller = None
        self._help_files = self._find_help_files()
        keymap = self.keymap = Keymap()
        custom_keymap = self._spec('keymap', ())
        assert is_sequence(custom_keymap), "Specifikace kl�vesov�ch zkratek " +\
               "'keymap' mus� vracet sekvenci dvojic (KEY, COMMAND)."
        for key, cmd in command.DEFAULT_KEYMAP + custom_keymap:
            if is_sequence(cmd):
                cmd, args = cmd
            else:
                args = {}
            keymap.define_key(key, cmd, args)
        global _application
        _application = self
        # Read the stored configuration.
        read_config = self._spec('read_config', self._read_config)
        items = read_config()
        for option, value in items:
            if option != 'dbconnection':
                setattr(config, option, value)
        log(OPERATIONAL, "Konfigurace na�tena: %d polo�ek" % len(items))
        # Init the recent forms list.
        recent_forms = config.application_state.get('recent_forms')
        if not isinstance(recent_forms, types.ListType):
            recent_forms = []
        self._recent_forms = []
        for title, args in recent_forms:
            try:
                assert issubclass(args['form_class'], Form), \
                       'Not a valid form class: %s' % args['form_class']
                # This is a simple way to test whether the specification
                # still exists.
                self._can_run_form(**args)
            except Exception, e:
                log(OPERATIONAL, "Ignoring recent form:", (args, e))
                continue
            self._recent_forms.append((title, args))
        config.application_state['recent_forms'] = self._recent_forms
        # Initialize the menubar.
        menus = list(self._spec('menu', ()))
        menus.append(Menu(self._WINDOW_MENU_TITLE, ()))
        self._create_command_menu(menus)
        self._create_help_menu(menus)
        self._menubar = mb = MenuBar(self._frame, menus, self.keymap)
        self._window_menu = mb.GetMenu(mb.FindMenu(self._WINDOW_MENU_TITLE))
        assert self._window_menu is not None
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
        if self._windows.empty():
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
                run_form(form_cls, name.strip())
        conn = config.dbconnection
        if conn:
            # Pozor, pokud b�hem inicializace aplikace nedojde k p�ipojen� k
            # datab�zi (nen� vyvol�na ��dn� datab�zov� operace), nemus� b�t
            # hodnoty spr�vn�.
            title = self._frame.GetTitle()            
            title += " %s@%s %s" % (conn.user(), conn.database(),
                                    conn.host())
            if conn.port():
                title += ":%d" % conn.port()
            self._frame.SetTitle(title)
        return True

    def _spec(self, name, default=None, **kwargs):
        try:
            result = resolver().get('application', name, **kwargs)
        except ResolverError, e:
            log(OPERATIONAL, str(e))
            result = default
        return result

    def _find_help_files(self):
        if not os.path.exists(config.help_dir):
            log(OPERATIONAL, "Neexistuj�c� adres�� n�pov�dy:", config.help_dir)
            return []
        result = []
        import zipfile
        for file in os.listdir(config.help_dir):
            if os.path.splitext(file)[1].lower() == '.zip':
                filename = os.path.join(config.help_dir, file)
                zfile = zipfile.ZipFile(filename)
                for f in zfile.namelist():
                    if f.endswith('.hhp'):
                        values = {}
                        for line in zfile.read(f).splitlines():
                            pos = line.find('=')
                            if pos != -1:
                                key = line[:pos].lower().strip()
                                values[key] = line[pos+1:].strip()
                        if values.has_key('default topic') and \
                           values.has_key('title'):
                            title = values['title']
                            index = os.path.splitext(values['default topic'])[0]
                            if values.has_key('charset'):
                                title = unicode(title, values['charset'])
                            result.append((filename, index, title))

        if len(result) == 0:
            log(OPERATIONAL, "��dn� soubory n�pov�dy nebyly nalezeny:",
                config.help_dir)
        return result

    def _create_command_menu(self, menus):
        items = []
        for group in FORM_COMMAND_MENU:
            if items:
                items.append(MSeparator())
            for cmd, title, help in group:
                if is_sequence(cmd):
                    cmd, args = cmd
                else:
                    args = {}
                items.append(MItem(title, command=cmd, args=args, help=help))
        menus.append(Menu(_("P��kazy"), items))

    def _create_help_menu(self, menus):
        if [m for m in menus if m.title() == _("N�pov�da")]:
            log(OPERATIONAL, "Menu n�pov�dy nalezeno - nevytv���m vlastn�.")
            return
        items = [MItem(title, command=Application.COMMAND_HELP(topic=index))
                 for file, index, title in self._help_files]
        if items:
            items.extend((MSeparator(),
                          MItem(_("N�pov�da k aktu�ln�mu formul��i"), 
                                command=Form.COMMAND_HELP)))
            menus.append(Menu(_("N�pov�da"), items))

    # Ostatn� metody

    def _form_menu_item_title(self, form):
        title = form.title()
        if form.__class__ != BrowseForm:
            title += " (%s)" % form.descr()
        return title

    def _update_window_menu(self):
        def wmitem(i, form):
            return CheckItem("&%d. %s" % (i, self._form_menu_item_title(form)),
                             help=_('Vyzvednout okno formul��e "%s" (%s/%s)') %\
                             (form.title(),form.__class__.__name__,form.name()),
                             command=Application.COMMAND_RAISE_FORM,
                             state=lambda : top_window() is form,
                             args={'form': form})
        menu = self._window_menu
        if menu is not None:
            for item in menu.GetMenuItems():
                menu.Remove(item.GetId())
                item.Destroy()
            for i, form in enumerate(self._windows.items()):
                menu.AppendItem(wmitem(i+1, form).create(self._frame, menu))

    def _update_recent_forms(self, item=None):
        menu = self._recent_forms_menu
        if menu is not None:
            recent = self._recent_forms
            if item is not None:
                try:
                    recent.remove(item)
                except ValueError:
                    pass
                recent.insert(0, item)
                if len(self._recent_forms) > 10:
                    self._recent_forms[10:] = []
            for item in menu.GetMenuItems():
                menu.Remove(item.GetId())
                item.Destroy()
            for item in self._recent_forms_menu_items():
                if isinstance(item, MSeparator):
                    menu.AppendSeparator()
                else:
                    menu.AppendItem(item.create(self._frame, menu))
                
    def _recent_forms_menu_items(self):
        items = [MItem(title,
                       help=_('Otev��t formul�� "%s" (%s/%s)') %
                            (title, args['form_class'].__name__, args['name']),
                       command=Application.COMMAND_RUN_FORM, args=args)
                 for title, args in self._recent_forms]
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

    def _post_init_form(self, form, select_row=None):
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
        to_delete = self._stored_options(wxconfig)
        mapping = ((pytis.data.String,  wxconfig.Write),
                   (pytis.data.Integer, wxconfig.WriteInt),
                   (pytis.data.Boolean, wxconfig.WriteBool))
        for option, value in items:
            t = config.type(option)
            if option in to_delete:
                to_delete.remove(option)
            for type, write in mapping:
                #TODO: Co None hodnoty???
                if isinstance(t, type):
                    write(option, value)
            else:
                wxconfig.Write(option, pickle.dumps(value))
        for option in to_delete:
            wxconfig.DeleteEntry(option)
        wxconfig.Flush()

    def _cleanup(self):
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
            if not self._windows.empty():
                q = _("Aplikace obsahuje otev�en� formul��e\n" + \
                      "Opravdu chcete ukon�it aplikaci?")
                if not self.run_dialog(Question, q):
                    return False
        except:
            pass
        try:
            for form in self._windows.items():
                try:
                    self._raise_form(form)
                    if not form.close():
                        return False
                except:
                    continue
        except:
            pass
        try:
            items = tuple([(o, getattr(config, o))
                           for o in configurable_options() + \
                           ('application_state', 'form_state') 
                           if config.changed(o)])
            write_config = self._spec('write_config', self._write_config)
            write_config(items)
            log(OPERATIONAL, "Konfigurace ulo�ena: %d polo�ek" % len(items))
        except Exception, e:
            try:
                log(EVENT, "Saving changed configuration failed:", str(e))
            except:
                pass
        try:
            if self._help_controller is not None:
                self._help_controller.GetFrame().Close()
        except:
            pass
        return True

    # Callbacky

    def _on_frame_close(self, event):
        if not self._cleanup():
            event.Veto()
        else:
            event.Skip()
            global _application
            _application = None

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

    def _on_form_close(self, event):
        form = event.GetEventObject()
        assert form is self._windows.active()
        log(EVENT, "Okno nemod�ln�ho formul��e uzav�eno:", form)
        self._windows.remove(form)
        self._update_window_menu()
        self.restore()
    
    def on_key_down(self, event, dont_skip=False):
        # Toto je z�chrann� odchyt�va�.  V��te tomu nebo ne, ale pokud tady ta
        # metoda nen�, wxWindows se p�i v�ce p��le�itostech po stisku kl�vesy
        # zhrout�.
        return KeyHandler.on_key_down(self, event)

    # Zpracov�n� p��kaz�

    def _cmd_break(self):
        message(_("Stop"), beep_=True)
        
    def _can_handled_action(self, handler=None, enabled=None, **kwargs):
        return enabled is None and True or enabled(**kwargs)
        
    def _cmd_handled_action(self, handler=None, enabled=None, **kwargs):
        return handler(**kwargs)
        
    def _cmd_raise_form(self, form):
        self._raise_form(form)
        
    def _can_raise_recent_form(self):
        return len(self._windows.mru()) > 1
        
    def _cmd_raise_recent_form(self):
        self._raise_form(self._windows.mru()[1])
       
    def _can_raise_next_form(self):
        return len(self._windows.items()) > 1
    
    def _cmd_raise_next_form(self):
        self._raise_form(self._windows.next())

    def _can_raise_prev_form(self):
        return len(self._windows.items()) > 1

    def _cmd_raise_prev_form(self):
        self._raise_form(self._windows.prev())

    def _can_clear_recent_forms(self):
        return len(self._recent_forms) > 0

    def _cmd_clear_recent_forms(self):
        self._recent_forms[:] = []
        self._update_recent_forms()

    def _cmd_refresh(self):
        for w in (self._modals.top(), self._windows.active()):
            if isinstance(w, Refreshable):
                w.refresh()

    def _can_run_form(self, form_class, name, **kwargs):
        if isinstance(self.current_form(), PopupForm) \
           and not issubclass(form_class, PopupForm):
            return False
        if issubclass(form_class, DualForm) and \
               not issubclass(form_class, DescriptiveDualForm):
            main_name, side_name = name.split('::')
            result = has_access(main_name) and has_access(side_name)
        else:
            result = has_access(name)
        return result

    def _cmd_run_form(self, form_class, name, select_row=None, **kwargs):
        # Dokumentace viz funkce run_form().
        result = None
        try:
            if callable(name):
                name = name()
                if name is None:
                    return None
            log(ACTION, 'Vytv���m nov� formul��:', (form_class, name, kwargs))
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
                self._post_init_form(form, select_row=select_row)
                return result
            if issubclass(form_class, PopupForm):
                parent = self._modals.top() or self._frame
                kwargs['guardian'] = self._modals.top() or self
            else:
                assert self._modals.empty()
                kwargs['guardian'] = self
                parent = self._frame
            args = (parent, resolver(), name)
            form = catch('form-init-error', form_class, *args, **kwargs)
            if form is None:
                self.run_dialog(Error, _("Formul�� se nepoda�ilo vytvo�it"))
            else:
                if isinstance(form, PopupForm):
                    log(EVENT, "Zobrazuji mod�ln� formul��:", form)
                    self._modals.push(form)
                    message('', root=True)
                    form.show()
                    self._post_init_form(form, select_row=select_row)
                    try:
                        form_str = str(form) # Dead form doesn't speak...
                        result = form.run()
                        log(EVENT, "Mod�ln� formul�� byl uzav�en:", form_str)
                        log(EVENT, "N�vratov� hodnota:", result)
                    finally:
                        self._modals.pop()
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
                    wx_callback(wx.EVT_CLOSE, form, self._on_form_close)
                    message('', root=True)
                    form.show()
                    self._update_window_menu()
                    if not isinstance(form, PrintForm):
                        item = (self._form_menu_item_title(form),
                                dict(form_class=form_class, name=name))
                        self._update_recent_forms(item)
                    self._post_init_form(form, select_row=select_row)
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def _can_new_record(self, name, **kwargs):
        return has_access(name, perm=pytis.data.Permission.INSERT)
    
    def _cmd_new_record(self, name, prefill=None, inserted_data=None,
                        block_on_new_record=False):
        # Dokumentace viz funkce new_record().
        view = resolver().get(name, 'view_spec')
        on_new_record = view.on_new_record()
        if not block_on_new_record and on_new_record is not None:
            result = on_new_record(prefill=prefill)
            top = self.current_form()
            if isinstance(top, Refreshable):
                top.refresh()
        else:
            result = run_form(PopupEditForm, name, mode=EditForm.MODE_INSERT,
                              prefill=prefill, inserted_data=inserted_data)
        return result

    def _can_run_procedure(self, spec_name, proc_name, args=(),
                           block_refresh_=False, enabled=None, **kwargs):
        return enabled is None and True or enabled(**kwargs)
    
    def _cmd_run_procedure(self, spec_name, proc_name, args=(),
                           block_refresh_=False, enabled=None, **kwargs):
        # Dokumentace viz funkce run_procedure().
        result = None
        try:
            message(_("Spou�t�m proceduru..."), root=True, timeout=2)
            log(ACTION, 'Spou�t�m proceduru:',
                (spec_name, proc_name, args, kwargs))
            # Kv�li wx.SafeYield() se ztr�c� focus, tak�e
            # si ho ulo��me a pak zase obnov�me.
            focused = wx_focused_window()
            wx_yield_()
            spec = resolver().get(spec_name, 'proc_spec')
            assert is_dictionary(spec), \
                   _("Specifikace procedur 'proc_spec' mus� vracet slovn�k!")
            assert spec.has_key(proc_name), \
                  _("Specifikace procedur neobsahuje definici '%s'") % proc_name
            proc = spec[proc_name]
            if block_refresh_:
                result = block_refresh(proc, *args, **kwargs)
            else:
                result = proc(*args, **kwargs)
            log(ACTION, "N�vratov� hodnota procedury:", result)
            if focused:
                focused.SetFocus()
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def _cmd_help(self, topic=None):
        """Zobraz dan� t�ma v prohl�e�i n�pov�dy."""
        if not self._help_files:
            msg = _("��dn� soubor s n�pov�dou nebyl nalezen.\n"
                    "Konfigura�n� volba 'help_dir' nyn� ukazuje na:\n%s\n"
                    "Zkontrolujte zda je cesta spr�vn�\n"
                    "a zda adres�� obsahuje soubory n�pov�dy.")
            run_dialog(Warning, msg % config.help_dir)
            return
        if self._help_controller is None:
            self._help_controller = controller = wx.html.HtmlHelpController()
            controller.SetTitleFormat(_("N�pov�da")+": %s")
            wx.FileSystem_AddHandler(wx.ZipFSHandler())
            for filename, index, title in self._help_files:
                controller.AddBook(filename)
        self._help_controller.Display((topic or self._help_files[0][1])+'.html')

    def _cmd_custom_debug(self):
        if __debug__:
            config.custom_debug()
        
    def _cmd_exit(self):
        self._frame.Close()
        
    def _cmd_nothing(self):
        pass
        
    # Ve�ejn� metody

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
        # Tento yield zaru�� spr�vn� p�ed�n� focusu oken.
        wx_yield_() 
        top = self.top_window()
        if top is not None:
            top.focus()
        else:
            self._panel.SetFocus()
        top = self
        return result
    
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

    def current_form(self, inner=True):
        """Vra� pr�v� aktivn� formul�� aplikace, pokud existuje.
        
        Pokud nen� otev�en ��dn� formul��, nebo aktivn�m oknem nen� formul��,
        vrac� None.  Pokud je aktivn�m formul��em du�ln� formul��, bude vr�cen
        jeho aktivn� podformul��, pr�v� pokud je argument 'inner' pravdiv�.
        
        """
        top = self.top_window()
        if isinstance(top, Form):
            if inner and isinstance(top, DualForm):
                return top.active_form()
            else:
                return top
        else:
            return None
        
        
    def set_status(self, id, message, timeout=None, root=False, log_=True):
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
            
          log_ -- pokud je pravda, bude ud�lost zalogov�na.

        Zobrazen� nen� garantov�no, nemus� se zobrazit nap��klad v�p��pad�, kdy
        stavov� ��dek neobsahuje odpov�daj�c� pole.

        """
        if __debug__:
            if log_:
                log(DEBUG, "Nastaven� pole stavov� ��dky:", data=(id, message))

        modal = self._modals.top()
        if root or not isinstance(modal, Form) \
               or not modal.set_status(id, message):
            return self._statusbar.message(id, message, timeout=timeout)
            
    def get_status(self, id):
        """Vra� text pole 'id' stavov�ho ��dku hlavn�ho okna aplikace.

        Pokud stavov� ��dek dan� pole neobsahuje, vra� None.
        
        """
        return self._statusbar.get_message(id)

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
            form.focus()    
        else:
            self._panel.SetFocus()

    def recent_forms_menu(self):
        """Vra� menu posledn� otev�en�ch formul��� jako instanci 'Menu'."""
        return Menu(self._RECENT_FORMS_MENU_TITLE,
                    self._recent_forms_menu_items())

    def wx_frame(self):
        """Vra� instanc� 'wx.Frame' hlavn�ho okna aplikace."""
        return self._frame


# Funkce odpov�daj�c� p��kaz�m aplikace.

def run_form(form_class, name, **kwargs):
    """Vytvo� formul�� a spus� jej v aplikaci.
    
    Argumenty:
    
      form_class -- t��da vytv��en�ho formul��e (odvozen� od t��dy 'Form').
        
      name -- n�zev specifikace pro resolver� �et�zec.

      kwargs -- kl��ov� arguementy, kter� maj� b�t p�ed�ny konstruktoru
        formul��e.  Argumenty 'parent' a 'resolver' budou dopln�ny automaticky.
        
    Vytvo�en� formul�� bude zobrazen v okn� aplikace, nebo v nov�m mod�ln�m
    okn� (pokud jde o mod�ln� formul�� odvozen� od t��dy 'PopupForm').  Okno
    nemod�ln�ho formul��e z�st�v� po n�vratu t�to funkce v aplikaci otev�eno
    (lze jej odstranit p��kazem 'Form.COMMAND_LEAVE_FORM').  V p��pad�
    mod�ln�ho formul��e se funkce vrac� a� po jeho uzav�en�.

    Vrac�: N�vratovou hodnotu metody 'run()' v p��pad� mod�ln�ho formul��e,
    nebo None v p��pad� nemod�ln�ho formul��e.  Pokud formul�� nelze spustit
    (nap�. nedostate�n� p��stupov� pr�va) , vrac� False.

    """
    cmd = Application.COMMAND_RUN_FORM
    kwargs = dict(form_class=form_class, name=name, **kwargs)
    if not cmd.enabled(**kwargs):
        message(_("Spu�t�n� formul��e zam�tnuto."), beep_=True)
        return False
    return cmd.invoke(**kwargs)

def run_procedure(spec_name, proc_name, *args, **kwargs):
    """Spus� proceduru.
    
    Argumenty:
    
      spec_name -- jm�no specifikace pro resolver.
    
      proc_name -- jm�no procedury, kter� m� b�t spu�t�na.  Jde o kl�� do
        slovn�ku, kter� je vracen specifika�n� funkc� 'proc_spec'.

    V�echny dal�� argumenty (v�etn� kl��ov�ch) budou p�ed�ny spou�t�n�
    procedu�e.  V�jimkou je kl��ov� argument 'block_refresh_', kter� p�ed�n
    nen�, ale pokud je pravdiv�, tak bude vol�n� procedury obaleno vol�n�m
    'block_refresh()'.

    N�vratov� hodnota procedury je n�vratovou hodnotou vol�n� t�to metody.

    """
    assert not kwargs.has_key('args'), \
           "The keyword argument 'args' is reserved for internal use!"
    return Application.COMMAND_RUN_PROCEDURE.invoke(spec_name=spec_name,
                                                    proc_name=proc_name,
                                                    args=args, **kwargs)

def new_record(name, prefill=None, inserted_data=None,
               block_on_new_record=False):
    """Spus� interaktivn� akci p�id�n� nov�ho z�znamu.
        
    Argumenty:
        
      name -- jm�no specifikace pro resolver.
      
      prefill -- slovn�k �et�zcov�ch (u�ivatelsk�ch) hodnot, kter� maj� b�t
        p�edvypln�ny p�i inicializaci formul��e.
            
      inserted_data -- sekvence datov�ch ��dk� (instanc� pytis.data.Row),
        kter�mi m� b�t vstupn� formul�� postupn� pln�n.
            
      block_on_new_record -- Pokud je True, nebude provedena procedura
        on_new_record; v podstat� umo��uje zavol�n� new_record v r�mci
        procedury on_new_record.
            
    """
    return Application.COMMAND_NEW_RECORD.invoke(**locals())

def refresh():
    """Aktualizuj zobrazen� viditeln�ch oken aplikace, pokud je to t�eba."""
    Application.COMMAND_REFRESH.invoke()

def help(topic=None):
    """Zobraz dan� t�ma v prohol�e�i n�pov�dy."""
    return Application.COMMAND_HELP.invoke(topic=topic)

def exit():
    """Ukon�i u�ivatelsk� rozhran� aplikace."""
    return Application.COMMAND_EXIT.invoke()

# Funkce, kter� jsou obrazem ve�ejn�ch metod aktu�ln� aplikace.

def run_dialog(*args, **kwargs):
    """Zobraz dialog v okn� aplikace (viz 'Application.run_dialog()')."""
    if _application is not None:
        return _application.run_dialog(*args, **kwargs)
    else:
        log(OPERATIONAL, "Pokus o spu�t�n� dialogu:", (args, kwargs))

def current_form(inner=True):
    """Vra� pr�v� aktivn� formul�� (viz 'Application.currnt_form()')."""
    return _application.current_form(inner=inner)

def top_window():
    """Vra� aktivn� okno aplikace (formul��, nebo dialog)."""
    return _application.top_window()

def set_status(id, message, log_=True):
    """Nastav pole 'id' stavov� ��dky (viz 'Application.set_status()')."""
    return _application.set_status(id, message, log_=log_)

def get_status(id):
    """Vra� text pole 'id' stavov� ��dky. (viz 'Application.get_status()')"""
    return _application.get_status(id)

def recent_forms_menu():
    """Vra� menu posledn� otev�en�ch formul��� jako instanci 'pytis.form.Menu'.

    Tato funkce je ur�ena pro vyu�it� p�i definici menu aplikace.  Pokud menu
    posledn� otev�en�ch formul��� t�mto zp�sobem do hlavn�ho menu aplikace
    p�id�me, bude jej aplikace d�le obhospoda�ovat.
        
    """
    if _application is not None:
        return _application.recent_forms_menu()
    else:
        return Menu(Application._RECENT_FORMS_MENU_TITLE, ())

def wx_frame():
    """Vra� instanci 'wx.Frame' hlavn�ho okna aplikace."""
    return _application.wx_frame()

# Ostatn� funkce.

def message(message, kind=EVENT, data=None, beep_=False, timeout=None,
            root=False, log_=True):
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
      log_ -- pokud je pravda, bude zpr�va tak� zalogov�na.
        
    Pro zobrazen� zpr�vy ve stavov� ��dce plat� stejn� pravidla, jako v p��pad�
    metody 'Application.set_status()'.

    """
    if beep_:
        beep()
    if log_ and (message or data):
        log(kind, message, data=data)
    if _application:
        if message and message[-1] == ':':
            message = message[:-1]
        _application.set_status('message', message, timeout=timeout,
                                root=root)

def global_keymap():
    """Vra� kl�vesovou mapu aplikace jako instanci t��dy 'Keymap'."""
    try:
        return _application.keymap
    except AttributeError:
        return Keymap()

def block_refresh(function, *args, **kwargs):
    """Zablokuj ve�ker� refresh po dobu prov�d�n� funkce 'function'.
    
    Vrac�: v�sledek vr�cen� volanou funkc�.
    
    """
    return Refreshable.block_refresh(function, *args, **kwargs)

def has_access(name, perm=pytis.data.Permission.VIEW):
    """Vra� pravdu, pokud m� p�ihl�en� u�ivatel pr�va k dan�mu n�hledu.

    Argumenty:
    
      name -- n�zev specifikace jako �et�zec.
      perm -- pr�vo jako jedna z konstant `pytis.data.Permission'.

    """
    try:
        data_spec = resolver().get(name, 'data_spec')
    except ResolverError:
        return True
    rights = data_spec.access_rights()
    if not rights:
        return True
    groups = pytis.data.default_access_groups(config.dbconnection)
    return rights.permitted(perm, groups)

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

