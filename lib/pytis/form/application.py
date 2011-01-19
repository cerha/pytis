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

"""Hlavní aplikační okno.

Tento modul definuje třídu 'Application', která představuje hlavní okno
aplikace a zajišťuje základní služby s ním související.  Modul se týká pouze
uživatelského rozhraní, neřeší obecně start a zastavení aplikace.

"""

import os.path
import string
import sys
import thread
import time
import cPickle as pickle

import config
import pytis.data
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
    """Aplikace systému Pytis.

    Pro každou aplikaci systému Pytis existuje po celou dobu jejího běhu jedno
    hlavní aplikační okno.  To se sestává jednak ze statických prvků a jednak z
    vyměnitelného vnitřku okna (vlastních formulářů).  Statickými prvky jsou
    pull-down menu a stavový řádek.

    Start aplikace a vytvoření statických prvků je možné parametrizovat
    specifikačním souborem aplikace.  Tím je soubor 'application.py' v adresáři
    resolveru (určeném konfigurační volbou 'def_dir').  Použitelné specifikační
    funkce jsou:

      read_config -- vrací odkaz na funkci, která bude spuštěna v průběhu
        inicializace aplikace.  Pokud je definována, je očekáváno, že funkce
        načte konfigurační volby z externího zdroje (např. z databáze) a předá
        je jako svou návratovou hodnotu ve formě sekvence dvojic (NÁZEV,
        HODNOTA), kde NÁZEV je vždy platným názvem konfigurační volby a HODNOTA
        je její uložená hodnota.  Všechny takto načtené volby budou automaticky
        nastaveny v globální proměnné `config' a využívány aplikací.  Pokud
        funkce není definována, pokusí se aplikace volby načíst vlastním
        mechanismem.

      write_config -- vrací odkaz na funkci jednoho argumentu, která bude
        spuštěna v průběhu ukončení aplikace.  Argumentem je sekvence dvojic,
        stejně jako v případě návratové hodnoty funkce 'read_config'.  Od
        funkce je očekáváno, že takto předané volby uloží do externího úložného
        prostoru, aby mohly být obnoveny funkcí 'read_config' při příštím
        spuštění aplikace.  Pokud funkce není definována, pokusí se aplikace
        volby uložit vlastním mechanismem.  Ukládané volby obsahují pouze ty
        položky konfigurace, které byly změněny za běhu aplikace oproti svým
        počátečním hodnotám načteným z příkazové řádky a konfiguračního
        souboru.
      
      menu -- specifikace hlavního menu aplikace ve formátu specifikačního
        argumentu konstruktoru třídy 'pytis.form.screen.MenuBar'.
        
      status_fields -- specifikace polí stavové řádky aplikace ve formátu
        specifikačního argumentu konstruktoru třídy
        'pytis.form.screen.StatusBar'.
        
      keymap -- specifikace přiřazení kláves příkazům jako sekvence trojic
        (KEY, COMMAND, ARGS), kde KEY je definice klávesové zkratky, COMMAND je
        instance třídy 'Command' a ARGS je slovník argumentů, které mají být
        příkazu předány.

      init -- Tato funkce může provádět libovolné, blíže neurčené,
        inicializační akce aplikace.  Je spuštěna až po sestavení hlavního
        aplikačního okna a načtení konfigurace, takže zde můžeme pracovat i s
        uživatelským rozhraním.

      TODO: Následující volbu by bylo vhodnější přesunout do konfigurace.
        
      default_font_encoding -- implicitní kódování fontů jako odpovídající wx
        konstanta.
        
    Start uživatelského rozhraní spočívá ve vytvoření instance této třídy a
    volání její metody 'run()'.
    
    """
    _menubar_forms = {}

    _WINDOW_MENU_TITLE = _("Okn&a")

    _STATE_RECENT_FORMS = 'recent_forms'
    _STATE_STARTUP_FORMS = 'startup_forms'
    _STATE_SAVE_FORMS_ON_EXIT = 'save_forms_on_exit'
    _STATE_FORM_CONFIG = 'form_config'

    def _get_command_handler_instance(cls):
        global _application
        return _application
    _get_command_handler_instance = classmethod(_get_command_handler_instance)

    def OnInit(self):
        init_colors()
        # Create the main application window.
        title = config.application_name
        if __debug__:
            try:
                release_version = wx.RELEASE_NUMBER
            except AttributeError:
                # *Some* wx 2.8 versions have the constant renemed!
                release_version = wx.RELEASE_VERSION
            title += ' (wx %d.%d.%d)' % (wx.MAJOR_VERSION, wx.MINOR_VERSION, release_version)
        frame = self._frame = wx.Frame(None, -1, title, pos=(0,0), size=(800, 600),
                                       style=wx.DEFAULT_FRAME_STYLE)
        wx_callback(wx.EVT_CLOSE, frame, self._on_frame_close)
        # This panel is here just to catch keyboard events (frame doesn't support EVT_KEY_DOWN).
        self._panel = wx.Panel(frame, -1)
        KeyHandler.__init__(self, self._panel)
        wx.ToolTip('').Enable(config.show_tooltips)
        self._logo = None
        logo_file = config.logo
        if logo_file is not None:
            if os.access(logo_file, os.R_OK):
                logo = wx.Image(logo_file, type=wx.BITMAP_TYPE_BMP)
                self._logo = wx.StaticBitmap(frame, -1, logo.ConvertToBitmap())
                self._logo.Show(False)
            else:
                log(OPERATIONAL, "Unable to read logo:", logo_file)
        icons = wx.IconBundle()
        for name in ('pytis', 'pytis-mini'):
            icon_bmp = get_icon(name)
            if icon_bmp:
                icon = wx.EmptyIcon()
                icon.CopyFromBitmap(icon_bmp)
                icons.AddIcon(icon)
        frame.SetIcons(icons)
        self._windows = XStack()
        self._modals = Stack()
        self._statusbar = StatusBar(frame, self._spec('status_fields',()))
        self._help_controller = None
        self._help_files = self._find_help_files()
        self._login_hook = self._spec('login_hook')
        keymap = self.keymap = Keymap()
        custom_keymap = self._spec('keymap', ())
        assert is_sequence(custom_keymap), "Keyboard shortcuts specification returned by " + \
               "'keymap' must be a sequence of (KEY, COMMAND) pairs."
        for key, cmd in command.DEFAULT_KEYMAP + custom_keymap:
            if is_sequence(cmd):
                cmd, args = cmd
            else:
                args = {}
            keymap.define_key(key, cmd, args)
        global _application
        _application = self
        self._initial_config = [(o, copy.copy(getattr(config, o)))
                                for o in configurable_options() + \
                                ('application_state', 'form_state')]
        # Read the stored configuration.
        read_config = self._spec('read_config', self._read_config)
        items = read_config()
        for option, value in items:
            if option != 'dbconnection':
                setattr(config, option, value)
        log(OPERATIONAL, "Konfigurace načtena: %d položek" % len(items))
        # Initialize login and password.
        def test():
            bindings = [pytis.data.DBColumnBinding(id, 'pg_catalog.pg_tables', id) for id in ('tablename',)]
            factory = pytis.data.DataFactory(pytis.data.DBDataDefault, bindings, bindings[0])
            dummy_data = factory.create(connection_data=config.dbconnection)
        db_operation(test)
        # Initialize the storage of form profile configurations.  If the
        # database storage fails (the needed table doesn't exist in the
        # database), form configurations will be stored as part of application
        # state (suboptimal for large scale applications).
        try:
            manager = DBFormProfileManager(config.dbconnection)
        except pytis.data.DBException:
            form_config = self._get_state_param(self._STATE_FORM_CONFIG, {}, dict)
            self._set_state_param(self._STATE_FORM_CONFIG, form_config)
            manager = DictionaryFormProfileManager(form_config)
        self._form_profile_manager = manager
        # Read in access rights.
        init_access_rights(config.dbconnection)
        # Init the recent forms list.
        recent_forms = self._get_state_param(self._STATE_RECENT_FORMS, (), (list, tuple), tuple)
        self._recent_forms = []
        for title, args in recent_forms:
            if not self._is_valid_spec(args['name']) or not issubclass(args['form_class'], Form):
                log(OPERATIONAL, "Ignoring recent form:", args)
                continue
            self._recent_forms.append((title, args))
        self._set_state_param(self._STATE_RECENT_FORMS, tuple(self._recent_forms))
        # Initialize the menubar.
        mb = self._create_menubar()
        if mb is None:
            return False
        # Finish and show the frame.
        default_font_encoding = self._spec('default_font_encoding')
        if default_font_encoding is not None:
            wx.Font.SetDefaultEncoding(default_font_encoding)
        wx_callback(wx.EVT_SIZE, frame, self._on_frame_size)
        self.SetTopWindow(frame)
        frame.Show(True)
        # Initialize the toolbar.
        self._toolbar = toolbar = frame.CreateToolBar(wx.NO_BORDER|wx.TB_DOCKABLE)
        for group in TOOLBAR_COMMANDS:
            if group != TOOLBAR_COMMANDS[0]:
                toolbar.AddSeparator()
            for uicmd in group:
                handler = uicmd.command().handler()
                handler.add_toolbar_ctrl(self._toolbar, uicmd)
        toolbar.Realize()
        # Run application specific initialization.
        self._spec('init')
        if self._windows.empty():
            self._panel.SetFocus()
        # Open the startup forms.
        startup_forms = []
        if config.startup_forms:
            for name in config.startup_forms.split(','):
                if name.find('/') != -1:
                    cls_name, name = name.split('/')
                    try:
                        cls = getattr(pytis.form, cls_name.strip())
                        if not issubclass(cls, Form):
                            raise AttributeError
                    except AttributeError:
                        self.run_dialog(Error, _("Neplatná formulářová třída v 'startup_forms':") +
                                        ' '+ cls_name)
                        continue
                else:
                    cls = name.find('::') == -1 and BrowseForm or BrowseDualForm
                startup_forms.append((cls, name.strip()))
        self._saved_startup_forms = []
        for pair in self._get_state_param(self._STATE_STARTUP_FORMS, (), tuple, tuple):
            if len(pair) == 2:
                cls, name = pair
                if issubclass(cls, Form) and self._is_valid_spec(name):
                    if pair not in startup_forms:
                        startup_forms.insert(0, pair)
                    self._saved_startup_forms.append(list(pair) + [None])
                    continue
            log(OPERATIONAL, "Ignoring saved startup form:", pair)
        def run_startup_forms(update, startup_forms):
            i, total = 0, len(startup_forms)
            msg = _("Otevírám formulář: %s (%d/%d)")
            for cls, name in startup_forms:
                update(int(float(i)/total*100), newmsg=msg % (name, i+1, total))
                try:
                    run_form(cls, name)
                except Exception, e:
                    log(OPERATIONAL, "Unable to init startup form:", (cls, name, e))
                else:
                    # Assign titles to saved startup forms (we need a form instance for this).
                    f = self._windows.active()
                    if f:
                        x = find([f.__class__, f.name(), None], self._saved_startup_forms)
                        if x:
                            x[2] = f.title()
                i += 1
        if len(startup_forms) > 1:
            run_dialog(ProgressDialog, run_startup_forms, args=(startup_forms,),
                       title=_("Automatické otevření uložených formulářů"),
                       message=_("Otevírám formulář")+' '*40) #, can_abort=True)
            # In wx2.8, keyboard navigation doesn't work now.  The following
            # lines raise the previous form and then back the top form, which
            # fixes the problem.  Running a Message dialog instead also helps,
            # but it's probably more obtrusive to the users.  Maybe you can
            # find a better solution!
            self._raise_form(self._windows.mru()[1])
            self._raise_form(self._windows.mru()[1])
        else:
            run_startup_forms(lambda *args, **kwargs: True, startup_forms)
        conn = config.dbconnection
        if conn:
            # Pozor, pokud během inicializace aplikace nedojde k připojení k
            # databázi (není vyvolána žádná databázová operace), nemusí být
            # hodnoty správně.
            title = frame.GetTitle()
            title += " %s@%s" % (conn.user(), conn.database())
            if conn.host():
                title += " "+conn.host()
                if conn.port():
                    title += ":%d" % conn.port()
            frame.SetTitle(title)
        return True

    def _spec(self, name, default=None, **kwargs):
        try:
            result = resolver().get('application', name, **kwargs)
        except ResolverError, e:
            log(OPERATIONAL, str(e))
            result = default
        return result

    def _is_valid_spec(self, name):
        # This is a simple way to test whether the specification still exists.
        try:
            has_access(name)
        except:
            return False
        else:
            return True

    def _public_spec(self, name):
        spec_class = None
        pos = name.rfind('.')
        if pos >= 0:
            module, spec_name = name[:pos], name[pos+1:]
            try:
                spec_class = resolver().get_object(module, spec_name)
            except Exception, e:
                pass
        return (spec_class is None or
                (issubclass(spec_class, Specification) and spec_class.public))

    def _find_help_files(self):
        if not os.path.exists(config.help_dir):
            log(OPERATIONAL, "Neexistující adresář nápovědy:", config.help_dir)
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
            log(OPERATIONAL, "Žádné soubory nápovědy nebyly nalezeny:",
                config.help_dir)
        return result

    def _create_command_menu(self, menus):
        items = []
        for group in FORM_MENU_COMMANDS:
            if items:
                items.append(MSeparator())
            for uicmd in group:
                items.append(mitem(uicmd))
        menus.append(Menu(_("Příkazy"), items))

    def _create_help_menu(self, menus):
        if [m for m in menus if m.title() == _("Nápověda")]:
            log(OPERATIONAL, "Menu nápovědy nalezeno - nevytvářím vlastní.")
            return
        items = [mitem(UICommands.PYTIS_HELP)]
        items.extend([MItem(title, command=Application.COMMAND_HELP(topic=index))
                      for file, index, title in self._help_files if index != 'pytis'])
        items.extend((MSeparator(),
                      mitem(UICommands.HELP),
                      mitem(UICommands.DESCRIBE)))
        menus.append(Menu(_("Nápověda"), items))

    def _dynamic_menu(self, connection_data):
        # Check for menu presence, if not available, return None
        I = pytis.data.Integer()
        S = pytis.data.String()
        try:
            menu_data = pytis.data.dbtable('pytis_view_user_menu',
                                           (('menuid', I,),
                                            ('name', S,), ('title', S,), ('fullname', S,),
                                            ('position', pytis.data.LTree(),),
                                            ('help', S,), ('hotkey', S,),),
                                           connection_data, arguments=())
            menu_rows = menu_data.select_map(identity, sort=(('position', pytis.data.ASCENDENT,),))
        except pytis.data.DBException:
            return None
        if not menu_rows:
            return None
        # Build visible menu items
        menu_template = []
        parents = []
        for row in menu_rows:
            menuid, name, title, action, position, help, hotkey = \
                [row[i].value() for i in (0, 1, 2, 3, 4, 5, 6,)]
            if not parents: # the top pseudonode, should be the first one
                parents.append((position or '', menu_template,))
                current_template = menu_template
            else:
                parent = string.join(position.split('.')[:-1], '.')
                parent_index = len(parents) - 1
                while parent_index >= 0 and parent != parents[parent_index][0]:
                    parent_index -= 1
                if parent_index >= 0:
                    parents = parents[:parent_index+1]
                else:
                    continue
                current_template = parents[-1][1]
                if not title: # separator
                    current_template.append(None)                    
                elif name: # terminal item
                    current_template.append((name, title, action, help, hotkey,))
                else:          # non-terminal item
                    upper_template = parents[-1][1]
                    current_template = [(name, title, help, hotkey,)]
                    upper_template.append(current_template)
                    parents.append((position, current_template,))
        # Done, return the menu structure
        return menu_template
        
    def _build_menu(self, menu_prototype, connection_data):
        menu_template = self._dynamic_menu(connection_data)
        if not menu_template:
            return list(menu_prototype)
        def build(template):
            used_letters = []
            def add_key(title):
                # This actually works only for the top menubar, other
                # accelerators are assigned by wx independently.  But it's OK
                # as it's consistent with pre-DMP era.
                for i in range(len(title)):
                    letter = title[i]
                    if letter in string.ascii_letters and letter not in used_letters:
                        used_letters.append(letter)
                        return title[:i] + '&' + title[i:]
                return title
            if isinstance(template, list):
                heading = template[0]
                items = [build(i) for i in template[1:]]
                if heading is None:
                    result = items
                else:
                    title = add_key(heading[1])
                    result = Menu(title, items)
            elif template is None:
                result = MSeparator()
            else:
                name, title, action, help, hotkey = template
                title = add_key(title)
                command = MItem.parse_action(action)
                if hotkey is None:
                    hotkeys = None
                else:
                    hotkeys = [key.replace('SPC', ' ') for key in hotkey.split(' ')]
                result = MItem(title, command, help=help, hotkey=hotkeys)
            return result
        menus = [build(t) for t in menu_template]
        return menus

    def _create_menubar(self):
        self._recent_forms_menu = None
        menus_prototype = self._spec('menu', ())
        menus = self._build_menu(menus_prototype, config.dbconnection)
        menus.append(Menu(self._WINDOW_MENU_TITLE, (
                    MItem(_("Předchozí okno"), command=Application.COMMAND_RAISE_PREV_FORM,
                          help=_("Přepnout na předchozí okno v pořadí seznamu oken.")),
                    MItem(_("Následující okno"), command=Application.COMMAND_RAISE_NEXT_FORM,
                          help=_("Přepnout na následující okno v pořadí seznamu oken.")),
                    MItem(_("Posledně aktivní okno"), command=Application.COMMAND_RAISE_RECENT_FORM,
                          help=_("Umožňuje cyklicky přepínat mezi dvěma posledně aktivními okny.")),
                    MItem(_("Uzavřít aktuální okno"), command=Form.COMMAND_LEAVE_FORM,
                          help=_("Uzavřít okno aktuálního formuláře.")),
                    MSeparator(),
                    ), allow_autoindex=False))
        self._create_command_menu(menus)
        self._create_help_menu(menus)
        # Determining availability of menu items may invoke database operations...
        success, mb = db_operation(MenuBar, self._frame, menus, self.keymap)
        if not success:
            return None
        self._menubar = mb
        self._window_menu = mb.GetMenu(mb.FindMenu(self._WINDOW_MENU_TITLE))
        assert self._window_menu is not None
        return mb

# Ostatní metody

    def _form_menu_item_title(self, form):
        title = form.title()
        if form.__class__ != BrowseForm:
            title += " (%s)" % form.descr()
        return title

    def _update_window_menu(self):
        def wmitem(i, form):
            return CheckItem(acceskey_prefix(i) + self._form_menu_item_title(form),
                             help=_('Vyzvednout okno formuláře "%s" (%s/%s)') %\
                             (form.title(),form.__class__.__name__,form.name()),
                             command=Application.COMMAND_RAISE_FORM,
                             state=lambda : top_window() is form,
                             args={'form': form})
        menu = self._window_menu
        if menu is not None:
            for i, item in enumerate(menu.GetMenuItems()):
                if i >= 5:
                    menu.Remove(item.GetId())
                    item.Destroy()
            for i, form in enumerate(self._windows.items()):
                menu.AppendItem(wmitem(i, form).create(self._frame, menu))

    def _update_recent_forms(self, item=None):
        if self._recent_forms_menu is not None:
            menu = self._recent_forms_menu.wx_menu()
            if menu is None:
                return
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
        items = [MItem(acceskey_prefix(i) + title,
                       help=_('Otevřít formulář "%s" (%s/%s)') %
                            (title, args['form_class'].__name__, args['name']),
                       command=Application.COMMAND_RUN_FORM, args=args)
                 for i, (title, args) in enumerate(self._recent_forms)]
        items.append(MSeparator())
        items.append(MItem(_("Vyčistit"),
                           help=_("Vymazat menu posledně otevřených formulářů"),
                           command=Application.COMMAND_CLEAR_RECENT_FORMS))
        return items
        
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

    def _get_state_param(self, name, default=None, cls=None, item_cls=None):
        try:
            param = config.application_state[name]
        except KeyError:
            return default
        if cls is not None and not isinstance(param, cls):
            log(OPERATIONAL, "Invalid saved application attribute value:", name)
            return default
        if item_cls is not None:
            for item in param:
                if not isinstance(item, item_cls):
                    log(OPERATIONAL, "Invalid saved application attribute value:", name)
                    return default
        return param

    def _set_state_param(self, name, value):
        config.application_state[name] = value
        
    def _unset_state_param(self, name):
        if config.application_state.has_key(name):
            del config.application_state[name]
            
    def _stored_options(self, wxconfig):
        options = []
        cont, key, index = wxconfig.GetFirstEntry()
        while cont:
            options.append(key)
            cont, key, index = wxconfig.GetNextEntry(index)
        return options

    def _read_config(self):
        name = self._config_name()
        log(OPERATIONAL, "Loading saved configuration:", name)
        wxconfig = wx.Config(name)
        mapping = ((pytis.data.String,  wxconfig.Read),
                   (pytis.data.Integer, wxconfig.ReadInt),
                   (pytis.data.Boolean, wxconfig.ReadBool))
        items = []
        for option in self._stored_options(wxconfig):
            t = config.option(option).type()
            for type, read in mapping:
                if isinstance(t, type):
                    value = read(option)
                    break
            else:
                value = pickle.loads(str(wxconfig.Read(option)))
            items.append((option, value))
        return tuple(items)
            
    def _write_config(self, items):
        name = self._config_name()
        log(OPERATIONAL, "Saving configuration:", name)
        wxconfig = wx.Config(name)
        to_delete = self._stored_options(wxconfig)
        mapping = ((pytis.data.String,  wxconfig.Write),
                   (pytis.data.Integer, wxconfig.WriteInt),
                   (pytis.data.Boolean, wxconfig.WriteBool))
        for option, value in items:
            t = config.option(option).type()
            if option in to_delete:
                to_delete.remove(option)
            for type, write in mapping:
                #TODO: Co None hodnoty???
                if isinstance(t, type):
                    write(option, value)
                    break
            else:
                wxconfig.Write(option, pickle.dumps(value))
        for option in to_delete:
            wxconfig.DeleteEntry(option)
        wxconfig.Flush()
        
    def _cleanup(self):
        # Zde ignorujeme všemožné výjimky, aby i při poměrně značně havarijní
        # situaci bylo možno aplikaci ukončit.
        def safelog(msg, *args):
            try:
                log(ACTION, msg, *args)
            except:
                print msg, args
        safelog('Voláno ukončení aplikace')
        try:
            if not self._modals.empty():
                log(EVENT, "Není možno zavřít aplikaci s modálním oknem:",
                    self._modals.top())
                return False
            forms = [(f.__class__, f.name(), f.title(), True) for f in self._windows.items()
                     if not isinstance(f, (PrintForm, AggregationForm, AggregationDualForm))]
            for cls, name, title in self._saved_startup_forms:
                if title is not None and (cls, name) not in [x[:2] for x in forms]:
                    forms.append((cls, name, title, False))
            if forms:
                items = [(checked, title, cls.descr()) for cls, name, title, checked in forms]
                save_state = self._get_state_param(self._STATE_SAVE_FORMS_ON_EXIT, True)
                exit, result = self.run_dialog(ExitDialog, save_columns=(_("Název"), _("Typ")),
                                               save_items=items, save_state=save_state)
                if not exit:
                    return False
                self._set_state_param(self._STATE_SAVE_FORMS_ON_EXIT, result is not None)
                if result:
                    startup_forms = [f[:2] for f, checked in zip(forms, result) if checked]
                    self._set_state_param(self._STATE_STARTUP_FORMS, tuple(startup_forms))
                else:
                    self._unset_state_param(self._STATE_STARTUP_FORMS)
        except Exception, e:
            safelog(str(e))
        try:
            for form in self._windows.items():
                try:
                    self._raise_form(form)
                    if not form.close():
                        return False
                except Exception, e:
                    safelog(str(e))
        except Exception, e:
            safelog(str(e))
        try:
            items = []
            for option, initial_value in self._initial_config:
                current_value = getattr(config, option)
                if current_value != initial_value:
                    items.append((option, current_value))
            write_config = self._spec('write_config', self._write_config)
            write_config(tuple(items))
            log(OPERATIONAL, "Konfigurace uložena: %d položek" % len(items))
        except Exception, e:
            safelog("Saving changed configuration failed:", str(e))
        try:
            if self._help_controller is not None:
                self._help_controller.GetFrame().Close()
        except Exception, e:
            safelog(str(e))
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
        log(EVENT, "Okno nemodálního formuláře uzavřeno:", form)
        self._windows.remove(form)
        self._update_window_menu()
        self.restore()

    def on_key_down(self, event, dont_skip=False):
        # Toto je záchranný odchytávač.  Věřte tomu nebo ne, ale pokud tady ta
        # metoda není, wxWindows se při více příležitostech po stisku klávesy
        # zhroutí.
        return KeyHandler.on_key_down(self, event)

    # Zpracování příkazů

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

    def _can_run_form(self, form_class, name, binding=None, **kwargs):
        if isinstance(self.current_form(), PopupForm) and not issubclass(form_class, PopupForm):
            return False
        if not self._public_spec(name):
            return False
        try:
            if has_access(name):
                if binding is not None or issubclass(form_class, MultiBrowseDualForm):
                    spec = resolver().get(name, 'view_spec')
                    if binding is None:
                        for b in spec.bindings():
                            if has_access(b.name()):
                                return True
                        return False
                    else:
                        b = find(binding, spec.bindings(), key=lambda b: b.id())
                        assert b is not None, "Unknown binding for %s: %s" % (name, binding)
                        return has_access(b.name())
                return True
            else:
                return False
        except ResolverError:
            # The spec is invalid, but we want the crash on attempt to run it.
            return True

    def _cmd_run_form(self, form_class, name, **kwargs):
        # Dokumentace viz funkce run_form().
        result = None
        try:
            if callable(name):
                name = name()
                if name is None:
                    return None
            log(ACTION, 'Vytvářím nový formulář:', (form_class, name, kwargs))
            message(_("Spouštím formulář..."), root=True)
            assert issubclass(form_class, Form)
            assert is_anystring(name)
            # We indicate busy state here so that the action is not delayed by
            # some time consuming _on_idle methods.
            busy_cursor(True)
            wx_yield_()
            result = None
            self.save()
            form = find((form_class, name), self._windows.items(),
                        key=lambda f: (f.__class__, f.name()))
            if form is not None:
                busy_cursor(False)
                self._raise_form(form)
                message(_('Formulář "%s" nalezen na zásobníku oken.') % form.title())
                if kwargs.has_key('select_row'):
                    form.select_row(kwargs['select_row'])
                if kwargs.has_key('filter'):
                    form.filter(kwargs['filter'])
                if kwargs.has_key('binding'):
                    form.select_binding(kwargs['binding'])
                return result
            if issubclass(form_class, PopupForm):
                parent = self._modals.top() or self._frame
                kwargs['guardian'] = self._modals.top() or self
            else:
                #assert self._modals.empty()
                kwargs['guardian'] = self
                parent = self._frame
            args = (parent, resolver(), name)
            try:
                form = form_class(*args, **kwargs)
            except Form.InitError:
                form = None
            if form is None:
                busy_cursor(False)
                self.run_dialog(Error, _("Formulář se nepodařilo vytvořit: %s") % name)
            else:
                if isinstance(form, PopupForm):
                    log(EVENT, "Zobrazuji modální formulář:", form)
                    self._modals.push(form)
                    message('', root=True)
                    form.show()
                    busy_cursor(False)
                    try:
                        form_str = str(form) # Dead form doesn't speak...
                        result = form.run()
                        log(EVENT, "Modální formulář byl uzavřen:", form_str)
                        log(EVENT, "Návratová hodnota:", result)
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
                    log(EVENT, "Zobrazuji nemodální formulář:", form)
                    old = self._windows.active()
                    if old is not None:
                        old.hide()
                    self._windows.push(form)
                    wx_callback(wx.EVT_CLOSE, form, self._on_form_close)
                    message('', root=True)
                    form.resize() # Needed in wx 2.8.x.
                    form.show()
                    self._update_window_menu()
                    if not isinstance(form, PrintForm):
                        item = (self._form_menu_item_title(form),
                                dict(form_class=form_class, name=name))
                        self._update_recent_forms(item)
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def _can_new_record(self, name, **kwargs):
        try:
            return has_access(name, perm=pytis.data.Permission.INSERT)
        except ResolverError:
            # The spec is invalid, but we want the crash on attempt to run it.
            return True
    
    def _cmd_new_record(self, name, prefill=None, inserted_data=None, multi_insert=True,
                        block_on_new_record=False, transaction=None, spec_kwargs={}):
        # Dokumentace viz funkce new_record().
        view = resolver().get(name, 'view_spec', **spec_kwargs)
        on_new_record = view.on_new_record()
        if not block_on_new_record and on_new_record is not None:
            kwargs = dict(prefill=prefill)
            if 'transaction' in argument_names(on_new_record):
                kwargs['transaction'] = transaction
            result = on_new_record(**kwargs)
            top = self.current_form()
            if isinstance(top, Refreshable):
                top.refresh()
        else:
            result = run_form(PopupInsertForm, name, prefill=prefill, inserted_data=inserted_data,
                              multi_insert=multi_insert, transaction=transaction,
                              spec_kwargs=spec_kwargs)
        return result

    def _can_run_procedure(self, spec_name, proc_name, args=(),
                           block_refresh_=False, enabled=None, **kwargs):
        if not self._public_spec(spec_name):
            return False
        return enabled is None and True or enabled(**kwargs)
    
    def _cmd_run_procedure(self, spec_name, proc_name, args=(),
                           block_refresh_=False, enabled=None, **kwargs):
        # Dokumentace viz funkce run_procedure().
        result = None
        try:
            message(_("Spouštím proceduru..."), root=True, timeout=2)
            log(ACTION, 'Spouštím proceduru:',
                (spec_name, proc_name, args, kwargs))
            # Kvůli wx.SafeYield() se ztrácí focus, takže
            # si ho uložíme a pak zase obnovíme.
            focused = wx_focused_window()
            wx_yield_()
            spec = resolver().get(spec_name, 'proc_spec')
            assert is_dictionary(spec), \
                   _("Specifikace procedur 'proc_spec' musí vracet slovník!")
            assert spec.has_key(proc_name), \
                  _("Specifikace procedur neobsahuje definici '%s'") % proc_name
            proc = spec[proc_name]
            if block_refresh_:
                result = block_refresh(proc, *args, **kwargs)
            else:
                result = proc(*args, **kwargs)
            log(ACTION, "Návratová hodnota procedury:", result)
            if focused:
                focused.SetFocus()
        except UserBreakException:
            pass
        except:
            top_level_exception()
        return result

    def _cmd_help(self, topic=None):
        """Zobraz dané téma v prohlížeči nápovědy."""
        if not self._help_files:
            msg = _("Žádný soubor s nápovědou nebyl nalezen.\n"
                    "Konfigurační volba 'help_dir' nyní ukazuje na:\n%s\n"
                    "Zkontrolujte zda je cesta správná\n"
                    "a zda adresář obsahuje soubory nápovědy.")
            self.run_dialog(Warning, msg % config.help_dir)
            return
        if self._help_controller is None:
            self._help_controller = controller = wx.html.HtmlHelpController()
            controller.SetTitleFormat(_("Nápověda")+": %s")
            wx.FileSystem_AddHandler(wx.ZipFSHandler())
            for filename, index, title in self._help_files:
                controller.AddBook(filename)
        self._help_controller.Display((topic or self._help_files[0][1])+'.html')

    def _can_help(self, topic=None):
        if topic == 'pytis':
            return 'pytis' in [index for filename, index, title in self._help_files]
        return True

    def _cmd_reload_rights(self):
        init_access_rights(config.dbconnection)
        self._create_menubar()
        self._update_window_menu()
        
    def _cmd_custom_debug(self):
        if __debug__:
            config.custom_debug()

    def _cmd_inspect(self):
        import wx.lib.inspection
        tool = wx.lib.inspection.InspectionTool()
        tool.Init(app=self)
        tool.Show()
        
    def _cmd_exit(self):
        self._frame.Close()
        
    def _can_nothing(self, enabled=True):
        return enabled
    
    def _cmd_nothing(self, enabled=True):
        pass
        
    # Veřejné metody

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
        # Tento yield zaručí správné předání focusu oken.
        wx_yield_() 
        top = self.top_window()
        if top is not None:
            top.focus()
        else:
            self._panel.SetFocus()
        top = self
        return result
    
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

    def current_form(self, inner=True):
        """Vrať právě aktivní formulář aplikace, pokud existuje.
        
        Pokud není otevřen žádný formulář, nebo aktivním oknem není formulář,
        vrací None.  Pokud je aktivním formulářem duální formulář, bude vrácen
        jeho aktivní podformulář, právě pokud je argument 'inner' pravdivý.
        
        """
        form = self.top_window()
        if not isinstance(form, Form):
            return None
        if inner:
            while isinstance(form, (DualForm, MultiForm)):
                form = form.active_form()
        return form
        
    def set_status(self, id, message, timeout=None, root=False, log_=True):
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
            
          log_ -- pokud je pravda, bude událost zalogována.

        Zobrazení není garantováno, nemusí se zobrazit například v případě, kdy
        stavový řádek neobsahuje odpovídající pole.

        """
        if __debug__:
            if log_:
                log(DEBUG, "Nastavení pole stavové řádky:", data=(id, message))

        modal = self._modals.top()
        if root or not isinstance(modal, Form) \
               or not modal.set_status(id, message):
            return self._statusbar.message(id, message, timeout=timeout)
            
    def get_status(self, id):
        """Vrať text pole 'id' stavového řádku hlavního okna aplikace.

        Pokud stavový řádek dané pole neobsahuje, vrať None.
        
        """
        return self._statusbar.get_message(id)

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
            form.focus()    
        else:
            self._panel.SetFocus()

    def recent_forms_menu(self):
        """Vrať menu posledně otevřených formulářů jako instanci 'Menu'."""
        menu = Menu(_("Posledně otevřené formuláře"),
                    self._recent_forms_menu_items(), allow_autoindex=False)
        self._recent_forms_menu = menu
        return menu

    def wx_frame(self):
        """Vrať instancí 'wx.Frame' hlavního okna aplikace."""
        return self._frame

    def login_hook(self, success):
        if self._login_hook:
            self._login_hook(success)
            if success:
                self._login_hook = None

    def form_profile_manager(self):
        return self._form_profile_manager


class FormProfileManager(object):
    """Generic accessor of form configuration storage.

    This class just defines the generic interface of all form configuration
    managers.  Different implementations may store the configurations
    differently.

    The actual form profile data are python dictionaries of arbitrary form
    settings at this level.  There are no rules, except that the data structure
    must be safe to pickle and unpickle (ideally they should consist just of
    basic python data types).  They are referenced by a string identifier and
    the upper layer is responsible for converting these structures into
    'pytis.presentation.Profile' instances.

    TODO: The conversion to 'pytis.presentation.Profile' instances might be
    done directly by this class, but the routines must be factored out of Form
    classes where they are now still used for other purposes.  Forms also use
    special profiles to store other kinds of settings (see
    '__global_settings__').

    """
    def save_profile(self, form, profile, config):
        """Save user specific configuration of a form.

        Arguments:
          form -- 'Form' instance to which the given configurtation belongs.
          profile -- string name of the form profile to which the given
            configurtation belongs.
          config -- dictionary of form configuration parameters.

        """
        pass

    def load_profile(self, form, profile):
        """Return previously saved user specific configuration of a form.

        Arguments:
          form -- 'Form' instance to which the given configurtation belongs.
          profile -- string name of the form profile to which the given
            configurtation belongs.

        Returns a dictionary of form configuration parameters.  If no
        configuration was previously saved (using 'save_profile()') or if a
        problem occures reading it, an empty dictionary is returned.

        """
        pass

    def drop_profile(self, form, profile):
        """Remove the previously saved form configuration.

        Arguments:
          form -- 'Form' instance to which the given configurtation belongs.
          profile -- string name of the form profile to which the
            configurtation belongs.

        """
        pass

    def list_profiles(self, form):
        """Return a sequence of all previously saved form configurations.

        Arguments:
          form -- 'Form' instance to which configurtations belong.

        Returns a sequence of all distinct strings previously passed to
        'save_profile' as the 'profile' argument for given form.

        """
        pass


class DictionaryFormProfileManager(FormProfileManager):
    """Accessor of a simple dictionary storage of form configurations.

    Form configurations are stored in a simple dictionary.  This is not optimal
    for large scale applications with lots of forms, since the dictionary may
    grow to huge sizes, so its lookup, storage and retrieval may take
    unreasonable time.

    The dictionary is passed to the constructor and the calling side is
    responsible for securing its persistence!

    """
    def __init__(self, config):
        assert isinstance(config, dict)
        self._config = config

    def _key(self, form, profile):
        return ':'.join((form.__class__.__name__, form.name(), profile))

    def save_profile(self, form, profile, config):
        profiles = self.list_profiles(form)
        if profile != '__saved_profiles__' and profile not in profiles:
            profiles += (profile,)
            self.save_profile(form, '__saved_profiles__', profiles)
        self._config[self._key(form, profile)] = config
            
    def load_profile(self, form, profile):
        return self._config.get(self._key(form, profile), {})

    def drop_profile(self, form, profile):
        try:
            del self._config[self._key(form, profile)]
        except KeyError:
            pass
        
    def list_profiles(self, form):
        return self.load_profile(form, '__saved_profiles__') or ()
        
    
class DBFormProfileManager(FormProfileManager):
    """Accessor of the database storage of form configurations.

    This manager will store form configurations in a database table, one row per form profile.
    This is more optimal for large scale applications

    The constructor will raise 'pytis.data.DBException' if the needed database table is not found.
    This means that some other manager has to be used.
        
    """
    _TABLE = 'e_pytis_form_config'
    _COLUMNS = ('id', 'username', 'form', 'profile', 'config',)

    def __init__(self, dbconnection):
        self._data = pytis.data.dbtable(self._TABLE, self._COLUMNS, dbconnection)

    def _fullname(self, form):
        cls, name = form.__class__, form.name()
        return 'form/%s.%s/%s//' % (cls.__module__, cls.__name__, name)

    def _key_values(self, form, profile=None):
        values = (('username', config.dbuser),
                  ('form', self._fullname(form)))
        if profile:
            values += (('profile', profile),)
        return [(key, pytis.data.Value(pytis.data.String(), value))
                for key, value in values]

    def _row_condition(self, form, profile=None):
        return pytis.data.AND(*[pytis.data.EQ(key, value) for key, value in
                                self._key_values(form, profile)])

    def _row(self, form, profile):
        count = self._data.select(condition=self._row_condition(form, profile))
        assert count in (0, 1)
        return self._data.fetchone()

    def save_profile(self, form, profile, config):
        row = self._row(form, profile)
        value = pytis.data.Value(pytis.data.String(), pickle.dumps(config))
        if row:
            row['config'] = value
            self._data.update(row['id'], row)
        else:
            values = self._key_values(form, profile)
            row = pytis.data.Row(values + [('config', value)])
            self._data.insert(row)

    def load_profile(self, form, profile):
        row = self._row(form, profile)
        if row:
            result = pickle.loads(str(row['config'].value()))
            if not isinstance(result, dict):
                result = {}
                self._data.delete(row['id'])
            return result
        else:
            return {}
           
    def drop_profile(self, form, profile):
        row = self._row(form, profile)
        if row:
            self._data.delete(row['id'])
        
    def list_profiles(self, form):
        count = self._data.select(condition=self._row_condition(form))
        profiles = []
        while True:
            row = self._data.fetchone()
            if row is None:
                break
            profiles.append(row['profile'].value())
        return tuple(profiles)



# Funkce odpovídající příkazům aplikace.

def run_form(form_class, name, **kwargs):
    """Vytvoř formulář a spusť jej v aplikaci.
    
    Argumenty:
    
      form_class -- třída vytvářeného formuláře (odvozená od třídy 'Form').
        
      name -- název specifikace pro resolverů řetězec.

      kwargs -- klíčové arguementy, které mají být předány konstruktoru
        formuláře.  Argumenty 'parent' a 'resolver' budou doplněny automaticky.
        
    Vytvořený formulář bude zobrazen v okně aplikace, nebo v novém modálním
    okně (pokud jde o modální formulář odvozený od třídy 'PopupForm').  Okno
    nemodálního formuláře zůstává po návratu této funkce v aplikaci otevřeno
    (lze jej odstranit příkazem 'Form.COMMAND_LEAVE_FORM').  V případě
    modálního formuláře se funkce vrací až po jeho uzavření.

    Vrací: Návratovou hodnotu metody 'run()' v případě modálního formuláře,
    nebo None v případě nemodálního formuláře.  Pokud formulář nelze spustit
    (např. nedostatečná přístupová práva) , vrací False.

    """
    cmd = Application.COMMAND_RUN_FORM
    kwargs = dict(form_class=form_class, name=name, **kwargs)
    if not cmd.enabled(**kwargs):
        message(_("Spuštění formuláře zamítnuto."), beep_=True)
        return False
    return cmd.invoke(**kwargs)

def run_procedure(spec_name, proc_name, *args, **kwargs):
    """Spusť proceduru.
    
    Argumenty:
    
      spec_name -- jméno specifikace pro resolver.
    
      proc_name -- jméno procedury, která má být spuštěna.  Jde o klíč do
        slovníku, který je vracen specifikační funkcí 'proc_spec'.

    Všechny další argumenty (včetně klíčových) budou předány spouštěné
    proceduře.  Výjimkou je klíčový argument 'block_refresh_', který předán
    není, ale pokud je pravdivý, tak bude volání procedury obaleno voláním
    'block_refresh()'.

    Návratová hodnota procedury je návratovou hodnotou volání této metody.

    """
    assert not kwargs.has_key('args'), \
           "The keyword argument 'args' is reserved for internal use!"
    return Application.COMMAND_RUN_PROCEDURE.invoke(spec_name=spec_name,
                                                    proc_name=proc_name,
                                                    args=args, **kwargs)

def new_record(name, prefill=None, inserted_data=None, multi_insert=True,
               block_on_new_record=False, transaction=None, spec_kwargs={}):
    """Start an interactive form for new record insertion.
        
    Arguments:
        
      name -- specification name for resolver.
      
      prefill -- A dictionary of values to be prefilled in the form.  Keys are field identifiers
        and values are either 'pytis.data.Value' instances or the corresponding Python internal
        values directly.
            
      inserted_data -- allows to pass a sequence of 'pytis.data.Row' instances to be inserted.  The
        form is then gradually prefilled by values of these rows and the user can individually
        accept or skip each row.

      multi_insert -- boolean flag indicating whether inserting multiple values is permitted.
        False value will disable this feature and the `Next' button will not be present on the
        form.
            
      block_on_new_record -- If true, the 'on_new_record' procedure will be blocked.  This makes it
        possible to call 'new_record' from within the 'on_new_record' procedure without recursion..

      transaction -- transaction for data operations.

      spec_kwargs -- a dictionary of keyword arguments passed to the specification.
            
    """
    return Application.COMMAND_NEW_RECORD.invoke(**locals())

def delete_record(view, data, transaction, record,
                  question=_("Opravdu chcete záznam zcela vymazat?")):
    # This is here only to prevent duplication of code in form.py and inputfield.py.
    # It Shound not be used as a public API method.
    ask = True
    key = record.row().columns([c.id() for c in data.key()])
    # Ošetření uživatelské funkce pro mazání
    on_delete_record = view.on_delete_record()
    if on_delete_record is not None:
        result = on_delete_record(record)
        if result is True:
            op, arg = data.delete, key
        elif result is False or result is None:
            return False
        elif result == 1:
            return True
        elif isinstance(result, (str, unicode)):
            run_dialog(Error, result)
            return False
        elif isinstance(result, pytis.data.Operator):
            ask = False
            op, arg = data.delete_many, result
        else:
            raise ProgramError("Invalid 'on_delete_record' return value.", result)
    else:
        op, arg = data.delete, key
    if ask and not run_dialog(Question, question):
        return False
    log(EVENT, 'Deleting record:', arg)
    success, result = db_operation(op, arg, transaction=transaction)
    if success:
        log(ACTION, 'Record deleted.')
        return True
    else:
        return False

def refresh():
    """Aktualizuj zobrazení viditelných oken aplikace, pokud je to třeba."""
    Application.COMMAND_REFRESH.invoke()

def help(topic=None):
    """Zobraz dané téma v proholížeči nápovědy."""
    return Application.COMMAND_HELP.invoke(topic=topic)

def exit():
    """Ukonči uživatelské rozhraní aplikace."""
    return Application.COMMAND_EXIT.invoke()

def db_operation(operation, *args, **kwargs):
    in_transaction = (kwargs.get('transaction') is not None)
    return db_op(operation, args, kwargs, in_transaction=in_transaction)

def db_op(operation, args=(), kwargs={}, in_transaction=False, quiet=False):
    """Invoke database operation with handling possible exceptions.

    The 'operation' is called with given arguments.  If a 'pytis.data.dbdata.DBException' exception
    is raised during the operation, the an error dialog is displayed with exception description and
    a question, whether the user wishes to re-invoke the operation.  The operation is repeated as
    long as user answers the question positively.

    The exceptions of type 'DBLoginException' result in displaying a login dialog and the supplied
    username and password is set before repeating the operation.

    When the operation is performed successfully (regardles whether on the first try or later), its
    result is returned.

    Arguments:

      operation -- function (callable object) performing a database operation and returning its
        result
      args, kwargs -- arguments and keyword arguments passed to the function
      in_transaction -- iff true, don't offer the user a chance for restoring the operation
      quiet -- iff true, don't report errors to the user

    Returns: Pair (SUCCESS, RESULT), where SUCCESS is a boolean flag indicating success (true) or
    failure (false) and RESULT is the value returned by 'operation' (if SUCCESS is false, RESULT is
    not defined).

    """
    FAILURE = False, None
    while True:
        try:
            result = operation(*args, **kwargs)
            if _application:
                _application.login_hook(success=True)
            return True, result
        except pytis.data.DataAccessException, e:
            run_dialog(Error, _("Přístup odmítnut"))
            return FAILURE
        except pytis.data.DBLoginException, e:
            import config
            if config.dbconnection.password() is not None and _application:
                _application.login_hook(success=False)
            login_and_password = run_dialog(Login, _("Zadejte heslo pro přístup do databáze"),
                                            login=config.dbuser)
	    if not login_and_password:
                return FAILURE
	    login, password = login_and_password
            if password is None:
                return FAILURE
            config.dbconnection.update_login_data(user=login, password=password)
        except pytis.data.DBException, e:
            log(OPERATIONAL, "Database exception in db_operation", format_traceback())
            message = e.message()
            if e.exception():
                message += '\n' + str(e.exception())
            if quiet:
                return FAILURE
            if in_transaction:
                run_dialog(Message, message, title=_("Databázová chyba"),
                           icon=Message.ICON_ERROR)
                return FAILURE
            else:
                message += '\n' + _("Zkusit znovu?")
                if not run_dialog(Question, message, title=_("Databázová chyba"),
                                  icon=Question.ICON_ERROR):
                    return FAILURE

def delete_record_question(msg=None):
    """Zeptej se uživatele, zda má být opravdu smazán záznam.

    Vrať pravdu, právě když uživatel odpoví kladně.
    
    """
    log(EVENT, 'Dialog mazání řádku')
    if msg == None:
        msg = _("Opravdu chcete záznam zcela vymazat?")        
    if not run_dialog(Question, msg):
        log(EVENT, 'Mazání řádku uživatelem zamítnuto')
        return False
    log(EVENT, 'Mazání řádku uživatelem potvrzeno')
    return True

# Funkce, které jsou obrazem veřejných metod aktuální aplikace.

def run_dialog(*args, **kwargs):
    """Zobraz dialog v okně aplikace (viz 'Application.run_dialog()')."""
    if _application is not None:
        return _application.run_dialog(*args, **kwargs)
    else:
        log(OPERATIONAL, "Attempt to run a dialog:", (args, kwargs))

def current_form(inner=True):
    """Vrať právě aktivní formulář (viz 'Application.currnt_form()')."""
    if _application is not None:
        return _application.current_form(inner=inner)

def top_window():
    """Vrať aktivní okno aplikace (formulář, nebo dialog)."""
    if _application is not None:
        return _application.top_window()

def set_status(id, message, log_=True):
    """Nastav pole 'id' stavové řádky (viz 'Application.set_status()')."""
    if _application is not None:
        return _application.set_status(id, message, log_=log_)
    else:
        log(OPERATIONAL, "Attempt to set status-line:", (id, message))

def get_status(id):
    """Vrať text pole 'id' stavové řádky. (viz 'Application.get_status()')"""
    return _application.get_status(id)

def recent_forms_menu():
    """Vrať menu posledně otevřených formulářů jako instanci 'pytis.form.Menu'.

    Tato funkce je určena pro využití při definici menu aplikace.  Pokud menu posledně otevřených
    formulářů tímto způsobem do hlavního menu aplikace přidáme, bude jej aplikace dále
    obhospodařovat.  Toto menu lze do hlavního menu umístit pouze jednou.
        
    """
    if _application:
        return _application.recent_forms_menu()
    else:
        # This may happen when generating help.
        return ()

def wx_frame():
    """Vrať instanci 'wx.Frame' hlavního okna aplikace."""
    return _application.wx_frame()

def form_profile_manager():
    """Return 'Application.form_profile_manager()' of the current application instance."""
    return _application.form_profile_manager()

# Ostatní funkce.

def message(message, kind=EVENT, data=None, beep_=False, timeout=None,
            root=False, log_=True):
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
      log_ -- pokud je pravda, bude zpráva také zalogována.
        
    Pro zobrazení zprávy ve stavové řádce platí stejná pravidla, jako v případě
    metody 'Application.set_status()'.

    """
    if beep_:
        beep()
    if log_ and (message or data):
        log(kind, message, data=data)
    if _application:
        if message and message[-1] == ':':
            message = message[:-1]
        _application.set_status('message', message, timeout=timeout, root=root)

def create_data_object(name, spec_kwargs={}, kwargs={}):
    """Create a data object for given specification.

    Arguments:

      name -- specification name for resolver as a string.
      spec_kwargs -- a dictionary of keyword arguments passed to the
        specification.
      kwargs -- a dictionary of keyword arguments passed to the data object
        constructor.  The argument 'connection_data' is added automatically
        if the data class is derived from 'pytis.data.DBData'.

    Raises 'ResolverError' or 'ProgramError' if data object creation fails.
    
    """
    factory = resolver().get(name, 'data_spec', **spec_kwargs)
    import config
    if __debug__ and config.server:
        import pytis.remote
        assert isinstance(factory, (pytis.data.DataFactory, pytis.remote.RemoteDataFactory))
    else:    
        import pytis.data    
        assert isinstance(factory, pytis.data.DataFactory)
    if issubclass(factory.class_(), pytis.data.DBData):
        kwargs = dict(kwargs, connection_data=config.dbconnection)
    t = time.time()
    success, data_object = db_operation(factory.create, **kwargs)
    if not success:
        raise ProgramError("Unable to create data object:", name)
    log(EVENT, 'Data object created in %.3fs:' % (time.time() - t), data_object)
    return data_object
        
def global_keymap():
    """Vrať klávesovou mapu aplikace jako instanci třídy 'Keymap'."""
    try:
        return _application.keymap
    except AttributeError:
        return Keymap()

def block_refresh(function, *args, **kwargs):
    """Zablokuj veškerý refresh po dobu provádění funkce 'function'.
    
    Vrací: výsledek vrácený volanou funkcí.
    
    """
    return Refreshable.block_refresh(function, *args, **kwargs)

_access_rights = None
_access_dbconnection = None
_user_roles = ()

def init_access_rights(connection_data):
    """Read application access rights from the database.

    This function must be called very early after start of an application.

    Arguments:

      connection_data -- 'pytis.data.DBConnection' instance
    
    """
    global _access_rights, _user_roles, _access_dbconnection
    _access_dbconnection = connection_data
    import config
    try:
        roles_data = pytis.data.dbtable('ev_pytis_user_roles', ('roleid',), connection_data)
        roles = [row[0].value() for row in roles_data.select_map(identity)]
    except pytis.data.DBException:
        return
    if not roles:
        _access_rights = 'nonuser'
        return
    _user_roles = roles
    S = pytis.data.String()
    rights_data = pytis.data.dbtable('pytis_view_user_rights',
                                     (('shortname', S,), ('rights', S,), ('columns', S,),),
                                     connection_data, arguments=())
    _access_rights = {}
    def process(row):
        shortname, rights_string, columns_string = row[0].value(), row[1].value(), row[2].value()
        if columns_string:
            columns = string.split(columns_string, ' ')
        else:
            columns = [None]
        rights = [r.upper() for r in rights_string.split(' ') if r != 'show']
        action_rights = _access_rights[shortname] = _access_rights.get(shortname, {})
        for r in rights:
            action_rights[r] = action_rights.get(r, []) + columns
    rights_data.select_map(process)
    Specification._init_access_rights(connection_data)
    
def has_access(name, perm=pytis.data.Permission.VIEW, column=True):
    """Return true if the current user has given permission for given spec.

    Arguments:
    
      name -- specification name as a string.  May also be a dual name
        (containing `::').  In such a case, the permission is checked for both
        names and 'column=True' is assumed regardless of the actual 'column'
        value.
      perm -- access permission as one of `pytis.data.Permission' constants.    
      column -- string identifier of the column to check or 'None' (no specific
        column checked) or 'True' (any of the columns is accessible)

    Raises 'ResolverError' if given specification name cannot be found.

    """
    if not action_has_access('form/'+name, perm=perm, column=column):
        return False
    try:
        main, side = name.split('::')
    except ValueError:
        dual = False
    else:
        dual = True
    if dual:
        return has_access(main, perm=perm) and has_access(side, perm=perm)
    else:
        rights = resolver().get(name, 'data_spec').access_rights()
        if rights:
            groups = pytis.data.default_access_groups(_access_dbconnection)
            if not rights.permitted(perm, groups, column=column):
                return False
    result = action_has_access('form/'+name, perm=perm, column=column)
    return result

def action_has_access(action, perm=pytis.data.Permission.CALL, column=True):
    """Return true iff 'action' has 'perm' permission.

    Arguments:

      action -- action identifier, string
      perm -- access permission as one of `pytis.data.Permission' constants
      column -- string identifier of the column to check or 'None' (no specific
        column checked) or 'True' (any of the columns is accessible)

    """
    if _access_rights == 'nonuser':
        return False
    if _access_rights is None:
        result = True
    else:
        rights = _access_rights.get(action)
        if rights is None:
            # No action rights defined => only system rights apply
            # (this function is *action* rights check).
            result = True
            access_rights = pytis.presentation.Specification.data_access_rights(action)
            if access_rights is not None:
                result = access_rights.permitted(perm, _user_roles, column=column)
        else:
            columns = rights.get(perm)
            if not columns:
                result = False
            elif column is True:
                result = True
            elif column not in columns and None not in columns:
                result = False
            else:
                result = perm in rights
    return result

_yield_lock = None
def wx_yield_(full=False):
    """Zpracuj wx messages ve frontě.

    Argumenty:

      full -- právě když je pravdivé, zpracuj i uživatelské události

    """
    if _yield_blocked:
        return
    global _yield_lock
    if _yield_lock is None:
        _yield_lock = thread.allocate_lock()
    if not _yield_lock.acquire(0):
        return
    try:
        if full:
            if _application is not None:
                _application.Yield()
        else:
            wx.SafeYield()
    finally:
        _yield_lock.release()

_yield_blocked = False
def block_yield(block=False):
    """Block or unblock processing of application events.

    Arguments:

      block -- if true, block processing of application events, otherwise unblock it
      
    """
    global _yield_blocked
    _yield_blocked = block
