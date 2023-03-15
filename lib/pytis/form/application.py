# -*- coding: utf-8 -*-

# Copyright (C) 2018-2023 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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
from __future__ import print_function
from past.builtins import basestring
from builtins import range

import copy
import decimal
import functools
import gi
import io
import lcg
import os.path
import string
import sys
import _thread
import threading
import tempfile
import time
import wx
import wx.html

import pytis.api
import pytis.data as pd
import pytis.form
from pytis.presentation import (
    Field, Specification, StatusField, computer, Text, TextFormat, PresentedRow,
    Menu, MenuItem, MenuSeparator,
)
import pytis.util
import pytis.remote
from pytis.api import app
from pytis.util import (
    ACTION, DEBUG, EVENT, OPERATIONAL, ProgramError, ResolverError, Stack, XStack,
    argument_names, find, format_traceback, identity, log, rsa_encrypt
)
from .command import CommandHandler, command_icon
from .event import (
    UserBreakException, interrupt_init, interrupt_watcher,
    top_level_exception, unlock_callbacks, wx_callback, yield_,
)
from .screen import (
    Browser, KeyHandler, Keymap, StatusBar,
    acceskey_prefix, beep, busy_cursor, get_icon, uicommand_mitem,
    wx_focused_window, make_in_operator, hotkey_string,
)
from . import dialog
from pytis.dbdefs.db_pytis_crypto import (
    PytisCryptoDbKey, PytisCryptoUnlockCurrentUserPasswords, PytisCryptoChangePassword,
)

_ = pytis.util.translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.


@pytis.api.implements(pytis.api.Application)
class Application(pytis.api.BaseApplication, wx.App, KeyHandler, CommandHandler):
    """Aplikace systému Pytis.

    Pro každou aplikaci systému Pytis existuje po celou dobu jejího běhu jedno
    hlavní aplikační okno.  To se sestává jednak ze statických prvků a jednak z
    vyměnitelného vnitřku okna (vlastních formulářů).  Statickými prvky jsou
    pull-down menu a stavový řádek.

    The application may be customized by defining a class named 'Application'
    within aplication's resolver modules.  This class must be derived from
    'pytis.presentation.Application' and customizations may be done by
    overriding its methods and attributes (see the base class docstring).

    Start uživatelského rozhraní spočívá ve vytvoření instance této třídy a
    volání její metody 'run()'.

    """
    _STATE_RECENT_FORMS = 'recent_forms'
    _STATE_STARTUP_FORMS = 'saved_startup_forms'  # Avoid name conflict with config.startup_forms!
    _STATE_RECENT_DIRECTORIES = 'recent_directories'
    _STATE_FRAME_SIZE = 'frame_size'
    _WINDOW_MENU_ID = 'window-menu'
    _RECENT_FORMS_MENU_ID = 'recent-forms-menu'

    class _StatusFieldAccess(object):
        def __init__(self, fields):
            self._fields = fields

        def __getattr__(self, name):
            f = find(name, self._fields, key=lambda f: f.spec.id().replace('-', '_'))
            if f:
                return f.provider()
            else:
                raise AttributeError("StatusBar has no field '{}'".format(name))

        def __call__(self, name):
            return self.__getattr__(name)

    @classmethod
    def _get_command_handler_instance(cls):
        return pytis.form.app

    def __init__(self, headless=False):
        """Arguments:

          headless -- Run the application without showing any windows/frames.
            Experimantal mode for running tests which require the application
            environment, but no user interaction.  Sample tests in Pytis Demo.

        """
        self._headless = headless
        self._menubar_forms = {}
        self._log_login = True
        self._recent_directories = {}
        self._remote_connection_last_available = None
        self._menu_by_id = {}
        super(Application, self).__init__()

    def OnInit(self):
        import pytis.extensions
        wx.Log.SetActiveTarget(wx.LogStderr())

        # Make sure menu icons are always displayed.  Without this, the icons are
        # not displayed under certain contitions (sometimes within an x2go session,
        # sometimes on certain installations).
        gi.require_version('Gtk', '3.0')
        from gi.repository import Gtk as gtk
        settings = gtk.Settings.get_default()
        settings.set_property('gtk-menu-images', True)

        # Create the main application frame (set frame title later on).
        frame = self._frame = wx.Frame(None, -1, '', pos=(0, 0), style=wx.DEFAULT_FRAME_STYLE)
        wx_callback(wx.EVT_CLOSE, frame, self._on_frame_close)
        # This panel is here just to catch keyboard events (frame doesn't support EVT_KEY_DOWN).
        self._panel = wx.Panel(frame, -1)
        KeyHandler.__init__(self, self._panel)
        wx.ToolTip('').Enable(pytis.config.show_tooltips)
        self._logo = None
        logo_file = pytis.config.logo
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
                icon = wx.Icon()
                icon.CopyFromBitmap(icon_bmp)
                icons.AddIcon(icon)
        frame.SetIcons(icons)
        self._windows = XStack()
        self._modals = Stack()
        self._statusbar = None
        self._help_browser = None
        self._login_success = False
        keymap = self.keymap = Keymap()
        custom_keymap = self._specification.keymap()
        assert isinstance(custom_keymap, (tuple, list)), custom_keymap
        for key, cmd in pytis.form.DEFAULT_KEYMAP + custom_keymap:
            if isinstance(cmd, (list, tuple)):
                cmd, args = cmd
            else:
                args = {}
            keymap.define_key(key, cmd, args)
        pytis.form.app = self
        if not self._headless:
            self._statusbar = sb = StatusBar(frame, self._specification.status_fields())
            self._status_fields = self._StatusFieldAccess(sb.fields)

        # Initialize login and password.
        success, result = db_operation(pd.dbtable, 'pg_catalog.pg_tables', ('tablename',))
        if not success:
            return False
        # Unlock crypto keys
        self._unlock_crypto_keys()

        self._initial_config = [
            (o, copy.copy(getattr(pytis.config, o)))
            for o in pytis.form.configurable_options() + ('initial_keyboard_layout',)]
        self._saved_state = {}
        # Initialize all needed user settings managers.
        self._application_config_manager = pytis.form.ApplicationConfigManager(
            pytis.config.dbconnection)
        self._form_settings_manager = pytis.form.FormSettingsManager(pytis.config.dbconnection)
        self._profile_manager = pytis.form.FormProfileManager(pytis.config.dbconnection)
        self._aggregated_views_manager = pytis.form.AggregatedViewsManager(
            pytis.config.dbconnection)
        # Initialize user action logger.
        try:
            self._logger = DbActionLogger(pytis.config.dbconnection, pytis.config.dbuser)
        except pd.DBException as e:
            # Logging is optional.  The application may choose not to include
            # the logging table in the database schema and this will simply
            # lead to logging being ignored.
            log(OPERATIONAL, "Form action logging not activated:", e)
            self._logger = None
        # Read the stored configuration.
        for option, value in self._application_config_manager.load().items():
            if hasattr(pytis.config, option):
                setattr(pytis.config, option, value)
            else:
                self._saved_state[option] = value
        # Read in access rights.
        self._init_access_rights()
        # Init the recent forms list.
        recent_forms = self._get_state_param(self._STATE_RECENT_FORMS, [], list, list)
        self._recent_forms = []
        for title, form_name, spec_name in recent_forms:
            form_class = getattr(pytis.form, form_name, None)
            if issubclass(form_class, pytis.form.Form) and self._is_valid_spec(spec_name):
                self._recent_forms.append((title, form_name, spec_name))
            else:
                log(OPERATIONAL, "Ignoring recent form:", (title, form_name, spec_name))
        self._set_state_param(self._STATE_RECENT_FORMS, list(self._recent_forms))
        # Init the recent directories memory.
        recent_directories = self._get_state_param(self._STATE_RECENT_DIRECTORIES, {}, dict)
        self._recent_directories.update(recent_directories)
        # Initialize the menubar.
        mb = self._create_menubar()
        if mb is None:
            return False
        # Finish and show the frame.
        #
        # TODO: There are problems when users use multiple monitors or start application
        # with different screens or screen resolutions. So until we find the adequate
        # solution, we use some safe default frame size.
        #
        # frame.SetSize(self._get_state_param(self._STATE_FRAME_SIZE, [1000, 800], list, int))
        frame.SetSize((1000, 800))
        wx_callback(wx.EVT_SIZE, frame, self._on_frame_size)
        self.SetTopWindow(frame)
        if not self._headless:
            frame.Show(True)
        # Initialize the toolbar.
        self._toolbar = toolbar = frame.CreateToolBar(wx.NO_BORDER | wx.TB_DOCKABLE)
        for group in pytis.form.TOOLBAR_COMMANDS:
            if group != pytis.form.TOOLBAR_COMMANDS[0]:
                toolbar.AddSeparator()
            for uicmd in group:
                uicmd.create_toolbar_ctrl(self._toolbar)
        toolbar.Realize()

        def init():
            try:
                self._init()
            except Exception:
                top_level_exception()

        wx.CallAfter(init)
        return True

    def _cache_menu_enabled(self, menu):
        # Cache the specification instances needed to compute the availability
        # of menu items.  This reduces the lag during the first user's attempt
        # to open a menu.
        for item in menu:
            if isinstance(item, Menu):
                self._cache_menu_enabled(list(item.items))
            elif not isinstance(item, MenuSeparator):
                enabled = item.command.enabled(**item.args)
                if __debug__:
                    if pytis.config.debug:
                        log(DEBUG, 'Menu item:', (item.title, enabled))

    def _spec_title(self, name):
        if name.find('::') != -1:
            names = name.split('::')
            return (pytis.config.resolver.get(names[0], 'binding_spec')[names[1]].title() or
                    ' / '.join([self._spec_title(n) for n in names]))
        else:
            return pytis.config.resolver.get(name, 'view_spec').title()

    def _init(self):
        # Run application specific initialization.
        self._specification.init()
        if self._windows.empty():
            self._panel.SetFocus()
        # (Re)open the startup forms saved on last exit.
        startup_forms = []
        if pytis.config.startup_forms:
            for name in pytis.config.startup_forms.split(','):
                if name.find('/') != -1:
                    cls_name, name = name.split('/')
                    try:
                        cls = getattr(pytis.form, cls_name.strip())
                        if not issubclass(cls, pytis.form.Form):
                            raise AttributeError
                    except AttributeError:
                        app.error(_("Invalid form class in 'startup_forms':") + ' ' + cls_name)
                        continue
                else:
                    cls = (name.find('::') == -1 and
                           pytis.form.BrowseForm or
                           pytis.form.BrowseDualForm)
                startup_forms.append((cls, name.strip()))
        saved_forms = []
        for cls, name in self._get_state_param(self._STATE_STARTUP_FORMS, [], list, list):
            if isinstance(cls, basestring):
                cls = getattr(pytis.form, cls)
            if ((issubclass(cls, pytis.form.Form) and
                 self._is_valid_spec(name) and
                 (cls, name) not in startup_forms)):
                saved_forms.append((cls, name))
            else:
                log(OPERATIONAL, "Ignoring saved startup form:", (cls, name))
        if saved_forms and not self._headless:
            if pytis.config.autostart_saved_forms:
                startup_forms.extend(reversed(saved_forms))
            else:
                checked = self.run_dialog(
                    dialog.CheckListDialog,
                    title=_("Restore forms"),
                    message=_("Restore these forms saved on last exit?"),
                    items=[(True, '%s (%s)' % (self._spec_title(name), f.descr()))
                           for f, name in saved_forms]
                )
                if checked:
                    startup_forms.extend(reversed([x for x, ch in zip(saved_forms, checked) if ch]))

        def run_startup_forms(update, startup_forms):
            total = len(startup_forms)
            msg = _("Opening form: %s (%d/%d)")
            for i, (cls, name) in enumerate(startup_forms):
                update(i, message=msg % (name, i + 1, total))
                try:
                    run_form(cls, name)
                except Exception as e:
                    log(OPERATIONAL, "Unable to init startup form:", (cls, name, e))
        menu_items = pytis.extensions.get_menu_forms()
        if menu_items:
            # get_menu_forms() returns an empty list if DMP is not used!
            filtered_forms = []
            for i, f in enumerate(startup_forms[:]):
                for m in menu_items:
                    if f[0] == m[0] and f[1] == m[1]:
                        filtered_forms.append(f)
                        break
            startup_forms = filtered_forms
        if len(startup_forms) > 1:
            app.run(run_startup_forms, args=(startup_forms,), maximum=len(startup_forms),
                    title=_("Opening saved forms"), message=_("Opening form") + ' ' * 40)
        else:
            run_startup_forms(lambda *args, **kwargs: True, startup_forms)
        app.title = pytis.config.application_name
        # Caching menu availibility must come after calling Application.init()
        # (here self._specification.init()) to allow the application defined
        # enabled() methods to refer things created Application.init().
        self._cache_menu_enabled(self._menu)
        self._specification.post_init()

    def _is_valid_spec(self, name):
        # Determine whether the specification name still exists.
        if name is None:
            return False
        elif '::' in name:
            name, side_name = name.split('::')
        else:
            side_name = None
        try:
            pytis.config.resolver.get(name, 'view_spec')
        except ResolverError:
            return False
        else:
            if side_name:
                if not self._is_valid_spec(side_name):
                    return False
                try:
                    bindings = pytis.config.resolver.get(name, 'binding_spec')
                except ResolverError:
                    return False
                if not isinstance(bindings, dict) or side_name not in bindings:
                    return False
            return True

    def _public_spec(self, name):
        try:
            spec_class = pytis.config.resolver.specification(name)
        except Exception:
            return True
        else:
            return spec_class.public

    def _dynamic_menu(self):
        def build(template):
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

            used_letters = []
            if template is None:
                result = MenuSeparator()
            elif isinstance(template, list):
                heading = template[0]
                items = [build(i) for i in template[1:]]
                if heading is None:
                    result = items
                else:
                    title = add_key(heading[1])
                    result = Menu(title, items)
            else:
                name, title, action, help, hotkey = template
                command = self._parse_action(action)
                if hotkey:
                    hotkey = tuple(key.replace('SPC', ' ') for key in hotkey.split(' '))
                result = MenuItem(add_key(title), command, help=help, hotkey=hotkey)
            return result

        # Check for menu presence, if not available, return None
        language = pytis.util.current_language()
        try:
            menu_data = pd.dbtable('pytis_view_user_menu',
                                   (('menuid', pd.Integer()),
                                    ('name', pd.String()),
                                    ('title', pd.String()),
                                    ('fullname', pd.String()),
                                    ('position', pd.LTree(),),
                                    ('help', pd.String()),
                                    ('hotkey', pd.String()),
                                    ('language', pd.String()),),
                                   arguments=())
            menu_rows = menu_data.select_map(identity,
                                             condition=pd.EQ('language', pd.sval(language)),
                                             sort=(('position', pd.ASCENDENT,),))
        except pd.DBException:
            return None
        if not menu_rows:
            return None
        # Build visible menu items
        menu_template = []
        parents = []
        for row in menu_rows:
            menuid, name, title, action, position, help, hotkey = \
                [row[i].value() for i in (0, 1, 2, 3, 4, 5, 6,)]
            if not parents:  # the top pseudonode, should be the first one
                parents.append((position or '', menu_template,))
                current_template = menu_template
            else:
                parent = '.'.join(position.split('.')[:-1])
                parent_index = len(parents) - 1
                while parent_index >= 0 and parent != parents[parent_index][0]:
                    parent_index -= 1
                if parent_index >= 0:
                    parents = parents[:parent_index + 1]
                else:
                    continue
                current_template = parents[-1][1]
                if not title:  # separator
                    current_template.append(None)
                elif name:  # terminal item
                    current_template.append((name, title, action, help, hotkey,))
                else:          # non-terminal item
                    upper_template = parents[-1][1]
                    current_template = [(name, title, help, hotkey,)]
                    upper_template.append(current_template)
                    parents.append((position, current_template,))
        # Done, build the menu structure from the template.
        return [build(t) for t in menu_template]

    def _parse_action(self, action):
        """Parse action id back to command and its arguments.

        Arguments:

          action -- the action id, string
          globals -- dictionary of global name space
          locals -- dictionary of local name space

        Return pair COMMAND, ARGUMENTS corresponding to the given action id.
        If the action id is invalid, behavior of this method is undefined.

        """
        components = action.split('/')
        kind = components[0]

        def find_symbol(symbol):
            # temporary hack to not crash on special situations to be solved
            # later
            try:
                return eval(symbol)
            except AttributeError:
                sys.stderr.write("Can't find object named `%s'\n" % (symbol,))
                return None
        if components[-1]:
            return components[-1]
        elif kind == 'form':
            command = pytis.form.Application.COMMAND_RUN_FORM
            class_name, form_name = components[1], components[2]
            arguments = dict(form_class=find_symbol(class_name), name=form_name)
            if components[3]:
                for extra in components[3].split('&'):
                    if extra[:len('binding=')] == 'binding=':
                        arguments['binding'] = extra[len('binding='):]
                        break
        elif kind == 'handle':
            command = pytis.form.Application.COMMAND_HANDLED_ACTION
            function_name = components[1]
            arguments = dict(handler=find_symbol(function_name),
                             enabled=lambda: pytis.form.app.action_has_access(action))
        elif kind == 'proc':
            command = pytis.form.Application.COMMAND_RUN_PROCEDURE
            proc_name, spec_name = components[1], components[2]
            arguments = dict(proc_name=proc_name, spec_name=spec_name,
                             enabled=lambda: pytis.form.app.action_has_access(action))
        elif kind == 'NEW_RECORD':
            command = pytis.form.Application.COMMAND_NEW_RECORD
            arguments = dict(name=components[1])
        else:
            command = pytis.form.Command.command(kind)
            arguments = None
        return command, arguments

    def _create_menubar(self):
        menu = self._dynamic_menu()
        if menu is None:
            menu = list(self._specification.menu())
        self._menu = menu

        menu.append(Menu(_("&Windows"), (
            MenuItem(_("Previous window"), command=Application.COMMAND_RAISE_PREV_FORM(),
                     help=_("Switch to the previous window in the window list order.")),
            MenuItem(_("Next window"), command=Application.COMMAND_RAISE_NEXT_FORM(),
                     help=_("Switch to the next window in the window list order.")),
            MenuItem(_("Most recently active window"),
                     command=Application.COMMAND_RAISE_RECENT_FORM(),
                     help=_("Allows mutual switching of two most recently active "
                            "windows cyclically.")),
            MenuItem(_("Close active window"), command=pytis.form.Form.COMMAND_LEAVE_FORM(),
                     help=_("Closes the window of the active form.")),
            MenuSeparator(),
        ), name=self._WINDOW_MENU_ID, autoindex=False))
        command_menu_items = []
        for group in pytis.form.FORM_MENU_COMMANDS:
            if command_menu_items:
                command_menu_items.append(MenuSeparator())
            for uicmd in group:
                command_menu_items.append(uicommand_mitem(uicmd))
        menu.append(Menu(_("Commands"), command_menu_items))
        if _("Help") in [m.title for m in menu]:
            log(OPERATIONAL, "Help menu found - not creating one.")
        else:
            menu.append(Menu(_("Help"), [
                uicommand_mitem(pytis.form.UICommands.PYTIS_HELP)
            ] + [
                MenuSeparator(),
                uicommand_mitem(pytis.form.UICommands.HELP),
                uicommand_mitem(pytis.form.UICommands.DESCRIBE),
            ]))

        self._menubar = menubar = wx.MenuBar()
        #print(self.keymap._keymap)
        for item in menu:
            # Determining availability of menu items may invoke database operations...
            success, result = db_operation(self._create_menu, menubar, item, self.keymap)
            if success:
                menubar.Append(result, item.title)
            else:
                return None
        self._frame.SetMenuBar(menubar)
        return menubar

    def _create_menu(self, parent, spec, keymap):
        def on_highlight_item(event):
            if event.GetMenuId() != -1:
                item = menu.FindItemById(event.GetMenuId())
                if item:
                    app.echo(item.GetHelp())
            event.Skip()

        def lookup_key(keymap, hotkey):
            key, hotkey = hotkey[0], hotkey[1:]
            keydef = keymap.lookup_key(key)
            if isinstance(keydef, Keymap) and hotkey:
                return lookup_key(keydef, hotkey)
            elif isinstance(keydef, list):
                return keydef[0]
            else:
                return None

        def cmdstr(command, args):
            return '%s(%s)' % (command.name(), ', '.join('%s=%r' % x for x in args.items()))

        # At first, compute the maximal width of hotkey string in this menu.
        max_hotkey_width = 0
        hotkey_strings = {}
        for item in spec.items:
            if isinstance(item, MenuItem):
                hotkey = item.hotkey
                if hotkey and keymap:
                    cmd = lookup_key(keymap, hotkey)
                    if cmd and cmd != (item.command, item.args):
                        log(OPERATIONAL, "Duplicate hotkey %s on menu item '%s': "
                            "Command %s collides with command %s defined elsewhere." %
                            (hotkey, item.title, cmdstr(item.command, item.args), cmdstr(*cmd)))
                if not hotkey and keymap:
                    args = dict([(k, v) for k, v in item.args.items()
                                 if k != '_command_handler'])
                    hotkey = keymap.lookup_command(item.command, args)
                elif keymap:
                    keymap.define_key(hotkey, item.command, item.args)
                if hotkey:
                    hotkey_strings[item] = string = '    ' + hotkey_string(hotkey)
                    hotkey_width = parent.GetTextExtent(string)[0]
                    max_hotkey_width = max(hotkey_width, max_hotkey_width)

        # Now create the items and remember max. width of whole item label.
        menu = wx.Menu()
        wx_callback(wx.EVT_MENU_HIGHLIGHT_ALL, menu, on_highlight_item)
        if spec.name:
            self._menu_by_id[spec.name] = menu
        hotkey_items = []
        max_label_width = 0
        i = 0

        for item in spec.items:
            if isinstance(item, MenuSeparator):
                menu.AppendSeparator()
            else:
                title = item.title
                if spec.autoindex and pytis.config.auto_menu_accel:
                    title = acceskey_prefix(i) + title
                i += 1
                width = parent.GetTextExtent(title.replace('&', ''))[0] + 20
                if isinstance(item, Menu):
                    menu.AppendSubMenu(self._create_menu(parent, item, keymap), title)
                    max_label_width = max(width + 20, max_label_width)
                elif isinstance(item, MenuItem):
                    mitem = self._append_menu_item(menu, item, title)
                    if item in hotkey_strings:
                        hotkey_items.append((item, mitem, title, width))
                    max_label_width = max(width + max_hotkey_width, max_label_width)
                else:
                    raise ProgramError('Invalid menu item type', item)
        # Append hotkey description string to the item labels.
        # Fill with spaces to justify hotkeys on the right edge.
        space_width = parent.GetTextExtent(' ')[0]
        for item, mitem, title, width in hotkey_items:
            fill_width = max_label_width - width - max_hotkey_width
            n = round(float(fill_width) / float(space_width))
            fill = "%%%ds" % n % ''
            mitem.SetItemLabel(title + fill + hotkey_strings[item])
        return menu

    def _append_menu_item(self, menu, item, title=None):
        def on_ui_event(event):
            event.Enable(item.command.enabled(**item.args))
            if item.state:
                state = item.state()
                print(item.title, state)
                event.Check(state)

        def on_invoke_command(event):
            # Invoke the command through CallAfter to finish menu event processing before
            # command invocation.  Calling dirrectly for example resulted in disfunctional
            # TAB traversal in a popup form which was opended through a popup menu command
            # due to FindFocus() returning a wrong window.  Using CallAfter fixes this.
            wx.CallAfter(item.command.invoke, **item.args)

        if item.state:
            # kind = wx.ITEM_RADIO
            # wx.ITEM_RADIO causes SEGFAULT.  wx.ITEM_CHECK, however,
            # seems to have the same behavior...
            kind = wx.ITEM_CHECK
            icon = None
        else:
            kind = wx.ITEM_NORMAL
            icon = get_icon(item.icon or command_icon(item.command, item.args))
        mitem = wx.MenuItem(menu, -1, title or item.title, item.help or "", kind=kind)
        wx_callback(wx.EVT_MENU, self._frame, on_invoke_command, source=mitem)
        wx_callback(wx.EVT_UPDATE_UI, self._frame, on_ui_event, source=mitem)
        if icon:
            mitem.SetBitmap(icon)
        menu.Append(mitem)
        return mitem

    def _unlock_crypto_keys(self):
        def password_dialog(title, message, verify=False, check=()):
            result = run_form(
                pytis.form.InputForm, title=title,
                fields=(Field('password', _("Password"),
                              type=pd.Password(verify=verify, not_null=True), width=40),),
                layout=(Text(message), 'password'),
                check=check,
            )
            if result:
                return rsa_encrypt(db_key, result['password'].value()).decode('ascii')
            else:
                return None

        self._decrypted_areas = decrypted_areas = set()

        try:
            data = pd.dbtable('ev_pytis_user_crypto_keys', ('key_id', 'name', 'fresh'))
        except pd.DBException:
            return
        else:
            rows = data.select_map(identity)
            data.close()
        if not rows:
            return

        db_key = pd.dbfunction(PytisCryptoDbKey, 'pytis')
        crypto_password = pytis.config.dbconnection.crypto_password()
        if not crypto_password:
            established_names = [r for r in rows if not r['fresh'].value()]
            crypto_password = password_dialog(
                _("Enter your password"),
                _("Enter your login password for encryption keys management"),
                verify=not established_names,
                check=(
                    lambda r: ('password', _("Invalid password"))
                    if established_names and not pd.dbfunction(
                            PytisCryptoUnlockCurrentUserPasswords,
                            rsa_encrypt(db_key, r['password'].value()).decode('ascii')
                    )
                    else None,),
            )
            if not crypto_password:
                return
            # Set this password for all DB connections (closes all current connections!).
            pd.reset_crypto_password(pytis.config.dbconnection, crypto_password)

        while True:
            established_names = set()
            fresh_names = set()
            for row in data.select_map(identity):
                name = row['name'].value()
                if row['fresh'].value():
                    fresh_names.add(name)
                else:
                    established_names.add(name)
            ok_names = pd.dbfunction(PytisCryptoUnlockCurrentUserPasswords, crypto_password)
            if isinstance(ok_names, list):
                ok_names = set([row[0].value() for row in ok_names])
            else:
                ok_names = set([ok_names])
            decrypted_areas.update(ok_names)
            bad_names = established_names.difference(ok_names)
            if fresh_names:
                name = list(fresh_names)[0]
                bad = False
            elif bad_names:
                name = list(bad_names)[0]
                bad = True
            else:
                break
            message = _("Enter the password to unlock the encryption area %s.", name)
            if bad:
                message += "\n(" + _("This is probably your old login password.") + ")"
            password = password_dialog(_("Encryption key password"), message)
            if not password:
                break
            for r in rows:
                r_name = r['name'].value()
                if r_name == name or (bad and r_name in bad_names):
                    try:
                        pd.dbfunction(PytisCryptoChangePassword,
                                      r['key_id'].value(), password, crypto_password)
                    except pd.DBException:
                        pass
        data.close()
        pd.dbfunction(PytisCryptoUnlockCurrentUserPasswords, crypto_password)

    # Ostatní metody

    def _form_menu_item_title(self, form):
        title = form.title()
        if form.__class__ != pytis.form.BrowseForm:
            title += " (%s)" % form.descr()
        return title

    def _modal_parent(self):
        # Return the wx parent for a newly created modal form or dialog.
        # Returns the top most modal form or the main application frame.
        # Modal pytis dialogs are ignored as they are not wx.Window subclasses.
        if not self._modals.empty() and isinstance(self._modals.top(), wx.Window):
            parent = self._modals.top()
        else:
            parent = self._frame
        return parent

    def _update_window_menu(self):
        menu = self._menu_by_id.get(self._WINDOW_MENU_ID)
        if menu is not None:
            for i, item in enumerate(menu.GetMenuItems()):
                if i >= 5:
                    menu.Remove(item.GetId())
                    item.Destroy()
            for i, form in enumerate(self._windows.items()):
                info = form.__class__.__name__
                if form.name():
                    info += '/' + form.name()
                self._append_menu_item(menu, MenuItem(
                    acceskey_prefix(i) + self._form_menu_item_title(form),
                    help=_("Bring form window to the top (%s)", info),
                    command=Application.COMMAND_RAISE_FORM(form=form),
                    state=lambda form=form: self.top_window() is form,
                ))

    def _update_recent_forms_menu(self):
        menu = self._menu_by_id.get(self._RECENT_FORMS_MENU_ID)
        if menu:
            for item in menu.GetMenuItems():
                menu.Remove(item.GetId())
                item.Destroy()
            for i, (title, form_name, spec_name) in enumerate(self._recent_forms):
                self._append_menu_item(menu, MenuItem(
                    acceskey_prefix(i) + title,
                    help=_("Open the form (%s)", form_name + '/' + spec_name),
                    command=Application.COMMAND_RUN_FORM(form_class=getattr(pytis.form, form_name),
                                                         name=spec_name),
                ))
            menu.AppendSeparator()
            self._append_menu_item(menu, MenuItem(
                _("Clear"),
                help=_("Clear the menu of recent forms"),
                command=Application.COMMAND_CLEAR_RECENT_FORMS(),
            ))

    def _raise_form(self, form):
        if form is not None:
            if form not in self._frame.GetChildren():
                log(EVENT, "Reparent -- maybe it is really needed here...")
                form.Reparent(self._frame)
            old = self._windows.active()
            if form is not old:
                self.save()
                old.hide()
                self._windows.activate(form)
                self.restore()

    def _close_forms(self):
        success = True
        for form in self._windows.items():
            try:
                self._raise_form(form)
                if not form.close():
                    success = False
            except Exception:
                success = False
            if not success:
                break
        if not success or self._windows.items():
            return False
        else:
            return True

    def _config_filename(self):
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
        name = pytis.config.application_name.lower()
        return str(re.sub("[^a-zA-Z0-9-]", safe_char, unistr(name)))

    def _get_state_param(self, name, default=None, cls=None, item_cls=None):
        try:
            param = self._saved_state[name]
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
        self._saved_state[name] = value

    def _unset_state_param(self, name):
        if name in self._saved_state:
            del self._saved_state[name]

    def _cleanup(self):
        # Zde ignorujeme všemožné výjimky, aby i při poměrně značně havarijní
        # situaci bylo možno aplikaci ukončit.
        def safelog(msg, *args):
            try:
                log(ACTION, msg, *args)
            except Exception:
                print(msg, args)
        safelog('Application exit called:', (pytis.config.dbschemas,))
        try:
            if not self._modals.empty():
                log(EVENT, "Couldn't close application with modal windows:",
                    self._modals.top())
                return False
            self._set_state_param(self._STATE_STARTUP_FORMS, [
                (f.__class__.__name__, f.name())
                for f in self._windows.items()
                if not isinstance(f, (pytis.form.AggregationForm,
                                      pytis.form.AggregationDualForm))
            ])
            self._set_state_param(self._STATE_RECENT_FORMS, list(self._recent_forms))
            self._set_state_param(self._STATE_RECENT_DIRECTORIES, self._recent_directories)
            self._set_state_param(self._STATE_FRAME_SIZE, list(self._frame.GetSize()))
        except Exception as e:
            safelog(str(e))
        try:
            for form in self._windows.items():
                try:
                    self._raise_form(form)
                    if not form.close():
                        return False
                except Exception as e:
                    safelog(str(e))
        except Exception as e:
            safelog(str(e))
        try:
            options = self._saved_state
            for option, initial_value in self._initial_config:
                current_value = getattr(pytis.config, option)
                if current_value != initial_value:
                    options[option] = current_value
            self._application_config_manager.save(options)
            log(OPERATIONAL, "Configuration saved: %s" % ', '.join(options.keys()))
        except Exception as e:
            safelog("Saving changed configuration failed:", str(e))
        try:
            if self._help_browser is not None:
                self._help_browser.GetParent().Close()
        except Exception as e:
            safelog(str(e))
        return True

    # Callbacky

    def _on_frame_close(self, event):
        if not self._cleanup():
            event.Veto()
        else:
            event.Skip()
            pytis.form.app = None

    def _on_frame_size(self, event):
        size = event.GetSize()
        self._frame.SetSize(size)
        top = self._windows.active()
        if top is not None:
            top.resize()
        if self._logo is not None:
            logo = self._logo.GetBitmap()
            logo_posx = max((size.GetWidth() - logo.GetWidth()) // 2, 0)
            logo_posy = max((size.GetHeight() - logo.GetHeight() - 50) // 2, 0)
            self._logo.SetPosition((logo_posx, logo_posy))
            if top is None:
                self._logo.Show(True)
        return True

    def _on_form_close(self, event):
        form = event.GetEventObject()
        assert form is self._windows.active()
        log(EVENT, "Non-modal form closed:", form)
        self._windows.remove(form)
        self._update_window_menu()
        self.restore()

    def on_key_down(self, event, dont_skip=False):
        # Toto je záchranný odchytávač.  Věřte tomu nebo ne, ale pokud tady ta
        # metoda není, wxWidgets se při více příležitostech po stisku klávesy
        # zhroutí.
        return KeyHandler.on_key_down(self, event)

    # Zpracování příkazů

    def _cmd_break(self):
        app.echo(_("Stopped..."), kind='error')

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
        self._update_recent_forms_menu()

    def _cmd_refresh(self, interactive=True):
        for w in (self._modals.top(), self._windows.active()):
            if isinstance(w, pytis.form.Refreshable):
                w.refresh(interactive=interactive)

    def _cmd_reload_specifications(self):
        pytis.config.resolver.reload()
        self._cache_menu_enabled(self._menu)

    def _can_run_form(self, form_class, name, binding=None, **kwargs):
        if form_class in (pytis.form.InputForm, pytis.form.WebForm) and name is None:
            return True
        if ((isinstance(self.current_form(), pytis.form.PopupForm) and
             not issubclass(form_class, pytis.form.PopupForm))):
            return False
        if not self._public_spec(name):
            return False
        try:
            if self.api_has_access(name):
                if binding is not None or issubclass(form_class, pytis.form.MultiBrowseDualForm):
                    spec = pytis.config.resolver.get(name, 'view_spec')
                    if binding is None:
                        for b in spec.bindings():
                            binding_name = b.name()
                            if binding_name is None or self.api_has_access(binding_name):
                                return True
                        return False
                    else:
                        b = find(binding, spec.bindings(), key=lambda b: b.id())
                        assert b is not None, "Unknown binding for %s: %s" % (name, binding)
                        if b.name():
                            return self.api_has_access(b.name())
                        else:
                            return True
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
            log(ACTION, 'Running form:', (form_class, name, kwargs))
            app.echo(_("Opening form..."))
            assert issubclass(form_class, pytis.form.Form)
            assert name is None or isinstance(name, basestring)  # May be None for InputForm.
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
                app.echo(_('Form "%s" found between open windows.', form.title()))
                if 'select_row' in kwargs and kwargs['select_row'] is not None:
                    form.select_row(kwargs['select_row'])
                if 'filter' in kwargs and kwargs['filter'] is not None:
                    form.filter(kwargs['filter'])
                if 'binding' in kwargs and kwargs['binding'] is not None:
                    form.select_binding(kwargs['binding'])
                if 'profile_id' in kwargs and kwargs['profile_id'] is not None:
                    form.apply_profile(kwargs['profile_id'])
                if isinstance(form, pytis.form.WebForm) and 'content' in kwargs:
                    form.load_content(kwargs['content'])
                return result
            if issubclass(form_class, pytis.form.PopupForm):
                parent = self._modal_parent()
                kwargs['guardian'] = self._modals.top() or self
            else:
                # assert self._modals.empty()
                parent = self._frame
                kwargs['guardian'] = self
            args = (parent, pytis.config.resolver, name)
            try:
                form = form_class(*args, **kwargs)
            except pytis.form.Form.InitError:
                form = None
            if form is None:
                busy_cursor(False)
                app.error(_("Form creation failed: %s", name))
            else:
                if isinstance(form, pytis.form.PopupForm):
                    log(EVENT, "Opening modal form:", form)
                    self._modals.push(form)
                    app.status.message.update(text=None, icon=None)
                    form.show()
                    busy_cursor(False)
                    try:
                        form_str = str(form)  # Dead form doesn't speak...
                        result = form.run()
                        log(EVENT, "Modal form closed:", form_str)
                        log(EVENT, "Return value:", result)
                    finally:
                        self._modals.pop()
                        busy_cursor(False)
                    top = self.top_window()
                    if top is not None:
                        if isinstance(top, pytis.form.Refreshable):
                            top.refresh()
                        top.focus()
                    else:
                        self._panel.SetFocus()
                else:
                    log(EVENT, "Opening non-modal form:", form)
                    old = self._windows.active()
                    if old is not None:
                        old.hide()
                    self._windows.push(form)
                    wx_callback(wx.EVT_CLOSE, form, self._on_form_close)
                    app.status.message.update(text=None, icon=None)
                    form.resize()  # Needed in wx 2.8.x.
                    form.show()
                    self._update_window_menu()
                    if not isinstance(form, pytis.form.WebForm):
                        item = (self._form_menu_item_title(form), form_class.__name__, name)
                        try:
                            self._recent_forms.remove(item)
                        except ValueError:
                            pass
                        self._recent_forms.insert(0, item)
                        if len(self._recent_forms) > 10:
                            self._recent_forms[10:] = []
                        self._update_recent_forms_menu()
        except UserBreakException:
            pass
        except SystemExit:
            raise
        except Exception:
            top_level_exception()
        return result

    def _can_new_record(self, name, **kwargs):
        try:
            return self.api_has_access(name, perm=pd.Permission.INSERT)
        except ResolverError:
            # The spec is invalid, but we want the crash on attempt to run it.
            return True

    def _cmd_new_record(self, **kwargs):
        return self.api_new_record(**kwargs)

    def _can_run_procedure(self, spec_name, proc_name, args=None, enabled=None,
                           block_refresh=False, **kwargs):
        if not self._public_spec(spec_name):
            return False
        return enabled is None and True or enabled(**kwargs)

    def _cmd_run_procedure(self, enabled=None, **kwargs):
        return self.api_run_procedure(**kwargs)

    def _cmd_help(self, topic='pytis'):
        """Zobraz dané téma v prohlížeči nápovědy."""
        browser = self._help_browser
        if not browser:
            frame = wx.Frame(None)
            self._help_browser = browser = Browser(frame)
            sizer = wx.BoxSizer(wx.VERTICAL)
            sizer.Add(browser.toolbar(frame), proportion=0, flag=wx.EXPAND)
            sizer.Add(browser, proportion=1, flag=wx.EXPAND)
            frame.SetSizer(sizer)
            frame.SetSize((800, 600))
            frame.SendSizeEvent()
            frame.SetTitle(_("Help"))
            browser.set_callback(browser.CALL_TITLE_CHANGED, frame.SetTitle)
        browser.GetParent().Raise()
        browser.load_uri('help:' + topic)

    def _cmd_reload_rights(self):
        self._init_access_rights()
        self._create_menubar()
        self._update_window_menu()
        self._cache_menu_enabled(self._menu)

    def _cmd_custom_debug(self):
        if __debug__:
            pytis.config.custom_debug()

    def _cmd_inspect(self):
        import wx.lib.inspection
        tool = wx.lib.inspection.InspectionTool()
        tool.Init(app=self)
        tool.Show()

    def _cmd_exit(self):
        self.api_exit()

    def _can_nothing(self, enabled=True):
        return enabled

    def _cmd_nothing(self, enabled=True):
        pass

    # Veřejné atributy a metody

    @property
    def headless(self):
        return self._headless

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
        if not isinstance(dialog_or_class_, pytis.form.Dialog):
            class_ = dialog_or_class_
            assert issubclass(class_, pytis.form.Dialog)
            parent = self._modal_parent()
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
        if not self._headless:
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

    def top_window(self, allow_modal=True):
        """Vrať momentálně aktivní okno aplikace.

        """
        if allow_modal and not self._modals.empty():
            return self._modals.top()
        else:
            return self._windows.active()

    def current_form(self, inner=True, allow_modal=True):
        """Vrať právě aktivní formulář aplikace, pokud existuje.

        Pokud není otevřen žádný formulář, nebo aktivním oknem není formulář,
        vrací None.  Pokud je aktivním formulářem duální formulář, bude vrácen
        jeho aktivní podformulář, právě pokud je argument 'inner' pravdivý.

        """
        form = self.top_window(allow_modal=allow_modal)
        if not isinstance(form, pytis.form.Form):
            return None
        if inner:
            while isinstance(form, (pytis.form.DualForm, pytis.form.MultiForm)):
                form = form.active_form()
        return form

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
            if isinstance(form, pytis.form.Refreshable):
                form.refresh()
            form.show()
            form.restore()
            form.focus()
        else:
            self._panel.SetFocus()

    def recent_forms_menu(self):
        """Return the menu of recently opened forms as 'Menu' instance."""
        return Menu(_("Recently opened forms"), (),
                    name=self._RECENT_FORMS_MENU_ID, autoindex=False)

    def wx_frame(self):
        """Return the main application frame as 'wx.Frame' instance."""
        return self._frame

    def login_hook(self, success):
        if not self._login_success:
            self._specification.login_hook(success)
            if success:
                self._login_success = True

    @property
    def profile_manager(self):
        return self._profile_manager

    @property
    def form_settings_manager(self):
        return self._form_settings_manager

    @property
    def aggregated_views_manager(self):
        return self._aggregated_views_manager

    def log(self, spec_name, form_name, action, info=None):
        if self._logger:
            self._logger.log(spec_name, form_name, action, info=info)
        else:
            log(ACTION, "Form action:", (spec_name, form_name, action, info))

    def client_mode(self):
        """Return the client operation mode as one of 'remote', 'local' or None.

        If the remote connection exists, 'remote' is returned.  If it existed at
        the application startup, but doesn't exist now, the user is asked whether
        to continue locally or cancel the operation.  If the user decides to
        cancel, None is returned.  In all other cases 'local' is returned.

        """
        if not pytis.remote.client_available():
            return 'local'
        elif pytis.remote.client_connection_ok():
            return 'remote'
        else:
            cancel = _("Cancel")
            answer = app.question(_("This operation requires remote client connection "
                                    "which is currently broken.\nYou may complete the "
                                    "operation with restriction to server's local "
                                    "resources or cancel."),
                                  #icon=dialog.Message.ICON_ERROR,
                                  answers=(_("Continue"), cancel),
                                  default=cancel)
            if answer is None or answer == cancel:
                return None
            else:
                return 'local'

    def _refresh_list_position(self):
        form = pytis.form.app.current_form(allow_modal=False)
        if hasattr(form, 'list_position'):
            return form.list_position()
        else:
            return ''

    def _refresh_remote_status(self):
        if not pytis.remote.client_available():
            status = _("N/A")
            icon = 'status-offline'
            tooltip = _("Running locally.")
        elif pytis.remote.client_connection_ok():
            version = pytis.remote.RPCInfo.remote_client_version or _("Not available")
            status = _("Ok")
            icon = 'status-online'
            tooltip = _("Connected.") + "\n" + _("Client version: %s", version)
            self._remote_connection_last_available = time.localtime()
        else:
            status = _("Error")
            icon = 'status-error'
            if self._remote_connection_last_available:
                timestamp = time.strftime('%Y-%m-%d %H:%M:%S', self._remote_connection_last_available)
                tooltip = _("Connection lost.") + '\n' + _("Last available: %s", timestamp)
            else:
                tooltip = _("Connection not established.")
        return (status, icon, tooltip)

    def _refresh_user_config(self):
        tooltip = "\n".join((_("Username: %s", pytis.config.dbuser),
                             _("Database name: %s", pytis.config.dbname),
                             _("Database host: %s", pytis.config.dbhost or 'localhost')))
        return (pytis.config.dbuser, 'user-icon', tooltip)

    def status_fields(self):
        """Return default status bar fields as a list of 'StatusField' instances.

        The result is returned by the default implementation of
        'pytis.presentation.Application.status_fields()'.  The derived
        application may, however, extend, reorder or redefine the fields as
        needed.

        """
        return [
            StatusField('message', width=None),
            StatusField('list-position', _("List position"), width=15,
                        refresh=self._refresh_list_position),
            StatusField('user', _("User and database parameters"), width=15,
                        refresh=self._refresh_user_config, refresh_interval=0),
            StatusField('remote-status', _("Remote communication status"), width=10,
                        refresh=self._refresh_remote_status, refresh_interval=10000),
        ]

    def popup_menu(self, parent, items, keymap=None, position=None):
        """Pop-up a wx context menu.

        Arguments:
           parent -- wx parent window of the menu
           items -- sequence of MenuItem, MenuSeparator or Menu instances
           keymap -- keymap to use for determination of menu command keyboard shortcuts
           position -- optional position of the menu as a tuple (x, y).  The menu is normally
             positioned automatically under the pointer so position may be needed when displaying menu
             in response to a keyboard command.

        """
        menu = self._create_menu(parent, Menu('', items), keymap)
        parent.PopupMenu(menu, position or wx.DefaultPosition)
        menu.Destroy()

    # Private methods supporting the public API methods (further below)

    def _wildcards(self, filetypes):
        if filetypes:
            wildcards = (_("Files of the required type") + ' (' + ', '.join(filetypes) + ')',
                         ';'.join(['*.{}'.format(ext) for ext in filetypes]),
                         _("All files"), "*")
        else:
            wildcards = ()
        return wildcards

    def _splitpath(self, cmode, path):
        # Quict hack: unistr makes a local string from rpyc netref obtained from fh.name.
        # Othervise fails with RPyC AttributeError on p.rfind('/') in posixpath.py
        path = unistr(path)
        if cmode == 'local':
            pathmod = os.path
        elif '\\' in path:
            import ntpath as pathmod
        else:
            import posixpath as pathmod
        return pathmod.dirname(path), pathmod.basename(path)

    def _dirname(self, cmode, path):
        return self._splitpath(cmode, path)[0]

    def _get_recent_directory(self, cmode, context):
        if cmode and context:
            directory = self._recent_directories.get(':'.join((cmode, context)))
            if directory is None:
                directory = self._recent_directories.get(':'.join((cmode, 'default')))
        else:
            directory = None
        return directory

    def _set_recent_directory(self, cmode, context, directory):
        self._recent_directories[':'.join((cmode, context))] = directory



    def _dialog_content_kwargs(self, content):
        if not content:
            return dict()
        elif isinstance(content, lcg.Content):
            return dict(report=content)
        elif isinstance(content, basestring):
            return dict(report=content, report_format=TextFormat.PLAIN)
        else:
            raise ProgramError("Invalid 'content': {}".format(content))

    class _ExposedFileWrapper(object):
        def __init__(self, instance):
            self._instance = instance
        def __getattr__(self, name):
            return getattr(self._instance, name)
        def __enter__(self):
            return self
        def __exit__(self, exc_type, exc_value, exc_tb):
            self._instance.close()

    def _wrap_exposed_file_wrapper(self, f):
        if f:
            # Further wrap the ExposedFileWrapper instance to add context manager
            # support to legacy ExposedFileWrapper from older Pytis2Go versions
            # (which don't load remote clientapi.py).
            f = self._ExposedFileWrapper(f)
        return f

    # Public API accessed through 'pytis.api.app' by Pytis applications.
    # See 'pytis.api.Application' for documentation.

    @property
    def api_title(self):
        return self._title

    @api_title.setter
    def api_title(self, title):
        self._title = title
        display = pytis.remote.x2go_display()
        if display:
            title += ' (:%s)' % display
        if __debug__:
            title += ' - wx ' + wx.version() + ', Python %d.%d.%d' % sys.version_info[:3]
        self._frame.SetTitle(title)

    @property
    def api_form(self):
        form = self.current_form(inner=True)
        return form.provider() if form else None

    @property
    def api_main_form(self):
        form = self.current_form(inner=False, allow_modal=False)
        return form.main_form().provider() if isinstance(form, pytis.form.DualForm) else None

    @property
    def api_side_form(self):
        form = self.current_form(inner=False, allow_modal=False)
        if isinstance(form, pytis.form.DualForm):
            side_form = form.side_form()
            if isinstance(side_form, pytis.form.MultiSideForm):
                side_form = side_form.active_form()
            if side_form:
                return side_form.provider()
        return None

    @property
    def api_status(self):
        return self._status_fields

    def api_refresh(self):
        self._cmd_refresh(interactive=False)

    def api_exit(self):
        self._frame.Close()

    def api_echo(self, message, kind='info'):
        assert kind in ('info', 'warning', 'error')
        if kind in ('warning', 'error'):
            beep()
        if message:
            log(EVENT, message)
        form = self._modals.top()
        if not (isinstance(form, pytis.form.Form) and form.set_status('message', message)):
            if kind == 'warning':
                app.status.message.icon = wx.ART_WARNING
            elif kind == 'error':
                app.status.message.icon = wx.ART_ERROR
            else:
                app.status.message.icon = None
            app.status.message.text = message

    def api_message(self, message=None, title=None, content=None):
        return self.run_dialog(dialog.Message, message, title=title or _("Message"),
                               **self._dialog_content_kwargs(content))

    def api_warning(self, message=None, title=None, content=None):
        return self.run_dialog(dialog.Warning, message, title=title or _("Warning"),
                               **self._dialog_content_kwargs(content))

    def api_error(self, message=None, title=None, content=None):
        return self.run_dialog(dialog.Error, message, title=title or _("Error"),
                               **self._dialog_content_kwargs(content))

    def api_question(self, message, answers=None, default=None, title=None, content=None,
                     timeout=None):
        if not title:
            title = _("Question")
        if answers is not None:
            return self.run_dialog(dialog.MultiQuestion, message, answers, default=default,
                                   title=title, timeout=timeout,
                                   **self._dialog_content_kwargs(content))
        else:
            if default is None:
                default = True
            return self.run_dialog(dialog.Question, message, default=default,
                                   title=title, timeout=timeout,
                                   **self._dialog_content_kwargs(content))

    def api_delete_record_question(self, message=None):
        log(EVENT, 'Record deletion dialog')
        if not app.question(message or _("Are you sure to delete the record permanently?")):
            log(EVENT, 'Record deletion refused by user')
            return False
        log(EVENT, u'Record deletion confirmed by user')
        return True

    def _input(self, type, title, label, default=None, width=None, height=None, descr=None,
               noselect=False):
        row = run_form(
            pytis.form.InputForm, title=title, fields=(
                Field('f', label, default=default, type=type, width=width, height=height,
                      descr=descr),
            ),
            avoid_initial_selection=noselect,
        )
        if row:
            return row['f'].value()
        else:
            return None

    def api_input_text(self, title, label, default=None, not_null=False, width=20, height=1,
                       descr=None, noselect=False):
        return self._input(pd.String(not_null=not_null), title, label, default=default,
                           width=width, height=height, descr=descr, noselect=noselect)

    def api_input_date(self, title, label, default=None, not_null=True, descr=None,
                       noselect=False):
        return self._input(pd.Date(not_null=not_null), title, label or _("Date"),
                           default=default, descr=descr, noselect=noselect)

    def api_input_number(self, title, label, default=None, not_null=True, width=14, precision=None,
                         minimum=None, maximum=None, descr=None, noselect=False):
        if precision:
            quantizer = decimal.Decimal('1.' + '0' * precision)

            def quantize(number):
                if number is not None:
                    return decimal.Decimal(number).quantize(quantizer)
                else:
                    return None

            t = pd.Float(precision=precision, not_null=not_null,
                         minimum=quantize(minimum), maximum=quantize(maximum))
        else:
            t = pd.Integer(not_null=not_null, minimum=minimum, maximum=maximum)
        return self._input(t, title, label, default=default, width=width,
                           descr=descr, noselect=noselect)

    def api_input_form(self, title, fields, prefill=None, layout=None, check=None, noselect=False):
        return run_form(pytis.form.InputForm, title=title, fields=fields,
                        prefill=prefill, layout=layout, check=check or (),
                        avoid_initial_selection=noselect)

    def api_new_record(self, name, prefill=None, inserted_data=None, multi_insert=True,
                       copied_row=None, set_values=None, block_on_new_record=False,
                       spec_kwargs={}, transaction=None):
        view = pytis.config.resolver.get(name, 'view_spec', **spec_kwargs)
        kwargs = dict(prefill=prefill)
        if copied_row and view.on_copy_record():
            on_new_record = view.on_copy_record()
            kwargs['row'] = copied_row
        else:
            on_new_record = view.on_new_record()
        if not block_on_new_record and on_new_record is not None:
            if 'transaction' in argument_names(on_new_record):
                kwargs['transaction'] = transaction
            result = on_new_record(**kwargs)
            if isinstance(result, dict):
                 prefill = result
            else:
                self.api_refresh()
                return result
        if view.arguments() is not None:
            app.echo(_("This form doesn't allow insertion."), kind='error')
            return None
        return run_form(pytis.form.PopupInsertForm, name,
                        prefill=prefill, inserted_data=inserted_data,
                        multi_insert=multi_insert, transaction=transaction,
                        set_values=set_values, spec_kwargs=spec_kwargs)

    def api_edit_record(self, name, row, set_values=None, block_on_edit_record=False,
                        transaction=None):
        view = pytis.config.resolver.get(name, 'view_spec')
        if not isinstance(row, PresentedRow):
            data = pytis.util.data_object(name)
            if not isinstance(row, pd.Row):
                row = data.row(row, transaction=transaction)
            row = PresentedRow(view.fields(), data, row, transaction=transaction)
        on_edit_record = view.on_edit_record()
        if not block_on_edit_record and on_edit_record is not None:
            kwargs = {}
            if 'transaction' in argument_names(on_edit_record):
                kwargs['transaction'] = transaction
            if set_values:
                if 'prefill' in argument_names(on_edit_record):
                    # Backwards compatibility - specifications originally expected set_values
                    # to be passed as prefill to on_edit_record.
                    kwargs['prefill'] = set_values
                else:
                    kwargs['set_values'] = set_values
            return on_edit_record(row, **kwargs)
        else:
            redirect = view.redirect()
            if redirect is not None:
                redirected_name = redirect(row)
                if redirected_name is not None:
                    assert isinstance(redirected_name, basestring)
                    name = redirected_name
            elif view.arguments() is not None:
                app.echo(_(u"This form is read-only."), kind='error')
                return
            key = row.row().columns([c.id() for c in row.data().key()])
            return run_form(pytis.form.PopupEditForm, name, select_row=key,
                            set_values=set_values, transaction=transaction)

    def api_delete_record(self, name, row, question=None, transaction=None):
        view = pytis.config.resolver.get(name, 'view_spec')
        data = pytis.util.data_object(name)
        if not isinstance(row, PresentedRow):
            if not isinstance(row, pd.Row):
                row = data.row(row, transaction=transaction)
            row = PresentedRow(view.fields(), data, row, transaction=transaction)
        on_delete_record = view.on_delete_record()
        ask = True
        if on_delete_record is not None:
            kwargs = {}
            if 'transaction' in argument_names(on_delete_record):
                kwargs['transaction'] = transaction
            result = on_delete_record(row, **kwargs)
            if result is True:
                op, arg = data.delete, tuple(row[c.id()] for c in data.key())

            elif result is False or result is None:
                return False
            elif result == 1:
                return True
            elif isinstance(result, basestring):
                app.error(result)
                return False
            elif isinstance(result, pd.Operator):
                ask = False
                op, arg = data.delete_many, result
            else:
                raise ProgramError("Invalid 'on_delete_record' return value.", result)
        else:
            if data.arguments() is not None:
                app.echo(_("This form doesn't allow deletion."), kind='error')
                return False
            op, arg = data.delete, tuple(row[c.id()] for c in data.key())
        if ask and not app.question(question or
                                    _("Are you sure to delete the record permanently?")):
            return False
        log(EVENT, 'Deleting record:', arg)
        success, result = db_operation(op, arg, transaction=transaction)
        if success:
            log(ACTION, 'Record deleted.')
            return True
        else:
            return False

    def api_run_form(self, name, select_row=None, multi=True, sorting=None, filter=None,
                     condition=None, profile=None, binding=None):
        form_class = pytis.form.BrowseForm
        kwargs = {}
        if '::' in name:
            form_class = pytis.form.BrowseDualForm
        elif multi:
            try:
                specification = pytis.config.resolver.specification(name)
            except ResolverError:
                pass
            else:
                if specification.view_spec().bindings():
                    form_class = pytis.form.MultiBrowseDualForm
                    kwargs = dict(binding=binding)
        return run_form(form_class, name, select_row=select_row, sorting=sorting,
                        filter=filter, condition=condition, profile_id=profile, **kwargs)

    def api_codebook(self, name, select_row=0, columns=None, sorting=None, filter=None,
                     condition=None, multirow=False, begin_search=None, transaction=None):
        if multirow:
            form_class = pytis.form.SelectRowsForm
        else:
            form_class = pytis.form.CodebookForm
        return run_form(form_class, name, select_row=select_row, columns=columns,
                        sorting=sorting, filter=filter, condition=condition,
                        begin_search=begin_search, transaction=transaction)

    def api_run_procedure(self, spec_name, proc_name, *args, **kwargs):
        result = None
        try:
            app.echo(_("Running procedure..."))
            log(ACTION, 'Running procedure:', (spec_name, proc_name, args, kwargs))
            # Kvůli wx.SafeYield() se ztrácí focus, takže
            # si ho uložíme a pak zase obnovíme.
            focused = wx_focused_window()
            wx_yield_()
            proc = pytis.config.resolver.get_object(spec_name, proc_name)
            if kwargs.pop('block_refresh', False):
                with pytis.form.Refreshable.block_refresh():
                    result = proc(*args, **kwargs)
            else:
                result = proc(*args, **kwargs)
            if False:
                # The return value may contain secret data, so we don't log it.
                log(ACTION, u"Procedure return value:", result)
            if focused:
                focused.SetFocus()
        except UserBreakException:
             pass
        except SystemExit:
            raise
        except Exception:
            top_level_exception()
        return result

    def api_web_view(self, title, content):
        run_form(pytis.form.WebForm, title=title, content=content)

    def api_run(self, function, args=(), kwargs={}, over=None, title=None, message=None,
                progress=True, maximum=None, elapsed_time=False, estimated_time=False,
                remaining_time=False, can_abort=False, new_thread=False):
        if over is not None:
            try:
                count = len(over)
            except TypeError:
                assert maximum is not None, \
                    "Pass item count as 'maximum' when 'over' is of unknown length."
                count = maximum
            # We use maximum 100 rather than the actual item count, because wx.ProgressBar
            # doesn't give sensible results for small maximum/progress values.
            maximum = 100
            func = function
            def function(update, *args, **kwargs):
                pass_n = ('n' in argument_names(func) and 'n' not in kwargs)
                for n, arg in enumerate(over):
                    if not update(progress=max(1, min(maximum, n / count * 100))):
                        break
                    if pass_n:
                        kwargs['n'] = n
                    func(update, arg, *args, **kwargs)
        if progress:
            dlg = dialog.ProgressDialog
            dialog_kwargs = dict(
                maximum=maximum if maximum is not None else 100,
                elapsed_time=elapsed_time,
                estimated_time=estimated_time,
                remaining_time=remaining_time,
                can_abort=can_abort,
                new_thread=new_thread,
            )
        else:
            assert not any((elapsed_time, estimated_time, remaining_time, can_abort))
            dlg = dialog.OperationDialog
            dialog_kwargs = dict()
        return self.run_dialog(
            dlg, function, args, kwargs,
            title=title or _("Operation in progress"),
            message=message or _("Please wait..."),
            **dialog_kwargs
        )

    def api_launch_file(self, path=None, data=None, suffix=None, decrypt=False):
        if path:
            assert data is None and suffix is None
        else:
            assert data is not None and suffix is not None
        cmode = self.client_mode()
        if cmode == 'remote':
            if path:
                source_file = open(path, 'rb')
                close_source_file = True
                suffix = os.path.splitext(path)[1]
            elif hasattr(data, 'read'):
                source_file = data
                close_source_file = False  # The calling side is supposed to close its file.
            else:
                source_file = io.BytesIO(data)
                close_source_file = False
            try:
                remote_file = pytis.remote.make_temporary_file(suffix=suffix, decrypt=decrypt)
            except Exception as e:
                log(OPERATIONAL, "Can't create remote temporary file:", str(e))
                app.error(_("Unable to create temporary file: %s", e))
            try:
                while True:
                    data = source_file.read(10 * 1024 * 1024)
                    if not data:
                        break
                    remote_file.write(data)
                log(OPERATIONAL, "Launching remote file viewer on %s:" % pytis.remote.client_ip(),
                    remote_file.name)
                pytis.remote.launch_file(remote_file.name)
            finally:
                if close_source_file:
                    source_file.close()
                remote_file.close()
        elif cmode == 'local':
            if path:
                import mimetypes
                import subprocess
                mime_type = mimetypes.guess_type(path)[0]
                if mime_type == 'application/pdf' and pytis.config.postscript_viewer:
                    # Open a local PDF viewer for a PDF file if a specific PDF viewer is configured.
                    command = (pytis.config.postscript_viewer, path)
                    shell = False
                elif mime_type:
                    # Find the viewer through mailcap.
                    import mailcap
                    match = mailcap.findmatch(mailcap.getcaps(), mime_type)[1]
                    if match:
                        command = match['view'] % (path,)
                        shell = True
                    else:
                        app.error(_("Viewer for '%s' (%s) not found.",
                                    path, mime_type or 'unknown'))
                        return
                log(OPERATIONAL, "Launching local file viewer:", command)
                proc = subprocess.Popen(command, shell=shell)
                proc.wait()
            else:
                with tempfile.NamedTemporaryFile(suffix=suffix) as f:
                    if hasattr(data, 'read'):
                        while True:
                            chunk = data.read(10 * 1024 * 1024)
                            if not chunk:
                                break
                            f.write(chunk)
                    else:
                        f.write(data)
                    f.flush()
                    # Call ourselves with temp file path.
                    self.api_launch_file(path=f.name)
                    # Give the viewer some time to read the file as it will be
                    # removed when the "with" statement is left.
                    time.sleep(1)

    def api_launch_url(self, url):
        cmode = self.client_mode()
        if cmode == 'remote':
            pytis.remote.launch_url(url)
        elif cmode == 'local':
            import webbrowser
            webbrowser.open(url)

    def api_splitpath(self, path):
        # Well, rely on self.client_mode() to return the same value as
        # for which the path was originally created.
        return self._splitpath(self.client_mode(), path)

    def api_select_file(self, filename=None, filetypes=None, directory=None, context='default'):
        cmode = self.client_mode()
        if directory is None:
            directory = self._get_recent_directory(cmode, context)
        else:
            context = None  # Prevent storing the explicitly passed directory when dialog closed.
        if cmode == 'remote':
            path = pytis.remote.select_file(filename=filename, directory=directory,
                                            filetypes=filetypes, multi=False)
        elif cmode == 'local':
            path = self.run_dialog(dialog.FileDialog, file=filename, dir=directory,
                                   mode=dialog.FileDialog.OPEN, multi=False,
                                   wildcards=self._wildcards(filetypes))
        else:
            path = None
        if path and context:
            self._set_recent_directory(cmode, context, self._dirname(cmode, path))
        return path

    def api_select_files(self, directory=None, filetypes=None, context='default'):
        # TODO: directory is ignored in the remote variant.
        cmode = self.client_mode()
        if directory is None:
            directory = self._get_recent_directory(cmode, context)
        else:
            context = None  # Prevent storing the explicitly passed directory when dialog closed.
        if cmode == 'remote':
            paths = pytis.remote.select_file(directory=directory, filetypes=filetypes, multi=True)
        elif cmode == 'local':
            paths = self.run_dialog(dialog.FileDialog, dir=directory,
                                    mode=dialog.FileDialog.OPEN, multi=True,
                                    wildcards=self._wildcards(filetypes))
        else:
            paths = None
        if paths and context:
            self._set_recent_directory(cmode, context, self._dirname(cmode, paths[0]))
        return paths

    def api_select_directory(self, directory=None, context='default'):
        cmode = self.client_mode()
        if directory is None:
            directory = self._get_recent_directory(cmode, context)
        else:
            context = None  # Prevent storing the explicitly passed directory when dialog closed.
        if cmode == 'remote':
            path = pytis.remote.select_directory(directory=directory)
        elif cmode == 'local':
            path = self.run_dialog(dialog.DirDialog, path=directory)
        else:
            path = None
        if path and context:
            self._set_recent_directory(cmode, context, path)
        return path

    def api_make_selected_file(self, filename, mode='w', encoding=None, filetypes=None,
                               directory=None, context='default'):
        cmode = self.client_mode()
        if directory is None:
            directory = self._get_recent_directory(cmode, context)
        else:
            context = None  # Prevent storing the explicitly passed directory when dialog closed.
        if encoding is None and 'b' not in mode:
            # Supply default encoding only for text modes.
            encoding = 'utf-8'
        if cmode == 'remote':
            f = self._wrap_exposed_file_wrapper(pytis.remote.make_selected_file(
                filename=filename, directory=directory, mode=mode, encoding=encoding,
                filetypes=filetypes,
            ))
        elif cmode == 'local':
            path = self.run_dialog(dialog.FileDialog, file=filename, dir=directory,
                                   mode=dialog.FileDialog.SAVE,
                                   wildcards=self._wildcards(filetypes))
            f = open(path, mode) if path else None
        else:
            f = None
        if f and context:
            self._set_recent_directory(cmode, context, self._dirname(cmode, f.name))
        return f

    def api_write_selected_file(self, data, filename, mode='w', encoding=None, filetypes=None,
                                context='default'):
        f = self.api_make_selected_file(filename=filename, mode=mode, encoding=encoding,
                                        filetypes=filetypes, context=context)
        if f:
            if sys.version_info[0] == 2 and isinstance(data, bytes):
                # TODO: The older version of P2Go's pytisproc.py currently distributed
                # between users doesn't handle text encoding quite well.  It attempts
                # to encode everything which is not a 'buffer'.  We can remove this
                # hack once all clients have a newer P2Go version.  As this doesn't
                # even work in Python 3 (which doesn't have buffer) old clients will
                # not work with Python3 at all.
                data = buffer(data)
            try:
                f.write(data)
            finally:
                f.close()
            return True
        else:
            return False

    def api_open_selected_file(self, directory=None, filetypes=None, encrypt=None,
                               context='default'):
        # TODO: Encryption not supported for the local variant.
        cmode = self.client_mode()
        if directory is None:
            directory = self._get_recent_directory(cmode, context)
        else:
            context = None  # Prevent storing the explicitly passed directory when dialog closed.
        if cmode == 'remote':
            f = self._wrap_exposed_file_wrapper(pytis.remote.open_selected_file(
                directory=directory, encrypt=encrypt,
                filetypes=filetypes
            ))
        elif cmode == 'local':
            path = self.run_dialog(dialog.FileDialog, dir=directory,
                                   mode=dialog.FileDialog.OPEN,
                                   wildcards=self._wildcards(filetypes))
            f = open(path, 'rb') if path else None
        else:
            f = None
        if f and context:
            self._set_recent_directory(cmode, context, self._dirname(cmode, f.name))
        return f

    def api_open_file(self, filename, mode='w'):
        cmode = self.client_mode()
        if cmode == 'remote':
            f = pytis.remote.open_file(filename, mode=mode)
        elif cmode == 'local':
            f = open(filename, mode)
        return f

    def api_write_file(self, data, filename, mode='w'):
        if isinstance(data, bytes):
            # Maybe the same problem as described in write_selected_file() may apply
            # here?  Morover RPyC doesn't seem to pass pd.Binary.Data correctly
            # and leads to "TypeError: argument 1 must be convertible to a buffer,
            # not Data" on remote write attempt.  See issue #2.
            data = buffer(data)
        f = self.api_open_file(filename, mode=mode)
        try:
            f.write(data)
        finally:
            f.close()

    def api_decrypted_areas(self):
        return self._decrypted_areas

    class _PrintResolver(pytis.output.OutputResolver):
        """Default print resolver used internally by 'api_printout()'."""

        class _Spec(object):
            # This class has to emulate a specification module as well as a
            # (new style) specification class.

            def body(self, resolver=None, **kwargs):
                app.error(_("Print specification not found!"))

            def doc_header(self, resolver=None, **kwargs):
                return None

            def doc_footer(self, resolver=None, **kwargs):
                return None

        def _get_module(self, name):
            # Supply a default specification module (old style spec).
            try:
                return super(Application._PrintResolver, self)._get_module(name)
            except pytis.util.ResolverError:
                return self._Spec()

        def _get_instance(self, key):
            # Supply a default specification class (new style spec).
            try:
                return super(Application._PrintResolver, self)._get_instance(key)
            except pytis.util.ResolverError:
                return self._Spec()

    def api_printout(self, spec_name, template_id, row=None,
                     parameters=None, output_file=None, language=None, form=None):
        if parameters is None:
            parameters = {}
        parameters[pytis.output.P_NAME] = spec_name
        parameters[spec_name + '/' + pytis.output.P_ROW] = row
        resolvers = (
            pytis.output.DatabaseResolver('ev_pytis_user_output_templates',
                                          ('template', 'rowtemplate', 'header',
                                           'first_page_header', 'footer', 'style'),
                                          ('body', 'row', 'page_header',
                                           'first_page_header', 'page_footer', 'style')),
            self._PrintResolver(pytis.config.print_spec_dir, pytis.config.resolver),
        )
        try:
            formatter = pytis.output.Formatter(pytis.config.resolver, resolvers, template_id,
                                               form=form, parameters=parameters,
                                               language=language or pytis.util.current_language(),
                                               translations=pytis.util.translation_path())
        except pytis.output.AbortOutput as e:
            log(OPERATIONAL, str(e))
            return

        def run_viewer(filename):
            try:
                app.launch_file(filename)
            finally:
                try:
                    os.remove(filename)
                except OSError as e:
                    log(OPERATIONAL, 'Error removing temporary file:', e)

        def do_printout(outfile):
            try:
                formatter.printout(outfile)
            except lcg.SubstitutionIterator.NotStartedError:
                tbstring = pytis.util.format_traceback()
                log(OPERATIONAL, 'Print exception caught', tbstring)
                app.error(_("Invalid use of identifier `data' in print specification.\n"
                            "Maybe use `current_row' instead?"))
                raise UserBreakException()

        try:
            if output_file:
                do_printout(output_file)
            else:
                # TODO: Use app.launch_file() here?
                with tempfile.NamedTemporaryFile(suffix='.pdf',
                                                 prefix='tmppytis', delete=False) as f:
                    do_printout(f)
                threading.Thread(target=run_viewer, args=(f.name,)).start()
            formatter.cleanup()
        except UserBreakException:
            pass


class DbActionLogger(object):
    """Log user actions into the database."""

    def __init__(self, dbconnection, username):
        factory = pytis.config.resolver.get('pytis.defs.logging.FormActionLog', 'data_spec')
        self._data = factory.create(connection_data=pytis.config.dbconnection)
        self._username = username

    def _values(self, **kwargs):
        return [(key, pd.Value(self._data.find_column(key).type(), value))
                for key, value in [('username', self._username)] + list(kwargs.items())]

    def log(self, spec_name, form_name, action, info=None):
        rdata = (('timestamp', pd.dtval(pd.DateTime.datetime())),
                 ('username', pd.sval(self._username)),
                 ('spec_name', pd.sval(spec_name)),
                 ('form_name', pd.sval(form_name)),
                 ('action', pd.sval(action)),
                 ('info', pd.sval(info)))
        row = pd.Row(rdata)
        result, success = self._data.insert(row)
        if not success:
            raise pd.DBException(result)


# Funkce odpovídající příkazům aplikace.

def run_form(form_class, name=None, **kwargs):
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
        app.echo(_("Opening form refused."), kind='error')
        return False
    return cmd.invoke(**kwargs)

def db_operation(operation, *args, **kwargs):
    """Invoke database operation with handling possible DB errors.

    'operation' is called with given arguments.  If 'pd.dbdata.DBException' is
    raised during the operation, an error dialog is displayed with exception
    description and a question asking whether the user wishes to re-invoke the
    operation.  The operation is repeated as long as the user answers the
    question positively.

    The exceptions of type 'DBLoginException' result in displaying a login
    dialog and the supplied username and password is set before repeating the
    operation.

    When the operation is performed successfully (regardles whether on the
    first try or later), its result is returned.

    Arguments:
      operation -- function (callable object) performing a database operation
        and returning its result.
      args, kwargs -- arguments and keyword arguments passed to the function,
        excluding the keyword arguments named below.
      allow_retry -- iff true, offer the user a chance for restoring the
        operation.  It is on by default when 'kwargs' contain a 'translation'.
        Off othervise.
      quiet -- iff true, don't report errors to the user at all (regardless
        of 'allow_retry').

    Returns: Pair (SUCCESS, RESULT), where SUCCESS is a boolean flag indicating
    success (true) or failure (false) and RESULT is the value returned by
    'operation' (if SUCCESS is false, RESULT is not defined).

    """
    # Don't offer the user a chance for restoring the operation when inside transaction.
    allow_retry = kwargs.pop('allow_retry', kwargs.get('transaction') is None)
    quiet = kwargs.pop('quiet', False)
    FAILURE = False, None
    while True:
        try:
            result = operation(*args, **kwargs)
            if pytis.form.app:
                if pytis.form.app._log_login:
                    log(ACTION, "Login action:", (pytis.config.dbschemas, 'True'))
                    pytis.form.app._log_login = False
                pytis.form.app.login_hook(success=True)
            return True, result
        except pd.DataAccessException:
            app.error(_("Access denied"))
            return FAILURE
        except pd.DBLoginException:
            if pytis.config.dbconnection.password() is not None and pytis.form.app:
                log(ACTION, "Login action:", (pytis.config.dbschemas, 'False'))
                pytis.form.app.login_hook(success=False)
            if pytis.config.login_selection:
                logins = [x[0] if isinstance(x, tuple) else x
                          for x in pytis.config.login_selection]
                passwords = dict([x for x in pytis.config.login_selection
                                  if isinstance(x, tuple)])
                login_enumerator = pd.FixedEnumerator(logins)
                password_computer = computer(lambda r, login: passwords.get(login))
                password_editable = computer(lambda r, login: login not in passwords)
                default_login = pytis.config.dbuser if pytis.config.dbuser in logins else None
                default_password = None
            else:
                login_enumerator = None
                password_computer = None
                password_editable = None
                default_login = pytis.config.dbuser
                if pytis.remote.client_available():
                    default_password = pytis.remote.session_password()
                else:
                    default_password = None

            def check_login(row):
                try:
                    pytis.config.dbconnection.update_login_data(
                        user=row['login'].value(),
                        password=row['password'].value()
                    )
                except pd.DBLoginException as e:
                    app.echo(e.message())
                    return 'password'
                else:
                    return None

            if not run_form(
                    pytis.form.InputForm, title=_("Log in for database access"),
                    fields=(Field('login', _("Login"), width=24, not_null=True,
                                  enumerator=login_enumerator, default=default_login),
                            Field('password', _("Password"), type=pd.Password(verify=False),
                                  editable=password_editable, computer=password_computer,
                                  default=default_password, width=24, not_null=True),),
                    focus_field='password',
                    check=check_login,
            ):
                return FAILURE
            pytis.config.dbconnection = pytis.config.dbconnection  # mark as changed
            pytis.config.dbuser = pytis.config.dbconnection.user()
        except pd.DBException as e:
            log(OPERATIONAL, "Database exception in db_operation", format_traceback())
            message = e.message()
            if e.exception():
                message += '\n' + str(e.exception())
            if quiet:
                return FAILURE
            if not allow_retry:
                app.error(message, title=_("Database error"))
                return FAILURE
            elif not app.question(message + '\n' + _("Try again?"), title=_("Database error")):
                return FAILURE


# Funkce, které jsou obrazem veřejných metod aktuální aplikace.


def run_dialog(arg1, *args, **kwargs):
    """Zobraz dialog v okně aplikace (viz 'Application.run_dialog()')."""
    if pytis.form.app is None:
        log(OPERATIONAL, "Attempt to run a dialog:", (arg1, args, kwargs))
    elif arg1 == InputDialog:
        return app.input_text(title=kwargs.get('message'),
                              label=kwargs.get('prompt', '').rstrip(':'),
                              default=kwargs.get('value'),
                              width=kwargs.get('input_width'),
                              height=kwargs.get('input_height'))
    elif arg1 == InputNumeric:
        precision = kwargs.get('decimal_width', 0)
        minimum = kwargs.get('min_value')
        maximum = kwargs.get('max_value')
        if precision:
            t = pd.Float(precision=precision)
            cast = float
        else:
            t = pd.Integer()
            cast = int
        value = app.input_number(title=kwargs.get('message'),
                                 label=kwargs.get('prompt', '').rstrip(':'),
                                 width=kwargs.get('integer_width', 10) + precision + 1,
                                 precision=precision,
                                 minimum=cast(minimum) if minimum else None,
                                 maximum=cast(maximum) if maximum else None,
                                 noselect=not kwargs.get('select_on_entry', False),
                                 default=kwargs.get('value'))
        return pd.Value(t, value)

    elif arg1 == InputDate:
        # Backwards compatibility hack.
        value = kwargs.get('value')
        if value:
            value, error = pd.Date().validate(value)
            if value:
                value = value.value()
        value = app.input_date(title=kwargs.get('message'),
                               label=kwargs.get('prompt', '').rstrip(':'),
                               default=value)
        return pd.Value(pd.Date(), value)
    else:
        return pytis.form.app.run_dialog(arg1, *args, **kwargs)


def recent_forms_menu():
    """Vrať menu posledně otevřených formulářů jako instanci 'pytis.presentation.Menu'.

    Tato funkce je určena pro využití při definici menu aplikace.  Pokud menu posledně otevřených
    formulářů tímto způsobem do hlavního menu aplikace přidáme, bude jej aplikace dále
    obhospodařovat.  Toto menu lze do hlavního menu umístit pouze jednou.

    """
    if pytis.form.app:
        return pytis.form.app.recent_forms_menu()
    else:
        # This may happen when generating help.
        return Menu(_("Recently opened forms"), ())


def wx_frame():
    """Vrať instanci 'wx.Frame' hlavního okna aplikace."""
    return pytis.form.app.wx_frame()

def close_forms():
    """Close all currently opened forms."""
    return pytis.form.app._close_forms()


# Ostatní funkce.


def global_keymap():
    """Vrať klávesovou mapu aplikace jako instanci třídy 'Keymap'."""
    try:
        return pytis.form.app.keymap
    except AttributeError:
        return Keymap()


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
        _yield_lock = _thread.allocate_lock()
    if not _yield_lock.acquire(0):
        return
    try:
        if full:
            if pytis.form.app is not None:
                pytis.form.app.Yield()
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


# Deprecated backwards compatibility aliases.

def refresh():
    app.refresh()

def exit():
    return app.exit()

def _file_selection_kwargs(pattern=None, patterns=(), filetypes=None, **kwargs):
    # Backwards compatibility treatment of pattern/patterns.
    if patterns or pattern:
        assert filetypes is None, filetypes
        if pattern:
            patterns = tuple(patterns) + (('', pattern),)
        filetypes = tuple(functools.reduce(
            lambda a, b: a + tuple(x.split('.')[-1] for x in b[-1].split(';')), patterns, (),
        ))
    return dict(kwargs, filetypes=filetypes)

def launch_url(*args, **kwargs):
    return app.launch_url(*args, **kwargs)

def select_file(*args, **kwargs):
    return app.select_file(*args, **_file_selection_kwargs(**kwargs))

def select_files(*args, **kwargs):
    return app.select_files(*args, **_file_selection_kwargs(**kwargs))

def select_directory(*args, **kwargs):
    return app.select_directory(*args, **kwargs)

def make_selected_file(*args, **kwargs):
    return app.make_selected_file(*args, **_file_selection_kwargs(**kwargs))

def write_selected_file(*args, **kwargs):
    return app.write_selected_file(*args, **_file_selection_kwargs(**kwargs))

def open_selected_file(*args, **kwargs):
    f = app.open_selected_file(*args, **_file_selection_kwargs(**kwargs))
    if f:
        filename = app.splitpath(f.name)[1]
    else:
        filename = None
    return f, filename

def open_file(*args, **kwargs):
    return app.open_file(*args, **kwargs)

def write_file(*args, **kwargs):
    return app.write_file(*args, **kwargs)

def launch_file(path):
    return app.launch_file(path)

def open_data_as_file(data, suffix, decrypt=False):
    return app.launch_file(data=data, suffix=suffix, decrypt=decrypt)

def has_access(name, perm=pd.Permission.VIEW, column=None):
    return app.has_access(name, perm=perm, column=column)

def run_procedure(spec_name, proc_name, *args, **kwargs):
    if 'block_refresh_' in kwargs:
        kwargs['block_refresh'] = kwargs.pop('block_refresh_')
    return app.run_procedure(spec_name, proc_name, *args, **kwargs)

def new_record(name, prefill=None, inserted_data=None, **kwargs):
    return app.new_record(name, prefill=prefill, inserted_data=inserted_data, **kwargs)

def delete_record(view, data, transaction, record,
                  question=_("Are you sure to delete the record permanently?")):
    assert pytis.config.resolver.get(view.spec_name(), 'view_spec') is view
    return app.delete_record(view.spec_name(), tuple(record[k.id()] for k in data.key()),
                             question=question, transaction=transaction)


def printout(spec_name, template_id, **kwargs):
    return app.printout(spec_name, template_id, **kwargs)

def message(message, beep_=False):
    return app.echo(message, kind='error' if beep_ else 'info')

def decrypted_names():
    return app.decrypted_areas()

def IN(*args, **kwargs):
    return make_in_operator(*args, **kwargs)

def refresh_status(id):
    return app.status(id).refresh()

def set_status(id, text, **kwargs):
    if __debug__:
        log(DEBUG, u"StatusBar field updated:", (id, text, kwargs))
    return  app.status(id).update(text, **kwargs)

def current_form(**kwargs):
    if pytis.form.app is not None:
        return pytis.form.app.current_form(**kwargs)

def top_window(**kwargs):
    if pytis.form.app is not None:
        return pytis.form.app.top_window(**kwargs)

def frame_title(title):
    app.title = title

def delete_record_question(msg=None):
    return app.delete_record_question(msg)

def built_in_status_fields():
    return pytis.form.app.status_fields()

class InputDialog(object):
    pass

class InputNumeric(object):
    pass

class InputDate(object):
    pass

from pytis.presentation import (
    Menu, MenuItem as MItem, MenuSeparator as MSeparator,
    MenuItem as CheckItem, MenuItem as RadioItem,
)
