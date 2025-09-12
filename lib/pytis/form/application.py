# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Implementation of the wx.Widgets application.

The class 'Application' handles creation of the main application window and
manages it during its lifetime.  It also implements the public API of
'pytis.api.Application' and other public methods used internally throughout
'pytis.form'.

"""

from __future__ import print_function
from past.builtins import basestring
from builtins import range

import copy
import decimal
import gi
import io
import lcg
import os.path
import sys
import _thread
import threading
import tempfile
import time
import wx
import wx.html

import pytis
import pytis.api
import pytis.data as pd
import pytis.form
import pytis.remote
import pytis.util

from pytis.api import app
from pytis.presentation import (
    Field, StatusField, computer, Text, TextFormat, PresentedRow,
    Menu, MenuItem, MenuSeparator, Command
)
from pytis.util import (
    ACTION, DEBUG, EVENT, OPERATIONAL, ProgramError, ResolverError,
    argument_names, find, format_traceback, identity, log, rsa_encrypt
)
from .command import CommandHandler
from .event import (
    UserBreakException, interrupt_init, interrupt_watcher,
    top_level_exception, unlock_callbacks, wx_callback, yield_,
)
from .screen import (
    Browser, KeyHandler, Keymap, StatusBar,
    acceskey_prefix, beep, busy_cursor, get_icon, uicommand_mitem,
    wx_focused_window, hotkey_string, wx_toolbar, command_icon,
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
    _STATE_ACTIVE_FORM = 'active_form'

    class _StatusFieldAccess(object):
        def __init__(self, fields):
            self._fields = fields

        def __getattr__(self, name):
            f = find(name, self._fields, key=lambda f: f.spec.id().replace('-', '_'))
            if f:
                return f.provider()
            else:
                raise AttributeError("StatusBar has no field '{}'".format(name))

        def __dir__(self):
            return [f.spec.id() for f in self._fields]

        def __call__(self, name):
            return self.__getattr__(name.replace('-', '_'))

    @classmethod
    def command_handler_instance(cls):
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
        self._remote_connection_info = None
        self._menu_by_id = {}
        self._yield_lock = None
        self._yield_blocked = False
        self._last_echo = None
        self._previously_active_tab_index = None
        self._initialized = False
        self._in_cleanup = False
        self.keymap = Keymap()
        super(Application, self).__init__()

    def OnInit(self):
        import pytis.extensions
        pytis.remote.write_python_version()
        wx.Log.SetActiveTarget(wx.LogStderr())

        # Make sure menu icons are always displayed.  Without this, the icons are
        # not displayed under certain contitions (sometimes within an x2go session,
        # sometimes on certain installations).
        gi.require_version('Gtk', '3.0')
        from gi.repository import Gtk as gtk
        settings = gtk.Settings.get_default()
        settings.set_property('gtk-menu-images', True)

        # Create the main application frame (set frame title later on).
        frame = self._frame = wx.Frame(None, -1, '', style=wx.DEFAULT_FRAME_STYLE)
        wx_callback(wx.EVT_CLOSE, frame, self._on_frame_close)
        wx_callback(wx.EVT_SIZE, frame, self._on_frame_size)
        frame.SetSizer(wx.BoxSizer(wx.VERTICAL))
        frame.Sizer.Fit(frame)
        wx.ToolTip('').Enable(pytis.config.show_tooltips)
        icons = wx.IconBundle()
        for name in ('pytis', 'pytis-mini'):
            icon_bmp = get_icon(name)
            if icon_bmp:
                icon = wx.Icon()
                icon.CopyFromBitmap(icon_bmp)
                icons.AddIcon(icon)
        frame.SetIcons(icons)
        self._notebook = nb = wx.aui.AuiNotebook(frame, style=(wx.aui.AUI_NB_TOP |
                                                               wx.aui.AUI_NB_TAB_MOVE |
                                                               wx.aui.AUI_NB_SCROLL_BUTTONS |
                                                               wx.aui.AUI_NB_TAB_MOVE |
                                                               wx.aui.AUI_NB_WINDOWLIST_BUTTON))

        nb.TabCtrlHeight = 36
        frame.Sizer.Add(nb, 1, wx.EXPAND)
        wx_callback(wx.aui.EVT_AUINOTEBOOK_PAGE_CHANGED, nb, self._on_page_change)
        wx_callback(wx.aui.EVT_AUINOTEBOOK_TAB_RIGHT_DOWN, nb, self._on_tab_mouse_right)
        # This panel is solely for catching keyboard events (it can't be done
        # with the frame or the notebook).
        self._panel = wx.Panel(frame)
        KeyHandler.__init__(self, self._panel)
        self._modals = []
        self._help_browser = None
        self._login_success = False
        for key, command in pytis.form.DEFAULT_KEYMAP + self._specification.keymap():
            self.keymap.define_key(key, command)
        pytis.form.app = self
        app.title = pytis.config.application_name

        # Initialize login and password.
        success, result = db_operation(pd.dbtable, 'pg_catalog.pg_tables', ('tablename',))
        if not success:
            return False
        # Unlock crypto keys.
        self._unlock_crypto_keys()

        # Build up the status bar.
        if not self._headless:
            sb = StatusBar(frame, list(self._specification.status_fields()))
            self._status_fields = self._StatusFieldAccess(sb.fields)

        import pytis.defs.configui
        configurable_options = tuple(f.id() for f in pytis.defs.configui.FIELDS)
        self._initial_config = [
            (o, copy.copy(getattr(pytis.config, o)))
            for o in configurable_options + ('initial_keyboard_layout',)]
        self._saved_state = {}
        # Initialize all needed user settings managers.
        self._application_config_manager = pytis.form.ApplicationConfigManager(
            pytis.config.dbconnection)
        self._form_settings_manager = pytis.form.FormSettingsManager(pytis.config.dbconnection)
        self._form_profile_manager = pytis.form.FormProfileManager(pytis.config.dbconnection)
        self._aggregated_views_manager = pytis.form.AggregatedViewsManager(
            pytis.config.dbconnection)
        # Initialize user action logger.
        from pytis.defs.logging import UserActionLog
        self._user_action_logger = UserActionLog.Logger(pytis.config.dbconnection,
                                                        pytis.config.dbuser)
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
        # Initialize the toolbar.
        self._toolbar = wx_toolbar(frame, pytis.form.TOOLBAR_COMMANDS)
        # Set the main application frame size and position.
        frame_sizes = self._get_state_param(self._STATE_FRAME_SIZE, [], list, list)
        current_display_size = list(wx.DisplaySize())
        for display_size, position, size in frame_sizes:
            if display_size == current_display_size:
                frame.SetSize(size)
                frame.SetPosition(position)
                break
        else:
            frame.SetSize((1000, 800))
        # Finish and show the frame.
        self.SetTopWindow(frame)
        if not self._headless:
            frame.Show(True)

        def init():
            try:
                self._init()
            except Exception:
                top_level_exception()
        wx.CallAfter(init)
        self._initialized = True
        return True

    def _cache_menu_enabled(self, menu):
        # Cache the specification instances needed to compute the availability
        # of menu items.  This reduces the lag during the first user's attempt
        # to open a menu.
        for item in menu:
            if isinstance(item, Menu):
                self._cache_menu_enabled(list(item.items))
            elif not isinstance(item, MenuSeparator):
                enabled = item.command.enabled
                if __debug__:
                    if pytis.config.debug:
                        log(DEBUG, 'Menu item:', (item.title, enabled))

    def _spec_title(self, name):
        if name.find('::') != -1:
            names = name.split('::')
            bindings = pytis.config.resolver.get(names[0], 'binding_spec')
            return bindings[names[1]].title() or ' / '.join([self._spec_title(n) for n in names])
        else:
            return pytis.config.resolver.get(name, 'view_spec').title()

    def _init(self):
        # Run application specific initialization.
        self._specification.init()
        if self._notebook.PageCount == 0:
            self._panel.SetFocus()
        # Open the startup forms passed as a command line argument.
        startup_forms = []
        if pytis.config.startup_forms:
            for name in pytis.config.startup_forms.split(','):
                if name.find('/') != -1:
                    cls_name, name = [n.strip() for n in name.split('/')]
                    try:
                        cls = getattr(pytis.form, cls_name)
                        if not issubclass(cls, pytis.form.Form):
                            raise AttributeError
                    except AttributeError:
                        app.error(_("Invalid form class in startup forms:") + ' ' + cls_name)
                        continue
                else:
                    name = name.strip()
                    if self._has_bindings(name):
                        cls = pytis.form.MultiBrowseDualForm
                    else:
                        cls = pytis.form.BrowseForm
                startup_forms.append((cls, name))

        # Add the forms saved on last exit to startup forms.
        saved_forms = []
        menu_names = pytis.extensions.get_menu_defs()
        for cls, name in self._get_state_param(self._STATE_STARTUP_FORMS, [], list, list):
            # TODO: This probably ramained after legacy support for configurantion
            # saved in picked objects.  Now with JSON, we will always get a string.
            if isinstance(cls, basestring):
                cls = getattr(pytis.form, cls)
            if ((issubclass(cls, pytis.form.Form) and
                 # The check aginst menu_names was added probably to avoid reopening forms
                 # which were opened by application code.  These forms may have special
                 # parameters, such as filters, which would not be recovered this way,
                 # so it looks safe to open only forms which are available through the
                 # application menu.
                 self._is_valid_spec(name) and name in menu_names and
                 (cls, name) not in startup_forms)):
                saved_forms.append((cls, name))
            else:
                log(OPERATIONAL, "Ignoring saved startup form:", (cls, name))
        if saved_forms and not self._headless:
            if not pytis.config.autostart_saved_forms:
                checked = self.run_dialog(
                    dialog.CheckListDialog,
                    title=_("Restore forms"),
                    message=_("Restore these forms saved on last exit?"),
                    items=[(True, '%s (%s)' % (self._spec_title(name), f.descr()))
                           for f, name in saved_forms],
                    icon=dialog.CheckListDialog.ICON_QUESTION,
                )
                saved_forms = [x for x, ch in zip(saved_forms, checked or []) if ch]
            startup_forms[:0] = saved_forms

        def run_startup_form(update, item):
            cls, name = item
            update(message=_("Opening form:") + " " + name)
            try:
                run_form(cls, name, activate=False)
            except Exception as e:
                log(OPERATIONAL, "Unable to init startup form:", (cls, name, e))
        app.run(run_startup_form, over=startup_forms, title=_("Opening saved forms"))
        # Activate the saved active form from the last run.
        initially_active_form_index = self._notebook.PageCount - 1
        last_active_form = self._get_state_param(self._STATE_ACTIVE_FORM, None, (list, type(None)))
        if last_active_form:
            for i, f in enumerate(self._forms()):
                if [f.__class__.__name__, f.name()] == last_active_form:
                    initially_active_form_index = i
                    break
        if initially_active_form_index >= 0:
            self._notebook.SetSelection(initially_active_form_index)

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

    def _has_bindings(self, name):
        try:
            specification = pytis.config.resolver.specification(name)
        except ResolverError:
            return False
        else:
            if specification.view_spec().bindings():
                return True
            else:
                return False

    def _create_menubar(self):
        # TODO: Temporary backwards compatilility hack.  Rely solely on self._specification.menu()
        # as soon as all DMP applications define menu by calling pytis.extensions.dmp_menu().
        import pytis.extensions
        try:
            menu = pytis.extensions.dmp_menu()
            if not menu:
                menu = list(self._specification.menu())
        except pd.DBException:
            menu = list(self._specification.menu())
        self._menu = menu

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
        for item in menu:
            # Determining availability of menu items may invoke database operations...
            success, result = db_operation(self._create_menu, self._frame, item, self.keymap)
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

        # At first, compute the maximal width of hotkey string in this menu.
        max_hotkey_width = 0
        hotkey_strings = {}
        for item in spec.items:
            if isinstance(item, MenuItem):
                hotkey = item.hotkey
                if hotkey and keymap:
                    command = lookup_key(keymap, hotkey)
                    if command and command != item.command:
                        log(OPERATIONAL, "Duplicate hotkey %s on menu item '%s': "
                            "%s collides with %s defined elsewhere." %
                            (hotkey, item.title, item.command, command))
                if not hotkey and keymap:
                    hotkey = keymap.lookup_command(item.command)
                elif keymap:
                    keymap.define_key(hotkey, item.command)
                if hotkey:
                    hotkey_strings[item] = string = '    ' + hotkey_string(hotkey)
                    hotkey_width = parent.GetTextExtent(string)[0]
                    max_hotkey_width = max(hotkey_width, max_hotkey_width)

        # Now create the items and remember max. width of whole item label.
        menu = wx.Menu()
        wx_callback(wx.EVT_MENU_HIGHLIGHT_ALL, menu, on_highlight_item)
        if spec.id:
            self._menu_by_id[spec.id] = menu
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
                    mitem = self._append_menu_item(parent, menu, item, title)
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

    def _append_menu_item(self, parent, menu, item, title=None):
        def on_ui_event(event):
            event.Enable(item.command.enabled)
            if item.state:
                state = item.state()
                event.Check(state)

        def on_invoke_command(event):
            # Invoke the command through CallAfter to finish menu event processing before
            # command invocation.  Calling dirrectly for example resulted in disfunctional
            # TAB traversal in a popup form which was opended through a popup menu command
            # due to FindFocus() returning a wrong window.  Using CallAfter fixes this.
            wx.CallAfter(item.command.invoke)

        if item.state:
            # kind = wx.ITEM_RADIO
            # wx.ITEM_RADIO causes SEGFAULT.  wx.ITEM_CHECK, however,
            # seems to have the same behavior...
            kind = wx.ITEM_CHECK
            icon = None
        else:
            kind = wx.ITEM_NORMAL
            icon = get_icon(item.icon or command_icon(item.command))
        mitem = wx.MenuItem(menu, -1, title or item.title, item.help or "", kind=kind)
        wx_callback(wx.EVT_MENU, parent, on_invoke_command, source=mitem)
        wx_callback(wx.EVT_UPDATE_UI, parent, on_ui_event, source=mitem)
        if icon:
            mitem.SetBitmap(icon)
        menu.Append(mitem)
        return mitem

    def _unlock_crypto_keys(self):
        def password_dialog(title, message, verify=False, check=()):
            result = app.input_form(
                title=title,
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

    def _top_modal(self):
        if self._modals:
            return self._modals[-1]
        else:
            return None

    def _modal_parent(self):
        # Return the wx parent for a newly created modal form or dialog.
        # Returns the top most modal form or the main application frame.
        # Modal Pytis dialogs are ignored as they are not wx.Window subclasses.
        if self._modals and isinstance(self._top_modal(), wx.Window):
            parent = self._top_modal()
        else:
            parent = self._frame
        return parent

    def _forms(self):
        return (self._notebook.GetPage(i) for i in range(self._notebook.PageCount))

    def _update_recent_forms_menu(self):
        menu = self._menu_by_id.get(Menu.RECENT_FORMS_MENU)
        if menu:
            for item in menu.GetMenuItems():
                menu.Remove(item.GetId())
                item.Destroy()
            for i, (title, form_name, spec_name) in enumerate(self._recent_forms):
                self._append_menu_item(self._frame, menu, MenuItem(
                    acceskey_prefix(i) + title,
                    help=_("Open the form (%s)", form_name + '/' + spec_name),
                    command=Command(Application.run_form,
                                    form_class=getattr(pytis.form, form_name),
                                    name=spec_name),
                ))
            menu.AppendSeparator()
            self._append_menu_item(self._frame, menu, MenuItem(
                _("Clear"),
                help=_("Clear the menu of recent forms"),
                command=Command(Application.clear_recent_forms),
            ))

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

    def _cleanup(self, force=False):
        # Ignore all kinds of exceptions in order to be able to exit the
        # application even in quite seriously dispair state.
        def safelog(msg, *args):
            try:
                log(ACTION, msg, *args)
            except Exception:
                print(msg, args)
        safelog('Application exit called:', (pytis.config.dbschemas,))
        try:
            self._set_state_param(self._STATE_STARTUP_FORMS, [
                (f.__class__.__name__, f.name())
                for f in self._forms()
                if not isinstance(f, (pytis.form.AggregationForm,
                                      pytis.form.AggregationDualForm))
            ])
            self._set_state_param(self._STATE_RECENT_FORMS, list(self._recent_forms))
            self._set_state_param(self._STATE_RECENT_DIRECTORIES, self._recent_directories)
            # The user may start the application from different systems with different
            # screen sizes/resolutions/configurations, so we only use the saved size
            # and position for the matching display size.  We remember the size and
            # position for up to 8 most recently used display sizes.
            frame_sizes = [[display_size, position, size]
                           for display_size, position, size
                           in self._get_state_param(self._STATE_FRAME_SIZE, [], list, list)
                           if display_size != list(wx.DisplaySize())]
            frame_sizes.append([list(wx.DisplaySize()), list(self._frame.Position),
                                list(self._frame.Size)])
            self._set_state_param(self._STATE_FRAME_SIZE, frame_sizes[-8:])
            active_form = self._notebook.CurrentPage
            if active_form:
                active_form_state = [active_form.__class__.__name__, active_form.name()]
            else:
                active_form_state = None
            self._set_state_param(self._STATE_ACTIVE_FORM, active_form_state)
        except Exception as e:
            safelog(str(e))
        try:
            if not force:
                for form in reversed(tuple(self._forms())):
                    try:
                        if not form.close():
                            return form
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
        return None

    def _activate_tab(self, index):
        notebook = self._notebook
        old_index = notebook.Selection
        notebook.SetSelection(index)
        # Calling SetSelection does not trigger _on_page_change()...
        self._switch_tabs(old_index, index)

    def _switch_tabs(self, old_index, new_index):
        notebook = self._notebook
        count = notebook.PageCount
        if 0 <= new_index < count and new_index != old_index:
            if 0 <= old_index < count:
                old_form = notebook.GetPage(old_index)
                old_form.save()
                old_form.hide()
                old_form.defocus()
            new_form = notebook.GetPage(new_index)
            if isinstance(new_form, pytis.form.Refreshable):
                new_form.refresh()
            new_form.show()
            new_form.Sizer.Layout()
            new_form.restore()
            new_form.focus()
            self._previously_active_tab_index = old_index

    # Callbacky

    def _on_page_change(self, event):
        # Strangely, we need to check whether this event belongs to the main
        # form notebook, because wx sends side form notebook events to this
        # event handler too.
        if event.EventObject is self._notebook and not self._in_cleanup:
            self._switch_tabs(event.OldSelection, event.Selection)
        event.Skip()

    def _on_tab_mouse_right(self, event):
        index = event.Selection
        self.popup_menu(self._notebook, (
            MenuItem(_("Move left"),
                     command=Command(Application.move_tab, index, back=True),
                     help=_("Move this tab one position left (or drag by mouse).")),
            MenuItem(_("Move right"),
                     command=Command(Application.move_tab, index),
                     help=_("Move this tab one position right (or drag by mouse).")),
            MenuItem(_("Close"),
                     command=Command(Application.close_tab, index),
                     help=_("Close the form in this tab.")),
            MenuSeparator(),
            MenuItem(_("Activate previous tab"),
                     command=Command(Application.activate_next_form, back=True),
                     help=_("Switch to the tab on left from the current tab.")),
            MenuItem(_("Activate next tab"),
                     command=Command(Application.activate_next_form),
                     help=_("Switch to the tab on right from the current tab.")),
            MenuItem(_("Activate recently active tab"),
                     command=Command(Application.activate_recent_form),
                     help=_("Allows cyclic switching between two most recently active tabs.")),
        ), keymap=self.keymap)

    def _on_frame_close(self, event):
        if event.CanVeto() and self._modals:
            app.echo(_("Can't exit when modal windows exist."), kind='error')
            event.Veto()
            return
        self._in_cleanup = True
        try:
            form = self._cleanup(force=not event.CanVeto())
        finally:
            self._in_cleanup = False
        if form:
            event.Veto()
            self.activate_form(form)
        else:
            event.Skip()
            pytis.form.app = None

    def _on_frame_size(self, event):
        if self._initialized:
            self._update_tab_titles(event.Size.width)
        event.Skip()

    def _update_tab_titles(self, frame_width):
        notebook = self._notebook
        count = notebook.PageCount
        if count:
            max_tab_width = max(100, frame_width // count)
            for i in range(count):
                title = full_title = notebook.GetPage(i).title()
                title_width = notebook.GetTextExtent(title)[0]
                approx_char_width = 1.2 * title_width / len(title)
                while title_width - 10 > max_tab_width:
                    chars = max(1, int((title_width - max_tab_width) / approx_char_width))
                    title = title[:-chars]
                    title_width = notebook.GetTextExtent(title)[0]
                if (len(full_title) - len(title)) < 5:
                    title = full_title
                else:
                    title += '…'
                notebook.SetPageText(i, title)

    def _on_form_close(self, event):
        form = event.EventObject
        log(EVENT, "Non-modal form closed:", form)
        self._notebook.RemovePage(self._notebook.GetPageIndex(form))
        if self._notebook.PageCount == 0:
            self._panel.SetFocus()
        else:
            self._update_tab_titles(self._frame.Size.x)

    def on_key_down(self, event, dont_skip=False):
        # Believe or not, wxWidgets crashes at various occassions when this
        # handler is not defied.
        return KeyHandler.on_key_down(self, event)

    # Zpracování příkazů

    @Command.define
    def stop(self):
        """Interrupt the currently running operation."""
        app.echo(_("Stopped..."), kind='error')

    @Command.define
    def call(self, function, *args, **kwargs):
        """Perform application defined action."""
        kwargs.pop('enabled', None)
        return function(*args, **kwargs)

    def _can_call(self, function, *args, **kwargs):
        enabled = kwargs.pop('enabled', None)
        if enabled is None:
            return True
        else:
            return enabled(*args, **kwargs)

    @Command.define
    def api_call(self, function, *args, **kwargs):
        return self.call(function, *args, **kwargs)

    def _can_api_call(self, function, *args, **kwargs):
        return self._can_call(function, *args, **kwargs)

    @Command.define
    def activate_form(self, form):
        index = self._notebook.GetPageIndex(form)
        if index != wx.NOT_FOUND:
            self._notebook.SetSelection(index)
            return True
        else:
            return False

    def _can_activate_form(self, form):
        return not self._modals

    @Command.define
    def activate_recent_form(self):
        self._activate_tab(self._previously_active_tab_index)

    def _can_activate_recent_form(self):
        i = self._previously_active_tab_index
        return not self._modals and (i is not None and i < self._notebook.PageCount)

    def _next_tab_index(self, back):
        if back:
            d = -1
        else:
            d = 1
        return self._notebook.Selection + d

    @Command.define
    def activate_next_form(self, back=False):
        self._activate_tab(self._next_tab_index(back))

    def _can_activate_next_form(self, back=False):
        return not self._modals and 0 <= self._next_tab_index(back) < self._notebook.PageCount

    @Command.define
    def move_tab(self, index, back=False):
        form = self._notebook.GetPage(index)
        self._notebook.RemovePage(index)
        self._add_notebook_tab(form, index=index + (-1 if back else 1))

    def _can_move_tab(self, index, back=False):
        return 0 <= (index + (-1 if back else 1)) < self._notebook.PageCount

    @Command.define
    def close_tab(self, index):
        self._notebook.DeletePage(index)
        if self._notebook.PageCount == 0:
            self._panel.SetFocus()
        else:
            self._update_tab_titles(self._frame.Size.x)

    def _can_close_tab(self, index):
        form = self._notebook.GetPage(index)
        return Command(form.leave_form).enabled

    @Command.define
    def clear_recent_forms(self):
        self._recent_forms[:] = []
        self._update_recent_forms_menu()

    def _can_clear_recent_forms(self):
        return len(self._recent_forms) > 0

    @Command.define
    def refresh(self, interactive=True):
        """Request refresh of currently visible form(s)."""
        for w in (self._top_modal(), self._notebook.CurrentPage):
            if isinstance(w, pytis.form.Refreshable):
                w.refresh(interactive=interactive)

    @Command.define
    def reload_specifications(self):
        """Request reload of specification files (development/tuning)."""
        pytis.config.resolver.reload()
        self._cache_menu_enabled(self._menu)

    @Command.define
    def run_form(self, form_class, name, activate=True, **kwargs):
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
            self.wx_yield()
            result = None
            form = find((form_class, name), self._forms(),
                        key=lambda f: (f.__class__, f.name()))
            if form is not None:
                busy_cursor(False)
                self.activate_form(form)
                app.echo(_('Form "%s" found between open windows.', form.title()))
                if 'select_row' in kwargs and kwargs['select_row'] is not None:
                    form.select_row(kwargs['select_row'])
                if 'filter' in kwargs and kwargs['filter'] is not None:
                    form.apply_filter(kwargs['filter'])
                if 'binding' in kwargs and kwargs['binding'] is not None:
                    form.select_binding(kwargs['binding'])
                if 'profile_id' in kwargs and kwargs['profile_id'] is not None:
                    form.apply_profile(kwargs['profile_id'])
                if isinstance(form, pytis.form.WebForm) and 'content' in kwargs:
                    form.load_content(kwargs['content'])
                return result
            if issubclass(form_class, pytis.form.PopupForm):
                parent = self._modal_parent()
                kwargs['guardian'] = self._top_modal() or self
            else:
                # assert not self._modals
                parent = self._notebook
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
                    self._modals.append(form)
                    app.echo(None)
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
                    self._add_notebook_tab(form, activate=activate)
                    wx_callback(wx.EVT_CLOSE, form, self._on_form_close)
                    app.echo(None)
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

    def _add_notebook_tab(self, form, index=None, activate=True):
        notebook = self._notebook
        if index is None:
            index = notebook.PageCount
        notebook.InsertPage(index, form, form.title(), select=activate)
        notebook.SetPageToolTip(index, form.title())
        self._update_tab_titles(self._frame.Size.x)

    @Command.define
    def new_record(self, name, **kwargs):
        """Insert new record into the data object of given specification name."""
        return self.api_new_record(name, **kwargs)

    def _can_new_record(self, name, **kwargs):
        return self._can_api_new_record(name, **kwargs)

    # TODO: This can be removed when all calls are turned to app.run_procedure().
    @Command.define
    def run_procedure(self, spec_name, proc_name, enabled=None, **kwargs):
        return self.api_run_procedure(spec_name, proc_name, **kwargs)

    def _can_run_procedure(self, spec_name, proc_name, enabled=None, **kwargs):
        if not self._public_spec(spec_name):
            return False
        return enabled is None and True or enabled(**kwargs)

    @Command.define
    def help(self, topic='pytis'):
        """Open given help topic in help browser."""
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

    @Command.define
    def reload_rights(self):
        """Reload application rights and menu from the database."""
        self._init_access_rights()
        self._create_menubar()
        self._cache_menu_enabled(self._menu)

    @Command.define
    def custom_debug(self):
        if __debug__:
            pytis.config.custom_debug()

    @Command.define
    def inspect(self):
        import wx.lib.inspection
        tool = wx.lib.inspection.InspectionTool()
        tool.Init(app=self)
        tool.Show()

    @Command.define
    def exit(self):
        """Exit the application."""
        self.api_exit()

    @Command.define
    def nothing(self, enabled=True):
        """Fake command which does nothing."""
        pass

    def _can_nothing(self, enabled=True):
        return enabled

    # Veřejné atributy a metody

    @property
    def headless(self):
        return self._headless

    @property
    def menu(self):
        """Return the application menu as a sequence of 'pytis.presentation.Menu' instances."""
        return self._menu

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
        self._modals.append(dialog)
        try:
            unlock_callbacks()
            result = dialog.run(*args, **kwargs)
        finally:
            self._modals.pop()
            busy_cursor(False)
        # Tento yield zaručí správné předání focusu oken.
        self.wx_yield()
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
        if allow_modal and self._modals:
            return self._top_modal()
        else:
            return self._notebook.CurrentPage

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

    def wx_yield(self, full=False):
        """Process wx events in the queue.

        Arguments:

          full -- iff true, process also user events.

        """
        if self._yield_blocked:
            return
        if self._yield_lock is None:
            self._yield_lock = _thread.allocate_lock()
        if not self._yield_lock.acquire(0):
            return
        try:
            if full:
                self.Yield()
            else:
                wx.SafeYield()
        finally:
            self._yield_lock.release()

    def block_yield(self, block=False):
        """Block or unblock processing of application events.

        Arguments:

          block -- if true, block processing of application events, otherwise unblock it

        """
        self._yield_blocked = block

    def login_hook(self, success):
        if not self._login_success:
            self._specification.login_hook(success)
            if success:
                self._login_success = True

    @property
    def form_profile_manager(self):
        return self._form_profile_manager

    @property
    def form_settings_manager(self):
        return self._form_settings_manager

    @property
    def aggregated_views_manager(self):
        return self._aggregated_views_manager

    def log_user_action(self, spec_name, form_name, action, data=None):
        """Log user action to the database.

        To activate database logging of user actions, include the table
        'pytis.dbdefs.db_pytis_logging.EPytisActionLog' in the database schema.

        If the logging table does not exist, the information will only be
        written to server log.

        """
        log(ACTION, "User action:", (spec_name, form_name, action, data))
        if self._user_action_logger:
            try:
                self._user_action_logger.log(spec_name, form_name, action, data)
            except pd.DBUserException as e:
                if 'relation "e_pytis_action_log" does not exist' in str(e):
                    # We get DBException if the logging table does not exist.
                    # TODO: Test table presence in Logger constructor and avoid
                    # exception handling here.  Successful data object creation
                    # doesn't mean that the table actually exists.  The error
                    # is already logged here, so we just ignore it.
                    pass
                else:
                    raise

    def client_mode(self):
        """Return the client operation mode as one of 'remote', 'local' or None.

        If the remote connection exists, 'remote' is returned.  If not and the
        configuration option 'allow_local_file_dialogs' is set to true, 'local'
        is returned.  If it is False, None is returned (the operation should be
        aborted).

        """
        if pytis.remote.client_connection_ok():
            return 'remote'
        elif pytis.config.allow_local_file_dialogs:
            return 'local'
        else:
            app.error(_("This operation requires remote client connection "
                        "which is currently not available.\n"
                        "You may try to repeat the operation once you see "
                        "a green dot in the connection status indicator (at "
                        "the bottom bar).\n"
                        "The operation will be canceled now."))
            return None

    def _refresh_list_position(self):
        form = pytis.form.app.current_form(allow_modal=False)
        if hasattr(form, 'list_position'):
            return form.list_position()
        else:
            return ''

    def _get_remote_connection_info(self):
        if self._remote_connection_info is not None:
            return self._remote_connection_info
        if not pytis.remote.client_available():
            return ()
        # Retrieve the info just once as soon as the connection is established.
        display = pytis.remote.x2go_display()
        self._remote_connection_info = info = (
            (_("Client version"), pytis.remote.RPCInfo.remote_client_version),
            (_("Remote display"), ':{}'.format(display) if display else None),
            (_("X2Go session ID"), pytis.remote.x2go_session_id()),
            (_("Local Python version"), '.'.join(map(str, sys.version_info[:3]))),
            (_("Remote Python version"), pytis.remote.python_version()),
            (_("Local RPyC version"), pytis.remote.local_rpyc_version()),
            (_("Remote RPyC version"), pytis.remote.rpyc_version()),
            (_("Backend"), pytis.remote.backend_info()),
        )
        for label, value in info:
            log(OPERATIONAL, "Remote connection info:", (label, value))
        return info

    def _refresh_remote_status(self):
        if not pytis.remote.client_available():
            status = _("N/A")
            icon = 'status-offline'
            tooltip = _("Running locally.")
        elif pytis.remote.client_connection_ok():
            status = _("Ok")
            icon = 'status-online'
            tooltip = _("Connected.")
            self._remote_connection_last_available = time.localtime()
        else:
            status = _("Error")
            icon = 'status-error'
            if self._remote_connection_last_available:
                timestamp = time.strftime('%Y-%m-%d %H:%M:%S', self._remote_connection_last_available)
                tooltip = _("Connection lost.") + '\n' + _("Last available: %s", timestamp)
            else:
                tooltip = _("Connection not established.")
        tooltip += '\n\n' + '\n'.join('{}: {}'.format(label, value or _("Not available"))
                                    for label, value in self._get_remote_connection_info())
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

    class _ExposedFileWrapper(object):
        """Wrapper over wrapper to hide difficiencies of legacy ExposedFileWrapper.

        ExposedFileWrapper implementations from older Pytis2Go versions (which
        don't load remote clientapi.py) don't support some features we want in
        Pytis API (context manager protocol, iterator protocol, text mode
        encoding), so this wrapper implements them on top of the
        ExposedFileWrapper instance.

        Older Pytis2Go clients always open the file in binary mode with
        'open_file()' and 'open_selected_file()'.  Encoding can not be passed
        to those methods so we perform encoding in this class and pass bytes
        data to the underlying class.  For these always open remote files in
        binary mode and encode/decode the data in this class.
        'make_selected_file()' on the othe hand does support text mode encoding
        so we pass the data without encoding in this case ('encoding' passed to
        our constructor is None).

        Temporarily disabled: Also, because reading and writing data in small
        chunks (such as using readline()) over a remote connection is really
        slow, we implement buffering through
        'io.BufferedReader'/'io.BufferedWriter' which significantly improves
        performance.

        """
        _BUFFER_SIZE = 512 * 1024

        class BufferingWrapper(object):
            """Wrap ExposedFileWrapper instance to serve buffered data.

            Implements the API expected by 'io.BufferedReader' and
            'io.BufferedWriter'.

            """
            def __init__(self, instance):
                self._instance = instance
                self.closed = False

            def readable(self):
                return True

            def read(self, size=-1):
                return self._instance.read(size)

            def readinto(self, b):
                data = self._instance.read(len(b))
                size = len(data)
                b[:size] = data
                return size

            def writable(self):
                return True

            def write(self, b):
                return self._instance.write(b)

            def close(self):
                self.closed = True

            def flush(self):
                return self._instance.flush()

        def __init__(self, instance, mode=None, encoding=None):
            self._instance = instance
            if mode is not None:
                self._binary_mode = binary_mode = 'b' in mode
                self._text_mode = not binary_mode
                if binary_mode:
                    assert encoding is None, (mode, encoding)
                    encoding = None
                elif encoding is None:
                    # Supply default encoding in text modes.
                    encoding = 'utf-8'
            else:
                assert encoding is None, (mode, encoding)
                # We don't know so we will not do any type checks...
                self._binary_mode = False
                self._text_mode = False
            self._encoding = encoding

            # Temporarily deactivate buffering as it proved to cause some problems.
            if False:  # mode is not None:
                if 'r' in mode:
                    buffer_class = io.BufferedReader
                else:
                    buffer_class = io.BufferedWriter
                self._buffer = buffer_class(self.BufferingWrapper(instance), self._BUFFER_SIZE)
            else:
                # TODO: Apply buffering in all situations (also for app.make_selected_file(),
                # which doesn't pass mode here?
                self._buffer = instance

        def __getattr__(self, name):
            return getattr(self._instance, name)

        def __enter__(self):
            return self

        def __exit__(self, exc_type, exc_value, exc_tb):
            self._instance.close()

        def __iter__(self):
            return self

        def __next__(self):
            line = self.readline()
            if line:
                return line
            else:
                raise StopIteration()

        next = __next__  # Py 2/3 compatibility.

        def _decode(self, data):
            if self._encoding is not None:
                data = data.decode(self._encoding)
            return data

        def read(self, *args, **kwargs):
            return self._decode(self._buffer.read(*args, **kwargs))

        def readline(self):
            return self._decode(self._buffer.readline())

        def readlines(self):
            return [self._decode(line) for line in self]

        def write(self, data):
            if self._text_mode:
                if sys.version_info[0] > 2 and not isinstance(data, str):
                    raise TypeError("a str object is required, not '{}'"
                                    .format(type(data).__name__))
                if sys.version_info[0] == 2 and not isinstance(data, unicode):
                    # The error is not critical in Python 2 to allow the applications
                    # to accommodate.
                    log(OPERATIONAL, ("A unicode object expected, not '{}'"
                                      .format(type(data).__name__)))
                data = data.encode(self._encoding)
            elif self._binary_mode and not isinstance(data, bytes):
                if sys.version_info[0] > 2:
                    raise TypeError("a bytes-like object is required, not '{}'"
                                    .format(type(data).__name__))
                elif not isinstance(data, buffer):
                    log(OPERATIONAL, ("A bytes-like object expected, not '{}'"
                                      .format(type(data).__name__)))
            return self._buffer.write(data)

        def close(self):
            # způsobuje: BlockingIOError: [Errno 11] write could not complete without blocking
            if self._buffer is not self._instance:
                self._buffer.close()
            self._instance.close()

    def _wrap_exposed_file_wrapper(self, f, mode=None, encoding=None):
        if f:
            # Further wrap the ExposedFileWrapper instance to provide
            # compatibility layer which can not be implemented in
            # ExposedFileWrapper itself as long as we have old Pytis2go
            # clients which don't load clientapi.py
            f = self._ExposedFileWrapper(f, mode=mode, encoding=encoding)
        return f

    # Public API accessed through 'pytis.api.app' by Pytis applications.
    # See 'pytis.api.Application' for documentation.

    @property
    def api_title(self):
        return self._title

    @api_title.setter
    def api_title(self, title):
        self._title = title
        if __debug__:
            title += ' - wx ' + wx.version() + ', Python %d.%d.%d' % sys.version_info[:3]
        self._frame.SetTitle(title)

    @property
    def api_form(self):
        form = self.current_form(inner=True)
        return form.provider() if form else None

    @property
    def api_forms(self):
        return ([form.provider() for form in reversed(self._modals)
                 if isinstance(form, pytis.form.Form)] +
                [form.provider() for form in self._forms()])

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
        self.refresh(interactive=False)

    @Command.define
    def api_exit(self, force=False):
        self._frame.Close(force=force)

    def api_echo(self, message, kind='info'):
        assert kind in ('info', 'warning', 'error')
        if self._last_echo != (message or '', kind):
            self._last_echo = (message or '', kind)
            log(EVENT, 'Echo [%s]:' % kind, message)
        if kind in ('warning', 'error'):
            beep()
        form = self._top_modal()
        if isinstance(form, pytis.form.Form) and form.set_status('message', message):
            return
        if not hasattr(self, '_status_fields'):
            # Suppress errors during opening DB login input form, which needs
            # to come up sooner than the status bar is initialized.
            return
        if kind == 'warning':
            app.status.message.icon = wx.ART_WARNING
        elif kind == 'error':
            app.status.message.icon = wx.ART_ERROR
        else:
            app.status.message.icon = None
        app.status.message.text = message

    def api_message(self, message=None, title=None, content=None):
        return self.run_dialog(dialog.Message, message, title=title or _("Message"),
                               content=content)

    def api_warning(self, message=None, title=None, content=None):
        return self.run_dialog(dialog.Warning, message, title=title or _("Warning"),
                               content=content)

    def api_error(self, message=None, title=None, content=None):
        return self.run_dialog(dialog.Error, message, title=title or _("Error"), content=content)

    def api_question(self, message, answers=None, default=None, title=None, content=None,
                     timeout=None):
        return self.run_dialog(dialog.Question, message, title=title or _("Question"),
                               answers=answers, default=default, timeout=timeout, content=content)

    def api_delete_record_question(self, message=None):
        log(EVENT, 'Record deletion dialog')
        if not app.question(message or _("Are you sure to delete the record permanently?")):
            log(EVENT, 'Record deletion refused by user')
            return False
        log(EVENT, u'Record deletion confirmed by user')
        return True

    def _input(self, type, title, label, default=None, width=None, height=None, descr=None,
               noselect=False, compact=False, resizable=False,
               text_format=TextFormat.PLAIN, attachment_storage=None):
        if resizable:
            form = pytis.form.ResizableInputForm
        else:
            form = pytis.form.InputForm
        row = run_form(
            form,
            title=title,
            avoid_initial_selection=noselect,
            fields=(
                # Convert None in label to '' because it makes more sense
                # to use None for unlabeled fields.  We historically used
                # empty string for that in field specifications, but we don't
                # want to propagate this unfortunate decision to Pytis API.
                Field('f', label or '', default=default, type=type, width=width,
                      height=height, descr=descr, compact=compact,
                      text_format=text_format, attachment_storage=attachment_storage),
            ),
        )
        if row:
            return row['f'].value()
        else:
            return None

    def api_input_text(self, title, label, default=None, not_null=False, width=20, height=1,
                       descr=None, noselect=False, compact=False,
                       text_format=TextFormat.PLAIN, attachment_storage=None):
        return self._input(pd.String(not_null=not_null), title, label, default=default,
                           width=width, height=height, descr=descr, noselect=noselect,
                           compact=compact, resizable=height > 1,
                           text_format=text_format, attachment_storage=attachment_storage)

    def api_input_date(self, title, label=None, default=None, not_null=True, descr=None,
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

    def api_input_form(self, title, fields, prefill=None, layout=None, check=None, noselect=False,
                       inserted_data=None, focus_field=None, on_commit_record=None,
                       transaction=None):
        return run_form(pytis.form.InputForm, title=title, fields=fields,
                        prefill=prefill, layout=layout, check=check or (),
                        avoid_initial_selection=noselect, inserted_data=inserted_data,
                        focus_field=focus_field, on_commit_record=on_commit_record,
                        transaction=transaction)

    def _spec_name(self, specification):
        if isinstance(specification, basestring):
            return specification
        else:
            return specification._spec_name()

    @Command.define
    def api_new_record(self, specification, prefill=None, inserted_data=None, multi_insert=True,
                       copied_row=None, set_values=None, block_on_new_record=False,
                       transaction=None):
        name = self._spec_name(specification)
        view = pytis.config.resolver.get(name, 'view_spec')
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
                        set_values=set_values)

    def _can_api_new_record(self, name, **kwargs):
        try:
            return self.api_has_access(name, perm=pd.Permission.INSERT)
        except ResolverError:
            # The spec is invalid, but we want the crash on attempt to run it.
            return True

    def api_show_record(self, specification, row):
        assert isinstance(row, (PresentedRow, pytis.data.Row, pytis.data.Value)), row
        name = self._spec_name(specification)
        if isinstance(row, PresentedRow):
            row = row.row()
        # TODO: show_record() doesn't support redirect() (see api_edit_record()
        # below), but redirect is nearly unused and should be removed so it is
        # not worth the complication here....
        return run_form(pytis.form.BrowsableShowForm, name, select_row=row)

    def api_edit_record(self, specification, row, set_values=None, layout=None,
                        block_on_edit_record=False, transaction=None):
        name = self._spec_name(specification)
        view = pytis.config.resolver.get(name, 'view_spec')
        if not isinstance(row, PresentedRow):
            data = pytis.util.data_object(name)
            if not isinstance(row, pd.Row):
                key = row
                row = data.row(row, transaction=transaction)
                if not row:
                    log(OPERATIONAL, "Invalid row reference:", key)
                    app.error(_("Record not found"))
                    return
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
            return run_form(pytis.form.PopupEditForm, name, select_row=key, layout=layout,
                            set_values=set_values, transaction=transaction)

    def api_delete_record(self, specification, row, question=None, transaction=None):
        name = self._spec_name(specification)
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

    @Command.define
    def api_run_form(self, specification, select_row=None, multi=True, preview=False, sorting=None,
                     filter=None, condition=None, profile=None, binding=None, transaction=None):
        name = self._spec_name(specification)
        kwargs = {}
        if '::' in name:
            form_class = pytis.form.BrowseDualForm
        elif preview:
            form_class = pytis.form.DescriptiveDualForm
        elif multi and self._has_bindings(name):
            form_class = pytis.form.MultiBrowseDualForm
            kwargs = dict(binding=binding)
        else:
            form_class = pytis.form.BrowseForm
        return run_form(form_class, name, select_row=select_row, sorting=sorting,
                        filter=filter, condition=condition, profile_id=profile,
                        transaction=transaction, **kwargs)

    def _can_api_run_form(self, name, *args, **kwargs):
        return app.has_access(name)

    def api_codebook(self, specification, select_row=0, columns=None, sorting=None, filter=None,
                     condition=None, multirow=False, begin_search=None, transaction=None):
        name = self._spec_name(specification)
        if multirow:
            form_class = pytis.form.SelectRowsForm
        else:
            form_class = pytis.form.CodebookForm
        return run_form(form_class, name, select_row=select_row, columns=columns,
                        sorting=sorting, filter=filter, condition=condition,
                        begin_search=begin_search, transaction=transaction)

    @Command.define
    def api_run_procedure(self, spec_name, proc_name, *args, **kwargs):
        result = None
        try:
            app.echo(_("Running procedure..."))
            log(ACTION, 'Running procedure:', (spec_name, proc_name, args, kwargs))
            # Kvůli wx.SafeYield() se ztrácí focus, takže
            # si ho uložíme a pak zase obnovíme.
            focused = wx_focused_window()
            self.wx_yield()
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

    def _can_api_run_procedure(self, spec_name, proc_name, *args, **kwargs):
        return self._public_spec(spec_name)

    @Command.define
    def api_web_view(self, title, content, name=None):
        if callable(content):
            content = content()
        run_form(pytis.form.WebForm, title=title, content=content, name=name)

    def api_run(self, function, args=(), kwargs={}, over=None, title=None, message=None,
                progress=True, maximum=None, elapsed_time=False, estimated_time=False,
                remaining_time=False, time_precision='seconds', can_abort=False):
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
                if update(progress=0):
                    for n, arg in enumerate(over):
                        if pass_n:
                            kwargs['n'] = n
                        func(lambda message=None: update(message=message), arg, *args, **kwargs)
                        if not update(progress=min(maximum, int(float(n + 1) / count * 100))):
                            break
        return self.run_dialog(
            dialog.ProgressDialog, function, args, kwargs,
            title=title or _("Operation in progress"),
            message=message or _("Please wait..."),
            show_progress=progress,
            maximum=maximum if maximum is not None else 100,
            elapsed_time=elapsed_time,
            estimated_time=estimated_time,
            remaining_time=remaining_time,
            time_precision=time_precision,
            can_abort=can_abort,
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
                                    os.path.basename(path), mime_type))
                        return
                else:
                    app.error(_("MIME type for '%s' not found.\n"
                                "Your system doesn't recognize the MIME type of this file.",
                                os.path.basename(path)))
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
                    os.fsync(f)
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
            f = io.open(path, mode, encoding=encoding) if path else None
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
            if ((sys.version_info[0] == 2 and isinstance(data, bytes)
                 and self.client_mode() == 'remote')):
                # TODO: The older version of P2Go's pytisproc.py currently distributed
                # between users doesn't handle text encoding quite well.  It attempts
                # to encode everything which is not a 'buffer'.  We can remove this
                # hack once all clients have a newer P2Go version.  As this doesn't
                # even work in Python 3 (which doesn't have buffer) old clients will
                # not work with Python3 at all.
                data = buffer(data)
            # TODO: This is a temporary hack to avoid "TypeError: write()
            # argument must be str, not bytes" when the application passes
            # invalid type due to insufficient Python 2/3 transition.  The
            # logs should be observed and this hack should be removed
            # once the problem does not occur for "some" time.
            if 'b' not in mode and isinstance(data, bytes):
                log(OPERATIONAL, "TypeError: write_selected_file() 'data' argument must be str, "
                    "not bytes when mode is '{}'.".format(mode), pytis.util.stack_info())
                data = data.decode(encoding or 'utf-8')
            try:
                f.write(data)
            finally:
                f.close()
            return f.name
        else:
            return None

    def api_open_selected_file(self, directory=None, mode='rb', encoding=None, encrypt=None,
                               filetypes=None, context='default'):
        # TODO: Encryption not supported for the local variant.
        cmode = self.client_mode()
        if directory is None:
            directory = self._get_recent_directory(cmode, context)
        else:
            context = None  # Prevent storing the explicitly passed directory when dialog closed.
        if cmode == 'remote':
            f = self._wrap_exposed_file_wrapper(pytis.remote.open_selected_file(
                directory=directory,
                encrypt=encrypt,
                filetypes=filetypes
            ), mode=mode, encoding=encoding)
        elif cmode == 'local':
            path = self.run_dialog(dialog.FileDialog, dir=directory,
                                   mode=dialog.FileDialog.OPEN,
                                   wildcards=self._wildcards(filetypes))
            f = io.open(path, mode, encoding=encoding) if path else None
        else:
            f = None
        if f and context:
            self._set_recent_directory(cmode, context, self._dirname(cmode, f.name))
        return f

    def api_open_file(self, filename, mode='r', encoding=None):
        cmode = self.client_mode()
        if cmode == 'remote':
            # Open remote file in binary mode and rely on text mode emulation
            # in self._ExposedFileWrapper, because older clients don't support
            # 'encoding' in pytis.remote.open_file().
            remote_mode = mode.replace('t', '') + 'b' if 'b' not in mode else mode
            f = self._wrap_exposed_file_wrapper(
                pytis.remote.open_file(filename, mode=remote_mode),
                mode=mode, encoding=encoding,
            )
        elif cmode == 'local':
            if 'b' not in mode and encoding is None:
                # Make sure encoding defaults to UTF-8, not the system default.
                encoding = 'utf-8'
            f = io.open(filename, mode, encoding=encoding)
        return f

    def api_write_file(self, data, filename, mode='w'):
        # TODO: Add 'encoding' argument.
        if sys.version_info[0] == 2 and isinstance(data, bytes):
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

    class _OutputResolver(pytis.output.OutputResolver):
        """Default print resolver used by 'app.printout()'."""

        class _Spec(object):
            # This class has to emulate a specification module as well as a
            # (new style) specification class.
            def __init__(self, error):
                self._error = error

            def init(self, resolver=None, **kwargs):
                app.error(_("Print specification not found: {}").format(self._error))
                return False

            def body(self, resolver=None, **kwargs):
                return None

            def doc_header(self, resolver=None, **kwargs):
                return None

            def doc_footer(self, resolver=None, **kwargs):
                return None

        def _get_module(self, name):
            # Supply a default specification module (old style spec).
            try:
                return super(Application._OutputResolver, self)._get_module(name)
            except pytis.util.ResolverError as e:
                return self._Spec(e)

        def _get_instance(self, key):
            # Supply a default specification class (new style spec).
            try:
                return super(Application._OutputResolver, self)._get_instance(key)
            except pytis.util.ResolverError as e:
                return self._Spec(e)

    class _OutputFormatter(pytis.output.Formatter):

        def printout(self, output_file):
            def do_printout(outfile):
                try:
                    super(Application._OutputFormatter, self).printout(outfile)
                except lcg.SubstitutionIterator.NotStartedError:
                    tbstring = pytis.util.format_traceback()
                    log(OPERATIONAL, 'Print exception caught', tbstring)
                    app.error(_("Invalid use of identifier `data' in print specification.\n"
                                "Maybe use `current_row' instead?"))
            def run_viewer(filename):
                try:
                    app.launch_file(filename)
                finally:
                    try:
                        os.remove(filename)
                    except OSError as e:
                        log(OPERATIONAL, 'Error removing temporary file:', e)
            try:
                if output_file:
                    do_printout(output_file)
                else:
                    # TODO: Use app.launch_file() here?
                    with tempfile.NamedTemporaryFile(suffix='.pdf',
                                                     prefix='tmppytis', delete=False) as f:
                        do_printout(f)
                    threading.Thread(target=run_viewer, args=(f.name,)).start()
            except UserBreakException:
                pass


    def _output_formatter(self, template_id, **kwargs):
        # api_printout() is implemented by BaseApplication, but we want to customize
        # the reaction to some errors in the wx application and launch a PDF viewer
        # automatically when output_file is not specified, so we use a customized
        # self._OutputResolver and self._OutputFormatter.
        output_resovers = (
            pytis.output.DatabaseResolver('ev_pytis_user_output_templates',
                                          ('template', 'rowtemplate', 'header',
                                           'first_page_header', 'footer', 'style'),
                                          ('body', 'row', 'page_header',
                                           'first_page_header', 'page_footer', 'style')),
            self._OutputResolver(pytis.config.print_spec_dir, pytis.config.resolver),
        )
        return self._OutputFormatter(pytis.config.resolver, output_resovers, template_id, **kwargs)


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
    (lze jej odstranit příkazem 'Form.leave_form').  V případě
    modálního formuláře se funkce vrací až po jeho uzavření.

    Vrací: Návratovou hodnotu metody 'run()' v případě modálního formuláře,
    nebo None v případě nemodálního formuláře.  Pokud formulář nelze spustit
    (např. nedostatečná přístupová práva) , vrací False.

    """
    command = Command(Application.run_form, form_class=form_class, name=name, **kwargs)
    if not command.enabled:
        app.echo(_("Opening form refused."), kind='error')
        return False
    return command.invoke()

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

            if not app.input_form(
                    title=_("Log in for database access"),
                    fields=(Field('login', _("Login"), width=24, not_null=True,
                                  enumerator=login_enumerator, default=default_login),
                            Field('password', _("Password"), type=pd.Password(verify=False),
                                  editable=password_editable, computer=password_computer,
                                  default=default_password, width=24, not_null=True),),
                    focus_field='password',
                    check=check_login):
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


# Deprecated backwards compatibility aliases.

def recent_forms_menu():
    return Menu(_("Recently opened forms"), (), id=Menu.RECENT_FORMS_MENU, autoindex=False)

def config_menu_items(hotkeys={}):
    return (
        MenuItem(_("User interface settings"),
                 Command(app.run_procedure, 'configui', 'ui_settings')),
        MenuItem(_("Export settings"),
                 Command(app.run_procedure, 'configui', 'export_settings')),
    )

from pytis.presentation import (
    Menu, MenuItem as MItem, MenuSeparator as MSeparator,
    MenuItem as CheckItem, MenuItem as RadioItem,
)

def close_forms():
    for form in app.forms:
        if not form.close():
            return False
    return not app.forms

class LegacyCommand(Command):

    def __call__(self, **kwargs):
        return Command(self._method, **kwargs)

class HandledAction(LegacyCommand):

    def __call__(self, handler, **kwargs):
        return Command(self._method, handler, **kwargs)

# Backwards compatibility aliases for menu definition.  Remove after migrating all applications.
# When removing, also remove the part marked by comment "Handle legacy menu commands."
# in pytis.extensions.defs.
Application.COMMAND_NEW_RECORD = LegacyCommand(Application.new_record)
Application.COMMAND_RUN_FORM = LegacyCommand(Application.run_form)
Application.COMMAND_RUN_PROCEDURE = LegacyCommand(Application.run_procedure)
Application.COMMAND_EXIT = LegacyCommand(Application.exit)
Application.COMMAND_HANDLED_ACTION = HandledAction(Application.call)
Application.COMMAND_RELOAD_RIGHTS = LegacyCommand(Application.reload_rights)
Application.COMMAND_CLEAR_RECENT_FORMS = LegacyCommand(Application.clear_recent_forms)
