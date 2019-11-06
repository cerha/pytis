# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
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
import os.path
import string
import sys
import _thread
import time
import datetime
import wx
import wx.html

import pytis.data
import pytis.form
from pytis.presentation import Field, Specification, StatusField, computer, Text
import pytis.util
import pytis.remote
from pytis.util import (
    ACTION, DEBUG, EVENT, OPERATIONAL, ProgramError, ResolverError, Stack, XStack,
    argument_names, find, format_traceback, identity, log, rsa_encrypt,
)
from .command import CommandHandler
from .event import (
    UserBreakException, interrupt_init, interrupt_watcher,
    top_level_exception, unlock_callbacks, wx_callback, yield_,
)
from .screen import (
    Browser, CheckItem, KeyHandler, Keymap, Menu, MenuBar, MItem, MSeparator, StatusBar,
    acceskey_prefix, beep, busy_cursor, get_icon, mitem, wx_focused_window,
)
from .dialog import (
    Message, Question, Error, CheckListDialog, ProgressDialog,
)


_ = pytis.util.translations('pytis-wx')

_application = None
unistr = type(u'')  # Python 2/3 transition hack.


class Application(wx.App, KeyHandler, CommandHandler):

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
    class NameSpace:
        pass

    _menubar_forms = {}
    _log_login = True
    _recent_directories = {}

    _WINDOW_MENU_TITLE = _("&Windows")

    _STATE_RECENT_FORMS = 'recent_forms'
    _STATE_STARTUP_FORMS = 'saved_startup_forms'  # Avoid name conflict with config.startup_forms!
    _STATE_RECENT_DIRECTORIES = 'recent_directories'
    _STATE_FRAME_SIZE = 'frame_size'

    @classmethod
    def _get_command_handler_instance(cls):
        global _application
        return _application

    def OnInit(self):
        import pytis.extensions
        self._specification = pytis.config.resolver.specification('Application')
        # Create the main application frame.
        frame = self._frame = wx.Frame(None, -1, self._frame_title(pytis.config.application_name),
                                       pos=(0, 0), style=wx.DEFAULT_FRAME_STYLE)
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
        global _application
        _application = self

        # Initialize login and password.
        def test():
            bindings = [pytis.data.DBColumnBinding(id, 'pg_catalog.pg_tables', id)
                        for id in ('tablename',)]
            factory = pytis.data.DataFactory(pytis.data.DBDataDefault, bindings, bindings[0])
            factory.create(connection_data=pytis.config.dbconnection)
        db_operation(test)
        # Define statusbar
        # TODO: This is temporary backwards compatible conversion of status_fields()
        # specifications.  It should be removed when all applications are updated.
        default_fields = dict([(f.id(), f)
                               for f in pytis.presentation.Application().status_fields()])
        status_fields = [default_fields.get(x[0], StatusField(x[0], width=x[1]))
                         if isinstance(x, tuple) else x
                         for x in self._specification.status_fields()]
        self._statusbar = StatusBar(frame, status_fields)
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
        except pytis.data.DBException as e:
            # Logging is optional.  The application may choose not to include
            # the logging table in the database schema and this will simply
            # lead to logging being ignored.
            log(OPERATIONAL, "Form action logging not activated:", e)
            self._logger = None
        # Read the stored configuration.
        for option, value in self._application_config_manager.load():
            if hasattr(pytis.config, option):
                setattr(pytis.config, option, value)
            else:
                self._saved_state[option] = value
        # Read in access rights.
        init_access_rights(pytis.config.dbconnection)
        # Unlock crypto keys
        self._unlock_crypto_keys()
        # Init the recent forms list.
        recent_forms = self._get_state_param(self._STATE_RECENT_FORMS, (), (list, tuple), tuple)
        self._recent_forms = []
        for title, args in recent_forms:
            if ((self._is_valid_spec(args['name']) and
                 issubclass(args['form_class'], pytis.form.Form))):
                self._recent_forms.append((title, args))
            else:
                log(OPERATIONAL, "Ignoring recent form:", args)
        self._set_state_param(self._STATE_RECENT_FORMS, tuple(self._recent_forms))
        # Init the recent directories memory.
        directories = self._get_state_param(self._STATE_RECENT_DIRECTORIES, (), tuple, tuple)
        self._recent_directories.update(dict(directories))
        # Initialize the menubar.
        mb = self._create_menubar()
        if mb is None:
            return False
        # Finish and show the frame.
        #
        # TODO: There are problems when users use multiply monitors or start application
        # with different screens or screen resolutions. So until we find the adequate
        # solution, we use some safe default frame size.
        #
        # frame.SetSize(self._get_state_param(self._STATE_FRAME_SIZE, (1000, 800), tuple, int))
        frame.SetSize((1000, 800))
        wx.Font.SetDefaultEncoding(wx.FONTENCODING_ISO8859_2)
        wx_callback(wx.EVT_SIZE, frame, self._on_frame_size)
        self.SetTopWindow(frame)
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
        rpc_info = pytis.remote.RPCInfo
        rpc_info.remote_connection_initially_available = pytis.remote.client_available()
        if rpc_info.remote_connection_initially_available:
            rpc_info.remote_status_info = (True, time.time())
            try:
                rpc_info.remote_client_version = version = pytis.remote.x2goclient_version()
            except Exception:
                version = 'unknown'
            log(OPERATIONAL, "RPC communication available. Version:", version)
        else:
            rpc_info.remote_status_info = (False, time.time())
            log(OPERATIONAL, "RPC communication unavailable")
        return True

    def _frame_title(self, title):
        display = pytis.remote.x2go_display()
        if display:
            title += ' (:%s)' % display
        if __debug__:
            title += ' - wx %s' % wx.version()
        return title

    def _cache_menu_enabled(self, menu):
        # Cache the specification instances needed to compute the availability
        # of menu items.  This reduces the lag during the first user's attempt
        # to open a menu.
        for item in menu:
            if isinstance(item, Menu):
                self._cache_menu_enabled(item.items())
            elif not isinstance(item, pytis.form.MSeparator):
                enabled = item.command().enabled(**item.args())
                if __debug__:
                    if pytis.config.debug:
                        log(DEBUG, 'Menu item:', (item.title(), enabled))

    def _spec_title(self, name):
        if name.find('::') != -1:
            names = name.split('::')
            return (pytis.config.resolver.get(names[0], 'binding_spec')[names[1]].title() or
                    ' / '.join([self._spec_title(n) for n in names]))
        else:
            return pytis.config.resolver.get(name, 'view_spec').title()

    def _init(self):
        # Check RPC client version
        self._check_x2goclient()
        # Create DBParams instances for all SharedParams specifications.
        self.param = self.NameSpace()
        for item in self._specification.params():
            setattr(self.param, item.name(), DBParams(item.spec_name(), item.condition()))
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
                        self.run_dialog(Error, _("Invalid form class in 'startup_forms':") +
                                        ' ' + cls_name)
                        continue
                else:
                    cls = (name.find('::') == -1 and
                           pytis.form.BrowseForm or
                           pytis.form.BrowseDualForm)
                startup_forms.append((cls, name.strip()))
        saved_forms = []
        for cls, name in self._get_state_param(self._STATE_STARTUP_FORMS, (), tuple, tuple):
            if ((issubclass(cls, pytis.form.Form) and
                 self._is_valid_spec(name) and
                 (cls, name) not in startup_forms)):
                saved_forms.append((cls, name))
            else:
                log(OPERATIONAL, "Ignoring saved startup form:", (cls, name))
        if saved_forms:
            if pytis.config.autostart_saved_forms:
                startup_forms.extend(reversed(saved_forms))
            else:
                checked = self.run_dialog(
                    CheckListDialog,
                    title=_("Restore forms"),
                    message=_("Restore these forms saved on last exit?"),
                    items=[(True, '%s (%s)' % (self._spec_title(name), f.descr()))
                           for f, name in saved_forms]
                )
                if checked:
                    startup_forms.extend(reversed([x for x, ch in zip(saved_forms, checked) if ch]))

        def run_startup_forms(update, startup_forms):
            i, total = 0, len(startup_forms)
            msg = _("Opening form: %s (%d/%d)")
            for cls, name in startup_forms:
                update(int(float(i) / total * 100), newmsg=msg % (name, i + 1, total,))
                try:
                    run_form(cls, name)
                except Exception as e:
                    log(OPERATIONAL, "Unable to init startup form:", (cls, name, e))
                i += 1
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
            run_dialog(ProgressDialog, run_startup_forms, args=(startup_forms,),
                       title=_("Opening saved forms"),
                       message=_("Opening form") + ' ' * 40)  # , can_abort=True)
            # In wx2.8, keyboard navigation doesn't work now.  The following
            # lines raise the previous form and then back the top form, which
            # fixes the problem.  Running a Message dialog instead also helps,
            # but it's probably more obtrusive to the users.  Maybe you can
            # find a better solution!
            mru_windows = self._windows.mru()
            if len(mru_windows) > 1:
                self._raise_form(mru_windows[1])
                self._raise_form(mru_windows[1])
        else:
            run_startup_forms(lambda *args, **kwargs: True, startup_forms)
        self._frame.SetTitle(self._frame_title(pytis.config.application_name))
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

    def _create_command_menu(self, menus):
        items = []
        for group in pytis.form.FORM_MENU_COMMANDS:
            if items:
                items.append(MSeparator())
            for uicmd in group:
                items.append(mitem(uicmd))
        menus.append(Menu(_("Commands"), items))

    def _create_help_menu(self, menus):
        if [m for m in menus if m.title() == _("Help")]:
            log(OPERATIONAL, "Help menu found - not creating one.")
            return
        items = [mitem(pytis.form.UICommands.PYTIS_HELP)]
        items.extend((MSeparator(),
                      mitem(pytis.form.UICommands.HELP),
                      mitem(pytis.form.UICommands.DESCRIBE)))
        menus.append(Menu(_("Help"), items))

    def _dynamic_menu(self, connection_data):
        # Check for menu presence, if not available, return None
        I_ = pytis.data.Integer()
        S = pytis.data.String()
        language = pytis.util.current_language()
        try:
            menu_data = pytis.data.dbtable('pytis_view_user_menu',
                                           (('menuid', I_,),
                                            ('name', S,),
                                            ('title', S,),
                                            ('fullname', S,),
                                            ('position', pytis.data.LTree(),),
                                            ('help', S,), ('hotkey', S,), ('language', S,),),
                                           connection_data, arguments=())
            menu_rows = menu_data.select_map(identity,
                                             condition=pytis.data.EQ('language',
                                                                     pytis.data.sval(language)),
                                             sort=(('position', pytis.data.ASCENDENT,),))
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
        return [build(t) for t in menu_template]

    def _create_menubar(self):
        self._recent_forms_menu = None
        menu = self._menu = self._build_menu(self._specification.menu(), pytis.config.dbconnection)
        menu.append(Menu(self._WINDOW_MENU_TITLE,
                         (MItem(_("Previous window"), command=Application.COMMAND_RAISE_PREV_FORM,
                                help=_("Switch to the previous window in the window list order.")),
                          MItem(_("Next window"), command=Application.COMMAND_RAISE_NEXT_FORM,
                                help=_("Switch to the next window in the window list order.")),
                          MItem(_("Most recently active window"),
                                command=Application.COMMAND_RAISE_RECENT_FORM,
                                help=_("Allows mutual switching of two most recently active "
                                       "windows cyclically.")),
                          MItem(_("Close active window"),
                                command=pytis.form.Form.COMMAND_LEAVE_FORM,
                                help=_("Closes the window of the active form.")),
                          MSeparator(),
                          ),
                         allow_autoindex=False))
        self._create_command_menu(menu)
        self._create_help_menu(menu)
        # Determining availability of menu items may invoke database operations...
        success, mb = db_operation(MenuBar, self._frame, menu, self.keymap)
        if not success:
            return None
        self._menubar = mb
        self._window_menu = mb.GetMenu(mb.FindMenu(self._WINDOW_MENU_TITLE))
        assert self._window_menu is not None
        return mb

    def _unlock_crypto_keys(self):
        def password_dialog(title, message, verify=False, check=()):
            result = run_form(
                pytis.form.InputForm, title=title,
                fields=(Field('password', _("Password"),
                              type=pytis.data.Password, verify=verify,
                              width=40, not_null=True),),
                layout=(Text(message), 'password'),
                check=check,
            )
            if result:
                return rsa_encrypt(db_key, result['password'].value()).decode('ascii')
            else:
                return None

        self._decrypted_names = decrypted_names = set()

        try:
            data = pytis.data.dbtable('ev_pytis_user_crypto_keys',
                                      ('key_id', 'name', 'fresh',),
                                      pytis.config.dbconnection)
        except pytis.data.DBException:
            return
        else:
            rows = data.select_map(identity)
            data.close()
        if not rows:
            return

        db_key = pytis.extensions.dbfunction('pytis_crypto_db_key',
                                             ('key_name_', pytis.data.sval('pytis'),))
        crypto_password = pytis.config.dbconnection.crypto_password()
        if not crypto_password:
            established_names = [r for r in rows if not r['fresh'].value()]
            crypto_password = password_dialog(
                _("Enter your password"),
                _("Enter your login password for encryption keys management"),
                verify=not established_names,
                check=(
                    lambda r: ('password', _("Invalid password"))
                    if established_names and not pytis.extensions.dbfunction(
                        'pytis_crypto_unlock_current_user_passwords',
                        ('password_', pytis.data.sval(
                            rsa_encrypt(db_key, r['password'].value()).decode('ascii')),))
                    else None,),
            )
            if not crypto_password:
                return
            # Set this password for all DB connections (closes all current connections!).
            pytis.data.DBFunctionDefault(
                'pytis_crypto_unlock_current_user_passwords',
                lambda: pytis.config.dbconnection
            ).reset_crypto_password(crypto_password)

        crypto_password_value = pytis.data.sval(crypto_password)
        while True:
            established_names = set()
            fresh_names = set()
            for row in data.select_map(identity):
                name = row['name'].value()
                if row['fresh'].value():
                    fresh_names.add(name)
                else:
                    established_names.add(name)
            ok_names = pytis.extensions.dbfunction('pytis_crypto_unlock_current_user_passwords',
                                                   ('password_', crypto_password_value,))
            if isinstance(ok_names, list):
                ok_names = set([row[0].value() for row in ok_names])
            else:
                ok_names = set([ok_names])
            decrypted_names.update(ok_names)
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
                        pytis.extensions.dbfunction('pytis_crypto_change_password',
                                                    ('id_', r['key_id']),
                                                    ('old_psw', pytis.data.sval(password)),
                                                    ('new_psw', crypto_password_value))
                    except pytis.data.DBException:
                        pass
        data.close()
        pytis.extensions.dbfunction('pytis_crypto_unlock_current_user_passwords',
                                    ('password_', crypto_password_value,))

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
        def wmitem(i, form):
            info = form.__class__.__name__
            if form.name():
                info += '/' + form.name()
            return CheckItem(acceskey_prefix(i) + self._form_menu_item_title(form),
                             help=_("Bring form window to the top (%s)", info),
                             command=Application.COMMAND_RAISE_FORM,
                             state=lambda: top_window() is form,
                             args={'form': form})
        menu = self._window_menu
        if menu is not None:
            for i, item in enumerate(menu.GetMenuItems()):
                if i >= 5:
                    menu.Remove(item.GetId())
                    item.Destroy()
            for i, form in enumerate(self._windows.items()):
                menu.Append(wmitem(i, form).create(self._frame, menu))

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
                    menu.Append(item.create(self._frame, menu))

    def _recent_forms_menu_items(self):
        items = [MItem(acceskey_prefix(i) + title,
                       help=_("Open the form (%s)",
                              args['form_class'].__name__ + '/' + args['name']),
                       command=Application.COMMAND_RUN_FORM, args=args)
                 for i, (title, args) in enumerate(self._recent_forms)]
        items.append(MSeparator())
        items.append(MItem(_("Clear"),
                           help=_("Clear the menu of recent forms"),
                           command=Application.COMMAND_CLEAR_RECENT_FORMS))
        return items

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
        safelog('Application exit called', (pytis.config.dbschemas,))
        try:
            if not self._modals.empty():
                log(EVENT, "Couldn't close application with modal windows:",
                    self._modals.top())
                return False
            self._set_state_param(self._STATE_STARTUP_FORMS, tuple(
                (f.__class__, f.name())
                for f in self._windows.items()
                if not isinstance(f, (pytis.form.PrintForm,
                                      pytis.form.AggregationForm,
                                      pytis.form.AggregationDualForm))
            ))
            self._set_state_param(self._STATE_RECENT_FORMS, tuple(self._recent_forms))
            self._set_state_param(self._STATE_RECENT_DIRECTORIES,
                                  tuple(self._recent_directories.items()))
            self._set_state_param(self._STATE_FRAME_SIZE, tuple(self._frame.GetSize()))
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
            options = list(self._saved_state.items())
            for option, initial_value in self._initial_config:
                current_value = getattr(pytis.config, option)
                if current_value != initial_value:
                    options.append((option, current_value))
            self._application_config_manager.save(options)
            log(OPERATIONAL, "Configuration saved: %d items" % len(options))
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
        message(_("Stopped..."), beep_=True)

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
            if has_access(name):
                if binding is not None or issubclass(form_class, pytis.form.MultiBrowseDualForm):
                    spec = pytis.config.resolver.get(name, 'view_spec')
                    if binding is None:
                        for b in spec.bindings():
                            binding_name = b.name()
                            if binding_name is None or has_access(binding_name):
                                return True
                        return False
                    else:
                        b = find(binding, spec.bindings(), key=lambda b: b.id())
                        assert b is not None, "Unknown binding for %s: %s" % (name, binding)
                        if b.name():
                            return has_access(b.name())
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
            message(_("Opening form..."), root=True)
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
                message(_('Form "%s" found between opened windows.', form.title()))
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
                self.run_dialog(Error, _("Form creation failed: %s", name))
            else:
                if isinstance(form, pytis.form.PopupForm):
                    log(EVENT, "Opening modal form:", form)
                    self._modals.push(form)
                    message('', root=True)
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
                    message('', root=True)
                    form.resize()  # Needed in wx 2.8.x.
                    form.show()
                    self._update_window_menu()
                    if not isinstance(form, (pytis.form.PrintForm, pytis.form.WebForm)):
                        item = (self._form_menu_item_title(form),
                                dict(form_class=form_class, name=name))
                        self._update_recent_forms(item)
        except UserBreakException:
            pass
        except SystemExit:
            raise
        except Exception:
            top_level_exception()
        return result

    def _can_new_record(self, name, **kwargs):
        try:
            return has_access(name, perm=pytis.data.Permission.INSERT)
        except ResolverError:
            # The spec is invalid, but we want the crash on attempt to run it.
            return True

    def _cmd_new_record(self, name, prefill=None, inserted_data=None, multi_insert=True,
                        block_on_new_record=False, transaction=None, spec_kwargs={},
                        copied_row=None, set_values=None):
        # See new_record() for documentation.
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
                result = self._cmd_new_record(name, prefill=result, inserted_data=inserted_data,
                                              multi_insert=multi_insert, block_on_new_record=True,
                                              transaction=transaction, spec_kwargs=spec_kwargs,
                                              copied_row=copied_row, set_values=set_values)
            else:
                Application.COMMAND_REFRESH.invoke(interactive=False)
        else:
            if view.arguments() is not None:
                message(_("This form doesn't allow insertion."), beep_=True)
                return None
            result = run_form(pytis.form.PopupInsertForm, name,
                              prefill=prefill, inserted_data=inserted_data,
                              multi_insert=multi_insert, transaction=transaction,
                              spec_kwargs=spec_kwargs, set_values=set_values)
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
            message(_("Running procedure..."), root=True, timeout=2)
            log(ACTION, 'Running procedure:',
                (spec_name, proc_name, args, kwargs))
            # Kvůli wx.SafeYield() se ztrácí focus, takže
            # si ho uložíme a pak zase obnovíme.
            focused = wx_focused_window()
            wx_yield_()
            try:
                proc = pytis.config.resolver.get_object(spec_name, proc_name)
            except ResolverError:
                # Legacy procedure definitions
                spec = pytis.config.resolver.get(spec_name, 'proc_spec')
                assert isinstance(spec, dict), spec
                assert proc_name in spec, (proc_name, spec)
                proc = spec[proc_name]
            if block_refresh_:
                result = block_refresh(proc, *args, **kwargs)
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
        init_access_rights(pytis.config.dbconnection)
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
        menu = Menu(_("Recently opened forms"),
                    self._recent_forms_menu_items(), allow_autoindex=False)
        self._recent_forms_menu = menu
        return menu

    def wx_frame(self):
        """Return the main application frame as 'wx.Frame' instance."""
        return self._frame

    def login_hook(self, success):
        if not self._login_success:
            self._specification.login_hook(success)
            if success:
                self._login_success = True

    def profile_manager(self):
        return self._profile_manager

    def form_settings_manager(self):
        return self._form_settings_manager

    def aggregated_views_manager(self):
        return self._aggregated_views_manager

    def decrypted_names(self):
        """Return set of encryption names the user has access to."""
        return self._decrypted_names

    def log(self, spec_name, form_name, action, info=None):
        if self._logger:
            self._logger.log(spec_name, form_name, action, info=info)
        else:
            log(ACTION, "Form action:", (spec_name, form_name, action, info))

    def custom_command(self, name):
        """Return the custom application command referred by 'name' as a pair (CMD, ARGS).

        The current application specification must define a method named 'cmd_'
        + name, which returns the command and its arguments as a tuple of two
        items -- (CMD, ARGS), where CMD is a 'pytis.form.Command' instance and
        ARGS is a dictionaty of its arguments.  This pair is then returned as
        the result of this method.  None is returned if no such command is
        defined by the current application.

        """
        try:
            method = getattr(self._specification, 'cmd_' + name)
        except AttributeError:
            try:
                return pytis.config.resolver.get('app_commands', name)
            except ResolverError:
                return None
        command, args = method()
        assert isinstance(command, pytis.form.Command), command
        assert isinstance(args, dict), args
        return command, args

    def _check_x2goclient(self):
        if pytis.remote.client_available():
            version_template = '%Y-%m-%d %H:%M'
            version = pytis.remote.x2goclient_version()
            try:
                version_date = datetime.datetime.strptime(version, version_template)
                required_version = pytis.x2goclient.X2GOCLIENT_REQUIRED_VERSION
            except Exception:
                return
            required_date = datetime.datetime.strptime(required_version, version_template)
            if version_date < required_date:
                msg = _("BEWARE!\n\n"
                        "You are running an incompatible version of Pytis2go (%s).\n"
                        "Some functionality may not be available and errors may occur.\n\n"
                        "Please perform Pytis2go update during application startup.\n\n"
                        "Shall the application be terminated now to allow Pytis2go update?",
                        version)
                if pytis.form.run_dialog(Question, msg):
                    self.COMMAND_EXIT.invoke()


class ApplicationProxy(object):
    """Proxy to access the public API of the current wx application instance.

    An instance of this class is available as 'pytis.form.app'.  The instance
    exists at the module level even if no actual application is running (yet).

    The recommended usage is importing app to the application code as:

    from pytis.form import app

    Then you can access the public 'Application' API methods and attributes
    through the 'app' object.

    """
    # TODO: Define the public methods and attributes using a decorator.
    _PUBLIC_API = ('param',)

    def __getattr__(self, name):
        if name not in self._PUBLIC_API:
            raise AttributeError("'%s' object has no attribute '%s'" %
                                 (self.__class__.__name__, name))
        return getattr(_application, name)


class DBParams(object):
    """Provides access to shared parameters.

    Shared parameters provide a way to share global and user specific values
    between the database and the application code.  Each
    'pytis.presentation.SharedParams' instance defined in
    'Application.params()' leads to creation of one 'DBParams' instance as
    'app.param.name', where 'app' is the current 'Application' instance and
    'name' corresponds to the name defined by the 'SharedParams' instance.

    The 'DBParams' instance provides access to the parameter values through its
    public attributes.  Their names correspond to the names of the columns
    present in the data object represented by the instance.  When read, the
    attributes return the internal Python value of the column, when assigned,
    they update the value in the database.

    It is not possible to us a parameter named 'add_callback' and 'cbvalue' due
    to the presence of the public methods of the same name.

    """
    _lock = _thread.allocate_lock()

    def __init__(self, name, condition=None):
        self._name = name
        self._condition = condition
        self._callbacks = {}

    def __getattr__(self, name):
        if name in ('_row', '_data'):
            self._data = pytis.util.data_object(self._name)
            self._data.add_callback_on_change(self._on_change)
            self._select()
            return self.__dict__[name]
        elif name in self._row:
            return self._row[name].value()
        else:
            raise AttributeError("'%s' object for '%s' has no attribute '%s'" %
                                 (self.__class__.__name__, self._name, name))

    def __setattr__(self, name, value):
        if not name.startswith('_') and name in self._row:
            row = pytis.data.Row(((name, pytis.data.Value(self._row[name].type(), value)),))
            key = [self._row[c.id()] for c in self._data.key()]
            with pytis.util.Locked(self._lock):
                self._row = self._data.update(key, row)[0]
        else:
            super(DBParams, self).__setattr__(name, value)

    def __contains__(self, key):
        return key in self._row

    def _select(self):
        data = self._data
        with pytis.util.Locked(self._lock):
            data.select(condition=self._condition)
            self._row = data.fetchone()
            data.close()

    def _on_change(self):
        orig_row = self._row
        self._select()
        for name, callbacks in self._callbacks.items():
            if self._row[name].value() != orig_row[name].value():
                for callback in callbacks:
                    callback()

    def add_callback(self, name, callback):
        """Registger a callback called on given parameter change.

        The callback function is called without arguments whenever the value of
        given parameter changes.

        """
        assert name is None or name in self._row
        self._callbacks.setdefault(name, []).append(callback)

    def cbvalue(self, name, cbcolumn):
        """Return the value of given codebook column for given parameter.

        Arguments:

          name -- name of the parameter with a codebook (enumerator).
          cbcolumn -- codebook column name

        ValueError is raised if given column has no enunerator in the
        underlying data object.

        None is returned if the codebook doesn't include the record for the
        current value of parameter 'name'.

        """
        value = self._row[name]
        enumerator = value.type().enumerator()
        if not enumerator:
            raise ValueError("Column '%s' has no enumerator!" % name)
        row = enumerator.row(value.value())
        if not row:
            return None
        return row[cbcolumn].value()


class DbActionLogger(object):

    """Log user actions into the database."""

    def __init__(self, dbconnection, username):
        factory = pytis.config.resolver.get('pytis.defs.logging.FormActionLog', 'data_spec')
        self._data = factory.create(connection_data=pytis.config.dbconnection)
        self._username = username

    def _values(self, **kwargs):
        return [(key, pytis.data.Value(self._data.find_column(key).type(), value))
                for key, value in [('username', self._username)] + kwargs.items()]

    def log(self, spec_name, form_name, action, info=None):
        rdata = (('timestamp', pytis.data.dtval(pytis.data.DateTime.datetime())),
                 ('username', pytis.data.sval(self._username)),
                 ('spec_name', pytis.data.sval(spec_name)),
                 ('form_name', pytis.data.sval(form_name)),
                 ('action', pytis.data.sval(action)),
                 ('info', pytis.data.sval(info)))
        row = pytis.data.Row(rdata)
        result, success = self._data.insert(row)
        if not success:
            raise pytis.data.DBException(result)


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
        message(_("Opening form refused."), beep_=True)
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
    assert 'args' not in kwargs, "The keyword argument 'args' is reserved for internal use!"
    return Application.COMMAND_RUN_PROCEDURE.invoke(spec_name=spec_name,
                                                    proc_name=proc_name,
                                                    args=args, **kwargs)


def new_record(name, prefill=None, inserted_data=None, multi_insert=True,
               block_on_new_record=False, transaction=None, spec_kwargs={},
               copied_row=None, set_values=None):
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
                  question=_("Are you sure to delete the record permanently?")):
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
        elif isinstance(result, basestring):
            run_dialog(Error, result)
            return False
        elif isinstance(result, pytis.data.Operator):
            ask = False
            op, arg = data.delete_many, result
        else:
            raise ProgramError("Invalid 'on_delete_record' return value.", result)
    else:
        if data.arguments() is not None:
            message(_("This form doesn't allow deletion."), beep_=True)
            return False
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
    Application.COMMAND_REFRESH.invoke(interactive=False)


def exit():
    """Ukonči uživatelské rozhraní aplikace."""
    return Application.COMMAND_EXIT.invoke()


def db_operation(operation, *args, **kwargs):
    in_transaction = (kwargs.get('transaction') is not None)
    return db_op(operation, args, kwargs, in_transaction=in_transaction)


def db_op(operation, args=(), kwargs={}, in_transaction=False, quiet=False):
    """Invoke database operation with handling possible exceptions.

    'operation' is called with given arguments.  If 'pytis.data.dbdata.DBException'
    is raised during the operation, an error dialog is displayed with exception description and
    a question asking whether the user wishes to re-invoke the operation.  The operation is repeated
    as long as the user answers the question positively.

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
                if _application._log_login:
                    log(ACTION, "Login action:", (pytis.config.dbschemas, 'True'))
                    _application._log_login = False
                _application.login_hook(success=True)
            return True, result
        except pytis.data.DataAccessException as e:
            run_dialog(Error, _("Access denied"))
            return FAILURE
        except pytis.data.DBLoginException as e:
            if pytis.config.dbconnection.password() is not None and _application:
                log(ACTION, "Login action:", (pytis.config.dbschemas, 'False'))
                _application.login_hook(success=False)
            if pytis.config.login_selection:
                logins = [x[0] if isinstance(x, tuple) else x
                          for x in pytis.config.login_selection]
                passwords = dict([x for x in pytis.config.login_selection
                                  if isinstance(x, tuple)])
                login_enumerator = pytis.data.FixedEnumerator(logins)
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
            login_result = run_form(
                pytis.form.InputForm, title=_("Log in for database access"),
                fields=(Field('login', _("Login"), width=24, not_null=True,
                              enumerator=login_enumerator, default=default_login),
                        Field('password', _("Password"), type=pytis.data.Password(verify=False),
                              editable=password_editable, computer=password_computer,
                              default=default_password, width=24, not_null=True),),
                focus_field='password',
            )
            if not login_result:
                return FAILURE
            pytis.config.dbconnection.update_login_data(user=login_result['login'].value(),
                                                        password=login_result['password'].value())
            pytis.config.dbconnection = pytis.config.dbconnection  # mark as changed
            pytis.config.dbuser = pytis.config.dbconnection.user()
        except pytis.data.DBException as e:
            log(OPERATIONAL, "Database exception in db_operation", format_traceback())
            message = e.message()
            if e.exception():
                message += '\n' + str(e.exception())
            if quiet:
                return FAILURE
            if in_transaction:
                run_dialog(Message, message, title=_("Database error"),
                           icon=pytis.form.Message.ICON_ERROR)
                return FAILURE
            else:
                message += '\n' + _("Try again?")
                if not run_dialog(Question, message, title=_("Database error"),
                                  icon=pytis.form.Question.ICON_ERROR):
                    return FAILURE


def delete_record_question(msg=None):
    """Zeptej se uživatele, zda má být opravdu smazán záznam.

    Vrať pravdu, právě když uživatel odpoví kladně.

    """
    log(EVENT, 'Record deletion dialog')
    if msg is None:
        msg = _("Are you sure to delete the record permanently?")
    if not run_dialog(Question, msg):
        log(EVENT, 'Record deletion refused by user')
        return False
    log(EVENT, u'Record deletion confirmed by user')
    return True


# Funkce, které jsou obrazem veřejných metod aktuální aplikace.


def run_dialog(arg1, *args, **kwargs):
    """Zobraz dialog v okně aplikace (viz 'Application.run_dialog()')."""
    if _application is None:
        log(OPERATIONAL, "Attempt to run a dialog:", (arg1, args, kwargs))
    elif arg1 == InputDialog:
        return input_text(title=kwargs.get('message'),
                          label=kwargs.get('prompt', '').rstrip(':'),
                          default=kwargs.get('value'),
                          width=kwargs.get('input_width'),
                          height=kwargs.get('input_height'))
    elif arg1 == InputNumeric:
        precision = kwargs.get('decimal_width', 0)
        minimum = kwargs.get('min_value')
        maximum = kwargs.get('max_value')
        if precision:
            t = pytis.data.Float(precision=precision)
            cast = float
        else:
            t = pytis.data.Integer()
            cast = int
        value = input_number(title=kwargs.get('message'),
                             label=kwargs.get('prompt', '').rstrip(':'),
                             width=kwargs.get('integer_width', 10) + precision + 1,
                             precision=precision,
                             minimum=cast(minimum) if minimum else None,
                             maximum=cast(maximum) if maximum else None,
                             avoid_initial_selection=kwargs.get('select_on_entry', False),
                             default=kwargs.get('value'))
        return pytis.data.Value(t, value)

    elif arg1 == InputDate:
        # Backwards compatibility hack.
        value = kwargs.get('value')
        if value:
            value, error = pytis.data.Date().validate(value)
            if value:
                value = value.value()
        value = input_date(title=kwargs.get('message'),
                           label=kwargs.get('prompt', '').rstrip(':'),
                           default=value)
        return pytis.data.Value(pytis.data.Date(), value)
    else:
        return _application.run_dialog(arg1, *args, **kwargs)


class InputDialog(object):
    """Legacy hack for replacing the DEPRECATED InputDialog by InputForm."""
    pass


class InputNumeric(object):
    """Legacy hack for replacing the DEPRECATED InputNumeric dialog by InputForm."""
    pass


class InputDate(object):
    """Legacy hack for replacing the DEPRECATED InputDate dialog by InputForm."""
    pass


def input_text(title, label, default=None, not_null=False, width=20, height=1, descr=None,
               avoid_initial_selection=True):
    """Display a form for entering a single textual value and return this value.

    Arguments:
      title -- input form main title (basestring).
      label -- field label (basestring).
      default -- initial field value (basestring).
      not_null -- iff True, it will not be possible to submit the form without
        entering a value.
      width -- input field width (number of characters).
      height -- input field height (number of characters).
      descr -- field description displayed in a tooltip of a blue icon right
        from the field.
      avoid_initial_selection -- the input field value is by default initially
        selected, which results in overwriting the whole value when the user
        starts typing.  Passing False here avoids this initial selection.

    Returns the value entered into the field as a basestring or None if the
    form was escaped or the value was empty (only possible when not_null is
    False).

    """
    row = run_form(
        pytis.form.InputForm, title=title, fields=(
            Field('text', label, default=default, type=pytis.data.String(not_null=not_null),
                  width=width, height=height, descr=descr),
        ),
        avoid_initial_selection=avoid_initial_selection,
    )
    if row:
        return row['text'].value()
    else:
        return None


def input_number(title, label, default=None, not_null=True, width=14, precision=None,
                 minimum=None, maximum=None, descr=None, avoid_initial_selection=True):
    """Display a form for entering a single numeric value and return this value.

    Arguments:
      title -- input form main title (basestring).
      label -- field label (basestring).
      default -- initial field value as int or float (float when
        precision is given).
      not_null -- iff True, it will not be possible to submit the form without
        entering a value.
      width -- total input field width (number of characters).
      precision -- number of digits after decimal point or None for an integer field.
      minimum -- minimal value; 'None' denotes no limit.
      maximum -- maximal value; 'None' denotes no limit.
      descr -- field description displayed in a tooltip of a blue icon right
        from the field.
      avoid_initial_selection -- the input field value is by default initially
        selected, which results in overwriting the whole value when the user
        starts typing.  Passing False here avoids this initial selection.

    Returns the value entered into the field as int or float (float when
    precision was given) or None if the form was escaped or the value was empty
    (only possible when not_null is False).

    """
    if precision:
        quantizer = decimal.Decimal('1.' + '0' * precision)

        def quantize(number):
            if number is not None:
                return decimal.Decimal(number).quantize(quantizer)
            else:
                return None

        t = pytis.data.Float(precision=precision, not_null=not_null,
                             minimum=quantize(minimum), maximum=quantize(maximum))
    else:
        t = pytis.data.Integer(not_null=not_null, minimum=minimum, maximum=maximum)
    row = run_form(
        pytis.form.InputForm, title=title, fields=(
            Field('number', label, default=default, type=t, width=width, descr=descr),
        ),
        avoid_initial_selection=avoid_initial_selection,
    )
    if row:
        return row['number'].value()
    else:
        return None


def input_date(title, label, default=None, not_null=True, descr=None,
               avoid_initial_selection=True):
    """Display a form for entering a date and return this value.

    Arguments:
      title -- input form main title (basestring).
      label -- field label (basestring).
      default -- initial field value as 'datetime.date' or None.
      not_null -- iff True, it will not be possible to submit the form without
        entering a value.
      descr -- field description displayed in a tooltip of a blue icon right
        from the field.
      avoid_initial_selection -- the input field value is by default initially
        selected, which results in overwriting the whole value when the user
        starts typing.  Passing False here avoids this initial selection.

    Returns the value entered into the field as a 'datetime.date' instance or
    None if the form was escaped or the value was empty (only possible when
    not_null is False).

    """
    row = run_form(
        pytis.form.InputForm, title=title, fields=(
            Field('date', label, default=default, type=pytis.data.Date(not_null=not_null),
                  descr=descr),
        ),
        avoid_initial_selection=avoid_initial_selection,
    )
    if row:
        return row['date'].value()
    else:
        return None


def current_form(**kwargs):
    """Vrať právě aktivní formulář (viz 'Application.current_form()')."""
    if _application is not None:
        return _application.current_form(**kwargs)


def top_window(**kwargs):
    """Vrať aktivní okno aplikace (formulář, nebo dialog)."""
    if _application is not None:
        return _application.top_window(**kwargs)


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


def profile_manager():
    """Return 'Application.profile_manager()' of the current application instance."""
    return _application.profile_manager()


def form_settings_manager():
    """Return 'Application.form_settings_manager()' of the current application instance."""
    return _application.form_settings_manager()


def aggregated_views_manager():
    """Return 'Application.aggregated_views_manager()' of the current application instance."""
    return _application.aggregated_views_manager()


def decrypted_names():
    """Return set of encryption names the user has access to."""
    return _application.decrypted_names()


def log_user_action(spec_name, form_name, action, info=None):
    """Log user action into the database."""
    return _application.log(spec_name, form_name, action, info=info)


def frame_title(title):
    """Set title of the main application frame"""
    _application._frame.SetTitle(_application._frame_title(title))


def close_forms():
    """Close all currently opened forms."""
    return _application._close_forms()

# Ostatní funkce.


def set_status(id, text, log_=True, **kwargs):
    """Set status bar field 'id' to display given 'text'."""
    if __debug__:
        if log_:
            log(DEBUG, u"Status text updated:", (id, text))
    return _application._statusbar.set_status(id, text, **kwargs)


def refresh_status(id=None):
    """Refresh given status bar field or all fields if 'id' is None."""
    return _application._statusbar.refresh(id)


def message(message, kind=EVENT, data=None, beep_=False, timeout=None,
            root=False, log_=True):
    """Display a non-interactive message in the status bar 'message' field.

    Arguments:

      message -- the text to be displayed; if the text ends with a colon,
        it will be stripped.
      beep_ -- iff true, the message will be accompanied by a beep.
      log_ -- iff true, the text will be also logged.
      kind -- logging kind; one of 'log' module constants.
      data -- additional data to be logged (same as in 'log.log').
      timeout -- optional timeout after which the message disappears.
      root -- iff true, the message is displayed always in the main
        application frame's status bar.  Otherwise the message will
        appear in the current modal window's status bar if there is
        a modal window, it has a status bar and the status bar
        contains the 'message' field.

    """
    if beep_:
        beep()
    if log_ and (message or data):
        log(kind, message, data=data)
    if not _application:
        return
    if message and message[-1] == ':':
        message = message[:-1]
    if not root:
        modal = _application._modals.top()
        if isinstance(modal, pytis.form.Form) and modal.set_status('message', message):
            return
    sb = _application._statusbar
    if not sb:
        return
    if timeout:
        class Timer(wx.Timer):
            def Notify(self):
                self.Stop()
                if sb.get_status_text('message') == unistr(message):
                    sb.set_status('message', '')
        Timer().Start(1000 * timeout)
    return sb.set_status('message', message)


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
    return pytis.form.Refreshable.block_refresh(function, *args, **kwargs)


_access_rights = None
_access_dbconnection = None
_user_roles = ()


def _dump_rights():
    import pytis.extensions
    registered_shortnames = set()
    if _access_rights not in (None, 'nonuser',):
        registered_shortnames = registered_shortnames.union(_access_rights.keys())
    if Specification._access_rights not in (None, 'nonuser'):
        registered_shortnames = registered_shortnames.union(Specification._access_rights.keys())
    resolver = pytis.config.resolver
    output = sys.stderr
    output.write("--- BEGIN list of registered rights ---\n")
    output.write("# source shortname right column permitted\n")

    def find_columns(spec_name):
        try:
            specification = resolver.specification(spec_name)
        except ResolverError:
            specification = None
        if specification is None:
            columns = []
        else:
            columns = [f.id() for f in specification.view_spec().fields()]
        return columns
    all_permissions = pytis.data.Permission.all_permissions()
    for shortname in registered_shortnames:
        if shortname.startswith('form/'):
            columns = find_columns(shortname[5:])
        else:
            columns = []
        for permission in all_permissions:
            permitted = action_has_access(shortname, permission)
            output.write('actions %s %s None %s\n' % (shortname, permission, permitted,))
            for c in columns:
                permitted = action_has_access(shortname, permission, c)
                output.write('actions %s %s %s %s\n' % (shortname, permission, c, permitted,))
    specification_names = pytis.extensions.get_form_defs()
    for spec_name in specification_names:
        columns = find_columns(spec_name)
        for permission in all_permissions:
            permitted = has_access(spec_name, permission)
            output.write('specifications %s %s None %s\n' % (spec_name, permission, permitted,))
            for c in columns:
                permitted = has_access(spec_name, permission, c)
                output.write('specifications %s %s %s %s\n' %
                             (spec_name, permission, c, permitted,))
    output.write("--- END list of registered rights ---\n")


def init_access_rights(connection_data):
    """Read application access rights from the database.

    This function must be called very early after start of an application.

    Arguments:

      connection_data -- 'pytis.data.DBConnection' instance

    """
    global _access_rights, _user_roles, _access_dbconnection
    _access_dbconnection = connection_data
    if not pytis.config.use_dmp_roles:
        return
    try:
        roles_data = pytis.data.dbtable('ev_pytis_user_roles', ('roleid',), connection_data)
        roles = [row[0].value() for row in roles_data.select_map(identity)]
    except pytis.data.DBException:
        return
    if not roles:
        _access_rights = 'nonuser'
        return
    _user_roles = roles
    if not pytis.config.use_dmp_rights:
        return
    S = pytis.data.String()
    _access_rights = {}
    # Prefill _access_rights so that default access by specification rights in
    # has_action_access is possible only for shortnames without any rights
    # defined in DMP.
    actions_data = pytis.data.dbtable('e_pytis_action_rights', ('shortname', 'status',),
                                      connection_data)
    condition = pytis.data.LE('status', pytis.data.Value(pytis.data.Integer(), 0))
    for value in actions_data.distinct('shortname', condition=condition):
        _access_rights[value.value()] = {}
    rights_data = pytis.data.dbtable('pytis_view_user_rights',
                                     (('shortname', S,), ('rights', S,), ('columns', S,),),
                                     connection_data, arguments=())

    def process(row):
        shortname, rights_string, columns_string = row[0].value(), row[1].value(), row[2].value()
        if columns_string:
            columns = columns_string.split(' ')
        else:
            columns = [None]
        rights = [r.upper() for r in rights_string.split(' ') if r != 'show']
        action_rights = _access_rights[shortname] = _access_rights.get(shortname, {})
        relaxed_action_rights = action_rights.get(True)
        if relaxed_action_rights is None:
            # Relaxed access rights are access rights to the action as a whole.
            # The action is accessible if it is accessible itself or if any of
            # its columns is accessible.
            action_rights[True] = relaxed_action_rights = []
        for c in columns:
            action_rights[c] = rights
            for r in rights:
                if r not in relaxed_action_rights:
                    relaxed_action_rights.append(r)
    rights_data.select_map(process)
    Specification._init_access_rights(connection_data)
    pytis.config.resolver.clear()
    if pytis.config.debug:
        _dump_rights()


def has_access(name, perm=pytis.data.Permission.VIEW, column=None):
    """Return true if the current user has given permission for given form specification.

    Arguments:

      name -- specification name as a string.  May also be a dual name
        (containing `::').  In such a case, the permission is checked for both
        names and 'column=None' is assumed regardless of the actual 'column'
        value.
      perm -- access permission as one of `pytis.data.Permission' constants.
      column -- string identifier of the column to check or 'None' (no specific
        column checked)

    Raises 'ResolverError' if given specification name cannot be found.

    """
    if not action_has_access('form/' + name, perm=perm, column=column):
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
        try:
            rights = pytis.config.resolver.get(name, 'data_spec').access_rights()
        except ResolverError:
            rights = None
        if rights:
            if _access_dbconnection is None:
                init_access_rights(pytis.config.dbconnection)
            groups = pytis.data.default_access_groups(_access_dbconnection)
            if not rights.permitted(perm, groups, column=column):
                return False
    result = action_has_access('form/' + name, perm=perm, column=column)
    return result


def action_has_access(action, perm=pytis.data.Permission.CALL, column=None):
    """Return true iff 'action' has 'perm' permission.

    Arguments:

      action -- action identifier, string
      perm -- access permission as one of `pytis.data.Permission' constants
      column -- string identifier of the column to check or 'None' (no specific
        column checked)

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
            if column is None:
                permissions = rights.get(True, ())
            else:
                permissions = rights.get(column, None)
                if permissions is None:
                    permissions = rights.get(None, ())
            result = perm in permissions
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
        _yield_lock = _thread.allocate_lock()
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


def custom_command(name):
    return _application.custom_command(name)


def built_in_status_fields():
    """Return the built-in status bar fields as a tuple of 'StatusField' instances.

    The following status bar fields are defined by this method:
      - message: displays various non-interactive messages
        set by 'pytis.form.message()'.
      - list-positin: Displays the current position in the list of
        records (such as 3/168) when a list form is active.
      - remote-status: Displays the current status of remote communication.

    """
    def _refresh_list_position():
        form = current_form(allow_modal=False)
        if hasattr(form, 'list_position'):
            return form.list_position()
        else:
            return ''

    def _refresh_remote_status():
        rpc_info = pytis.remote.RPCInfo
        last_status, last_time = rpc_info.remote_status_info
        if pytis.remote.client_available():
            if not last_status:
                last_time = time.time()
                rpc_info.remote_status_info = (True, last_time)
            if rpc_info.remote_client_version:
                version = rpc_info.remote_client_version
            else:
                try:
                    version = pytis.remote.x2goclient_version()
                    rpc_info.remote_client_version = version
                except Exception:
                    version = _("Not available")
            status = _("Ok")
            icon = 'status-online'
            tooltip = _("Connected.") + "\n" + _("Client version: %s", version)
        else:
            if last_status:
                last_time = time.time()
                rpc_info.remote_status_info = (False, time.time())
            tooltip = _("Not available.")
            status = _("N/A")
            icon = 'status-error'
        changed = time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(last_time))
        tooltip += '\n' + _("Status changed: %s", changed)
        return (status, icon, tooltip)

    def _refresh_user_config():
        tooltip = "\n".join((_("Username: %s", pytis.config.dbuser),
                             _("Database name: %s", pytis.config.dbname),
                             _("Database host: %s", pytis.config.dbhost or 'localhost')))
        return (pytis.config.dbuser, 'user-icon', tooltip)

    return (
        StatusField('message', width=None),
        StatusField('list-position', _("List position"),
                    refresh=_refresh_list_position, width=15),
        StatusField('user', _("User and database parameters"),
                    refresh=_refresh_user_config,
                    refresh_interval=10000000, width=15),
        StatusField('remote-status', _("Remote communication status"),
                    refresh=_refresh_remote_status,
                    refresh_interval=10000, width=8),
    )

# Duplication of application methods here is a huge mess, so we
# don't do so for the functions below.  If the 'application' object
# is made public in future (which seems most desirable), these
# functions can be turned into its methods.


def remote_connection_initially_available():
    """Return True if the remote connection was available at application startup."""
    return pytis.remote.RPCInfo.remote_connection_initially_available


def get_recent_directory(key):
    """Return the last directory set for given 'key' as a string or None."""
    return _application._recent_directories.get(key)


def set_recent_directory(key, directory):
    """Remember given 'directory' for given 'key'."""
    _application._recent_directories[key] = directory


def menu():
    """Return the application menu structure as a sequence of 'Menu' instances."""
    return _application._menu
