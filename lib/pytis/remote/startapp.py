#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011-2017 Brailcom, o.p.s.
# Copyright (C) 2010-2014 by Mike Gabriel <mike.gabriel@das-netzwerkteam.de>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
import wx
import collections
import string

def _(x, *args, **kwargs):
    return x % (args or kwargs) if args or kwargs else x

class ui(object):
    """Private helper methods for simple UI construction (not to be used outside this module)."""

    @staticmethod
    def _add_to_sizer(sizer, items, padding=None, spacing=0):
        for item in items:
            if isinstance(item, ui.spacer):
                sizer.AddSpacer(item.size)
                continue
            if not isinstance(item, ui.item):
                item = ui.item(item)
            if item.content is not None:
                flag = 0
                if item.expand:
                    flag |= wx.EXPAND
                if item.center:
                    flag |= wx.ALIGN_CENTER
                border = space = 0
                if item.padding is not None:
                    if isinstance(item.padding, tuple):
                        if sizer.GetOrientation() == wx.VERTICAL:
                            flag |= wx.LEFT | wx.RIGHT
                            space, border = item.padding
                        else:
                            flag |= wx.TOP | wx.BOTTOM
                            border, space = item.padding
                    else:
                        flag |= wx.ALL
                        border = item.padding
                if spacing and sizer.GetItemCount() != 0:
                    sizer.AddSpacer(spacing + space)
                elif space:
                    sizer.AddSpacer(space)
                sizer.Add(item.content, item.proportion, flag, border)
                if space:
                    sizer.AddSpacer(space)
        if padding:
            sizer = ui.hgroup(ui.item(sizer, padding=padding, proportion=1, expand=True))
        return sizer

    class item(object):
        """Item of 'ui.hgroup' or 'ui.vgroup' providing more arrangement information.

        Arguments:

          content -- the actual content to be placed into the group ('wx.Window' or nested
            'wx.Sizer').
          proportion -- indicate if this item can change its size in the main
            orientation of the group, where 0 stands for not changeable and a
            value of more than zero is interpreted relative (a proportion of
            the total) to the value of other items of the same group.  This
            makes the item stretchable vertically in a vertical group or
            horizontally in a horizontal group.  The value determines the share
            of the space among other stretchable items in the group.
          expand -- resize the item to fit the full space available to the
            sizer in the opposite direction to the direction of the group
            (width in a vertical group and height in a horizontal group).
          padding -- margin around the item in pixels or as a tuple of two int
            values, where the first value determines the padding at the top and
            bottom and the second value determines the padding on right and
            left side.  Tho order of these velue is the same in a vertical as
            well as in a horizontal group.
          center -- iff True, center the item content inside the space
            available for it.

        """
        def __init__(self, content, proportion=0, expand=False, padding=None, center=False):
            assert (isinstance(content, (wx.Window, wx.Sizer, wx.Size)) or
                    isinstance(content, (tuple, list)) and len(content) == 2
                    and all(isinstance(x, int) for x in content) or content is None), content
            self.content = content
            self.proportion = proportion
            self.expand = expand
            self.padding = padding
            self.center = center

    class spacer(object):
        def __init__(self, size):
            self.size = size

    @staticmethod
    def vgroup(*items, **kwargs):
        """Arrange UI items into a vertical group returning 'wx.BoxSizer'.

        Arguments:

          items -- sequence of group items as instances of 'wx.Window', nested
            'wx.Sizer' or 'ui.item' (when more control of placing the item
            within the group is needed (see 'ui.item' arguments).  Items may
            also be None in which case they are ignored.
          spacing -- space between individual items in the grouping direction
            (vertical in this method) in pixels (int).
          padding -- margin around the whole group in pixels or as a tuple of
            two int values, where the first value determines the padding at the
            top and bottom and the second value determines the padding on right
            and left side.  Tho order of these velue is the same in a vertical
            as well as in a horizontal group.

        """
        return ui._add_to_sizer(wx.BoxSizer(wx.VERTICAL), items, **kwargs)

    @staticmethod
    def hgroup(*items, **kwargs):
        """Arrange UI items into a horizontal group returning 'wx.BoxSizer'.

        See 'ui.vgroup' for more information with the only difference that the
        items are arranged horizontally in this case.

        """
        return ui._add_to_sizer(wx.BoxSizer(wx.HORIZONTAL), items, **kwargs)

    @staticmethod
    def vbox(parent, label, items):
        box = wx.StaticBox(parent, label=label)
        return ui._add_to_sizer(wx.StaticBoxSizer(box, wx.VERTICAL), items)

    @staticmethod
    def panel(parent, method, *args, **kwargs):
        panel = wx.Panel(parent)
        sizer = method(panel, *args, **kwargs)
        panel.SetSizer(sizer)
        return panel

    @staticmethod
    def field(parent, value=None, length=20, style=wx.DEFAULT, disabled=False, on_enter=None):
        if on_enter:
            style |= wx.TE_PROCESS_ENTER
        ctrl = wx.TextCtrl(parent, -1, value or '', style=style)
        width, height = parent.GetTextExtent('x' * length)
        ctrl.SetMinSize((width, ctrl.GetSize().height))
        if disabled:
            ctrl.Enable(False)
        if on_enter:
            ctrl.Bind(wx.EVT_TEXT_ENTER, on_enter)
        return ctrl

    @staticmethod
    def button(parent, label, callback, updateui=None, icon=None, disabled=False):
        button = wx.Button(parent, -1, label=label)
        if icon:
            # This doesn't seem to work...
            bitmap = wx.ArtProvider_GetBitmap(icon, wx.ART_TOOLBAR, (16, 16))
            button.SetBitmap(bitmap)
        button.Bind(wx.EVT_BUTTON, callback)
        if updateui:
            button.Bind(wx.EVT_UPDATE_UI, updateui)
        if disabled:
            button.Enable(False)
        return button

    @staticmethod
    def label(parent, text, size=None, face=None, bold=False, italic=False, underline=False):
        label = wx.StaticText(parent, -1, text)
        if size or face or bold or italic or underline:
            label.SetFont(wx.Font(size, wx.DEFAULT, faceName=(face or ''), underline=underline,
                                  style=wx.ITALIC if italic else wx.NORMAL,  # wx.SLANT
                                  weight=wx.BOLD if bold else wx.NORMAL))  # wx.LIGHT
        return label

    @staticmethod
    def listbox(parent, choices=(), on_select=None):
        def on_key_up(event):
            if ((listbox.GetSelection() != -1 and
                 event.GetKeyCode() in (wx.WXK_SPACE, wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER))):
                on_select(event)
            event.Skip()

        def on_dclick(event):
            if listbox.GetSelection() != -1:
                on_select(event)
            event.Skip()

        listbox = wx.ListBox(parent, -1, choices=choices, style=wx.LB_SINGLE)
        # Bind to EVT_UPDATE_UP as EVT_KEY_DOWN doesn't process the Enter key...
        if on_select:
            listbox.Bind(wx.EVT_KEY_UP, on_key_up)
            listbox.Bind(wx.EVT_LEFT_DCLICK, on_dclick)
        return listbox

    @staticmethod
    def checklist(parent, columns, items):
        """Create a control to check/uncheck individual items in a tabular list.

        Arguments:
          columns -- sequence of column labels -- selectable items may contain
            multiple values to be displayed in a tabular layout in columns.
            This sequence defines the column headers.
          items -- sequence of sequences, where the top level sequence
            determines the options which may be checked/unchecked individually
            (table rows) and the inner sequences determine the values displayed
            in table columns for given row plus the initial checbox state.  The
            first value in each inner sequence is a bool (True for a checked
            item, False for unchecked) and the following are the string values
            for table columns.  Thus each inner sequence has n + 1 items where
            n is the length of 'columns'.

        """
        import wx.lib.mixins.listctrl
        class CheckListCtrl(wx.ListCtrl, wx.lib.mixins.listctrl.CheckListCtrlMixin):
            def __init__(self, parent, columns, items):
                wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_REPORT)
                wx.lib.mixins.listctrl.CheckListCtrlMixin.__init__(self)
                self.Bind(wx.EVT_LIST_ITEM_ACTIVATED, lambda e: self.ToggleItem(e.m_itemIndex))
                for i, label in enumerate(columns):
                    self.InsertColumn(i, label)
                for i, item in enumerate(items):
                    self.InsertStringItem(i, item[1])
                    self.CheckItem(i, item[0])
                    for j, value in enumerate(item[1:]):
                        self.SetStringItem(i, j, value)
                width = 4
                for i in range(len(columns)):
                    self.SetColumnWidth(i, wx.LIST_AUTOSIZE)
                    width += self.GetColumnWidth(i)
                height = min(320, len(items) * 27 + 31)
                # The resulting height is (at least on Linux) actually 40px less than
                # what we request, so request 40px more (yet another wx weirdness...).
                self.SetMinSize((width, height + 40))
        return CheckListCtrl(parent, columns, items)


class X2GoStartApp(wx.App):
    """X2Go startup application."""

    _MAX_PROGRESS = 40

    def __init__(self, args, session_parameters, force_parameters, backends):
        from pytis.remote.x2goclient import StartupController
        self._progress = 1
        self._args = args
        self._controller = StartupController(self, session_parameters,
                                             force_parameters=force_parameters,
                                             backends=backends,
                                             add_to_known_hosts=args.add_to_known_hosts,
                                             broker_url=args.broker_url,
                                             broker_password=args.broker_password)
        super(X2GoStartApp, self).__init__(redirect=False)

    def _selected_profile_id(self):
        return self._profiles_field.GetClientData(self._profiles_field.GetSelection())

    def _on_select_profile(self, event):
        profile_id = self._selected_profile_id()
        self.update_progress(_("Selected profile %s: Contacting server...", profile_id))
        self._controller.select_profile(profile_id)
        self._connect()

    def _on_create_shortcut(self, event):
        error = self._controller.create_shortcut(self._username(), self._selected_profile_id())
        if error:
            # TODO: Specific dialog for error messages (icons)?
            self._info(_("Failed creating desktop shortcut"), error)
        else:
            self.update_progress(_("Shortcut created successfully."))

    def _can_create_shortcut(self):
        return not self._controller.shortcut_exists(self._username(), self._selected_profile_id())

    def _on_exit(self, event):
        self.Exit()

    def _create_main_heading(self, parent):
        heading = self._args.heading or _("Pytis2Go")
        return ui.label(parent, heading, size=18, bold=True)

    def _create_menu_button(self, parent):
        items = [
            (_("Generate new SSH key pair"), self._controller.generate_key),
            (_("Change key passphrase"), self._controller.change_key_passphrase),
            (_("Upload public key to server"), self._controller.upload_key),
            (_("Send public key to admin"), self._controller.send_key),
        ]
        if self._controller.on_windows():
            items.append((_("Cleanup desktop shortcuts"), self._controller.cleanup_shortcuts))
        menu = wx.Menu()
        for label, callback in items:
            item = wx.MenuItem(menu, -1, label)
            menu.Bind(wx.EVT_MENU, lambda e, callback=callback: callback(), item)
            menu.AppendItem(item)
        return ui.button(parent, _("More actions..."), #icon=wx.ART_EXECUTABLE_FILE,
                         callback=lambda e: parent.PopupMenu(menu)) #style=wx.BU_EXACTFIT)

    def _create_username_field(self, parent):
        label = ui.label(parent, _("Login name:"))
        username = self._args.username
        if not username and self._args.broker_url:
            username = self._controller.broker_url_username()
        if username:
            self._username_value = username
            self._username_field = None
            return ui.hgroup(label, ui.label(parent, username), spacing=2)
        else:
            import getpass
            username = getpass.getuser()  # x2go.defaults.CURRENT_LOCAL_USER
            field = ui.field(parent, username, on_enter=lambda e: self._start())
            self._username_value = None
            self._username_field = field
            return ui.hgroup(ui.hgroup(label, padding=2), field,
                             ui.button(parent, _("Continue"), lambda e: self._start()))

    def _username(self):
        return self._username_value or self._username_field.GetValue()

    def _create_profile_selection(self, parent):
        if self._args.broker_url is None or self._args.session_profile is not None:
            self._profiles_field = None
            return None
        else:
            self._profiles_field = listbox = ui.listbox(parent, on_select=self._on_select_profile)
            listbox.Enable(False)
            listbox.SetMinSize((360, 180))
            buttons = [ui.button(parent, _("Start session"), self._on_select_profile,
                                  lambda e: e.Enable(listbox.GetSelection() != -1))]
            if self._controller.on_windows():
                buttons.append(ui.button(parent, _("Create shortcut"), self._on_create_shortcut,
                                          lambda e: e.Enable(listbox.GetSelection() != -1 and
                                                             self._can_create_shortcut())))
            return ui.vgroup(
                ui.label(parent, _("Available profiles:")),
                ui.item(
                    ui.hgroup(
                        ui.item(listbox, proportion=1, expand=True),
                        ui.vgroup(*[ui.item(b, expand=True) for b in buttons], spacing=10),
                        spacing=8,
                    ),
                    proportion=1, expand=True,
                )
            )

    def _create_status(self, parent):
        self._status = status = ui.label(parent, '')
        self._gauge = gauge = wx.Gauge(parent, -1, self._MAX_PROGRESS)
        gauge.SetMinSize((450, 10))
        return ui.vgroup(
            ui.item(gauge, expand=True),
            ui.item(status, expand=True),
        )

    def _create_main_content(self, parent):
        return ui.vgroup(
            ui.item(self._create_main_heading(parent)),
            ui.item(ui.hgroup(ui.item(self._create_username_field(parent), proportion=1),
                              self._create_menu_button(parent)),
                    expand=True, center=True),
            ui.item(self._create_profile_selection(parent), proportion=1, expand=True),
            ui.item(self._create_status(parent), expand=True),
            padding=(0, 8), spacing=8,
        )

    def _show_dialog(self, title, create, *args, **kwargs):
        class Dialog(wx.Dialog):
            result = None
            callback = None

            def close(self, result):
                self.result = result
                self.Close()

            def set_callback(self, callback):
                self.callback = callback
        dialog = Dialog(self._frame, -1, title=title)
        content = create(dialog, *args, **kwargs)
        dialog.SetSizer(content)
        dialog.Fit()
        dialog.SetClientSize(dialog.GetBestSize())
        pos, fsize, dsize = self._frame.GetPosition(), self._frame.GetSize(), dialog.GetSize()
        dialog.SetPosition((pos.x + (fsize.width - dsize.width) / 2, pos.y + 40))
        if dialog.callback:
            dialog.callback()
        dialog.ShowModal()
        dialog.Destroy()
        self._frame.Raise()
        return dialog.result

    def _session_selection_dialog(self, dialog, client, sessions):
        def on_terminate_session(event):
            selection = listbox.GetSelection()
            session = listbox.GetClientData(selection)
            self.update_progress(_("Terminating session: %s", session.name), 0)
            client.terminate_session(session)
            listbox.Delete(selection)
            self.update_progress(_("Session terminated: %s", session.name), 0)

        def on_resume_session(event):
            dialog.close(listbox.GetClientData(listbox.GetSelection()))

        listbox = ui.listbox(dialog, on_select=on_resume_session)
        for session in sessions:
            session_label = '%s@%s %s' % (session.username or '', session.hostname or '',
                                          (session.date_created or '').replace('T', ' '),)
            listbox.Append(session_label, session)
        dialog.set_callback(lambda: listbox.SetFocus())
        return ui.vgroup(
            ui.label(dialog, _("Existing sessions:")),
            ui.item(ui.hgroup(
                ui.item(listbox, proportion=1, expand=True),
                ui.item(ui.vgroup(*[
                    ui.button(dialog, label, callback, updateui, disabled=True)
                    for label, callback, updateui in (
                        (_(u"Resume"), on_resume_session,
                         lambda e: e.Enable(listbox.GetSelection() != -1)),
                        (_(u"Terminate"), on_terminate_session,
                         lambda e: e.Enable(listbox.GetSelection() != -1)),
                    )], spacing=6)),
                spacing=8,
            ), proportion=1, expand=True),
            ui.item(ui.button(dialog, _("Start New Session"), lambda e: dialog.close(None)),
                    padding=(10, 0)),
            padding=(0, 10),
        )

    def _question(self, title, question):
        def create_dialog(dialog):
            buttons = (ui.button(dialog, _(u"Yes"), lambda e: dialog.close(True)),
                       ui.button(dialog, _(u"No"), lambda e: dialog.close(False)))
            dialog.set_callback(lambda: buttons[0].SetFocus())
            return ui.vgroup(
                ui.label(dialog, question),
                ui.item(ui.hgroup(*buttons, spacing=10), center=True),
                padding=10, spacing=10,
            )
        return self._show_dialog(title, create_dialog)

    def _info(self, title, text):
        def create_dialog(dialog):
            button = ui.button(dialog, _(u"Ok"), lambda e: dialog.close(None))
            dialog.set_callback(lambda: button.SetFocus())
            return ui.vgroup(
                ui.label(dialog, text),
                ui.item(button, center=True),
                padding=10, spacing=10,
            )
        return self._show_dialog(title, create_dialog)

    def _start(self):
        if self._args.broker_url:
            self._load_profiles()
        else:
            self._connect()

    def _connect(self):
        client = self._controller.connect(self._username())
        if client:
            self.update_progress(_("Starting Pytis client."))
            self._start_session(client)
        else:
            self.Exit()

    def _load_profiles(self):
        profiles = self._controller.list_profiles(self._username())
        if not profiles:
            # Happens when the user cancels the broker authentication dialog.
            return self.Exit()  # Return is necessary because Exit() doesn't quit immediately.
        if self._args.list_profiles:
            self._list_profiles(profiles)
            return self.Exit()
        else:
            if self._controller.on_windows():
                current_version = self._controller.current_version()
                available_version = self._controller.available_upgrade_version()
                if ((available_version and available_version > current_version and
                     self._question(_("Upgrade available"),
                                    '\n'.join((_("New Pytis client version available."),
                                               _("Current version: %s", current_version),
                                               _("New version: %s", available_version),
                                               _("Install?")))))):
                    error = self._controller.upgrade(self._username())
                    if error:
                        # TODO: Specific dialog for error messages (icons)?
                        self._info(_("Upgrade failed"), error)
                    else:
                        self._info(_(u"Upgrade finished"),
                                   _(u"Pytis successfully upgraded. Restart the application."))
                        return self.Exit()
            profile_id = self._args.session_profile
            if profile_id:
                if profile_id not in profiles.profile_ids:
                    raise Exception("Unknown profile %s!" % profile_id)
                self._controller.select_profile(profile_id)
                self._connect()
            else:
                items = [(pid, profiles.to_session_params(pid)['profile_name'])
                         for pid in profiles.profile_ids]
                for profile_id, name in sorted(items, key=lambda x: x[1]):
                    self._profiles_field.Append(name, profile_id)
                self._profiles_field.Enable(True)
                self._profiles_field.SetFocus()

    def _list_profiles(self, profiles):
        import pprint
        print
        print "Available X2Go session profiles"
        print "==============================="
        if hasattr(profiles, 'config_files') and profiles.config_files is not None:
            print "configuration files: %s" % profiles.config_files
        if hasattr(profiles, 'user_config_file') and profiles.user_config_file is not None:
            print "user configuration file: %s" % profiles.user_config_file
        if hasattr(profiles, 'broker_url') and profiles.broker_url is not None:
            print "X2Go Session Broker URL: %s" % profiles.broker_url
        for profile_id in profiles.profile_ids:
            profile_config = profiles.get_profile_config(profile_id)
            session_params = profiles.to_session_params(profile_id)
            print 'Profile ID: %s' % profile_id
            print 'Profile Name: %s' % session_params['profile_name']
            print 'Profile Configuration:'
            pprint.pprint(profile_config)
            print 'Derived session parameters:'
            pprint.pprint(session_params)
            print

    def _start_session(self, client):
        self.update_progress(_("Retrieving available sessions."))
        sessions = client.list_sessions()
        if len(sessions) == 0:
            session = None
        else:
            session = self._show_dialog(_("Select session"),
                                        self._session_selection_dialog, client, sessions)
        if session:
            self.update_progress(_("Resuming session: %s", session.name), 10)
            client.resume_session(session)
        else:
            self.update_progress(_("Starting new session."), 10)
            client.start_new_session()
        self.ExitMainLoop()
        self._frame.Show(False)
        wx.Yield()
        client.main_loop()

    def update_progress(self, message=None, progress=1):
        """Update progress bar and display a progress message.

        Arguments:
          message -- Message roughly describing the current progress of
            application startup to the user.  Messages can also help to diagnose
            problems.  If None, the previous progress message is kept.
          progress -- progress bar increment in percents (int).

        """
        self._gauge.SetValue(self._gauge.GetValue() + progress)
        if message:
            self._status.SetLabel(message)
        self.Yield()

    def message(self, message):
        """Display a status message.

        Status message replaces any previous progress message, but doesn't
        change the progress bar state.

        """
        self.update_progress(message, progress=0)

    def _create_authentication_dialog(self, dialog, methods, key_files):
        def close(method):
            if isinstance(method, collections.Callable):
                method = method()
            if method is None:
                result = (None, None)
            elif method == 'password':
                result = (None, self._password_field.GetValue().rstrip('\r\n'))
            else:
                result = (self._keyfile_field.GetValue().rstrip('\r\n'),
                          self._passphrase_field.GetValue().rstrip('\r\n'))
                if not result[0]:
                    self._keyfile_field.SetFocus()
                    return
            dialog.close(result)

        def publickey_authentication(parent):
            def on_select_key_file(event):
                filename = wx.FileSelector(
                    _(u"Select SSH key file"),
                    default_path=os.path.join(os.path.expanduser('~'), '.ssh', '')
                )
                self._keyfile_field.SetValue(filename)
            label1 = ui.label(parent, _("Key File:"))
            self._keyfile_field = field1 = ui.field(parent, key_files[0] if key_files else None,
                                                    length=40, style=wx.TE_READONLY)
            button1 = ui.button(parent, _("Select"), on_select_key_file)
            label2 = ui.label(parent, _("Passphrase:"))
            self._passphrase_field = field2 = ui.field(parent, length=28, style=wx.PASSWORD,
                                                       on_enter=lambda e: close('publickey'))
            return ui.vgroup(
                ui.hgroup(ui.item(label1, padding=(3, 0)), field1, button1, spacing=2),
                ui.hgroup(ui.item(label2, padding=(3, 0)), field2, spacing=2),
                padding=10, spacing=8,
            )

        def password_authentication(parent):
            label = ui.label(parent, _("Password:"))
            self._password_field = field = ui.field(parent, style=wx.PASSWORD,
                                                    on_enter=lambda e: close('password'))
            return ui.hgroup(ui.item(label, padding=(3, 0)), field, padding=10, spacing=2)

        def on_show_dialog():
            for f in [getattr(self, a, None) for a in ('_password_field', '_passphrase_field')]:
                if f and f.IsShown():
                    f.SetFocus()

        dialog.set_callback(on_show_dialog)
        if 'password' in methods and 'publickey' in methods:
            nb = wx.Notebook(dialog, -1)
            nb.AddPage(ui.panel(nb, publickey_authentication), _(u"Public Key"))
            nb.AddPage(ui.panel(nb, password_authentication), _(u"Password"))
            nb.SetSelection(0 if key_files else 1)
            content = nb

            def method():
                return 'publickey' if nb.GetSelection() == 0 else 'password'

        elif 'publickey' in methods:
            content = publickey_authentication(dialog)
            method = 'publickey'
        elif 'password' in methods:
            content = password_authentication(dialog)
            method = 'password'
        else:
            raise Exception(_("No supported SSH authentication method available."))
        return ui.vgroup(
            content,
            ui.item(
                ui.hgroup(
                    ui.button(dialog, _("Log in"), lambda e: close(method)),
                    ui.button(dialog, _("Cancel"), lambda e: close(None)),
                    spacing=20, padding=12,
                ),
                center=True),
            padding=(0, 10),
        )

    def authentication_dialog(self, methods, key_files):
        """Interactively ask the user for authentication credentials.

        Arguments:
          methods -- sequence of authentication methods supported by the
            server (strings 'password', 'publickey').
          key_files -- sequence of SSH private key files present on local
            machine for which the server has a public key (if public key
            authentication is supported).

        The return value is a two-tuple (key_filename, password). If
        'key_filename' is not None, the user prefers public key authentication.
        In this case 'key_filename' is the file name of the SSH private key
        (string) and 'password' is its passphrase.  If 'key_filename' is None,
        the user prefers password authentication with given password.

        """
        return self._show_dialog(_("Authentication"), self._create_authentication_dialog,
                                 methods, key_files)

    def checklist_dialog(self, title, message, columns, items):
        """Display a dialog to select multiple items from a list.

        Arguments:
          title -- Dialog window top title as a string
          message -- Short prompt displayed above the list of choices
          columns -- sequence of column labels -- selectable items may contain
            multiple values to be displayed in a tabular layout in columns.
            This sequence defines the column headers.
          items -- sequence of sequences, where the top level sequence
            determines the options which may be checked/unchecked individually
            (table rows) and the inner sequences determine the values displayed
            in table columns for given row plus the initial checbox state.  The
            first value in each inner sequence is a bool (True for a checked
            item, False for unchecked) and the following are the string values
            for table columns.  Thus each inner sequence has n + 1 items where
            n is the length of 'columns'.

        """
        def create_dialog(dialog):
            checklist = ui.checklist(dialog, columns, items)
            dialog.set_callback(lambda: checklist.SetFocus())
            return ui.vgroup(
                ui.label(dialog, message),
                ui.item(checklist, proportion=1, expand=True),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _(u"Ok"),
                                  lambda e: dialog.close([checklist.IsChecked(i)
                                                          for i in range(len(items))])),
                        ui.button(dialog, _(u"Cancel"),
                                  lambda e: dialog.close(None)),
                        spacing=20,
                    ),
                    center=True, padding=12,
                ),
                padding=(0, 12),
            )
        return self._show_dialog(title, create_dialog)

    def passphrase_dialog(self, title, check=None):
        """Display a dialog to enter a key passphrase with strength checking.

        Arguments:
          title -- Dialog window top title as a string
          check -- function of one argument (the passphrase entered by the
            user) returning None if the passphrase is ok or a string
            determining the kind of error if the passphrase is not acceptable.
            The following values are recognized: 'unallowed' if the passphrase
            contains unallowed characters, 'short' if the passphrase is too
            short or 'weak' if the passphrase is not strong enough.

        """
        def create_dialog(dialog):
            def submit(event):
                password = field1.GetValue()
                password2 = field2.GetValue()
                for value, f in ((password, field1), (password2, field2)):
                    if not value:
                        f.SetFocus()
                        return
                error = None
                if password != password2:
                    error = _("Passphrases don't match.")
                elif check:
                    check_result = check(password)
                    if check_result == 'unallowed':
                        error = _("Unallowed characters.")
                    elif check_result == 'short':
                        error = _("Passphrase too short (minimum is 10 characters).")
                    elif check_result == 'weak':
                        error = _("Passphrase too weak (use capitals and numbers).")
                    elif check_result is not None:
                        raise Exception('Unsupported check result: %s' % check_result)
                if error:
                    message.SetLabel(error)
                    field1.SetFocus()
                else:
                    dialog.close(password)
            message = ui.label(dialog, "")
            field1 = ui.field(dialog, style=wx.PASSWORD, on_enter=submit)
            field2 = ui.field(dialog, style=wx.PASSWORD, on_enter=submit)
            return ui.vgroup(
                ui.item(ui.label(dialog, _("Enter the same passphrase into both fields.")),
                        padding=4),
                ui.hgroup(ui.item(ui.label(dialog, _("Passphrase:")), padding=3), field1),
                ui.hgroup(ui.item(ui.label(dialog, _("Repeat:")), padding=3), field2),
                ui.item(message, padding=4),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _("Ok"), submit),
                        ui.button(dialog, _("Cancel"), lambda e: dialog.close(None)),
                        spacing=20, padding=12,
                    ),
                    center=True),
                padding=(0, 10), spacing=4,
            )
        return self._show_dialog(title, create_dialog)

    def OnInit(self):
        title = self._args.window_title or _("Starting application")
        self._frame = frame = wx.Frame(None, -1, title)
        panel = ui.panel(frame, self._create_main_content)
        self.SetTopWindow(frame)
        frame.SetClientSize(panel.GetBestSize())
        frame.Show()
        self.Yield()
        if self._username_field:
            self.update_progress(_("Enter your user name and press Continue to start."))
        else:
            # Start automatically when username was passed explicitly.
            self._start()
        return True
