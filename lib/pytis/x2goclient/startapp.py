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
import sys
import time
import collections
import pytis.util
import pytis.x2goclient

_ = pytis.util.translations('pytis-x2go')

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
                content = item.content
                if item.padding is not None:
                    if isinstance(item.padding, tuple):
                        if isinstance(sizer, wx.FlexGridSizer):
                            content = ui.hgroup(ui.item(content, padding=item.padding,
                                                        proportion=1, expand=True))
                        elif sizer.GetOrientation() == wx.VERTICAL:
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
                sizer.Add(content, item.proportion, flag, border)
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
                    isinstance(content, (tuple, list)) and len(content) == 2 and
                    all(isinstance(x, int) for x in content) or content is None), content
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
    def grid(*items, **kwargs):
        """Arrange UI items into a horizontal group returning 'wx.BoxSizer'.

        Arguments:
          items -- same as in 'ui.vgroup'.
          rows -- number of rows in the grid (int).
          cols -- number of columns in the grid (int).
          spacing -- space between individual items of the grid in pixels
            as int or a tuple of two ints to specify vertical and horizontral
            spacing separately (in this order).
          padding -- same as in 'ui.vgroup'.

        """
        spacing = kwargs.pop('spacing', None)
        if isinstance(spacing, tuple):
            vgap, hgap = spacing
        else:
            vgap, hgap = spacing or 0, spacing or 0
        return ui._add_to_sizer(wx.FlexGridSizer(rows=kwargs.pop('rows', 1),
                                                 cols=kwargs.pop('cols', 1),
                                                 hgap=hgap, vgap=vgap),
                                items, **kwargs)

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
    _DEFAULT_SESSION_PARAMETERS = dict(
        gss_auth=False,
        look_for_keys=True,
        allow_agent=True,
        profile_name='Pytis-Client-Session',
        cache_type='unix-kde',
        allow_share_local_folders=True
    )

    class _TaskBarIcon(wx.TaskBarIcon):

        def __init__(self, on_click):
            super(X2GoStartApp._TaskBarIcon, self).__init__()
            self._menu = []
            self.set_icon('disconnected')
            self.Bind(wx.EVT_TASKBAR_LEFT_DOWN, lambda e: on_click())

        def set_icon(self, name):
            path = os.path.normpath(os.path.join(sys.path[0], '..', 'icons', name + '.png'))
            icon = wx.IconFromBitmap(wx.Bitmap(path))
            self.SetIcon(icon, _("Pytis2Go Service"))

        def update_menu(self, menu):
            self._menu = [item + (None, True)[len(item) - 1:] for item in menu if item]

        def CreatePopupMenu(self):
            menu = wx.Menu()
            for label, callback, enabled in self._menu:
                if callback:
                    item = wx.MenuItem(menu, -1, label)
                    menu.Bind(wx.EVT_MENU, lambda event, callback=callback: callback(),
                              id=item.GetId())
                    menu.AppendItem(item)
                    menu.Enable(item.GetId(), bool(enabled))
                else:
                    menu.AppendSeparator()
            return menu

    def __init__(self, args, session_parameters, force_parameters):
        self._progress = 1
        self._args = args
        self._session_parameters = dict(self._DEFAULT_SESSION_PARAMETERS, **session_parameters)
        self._force_parameters = force_parameters
        self._controller = pytis.x2goclient.StartupController(
            self,
            add_to_known_hosts=args.add_to_known_hosts,
            broker_url=args.broker_url,
            broker_password=args.broker_password,
            wait_for_pytis=not args.nowait,
        )
        self._profiles = []
        username = args.username
        if not username:
            import getpass
            username = getpass.getuser()  # x2go.defaults.CURRENT_LOCAL_USER
        self._default_username = username
        super(X2GoStartApp, self).__init__(redirect=False)

    def _menu_items(self):
        items = [
            (parameters['profile_name'], lambda p=profile_id: self._connect(p))
            for profile_id, parameters in self._profiles
        ]
        items.extend((
            ('---',) if items else None,
            (_("Reload profiles") if items else _("Load profiles"), self._load_profiles),
            ('---',),
            (_("Generate new SSH key pair"), self._controller.generate_key),
            (_("Change key passphrase"), self._controller.change_key_passphrase),
            (_("Upload public key to server"), self._controller.upload_key),
            (_("Send public key to admin"), self._controller.send_key),
            (_("Create desktop shortcut"), self._create_shortcut, bool(items))
            if pytis.util.on_windows() else None,
            (_("Cleanup desktop shortcuts"), self._controller.cleanup_shortcuts)
            if pytis.util.on_windows() else None,
            ('---',),
            (_("Exit"), self._on_exit),
        ))
        return items

    def _create_main_content(self, parent):
        self._message = message = ui.label(parent, '')
        self._gauge = gauge = wx.Gauge(parent, -1, self._MAX_PROGRESS)
        gauge.SetMinSize((450, 14))
        return ui.vgroup(
            ui.item(message, expand=True),
            ui.item(gauge, expand=True),
            padding=(10, 16),
            spacing=10,
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
        content.Fit(dialog)
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

    def question_dialog(self, title, question, default=wx.YES_DEFAULT):
        style = wx.YES_NO | default | wx.ICON_QUESTION
        dlg = wx.MessageDialog(self._frame, question, caption=title, style=style)
        if not dlg.HasFlag(wx.STAY_ON_TOP):
            dlg.ToggleWindowStyle(wx.STAY_ON_TOP)
        # Raise should not be necessary, but there was a problem with focus
        # when used on windows
        dlg.Raise()
        result = dlg.ShowModal() == wx.ID_YES
        dlg.Destroy()
        return result

    def info_dialog(self, title, text):
        def create_dialog(dialog):
            button = ui.button(dialog, _(u"Ok"), lambda e: dialog.close(None))
            dialog.set_callback(lambda: button.SetFocus())
            return ui.vgroup(
                ui.label(dialog, text),
                ui.item(button, center=True),
                padding=10, spacing=10,
            )
        return self._show_dialog(title, create_dialog)

    def _connect(self, profile_id):
        self._frame.Show()
        self.update_progress(_("Selected profile %s: Contacting server...", profile_id))
        session_parameters = dict(
            self._session_parameters,
            **dict((k, v) for k, v in dict(self._profiles)[profile_id].items()
                   if k not in self._force_parameters)
        )
        client = self._controller.connect(session_parameters)
        if client:
            self.update_progress(_("Starting Pytis client."))
            self._start_session(client)
        else:
            self.Exit()

    def _load_profiles(self):
        self._frame.Show()
        self._profiles = self._controller.list_profiles()
        self._icon.update_menu(self._menu_items())
        self.update_progress(_("Successfully loaded %d profiles.", len(self._profiles)))
        time.sleep(2)
        # Profiles are empty when the user cancels the broker authentication dialog.
        if self._profiles and pytis.util.on_windows():
            current_version = self._controller.current_version()
            available_version = self._controller.available_upgrade_version()
            if ((available_version and available_version > current_version and
                self._question(_("Upgrade available"),
                               '\n'.join((_("New Pytis client version available."),
                                          _("Current version: %s", current_version),
                                          _("New version: %s", available_version),
                                          _("Install?")))))):
                error = self._controller.upgrade()
                if error:
                    # TODO: Specific dialog for error messages (icons)?
                    self.info_dialog(_("Upgrade failed"), error)
                else:
                    self.info_dialog(
                        _(u"Upgrade finished"),
                        _(u"Pytis successfully upgraded. Restart the application."))
                    return self.Exit()
        self._frame.Hide()

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
        if self._args.nowait:
            # Close automatically right now.
            self.close()
        else:
            # Rely on the client to call 'close()' explicitly from a callback.
            self.update_progress(_("Waiting for the application to come up..."), 10)
        client.main_loop()
        self.Exit()

    def _on_taskbar_click(self):
        pass

    def _on_exit(self):
        wx.CallAfter(self._icon.Destroy)
        self._frame.Close()

    def _create_shortcut(self):
        def create_shortcut(profile_id):
            error = self._controller.create_shortcut(profile_id)
            if error:
                # TODO: Specific dialog for error messages (icons)?
                self.info_dialog(_("Failed creating desktop shortcut"), error)
            else:
                self.info_dialog(_("Shortcut created"), _("Shortcut created successfully."))
        menu = wx.Menu()
        for profile_id, parameters in self._profiles:
            item = wx.MenuItem(menu, -1, parameters['profile_name'])
            menu.Bind(wx.EVT_MENU,
                      lambda event, profile_id=profile_id: create_shortcut(profile_id),
                      id=item.GetId())
            menu.Enable(item.GetId(), not self._controller.shortcut_exists(profile_id))
            menu.AppendItem(item)
        self._icon.PopupMenu(menu)
        menu.Destroy()

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
            self._message.SetLabel(message)
        self.Yield()

    def message(self, message):
        """Display a message.

        The message replaces any previous message, but doesn't
        change the progress bar state.

        """
        self.update_progress(message, progress=0)

    def authentication_dialog(self, server, username, methods, key_files):
        """Interactively ask the user for authentication credentials.

        Arguments:
          server -- server hostname as a string (displayed for user's
            information which credentials to enter).
          username -- initial user name (string).  May be changed by the
            user.
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
        def create_dialog(dialog):
            def close(method):
                if isinstance(method, collections.Callable):
                    method = method()
                if method is None:
                    return dialog.close((None, None, None))
                username = self._username_field.GetValue().rstrip('\r\n')
                if not username:
                    self._username_field.SetFocus()
                    return
                key_filename, password = (None, None)
                if method == 'password':
                    password = self._password_field.GetValue().rstrip('\r\n')
                elif method == 'publickey':
                    key_filename = self._keyfile_field.GetValue().rstrip('\r\n')
                    if not key_filename:
                        self._keyfile_field.SetFocus()
                        return
                    password = self._passphrase_field.GetValue().rstrip('\r\n')
                dialog.close((username, key_filename, password))

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
            self._username_field = ui.field(dialog, username or self._default_username)
            return ui.vgroup(
                ui.hgroup(ui.hgroup(ui.label(dialog, _("Login name:")), padding=(3, 0)),
                          self._username_field),
                content,
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _("Log in"), lambda e: close(method)),
                        ui.button(dialog, _("Cancel"), lambda e: close(None)),
                        spacing=20, padding=12,
                    ),
                    center=True),
                padding=10,
            )
        return self._show_dialog(_("Log in to %s", server), create_dialog)

    def keyfile_passphrase_dialog(self, key_filename):
        """Interactively choose keyfile and its passphrase.

        The return value is two-tuple (key_filename, password).

        """
        def create_dialog(dialog):
            def submit(event):
                for f in fields:
                    if not f.GetValue():
                        f.SetFocus()
                        return
                dialog.close([f.GetValue() for f in fields])
            default_path = os.path.join(os.path.expanduser('~'), '.ssh', '')
            fields = (
                ui.field(dialog, key_filename, length=40, style=wx.TE_READONLY),
                ui.field(dialog, length=28, style=wx.PASSWORD),
            )
            return ui.vgroup(
                ui.hgroup(ui.item(ui.label(dialog, _("Key File:")), padding=(3, 0)),
                          fields[0],
                          ui.button(dialog, _("Select"), lambda e:
                                    fields[0].SetValue(wx.FileSelector(
                                        _(u"Select SSH key file"),
                                        default_path=default_path,
                                    ))
                          ),
                          spacing=2),
                ui.hgroup(ui.item(ui.label(dialog, _("Passphrase:")), padding=(3, 0)),
                          fields[1], spacing=2),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _("Ok"), submit),
                        ui.button(dialog, _("Cancel"), lambda e: dialog.close((None, None, None))),
                        spacing=20, padding=6,
                    ),
                    center=True),
                padding=(10, 20), spacing=4,
            )
        return self._show_dialog(_("Select the key file"), create_dialog)

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
                for f in fields:
                    if not f.GetValue():
                        f.SetFocus()
                        return
                passphrase, passphrase2 = [
                    f.GetValue() for f in fields
                ]
                error = None
                if passphrase != passphrase2:
                    error = _("Passphrases don't match.")
                elif check:
                    check_result = check(passphrase)
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
                    fields[0].SetFocus()
                else:
                    dialog.close(passphrase)
            fields = (
                ui.field(dialog, style=wx.PASSWORD, length=30, on_enter=submit),
                ui.field(dialog, style=wx.PASSWORD, length=30, on_enter=submit)
            )
            message = ui.label(dialog, "")
            return ui.vgroup(
                ui.item(ui.label(dialog, _("Enter the same passphrase into both fields:")),
                        padding=4),
                ui.grid(ui.item(ui.label(dialog, _("Passphrase:")), padding=3), fields[0],
                        ui.item(ui.label(dialog, _("Repeat:")), padding=3), fields[1],
                        rows=2, cols=2, spacing=(0, 4)),
                ui.item(message, padding=4),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _("Ok"), submit),
                        ui.button(dialog, _("Cancel"), lambda e: dialog.close(None)),
                        spacing=20, padding=6,
                    ),
                    center=True),
                padding=(10, 20), spacing=4,
            )
        return self._show_dialog(title, create_dialog)

    def close(self):
        """Close the startup application window.

        The window is closed automatically after starting the X2Go client when
        --nowait is passed.  If --nowait is not passed, this public method must
        be called from an X2Go client callback when an application window comes
        up.

        """
        self.ExitMainLoop()
        self._frame.Show(False)
        wx.Yield()

    def OnInit(self):
        title = self._args.window_title or _("Pytis2Go")
        self._frame = frame = wx.Frame(None, -1, title)
        frame.Hide()
        self.SetTopWindow(frame)
        self._icon = self._TaskBarIcon(self._on_taskbar_click)
        self._icon.update_menu(self._menu_items())
        panel = ui.panel(frame, self._create_main_content)
        frame.SetClientSize(panel.GetBestSize())
        if pytis.util.on_windows():
            self.update_progress(_("Starting up X-server."))
            self._xserver = pytis.x2goclient.XServer()
        self.Yield()
        profile_id = self._args.session_profile
        if profile_id or self._args.autoload:
            self._load_profiles()
        if profile_id:
            self._connect(profile_id)
        return True
