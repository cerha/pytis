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
import shutil
import socket
import tarfile
import datetime
import tempfile
import paramiko
import StringIO
import threading
import collections

import pytis.util
import pytis.x2goclient
from .ssh import public_key_acceptable, ssh_connect
from .clientprocess import Broker, ClientProcess, XServer

_ = pytis.util.translations('pytis-x2go')

class ui(object):
    """Private helper methods for simple UI construction (not to be used outside this module)."""

    CENTER = 'CENTER'
    RIGHT = 'RIGHT'

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
                if item.align == ui.CENTER:
                    flag |= wx.ALIGN_CENTER
                elif item.align == ui.RIGHT:
                    flag |= wx.ALIGN_RIGHT
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
          align -- alignment of the item content inside the space available for
            it.  One of 'ui.LEFT', 'ui.RIGHT', 'ui.CENTER'.  Default is left.

        """
        def __init__(self, content, proportion=0, expand=False, padding=None, align=wx.LEFT):
            assert (isinstance(content, (wx.Window, wx.Sizer, wx.Size)) or
                    isinstance(content, (tuple, list)) and len(content) == 2 and
                    all(isinstance(x, int) for x in content) or content is None), content
            self.content = content
            self.proportion = proportion
            self.expand = expand
            self.padding = padding
            self.align = align

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
            vgap = hgap = spacing or 0
        return ui._add_to_sizer(wx.FlexGridSizer(rows=kwargs.pop('rows', 0),
                                                 cols=kwargs.pop('cols', 0),
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
    def field(parent, name=None, value=None, length=20, style=wx.DEFAULT, readonly=False,
              disabled=False, on_enter=None, updateui=None):
        if on_enter:
            style |= wx.TE_PROCESS_ENTER
        if readonly:
            style |= wx.TE_READONLY
        ctrl = wx.TextCtrl(parent, -1, value or '', name=name, style=style)
        width, height = parent.GetTextExtent('x' * length)
        ctrl.SetMinSize((width, ctrl.GetSize().height))
        if disabled:
            ctrl.Enable(False)
        if on_enter:
            ctrl.Bind(wx.EVT_TEXT_ENTER, on_enter)
        if updateui:
            ctrl.Bind(wx.EVT_UPDATE_UI, updateui)
        return ctrl

    @staticmethod
    def button(parent, label, callback, updateui=None, name=None, icon=None, disabled=False,
               size=None):
        button = wx.Button(parent, -1, label=label, size=size)
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
    def label(parent, text, name=None, size=None, face=None, bold=False, italic=False,
              underline=False):
        label = wx.StaticText(parent, -1, text, name=name or '')
        if size or face or bold or italic or underline:
            label.SetFont(wx.Font(size, wx.DEFAULT, faceName=(face or ''), underline=underline,
                                  style=wx.ITALIC if italic else wx.NORMAL,  # wx.SLANT
                                  weight=wx.BOLD if bold else wx.NORMAL))  # wx.LIGHT
        return label

    @staticmethod
    def radio(parent, label, choices, name=None, horizontal=False, selected=None, on_change=None):
        if horizontal:
            style = wx.RA_SPECIFY_COLS
        else:
            style = wx.RA_SPECIFY_ROWS
        control = wx.RadioBox(parent, -1, label, name=name, style=style, choices=choices)
        if on_change:
            control.Bind(wx.EVT_RADIOBOX, on_change)
        if selected is not None:
            control.SetSelection(selected)
        return control

    @staticmethod
    def listbox(parent, choices=(), name=None, on_select=None):
        def on_key_up(event):
            if ((listbox.GetSelection() != -1 and
                 event.GetKeyCode() in (wx.WXK_SPACE, wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER))):
                on_select(event)
            event.Skip()

        def on_dclick(event):
            if listbox.GetSelection() != -1:
                on_select(event)
            event.Skip()

        listbox = wx.ListBox(parent, -1, style=wx.LB_SINGLE, name=name)
        for item in choices:
            if isinstance(item, (list, tuple)):
                listbox.Append(*item)
            else:
                listbox.Append(item)
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

class ProgressDialog(object):

    def __init__(self, title):
        self._dialog = dialog = wx.Dialog(None, -1, title)
        self._gauge = gauge = wx.Gauge(dialog, -1)
        self._label = label = ui.label(dialog, '')
        gauge.SetMinSize((450, 14))
        content = ui.vgroup(
            ui.item(label, expand=True),
            ui.item(gauge, expand=True),
            padding=(10, 16),
            spacing=10,
        )
        dialog.SetSizer(content)
        content.Fit(dialog)
        dialog.Show()

    def message(self, message):
        """Display a progress message.

        Arguments:
          message -- Message roughly describing the current progress state to the user.

        """
        self._gauge.Pulse()
        self._label.SetLabel(message)
        wx.Yield()

    def close(self):
        self._dialog.Close()
        self._dialog.Destroy()


class Session(threading.Thread):

    def __init__(self, client):
        self._client = client
        threading.Thread.__init__(self)

    def run(self):
        self._client.main_loop()

    def terminate(self):
        self._client.terminate()


class X2GoStartApp(wx.App):
    """X2Go startup application."""

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

    def __init__(self, args):
        self._args = args
        self._profiles = []
        self._sessions = []
        username = args.username
        if not username:
            import getpass
            username = getpass.getuser()  # x2go.defaults.CURRENT_LOCAL_USER
        self._default_username = username
        if args.broker_url:
            self._broker = Broker(args.broker_url, password=args.broker_password)
        else:
            self._broker = None
        self._keyring = []
        super(X2GoStartApp, self).__init__(redirect=False)

    def _menu_items(self):
        items = [
            (params['profile_name'], lambda params=params: self._start_session(params))
            for profile_id, params in self._profiles
        ]
        items.extend((
            ('---',) if items else None,
            (_("Reload profiles") if items else _("Load profiles"), self._load_profiles),
            ('---',),
            (_("Generate new SSH key pair"), self._generate_key),
            (_("Change key passphrase"), self._change_key_passphrase),
            (_("Upload public key to server"), self._upload_key),
            (_("Send public key to admin"), self._send_key),
            (_("Create desktop shortcut"), self._create_shortcut_menu, bool(items))
            if pytis.util.on_windows() else None,
            (_("Cleanup desktop shortcuts"), self._cleanup_shortcuts)
            if pytis.util.on_windows() else None,
            ('---',),
            (_("Exit"), self._on_exit),
        ))
        return items

    def _on_exit(self):
        for session in self._sessions:
            session.terminate()
        wx.CallAfter(self._icon.Destroy)
        self.Exit()

    def _on_taskbar_click(self):
        pass

    def _show_dialog(self, title, create, *args, **kwargs):
        class Dialog(wx.Dialog):
            result = None
            callback = None

            def close(self, result):
                self.result = result
                self.Close()

            def set_callback(self, callback):
                self.callback = callback

            def widget(self, name):
                return self.FindWindowByName(name)

        dialog = Dialog(None, -1, title=title)
        content = create(dialog, *args, **kwargs)
        dialog.SetSizer(content)
        content.Fit(dialog)
        if dialog.callback:
            dialog.callback()
        dialog.ShowModal()
        dialog.Destroy()
        return dialog.result

    def _session_selection_dialog(self, dialog, progress, client, sessions):
        def on_terminate_session(event):
            listbox = dialog.widget('session')
            selection = listbox.GetSelection()
            session = listbox.GetClientData(selection)
            progress.message(_("Terminating session: %s", session.name))
            client.terminate_session(session)
            listbox.Delete(selection)
            progress.message(_("Session terminated: %s", session.name))

        def on_resume_session(event):
            listbox = dialog.widget('session')
            dialog.close(listbox.GetClientData(listbox.GetSelection()))

        def session_label(session):
            return '%s@%s %s' % (session.username or '', session.hostname or '',
                                 (session.date_created or '').replace('T', ' '))

        dialog.set_callback(lambda: dialog.widget('session').SetFocus())
        return ui.vgroup(
            ui.label(dialog, _("Existing sessions:")),
            ui.item(ui.hgroup(
                ui.item(ui.listbox(dialog, name='session', on_select=on_resume_session,
                                   choices=[(session_label(s), s) for s in sessions]),
                        proportion=1, expand=True),
                ui.item(ui.vgroup(*[
                    ui.button(dialog, label, callback, updateui, disabled=True)
                    for label, callback, updateui in (
                        (_(u"Resume"), on_resume_session,
                         lambda e: e.Enable(dialog.widget('session').GetSelection() != -1)),
                        (_(u"Terminate"), on_terminate_session,
                         lambda e: e.Enable(dialog.widget('session').GetSelection() != -1)),
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
                ui.item(ui.hgroup(*buttons, spacing=10), align=ui.CENTER),
                padding=10, spacing=10,
            )
        return self._show_dialog(title, create_dialog)

    def _load_profiles(self):
        title = self._args.window_title or _("Pytis2Go")
        progress = ProgressDialog(_("%s: Loading profiles...", title))
        profiles = self._broker.list_profiles(lambda f, params:
                                              self._authenticate(progress, f, params))
        if profiles is not None:
            progress.message(self._broker.server() + ': ' +
                             _.ngettext("Returned %d profile.",
                                        "Returned %d profiles.",
                                        len(profiles)))
            self._profiles = profiles
        else:
            progress.message(_("Failed loading broker profiles."))
            self._profiles = ()
        self._icon.update_menu(self._menu_items())
        time.sleep(2)
        # Profiles are empty when the user cancels the broker authentication dialog.
        if profiles and pytis.util.on_windows():
            current_version = pytis.x2goclient.X2GOCLIENT_VERSION
            available_version, connection_parameters, path = self._broker.upgrade_parameters()
            if ((available_version and available_version > current_version and
                self._question(_("Upgrade available"),
                               '\n'.join((_("New Pytis client version available."),
                                          _("Current version: %s", current_version),
                                          _("New version: %s", available_version),
                                          _("Install?")))))):
                error = self._upgrade(connection_parameters, path)
                if error:
                    # TODO: Specific dialog for error messages (icons)?
                    self._info_dialog(_("Upgrade failed"), error)
                else:
                    self._info_dialog(
                        _(u"Upgrade finished"),
                        _(u"Pytis successfully upgraded. Restart the application."))
                    return self.Exit()
        progress.close()

    def _connect(self, session_parameters):
        # Authenticate to server and return session_parameters including
        # also all necessary authentication parameters.
        connection_parameters = dict((k, session_parameters[k]) for k in
                                     ('server', 'port', 'username', 'password',
                                      'key_filename', 'allow_agent', 'gss_auth'))
        if ssh_connect(**connection_parameters):
            return session_parameters
        else:
            return None

    def _start_session(self, session_parameters):
        progress = ProgressDialog(
            _("Starting session: %s", session_parameters['profile_name'])
        )
        progress.message(_("Contacting server %s...", session_parameters['server']))
        session_parameters = self._authenticate(progress, self._connect, session_parameters)
        if not session_parameters:
            return
        progress.message(_("Starting X2Go client"))
        client = ClientProcess(session_parameters)
        progress.message(_("Retrieving available sessions."))
        sessions = client.list_sessions()
        if len(sessions) == 0:
            session = None
        else:
            session = self._show_dialog(_("Select session"), self._session_selection_dialog,
                                        progress, client, sessions)
        if session:
            progress.message(_("Resuming session: %s", session.name))
            client.resume_session(session)
        else:
            progress.message(_("Starting new session."))
            client.start_new_session()
        progress.message(_("Waiting for the application to come up..."))
        wx.CallLater(4000, progress.close)
        session = Session(client)
        self._sessions.append(session)
        session.start()

    def _question_dialog(self, title, question, default=wx.YES_DEFAULT):
        style = wx.YES_NO | default | wx.ICON_QUESTION
        dlg = wx.MessageDialog(None, question, caption=title, style=style)
        if not dlg.HasFlag(wx.STAY_ON_TOP):
            dlg.ToggleWindowStyle(wx.STAY_ON_TOP)
        # Raise should not be necessary, but there was a problem with focus
        # when used on windows
        dlg.Raise()
        result = dlg.ShowModal() == wx.ID_YES
        dlg.Destroy()
        return result

    def _info_dialog(self, title, text):
        def create_dialog(dialog):
            button = ui.button(dialog, _(u"Ok"), lambda e: dialog.close(None))
            dialog.set_callback(lambda: button.SetFocus())
            return ui.vgroup(
                ui.label(dialog, text),
                ui.item(button, align=ui.CENTER),
                padding=10, spacing=10,
            )
        return self._show_dialog(title, create_dialog)

    def _authentication_dialog(self, server, username, key_files, methods, default_method):
        """Interactively ask the user for authentication credentials.

        Arguments:
          server -- server hostname as a string (displayed for user's
            information which credentials to enter).
          username -- initial user name (string).  May be changed by the
            user.
          key_files -- sequence of SSH private key files present on local
            machine for which the server has a public key (if public key
            authentication is supported).
          methods -- authentication methods supported by the server as a tuple
            of strings ('publickey', 'password') where one of those may be
            missing if not supported.
          default_method -- default authentication method as a string from
            ('publickey', 'password').

        The return value is a three-tuple (username, key_filename,
        password). If 'key_filename' is not None, the user prefers public key
        authentication.  In this case 'key_filename' is the file name of the
        SSH private key (string) and 'password' is its passphrase.  If
        'key_filename' is None, the user prefers password authentication with
        given password.  If the dialog was canceled, (None, None, None) is
        returned.

        """
        def create_dialog(dialog):
            def submit(event):
                if 'publickey' in methods and 'password' in methods:
                    method = dialog.widget('method').GetSelection()
                elif 'publickey' in methods:
                    method = 0
                else:
                    method = 1
                result = []
                for f in ('username', 'filename', 'password'):
                    if f == 'filename' and method != 0:
                        value = None
                    else:
                        widget = dialog.widget(f)
                        value = widget.GetValue().rstrip('\r\n')
                        if not value:
                            widget.SetFocus()
                            return
                    result.append(value)
                dialog.close(result)

            def on_button(event):
                filename = wx.FileSelector(
                    _(u"Select SSH key file"),
                    default_path=os.path.join(os.path.expanduser('~'), '.ssh', '')
                )
                dialog.widget('filename').SetValue(filename)

            def updateui(event):
                radio = dialog.widget('method')
                if radio:
                    event.Enable(radio.GetSelection() == 0)

            content = (
                ui.item(ui.label(dialog, _("Login name:")), padding=(5, 0)),
                ui.field(dialog, name='username', length=40,
                         value=username or self._default_username),
            )
            if 'publickey' in methods and 'password' in methods:
                content += (
                    ui.hgroup(),
                    ui.radio(dialog, _("Authentication method:"), name='method',
                             choices=(_("Public Key"), _("Password")),
                             selected=1 if default_method == 'password' else 0),
                )
            if 'publickey' in methods:
                content += (
                    ui.item(ui.label(dialog, _("Key File:")), padding=(5, 0)),
                    ui.hgroup(
                        ui.field(dialog, name='filename', length=40, readonly=True,
                                 value=key_files[0] if key_files else None, updateui=updateui),
                        ui.button(dialog, _("Select"), on_button, name='button', size=(80, 25),
                                  updateui=updateui),
                        spacing=3,
                    ),
                )
            content += (
                ui.item(ui.label(dialog, _("Password:")), padding=(5, 0)),
                ui.field(dialog, name='password', length=40, style=wx.PASSWORD,
                         on_enter=submit),
            )
            dialog.set_callback(lambda: dialog.widget('password').SetFocus())
            return ui.vgroup(
                ui.grid(*content, spacing=(8, 3), cols=2),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _("Log in"), submit),
                        ui.button(dialog, _("Cancel"), lambda e: dialog.close((None, None, None))),
                        spacing=20, padding=12,
                    ),
                    align=ui.CENTER,
                ),
                padding=10, spacing=8,
            )
        return self._show_dialog(_("Log in to %s", server), create_dialog)

    def _keyfile_passphrase_dialog(self, key_filename):
        """Interactively choose keyfile and its passphrase.

        The return value is two-tuple (key_filename, password).

        """
        def create_dialog(dialog):
            def submit(event):
                values = [dialog.widget(f).GetValue() for f in ('filename', 'passphrase')]
                if '' in values:
                    dialog.widget(('filename', 'passphrase')[values.index('')]).SetFocus()
                    return
                dialog.close(values)
            return ui.vgroup(
                ui.hgroup(ui.item(ui.label(dialog, _("Key File:")), padding=(3, 0)),
                          ui.field(dialog, name='filename', value=key_filename, length=40,
                                   readonly=True),
                          ui.button(dialog, _("Select"),
                                    lambda e: dialog.widget('f1').SetValue(wx.FileSelector(
                                        _(u"Select SSH key file"),
                                        default_path=os.path.join(os.path.expanduser('~'),
                                                                  '.ssh', ''),
                                    ))),
                          spacing=2),
                ui.hgroup(ui.item(ui.label(dialog, _("Passphrase:")), padding=(3, 0)),
                          ui.field(dialog, name='passphrase', length=28, style=wx.PASSWORD),
                          spacing=2),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _("Ok"), submit),
                        ui.button(dialog, _("Cancel"), lambda e: dialog.close((None, None, None))),
                        spacing=20, padding=6,
                    ),
                    align=ui.CENTER),
                padding=(10, 20), spacing=4,
            )
        return self._show_dialog(_("Select the key file"), create_dialog)

    def _checklist_dialog(self, title, message, columns, items):
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
                    align=ui.CENTER, padding=12,
                ),
                padding=(0, 12),
            )
        return self._show_dialog(title, create_dialog)

    def _passphrase_dialog(self, title, check=None):
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
                values = [dialog.widget(f).GetValue() for f in ('f1', 'f2')]
                if '' in values:
                    dialog.widget(('f1', 'f2')[values.index('')]).SetFocus()
                    return
                error = None
                if values[0] != values[1]:
                    error = _("Passphrases don't match.")
                elif check:
                    check_result = check(values[0])
                    if check_result == 'unallowed':
                        error = _("Unallowed characters.")
                    elif check_result == 'short':
                        error = _("Passphrase too short (minimum is 10 characters).")
                    elif check_result == 'weak':
                        error = _("Passphrase too weak (use capitals and numbers).")
                    elif check_result is not None:
                        raise Exception('Unsupported check result: %s' % check_result)
                if error:
                    dialog.widget('message').SetLabel(error)
                    dialog.widget('f1').SetFocus()
                else:
                    dialog.close(values[0])
            return ui.vgroup(
                ui.item(ui.label(dialog, _("Enter the same passphrase into both fields:")),
                        padding=4),
                ui.grid(ui.item(ui.label(dialog, _("Passphrase:")), padding=3),
                        ui.field(dialog, name='f1', style=wx.PASSWORD, length=30, on_enter=submit),
                        ui.item(ui.label(dialog, _("Repeat:")), padding=3),
                        ui.field(dialog, name='f2', style=wx.PASSWORD, length=30, on_enter=submit),
                        rows=2, cols=2, spacing=(0, 4)),
                ui.item(ui.label(dialog, "", name='message'), padding=4),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _("Ok"), submit),
                        ui.button(dialog, _("Cancel"), lambda e: dialog.close(None)),
                        spacing=20, padding=6,
                    ),
                    align=ui.CENTER),
                padding=(10, 20), spacing=4,
            )
        return self._show_dialog(title, create_dialog)

    def _authentication_methods(self, connection_parameters):
        sock = socket.socket()
        sock.connect((connection_parameters['server'], connection_parameters['port']))
        transport = paramiko.Transport(sock)
        transport.connect()
        try:
            transport.auth_none('')
        except paramiko.ssh_exception.BadAuthenticationType as e:
            methods = e.allowed_types
        transport.close()
        sock.close()
        return methods

    def _acceptable_key_files(self, connection_parameters):
        def key_acceptable(path):
            if os.access(path, os.R_OK):
                try:
                    return public_key_acceptable(
                        connection_parameters['server'],
                        connection_parameters['username'],
                        path,
                        port=connection_parameters['port'])
                except:
                    return True
            else:
                return True
        return [path for path in [os.path.join(os.path.expanduser('~'), '.ssh', name)
                                  for name in ('id_ecdsa', 'id_rsa', 'id_dsa')]
                if os.access(path, os.R_OK) and key_acceptable(path + '.pub')]

    def _authenticate(self, progress, function, connection_parameters, **kwargs):
        """Try calling 'method' with different authentication parameters.

        Arguments:
          function -- connection function to be called with different
            connection parameters to try different authentication methods.  The
            function must accept the dictionary of connection parameters as its
            first positional argument plus optionally any keyword arguemnts
            passed in '**kwargs'.  The function must return a result which (in
            boolean context) evaluates to True to indicate success or False for
            failure.  If failure is returned for one authentication method,
            another method may be tried (with different connection parameters).
          connection_parameters -- initial connection parameters as a
            dictionary with keys such as 'server', 'port', 'username', etc.
            Authentication related keys in this dictionary will be overriden
            before passing the parameters to 'function' as described below.
          **kwargs -- all remaining keyword arguments will be passed on to
            'function'.

        Authentication related connection parameters:

          username -- user's login name (mandatory)
          gss_auth -- True if GSS-API (Kerberos) authentication is to be used
            or False for other authentication schemes
          key_filename -- file name of the private SSH key to use for
            authentication or None
          password -- SSH key passphrase if 'key_filename' is given or user's
            password for password authentication

        Supported authentication methods:
          - GSS-API (Kerberos) -- 'gss_auth' is True and 'key_filename' and
            'password' are empty
          - SSH Agent -- 'gss_auth' is False and 'key_filename' and 'password'
            are empty
          - Public Key -- 'gss_auth' is False and 'key_filename' and 'password'
            are given
          - Password -- 'gss_auth' is False, 'key_filename' is empty and
            'password' is given

        """
        def message(msg):
            progress.message(connection_parameters['server'] + ': ' + msg)

        def connect(username, gss_auth=False, key_filename=None, password=None):
            return function(dict(
                connection_parameters,
                username=username,
                gss_auth=gss_auth,
                key_filename=key_filename,
                password=password,
                look_for_keys=key_filename is not None,
                allow_agent=True,
                add_to_known_hosts=self._args.add_to_known_hosts,
            ), **kwargs)

        success = False
        message(_("Retrieving supported authentication methods."))
        methods = self._authentication_methods(connection_parameters)
        for username, key_filename, password in self._keyring:
            if key_filename and password and 'publickey' in methods:
                message(_("Trying public key authentication."))
                success = connect(username, key_filename=key_filename, password=password)
            elif key_filename is None and password and 'password' in methods:
                message(_("Trying password authentication."))
                success = connect(username, password=password)
            if success:
                break
        username = connection_parameters['username']
        if not success and not self._args.no_agent_authentication:
            message(_("Trying SSH Agent authentication."))
            success = connect(username)
        if not success and not self._args.no_kerberos_authentication:
            message(_("Trying Kerberos authentication."))
            success = connect(username, gss_auth=True)
        if not success:
            if 'publickey' in methods:
                key_files = self._acceptable_key_files(connection_parameters)
                default_method = 'publickey'
            elif 'password' in methods:
                key_files = ()
                default_method = 'password'
            else:
                raise Exception(_("No supported SSH authentication method available."))
            message(_("Trying interactive authentication."))
            while not success:
                username, key_filename, password = self._authentication_dialog(
                    connection_parameters['server'],
                    username, key_files, methods, default_method,
                )
                if username is None:
                    return None
                if key_filename or password:
                    if key_filename:
                        message(_("Trying public key authentication."))
                        default_method = 'publickey'
                    else:
                        message(_("Trying password authentication."))
                        default_method = 'password'
                    success = connect(username, key_filename=key_filename, password=password)
                    if success:
                        self._keyring.append((username, key_filename, password))
                elif username != connection_parameters['username']:
                    message(_("Trying SSH Agent authentication."))
                    success = connect(username)
                    if not success:
                        message(_("Trying Kerberos authentication."))
                        success = connect(username, gss_auth=True)
        if success:
            message(_("Connected successfully."))
        return success

    def _check_key_password(self, key_filename, password):
        for handler in (paramiko.RSAKey, paramiko.DSSKey, paramiko.ECDSAKey):
            try:
                handler.from_private_key_file(key_filename, password)
                return True
            except:
                continue
        return False

    def _upgrade(self, connection_parameters, path):
        progress = ProgressDialog(_("Upgrade"))
        client = self._authenticate(progress, lambda params: ssh_connect(**params),
                                    connection_parameters)
        if not client:
            return _(u"Couldn't connect to upgrade server.")
        sftp = client.open_sftp()
        upgrade_file = sftp.open(path)
        # Unpack the upgrade file and replace the current installation.
        install_directory = os.path.normpath(os.path.join(sys.path[0], '..'))
        old_install_directory = install_directory + '.old'
        tmp_directory = tempfile.mkdtemp(prefix='pytisupgrade')
        pytis_directory = os.path.join(tmp_directory, 'pytis2go', 'pytis')
        scripts_directory = os.path.join(tmp_directory, 'pytis2go', 'scripts')
        scripts_install_dir = os.path.normpath(os.path.join(install_directory, '..', 'scripts'))
        tarfile.open(fileobj=upgrade_file).extractall(path=tmp_directory)
        if not os.path.isdir(pytis_directory):
            return _(u"Package unpacking failed.")
        if os.path.exists(old_install_directory):
            shutil.rmtree(old_install_directory)
        shutil.move(install_directory, old_install_directory)
        shutil.move(pytis_directory, install_directory)
        shutil.rmtree(old_install_directory)
        xconfig = os.path.join(os.path.expanduser('~'), '.x2goclient', 'xconfig')
        if os.access(xconfig, os.W_OK):
            os.remove(xconfig)
        if os.access(scripts_directory, os.R_OK):
            for fname in os.listdir(scripts_directory):
                fpath = os.path.join(scripts_directory, fname)
                dpath = os.path.normpath(os.path.join(scripts_install_dir, fname))
                if os.access(dpath, os.W_OK):
                    os.remove(dpath)
                try:
                    shutil.move(fpath, scripts_install_dir)
                except Exception:
                    pass
        # Execute supplied update procedure if it exists.
        if os.access(os.path.join(tmp_directory, 'updatescript.py'), os.R_OK):
            sys.path.append(tmp_directory)
            try:
                import updatescript
            except:
                pass
            else:
                updatescript.run(version=pytis.x2goclient.X2GOCLIENT_VERSION, path=path)
        shutil.rmtree(tmp_directory)

    def _vbs_path(self, directory, profile_id):
        return os.path.join(directory, '%s__%s__%s__%s.vbs' % (
            self._broker.username(),
            self._broker.server(),
            dict(self._profiles)[profile_id]['server'],
            profile_id,
        ))

    def _scripts_directory(self):
        return os.path.normpath(os.path.join(sys.path[0], '..', '..', 'scripts'))

    def _local_username(self):
        if pytis.util.on_windows():
            userp = os.environ.get('userprofile')
            if not isinstance(userp, unicode):
                userp = userp.decode(sys.getfilesystemencoding())
                username = os.path.split(userp)[-1] or ''
        else:
            username = os.environ.get('USER', '')
        if isinstance(username, unicode):
            username = username.encode('utf-8')
        return username

    def _desktop_shortcuts(self):
        import winshell
        directory = winshell.desktop()
        for name in os.listdir(directory):
            if os.path.splitext(name)[1].lower() == '.lnk':
                filename = os.path.join(directory, name)
                if os.path.isfile(filename):
                    try:
                        with winshell.shortcut(filename) as shortcut:
                            yield shortcut
                    except Exception:
                        pass

    def _shortcut_exists(self, profile_id):
        vbs_path = self._vbs_path(self._scripts_directory(), profile_id)
        if not os.path.exists(vbs_path):
            return False
        for shortcut in self._desktop_shortcuts():
            if shortcut.path.lower() == vbs_path.lower():
                return True
        return False

    def _create_shortcut_menu(self):
        menu = wx.Menu()
        for profile_id, parameters in self._profiles:
            item = wx.MenuItem(menu, -1, parameters['profile_name'])
            menu.Bind(wx.EVT_MENU,
                      lambda event, profile_id=profile_id: self._create_shortcut(profile_id),
                      id=item.GetId())
            menu.Enable(item.GetId(), not self._shortcut_exists(profile_id))
            menu.AppendItem(item)
        self._icon.PopupMenu(menu)
        menu.Destroy()

    def _create_shortcut(self, profile_id):
        import winshell
        directory = self._scripts_directory()
        if not os.path.isdir(directory):
            # TODO: Specific dialog for error messages (icons)?
            self._info_dialog(_("Failed creating desktop shortcut"),
                              _("Unable to find the scripts directory: %s", directory))
            return
        vbs_path = self._vbs_path(directory, profile_id)
        # Create the VBS script to which the shortcut will point to.
        profile_name = dict(self._profiles)[profile_id]['profile_name']
        if not os.path.exists(vbs_path):
            vbs_script = '\n'.join((
                "'{}".format(profile_name),
                'dim scriptdir, appshell',
                'Set appshell = CreateObject("Shell.Application")',
                'appshell.MinimizeAll',
                'Set fso = CreateObject("Scripting.FileSystemObject")',
                'scriptdir = fso.GetParentFolderName(Wscript.ScriptFullName)',
                'Set WshShell = CreateObject("WScript.Shell")',
                'WshShell.CurrentDirectory = scriptdir',
                ('WshShell.RUN "cmd /c {} {} '
                 '--add-to-known-hosts '
                 '--heading=""{}"" '
                 '--broker-url={} -P {}" , 0'.format(
                     sys.executable,
                     os.path.abspath(sys.argv[0]),
                     profile_name,
                     self._broker.url(),
                     profile_id)),
            ))
            with open(vbs_path, 'w') as f:
                f.write(vbs_script)
        # Create the shortcut on the desktop.
        shortcut_path, n = os.path.join(winshell.desktop(), '%s.lnk' % profile_name), 0
        while os.path.exists(shortcut_path):
            # If the shortcut of given name already exists, it is probably something else as
            # it was not found by _shortcut_exists(), so try to find the first unused name.
            n += 1
            shortcut_path = os.path.join(winshell.desktop(), '%s(%d).lnk' % (profile_name, n))
        with winshell.shortcut(shortcut_path) as link:
            link.path = vbs_path
            link.description = profile_id
            link.working_directory = directory
            icon_location = os.path.normpath(os.path.join(directory, '..', 'icons', 'p2go.ico'))
            if os.path.exists(icon_location):
                link.icon_location = (icon_location, 0)
        self._info_dialog(_("Shortcut created"), _("Shortcut created successfully."))

    def _cleanup_shortcuts(self):
        """Cleanup desktop shortcuts."""
        shortcuts = [x for x in self._desktop_shortcuts() if not os.path.isfile(x.path)]
        if shortcuts:
            confirmed = self._checklist_dialog(
                title=_("Confirm shortcuts removal"),
                message=(_("The following desktop shortcuts are invalid.") + "\n" +
                         _("Press Ok to remove the checked items.")),
                columns=(_("Name"),),
                items=[(True, os.path.splitext(os.path.basename(shortcut.lnk_filepath))[0],)
                       for shortcut in shortcuts],
            )
            n = 0
            for shortcut, checked in zip(shortcuts, confirmed):
                if checked:
                    os.remove(shortcut.lnk_filepath)
                    n += 1
            self._info_dialog(_("Invalid shortcuts removed"),
                              _.ngettext("%d shortcut removed succesfully.",
                                         "%d shortcuts removed succesfully.", n))
        else:
            self._info_dialog(_("All shortcuts ok"), _("No invalid shortcut found."))

    def _check_password(self, passwd):
        """Simple password validator."""
        import string
        set_digits = set(string.digits)
        set_lower = set(string.ascii_lowercase)
        set_upper = set(string.ascii_uppercase)
        allowed_chars = string.digits + string.ascii_letters + string.punctuation
        if any(c not in allowed_chars for c in passwd):
            return 'unallowed'
        if len(passwd) < 10:
            return 'short'
        if not all(set(passwd) & s for s in (set_digits, set_lower, set_upper)):
            return 'weak'
        else:
            return None

    def _write_key(self, key, key_file, passwd):
        """Write given key to private and public key files."""
        # Write private part
        key.write_private_key_file(key_file, password=passwd)
        # Write public part
        username = self._local_username()
        with open(key_file + '.pub', 'w') as f:
            f.write(key.get_name())
            f.write(key.get_base64())
            f.write(str(" "))
            f.write(username)

    def _generate_key(self):
        """Generate new SSH key pair."""
        sshdir = os.path.join(os.path.expanduser('~'), '.ssh')
        if not os.path.exists(sshdir):
            os.mkdir(sshdir)
        # Check if key exists
        key_file = None
        for name in ('id_rsa', 'id_dsa'):
            fname = os.path.join(sshdir, name)
            if os.access(fname, os.R_OK):
                key_file = fname
                break
        if key_file:
            self._info_dialog(_("Generate key"), _("An existing key found: %s", key_file))
            return
        key_file = os.path.join(sshdir, 'id_rsa')
        passwd = self._passphrase_dialog(_("Enter new key passphrase"), check=self._check_password)
        if passwd:
            key = paramiko.RSAKey.generate(2048)
            self._write_key(key, key_file, passwd)
            if os.access(key_file, os.R_OK):
                self._info_dialog(_("Generate key"), _("Keys created in directory: %s", sshdir))

    def _change_key_passphrase(self):
        """Change key passphrase."""
        key_filename = None
        for name in ('id_rsa', 'id_dsa', 'id_ecdsa',):
            f = os.path.join(os.path.expanduser('~'), '.ssh', name)
            if os.access(f, os.R_OK):
                key_filename = f
                break
        while True:
            key_filename, old_passphrase = self._keyfile_passphrase_dialog(key_filename)
            if key_filename is None:
                return
            else:
                key = None
                for handler in (paramiko.RSAKey, paramiko.DSSKey, paramiko.ECDSAKey):
                    try:
                        key = handler.from_private_key_file(key_filename, old_passphrase)
                    except:
                        break
                if key:
                    break
                else:
                    if self._question_dialog(_("Question"),
                                             _("Wrong passphrase! Try again?")):
                        continue
                    else:
                        return
        new_passphrase = self._passphrase_dialog(_("Enter new key passphrase"),
                                                 check=self._check_password)
        if new_passphrase:
            self._write_key(key, key_filename, new_passphrase)
            self._info_dialog(_("Passphrase changed"),
                              _("Passphrase changed for: %s", key_filename))

    def _upload_key(self):
        """Upload user's public key to the broker server."""
        key, key_file = self._choose_key()
        if key and key_file:
            username = self._local_username()
            upload_key = paramiko.RSAKey.from_private_key(StringIO.StringIO(pytis.config.upload_key))
            pubkey = "{} {} {}".format(key.get_name(), key.get_base64(), username)
            filename = "{}_{}.pub".format(datetime.datetime.now().strftime("%Y%m%d%H%M"), username)
            transport = paramiko.Transport((self._broker.server(), self._broker.port()))
            try:
                transport.connect(username='p2gokeys', pkey=upload_key)
                sftp = paramiko.SFTPClient.from_transport(transport)
                with sftp.open('p2gokeys/{}'.format(filename), 'w') as f:
                    f.write(pubkey)
            finally:
                transport.close()

    def _send_key(self):
        """Send public key to admin."""
        ssh_dir = os.path.join(os.path.expanduser('~'), '.ssh')
        msg = _("Use your usual email application (Thunderbird, Outlook)\n"
                "to send the public key file as an attachment.\n\n"
                "Your public key file should have an extension '.pub' (e.g. id_rsa.pub)\n"
                "and it is located in the directory {}.")
        self._info_dialog(_("Send key to admin"), msg.format(ssh_dir))

    def OnInit(self):
        self._icon = self._TaskBarIcon(self._on_taskbar_click)
        self._icon.update_menu(self._menu_items())
        # Work around: The wx main loop exits if there is not at least one frame.
        wx.Frame(None, -1, '').Hide()
        if pytis.util.on_windows():
            progress = ProgressDialog(_("X-server startup"))
            progress.message(_("Starting up X-server."))
            self._xserver = XServer()
            progress.close()
        self.Yield()
        profile_id = self._args.session_profile
        if profile_id or self._args.autoload:
            self._load_profiles()
        if profile_id:
            self._start_session(dict(self._profiles)[profile_id])
        return True
