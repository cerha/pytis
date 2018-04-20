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
import wx.lib.mixins.listctrl

import pytis.util
import pytis.x2goclient
from .ssh import public_key_acceptable, ssh_connect
from .clientprocess import Broker, ClientProcess, start_xserver

_ = pytis.util.translations('pytis-x2go')


class CheckListCtrl(wx.ListCtrl, wx.lib.mixins.listctrl.CheckListCtrlMixin):
    def __init__(self, *args, **kwargs):
        wx.ListCtrl.__init__(self, *args, **kwargs)
        wx.lib.mixins.listctrl.CheckListCtrlMixin.__init__(self)


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

    class column(object):
        def __init__(self, label, align='left', width=None):
            assert align in ('left', 'right')
            self.label = label
            self.align = align
            self.width = width

    @staticmethod
    def _init_listctrl(ctrl, columns, items, on_activation=None):
        for i, column in enumerate(columns):
            ctrl.InsertColumn(i, column.label,
                              wx.LIST_FORMAT_LEFT if column.align == 'left' else
                              wx.LIST_FORMAT_RIGHT)
        for i, row in enumerate(items):
            ctrl.InsertStringItem(i, '')
            for j, value in enumerate(row):
                ctrl.SetStringItem(i, j, value)
        for i, column in enumerate(columns):
            # Width must be set when values are present to autosize properly
            if column.width is None:
                width = wx.LIST_AUTOSIZE
            else:
                width = wx.DLG_SZE(ctrl, (4 * (column.width + 1)), 0).GetWidth()
            ctrl.SetColumnWidth(i, width)
        if on_activation:
            ctrl.Bind(wx.EVT_LIST_ITEM_ACTIVATED, on_activation)

    @staticmethod
    def listctrl(parent, columns, items=(), name=None, on_activation=None):
        """Create a list control with selectable items in tabular presentation.

        Arguments:
          columns -- sequence of 'ui.column' instances defining table column
            headers and their properties.
          items -- sequence of selectable items, where each item corresponds to
            one table row and contains as many string values as the number of
            'columns'.

        """
        ctrl = wx.ListCtrl(parent, -1, style=wx.LC_REPORT | wx.LC_SINGLE_SEL, name=name)
        ui._init_listctrl(ctrl, columns, items, on_activation)
        return ctrl

    @staticmethod
    def checklist(parent, columns, items, name=None):
        """Create a control to check/uncheck individual items in a tabular list.

        Arguments:
          columns -- sequence of 'ui.column' instances defining table column
            headers and their properties.
          items -- sequence of sequences, where the top level sequence
            determines the options which may be checked/unchecked individually
            (table rows) and the inner sequences determine the values displayed
            in table columns for given row plus the initial checbox state.  The
            first value in each inner sequence is a bool (True for a checked
            item, False for unchecked) and the following are the string values
            for table columns.  Thus each inner sequence has n + 1 items where
            n is the length of 'columns'.

        """
        ctrl = CheckListCtrl(parent, -1, style=wx.LC_REPORT)
        ui._init_listctrl(ctrl, columns, [row[1:] for row in items],
                          on_activation=lambda e: ctrl.ToggleItem(e.m_itemIndex))
        for i, row in enumerate(items):
            ctrl.CheckItem(i, row[0])
        # The space for the checkbox does not seem to be added automatically - add 20px.
        ctrl.SetColumnWidth(0, ctrl.GetColumnWidth(0) + 20)
        return ctrl

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


class MenuItem(object):
    def __init__(self, label, callback, icon=None, enabled=True, visible=True):
        self.label = label
        self.callback = callback
        self.icon = icon
        self.enabled = bool(enabled)
        self.visible = bool(visible)


class Session(threading.Thread):

    def __init__(self, client, session_parameters):
        self._client = client
        self._session_parameters = session_parameters
        threading.Thread.__init__(self)

    def session_parameters(self):
        return self._session_parameters

    def run(self):
        self._client.main_loop()

    def terminate(self):
        self._client.terminate()


class Pytis2GoApp(wx.App):
    """X2Go startup application."""

    class _TaskBarIcon(wx.TaskBarIcon):

        def __init__(self, menu, on_click):
            super(Pytis2GoApp._TaskBarIcon, self).__init__()
            self._menu = menu
            self.Bind(wx.EVT_TASKBAR_LEFT_DOWN, lambda e: on_click())

        def _get_bitmap(self, name):
            if name.startswith('wx'):
                bitmap = wx.ArtProvider_GetBitmap(name, wx.ART_MENU, (16, 16))
            else:
                path = os.path.normpath(os.path.join(sys.path[0], '..', 'icons', name + '.png'))
                bitmap = wx.Bitmap(path)
            return bitmap

        def set_icon(self, name):
            icon = wx.IconFromBitmap(self._get_bitmap(name))
            self.SetIcon(icon, _("Pytis2Go Service"))

        def CreatePopupMenu(self):
            menu = wx.Menu()
            for item in self._menu():
                if item == '---':
                    menu.AppendSeparator()
                elif item.visible:
                    wxitem = wx.MenuItem(menu, -1, item.label)
                    menu.Bind(wx.EVT_MENU, lambda event, item=item: item.callback(),
                              id=wxitem.GetId())
                    if item.icon:
                        wxitem.SetBitmap(self._get_bitmap(item.icon))
                    menu.AppendItem(wxitem)
                    menu.Enable(wxitem.GetId(), item.enabled)
            return menu

    def __init__(self, args):
        self._args = args
        self._profiles = []
        self._sessions = []
        self._session_manager_frame = None
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
        self._window_title = args.window_title or _("Pytis2Go")
        super(Pytis2GoApp, self).__init__(redirect=False)

    def _menu(self):
        running = [s.session_parameters()['profile_name'] for s in self._sessions if s.isAlive()]
        menu = [
            MenuItem(params['profile_name'], lambda params=params: self._start_session(params),
                     icon='connected' if params['profile_name'] in running else 'disconnected')
            for profile_id, params in self._profiles
        ]
        if menu:
            menu.append('---')
        menu.extend((
            MenuItem(_("Reload profiles") if menu else _("Load profiles"), self._load_profiles),
            '---',
            MenuItem(_("Generate new SSH key pair"), self._generate_key),
            MenuItem(_("Change key passphrase"), self._change_key_passphrase),
            MenuItem(_("Upload public key to server"), self._upload_key),
            MenuItem(_("Send public key to admin"), self._send_key),
            MenuItem(_("Create desktop shortcut"), self._create_shortcut_menu,
                     enabled=bool(menu), visible=pytis.util.on_windows()),
            MenuItem(_("Cleanup desktop shortcuts"), self._cleanup_shortcuts,
                     visible=pytis.util.on_windows()),
            MenuItem(_("Session Manager"), self._session_manager),
            '---',
            MenuItem(_("Exit"), self._on_exit, icon=wx.ART_QUIT),
        ))
        return menu

    def _on_exit(self):
        for session in self._sessions:
            if session.isAlive():
                session.terminate()
        wx.CallAfter(self._icon.Destroy)
        self.Exit()

    def _on_taskbar_click(self):
        pass

    def _show_dialog(self, title, create, focus):
        class Dialog(wx.Dialog):
            result = None

            def close(self, result):
                self.result = result
                self.Close()

            def widget(self, name):
                return self.FindWindowByName(name)

        dialog = Dialog(None, -1, title=title)
        content = create(dialog)
        dialog.SetSizer(content)
        content.Fit(dialog)
        if focus:
            dialog.widget(focus).SetFocus()
        dialog.ShowModal()
        dialog.Destroy()
        return dialog.result

    def _show_frame(self, title, create):
        class Frame(wx.Frame):
            def widget(self, name):
                return self.FindWindowByName(name)

        frame = Frame(None, -1, title=title)
        content = create(frame)
        frame.SetSizer(content)
        content.Fit(frame)
        frame.Show()
        return frame

    def _session_selection_dialog(self, progress, client, sessions):
        def create_dialog(dialog):
            def on_terminate_session(event):
                listctrl = dialog.widget('sessions')
                selection = listctrl.GetFirstSelected()
                session = sessions[selection]
                progress.message(_("Terminating session: %s", session.name))
                client.terminate_session(session)
                listctrl.DeleteItem(selection)
                del sessions[selection]
                progress.message(_("Session terminated: %s", session.name))

            def on_resume_session(event):
                listctrl = dialog.widget('sessions')
                dialog.close(sessions[listctrl.GetFirstSelected()])

            def update_ui(event):
                event.Enable(dialog.widget('sessions').GetFirstSelected() != -1)

            return ui.vgroup(
                ui.label(dialog, _("Suspended sessions:")),
                ui.item(ui.hgroup(
                    ui.item(ui.listctrl(dialog, name='sessions', on_activation=on_resume_session,
                                        columns=(ui.column(_("Session"), width=24),
                                                 ui.column(_("Date"), width=18)),
                                        items=[('%s@%s' % (s.username, s.hostname),
                                                  (s.date_created or '').replace('T', ' '))
                                                 for s in sessions]),
                            proportion=1, expand=True),
                    ui.item(ui.vgroup(*[
                        ui.button(dialog, label, callback, update_ui, disabled=True)
                        for label, callback in (
                                (_(u"Resume"), on_resume_session),
                                (_(u"Terminate"), on_terminate_session),
                        )], spacing=6)),
                    spacing=8,
                ), proportion=1, expand=True),
                ui.button(dialog, _("Start New Session"), lambda e: dialog.close(None)),
                padding=14, spacing=3,
            )
        return self._show_dialog(_("Select session"), create_dialog, 'sessions')

    def _session_manager(self):
        if self._session_manager_frame:
            self._session_manager_frame.Show()
        else:
            def create(frame):
                def on_terminate_session(event):
                    listctrl = frame.widget('sessions')
                    selection = listctrl.GetFirstSelected()
                    sessions[selection][0].terminate()
                    listctrl.DeleteItem(selection)
                    del sessions[selection]

                def update_ui(event):
                    event.Enable(frame.widget('sessions').GetFirstSelected() != -1)

                sessions = [(s, s.session_parameters()) for s in self._sessions if s.isAlive()]
                return ui.vgroup(
                    ui.label(frame, _("Running sessions:")),
                    ui.item(ui.listctrl(frame, name='sessions',
                                        columns=(ui.column(_("Session"), width=24),
                                                 ui.column(_("Server"), width=18)),
                                        items=[(p['profile_name'], p['server'])
                                               for s, p in sessions]),
                            proportion=1, expand=True),
                    ui.item(ui.hgroup(*[
                        ui.button(frame, label, callback, update_ui, disabled=True)
                        for label, callback in (
                                (_(u"Terminate"), on_terminate_session),
                        )], spacing=6, padding=(3, 0))),
                    padding=14, spacing=3,
                )
            self._session_manager_frame = frame = self._show_frame(_("Session Manager"), create)
            frame.widget('sessions').SetFocus()

    def _question_dialog(self, title, question):
        def create_dialog(dialog):
            return ui.vgroup(
                ui.label(dialog, question),
                ui.item(ui.hgroup(
                    ui.button(dialog, _(u"Yes"), lambda e: dialog.close(True), name='yes'),
                    ui.button(dialog, _(u"No"), lambda e: dialog.close(False)),
                    spacing=10,
                ), align=ui.CENTER),
                padding=10, spacing=10,
            )
        return self._show_dialog(title, create_dialog, 'yes')

    def _load_profiles(self):
        progress = ProgressDialog(_("%s: Loading profiles...", self._window_title))
        profiles = self._broker.list_profiles(lambda f, params:
                                              self._authenticate(progress, f, params))
        if profiles:
            # Profiles are empty when the user cancels the broker authentication dialog
            # or when connection or authentication fails.
            self._profiles = profiles
            self._icon.set_icon('connected')
            progress.message(self._broker.server() + ': ' +
                             _.ngettext("Returned %d profile.",
                                        "Returned %d profiles.",
                                        len(profiles)))
            time.sleep(2)
        progress.close()
        if profiles and pytis.util.on_windows():
            current_version = pytis.x2goclient.X2GOCLIENT_VERSION
            available_version, connection_parameters, path = self._broker.upgrade_parameters()
            if ((available_version and available_version > current_version and
                self._question_dialog(_("Upgrade available"),
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

    def _get_display(self, session_parameters):
        # TODO: We would like to start X-servers here according to session
        # parameters (different parameters may require different server setup).
        if pytis.util.on_windows():
            return self._display
        else:
            return None

    def _connect(self, session_parameters):
        # Authenticate to server and return session_parameters including
        # also all necessary authentication parameters.
        connection_parameters = dict((k, session_parameters[k]) for k in
                                     ('server', 'port', 'username', 'password',
                                      'key_filename', 'allow_agent', 'gss_auth',
                                      'add_to_known_hosts'))
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
        client = ClientProcess(session_parameters, display=self._get_display(session_parameters))
        progress.message(_("Retrieving available sessions."))
        sessions = client.list_sessions()
        if len(sessions) == 0:
            session = None
        else:
            session = self._session_selection_dialog(progress, client, sessions)
        if session:
            progress.message(_("Resuming session: %s", session.name))
            client.resume_session(session)
            delay = 2000
        else:
            progress.message(_("Starting new session."))
            client.start_new_session()
            delay = 11000
        progress.message(_("Waiting for the application to come up..."))
        wx.CallLater(delay, progress.close)
        session = Session(client, session_parameters)
        self._sessions.append(session)
        session.start()

    def _info_dialog(self, title, text):
        def create_dialog(dialog):
            return ui.vgroup(
                ui.label(dialog, text),
                ui.item(
                    ui.button(dialog, _(u"Ok"), lambda e: dialog.close(None), name='button'),
                    align=ui.CENTER,
                ),
                padding=10, spacing=10,
            )
        return self._show_dialog(title, create_dialog, 'button')

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
        return self._show_dialog(_("Log in to %s", server), create_dialog, 'password')

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
          columns -- sequence of checklist columns as ui.column instances.
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
            return ui.vgroup(
                ui.label(dialog, message),
                ui.item(
                    ui.checklist(dialog, columns, items, name='checklist'),
                    proportion=1, expand=True,
                ),
                ui.item(
                    ui.hgroup(
                        ui.button(dialog, _(u"Ok"),
                                  lambda e: dialog.close([checklist.IsChecked(i)
                                                          for i in range(len(items))])),
                        ui.button(dialog, _(u"Cancel"),
                                  lambda e: dialog.close(None)),
                        spacing=20,
                    ),
                    align=ui.CENTER, padding=(0, 12),
                ),
                padding=14, spacing=14,
            )
        return self._show_dialog(title, create_dialog, 'checklist')

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

        Returns the result of the first successful call of 'function' or None if
          A) the user aborted the authentication dialog (authentication failed),
          B) connection failed.
        In case B) the error has been retorted to the user by displaying a dialog
        which the user needs to confirm.

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

        success = None
        message(_("Retrieving supported authentication methods."))
        try:
            methods = self._authentication_methods(connection_parameters)
        except socket.error as e:
            self._info_dialog(_("Connection Failed"),
                              _("Failed connecting to %s:\n%s", connection_parameters['server'], e))
            return None
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
                columns=(ui.column(_("Name")),),
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
            upload_key = paramiko.RSAKey.from_private_key(
                StringIO.StringIO(pytis.config.upload_key)
            )
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

    def init(self):
        # This init is separated from OnInit in order to avoid running
        # long running tasks in the constructor.  This makes instance
        # creation fast and the instance is available for the RPyC server
        # in pytis2go.py immediately.
        if pytis.util.on_windows():
            progress = ProgressDialog(_("X-server startup"))
            progress.message(_("Starting up X-server."))
            # TODO - because of http://bugs.x2go.org/cgi-bin/bugreport.cgi?bug=1044
            # we use older variant of VcXsrv by default. Later we will switch back
            # to the current version.
            self._display = start_xserver('VcXsrv_pytis_old')
            progress.close()
            if not self._display:
                self._info_dialog(self._window_title, _("Failed starting X-server."))
                self.Exit()
        if self._args.autoload or self._args.profile:
            self._load_profiles()
        if self._args.profile:
            self._start_session(dict(self._profiles)[self._args.profile])

    def start_session(self, profile_id):
        # Used by the RPyC server in pytis2go.py.
        def start_session():
            if not self._profiles:
                self._load_profiles()
            try:
                session_parameters = dict(self._profiles)[profile_id]
            except KeyError:
                return False
            else:
                self._start_session(session_parameters)
        wx.CallAfter(start_session)

    def OnInit(self):
        self._icon = self._TaskBarIcon(self._menu, self._on_taskbar_click)
        self._icon.set_icon('disconnected')
        # Work around: The wx main loop exits if there is not at least one frame.
        wx.Frame(None, -1, '').Hide()
        self.Yield()
        return True
