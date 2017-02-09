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

def _(x, *args, **kwargs):
    return x % (args or kwargs) if args or kwargs else x

class ui(object):
    """Private helper methods for simple UI construction (not to be used outside this module)."""

    @staticmethod
    def _add_to_sizer(sizer, items, expand=False):
        for item in items:
            if not isinstance(item, (tuple, list)):
                item = (item,)
            if item[0]:
                sizer.Add(*item)
        return sizer

    @staticmethod
    def vgroup(*items):
        return ui._add_to_sizer(wx.BoxSizer(wx.VERTICAL), items)

    @staticmethod
    def hgroup(*items):
        return ui._add_to_sizer(wx.BoxSizer(wx.HORIZONTAL), items)

    @staticmethod
    def vbox(parent, label, items):
        box = wx.StaticBox(parent, label=label)
        return ui._add_to_sizer(wx.StaticBoxSizer(box, wx.VERTICAL), items)

    @staticmethod
    def panel(parent, method, *args, **kwargs):
        p = wx.Panel(parent)
        p.SetSizer(method(p, *args, **kwargs))
        return p

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
    def button(parent, label, callback, updateui=None, disabled=False):
        button = wx.Button(parent, -1, label=label)
        button.Bind(wx.EVT_BUTTON, callback)
        if updateui:
            button.Bind(wx.EVT_UPDATE_UI, updateui)
        if disabled:
            button.Enable(False)
        return button

    @staticmethod
    def label(parent, text):
        return wx.StaticText(parent, -1, text)


class X2GoStartApp(wx.App):
    """X2Go startup application."""

    _MAX_PROGRESS = 40

    def __init__(self, args):
        from pytis.remote.x2goclient import StartupController
        self._progress = 1
        self._args = args
        self._controller = StartupController(args, self._update_progress)
        self._keyring = []
        super(X2GoStartApp, self).__init__(redirect=False)

    def _selected_profile_id(self):
        return self._profiles_field.GetClientData(self._profiles_field.GetSelection())

    def _on_select_profile(self, event):
        profile_id = self._selected_profile_id()
        self._update_progress(_("Selected profile %s: Contacting server...") % profile_id)
        self._controller.select_profile(profile_id)
        self._connect()

    def _on_create_shortcut(self, event):
        error = self._controller.create_shortcut(self._username(), self._selected_profile_id())
        if error:
            # TODO: Specific dialog for error messages (icons)?
            self._info(_("Failed creating desktop shortcut"), error)
        else:
            self._update_progress(_("Shortcut created successfully."))

    def _can_create_shortcut(self):
        return not self._controller.shortcut_exists(self._username(), self._selected_profile_id())

    def _on_session_started(self):
        self.ExitMainLoop()
        self._frame.Show(False)
        wx.Yield()

    def _on_exit(self, event):
        self.Exit()

    def _create_username_field(self, parent):
        label = ui.label(parent, _("Login name:"))
        username = self._args.username
        if not username and self._args.broker_url:
            username = self._controller.broker_url_username()
        if not username:
            import getpass
            username = getpass.getuser()  # x2go.defaults.CURRENT_LOCAL_USER,
            button = ui.button(parent, _("Continue"), lambda e: self._start())
        else:
            button = None
        self._username_field = field = ui.field(parent, username, disabled=button is None)
        return ui.hgroup((label, 0, wx.RIGHT | wx.TOP, 2), field, button)

    def _username(self):
        return self._username_field.GetValue()

    def _create_profiles_field(self, parent):
        if self._args.broker_url is None or self._args.session_profile is not None:
            self._profiles_field = None
            return None
        else:
            self._profiles_field = listbox = wx.ListBox(parent, -1, choices=(), style=wx.LB_SINGLE)
            listbox.Enable(False)
            buttons = [(ui.button(parent, _("Start session"), self._on_select_profile,
                                  lambda e: e.Enable(listbox.GetSelection() != -1)),
                        0, wx.EXPAND)]
            if self._controller.on_windows() and self._args.calling_script:
                buttons.append((ui.button(parent, _("Create shortcut"), self._on_create_shortcut,
                                          lambda e: e.Enable(listbox.GetSelection() != -1 and
                                                             self._can_create_shortcut())),
                                0, wx.EXPAND | wx.TOP, 10))
            return ui.vgroup(
                ui.label(parent, _("Available profiles:")),
                (ui.hgroup(
                    (listbox, 1, wx.EXPAND),
                    (ui.vgroup(*buttons), 0, wx.LEFT, 8),
                ), 1, wx.EXPAND)
            )

    def _create_status(self, parent):
        self._status = status = ui.label(parent, '')
        self._gauge = gauge = wx.Gauge(parent, -1, self._MAX_PROGRESS)
        return ui.vgroup((gauge, 0, wx.EXPAND), (status, 0, wx.EXPAND | wx.BOTTOM, 4))

    def _create_main_content(self, parent):
        return ui.vgroup(
            (self._create_username_field(parent), 0, wx.EXPAND | wx.ALL, 8),
            (self._create_profiles_field(parent), 1, wx.EXPAND | wx.ALL, 8),
            (self._create_status(parent), 0, wx.EXPAND),
        )

    def _show_dialog(self, title, method, *args, **kwargs):
        class Dialog(wx.Dialog):
            result = None
            callback = None

            def close(self, result):
                self.result = result
                self.Close()

            def set_callback(self, callback):
                self.callback = callback
        dialog = Dialog(self._frame, -1, title=title)
        content = method(dialog, *args, **kwargs)
        dialog.SetSizer(content)
        dialog.Fit()
        pos, fsize, dsize = self._frame.GetPosition(), self._frame.GetSize(), dialog.GetSize()
        dialog.SetPosition((pos.x + (fsize.width - dsize.width) / 2, pos.y + 40))
        if dialog.callback:
            dialog.callback()
        dialog.ShowModal()
        dialog.Destroy()
        self._frame.Raise()
        return dialog.result

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
                    _(u"Select ssh key file"),
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
                (ui.hgroup((label1, 0, wx.RIGHT | wx.TOP, 2), field1,
                           (button1, 0, wx.LEFT, 3)), 0, wx.BOTTOM, 4),
                ui.hgroup((label2, 0, wx.RIGHT | wx.TOP, 2), field2),
            )

        def password_authentication(parent):
            label = ui.label(parent, _("Password:"))
            self._password_field = field = ui.field(parent, style=wx.PASSWORD,
                                                    on_enter=lambda e: close('password'))
            return ui.hgroup((label, 0, wx.RIGHT | wx.TOP, 2), field)

        def on_show_dialog():
            for f in [getattr(self, a, None) for a in ('_password_field', '_passphrase_field')]:
                if f and f.IsShown():
                    f.SetFocus()
        dialog.set_callback(on_show_dialog)
        if 'password' in methods and 'publickey' in methods:
            nb = wx.Notebook(dialog, -1)
            nb.AddPage(ui.panel(nb, publickey_authentication), _(u"Public Key"))
            nb.AddPage(ui.panel(nb, password_authentication), _(u"Password"))
            content = nb

            def method():
                return 'publickey' if nb.GetSelection() == 0 else 'password'

        elif 'publickey' in methods:
            content = ui.panel(dialog, publickey_authentication)
            method = 'publickey'
        elif 'password' in methods:
            content = ui.panel(dialog, password_authentication)
            method = 'password'
        else:
            raise Exception(_(u"No supported ssh connection method available"))
        return ui.vgroup(
            (content, 0, wx.LEFT | wx.RIGHT, 8),
            (ui.hgroup(
                (ui.button(dialog, _("Log in"), lambda e: close(method)), 0, wx.RIGHT, 20),
                ui.button(dialog, _("Cancel"), lambda e: close(None)),
            ), 0, wx.ALIGN_CENTER | wx.ALL, 14),
        )

    def _authentication_dialog(self, *args):
        return self._show_dialog(_("Authentication"), self._create_authentication_dialog, *args)

    def _session_selection_dialog(self, dialog, sessions):
        listbox = wx.ListBox(dialog, -1, choices=(), style=wx.LB_SINGLE)

        def on_terminate_session(event):
            selection = listbox.GetSelection()
            session = listbox.GetClientData(selection)
            self._update_progress(_("Terminating session: %s", session.name), 0)
            self._controller.terminate_session(session)
            listbox.Delete(selection)
            self._update_progress(_("Session terminated: %s", session.name), 0)

        for session in sessions:
            session_label = '%s@%s %s' % (session.username or '', session.hostname or '',
                                          (session.date_created or '').replace('T', ' '),)
            listbox.Append(session_label, session)
        dialog.set_callback(lambda: listbox.SetFocus())
        return ui.vgroup(
            ui.label(dialog, _("Existing sessions:")),
            (ui.hgroup(
                (listbox, 1, wx.EXPAND),
                (ui.vgroup(*[
                    (ui.button(dialog, label, callback, updateui, disabled=True), 0, wx.BOTTOM, 2)
                    for label, callback, updateui in (
                        (_(u"Resume"),
                         lambda e: dialog.close(listbox.GetClientData(listbox.GetSelection())),
                         lambda e: e.Enable(listbox.GetSelection() != -1)),
                        (_(u"Terminate"), on_terminate_session,
                         lambda e: e.Enable(listbox.GetSelection() != -1)),
                    )]), 0, wx.LEFT, 8)), 1, wx.EXPAND),
            (ui.button(dialog, _("Start New Session"), lambda e: dialog.close(None)), 0, wx.TOP, 8),
        )

    def _question(self, title, question):
        def create_dialog(dialog):
            buttons = (ui.button(dialog, _(u"Yes"), lambda e: dialog.close(True)),
                       ui.button(dialog, _(u"No"), lambda e: dialog.close(False)))
            dialog.set_callback(lambda: buttons[0].SetFocus())
            return ui.vgroup(
                (ui.label(dialog, question), 0, wx.ALL, 10),
                (ui.hgroup(*[(b, 0, wx.ALL, 10) for b in buttons]), 1, wx.ALIGN_CENTER),
            )
        return self._show_dialog(title, create_dialog)

    def _info(self, title, text):
        def create_dialog(dialog):
            button = ui.button(dialog, _(u"Ok"), lambda e: dialog.close(None))
            dialog.set_callback(lambda: button.SetFocus())
            return ui.vgroup(
                (ui.label(dialog, text), 0, wx.ALL, 10),
                (button, 1, wx.ALIGN_CENTER | wx.ALL, 10),
            )
        return self._show_dialog(title, create_dialog)

    def _update_progress(self, message=None, progress=1):
        self._gauge.SetValue(self._gauge.GetValue() + progress)
        if message:
            self._status.SetLabel(message)
        self.Yield()

    def _start(self):
        if self._args.broker_url:
            self._load_profiles()
        else:
            self._connect()

    def _connect(self):
        if self._controller.connect(self._username(), self._authentication_dialog, self._keyring):
            self._update_progress(_("Starting Pytis client."))
            self._start_session()
        else:
            self.Exit()

    def _load_profiles(self):
        profiles = self._controller.list_profiles(self._username(),
                                                  self._authentication_dialog, self._keyring)
        if not profiles:
            # Happens when the user cancels the broker authentication dialog.
            self.Exit()
        profile_id = self._args.session_profile
        if self._args.list_profiles:
            self._list_profiles(profiles)
            self.Exit()
        else:
            if self._controller.on_windows():
                current_version = self._controller.current_version()
                available_version = self._controller.available_upgrade_version()
                if ((available_version and available_version > current_version and
                     self._question(_("Upgrade available"),
                                    '\n'.join((_("New pytis client version available."),
                                               _("Current version: %s") % current_version,
                                               _("New version: %s") % available_version,
                                               _("Install?")))))):
                    error = self._controller.upgrade(self._username(),
                                                     self._authentication_dialog, self._keyring)
                    if error:
                        # TODO: Specific dialog for error messages (icons)?
                        self._info(_("Upgrade failed"), error)
                    else:
                        self._info(_(u"Upgrade finished"),
                                   _(u"Pytis successfully upgraded. Restart the application."))
                        self.Exit()
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

    def _start_session(self):
        args = self._args
        if ((args.share_desktop or args.suspend or args.terminate or args.list_sessions or
             args.list_desktops or args.list_profiles)):
            return
        self._update_progress(_("Retrieving available sessions."))
        sessions = self._controller.list_sessions()
        if len(sessions) == 0:
            session = None
        else:
            session = self._show_dialog(_("Select session"),
                                        self._session_selection_dialog, sessions)
        if session:
            self._update_progress(_("Resuming session: %s", session.name), 10)
            self._controller.resume_session(session, callback=self._on_session_started)
        else:
            self._update_progress(_("Starting new session."), 10)
            self._controller.start_new_session(callback=self._on_session_started)

    def OnInit(self):
        self._frame = frame = wx.Frame(None, -1, _("Starting application"))
        ui.panel(frame, self._create_main_content)
        self.SetTopWindow(frame)
        frame.SetSize((500, 360 if self._profiles_field else 146))
        frame.Show()
        self.Yield()
        if self._username_field.IsEnabled():
            self._update_progress(_("Enter your user name and press “Continue” to start."))
        else:
            # Start automatically when username was passed explicitly.
            self._start()
        return True
