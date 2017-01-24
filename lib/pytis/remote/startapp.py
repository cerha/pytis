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


class X2GoStartApp(wx.App):
    """X2Go startup application."""

    _MAX_PROGRESS = 40

    def __init__(self, args):
        from pytis.remote.x2goclient import X2GoStartAppClientAPI
        self._progress = 1
        self._args = args
        self._client = X2GoStartAppClientAPI(args, self._update_progress)
        self._username = args.username
        self._authentication = None
        super(X2GoStartApp, self).__init__(redirect=False)

    def _on_submit_authentication(self, event):
        self._authentication_dialog_confirmed = True
        dialog = event.GetEventObject()
        while not isinstance(dialog, wx.Dialog):
            dialog = dialog.GetParent()
        dialog.Close()

    def _on_select_profile(self, event):
        selection = self._profiles_field.GetSelection()
        profile_id = self._profiles_field.GetClientData(selection)
        self._client.select_profile(profile_id)
        self._connect()

    def _on_terminate_session(self, event):
        selection = self._sessions_field.GetSelection()
        session = self._sessions_field.GetClientData(selection)
        self._update_progress(_("Terminating session: %s", session.name), 0)
        self._client.terminate_session(session)
        self._sessions_field.Delete(selection)
        self._update_progress(_("Session terminated: %s", session.name), 0)

    def _on_resume_session(self, event):
        session = self._sessions_field.GetClientData(self._sessions_field.GetSelection())
        self._update_progress(_("Resuming session: %s", session.name), 10)
        self._client.resume_session(session, callback=self._on_session_started)

    def _on_new_session(self, event):
        self._update_progress(_("Starting new session."), 10)
        self._client.start_new_session(callback=self._on_session_started)

    def _on_session_started(self):
        self.ExitMainLoop()
        self._frame.Show(False)
        wx.Yield()

    def _on_exit(self, event):
        self.Exit()

    def _create_username(self, parent):
        label = wx.StaticText(parent, -1, _("Login name:"))
        username = self._username
        if not username:
            import getpass
            username = getpass.getuser()  # x2go.defaults.CURRENT_LOCAL_USER,
            button = ui.button(parent, _("Continue"), lambda e: self._start())
        else:
            button = None
        self._username_field = field = ui.field(parent, username, disabled=button is None)
        return ui.hgroup((label, 0, wx.RIGHT | wx.TOP, 2), field, button)

    def _create_profiles_field(self, parent):
        if self._args.broker_url is None or self._args.session_profile is not None:
            self._profiles_field = None
        else:
            self._profiles_field = listbox = wx.ListBox(parent, -1, choices=(), style=wx.LB_SINGLE)
            listbox.Enable(False)
            button = ui.button(parent, _("Select Profile"), self._on_select_profile,
                               lambda e: e.Enable(listbox.GetSelection() != -1))
            checkbox = wx.CheckBox(parent, label=_("Create despktop shortcut"))
            return ui.vgroup(
                wx.StaticText(parent, -1, _("Available profiles:")),
                (ui.hgroup(
                    (listbox, 1, wx.EXPAND),
                    (ui.vgroup(button, (checkbox, 0, wx.TOP, 10)), 0, wx.LEFT, 8),
                ), 1, wx.EXPAND)
            )

    def _create_sessions_field(self, parent):
        self._sessions_field = listbox = wx.ListBox(parent, -1, choices=(), style=wx.LB_SINGLE)
        listbox.Enable(False)
        return ui.vgroup(
            wx.StaticText(parent, -1, _("Existing sessions:")),
            (ui.hgroup(
                (listbox, 1, wx.EXPAND),
                (ui.vgroup(*[
                    (ui.button(parent, label, callback, updateui, disabled=True),
                     0, wx.BOTTOM, 2)
                    for label, callback, updateui in (
                        (_(u"Resume"), self._on_resume_session,
                         lambda e: e.Enable(self._sessions_field.GetSelection() != -1)),
                        (_(u"Terminate"), self._on_terminate_session,
                         lambda e: e.Enable(self._sessions_field.GetSelection() != -1)),
                    )]), 0, wx.LEFT, 8)),
             1, wx.EXPAND),
            (ui.hgroup(
                ui.button(parent, _("Start New Session"), self._on_new_session,
                          lambda e: e.Enable(self._sessions_field.IsEnabled())),
                (ui.button(parent, _("Exit"), self._on_exit), 0, wx.LEFT, 10),
            ), 0, wx.TOP, 8),
        )

    def _create_status(self, parent):
        self._status = status = wx.StaticText(parent, -1, '')
        self._gauge = gauge = wx.Gauge(parent, -1, self._MAX_PROGRESS)
        return ui.vgroup((gauge, 0, wx.EXPAND), (status, 0, wx.EXPAND | wx.BOTTOM, 4))

    def _create_main_content(self, parent):
        return ui.vgroup(
            (self._create_username(parent), 0, wx.EXPAND | wx.ALL, 8),
            (self._create_profiles_field(parent), 1, wx.EXPAND | wx.ALL, 8),
            (self._create_sessions_field(parent), 1, wx.EXPAND | wx.ALL, 8),
            (self._create_status(parent), 0, wx.EXPAND),
        )

    def _create_publickey_authentication(self, parent):
        path = os.path.join(os.path.expanduser('~'), '.ssh', '')
        def on_select_key_file(event):
            filename = wx.FileSelector(_(u"Select ssh key file"), default_path=path)
            self._keyfile_field.SetValue(filename)
        label1 = wx.StaticText(parent, -1, _("Key File:"))
        keys = [name for name in ('id_rsa', 'id_dsa', 'id_ecdsa')
                if os.access(os.path.join(path, name), os.R_OK)]
        filename = os.path.join(path, keys[0]) if len(keys) == 1 else None
        self._keyfile_field = field1 = ui.field(parent, filename, length=40, style=wx.TE_READONLY)
        button1 = ui.button(parent, _("Select"), on_select_key_file)
        label2 = wx.StaticText(parent, -1, _("Passphrase:"))
        self._passphrase_field = field2 = ui.field(parent, length=28, style=wx.PASSWORD,
                                                   on_enter=self._on_submit_authentication)
        return ui.vgroup(
            (ui.hgroup((label1, 0, wx.RIGHT | wx.TOP, 2), field1,
                       (button1, 0, wx.LEFT, 3)), 0, wx.ALL, 2),
            (ui.hgroup((label2, 0, wx.RIGHT | wx.TOP, 2), field2), 0, wx.ALL, 2),
        )

    def _create_password_authentication(self, parent):
        label = wx.StaticText(parent, -1, _("Password:"))
        self._password_field = field = ui.field(parent, style=wx.PASSWORD)
        return ui.hgroup((label, 0, wx.RIGHT | wx.TOP, 2), field)

    def _show_authentication_dialog(self, methods):
        self._authentication_dialog_confirmed = False
        dialog = wx.Dialog(self._frame, -1, title=_("Authentication"))
        if 'password' in methods and 'publickey' in methods:
            nb = wx.Notebook(dialog, -1)
            nb.AddPage(ui.panel(nb, self._create_publickey_authentication), _(u"Public Key"))
            nb.AddPage(ui.panel(nb, self._create_password_authentication), _(u"Password"))
            content = nb
            method = None
        elif 'publickey' in methods:
            content = ui.panel(dialog, self._create_publickey_authentication)
            method = 'publickey'
        elif 'password' in methods:
            content = ui.panel(dialog, self._create_password_authentication)
            method = 'password'
        else:
            raise Exception(_(u"No supported ssh connection method available"))
        dialog.SetSizer(ui.vgroup(
            content,
            (ui.hgroup(
                (ui.button(dialog, _("Log in"), self._on_submit_authentication), 0, wx.RIGHT, 20),
                ui.button(dialog, _("Cancel"), lambda e: dialog.Close()),
            ), 0, wx.ALIGN_CENTER | wx.ALL, 14),
        ))
        dialog.Fit()
        for f in self._password_field, self._passphrase_field:
            if f.IsShown():
                f.SetFocus()
        pos, fsize, dsize = self._frame.GetPosition(), self._frame.GetSize(), dialog.GetSize()
        dialog.SetPosition((pos.x + (fsize.width - dsize.width) / 2, pos.y + 40))
        dialog.ShowModal()
        dialog.Destroy()
        self._frame.Raise()
        if self._authentication_dialog_confirmed:
            if method is None:
                method = 'publickey' if nb.GetSelection() == 0 else 'password'
            if method == 'password':
                fields = (('password', self._password_field),)
            else:
                fields = (('key_filename', self._keyfile_field),
                          ('password', self._passphrase_field),)
            return (method, dict([(param, f.GetValue().rstrip('\r\n')) for param, f in fields]))
        else:
            return (None, ())

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
        username = self._username_field.GetValue()
        if self._client.connect(username, self._show_authentication_dialog):
            #if on_windows() and args.create_shortcut:
            #    self._update_progress(_("Checking desktop shortcut."))
            #    self._create_shortcut(broker_url, args.server, profile_id,
            #                          params['profile_name'], args.calling_script)
            self._update_progress(_("Starting Pytis client."))
            self._load_sessions()
        else:
            self.Exit()

    def _load_profiles(self):
        username = self._username_field.GetValue()
        profiles = self._client.list_profiles(username, self._show_authentication_dialog)
        profile_id = self._args.session_profile
        if self._args.list_profiles:
            self._list_profiles(profiles)
            self.Exit()
        elif profile_id:
            if profile_id not in profiles.profile_ids:
                raise Exception("Unknown profile %s!" % profile_id)
            self._client.select_profile(profile_id)
            self._connect()
        else:
            items = [(profile_id, profiles.to_session_params(profile_id)['profile_name'])
                     for profile_id in profiles.profile_ids]
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

    def _load_sessions(self):
        self._check_upgrade()
        args = self._args
        if ((args.share_desktop or args.suspend or args.terminate or args.list_sessions or
             args.list_desktops or args.list_profiles)):
            return
        self._update_progress(_("Retrieving available sessions."))
        for session in self._client.list_sessions():
            label = '%s@%s %s' % (session.username or '', session.hostname or '',
                                  (session.date_created or '').replace('T', ' '),)
            self._sessions_field.Append(label, session)
        self._sessions_field.Enable(True)
        self._update_progress(_("Resume an existing session or start a new one."))

    def _check_upgrade(self):
        if self._args.broker_url is not None:
            self._update_progress(_("Checking for new client version."))
            if self._client.needs_upgrade():
                return # TODO: finish
                if self._question(_(u"New pytis client version available. Install?")):
                    self._client.pytis_upgrade()

    def OnInit(self):
        self._frame = frame = wx.Frame(None, -1, _("Starting application"))
        ui.panel(frame, self._create_main_content)
        self.SetTopWindow(frame)
        frame.SetSize((500, 500 if self._profiles_field else 300))
        frame.Show()
        self.Yield()
        if self._username is not None:
            # Start automatically when username was passed explicitly.
            self._start()
        else:
            self._update_progress(_("Enter your user name and press “Continue” to start."))
        return True
