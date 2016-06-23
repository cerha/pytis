#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011-2016 Brailcom, o.p.s.
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

import argparse
import gevent.monkey
gevent.monkey.patch_all()
import os
import time
import wx

_ = lambda x, *a, **kw: x % (a or kw) if a or kw else x


class ui(object):
    """Just a few helper methods for simple UI construction."""

    @staticmethod
    def _add_to_sizer(sizer, items, expand=False):
        for item in items:
            if not isinstance(item, (tuple, list)):
                item = (item,)
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
    def panel(parent, method):
        p = wx.Panel(parent)
        p.SetSizer(method(p))
        return p

    @staticmethod
    def field(parent, value=None, length=20, style=wx.DEFAULT, disabled=False):
        ctrl = wx.TextCtrl(parent, -1, value or '', style=style)
        width, height = parent.GetTextExtent('x' * length)
        ctrl.SetMinSize((width, ctrl.GetSize().height))
        if disabled:
            ctrl.Enable(False)
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


class App(wx.App):

    _MAX_PROGRESS = 40

    def __init__(self, client, username=None, profile=None, session=None):
        self._progress = 1
        self._client = client
        self._username = username
        self._profile = profile
        self._session = session
        self._authentication = None
        super(App, self).__init__(False)

    def _on_select_profile(self, event):
        profile = self._profiles_field.GetStringSelection()
        print "PROFILE SELECTED:", profile
        self._profile = profile
        self._list_sessions()

    def _on_terminate_session(self, event):
        session = self._sessions_field.GetStringSelection()
        self._update_progress(_("Terminating session: %s", session), 0)
        self._client.terminate_session(session)
        self._sessions_field.Delete(self._sessions_field.GetSelection())
        self._update_progress(_("Session terminated: %s", session), 0)

    def _on_resume_session(self, event):
        session = self._sessions_field.GetStringSelection()
        print "RESUME SESSION:", session
        self._update_progress(_("Resuming session: %s", session), 10)
        time.sleep(3)
        self.Exit()

    def _on_new_session(self, event):
        print "NEW SESSION"
        self._update_progress(_("Starting new session."), 10)
        time.sleep(3)
        self.Exit()

    def _on_exit(self, event):
        print "EXIT"
        self.Exit()

    def _create_username(self, parent):
        label = wx.StaticText(parent, -1, _("Login name:"))
        username = self._username
        if not username:
            import getpass
            username = getpass.getuser()  # x2go.defaults.CURRENT_LOCAL_USER,
        self._username_field = field = ui.field(parent, username,
                                                disabled=self._username is not None)
        def on_login(event):
            pass
        return ui.hgroup(
            (label, 0, wx.RIGHT | wx.TOP, 2),
            field,
            ui.button(parent, _("Log in"), on_login, disabled=self._username is not None),
        )

    def _create_profiles(self, parent):
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

    def _create_sessions(self, parent):
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
                          lambda e: e.Enable(self._profile is not None)),
                (ui.button(parent, _("Exit"), self._on_exit), 0, wx.LEFT, 10),
            ), 0, wx.TOP, 8),
        )

    def _create_status(self, parent):
        self._status = status = wx.StaticText(parent, -1, 'Bla')
        self._gauge = gauge = wx.Gauge(parent, -1, self._MAX_PROGRESS)
        return ui.vgroup((gauge, 0, wx.EXPAND), (status, 0, wx.EXPAND | wx.BOTTOM, 4))

    def _create_main_content(self, parent):
        return ui.vgroup(
            (self._create_username(parent), 0, wx.EXPAND | wx.ALL, 8),
            #(self._create_authentication(parent), 0, wx.EXPAND),
            (self._create_profiles(parent), 1, wx.EXPAND | wx.ALL, 8),
            (self._create_sessions(parent), 1, wx.EXPAND | wx.ALL, 8),
            (self._create_status(parent), 0, wx.EXPAND),
        )

    def _create_publickey_authentication(self, parent):
        def on_select_key_file(event):
            directory = os.path.join(os.path.expanduser('~'), '.ssh', '')
            filename = wx.FileSelector(_(u"Select ssh key file"), default_path=directory)
            self._keyfile_field.SetValue(filename)
        label1 = wx.StaticText(parent, -1, _("Key File:"))
        self._keyfile_field = field1 = ui.field(parent, length=40, style=wx.TE_READONLY)
        button1 = ui.button(parent, _("Select"), on_select_key_file)
        label2 = wx.StaticText(parent, -1, _("Passphrase:"))
        self._passphrase_field = field2 = ui.field(parent, length=28, style=wx.PASSWORD)
        return ui.vgroup(
            (ui.hgroup((label1, 0, wx.RIGHT | wx.TOP, 2), field1,
                       (button1, 0, wx.LEFT, 3)), 0, wx.ALL, 2),
            (ui.hgroup((label2, 0, wx.RIGHT | wx.TOP, 2), field2), 0, wx.ALL, 2),
        )

    def _create_password_authentication(self, parent):
        label = wx.StaticText(parent, -1, _("Password:"))
        self._password_field = field = ui.field(parent, style=wx.PASSWORD)
        return ui.hgroup((label, 0, wx.RIGHT | wx.TOP, 2), field)

    def _authenticate(self):
        self._update_progress(_("Trying Kerberos authentication."))
        result = self._client.do_kerberos_authentication()
        if not result:
            self._update_progress(_("Trying SSH Agent authentication."))
            result = self._client.do_ssh_agent_authentication()
        if not result:
            while not result:
                self._update_progress(_("Trying password or public key authentication."))
                method, args = self._show_authentication_dialog()
                if method == 'password':
                    self._update_progress(_("Trying password authentication."))
                    result = self._client.do_password_authentication(*args)
                elif method == 'publickey':
                    self._update_progress(_("Trying public key authentication."))
                    result = self._client.do_publickey_authentication(*args)
                else:
                    break
        return result

    def _show_authentication_dialog(self):
        dialog = wx.Dialog(self._frame, -1, title=_("Authentication"))
        log_in = []
        def on_log_in(event):
            log_in.append(True)
            dialog.Close()
        def on_cancel(event):
            dialog.Close()
        allow_publickey = self._client.allow_publickey_authentication()
        allow_password = self._client.allow_password_authentication()
        if allow_password and allow_publickey:
            nb = wx.Notebook(dialog, -1)
            nb.AddPage(ui.panel(nb, self._create_publickey_authentication), _(u"Public Key"))
            nb.AddPage(ui.panel(nb, self._create_password_authentication), _(u"Password"))
            content = nb
            method = lambda: 'publickey' if nb.GetSelection() == 0 else 'password'
        elif allow_publickey:
            content = ui.panel(dialog, self._create_publickey_authentication)
            method = lambda: 'publickey'
        elif allow_password:
            content = ui.panel(dialog, self._create_password_authentication)
            method = lambda: 'password'
        else:
            raise Exception(_(u"No supported ssh connection method available"))
        dialog.SetSizer(ui.vgroup(
            content,
            (ui.hgroup(
                (ui.button(dialog, _("Log in"), on_log_in), 0, wx.RIGHT, 20),
                ui.button(dialog, _("Cancel"), on_cancel),
            ), 0, wx.ALIGN_CENTER | wx.ALL, 14),
        ))
        dialog.Fit()
        pos, fsize, dsize = self._frame.GetPosition(), self._frame.GetSize(), dialog.GetSize()
        dialog.SetPosition((pos.x + (fsize.width - dsize.width) / 2, pos.y + 40))
        dialog.ShowModal()
        dialog.Destroy()
        self._frame.Raise()
        if log_in:
            m = method()
            if m == 'password':
                fields = (self._username_field, self._password_field)
            else:
                fields = (self._keyfile_field, self._passphrase_field)
            return (m, tuple([f.GetValue() for f in fields]))
        else:
            return (None, ())

    def _list_profiles(self):
        self._update_progress(_("Retrieving available profiles."))
        profiles = self._client.list_profiles()
        self._profiles_field.InsertItems(profiles, 0)
        self._profiles_field.Enable(True)
        self._update_progress(_("Waiting for profile selection."))

    def _list_sessions(self):
        self._update_progress(_("Retrieving available sessions."))
        profiles = self._client.list_sessions()
        self._sessions_field.InsertItems(profiles, 0)
        self._sessions_field.Enable(True)
        self._update_progress(_("Waiting for session selection."))

    def _update_progress(self, message=None, progress=1):
        self._gauge.SetValue(self._gauge.GetValue() + progress)
        if message:
            self._status.SetLabel(message)
        self.Yield()

    def OnInit(self):
        self._frame = frame = wx.Frame(None, -1, _("Starting application"))
        ui.panel(frame, self._create_main_content)
        self.SetTopWindow(frame)
        frame.SetSize((440, 500))
        frame.Show()
        self.Yield()
        authentication = self._authenticate()
        if not authentication:
            self.Exit()
        self._list_profiles()
        return True


class X2GoClient(object):

    def do_kerberos_authentication(self):
        time.sleep(1)
        return None

    def do_ssh_agent_authentication(self):
        time.sleep(1)
        return None

    def allow_password_authentication(self):
        return True

    def do_password_authentication(self, login, password):
        time.sleep(1)
        return True

    def allow_publickey_authentication(self):
        return True

    def do_publickey_authentication(self, filename, passphrase):
        time.sleep(1)
        return True

    def list_profiles(self):
        time.sleep(2)
        return ('Profile 1', 'Profile 2')

    def list_sessions(self):
        time.sleep(2)
        return ('Session 1', 'Session 2')

    def terminate_session(self, session):
        time.sleep(2)
        return True


def main():
    parser = argparse.ArgumentParser(description="Pytis X2Go client startup application")
    parser.add_argument('-u', '--username', metavar='USER_NAME',
                        help="Startup profile")
    parser.add_argument('-p', '--profile', metavar='PROFILE_ID',
                        help="Startup profile")
    parser.add_argument('-s', '--session', metavar='SESSION_ID',
                        help="Session to resume")
    args = parser.parse_args()

    client = X2GoClient()
    app = App(client, username=args.username, profile=args.profile, session=args.session)
    app.MainLoop()

if __name__ == '__main__':
    main()
