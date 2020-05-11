# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2007-2018 OUI Technology Ltd.
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

from __future__ import print_function
from __future__ import unicode_literals

import os
import pytest
import sys
import wx

import pytis.presentation
import pytis.form
import pytis.util
import pytis.data as pd
from pytis.presentation import StatusField, SharedParams
from pytis.extensions import mf, bf, nr
from pytis.form import Menu, MItem, app
_ = pytis.util.translations('pytis-demo')
____ = pytis.form.MSeparator()


class Application(pytis.presentation.Application):

    def params(self):
        return (
            # Don't bother creating a separate table for testing shared params.
            # Simply use any existing table...
            SharedParams('country', 'cb.Countries', pd.EQ('id', pd.sval('CZ'))),
        )

    def init(self):
        basedir = os.path.dirname(os.path.dirname(__file__))
        sys.path.append(os.path.join(basedir, 'www'))

    def menu(self):
        return (
            Menu(_("&System"), pytis.form.config_menu_items() +
                 (MItem(_("&Run form"), hotkey=('Alt-a', 'a'),
                        command=pytis.extensions.cmd_run_any_form()),
                  MItem(_("&Check specification files"),
                        command=pytis.extensions.cmd_check_menus_defs()),
                  Menu(_("Management of menus and user roles"),
                       (mf(_("Roles"), 'menu.ApplicationRoles'),
                        bf(_("Menu"), 'menu.ApplicationMenu'),
                        mf(_("Menu Rights"), 'menu.ApplicationMenuM'),
                        )),
                  Menu(_("Form statistics"),
                       (mf(_("Short statistics"), 'statistics.FormShortStatistics'),
                        mf(_("Full statistics"), 'statistics.FormStatistics'),
                        mf(_("Users"), 'statistics.FormUserList'),
                        )),
                  bf(_("User printing templates"), 'printing.UserOutputTemplates'),
                  bf(_("Global printing templates"), 'printing.GlobalOutputTemplates'),
                  mf(_("Crypto users"), 'crypto.CryptoAreas'),
                  ____,
                  pytis.form.recent_forms_menu(),
                  ____,
                  MItem(_("E&xit"), hotkey='Alt-x', command=pytis.form.Application.COMMAND_EXIT),
                  )),
            Menu(_("&Misc"),
                 (nr(_("&Input fields"), 'misc.InputFields'),
                  nr(_("Runtime &filters"), 'misc.RuntimeFilter'),
                  nr(_("&Inheritance"), 'misc.Inheritance'),
                  Menu(_("&Binary data"),
                       (mf(_("&Generic binary data"), 'binary.BinaryData'),
                        bf(_("&Images"), 'binary.Images'),
                        )),
                  Menu(_("&Dual forms"),
                       (mf(_("Continents"), 'cb.Continents', binding='islands'),
                        mf(_("Countries"), 'cb.Countries'),
                        )),
                  MItem(_("Dialog test"), command='dialog_test'),
                  bf(_("Password fields"), 'misc.Passwords'),
                  mf(_('Products'), 'misc.Products'),
                  bf(_('Tree Order'), 'misc.ObsoleteTree'),
                  bf(_('Foldable Tree'), 'misc.Tree'),
                  bf(_('Long Table'), 'misc.LongTable'),
                  bf(_('Slow Long Table'), 'misc.SlowLongTable'),
                  bf(_('Fast Long Table'), 'misc.FastLongTable'),
                  bf(_('Files and URLs'), 'misc.Files'),
                  bf(_('Query Fields'), 'misc.RandomNumbers'),
                  bf(_('Range Types'), 'misc.RangeTypes'),
                  )),
            Menu(_("&Codebooks"),
                 (bf(_("Con&tinents"), 'cb.Continents'),
                  bf(_("&Countries"), 'cb.Countries'),
                  bf(_('Insurance'), 'cb.Insurance'),
                  )),
            Menu(_("&CMS"),
                 (mf(_("Menu"), 'cms.Menu'),
                  bf(_("Languages"), 'cms.Languages'),
                  mf(_("Modules"), 'cms.Modules'),
                  bf(_("Generic actions"), 'cms.GenericActions'),
                  mf(_("Users"), 'cms.Users'),
                  mf(_("User Roles"), 'cms.Roles'),
                  bf(_("Session Log"), 'cms.SessionLog'),
                  bf(_("Access Log"), 'cms.AccessLog'),
                  bf(_("Color themes"), 'cms.Themes'),
                  )),
        )

    def _refresh_counter(self):
        # Return value: (visible label, icon, tooltip).
        self._counter += 1
        if self._counter > 30:
            self._counter = 0
        if self._counter > 20:
            return (None, wx.ART_TIP, _("Counter value: %d", self._counter))
        else:
            return (None, wx.ART_EXECUTABLE_FILE, _("Counter value: %d", self._counter))

    def status_fields(self):
        self._counter = 0
        return super(Application, self).status_fields() + (
            StatusField('counter', _("Counter"), refresh=self._refresh_counter,
                        refresh_interval=5000, width=3),
        )

    def cmd_dialog_test(self):
        return pytis.form.Application.COMMAND_HANDLED_ACTION(handler=self._dialog_test,)

    def _dialog_test(self):
        NOTHING = object()

        def dialog(type, *args, **kwargs):
            expect = kwargs.pop('expect', NOTHING)
            return type, args, kwargs, expect

        def xx(update=None):
            print("xx() starting...")
            a = 1
            for i in range(1000):
                if update:
                    update(i // 10, str(i))
                else:
                    for j in range(10000):
                        a += 1
            print("xx() finished...")
            return "xxxx"

        dialogs = (
            dialog(pytis.form.Message,
                   "A series of dialogs will follow.\n"
                   "New dialogs will appear as long as you\n"
                   "answer the questions as suggested."),
            dialog(pytis.form.Question,
                   "The default answer shold be `No'.\nCorrect?",
                   default=False, expect=True),
            dialog(pytis.form.Question,
                   "The default answer shold be `Yes'.\nCorrect?",
                   default=True, expect=True),
            dialog(pytis.form.Question,
                   "Do you want to exit now?\nAnswer `No' if you want the test to continue.",
                   default=False, expect=False),
            dialog(pytis.form.Question,
                   "Plese, exit this dialog by pressing Escape.",
                   default=False, expect=False),
            dialog(pytis.form.Calendar, pd.DateTime.now().value()),
            dialog(pytis.form.ColorSelector),
            dialog(pytis.form.OperationDialog, xx),
            dialog(pytis.form.ProgressDialog, xx),
        )

        for type, args, kwargs, expect in dialogs:
            print("***", type.__name__, args, kwargs, expect)
            result = pytis.form.run_dialog(type, *args, **kwargs)
            if expect is not NOTHING and result != expect:
                pytis.form.run_dialog(
                    pytis.form.Warning,
                    "Your the returned value was %r, but %r was expected.\n"
                    "You either didn't answer as suggested or there is a bug!\n" %
                    (result, expect)
                )
                break


class TestApplication(object):
    """Experimantal test running inside wx Application environment.

    This test starts a wx application in a separate thread in a hidden mode and
    runs test cases within its environment.  It may be useful for testing
    fetures which require the wx Application to exist.

    """

    def setup_class(cls):
        import threading
        pytis.util.set_configuration_file('/home/cerha/work/pytis/demo/config.py')
        threading.Thread(target=pytis.form.Application(show_frame=False).run).start()

    def teardown_class(cls):
        pytis.form.Application.COMMAND_EXIT.invoke()

    def test_shared_params(self):
        assert app.param.country.continent == 'EU'
        with pytest.raises(AttributeError):
            app.param.user.xy
        with pytest.raises(AttributeError):
            app.param.user.xy = 1

    def test_shared_param_callbacks(self):
        import time
        name = app.param.country.name
        try:
            names = []

            def callback():
                names.append(app.param.country.name)

            app.param.country.add_callback('name', callback)
            app.param.country.name = 'x'
            assert app.param.country.name == 'x'
            time.sleep(0.2)  # Let the callbacks be called.
            app.param.country.name = 'y'
            assert app.param.country.name == 'y'
            time.sleep(0.2)  # Let the callbacks be called.
            assert names == ['x', 'y']
        finally:
            app.param.country.name = name
