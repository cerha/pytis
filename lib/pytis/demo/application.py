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

from pytis.api import app
from pytis.presentation import StatusField, SharedParams
from pytis.extensions import mf, bf, nr
from pytis.form import Menu, MItem
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
        app.message("A series of dialogs will follow.\nAnswer the questions as they come.")
        app.question("The default answer shold be `No'.\nCorrect?", default=False)
        app.question("The default answer shold be `Yes'.\nCorrect?", default=True)
        if app.question("Do you want to exit now?\nAnswer `No' if you want the test to continue."):
            return

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

        pytis.form.run_dialog(pytis.form.OperationDialog, xx)
        pytis.form.run_dialog(pytis.form.ProgressDialog, xx)


class TestApplication(object):
    """Experimantal test running inside wx Application environment.

    This test starts a headless wx application in a separate thread and runs
    test cases within its environment.  It may be useful for testing fetures
    which require the wx Application to exist.

    """

    def setup_class(cls):
        import threading
        directory = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
        # Try pytis-demo-config-local.py to allow local modified version outside git control.
        for filename in ('pytis-demo-config-local.py', 'pytis-demo-config.py'):
            path = os.path.join(directory, 'pytis-demo-config.py')
            if os.path.isfile(path):
                pytis.util.set_configuration_file(path)
                break
        threading.Thread(target=pytis.form.Application(headless=True).run).start()

    def teardown_class(cls):
        pytis.form.Application.COMMAND_EXIT.invoke()
        # Release the pytis.form.Application instance from pytis.api.app to make sure
        # the tests outside this class don't use the previously created instance.
        app.release()

    def test_api_form(self):
        import time
        assert app.form is None
        pytis.form.Application.COMMAND_RUN_FORM.invoke(name='misc.RandomNumbers',
                                                       form_class=pytis.form.BrowseForm)
        assert app.form is not None
        assert app.form.query_fields is not None
        assert app.form.query_fields.row is not None
        #time.sleep(.4)
        #assert app.form.query_fields.row['count'].value() == 10

    def test_shared_params(self):
        test_shared_params()

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


def test_shared_params():
    # Shared params must work with the pytis.form.Application instance
    # (called from TestApplication.test_shared_params) as well as without
    # it (with automatically created pytis.api.BaseApplication).
    assert app.param.country.continent == 'EU'
    with pytest.raises(AttributeError):
        app.param.user.xy
    with pytest.raises(AttributeError):
        app.param.user.xy = 1
