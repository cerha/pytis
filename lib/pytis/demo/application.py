# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
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
import sys
import time
import wx

import pytis.presentation
import pytis.form
import pytis.util
import pytis.data as pd

from pytis.api import app
from pytis.presentation import StatusField, SharedParams, Menu, MenuItem, MenuSeparator
from pytis.extensions import mf, bf
from pytis.util import log, OPERATIONAL
_ = pytis.util.translations('pytis-demo')


def nr(title, name):
    return MenuItem(title, command=pytis.form.Application.COMMAND_NEW_RECORD(name=name),
                    help=_('Open insertion form "%s"', title))


class Application(pytis.presentation.Application):

    def init(self):
        basedir = os.path.dirname(os.path.dirname(__file__))
        sys.path.append(os.path.join(basedir, 'www'))

    def menu(self):
        return (
            Menu(_("&System"), pytis.form.config_menu_items() +
                 (MenuItem(_("&Run form"), hotkey=('Alt-a', 'a'),
                           command=pytis.extensions.cmd_run_any_form()),
                  MenuItem(_("&Check specification files"),
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
                  MenuSeparator(),
                  pytis.form.recent_forms_menu(),
                  MenuSeparator(),
                  MenuItem(_("E&xit"), hotkey='Alt-x',
                           command=pytis.form.Application.COMMAND_EXIT()),
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
                  MenuItem(_("Question dialog test"),
                           command=pytis.form.Application.COMMAND_HANDLED_ACTION(
                               handler=self._question_dialog_test,
                           ),
                           hotkey=('Alt-d', 'q')),
                  MenuItem(_("Progress dialog test"),
                           command=pytis.form.Application.COMMAND_HANDLED_ACTION(
                               handler=self._progress_dialog_test,
                           ),
                           hotkey=('Alt-d', 'd')),
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
                  bf(_("Countries (insufficient access rights)"), 'cb.DisabledCountries'),
                  bf(_("Inexistent specification"), 'cb.Something'),
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
        return super(Application, self).status_fields() + [
            StatusField('counter', _("Counter"), refresh=self._refresh_counter,
                        refresh_interval=5000, width=3),
        ]

    def _question_dialog_test(self):
        answers = (
            app.question(_("Question with the default answer set to '%s'.", _("No")) + "\n" +
                         _("Do you want to continue?"), default=False),
            app.question(_("Question with the default answer set to '%s'.", _("Yes")) + "\n" +
                         _("Do you still want to continue?"), default=True),
            app.question(_("Question dialog with multiple answers.\nAdvance to the next record?"),
                         answers=(_("Next"), _("Previous"), _("Cancel"))),
        )
        app.message(_("Python values corresponding to your answers were:") + "\n" + "\n".join(
            "{}. {!r}".format(i + 1, a) for i, a in enumerate(answers)
        ))

    def _progress_dialog_test(self):
        import string

        def func1(update, data):
            for i, x in enumerate(data):
                if update and not update(i, _("Processing: %d/%d", i, len(data))):
                    break
                log(OPERATIONAL, 'Processing:', x)
                time.sleep(0.1)
        app.run(func1, (string.ascii_letters,), maximum=len(string.ascii_letters), can_abort=True,
                title=_("Operation with time measures"),
                elapsed_time=True, remaining_time=True, estimated_time=True)

        # Test dialog expansion when longer message is passed.
        # Also test over with generator (maximum must be passed).
        def func2(update, x):
            update(message=_("Processing: %s", x))
            log(OPERATIONAL, 'Processing:', x)
            time.sleep(2)
        names = (
            'South Africa',
            'Democratic Socialist Republic of Sri Lanka',
            'Spain',
            'South Georgia and the South Sandwich Islands',
            'Sudan',
        )
        # Also test over with generator (maximum must be passed).
        def gen():
            for name in names:
                yield name
        app.run(func2, over=gen(), maximum=len(names), can_abort=True,
                elapsed_time=True, remaining_time=True, estimated_time=True)

        def func3(update, count):
            for i in range(count):
                update(-1)
                log(OPERATIONAL, 'Processing:', i)
                time.sleep(0.1)
        app.run(func3, args=(44,), title=_("Indeterminate mode operation"))
