# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
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

import lcg
import os
import sys
import time
import wx

import pytis.presentation
import pytis.form
import pytis.util
import pytis.data as pd

from pytis.api import app
from pytis.presentation import StatusField, SharedParams, Menu, MenuItem, MenuSeparator, TextFormat
from pytis.extensions import mf, bf
from pytis.util import log, OPERATIONAL, public_attr_values
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
            Menu(_("&System"), pytis.form.config_menu_items() + (
                MenuItem(_("&Run form"), hotkey=('Alt-a', 'a'),
                         command=pytis.extensions.cmd_run_any_form()),
                MenuItem(_("&Check specification files"),
                         command=pytis.extensions.cmd_check_menus_defs()),
                Menu(_("Management of menus and user roles"), (
                    mf(_("Roles"), 'menu.ApplicationRoles'),
                    bf(_("Menu"), 'menu.ApplicationMenu'),
                    mf(_("Menu Rights"), 'menu.ApplicationMenuM'),
                )),
                Menu(_("Form statistics"), (
                    mf(_("Short statistics"), 'statistics.FormShortStatistics'),
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
            Menu(_("&Demo"), (
                nr(_("&Input fields"), 'misc.InputFields'),
                MenuItem(_("&Dialogs"),
                    command=pytis.form.Application.COMMAND_HANDLED_ACTION(
                        handler=self._dialog_test,
                    ),
                    hotkey=('Alt-d', 'd')),
                mf(_("Continents"), 'cb.Continents', binding='islands'),
                mf(_("Countries"), 'cb.Countries'),
                mf(_('Products'), 'misc.Products'),
                Menu(_("Working with files"), (
                    mf(_("Storing &binary data"), 'binary.BinaryData'),
                    bf(_("Storing binary &images"), 'binary.Images'),
                    bf(_('&File names and URLs'), 'misc.Files'),
                )),
                nr(_("Runtime &filters"), 'misc.RuntimeFilter'),
                bf(_('&Query Fields'), 'misc.RandomNumbers'),
                bf(_("Password fields"), 'misc.Passwords'),
                bf(_('Insurance (table function)'), 'cb.Insurance'),
                bf(_('Tree Order'), 'misc.ObsoleteTree'),
                bf(_('Foldable Tree'), 'misc.Tree'),
                bf(_('Long Table'), 'misc.LongTable'),
                bf(_('Slow Long Table'), 'misc.SlowLongTable'),
                bf(_('Fast Long Table'), 'misc.FastLongTable'),
                bf(_('Range Types'), 'misc.RangeTypes'),
                MenuSeparator(),
                bf(_("Insufficient access rights"), 'cb.DisabledCountries'),
                bf(_("Inexistent specification"), 'cb.Something'),
            )),
            Menu(_("&CMS"), (
                mf(_("Menu"), 'cms.Menu'),
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

    def _dialog_test(self):
        # This test uses some Pytis internals which should not be used by standard
        # Pytis applications as the APIs are not guaranted to stay.
        from pytis.presentation import Button, Field, FieldSet, HGroup
        import pytis.form.dialog as dialog
        import datetime
        p1, p2 = (
            'Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempora\n'
            'incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud\n'
            'exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat.',
            'Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu\n'
            'fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in\n'
            'culpa qui officia deserunt mollit anim id est laborum.'
        )
        html = '<h1>Lorem ipsum</h1>\n<p>\n{}\n</p>\n<p>\n{}\n</p>\n'.format(p1, p2)

        def test(f, *args, **kwargs):
            while True:
                result = f(*args, **kwargs)
                if not app.question(
                        title=("Dialog test result"),
                        message=_("The returned value:") + "\n\n" + repr(result),
                        answers=(dict(label=_("Repeat"), icon='undo', value=True,
                                      descr=_("Run the same dialog again.")),
                                 dict(label=_("Done"), icon='ok',
                                      descr=_("Back to dialog test control panel."))),
                ):
                    break

        def button(label, *args, **kwargs):
            return Button(label, lambda r: test(*args, **kwargs))

        def run_dialog_button(cls, *args, **kwargs):
            return Button(cls.__name__,
                          lambda r: test(pytis.form.app.run_dialog, cls, *args, **kwargs))

        try:
            raise Exception("BugReport dialog test")
        except:
            einfo = sys.exc_info()

        # We are using an input form just because it allows us to lay the buttons out nicely.
        app.input_form(_("Dialog test"), (Field('x'),), layout=HGroup((
            FieldSet(_("Simple announcements"), (
                button(_("Message"), app.message, _("Informational message.")),
                button(_("Warning"), app.warning, _("Something is dangerous.")),
                button(_("Error"), app.error, _("Something failed.")),
            )),
            FieldSet(_("Announcements with additional content"), (
                button(_("Message with plain text"), app.message,
                       _("This is the HTML source code:"), content=html),
                button(_("HTML cotnent without message"), app.message,
                       content=pytis.util.content(html, format=TextFormat.HTML)),
                button(_("LCG content"), app.message,
                       content=lcg.sec('Lorem ipsum', (lcg.p(p1), lcg.p(p2)))),
            )),
            FieldSet(_("Progress dialogs"), (
                button(_("Operation with time measures"), self._measured_progress_dialog_test),
                button(_("Expanding progress dialog"), self._expanding_progress_dialog_test),
                button(_("Indeterminate mode"), self._indeterminate_progress_dialog_test),
                button(_("Without progress indication"), self._operation_dialog_test),
            )),
        ), (
            FieldSet(_("Question dialogs"), (
                button(_("Yes/No Question (default No)"), app.question,
                       _("Question with the default answer set to '%s'.", _("No")),
                    default=False),
                button(_("Yes/No Question (default Yes)"), app.question,
                       _("Question with the default answer set to '%s'.", _("Yes")),
                       default=True),
                button(_("Question with answers"), app.question,
                       _("Question with multiple answers."),
                       answers=(_("Next"), _("Previous"), _("Cancel"))),
            )),
            FieldSet(_("Internal dialogs"), (
                run_dialog_button(dialog.Calendar, date=datetime.date.today()),
                run_dialog_button(dialog.ColorSelector, color='#ff0000'),
                run_dialog_button(dialog.CheckListDialog,
                                  items=((True, _("Checked item")),
                                         (False, _("Unchecked item")),
                                         (False, _("Another unchecked item")))),
                run_dialog_button(dialog.BugReport, einfo=einfo),
                run_dialog_button(
                    dialog.AggregationSetupDialog,
                    aggregation_functions=[(a, a[4:]) for a in
                                           public_attr_values(pd.Data, prefix='AGG_')],
                    columns=[('c{}'.format(i), t.__class__.__name__, t)
                             for i, t in enumerate((pd.String(), pd.Integer(), pd.Date()))],
                    aggregation_valid=lambda op, t: (
                        op == pd.Data.AGG_COUNT or
                        isinstance(t, pd.Number) or
                        isinstance(t, pd.Date) and op != pd.Data.AGG_SUM),
                    grouping_functions=(), name="", group_by_columns=(), aggregation_columns=()),
                run_dialog_button(dialog.FileDialog),
                run_dialog_button(dialog.DirDialog),
            )),
        )))

    def _measured_progress_dialog_test(self):
        import string

        def func(update, data):
            for i, x in enumerate(data):
                if update and not update(i, _("Processing: %d/%d", i, len(data))):
                    break
                log(OPERATIONAL, 'Processing:', x)
                time.sleep(0.1)

        app.run(func, (string.ascii_letters,), maximum=len(string.ascii_letters), can_abort=True,
                title=_("Operation with time measures"),
                elapsed_time=True, remaining_time=True, estimated_time=True)

    def _expanding_progress_dialog_test(self):
        # Test dialog expansion when longer message is passed.
        # Also test over with generator (maximum must be passed).
        def func(update, x):
            update(message=_("Processing: %s", x))
            log(OPERATIONAL, 'Processing:', x)
            time.sleep(2)
        names = (
            'South Africa',
            'Democratic Socialist Republic of Sri Lanka',
            'Spain',
            'South Georgia and the South Sandwich Islands',
            'Sudan',
            'Jemen',
        )
        # Also test over with generator (maximum must be passed).
        def gen():
            for name in names:
                yield name
        app.run(func, over=gen(), maximum=len(names), can_abort=True,
                title=_("Operation with dialog growing on longer messages"))

    def _indeterminate_progress_dialog_test(self):
        def func(update, count):
            for i in range(count):
                update(-1)
                log(OPERATIONAL, 'Processing:', i)
                time.sleep(0.1)
        app.run(func, args=(44,), title=_("Indeterminate mode operation"))

    def _operation_dialog_test(self):
        app.run(time.sleep, (4,), progress=False, title=_("Operation with no progress indication"),
                message=_("Please wait for the operation to finish."))
