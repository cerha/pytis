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
import string
import sys
import time

import pytis.presentation
import pytis.extensions
import pytis.form
import pytis.util
import pytis.data as pd

from pytis.api import app
from pytis.presentation import Command, Menu, MenuItem, MenuSeparator, StatusField, TextFormat
from pytis.util import log, OPERATIONAL, public_attr_values
_ = pytis.util.translations('pytis-demo')


class Application(pytis.presentation.Application):

    def init(self):
        basedir = os.path.dirname(os.path.dirname(__file__))
        sys.path.append(os.path.join(basedir, 'www'))

    def menu(self):
        return (
            Menu(_("&System"), (
                MenuItem(_("User interface settings"),
                         Command(app.call, pytis.form.edit_config, 'ui')),
                MenuItem(_("Export settings"),
                         Command(app.call, pytis.form.edit_config, 'export')),
                MenuItem(_("&Run form"),
                         Command(pytis.extensions.Commands.run_any_form),
                         hotkey=('Alt-a', 'a')),
                MenuItem(_("&Check specification files"),
                         Command(pytis.extensions.Commands.check_menus_defs)),
                Menu(_("Management of menus and user roles"), (
                    MenuItem(_("Roles"), Command(app.run_form, 'menu.ApplicationRoles')),
                    MenuItem(_("Menu"), Command(app.run_form, 'menu.ApplicationMenu')),
                    MenuItem(_("Menu Rights"), Command(app.run_form, 'menu.ApplicationMenuM')),
                )),
                Menu(_("Form statistics"), (
                    MenuItem(_("Short statistics"),
                             Command(app.run_form, 'statistics.FormShortStatistics')),
                    MenuItem(_("Full statistics"),
                             Command(app.run_form, 'statistics.FormStatistics')),
                    MenuItem(_("Users"),
                             Command(app.run_form, 'statistics.FormUserList')),
                )),
                MenuItem(_("User printing templates"),
                         Command(app.run_form, 'printing.UserOutputTemplates')),
                MenuItem(_("Global printing templates"),
                         Command(app.run_form, 'printing.GlobalOutputTemplates')),
                MenuItem(_("Crypto users"),
                         Command(app.run_form, 'crypto.CryptoAreas')),
                MenuSeparator(),
                Menu(_("Recently opened forms"), (), id=Menu.RECENT_FORMS_MENU, autoindex=False),
                MenuSeparator(),
                MenuItem(_("E&xit"), Command(app.exit), hotkey='Alt-x'),
            )),
            Menu(_("&Demo"), (
                MenuItem(_("&Input fields"),
                         Command(app.new_record, 'misc.InputFields')),
                MenuItem(_("&Dialogs"),
                         Command(self.dialog_test), hotkey=('Alt-d', 'd')),
                MenuItem(_("Runtime &filters"),
                         Command(app.new_record, 'misc.RuntimeFilter')),
                MenuItem(_('&Query Fields'),
                         Command(app.run_form, 'misc.RandomNumbers')),
                MenuItem(_("Continents"),
                         Command(app.run_form, 'cb.Continents', binding='islands')),
                MenuItem(_("Countries"),
                         Command(app.run_form, 'cb.Countries')),
                MenuItem(_('Products'),
                         Command(app.run_form, 'misc.Products')),
                MenuItem(_('Insurance (table function)'),
                         Command(app.run_form, 'cb.Insurance')),
                Menu(_("Working with files"), (
                    MenuItem(_("Storing &binary data"),
                             Command(app.run_form, 'binary.BinaryData')),
                    MenuItem(_("Storing &images"),
                             Command(app.run_form, 'binary.Images')),
                    MenuItem(_('Opening &file names and URLs'),
                             Command(app.run_form, 'misc.Files')),
                )),
                MenuItem(_("Password fields"),
                         Command(app.run_form, 'misc.Passwords')),
                MenuItem(_('Tree Order'),
                         Command(app.run_form, 'misc.ObsoleteTree')),
                MenuItem(_('Foldable Tree'),
                         Command(app.run_form, 'misc.Tree')),
                MenuItem(_('Long Table'),
                         Command(app.run_form, 'misc.LongTable')),
                MenuItem(_('Slow Long Table'),
                         Command(app.run_form, 'misc.SlowLongTable')),
                MenuItem(_('Fast Long Table'),
                         Command(app.run_form, 'misc.FastLongTable')),
                MenuItem(_('Range Types'),
                         Command(app.run_form, 'misc.RangeTypes')),
                MenuSeparator(),
                MenuItem(_("Insufficient access rights"),
                         Command(app.run_form, 'cb.DisabledCountries')),
                MenuItem(_("Inexistent specification"),
                         Command(app.run_form, 'cb.Something')),
            )),
            Menu(_("&CMS"), (
                MenuItem(_("Menu"), Command(app.run_form, 'cms.Menu')),
                MenuItem(_("Languages"), Command(app.run_form, 'cms.Languages')),
                MenuItem(_("Modules"), Command(app.run_form, 'cms.Modules')),
                MenuItem(_("Generic actions"), Command(app.run_form, 'cms.GenericActions')),
                MenuItem(_("Users"), Command(app.run_form, 'cms.Users')),
                MenuItem(_("User Roles"), Command(app.run_form, 'cms.Roles')),
                MenuItem(_("Session Log"), Command(app.run_form, 'cms.SessionLog')),
                MenuItem(_("Access Log"), Command(app.run_form, 'cms.AccessLog')),
                MenuItem(_("Color themes"), Command(app.run_form, 'cms.Themes')),
            )),
        )

    def _refresh_counter(self):
        # Return value: (visible label, icon, tooltip).
        self._counter += 1
        if self._counter > 30:
            self._counter = 0
        if self._counter > 20:
            return (None, 'status-offline', _("Counter value: %d", self._counter))
        else:
            return (None, 'status-online', _("Counter value: %d", self._counter))

    def status_fields(self):
        self._counter = 0
        return super(Application, self).status_fields() + [
            StatusField('counter', _("Counter"), refresh=self._refresh_counter,
                        refresh_interval=5000, width=3),
        ]

    @Command.define
    def dialog_test(self):
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
                       _("Question with multiple answers defined just by labels."),
                       answers=(_("Next"), _("Previous"), _("Cancel"))),
                button(_("Answers with values and icons"), app.question,
                       _("Question with multiple answers, where each answer\n"
                         "has a given label, return value, icon and tooltip."),
                       answers=(
                           dict(label=_("Next"), icon='go-forward', value='next',
                                descr=_("Proceed to the next item.")),
                           dict(label=_("Previous"), icon='go-back', value='prev',
                                descr=_("Return to the previous item.")),
                           dict(label= _("Cancel"), icon='cancel',
                                descr=_("Get back to the current item.")),
                       )),
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
