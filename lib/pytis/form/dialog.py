# -*- coding: utf-8 -*-

# Copyright (C) 2019-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 OUI Technology Ltd.
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

"""Dialogs for modal user interaction."""
from __future__ import unicode_literals
from __future__ import division
from __future__ import print_function

from past.builtins import basestring
from builtins import range
from future import standard_library

import cgitb
import datetime
import email.utils
import lcg
import os
import sys
import time
import traceback
import wx.adv

import pytis.data
import pytis.form
import pytis.util

from pytis.api import app
from pytis.presentation import TextFormat
from pytis.util import ProgramError, send_mail, public_attr_values

from .command import CommandHandler
from .event import wx_callback
from .screen import KeyHandler, wx_focused_window, wx_text_ctrl, wx_text_view, wx_button

# Needed for subprocess.getstatusoutput (commands.getstatusoutput in Python 2).
standard_library.install_aliases()

_ = pytis.util.translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.


class Dialog(KeyHandler, CommandHandler, object):
    """Common base class for all dialogs.

    This class defines the public API.  All dialog classes used within Pytis
    are derived from it.

    Creating a dialog instance only sets its properties.  No UI elements are
    created at that point.  Those are created by calling 'run()' on the
    instance.

    """
    @classmethod
    def _get_command_handler_instance(cls):
        return pytis.form.app.top_window()

    def __init__(self, parent):
        self._parent = parent
        KeyHandler.__init__(self)
        self._key_guardian = None

    def run(self):
        """Construct the dialog, display it and wait for user interaction.

        Returns: The result depends on the dialog type.

        The call blocks until the dialog is submitted, typically by pressing
        one of its buttons.

        """
        return True


class GenericDialog(Dialog):
    """Common base class for more specific dialog classes defined below.

    This class provides some common infrastructure for building dialogs.

    """
    class Button(object):
        def __init__(self, label, value=None, icon=None):
            self._label = label
            self._value = value
            self._icon = icon

        def __str__(self):
            return "<Button{}>".format(''.join(' {}={!r}'.format(k[1:], v)
                                               for k, v in self.__dict__.items()))

        def __eq__(self, other):
            if pytis.util.sameclass(self, other):
                return other.value == self.value
            else:
                return NotImplemented

        def __ne__(self, other):
            # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
            return not self.__eq__(other)

        @property
        def label(self):
            return self._label

        @property
        def value(self):
            return self._value

        @property
        def icon(self):
            return self._icon

    BUTTON_OK = Button(_("Ok"), value=True)
    BUTTON_CANCEL = Button(_("Cancel"), value=None)

    _BUTTONS = ()
    """Sequence of Button instances representing dialog submit buttons."""
    _DEFAULT_BUTTON = None
    """Initially selected button.

    One of the instances present in _BUTTONS or None.  The default button has
    initial focus so it is pressed on Enter and Space keys.  The default button
    press is also simulated on dialog force commit command (Ctrl-Enter) even if
    it does not currently have keyboard focus.

    """

    _HELP_TOPIC = 'dialog'
    _STYLE = wx.CAPTION | wx.CLOSE_BOX | wx.MINIMIZE_BOX | wx.SYSTEM_MENU

    def __init__(self, parent, title=None, report=None, report_format=None, report_size=(None, None)):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent; 'wx.Frame' or 'wx.Dialog' instance
          title -- title to show in the dialog title bar as a string
          report -- Dodatečný obsah, který má být zobrazen v okně dialogu
            (typicky delší scrollovatelný text).  Jde buďto o řetězec, jehož
            formátování je možné dále určit arguemntem 'report_format', nebo
            přímo LCG obsah jako instance 'lcg.Content'.
          report_format -- konstanta třídy 'TextFormat' určující jak má být
            nakládáno se vstupním textem argumentu 'report'.  V případě, že
            report není specifikován, nebo nejde o řetězec, je tento argument
            irelevantní.
          report_size -- report window size as a pair of integers (width,
            height) in characters.  If any of the numbers is 'None' given size
            will be will automatically accommodate to the size of the contents
            (for plain text) or use a default value.

        """
        assert isinstance(title, basestring), title
        assert report is None or isinstance(report, (basestring, lcg.Content)), report
        assert report_format is None or \
            report_format in pytis.util.public_attr_values(TextFormat), report_format
        assert isinstance(report_size, (list, tuple)) and len(report_size) == 2, report_size
        super(GenericDialog, self).__init__(parent)
        self._title = unistr(title)
        self._report = report
        self._report_format = report_format
        self._report_size = report_size
        self._shown = False

    def _create_dialog(self):
        """Create the dialog wx instance and build its contents.

        The goal is of this method is to assign the wx instance to
        'self._dialog' and initialize its contents.

        The base class creates a 'wx.Dialog' and calls
        '_create_dialog_elements()'.  Most subclasses will only need to
        override the methods used by '_create_dialog_elements()' to build
        specific dialog contents.  Overriding this method may be necessary when
        another wx class is to be used instead of 'wx.Dialog'.

        """
        style = self._STYLE
        if self._report is not None:
            style |= wx.RESIZE_BORDER
        self._dialog = dialog = wx.Dialog(self._parent, title=self._title, style=style)
        self._want_focus = None
        self._create_dialog_elements()
        self._handle_keys(dialog)

    def _rebuild(self):
        self._dialog.DestroyChildren()
        self._create_dialog_elements()
        self._dialog.Layout()
        self.focus()

    def _create_dialog_elements(self):
        """Build the dialog UI.

        The main dialog content is created by calling '_create_content()' and
        submit buttons are appended underneath.

        Most derived classes will just override '_create_content()' to
        customize dialog appearance.

        """
        dialog = self._dialog
        sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_content(sizer)
        if self._report is not None:
            report = wx_text_view(dialog, self._report,
                                  format=self._report_format,
                                  width=self._report_size[0],
                                  height=self._report_size[1])
            if self._report_format == TextFormat.PLAIN:
                report.SetMinSize((300, report.MinSize.height))
            sizer.Add(report, 1, wx.EXPAND)
        buttons = self._buttons()
        default_button = self._default_button(buttons)
        self._buttons_map = {}
        self._default_button_instance = None
        if buttons:
            bsizer = wx.BoxSizer()
            bsizer.AddSpacer(30)
            for button in buttons:
                wxbutton = wx_button(dialog, button.label, icon=button.icon)
                self._buttons_map[wxbutton.Id] = button
                bsizer.Add(wxbutton, 0)
                bsizer.AddSpacer(30)
                wx_callback(wx.EVT_BUTTON, wxbutton, self._on_button)
                self._handle_keys(wxbutton)
                if button == default_button:
                    wxbutton.SetDefault()
                    wxbutton.SetFocus()
                    if not self._want_focus:
                        # Note: Widgets created in _create_content() may override self._want_focus.
                        self._want_focus = wxbutton
                    self._default_button_instance = wxbutton
            sizer.AddSpacer(16)
            sizer.Add(bsizer, 0, wx.CENTER)
        sizer.AddSpacer(16)
        dialog.SetSizer(sizer)
        sizer.Fit(dialog)
        wx_callback(wx.EVT_IDLE, dialog, self._on_idle)

    def _create_content(self, sizer):
        """Create the main dialog content and add it to given top level sizer.

        Builds the main dialog content area above the dialog buttons.

        """
        pass

    def _create_icon(self, artid):
        bitmap = wx.ArtProvider.GetBitmap(artid, wx.ART_MESSAGE_BOX, (48, 48))
        if bitmap.IsOk():
            return wx.StaticBitmap(self._dialog, -1, bitmap)
        else:
            return None

    def _on_idle(self, event):
        event.Skip()
        if self._dialog.IsShown():
            # Note, self._want_focus may be set (not None), but dead (evaluate to False).
            if self._want_focus:
                self._want_focus.SetFocus()
                self._want_focus.SetFocusFromKbd()
                self._want_focus = None
            if not self._shown:
                self._shown = True
                self._on_show()

    def _on_show(self):
        pass

    def _navigate(self):
        nav = wx.NavigationKeyEvent()
        nav.SetDirection(True)
        nav.SetCurrentFocus(self._dialog)
        self._dialog.GetEventHandler().ProcessEvent(nav)

    def _end_modal(self, wxid):
        self._dialog.EndModal(wxid)

    def _on_button(self, event):
        self._end_modal(event.GetId())

    def _buttons(self):
        """Override if static definition through the '_BUTTONS' constant is not enough."""
        return self._BUTTONS

    def _default_button(self, buttons):
        """Override if static definition through the '_DEFAULT_BUTTON' constant is not enough."""
        return self._DEFAULT_BUTTON

    def _button(self, wxid):
        """Return the GenericDialog.Button instance for given wx button id or None."""
        return self._buttons_map.get(wxid)

    def _can_commit(self, widget):
        # Override to allow certain widgets to commit the whole dialog, when
        # COMMIT_DIALOG command is invoked (from the keyboard).
        return False

    def _cmd_commit_dialog(self, force=False):
        widget = wx_focused_window()
        if force and (not widget or widget.Id not in self._buttons_map):
            widget = self._default_button_instance
        if isinstance(widget, wx.Button):
            # Simulate a click on the button.
            widget.Command(wx.CommandEvent(wx.wxEVT_COMMAND_BUTTON_CLICKED, widget.Id))
        elif widget and self._can_commit(widget):
            self._end_modal(widget.Id)
        else:
            self._navigate()

    def _cmd_close_dialog(self):
        self._end_modal(wx.ID_CANCEL)

    def _cmd_help(self):
        pytis.form.Application.COMMAND_HELP.invoke(topic='pytis/' + self._HELP_TOPIC)

    def _run_dialog(self):
        return self._customize_result(self._dialog.ShowModal())

    def _customize_result(self, wxid):
        """Return the value to be returned by 'run()' for given wx '_run_dialog()' result.

        Given 'wxid' is the integer returned by dialog's 'ShowModal()'.  The
        default implementation returns the value of the dialog button pressed
        or None if the dialog is ended otherwise (usually pressing escape or
        closing by mouse).  May be overridden in derived classes.

        """
        button = self._button(wxid)
        if button:
            return button.value
        else:
            return None

    def run(self):
        """Show the dialog as modal and return its result when closed."""
        self._create_dialog()
        self._dialog.SetFocus()
        result = self._run_dialog()
        self._dialog.Destroy()
        return result

    def focus(self):
        self._dialog.SetFocus()


class Message(GenericDialog):
    """Dialog zobrazující zprávu a vracející odpověď.

    Tato třída pouze zobrazuje zprávu a tlačítko pro akceptování dialogu.

    Vrácená hodnota metody 'run()' je jednoduše nápis tlačítka, kterým byl
    dialog ukončen (None v případě, že byl ukončen jiným způsobem než
    tlačítkem).

    """
    ICON_INFO = wx.ART_INFORMATION
    "Ikona pro informativní zprávy (žárovka)"
    ICON_QUESTION = wx.ART_QUESTION
    "Ikona s otazníkem."
    ICON_WARNING = wx.ART_WARNING
    "Ikona s vykřičníkem."
    ICON_ERROR = wx.ART_ERROR
    "Ikona pro chybové zprávy."
    ICON_TIP = wx.ART_TIP
    "Ikona pro tipy, rady apod."
    ICON_QUIT = wx.ART_QUIT
    "Ikona opuštění aplikace."
    ICON_RUN = wx.ART_EXECUTABLE_FILE
    "Ikona opuštění aplikace."

    _BUTTONS = (GenericDialog.BUTTON_OK,)
    _DEFAULT_BUTTON = GenericDialog.BUTTON_OK

    def __init__(self, parent, message, title=_("Message"), icon=None, **kwargs):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent; 'wx.Frame' or 'wx.Dialog' instance
          title -- title to show in the dialog title bar as a string
          message -- the message displayed in the main dialog area as a string (newlines respected).
          icon -- One of ICON_* constants of the class.

        """
        super(Message, self).__init__(parent, title=title, **kwargs)
        assert icon is None or icon in public_attr_values(self.__class__, prefix='ICON_'), icon
        if message is not None:
            message = unistr(message)
            if not icon:
                # Only supply the default icon if message is given.  This will prevent
                # a useless icon above the report widget.
                icon = self.ICON_INFO
        self._message = message
        self._icon = icon

    def _set_message(self, message):
        dialog = self._dialog
        size = dialog.Size
        self._message_display.SetLabel(message)
        # Allow to expand, but don't shrink back.
        self._message_display.SetMinSize(self._message_display.Size)
        dialog.Sizer.Fit(dialog)
        dialog.Refresh()
        from pytis.util import log, OPERATIONAL
        for i in range(4):
            pytis.form.app.wx_yield(full=True)
        if dialog.Size != size:
            # Needed on wxGTK in order to let the dialog repaint correctly.
            for i in range(10):
                pytis.form.app.wx_yield(full=True)

    def _create_content(self, sizer):
        if self._message is not None:
            self._message_display = wx.StaticText(self._dialog, wx.ID_ANY, self._message)
        if self._icon and self._message is not None:
            box = wx.BoxSizer()
            box.Add(self._create_icon(self._icon), 0, wx.RIGHT, 12)
            box.Add(self._message_display, 1, wx.EXPAND | wx.TOP,
                    16 if len(self._message.splitlines()) == 1 else 5)
            sizer.Add(box, 0, wx.EXPAND | wx.ALL, 16)
        elif self._icon:
            sizer.Add(self._create_icon(self._icon), 0, wx.ALL, 16)
        elif self._message is not None:
            sizer.Add(self._message_display, 0, wx.ALL, 16)


class Warning(Message):
    """Dialog for displaying a warning."""

    def __init__(self, parent, message, title=_("Warning"), icon=Message.ICON_WARNING, **kwargs):
        super(Warning, self).__init__(parent, message, title=title, icon=icon, **kwargs)


class Error(Message):
    """Dialog for displaying an error message."""

    def __init__(self, parent, message, title=_("Error"), icon=Message.ICON_ERROR, **kwargs):
        super(Error, self).__init__(parent, message, title=title, icon=icon, **kwargs)


class Question(Message):
    """Dialog asking for answer to given question from list of answers (buttons).

    The available answers are either specified explicitly or the default
    answers are 'Yes' and 'No' ('Yes' being the default).

    With default answers 'Yes'/'No', 'run()' returns True if the user pressed the
    'Yes' button, False if the user pressed the 'No' button and None otherwise.

    With answers explicitly passed to the constructor, 'run()' returns the
    answer itself (string) of the pressed button or None.

    """
    BUTTON_YES = GenericDialog.Button(_("Yes"), value=True)
    BUTTON_NO = GenericDialog.Button(_("No"), value=False)

    def __init__(self, parent, message, title=_("Question"), icon=Message.ICON_QUESTION,
                 answers=None, default=None, timeout=None, **kwargs):
        """Initialize the dialog.

        Arguments:

          answers -- sequence of string labels to construct available buttons
            for dialog submission.  If None, the default buttons are 'Yes' and
            'No'.

          default -- The initially selected answer.  If default answers 'Yes'
            and 'No' are used, True and None stand for 'Yes' and False for
            'No'.  With explicitly defined answers the string value
            corresponding to one of the options is expected.

          timeout -- dialog timeout in seconds; integer.  When the dialog is
            shown for more than the given time, it gets automatically closed
            and 'None' is returned as the answer.  If the argument value is
            'None' then the dialog is shown until user chooses an answer.

        """
        super(Question, self).__init__(parent, message, title=title, icon=icon, **kwargs)
        assert default is None or answers is None and default in (True, False) or default in answers
        self._answers = answers
        self._default = default
        self._timeout = timeout

    def _buttons(self):
        if self._answers is not None:
            return [self.Button(answer, value=answer) for answer in self._answers]
        else:
            return (self.BUTTON_YES, self.BUTTON_NO)

    def _default_button(self, buttons):
        if self._answers is not None:
            return pytis.util.find(self._default, buttons, lambda b: b.value)
        elif self._default is None or self._default:
            return self.BUTTON_YES
        else:
            return self.BUTTON_NO

    def _create_dialog(self):
        super(Question, self)._create_dialog()
        if self._timeout is not None:
            def destroy():
                try:
                    self._dialog.EndModal(-1000)
                except Exception:
                    # The wx instance of `self' may already be inactive
                    pass
            wx.CallLater(self._timeout * 1000, destroy)


class ProgressDialog(Message):
    """Dialog for execution of a long running operation in progress.

    The dialog is displayed until the operation is finished and may inform
    the user about the progress of the operation visually (by a progress
    bar) and textually (by a gradually updated progress message).

    The operation is launched by executing the method 'run()' which will block
    until the operation is finished and will return its result.

    This dialog implements the API defined by 'pytis.api.Application.run()' so
    most of its documentation applies here.

    """
    BUTTON_ABORT = GenericDialog.Button(_("Abort"))

    def __init__(self, parent, function, args=(), kwargs={},
                 title=_("Operation in progress"), message=_("Please wait..."),
                 show_progress=True, maximum=100, can_abort=False,
                 elapsed_time=False, estimated_time=False, remaining_time=False,
                 time_precision='seconds'):
        """Initialize the dialog.

        Arguments:

          parent, title -- as in the parent class.
          function -- function to be called and tracked by the progress dialog.
            The function must accept the 'update' callback as its first
            argument unless 'show_progress' is false (see the class docstring).
          args, kwargs -- additional positional and keyword arguments to be
            passed to 'function' after the 'update' callback.
          message -- initial message displayed above the progress bar.  May be
            changed later calling the 'update' callback with the 'message'
            argument.
          show_progress -- if false, the dialog desn't show progress and the
            message can not be updated.  The 'function' does not receive the
            'update' callback as the first argument in this case and all the
            remaining arguments are irrelevant (and should not be passed).
          maximum -- integer value determining the range in which the progress
            is tracked (corresponds to 100% progress).
          elapsed_time -- if true, show the time from the beginning.
          estimated_time -- if true, show the estimated total time.
          remaining_time -- if true, show the estimated time to the end.
          time_precision -- elapsed/total/remaining time is by default
            displayed with precision to seconds unless it has non-zero hours,
            in which case it only displays hours and minutes.  Setting this
            option to 'minutes' (a string literal) will suppress displaying
            seconds in any case.
          can_abort -- if true, display the "Abort" button.  Operation abortion
            must be supported by the called operation through the 'update'
            callback return value (see the class docstring).

        See the documentation of 'pytis.api.Application.run()' for more
        information about progress updates and arguemnt relations.

        """
        super(ProgressDialog, self).__init__(parent, message=message or '',
                                             title=title, icon=self.ICON_RUN)
        assert callable(function)
        assert isinstance(args, (tuple, list))
        assert isinstance(kwargs, dict)
        assert show_progress or (not elapsed_time and not estimated_time and
                                 not remaining_time and not can_abort)
        assert time_precision in ('minutes', 'seconds'), time_precision
        self._function = function
        self._args = args
        self._kwargs = kwargs
        self._maximum = maximum
        self._last_progress = 0
        self._show_progress = show_progress
        self._show_elapsed_time = elapsed_time
        self._show_estimated_time = estimated_time
        self._show_remaining_time = remaining_time
        self._time_precision = time_precision
        self._can_abort = can_abort
        self._abort = False
        self._time_display = {}

    def _buttons(self):
        if self._can_abort:
            return (self.BUTTON_ABORT,)
        else:
            return ()

    def _create_content(self, sizer):
        super(ProgressDialog, self)._create_content(sizer)
        if self._show_progress:
            self._gauge = gauge = wx.Gauge(self._dialog, wx.ID_ANY, range=self._maximum,
                                           style=wx.GA_HORIZONTAL | wx.GA_SMOOTH | wx.GA_PROGRESS)
            gauge.SetMinSize((340, gauge.Size.height))
            gauge.SetValue(0)
            sizer.Add(gauge, 0, wx.EXPAND | wx.LEFT | wx.RIGHT, 16)
            grid = wx.FlexGridSizer(2, 0, 6)
            for display, label, shown in (
                    ('elapsed-time', _("Elapsed time:"), self._show_elapsed_time),
                    ('estimated-time', _("Estimated total time:"), self._show_estimated_time),
                    ('remaining-time', _("Estimated remaining time:"), self._show_remaining_time),
            ):
                if shown:
                    grid.Add(wx.StaticText(self._dialog, wx.ID_ANY, label), 0, wx.ALIGN_RIGHT)
                    ctrl = wx.StaticText(self._dialog, wx.ID_ANY, '', size=(24, 10))
                    self._time_display[display] = ctrl
                    grid.Add(ctrl)
            if grid.ItemCount:
                sizer.Add(grid, 0, wx.LEFT | wx.RIGHT | wx.ALIGN_RIGHT, 20)

    def _show_time(self, display, time):
        try:
            ctrl = self._time_display[display]
        except KeyError:
            pass
        else:
            if time is None:
                formatted = ''
            else:
                minutes, seconds = divmod(int(time), 60)
                hours, minutes = divmod(minutes, 60)
                if hours:
                    fmt = _("{hours} h {minutes} m")
                elif self._time_precision == 'seconds' and not minutes:
                    fmt = _("{seconds} s")
                elif self._time_precision == 'seconds':
                    fmt = _("{minutes} m {seconds} s")
                else:
                    fmt = _("{minutes} m")
                formatted = fmt.format(hours=hours, minutes=minutes, seconds=seconds)
            ctrl.Label = formatted
            size = self._dialog.GetTextExtent(formatted)
            ctrl.SetMinSize((max(size.x, 24), size.y))
            self._dialog.Sizer.Layout()

    def _on_button(self, event):
        if self._button(event.GetId()) == self.BUTTON_ABORT:
            self._abort = True
        else:
            return super(ProgressDialog, self)._on_button(event)

    def _update(self, progress=None, message=None):
        # progress is a number in range from 0 to the 'maximum' passed to the
        # constructor or -1 to switch to indeterminate mode pulsing.
        if message is not None:
            self._set_message(message)
        if progress == -1:
            # Note: Pulsing starts to look good when update is
            # called at least a few times per second.
            self._gauge.Pulse()
        elif progress is not None:
            self._gauge.Value = self._last_progress = max(0, min(progress, self._maximum))
        if self._time_display:
            elapsed_time = time.time() - self._start_time
            self._show_time('elapsed-time', elapsed_time)
            if self._last_progress:
                estimated_time = elapsed_time / self._last_progress * self._maximum
                self._show_time('estimated-time', estimated_time)
                self._show_time('remaining-time', estimated_time - elapsed_time)
        pytis.form.app.wx_yield(full=True)
        return not self._abort

    def _run_dialog(self):
        self._dialog.Show()
        pytis.form.app.wx_yield(full=True)
        if self._time_display:
            self._start_time = time.time()
        args = self._args
        if self._show_progress:
            args = (self._update,) + tuple(args)
        return self._function(*args, **self._kwargs)


class Calendar(GenericDialog):
    """Dialog with calendar widget, allowing date selection.

    Date can be preset using constructor arguemnt 'date'.  The method 'run()'
    returns the selected date as a 'datetime.datetime' instance or None if the
    dialog was canceled.

    """
    _BUTTONS = (GenericDialog.BUTTON_OK, GenericDialog.BUTTON_CANCEL)
    _DEFAULT_BUTTON = GenericDialog.BUTTON_OK

    def __init__(self, parent, date, title=_("Calendar"), enable_year=True, enable_month=True,
                 monday_first=True):
        """Initialize the dialog.

        Arguments:

          parent, title -- as in the parent class.
          date -- přednastavený datum jako instance 'datetime.datetime'.
          enable_year -- když je pravda, zobrazí výběr roku; boolean
          enable_month -- když je pravda, zobrazí výběr měsíce; boolean
          monday_first -- když je pravda, bude pondělí prvním dnem v týdnu;
            boolean

        Pokud argument date neobsahuje řetězec, který je možné zpracovat pomocí
        'wx.DateTime.ParseDate()', bude datum nastaven na dnešní datum.

        """
        super(Calendar, self).__init__(parent, title=title)
        # vytvoř kalendář
        style = (wx.adv.CAL_SHOW_HOLIDAYS |
                 wx.adv.CAL_SHOW_SURROUNDING_WEEKS)
        if not enable_year:
            style = style | wx.adv.CAL_NO_YEAR_CHANGE
        if not enable_month:
            style = style | wx.adv.CAL_NO_MONTH_CHANGE
        if monday_first:
            style = style | wx.adv.CAL_MONDAY_FIRST
        else:
            style = style | wx.adv.CAL_SUNDAY_FIRST
        self._style = style
        if date is None:
            self._date = pytis.data.Date.datetime()
        else:
            assert isinstance(date, datetime.date), date
            self._date = date

    def _create_content(self, sizer):
        cal = wx.adv.GenericCalendarCtrl(self._dialog, -1, style=self._style)
        # This makes year +/- buttons visible, but the calendar is not centered (not nice).
        cal.SetMinSize((cal.Size.width + 40, cal.Size.height))
        wx_date = wx.DateTime()
        if wx_date.ParseDate(str(self._date)) is None:
            wx_date = wx.DateTime_Today()
        wx_callback(wx.adv.EVT_CALENDAR, cal, self._on_calendar)
        self._handle_keys(cal)
        cal.SetDate(wx_date)
        self._cal = cal
        self._want_focus = cal
        sizer.Add(cal, 0, wx.ALL | wx.CENTER, 5)

    def _can_commit(self, widget):
        return super(Calendar, self)._can_commit(widget) or widget == self._cal

    def _customize_result(self, wxid):
        if wxid == self._cal.Id or self._button(wxid) == self.BUTTON_OK:
            date_string = str(self._cal.GetDate().FormatISODate())
            t = pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT)
            return t.validate(date_string)[0].value()
        return None

    def _on_calendar(self, event):
        return self._end_modal(self._cal.Id)


class BugReport(GenericDialog):
    """Dialog displaying information about unhandled exception.

    It is possible to send a bug report by email before closing the dialog.

    The user may close the dialog by choosing between two options:
      - Ignore the exception and try continuing running the program
        (which may not always work).
      - Exit the application

    The return value is True if exit is requested or False otherwise.

    """
    BUTTON_IGNORE = GenericDialog.Button(_("Ignore"), value=False)
    BUTTON_EXIT = GenericDialog.Button(_("Exit application"), value=True)

    _BUTTONS = (BUTTON_IGNORE, BUTTON_EXIT)
    _DEFAULT_BUTTON = BUTTON_IGNORE
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER

    def __init__(self, parent, einfo):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent window; 'wx.Frame' or 'wx.Dialog' instance
          einfo -- exception information as returned by 'sys.exc_info()'

        """
        super(BugReport, self).__init__(parent, title=_("Unhandled exception"))
        self._einfo = einfo

    def _create_content(self, sizer):
        dialog = self._dialog
        label = wx.StaticText(dialog, -1, _("Program Error"))
        label.SetFont(wx.Font(18, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_BOLD,
                              encoding=wx.FONTENCODING_DEFAULT))
        icon = self._create_icon(Message.ICON_ERROR)
        self._sent = False
        if icon is not None:
            hsizer = wx.BoxSizer(wx.HORIZONTAL)
            hsizer.Add(label, 1, wx.ALIGN_CENTER_VERTICAL)
            hsizer.Add(icon, 0, wx.ALL, 5)
            label = hsizer
        sizer.Add(label, 0, wx.EXPAND | wx.ALL | wx.CENTER, 6)
        sizer.Add(wx.StaticText(dialog, -1, _(
            "Unhandled exception caught. Please, use the button below to report the problem."
        )), 0, wx.EXPAND | wx.ALL | wx.CENTER, 6)

        try:
            # cgitb may fail here resolving attributes...
            html = cgitb.html(self._einfo)
        except Exception:
            html = '<pre>' + ''.join(traceback.format_exception(*self._einfo)) + '</pre>'
        if isinstance(html, bytes):
            # cgitb returns bytes in Python 2...
            html = html.decode('utf-8')

        nb = wx.Notebook(dialog)
        nb.AddPage(wx_text_view(nb, html, format=TextFormat.HTML, width=74, height=14),
                   _("Exception details"))
        nb.AddPage(wx.TextCtrl(nb, value='', name='message', size=(740, 200),
                               style=wx.TE_MULTILINE),
                   _("Your message (optional)"))
        sizer.Add(nb, 1, wx.EXPAND | wx.ALL, 6)

        if not pytis.config.sender_address:
            import subprocess
            status, domain = subprocess.getstatusoutput('hostname -f')
            if not status and domain != 'localhost':
                addr = '%s@%s' % (pytis.config.dbconnection.user(), domain)
            else:
                addr = ''
            email_ctrl = wx.TextCtrl(dialog, value=addr or '', name='from')  # size=(740, 30),
            email_ctrl.SetToolTip(_('Set your address in form "%s" to '
                                    'avoid being asked next time.',
                                    _("User interface settings")))
            sizer.Add(wx.StaticText(dialog, -1, _("Your email address:")), 0,
                      wx.TOP | wx.LEFT | wx.RIGHT, 6)
            sizer.Add(email_ctrl, 0, wx.EXPAND | wx.ALL, 6)

        button = wx.Button(dialog, -1, label=_("Send error report"))
        self._handle_keys(button)
        button.Bind(wx.EVT_BUTTON, self._on_send_bug_report)
        button.Bind(wx.EVT_UPDATE_UI, lambda e: e.Enable(bool(
            not self._sent and (pytis.config.sender_address or
                                dialog.FindWindowByName('from').GetValue() != '')
        )))
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        bitmap = wx.ArtProvider.GetBitmap(wx.ART_TICK_MARK, wx.ART_MESSAGE_BOX, (16, 16))
        icon = wx.StaticBitmap(dialog, -1, bitmap, name='icon')
        icon.Show(False)
        hsizer.Add(icon, 0, wx.ALIGN_CENTER_VERTICAL | wx.RIGHT | wx.LEFT, 6)
        hsizer.Add(wx.StaticText(dialog, -1, "", name='feedback'), 1, wx.ALIGN_CENTER_VERTICAL)
        hsizer.Add(button, 0)
        sizer.Add(hsizer, 0, wx.EXPAND | wx.ALL, 6)
        self._want_focus = button

    def _on_send_bug_report(self, event):
        to = pytis.config.bug_report_address
        if not to:
            app.message(_("Destination address not known. The configuration option "
                          "`bug_report_address' must be set."))
            return
        sender = pytis.config.sender_address
        if not sender:
            sender = self._dialog.FindWindowByName('from').GetValue()

        tb = self._einfo[2]
        while tb.tb_next is not None:
            tb = tb.tb_next
        subject = '{}: {} at {} line {}'.format(
            pytis.config.bug_report_subject or _("Error"),
            self._einfo[0].__name__,
            os.path.split(tb.tb_frame.f_code.co_filename)[-1],  # file name
            tb.tb_lineno,
        )

        message = self._dialog.FindWindowByName('message').GetValue().strip()
        if message:
            message += "\n\n"
        message += pytis.util.exception_info(self._einfo)

        try:
            send_mail(subject, message, to, sender,
                      message_id=email.utils.make_msgid('pytis_bugs'))
        except Exception as e:
            app.error(_("Failed sending error report:") + "\n" + unistr(e))
        else:
            self._dialog.FindWindowByName('feedback').SetLabel(
                _("The report has been sent succesfully.")
            )
            self._dialog.FindWindowByName('icon').Show()
            self._dialog.Sizer.Layout()
            self._sent = True


class CheckListDialog(Message):
    """A question dialog with a list of checkable items.

    The dialog displays a question with a list of items and a checkbox for each
    of the items.  The question is passed as 'message' (report arguments are
    actually allowed too as in 'Message').

    The result returned by the `run()' method is a sequence of boolean values,
    one for each item of 'items' passed to the constructor.  The value is True
    for items which were checked and False for unchecked items.

    """
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER
    _BUTTONS = (GenericDialog.BUTTON_OK, GenericDialog.BUTTON_CANCEL)

    def __init__(self, parent, columns=(), items=(), title=_("Select"), message=None, **kwargs):
        """Initialize the dialog.

        Arguments:
          items -- a sequence of checkable items.  Each item is a pair of
            (bool, unicode).  The bool value in indicates the initial checkbox
            state for this item.  The unicode value is the textual label for
            the item.

        """
        super(CheckListDialog, self).__init__(parent, title=title, message=message, **kwargs)
        assert isinstance(columns, (list, tuple))
        assert isinstance(items, (list, tuple))
        self._columns = columns
        self._items = items

    def _create_content(self, sizer):
        super(CheckListDialog, self)._create_content(sizer)
        self._checklist = checklist = wx.CheckListBox(
            self._dialog,
            choices=[label for state, label in self._items],
        )
        checklist.SetCheckedItems([i for i, (state, label) in enumerate(self._items) if state])
        sizer.Add(checklist, 1, wx.EXPAND | wx.ALL, 5)

    def _customize_result(self, wxid):
        if self._button(wxid) == self.BUTTON_OK:
            return [self._checklist.IsChecked(i) for i in range(len(self._items))]
        else:
            return None


class AggregationSetupDialog(GenericDialog):
    """A dialog for setting up an aggregated form.

    The result returned by the `run()' is a tuple of two tuples
    (name, group_by_columns, aggregation_columns).

    name -- user supplied human readable title of the aggregated view for
      further reference (the values of group_by_columns and aggregation_columns
      may be stored and further used under this title).

    group_by_columns -- selected group by columns as a sequence of pairs
      (column_id, function), where function is the name of the grouping
      function from 'grouping_functions' constructor argument or None if the
      column is used directly with no function applied.

    aggregation_columns -- preselected aggregation columns as a sequence of
      pairs (column_id, operation), where operation is the name of the
      aggregation function from 'aggregation_functions' constructor argument.

    """
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER
    _BUTTONS = (GenericDialog.BUTTON_OK, GenericDialog.BUTTON_CANCEL)

    def __init__(self, parent, aggregation_functions, grouping_functions, columns,
                 name, group_by_columns, aggregation_columns, aggregation_valid,
                 title=_("Aggregated view parameters")):
        """Initialize the dialog.

        Arguments:

             aggregation_functions -- specification of available aggregation
               functions as a sequence of pairs (operation, label), where
               operation is one of `pytis.data.AGG_*' constants and label is
               the string title of given function.
             grouping_functions -- specification of available functions
               aplicable to group by columns in the same format as the
               'ViewSpec' argument 'grouping_functions'.
             columns -- sequence of available columns as tuples (column_id,
               column_label, column_type).
             aggregation_valid -- function of two arguments (operation,
               column_type) returning true if given aggregation operation is
               valid for given column type and false otherwise.
             name -- user supplied human readable name as in the result of
               run() described in the class docstring.
             group_by_columns -- preselected group by columns in the same
               format as in the result of run() as described in the class
               docstring.
             aggregation_columns -- preselected aggregation columns in the same
               format as in the result of run() as described in the class
               docstring.

        """
        super(AggregationSetupDialog, self).__init__(parent, title=title)
        self._aggregation_functions = aggregation_functions
        self._grouping_functions = grouping_functions
        self._columns = columns
        self._aggregation_valid = aggregation_valid
        self._name = name
        self._group_by_columns = group_by_columns
        self._aggregation_columns = aggregation_columns

    def _create_content(self, sizer):
        super(AggregationSetupDialog, self)._create_content(sizer)
        self._name_control = wx_text_ctrl(self._dialog, value=self._name, length=50,
                                          tooltip=_("Enter the name for saving the view, or "
                                                    "leave empty, if you prefer not to save it."))
        box = wx.BoxSizer(wx.HORIZONTAL)
        box.Add(wx.StaticText(self._dialog, -1, _("Title") + ':'), wx.ALL, 3)
        box.Add(self._name_control)
        sizer.Add(box, 0, wx.EXPAND | wx.ALL, 5)
        panel = wx.ScrolledWindow(self._dialog, style=wx.TAB_TRAVERSAL)
        panel.SetScrollRate(20, 20)
        self._grid = grid = wx.FlexGridSizer(len(self._columns) + 1,
                                             len(self._aggregation_functions) + 2, 2, 6)
        self._grouping_controls = []
        self._aggregation_controls = []
        for label in ['', _("Group by")] + [x[1] for x in self._aggregation_functions]:
            grid.Add(wx.StaticText(panel, -1, label))
        for (column_id, column_label, column_type) in self._columns:
            grid.Add(wx.StaticText(panel, -1, column_label))
            checkbox = wx.CheckBox(panel, -1)
            checkbox.SetValue((column_id, None) in self._group_by_columns)
            self._grouping_controls.append(((column_id, None), checkbox))
            functions = [x for x in self._grouping_functions if isinstance(column_type, x[2])]
            if functions:
                fsizer = wx.BoxSizer(wx.VERTICAL)
                fsizer.Add(checkbox)
                cp = wx.CollapsiblePane(panel, label=_("Function"), style=wx.CP_DEFAULT_STYLE)
                panel.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self._on_collapsiblepane_changed, cp)
                pane = cp.GetPane()
                cpsizer = wx.BoxSizer(wx.VERTICAL)
                collapse = True
                for function, label, input_type, return_type in functions:
                    checkbox = wx.CheckBox(pane, -1, label=label)
                    checked = (column_id, function) in self._group_by_columns
                    checkbox.SetValue(checked)
                    if checked:
                        collapse = False
                    self._grouping_controls.append(((column_id, function), checkbox))
                    cpsizer.Add(checkbox)
                pane.SetSizer(cpsizer)
                cp.Collapse(collapse)
                fsizer.Add(cp)
                grid.Add(fsizer)
            else:
                grid.Add(checkbox)
            for operation, title in self._aggregation_functions:
                checkbox = wx.CheckBox(panel, -1)
                checkbox.SetValue((column_id, operation) in self._aggregation_columns)
                checkbox.Enable(self._aggregation_valid(operation, column_type))
                grid.Add(checkbox)
                self._aggregation_controls.append(((column_id, operation), checkbox))
        panel.SetSizer(grid)
        sizer.Add(panel, 1, wx.EXPAND | wx.ALL, 5)

    def _create_dialog(self):
        dialog = super(AggregationSetupDialog, self)._create_dialog()
        self._resize()
        return dialog

    def _on_collapsiblepane_changed(self, event):
        self._grid.Layout()
        self._resize()

    def _resize(self):
        sizer_size = self._dialog.Sizer.CalcMin()
        grid_size = self._grid.CalcMin()
        size = wx.Size(max(sizer_size.width, grid_size.width + 30),
                       sizer_size.height + grid_size.height)
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 80))
        self._dialog.SetMinClientSize(size)

    def _on_button(self, event):
        if self._button(event.GetId()) == self.BUTTON_OK:
            self._name = self._name_control.GetValue()
            self._group_by_columns = [spec for spec, checkbox in self._grouping_controls
                                      if checkbox.IsChecked()]
            self._aggregation_columns = [spec for spec, checkbox in self._aggregation_controls
                                         if checkbox.IsChecked()]
            if not self._group_by_columns:
                app.warning(_("You need to select at least one grouping column."))
                return
        return super(AggregationSetupDialog, self)._on_button(event)

    def _customize_result(self, wxid):
        if self._button(wxid) == self.BUTTON_OK:
            return self._name, tuple(self._group_by_columns), tuple(self._aggregation_columns)
        else:
            return None


class ColorSelector(Dialog):
    """Color selection dialog.

    'run()' returns the selected color as an '#RRGGBB' string or None if the
    dialog was canceled.

    """

    def __init__(self, parent, color=None):
        """Initialize the dialog.

        Arguments:

          parent, title -- as in the parent class.
          color -- initial color as '#RRGGBB' string.

        """
        super(ColorSelector, self).__init__(parent)
        assert isinstance(color, basestring) or color is None
        self._color = color

    def run(self):
        if self._color is not None:
            cdata = wx.ColourData()
            cdata.SetColour(self._color)
        else:
            cdata = None
        dialog = wx.ColourDialog(self._parent, cdata)
        self._handle_keys(dialog)
        dialog.SetFocus()
        result = dialog.ShowModal()
        if result == wx.ID_OK:
            color = dialog.GetColourData().GetColour()
        else:
            color = None
        dialog.Destroy()
        if color:
            return '#%02x%02x%02x' % (color.Red(), color.Green(), color.Blue())
        else:
            return None


class FileDialog(Dialog):
    """File selection dialog.

    Displays a dialog with a file browser for selecting one or more files.

    """
    OPEN = 'OPEN'
    """Constant for the 'mode' constructor argument to open an existing file."""
    SAVE = 'SAVE'
    """Constant for the 'mode' constructor argument to enter the name of a file to save."""

    _last_directory = {}

    def __init__(self, parent, title=None, dir=None, file=None, mode=OPEN,
                 wildcards=(_("All files"), "*"),
                 multi=False, overwrite_prompt=True):
        """Initialize the dialog.

        Arguments:
          parent -- wx parent; 'wx.Frame' or 'wx.Dialog' instance
          title -- title to show in the dialog title bar as a string
          dir -- preselected directory name as a string or None.
          file -- preselected file name as a string or None.
          mode -- one of class constants 'OPEN' or 'SAVE'.
          wildcards -- seznam masek souborů a popisů, podle kterých bude možno
            filtrovat; jedná se o sekvenci, kde každý lichý prvek určuje popis
            a každý sudý prvek je wildcard řetězcem, podle kterého budou
            soubory filtrovány, pokud je zvolen; výchozí filtrování je podle
            první dvojice. příklad: ("BMP soubory (*.bmp)", "*.bmp",
                                     "GIF soubory (*.gif)", "*.gif")
          multi -- if true, allow selection of multiple files at once (will be
            returned as a tuple); only relevant when 'mode' is 'OPEN'
          overwrite_prompt -- if true, selection of an existing file for save
            will invoke an overwrite confirmation dialog; only relevant when
            'mode' is 'SAVE'

        """
        super(FileDialog, self).__init__(parent)
        assert mode in (FileDialog.OPEN, FileDialog.SAVE)
        if title is None:
            title = {(FileDialog.OPEN, False): _("Open file"),
                     (FileDialog.OPEN, True): _("Open files"),
                     (FileDialog.SAVE, False): _("Save file"),
                     (FileDialog.SAVE, True): _("Save files")}[(mode, multi)]
        assert dir is None or isinstance(dir, basestring)
        assert file is None or isinstance(file, basestring)
        self._title = unistr(title)
        self._dir = dir
        self._file = file
        self._mode = mode
        self._wildcards = wildcards
        self._multi = multi
        self._overwrite_prompt = overwrite_prompt

    def run(self):
        directory = self._dir or FileDialog._last_directory.get(self._mode, '')
        style = {FileDialog.OPEN: wx.FD_OPEN,
                 FileDialog.SAVE: wx.FD_SAVE}[self._mode]
        if self._multi and self._mode == FileDialog.OPEN:
            style = style | wx.FD_MULTIPLE
        if self._overwrite_prompt and self._mode == FileDialog.SAVE:
            style = style | wx.FD_OVERWRITE_PROMPT
        dialog = wx.FileDialog(self._parent,
                               message=self._title,
                               defaultDir=directory,
                               defaultFile=self._file or '',
                               # TODO: We may need to construct the wildcards
                               # to be case insensitive, such as '*.[jJ][pP][gG]':
                               wildcard='|'.join(self._wildcards),
                               style=style)
        result = dialog.ShowModal()
        directory = dialog.GetDirectory()
        if self._multi:
            path = tuple(dialog.GetPaths())
        else:
            path = dialog.GetPath()
        dialog.Destroy()
        if result == wx.ID_OK:
            FileDialog._last_directory[self._mode] = directory
            return path
        else:
            return None


class DirDialog(Dialog):
    """Dialog for directory selection.

    Displays a dialog to browse existing directories and also allows creation
    of a new directory.

    """
    _last_directory = None

    def __init__(self, parent, title=_("Directory selection"), path=None):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent; 'wx.Frame' or 'wx.Dialog' instance
          title -- title to show in the dialog title bar as a string
          path -- initial derectory or None to use the last selected directory.

        """
        super(DirDialog, self).__init__(parent)
        assert isinstance(title, basestring)
        assert path is None or isinstance(path, basestring)
        self._title = unistr(title)
        self._path = path

    def run(self):
        """Zobraz dialog a vrať cestu k vybranému souboru jeko řetězec.

        Pokud je argument konstruktoru 'multi' pravdivý, bude vrácen tuple
        řetězců.

        """
        dialog = wx.DirDialog(self._parent,
                              message=self._title,
                              defaultPath=self._path or DirDialog._last_directory or '',
                              style=wx.DD_DEFAULT_STYLE)
        result = dialog.ShowModal()
        path = dialog.GetPath()
        dialog.Destroy()
        if result == wx.ID_OK:
            DirDialog._last_directory = path
            return path
        else:
            return None
