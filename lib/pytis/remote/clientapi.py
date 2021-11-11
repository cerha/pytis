# -*- coding: utf-8 -*-

# Copyright (C) 2018-2021 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2011-2018 OUI Technology Ltd.
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""This module defines the Client RPyC service API.

The service providing this API runs on the client which runs pytis application
remotely through X2Go using the Pytis2Go client.  The service allows the Pytis
application running on the server to perform certain tasks on the client
machine, such as open local file through a GUI file dialog and pass that file
to the server.  It simply attempts to transparently provide the functionality
which is not directly available due to the fact that the application is running
on a remote server.

This module is actually not part of Pytis.  It is not imported (executed) by
Pytis on the server.  It is only read as a file and pushed to the Pytis2Go
client which executes this code.  It comes with Pytis, because it defines the
functionality consumed by Pytis.  The remote functions defined in remote.py
rely on the API defined here so it belongs here to keep the API in sync with
the consumer expectations.  This also means that the dependencies of this
module don't need to be installed on the application server, but on the client
machine running Pytis2Go.

"""
from __future__ import print_function

from future import standard_library
from builtins import str
from builtins import zip
from builtins import range
from builtins import object
from past.builtins import basestring
from past.builtins import execfile

# Beware: These dependencies (and the others imported in this file later in runtime)
# do not need to be installed on the application server, but on the client machine
# running Pytis2go where this code is (only) executed (see module docstring).

import hashlib
import io
import os
import random
import rpyc
import socket
import subprocess
import sys
import tempfile
import threading


standard_library.install_aliases()


class BackendNotAvailable(Exception):
    """Error raised in backend constructor when the backend is unavailable.

    This typically means that certain Python library of other dependency is not
    installed.

    """
    pass


class ClientUIBackend(object):
    """Backend for UI operations running on the client side.

    This class defines the abstract API for performing various client side
    operations, such as running a file selection dialog with access to the
    client's file system.

    Derived classes (the actual backends) implement the API for a particular
    GUI toolkit/library.  Backends should always override the private
    counterpart of given public API method (the same name, just beginning with
    an underscore).

    Instantiating 'ClientUIBackend' performs selection of available backends
    and actually returns an instance of a particular backend (subclass), rather
    than 'ClientUIBackend' itself.

    """

    _DEPENDS = ()
    """Tuple of all python module names required by the backend.

    Creation of module instance will fail with 'BackendNotAvailable' if one of
    the named modules is not available.

    """
    class _Object(object):
        """Helper class for method result storage."""
        def __init__(self, **kwargs):
            self.__dict__.update(kwargs)

    def __new__(cls, *args, **kwargs):
        if cls is ClientUIBackend:
            if sys.platform == 'win32':
                backends = (
                    TkUIBackend,
                    WxUIBackend,
                    ZenityUIBackend,
                    Win32UIBackend,
                )
            elif sys.platform == 'darwin':
                backends = (
                    WxUIBackend,
                    TkUIBackend,
                    # ZenityUIBackend,  # TODO: Currently unusable.
                )
            else:
                backends = (
                    WxUIBackend,
                    ZenityUIBackend,
                    TkUIBackend,
                )
            for subclass in backends:
                try:
                    backend = subclass.__new__(subclass, *args, **kwargs)
                except BackendNotAvailable:
                    continue
                # print("Using %s" % backend)
                return backend
            raise Exception(u'No suitable UI backend found.')
        else:
            return object.__new__(cls)

    def __init__(self):
        import pkgutil
        for module in self._DEPENDS:
            if not pkgutil.find_loader(module):
                raise BackendNotAvailable("Module %s not installed." % module)

    def _unicode(self, x):
        if isinstance(x, (tuple, list)):
            x = [self._unicode(y) for y in x]
        elif not isinstance(x, str) and x is not None:
            x = str(x, sys.getfilesystemencoding())
        return x

    def _pattern_label(self, label, patterns):
        # This method is called from the backend only if needed because some
        # backends (actually Tk) don't need it (they add the list of patterns
        # to the label automatically).
        return "%s (%s)" % (label, ', '.join(patterns))

    def enter_text(self, title=u"Zadejte text", label=None, password=False):
        """Prompt the user to enter text and return the text.

        Arguments:

          title -- text entry dialog title
          label -- text field label; If None, title is used
          password -- if True, text entered should be hidden by stars

        """
        assert isinstance(title, basestring), title
        assert label is None or isinstance(label, basestring), label
        assert isinstance(password, bool), password
        if label is None:
            label = title
        return self._enter_text(title, label, password)

    def _enter_text(self, title, label, password):
        raise NotImplementedError('%s._enter_text()' % self.__class__.__name__)

    def select_option(self, title=u"Výběr položky",
                      label=u"Zvolte jednu z níže uvedených položek:",
                      columns=(), data=(), return_column=1):
        """Prompt the user to select from a given list of options.

        Arguments:

          title -- text entry dialog title
          label -- selection field label
          columns -- sequence of column labels
          data -- sequence of tuples containing the values to be displayed in
            the selection.  The items of each tuple are displayed as one table
            row.  Thus the length of each tuple must match the number of
            columns.
          return_column -- number of column (beginning from one).  The value
            of this column in the selected row will be returned.

        Returns the value of 'return_column' in the selected row or None if
        nothing selected.

        """
        assert isinstance(title, basestring), title
        assert label is None or isinstance(label, basestring), label
        assert isinstance(columns, (tuple, list)), columns
        assert isinstance(data, (tuple, list)), data
        assert isinstance(return_column, int) \
            and return_column >= 1 and return_column <= len(columns), return_column
        return self._select_option(title, label, columns, data, return_column)

    def _select_option(self, title, label, columns, data, return_column):
        raise NotImplementedError('%s._select_option()' % self.__class__.__name__)

    def select_file(self, title=None, directory=None, filename=None,
                    patterns=(), pattern=None, save=False, multi=False):
        """Return the file name(s) of user selected file(s).

        The file is selected by the user using a GUI dialog.  If the user
        cancels the dialog, 'None' is returned.

        Arguments:

          title -- dialog title
          directory -- initial directory for the dialog
          filename -- default filename or None
          patterns -- a sequence of pairs (LABEL, PATTERN) determining the
            filename filters available within the dialog (for backends which
            support it).  Label is a user visible description of the filter,
            such as "PDF document".  PATTERN is a filename pattern as a
            basestring, such as '*.pdf', or a sequence of such patterns, such
            as ('*.jpg', '*.jpeg', '*.png', '*.gif') if multiple file types are
            to match a single filter.  An additional entry "All files" with
            pattern '*.*' is always added automatically to the end.  The first
            item is the initially selected filter in the dialog.
          pattern -- the required file name pattern as a basestring, sequence
            of basestrings or 'None'.  If 'patterns' don't already contain an
            entry for given pattern(s), such item is added as the first (and
            thus the initially selected) entry with label "Files of the
            required type".
          save -- True iff the file is to be open for writing
          multi -- iff true, allow selecting multiple files (not possible when save is True)

        """
        assert title is None or isinstance(title, basestring), title
        assert directory is None or isinstance(directory, basestring), directory
        assert filename is None or isinstance(filename, basestring), filename
        assert pattern is None or isinstance(pattern, (basestring, tuple, list)), pattern
        assert isinstance(patterns, (tuple, list)), patterns
        assert isinstance(save, bool), save
        assert isinstance(multi, bool), multi
        assert not (save and multi), (save, multi)

        def xtuple(x):
            return tuple(x) if isinstance(x, (tuple, list)) else (x,)

        if title is None:
            if save:
                title = u"Uložit soubor"
            elif multi:
                title = u"Výběr souborů"
            else:
                title = u"Výběr souboru"
        if filename:
            extension = os.path.splitext(filename)[1].lstrip('.') or None
            if extension and not pattern:
                pattern = "*." + extension
        else:
            extension = None
        patterns = list(patterns) + [(u"Všechny soubory", "*.*")]
        if pattern and xtuple(pattern) not in [xtuple(pat) for label, pat in patterns]:
            patterns.insert(0, (u"Soubory požadovaného typu", pattern))
        return self._unicode(self._select_file(title, directory, filename,
                                               [(label, xtuple(pat)) for label, pat in patterns],
                                               extension, save, multi))

    def _select_file(self, title, directory, filename, patterns, extension, save, multi):
        raise NotImplementedError('%s._select_file()' % self.__class__.__name__)

    def select_directory(self, title=u"Výběr adresáře", directory=None):
        """Return the name of user selected directory.

        The directory is selected by the user using a GUI dialog.  If the user
        cancels the dialog, 'None' is returned.

        Arguments:

          directory -- initial directory for the dialog

        """
        assert isinstance(title, basestring), title
        assert directory is None or isinstance(directory, basestring), directory
        return self._unicode(self._select_directory(title, directory))

    def _select_directory(self, title, directory):
        raise NotImplementedError('%s._select_directory()' % self.__class__.__name__)

    def get_clipboard_text(self):
        """Return the text stored in system clipboard on user's machine."""
        return self._unicode(self._get_clipboard_text())

    def _get_clipboard_text(self, directory):
        raise NotImplementedError('%s._get_clipboard_text()' % self.__class__.__name__)

    def set_clipboard_text(self, text):
        """Store given text in system clipboard on user's machine.

        Arguments:

          text -- the text to be stored in the clipboard.

        """
        assert text is None or isinstance(text, basestring), text
        self._set_clipboard_text(self._unicode(text))

    def _set_clipboard_text(self, text):
        raise NotImplementedError('%s._set_clipboard_text()' % self.__class__.__name__)


class WxUIBackend(ClientUIBackend):
    """Implements UI backend operations using wx Widgets (requires wxPython)."""

    _DEPENDS = ('wx',)

    def __init__(self):
        super(WxUIBackend, self).__init__()
        if os.path.basename(sys.argv[0]) == 'pytest' or os.getenv('P2GO_RPYC_PROCESS') == 'true':
            # TODO: This is only run in tests and when Pytis2go RPyC service is run
            # in a separate process.  This is because older Pytis2go versions run the
            # service within its main process, where 'wx.App' already exists.
            # Starting another instance would cause conflicts and might crash
            # the whole Python process.
            # The condition may be removed (together with setting P2GO_RPYC_PROCESS
            # in Pytis2go) as soon as there are no older Pytis2go versions in use.
            import wx
            self._app = wx.App(False)

    def _enter_text(self, title, label, password):
        import wx
        if password:
            cls = wx.PasswordEntryDialog
        else:
            cls = wx.TextEntryDialog
        dialog = cls(None, message=label, caption=title)
        if not dialog.HasFlag(wx.STAY_ON_TOP):
            dialog.ToggleWindowStyle(wx.STAY_ON_TOP)
        wx.STAY_ON_TOP
        dialog.ShowModal()
        result = dialog.GetValue()
        dialog.Destroy()
        return result

    def _select_option(self, title, label, columns, data, return_column):
        import wx
        # TODO: The column labels are not displayed and columns may not be aligned properly,
        # but is this really necessary for the puropse where it is used (encryption key
        # selection in 'pytis.remote.PytisService')?  Maybe the definition of
        # 'select_option()' should be simplified.
        dialog = wx.SingleChoiceDialog(None, message=label, caption=title,
                                       choices=['\t'.join(item) for item in data])
        if not dialog.HasFlag(wx.STAY_ON_TOP):
            dialog.ToggleWindowStyle(wx.STAY_ON_TOP)
        if dialog.ShowModal() != wx.ID_OK:
            result = None
        else:
            result = data[dialog.GetSelection()][return_column - 1]
        dialog.Destroy()
        return result

    def _select_file(self, title, directory, filename, patterns, extension, save, multi):
        import wx
        style = wx.STAY_ON_TOP
        if save:
            # TODO: Overwrite prompt doesn't seem to appear on Mac OS X.
            style |= wx.FD_SAVE | wx.FD_OVERWRITE_PROMPT
        else:
            style |= wx.FD_OPEN
        if multi:
            style |= wx.FD_MULTIPLE
        dialog = wx.FileDialog(None, style=style, message=title,
                               defaultDir=directory or '', defaultFile=filename or '',
                               wildcard='|'.join(["%s|%s" % (self._pattern_label(label, pat),
                                                             ';'.join(pat))
                                                  for label, pat in patterns]))
        if dialog.ShowModal() != wx.ID_OK:
            result = None
        elif multi:
            result = dialog.GetPaths()
        else:
            result = dialog.GetPath()
        dialog.Destroy()
        return result

    def _select_directory(self, title, directory):
        import wx
        dialog = wx.DirDialog(None, message=title, defaultPath=directory or '',
                              style=wx.DD_DEFAULT_STYLE | wx.STAY_ON_TOP)
        if dialog.ShowModal() == wx.ID_OK:
            result = dialog.GetPath()
        else:
            result = None
        dialog.Destroy()
        return result

    def _get_clipboard_text(self):
        import wx
        if wx.TheClipboard.Open():
            data = wx.TextDataObject()
            success = wx.TheClipboard.GetData(data)
            wx.TheClipboard.Close()
            if success:
                return data.GetText()

    def _set_clipboard_text(self, text):
        import wx
        if wx.TheClipboard.Open():
            data = wx.TextDataObject()
            data.SetText(text)
            wx.TheClipboard.SetData(data)
            wx.TheClipboard.Close()


class Win32UIBackend(ClientUIBackend):
    """Implements UI backend operations using win32 Python API."""

    _DEPENDS = ('win32ui', 'win32con', 'win32gui', 'win32clipboard', 'pywin.mfc.dialog')

    def _enter_text(self, title, label, password):
        import pywin.mfc.dialog
        return pywin.mfc.dialog.GetSimpleInput(label)

    def _select_option(self, title, label, columns, data, return_column):
        import win32con
        from pywin.mfc.dialog import Dialog
        rows = ['   '.join(row) for row in data]
        width = max(max(len(row) for row in rows) * 4, len(label) * 4, 100)
        height = len(rows) * 8 + 4
        IDC_LIST = 9000
        IDC_TEXT = 9001
        result = self._Object(selection=None)

        class SelectionDialog(Dialog):

            def OnSelect(self, ctrl, action):
                if ctrl == IDC_LIST:
                    result.selection = self.GetDlgItem(IDC_LIST).GetCurSel()

            def OnInitDialog(self):
                rc = Dialog.OnInitDialog(self)
                self.SetDlgItemText(IDC_TEXT, label)
                listbox = self.GetDlgItem(IDC_LIST)
                for row in data:
                    listbox.AddString('   '.join(row))
                self.HookCommand(self.OnSelect, IDC_LIST)
                return rc

        dialog = SelectionDialog([
            [title, (0, 0, width + 10, height + 38), win32con.WS_CAPTION | win32con.DS_MODALFRAME,
             None, (8, "MS SansSerif")],
            ["static", "", IDC_TEXT, (5, 4, 150, 14), win32con.WS_CHILD | win32con.WS_VISIBLE],
            ["listbox", "List", IDC_LIST, (5, 16, width, height), win32con.WS_VISIBLE],
            [128, u"Ok", win32con.IDOK, (width - 45, height + 18, 50, 14),
             win32con.BS_PUSHBUTTON | win32con.WS_VISIBLE],
        ])
        dialog.DoModal()
        if result.selection is not None:
            return data[result.selection][return_column - 1]
        else:
            return None

    def _select_file(self, title, directory, filename, patterns, extension, save, multi):
        import win32ui
        import win32con
        flags = win32con.OFN_HIDEREADONLY
        if save:
            flags |= win32con.OFN_OVERWRITEPROMPT
            mode = 0
        else:
            mode = 1
        if multi:
            flags |= win32con.OFN_ALLOWMULTISELECT
        # This hack with finding non-specified windows is used so that
        # we get some parent window for CreateFileDialog.
        # Without this parent windows the method DoModal doesn't show
        # the dialog window on top...
        parent = win32ui.FindWindow(None, None)
        dialog = win32ui.CreateFileDialog(mode, extension, "%s" % filename or '*.*', flags,
                                          '|'.join(["%s|%s" % (self._pattern_label(label, pat),
                                                               ';'.join(pat))
                                                    for label, pat in patterns]) + '||',
                                          parent)
        if directory:
            dialog.SetOFNInitialDir(directory)
        result = dialog.DoModal()
        if result != 1:
            return None
        if multi:
            return dialog.GetPathNames()
        else:
            return dialog.GetPathName()

    def _select_directory(self, title, directory):
        import win32gui
        from win32com.shell import shell, shellcon

        def callback(hwnd, msg, lp, data):
            if msg == shellcon.BFFM_INITIALIZED:
                win32gui.SendMessage(hwnd, shellcon.BFFM_SETSELECTION, 1, directory)

        pidl, dname, imglist = shell.SHBrowseForFolder(
            win32gui.GetDesktopWindow(),
            # Get PIDL of the topmost folder for the dialog
            shell.SHGetFolderLocation(0, shellcon.CSIDL_DESKTOP, 0, 0),
            title,
            0,
            callback,
            None,
        )
        # Transform PIDL back to a directory name and return it
        return shell.SHGetPathFromIDList(pidl)

    def _get_clipboard_text(self):
        import win32clipboard
        win32clipboard.OpenClipboard()
        try:
            data = win32clipboard.GetClipboardData(win32clipboard.CF_UNICODETEXT)
        except Exception:  # may happen when there is no clipboard data
            data = None
        win32clipboard.CloseClipboard()
        return data

    def _set_clipboard_text(self, text):
        import win32clipboard
        win32clipboard.OpenClipboard()
        win32clipboard.EmptyClipboard()
        win32clipboard.SetClipboardData(win32clipboard.CF_UNICODETEXT, text)
        win32clipboard.CloseClipboard()


class ClipboardUIBackend(ClientUIBackend):
    """Implements clipboard operations using the Python module 'pyperclip'."""

    _DEPENDS = ('pyperclip',)

    def _get_clipboard_text(self):
        import pyperclip
        return pyperclip.paste()

    def _set_clipboard_text(self, text):
        import pyperclip
        pyperclip.copy(text)


class TkUIBackend(ClipboardUIBackend):
    """Implements UI backend operations using Tkinter."""

    _DEPENDS = ClipboardUIBackend._DEPENDS + ('tkinter', 'tkinter.ttk',
                                              'tkinter.simpledialog', 'tkinter.filedialog')

    def _in_tk_app(method):
        def run(self, *args, **kwargs):
            import tkinter
            root = tkinter.Tk()
            root.withdraw()
            # Make it almost invisible - no decorations, 0 size, top left corner.
            root.overrideredirect(True)
            root.geometry('0x0+0+0')
            # Show window again and lift it to top so it can get focus,
            # otherwise dialogs will end up behind the terminal.
            root.deiconify()
            root.lift()
            root.focus_force()
            try:
                return method(self, root, *args, **kwargs)
            finally:
                root.destroy()
        return run

    @_in_tk_app
    def _enter_text(self, root, title, label, password):
        import tkinter.simpledialog
        if password:
            kwargs = dict(show='*')
        else:
            kwargs = {}
        return tkinter.simpledialog.askstring(title, label, parent=root, **kwargs)

    @_in_tk_app
    def _select_option(self, root, title, label, columns, data, return_column):
        import tkinter
        import tkinter.ttk
        rows = ['   '.join(row) for row in data]
        result = self._Object(selection=None)
        dialog = tkinter.Toplevel(root)
        dialog.title(title)
        tklabel = tkinter.ttk.Label(dialog, text=label)
        tklabel.pack(padx=5, pady=2, anchor=tkinter.W)
        listbox = tkinter.Listbox(dialog, listvariable=tkinter.StringVar(value=tuple(rows)),
                                  height=len(rows), activestyle='dotbox')
        listbox.pack(expand=True, fill=tkinter.BOTH, padx=5, pady=5)

        def submit():
            idxs = listbox.curselection()
            if len(idxs) == 1:
                result.selection = int(idxs[0])
            dialog.quit()

        button = tkinter.ttk.Button(dialog, text=u"Ok", command=submit, default='active')
        button.pack(pady=5, padx=5, side=tkinter.RIGHT)
        listbox.bind('<Double-1>', lambda e: submit())
        dialog.bind('<Return>', lambda e: submit())
        dialog.bind('<Escape>', lambda e: dialog.quit())
        dialog.protocol('WM_DELETE_WINDOW', dialog.quit)
        dialog.update()
        dialog.minsize(dialog.winfo_width() + 50, dialog.winfo_height())
        dialog.after(1, lambda: listbox.focus_force())
        root.mainloop()
        if result.selection is not None:
            return data[result.selection][return_column - 1]
        else:
            return None

    @_in_tk_app
    def _select_file(self, root, title, directory, filename, patterns, extension, save, multi):
        import tkinter.filedialog
        if save:
            dialog = tkinter.filedialog.asksaveasfilename
            kwargs = dict()
        else:
            dialog = tkinter.filedialog.askopenfilename
            kwargs = dict(multiple=multi)
        if sys.platform != 'darwin':
            # The patterns don't work on Mac OS X.  If present, all files are grayed out...
            filetypes = []
            for label, pat in patterns:
                for pattern in pat:
                    filetypes.append((label, pattern[1:] if pattern.startswith('*.') else pattern))
            if sys.platform == 'win32':
                filetypes.reverse()
            kwargs['filetypes'] = filetypes
        result = dialog(parent=root, title=title, initialdir=directory, initialfile=filename,
                        defaultextension=extension, **kwargs)
        if not result:
            return None
        elif multi:
            # Work around a strange problem which appears on Linux - a file named '*.*' prefixed
            # with the selected dirtectory name is added as the first item to the returned list.
            return [fname for fname in root.tk.splitlist(result) if not fname.endswith('*.*')]
        else:
            return result

    @_in_tk_app
    def _select_directory(self, root, title, directory):
        import tkinter.filedialog
        return tkinter.filedialog.askdirectory(title=title, parent=root, initialdir=directory)


class ZenityUIBackend(ClipboardUIBackend):
    """Implements UI backend operations using the Zenity command line tool."""

    def __init__(self):
        if self._run_zenity('--version') is None:
            raise BackendNotAvailable("Zenity command line interface not installed.")
        super(ZenityUIBackend, self).__init__()

    def _run_zenity(self, *args):
        import subprocess
        try:
            output = subprocess.check_output(('zenity',) + args)
        except (subprocess.CalledProcessError, OSError):
            return None
        return output.rstrip('\r\n')

    def _enter_text(self, title, label, password):
        args = ('--password',) if password else ()
        return self._run_zenity('--entry', '--title', title, '--text', label, *args)

    def _select_option(self, title, label, columns, data, return_column):
        args = []
        for column in columns:
            args.extend(('--column', column))
        for row in data:
            args.extend(row)
        answer = self._run_zenity('--list', '--title', title, '--text', label,
                                  '--print-column', str(return_column), *args)
        if answer and '|' in answer:
            # This is a bug in version 3.8.0.
            answer = answer.split('|')[0]
        return answer

    def _select_file(self, title, directory, filename, patterns, extension, save, multi):
        args = []
        if directory is not None:
            filename = os.path.join(directory, filename or '')
        if filename is not None:
            args.extend(('--filename', filename,))
        for label, pat in patterns:
            args.extend(('--file-filter', self._pattern_label(label, pat) + ' | ' + ' '.join(pat)))
        if save:
            args.append('--save')
        if multi:
            args.append('--multiple')
        return self._run_zenity('--file-selection', '--title', title, *args)

    def _select_directory(self, title, directory):
        args = []
        if directory is not None:
            args.extend(('--filename', directory,))
        return self._run_zenity('--file-selection', '--title', title, '--directory', *args)


class PyZenityUIBackend(ClipboardUIBackend):
    """Implements UI backend operations using the PyZenity Python module."""
    # This backend is unused because it only adds another dependency
    # compared to 'ZenityUIBackend'.

    _DEPENDS = ClipboardUIBackend._DEPENDS + ('PyZenity',)

    def _enter_text(self, title, label, password):
        import PyZenity
        text = PyZenity.GetText(title=title, text=label, password=password)
        if text is None:
            return None
        else:
            return text.rstrip('\r\n')

    def _select_option(self, title, label, columns, data, return_column):
        import PyZenity
        return PyZenity.List(columns, title=title, text=label, data=data)

    def _select_file(self, title, directory, filename, patterns, extension, save, multi):
        import PyZenity
        # TODO: Pass the remaining arguments (undocumented in PyZenity).
        files = PyZenity.GetFilename(title=title, multiple=multi)
        if not files:
            return None
        elif not multi:
            return files[0]
        else:
            return files

    def _select_directory(self, title, directory):
        import PyZenity
        directory_list = PyZenity.GetDirectory(title=title, selected=directory)
        if directory_list and len(directory_list) > 0:
            return directory_list[0]
        else:
            return None


class ExposedFileWrapper(object):
    """Exposed 'file' like object.

    This class makes it possible to open files across the remote connection.

    The file resides on the client machine, but the server may access it.

    """
    def __init__(self, filename, mode='r', handle=None, encoding=None,
                 encrypt=None, decrypt=False):
        """Arguments:

        filename -- name of the underlying file on client's filesystem.
        handle -- file descriptor; If None, the file is opened by filename.
        mode -- mode for opening the file.
        encoding -- file content output encoding, string or None.  Not
          applicable in binary modes.
        encrypt -- function performing data encryption.  The function must
          accept a file like object as argument and return its encrypted
          contents as a string.  If 'None' then don't encrypt the file;
          applicable only for input modes
        decrypt -- function performing data decryption.  The function must
          accept a string argument and return its decrypted content as a
          string.  If 'None' then don't decrypt the file.  Applicable only for
          output modes

        """
        assert encoding is None or 'b' not in mode, (encoding, mode)
        if handle is None:
            f = open(filename, mode)
        else:
            f = os.fdopen(handle, mode)
        if encrypt is not None:
            assert mode[0] == 'r', mode
            f = io.BytesIO(encrypt(f))
        if decrypt:
            assert mode[0] != 'r', mode
            self._decrypted = f
            f = io.BytesIO()
        else:
            self._decrypted = None
        self._f = f
        self._filename = filename
        self._encoding = encoding
        self._decrypt = decrypt

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, exc_tb):
        self.exposed_close()

    def _decode(self, data):
        # TODO: Remove when Python 2 support not needed and pass encoding to open().
        if self._encoding is not None:
            data = data.decode(self._encoding)
        return data

    @property
    def exposed_name(self):
        return self._filename

    def exposed_read(self, *args, **kwargs):
        return self._decode(self._f.read(*args, **kwargs))

    def exposed_readline(self):
        return self._decode(self._f.readline())

    def exposed_readlines(self):
        lines = self._f.readlines()
        if self._encoding is not None:
            lines = [self._decode(l) for l in lines]
        return lines

    def exposed_write(self, data):
        if sys.version_info[0] == 2 and isinstance(data, buffer):
            data = data[:]
        elif self._encoding is not None and self._decrypted is None:
            data = data.encode(self._encoding)
        self._f.write(data)

    def exposed_seek(self, *args, **kwargs):
        return self._f.seek(*args, **kwargs)

    def exposed_flush(self):
        return self._f.flush()

    def exposed_fileno(self, *args, **kwargs):
        return self._f.fileno(*args, **kwargs)

    def exposed_close(self):
        if self._decrypted is not None:
            encrypted = self._f.getvalue()
            if encrypted:
                decrypted = self._decrypt(encrypted)
                if self._encoding is not None:
                    decrypted = decrypted.encode(self._encoding)
                self._decrypted.write(decrypted)
                self._decrypted.close()
        self._f.close()


class PytisClientAPIService(rpyc.Service):

    _client_instance = None
    _gpg_instance = None

    def _client(self):
        if self._client_instance is None:
            self._client_instance = ClientUIBackend()
        return self._client_instance

    def _gpg(self):
        if self._gpg_instance is None:
            # TODO: Either rely on gnupg from pip or include it here in clientapi.py.
            import pytis.remote.gnupg
            self._gpg_instance = pytis.remote.gnupg.GPG(options=['--trust-model', 'always'])
        return self._gpg_instance

    def _select_encryption_keys(self, gpg, keys):
        if keys:
            fingerprints = []
            for k in keys:
                fingerprints.extend(gpg.import_keys(k).fingerprints)
            return fingerprints
        keys = gpg.list_keys(True)
        n_keys = len(keys)
        if n_keys == 0:
            raise Exception("No encryption key found")
        elif n_keys == 1:
            selected_key = keys[0]['keyid']
        else:
            data = [(k['keyid'], ', '.join(k['uids'])) for k in keys]
            selected_key = self._client().select_option(title="Výběr šifrovacího klíče",
                                                        label="Vyberte šifrovací klíč",
                                                        columns=(u"Id", u"Uživatel",),
                                                        data=data)
            if not selected_key:
                raise Exception("Canceled")
        return [selected_key]

    def _encrypt(self, keys):
        def encrypt(f):
            gpg = self._gpg()
            selected_keys = self._select_encryption_keys(gpg, keys)
            return str(gpg.encrypt_file(f, selected_keys))
        if keys is not None:
            return encrypt
        else:
            return None

    def _decrypt(self, decrypt):
        def decrypt_(encrypted):
            gpg = self._gpg()
            while True:
                passphrase = self._client().enter_text(u"Zadejte heslo",
                                                       label=u"Heslo k dešifrovacímu klíči",
                                                       password=True)
                if passphrase is None:
                    return ''
                decrypted = gpg.decrypt(encrypted, passphrase=passphrase).data
                if decrypted:
                    return decrypted
        if decrypt:
            return decrypt_
        else:
            return None

    def _open_file(self, filename, encoding, mode, encrypt=None, decrypt=False):
        return ExposedFileWrapper(filename, encoding=encoding, mode=mode,
                                  encrypt=self._encrypt(encrypt),
                                  decrypt=self._decrypt(decrypt))

    def exposed_restart(self):
        """Restart the user service."""
        file_name = sys.argv[0]
        if file_name[:-4] == '.pyc':
            file_name = file_name[:-1]
        execfile(file_name)

    def exposed_get_clipboard_text(self):
        """Return current clipboard text, as unicode.

        If the text can't be retrieved, return 'None'.

        """
        return self._client().get_clipboard_text()

    def exposed_set_clipboard_text(self, text):
        """Set clipboard content to text.

        Arguments:

          text -- text to store into the clipboard; unicode

        """
        assert isinstance(text, str), text
        return self._client().set_clipboard_text(text)

    def exposed_launch_file(self, path):
        """Start associated application on path.

        Arguments:

          path -- path to the file to be started with its associated
            application; basestring.

        """
        assert isinstance(path, basestring), path
        if sys.platform == 'win32':
            os.startfile(path)
        else:
            subprocess.Popen(['xdg-open', path])

    def exposed_open_file(self, filename, mode, encoding=None, encrypt=None, decrypt=False):
        """Return a read-only 'file' like object of the given file.

        Arguments:

          filename -- name of the file to open, basestring
          mode -- mode for opening the file
          encoding -- file content output encoding, string or None
          encrypt -- list of encryption keys to use to encrypt the file; if the
            list is empty then let the user select the keys; if 'None' then
            don't encrypt the file; applicable only for input modes
          decrypt -- if true then decrypt the file contents; applicable only
            for output modes

        """
        return self._open_file(filename, encoding, mode, encrypt=encrypt, decrypt=decrypt)

    def exposed_open_selected_file(self, directory=None, patterns=(), pattern=None, encrypt=None):
        """Return a read-only 'file' like object of a user selected file.

        The file is selected by the user using a GUI dialog.  If the user
        cancels the dialog, 'None' is returned.

        Arguments:

          directory -- initial directory of the GUI dialog
          patterns -- a sequence of pairs (label, pattern) for file filtering
            as described in 'pytis.remote.ClientUIBackend.select_file()'.
          pattern -- a string defining the required file name pattern, or 'None'
          encrypt -- list of encryption keys to use to encrypt the file; if the
            list is empty then let the user select the keys; if 'None' then
            don't encrypt the file

        """
        assert isinstance(patterns, (tuple, list)), patterns
        assert pattern is None or isinstance(pattern, basestring), pattern
        filename = self._client().select_file(directory=directory,
                                              patterns=patterns, pattern=pattern)
        if filename is None:
            return None
        return ExposedFileWrapper(filename, mode='rb', encrypt=self._encrypt(encrypt))

    def exposed_make_selected_file(self, directory=None, filename=None, patterns=(), pattern=None,
                                   encoding=None, mode='wb', decrypt=False):
        """Return a write-only 'file' like object of a user selected file.

        The file is selected by the user using a GUI dialog.  If the user
        cancels the dialog, 'None' is returned.

        Arguments:

          directory -- initial directory of the GUI dialog
          filename -- default filename or None
          patterns -- a sequence of pairs (label, pattern) for file filtering
            as described in 'pytis.remote.ClientUIBackend.select_file()'.
          pattern -- a string defining the required file name pattern, or 'None'
          encoding -- output encoding, string or None
          mode -- default mode for opening the file
          decrypt -- if true then decrypt the file contents

        """
        assert isinstance(patterns, (tuple, list)), patterns
        assert pattern is None or isinstance(pattern, basestring), pattern
        filename = self._client().select_file(filename=filename, directory=directory,
                                              patterns=patterns, pattern=pattern, save=True)
        if filename is None:
            return None
        else:
            return self._open_file(filename, encoding, mode, decrypt=decrypt)

    def exposed_make_temporary_file(self, suffix='', encoding=None, mode='wb', decrypt=False):
        """Create a temporary file and return its instance.

        The return value is a 'tempfile.NamedTemporaryFile' instance.

        Arguments:

          suffix -- suffix to use in the temporary file name; if a dot should
            be part of the suffix then it must be explicitly included in it
          encoding -- output encoding, string or None
          mode -- default mode for opening the file
          decrypt -- if true then decrypt the file contents

        """
        handle, filename = tempfile.mkstemp(prefix='pytistmp', suffix=suffix)
        return ExposedFileWrapper(filename, handle=handle, encoding=encoding, mode=mode,
                                  decrypt=self._decrypt(decrypt))

    def exposed_select_directory(self, directory=None, title=u"Výběr adresáře"):
        return self._client().select_directory(directory=directory, title=title)

    def exposed_select_file(self, filename=None, directory=None, title=None,
                            patterns=(), pattern=None, save=False, multi=False):
        """Return a list of filenames selected by user in GUI dialog.

        The filenames are selected by the user using a GUI dialog.  If the user
        cancels the dialog, empty list is returned.

        Arguments:

          filename -- default filename or None
          directory -- initial directory of the GUI dialog
          title -- dialog title.  The default is "Select file" or "Select files"
            (if 'multi') or "Save file" if 'save' is True.
          patterns -- a sequence of pairs (label, pattern) for file filtering
            as described in 'pytis.remote.ClientUIBackend.select_file()'.
          pattern -- a string defining the required file name pattern, or 'None'
          save -- iff True, the file is to be open for saving (overwrite warning
            is displayed when selecting an existing file).
          multi -- iff true, allow selecting multiple files

        """
        assert filename is None or isinstance(filename, basestring), filename
        assert directory is None or isinstance(directory, basestring), directory
        assert title is None or isinstance(title, basestring), title
        assert isinstance(patterns, (tuple, list)), patterns
        assert pattern is None or isinstance(pattern, basestring), pattern
        assert isinstance(multi, bool), multi
        assert isinstance(save, bool), save
        return self._client().select_file(filename=filename, directory=directory, title=title,
                                          patterns=patterns, pattern=pattern,
                                          save=save, multi=multi)


class PasswordAuthenticator(object):

    _MAX_CHALLENGES = 10000

    def __init__(self, password=None, ssh_tunnel_dead=None):
        if password is None:
            password = hashlib.sha256(os.urandom(16)).hexdigest()
        self._password = password
        self._challenges = set()
        self._lock = threading.Lock()
        self._ssh_tunnel_dead = ssh_tunnel_dead

    def __call__(self, sock):
        n = len(self._password)
        challenge = sock.recv(n)
        hash = sock.recv(n)
        if challenge in self._challenges or hash != self.password_hash(challenge):
            raise rpyc.utils.authenticators.AuthenticationError("Invalid password")
        if len(self._challenges) >= self._MAX_CHALLENGES:
            raise rpyc.utils.authenticators.AuthenticationError("Too many connection attempts")
        if self._ssh_tunnel_dead is not None and self._ssh_tunnel_dead.ready():
            raise rpyc.utils.authenticators.AuthenticationError("Unknown connection source")
        lock = self._lock
        lock.acquire()
        try:
            self._challenges.add(challenge)
        finally:
            lock.release()
        return sock, None

    def password(self):
        return self._password

    def challenge(self):
        r = random.SystemRandom()
        return bytes(bytearray([r.choice(b'0123456789abcdef') for i in range(len(self._password))]))

    def password_hash(self, challenge):
        if not isinstance(challenge, str):
            # Convert to str in python 3, leave alone in Python 2.
            challenge = str(challenge)
        token = bytes(bytearray([ord(x) ^ ord(y) for x, y in zip(self._password, challenge)]))
        return hashlib.sha256(token).hexdigest().encode('ascii')

    def connect(self, host, port):
        challenge = self.challenge()
        connection = rpyc.connect(host, port)
        if hasattr(socket, 'fromfd'):
            fd = connection.fileno()
            sock = socket.fromfd(fd, socket.AF_INET, socket.SOCK_STREAM)
        else:
            # There is no socket.fromfd in Python 2.x on Windows, so let's use
            # the original hidden socket.
            sock = connection._channel.stream.sock
        sock.send(challenge + self.password_hash(challenge))
        server_challenge = self.challenge()
        server_hash = connection.root.authenticate_server(server_challenge)
        if server_hash != self.password_hash(server_challenge):
            raise rpyc.utils.authenticators.AuthenticationError("Invalid server authentication")
        return connection
