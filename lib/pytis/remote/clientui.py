# -*- coding: utf-8 -*-

# Copyright (C) 2011-2016 Brailcom, o.p.s.
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

import os
import sys


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
            else:
                backends = (
                    WxUIBackend,
                    ZenityUIBackend,
                    TkUIBackend,
                    Win32UIBackend,
                )
            for subclass in backends:
                try:
                    backend = subclass.__new__(subclass, *args, **kwargs)
                except BackendNotAvailable:
                    continue
                #print "Using %s" % backend
                return backend
            raise Exception(u'No suitable UI backend found.')
        else:
            return object.__new__(cls)

    def _unicode(self, x):
        if isinstance(x, (tuple, list)):
            x = [self._unicode(y) for y in x]
        elif not isinstance(x, unicode) and x is not None:
            x = unicode(x, sys.getfilesystemencoding())
        return x

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

    def select_file(self, title=None, directory=None, filename=None, template=None,
                    save=False, multi=False):
        """Return the file name(s) of user selected file(s).

        The file is selected by the user using a GUI dialog.  If the user
        cancels the dialog, 'None' is returned.

        Arguments:

          title -- dialog title
          directory -- initial directory for the dialog
          filename -- default filename or None
          template -- a string defining the required file name pattern, or 'None'
          save -- True iff the file is to be open for writing
          multi -- iff true, allow selecting multiple files (not possible when save is True)

        """
        assert title is None or isinstance(title, basestring), title
        assert directory is None or isinstance(directory, basestring), directory
        assert filename is None or isinstance(filename, basestring), filename
        assert template is None or isinstance(template, basestring), template
        assert isinstance(save, bool), save
        assert isinstance(multi, bool), multi
        assert not (save and multi), (save, multi)
        if title is None:
            if save:
                title = u"Uložit soubor"
            elif multi:
                title = u"Výběr souborů"
            else:
                title = u"Výběr souboru"
        extension = None
        filters = [(u"Všechny soubory (*.*)", "*.*")]
        if filename:
            name, ext = os.path.splitext(filename)
            if ext:
                template = "*" + ext
                filters.insert(0, (u"Soubory požadovaného typu (%s)" % template, template))
                extension = ext[1:]
        else:
            filename = "*.*"
            if template:
                filters.insert(0, (u"Soubory požadovaného typu (%s)" % template, template))
        return self._unicode(self._select_file(title, directory, filename, filters,
                                               extension, save, multi))

    def _select_file(self, title, directory, filename, filters, extension, save, multi):
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

    def __init__(self):
        try:
            import wx
        except ImportError:
            raise BackendNotAvailable()
        # TODO: This is now commented out because x2goclient.py creates a 'wx.App'
        # already at startup and this application stays running during the whole
        # Pytis run.  Starting another here would cause conflicts and might crash
        # the whole Python process.  Thus we avoid starting a new wx.App here until
        # pytis2go.py is ready to replace x2goclient.py.
        #self._app = wx.App(False)

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

    def _select_file(self, title, directory, filename, filters, extension, save, multi):
        import wx
        style = wx.STAY_ON_TOP
        if save:
            # TODO: Overwrite prompt doesn't seem to appear on Mac OS X.
            style |= wx.SAVE | wx.OVERWRITE_PROMPT
        else:
            style |= wx.OPEN
        if multi:
            style |= wx.MULTIPLE
        dialog = wx.FileDialog(None, style=style, message=title,
                               defaultDir=directory or '', defaultFile=filename or '',
                               wildcard='|'.join(["%s|%s" % item for item in filters]))
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

    def __init__(self):
        import pkgutil
        for m in ('win32ui', 'win32con', 'win32gui', 'win32clipboard', 'pywin.mfc.dialog'):
            if not pkgutil.find_loader(m):
                raise BackendNotAvailable()

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
        template = [
            [title, (0, 0, width + 10, height + 38), win32con.WS_CAPTION | win32con.DS_MODALFRAME,
             None, (8, "MS SansSerif")],
            ["static", "", IDC_TEXT, (5, 4, 150, 14), win32con.WS_CHILD | win32con.WS_VISIBLE],
            ["listbox", "List", IDC_LIST, (5, 16, width, height), win32con.WS_VISIBLE],
            [128, u"Ok", win32con.IDOK, (width - 45, height + 18, 50, 14),
             win32con.BS_PUSHBUTTON | win32con.WS_VISIBLE],
        ]
        dialog = SelectionDialog(template)
        dialog.DoModal()
        if result.selection is not None:
            return data[result.selection][return_column - 1]
        else:
            return None

    def _select_file(self, title, directory, filename, filters, extension, save, multi):
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
        dialog = win32ui.CreateFileDialog(mode, extension, "%s" % filename, flags,
                                          '|'.join(["%s|%s" % item for item in filters]) + '||',
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
        except:  # may happen when there is no clipboard data
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

    def __init__(self):
        import pkgutil
        if not pkgutil.find_loader('pyperclip'):
            raise BackendNotAvailable()

    def _get_clipboard_text(self):
        import pyperclip
        return pyperclip.paste()

    def _set_clipboard_text(self, text):
        import pyperclip
        pyperclip.copy(text)


class TkUIBackend(ClipboardUIBackend):
    """Implements UI backend operations using Tkinter."""

    def __init__(self):
        import pkgutil
        for m in ('Tkinter', 'ttk', 'tkSimpleDialog', 'tkFileDialog'):
            if not pkgutil.find_loader(m):
                raise BackendNotAvailable()
        super(TkUIBackend, self).__init__()

    def _in_tk_app(method):
        def run(self, *args, **kwargs):
            import Tkinter
            root = Tkinter.Tk()
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
        import tkSimpleDialog
        if password:
            kwargs = dict(show='*')
        else:
            kwargs = {}
        return tkSimpleDialog.askstring(title, label, parent=root, **kwargs)

    @_in_tk_app
    def _select_option(self, root, title, label, columns, data, return_column):
        import Tkinter
        import ttk
        rows = ['   '.join(row) for row in data]
        result = self._Object(selection=None)
        dialog = Tkinter.Toplevel(root)
        dialog.title(title)
        tklabel = ttk.Label(dialog, text=label)
        tklabel.pack(padx=5, pady=2, anchor=Tkinter.W)
        listbox = Tkinter.Listbox(dialog, listvariable=Tkinter.StringVar(value=tuple(rows)),
                                  height=len(rows), activestyle='dotbox')
        listbox.pack(expand=True, fill=Tkinter.BOTH, padx=5, pady=5)
        def submit():
            idxs = listbox.curselection()
            if len(idxs) == 1:
                result.selection = int(idxs[0])
            dialog.quit()
        button = ttk.Button(dialog, text=u"Ok", command=submit, default='active')
        button.pack(pady=5, padx=5, side=Tkinter.RIGHT)
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
    def _select_file(self, root, title, directory, filename, filters, extension, save, multi):
        import tkFileDialog
        if save:
            dialog = tkFileDialog.asksaveasfilename
            kwargs = dict()
        else:
            dialog = tkFileDialog.askopenfilename
            kwargs = dict(multiple=multi)
        if sys.platform != 'darwin':
            # The filters don't work on Mac OS X.  If present, all files are grayed out...
            kwargs['filetypes'] = [(label, pattern[1:] if pattern.startswith('*.') else pattern)
                                   for label, pattern in filters]
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
        import tkFileDialog
        return tkFileDialog.askdirectory(title=title, parent=root, initialdir=directory)


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
        except (subprocess.CalledProcessError, WindowsError):
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

    def _select_file(self, title, directory, filename, filters, extension, save, multi):
        args = []
        if directory is not None:
            filename = os.path.join(directory, filename or '')
        if filename is not None:
            args.extend(('--filename', filename,))
        for name, pattern in filters:
            args.extend(('--file-filter', pattern,))
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

    def __init__(self):
        import pkgutil
        if not pkgutil.find_loader('PyZenity'):
            raise BackendNotAvailable()
        super(PyZenityUIBackend, self).__init__()

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

    def _select_file(self, title, directory, filename, filters, extension, save, multi):
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
