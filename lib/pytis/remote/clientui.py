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

    """

    def __new__(cls, *args, **kwargs):
        if cls is ClientUIBackend:
            backends = (
                WxUIBackend,
                TkUIBackend,
                ZenityUIBackend,
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
        return self._get_clipboard_text()

    def _get_clipboard_text(self, directory):
        raise NotImplementedError('%s._get_clipboard_text()' % self.__class__.__name__)

    def set_clipboard_text(self, text):
        """Store given text in system clipboard on user's machine.

        Arguments:

          text -- the text to be stored in the clipboard.

        """
        assert text is None or isinstance(text, basestring), text
        return self._set_clipboard_text(text)

    def _set_clipboard_text(self, text):
        raise NotImplementedError('%s._set_clipboard_text()' % self.__class__.__name__)


class WxUIBackend(ClientUIBackend):

    def __init__(self):
        try:
            import wx
        except ImportError:
            raise BackendNotAvailable("Python module 'wx' not installed.")

    def _in_wx_app(method):
        def run(*args, **kwargs):
            import wx
            class App(wx.App):
                def OnInit(self):
                    self.result = method(*args, **kwargs)
                    return True
            return App(False).result
        return run

    @_in_wx_app
    def _select_file(self, title, directory, filename, filters, extension, save, multi):
        import wx
        style = 0
        if save:
            style |= wx.SAVE | wx.OVERWRITE_PROMPT
        else:
            style |= wx.OPEN
        if multi:
            style |= wx.MULTIPLE
        dialog = wx.FileDialog(None, defaultDir=directory or '', defaultFile=filename or '',
                               wildcard='|'.join(["%s|%s" % item for item in filters]),
                               style=style)
        result = dialog.ShowModal()
        if result != wx.ID_OK:
            return None
        elif multi:
            result = dialog.GetPaths()
        else:
            result = dialog.GetPath()
        dialog.Destroy()
        return result

    @_in_wx_app
    def _select_directory(self, title, directory):
        import wx
        dialog = wx.DirDialog(None, defaultPath=directory or '', style=wx.DD_DEFAULT_STYLE)
        result = dialog.ShowModal()
        path = dialog.GetPath()
        dialog.Destroy()
        if result == wx.ID_OK:
            return path
        else:
            return None

    @_in_wx_app
    def _get_clipboard_text(self):
        import wx
        if wx.TheClipboard.Open():
            data = wx.TextDataObject()
            success = wx.TheClipboard.GetData(data)
            wx.TheClipboard.Close()
            if success:
                return data.GetText()

    @_in_wx_app
    def _set_clipboard_text(self, text):
        import wx
        if wx.TheClipboard.Open():
            data = wx.TextDataObject()
            data.SetText(text)
            wx.TheClipboard.SetData(data)
            wx.TheClipboard.Close()


class Win32UIBackend(ClientUIBackend):

    def __init__(self):
        try:
            import win32ui
            import win32con
        except ImportError:
            raise BackendNotAvailable("Python module 'win32ui' not available.")

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
                win32gui.SendMessage(hwnd, shellcon.BFFM_SETSELECTION, 1, directory);
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
        except: # may happen when there is no clipboard data
            data = None
        win32clipboard.CloseClipboard()
        return data

    def _set_clipboard_text(self, text):
        import win32clipboard
        win32clipboard.OpenClipboard()
        win32clipboard.EmptyClipboard()
        win32clipboard.SetClipboardData(win32clipboard.CF_UNICODETEXT, text)
        win32clipboard.CloseClipboard()


class TkUIBackend(ClientUIBackend):

    def __init__(self):
        try:
            import Tkinter
        except ImportError:
            raise BackendNotAvailable("Python module 'Tkinter' not installed.")

    def _select_file(self, title, directory, filename, filters, extension, save, multi):
        import Tkinter
        import tkFileDialog
        root = Tkinter.Tk()
        root.withdraw()
        result = tkFileDialog.askopenfilename(parent=root, initialdir=directory,
                                              initialfile=filename,
                                              defaultextension=extension, multiple=multi)
        filenames = root.tk.splitlist(result)
        root = None
        if multi:
            return filenames
        else:
            return result


class PyZenityUIBackend(ClientUIBackend):

    def __init__(self):
        try:
            import PyZenity
        except ImportError:
            raise BackendNotAvailable("Python module 'PyZenity' not installed.")

    def _select_directory(self, title, directory):
        import PyZenity
        directory_list = PyZenity.GetDirectory(title=title, selected=directory)
        if directory_list and len(directory_list) > 0:
            return directory_list[0]
        else:
            return None

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


class ZenityUIBackend(ClientUIBackend):

    def __init__(self):
        if self._run_zenity('--version') is None:
            raise BackendNotAvailable("Zenity command line interface not installed.")

    @classmethod
    def _run_zenity(cls, *args):
        import subprocess
        try:
            output = subprocess.check_output(('zenity',) + args)
        except subprocess.CalledProcessError:
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
        if '|' in answer:
            # This is a bug in version 3.8.0.
            answer = answer.split('|')[0]
        return answer

    def _select_file(self, title, directory, filename, filters, extension, save, multi):
        args = []
        if directory is not None:
            filename = os.path.join(directory, filename or '')
        if filename is not None:
            args.extend(('--filename', filename,))
        for title, pattern in filters:
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
