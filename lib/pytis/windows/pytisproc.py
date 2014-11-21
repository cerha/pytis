# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012, 2013, 2014 Brailcom, o.p.s.
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

# ATTENTION: This should be updated on each code change.
_VERSION = '2014-11-21 16:19'

import hashlib
import os
import random
import socket
import string
import sys
import tempfile
import time

import rpyc

class PytisService(rpyc.Service):

    registration = None

    def exposed_authenticate_server(self, challenge):
        """Return password hash based on 'challenge'.

        This method serves for the purpose of the server authentication.

        Arguments:

          challenge -- string to use as the challenge

        """
        return self._authenticator.password_hash(challenge)

    def exposed_echo(self, text):
        """Return 'text'.

        This is useful for checking whether the service is working.
        
        """
        return text

    def exposed_version(self):
        """Return server version."""
        version = 'library: %s' % (_VERSION,)
        if self.registration is not None:
            version = 'service: %s; %s' % (self.registration.version, version,)
        return version

    def exposed_run_python(self, script):
        """Run Python on given script.

        Arguments:

          script -- source text of the Python script to run

        There is no way to get any output from the script here.  If you need to
        get some information from the script then you must handle it in the
        script itself, e.g. by storing it to some file.

        """
        assert isinstance(script, basestring), script
        def get_pythonw_interpreter():
            osfile = os.__file__
            libpath = os.path.split(osfile)[0]
            pythonw = os.path.join(os.path.split(libpath)[0], 'pythonw.exe')
            if not os.path.exists(pythonw):
                pythonw = 'pythonw.exe'
            return pythonw
        pythonw = get_pythonw_interpreter()
        tmpdir = tempfile.mkdtemp(prefix='pytisexec')
        try:
            python_file = os.path.join(tmpdir, 'script.py')
            try:
                open(python_file, 'wb').write(script)
                import win32api
                win32api.WinExec('%s %s' % (pythonw, python_file,))
            finally:
                os.remove(python_file)
        finally:
            try:
                os.rmdir(tmpdir)
            except:
                pass

class PytisUserService(PytisService):

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
        import win32clipboard
        win32clipboard.OpenClipboard()
        try:
            data = win32clipboard.GetClipboardData(win32clipboard.CF_UNICODETEXT)
        except:                   # may happen when there is no clipboard data
            data = None
        win32clipboard.CloseClipboard()
        return data
        
    def exposed_set_clipboard_text(self, text):
        """Set clipboard content to text.

        Arguments:

          text -- text to store into the clipboard; unicode
          
        """
        assert isinstance(text, unicode), text
        import win32clipboard
        win32clipboard.OpenClipboard()
        win32clipboard.EmptyClipboard()
        win32clipboard.SetClipboardData(win32clipboard.CF_UNICODETEXT, text)
        win32clipboard.CloseClipboard()

    def exposed_launch_file(self, path):
        """Start associated application on path.

        Arguments:

          path -- path to the file to be started with its associated
            application; basestring.  Note that path must be given in the
            Windows form, i.e. use backslashes to separate path elements.

        """
        assert isinstance(path, basestring), path
        os.startfile(path)

    def _open_file(self, filename, encoding, mode):
        class Wrapper(object):
            def __init__(self, filename, encoding, mode):
                self._f = open(filename, mode)
                self._filename = filename
                self._encoding = encoding
            def exposed_read(self):
                return self._f.read()
            def exposed_write(self, data):
                if isinstance(data, buffer):
                    data = data[:]
                elif self._encoding is not None:
                    data = data.encode(self._encoding)
                self._f.write(data)
            def exposed_close(self):
                self._f.close()
            def exposed_name(self):
                return self._filename
        return Wrapper(filename, encoding, mode)

    def exposed_open_file(self, filename, mode, encoding=None):
        """Return a read-only 'file' like object of the given file.

        Arguments:

          filename -- name of the file to open, basestring
          mode -- mode for opening the file
          encoding -- file content output encoding, string or None

        """
        return self._open_file(filename, encoding, mode)

    def exposed_open_selected_file(self, template=None):
        """Return a read-only 'file' like object of a user selected file.

        The file is selected by the user using a GUI dialog.  If the user
        cancels the dialog, 'None' is returned.

        Arguments:

          template -- a string defining the required file name pattern, or 'None'

        """
        assert template is None or isinstance(template, basestring), template
        import win32ui
        import win32con
        extension = None
        if template:
            file_filter = (u"Soubory požadovaného typu (%s)|%s|Všechny soubory (*.*)|*.*||" %
                           (template, template,))
            filename = template
            if template.find('.') != -1:
                extension = template.split('.')[-1]
        else:
            file_filter = u"Všechny soubory (*.*)|*.*||"
            filename = "*.*"
        parent = win32ui.FindWindow(None, None)
        dialog = win32ui.CreateFileDialog(1, extension, "%s" % filename,
                                          win32con.OFN_HIDEREADONLY | win32con.OFN_OVERWRITEPROMPT,
                                          file_filter, parent)
        result = dialog.DoModal()
        if result != 1:
            return None
        filename = unicode(dialog.GetPathName(), sys.getfilesystemencoding())
        if filename is None:
            return None
        class Wrapper(object):
            def __init__(self, filename):
                self._f = open(filename, 'rb')
                self._filename = filename
            def exposed_read(self):
                return self._f.read()
            def exposed_close(self):
                self._f.close()
            def exposed_name(self):
                return self._filename
        return Wrapper(filename)

    def exposed_make_selected_file(self, directory=None, filename=None, template=None,
                                   encoding=None, mode='wb'):
        """Return a write-only 'file' like object of a user selected file.

        The file is selected by the user using a GUI dialog.  If the user
        cancels the dialog, 'None' is returned.

        Arguments:

          directory -- initial directory for the dialog
          filename -- default filename or None
          template -- a string defining the required file name pattern, or 'None'
          encoding -- output encoding, string or None
          mode -- default mode for opening the file

        """
        assert template is None or isinstance(template, basestring), template
        import win32ui
        import win32con
        file_filter = u"Všechny soubory (*.*)|*.*||"
        extension = None
        if filename:
            name, ext = os.path.splitext(filename)
            if ext:
                template = "*" + ext
                file_filter = (u"Soubory požadovaného typu (%s)|%s|%s" %
                               (template, template, file_filter))
                extension = ext[1:]
        else:
            filename = "*.*"
            if template:
                file_filter = (u"Soubory požadovaného typu (%s)|%s|%s" %
                               (template, template, file_filter))
        # This hack with finding non-specified windows is used so that
        # we get some parent window for CreateFileDialog.
        # Without this parent windows the method DoModal doesn't show
        # the dialog window on top...
        parent = win32ui.FindWindow(None, None)
        dialog = win32ui.CreateFileDialog(0, extension, "%s" % filename,
                                          win32con.OFN_HIDEREADONLY | win32con.OFN_OVERWRITEPROMPT,
                                          file_filter, parent)
        if directory:
            dialog.SetOFNInitialDir(directory)
        result = dialog.DoModal()
        if result != 1:
            return None
        filename = dialog.GetPathName()
        if filename is None:
            return None
        filename = unicode(filename, sys.getfilesystemencoding())
        return self._open_file(filename, encoding, mode)

    def exposed_make_temporary_file(self, suffix='', encoding=None, mode='wb'):
        """Create a temporary file and return its instance.

        The return value is a 'tempfile.NamedTemporaryFile' instance.

        Arguments:

          suffix -- suffix to use in the temporary file name; if a dot should
            be part of the suffix then it must be explicitly included in it
          encoding -- output encoding, string or None
          mode -- default mode for opening the file
        
        """
        class Wrapper(object):
            def __init__(self, handle, filename, encoding, mode):
                self._f = os.fdopen(handle, mode)
                self._filename = filename
                self._encoding = encoding
            def exposed_write(self, data):
                if isinstance(data, buffer):
                    data = data[:]
                elif self._encoding is not None:
                    data = data.encode(self._encoding)
                self._f.write(data)
            def exposed_close(self):
                self._f.close()
            def exposed_name(self):
                return self._filename
        handle, filename = tempfile.mkstemp(prefix='pytis', suffix=suffix)
        return Wrapper(handle, filename, encoding, mode)

    def exposed_select_directory(self):
        import win32gui
        from win32com.shell import shell, shellcon
        # Get PIDL of the topmost folder for the dialog
        desktop_pidl = shell.SHGetFolderLocation(0, shellcon.CSIDL_DESKTOP, 0, 0)
        pidl, display_name, image_list = shell.SHBrowseForFolder(
            win32gui.GetDesktopWindow(), desktop_pidl, u"Výběr adresáře", 0, None, None)
        # Transform PIDL back to a directory name and return it
        return shell.SHGetPathFromIDList(pidl)

    def exposed_select_file(self, filename=None, template=None, multi=False):
        """Return a list of filenames selected by user in GUI dialog.

        The filenames are selected by the user using a GUI dialog.  If the user
        cancels the dialog, empty list is returned.

        Arguments:

          filename -- default filename or None
          template -- a string defining the required file name pattern, or 'None'
          multi -- iff true, allow selecting multiple files

        """
        assert template is None or isinstance(template, basestring), template
        assert filename is None or isinstance(filename, basestring), filename
        assert isinstance(multi, bool), multi
        import Tkinter
        import tkFileDialog
        root = Tkinter.Tk()
        root.withdraw()
        file_filter = []
        extension = None
        if filename:
            name, ext = os.path.splitext(filename)
            if ext:
                template = "*" + ext
                file_filter.append((u"Soubory požadovaného typu (%s)" % template, template))
                extension = ext[1:]
        else:
            filename = None
            if template:
                file_filter.append((u"Soubory požadovaného typu (%s)" % template, template))
        if len(file_filter) == 0:
            file_filter.append((u"Všechny soubory (*.*)", "*.*"))
        result = tkFileDialog.askopenfilename(parent=root, initialfile=filename,
                                              defaultextension=extension, multi=multi)
        filenames = root.tk.splitlist(result)
        root = None
        return [unicode(f, sys.getfilesystemencoding()) for f in filenames]

class PytisAdminService(PytisService):

    def _true_authentication(self):
        # Dirty: We access private attribute of self._conn.  But how to access
        # credentials otherwise?  And RPyC's SlaveService accesses the
        # attribute too.
        return self._conn._config.get('credentials')

    def exposed_restart(self, delay=1):
        """Restart the admin service after given timeout.

        Arguments:

          delay -- number of minutes to wait before the server gets restarted

        """
        import pythoncom
        from win32com.taskscheduler import taskscheduler
        scheduler = pythoncom.CoCreateInstance(taskscheduler.CLSID_CTaskScheduler,
                                               None,
                                               pythoncom.CLSCTX_INPROC_SERVER,
                                               taskscheduler.IID_ITaskScheduler)
        def add_task(name, command, run_time):
            if '%s.job' % (name,) in scheduler.Enum():
                scheduler.Delete(name)
            task = scheduler.NewWorkItem(name)
            cmd = command.split()
            task.SetApplicationName(cmd[0])
            task.SetParameters(' '.join(cmd[1:]))
            task.SetPriority(taskscheduler.REALTIME_PRIORITY_CLASS)
            task.SetFlags(taskscheduler.TASK_FLAG_DELETE_WHEN_DONE |
                          taskscheduler.TASK_FLAG_RUN_ONLY_IF_LOGGED_ON)
            task.SetAccountInformation('', None)
            scheduler.AddWorkItem(name, task)
            __init__, task_trigger = task.CreateTrigger()
            trigger = task_trigger.GetTrigger()
            trigger.Flags = 0
            time_tupple = time.localtime(run_time)
            trigger.BeginYear = time_tupple[0]
            trigger.BeginMonth = time_tupple[1]
            trigger.BeginDay = time_tupple[2]
            trigger.StartHour = time_tupple[3]
            trigger.StartMinute = time_tupple[4]
            trigger.TriggerType = int(taskscheduler.TASK_TIME_TRIGGER_ONCE)
            task_trigger.SetTrigger(trigger)
            persist_file = task.QueryInterface(pythoncom.IID_IPersistFile)
            persist_file.Save(None, 1)
            task = scheduler.Activate(name)
        now = time.time()
        add_task('pytis_service_stop', 'sc stop PytisService', now + delay * 60)
        add_task('pytis_service_start', 'sc start PytisService', now + (delay + 1) * 60)
        
    def exposed_register_user(self, user, port):
        assert isinstance(user, basestring), user
        assert isinstance(port, int), port
        self.registration.registered_users[user] = port

    def exposed_user_port(self, user):
        assert isinstance(user, basestring), user
        if user == '-':
            port = None
            for p in self.registration.registered_users.values():
                if port is None or port > p:
                    port = p
        else:
            port = self.registration.registered_users.get(user)
        return port

    def exposed_upgrade(self, source):
        assert source and isinstance(source, basestring), source
        if not self._true_authentication():
            raise Exception("Permission denied")
        file_name = __file__
        file_name = os.path.join(os.path.dirname(file_name), 'windows_server.py')
        self._upgrade(file_name, source)
        file_name = os.path.join(os.path.dirname(file_name), 'windows_user_server.py')
        self._upgrade(file_name, source)

    def exposed_upgrade_module(self, source):
        assert source and isinstance(source, basestring), source
        if not self._true_authentication():
            raise Exception("Permission denied")
        file_name = __file__
        file_name = os.path.join(os.path.dirname(file_name), 'pytisproc.py')
        self._upgrade(file_name, source)

    def _upgrade(self, file_name, source):
        new_file_name = file_name + '.new'
        f = open(new_file_name, 'wb')
        f.write(source)
        f.close()
        for suffix in ('c', 'o',):
            try:
                os.remove(file_name + suffix)
            except OSError:
                pass
        os.remove(file_name)
        os.rename(new_file_name, file_name)

class PasswordAuthenticator(object):

    def __init__(self, password=None):
        if password is None:
            password = hashlib.sha256(os.urandom(16)).hexdigest()
        self._password = password
    
    def __call__(self, sock):
        n = len(self._password)
        challenge = sock.recv(n)
        hash = sock.recv(n)
        if hash != self.password_hash(challenge):
            raise rpyc.utils.authenticators.AuthenticationError("Invalid password")
        return sock, None

    def password(self):
        return self._password

    def challenge(self):
        chars = '0123456789abcdef'
        r = random.SystemRandom()
        return string.join([r.choice(chars) for i in range(len(self._password))], '')

    def password_hash(self, challenge):
        token = string.join([chr(ord(x) ^ ord(y)) for x, y in zip(self._password, challenge)], '')
        return hashlib.sha256(token).hexdigest()

    def connect(self, host, port):
        challenge = self.challenge()
        hash_ = self.password_hash(challenge)
        c = rpyc.connect(host, port)
        fd = c.fileno()
        s = socket.fromfd(fd, socket.AF_INET, socket.SOCK_STREAM)
        s.send(challenge + hash_)
        server_challenge = self.challenge()
        server_hash = c.root.authenticate_server(server_challenge)
        if server_hash != self.password_hash(server_challenge):
            raise rpyc.utils.authenticators.AuthenticationError("Invalid server authentication")
        return c

# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
