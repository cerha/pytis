# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import unicode_literals
from __future__ import print_function

from past.builtins import basestring
from builtins import range

import getpass
import hashlib
import os
import random
import re
import rpyc
import socket
import subprocess
import sys
import time

import pytis
from pytis.api import app
from pytis.util import OPERATIONAL, log, translations

_ = translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.

_ipv4_regexp = r'[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+'
_ipv6_regexp = r'.*:.*:.*'
_ip_matcher = re.compile('%s|%s' % (_ipv4_regexp, _ipv6_regexp,))
_x2go_ip = None

_remove_x2go_info_file = True
_announce_obsolete_client_version = True


class RPCInfo(object):
    """Container for RPC communication data."""
    connection = None
    connection_order = 0
    access_data = None
    access_data_version = 0
    remote_client_version = None


class Connector(object):
    """Pytis RPyC service connector

    This is the client part of the authentication protocol implemented by
    'p2go.PasswordAuthenticator'.  This class is actually a partial duplication
    of 'p2go.PasswordAuthenticator' because the server side also implements the
    method 'connect()' for P2Go's internal purposes.  The duplication is
    necessary in order to avoid Pytis runtime to have dependencies on P2go and
    vice versa.

    """
    def __init__(self, password):
        self._password = password

    def _challenge(self):
        r = random.SystemRandom()
        return bytes(bytearray([r.choice(b'0123456789abcdef') for i in range(len(self._password))]))

    def _password_hash(self, challenge):
        if not isinstance(challenge, str):
            # Convert to str in python 3, leave alone in Python 2.
            challenge = str(challenge)
        token = bytes(bytearray([ord(x) ^ ord(y) for x, y in zip(self._password, challenge)]))
        return hashlib.sha256(token).hexdigest().encode('ascii')

    def connect(self, host, port):
        client_challenge = self._challenge()
        connection = rpyc.connect(host, port)
        if hasattr(socket, 'fromfd'):
            fd = connection.fileno()
            sock = socket.fromfd(fd, socket.AF_INET, socket.SOCK_STREAM)
        else:
            # There is no socket.fromfd in Python 2.x on Windows, so let's use
            # the original hidden socket.
            sock = connection._channel.stream.sock
        sock.send(client_challenge + self._password_hash(client_challenge))
        server_challenge = self._challenge()
        server_hash = connection.root.authenticate_server(server_challenge)
        if server_hash != self._password_hash(server_challenge):
            raise rpyc.utils.authenticators.AuthenticationError("Invalid server authentication")
        return connection


def write_python_version():
    """If inside X2Go session, write running Python version to the file for Pytis2Go.

    Pytis2Go needs to know the Python version of the running application in
    order to start the RPyC service with the same Python version (otherwise
    RPyC communication will not work).  We write the version number to a known
    location and Pytis2go will wait until it finds the file, read the
    information and start the RPyC service with that version.

    """
    try:
        session_id = x2go_session_id()
        if session_id:
            filename = pytis_x2go_info_file(session_id) + '.python-version'
            version = sys.version_info[0]
            log(OPERATIONAL, "Writing Python version {} to {}.".format(version, filename))
            with open(filename, 'wt') as f:
                f.write('{}\n'.format(sys.version_info[0]))
    except Exception as e:
        log(OPERATIONAL, "Error writing Python version:", e)


def client_ip():
    """Return IP address of the x2go client as a string.

    If pytis is not run from an x2go client, return 'None'.

    """
    if pytis.config.session_id:
        ip = '127.0.0.1'
    else:
        global _x2go_ip
        if _x2go_ip is None:
            x2go_agent_pid = os.getenv('X2GO_AGENT_PID')
            if x2go_agent_pid is not None:
                kwargs = dict(encoding='ascii') if sys.version_info[0] > 2 else {}
                p = subprocess.Popen('x2golistsessions', stdout=subprocess.PIPE,
                                     shell=True, **kwargs)
                output, __ = p.communicate()
                for line in output.splitlines():
                    items = line.split('|')
                    if items[0] == x2go_agent_pid:
                        maybe_ip = items[7]
                        if _ip_matcher.match(maybe_ip):
                            _x2go_ip = maybe_ip
                            break
        ip = _x2go_ip
    return ip


def client_available():
    """Return True iff running a remote Pytis client session (Pytis2Go).

    The result of client_ip() returns an IP address for any X2Go session, even
    if a remote RPyC connection was not initialized.

    This function returns True if we are inside an X2Go session ('client_ip()'
    returns an IP address) and the remote Pytis RPyC connection is available.

    Use 'client_connection_ok()' to further check if the client connection
    actually works.

    """
    return client_ip() is not None and _rpc_access_data() is not None


def client_connection_ok():
    """Return True, iff remote client connection is active.

    Returns True if running remotely and the remote connection actually works.

    """
    if not client_available():
        return False
    try:
        return _request('echo', 'hello') == 'hello'
    except Exception as e:
        log(OPERATIONAL, "RPC exception:", e)
        return False


def x2go_session_id(fake=False):
    if pytis.config.session_id is not None:
        return pytis.config.session_id
    session_id = os.getenv('X2GO_SESSION')
    if fake:
        session_id += 'ssh'
    return session_id


def x2go_display():
    # TODO: This duplicates _SESSION_COMMAND_MATCHER in x2goclient.py
    session_id = x2go_session_id()
    if session_id:
        match = re.match(r'^[^-]+-(\d+)-\d+_stR.+_dp\d+$', session_id)
        if match:
            return match.group(1)
    return None


def pytis_x2go_info_file(session_id=None):
    if session_id is None:
        session_id = x2go_session_id()
    return os.path.expanduser(os.path.join('~', '.x2go/ssh/pytis.%s' % (session_id,)))


class X2GoInfoException(Exception):
    pass


class X2GoInfoSoftException(X2GoInfoException):
    pass


class X2GoInfoHardException(X2GoInfoException):
    pass


def parse_x2go_info_file(filename):
    try:
        f = open(filename)
    except Exception as e:
        log(OPERATIONAL, "Can't read pytis X2Go file", e)
        return None
    data = ''
    while True:
        d = f.read()
        if not d:
            break
        data += d
    items = data.split(':')
    if len(items) != 4:
        raise X2GoInfoSoftException("Incomplete or invalid X2Go file")
    if items[0] != '0':
        raise X2GoInfoHardException("Unknown pytis X2Go format")
    try:
        port = int(items[1])
    except ValueError:
        raise X2GoInfoHardException("Invalid port number in X2Go file", items[1])
    return dict(port=port, password=items[2])


def keep_x2go_info_file():
    """Prevent removing the info file after it is read.

    The info file (containing the connection port and password) is normally
    removed after it is read succesfully because it contains sensitive data and
    is normally not neeeded any more.  This, however, makes it impossible to
    run pytis twice within one pytis2go session.  This may be a problem in
    tests, which we want to be able to run multiple times within one session.
    Call this function before any other method in this module in order to
    prevent info file removal.

    """
    global _remove_x2go_info_file
    _remove_x2go_info_file = False


def _read_x2go_info_file():
    pytis_x2go_file = pytis_x2go_info_file()
    if os.path.exists(pytis_x2go_file):
        for i in range(3):
            try:
                access_data = parse_x2go_info_file(pytis_x2go_file)
            except X2GoInfoSoftException as e:
                log(OPERATIONAL, *e.args)
                time.sleep(1)
                continue
            except X2GoInfoHardException as e:
                log(OPERATIONAL, *e.args)
                return None
            else:
                global _remove_x2go_info_file
                if _remove_x2go_info_file:
                    os.remove(pytis_x2go_file)
                return access_data
    return None

def _rpc_access_data():
    access_data = _read_x2go_info_file() or RPCInfo.access_data
    if not access_data:
        return None
    if access_data != RPCInfo.access_data:
        RPCInfo.access_data = access_data
        RPCInfo.access_data_version += 1
    return access_data

def _connect():
    access_data = _rpc_access_data()
    connector = Connector(access_data.get('password'))
    RPCInfo.connection = connection = connector.connect('localhost', access_data.get('port'))
    RPCInfo.connection_order += 1
    RPCInfo.remote_client_version = version = connection.root.x2goclient_version()
    RPCInfo.client_api_pushed = False
    log(OPERATIONAL, "Client connection {} ({}) established with version: {}".format(
        RPCInfo.connection_order,
        RPCInfo.access_data_version,
        version
    ))
    try:
        connection.root.extend
    except AttributeError:
        # This is an older client versinon, which doesn't support API push, but
        # contains a fixed API.  Pushing is not necessary in this case as long
        # as we do not rely on newer API features.  Once we get rid of all
        # "fixed API" clients, we can start relying on all API features.
        global _announce_obsolete_client_version
        if _announce_obsolete_client_version:
            _announce_obsolete_client_version = False
            log(OPERATIONAL, "Obsolete client version {}: {} from {}"
                .format(version, getpass.getuser(), client_ip()))
            app.warning(_("You are using an obsolete version of Pytis2Go.\n"
                          "Please, contact IT support to install a newer version.\n"
                          "Your current version will stop working soon."))
    else:
        # We may be just reconnecting to a previously extended service instance.
        if 'PytisClientAPIService' not in connection.root.extensions():
            with open(os.path.join(os.path.dirname(__file__), 'clientapi.py')) as f:
                clientapi = f.read()
            error = connection.root.extend(clientapi, 'PytisClientAPIService')
            if error:
                log(OPERATIONAL, "Client API push failed:", error)
            else:
                log(OPERATIONAL, "Client API pushed successfully.")
                RPCInfo.client_api_pushed = True
    return connection


def _request(request, *args, **kwargs):
    def retype(arg):
        # Convert lcg.TranslatableText instances to unicode before passing
        # to the remote side.  This is because rpyc doesn't create the
        # remote instances with the right encoding, while it does it
        # correctly for pure unicode instances (even though
        # lcg.TranslatableText is a unicode subclass).
        if isinstance(arg, tuple):
            return tuple(retype(a) for a in arg)
        elif isinstance(arg, list):
            return [retype(a) for a in arg]
        elif isinstance(arg, dict):
            return dict((retype(k), retype(v)) for k, v in arg.items())
        elif isinstance(arg, unistr) and type(arg).__name__ == 'TranslatableText':
            return unistr(arg)
        else:
            return arg
    if RPCInfo.connection is None:
        _connect()
    try:
        RPCInfo.connection.root.echo
    except Exception:
        _connect()
    method = getattr(RPCInfo.connection.root, request)
    return method(*retype(args), **retype(kwargs))


def session_password():
    try:
        return _request('session_password')
    except Exception:
        return None


def get_clipboard_text():
    try:
        text = _request('get_clipboard_text')
    except Exception:
        return None
    if text:
        text = text.replace('\r\n', '\n')
    return text


def set_clipboard_text(text):
    assert isinstance(text, unistr), text
    text = text.replace('\n', '\r\n')
    try:
        _request('set_clipboard_text', text)
    except Exception:
        pass


def launch_file(path):
    assert isinstance(path, basestring), path
    try:
        return _request('launch_file', path)
    except Exception as e:
        app.error(_("Unable to open file %(filename)s: %(error)s", filename=path, error=e))


def launch_url(url):
    assert isinstance(url, basestring), url
    try:
        return _request('launch_file', url)
    except Exception:
        app.error(_("Unable to open URL %s", url))


def open_file(filename, mode, encoding=None, encrypt=None, decrypt=False):
    assert isinstance(filename, basestring), filename
    assert isinstance(mode, basestring), mode
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert encrypt is None or isinstance(encrypt, list), encrypt
    assert isinstance(decrypt, bool), decrypt
    try:
        return _request('open_file', filename, mode, encoding=encoding, encrypt=encrypt)
    except Exception as e:
        app.error(_("Unable to open file %(filename)s: %(error)s", filename=filename, error=e))


def open_selected_file(directory=None, patterns=(), pattern=None, filetypes=None, encrypt=None):
    assert directory is None or isinstance(directory, basestring), directory
    assert isinstance(patterns, (tuple, list)), patterns
    assert pattern is None or isinstance(pattern, (basestring, tuple, list)), pattern
    assert encrypt is None or isinstance(encrypt, list), encrypt
    if filetypes:
        # TODO: patterns and pattern will be removed and only filetypes will be supported
        # in future Client API versions, but for now we need to implement filetypes using
        # patterns because there are still clients with hardcoded pytisproc.py instead of
        # server pushed Client API (which only support patterns/pattern).
        assert isinstance(filetypes, (tuple, list)), filetypes
        assert not patterns and not pattern, (filetypes, patterns, pattern)
        patterns = ((_("Files of the required type"), ['*.' + ext for ext in filetypes]),)
    try:
        return _request('open_selected_file', directory=directory,
                        patterns=patterns, pattern=pattern, encrypt=encrypt)
    except Exception as e:
        app.error(_("Unable to select a file for download: %s", e))


def make_selected_file(directory=None, filename=None, patterns=(), pattern=None, filetypes=None,
                       encoding=None, mode='wb', decrypt=False):
    assert directory is None or isinstance(directory, basestring), directory
    assert filename is None or isinstance(filename, basestring), filename
    assert isinstance(patterns, (tuple, list)), patterns
    assert pattern is None or isinstance(pattern, (basestring, tuple, list)), pattern
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert mode is None or isinstance(mode, basestring), mode
    if filetypes:
        # TODO: patterns and pattern will be removed and only filetypes will be supported
        # in future Client API versions, but for now we need to implement filetypes using
        # patterns because there are still clients with hardcoded pytisproc.py instead of
        # server pushed Client API (which only support patterns/pattern).
        assert isinstance(filetypes, (tuple, list)), filetypes
        assert not patterns and not pattern, (filetypes, patterns, pattern)
        patterns = ((_("Files of the required type"), ['*.' + ext for ext in filetypes]),)
    try:
        return _request('make_selected_file', directory=directory, filename=filename,
                        patterns=patterns, pattern=pattern, encoding=encoding,
                        mode=mode, decrypt=decrypt)
    except Exception as e:
        app.error(_("Unable to select a file to save: %s", e))


def make_temporary_file(suffix='', encoding=None, mode='wb', decrypt=False):
    assert isinstance(suffix, basestring), suffix
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert mode is None or isinstance(mode, basestring), mode
    assert isinstance(decrypt, bool), decrypt
    return _request('make_temporary_file', suffix=suffix, encoding=encoding, mode=mode,
                    decrypt=decrypt)


def select_directory(directory=None, title=_("Directory selection")):
    assert directory is None or isinstance(directory, basestring), directory
    # Older clients don't support this argument...
    kwargs = dict(title=title) if RPCInfo.client_api_pushed else dict()
    try:
        return _request('select_directory', directory=directory, **kwargs)
    except Exception as e:
        app.error(_("Failed selecting directory: %s", e))


def select_file(filename=None, directory=None, title=None,
                patterns=(), pattern=None, filetypes=None, save=False, multi=False):
    assert filename is None or isinstance(filename, basestring), filename
    assert directory is None or isinstance(directory, basestring), directory
    assert title is None or isinstance(title, basestring), title
    assert isinstance(patterns, (tuple, list)), patterns
    assert pattern is None or isinstance(pattern, (basestring, tuple, list)), pattern
    assert isinstance(save, bool), save
    assert not save, "Save can not be relied upon as long as there are old clients."
    assert isinstance(multi, bool), multi
    if filetypes:
        # TODO: patterns and pattern will be removed and only filetypes will be supported
        # in future Client API versions, but for now we need to implement filetypes using
        # patterns because there are still clients with hardcoded pytisproc.py instead of
        # server pushed Client API (which only support patterns/pattern).
        assert isinstance(filetypes, (tuple, list)), filetypes
        assert not patterns and not pattern, (filetypes, patterns, pattern)
        patterns = ((_("Files of the required type"), ['*.' + ext for ext in filetypes]),)
    if title is None:
        if save:
            title = _("Save file")
        elif multi:
            title = _("Files selection")
        else:
            title = _("File selection")
    # Older clients don't support these arguments...
    kwargs = dict(title=title, save=save) if RPCInfo.client_api_pushed else dict()
    try:
        return _request('select_file', filename=filename, directory=directory,
                        patterns=patterns, pattern=pattern, multi=multi, **kwargs)
    except Exception as e:
        app.error(_("Failed selecting file: %s", e))


def run_python(script):
    try:
        return _request('run_python', script)
    except Exception:
        return None


def python_version():
    try:
        return _request('python_version')
    except Exception:
        return ()


def backend_info():
    try:
        return _request('backend_info')
    except Exception:
        return None

def _rpyc_version(version):
    # The rpyc.__version__ contents is inconsistent across rpyc versions...
    if isinstance(version, tuple):
        return '.'.join(map(str, version))
    else:
        return version

def rpyc_version():
    try:
        return _rpyc_version(_request('rpyc_version'))
    except Exception:
        return None

def local_rpyc_version():
    return _rpyc_version(rpyc.__version__)
