# -*- coding: utf-8 -*-

# Copyright (C) 2018, 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2011-2018 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
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

from past.builtins import basestring
from builtins import range

import os
import re
import rpyc
import time
import random
import string
import socket
import hashlib
import getpass
import subprocess
import pytis
from pytis.util import DEBUG, OPERATIONAL, UNDEFINED, log, translations

_ = translations('pytis-wx')


_ipv4_regexp = r'[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+'
_ipv6_regexp = r'.*:.*:.*'
_ip_matcher = re.compile('%s|%s' % (_ipv4_regexp, _ipv6_regexp,))
_nx_ip = UNDEFINED
_x2go_ip = None


class RPCInfo(object):
    """Container for RPC communication data."""
    connection = None
    direct_connection = False
    access_data = None
    remote_client_version = None
    remote_status_info = (False, time.time())
    remote_connection_initially_available = False


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
        return ''.join([r.choice('0123456789abcdef') for i in range(len(self._password))])

    def _password_hash(self, challenge):
        token = ''.join([chr(ord(x) ^ ord(y)) for x, y in zip(self._password, challenge)])
        return hashlib.sha256(token).hexdigest()

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


def nx_ip():
    """Return IP address of the nx client, as a string.

    If pytis is not run from an nx client, return 'None'.

    """
    global _nx_ip
    if _nx_ip is not UNDEFINED:
        return _nx_ip
    _nx_ip = os.getenv('NXUSERIP')
    if _nx_ip is not None:
        return _nx_ip
    nxsessionid = os.getenv('NXSESSIONID')
    if nxsessionid is not None:
        session_id = ':' + nxsessionid.split('-')[1]
        p = subprocess.Popen('who', stdout=subprocess.PIPE, shell=True)
        output, __ = p.communicate()
        for line in output.splitlines():
            items = line.split()
            if items[1] == session_id:
                maybe_ip = items[4][1:-1]
                if _ip_matcher.match(maybe_ip):
                    _nx_ip = maybe_ip
                    break
    return _nx_ip


def x2go_ip():
    """Return IP address of the x2go client, as a string.

    If pytis is not run from an x2go client, return 'None'.

    """
    global _x2go_ip
    if _x2go_ip is not None:
        return _x2go_ip
    _x2go_agent_pid = os.getenv('X2GO_AGENT_PID')
    if _x2go_agent_pid is not None:
        p = subprocess.Popen('x2golistsessions', stdout=subprocess.PIPE, shell=True)
        output, __ = p.communicate()
        for line in output.splitlines():
            items = line.split('|')
            if items[0] == _x2go_agent_pid:
                maybe_ip = items[7]
                if _ip_matcher.match(maybe_ip):
                    _x2go_ip = maybe_ip
                    break
    return _x2go_ip


def client_ip():
    """Return IP address of the x2go client or nx client, as a string.

    If pytis is not run from an x2go or nx client, return 'None'.

    """
    if pytis.config.session_id:
        ip = '127.0.0.1'
    else:
        ip = x2go_ip() or nx_ip()
    return ip


def client_available():
    """Return true, iff remote client is available."""
    if not pytis.config.rpc_communication_enabled or client_ip() is None:
        level = OPERATIONAL if pytis.config.rpc_communication_enabled else DEBUG
        log(level, "RPC unavailable")
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
    access_data = {}
    try:
        access_data['port'] = int(items[1])
    except ValueError:
        raise X2GoInfoHardException("Invalid port number in X2Go file", items[1])
    access_data['password'] = items[2]
    return access_data


def read_x2go_info_file(rename=False, use_defaults=True):
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
            if rename:
                try:
                    os.rename(pytis_x2go_file, pytis_x2go_info_file(x2go_session_id(fake=True)))
                except Exception as e:
                    return
            else:
                os.remove(pytis_x2go_file)
            break
    elif use_defaults:
        access_data = dict(port=pytis.config.rpc_local_port, password=None)
    else:
        access_data = None
    return access_data


def _connect():
    access_data = read_x2go_info_file()
    rpc_info = RPCInfo
    if access_data is None:
        access_data = rpc_info.access_data
    else:
        rpc_info.access_data = access_data
    if access_data is None:
        return None
    port = access_data.get('port')
    password = access_data.get('password')
    if password is None:
        rpc_info.direct_connection = False
        import rpyc
        connection = rpyc.connect('localhost', port)
    else:
        rpc_info.direct_connection = True
        connector = Connector(password)
        connection = connector.connect('localhost', port)
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
        elif isinstance(arg, unicode) and type(arg).__name__ == 'TranslatableText':
            return unicode(arg)
        else:
            return arg
    if RPCInfo.connection is None:
        # Make sure direct_connection is initialized before first use
        RPCInfo.connection = _connect()
    if RPCInfo.direct_connection:
        try:
            RPCInfo.connection.root.echo
        except Exception:
            RPCInfo.connection = _connect()
        r = getattr(RPCInfo.connection.root, request)
    else:
        target_ip = client_ip()
        user_name = getpass.getuser()
        try:
            r = RPCInfo.connection.root.request
        except Exception:
            RPCInfo.connection = _connect()
            r = RPCInfo.connection.root.request
        args = (target_ip, user_name, request,) + args
    return r(*retype(args), **retype(kwargs))


def version():
    try:
        version = _request('version')
    except Exception:
        try:
            if _request('echo', 'hello') == 'hello':
                version = '-old-'
            else:
                version = ''
        except Exception:
            version = ''
    return version or ''


def library_version():
    version_string = version()
    if not version_string or version_string == '-old-':
        return None
    else:
        try:
            versions = dict([x.strip() for x in v.split(':', 1)]
                            for v in version_string.split(';'))
            return versions['library']
        except Exception:
            return None


def session_password():
    version = library_version()
    if version and version >= '2018-06-27 16:00':
        # We try to be safer by not even trying to call the method for older
        # P2Go versions.  The try/except block would handle it here
        # anyway, but in some more complicated cases testing the version
        # might be necessary.
        try:
            return _request('session_password')
        except Exception:
            return None
    else:
        return None


def x2goclient_version():
    try:
        version = _request('x2goclient_version')
    except Exception:
        try:
            if _request('echo', 'hello') == 'hello':
                version = '-old-'
            else:
                version = ''
        except Exception:
            version = ''
    return version or ''


def get_clipboard_text():
    try:
        text = _request('get_clipboard_text')
    except Exception:
        return None
    if text:
        text = text.replace('\r\n', '\n')
    return text


def set_clipboard_text(text):
    assert isinstance(text, unicode), text
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
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Unable to open file %(filename)s: %(error)s",
                                                  filename=path, error=e))


def launch_url(url):
    assert isinstance(url, basestring), url
    try:
        return _request('launch_file', url)
    except Exception:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Unable to open URL %s", url))


def open_file(filename, mode, encoding=None, encrypt=None, decrypt=False):
    assert isinstance(filename, basestring), filename
    assert isinstance(mode, str), mode
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert encrypt is None or isinstance(encrypt, list), encrypt
    assert isinstance(decrypt, bool), decrypt
    try:
        return _request('open_file', filename, mode, encoding=encoding, encrypt=encrypt)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Unable to open file %(filename)s: %(error)s",
                                                  filename=filename, error=e))


def open_selected_file(directory=None, patterns=(), pattern=None, template=None, encrypt=None):
    assert directory is None or isinstance(directory, basestring), directory
    assert isinstance(patterns, (tuple, list)), patterns
    assert pattern is None or isinstance(pattern, (basestring, tuple, list)), pattern
    assert encrypt is None or isinstance(encrypt, list), encrypt
    if template:
        # TODO: template is just for backwards compatibility. REMOVE THIS
        assert isinstance(template, basestring), template
        assert not pattern, (template, pattern)
        pattern = template
    try:
        return _request('open_selected_file', directory=directory,
                        patterns=patterns, pattern=pattern, encrypt=encrypt)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Unable to select a file for download: %s", e))


def make_selected_file(directory=None, filename=None, patterns=(), pattern=None, template=None,
                       encoding=None, mode='wb', decrypt=False):
    assert directory is None or isinstance(directory, basestring), directory
    assert filename is None or isinstance(filename, basestring), filename
    assert isinstance(patterns, (tuple, list)), patterns
    assert pattern is None or isinstance(pattern, (basestring, tuple, list)), pattern
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert mode is None or isinstance(mode, basestring), mode
    if template:
        # TODO: template is just for backwards compatibility. REMOVE THIS
        assert isinstance(template, basestring), template
        assert not pattern, (template, pattern)
        pattern = template
    try:
        return _request('make_selected_file', directory=directory, filename=filename,
                        patterns=patterns, pattern=pattern, encoding=encoding,
                        mode=mode, decrypt=decrypt)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Unable to select a file to save: %s", e))


def make_temporary_file(suffix='', encoding=None, mode='wb', decrypt=False):
    assert isinstance(suffix, basestring), suffix
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert mode is None or isinstance(mode, basestring), mode
    assert isinstance(decrypt, bool), decrypt
    return _request('make_temporary_file', suffix=suffix, encoding=encoding, mode=mode,
                    decrypt=decrypt)


def select_directory(directory=None):
    assert directory is None or isinstance(directory, (str, unicode)), directory
    try:
        return _request('select_directory', directory=directory)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Failed selecting directory: %s", e))


def select_file(filename=None, directory=None, patterns=(), pattern=None, template=None,
                multi=False):
    assert filename is None or isinstance(filename, basestring), filename
    assert directory is None or isinstance(directory, (str, unicode)), directory
    assert isinstance(patterns, (tuple, list)), patterns
    assert pattern is None or isinstance(pattern, (basestring, tuple, list)), pattern
    assert isinstance(multi, bool), multi
    if template:
        # TODO: template is just for backwards compatibility. REMOVE THIS
        assert isinstance(template, basestring), template
        assert not pattern, (template, pattern)
        pattern = template
    try:
        return _request('select_file', filename=filename, directory=directory,
                        patterns=patterns, pattern=pattern, multi=multi)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Failed selecting file: %s", e))


def run_python(script):
    try:
        return _request('run_python', script)
    except Exception:
        return None
