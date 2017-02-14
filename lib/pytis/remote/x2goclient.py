#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012, 2014, 2015, 2016, 2017 Brailcom, o.p.s.
# Copyright (C) 2010-2014 by Mike Gabriel <mike.gabriel@das-netzwerkteam.de>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# ATTENTION: This should be updated on each code change.
_VERSION = '2017-02-09 22:59'

import signal

import os
import gettext
import platform
import sys

import copy
import re
import shutil
import tarfile
import tempfile
import types
import time

import gevent
import gevent.event
import gevent.queue
import paramiko

import rpyc
import x2go
import x2go.backends.profiles.base
import x2go.client
import x2go.defaults
import x2go.log
import x2go.xserver
import pytis.remote

pytis.remote.X2GOCLIENT_VERSION = _VERSION

XSERVER_VARIANTS = ('VcXsrv_pytis', 'VcXsrv_pytis_old', 'VcXsrv_pytis_desktop')
# TODO - because of http://bugs.x2go.org/cgi-bin/bugreport.cgi?bug=1044
# we use older variant of VcXsrv. Later we will switch back to the current version.
XSERVER_VARIANT_DEFAULT = 'VcXsrv_pytis_old'


XCONFIG_DEFAULTS = {
    'XServers': {
        'known_xservers': ['VcXsrv_development', 'VcXsrv_shipped', 'VcXsrv', 'Xming',
                           'Cygwin-X', 'VcXsrv_pytis', 'VcXsrv_pytis_desktop', 'VcXsrv_pytis_old'],
    },
    'VcXsrv_pytis': {
        'display': 'localhost:20',
        'last_display': 'localhost:20',
        'process_name': 'vcxsrv_pytis.exe',
        'test_installed': os.path.join(os.getcwd(), 'VcXsrv', 'vcxsrv_pytis.exe'),
        'run_command': os.path.join(os.getcwd(), 'VcXsrv', 'vcxsrv_pytis.exe'),
        'parameters': [':20', '-clipboard', '-noprimary', '-multiwindow',
                       '-nowinkill', '-nounixkill', '-swcursor'],
    },
    'VcXsrv_pytis_old': {
        'display': 'localhost:30',
        'last_display': 'localhost:30',
        'process_name': 'vcxsrv_pytis_old.exe',
        'test_installed': os.path.join(os.getcwd(), 'VcXsrv_old', 'vcxsrv_pytis_old.exe'),
        'run_command': os.path.join(os.getcwd(), 'VcXsrv_old', 'vcxsrv_pytis_old.exe'),
        'parameters': [':30', '-clipboard', '-noclipboardprimary', '-multiwindow',
                       '-nowinkill', '-nounixkill', '-swcursor'],
    },
    'VcXsrv_pytis_desktop': {
        'display': 'localhost:50',
        'last_display': 'localhost:50',
        'process_name': 'vcxsrv_pytis_desktop.exe',
        'test_installed': os.path.join(os.getcwd(), 'VcXsrv', 'vcxsrv_pytis_desktop.exe'),
        'run_command': os.path.join(os.getcwd(), 'VcXsrv', 'vcxsrv_pytis_desktop.exe'),
        'parameters': [':50', '-clipboard', 'noprimary', '-nowinkill',
                       '-nounixkill', '-swcursor', ],
    },
}

def on_windows():
    return platform.system() == 'Windows'

def pytis_path(*path):
    """Return absolute path given by path components relative to the current Pytis base directory.

    It is assumed, that the currently running script is located in the 'bin'
    subdirectory of the Pytis base directory.

    """
    return os.path.normpath(os.path.join(sys.path[0], '..', *path))

def runtime_error(message, exitcode=-1):
    # TODO: Raise an exception instead and catch it in the
    # startup application to display errors in the UI.
    sys.stderr.write("%s: error: %s\n" % (os.path.basename(sys.argv[0]), message))
    sys.exit(exitcode)


sys.path.append(pytis_path('lib'))

_ = gettext.translation('pytis-x2go', pytis_path('translations'), fallback=True).ugettext

X2GO_CLIENTXCONFIG_DEFAULTS = x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS

# Windows specific setup
if on_windows():
    X2GO_CLIENTXCONFIG_DEFAULTS.update(XCONFIG_DEFAULTS)
    x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS = X2GO_CLIENTXCONFIG_DEFAULTS
    reload(sys)
    sys.setdefaultencoding('cp1250')
    WIN_APPS_PATH = pytis_path('..', 'win_apps')
    os.environ['NXPROXY_BINARY'] = os.path.join(WIN_APPS_PATH, 'nxproxy', 'nxproxy.exe')
    # Set locale language
    import ctypes
    lcid_user = ctypes.windll.kernel32.GetUserDefaultLCID()
    lcid_system = ctypes.windll.kernel32.GetSystemDefaultLCID()
    if lcid_user:
        lcid = lcid_user
    else:
        lcid = lcid_system
    import locale
    os.environ["LANGUAGE"] = locale.windows_locale.get(lcid)

def get_language_windows(system_lang=True):
    """Get language code based on current Windows settings.
    @return: list of languages.
    """
    try:
        import ctypes
    except ImportError:
        return [locale.getdefaultlocale()[0]]
    # get all locales using windows API
    lcid_user = ctypes.windll.kernel32.GetUserDefaultLCID()
    lcid_system = ctypes.windll.kernel32.GetSystemDefaultLCID()
    if system_lang and lcid_user != lcid_system:
        lcids = [lcid_user, lcid_system]
    else:
        lcids = [lcid_user]
    return filter(None, [locale.windows_locale.get(i) for i in lcids]) or None

def ssh_connect(server, password=None, add_to_known_hosts=False, **kwargs):
    """Establish SSH connection according to given parameters.

    Arguments:
      server -- remote server host name (string)
      port -- remote server port (int)
      username -- remote user name (string)
      password -- remote user password or passphrase (if key_filename is not None) (string)
      key_filename -- name of the SSH private key file (string)
      allow_agent -- true if SSH agent should be allowed (bool)
      gss_auth -- true if GSS (Kerberos) authenticationexception should be allowed (bool)
      add_to_known_hosts -- Iff true, server keys not already present in known hosts will be
        automatically added.

    Returns 'paramiko.SSHClient' instance if successfully connected or None if
    connection fails.

    """
    client = paramiko.SSHClient()
    client.load_system_host_keys()
    if add_to_known_hosts:
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
    try:
        client.connect(server, password=password or 'X', **kwargs)
    except (paramiko.ssh_exception.AuthenticationException,
            paramiko.ssh_exception.SSHException,  # Happens on GSS auth failure.
            ImportError):  # Happens on GSS auth attempt with GSS libs uninstalled.
        return None
    return client


class ClientException(Exception):
    pass


class SSHClient(paramiko.SSHClient):
    pytis_client = True
    _pytis_gss_auth = False

    def connect(self, hostname, gss_auth=None, **kwargs):
        if gss_auth is None:
            gss_auth = SSHClient._pytis_gss_auth
        return super(SSHClient, self).connect(hostname, gss_auth=gss_auth, **kwargs)


try:
    paramiko.SSHClient.pytis_client
except AttributeError:
    paramiko.SSHClient = SSHClient


class X2GoClientSettings(x2go.X2GoClientSettings):

    def __init__(self, *args, **kwargs):
        super(X2GoClientSettings, self).__init__(*args, **kwargs)
        default_command = x2go.defaults.X2GO_SESSIONPROFILE_DEFAULTS['command']
        self.defaultValues['pytis'] = dict(hostname=None,
                                           password=None,
                                           port=22,
                                           key_filename=None,
                                           command=default_command)


class RpycInfo(object):

    def __init__(self, settings, port=None, password=None):
        self._filename = os.path.join(os.path.dirname(settings.config_files[0]), 'pytis-rpyc')
        self._port = port
        self._password = password

    def read(self):
        try:
            f = open(self._filename)
            port = int(f.next().rstrip())
            password = f.next().rstrip()
        except Exception as e:
            raise ClientException(_(u"Can't read RPyC info file: %s") % (self._filename,), e)
        self._port = int(port)
        self._password = password

    def store(self):
        try:
            f = open(self._filename, 'w')
            f.write('%s\n%s' % (self._port, self._password,))
            f.close()
        except Exception as e:
            raise ClientException(_(u"Error when writing RPyC info file: %s") %
                                  (self._filename,), e)

    def port(self):
        return self._port

    def password(self):
        return self._password


class SshProfiles(x2go.backends.profiles.base.X2GoSessionProfiles):

    defaultSessionProfile = copy.deepcopy(x2go.defaults.X2GO_SESSIONPROFILE_DEFAULTS)
    _DEFAULT_BROKER_PATH = '/usr/bin/x2gobroker'

    class ConnectionFailed(Exception):
        pass

    def __init__(self, connection_parameters, broker_path=None,
                 logger=None, loglevel=x2go.log.loglevel_DEFAULT, **kwargs):
        self._connection_parameters = connection_parameters
        self._broker_path = broker_path or self._DEFAULT_BROKER_PATH
        self._broker_profiles = None
        self._broker_profile_cache = {}
        self._ssh_client_instance = None
        x2go.backends.profiles.base.X2GoSessionProfiles.__init__(self, logger=logger,
                                                                 loglevel=loglevel, **kwargs)

    def __call__(self, profile_id_or_name):
        # Broken in upstream source
        _profile_id = self.check_profile_id_or_name(profile_id_or_name)
        return self.get_profile_config(profile_id=_profile_id)

    def _ssh_client(self):
        if self._ssh_client_instance is None:
            client = ssh_connect(**dict(self._connection_parameters, look_for_keys=False))
            if client:
                self._ssh_client_instance = client
            else:
                raise self.ConnectionFailed()
        return self._ssh_client_instance

    def broker_listprofiles(self):
        if self._broker_profiles is not None:
            return self._broker_profiles
        client = self._ssh_client()
        sessions = ''
        text = ''
        in_config = False
        command = "%s --task listsessions" % (self._broker_path,)
        __stdin, stdout, __stderr = client.exec_command(command)
        while True:
            line = stdout.readline()
            if not line:
                break
            text += line
            if line.strip() == 'START_USER_SESSIONS':
                in_config = True
                continue
            elif line.strip() == 'END_USER_SESSIONS':
                in_config = False
            if in_config:
                sessions += line
        import ConfigParser
        import StringIO
        parser = ConfigParser.ConfigParser()
        parser.readfp(StringIO.StringIO(sessions))
        # Get sessions
        sections = parser.sections()
        self._broker_profiles = profiles = {}
        for session_name in sections:
            session_data = {}
            for param in self.defaultSessionProfile:
                if parser.has_option(session_name, param):
                    try:
                        option_type = type(self.defaultSessionProfile[param])
                    except KeyError:
                        option_type = types.StringType
                    if isinstance(option_type, types.BooleanType):
                        val = parser.getboolean(session_name, param)
                    elif isinstance(option_type, types.IntType):
                        val = parser.getint(session_name, param)
                    elif isinstance(option_type, types.ListType):
                        _val = parser.get(session_name, param)
                        _val = _val.strip()
                        if _val.startswith('[') and _val.endswith(']'):
                            val = eval(_val)
                        elif ',' in _val:
                            _val = [v.strip() for v in _val.split(',')]
                        else:
                            val = [_val]
                    else:
                        _val = parser.get(session_name, param)
                        val = _val.decode('UTF8')
                else:
                    val = self.defaultSessionProfile[param]
                session_data[param] = val
            profiles[session_name] = session_data
        return profiles

    def broker_selectsession(self, profile_id):
        session_data = self._broker_profile_cache.get(profile_id)
        if session_data is not None:
            return session_data
        client = self._ssh_client()
        if client is None:
            return {}
        session_data = self._broker_profile_cache[profile_id] = \
            copy.copy(self._broker_profiles[profile_id])
        server_regexp = re.compile('^SERVER:(.*):(.*)$')
        text = ''
        command = "%s --task selectsession --sid %s" % (self._broker_path, profile_id)
        __stdin, stdout, __stderr = client.exec_command(command)
        while True:
            line = stdout.readline()
            if not line:
                break
            text += line
            line = line.strip()
            m = server_regexp.match(line)
            if m is not None:
                host, port = m.groups()
                try:
                    port = int(port)
                except ValueError:
                    raise Exception(_("Invalid session format"), text)
                session_data['host'] = host
                session_data['sshport'] = port
        return session_data

    def _init_profile_cache(self, profile_id):
        profile_id = unicode(profile_id)
        if profile_id in self._broker_profile_cache:
            del self._broker_profile_cache[profile_id]

    def _populate_session_profiles(self):
        session_profiles = self.broker_listprofiles()
        _session_profiles = copy.deepcopy(session_profiles)
        for session_profile in _session_profiles:
            session_profile = unicode(session_profile)
            for key, default_value in self.defaultSessionProfile.iteritems():
                key = unicode(key)
                if isinstance(default_value, str):
                    default_value = unicode(default_value)
                if key not in session_profiles[session_profile]:
                    session_profiles[session_profile][key] = default_value
        return session_profiles

    def _is_mutable(self, profile_id):
        return False

    def _supports_mutable_profiles(self):
        return False

    def _get_profile_parameter(self, profile_id, option, key_type):
        value = self.session_profiles[unicode(profile_id)][unicode(option)]
        if key_type is list and isinstance(value, basestring):
            value = unicode(value).split(',')
        return key_type(value)

    def _get_profile_options(self, profile_id):
        return self.session_profiles[unicode(profile_id)].keys()

    def _get_profile_ids(self):
        self.session_profiles.keys()
        return self.session_profiles.keys()

    def _get_server_hostname(self, profile_id):
        selected_session = self.broker_selectsession(profile_id)
        return selected_session['host']

    def _get_server_port(self, profile_id):
        selected_session = self.broker_selectsession(profile_id)
        return int(selected_session['sshport'])


class PytisSshProfiles(SshProfiles):

    def __init__(self, *args, **kwargs):
        self._pytis_upgrade_parameters = (None, None)
        SshProfiles.__init__(self, *args, **kwargs)

    def broker_listprofiles(self):
        profiles = SshProfiles.broker_listprofiles(self)
        filtered_profiles = {}
        last_version = _VERSION
        for section, data in profiles.items():
            if section.startswith('pytis-client-upgrade'):
                if data['name'] > last_version:
                    # We can use only supported parameters from session_profiles
                    # So we'll use 'name' for the version and 'command' for url.
                    last_version = data['name']
                    self._pytis_upgrade_parameters = (data['name'], data['command'])
            else:
                filtered_profiles[section] = data
        return filtered_profiles

    def pytis_upgrade_parameters(self):
        return self._pytis_upgrade_parameters


class X2GoClientXConfig(x2go.xserver.X2GoClientXConfig):

    def _fix_win_path(self, path):
        """
        Windows has lots of problems with executables with spaces in
        the name; this function will remove them (using the ~1
        format):
        """
        if ' ' in path:
            import ctypes
            GetShortPathName = ctypes.windll.kernel32.GetShortPathNameW
            size = max(len(path) + 1, 256)
            buf = ctypes.create_unicode_buffer(size)
            try:
                u = unicode
            except NameError:
                u = str
            ret = GetShortPathName(u(path), buf, size)
            if not ret:
                error_msg = ('Error: the path "%s" has a space in it. '
                             'We could not determine the short pathname for it.' % path)
                raise ClientException(error_msg)
            path = str(buf.value)
        return path

    def get_xserver_config(self, xserver_name):
        if xserver_name not in XSERVER_VARIANTS:
            return super(X2GoClientXConfig, self).get_xserver_config(xserver_name)
        _xserver_config = {}
        _changed = False
        _defaults = XCONFIG_DEFAULTS[xserver_name]
        for option in self.iniConfig.options(xserver_name):
            if option in ('test_installed', 'run_command'):
                defaults_path = _defaults[option]
                d, f = os.path.split(defaults_path)
                _xserver_config[option] = self._fix_win_path(
                    os.path.join(WIN_APPS_PATH, os.path.split(d)[-1], f))
            elif option in ('display', 'last_display', 'process_name', 'parameters'):
                _xserver_config[option] = _defaults[option]
            else:
                try:
                    _xserver_config[option] = self.get(xserver_name, option,
                                                       key_type=self.get_type(xserver_name, option))
                except KeyError:
                    pass
            if self.get_value(xserver_name, option) != _xserver_config[option]:
                _changed = True
                self.update_value(xserver_name, option, _xserver_config[option])
        if _changed:
            self.write_user_config = True
            self.write()
        return _xserver_config


class X2GoClient(x2go.X2GoClient):

    _DEFAULT_RPYC_PORT = 10000
    _MAX_RPYC_PORT_ATTEMPTS = 100

    def __init__(self, session_parameters, settings, update_progress,
                 xserver_variant=XSERVER_VARIANT_DEFAULT, **kwargs):
        self._session_parameters = session_parameters
        self._update_progress = update_progress
        self._update_progress(_("Preparing X2Go session."))
        x2go.X2GoClient.__init__(self, start_xserver=False, use_cache=False, **kwargs)
                                 #logger = x2go.X2GoLogger(tag='PytisClient')
        if on_windows() and xserver_variant:
            self._update_progress(_("Starting up X11 server."))
            self._start_xserver(xserver_variant)
        self._x2go_session_hash = self._X2GoClient__register_session(**session_parameters)
        self._pytis_port_value = gevent.event.AsyncResult()
        self._pytis_password_value = gevent.event.AsyncResult()
        self._pytis_terminate = gevent.event.Event()
        session = self.session_registry(self._x2go_session_hash)
        session.sshproxy_params['key_filename'] = session_parameters['key_filename']
        session.sshproxy_params['look_for_keys'] = False
        self._update_progress(_("Starting up server connections."))
        self._start_processes(settings)
        self._run_pytis_setup = True
        self._update_progress(_("Connecting to X2Go session."))
        # try:
        self._X2GoClient__connect_session(self._x2go_session_hash,
                                          username=session_parameters['username'],
                                          password=session_parameters['password'])
        # TODO: We don't handle these exceptions because we should already have
        # valid authentication credentials thanks to calling 'ssh_connect()' prior
        # to X2GoClient instance creation.  But maybe at least some (socket error,
        # key error, ...) of them would still be worth catching.
        #
        # except x2go.PasswordRequiredException, e:
        # except x2go.AuthenticationException, e:
        # except x2go.BadHostKeyException:
        #     runtime_error('SSH host key verification for remote host [%s]:%s failed' %
        #                   (session_parameters['server'], session_parameters['port']),
        #                   exitcode=-254)
        # except x2go.SSHException, e:
        #     if str(e) not in ('not a valid DSA private key file',
        #                       'Incompatible ssh peer (no acceptable kex algorithm)',
        #                       'No authentication methods available'):
        #         runtime_error(str(e), exitcode=253)
        #     self.logger('passwordless login for ,,%s\'\' failed' % _username,
        #                 loglevel=x2go.loglevel_WARN)
        #     self.logger('proceeding to interactive login for user ,,%s\'\'' % _username,
        #                 loglevel=x2go.loglevel_NOTICE)
        # except socket.error, e:
        #     runtime_error('a socket error occured while establishing the connection: %s' %
        #                   str(e), exitcode=-245)

    def _pytis_setup(self, s_uuid):
        # Configuration transfer to the server
        session = self.get_session(s_uuid)
        control_session = session.control_session
        # It would be better to use self.info_backend or
        # self.get_session_info(s_uuid) as the source of remote_container
        # (instead of _x2go_remote_home + '/.x2go') and agent_pid (instead of
        # application) but this information is often unavailable here for
        # unclear reasons.
        session_id = self.session_registry(s_uuid).terminal_session.session_info.name
        server_file_name = '%s/.x2go/ssh/pytis.%s' % (control_session._x2go_remote_home,
                                                      session_id,)

        class ServerInfo(object):
            def __init__(self):
                self._port = None
                self._password = None

            def port(self):
                return self._port

            def set_port(self, port):
                self._port = port

            def password(self):
                return self._password

            def set_password(self, password):
                self._password = password

            def write(self):
                if self._port is None or self._password is None:
                    return
                data = '0:%s:%s:' % (self._port, self._password,)
                control_session._x2go_sftp_write(server_file_name, data)
        self._pytis_server_info = ServerInfo()

    def _handle_info(self):
        try:
            password = self._pytis_password_value.get_nowait()
            port = self._pytis_port_value.get_nowait()
        except gevent.Timeout:
            return
        info = self._pytis_server_info
        if password != info.password() or port != info.port():
            info.set_password(password)
            info.set_port(port)
            info.write()

    def _check_rpyc_server(self, settings, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead):
        import pytis.remote.pytisproc as pytisproc
        server = None
        while True:
            if self._pytis_terminate.is_set():
                if server is not None:
                    server.close()
                    server = None
                return
            # Look for a running RPyC instance
            running = True
            rpyc_info = RpycInfo(settings)
            try:
                rpyc_info.read()
            except ClientException:
                running = False
            if running:
                # Check whether it's our instance
                # We must be careful here not to send the password to an RPyC instance
                # of another user.
                port = rpyc_info.port()
                password = rpyc_info.password()
                authenticator = pytisproc.PasswordAuthenticator(password,
                                                                ssh_tunnel_dead=ssh_tunnel_dead)
                try:
                    authenticator.connect('localhost', port)
                except:
                    running = False
            if not rpyc_stop_queue.empty():
                while not rpyc_stop_queue.empty():
                    rpyc_stop_queue.get()
                if running and server is not None:
                    server.close()
                    server = None
                running = False
            # Maybe the instance was started by another process so we must set
            # the parameters.
            if running and not rpyc_port.ready():
                rpyc_port.set(port)
                rpyc_info.store()
                self._pytis_password_value.set(rpyc_info.password())
            # If no running RPyC instance was found then start one
            if not running:
                if server is not None:
                    server.close()
                    server = None
                authenticator = pytisproc.PasswordAuthenticator()
                default_port = self._DEFAULT_RPYC_PORT
                port_limit = default_port + self._MAX_RPYC_PORT_ATTEMPTS
                for port in range(default_port, port_limit):
                    try:
                        server = rpyc.utils.server.ThreadedServer(pytisproc.PytisUserService,
                                                                  hostname='localhost',
                                                                  port=port,
                                                                  authenticator=authenticator)
                        break
                    except:
                        pass
                else:
                    raise ClientException(_(u"No free port found for RPyC in the range %s-%s") %
                                          (default_port, port_limit - 1,))
                server.service.authenticator = authenticator
                rpyc_port.set(port)
                rpyc_info = RpycInfo(settings, port=port, password=authenticator.password())
                rpyc_info.store()
                gevent.spawn(server.start)
                self._pytis_password_value.set(rpyc_info.password())
            gevent.sleep(1)

    def _check_ssh_tunnel(self, settings, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead):
        while True:
            while not rpyc_port.ready():
                if self._pytis_terminate.is_set():
                    return
                gevent.sleep(0.1)
            current_rpyc_port = rpyc_port.get()
            port = gevent.event.AsyncResult()
            tunnel = pytis.remote.ReverseTunnel(
                self._session_parameters['server'],
                current_rpyc_port,
                ssh_port=self._session_parameters['port'],
                ssh_forward_port=0,
                ssh_user=self._session_parameters['username'],
                ssh_password=self._session_parameters['password'],
                key_filename=self._session_parameters['key_filename'],
                ssh_forward_port_result=port,
                gss_auth=self._session_parameters['gss_auth'],
            )
            tunnel.start()
            while not tunnel.ready():
                if port.wait(0.1) is not None:
                    self._pytis_port_value.set(port.get())
                    break
            while True:
                if self._pytis_terminate.is_set():
                    tunnel.kill()
                    return
                if tunnel.ready():
                    ssh_tunnel_dead.set()
                    # We must restart RPyC as well in order to prevent password leak
                    rpyc_stop_queue.put(True)
                    break
                elif rpyc_port.get() != current_rpyc_port:
                    tunnel.kill()
                    break
                else:
                    gevent.sleep(1)

    def _start_processes(self, settings):
        # RPyC server
        rpyc_stop_queue = gevent.queue.Queue()
        rpyc_port = gevent.event.AsyncResult()
        ssh_tunnel_dead = gevent.event.Event()
        self._pytis_terminate.clear()
        args = (settings, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead,)
        gevent.spawn(self._check_rpyc_server, *args)
        gevent.spawn(self._check_ssh_tunnel, *args)

    def _start_xserver(self, variant=None):
        if self.client_rootdir:
            kwargs = dict(config_files=os.path.join(self.client_rootdir,
                                                    x2go.defaults.X2GO_XCONFIG_FILENAME))
        else:
            kwargs = dict()
        self.client_xconfig = X2GoClientXConfig(logger=self.logger, **kwargs)
        if not self.client_xconfig.installed_xservers:
            self.HOOK_no_installed_xservers_found()
        else:
            last_display = None
            if isinstance(variant, types.BooleanType):
                p_xs_name = self.client_xconfig.preferred_xserver_names[0]
                last_display = self.client_xconfig.get_xserver_config(p_xs_name)['last_display']
                new_display = self.client_xconfig.detect_unused_xdisplay_port(p_xs_name)
                p_xs = (p_xs_name, self.client_xconfig.get_xserver_config(p_xs_name))
            elif isinstance(variant, types.StringType):
                last_display = self.client_xconfig.get_xserver_config(variant)['last_display']
                new_display = self.client_xconfig.detect_unused_xdisplay_port(variant)
                p_xs = (variant, self.client_xconfig.get_xserver_config(variant))
            if not self.client_xconfig.running_xservers:
                if p_xs is not None:
                    self.xserver = x2go.xserver.X2GoXServer(p_xs[0], p_xs[1], logger=self.logger)
            else:
                if p_xs is not None and last_display is not None:
                    if last_display == new_display:
                        #
                        # FIXME: this trick is nasty, client implementation should rather
                        # cleanly shutdown launch X-server processes
                        #
                        # re-use a left behind X-server instance of a previous/crashed
                        # run of Python X2Go Client
                        self.logger('found a running (and maybe stray) X-server, trying to '
                                    're-use it on X DISPLAY port: %s' % last_display,
                                    loglevel=x2go.log.loglevel_WARN)
                        os.environ.update({'DISPLAY': last_display})
                else:
                    # presume the running XServer listens on :0
                    self.logger('using fallback display for X-server: localhost:0',
                                loglevel=x2go.log.loglevel_WARN)
                    os.environ.update({'DISPLAY': 'localhost:0'})

    def _main_loop(self):
        try:
            session_hash = self._x2go_session_hash
            while 0 < self.get_session(session_hash).get_progress_status() < 100:
                time.sleep(1)
            if self._X2GoClient__session_ok(session_hash):
                profile_name = self._X2GoClient__get_session_profile_name(session_hash)
                session_name = self._X2GoClient__get_session_name(session_hash)
                self.logger("X2Go session is now running, the X2Go client's profile name is: %s" %
                            profile_name, loglevel=x2go.loglevel_INFO)
                self.logger("X2Go session name is: %s" % session_name, loglevel=x2go.loglevel_INFO)
                while self._X2GoClient__session_ok(session_hash):
                    time.sleep(2)
        except x2go.X2GoSessionException, e:
            self.logger("X2GoSessionException occured:", loglevel=x2go.loglevel_ERROR)
            self.logger("-> %s" % str(e), loglevel=x2go.loglevel_ERROR)

    def list_sessions(self):
        sessions = [info for info in
                    self._X2GoClient__list_sessions(self._x2go_session_hash).values()
                    if info.status == 'S']
        sessions.sort(lambda a, b: (cmp(a.username, b.username) or
                                    cmp(a.hostname, b.hostname) or
                                    cmp(b.date_created, a.date_created)))
        return sessions

    def terminate_session(self, session):
        """Terminate given X2Go Session."""
        try:
            # send a terminate request to a session
            self.logger('Requesting X2Go session terminate of session: %s' % session.name,
                        loglevel=x2go.loglevel_INFO)
            available_sessions = self._X2GoClient__list_sessions(self._x2go_session_hash)
            if session.name in available_sessions.keys():
                self._X2GoClient__terminate_session(self._x2go_session_hash, session.name)
                self.logger("X2Go session %s has been terminated" % session.name,
                            loglevel=x2go.loglevel_NOTICE)
            else:
                runtime_error('Session %s not available on X2Go server [%s]:%s' %
                              (self._session_parameters['server'],
                               self._session_parameters['port']),
                              exitcode=22)
        finally:
            self._pytis_terminate.set()

    def start_new_session(self, callback=None):
        """Launch a new X2Go session."""
        self.logger('starting a new X2Go session', loglevel=x2go.loglevel_INFO)
        self.logger('command for new session is: %s' % self._session_parameters['cmd'],
                    loglevel=x2go.loglevel_DEBUG)
        self._X2GoClient__start_session(self._x2go_session_hash)
        if self._run_pytis_setup:
            self._pytis_setup(self._x2go_session_hash)
            self._run_pytis_setup = False

            def info_handler():
                while self.session_ok(self._x2go_session_hash):
                    self._handle_info()
                    gevent.sleep(0.1)
            gevent.spawn(info_handler)
        if callback:
            callback()
        self._main_loop()

    def resume_session(self, session, callback=None):
        """Resume given server-side suspended X2Go session."""
        self.logger('resuming X2Go session: %s' % session.name, loglevel=x2go.loglevel_INFO)
        available_sessions = self._X2GoClient__list_sessions(self._x2go_session_hash)
        if session.name in available_sessions.keys():
            self._X2GoClient__resume_session(self._x2go_session_hash, session.name)
        else:
            runtime_error('Session %s not available on X2Go server [%s]:%s' %
                          (self._session_parameters['server'], self._session_parameters['port']),
                          exitcode=20)
        if callback:
            callback()
        self._main_loop()


class StartupController(object):
    """Interface between the start-up application and X2GoClient.

    It will be probably more logical to rearrange the code in the following way:

      - methods working before X2GoClient instance creation can stay in a separate
        class (this one).

      - the method connect() would return a X2GoClient instance.

      - methods working on X2GoClient instance can be moved to X2GoClient
        itself and called from the startup application directly

    """

    DEFAULT_SSH_PORT = 22

    def __init__(self, args, update_progress):
        self._args = args
        self._update_progress = update_progress
        self._xserver_variant = XSERVER_VARIANT_DEFAULT
        self._session_parameters = {}
        self._settings = settings = X2GoClientSettings()

        def cfg(*params):
            if args.broker_url is None:
                try:
                    return settings.get_value(*params)
                except ClientException:
                    pass
            return None
        self._session_parameters.update(
            server=args.server or cfg('pytis', 'hostname'),
            port=int(args.remote_ssh_port or cfg('pytis', 'port', int)),
            username=args.username or None,
            password=args.password or cfg('pytis', 'password'),
            key_filename=args.ssh_privkey or cfg('pytis', 'key_filename'),
            gss_auth=False,
            look_for_keys=True,
            allow_agent=True,
            known_hosts=os.path.join(x2go.LOCAL_HOME, x2go.X2GO_SSH_ROOTDIR, 'known_hosts'),
        )
        if args.broker_url:
            # If broker_url is given, the session parameters will be updated
            # later from the selected profile in select_profile().
            self._broker_parameters, self._broker_path = self._parse_url(args.broker_url)
        elif args.session_profile:
            # Override session profile options by command line options.
            arg_to_session_param = {
                'command': 'cmd',
                'kb_layout': 'kblayout',
                'kb_type': 'kbtype',
                'sound': 'snd_system',
            }
            parameters = {}
            if hasattr(args, 'parser'):
                for arg, value in args._get_kwargs():
                    if value != args.parser.get_default(arg):
                        try:
                            param = arg_to_session_param[arg]
                        except KeyError:
                            param = arg
                        parameters[param] = value
            self._session_parameters.update(
                profile_name=args.session_profile,
                **parameters
            )
        else:
            # Set up the manually configured X2Go session.
            self._session_parameters.update(
                profile_id=args.session_profile,
                profile_name='Pytis-Client-Session',
                session_type=args.session_type,
                link=args.link,
                geometry=args.geometry,
                pack=args.pack,
                cache_type='unix-kde',
                kblayout=args.kbd_layout,
                kbtype=args.kbd_type,
                snd_system=args.sound,
                printing=args.printing,
                print_action=args.print_action,
                print_action_args=args.print_action_args,
                share_local_folders=args.share_local_folders,
                allow_share_local_folders=True,
                cmd=args.command,
            )
        quit_signal = signal.SIGTERM if on_windows() else signal.SIGQUIT
        gevent.signal(quit_signal, gevent.kill)

    def _parse_url(self, url):
        match = re.match(('^(?P<protocol>(ssh|http(|s)))://'
                          '(|(?P<username>[a-zA-Z0-9_\.-]+)'
                          '(|:(?P<password>.*))@)'
                          '(?P<server>[a-zA-Z0-9\.-]+)'
                          '(|:(?P<port>[0-9]+))'
                          '($|(?P<path>/.*)$)'), url)
        if match:
            parameters = match.groupdict()
            parameters['port'] = int(parameters['port'] or self.DEFAULT_SSH_PORT)
            if parameters.pop('protocol', None) not in (None, 'ssh'):
                raise Exception(_(u"Unsupported broker protocol: %s") % url)
            path = parameters.pop('path')
            return parameters, path
        else:
            raise Exception(_("Invalid broker URL:"), args.broker_url)

    def _authentication_methods(self, connection_parameters):
        import socket
        sock = socket.socket()
        sock.connect((connection_parameters['server'], connection_parameters['port']))
        transport = paramiko.Transport(sock)
        transport.connect()
        try:
            transport.auth_none('')
        except paramiko.ssh_exception.BadAuthenticationType as e:
            methods = e.allowed_types
        transport.close()
        sock.close()
        return methods

    def _acceptable_key_files(self, connection_parameters):
        def key_acceptable(path):
            if os.access(path, os.R_OK):
                try:
                    return pytis.remote.public_key_acceptable(
                        connection_parameters['server'],
                        connection_parameters['username'],
                        path,
                        port=connection_parameters['port'])
                except:
                    return True
            else:
                return True
        return [path for path in [os.path.join(os.path.expanduser('~'), '.ssh', name)
                                  for name in ('id_ecdsa', 'id_rsa', 'id_dsa')]
                if os.access(path, os.R_OK) and key_acceptable(path + '.pub')]

    def _authenticate(self, function, connection_parameters, askpass, keyring=None, **kwargs):
        """Try calling 'method' with different authentication parameters.

        Arguments:
          function -- connection function to be called with different
            connection parameters to try different authentication methods.  The
            function must accept the dictionary of connection parameters as its
            first positional argument plus optionally any keyword arguemnts
            passed in '**kwargs'.  The function must return a result which (in
            boolean context) evaluates to True to indicate success or False for
            failure.  If failure is returned for one authentication method,
            another method may be tried (with different connection parameters).
          connection_parameters -- initial connection parameters as a
            dictionary with keys such as 'server', 'port', 'username', etc.
            Authentication related keys in this dictionary will be overriden
            before passing the parameters to 'function' as described below.
          askpass -- function called when authentication credentials need to be
            obtained interactively from the user.  The function must accept two
            arguments.  The first is the sequence of authentication methods
            supported by the server and the second is the list of SSH private
            key files on local machine for which the server has a public key
            (if public key authentication is supported).  The return value is a
            two-tuple of (key_filename, password), where 'key_filename' is the
            SSH private key and 'password' is its passphrase 'key_filename' is
            None and 'password' is the password for password authentication.
          keyring -- known authentication credentials as a list of pairs
            (key_filename, password), where 'key_filename' is a string path to
            a SSH private key and 'password' is its passphrase or
            'key_filename' is None and 'password' is the password for password
            authentication.  The credentials present in the list will be used
            for authentication.  Also any successfull newly entered credentials
            obtained from the user (using 'askpass') will be added to list if
            it is not None.

          **kwargs -- all remaining keyword arguments will be passed on to
            'function'.

        Authentication related connection parameters:

          gss_auth -- True if GSS-API (Kerberos) authentication is to be used
            or False for other authentication schemes
          key_filename -- file name of the private SSH key to use for
            authentication or None
          password -- SSH key passphrase if 'key_filename' is given or user's
            password for password authentication

        Supported authentication methods:
          - GSS-API (Kerberos) -- 'gss_auth' is True and 'key_filename' and
            'password' are empty
          - SSH Agent -- 'gss_auth' is False and 'key_filename' and 'password'
            are empty
          - Public Key -- 'gss_auth' is False and 'key_filename' and 'password'
            are given
          - Password -- 'gss_auth' is False, 'key_filename' is empty and
            'password' is given

        """
        def message(msg):
            self._update_progress(connection_parameters['server'] + ': ' + msg)

        def connect(gss_auth=False, key_filename=None, password=None):
            return function(**dict(
                connection_parameters,
                gss_auth=gss_auth,
                key_filename=key_filename,
                password=password,
                look_for_keys=key_filename is not None,
                allow_agent=True,
                add_to_known_hosts=self._args.add_to_known_hosts,
                **kwargs
            ))

        success = False
        message(_("Retrieving supported authentication methods."))
        methods = self._authentication_methods(connection_parameters)
        for key_filename, password in (keyring or ()):
            if key_filename and password and 'publickey' in methods:
                message(_("Trying public key authentication."))
                success = connect(key_filename=key_filename, password=password)
            elif key_filename is None and password and 'password' in methods:
                message(_("Trying password authentication."))
                success = connect(password=password)
            if success:
                break
        if not success:
            message(_("Trying SSH Agent authentication."))
            success = connect()
        if not success:
            message(_("Trying Kerberos authentication."))
            success = connect(gss_auth=True)
        if not success:
            message(_("Trying interactive authentication."))
            if 'publickey' in methods:
                key_files = self._acceptable_key_files(connection_parameters)
            else:
                key_files = ()
            while not success:
                key_filename, password = askpass(methods, key_files)
                if key_filename:
                    message(_("Trying public key authentication."))
                elif password:
                    message(_("Trying password authentication."))
                else:
                    return None
                success = connect(key_filename=key_filename, password=password)
                if success and keyring is not None:
                    keyring.append((key_filename, password))
        if success:
            message(_("Connected successfully."))
        return success

    def _connect(self, client_kwargs, **connection_parameters):
        if ssh_connect(**connection_parameters):
            # Clean tempdir.
            tempdir = tempfile.gettempdir()
            for f in os.listdir(tempdir):
                if f.startswith('pytistmp'):
                    try:
                        os.remove(os.path.join(tempdir, f))
                    except:
                        pass
            # TODO: Is this really necessary?
            SSHClient._pytis_gss_auth = connection_parameters['gss_auth']
            # Create and set up the client instance.
            session_parameters = dict(self._session_parameters, **connection_parameters)
            self._client = X2GoClient(session_parameters, self._settings, self._update_progress,
                                      xserver_variant=self._xserver_variant,
                                      loglevel=x2go.log.loglevel_DEBUG, **client_kwargs)
            return True
        else:
            return False

    def connect(self, username, askpass, keyring=None):
        connection_parameters = dict([(k, self._session_parameters[k])
                                      for k in ('server', 'port', 'username', 'password',
                                                'key_filename', 'allow_agent', 'gss_auth')])
        client_kwargs = dict((param, getattr(self._args, arg))
                             for (arg, param) in (('backend_controlsession', 'control_backend'),
                                                  ('backend_terminalsession', 'terminal_backend'),
                                                  ('backend_serversessioninfo', 'info_backend'),
                                                  ('backend_serversessionlist', 'list_backend'),
                                                  ('backend_proxy', 'proxy_backend'),
                                                  ('backend_sessionprofiles', 'profiles_backend'),
                                                  ('backend_clientsettings', 'settings_backend'),
                                                  ('backend_clientprinting', 'printing_backend'))
                      if getattr(self._args, arg) is not None)
        return self._authenticate(self._connect, connection_parameters, askpass, keyring=keyring,
                                  client_kwargs=client_kwargs)

    def check_key_password(self, key_filename, password):
        for handler in (paramiko.RSAKey, paramiko.DSSKey, paramiko.ECDSAKey):
            try:
                handler.from_private_key_file(key_filename, password)
                return True
            except:
                continue
        return False

    def _list_profiles(self, broker_path=None, **connection_parameters):
        try:
            return PytisSshProfiles(connection_parameters,
                                    logger=x2go.X2GoLogger(tag='PytisClient'),
                                    broker_path=broker_path,
                                    broker_password=self._args.broker_password)
        except PytisSshProfiles.ConnectionFailed:
            return None

    def list_profiles(self, username, askpass, keyring=None):
        connection_parameters = dict(self._broker_parameters,
                                     username=self._broker_parameters['username'] or username)
        self._profiles = self._authenticate(self._list_profiles, connection_parameters, askpass,
                                            keyring=keyring, broker_path=self._broker_path)
        if self._profiles:
            self._update_progress(self._broker_parameters['server'] + ': ' +
                                  _("Returned %d profiles.") % len(self._profiles.profile_ids))
        return self._profiles

    def broker_url_username(self):
        # This is actually caled before list_profiles() and it provides the default value
        # for its username argument (which may be altered by user in the UI).
        return self._broker_parameters['username']

    def _profile_session_parameters(self, profile_id):
        parameters = self._profiles.to_session_params(profile_id)
        if isinstance(parameters['server'], list):
            parameters['server'] = parameters['server'][0]
        return parameters

    def select_profile(self, profile_id):
        profile = self._profiles.broker_selectsession(profile_id)
        rootless = profile.get('rootless', True)
        if not rootless or int(rootless) == 0:
            self._xserver_variant = 'VcXsrv_pytis_desktop'
        self._session_parameters.update(self._profile_session_parameters(profile_id))

    def current_version(self):
        return _VERSION

    def available_upgrade_version(self):
        version, url = self._profiles.pytis_upgrade_parameters()
        if version and url:
            return version
        else:
            return None

    def upgrade(self, username, askpass, keyring=None):
        url_params, path = self._parse_url(self._profiles.pytis_upgrade_parameters()[1])
        connection_parameters = dict(url_params, username=url_params['username'] or username)
        client = self._authenticate(ssh_connect, connection_parameters, askpass, keyring=keyring)
        if client is None:
            return _(u"Couldn't connect to upgrade server.")
        sftp = client.open_sftp()
        upgrade_file = sftp.open(path)
        # Unpack the upgrade file and replace the current installation.
        install_directory = pytis_path()
        old_install_directory = install_directory + '.old'
        tmp_directory = tempfile.mkdtemp(prefix='pytisupgrade')
        pytis_directory = os.path.join(tmp_directory, 'pytis2go', 'pytis')
        scripts_directory = os.path.join(tmp_directory, 'pytis2go', 'scripts')
        scripts_install_dir = os.path.normpath(os.path.join(install_directory, '..', 'scripts'))
        tarfile.open(fileobj=upgrade_file).extractall(path=tmp_directory)
        if not os.path.isdir(pytis_directory):
            return _(u"Package unpacking failed.")
        if os.path.exists(old_install_directory):
            shutil.rmtree(old_install_directory)
        shutil.move(install_directory, old_install_directory)
        shutil.move(pytis_directory, install_directory)
        shutil.rmtree(old_install_directory)
        xconfig = os.path.join(os.path.expanduser('~'), '.x2goclient', 'xconfig')
        if os.access(xconfig, os.W_OK):
            os.remove(xconfig)
        if os.access(scripts_directory, os.R_OK):
            for fname in os.listdir(scripts_directory):
                fpath = os.path.join(scripts_directory, fname)
                dpath = os.path.normpath(os.path.join(scripts_install_dir, fname))
                if os.access(dpath, os.W_OK):
                    os.remove(dpath)
                try:
                    shutil.move(fpath, scripts_install_dir)
                except Exception:
                    pass
        # Execute supplied update procedure if it exists.
        if os.access(os.path.join(tmp_directory, 'updatescript.py'), os.R_OK):
            sys.path.append(tmp_directory)
            try:
                import updatescript
            except:
                pass
            else:
                updatescript.run(version=_VERSION, path=path)
        shutil.rmtree(tmp_directory)

    def on_windows(self):
        return on_windows()

    def list_sessions(self):
        return self._client.list_sessions()

    def terminate_session(self, session):
        return self._client.terminate_session(session)

    def resume_session(self, session, callback=None):
        self._client.resume_session(session, callback=callback)

    def start_new_session(self, callback=None):
        self._client.start_new_session(callback=callback)

    def _vbs_path(self, directory, username, profile_id):
        return os.path.join(directory, '%s__%s__%s__%s.vbs' % (
            username,
            self._broker_parameters['server'],
            self._profile_session_parameters(profile_id)['server'],
            profile_id,
        ))

    def _scripts_directory(self):
        return pytis_path('..', 'scripts')

    def shortcut_exists(self, username, profile_id):
        import winshell
        vbs_path = self._vbs_path(self._scripts_directory(), username, profile_id)
        if not os.path.exists(vbs_path):
            return False
        for name in os.listdir(winshell.desktop()):
            if os.path.splitext(name)[1].lower() == '.lnk':
                link_path = os.path.join(winshell.desktop(), name)
                if os.path.isfile(link_path):
                    try:
                        with winshell.shortcut(link_path) as link:
                            if link.path.lower() == vbs_path.lower():
                                return True
                    except Exception:
                        pass
        return False

    def create_shortcut(self, username, profile_id):
        import winshell
        directory = self._scripts_directory()
        if not os.path.isdir(directory):
            return _("Unable to find the scripts directory: %s") % directory
        vbs_path = self._vbs_path(directory, username, profile_id)
        # Create the VBS script to which the shortcut will point to.
        if not os.path.exists(vbs_path):
            calling_script = os.path.join(directory, os.path.basename(self._args.calling_script))
            if not os.path.exists(calling_script):
                # Check if the VBS script was created properly.
                return _("Unable to find the calling script: %s") % calling_script
            with open(calling_script, 'r') as f:
                script_src = f.read()
            params = self._broker_parameters
            broker_url = "ssh://%s%s@%s%s/%s" % (
                username,
                ':' + params['password'] if params['password'] else '',
                params['server'],
                ':' + params['port'] if params['port'] != self.DEFAULT_SSH_PORT else '',
                self._broker_path,
            )
            for regexp, replacement in (
                    (r'--broker-url=[^"\s]+', '--broker-url=%s -P %s' % (broker_url, profile_id)),
                    (r'--create-shortcut', ''),
                    (r'--calling-script=', ''),
                    (r'&\s+Wscript.ScriptFullName', ''),
                    (r'&\s+Wscript.ScriptName', ''),
                    (r'&\s+Wscript.ScriptName', ''),
            ):
                script_src = re.sub(regexp, replacement, script_src)
            with open(vbs_path, 'w') as f:
                f.write(script_src)
        # Create the shortcut on the desktop.
        profile_name = self._profile_session_parameters(profile_id)['profile_name']
        shortcut_path, n = os.path.join(winshell.desktop(), '%s.lnk' % profile_name), 0
        while os.path.exists(shortcut_path):
            # If the shortcut of given name already exists, it is probably something else as
            # it was not found by shortcut_exists(), so try to find the first unused name.
            n += 1
            shortcut_path = os.path.join(winshell.desktop(), '%s(%d).lnk' % (profile_name, n))
        with winshell.shortcut(shortcut_path) as link:
            link.path = vbs_path
            link.description = profile_id
            link.working_directory = directory
            icon_location = os.path.normpath(os.path.join(directory, '..', 'icons', 'p2go.ico'))
            if os.path.exists(icon_location):
                link.icon_location = (icon_location, 0)
        return None


# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
