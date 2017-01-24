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

# ATTENTION: This should be updated on each code change.
_VERSION = '2016-06-22 08:08'

pytis.remote.X2GOCLIENT_VERSION = _VERSION

XSERVER_VARIANTS = ('VcXsrv_pytis', 'VcXsrv_pytis_desktop')
XSERVER_VARIANT_DEFAULT = 'VcXsrv_pytis'

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

def run_directory():
    return sys.path[0]


X2GO_CLIENTXCONFIG_DEFAULTS = x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS

if on_windows():
    X2GO_CLIENTXCONFIG_DEFAULTS.update(pytis.remote.XCONFIG_DEFAULTS)
    x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS = X2GO_CLIENTXCONFIG_DEFAULTS

sys.path.append(os.path.normpath(os.path.join(run_directory(), '..', 'lib')))

t = gettext.translation('pytis-x2go',
                        os.path.normpath(os.path.join(run_directory(), '..', 'translations')),
                        fallback=True)
_ = t.ugettext

# Windows specific setup
if on_windows():
    reload(sys)
    sys.setdefaultencoding('cp1250')
    win_apps_path = os.path.normpath(os.path.join(run_directory(), '..', '..', 'win_apps'))
    os.environ['NXPROXY_BINARY'] = os.path.join(win_apps_path, 'nxproxy', 'nxproxy.exe')
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


_auth_info = None
class SSHClient(paramiko.SSHClient):
    pytis_client = True
    def connect(self, hostname, gss_auth=None, **kwargs):
        if gss_auth is None:
            gss_auth = _auth_info.get('gss_auth', False)
        return super(SSHClient, self).connect(hostname, gss_auth=gss_auth, **kwargs)


try:
    paramiko.SSHClient.pytis_client
except AttributeError:
    paramiko.SSHClient = SSHClient

class ClientException(Exception):
    pass

class AuthInfo(dict):

    _CONNECT_KEYS = ('hostname', 'port', 'username', 'password', 'key_filename', 'allow_agent',
                     'gss_auth',)
    _EXTRA_KEYS = ('_add_to_known_hosts', '_command', '_method',)

    _ARGS_PARAMETERS = (('server', 'hostname'),
                        ('remote_ssh_port', 'port'),
                        ('username', 'username'),
                        ('password', 'password'),
                        ('ssh_privkey', 'key_filename'),
                        ('add_to_known_hosts', '_add_to_known_hosts'),
                        ('command', '_command'),)

    def _check_key(self, key):
        if key not in self._CONNECT_KEYS + self._EXTRA_KEYS:
            raise KeyError(key)

    def __getitem__(self, key):
        self._check_key(key)
        return super(AuthInfo, self).__getitem__(key)

    def __setitem__(self, key, value):
        self._check_key(key)
        return super(AuthInfo, self).__setitem__(key, value)

    def update_non_empty(self, **kwargs):
        kwargs = dict([a for a in kwargs.items() if a[1]])
        return self.update(**kwargs)

    def connection_parameters(self):
        return dict([(k, v,) for k, v in self.items() if k in self._CONNECT_KEYS])

    def update_from_profile(self, profile):
        for a, p in (('host', 'hostname'),
                     ('user', 'username'),
                     ('sshport', 'port'),):
            v = profile.get(a)
            if v is not None and v != '':
                self[p] = v

    def update_args(self, args):
        for a, p in self._ARGS_PARAMETERS:
            v = self.get(p)
            if v is not None and v != '':
                setattr(args, a, unicode(v))


_auth_info = AuthInfo()


class Configuration(x2go.X2GoClientSettings):

    def __init__(self, *args, **kwargs):
        super(Configuration, self).__init__(*args, **kwargs)
        default_command = x2go.defaults.X2GO_SESSIONPROFILE_DEFAULTS['command']
        self.defaultValues['pytis'] = dict(hostname=None,
                                           password=None,
                                           port=22,
                                           key_filename=None,
                                           command=default_command)

class RpycInfo(object):

    def __init__(self, configuration, port=None, password=None):
        self._filename = os.path.join(os.path.dirname(configuration.config_files[0]), 'pytis-rpyc')
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

    class ConnectionFailed(Exception):
        pass

    def __init__(self, broker_url, logger=None, loglevel=x2go.log.loglevel_DEFAULT,
                 connection_parameters=None, default_broker_path='/usr/bin/x2gobroker',
                 add_to_known_hosts=False, **kwargs):
        match = re.match(('^(?P<protocol>(ssh|http(|s)))://'
                          '(|(?P<user>[a-zA-Z0-9_\.-]+)'
                          '(|:(?P<password>.*))@)'
                          '(?P<host>[a-zA-Z0-9\.-]+)'
                          '(|:(?P<port>[0-9]+))'
                          '($|(?P<path>/.*)$)'), broker_url)
        if match is None:
            raise Exception(_("Invalid broker address"), broker_url)
        parameters = match.groupdict()
        protocol = parameters['protocol']
        if protocol != 'ssh':
            raise Exception(_(u"Unsupported broker protocol"), protocol)
        p = connection_parameters or {}
        self._parameters = dict(
            p,
            hostname=parameters['host'],
            username=parameters['user'],
            password=parameters.get('password') or p.get('password'),
            port=int(parameters.get('port') or '22') or p.get('port'),
        )
        self._broker_path = parameters.get('path') or default_broker_path
        self._broker_profiles = None
        self._broker_profile_cache = {}
        self._ssh_client_ = None
        self._add_to_known_hosts = add_to_known_hosts
        x2go.backends.profiles.base.X2GoSessionProfiles.__init__(self, logger=logger,
                                                                 loglevel=loglevel, **kwargs)

    def __call__(self, profile_id_or_name):
        # Broken in upstream source
        _profile_id = self.check_profile_id_or_name(profile_id_or_name)
        return self.get_profile_config(profile_id=_profile_id)

    def _ssh_client(self):
        if self._ssh_client_ is None:
            self._ssh_client_ = self._make_ssh_client()
        return self._ssh_client_

    def _make_ssh_client(self):
        client = paramiko.SSHClient()
        client.load_system_host_keys()
        if self._add_to_known_hosts:
            client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        try:
            client.connect(look_for_keys=False, **self._parameters)
        except (paramiko.ssh_exception.AuthenticationException,
                paramiko.ssh_exception.SSHException,  # Happens on GSS auth failure.
                ImportError):  # Happens on GSS auth attempt with GSS libs uninstalled.
            raise self.ConnectionFailed()
        return client

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
        self._pytis_client_upgrade = {}
        SshProfiles.__init__(self, *args, **kwargs)

    def broker_listprofiles(self):
        profiles = SshProfiles.broker_listprofiles(self)
        filtered_profiles = {}
        for section, data in profiles.items():
            if section == 'pytis-client-upgrade':
                self._pytis_client_upgrade = data
            else:
                filtered_profiles[section] = data
        return filtered_profiles

    def pytis_upgrade_parameter(self, parameter, default=None):
        return self._pytis_client_upgrade.get(parameter, default)

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
            if option == 'test_installed':
                _xserver_config[option] = self._fix_win_path(os.path.join(win_apps_path, 'VcXsrv',
                                                                          'vcxsrv_pytis.exe'))
            elif option == 'run_command':
                _xserver_config[option] = self._fix_win_path(os.path.join(win_apps_path, 'VcXsrv',
                                                                          'vcxsrv_pytis.exe'))
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


x2go.client.X2GoClientXConfig = X2GoClientXConfig

class X2GoClient(x2go.X2GoClient):

    def start_xserver_pytis(self, variant=None):
        if on_windows() and variant:
            if self.client_rootdir:
                _xconfig_config_file = os.path.join(self.client_rootdir,
                                                    x2go.defaults.X2GO_XCONFIG_FILENAME)
                self.client_xconfig = x2go.client.X2GoClientXConfig(
                    config_files=[_xconfig_config_file],
                    logger=self.logger
                )
            else:
                self.client_xconfig = x2go.client.X2GoClientXConfig(logger=self.logger)
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
                        self.xserver = x2go.xserver.X2GoXServer(p_xs[0], p_xs[1],
                                                                logger=self.logger)
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
    __start_xserver_pytis = start_xserver_pytis


x2go.X2GoClient = X2GoClient
from pyhoca.cli import PyHocaCLI


class PytisClient(PyHocaCLI):

    _DEFAULT_RPYC_PORT = 10000
    _MAX_RPYC_PORT_ATTEMPTS = 100

    def __init__(self, args, params, logger=None, liblogger=None, on_update_progress=None,
                 xserver_variant=XSERVER_VARIANT_DEFAULT, **kwargs):
        self._callbacks = {}
        if on_update_progress:
            self.set_callback('update-progress', on_update_progress)
        self._update_progress(_("Client setup."))
        self.args = args
        if on_windows():
            self.args.from_stdin = None
        if logger is None:
            self._pyhoca_logger = x2go.X2GoLogger(tag='PyHocaCLI')
        else:
            self._pyhoca_logger = copy.deepcopy(logger)
            self._pyhoca_logger.tag = 'PyHocaCLI'
        for (arg, param) in (('backend_controlsession', 'control_backend'),
                             ('backend_terminalsession', 'terminal_backend'),
                             ('backend_serversessioninfo', 'info_backend'),
                             ('backend_serversessionlist', 'list_backend'),
                             ('backend_proxy', 'proxy_backend'),
                             ('backend_sessionprofiles', 'profiles_backend'),
                             ('backend_clientsettings', 'settings_backend'),
                             ('backend_clientprinting', 'printing_backend')):
            value = getattr(self.args, arg)
            if value is not None:
                kwargs[param] = value
        self._pyhoca_logger('preparing requested X2Go session', loglevel=x2go.loglevel_NOTICE, )
        x2go.X2GoClient.__init__(self, logger=liblogger, **kwargs)
        self._X2GoClient__start_xserver_pytis(variant=xserver_variant)
        self.auth_attempts = int(self.args.auth_attempts)
        self.x2go_session_hash = self._X2GoClient__register_session(**params)
        self._pytis_port_value = gevent.event.AsyncResult()
        self._pytis_password_value = gevent.event.AsyncResult()
        self._pytis_terminate = gevent.event.Event()

    def set_callback(self, callback, function):
        self._callbacks[callback] = function

    def _run_callback(self, callback, *args, **kwargs):
        try:
            function = self._callbacks[callback]
        except KeyError:
            pass
        else:
            function(*args, **kwargs)

    def _update_progress(self, *args, **kwargs):
        self._run_callback('update-progress', *args, **kwargs)

    def pytis_setup(self, s_uuid):
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

    def pytis_handle_info(self):
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

    def pytis_list_sessions(self):
        sessions = [info for info in
                    self._X2GoClient__list_sessions(self.x2go_session_hash).values()
                    if info.status == 'S']
        sessions.sort(lambda a, b: (cmp(a.username, b.username) or
                                    cmp(a.hostname, b.hostname) or
                                    cmp(b.date_created, a.date_created)))
        return sessions

    def pytis_terminate_session(self, session):
        self._X2GoClient__terminate_session(self.x2go_session_hash, session.name)

    def pytis_upgrade(self):
        upgrade_url = self.session_profiles.pytis_upgrade_parameter('command')
        match = re.match(('^(?P<protocol>(ssh|http(|s)))://'
                          '(|(?P<username>[a-zA-Z0-9_\.-]+)'
                          '(|:(?P<password>.*))@)'
                          '(?P<hostname>[a-zA-Z0-9\.-]+)'
                          '(|:(?P<port>[0-9]+))'
                          '($|/(?P<path>.*)$)'), upgrade_url)
        if match is None:
            raise Exception(_(u"Invalid broker address"), upgrade_url)
        parameters = match.groupdict()
        protocol = parameters['protocol']
        if protocol != 'ssh':
            raise Exception(_(u"Unsupported broker protocol"), protocol)
        password = parameters.get('password')
        port = int(parameters.get('port') or '22')

        _auth_info.update_non_empty(hostname=parameters['hostname'], port=port,
                                    password=password)

        path = parameters.get('path')
        client = PytisClient.pytis_ssh_connect()
        if client is None:
            # TODO: Some new dialog should be used...
            # app.info_dialog(_(u"Couldn't connect to upgrade server"), error=True)
            return
        install_directory = os.path.normpath(os.path.join(run_directory(), '..', ''))
        old_install_directory = install_directory + '.old'
        tmp_directory = tempfile.mkdtemp(prefix='pytisupgrade')
        pytis_directory = os.path.join(tmp_directory, 'pytis2go', 'pytis')
        scripts_directory = os.path.join(tmp_directory, 'pytis2go', 'scripts')
        scripts_install_dir = os.path.normpath(os.path.join(install_directory, '..', 'scripts'))
        sftp = client.open_sftp()
        f = sftp.open(path)
        tarfile.open(fileobj=f).extractall(path=tmp_directory)
        if not os.path.isdir(pytis_directory):
            # TODO: Some new dialog should be used...
            # app.info_dialog(_(u"Package unpacking failed"), error=True)
            return
        if os.path.exists(old_install_directory):
            shutil.rmtree(old_install_directory)
        shutil.move(install_directory, old_install_directory)
        shutil.move(pytis_directory, install_directory)
        shutil.rmtree(old_install_directory)
        f_config = os.path.join(os.path.expanduser('~'), '.x2goclient', 'xconfig')
        if os.access(f_config, os.W_OK):
            os.remove(f_config)
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
        # Execute supplied update procedure if exists
        updatescript = os.path.join(tmp_directory, 'updatescript.py')
        updateproc = None
        if os.access(updatescript, os.R_OK):
            sys.path.append(tmp_directory)
            try:
                import updatescript
                updateproc = getattr(updatescript, 'run')
            except:
                pass
        if updateproc:
            updateproc(version=_VERSION, path=path)
        shutil.rmtree(tmp_directory)
        # TODO: Some new dialog should be used...
        # app.info_dialog(_(u"Pytis successfully upgraded. Restart the application."))
        sys.exit(0)

    def _check_rpyc_server(self, configuration, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead):
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
            rpyc_info = RpycInfo(configuration)
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
                rpyc_info = RpycInfo(configuration, port=port,
                                     password=authenticator.password())
                rpyc_info.store()
                gevent.spawn(server.start)
                self._pytis_password_value.set(rpyc_info.password())
            gevent.sleep(1)

    def _check_ssh_tunnel(self, _configuration, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead):
        hostname = _auth_info.get('hostname')
        ssh_port = _auth_info.get('port')
        username = _auth_info.get('username')
        password = _auth_info.get('password')
        key_filename = _auth_info.get('key_filename')
        gss_auth = _auth_info.get('gss_auth')
        while True:
            while not rpyc_port.ready():
                if self._pytis_terminate.is_set():
                    return
                gevent.sleep(0.1)
            current_rpyc_port = rpyc_port.get()
            port = gevent.event.AsyncResult()
            tunnel = pytis.remote.ReverseTunnel(hostname,
                                                current_rpyc_port,
                                                ssh_port=ssh_port,
                                                ssh_forward_port=0,
                                                ssh_user=username,
                                                ssh_password=password,
                                                key_filename=key_filename,
                                                ssh_forward_port_result=port,
                                                gss_auth=gss_auth)
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

    def pytis_start_processes(self, configuration):
        # RPyC server
        rpyc_stop_queue = gevent.queue.Queue()
        rpyc_port = gevent.event.AsyncResult()
        ssh_tunnel_dead = gevent.event.Event()
        self._pytis_terminate.clear()
        args = (configuration, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead,)
        self._update_progress(_("Starting server connections."))
        gevent.spawn(self._check_rpyc_server, *args)
        self._update_progress()
        gevent.spawn(self._check_ssh_tunnel, *args)
        self._update_progress()

    def terminate_session(self, *args, **kwargs):
        try:
            return super(PytisClient, self).terminate_session(*args, **kwargs)
        finally:
            self._pytis_terminate.set()

    def new_session(self, s_hash):
        self._update_progress(_("Creating new session."))
        super(PytisClient, self).new_session(s_hash)
        if self._pytis_setup_configuration:
            self._update_progress(_("Setting up new session."))
            self.pytis_setup(s_hash)
            self._pytis_setup_configuration = False

            def info_handler():
                while self.session_ok(s_hash):
                    self.pytis_handle_info()
                    gevent.sleep(0.1)
            gevent.spawn(info_handler)
        self._run_callback('session-started')

    def resume_session(self, s_hash):
        super(PytisClient, self).resume_session(s_hash)
        self._run_callback('session-started')

    def _create_shortcut(self, broker_url, host, profile_id, profile_name, calling_script):
        import urlparse
        import winshell
        scripts_path = os.path.normpath(os.path.join(run_directory(), '..', '..', 'scripts'))
        if not os.path.isdir(scripts_path):
            return
        broker_host = urlparse.urlparse(broker_url).netloc
        username = _auth_info.get('username') or ''
        vbs_name = '%s__%s__%s.vbs' % (broker_host, host, profile_id,)
        if username:
            vbs_name = '%s__%s' % (username, vbs_name)
        vbs_path = os.path.join(scripts_path, vbs_name)
        if not os.path.exists(vbs_path):
            if calling_script:
                calling_script = os.path.join(scripts_path, os.path.basename(calling_script))
            if os.path.exists(calling_script):
                with open(calling_script, 'r') as f:
                    broker_src = f.read()
                match = re.match(('^(?P<protocol>(ssh|http(|s)))://'
                                  '(|(?P<username>[a-zA-Z0-9_\.-]+)'
                                  '(|:(?P<password>.*))@)'
                                  '(?P<hostname>[a-zA-Z0-9\.-]+)'
                                  '(|:(?P<port>[0-9]+))'
                                  '($|/(?P<path>.*)$)'), broker_url)
                parameters = match.groupdict()
                password = parameters['password'] and ":" + parameters['password'] or ''
                port = parameters['port'] and ":" + parameters['port'] or ''
                broker_url_tmpl = "--broker-url=%s://%s%s@%s%s/%s -P %s"
                profile_src = re.sub(r'(--broker-url=[^"\s]+)',
                                     broker_url_tmpl % (parameters['protocol'],
                                                        username,
                                                        password,
                                                        parameters['hostname'],
                                                        port,
                                                        parameters['path'],
                                                        profile_id),
                                     broker_src)
                profile_src = re.sub(r'--create-shortcut', r'', profile_src)
                profile_src = re.sub(r'--calling-script=', r'', profile_src)
                profile_src = re.sub(r'&\s+Wscript.ScriptFullName', r'', profile_src)
                profile_src = re.sub(r'&\s+Wscript.ScriptName', r'', profile_src)
                with open(vbs_path, 'w') as f:
                    f.write(profile_src)
        # check if vbs script was created properly
        if not os.path.exists(vbs_path):
            return
        # Check if shortcut for vbs_path allready exists
        shortcut_exists = False
        shortcut_list = [os.path.join(winshell.desktop(), d)
                         for d in os.listdir(winshell.desktop())
                         if os.path.isfile(os.path.join(winshell.desktop(), d)) and
                         os.path.splitext(d)[1].lower() == '.lnk']
        for lpath in shortcut_list:
            try:
                with winshell.shortcut(lpath) as link:
                    if link.path.lower() == vbs_path.lower():
                        shortcut_exists = True
                        break
            except Exception:
                pass
        if ((shortcut_exists or
             # TODO: some new question dialog should be used
             # not app.question_dialog(_(u"Create desktop shortcut for this session profile?")))):
             True)):
            return
        # Create shortcut on desktop
        shortcut_name = profile_name
        while True:
            shortcut_path = os.path.join(winshell.desktop(), '%s.lnk' % shortcut_name)
            if not os.path.exists(shortcut_path):
                break
            else:
                # TODO: some new text_dialog should be used
                # msg = _(u"Shortcut %s allready exists. Please, rename it:") % shortcut_name
                # new_name = app.text_dialog(msg, caption=_(u"Edit shortcut name"),
                #                            default_value=shortcut_name)
                new_name = None
                if not new_name:
                    return
                elif shortcut_name != new_name:
                    shortcut_name = new_name
        with winshell.shortcut(shortcut_path) as link:
            link.path = vbs_path
            link.description = profile_id
            link.working_directory = scripts_path
            icon_location = os.path.normpath(os.path.join(scripts_path, '..', 'icons', 'p2go.ico'))
            if os.path.exists(icon_location):
                link.icon_location = (icon_location, 0)

    @classmethod
    def pytis_ssh_connect(cls):
        if not _auth_info.get('password'):
            _auth_info['password'] = 'X'
        client = paramiko.SSHClient()
        client.load_system_host_keys()
        if _auth_info.get('_add_to_known_hosts'):
            client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        params = _auth_info.connection_parameters()
        try:
            client.connect(**params)
        except (paramiko.ssh_exception.AuthenticationException,
                paramiko.ssh_exception.SSHException,  # Happens on GSS auth failure.
                ImportError):  # Happens on GSS auth attempt with GSS libs uninstalled.
            return None
        return client


class X2GoStartAppClientAPI(object):
    """Interface between the start-up application and X2GoClient.

    It will be probably more logical to rearrange the code in the following way:

      - methods working before PytisClient instance creation can stay in a separate
        class (with a different name).

      - the method connect() would return a PytisClient instance.

      - methods working on PytisClient instance can be moved to PytisClient
        itself and called from the startup application directly

    """

    def __init__(self, args, update_progress):
        self._args = args
        self._update_progress = update_progress
        self._configuration = configuration = Configuration()
        self._xserver_variant = XSERVER_VARIANT_DEFAULT
        self._session_parameters = {}

        def cfg(*params):
            if args.broker_url is None:
                try:
                    return configuration.get_value(*params)
                except ClientException:
                    pass
            return None
        self._update_session_parameters(
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
            pass
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
            self._update_session_parameters(
                profile_name=args.session_profile,
                **parameters
            )
        else:
            # Set up the manually configured X2Go session.
            self._update_session_parameters(
                profile_id=args.session_profile,
                profile_name='Pyhoca-Client_Session',
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
        _auth_info['_add_to_known_hosts'] = args.add_to_known_hosts
        quit_signal = signal.SIGTERM if on_windows() else signal.SIGQUIT
        gevent.signal(quit_signal, gevent.kill)

    def _update_session_parameters(self, **kwargs):
        self._session_parameters.update(**kwargs)
        parameters = self._session_parameters
        _auth_info.update(
            hostname=parameters['server'],
            port=parameters['port'],
            username=parameters['username'],
            password=parameters['password'],
            key_filename=parameters['key_filename'],
            gss_auth=parameters['gss_auth'],
            look_for_keys=parameters['look_for_keys'],
            allow_agent=parameters['allow_agent'],
        )
        _auth_info.update_args(self._args)

    def authentication_methods(self):
        import socket
        s = socket.socket()
        s.connect((_auth_info.get('hostname'), _auth_info.get('port')))
        transport = paramiko.Transport(s)
        transport.connect()
        try:
            transport.auth_none('')
        except paramiko.ssh_exception.BadAuthenticationType as e:
            methods = e.allowed_types
        transport.close()
        s.close()
        return methods

    def connect(self, username, gss_auth=False, key_filename=None, password=None):
        """Arguments:

          username -- user's login name as a string or unicode
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
        self._update_session_parameters(
            username=username,
            gss_auth=gss_auth,
            key_filename=key_filename,
            password=password,
            look_for_keys=key_filename is not None,
            allow_agent=True,
        )
        if PytisClient.pytis_ssh_connect():
            # Clean tempdir.
            tempdir = tempfile.gettempdir()
            for f in os.listdir(tempdir):
                if f.startswith('pytistmp'):
                    try:
                        os.remove(os.path.join(tempdir, f))
                    except:
                        pass
            # Create client
            self._client = client = PytisClient(self._args, self._session_parameters,
                                                use_cache=False,
                                                start_xserver=False,
                                                xserver_variant=self._xserver_variant,
                                                loglevel=x2go.log.loglevel_DEBUG,
                                                on_update_progress=self._update_progress)
            session = client.session_registry(client.x2go_session_hash)
            session.sshproxy_params['key_filename'] = key_filename
            session.sshproxy_params['look_for_keys'] = False
            client.pytis_start_processes(self._configuration)
            client._pytis_setup_configuration = True
            #self._update_progress(_("Authenticating."))
            client.authenticate()
            return True
        else:
            return False

    def search_key_files(self, username):
        def key_acceptable(key_filename):
            public_key_filename = key_filename + '.pub'
            acceptable = True
            if os.access(public_key_filename, os.R_OK):
                try:
                    acceptable = pytis.remote.public_key_acceptable(
                        _auth_info.get('hostname'),
                        username,
                        public_key_filename,
                        port=_auth_info.get('port'))
                except:
                    pass
            return acceptable
        return [path for path in [os.path.join(os.path.expanduser('~'), '.ssh', name)
                                  for name in ('id_rsa', 'id_dsa', 'id_ecdsa',)]
                if os.access(path, os.R_OK) and key_acceptable(path)]

    def check_key_password(self, key_filename, password):
        for handler in (paramiko.RSAKey, paramiko.DSSKey, paramiko.ECDSAKey):
            try:
                handler.from_private_key_file(key_filename, password)
                return True
            except:
                continue
        return False

    def list_profiles(self, **kwargs):
        try:
            profiles = PytisSshProfiles(logger=x2go.X2GoLogger(tag='PyHocaCLI'),
                                        broker_url=self._args.broker_url,
                                        add_to_known_hosts=self._args.add_to_known_hosts,
                                        connection_parameters=kwargs,
                                        broker_password=self._args.broker_password)
        except PytisSshProfiles.ConnectionFailed:
            return None
        self._profiles = profiles
        return profiles

    def select_profile(self, profile_id):
        profile = self._profiles.broker_selectsession(profile_id)
        rootless = profile.get('rootless', True)
        if not rootless or int(rootless) == 0:
            self._xserver_variant = 'VcXsrv_pytis_desktop'
        parameters = self._profiles.to_session_params(profile_id)
        if isinstance(parameters['server'], list):
            parameters['server'] = parameters['server'][0]
        self._update_session_parameters(**parameters)

    def needs_upgrade(self):
        # We can use only supported parameters from session_profiles
        # So we'll use 'name' for the version and 'command' for url
        version = self._profiles.pytis_upgrade_parameter('name')
        if version and version > _VERSION:
            upgrade_url = self._profiles.pytis_upgrade_parameter('command')
            if upgrade_url:
                return True
        return False

    def list_sessions(self):
        return self._client.pytis_list_sessions()

    def terminate_session(self, session):
        return self._client.pytis_terminate_session(session)

    def resume_session(self, session, callback=None):
        self._args.resume = session.name
        self._args.new = False
        if callback:
            self._client.set_callback('session-started', callback)
        self._client.MainLoop()

    def start_new_session(self, callback=None):
        self._args.resume = None
        self._args.new = True
        if callback:
            self._client.set_callback('session-started', callback)
        self._client.MainLoop()

# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
