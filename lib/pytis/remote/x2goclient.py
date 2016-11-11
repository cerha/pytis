#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012, 2014, 2015, 2016 Brailcom, o.p.s.
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

    def __init__(self, broker_url, logger=None, loglevel=x2go.log.loglevel_DEFAULT,
                 **kwargs):
        match = re.match(('^(?P<protocol>(ssh|http(|s)))://'
                          '(|(?P<user>[a-zA-Z0-9_\.-]+)'
                          '(|:(?P<password>.*))@)'
                          '(?P<host>[a-zA-Z0-9\.-]+)'
                          '(|:(?P<port>[0-9]+))'
                          '($|/(?P<path>.*)$)'), broker_url)
        if match is None:
            raise Exception(_("Invalid broker address"), broker_url)
        parameters = match.groupdict()
        protocol = parameters['protocol']
        if protocol != 'ssh':
            raise Exception(_(u"Unsupported broker protocol"), protocol)
        self._parameters = p = {}
        p['password'] = parameters.get('password')
        p['port'] = int(parameters.get('port') or '22')
        p['hostname'] = parameters['host']
        p['username'] = parameters['user']
        p['path'] = parameters.get('path')
        self._broker_profiles = None
        self._broker_profile_cache = {}
        self._ssh_client_ = None
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
        parameters = copy.copy(self._parameters)
        try:
            del parameters['path']
        except KeyError:
            pass
        client = paramiko.SSHClient()
        client.load_system_host_keys()
        try:
            client.connect(look_for_keys=False, **parameters)
        except paramiko.ssh_exception.AuthenticationException:
            return None
        return client

    def _broker_path(self):
        return os.path.join('/', self._parameters['path'] or '/usr/bin/x2gobroker')

    def broker_listprofiles(self):
        if self._broker_profiles is not None:
            return self._broker_profiles
        client = self._ssh_client()
        if client is None:
            return {}
        sessions = ''
        text = ''
        in_config = False
        command = "%s --task listsessions" % (self._broker_path(),)
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
        self._broker_profiles = data = {}
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
            data[session_name] = session_data
        return data

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
        command = "%s --task selectsession --sid %s" % (self._broker_path(), profile_id)
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

    def _make_ssh_client(self):
        parameters = dict([p for p in self._parameters.items() if p != 'path' and p[1]])
        _auth_info.update(**parameters)
        return PytisClient.pytis_ssh_connect()

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
import pyhoca.cli
from pyhoca.cli import runtime_error


class PytisClient(pyhoca.cli.PyHocaCLI):

    _DEFAULT_RPYC_PORT = 10000
    _MAX_RPYC_PORT_ATTEMPTS = 100

    def __init__(self, args, logger=None, liblogger=None, **kwargs):
        import pprint
        #app.update_progress(_("Client setup."))
        ssh_known_hosts_filename = os.path.join(x2go.LOCAL_HOME, x2go.X2GO_SSH_ROOTDIR,
                                                'known_hosts')
        #
        self.args = args
        if on_windows():
            self.args.from_stdin = None
        if logger is None:
            self._pyhoca_logger = x2go.X2GoLogger(tag='PyHocaCLI')
        else:
            self._pyhoca_logger = copy.deepcopy(logger)
            self._pyhoca_logger.tag = 'PyHocaCLI'
        _backend_kwargs = copy.copy(kwargs)
        if self.args.backend_controlsession is not None:
            _backend_kwargs['control_backend'] = self.args.backend_controlsession
        if self.args.backend_terminalsession is not None:
            _backend_kwargs['terminal_backend'] = self.args.backend_terminalsession
        if self.args.backend_serversessioninfo is not None:
            _backend_kwargs['info_backend'] = self.args.backend_serversessioninfo
        if self.args.backend_serversessionlist is not None:
            _backend_kwargs['list_backend'] = self.args.backend_serversessionlist
        if self.args.backend_proxy is not None:
            _backend_kwargs['proxy_backend'] = self.args.backend_proxy
        if self.args.backend_sessionprofiles is not None:
            _backend_kwargs['profiles_backend'] = self.args.backend_sessionprofiles
        if self.args.backend_clientsettings is not None:
            _backend_kwargs['settings_backend'] = self.args.backend_clientsettings
        if self.args.backend_clientprinting is not None:
            _backend_kwargs['printing_backend'] = self.args.backend_clientprinting
        self._pyhoca_logger('preparing requested X2Go session', loglevel=x2go.loglevel_NOTICE, )
        broker_url = self.args.broker_url
        ssh_p = args.broker_url and args.broker_url.lower().startswith('ssh')
        if not ssh_p:
            _backend_kwargs['broker_url'] = broker_url
            _backend_kwargs['broker_password'] = self.args.broker_password
        x2go.X2GoClient.__init__(self, logger=liblogger, **_backend_kwargs)
        if not self.args.broker_url:
            # Start xserver with default variant
            self._X2GoClient__start_xserver_pytis(variant=XSERVER_VARIANT_DEFAULT)
        # Let's substitute unimplemented sshbroker backend
        if self.args.broker_url is not None and ssh_p:
            self.profiles_backend = PytisSshProfiles
        self.session_profiles = self.profiles_backend(logger=self.logger, broker_url=broker_url,
                                                      broker_password=self.args.broker_password)
        # TODO: Check for upgrade was originally here
        self.auth_attempts = int(self.args.auth_attempts)

        # Examine profiles
        # TODO: Printing profiles list on --list-profiles was originally here (with exit).
        if self.args.session_profile and not args.broker_url:
            _cmdlineopt_to_sessionopt = {
                'command': 'cmd',
                'kb_layout': 'kblayout',
                'kb_type': 'kbtype',
                'sound': 'snd_system',
                'ssh_privkey': 'key_filename',
                'server': 'hostname',
                'remote_ssh_port': 'port',
            }
            # override session profile options by option values from the arg parser
            kwargs = {}
            if hasattr(self.args, 'parser'):
                for a, v in self.args._get_kwargs():
                    if v != self.args.parser.get_default(a):
                        try:
                            kwargs[_cmdlineopt_to_sessionopt[a]] = v
                        except KeyError:
                            kwargs[a] = v
            # setup up the session profile based X2Go session
            self.x2go_session_hash = self._X2GoClient__register_session(
                profile_name=self.args.session_profile,
                known_hosts=ssh_known_hosts_filename,
                **kwargs
            )

        else:
            # Select session
            profile_id = self.args.session_profile
            profile_name = 'Pyhoca-Client_Session'
            if args.broker_url is not None:
                data = [(k, v['name'],) for k, v in profiles.broker_listprofiles().items()]
                data.sort(key=lambda tup: tup[1])
                if not profile_id:
                    choices = [d[1] for d in data]
                    answer = app.choice_dialog_index(_(u"Select session"), choices=choices)
                    profile_id = answer is not None and data[answer][0].rstrip()
                profile_name = None
                if not profile_id:
                    app.close_progress_dialog()
                    raise Exception(_(u"No session selected."))
                profile = profiles.broker_selectsession(profile_id)
                app.update_progress(_("Opening selected profile."))

                _auth_info.update_from_profile(profile)
                _auth_info.update_args(self.args)

                # We have to check, if we are able to connect to application server
                # with current connection_parameters.
                test_client = PytisClient.pytis_ssh_connect()
                if test_client:
                    auth_password = _auth_info.get('password')
                    if auth_password and auth_password != self.args.password:
                        self.args.password = auth_password
                    test_client.close()
                else:
                    raise Exception(_("Couldn't connect to application server %s." % self.args.server))
                params = profiles.to_session_params(profile_id)
                # Start xserver according to the selected profile
                rootless = profile.get('rootless', True)
                if not rootless or int(rootless) == 0:
                    rootless = False
                if not rootless:
                    variant = 'VcXsrv_pytis_desktop'
                else:
                    variant = 'VcXsrv_pytis'
                self._X2GoClient__start_xserver_pytis(variant=variant)
                self.x2go_session_hash = self._X2GoClient__register_session(
                    **params)
                if on_windows() and args.create_shortcut:
                    app.update_progress(_("Checking desktop shortcut."))
                    self._create_shortcut(broker_url, args.server, profile_id,
                                          params['profile_name'], args.calling_script)
            else:
                # setup up the manually configured X2Go session
                self.x2go_session_hash = self._X2GoClient__register_session(
                    args.server,
                    port=int(self.args.remote_ssh_port),
                    known_hosts=ssh_known_hosts_filename,
                    username=(self.args.username or app.username_dialog),
                    key_filename=self.args.ssh_privkey,
                    add_to_known_hosts=self.args.add_to_known_hosts,
                    profile_id=profile_id,
                    profile_name=profile_name,
                    session_type=self.args.session_type,
                    link=self.args.link,
                    geometry=self.args.geometry,
                    pack=self.args.pack,
                    cache_type='unix-kde',
                    kblayout=self.args.kbd_layout,
                    kbtype=self.args.kbd_type,
                    snd_system=self.args.sound,
                    printing=self.args.printing,
                    print_action=self.args.print_action,
                    print_action_args=self.args.print_action_args,
                    share_local_folders=self.args.share_local_folders,
                    allow_share_local_folders=True,
                    cmd=self.args.command)
        self._pytis_port_value = gevent.event.AsyncResult()
        self._pytis_password_value = gevent.event.AsyncResult()
        self._pytis_terminate = gevent.event.Event()

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

    def pytis_list_profiles(self):
        profiles = self._X2GoClient__get_profiles()
        if self.args.session_profile and not profiles.has_profile(self.args.session_profile):
            self._runtime_error('no such session profile of name: %s' % (self.args.session_profile),
                                exitcode=31)
        return profiles

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

    def pytis_needs_upgrade(self):
        # We can use only supported parameters from session_profiles
        # So we'll use 'name' for the version and 'command' for url
        version = self.session_profiles.pytis_upgrade_parameter('name')
        if version and version > _VERSION:
            upgrade_url = self.session_profilesprofiles.pytis_upgrade_parameter('command')
            if upgrade_url:
                return True
        return False

    def pytis_upgrade(self):
        upgrade_url = self.session_profilesprofiles.pytis_upgrade_parameter('command')
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
            app.info_dialog(_(u"Couldn't connect to upgrade server"), error=True)
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
            app.info_dialog(_(u"Package unpacking failed"), error=True)
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
        app.info_dialog(_(u"Pytis successfully upgraded. Restart the application."))
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
        # TODO: app.update_progress(_("Starting server connections."))
        gevent.spawn(self._check_rpyc_server, *args)
        #app.update_progress()
        gevent.spawn(self._check_ssh_tunnel, *args)
        #app.update_progress()

    def terminate_session(self, *args, **kwargs):
        try:
            return super(PytisClient, self).terminate_session(*args, **kwargs)
        finally:
            self._pytis_terminate.set()

    def new_session(self, s_hash):
        #app.update_progress(_("Creating new session."))
        super(PytisClient, self).new_session(s_hash)
        if self._pytis_setup_configuration:
            #app.update_progress(_("Setting up new session."))
            self.pytis_setup(s_hash)
            self._pytis_setup_configuration = False
            def info_handler():
                while self.session_ok(s_hash):
                    self.pytis_handle_info()
                    gevent.sleep(0.1)
            gevent.spawn(info_handler)
        #app.close_progress_dialog()

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
                         if os.path.isfile(os.path.join(winshell.desktop(), d))
                         and os.path.splitext(d)[1].lower() == '.lnk']
        for lpath in shortcut_list:
            try:
                with winshell.shortcut(lpath) as link:
                    if link.path.lower() == vbs_path.lower():
                        shortcut_exists = True
                        break
            except Exception:
                pass
        if ((shortcut_exists or
             not app.question_dialog(_(u"Create desktop shortcut for this session profile?")))):
            return
        # Create shortcut on desktop
        shortcut_name = profile_name
        while True:
            shortcut_path = os.path.join(winshell.desktop(), '%s.lnk' % shortcut_name)
            if not os.path.exists(shortcut_path):
                break
            else:
                msg = _(u"Shortcut %s allready exists. Please, rename it:") % shortcut_name
                new_name = app.text_dialog(msg, caption=_(u"Edit shortcut name"),
                                           default_value=shortcut_name)
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
        try:
            client.connect(**_auth_info.connection_parameters())
        except (paramiko.ssh_exception.AuthenticationException,
                paramiko.ssh_exception.SSHException, # Happens on GSS auth failure.
                ImportError): # Happens on GSS auth attempt with GSS libs uninstalled.
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

    def __init__(self, args):
        self._args = args
        for param, arg in (
                ('hostname', 'server'),
                ('port', 'remote_ssh_port'),
                ('username', 'username'),
                ('password', 'password'),
                ('key_filename', 'ssh_privkey'),
                ('_add_to_known_hosts', 'add_to_known_hosts'),
                # ('command', 'command'),
        ):
            v = getattr(args, arg)
            if v is not None and v != '':
                _auth_info[param] = int(v) if param == 'port' else v
        # Read in configuration
        self._configuration = Configuration()
        if args.broker_url is None:
            for param, config in (('hostname', ('pytis', 'hostname')),
                                  ('port', ('pytis', 'port', int)),
                                  # ('command', ('pytis', 'command')),
                                  ('password', ('pytis', 'password')),
                                  ('key_filename', ('pytis', 'key_filename')),):
                if not _auth_info.get(param):
                    try:
                        _auth_info[param] = self._configuration.get_value(*config)
                    except ClientException:
                        pass
        quit_signal = signal.SIGTERM if on_windows() else signal.SIGQUIT
        gevent.signal(quit_signal, gevent.kill)

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
        _auth_info.update(username=username,
                          gss_auth=gss_auth,
                          key_filename=key_filename,
                          password=password,
                          look_for_keys=key_filename is not None)
        if not PytisClient.pytis_ssh_connect():
            return False
        # Clean tempdir.
        tempdir = tempfile.gettempdir()
        for f in os.listdir(tempdir):
            if f.startswith('pytistmp'):
                try:
                    os.remove(os.path.join(tempdir, f))
                except:
                    pass
        # Create client
        _auth_info.update_args(self._args)
        self._client = client = PytisClient(self._args,
                                            use_cache=False,
                                            start_xserver=False,
                                            loglevel=x2go.log.loglevel_DEBUG)
        session = client.session_registry(client.x2go_session_hash)
        session.sshproxy_params['key_filename'] = _auth_info.get('key_filename')
        session.sshproxy_params['look_for_keys'] = False
        client.pytis_start_processes(self._configuration)
        #TODO: app.update_progress(_("Authenticating."))
        client._pytis_setup_configuration = True
        client.authenticate()
        return True

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

    def default_username(self):
        return None

    def default_profile(self):
        return None

    def list_profiles(self):
        return self._client.pytis_list_profiles()

    def list_sessions(self):
        return self._client.pytis_list_sessions()

    def terminate_session(self, session):
        return self._client.pytis_terminate_session(session)

    def resume_session(self, session):
        self._args.resume = session.name
        self._args.new = False
        self._client.MainLoop()

    def start_new_session(self):
        self._args.resume = None
        self._args.new = True
        self._client.MainLoop()

# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
