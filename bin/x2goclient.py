#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012, 2014, 2015 Brailcom, o.p.s.
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

from __future__ import unicode_literals

# ATTENTION: This should be updated on each code change.
_VERSION = '2015-08-31 15:49'

XSERVER_VARIANT = 'VcXsrv_shipped'

import gevent.monkey
gevent.monkey.patch_all()

import gettext
import os
import platform
import sys

def on_windows():
    return platform.system() == 'Windows'

def run_directory():
    return sys.path[0]

# Windows specific setup
if on_windows():
    reload(sys)
    sys.setdefaultencoding('cp1250')
    win_apps_path = os.path.normpath(os.path.join(run_directory(), '..', '..', 'win_apps'))
    os.environ[str('NXPROXY_BINARY')] = str(os.path.join(win_apps_path, 'nxproxy', 'nxproxy.exe'))
    # Set locale language
    import ctypes
    lcid_user = ctypes.windll.kernel32.GetUserDefaultLCID()
    lcid_system = ctypes.windll.kernel32.GetSystemDefaultLCID()
    if lcid_user:
        lcid = lcid_user
    else:
        lcid = lcid_system
    import locale
    os.environ[str("LANGUAGE")] = locale.windows_locale.get(lcid)

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

t = gettext.translation('pytis-x2go',
                        os.path.normpath(os.path.join(run_directory(), '..', 'translations')),
                        fallback=True)
_ = t.ugettext

import wx

class App(wx.App):

    def __init__(self, *args, **kwargs):
        self._pytis_progress = None
        self._pytis_progress_value = 1
        self._pytis_progress_max = None
        super(App, self).__init__(*args, **kwargs)

    def info_dialog(self, message, caption='', error=False):
        self.hide_progress_dialog()
        style = wx.OK
        if error:
            style = style | wx.ICON_ERROR
        dlg = wx.MessageDialog(None, message, caption=caption, style=style)
        if not dlg.HasFlag(wx.STAY_ON_TOP):
            dlg.ToggleWindowStyle(wx.STAY_ON_TOP)
        # Raise should not be necessary, but there was a problem with focus
        # when used on windows
        dlg.Raise()
        dlg.ShowModal()
        dlg.Destroy()
        app.Yield()
        self.show_progress_dialog()

    def question_dialog(self, question, caption=''):
        self.hide_progress_dialog()
        style = wx.YES_NO
        dlg = wx.MessageDialog(None, question, caption=caption, style=style)
        if not dlg.HasFlag(wx.STAY_ON_TOP):
            dlg.ToggleWindowStyle(wx.STAY_ON_TOP)
        # Raise should not be necessary, but there was a problem with focus
        # when used on windows
        dlg.Raise()
        if dlg.ShowModal() == wx.ID_YES:
            answer = True
        else:
            answer = False
        dlg.Destroy()
        self.Yield()
        self.show_progress_dialog()
        return answer

    def text_dialog(self, prompt, caption='', default_value='', password=False):
        self.hide_progress_dialog()
        style = wx.TextEntryDialogStyle
        if password:
            style = style | wx.TE_PASSWORD
            dlg = wx.PasswordEntryDialog(None, prompt, caption=caption, style=style)
        else:
            dlg = wx.TextEntryDialog(None, prompt, caption=caption, defaultValue=default_value,
                                     style=style)
        if not dlg.HasFlag(wx.STAY_ON_TOP):
            dlg.ToggleWindowStyle(wx.STAY_ON_TOP)
        # Raise should not be necessary, but there was a problem with focus
        # when used on windows
        dlg.Raise()
        if dlg.ShowModal() == wx.ID_OK:
            answer = dlg.GetValue()
        else:
            answer = None
        dlg.Destroy()
        self.Yield()
        self.show_progress_dialog()
        return answer

    def username_dialog(self):
        return self.text_dialog(_("User name"), default_value=x2go.defaults.CURRENT_LOCAL_USER)

    def choice_dialog(self, prompt, choices, index=False):
        self.hide_progress_dialog()
        style = wx.CHOICEDLG_STYLE
        dlg = wx.SingleChoiceDialog(None, prompt, prompt, choices=choices, style=style)
        if not dlg.HasFlag(wx.STAY_ON_TOP):
            dlg.ToggleWindowStyle(wx.STAY_ON_TOP)
        # Raise should not be necessary, but there was a problem with focus
        # when used on windows
        dlg.Raise()
        if dlg.ShowModal() == wx.ID_OK:
            if index:
                answer = dlg.GetSelection()
                if answer == -1:
                    answer = None
            else:
                answer = dlg.GetStringSelection()
        else:
            answer = None
        dlg.Destroy()
        self.Yield()
        self.show_progress_dialog()
        return answer

    def choice_dialog_index(self, prompt, choices):
        return self.choice_dialog(prompt, choices, index=True)

    def file_dialog(self, prompt, directory=None):
        self.hide_progress_dialog()
        kwargs = {}
        if directory is not None:
            kwargs['default_path'] = directory
        answer = wx.FileSelector(prompt, **kwargs)
        self.Yield()
        self.show_progress_dialog()
        return answer or None

    def progress_dialog(self, title, message, maximum):
        self._pytis_progress_max = maximum
        self._pytis_progress_title = title
        self._pytis_progress_message = message
        self._pytis_progress = wx.ProgressDialog(title, message, maximum)
        self.Yield()

    def update_progress_dialog(self, value=None, title=None, message=None):
        if value:
            self._pytis_progress_value = value
        else:
            if self._pytis_progress_value < self._pytis_progress_max - 1:
                self._pytis_progress_value += 1
        if title:
            self._pytis_progress_title = title
        if message:
            self._pytis_progress_message = message
        if not self._pytis_progress:
            self.progress_dialog(self._pytis_progress_title, self._pytis_progress_message,
                                 self._pytis_progress_max)
            self.Yield()
        else:
            self._pytis_progress.Show()
        self._pytis_progress.Update(self._pytis_progress_value, self._pytis_progress_message)
        self.Yield()

    def hide_progress_dialog(self):
        if on_windows():
            return self.close_progress_dialog()
        if self._pytis_progress is not None:
            self._pytis_progress.Hide()

    def show_progress_dialog(self):
        if self._pytis_progress_max is not None:
            self.update_progress_dialog()

    def close_progress_dialog(self):
        if self._pytis_progress is not None:
            self._pytis_progress.Update(self._pytis_progress_max)
            self._pytis_progress = None
            self.Yield()

app = App()

app.progress_dialog(_("Starting application"), _("Initializing libraries. Please wait..."), 20)

import argparse
import copy
import re
import shutil
import signal
import tarfile
import tempfile
import types

sys.path.append(os.path.normpath(os.path.join(run_directory(), '..', 'lib')))

import gevent
import gevent.event
import gevent.queue
import paramiko

app.update_progress_dialog()

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

import pyhoca.cli
from pyhoca.cli import runtime_error
import rpyc
import x2go
import x2go.backends.profiles.base
import x2go.client
import x2go.defaults
import x2go.log
import x2go.xserver
import pytis.remote

app.update_progress_dialog(message=_("Initializing application. Please wait..."))

_NONE = object()

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

    def connect_parameters(self):
        return dict([(k, v,) for k, v in self.items() if k in self._CONNECT_KEYS])

    def update_from_profile(self, profile):
        for a, p in (('host', 'hostname'),
                     ('user', 'username'),
                     ('sshport', 'port'),):
            v = profile.get(a)
            if v is not None and v != '':
                self[p] = v

    def update_from_args(self, args):
        for a, p in self._ARGS_PARAMETERS:
            v = getattr(args, a)
            if v is not None and v != '':
                if p == 'port':
                    v = int(v)
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
            raise ClientException(_("Can't read RPyC info file: %s") % (self._filename,), e)
        self._port = int(port)
        self._password = password

    def store(self):
        try:
            f = open(self._filename, 'w')
            f.write('%s\n%s' % (self._port, self._password,))
            f.close()
        except Exception as e:
            raise ClientException(_("Error when writing RPyC info file: %s") % (self._filename,), e)

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
            raise Exception(_("Unsupported broker protocol"), protocol)
        self._parameters = p = {}
        p['password'] = parameters.get('password')
        p['port'] = int(parameters.get('port') or '22')
        p['hostname'] = parameters['host']
        p['username'] = parameters['user'] or app.username_dialog()
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
        if not xserver_name == XSERVER_VARIANT:
            return super(X2GoClientXConfig, self).get_xserver_config(xserver_name)
        _xserver_config = {}
        _changed = False
        for option in self.iniConfig.options(xserver_name):
            if option == 'test_installed':
                _xserver_config[option] = self._fix_win_path(os.path.join(win_apps_path, 'VcXsrv',
                                                                          'vcxsrv.exe'))
            elif option == 'run_command':
                _xserver_config[option] = self._fix_win_path(os.path.join(win_apps_path, 'VcXsrv',
                                                                          'vcxsrv.exe'))
            elif option == 'parameters':
                parameters = self.get(xserver_name, option,
                                      key_type=self.get_type(xserver_name, option))
                if '-notrayicon' in parameters:
                    parameters.remove('-notrayicon')
                _xserver_config[option] = parameters
            else:
                try:
                    _xserver_config[option] = self.get(xserver_name, option,
                                                       key_type=self.get_type(xserver_name, option))
                except KeyError:
                    pass
            if self.get_value(XSERVER_VARIANT, option) != _xserver_config[option]:
                _changed = True
                self.update_value(XSERVER_VARIANT, option, _xserver_config[option])
        if _changed:
            self.write_user_config = True
            self.write()
        return _xserver_config

x2go.client.X2GoClientXConfig = X2GoClientXConfig


class PytisClient(pyhoca.cli.PyHocaCLI):

    _DEFAULT_RPYC_PORT = 10000
    _MAX_RPYC_PORT_ATTEMPTS = 100

    def __init__(self, args, logger=None, liblogger=None, **kwargs):
        import pprint
        app.update_progress_dialog(message=_("Client setup. Please wait..."))
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
        # Let's substitute unimplemented sshbroker backend
        if self.args.broker_url is not None and ssh_p:
            self.profiles_backend = PytisSshProfiles
        self.session_profiles = self.profiles_backend(logger=self.logger, broker_url=broker_url,
                                                      broker_password=self.args.broker_password)
        # Check for upgrade
        app.update_progress_dialog(message=_("Checking for new client version. Please wait..."))
        if self.args.broker_url is not None:
            profiles = self.session_profiles
            # We can use only supported parameters from session_profiles
            # So we'll use 'name' for the version and 'command' for url
            version = profiles.pytis_upgrade_parameter('name')
            if version and version > _VERSION:
                p = profiles.pytis_upgrade_parameter
                upgrade_url = p('command')
                if upgrade_url:
                    if app.question_dialog(_("New pytis client version available. Install?")):
                        self._pytis_upgrade(upgrade_url)
        _profiles = self._X2GoClient__get_profiles()
        if self.args.session_profile and not _profiles.has_profile(self.args.session_profile):
            self._runtime_error('no such session profile of name: %s' % (self.args.session_profile),
                                exitcode=31)
        self.auth_attempts = int(self.args.auth_attempts)
        # Examine profiles
        app.update_progress_dialog(message=_("Checking sessions and profiles. Please wait..."))
        if args.list_profiles:
            _session_profiles = self._X2GoClient__get_profiles()
            # retrieve a session list
            print
            print "Available X2Go session profiles"
            print "==============================="
            if ((hasattr(_session_profiles, 'config_files') and
                 _session_profiles.config_files is not None)):
                print "configuration files: %s" % _session_profiles.config_files
            if ((hasattr(_session_profiles, 'user_config_file') and
                 _session_profiles.user_config_file is not None)):
                print "user configuration file: %s" % _session_profiles.user_config_file
            if ((hasattr(_session_profiles, 'broker_url') and
                 _session_profiles.broker_url is not None)):
                print "X2Go Session Broker URL: %s" % _session_profiles.broker_url
            print
            for _profile_id in _session_profiles.profile_ids:
                _profile_config = _session_profiles.get_profile_config(_profile_id)
                _session_params = _session_profiles.to_session_params(_profile_id)
                print 'Profile ID: %s' % _profile_id
                print 'Profile Name: %s' % _session_params['profile_name']
                print 'Profile Configuration:'
                pprint.pprint(_profile_config)
                print 'Derived session parameters:'
                pprint.pprint(_session_params)
                print
            # done
            sys.exit(0)
        elif self.args.session_profile and not args.broker_url:
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
                    answer = app.choice_dialog_index(_("Select session"), choices=choices)
                    profile_id = answer is not None and data[answer][0].rstrip()
                profile_name = None
                if not profile_id:
                    app.close_progress_dialog()
                    raise Exception(_("No session selected."))
                profile = profiles.broker_selectsession(profile_id)
                app.update_progress_dialog(message=_("Opening selected profile. Please wait..."))
                _auth_info.update_from_profile(profile)
                _auth_info.update_args(self.args)
                params = profiles.to_session_params(profile_id)
                self.x2go_session_hash = self._X2GoClient__register_session(
                    **params)
                if on_windows() and args.create_shortcut:
                    app.update_progress_dialog(
                        message=_("Checking desktop shortcut. Please wait..."))
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

    def _check_rpyc_server(self, configuration, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead):
        import pytis.remote.pytisproc as pytisproc
        process = None
        while True:
            if self._pytis_terminate.is_set():
                if process is not None:
                    process.kill()
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
                if running and process is not None:
                    process.kill()
                running = False
            # Maybe the instance was started by another process so we must set
            # the parameters.
            if running and not rpyc_port.ready():
                rpyc_port.set(port)
                rpyc_info.store()
                self._pytis_password_value.set(rpyc_info.password())
            # If no running RPyC instance was found then start one
            if not running:
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
                    raise ClientException(_("No free port found for RPyC in the range %s-%s") %
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
                                                current_rpyc_port, ssh_forward_port=0,
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
        app.update_progress_dialog(message=_("Starting server connections. Please wait..."))
        gevent.spawn(self._check_rpyc_server, *args)
        app.update_progress_dialog()
        gevent.spawn(self._check_ssh_tunnel, *args)
        app.update_progress_dialog()

    def terminate_session(self, *args, **kwargs):
        try:
            return super(PytisClient, self).terminate_session(*args, **kwargs)
        finally:
            self._pytis_terminate.set()

    @classmethod
    def _ssh_server_methods(class_, host, port):
        import socket
        s = socket.socket()
        s.connect((host, port))
        transport = paramiko.Transport(s)
        transport.connect()
        try:
            transport.auth_none('')
        except paramiko.ssh_exception.BadAuthenticationType as e:
            methods = e.allowed_types
        transport.close()
        s.close()
        return methods

    @classmethod
    def pytis_ssh_connect(class_):
        if not _auth_info.get('password'):
            _auth_info['password'] = 'X'
        client = paramiko.SSHClient()
        client.load_system_host_keys()
        if _auth_info.get('_add_to_known_hosts'):
            client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        selected_method = None
        # Test ssh agent first
        connect_parameters = _auth_info.connect_parameters()
        connect_parameters['key_filename'] = None
        try:
            client.connect(look_for_keys=False, **connect_parameters)
            _auth_info['key_filename'] = None
            return client
        except paramiko.ssh_exception.AuthenticationException:
            pass
        # ssh agent not available, try GSS-API (Kerberos)
        try:
            client.connect(look_for_keys=False, gss_auth=True, **connect_parameters)
            _auth_info['gss_auth'] = True
            return client
        except paramiko.ssh_exception.SSHException:
            pass
        # Other authentication methods
        methods = class_._ssh_server_methods(_auth_info['hostname'], _auth_info.get('port', 22))
        key_handlers = (paramiko.RSAKey, paramiko.DSSKey, paramiko.ECDSAKey,)
        key_files = []
        for name in ('id_rsa', 'id_dsa', 'id_ecdsa',):
            f = os.path.join(os.path.expanduser('~'), '.ssh', name)
            if os.access(f, os.R_OK):
                key_files.append(f)
        def ok_password(key_filename, password=''):
            for h in key_handlers:
                try:
                    h.from_private_key_file(key_filename, password)
                    return True
                except:
                    pass
            return False
        def key_password(key_filename, password=''):
            while True:
                if ok_password(key_filename, password):
                    return password
                password = app.text_dialog(_("Password key for %s") % (key_filename,),
                                           password=True)
                if password is None:
                    return None
        def key_acceptable(key_filename):
            public_key_filename = key_filename + '.pub'
            acceptable = True
            if os.access(public_key_filename, os.R_OK):
                try:
                    acceptable = pytis.remote.public_key_acceptable(
                        connect_parameters['hostname'],
                        connect_parameters['username'],
                        public_key_filename,
                        port=connect_parameters['port'])
                except:
                    pass
            return acceptable
        while True:
            connect_parameters = _auth_info.connect_parameters()
            try:
                client.connect(look_for_keys=False, **connect_parameters)
                break
            except paramiko.ssh_exception.AuthenticationException:
                if selected_method != 'password' and 'publickey' in methods:
                    key_filename = _auth_info.get('key_filename')
                    if key_filename is not None and os.access(key_filename, os.R_OK):
                        if ((key_acceptable(key_filename) and
                             ok_password(key_filename, _auth_info.get('password')))):
                            filenames = []
                        else:
                            filenames = [key_filename]
                    else:
                        filenames = key_files
                    password = None
                    for f in filenames:
                        if not key_acceptable(f):
                            continue
                        password = key_password(f, _auth_info.get('password') or '')
                        if password is not None:
                            _auth_info['key_filename'] = f
                            _auth_info['password'] = password
                            break
                    if password is not None:
                        continue
                if 'password' in methods:
                    if 'publickey' in methods:
                        choice_password = _("Login using password")
                        choice_key_file = _("Login using a key file")
                        answer = app.choice_dialog(_("Default authentication failed"),
                                                   [choice_password, choice_key_file])
                        selected_method = 'publickey' if answer == choice_key_file else 'password'
                    else:
                        selected_method = 'password'
                elif 'publickey' in methods:
                    selected_method = 'publickey'
                else:
                    raise Exception(_("No supported ssh connection method available"))
                _auth_info['_method'] = selected_method
                if selected_method == 'password':
                    password = app.text_dialog(_("Login password"), password=True)
                    if not password:
                        return None
                    _auth_info['password'] = password.rstrip('\r\n')
                elif selected_method == 'publickey':
                    ssh_directory = os.path.join(os.path.expanduser('~'), '.ssh', '')
                    while True:
                        key_filename = app.file_dialog(_("Select ssh key file"),
                                                       directory=ssh_directory)
                        if key_filename is None:
                            return None
                        password = key_password(key_filename, _auth_info.get('password') or '')
                        if password is not None:
                            _auth_info['key_filename'] = key_filename
                            _auth_info['password'] = password
                            break
                else:
                    raise Exception(_("Program error"))
        return client

    @classmethod
    def _pytis_upgrade(class_, upgrade_url):
        match = re.match(('^(?P<protocol>(ssh|http(|s)))://'
                          '(|(?P<username>[a-zA-Z0-9_\.-]+)'
                          '(|:(?P<password>.*))@)'
                          '(?P<hostname>[a-zA-Z0-9\.-]+)'
                          '(|:(?P<port>[0-9]+))'
                          '($|/(?P<path>.*)$)'), upgrade_url)
        if match is None:
            raise Exception(_("Invalid broker address"), upgrade_url)
        parameters = match.groupdict()
        protocol = parameters['protocol']
        if protocol != 'ssh':
            raise Exception(_("Unsupported broker protocol"), protocol)
        password = parameters.get('password')
        port = int(parameters.get('port') or '22')
        _auth_info.update_non_empty(hostname=parameters['hostname'], port=port,
                                    username=(parameters['username'] or app.username_dialog()),
                                    password=password)
        path = parameters.get('path')
        client = class_.pytis_ssh_connect()
        if client is None:
            app.info_dialog(_("Couldn't connect to upgrade server"), error=True)
            return
        install_directory = os.path.normpath(os.path.join(run_directory(), '..', ''))
        old_install_directory = install_directory + '.old'
        tmp_directory = tempfile.mkdtemp(prefix='pytisupgrade')
        pytis_directory = os.path.join(tmp_directory, 'pytis2go', 'pytis')
        sftp = client.open_sftp()
        f = sftp.open(path)
        tarfile.open(fileobj=f).extractall(path=tmp_directory)
        if not os.path.isdir(pytis_directory):
            app.info_dialog(_("Package unpacking failed"), error=True)
            return
        os.rename(install_directory, old_install_directory)
        os.rename(pytis_directory, install_directory)
        os.rmdir(tmp_directory)
        shutil.rmtree(old_install_directory)
        app.info_dialog(_("Pytis successfully upgraded. Restart the application."))
        sys.exit(0)

    def new_session(self, s_hash):
        app.update_progress_dialog(message=_("Creating new session. Please wait..."))
        super(PytisClient, self).new_session(s_hash)
        if self._pytis_setup_configuration:
            app.update_progress_dialog(message=_("Setting up new session. Please wait..."))
            self.pytis_setup(s_hash)
            self._pytis_setup_configuration = False
            def info_handler():
                while self.session_ok(s_hash):
                    self.pytis_handle_info()
                    gevent.sleep(0.1)
            gevent.spawn(info_handler)
        app.close_progress_dialog()

    def pytis_maybe_resume_session(self, s_hash):
        args = self.args
        if ((args.share_desktop or args.suspend or args.terminate or args.list_sessions or
             args.list_desktops or args.list_profiles)):
            return
        session_infos = [info for info in self._X2GoClient__list_sessions(s_hash).values()
                         if info.status == 'S']
        session_infos.sort(lambda i1, i2: (cmp(i1.username, i2.username) or
                                           cmp(i1.hostname, i2.hostname) or
                                           cmp(i2.date_created, i1.date_created)))
        if session_infos:
            def session(info):
                return '%s@%s %s' % (info.username or '', info.hostname or '',
                                     (info.date_created or '').replace('T', ' '),)
            new_session = _("New session")
            choices = ([new_session] + [session(item) for item in session_infos])
            answer = app.choice_dialog(_("Resume session"), choices, index=True)
            if answer:
                args.resume = session_infos[answer - 1].name
                args.new = False

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
            if calling_script and os.path.exists(calling_script):
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
            except Exception:
                pass
        if ((shortcut_exists or
             not app.question_dialog(_("Create desktop shortcut for this session profile?")))):
            return
        # Create shortcut on desktop
        shortcut_name = profile_name
        while True:
            shortcut_path = os.path.join(winshell.desktop(), '%s.lnk' % shortcut_name)
            if not os.path.exists(shortcut_path):
                break
            else:
                msg = _("Shortcut %s allready exists. Please, rename it:") % shortcut_name
                new_name = app.text_dialog(msg, caption=_("Edit shortcut name"),
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
    def run(class_, args):
        _auth_info.update_from_args(args)
        # Read in configuration
        configuration = Configuration()
        if args.broker_url is None:
            if not _auth_info.get('hostname'):
                _auth_info['hostname'] = configuration.get_value('pytis', 'hostname')
            if not _auth_info.get('port'):
                _auth_info['port'] = configuration.get_value('pytis', 'port', int)
            if not _auth_info.get('username'):
                _auth_info['username'] = app.username_dialog()
            if not _auth_info.get('_command'):
                _auth_info['_command'] = configuration.get_value('pytis', 'command')
            try:
                _auth_info['password'] = configuration.get_value('pytis', 'password')
            except ClientException:
                pass
            if not _auth_info.get('key_filename'):
                try:
                    _auth_info['key_filename'] = configuration.get_value('pytis', 'key_filename')
                except ClientException:
                    pass
            # Check connection parameters and update password
            ssh_client = class_.pytis_ssh_connect()
            if ssh_client is None:
                return
        # Create client
        _auth_info.update_args(args)
        client = class_(args, use_cache=False, start_xserver=str(XSERVER_VARIANT),
                        loglevel=x2go.log.loglevel_DEBUG)
        app.update_progress_dialog(message=_("Starting server connections. Please wait..."))
        # Run
        s_uuid = client.x2go_session_hash
        session = client.session_registry(s_uuid)
        session.sshproxy_params['key_filename'] = _auth_info.get('key_filename')
        session.sshproxy_params['look_for_keys'] = False
        client.pytis_start_processes(configuration)
        app.update_progress_dialog(message=_("Authenticating. Please wait..."))
        client._pytis_setup_configuration = True
        client.authenticate()
        app.close_progress_dialog()
        client.pytis_maybe_resume_session(s_uuid)
        client.MainLoop()

# ---------------------
# Taken from pyhoca-cli

logger = x2go.X2GoLogger()
liblogger = x2go.X2GoLogger()

# exclusive client control options
action_options = [
    {'args': ['-N', '--new'], 'default': False, 'action': 'store_true',
     'help': 'start a new X2Go session on server (default)', },
    {'args': ['-R', '--resume'], 'default': None, 'metavar': 'SESSION_NAME',
     'help': 'resume a suspended X2Go session with name SESSION_NAME', },
    {'args': ['-D', '--share-desktop'], 'default': None, 'metavar': 'USER@DISPLAY',
     'help': 'share an X2Go session on server specified by USER@DISPLAY', },
    {'args': ['-S', '--suspend'], 'default': None, 'metavar': 'SESSION_NAME',
     'help': 'suspend running X2Go session SESSION_NAME', },
    {'args': ['-T', '--terminate'], 'default': None, 'metavar': 'SESSION_NAME',
     'help': 'terminate running X2Go session SESSION_NAME', },
    {'args': ['-L', '--list-sessions'], 'default': False, 'action': 'store_true', 'help':
     'list user\'s X2Go sessions on server', },
    {'args': ['--list-desktops'], 'default': False, 'action': 'store_true',
     'help': 'list X2Go desktop sessions that are available for sharing', },
    {'args': ['-l', '--list-profiles'], 'default': False, 'action': 'store_true',
     'help': 'list user\'s X2Go pre-configured session profiles', },
    {'args': ['-P', '--session-profile'], 'default': None,
     'help': 'load x2goclient session profiles and use the session profile SESSION_PROFILE', },
]
if not on_windows():
    action_options.append(
        {'args': ['--from-stdin'], 'default': False, 'action': 'store_true',
         'help': ('for LightDM remote login: '
                  'read <username> <password> <host[:port]> <desktopshell> from STDIN'),
         },
    )
# debug options...
debug_options = [
    {'args': ['-d', '--debug'], 'default': False, 'action': 'store_true',
     'help': 'enable application debugging code', },
    {'args': ['--quiet'], 'default': False, 'action': 'store_true',
     'help': 'disable any kind of log output', },
    {'args': ['--libdebug'], 'default': False, 'action': 'store_true',
     'help': 'enable debugging code of the underlying Python X2Go module', },
    {'args': ['--libdebug-sftpxfer'], 'default': False, 'action': 'store_true',
     'help': ('enable debugging code of Python X2Go\'s sFTP server code '
              '(very verbose, and even promiscuous)'),
     },
    {'args': ['-V', '--version'], 'default': False, 'action': 'store_true',
     'help': 'print version number and exit', },
]
# possible programme options are
x2go_options = [
    {'args': ['-c', '--command'],
     'help': 'command to run with -R mode on server (default: xterm)', },
    {'args': ['-u', '--username'],
     'help': 'username for the session (default: current user)', },
    {'args': ['--password'], 'default': None, 'help': 'user password for session authentication', },
    {'args': ['-p', '--remote-ssh-port'], 'default': '22',
     'help': 'remote SSH port (default: 22)', },
    {'args': ['-k', '--ssh-privkey'], 'default': None,
     'help': 'use file \'SSH_PRIVKEY\' as private key for the SSH connection (e.g. ~/.ssh/id_rsa)',
     },
    {'args': ['--add-to-known-hosts'], 'default': False, 'action': 'store_true',
     'help': ('add RSA host key fingerprint to ~/.ssh/known_hosts '
              'if authenticity of server can\'t be established (default: not set)'),
     },
    {'args': ['--sound'], 'default': 'pulse', 'choices': ('pulse', 'esd', 'none'),
     'help': 'X2Go server sound system (default: \'pulse\')', },
    {'args': ['--printing'], 'default': False, 'action': 'store_true',
     'help': 'use X2Go printing (default: disabled)', },
    {'args': ['--share-mode'], 'default': 0,
     'help': 'share mode for X2Go desktop sharing (0: view-only, 1: full access)', },
    {'args': ['-F', '--share-local-folders'], 'metavar': '<folder1>[,<folder2[,...]]',
     'default': None,
     'help': 'a comma separated list of local folder names to mount in the X2Go session', },
    {'args': ['--clean-sessions'], 'default': False, 'action': 'store_true',
     'help': 'clean all suspended sessions before starting a new one', },
    {'args': ['--terminate-on-ctrl-c'], 'default': False, 'action': 'store_true',
     'help': ('terminate the connected session when pressing CTRL+C '
              '(instead of suspending the session)'), },
    {'args': ['--auth-attempts'], 'default': 3,
     'help': 'number of authentication attempts before authentication fails (default: 3)', },
]
print_options = [
    {'args': ['--print-action'], 'default': 'PDFVIEW',
     'choices': x2go.defaults.X2GO_PRINT_ACTIONS.keys(),
     'help': 'action to be performed for incoming X2Go print jobs (default: \'PDFVIEW\')', },
    {'args': ['--pdfview-cmd'], 'default': None,
     'help': (('PDF viewer command for displaying incoming X2Go print jobs (default: \'%s\');'
              ' this option selects \'--print-action PDFVIEW\'') %
              x2go.defaults.DEFAULT_PDFVIEW_CMD), },
    {'args': ['--save-to-folder'], 'default': None, 'metavar': 'PRINT_DEST',
     'help': (('save print jobs as PDF files to folder PRINT_DEST (default: \'%s\'); '
               'this option selects \'--print-action PDFSAVE\'') %
              x2go.defaults.DEFAULT_PDFSAVE_LOCATION), },
    {'args': ['--printer'], 'default': None,
     'help': ('target CUPS print queue for incoming X2Go print jobs '
              '(default: CUPS default printer); this option selects \'--print-action CUPS\''), },
    {'args': ['--print-cmd'], 'default': None,
     'help': (('print command including cmd line arguments (default: \'%s\'); '
               'this option selects \'--print-action PRINTCMD\'') %
              x2go.defaults.DEFAULT_PRINTCMD_CMD), },
]
broker_options = [
    {'args': ['-B', '--broker-url'], 'default': None,
     'help': 'retrieve session profiles via an X2Go Session Broker under the given URL', },
    {'args': ['--broker-password'], 'default': None,
     'help': 'password for authenticating against the X2Go Session Broker', },
]
nx_options = [
    {'args': ['-g', '--geometry'], 'default': '800x600',
     'help': 'screen geometry: \'<width>x<height>\' or \'fullscreen\' (default: \'800x600\')', },
    {'args': ['-q', '--link'], 'default': 'adsl',
     'choices': ('modem', 'isdn', 'adsl', 'wan', 'lan'),
     'help': 'link quality (default: \'adsl\')', },
    {'args': ['-t', '--session-type'], 'default': 'application',
     'choices': ('desktop', 'application'), 'help': 'session type (default: \'application\')', },
    {'args': ['--pack'], 'default': '16m-jpeg-9',
     'help': 'compression methods (see below for possible values)', },
    {'args': ['--kbd-layout'], 'default': 'us',
     'help': 'use keyboard layout (default: \'us\')', },
    {'args': ['--kbd-type'], 'default': 'pc105/us',
     'help': 'set Keyboard type (default: \'pc105/us\')', },
]
compat_options = [
    {'args': ['--port'], 'default': None,
     'help': 'compatibility option, synonymous to --remote-ssh-port PORT', },
    {'args': ['--ssh-key'], 'default': None,
     'help': 'compatibility option, synonymous to --ssh-privkey SSH_KEY', },
    {'args': ['--use-sound'], 'default': None, 'choices': ('yes', 'no'),
     'help': 'compatibility option, synonymous to --sound {pulse|none}', },
    {'args': ['--client-ssh-port'], 'default': None,
     'help': ('compatibility option for the x2goclient GUI; as Python X2Go brings its own internal '
              'SFTP server, this option will be ignored'), },
]
if on_windows():
    pytis2go_options = [
        {'args': ['--create-shortcut'], 'default': False, 'action': 'store_true',
         'help': 'create desktop shortcut if not present (default: disabled)', },
        {'args': ['--calling-script'], 'default': False,
         'help': 'full file name of the script invoking this command', },
    ]
_profiles_backend_default = x2go.BACKENDS['X2GoSessionProfiles']['default']
_settings_backend_default = x2go.BACKENDS['X2GoClientSettings']['default']
_printing_backend_default = x2go.BACKENDS['X2GoClientPrinting']['default']
if on_windows():
    _config_backends = ('FILE', 'WINREG')
else:
    _config_backends = ('FILE', 'GCONF')
backend_options = [
    {'args': ['--backend-controlsession'], 'default': None, 'metavar': '<CONTROLSESSION_BACKEND>',
     'choices': x2go.BACKENDS['X2GoControlSession'].keys(),
     'help': ('force usage of a certain CONTROLSESSION_BACKEND '
              '(do not use this unless you know exactly what you are doing)'), },
    {'args': ['--backend-terminalsession'], 'default': None, 'metavar': '<TERMINALSESSION_BACKEND>',
     'choices': x2go.BACKENDS['X2GoTerminalSession'].keys(),
     'help': ('force usage of a certain TERMINALSESSION_BACKEND '
              '(do not use this unless you know exactly what you are doing)'), },
    {'args': ['--backend-serversessioninfo'], 'default': None,
     'metavar': '<SERVERSESSIONINFO_BACKEND>',
     'choices': x2go.BACKENDS['X2GoServerSessionInfo'].keys(),
     'help': ('force usage of a certain SERVERSESSIONINFO_BACKEND '
              '(do not use this unless you know exactly what you are doing)'), },
    {'args': ['--backend-serversessionlist'], 'default': None,
     'metavar': '<SERVERSESSIONLIST_BACKEND>',
     'choices': x2go.BACKENDS['X2GoServerSessionList'].keys(),
     'help': ('force usage of a certain SERVERSESSIONLIST_BACKEND '
              '(do not use this unless you know exactly what you are doing)'), },
    {'args': ['--backend-proxy'], 'default': None, 'metavar': '<PROXY_BACKEND>',
     'choices': x2go.BACKENDS['X2GoProxy'].keys(),
     'help': ('force usage of a certain PROXY_BACKEND '
              '(do not use this unless you know exactly what you are doing)'), },
    {'args': ['--backend-sessionprofiles'], 'default': None, 'metavar': '<SESSIONPROFILES_BACKEND>',
     'choices': _config_backends,
     'help': ('use given backend for accessing session profiles, available backends on your system:'
              ' %s (default: %s)') % (', '.join(_config_backends), _profiles_backend_default), },
    {'args': ['--backend-clientsettings'], 'default': None, 'metavar': '<CLIENTSETTINGS_BACKEND>',
     'choices': _config_backends,
     'help': (('use given backend for accessing the client settings configuration, '
               'available backends on your system: %s (default: %s)') %
              (', '.join(_config_backends), _settings_backend_default)), },
    {'args': ['--backend-clientprinting'], 'default': None, 'metavar': '<CLIENTPRINTING_BACKEND>',
     'choices': _config_backends,
     'help': (('use given backend for accessing the client printing configuration, '
               'available backends on your system: %s (default: %s)') %
              (', '.join(_config_backends), _printing_backend_default)), },
]

# print version text and exit
def version():
    sys.stderr.write("%s\n" % (_VERSION,))
    sys.exit(0)

def parseargs():
    global logger
    global liblogger
    p = argparse.ArgumentParser(description='X2Go pytis client.',
                                epilog="""
Possible values for the --pack NX option are:
    %s
""" % x2go.defaults.pack_methods_nx3_formatted,
                                formatter_class=argparse.RawDescriptionHelpFormatter,
                                add_help=True, argument_default=None)
    p_reqargs = p.add_argument_group('X2Go server name')
    p_reqargs.add_argument('--server', help='server hostname or IP address')
    p_actionopts = p.add_argument_group('client actions')
    p_debugopts = p.add_argument_group('debug options')
    p_x2goopts = p.add_argument_group('X2Go options')
    p_printopts = p.add_argument_group('X2Go print options')
    p_brokeropts = p.add_argument_group('X2Go Session Broker client options')
    p_nxopts = p.add_argument_group('NX options')
    p_backendopts = p.add_argument_group('Python X2Go backend options (for experts only)')
    p_compatopts = p.add_argument_group('compatibility options')
    option_groups = [(p_x2goopts, x2go_options), (p_printopts, print_options),
                     (p_brokeropts, broker_options), (p_actionopts, action_options),
                     (p_debugopts, debug_options), (p_nxopts, nx_options),
                     (p_backendopts, backend_options), (p_compatopts, compat_options)]
    if on_windows():
        p_pytis2goopts = p.add_argument_group('pytis2go options')
        option_groups.append((p_pytis2goopts, pytis2go_options))
    for (p_group, opts) in option_groups:
        for opt in opts:
            args = opt['args']
            del opt['args']
            p_group.add_argument(*args, **opt)
    a = p.parse_args()
    if a.debug:
        logger.set_loglevel_debug()
    if a.libdebug:
        liblogger.set_loglevel_debug()
    if a.quiet:
        logger.set_loglevel_quiet()
        liblogger.set_loglevel_quiet()
    if a.libdebug_sftpxfer:
        liblogger.enable_debug_sftpxfer()
    if a.password and on_windows():
        runtime_error("The --password option is forbidden on Windows platforms", parser=p,
                      exitcode=222)
    if a.version:
        version()
    if not (a.session_profile or a.list_profiles):
        # check for mutual exclusiveness of -N, -R, -S, -T and -L, -N is
        # default if none of them is set
        if ((bool(a.new) + bool(a.resume) + bool(a.share_desktop) + bool(a.suspend) +
             bool(a.terminate) + bool(a.list_sessions) + bool(a.list_desktops) +
             bool(a.list_profiles)) > 1):
            runtime_error("modes --new, --resume, --share-desktop, --suspend, --terminate, "
                          "--list-sessions, --list-desktops and "
                          "--list-profiles are mutually exclusive", parser=p, exitcode=2)
        if ((bool(a.new) + bool(a.resume) + bool(a.share_desktop) + bool(a.suspend) +
             bool(a.terminate) + bool(a.list_sessions) + bool(a.list_desktops) +
             bool(a.list_profiles)) == 0):
            a.new = True
        # check if pack method is available
        if not x2go.utils.is_in_nx3packmethods(a.pack):
            runtime_error("unknown pack method '%s'" % args.pack, parser=p, exitcode=10)
    else:
        if not (a.resume or a.share_desktop or a.suspend or a.terminate or a.list_sessions or
                a.list_desktops or a.list_profiles):
            a.new = True
    # X2Go printing
    if (((a.pdfview_cmd and a.printer) or
         (a.pdfview_cmd and a.save_to_folder) or
         (a.pdfview_cmd and a.print_cmd) or
         (a.printer and a.save_to_folder) or
         (a.printer and a.print_cmd) or
         (a.print_cmd and a.save_to_folder))):
        runtime_error(("--pdfviewer, --save-to-folder, --printer and --print-cmd options are "
                       "mutually exclusive"), parser=p, exitcode=81)
    if a.pdfview_cmd:
        a.print_action = 'PDFVIEW'
    elif a.save_to_folder:
        a.print_action = 'PDFSAVE'
    elif a.printer:
        a.print_action = 'PRINT'
    elif a.print_cmd:
        a.print_action = 'PRINTCMD'
    if a.pdfview_cmd is None and a.print_action == 'PDFVIEW':
        a.pdfview_cmd = x2go.defaults.DEFAULT_PDFVIEW_CMD
    if a.save_to_folder is None and a.print_action == 'PDFSAVE':
        a.save_to_folder = x2go.defaults.DEFAULT_PDFSAVE_LOCATION
    if a.printer is None and a.print_action == 'PRINT':
        # None means CUPS default printer...
        a.printer = None
    if a.print_cmd is None and a.print_action == 'PRINTCMD':
        a.print_cmd = x2go.defaults.DEFAULT_PRINTCMD_CMD
    a.print_action_args = {}
    if a.pdfview_cmd:
        a.print_action_args = {'pdfview_cmd': a.pdfview_cmd, }
    elif a.save_to_folder:
        a.print_action_args = {'save_to_folder': a.save_to_folder, }
    elif a.printer:
        a.print_action_args = {'printer': a.printer, }
    elif a.print_cmd:
        a.print_action_args = {'print_cmd': a.print_cmd, }
    # take care of compatibility options
    # option --use-sound yes as synonomyn for --sound
    if a.use_sound is not None:
        if a.use_sound == 'yes':
            a.sound = 'pulse'
        if a.use_sound == 'no':
            a.sound = 'none'
    if a.ssh_key is not None:
        a.ssh_privkey = a.ssh_key
    if a.port is not None:
        a.remote_ssh_port = a.port
    if a.share_local_folders is not None:
        a.share_local_folders = a.share_local_folders.split(',')
    try:
        int(a.auth_attempts)
    except ValueError:
        runtime_error("value for cmd line argument --auth-attempts has to be of type integer",
                      parser=p, exitcode=1)
    if a.server:
        # check if SERVER is in .ssh/config file, extract information from there...
        ssh_config = paramiko.SSHConfig()
        from pyhoca.cli import ssh_config_filename
        ssh_config_fileobj = open(ssh_config_filename)
        ssh_config.parse(ssh_config_fileobj)
        ssh_host = ssh_config.lookup(a.server)
        if ssh_host:
            if 'hostname' in ssh_host.keys():
                a.server = ssh_host['hostname']
            if 'port' in ssh_host.keys():
                a.remote_ssh_port = ssh_host['port']
        ssh_config_fileobj.close()
    # check if ssh priv key exists
    if a.ssh_privkey and not os.path.isfile(a.ssh_privkey):
        runtime_error("SSH private key %s file does not exist." % a.ssh_privkey, parser=p,
                      exitcode=30)
    # lightdm remote login magic takes place here
    if not on_windows() and a.from_stdin:
        lightdm_remote_login_buffer = sys.stdin.readline()
        (a.username, a.server, a.command) = lightdm_remote_login_buffer.split()[0:3]
        a.password = " ".join(lightdm_remote_login_buffer.split()[3:])
        if ":" in a.server:
            a.remote_ssh_port = a.server.split(':')[-1]
            a.server = ':'.join(a.server.split(':')[:-1])
        a.command = a.command.upper()
        a.geometry = 'fullscreen'
    return p, a

# -----
# Start

def parse_arguments():
    parser, args = parseargs()
    args.parser = parser
    return args

def main():
    args = parse_arguments()
    quit_signal = signal.SIGTERM if on_windows() else signal.SIGQUIT
    gevent.signal(quit_signal, gevent.kill)
    PytisClient.run(args)

app.update_progress_dialog(message=_("Launching application. Please wait..."))

if __name__ == '__main__':
    main()

# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
