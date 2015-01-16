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
_VERSION = '2015-01-16 20:42'

import gevent.monkey
gevent.monkey.patch_all()

import argparse
import copy
import gettext
import imp
import os
import platform
import re
import shutil
import signal
import sys
import tarfile
import tempfile
import types

def run_directory():
    return sys.path[0]

sys.path.append(os.path.normpath(os.path.join(run_directory(), '..', 'lib')))

import gevent
import gevent.event
import gevent.queue
import paramiko
import pyhoca.cli
from pyhoca.cli import current_home, runtime_error
import rpyc
import x2go
import x2go.defaults
import PyZenity as zenity
import pytis.remote

t = gettext.translation('pytis-x2go',
                        os.path.normpath(os.path.join(run_directory(), '..', 'translations')),
                        fallback=True)
_ = t.ugettext

def on_windows():
    return platform.system() == 'Windows'

if on_windows():
    os.environ[str('NXPROXY_BINARY')] = str(os.path.join(run_directory(), 'nxproxy', 'nxproxy.exe'))

_NONE = object()

class ClientException(Exception):
    pass

class Configuration(object):

    _CONFIGURATION_FILE = 'config.py'
    _RPYC_FILE = 'rpyc'
    _default_session_profile = copy.deepcopy(x2go.defaults.X2GO_SESSIONPROFILE_DEFAULTS)

    def __init__(self):
        self._directory = self._configuration_directory()
        self._configuration = self._read_configuration()
        self._session_params = {}

    def _configuration_directory(self):
        basename = '_pytis_x2go' if on_windows() else '.pytis-x2go'
        dirname = os.path.expanduser(os.path.join('~', basename))
        if not os.access(dirname, os.F_OK):
            try:
                os.mkdir(dirname, 0700)
            except Exception as e:
                raise ClientException(_("Can't create client directory: %s") % (dirname,), e)
        elif not os.access(dirname, os.R_OK | os.W_OK | os.X_OK):
            raise ClientException(_("Client directory not accessible: %s") % (dirname,))
        return dirname

    def _configuration_file(self, filename):
        return os.path.join(self._configuration_directory(), filename)

    def _read_configuration(self):
        filename = self._configuration_file(self._CONFIGURATION_FILE)
        if not os.access(filename, os.F_OK):
            try:
                open(filename, 'w').close()
            except Exception as e:
                raise ClientException(_("Can't create client configuration file: %s") % (filename,),
                                      e)
        elif not os.access(filename, os.R_OK | os.W_OK):
            raise ClientException(_("Client configuration not accessible: %s") % (filename,))
        try:
            c = imp.load_source('_config', filename)
        except Exception as e:
            raise ClientException(_("Error when loading client configuration: %s") % (filename,), e)
        configuration = copy.copy(c.__dict__)
        del sys.modules['_config']
        return configuration

    def get(self, key, type_, default=_NONE):
        args = () if default is _NONE else (default,)
        try:
            value = self._configuration.get(key, *args)
        except KeyError:
            raise ClientException(_("Configuration parameter not available: %s") % (key,))
        if not isinstance(value, type_) and value != default:
            raise ClientException(_("Invalid configuration parameter type: %s = %r (is not %s)") %
                                  (key, value, type_,))
        return value

    def _get_profile_option_type(self, option):
        "Get the data type for a specific session profile option."
        try:
            return type(self._default_session_profile[option])
        except KeyError:
            return types.StringType

    def get_session_params(self):
        if self._session_params:
            return x2go.utils._convert_SessionProfileOptions_2_SessionParams(self._session_params)
        else:
            return {}

    def set_session_params(self, session_name, parser):
        for param in self._default_session_profile:
            if parser.has_option(session_name, param):
                option_type = self._get_profile_option_type(param)
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
                val = self._default_session_profile[param]
            self._session_params[param] = val

    def set(self, key, value):
        self._configuration[key] = value

    def rpyc_file(self):
        return self._configuration_file(self._RPYC_FILE)

class RpycInfo(object):

    def __init__(self, configuration, port=None, password=None):
        self._filename = configuration.rpyc_file()
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

class PytisClient(pyhoca.cli.PyHocaCLI):

    _DEFAULT_RPYC_PORT = 10000
    _MAX_RPYC_PORT_ATTEMPTS = 100
    _DEFAULT_KEY_FILENAME = os.path.expanduser('~/.ssh/id_rsa')

    def __init__(self, args, logger=None, liblogger=None, **kwargs):
        import pprint
        ssh_known_hosts_filename = os.path.join(x2go.LOCAL_HOME, x2go.X2GO_SSH_ROOTDIR,
                                                'known_hosts')
        #
        self.args = args
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
        x2go.X2GoClient.__init__(self, broker_url=self.args.broker_url,
                                 broker_password=self.args.broker_password, logger=liblogger,
                                 **_backend_kwargs)
        _profiles = self._X2GoClient__get_profiles()
        if self.args.session_profile and not _profiles.has_profile(self.args.session_profile):
            self._runtime_error('no such session profile of name: %s' % (self.args.session_profile),
                                exitcode=31)
        self.auth_attempts = int(self.args.auth_attempts)
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
        elif self.args.session_profile:
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
            # setup up the manually configured X2Go session
            self.x2go_session_hash = self._X2GoClient__register_session(
                args.server,
                port=int(self.args.remote_ssh_port),
                known_hosts=ssh_known_hosts_filename,
                username=self.args.username,
                key_filename=self.args.ssh_privkey,
                add_to_known_hosts=self.args.add_to_known_hosts,
                profile_name='Pyhoca-Client_Session',
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
        # super(PytisClient, self).__init__(*args, **kwargs)
        self._pytis_port_value = gevent.event.AsyncResult()
        self._pytis_password_value = gevent.event.AsyncResult()
        self._pytis_terminate = gevent.event.Event()

    def pytis_setup(self, s_uuid, configuration):
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

    def _check_ssh_tunnel(self, configuration, rpyc_stop_queue, rpyc_port, ssh_tunnel_dead):
        try:
            password = configuration.get('password', basestring)
        except ClientException:
            password = None
        try:
            key_filename = configuration.get('key_filename', basestring)
        except ClientException:
            key_filename = None
        user = configuration.get('user', basestring, x2go.defaults.CURRENT_LOCAL_USER)
        while True:
            while not rpyc_port.ready():
                if self._pytis_terminate.is_set():
                    return
                gevent.sleep(0.1)
            current_rpyc_port = rpyc_port.get()
            port = gevent.event.AsyncResult()
            tunnel = pytis.remote.ReverseTunnel(configuration.get('host', basestring),
                                                current_rpyc_port, ssh_forward_port=0,
                                                ssh_user=user,
                                                ssh_password=password,
                                                key_filename=key_filename,
                                                ssh_forward_port_result=port)
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
        gevent.spawn(self._check_rpyc_server, *args)
        gevent.spawn(self._check_ssh_tunnel, *args)

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
    def _pytis_ssh_connect(class_, parameters):
        if not parameters['password']:
            parameters['password'] = 'X'
        methods = []
        client = paramiko.SSHClient()
        client.load_system_host_keys()
        if parameters.get('_add_to_known_hosts'):
            client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        selected_method = None
        # Test ssh agent first
        connect_parameters = dict([(k, v,) for k, v in parameters.items() if k[0] != '_'])
        connect_parameters['key_filename'] = None
        try:
            client.connect(look_for_keys=False, **connect_parameters)
            parameters['key_filename'] = None
            return client
        except paramiko.ssh_exception.AuthenticationException:
            pass
        # ssh agent not available, try other authentication methods
        connect_parameters['key_filename'] = parameters.get('key_filename')
        while True:
            connect_parameters = dict([(k, v,) for k, v in parameters.items() if k[0] != '_'])
            try:
                client.connect(look_for_keys=False, **connect_parameters)
                break
            except paramiko.ssh_exception.AuthenticationException:
                if selected_method != 'password' and parameters.get('key_filename') is not None:
                    password = zenity.GetText(text=_("Key password"), password=True,
                                              title="")
                    if password is not None:
                        parameters['password'] = password
                        continue
                if not methods:
                    methods = class_._ssh_server_methods(parameters['hostname'],
                                                         parameters['port'])
                if 'publickey' in methods and parameters.get('key_filename') is None:
                    if os.access(class_._DEFAULT_KEY_FILENAME, os.R_OK):
                        parameters['key_filename'] = class_._DEFAULT_KEY_FILENAME
                        continue
                if 'password' in methods:
                    if 'publickey' in methods:
                        answer = zenity.Question(_("Default authentication failed"),
                                                 title='',
                                                 ok_label=_("Login using password"),
                                                 cancel_label=_("Login using a key file"))
                        selected_method = 'publickey' if not answer else 'password'
                    else:
                        selected_method = 'password'
                elif 'publickey' in methods:
                    selected_method = 'publickey'
                else:
                    raise Exception(_("No supported ssh connection method available"))
                parameters['_method'] = selected_method
                if selected_method == 'password':
                    password = zenity.GetText(text=_("Login password"), password=True,
                                              title=_("Password input"))
                    if not password:
                        return None
                    parameters['password'] = password.rstrip('\r\n')
                elif selected_method == 'publickey':
                    ssh_directory = os.path.join(os.path.expanduser('~'), '.ssh', '')
                    key_filename = zenity.GetFilename(title=_("Select ssh key file"),
                                                      filename=ssh_directory)
                    if key_filename is None:
                        return None
                    parameters['key_filename'] = key_filename[0]
                else:
                    raise Exception(_("Program error"))
        return client

    @classmethod
    def _pytis_parse_url(class_, url, add_to_known_hosts=False):
        match = re.match(('^(?P<protocol>(ssh|http(|s)))://'
                          '(|(?P<user>[a-zA-Z0-9_\.-]+)'
                          '(|:(?P<password>.*))@)'
                          '(?P<host>[a-zA-Z0-9\.-]+)'
                          '(|:(?P<port>[0-9]+))'
                          '($|/(?P<path>.*)$)'), url)
        if match is None:
            raise Exception(_("Invalid broker address"), url)
        parameters = match.groupdict()
        protocol = parameters['protocol']
        if protocol != 'ssh':
            raise Exception(_("Unsupported broker protocol"), protocol)
        password = parameters.get('password')
        port = int(parameters.get('port') or '22')
        ssh_parameters = dict(hostname=parameters['host'], port=port,
                              username=parameters['user'], password=password,
                              _add_to_known_hosts=add_to_known_hosts)
        path = parameters.get('path')
        return ssh_parameters, path

    @classmethod
    def _pytis_select_broker_session(class_, broker_url, add_to_known_hosts):
        parameters, path = class_._pytis_parse_url(broker_url, add_to_known_hosts)
        # Fetch session list
        sessions = ''
        text = ''
        configuration = None
        in_config = False
        client = class_._pytis_ssh_connect(parameters)
        if client is None:
            return
        broker_path = os.path.join('/', path or '/usr/bin/x2gobroker')
        command = "%s --task listsessions" % (broker_path,)
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
        data = []
        for s in sections:
            name = parser.get(s, 'name')
            if name == 'pytis-client-upgrade':
                continue
            elif name:
                data.append([s, name])
            else:
                data.append([s, ''])
        # Check for upgrade
        try:
            version = parser.get('pytis-client-upgrade', 'version')
        except ConfigParser.NoSectionError:
            version = None
        if version and version > _VERSION:
            upgrade_url = parser.get('pytis-client-upgrade', 'url')
            if upgrade_url:
                if zenity.Question(_("New pytis client version available. Install?"),
                                   title='', ok_label=_("Yes"), cancel_label=_("No")):
                    class_._pytis_upgrade(copy.copy(parameters), upgrade_url)
        # Select session
        answer = zenity.List(['Session id', 'Session name'], title=_("Select session"),
                             data=data)
        session_name = answer and answer[0]
        if not session_name:
            raise Exception(_("No session selected."))
        configuration = Configuration()
        configuration.set_session_params(session_name, parser)
        # Fetch additional session parameters
        server_regexp = re.compile('^SERVER:(.*):(.*)$')
        text = ''
        command = "%s --task selectsession --sid %s" % (broker_path, session_name)
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
                configuration.set('host', host)
                configuration.set('sshport', port)
        # Finish configuration and return it
        configuration.set('password', parameters['password'])
        return configuration

    @classmethod
    def _pytis_upgrade(class_, parameters, upgrade_url):
        upgrade_parameters, path = class_._pytis_parse_url(upgrade_url,
                                                           parameters.get('add_to_known_hosts'))
        parameters.update(upgrade_parameters)
        client = class_._pytis_ssh_connect(parameters)
        if client is None:
            zenity.Error(_("Couldn't connect to upgrade server"))
            return
        install_directory = os.path.normpath(os.path.join(run_directory(), '..', ''))
        old_install_directory = install_directory + '.old'
        tmp_directory = tempfile.mkdtemp(prefix='pytisupgrade')
        pytis_directory = os.path.join(tmp_directory, 'pytis')
        sftp = client.open_sftp()
        f = sftp.open(path)
        tarfile.open(fileobj=f).extractall(path=tmp_directory)
        if not os.path.isdir(pytis_directory):
            zenity.Error(_("Package unpacking failed"))
            return
        os.rename(install_directory, old_install_directory)
        os.rename(pytis_directory, install_directory)
        os.rmdir(tmp_directory)
        shutil.rmtree(old_install_directory)
        zenity.InfoMessage(_("Pytis successfully upgraded. Restart the application."))
        sys.exit(0)

    def new_session(self, s_hash):
        super(PytisClient, self).new_session(s_hash)
        if self._pytis_setup_configuration is not None:
            self.pytis_setup(s_hash, self._pytis_setup_configuration)
            self._pytis_setup_configuration = None
            def info_handler():
                while self.session_ok(s_hash):
                    self.pytis_handle_info()
                    gevent.sleep(0.1)
            gevent.spawn(info_handler)
            
    @classmethod
    def run(class_, args):
        broker_url = args.broker_url
        server = args.server
        username = args.username
        command = args.command
        key_filename = args.ssh_privkey
        add_to_known_hosts = args.add_to_known_hosts
        # Get parameters
        if on_windows():
            zenity.zen_exec = os.path.join(run_directory(), 'zenity.exe')
        else:
            zenity.zen_exec = 'zenity'
        if broker_url:
            configuration = class_._pytis_select_broker_session(broker_url, add_to_known_hosts)
            if configuration is None:
                return
        else:
            configuration = Configuration()
        if server is None:
            server = configuration.get('host', basestring)
        else:
            configuration.set('host', server)
        args.server = server
        port = configuration.get('sshport', int, 22)
        if username is None:
            username = configuration.get('user', basestring, x2go.defaults.CURRENT_LOCAL_USER)
        else:
            configuration.set('user', username)
        args.username = username
        if command is None:
            command = configuration.get('command', basestring,
                                        x2go.defaults.X2GO_SESSIONPROFILE_DEFAULTS['command'])
        else:
            configuration.set('command', command)
        args.command = command
        try:
            password = configuration.get('password', basestring)
        except ClientException:
            password = None
        if key_filename is None:
            try:
                key_filename = configuration.get('key_filename', basestring)
            except ClientException:
                pass
        else:
            configuration.set('key_filename', key_filename)
        parameters = dict(hostname=server, port=port, username=username, password=password,
                          key_filename=key_filename, _add_to_known_hosts=add_to_known_hosts)
        # Check connection parameters and update password
        client = class_._pytis_ssh_connect(parameters)
        if client is None:
            return
        client = None
        password = parameters['password']
        configuration.set('password', password)
        args.password = password
        key_filename = parameters['key_filename']
        configuration.set('key_filename', key_filename)
        args.ssh_privkey = key_filename
        # Run
        client = class_(args, use_cache=False, start_xserver=True, loglevel=x2go.log.loglevel_DEBUG)
        s_uuid = client.x2go_session_hash
        session = client.session_registry(s_uuid)
        session.sshproxy_params['key_filename'] = key_filename
        session.sshproxy_params['look_for_keys'] = False
        client.pytis_start_processes(configuration)
        client._pytis_setup_configuration = configuration
        client.authenticate()
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
    {'args': ['-u', '--username'], 'default': None,
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
    for (p_group, opts) in ((p_x2goopts, x2go_options), (p_printopts, print_options),
                            (p_brokeropts, broker_options), (p_actionopts, action_options),
                            (p_debugopts, debug_options), (p_nxopts, nx_options),
                            (p_backendopts, backend_options), (p_compatopts, compat_options)):
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
    if not a.ssh_privkey and os.path.isfile('%s/.ssh/id_rsa' % current_home):
        a.ssh_privkey = '%s/.ssh/id_rsa' % current_home
    if not a.ssh_privkey and os.path.isfile('%s/.ssh/id_dsa' % current_home):
        a.ssh_privkey = '%s/.ssh/id_dsa' % current_home
    # lightdm remote login magic takes place here
    if a.from_stdin:
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

if __name__ == '__main__':
    main()

# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
