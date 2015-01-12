#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012, 2014, 2015 Brailcom, o.p.s.
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

from __future__ import unicode_literals

# ATTENTION: This should be updated on each code change.
_VERSION = '2015-01-12 15:47'

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
import rpyc
import x2go
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

class PytisClient(x2go.X2GoClient):

    _DEFAULT_RPYC_PORT = 10000
    _MAX_RPYC_PORT_ATTEMPTS = 100
    _DEFAULT_KEY_FILENAME = os.path.expanduser('~/.ssh/id_rsa')

    def __init__(self, *args, **kwargs):
        super(PytisClient, self).__init__(*args, **kwargs)
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

    @classmethod
    def run(class_, broker_url=None, server=None, username=None, command=None,
            key_filename=None, add_to_known_hosts=False):
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
        port = configuration.get('sshport', int, 22)
        if username is None:
            username = configuration.get('user', basestring, x2go.defaults.CURRENT_LOCAL_USER)
        else:
            configuration.set('user', username)
        if command is None:
            command = configuration.get('command', basestring,
                                        x2go.defaults.X2GO_SESSIONPROFILE_DEFAULTS['command'])
        else:
            configuration.set('command', command)
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
        key_filename = parameters['key_filename']
        configuration.set('key_filename', key_filename)
        # Run
        client = class_(use_cache=False, start_xserver=True, loglevel=x2go.log.loglevel_DEBUG)
        s_uuid = client.register_session(server, port=port, username=username,
                                         key_filename=key_filename,
                                         cmd=command, add_to_known_hosts=add_to_known_hosts)
        client.pytis_start_processes(configuration)
        session = client.session_registry(s_uuid)
        session.sshproxy_params['key_filename'] = key_filename
        session.sshproxy_params['look_for_keys'] = False
        client.connect_session(s_uuid, username=username, password=password)
        client.clean_sessions(s_uuid)
        session_params = configuration.get_session_params()
        client.get_session(s_uuid).update_params(session_params)
        client.start_session(s_uuid)
        client.pytis_setup(s_uuid, configuration)
        try:
            while client.session_ok(s_uuid):
                client.pytis_handle_info()
                gevent.sleep(0.1)
        except KeyboardInterrupt:
            pass
        client.terminate_session(s_uuid)

def run():
    parser = argparse.ArgumentParser()
    parser.add_argument('--broker-url')
    parser.add_argument('--server')
    parser.add_argument('--username')
    parser.add_argument('--command')
    parser.add_argument('--ssh-privkey')
    parser.add_argument('--add-to-known-hosts', action='store_true')
    args = parser.parse_args()
    quit_signal = signal.SIGTERM if on_windows() else signal.SIGQUIT
    gevent.signal(quit_signal, gevent.kill)
    PytisClient.run(args.broker_url, args.server, args.username, args.command, args.ssh_privkey,
                    args.add_to_known_hosts)

if __name__ == '__main__':
    run()

# Local Variables:
# time-stamp-pattern: "30/^_VERSION = '%Y-%02m-%02d %02H:%02M'"
# End:
