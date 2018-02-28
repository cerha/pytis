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

import gevent.monkey
gevent.monkey.patch_all() # noqa: E402

import os
import sys

import re
import copy
import time
import types
import socket
import hashlib
import datetime
import paramiko
import tempfile
import threading
import subprocess

import gevent
import gevent.event
import gevent.queue

import rpyc
import x2go
import x2go.backends.profiles.base
import x2go.backends.terminal.plain
import x2go.client
import x2go.defaults
import x2go.log
import x2go.xserver

import pytis.x2goclient
import pytis.remote.pytisproc as pytisproc

from pytis.util import log, EVENT, on_windows
from .ssh import ssh_connect

XSERVER_VARIANTS = ('VcXsrv_pytis', 'VcXsrv_pytis_old')

XCONFIG_DEFAULTS = {
    'XServers': {
        'known_xservers': ['VcXsrv_development', 'VcXsrv_shipped', 'VcXsrv', 'Xming',
                           'Cygwin-X', 'VcXsrv_pytis', 'VcXsrv_pytis_old'],
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
}

if on_windows():
    # Windows specific X2Go setup
    X2GO_CLIENTXCONFIG_DEFAULTS = x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS
    X2GO_CLIENTXCONFIG_DEFAULTS.update(XCONFIG_DEFAULTS)
    x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS = X2GO_CLIENTXCONFIG_DEFAULTS
    os.environ['NXPROXY_BINARY'] = os.path.normpath(os.path.join(
        sys.path[0], '..', '..', 'win_apps', 'nxproxy', 'nxproxy.exe',
    ))

def runtime_error(message, exitcode=-1):
    # TODO: Raise an exception instead and catch it in the
    # startup application to display errors in the UI.
    sys.stderr.write("%s: error: %s\n" % (os.path.basename(sys.argv[0]), message))
    sys.exit(exitcode)

class ClientException(Exception):
    pass


class X2GoClientSettings(x2go.X2GoClientSettings):

    def __init__(self, *args, **kwargs):
        super(X2GoClientSettings, self).__init__(*args, **kwargs)
        default_command = x2go.defaults.X2GO_SESSIONPROFILE_DEFAULTS['command']
        self.defaultValues['pytis'] = dict(hostname=None,
                                           password=None,
                                           port=22,
                                           key_filename=None,
                                           command=default_command)


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
        session_data = self._broker_profile_cache[profile_id] = copy.copy(
            self._broker_profiles[profile_id])
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
                    raise Exception("Invalid session format", text)
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
        last_version = pytis.x2goclient.X2GOCLIENT_VERSION
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


class Broker(object):
    """Higher level X2Go broker interface.

    This class hides the details of working with 'PytisSshProfiles' API behind
    a few simple public methods.

    """
    _DEFAULT_SSH_PORT = 22
    _URL_MATCHER = re.compile('^(?P<protocol>(ssh|http(|s)))://'
                              '(|(?P<username>[a-zA-Z0-9_\.-]+)'
                              '(|:(?P<password>.*))@)'
                              '(?P<server>[a-zA-Z0-9\.-]+)'
                              '(|:(?P<port>[0-9]+))'
                              '($|(?P<path>/.*)$)')

    def __init__(self, url, password=None):
        parameters, path = self._split_url(url)
        self._connection_parameters = parameters
        self._path = path
        self._password = password
        self._upgrade_parameters = None

    def _split_url(self, url):
        match = self._URL_MATCHER.match(url)
        if not match:
            raise Exception("Invalid URL: %s", url)
        parameters = match.groupdict()
        parameters['port'] = int(parameters['port'] or self._DEFAULT_SSH_PORT)
        if parameters.pop('protocol', None) not in (None, 'ssh'):
            raise Exception("Unsupported broker protocol: %s" % url)
        path = parameters.pop('path')
        return parameters, path

    def _list_profiles(self, **connection_parameters):
        def session_parameters(profile_id):
            parameters = profiles.to_session_params(profile_id)
            if isinstance(parameters['server'], list):
                parameters['server'] = parameters['server'][0]
            rootless = profiles.session_profiles[profile_id].get('rootless', True)
            if not rootless or int(rootless) == 0:
                parameters['session_type'] = 'desktop'
                parameters['geometry'] = 'maximize'
                parameters['known_hosts'] = os.path.join(x2go.LOCAL_HOME, x2go.X2GO_SSH_ROOTDIR,
                                                         'known_hosts')
            return parameters
        try:
            profiles = PytisSshProfiles(connection_parameters,
                                        broker_path=self._path,
                                        broker_password=self._password,
                                        logger=x2go.X2GoLogger(tag='PytisClient'))
        except PytisSshProfiles.ConnectionFailed:
            return None
        else:
            self._connection_parameters.update(connection_parameters)
            self._upgrade_parameters = profiles.pytis_upgrade_parameters()
            return sorted([(profile_id, session_parameters(profile_id))
                           for profile_id in profiles.profile_ids],
                          key=lambda x: x[1]['profile_name'])

    def list_profiles(self, authenticate):
        """Return a list of two-tuples (profile_id, session_parameters).

        The list is sorted by profile name.  'session_parameters' is a
        dictionary of X2Go session parameters.

        """
        return authenticate(self._list_profiles, self._connection_parameters)

    def server(self):
        """Return broker's server hostname as a string."""
        return self._connection_parameters['server']

    def username(self):
        """Return broker authentication username as a string."""
        return self._connection_parameters['username']

    def url(self):
        """Return broker URL as a string."""
        params = self._connection_parameters
        return "ssh://%s%s@%s%s/%s" % (
            params['username'],
            ':' + params['password'] if params['password'] else '',
            params['server'],
            ':' + params['port'] if params['port'] != self._DEFAULT_SSH_PORT else '',
            self._path,
        )

    def upgrade_parameters(self):
        """Return Pytis upgrade parameters as (version, connection_parameters, path).

        'version' is the available upgrade version as a string,
        'connection_parameters' is a dictionary of upgrade server connection parameters,
        'path' is the upgrade server path.

        Must be called after 'list_profiles()' and only if 'list_profiles()'
        doesn't return None.  Otherwise the behavior is undefined.

        """
        version, url = self._upgrade_parameters()
        connection_parameters, path = self._split_url(url)
        return version, connection_parameters, path

    def upload_key(self, username, key, key_file):
        """Upload user's public key to the broker server."""
        import StringIO
        upload_key = paramiko.RSAKey.from_private_key(StringIO.StringIO(pytis.config.upload_key))
        pubkey = "{} {} {}".format(key.get_name(), key.get_base64(), username)
        filename = "{}_{}.pub".format(datetime.datetime.now().strftime("%Y%m%d%H%M"), username)
        transport = paramiko.Transport((self._connection_parameters['server'],
                                        self._connection_parameters['port']))
        try:
            transport.connect(username='p2gokeys', pkey=upload_key)
            sftp = paramiko.SFTPClient.from_transport(transport)
            with sftp.open('p2gokeys/{}'.format(filename), 'w') as f:
                f.write(pubkey)
        finally:
            transport.close()


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
        win_apps_path = os.path.normpath(os.path.join(sys.path[0], '..', '..', 'win_apps'))
        for option in self.iniConfig.options(xserver_name):
            if option in ('test_installed', 'run_command'):
                defaults_path = _defaults[option]
                d, f = os.path.split(defaults_path)
                _xserver_config[option] = self._fix_win_path(
                    os.path.join(win_apps_path, os.path.split(d)[-1], f))
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


class XServer(object):

    def __init__(self, variant='VcXsrv_pytis_old', loglevel=x2go.log.loglevel_DEFAULT):
        self.logger = x2go.log.X2GoLogger(loglevel=loglevel)
        xconfig = X2GoClientXConfig(config_files=os.path.normpath(os.path.join(
            x2go.LOCAL_HOME,
            x2go.X2GO_CLIENT_ROOTDIR,
            x2go.defaults.X2GO_XCONFIG_FILENAME,
        )))
        xserver = None
        if xconfig.running_xservers:
            self.logger('X-server is already running!', loglevel=x2go.log.loglevel_WARN)
        elif not xconfig.installed_xservers:
            self.logger("No installed X-servers found!", loglevel=x2go.log.loglevel_WARN)
        else:
            server_config = xconfig.get_xserver_config(variant)
            self.logger("Starting X-server %s: %s" % (variant, server_config),
                        loglevel=x2go.log.loglevel_DEFAULT)
            xserver = x2go.xserver.X2GoXServer(variant, server_config)
        self._xserver = xserver


def tunnel_tcp_handler(chan, (origin_addr, origin_port), (server_addr, server_port)):
    """This function redefines 'x2go.rforward.x2go_transport_tcp_handler'.

    The x2go definition specifically handles the tunnels defined by the X2Go
    module ('sshfs' and 'snd'), but we need to nandle the 'rpyc' tunnel as
    well.  For some reason, the function can not be defined as 'RPyCTunnel'
    method (this causes a segfault).

    """
    transport = chan.get_transport()
    transport._queue_incoming_channel(chan)
    for reverse_tunnels in transport.reverse_tunnels.values():
        for tun_id, (tun_port, tunnel) in reverse_tunnels.items():
            if int(server_port) == int(tun_port):
                tunnel.notify()
                return

class RPyCTunnel(x2go.rforward.X2GoRevFwTunnel):
    """Tunnel RPyC communication from X2Go server to the RPyC server on X2Go client.

    We overide 'x2go.rforward.X2GoRevFwTunnel' to make automatic selection of
    available server port (the port on the X2Go server side).  We need to pass
    0 as server_port when calling 'request_port_forward()', update the
    'server_port' attribute according to the 'request_port_forward()' return
    value and run a callback which will update the port number also for the
    X2GoClient instance.

    """

    def __init__(self, rpyc_server_port, terminal_session, on_tunnel_started):
        super(RPyCTunnel, self).__init__(
            server_port=None,
            remote_host='127.0.0.1',
            remote_port=rpyc_server_port,
            ssh_transport=terminal_session.control_session.get_transport(),
            session_instance=terminal_session.session_instance,
            logger=terminal_session.logger,
        )
        self._on_tunnel_started = on_tunnel_started

    def _request_port_forwarding(self):
        port = self.ssh_transport.request_port_forward('127.0.0.1', 0,
                                                       handler=tunnel_tcp_handler)
        self.server_port = port
        self._on_tunnel_started(port)

    def resume(self):
        if self._accept_channels is False:
            self._accept_channels = True
            self._request_port_forwarding()
            self.logger('resumed thread: %s' % repr(self), loglevel=x2go.log.loglevel_DEBUG)


class RPyCServerLauncher(threading.Thread):
    """Thread controlling the RPyC server."""

    _DEFAULT_PORT = 10000
    _MAX_PORT_ATTEMPTS = 100

    def __init__(self, on_server_started, on_echo, logger=None, loglevel=x2go.log.loglevel_DEFAULT):
        if logger is None:
            self.logger = x2go.log.X2GoLogger(loglevel=loglevel)
        else:
            self.logger = copy.deepcopy(logger)
        self.logger.tag = self.__class__.__name__
        self._keepalive = None
        self._server = None
        self._server_thread = None
        self._on_server_started = on_server_started
        self._on_echo = on_echo
        self._port = None
        self._password = None
        threading.Thread.__init__(self)
        self.daemon = True
        self.start()

    def _server_running(self):
        # Look for a running RPyC server instance.
        # Check whether it is our instance.  We must be careful here not to send
        # the password to an RPyC instance of another user.
        if not self._server_thread or not self._server_thread.is_alive():
            return False
        if not self._server:
            return False
        if not self._port or not self._password:
            return False
        authenticator = pytisproc.PasswordAuthenticator(self._password)
        try:
            authenticator.connect('localhost', self._port)
        except:
            return False
        return True

    def _start_server(self):
        callback = self._on_echo

        class Service(pytisproc.PytisUserService):
            def exposed_echo(self, text):
                callback()
                return super(Service, self).exposed_echo(text)
        default_port = self._DEFAULT_PORT
        max_port = default_port + self._MAX_PORT_ATTEMPTS
        authenticator = pytisproc.PasswordAuthenticator()
        for port in xrange(default_port, max_port):
            try:
                server = rpyc.utils.server.ThreadedServer(Service,
                                                          hostname='localhost',
                                                          port=port,
                                                          authenticator=authenticator)
            except:
                continue
            server.service.authenticator = authenticator
            thread = threading.Thread(target=server.start)
            thread.start()
            try:
                # Detect whether it really works, ie. the port is not blocked...
                authenticator.connect('localhost', port)
            except:
                server.close()
                continue
            self.logger('RPyC server started on port %d' % port,
                        loglevel=x2go.log.loglevel_DEBUG)
            self._server = server
            self._server_thread = thread
            self._port = port
            self._password = password = authenticator.password()
            self._on_server_started(port, password)
            return
        raise ClientException("No free port found for RPyC in the range %s-%s" %
                              (default_port, max_port - 1,))

    def _shutdown_server(self):
        self.logger('Terminating RPyC server on port %d' % self._server.port,
                    loglevel=x2go.log.loglevel_DEBUG)
        self._server.close()
        gevent.sleep(1)
        self._password = self._port = None

    def run(self):
        self._keepalive = True
        while self._keepalive:
            if not self._server_running():
                if self._server:
                    self._shutdown_server()
                self._start_server()
            gevent.sleep(1)
        if self._server:
            self._shutdown_server()
        self.logger('RPyC server terminated', loglevel=x2go.log.loglevel_DEBUG)

    def stop_thread(self):
        """Tear down a running RPyC server."""
        self.logger('RPyCServerLauncher.stop_thread() has been called',
                    loglevel=x2go.log.loglevel_DEBUG)
        self._keepalive = False


class TerminalSession(x2go.backends.terminal.plain.X2GoTerminalSession):

    def start_rpyc_tunnel(self, rpyc_server_port, on_tunnel_started):
        reverse_tunnels = self.reverse_tunnels[self.session_info.name]
        if 'rpyc' not in reverse_tunnels:
            reverse_tunnels['rpyc'] = (0, None)
        tunnel = reverse_tunnels['rpyc'][1]
        if tunnel and tunnel.remote_port != rpyc_server_port:
            self.stop_rpyc_tunnel()
            tunnel = None
        if tunnel is None:
            def callback(forwarded_port):
                reverse_tunnels['rpyc'] = (forwarded_port, tunnel)
                on_tunnel_started(forwarded_port)
                log(EVENT, 'Port %d on X2Go server forwarded to RPyC server port %d '
                    'on X2Go client.' % (forwarded_port, rpyc_server_port))
            tunnel = RPyCTunnel(rpyc_server_port, self, on_tunnel_started=callback)
            self.active_threads.append(tunnel)
            tunnel.start()
        else:
            tunnel.resume()
        return tunnel

    def stop_rpyc_tunnel(self):
        for t in self.active_threads:
            if type(t) == RPyCTunnel:
                t.stop_thread()
                del t


class X2GoClient(x2go.X2GoClient):

    _DEFAULT_SESSION_PARAMETERS = dict(
        gss_auth=False,
        look_for_keys=True,
        allow_agent=True,
        profile_name='Pytis-Client-Session',
        cache_type='unix-kde',
        allow_share_local_folders=True
    )

    class ServerInfo(object):
        # It would be better to use self.info_backend or
        # self.get_session_info(s_uuid) as the source of remote_container
        # (instead of _x2go_remote_home + '/.x2go') and agent_pid (instead of
        # application) but this information is often unavailable here for
        # unclear reasons.

        def __init__(self, terminal_session):
            self._port = None
            self._password = None
            self._changed = False
            self._control_session = control_session = terminal_session.control_session
            self._server_file_name = '%s/.x2go/ssh/pytis.%s' % (
                control_session._x2go_remote_home,
                terminal_session.session_info.name,
            )

        def port(self):
            return self._port

        def set_port(self, port):
            if self._port != port:
                self._changed = True
                self._port = port

        def password(self):
            return self._password

        def set_password(self, password):
            if self._password != password:
                self._changed = True
                self._password = password

        def changed(self):
            return self._changed

        def write(self):
            if self._port is None or self._password is None:
                return
            if self._changed:
                data = '0:%s:%s:' % (self._port, self._password,)
                self._control_session._x2go_sftp_write(self._server_file_name, data)
                self._changed = False

    def __init__(self, session_parameters, on_rpyc_echo,
                 loglevel=x2go.log.loglevel_DEBUG, **kwargs):
        self._session_parameters = session_parameters
        session_parameters = dict(self._DEFAULT_SESSION_PARAMETERS, **session_parameters)
        self._settings = settings = X2GoClientSettings()
        for param, option, t in (('server', 'hostname', None),
                                 ('port', 'port', int),
                                 ('password', 'password', None),
                                 ('key_filename', 'key_filename', None)):
            if not session_parameters[param]:
                try:
                    session_parameters[param] = settings.get_value('pytis', option, t)
                except ClientException:
                    pass
        # Clean tempdir.
        tempdir = tempfile.gettempdir()
        for f in os.listdir(tempdir):
            if f.startswith('pytistmp'):
                try:
                    os.remove(os.path.join(tempdir, f))
                except:
                    pass
        x2go.X2GoClient.__init__(self, start_xserver=False, use_cache=False,
                                 loglevel=loglevel, **kwargs)
        self.terminal_backend = TerminalSession
        self._x2go_session_hash = self._X2GoClient__register_session(**session_parameters)
        session = self.session_registry(self._x2go_session_hash)
        session.sshproxy_params['key_filename'] = session_parameters['key_filename']
        session.sshproxy_params['look_for_keys'] = False
        # Start up server connections.
        self._rpyc_tunnel_port = gevent.event.AsyncResult()
        self._rpyc_server_port = None
        self._rpyc_server_password = gevent.event.AsyncResult()
        self._rpyc_launcher = RPyCServerLauncher(on_server_started=self._on_rpyc_server_started,
                                                 on_echo=on_rpyc_echo, logger=self.logger)
        try:
            self._X2GoClient__connect_session(self._x2go_session_hash,
                                          username=session_parameters['username'],
                                          password=session_parameters['password'])
        # TODO: We don't handle these exceptions because we should already have
        # valid authentication credentials thanks to calling 'ssh_connect()' prior
        # to X2GoClient instance creation.
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
        except socket.error, e:
            runtime_error('a socket error occured while establishing the connection: %s' %
                          str(e), exitcode=-245)

    def _on_rpyc_server_started(self, port, password):
        self._rpyc_server_port = port
        self._rpyc_server_password.set(password)

    def _on_rpyc_tunnel_started(self, port):
        self._rpyc_tunnel_port.set(port)

    def _init_session(self):
        def info_handler():
            while self.session_ok(self._x2go_session_hash):
                self._handle_info()
                gevent.sleep(0.1)
        terminal_session = self.get_session(self._x2go_session_hash).terminal_session
        self._pytis_server_info = self.ServerInfo(terminal_session)
        gevent.spawn(info_handler)

    def _handle_info(self):
        try:
            password = self._rpyc_server_password.get_nowait()
            port = self._rpyc_tunnel_port.get_nowait()
        except gevent.Timeout:
            return
        info = self._pytis_server_info
        if password != info.password() or port != info.port():
            info.set_password(password)
            info.set_port(port)
            if info.changed():
                info.write()

    def _cleanup(self):
        terminal_session = self.get_session(self._x2go_session_hash).terminal_session
        terminal_session.stop_rpyc_tunnel()
        self._rpyc_launcher.stop_thread()
        x2go.x2go_cleanup()

    def list_sessions(self):
        """Return a list of suspended sessions found on the server.

        Returns a list of 'X2GoServerSessionInfo' instances.

        """
        return sorted([info for info in
                       self._X2GoClient__list_sessions(self._x2go_session_hash).values()
                       if info.status == 'S'],
                      lambda a, b: (cmp(a.username, b.username) or
                                    cmp(a.hostname, b.hostname) or
                                    cmp(b.date_created, a.date_created)))

    def terminate_session(self, session):
        """Terminate given X2Go Session."""
        # send a terminate request to a session
        self.logger('Requesting termination od X2Go session: %s' % session.name,
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

    def start_new_session(self):
        """Launch a new X2Go session."""
        self.logger('starting a new X2Go session', loglevel=x2go.loglevel_INFO)
        self.logger('command for new session is: %s' % self._session_parameters['cmd'],
                    loglevel=x2go.log.loglevel_DEBUG)
        self._X2GoClient__start_session(self._x2go_session_hash)
        self._init_session()

    def resume_session(self, session):
        """Resume given server-side suspended X2Go session."""
        self.logger('resuming X2Go session: %s' % session.name, loglevel=x2go.loglevel_INFO)
        available_sessions = self._X2GoClient__list_sessions(self._x2go_session_hash)
        if session.name not in available_sessions.keys():
            runtime_error('Session %s not available on X2Go server [%s]:%s' %
                          (self._session_parameters['server'],
                           self._session_parameters['port']),
                          exitcode=20)
        self._X2GoClient__resume_session(self._x2go_session_hash, session.name)
        self._init_session()

    def main_loop(self):
        """Monitor the running X2Go session and wait for its completion.

        Periodically checks the state of the running session and waits for its
        completion.  Call this method after 'start_new_session()' or
        'resume_session()'.

        """
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
                terminal_session = self.get_session(session_hash).terminal_session
                tunnel = None
                while self._X2GoClient__session_ok(session_hash):
                    server_port = self._rpyc_server_port
                    if server_port and (not tunnel or tunnel.remote_port != server_port):
                        # This will actually restart the tunnel if already running.
                        tunnel = terminal_session.start_rpyc_tunnel(
                            server_port,
                            on_tunnel_started=self._on_rpyc_tunnel_started,
                        )
                    time.sleep(2)
        except x2go.X2GoSessionException, e:
            self.logger("X2GoSessionException occured:", loglevel=x2go.loglevel_ERROR)
            self.logger("-> %s" % str(e), loglevel=x2go.loglevel_ERROR)
        finally:
            self._cleanup()


class ClientService(rpyc.Service):
    """RPyC Service exposing the public API of X2GoClient to the parent process.

    This class is not meant to be used directly.  Use 'ClientProcess' instead.

    """
    class Authenticator(object):
        def __init__(self, key):
            self._key = key

        def __call__(self, sock):
            if sock.recv(len(key)) != key:
                raise rpyc.utils.authenticators.AuthenticationError("wrong magic word")
            return sock, None

        def connect(self, host, port):
            conn = rpyc.connect(host, port, config=dict(allow_pickle=True,
                                                        allow_public_attrs=True))
            if hasattr(socket, 'fromfd'):
                fd = conn.fileno()
                sock = socket.fromfd(fd, socket.AF_INET, socket.SOCK_STREAM)
            else:
                # There is no socket.fromfd in Python 2.x on Windows, so let's use
                # the original hidden socket.
                sock = conn._channel.stream.sock
            sock.send(self._key)
            return conn

    def exposed_connect(self, connection_parameters, on_echo=None):
        self._client = X2GoClient(connection_parameters,
                                  on_rpyc_echo=rpyc.async(on_echo) if on_echo else None)

    def exposed_list_sessions(self):
        return self._client.list_sessions()

    def exposed_terminate_session(self, session):
        self._client.terminate_session(session)

    def exposed_resume_session(self, session):
        self._client.resume_session(session)

    def exposed_start_new_session(self):
        self._client.start_new_session()

    def exposed_main_loop(self):
        self._client.main_loop()


class ClientProcess(object):
    """Run Pytis X2Go client in a separate process and control it from another Python process.

    This class invokes a new process and runs a Pytis X2Go client inside it.
    The running 'X2GoClient' instance inside this process may be controlled
    through the public methods of this class's instance.

    This class hides the details of spawning a new Python process, connecting
    and authenticating to its RPyC service and working with its RPyC interface
    behind a simple API.  The 'X2GoClient' run's in another process, but its
    public methods may be called as if the client was running locally.

    """

    def __init__(self, session_parameters, port=18861, on_echo=None):
        """Start a Python subprocess running 'X2GoClient' and 'ClientService' server.

        Arguments:
          session_parameters -- X2Go session parameters as a dictionary
          port -- port number for the 'ClientService' RPyC server
          on_echo -- callback to run on each 'PytisService' echo call.  A Pytis
            application running on the X2Go server calls echo on startup and
            then every few minutes, so the callback may be used to detect
            a running Pytis application.

        """
        self._process = subprocess.Popen((sys.executable, '-m', 'pytis.x2goclient.x2goclient',
                                          '--port', str(port)),
                                         env=dict(os.environ.copy(), PYTHONPATH=":".join(sys.path)),
                                         stdin=subprocess.PIPE)
        # Generate a secret token and pass it to the subprocess through its STDIN.
        key = hashlib.sha256(os.urandom(16)).hexdigest()
        self._process.stdin.write(key + '\n')
        # Wait for the RPyC server inside the subprocess to come up.
        time.sleep(3)
        authenticator = ClientService.Authenticator(key)
        self._conn = authenticator.connect('localhost', port=port)
        # Start a new X2GoClient instance inside the subprocess.
        self._conn.root.connect(session_parameters, on_echo=on_echo)

    def list_sessions(self):
        """Return the result of 'X2GoClient.list_sessions()' on the subprocess instance."""
        return self._conn.root.list_sessions()

    def terminate_session(self, session):
        """Call 'X2GoClient.terminate_session()' on the subprocess instance."""
        self._conn.root.terminate_session(session)

    def resume_session(self, session):
        """Call 'X2GoClient.resume_session()' on the subprocess instance."""
        self._conn.root.resume_session(session)

    def start_new_session(self):
        """Call 'X2GoClient.start_new_session()' on the subprocess instance."""
        self._conn.root.start_new_session()

    def main_loop(self):
        """Call 'X2GoClient.main_loop()' on the subprocess instance."""
        self._conn.root.main_loop()

    def terminate(self):
        """Terminate the subprocess and the 'X2GoClient' instance running inside it."""
        self._process.terminate()


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(
        description=(
            """Run a Pytis X2Go client in a separate process controlled through an RPyC API.

            When this module is run as 'python -m pytis.x2goclient.x2goclient ...' it
            starts an RPyC service with the interface defined by 'ClientService'.  A
            new Pytis X2Go client may be invoked and controlled through this service
            passing it X2Go session parameters from another Python process.

            However you don't normally want to run this module and connect its RPyC
            service directly.  Use the Python class 'ClientProcess' to do so through
            its API.

            """),
        # formatter_class=argparse.RawDescriptionHelpFormatter,
        add_help=True, argument_default=None
    )
    parser.add_argument(
        '--port',
        default=18861, type=int,
        help="Port number where to start the RPyC server.",
    )
    args = parser.parse_args()

    # Read the service authentication key from STDIN
    key = sys.stdin.readline().strip()

    server = rpyc.utils.server.OneShotServer(
        ClientService, port=args.port,
        protocol_config=dict(allow_public_attrs=True),
        authenticator=ClientService.Authenticator(key),
    )
    server.start()
