#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011-2017 Brailcom, o.p.s.
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
gevent.monkey.patch_all()

import os
import sys

import argparse

import x2go
import x2go.defaults
import paramiko

import pytis.remote
import pytis.util
from pytis.remote.x2goclient import runtime_error

logger = x2go.X2GoLogger()
liblogger = x2go.X2GoLogger()

# exclusive client control options
action_options = [
    #{'args': ['-N', '--new'], 'default': False, 'action': 'store_true',
    # 'help': 'start a new X2Go session on the server even if suspended sessions exist', },
    #{'args': ['-R', '--resume'], 'default': None, 'metavar': 'SESSION_NAME',
    # 'help': 'resume a suspended X2Go session with name SESSION_NAME', },
    #{'args': ['-D', '--share-desktop'], 'default': None, 'metavar': 'USER@DISPLAY',
    # 'help': 'share an X2Go session on server specified by USER@DISPLAY', },
    #{'args': ['-S', '--suspend'], 'default': None, 'metavar': 'SESSION_NAME',
    # 'help': 'suspend running X2Go session SESSION_NAME', },
    #{'args': ['-T', '--terminate'], 'default': None, 'metavar': 'SESSION_NAME',
    # 'help': 'terminate running X2Go session SESSION_NAME', },
    #{'args': ['-L', '--list-sessions'], 'default': False, 'action': 'store_true', 'help':
    # 'list user\'s X2Go sessions on server', },
    #{'args': ['--list-desktops'], 'default': False, 'action': 'store_true',
    # 'help': 'list X2Go desktop sessions that are available for sharing', },
    {'args': ['-l', '--list-profiles'], 'default': False, 'action': 'store_true',
     'help': 'list user\'s X2Go pre-configured session profiles', },
    {'args': ['-P', '--session-profile'], 'default': None,
     'help': 'load x2goclient session profiles and use given profile to start a session', },
]
#if not pytis.util.on_windows():
#    action_options.append(
#        {'args': ['--from-stdin'], 'default': False, 'action': 'store_true',
#         'help': ('for LightDM remote login: '
#                  'read <username> <password> <host[:port]> <desktopshell> from STDIN'),
#         },
#    )
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
              '(very verbose, and even promiscuous)'), },
    {'args': ['-V', '--version'], 'default': False, 'action': 'store_true',
     'help': 'print version number and exit', },
]
# X2Go session options
x2go_options = [
    {'args': ['-c', '--command'],
     'help': 'command to run with -R mode on server (default: xterm)', },
    {'args': ['-u', '--username'],
     'help': 'username for the session (default: current user)', },
    {'args': ['--password'], 'default': None,
     'help': 'user password for session authentication', },
    {'args': ['-p', '--remote-ssh-port'], 'default': '22',
     'help': 'remote SSH port (default: 22)', },
    {'args': ['-k', '--ssh-privkey'], 'default': None,
     'help': ('use file \'SSH_PRIVKEY\' as private key for the '
              'SSH connection (e.g. ~/.ssh/id_rsa)'), },
    {'args': ['--add-to-known-hosts'], 'default': False, 'action': 'store_true',
     'help': ('add RSA host key fingerprint to ~/.ssh/known_hosts '
              'if authenticity of server can\'t be established (default: not set)'), },
    {'args': ['--sound'], 'default': 'pulse', 'choices': ('pulse', 'esd', 'none'),
     'help': 'X2Go server sound system (default: \'pulse\')', },
    {'args': ['--printing'], 'default': False, 'action': 'store_true',
     'help': 'use X2Go printing (default: disabled)', },
    # {'args': ['--share-mode'], 'default': 0,
    #  'help': 'share mode for X2Go desktop sharing (0: view-only, 1: full access)', },
    {'args': ['-F', '--share-local-folders'], 'metavar': '<folder1>[,<folder2[,...]]',
     'default': None,
     'help': 'a comma separated list of local folder names to mount in the X2Go session', },
    # {'args': ['--clean-sessions'], 'default': False, 'action': 'store_true',
    #  'help': 'clean all suspended sessions before starting a new one', },
    # {'args': ['--terminate-on-ctrl-c'], 'default': False, 'action': 'store_true',
    #  'help': ('terminate the connected session when pressing CTRL+C '
    #           '(instead of suspending the session)'), },
    # {'args': ['--auth-attempts'], 'default': 3,
    #  'help': 'number of authentication attempts before authentication fails (default: 3)', },
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
pytis2go_options = [
    {'args': ['--window-title'], 'default': None,
     'help': 'Override startup application main window title (default: Starting application)', },
    {'args': ['--heading'], 'default': None,
     'help': 'Override startup application main window heading (default: Pytis2Go)', },
]
if pytis.util.on_windows():
    pytis2go_options += [
        {'args': ['--create-shortcut'], 'default': False, 'action': 'store_true',
         'help': 'create desktop shortcut if not present (default: disabled)', },
    ]

# print version text and exit
def version():
    sys.stderr.write("%s\n" % (pytis.remote.X2GOCLIENT_VERSION,))
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
    option_groups = [(p_x2goopts, x2go_options),
                     (p_printopts, print_options),
                     (p_brokeropts, broker_options),
                     (p_actionopts, action_options),
                     (p_debugopts, debug_options),
                     (p_nxopts, nx_options)]
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
    if a.password and pytis.util.on_windows():
        runtime_error("The --password option is forbidden on Windows platforms", parser=p,
                      exitcode=222)
    if a.version:
        version()
    if not (a.session_profile or a.list_profiles):
        # check for mutual exclusiveness of -N, -R, -S, -T and -L, -N is
        # default if none of them is set
        #if ((bool(a.new) + bool(a.resume) + bool(a.share_desktop) + bool(a.suspend) +
        #     bool(a.terminate) + bool(a.list_sessions) + bool(a.list_desktops) +
        #     bool(a.list_profiles)) > 1):
        #    runtime_error("modes --new, --resume, --share-desktop, --suspend, --terminate, "
        #                  "--list-sessions, --list-desktops and "
        #                  "--list-profiles are mutually exclusive", parser=p, exitcode=2)
        #if ((bool(a.new) + bool(a.resume) + bool(a.share_desktop) + bool(a.suspend) +
        #     bool(a.terminate) + bool(a.list_sessions) + bool(a.list_desktops) +
        #     bool(a.list_profiles)) == 0):
        #    a.new = True
        # check if pack method is available
        if not x2go.utils.is_in_nx3packmethods(a.pack):
            runtime_error("unknown pack method '%s'" % args.pack, parser=p, exitcode=10)
    #else:
    #    if not (a.resume or a.share_desktop or a.suspend or a.terminate or a.list_sessions or
    #            a.list_desktops or a.list_profiles):
    #        a.new = True
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
    if a.server:
        # check if SERVER is in .ssh/config file, extract information from there...
        ssh_config = paramiko.SSHConfig()
        ssh_config_filename = os.path.join(x2go.LOCAL_HOME, x2go.X2GO_SSH_ROOTDIR, 'config')
        if not os.path.isfile(ssh_config_filename):
            try:
                x2go.utils.touch_file(ssh_config_filename)
            except OSError:
                pass
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
    #if not pytis.util.on_windows() and a.from_stdin:
    #    lightdm_remote_login_buffer = sys.stdin.readline()
    #    (a.username, a.server, a.command) = lightdm_remote_login_buffer.split()[0:3]
    #    a.password = " ".join(lightdm_remote_login_buffer.split()[3:])
    #    if ":" in a.server:
    #        a.remote_ssh_port = a.server.split(':')[-1]
    #        a.server = ':'.join(a.server.split(':')[:-1])
    #    a.command = a.command.upper()
    #    a.geometry = 'fullscreen'
    return p, a

# -----
# Start

def main():
    sys.path.append(os.path.normpath(os.path.join(sys.path[0], '..', 'lib')))

    # Windows specific setup
    if pytis.util.on_windows():
        X2GO_CLIENTXCONFIG_DEFAULTS = x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS
        X2GO_CLIENTXCONFIG_DEFAULTS.update(pytis.remote.XCONFIG_DEFAULTS)
        x2go.defaults.X2GO_CLIENTXCONFIG_DEFAULTS = X2GO_CLIENTXCONFIG_DEFAULTS
        reload(sys)
        sys.setdefaultencoding('cp1250')
        os.environ['NXPROXY_BINARY'] = os.path.normpath(os.path.join(
            sys.path[0], '..', '..', 'win_apps', 'nxproxy', 'nxproxy.exe',
        ))
        # Set locale language
        import ctypes
        lcid = ctypes.windll.kernel32.GetUserDefaultLCID()
        if not lcid:
            lcid = ctypes.windll.kernel32.GetSystemDefaultLCID()
        import locale
        os.environ["LANGUAGE"] = locale.windows_locale.get(lcid)

    parser, args = parseargs()
    session_parameters = dict(
        server=args.server,
        port=int(args.remote_ssh_port),
        username=args.username,
        password=args.password,
        key_filename=args.ssh_privkey,
        profile_id=args.session_profile,
        session_type=args.session_type,
        link=args.link,
        geometry=args.geometry,
        pack=args.pack,
        kblayout=args.kbd_layout,
        kbtype=args.kbd_type,
        snd_system=args.sound,
        printing=args.printing,
        print_action=args.print_action,
        print_action_args=dict((arg, getattr(args, arg)) for arg in
                               ('pdfview_cmd', 'save_to_folder', 'printer', 'print_cmd')
                               if getattr(args, arg)),
        share_local_folders=args.share_local_folders,
        cmd=args.command,
    )
    param2arg = {
        'cmd': 'command',
        'kblayout': 'kbd_layout',
        'kbtype': 'kbd_type',
        'snd_system': 'sound',
        'port': 'remote_ssh_port',
    }

    # Command line options with non-default values will override session profile parameters.
    force_parameters = [
        param for param, value in session_parameters.items()
        # Apply str() because of 'port' which is int in session_parameters and str in args.
        if value and str(value) != parser.get_default(param2arg.get(param, param)) and
        # TODO: print_action_arg is omited because the arg parser doesn't know its default.
        param != 'print_action_arg'
    ]

    app = pytis.remote.X2GoStartApp(args, session_parameters, force_parameters)
    app.MainLoop()


if __name__ == '__main__':
    main()
