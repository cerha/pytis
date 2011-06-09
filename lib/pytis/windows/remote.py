# -*- coding: utf-8 -*-

# Copyright (C) 2011 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
import pwd
import re
import rpyc
import subprocess

from pytis.util import *
import config

_ipv4_regexp = r'[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+'
_ipv6_regexp = r'.*:.*:.*'
_ip_matcher = re.compile('%s|%s' % (_ipv4_regexp, _ipv6_regexp,))
_nx_ip = UNDEFINED
def nx_ip():
    """Return IP address of the nx client, as a string.

    If pytis is not run from an nx client, return 'None'.
    
    """
    global _nx_ip
    if _nx_ip is not UNDEFINED:
        return _nx_ip
    _nx_ip = None
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

def windows_available():
    """Return true, iff Windows client is available."""
    try:
        return self._request('echo', 'hello') == 'hello'
    except:
        return False

_connection = None
def _request(request, *args, **kwargs):
    global _connection
    target_ip = nx_ip()
    user_name = pwd.getpwuid(os.getuid())[0]
    try:
        _connection.root.request
    except:
        _connection = rpyc.connect('localhost', config.rpc_local_port)
    return _connection.root.request(target_ip, user_name, request, *args, **kwargs)
    
def get_clipboard_text():
    try:
        return _request('get_clipboard_text')
    except:
        return None

def set_clipboard_text(text):
    assert isinstance(text, unicode), text
    try:
        _request('set_clipboard_text', text)
    except:
        pass

def launch_file(path):
    assert isinstance(path, basestring), path
    wpath = path.replace('/', '\\')
    try:
        return _request('launch_file', wpath)
    except Exception, e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("Soubor %s se nepodařilo otevřít: %s") % (wpath, e,))

def launch_url(url):
    assert isinstance(url, basestring), url
    try:
        return _request('launch_file', url)
    except:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("URL %s se nepodařilo otevřít") % (url,))

def make_temporary_file(suffix='', encoding=None):
    assert isinstance(suffix, basestring), suffix
    assert encoding is None or isinstance(encoding, basestring), encoding
    try:
        return _request('make_temporary_file', suffix=suffix, encoding=encoding)
    except:
        return None
