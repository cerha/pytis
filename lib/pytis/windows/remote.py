# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012, 2013 Brailcom, o.p.s.
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
import subprocess

from pytis.util import DEBUG, UNDEFINED, log, translations
import config

_ = translations('pytis-wx')


_ipv4_regexp = r'[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+'
_ipv6_regexp = r'.*:.*:.*'
_ip_matcher = re.compile('%s|%s' % (_ipv4_regexp, _ipv6_regexp,))
_nx_ip = UNDEFINED
_x2go_ip = None
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
    return x2go_ip() or nx_ip()

def windows_available():
    """Return true, iff Windows client is available."""
    if not config.rpc_communication_enabled or client_ip() is None:
        log(DEBUG, "RPC unavailable")
        return False
    try:
        return _request('echo', 'hello') == 'hello'
    except Exception, e:
        log(DEBUG, "RPC exception:", e)
        return False

_connection = None
def _request(request, *args, **kwargs):
    global _connection
    target_ip = client_ip()
    user_name = pwd.getpwuid(os.getuid())[0]
    try:
        _connection.root.request
    except:
        import rpyc
        _connection = rpyc.connect('localhost', config.rpc_local_port)
    return _connection.root.request(target_ip, user_name, request, *args, **kwargs)

def version():
    try:
        version = _request('version')
    except:
        try:
            if _request('echo', 'hello') == 'hello':
                version = '-old-'
            else:
                version = ''
        except:
            version = ''
    return version

def get_clipboard_text():
    try:
        text = _request('get_clipboard_text')
    except:
        return None
    if text:
        text = text.replace('\r\n', '\n')
    return text

def set_clipboard_text(text):
    assert isinstance(text, unicode), text
    text = text.replace('\n', '\r\n')
    try:
        _request('set_clipboard_text', text)
    except:
        pass

def launch_file(path):
    assert isinstance(path, basestring), path
    wpath = path.replace('/', '\\')
    try:
        return _request('launch_file', wpath)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error,
                              _("Soubor %s se nepodařilo otevřít: %s") % (wpath, e,))

def launch_url(url):
    assert isinstance(url, basestring), url
    try:
        return _request('launch_file', url)
    except:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error, _("URL %s se nepodařilo otevřít") % (url,))

def open_file(filename, mode, encoding=None):
    assert isinstance(filename, basestring), filename
    assert isinstance(mode, str), mode
    assert encoding is None or isinstance(encoding, basestring), encoding
    try:
        return _request('open_file', filename, mode, encoding=encoding)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error,
                              _("Nepodařilo se otevřít soubor %s: %s") % (filename, e,))

def open_selected_file(template=None):
    assert template is None or isinstance(template, basestring), template
    try:
        return _request('open_selected_file', template)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error,
                              _("Nepodařilo se vybrat soubor ke stažení: %s") % (e,))

def make_selected_file(directory=None, filename=None, template=None, encoding=None, mode='wb'):
    assert directory is None or isinstance(directory, basestring), directory
    assert filename is None or isinstance(filename, basestring), filename
    assert template is None or isinstance(template, basestring), template
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert mode is None or isinstance(mode, basestring), mode
    try:
        return _request('make_selected_file', directory=directory, filename=filename,
                        template=template, encoding=encoding, mode=mode)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error,
                              _("Nepodařilo se vybrat soubor k uložení: %s") % (e,))

def make_temporary_file(suffix='', encoding=None, mode='wb'):
    assert isinstance(suffix, basestring), suffix
    assert encoding is None or isinstance(encoding, basestring), encoding
    assert mode is None or isinstance(mode, basestring), mode
    try:
        return _request('make_temporary_file', suffix=suffix, encoding=encoding, mode=mode)
    except:
        return None

def select_directory():
    try:
        return _request('select_directory')
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error,
                              _("Nepodařilo se vybrat adresář: %s") % (e,))

def select_file(filename=None, template=None, multi=False):
    try:
        return _request('select_file', filename=filename, template=template, multi=multi)
    except Exception as e:
        import pytis.form
        pytis.form.run_dialog(pytis.form.Error,
                              _("Nepodařilo se vybrat soubor: %s") % (e,))
