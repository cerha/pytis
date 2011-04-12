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
import rpyc
import subprocess

import config

def _nx_ip():
    nxsessionid = os.getenv('NXSESSIONID')
    if nxsessionid is None:
        return None
    session_id = ':' + nxsessionid.split('-')[1]
    p = subprocess.Popen('who', stdout=subprocess.PIPE, shell=True)
    output, __ = p.communicate()
    for line in output.splitlines():
        items = line.split()
        if items[1] == session_id:
            return items[4][1:-1]
    return None
    
def _request(request, *args, **kwargs):
    target_ip = _nx_ip()
    connection = rpyc.connect('localhost', config.rpc_local_port)
    return connection.root.request(target_ip, request, *args, **kwargs)
    
def get_clipboard_text():
    return _request('get_clipboard_text')

def set_clipboard_text(text):
    assert isinstance(text, unicode), text
    return _request('set_clipboard_text', text)

def launch_file(path):
    assert isinstance(path, basestring), path
    wpath = path.replace('/', '\\')
    return _request('launch_file', wpath)
