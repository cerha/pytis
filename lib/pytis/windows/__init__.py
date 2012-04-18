# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012 Brailcom, o.p.s.
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

try:
    from proxy import run_proxy
except ImportError as e:
    def run_proxy(*args, **kwargs):
        raise e
from remote import nx_ip, windows_available, get_clipboard_text, set_clipboard_text, \
     launch_file, launch_url, open_selected_file, make_temporary_file
