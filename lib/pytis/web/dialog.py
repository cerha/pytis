# -*- coding: utf-8 -*-

# Copyright (C) 2007, 2008, 2011 Brailcom, o.p.s.
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

"""Web dialogs.

This module implements web-based dialogs.

"""

from pytis.web import *

_ = lcg.TranslatableTextFactory('pytis')

class Dialog(lcg.Content):

    def __init__(self, hidden=(), handler='#', action=None):
        assert isinstance(hidden, (tuple, list))
        assert isinstance(handler, basestring)
        assert isinstance(action, basestring) or action is None
        super(Dialog, self).__init__()
        self._hidden = list(hidden)
        self._handler = handler
        self._action = action
    
    def _export_content(self, exporter):
        return ()

    def export(self, exporter):
        g = exporter.generator()
        content = self._export_content(exporter)
        hidden = self._hidden
        if self._action:
            hidden += [('action', self._action)]
        # Translators: Default edit form submit button label.
        content += tuple([g.hidden(k, v) for k, v in hidden]) + \
                   (g.submit(_(u"Submit")),)
        return g.form(content, action=self._handler, cls="dialog", method="POST") + "\n"


class SelectionDialog(Dialog):
    
    def __init__(self, id, label, values, selected=None, **kwargs):
        super(SelectionDialog, self).__init__(**kwargs)
        assert isinstance(id, basestring)
        assert isinstance(label, basestring)
        assert isinstance(values, (tuple, list))
        assert selected is None or selected in [v for v, uv in values]
        self._id = id
        self._label = label
        self._values = values
        self._selected = selected
        
    def _export_content(self, exporter):
        g = exporter.generator()
        label = g.label(self._label, self._id)
        ctrl = g.select(self._id, [(uv, str(v)) for v, uv in self._values],
                            id=self._id, selected=self._selected)
        return (label, ": ", ctrl)
