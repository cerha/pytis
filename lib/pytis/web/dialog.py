# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2007-2017 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Web dialogs.

This module implements web-based dialogs.

"""
from __future__ import print_function

import lcg
import pytis.util

_ = pytis.util.translations('pytis-web')


class Dialog(lcg.Content):

    def __init__(self, hidden=(), handler='#', action=None):
        assert isinstance(hidden, (tuple, list))
        assert isinstance(handler, str)
        assert isinstance(action, str) or action is None
        super().__init__()
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
        content += tuple([g.hidden(k, v) for k, v in hidden] +
                         [g.button(g.span(_(u"Submit")), type='submit')])
        return g.form(content, action=self._handler, cls="dialog", method="POST") + "\n"


class SelectionDialog(Dialog):

    def __init__(self, id, label, values, selected=None, **kwargs):
        super().__init__(**kwargs)
        assert isinstance(id, str)
        assert isinstance(label, str)
        assert isinstance(values, (tuple, list))
        assert selected is None or selected in [v for v, uv in values]
        self._id = id
        self._label = label
        self._values = values
        self._selected = selected

    def _export_content(self, exporter):
        g = exporter.generator()
        label = g.label(self._label, self._id)
        ctrl = g.select(name=self._id, id=self._id, content=[
            g.option(uv, value=str(v), selected=self._selected) for v, uv in self._values])
        return (label, ": ", ctrl)
