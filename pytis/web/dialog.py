# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
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

    def _export_content(self, context):
        return ()

    def _export_buttons(self, context):
        g = context.generator()
        # Translators: Default edit form submit button label.
        return (g.button(g.span(_("Submit")), type='submit'),)

    def _css_class(self):
        return 'dialog'

    def export(self, context):
        g = context.generator()
        hidden = self._hidden
        if self._action:
            hidden = hidden + [('action', self._action)]
        content = (self._export_content(context) +
                   tuple(g.hidden(k, v) for k, v in hidden) +
                   self._export_buttons(context))
        return g.form(content, action=self._handler, cls=self._css_class(), method="POST") + "\n"


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

    def _export_content(self, context):
        g = context.generator()
        label = g.label(self._label, self._id)
        ctrl = g.select(name=self._id, id=self._id, content=[
            g.option(uv, value=str(v), selected=self._selected) for v, uv in self._values])
        return (label, ": ", ctrl)


class ConfirmationDialog(Dialog):
    """Dialog asking the user to confirm an action.

    The prompt is followed by the confirmation button and the cancel button.
    The submitted form contains the request parameter 'submit' or '_cancel'
    (with value '1') according to the button pressed.

    The confirmation button receives the initial focus and the prompt is its
    description, so that screen readers read the prompt together with the
    button.  The same applies to the record deletion confirmation in
    'DeletionForm'.

    """

    def __init__(self, prompt, submit_label=_("Confirm"), submit_icon='ok-icon',
                 cancel_label=_("Cancel"), cancel_icon='remove-icon', **kwargs):
        """Arguments:

          prompt: the question to be confirmed as a string or 'lcg.Content'.
          submit_label: label of the confirmation button.
          submit_icon: icon CSS class name of the confirmation button or None.
          cancel_label: label of the cancel button or None to omit the button.
          cancel_icon: icon CSS class name of the cancel button or None.

          See the parent class for definition of the remaining arguments.

        """
        assert isinstance(prompt, (str, lcg.Content)), prompt
        super().__init__(**kwargs)
        self._prompt = prompt
        self._submit_label = submit_label
        self._submit_icon = submit_icon
        self._cancel_label = cancel_label
        self._cancel_icon = cancel_icon
        self._prompt_id = None

    def _css_class(self):
        return 'dialog confirmation-dialog'

    def _export_content(self, context):
        prompt = self._prompt
        if isinstance(prompt, lcg.Content):
            prompt = prompt.export(context)
        self._prompt_id = context.unique_id()
        return (context.generator().div(prompt, id=self._prompt_id, cls='confirmation-prompt'),)

    def _export_buttons(self, context):
        g = context.generator()

        def button(label, icon, **kwargs):
            return g.button(g.span('', cls=' '.join(('icon', icon)) if icon else 'icon') +
                            g.span(label, cls='label'),
                            type='submit', value='1', **kwargs)
        buttons = [button(self._submit_label, self._submit_icon, name='submit', cls='submit',
                          autofocus=True, aria_describedby=self._prompt_id)]
        if self._cancel_label:
            buttons.append(button(self._cancel_label, self._cancel_icon,
                                  name='_cancel', cls='cancel'))
        # The generic icons and the buttons are styled there.
        context.resource('lcg-widgets.css')
        context.resource('pytis-forms.css')
        return (g.div(buttons, cls='submit-buttons'),)
