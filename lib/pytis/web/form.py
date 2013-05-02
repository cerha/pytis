# -*- coding: utf-8 -*-
#
# Copyright (C) 2006-2013 Brailcom, o.p.s.
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

"""Web forms.

This module provides an implementation of Pytis forms which can be used for web
interfaces to Pytis informations systems.  The intention is to be able to
generate web forms from the same specification as GUI forms implemented by the
`pytis.forms' package.

Pytis currently does not include support running the actual web application.
Application framework which makes use of pytis web forms is implemented
separately.  See the Wiking project at http://www.freebsoft.org/wiking for more
information.

All the content generation is done using the LCG framework.  See
http://www.freebsoft.org/lcg.

"""

import collections
import string

from pytis.web import *
from pytis.presentation import ActionContext
import pytis.util

_ = pytis.util.translations('pytis-web')

class Type(pd.Type):
    def _validation_error(self, id, **kwargs):
        message = self._validation_messages[id]
        if kwargs:
            message = _(message, **kwargs)
        return pd.ValidationError(message)
    # Needed to postpone variable interpolation to translation time.
    pd.Type._validation_error = _validation_error

    
class BadRequest(Exception):
    """Exception raised by 'EditForm.ajax_response()' on invalid request parameters."""
    pass
    

class Form(lcg.Content):
    """Pytis HTML form as an LCG Content element.

    The form instance behaves as any other LCG Content element.  The HTML
    rendering is done in the export phase.
    
    URI provider interface

    URI provider makes Pytis web forms independent on the application's URI
    scheme.  It is a function which returns concrete URIs for different
    situations (determined by function arguments).

    The function 'uri_provider' must accept three positional arguments:
    
      record -- the 'pytis.presentation.PresentedRow' instance
      kind -- one of 'UriType' constants.  It is used for distinction of
        the purpose, for which the uri is used (eg. for a field link, image
        src, action link etc.).
      target -- the target for which the URI is requested.  This is the
        'Action' instance for 'UriType.ACTION'.  For other kinds this is
        either a field identifier (basestring) or None when requesting an
        URI for the whole record.

    The return value may be:
      - None when there should be no link,
      - string URI if the field links to that URI,
      - a 'pytis.web.Link' instance if it is necessary to specify also
        some extended link attributes, such as title (tooltip text) or
        target (such as _blank).
      - For array fields (when 'cid' belongs to a field of type
        'pytis.data.Array'), the return value must be a function of one
        argument -- the internal python value of the field's inner type.
        The function will return an URI or Link instance as above for
        given array value.
        
    """
    _HTTP_METHOD = 'POST'
    _CSS_CLS = None
    def __init__(self, view, req, row, handler='#', prefill=None, hidden=(), name=None,
                 uri_provider=None, actions=None, **kwargs):
        """Arguments:

          view -- presentation specification as a 'pytis.presentation.ViewSpec'
            instance.
          req -- instance of a class implementing the 'Request' API.
          row -- 'pytis.presentation.PresentedRow' instance.
          handler -- form handler URI as a string.  This URI is used in the
            form's 'action' attribute.
          prefill -- form prefill data as a dictionary of string values.
          uri_provider -- callable object (function) returning URIs for form
            fields.  The function must have the interface
            described in the class docstring.
          hidden -- hardcoded hidden form fields as a sequence of pairs (name,
            value).
          name -- form name as a string or None.  This name will be sent as a
            hidden field and used to distinguish which request parameters
            belong to which form.
          actions -- form actions as a sequence of pytis.presentation.Action
            instances or a callable object returning such a sequence when given
            two positional arguments: (FORM, RECORD), where FORM is the 'Form'
            instance and RECORD is the form record as a
            'pytis.presentation.PresentedRow' instance or None when asking for
            global actions (see 'pytis.presentation.ActionContext').  The
            default value (None) means to use the actions as defined in the
            specification.
            
        """
        super(Form, self).__init__(**kwargs)
        assert isinstance(view, ViewSpec), view
        assert isinstance(row, pytis.presentation.PresentedRow), row
        assert isinstance(handler, basestring), handler
        assert isinstance(hidden, (tuple, list)), hidden
        assert prefill is None or isinstance(prefill, dict), prefill
        assert actions is None or isinstance(actions, (tuple, list, collections.Callable)), actions
        self._view = view
        self._req = req
        self._row = row
        self._key = row.data().key()[0].id()
        self._handler = handler
        self._prefill = prefill or {}
        self._uri_provider = uri_provider
        self._hidden = list(hidden)
        self._name = name
        self._actions = actions

    def _export_body(self, context, form_id):
        return []

    def _export_javascript(self, context, form_id):
        return None

    def form_id(self):
        """Return the HTML id of the form used in the last export.

        Knowing the HTML id can be usefull for example in JavaScript code
        accessing the form's DOM elements or its JavaScript instance.

        The form can be theoretically exported several times but usually
        there's just one export alltogether.  In any case, the HTML id is
        generated when the export starts, so calling this method before
        exporting the form will raise an AttributeError exception.

        """

        
        return self._form_id

    def export(self, context):
        g = context.generator()
        cls = 'pytis-form ' + self._CSS_CLS
        if self._name:
            cls += ' ' + camel_case_to_lower(self._name, '-')
        self._form_id = form_id = context.unique_id()
        javascript = self._export_javascript(context, form_id)
        if javascript:
            # Javascript dependencies must be allocated before the form is
            # exported because form field export may rely on these dependencies
            # to be available.
            context.resource('prototype.js')
            context.resource('gettext.js')
            context.resource('pytis.js')
            if context.lang() != 'en':
                context.resource('pytis.%s.po' % context.lang()) # Translations for Javascript
        result = g.div(self._export_form(context, form_id), cls=cls, id=form_id)
        if javascript:
            result += g.script(javascript)
        context.resource('pytis-forms.css')
        return result

    def _export_form(self, context, form_id):
        return self._export_body(context, form_id)

    def heading_info(self):
        """Return basestring to be possibly put into a document heading.

        If the return value is 'None' or an empty basestring, nothing is added
        to the document heading.  Otherwise the returned basestring may or may
        not be added to some document heading, based on decision of the code
        making the final document.

        """
        return None

    def _export_actions(self, context, record, uri):
        g = context.generator()
        buttons = [
            g.form([g.hidden(name, value is True and 'true' or value) for name, value in
                    [('action', action.id()),
                     ('__invoked_from', self.__class__.__name__),
                     ] + action.kwargs().items()] +
                   [g.button(g.span(action.title()), title=action.descr(),
                             disabled=not enabled,
                             cls='action-'+action.id() + (not enabled and ' disabled' or ''))],
                   action=uri)
            for action, enabled in self._visible_actions(context, record)]
        if buttons:
            return [g.div(buttons, cls='actions module-actions-' + self._name)]
        else:
            return []

    def _visible_actions(self, context, record):
        class Action(object):
            def __init__(self, **kwargs):
                self.__dict__.update(kwargs)
        if record is not None:
            required_context = ActionContext.RECORD
            function_args = (record,)
        else:
            required_context = ActionContext.GLOBAL
            function_args = (self._req,)
        def is_visible(action):
            context = action.context()
            if context != required_context:
                return False
            visible = action.visible()
            if isinstance(visible, collections.Callable):
                visible = visible(*function_args)
            return visible
        def is_enabled(action):
            enabled = action.enabled()
            if isinstance(enabled, collections.Callable):
                enabled = enabled(*function_args)
            return enabled
        if self._actions is not None:
            actions = self._actions
            if isinstance(actions, collections.Callable):
                actions = actions(self, record)
        else:
            actions = self._view.actions()
        return [(action, is_enabled(action)) for action in actions if is_visible(action)]


class FieldForm(Form):
    """Form with formattable fields."""
    
    def __init__(self, *args, **kwargs):
        super(FieldForm, self).__init__(*args, **kwargs)
        self._fields = dict([(f.id(), self._field(f.id())) for f in self._view.fields()])
        
    def _field(self, id, multirow=False):
        return Field.create(self._row, self._view.field(id), self, self._uri_provider, multirow=multirow)
        
    def _export_field(self, context, field, editable=False):
        if editable:
            result = field.editor(context, prefill=self._prefill.get(field.id),
                                           error=dict(self._errors).get(field.id))
        else:
            formatted = field.format(context)
            if field.spec.text_format() == pytis.presentation.TextFormat.LCG:
                if field.spec.printable():
                    uri = self._uri_provider(self._row, UriType.PRINT, field.id)
                    if uri:
                        g = context.generator()
                        img = context.resource('print-field.png')
                        if img:
                            label = g.img(src=context.uri(img), alt=_(u"Print"))
                        else:
                            label = _(u"Print")
                        formatted = g.a(label, href=uri,
                                        title=_(u"Export the contents of this field into PDF"),
                                        cls='print-field-link') + formatted
                wrap = context.generator().div
            else:
                wrap = context.generator().span
            result = wrap(formatted, cls='field id-'+field.id)
        return result

    def _interpolate(self, context, template, row):
        def export_field(fid):
            if row.visible(fid):
                return self._export_field(context, self._fields[fid])
            else:
                return ''
        if isinstance(template, collections.Callable):
            template = template(row)
        # Translation is called immediately to force immediate interpolation
        # (with the current row data).  Delayed translation (which invokes the
        # interpolation) would use invalid row data (the 'PresentedRow'
        # instance is reused and filled with table data row by row).
        return context.localize(template.interpolate(export_field))
    

class LayoutForm(FieldForm):
    """Form with fields arranged according to pytis layout specification."""
    _MAXLEN = 100
    _ALIGN_NUMERIC_FIELDS = False
    _EDITABLE = False

    class _GroupContent(object):
        """Export helper class.

        This class tries to encapsulate some of the group export logic to make
        the '_export_group()' method more readable.
        
        """
        def __init__(self):
            self._content = []
            self._needs_panel = False
            self._has_labeled_items = False
            self._allow_right_aligned_fields = False
            self._last_field_was_right_aligned = False
            
        def append(self, content, label=None, fullsize=True, right_aligned=False,
                   needs_panel=False):
            """Append exported content to the group .
            
            Arguments:
              content -- exported content of one group item.
              label -- exported label of one group item.  This is typically
                used for fields, but there may be also a labeled nested group.
              fullsize -- boolean flag only relevent for vertical group, where
                fullsize content spans across the label and field column (if
                there are any labeled fields).  Fullsize content has no label,
                but having no label doesn't imply being fullsize.
              right_aligned -- only relevent for vertical group.  Group allows
                right aligned fields are aligned to the right, if at least two
                right aligned fields appear above each other.
              needs_panel -- boolean indicating that the appended content
                should appear on a visually distinct panel.  A group will be
                rendered as a panel if there is at least one item which needs a
                panel.  If the group is only a composition of nested groups, it
                does not need a panel, if it contains fields and other directly
                visible content needs a panel.
              
            """
            self._content.append((label, content, fullsize, right_aligned))
            if needs_panel:
                self._needs_panel = needs_panel
            if right_aligned and self._last_field_was_right_aligned:
                self._allow_right_aligned_fields = True
            self._last_field_was_right_aligned = right_aligned
            if label:
                self._has_labeled_items = True
            if needs_panel:
                self._needs_panel = needs_panel
                
        def needs_panel(self):
            return self._needs_panel
        def has_labeled_items(self):
            return self._has_labeled_items
        def allow_right_aligned_fields(self):
            return self._allow_right_aligned_fields
        def content(self):
            return self._content
            
    def __init__(self, view, req, row, layout=None, **kwargs):
        assert layout is None or isinstance(layout, GroupSpec)
        self._layout = layout
        super(LayoutForm, self).__init__(view, req, row, **kwargs)
        
    def _export_group(self, context, group, inner=False, id=None, omit_first_field_label=False):
        g = context.generator()
        content = self._GroupContent()
        subgroup_number = 0
        for i, item in enumerate(group.items()):
            if isinstance(item, collections.Callable):
                item = item(self._row)
            if item is None:
                pass
            elif isinstance(item, basestring):
                if self._row.visible(item):
                    field = self._fields[item]
                    if omit_first_field_label and i == 0:
                        label = None
                    else:
                        label = self._export_field_label(context, field)
                    ctrl = self._export_field(context, field, editable=self._EDITABLE)
                    help = self._export_field_help(context, field)
                    if help is not None:
                        ctrl += help
                    if field.spec.compact():
                        if label:
                            ctrl = g.div(label) + ctrl
                        content.append(ctrl, needs_panel=True)
                    else:
                        # Codebook field display is not numeric even though the
                        # underlying type is...
                        right_aligned = (self._ALIGN_NUMERIC_FIELDS and not field.type.enumerator()
                                         and isinstance(field.type, pytis.data.Number))
                        content.append(ctrl, label=label, right_aligned=right_aligned,
                                       needs_panel=True, fullsize=False)
            elif isinstance(item, GroupSpec):
                subgroup_number += 1
                subgroup_id = '%s-%d' % (id or 'group', subgroup_number)
                label = None
                if group.orientation() == Orientation.VERTICAL \
                        and item.orientation() == Orientation.HORIZONTAL:
                    items = copy.copy(list(item.items()))
                    while items and isinstance(items[0], basestring):
                        fid = items.pop(0)
                        if self._row.visible(fid):
                            field = self._fields[fid]
                            # Nested horizontal group which starts with a
                            # labeled field will be aligned within the current
                            # vertical group if possible.
                            if not field.spec.compact():
                                label = self._export_field_label(context, field)
                                break
                exported_group = self._export_group(context, item, inner=True, id=subgroup_id,
                                                    omit_first_field_label=label is not None)
                if exported_group:
                    content.append(exported_group, label=label, fullsize=label is None)
            elif isinstance(item, lcg.Content):
                content.append(item.export(context))
            elif isinstance(item, Text):
                text = g.div(g.escape(item.text()).replace("\n", g.br()))
                content.append(text, fullsize=False, needs_panel=True)
            elif isinstance(item, Button):
                pass
            else:
                raise ProgramError("Unsupported layout item type:", item)
        if group.orientation() == Orientation.HORIZONTAL:
            def td(i, label, content_):
                spaced = (i != 0 and ' spaced' or '')
                if label:
                    return (g.th(label, valign='top', cls='label'+spaced) +
                            g.td(content_, valign='top', cls='ctrl'))
                else:
                    return g.td(content_, valign='top', cls='ctrl'+spaced)
            cells = [td(i, label, content_)
                     for i, (label, content_, fullsize, right_aligned)
                     in enumerate(content.content())]
            if cells:
                result = [g.table([g.tr(cells)], cellspacing=0, cellpadding=0,
                                  cls='horizontal-group' + (not omit_first_field_label
                                                            and ' expanded' or ''))]
            else:
                result = []
        else:
            def td(label, content_, fullsize, right_aligned, fullspan, normalspan):
                if fullsize:
                    return g.td(content_, cls='ctrl', valign='top', colspan=fullspan)
                else:
                    if content.allow_right_aligned_fields() and right_aligned:
                        spacer = g.td('', width='100%', cls='spacer')
                        kwargs =  dict(align='right')
                    else:
                        spacer = ''
                        kwargs =  dict(width='100%', colspan=normalspan)
                    return (g.th(label or '', valign='top', cls='label', align='right') +
                            g.td(content_, cls='ctrl', valign='top', **kwargs) + spacer)
            if content.has_labeled_items():
                if content.allow_right_aligned_fields():
                    normalspan = 2
                    fullspan = 3
                else:
                    normalspan = None
                    fullspan = 2
                rows = [g.tr(td(label, content_, fullsize, right_aligned, fullspan, normalspan))
                        for label, content_, fullsize, right_aligned in content.content()]
                if rows:
                    result = [g.table(rows, cls='vertical-group')]
                else:
                    result = []
            else:
                result = [item[1] for item in content.content()]
        if result:
            cls = 'group' + (id and ' ' + id or '')
            if group.label():
                result = g.fieldset(group.label()+':', result, cls=cls)
            elif content.needs_panel():
                # If there are any items which need a panel and there is no
                # fieldset panel, add a panel styled div.
                result = g.div(result, cls=cls)
            elif not inner:
                # This fieldset fixes MSIE display of top-level horizontal groups...
                result = g.fieldset(None, result, cls='outer')
            else:
                result = concat(result, separator="\n")
            return result
        else:
            return ''

    def _export_field_label(self, context, field):
        if field.label:
            g = context.generator()
            return g.span(g.label(field.label, None)+":", cls='field-label id-'+field.id)
        else:
            return None
    
    def _export_field_help(self, context, field):
        return None


class _SingleRecordForm(LayoutForm):

    def __init__(self, view, req, row, layout=None, **kwargs):
        layout = layout or view.layout().group()
        super(_SingleRecordForm, self).__init__(view, req, row, layout=layout, **kwargs)
        
    def _export_body(self, context, form_id):
        return [self._export_group(context, self._layout)]
    
    
class _SubmittableForm(Form):
    """Mix-in class for forms with submit buttons."""

    def __init__(self, view, req, row, submit=_(u"Submit"), reset=_(u"Undo all changes"), **kwargs):
        """Arguments:

          submit -- custom submit buttons as a sequence of (label, name) pairs.
            A single unnamed button can be passed as just the label string.
            Default is one button labeled `Submit'.
          reset -- reset button label as a string or None to omit the reset
            button.
            
          See the parent classes for definition of the remaining arguments.

        """
        if not isinstance(submit, (list, tuple)):
            submit = ((submit or _(u"Submit"), None),)
        self._submit = submit
        self._reset = reset
        self._enctype = None
        super(_SubmittableForm, self).__init__(view, req, row, **kwargs)
    

    def _export_form(self, context, form_id):
        g = context.generator()
        return [g.form((super(_SubmittableForm, self)._export_form(context, form_id) +
                        self._export_submit(context)),
                       action=g.uri(self._handler), method=self._HTTP_METHOD,
                       enctype=self._enctype)]

    def _export_submit(self, context):
        g = context.generator()
        hidden = [('form_name', self._name)]
        for name, value in self._hidden:
            if isinstance(value, (tuple, list)):
                hidden.extend([(name, v) for v in value])
            else:
                hidden.append((name, value))
        invoked_from = self._req.param('__invoked_from')
        if invoked_from is not None:
            hidden.append(('__invoked_from', invoked_from))
        content = [g.hidden(name, value) for name, value in hidden] + \
            [g.button(g.span(label), name=name, value='1', title=_(u"Submit the form"))
             for label, name in self._submit]
        if self._reset:
            content.append(g.button(g.span(_(u"Reset")), type='reset', title=self._reset))
        return [g.div(content, cls='submit')]



class ShowForm(_SingleRecordForm):
    _CSS_CLS = 'show-form'
    _ALIGN_NUMERIC_FIELDS = True

    def _export_form(self, context, form_id):
        return (super(ShowForm, self)._export_form(context, form_id) +
                self._export_actions(context, self._row, self._req.uri()))

    
    
class EditForm(_SingleRecordForm, _SubmittableForm):
    _CSS_CLS = 'edit-form'
    _EDITABLE = True
    
    def __init__(self, view, req, row, errors=(), multipart=None, **kwargs):
        """Arguments:

          errors -- a sequence of error messages to display within the form
            (results of previous attempt to commit the form).  The sequence
            consists of pairs (ID, MESSAGE), where ID is the field identifier
            and MESSAGE is the error message for given field.  ID can also be
            None for messages which don't belong to any particular field and it
            is also legal to pass field identifiers, which don't appear in the
            current form or even don't exist in the current specification
            (typically for fields which only appear in the underlying database
            objects).
          multipart -- force form encoding type to 'multipart/form-data'.  If
            None, the encoding is set to 'multipart/form-data' automatically
            when the form includes any binary (file upload) fields.  If False,
            the form will always use the default encoding type (The 'enctype'
            HTML form attribute is not used) and if True, encoding is always
            forced to 'multipart/form-data'.

          See the parent classes for definition of the remaining arguments.

        """
        super(EditForm, self).__init__(view, req, row, **kwargs)
        key, order = self._key, tuple(self._layout.order())
        self._hidden += [(k, v) for k, v in self._prefill.items()
                         if view.field(k) and not k in order and k != key]
        if not self._row.new() and key not in order + tuple([k for k,v in self._hidden]):
            self._hidden += [(key,  self._row[key].export())]
        assert isinstance(errors, (tuple, list)), errors
        if __debug__:
            for e in errors:
                assert isinstance(e, tuple), ('type error', e, errors)
                assert len(e) == 2, ('type error', e, errors)
                assert e[0] is None or isinstance(e[0], basestring), ('type error', e[0], errors)
                assert isinstance(e[1], basestring), ('type error', e[1], errors)
        self._errors = errors
        assert multipart in (None, True, False), multipart
        if multipart is None:
            multipart = any([f for f in order if isinstance(self._row.type(f), pytis.data.Binary)])
        self._enctype = (multipart and 'multipart/form-data' or None)

    def _export_field_label(self, context, field):
        g = context.generator()
        if not field.label:
            return None
        if field.not_null():
            sign = g.sup("*", cls="not-null")
        else:
            sign = ''
        cls='field-label id-'+field.id
        if not self._row.editable(field.id):
            cls += ' disabled'
        return g.span(g.label(field.label, field.html_id()) + sign + ":", cls=cls)
            
    def _export_field_help(self, context, field):
        descr = field.spec.descr()
        if descr:
            g = context.generator()
            field_id = field.html_id()
            help_id = field_id + '-help'
            # Set through a script to avoid invalid HTML ('aria-describedby' in not valid HTML).
            return (g.div(descr, id=help_id, cls="help") +
                    g.script("$('%s').setAttribute('aria-describedby', '%s');" %
                             (field_id, help_id)))
        else:
            return None

    def _export_form(self, context, form_id):
        return (self._export_errors(context, form_id) +
                super(EditForm, self)._export_form(context, form_id))
    
    def _export_errors(self, context, form_id):
        g = context.generator()
        errors = []
        for id, msg in self._errors:
            if id is not None:
                f = self._view.field(id)
                msg = g.strong(f and f.label() or id) + ": " + msg
            errors.append(g.p(msg))
        if errors:
            return [g.div(errors, cls='errors')]
        else:
            return []

    def _export_javascript(self, context, form_id):
        g = context.generator()
        layout_fields = self._layout.order()
        js_fields = []
        state = {}
        for fid in layout_fields:
            if self._row.visible(fid):
                field = self._fields[fid]
                active = self._row.depends(fid, layout_fields)
                if field.spec.runtime_filter() or field.spec.runtime_arguments():
                    # NOTE: The format here must be same as in EditForm.ajax_response()!
                    state[fid] = 'f=%s;a=%s' % (self._row.runtime_filter(fid),
                                            self._row.runtime_arguments(fid))
                js_fields.append(field.javascript(context, form_id, active))
        return g.js_call('new pytis.FormHandler', form_id, js_fields, state)
        
    
    def _export_footer(self, context):
        for f in self._fields.values():
            if f.label and f.not_null() and f.id in self._layout.order():
                g = context.generator()
                return [g.div(g.span("*", cls="not-null") +") "+\
                                  _(u"Fields marked by an asterisk are mandatory."),
                              cls='footer')]
        return []

    @classmethod
    def ajax_response(cls, req, row, layout, errors, localizer, uri_provider=None):
        """Return the AJAX request response as a JSON encoded data structure.

        Arguments:
          req -- AJAX request object as an instance of class implementing the
            pytis 'Request' API.
          row -- edited form record as a 'PresentedRow' instance.
          layout -- edited form layout as a 'GroupSpec' instance.
          errors -- form data validation result as a sequence of pairs
            (field_id, error_message).
          localizer -- 'lcg.Localizer' instance used for localization of
            computed field values (such as dates, numbers, etc).
          uri_provider -- URI provider function same as in the Form constructor
            argument of the same name.

        This method acts as the server side counter-part of the client side
        code defined in the pytis form JavaScript code in 'pytis.js'.  The
        returned string is supposed to be sent back to the client within the
        response body with the 'application/json' content type set in HTTP
        response headers.  That way the client side code will be able to
        process it.

        May raise 'BadRequest' exception id the request parameters don't make
        sense.
        
        """
        try:
            import json
        except:
            import simplejson as json
        if req.param('_pytis_attachment_storage_request'):
            return json.dumps(cls._attachment_storage_request(req, row))
        request_number = req.param('_pytis_form_update_request')
        changed_field = str(req.param('_pytis_form_changed_field'))
        state = req.param('_pytis_form_state')
        if state:
            import urlparse
            field_states = dict((k, v[0]) for k, v in urlparse.parse_qs(state).items())
        else:
            field_states = {}
        fields = {}
        computed_fields = [f.id() for f in row.fields() if f.computer() is not None]
        for fid in layout.order():
            fields[fid] = fdata = {}
            if fid != changed_field:
                fdata['editable'] = row.editable(fid)
                if fid in computed_fields and row.invalid_string(fid) is None \
                        and not isinstance(row.type(fid), pd.Binary):
                    value = row[fid]
                    if isinstance(value.type(), pd.DateTime):
                        exported_value = localizable_datetime(value)
                    else:
                        exported_value = value.export()
                    localized_value = localizer.localize(exported_value)
                    # Values of disabled fields are not in the request, so send them always...
                    if not req.has_param(fid) or localized_value != req.param(fid):
                        fdata['value'] = localized_value
                if fid in field_states:
                    old_state = field_states[fid]
                    # We rely on the fact, that a stringified
                    # 'pytis.data.Operator' uniquely represents the
                    # corresponding runtime filter state.
                    new_state = 'f=%s;a=%s' % (row.runtime_filter(fid), row.runtime_arguments(fid))
                    if new_state != old_state:
                        enumeration = [(value, localizer.localize(label))
                                       for value, label in row.enumerate(fid)]
                        fdata['state'] = new_state
                        fdata['enumeration'] = enumeration
                        if uri_provider and isinstance(row.type(fid), pd.Array):
                            func = uri_provider(row, UriType.LINK, fid)
                            def link(value):
                                lnk = func(value)
                                if isinstance(lnk, Link):
                                    return dict(href=lnk.uri(),
                                                title=localizer.localize(lnk.title()),
                                                target=lnk.target())
                                else:
                                    return dict(href=lnk)
                            if func:
                                fdata['links'] = dict([(value, link(value))
                                                       for (value, display) in enumeration])
        for fid, error in errors:
            if fid in fields:
                fields[fid]['error'] = localizer.localize(error)
        result = dict(request_number=request_number, fields=fields)
        return json.dumps(result)

    @classmethod
    def _attachment_storage_request(cls, req, row):
        method_name = req.param('_pytis_attachment_storage_request')
        try:
            method = getattr(cls, '_attachment_storage_'+method_name)
        except AttributeError:
            raise BadRequest()
        storage = row.attachment_storage(req.param('_pytis_attachment_storage_field'))
        if not storage:
            raise BadRequest()
        return method(req, row, storage)

    @classmethod
    def _resource2dict(cls, resource):
        result = dict([(attr, getattr(resource, attr)())
                       for attr in ('filename', 'uri', 'title', 'descr')],
                      type=resource.__class__.__name__,
                      **(resource.info() or {}))
        if isinstance(resource, lcg.Image) and resource.thumbnail():
            result['thumbnail'] = cls._resource2dict(resource.thumbnail())
        return result

    @classmethod
    def _attachment_storage_get(cls, req, row, storage):
        resource = storage.resource(req.param('filename'))
        return resource and cls._resource2dict(resource)
        
    @classmethod
    def _attachment_storage_list(cls, req, row, storage):
        return [cls._resource2dict(r) for r in storage.resources()]
    
    @classmethod
    def _attachment_storage_update(cls, req, row, storage):
        try:
            import json
        except:
            import simplejson as json
        return storage.update(req.param('filename'), json.loads(req.param('values')))

    @classmethod
    def _attachment_storage_insert(cls, req, row, storage):
        upload = req.param('upload')
        if not upload:
            raise BadRequest()
        error = storage.insert(upload.filename(), upload.file(), dict(mime_type=upload.mime_type()))
        return {'success': error is None, 'message': error and req.localize(error),
                'filename': upload.filename()}
    
class FilterForm(EditForm):
    """Simple form for displaying a list of fields for advanced filtering."""
    # This form is currently only used in Wiking Biblio CatalogNews module.
    # The whole thing needs some further work to be generally usable ...
    _CSS_CLS = 'edit-form filter-form'
    
    def __init__(self, fields, req, row, **kwargs):
        view = ViewSpec(_(u"Filter"), fields)
        kwargs['reset'] = kwargs.get('reset') # Default to None in this class.
        kwargs['submit'] = kwargs.get('submit', _(u"Apply Filter"))
        super(FilterForm, self).__init__(view, req, row, **kwargs)
        
    def _export_footer(self, context):
        return []

    
class BrowseForm(LayoutForm):
    _CSS_CLS = 'browse-form'
    _HTTP_METHOD = 'GET'
    _SORTING_DIRECTIONS = {pytis.data.ASCENDENT: 'asc',
                           pytis.data.DESCENDANT: 'desc'}
    _NULL_FILTER_ID = '-'

    def __init__(self, view, req, row, uri_provider=None, condition=None, arguments=None,
                 columns=None, sorting=None, grouping=None,
                 limits=(25, 50, 100, 200, 500), limit=50, offset=0,
                 search=None, query=None, allow_query_search=None, filter=None, message=None, 
                 filter_sets=None, profiles=None, filter_fields=None, immediate_filters=True,
                 top_actions=False, bottom_actions=True, row_actions=False, async_load=False,
                 **kwargs):
        """Arguments:

          uri_provider -- as in the parent class.
          condition -- current condition for filtering the records as
            'pytis.data.Operator' instance or None.
          arguments -- dictionary of table function call arguments, with
            function argument identifiers as keys and 'pytis.data.Value'
            instances as values.  Useful only when the table is actually a row
            returning function, otherwise ignored.
          columns -- sequence of column identifiers to be displayed.  If not
            None, this value overrides the default columns defined by the
            specification, but may be further overriden by the columns defined
            by the currently selected form profile (see 'profiles').
          sorting -- form sorting specification in the format recognized by the
            'sort' argument of 'pytis.data.Data.select()'.  If not None, this
            value overrides the default sorting defined by the specification,
            but may be further overriden by the sorting defined by the
            currently selected form profile (see 'profiles').
          grouping -- visual grouping of table rows.  The value is a column
            identifier or a sequence of column identifiers.  Grouping allows
            you to visually distinguish table rows, which have the same
            value(s) in grouping columns(s).  This usually only makes sense
            when the table is sorted by these columns so grouping is ignored
            when the user changes sorting.  If not None, this value overrides
            the default grouping defined by the specification, but may be
            further overriden by the grouping defined by the currently selected
            form profile (see 'profiles').
          limit -- maximal number of rows per page.  If the current condition
            produces more rows, the listing will be split into pages and the
            form will include controls for navigation between these pages.
            None value results in an unlimited list -- all records will be
            printed on just one page and paging controls will be disabled.  The
            request parameter 'limit' overrides this value and it is stored as
            a cookie.  The cookie has lower precedence than the request
            parameter, but still higher than the 'limit' constructor argument,
            so this argument really only serves as a default value.  The
            request parameter/cookie is checked against the 'limits' argument
            (see below) and if it is not one of the values defined there, it is
            ignored, so the user may only use one of the allowed limits.
          limits -- a sequence of available 'limit' values.  These values are
            used to create the limit selection control and also determine valid
            values of the 'limit' request parameter and cookie (described
            above).
          offset -- determines the page within paged listing.  The number
            indicates the offset of the record within all the records of the
            current select.  The page, which contains this record will be
            displayed if possible.  If not (the listing is shorter than given
            number), the nearest page is displayed.  The request argument
            'offset' overrides this value.  Also request arguments 'next' and
            'prev' modify this value.
          search -- search condition as a 'pytis.data.Operator' instance or
            None.  If used, the offset will be set automatically to ensure,
            that the first record matching the search condition will be
            displayed on the current page.  The request parameters 'search' or
            'index_search' can be also used to initialize the search condition
            (if this constructor argument is not used).  The request parameter
            'search' is for searching by the value of the key column.  The
            request parameter 'index_search' is for searching by a prefix
            string and is always performed on the primary sorting column.  If
            no search condition is passed (either to the constructor or through
            the request), the offset is controlled by the 'offset' argument.
            Searching is ignored when the current limit is greater than the
            total number of records.
          query -- query search string.  If None, the form automatically
            displays search controls when the number of records exceeds one
            page.  If 'query' is passed, these embedded search conrols are
            disabled (it is considered, that the application has it's own
            search interface), but otherwise the form behaves as if the query
            was filled in its own search field.  The query string is split into
            query words by space and the form is filtered to contain only
            records containing all the words in any of its string columns.
          allow_query_search -- explicitly enable or disable displaying the
            query search controls.  Query search allows filtering the form
            records by a text string.  By default (when None), query search
            controls are displayed automatically when the number of records
            exceeds one page.  The controls initially contain only a "Search"
            button, which, when pressed, displays a text field for entering the
            search query above the form.  When the query is submitted, the form
            is filtered to contain only matching records (the query string is
            split into separate query words by space and matching records must
            contain all the words in any of its string columns).  Passing True
            to this argument will force displaying the query field, which may
            be practical for forms where searching is highly expected.  The
            unnecessary step of pressing the "Search" button is not necessary,
            but the user interface is a little more cluttered by search
            controls.  Passing False, on the other hand, will disable the query
            search controls altogether.  The search query string may still be
            passed programatically through the 'query' argument in this case.
          filter -- filter condition as a 'pytis.data.Operator' instance.  This
            condition will be appended to 'condition', but the difference is
            that 'condition' is invisible to the user, but 'filter' may be
            indicated in the user interface.
          message -- function returning a custom search result message.  If
            none, a default message will be used according to current query,
            such as 'Found 5 records matching the search expression.'.  A
            function of one argument (integer determining the number of records
            in the form) may be used to return a custom message as a string
            (possibly LCG Translatable object).  An example of a custom message
            might be 'Found 15 articles in category Python'.
          filter_sets -- a sequence of filter sets as
            'pytis.presentation.FilterSet' instances.  Filter sets are used to
            present multiple filter selectors which can be combined into one
            final filtering condition.  Filter sets can be also combined with
            profiles.  In this case filter set selectors will appear prior to
            the profile selector.  If None, the default filter sets from the
            specification are used.  If not None, the filter sets defined by
            the specification are ignored.
          profiles -- specification of form profiles as a 'Profiles' instance
            or a sequence of 'Profile' instances.  These profiles will be
            available in the user interface for user's selection.  If None, the
            default set of profiles defined by specification is used.  If not
            None, the profiles from specification are ignored.  This argument
            is mostly useful to construct the list of profiles dynamically
            (specification profiles are static).
          filter_fields -- specification of editable filter fields as a
            sequence of tuples of three items (field_specification, operator,
            field_id), where 'field_specification' is a
            'pytis.Presentation.Field' instance defining a unique filter field
            id, label, default value and other possible presentational
            properties.  'operator' is a conditional operator function (a
            function of two arguments - field id and 'pytis.data.Value'
            instance returning a 'pytis.data.Operator' instance).  'field_id'
            will be used as the first argument to the 'operator'.  It must also
            refer to one of existing form fields.  The filter field will
            inherit all attributes of the form field given by 'field_id', but
            attributes defined by 'field_specification' override the form
            field's attributes.  An input field will be created for each of the
            named filter fields and the form filter will automatically include
            also the conditions given by the operators using the current field
            values.  Operators of all filter fields are applied in conjunction
            and also in conjunction with any other form filters/conditions.
            The field values are automatically stored in browser cookies, so
            the filters should be persistent.  If 'operator' is None, the field
            will be used as a table function argument instead of a condition.
            In this case 'field_id' must be present on data object's
            'arguments' (defined by 'Specification' attribute 'arguments').
            The field value will be added to 'arguments' passed to the data
            object's 'select()' call.  Such arguments are combined with the
            'arguments' passed to the form constructor (the values from filter
            fields override the values in 'arguments').
          immediate_filters -- when True, filters and profiles apply
            immediately after their selection in the corresponding selector;
            when False, there is a separate button for filter application.
            When 'filter_fields' are present, filters must always be applied
            using a button, so this argument is ignored in this case.
          top_actions -- boolean flag to control the presence of the
            global action buttons above the form.
          bottom_actions -- boolean flag to control the presence of the
            global action buttons menu below the form.
          row_actions -- boolean flag to control the presence of the row level
            action buttons within the form.  Row actions are normally disabled,
            so that the user must visit the ShowForm by clicking the row link
            and invoke actions from there.  Including action buttons directly
            within the browse form allows direct access but clutters the UI.
          async_load -- boolean flag indicating whether form data can be loaded
            asynchronously through AJAX.  If True, the form will not be
            rendered on export.  Only a container is created with a JavaScript
            code which loads the data through AJAX in a separate request.  This
            may dramatically reduce the time needed for form export so the page
            can be returned to the client more promptly.  If allowed, the
            application must also support the asynchronous requests (the
            parameter '_pytis_async_load_request' is added for such requests).

        See the parent classes for definition of the remaining arguments.

        """
        if uri_provider:
            def uri_provider_(row, kind, target):
                if kind == UriType.LINK and target == self._columns[0]:
                    uri = uri_provider(row, kind, None)
                    if uri is not None:
                        return uri
                return uri_provider(row, kind, target)
        else:
            uri_provider_ = None
        super(BrowseForm, self).__init__(view, req, row, uri_provider=uri_provider_, **kwargs)
        data = self._row.data()
        def param(name, func=None, default=None):
            # Consider request params only if they belong to the current form.
            if req.param('form_name') == self._name and req.has_param(name):
                value = req.param(name)
                if func:
                    try:
                        return func(value)
                    except:
                        return default
                else:
                    return value
            else:
                return default
        self._condition = condition
        self._arguments = arguments
        # Process filter sets and profiles first.
        if filter_sets is None:
            filter_sets = self._view.filter_sets()
        filter_sets = list(filter_sets)
        if profiles is None:
            profiles = self._view.profiles()
        elif not isinstance(profiles, Profiles):
            profiles = Profiles(*profiles)
        self._profiles = profiles
        self._filter = filter
        self._filter_ids = {}
        self._filter_sets = []
        if profiles:
            assert 'profile' not in [fs.id() for fs in filter_sets]
            # Add profile selection as another filter set, since the user interface is the same.
            filter_set = FilterSet('profile', profiles.label() or _("Profile"),
                                   [Filter(p.id(), p.title(), p.filter()) for p in profiles],
                                   default=profiles.default())
            self._init_filter_sets((filter_set,), req, param)
            profile_id = self._filter_ids['profile']
            if profile_id is not None:
                profile = find(profile_id, profiles, key=lambda p: p.id())
                if profile.columns() is not None:
                    columns = profile.columns()
                if profile.sorting() is not None:
                    sorting = profile.sorting()
                if profile.grouping() is not None:
                    grouping = profile.grouping()
                profile_filter_sets = profile.filter_sets()
                if profile_filter_sets is not None:
                    self._init_filter_sets(profile_filter_sets, req, param)
        self._init_filter_sets(filter_sets, req, param)
        # Determine the current sorting.
        self._user_sorting = None
        sorting_column, direction = param('sort', str), param('dir', str)
        if sorting_column and direction and data.find_column(sorting_column):
            direction = dict([(b, a) for a, b in self._SORTING_DIRECTIONS.items()]).get(direction)
            if direction:
                self._user_sorting = (sorting_column, direction)
                sorting = ((sorting_column, direction),)
        if sorting is None:
            sorting = self._view.sorting()
        if sorting is None:
            sorting = ((self._key, pytis.data.ASCENDENT),)
        self._sorting = sorting

        # Determine the current grouping.
        if grouping is None:
            grouping = self._view.grouping()
        if self._user_sorting:
            grouping = None
        self._grouping = grouping
        # Determine the limit of records per page.
        self._limits = limits
        limit_ = param('limit', int)
        if limit_ is None:
            cookie = req.cookie('pytis-form-limit')
            if cookie and isinstance(cookie, basestring) and cookie.isdigit():
                limit_ = int(cookie)
        else:
            req.set_cookie('pytis-form-limit', limit_)
        if limit_ in limits:
            limit = limit_
        self._limit = limit
        # Determine the current offset.
        if limit is None:
            offset = 0
        else:
            offset_param = param('offset', int)
            if offset_param is not None:
                offset = offset_param
            if param('next', bool):
                offset += limit
            if param('prev', bool) and offset >= limit:
                offset -= limit
        self._offset = offset
        # Determine the current key or index search condition.
        index_search_string = ''
        if search is None:
            search_string = param('search', str)
            if search_string:
                type = data.find_column(self._key).type()
                value, error = type.validate(search_string)
                if not error:
                    search = pytis.data.EQ(self._key, value)
            else:
                index_search_string = param('index_search', default='')
                if index_search_string and isinstance(self._row.type(sorting[0][0]), pd.String):
                    search = self._index_search_condition(index_search_string)
        self._index_search_string = index_search_string
        self._search = search
        # Determine the current query search condition.
        if allow_query_search is False or query is not None:
            show_query_field = False
            allow_query_field = False
        else:
            query = param('query', unicode)
            show_query_field = bool(query or param('show_query_field', bool) or allow_query_search)
            allow_query_field = True
        if query:
            query_condition = pd.AND(*[pd.OR(*[pd.WM(f.id, pd.WMValue(f.type, '*'+word+'*'))
                                               for f in self._fields.values()
                                               if isinstance(f.type, pd.String) and not f.virtual])
                                       for word in query.split()])
        else:
            query_condition = None
        self._query = query
        self._query_condition = query_condition
        self._show_query_field = show_query_field
        self._allow_query_field = allow_query_field
        # Determine whether tree emulation should be used.
        if sorting and isinstance(self._row.type(sorting[0][0]),
                                  (pytis.data.LTree, pytis.data.TreeOrder)):
            self._tree_order_column = sorting[0][0]
        else:
            self._tree_order_column = None
        self._columns = columns or view.columns()
        self._column_fields = cfields = [self._fields[cid] for cid in self._columns
                                         if self._row.visible(cid)]
        self._align = dict([(f.id, 'right') for f in cfields
                            if not f.type.enumerator() and isinstance(f.type, pd.Number)])
        self._custom_message = message
        # Hack allowing locale dependent index search controls.  The method
        # 'preferred_language()' is Wiking specific so it is not correct to rely
        # on it here.
        try:
            self._lang = str(req.preferred_language())
        except:
            self._lang = None
        self._init_filter_fields(req, filter_fields or ())
        if data.arguments():
            assert arguments or self._filter_fields_arguments
        else:
            assert not arguments and not self._filter_fields_arguments
        self._immediate_filters = immediate_filters
        self._top_actions = top_actions
        self._bottom_actions = bottom_actions
        self._row_actions = row_actions
        self._async_load = async_load

    def _init_filter_fields(self, req, filter_fields_spec):
        columns = []
        fields = []
        conditions = []
        arguments = []
        values = []
        errors = []
        for fspec, operator, field_id in filter_fields_spec:
            if operator is None:
                binding = find(field_id, self._row.data().arguments(), key=lambda b: b.id())
                ftype = binding.type()
            else:
                fspec = self._view.field(field_id).clone(fspec)
                ftype = self._row.type(field_id)
            filter_id = fspec.id()    
            cookie = 'pytis-filter-%s-%s' % (self._name, filter_id)
            if req.has_param(filter_id):
                # TODO: This validation will only work for simple fields.  It
                # is neceddary to move the method
                # wiking.PytisModule._validate() into pytis forms to make it
                # work in all cases.  Now it is only hacked for the Datetime
                # fields (by copying the relevant part of the above mentioned
                # method).
                if isinstance(ftype, pd.DateTime):
                    locale_data = lcg.Localizer(self._lang).locale_data()
                    if isinstance(ftype, pd.Date):
                        format = locale_data.date_format
                    elif isinstance(ftype, pd.Time):
                        format = locale_data.exact_time_format
                    elif hasattr(ftype, 'exact') and not ftype.exact(): # for wiking.DateTime
                        format = locale_data.date_format +' '+ locale_data.time_format
                    else:
                        format = locale_data.date_format +' '+ locale_data.exact_time_format
                    kwargs = dict(format=format)
                else:
                    kwargs = {}
                value, error = ftype.validate(req.param(filter_id), **kwargs)
                if error:
                    errors.append((filter_id, error.message()))
                    value = pytis.data.Value(ftype, None)
                else:
                    req.set_cookie(cookie, value.export())
            else:
                saved_value = req.cookie(cookie)
                if saved_value:
                    value, error = ftype.validate(saved_value)
                else:
                    value = None
                if value is None:
                    default = fspec.default()
                    if callable(default):
                        default = default()
                    value = pytis.data.Value(ftype, default)
            columns.append(pytis.data.ColumnSpec(filter_id, ftype))
            values.append((filter_id, value))
            if operator is None:
                arguments.append((field_id, value))
            else:
                conditions.append(operator(field_id, value))
            fields.append(fspec)
        if fields:
            data = pytis.data.DataFactory(pytis.data.RestrictedMemData, columns).create()
            row = PresentedRow(fields, data, pytis.data.Row(values), resolver=self._row.resolver())
            condition = pytis.data.AND(*conditions)
            filter_fields = [Field.create(row, f, self, self._uri_provider) for f in fields]
        else:
            row = None
            condition = None
            filter_fields = []
        # TODO: self._errors doesn't really belong here -- it belongs to
        # Editform, but it is used within _export_field() so we need to
        # initialize it here or (better) fix _export_field().
        self._filter_fields_errors = self._errors = errors
        self._filter_fields = filter_fields
        self._filter_fields_condition = condition
        self._filter_fields_arguments = arguments
        self._filter_fields_row = row

    def _init_filter_sets(self, filter_sets, req, param):
        for i, filter_set in enumerate(filter_sets):
            filter_set_id = filter_set.id()
            # Determine the current set of user selectable filters.
            null_filter = find(None, filter_set, key=lambda f: f.condition())
            if not null_filter:
                # Translators: Label used in filter selection box for the
                # option which disables filtering and thus results in all
                # records to be displayed.
                null_filter = Filter(self._NULL_FILTER_ID, _("All items"), None)
                filter_set = FilterSet(filter_set_id, filter_set.title(),
                                       [null_filter] + [f for f in filter_set],
                                       default=filter_set.default())
            # Determine the currently selected filter.
            filter_id = param('filter_%s' % (filter_set_id,), str)
            if filter_id is not None:
                req.set_cookie('pytis-form-last-filter-%s' % (filter_set_id,),
                               self._name +':'+ filter_id)
            else:
                cookie = req.cookie('pytis-form-last-filter-%s' % (filter_set_id,))
                if cookie and cookie.startswith(self._name +':'):
                    filter_id = cookie[len(self._name)+1:]
                elif filter_set.default():
                    filter_id = filter_set.default()
                else:
                    filter_id = null_filter.id()
            if filter_id:
                matching_filter = find(filter_id, filter_set, key=lambda f: f.id())
                # Append the current user selected filter to the filter passed
                # as 'filter' argument.
                if matching_filter:
                    cond = matching_filter.condition()
                    if self._filter and cond:
                        self._filter = pd.AND(self._filter, cond)
                    elif cond:
                        self._filter = cond
                else:
                    filter_id = None
            self._filter_ids[filter_set_id] = filter_id
            self._filter_sets.append(filter_set)

    def _tree_level(self):
        if self._tree_order_column:
            order = self._row[self._tree_order_column].value()
            if order is not None:
                # Strip, since LTree values look like '0.2.1', but TreeOrder like '.0.2.1'
                return len(order.strip('.').split('.')) - 1
        return None


    def _export_popup_ctrl(self, context, row, selector):
        items = [lcg.PopupMenuItem(action.title(),
                                   tooltip=action.descr(),
                                   enabled=enabled,
                                   uri=self._uri_provider(row, UriType.ACTION, action))
                 for action, enabled in self._visible_actions(context, row)]
        if items:
            ctrl = lcg.PopupMenuCtrl(items, _("Popup the menu of actions for this record"),
                                     selector)
            return ctrl.export(context)
        else:
            return ''

    def _export_cell(self, context, row, n, field, editable=False):
        value = self._export_field(context, field, editable=editable)
        if field.id == self._column_fields[0].id:
            g = context.generator()
            tree_level = self._tree_level()
            if tree_level is not None and tree_level > 0:
                indent = tree_level * g.span(2*'&nbsp;', cls='tree-indent')
                value = indent + '&bull;&nbsp;'+ g.span(value, cls='tree-node')
            # &#8227 does not work in MSIE
            if self._row_actions:
                value += self._export_popup_ctrl(context, row, 'tr')
        return value

    def _style(self, style):
        def color(c):
            if type(c) is tuple:
                return '#%02x%02x%02x !important' % c
            elif c:
                return c + ' !important'
            else:
                return None
        styles = [name +': '+ f(attr()) for attr, name, f in (
            (style.foreground, 'color',            color),
            (style.background, 'background-color', color),
            (style.slanted,    'font-style',       lambda x: x and 'italic' or 'normal'),
            (style.bold,       'font-weight',      lambda x: x and 'bold' or 'normal'),
            (style.overstrike, 'text-decoration',  lambda x: x and 'line-through' or 'none'),
            (style.underline,  'text-decoration',  lambda x: x and 'underline' or 'none'),
            ) if attr() is not None]
        return '; '.join(styles)
    
    def _field_style(self, field, row):
        field_style = field.style
        if isinstance(field_style, collections.Callable):
            field_style = field_style(row)
        kwargs = {}
        if field_style is not None:
            name = field_style.name()
            if name:
                kwargs['cls'] = name
            style = self._style(field_style)
            if style:
                kwargs['style'] = style
        return kwargs

    def _row_style(self, row, n):
        row_style = self._view.row_style()
        if isinstance(row_style, collections.Callable):
            row_style = row_style(row)
        cls = [n % 2 and 'even' or 'odd', self._group and 'even-group' or 'odd-group']
        if self._group != self._last_group:
            cls.append('group-start')
            if n != 0:
                cls.append('group-change')
        if row_style is not None:
            name = row_style.name()
            if name:
                cls.append(name)
            style = self._style(row_style)
        else:
            style = None
        kwargs = dict(cls=' '.join(cls))
        if style:
            kwargs['style'] = style
        return kwargs
    
    def _export_row(self, context, row, n, row_id):
        g = context.generator()
        cells = [g.td(self._export_cell(context, row, n, field), align=self._align.get(field.id),
                      **self._field_style(field, row))
                 for field in self._column_fields]
        return g.tr(cells, id=row_id, **self._row_style(row, n))

    def _export_aggregation(self, context, op):
        def export_aggregation_value(data, op, field):
            if not field.virtual and isinstance(field.type, pytis.data.Number):
                return data.select_aggregate((op, field.id), condition=self._conditions()).export()
            else:
                return ''
        g = context.generator()
        data = self._row.data()
        agg_id, label = {pytis.data.Data.AGG_SUM: ('agg-sum', _(u"Sum")),
                         pytis.data.Data.AGG_AVG: ('agg-avg', _(u"Average")),
                         pytis.data.Data.AGG_MIN: ('agg-min', _(u"Minimum")),
                         pytis.data.Data.AGG_MAX: ('agg-max', _(u"Maximum"))}[op]
        cells = [i==0 and \
                 g.th(label+':', scope='row') or \
                 g.td(export_aggregation_value(data, op, field), cls='id-'+field.id)
                 for i, field in enumerate(self._column_fields)]
        return g.tr(cells, cls='aggregation-results '+agg_id)
    
    def _export_group_heading(self, context):
        group_heading = self._view.group_heading()
        if isinstance(group_heading, lcg.TranslatableText):
            heading = self._interpolate(context, group_heading, self._row)
        elif group_heading is not None and self._row.visible(group_heading):
            field = self._fields[group_heading]
            heading = self._export_field(context, field)
        else:
            return None
        g = context.generator()
        return g.tr(g.th(heading, colspan=len(self._column_fields)), cls='group-heading')
    
    def _export_headings(self, context):
        g = context.generator()
        current_sorting_column, current_dir = self._sorting[0]
        def sorting_indicator(field):
            sorting = find(field.id, self._sorting, key=lambda x: x[0])
            if sorting:
                return g.span('', cls='sort-indicator sort-direction-%s sort-position-%d' %
                              (self._SORTING_DIRECTIONS[sorting[1]],
                               self._sorting.index(sorting) + 1))
            else:
                return ''
        return [g.th(f.column_label + sorting_indicator(f),
                     cls='column-heading column-id-%s' % f.id
                     + (not f.virtual and ' sortable-column' or ''))
                 for f in self._column_fields]

    def _conditions(self, condition=None):
        conditions = [c for c in (self._condition, self._filter, self._query_condition, condition,
                                  self._filter_fields_condition)
                      if c is not None]
        if len(conditions) == 0:
            return None
        elif len(conditions) == 1:
            return conditions[0]
        else:
            return pytis.data.AND(*conditions)
    
    def _message(self, count):
        if self._custom_message:
            msg = self._custom_message(count)
        elif self._query:
            # Translators: This string uses plural forms.  '%d' is replaced by
            # the number and this number also denotes the plural form used.
            # Please supply translations for all plural forms relevant for the
            # target language.
            msg = _.ngettext("Found %d record matching the search expression.",
                             "Found %d records matching the search expression.",
                             count)
        else:
            msg = None
        return msg

    def _export_body(self, context, form_id):
        g = context.generator()
        if self._async_load: # TODO: Avoid for robot's requests
            content = [g.div(g.div(_("Loading form data..."), cls='ajax-loading'),
                             cls='ajax-container')]
        else:
            content = self._export_table(context, form_id)
        actions = self._export_actions(context, None,
                                       self._uri_provider(None, UriType.LINK, None))
        if actions:
            if self._top_actions:
                content[0:0] = actions
            if self._bottom_actions:
                content += actions
        return content

    def _export_javascript(self, context, form_id):
        g = context.generator()
        uri = self._uri_provider(None, UriType.LINK, None)
        return g.js_call("new pytis.BrowseFormHandler", form_id, self._name, uri)

    def _export_table(self, context, form_id):
        g = context.generator()
        data = self._row.data()
        row = self._row
        limit = self._limit
        exported_rows = []
        if self._arguments or self._filter_fields_arguments:
            arguments = self._arguments or {}
            arguments.update(dict(self._filter_fields_arguments))
        else:
            arguments = None
        count = data.select(condition=self._conditions(), sort=self._sorting, arguments=arguments)
        found = False
        offset = self._offset
        if self._search:
            dist = data.search(self._search)
            if dist:
                found = True
                offset = dist - 1
        if limit is not None:
            page = int(max(0, min(offset, count-1)) / limit)
            first_record_offset = page*limit
        else:
            page = 0
            first_record_offset = 0
        if count == 0:
            pages = 0
        elif limit is None:
            pages = 1
        else:
            pages, modulo = divmod(count, min(limit, count))
            pages += modulo and 1 or 0
        self._group = True
        self._last_group = None
        group_values = last_group_values = None
        n = 0
        data.skip(first_record_offset)
        while True:
            r = data.fetchone()
            if r is None:
                break
            row.set_row(r)
            if self._grouping:
                group_values = [row[cid].value() for cid in self._grouping]
                if group_values != last_group_values:
                    self._group = not self._group
                    last_group_values = group_values
                    group_heading = self._export_group_heading(context)
                    if group_heading is not None:
                        exported_rows.append(group_heading)
            if found and (limit is None and offset == n or \
                          limit is not None and offset == (n + page * limit)):
                row_id = 'found-record'
            else:
                row_id = None
            exported_rows.append(self._export_row(context, row, n, row_id))
            self._last_group = self._group
            n += 1 
            if limit is not None and n >= limit:
                break
        data.close()
        if n == 0:
            if self._message(0):
                # The message about search/filter results is printed in _export_message().
                result = None
            else:
                # Translators: Used in empty list forms.  "Records" refers to
                # database records in the most generic senese possible.
                result = g.strong(_(u"No records."))
        else:
            if limit is None or count <= self._limits[0]:
                summary = _(u"Total records:") +' '+ g.strong(str(count))
            else:
                # Translators: The variables '%(first)s', '%(last)s' and
                # '%(total)s' are replaced by the numbers corresponding to the
                # current listing range.
                summary = _(u"Displayed records %(first)s-%(last)s of total %(total)s",
                            first=g.strong(str(first_record_offset+1)),
                            last=g.strong(str(first_record_offset+n)),
                            total=g.strong(str(count)))
            result = self._wrap_exported_rows(context, exported_rows, summary, count, page, pages)
        return [x for x in
                (self._export_message(context, count),
                 self._export_controls(context, form_id, count, page, pages),
                 result,
                 self._export_controls(context, form_id, count, page, pages, bottom=True))
                if x]

    def _wrap_exported_rows(self, context, rows, summary, count, page, pages):
        g = context.generator()
        if page + 1 == pages:
            # Display aggregations on the last page.
            foot_rows = [self._export_aggregation(context, op)
                         for op in self._view.aggregations()]
        else:
            foot_rows = []
        if self._row_actions:
            context.resource('lcg.js')
            context.resource('lcg-widgets.css')
        headings = self._export_headings(context)
        foot_rows.append(g.tr(g.td(summary, colspan=len(headings))))
        return g.table((g.thead(g.tr(headings)),
                        g.tfoot(foot_rows),
                        g.tbody(rows)), border=1)
    
    def _link_ctrl_uri(self, generator, **kwargs):
        if not kwargs.get('sort') and self._user_sorting:
            sorting_column, direction = self._user_sorting
            kwargs = dict(kwargs, sort=sorting_column, dir=self._SORTING_DIRECTIONS[direction])
        # TODO: Excluding the 'submit' argument is actually a hack, since it is
        # defined in Wiking and should be transparent for the form.
        args = [('form_name', self._name)]
        args += [('filter_%s' % (k,), v,) for k, v in self._filter_ids.items()]
        args += [(k, v) for k, v in self._hidden if k != 'submit']
        return generator.uri(self._handler, *args, **kwargs)

    def _index_search_condition(self, search_string):
        value = pd.Value(pd.String(), search_string+"*")
        return pytis.data.WM(self._sorting[0][0], value, ignore_case=False)
    
    def _export_index_search_controls(self, context):
        g = context.generator()
        field = self._field(self._sorting[0][0])
        if not isinstance(field.type, pd.String) or field.type.enumerator():
            return ()
        result = []
        data = self._row.data()
        for level in range(len(self._index_search_string)+1):
            if level:
                search_string = self._index_search_string[:level]
                condition = self._index_search_condition(search_string)
            else:
                search_string = None
                condition = None
            values = [v.value() for v in data.distinct(field.id, prefix=level+1,
                                                       condition=self._conditions(condition))
                      if v.value() is not None]
            if self._lang == 'cs':
                # Total hack allowing correct usage of Czech character 'ch' in index search.
                # A more appropriate solution would be to handle that on the database level.
                values = [v for v in values if not v.lower().endswith('ch')]
                if search_string and search_string[-1] == search_string[-1].lower():
                    ch = 'ch'
                else:
                    ch = 'CH'
                search_ch_string = (search_string or '')+ch
                condition = self._index_search_condition(search_ch_string)
                try:
                    count = data.select(condition=self._conditions(condition))
                finally:
                    data.close()
                if count:
                    i = 0;
                    while i < len(values) \
                              and (values[i][-1].lower() in (u'á',u'č',u'ď',u'é',u'ě') \
                                   or values[i][-1].lower() < u'i'):
                        i += 1
                    values.insert(i, search_ch_string)
            if len(values) < 3 or len(values) > 100:
                break
            if search_string:
                # Translators: This is a label preceding index search controls.
                # These controls allow the user to move in a long
                # alphabetically sorted list by alphabetical prefix.  For
                # example in a listing of books, one might have the following
                # index search controls: 'Author: A, B, C, D, ...' and
                # selecting 'A' will move us to the first author beginning with
                # A.  At the same time the form will automatically display
                # subordinate index search controls for all authors beginning
                # with 'A': 'Author on "A": Ab, Ac, Ad, Af, ....'.  And this is
                # where this label is used.  '%(label)s' is replaced by the
                # label of the controlling column. '%(prefix)s' is replaced by
                # the selected letter or substring and 'on' has the meaning
                # "beginning with".  Also take care to use the correct
                # quotation marks for the target language (written as the
                # corresponding unicode characters).
                label = _('%(label)s on "%(prefix)s":', label=field.label, prefix=search_string)
            else:
                label = field.label + ":"
            links = [g.a(v, href=self._link_ctrl_uri(g, index_search=v)+'#found-record',
                         # Translators: Index search controls link tooltip.
                         title=_('Skip to the first record beginning with "%s"', v))
                     for v in values]
            result.append(g.div(label +' '+ concat(links, separator=' ')))
        return (g.div(result, cls='index-search-controls'),)

    def _export_message(self, context, count):
        msg = self._message(count)
        if msg:
            return context.generator().div(msg, cls='results')
        else:
            return None

    def _export_controls(self, context, form_id, count, page, pages, bottom=False):
        g = context.generator()
        html_id = form_id + (bottom and '-top' or '-bottom')
        content = []
        # Construct a list of filters for export.
        show_filters = (self._filter_sets and (count or
                                               [v for v in self._filter_ids.values()
                                                if v is not None])
                        or self._filter_fields)
        show_query_field = self._show_query_field
        if show_filters and not bottom:
            # Translators: Button for manual filter invocation.
            submit_button = g.button(g.span(_("Change filters")), cls='apply-filters')
            if self._immediate_filters and not self._filter_fields:
                onchange = 'this.form.submit(); return true'
                # Leave the submit button in place for non-Javascript browsers.
                submit_button = g.noscript(submit_button)
            else:
                onchange = None
            for fid, msg in self._filter_fields_errors:
                f = find(fid, self._filter_fields, key=lambda f: f.id)
                content.append(g.div(g.strong(f.label) + ": " + msg, cls='errors'))
            filter_content = []
            for field in self._filter_fields:
                filter_content.extend((
                        g.label(field.label+':', field.html_id()),
                        self._export_field(context, field, editable=True)))
            for filter_set in self._filter_sets:
                filter_set_id = filter_set.id()
                filter_name = 'filter_%s' % (filter_set_id,)
                filter_id = filter_name + '-' + html_id
                filter_set_content = (
                    g.label(filter_set.title()+': ', filter_id),
                    g.select(name=filter_name, id=filter_id,
                             selected=self._filter_ids.get(filter_set_id),
                             # Translators: Label of filter selection box.
                             # Filtering limits the displayed form records by
                             # certain criterias.
                             title=(_(u"Filter")+' '+
                                    # Translators: Tooltip text suggesting
                                    # keyboard combination to use for selection
                                    # without unexpected invocation of the
                                    # option.
                                    _(u"(Use ALT+arrow down to select)")),
                             options=[(f.title(), f.id()) for f in filter_set],
                             onchange=onchange),
                    )
                filter_content.append(g.span(filter_set_content))
            filter_content.append(submit_button)
            content.append(g.div(filter_content, cls="filter"))
        limit, limits = self._limit, self._limits
        if limit is not None and count > limits[0]:
            controls = ()
            if count > 100:
                if not bottom:
                    index_search_controls = self._export_index_search_controls(context)
                    self._index_search_controls = index_search_controls
                else:
                    index_search_controls = self._index_search_controls
                if index_search_controls:
                    controls += index_search_controls
            if pages > 1:
                offset_id = 'offset-' + html_id
                if self._allow_query_field:
                    search_button = g.button(g.span(_(u"Search")), cls='search-button',
                                             style=show_query_field and 'display:none' or None)
                else:
                    search_button = None
                # Translators: Paging controls allow navigation in long lists which are split into
                # several pages.  The user can select a specific page or browse forward/backwards.
                controls += (g.span((g.label(_(u"Page")+': ', offset_id),
                                     g.select(name='offset', id=offset_id, selected=page*limit,
                                              title=(_(u"Page")+' '+
                                                     _(u"(Use ALT+arrow down to select)")),
                                              onchange='this.form.submit(); return true',
                                              options=[(str(i+1), i*limit) for i in range(pages)]),
                                     ' / ',
                                     g.strong(str(pages))), cls="offset"),
                             g.span((g.button(g.span(_(u"Previous")), name='prev', value='1',
                                              title=_(u"Go to previous page"), disabled=(page == 0),
                                              cls='prev-page-button'),
                                     g.button(g.span(_(u"Next")), name='next', value='1',
                                              title=_(u"Go to next page"),
                                              disabled=(page+1)*limit >= count,
                                              cls='next-page-button'),
                                     ) + (search_button and (search_button,) or ()),
                                    cls="buttons"))
            limit_id = 'limit-' + html_id
            controls += (g.span((g.label(_(u"Records per page")+':', limit_id)+' ',
                                 g.select(name='limit', id=limit_id, selected=limit,
                                          title=(_(u"Records per page")+' '+
                                                 _(u"(Use ALT+arrow down to select)")),
                                          onchange='this.form.submit(); return true',
                                          options=[(str(i), i) for i in limits])),
                                cls='limit'),
                         g.noscript(g.button(g.span(_(u"Go")), cls='goto-page-button')))
            if controls:
                cls = 'paging-controls' + (pages == 1 and ' one-page' or '')
                content.append(g.div(controls, cls=cls))
        if content:
            if not bottom and self._allow_query_field:
                query_id = 'filter-' + html_id
                query_field = g.div((g.label(_(u"Search expression") +': ', query_id),
                                     g.field(self._query, name='query', id=query_id,
                                             cls='query-field'),
                                     g.hidden('show_query_field', show_query_field and '1' or ''),
                                     # Translators: Search button label.
                                     g.button(g.span(_(u"Search")), cls='search-button'),
                                     g.button(g.span(_(u"Cancel")), cls='cancel-search')),
                                    cls='query' + (show_filters and ' with-filter' or ''),
                                    style=not show_query_field and 'display:none' or None)
                content.insert(0, query_field)
            if self._name is not None:
                content.append(g.hidden('form_name', self._name))
            if self._user_sorting:
                sorting_column, direction = self._user_sorting
                content.extend((g.hidden('sort', sorting_column),
                                g.hidden('dir', self._SORTING_DIRECTIONS[direction])))
            return g.form(content, action=g.uri(self._handler), method='GET',
                          cls='list-form-controls')
        else:
            return None

    def export(self, context):
        if self._async_load and self._req.param('_pytis_async_load_request'):
            form_id = context.unique_id()
            return concat(self._export_table(context, form_id))
        else:
            return super(BrowseForm, self).export(context)
        
    def heading_info(self):
        active_filters = [(k, v) for k, v in self._filter_ids.items() if v is not None]
        filter_labels = []
        for filter_id, filter_value in active_filters:
            for filter_set in self._filter_sets:
                if filter_set.id() == filter_id:
                    break
            else:
                continue
            for filter in filter_set:
                if filter.id() == filter_value:
                    if filter.condition():
                        filter_labels.append(filter.name())
                    break
        if filter_labels:
            info = _(u"filtered by: ") + lcg.concat(filter_labels, separator=', ')
        else:
            info = None
        return info

    def filter_field_values(self):
        """Return the current filter field values as a list of pairs (filter_id, value).

        Filter id is the id from the 'filter_fields' specification passed to
        the constructor.  Values are 'pytis.data.Value' instances.  All fields
        present in the 'filter_fields' specification are returned.

        """
        row = self._filter_fields_row
        return [(key, row[key]) for key in row.keys()]


class ListView(BrowseForm):
    """Listing with a customizable layout for each record.

    This form behaves similarly to a regular 'BrowseForm', but the records are
    not represented as table rows but as individual (sub)sections of a
    document.  Field values can be displayed within each section and their
    layout can be customized through the 'list_layout' argument within
    specification (see 'ViewSpec').  If 'list_layout' is not defined, the form
    presentation will degrade to the regular table presentation as in
    'BrowseForm'.

    If list layout is used, each record is wrapped in a div with CSS class
    `list-item' and also all class identifiers supported by 'BrowseForm', such
    as even/odd row and group identifiers and any identifiers computed through
    'row_style' specification (see 'ViewSpec').  Direct row style attributes
    such as color, background or font styles are not supported in list layout,
    since they are considered specific for table layout (used by BrowseForm).

    """
    _CSS_CLS = 'list-view'
    class _Interpolator(object):
        def __init__(self, func):
            self._func = func
        def __getitem__(self, key):
            return self._func(key)

    def __init__(self, view, req, row, list_layout=None, **kwargs):
        if list_layout is None:
            list_layout = view.list_layout()
        layout = list_layout and list_layout.layout() or None
        super(ListView, self).__init__(view, req, row, layout=layout, **kwargs)
        self._list_layout = list_layout
        if list_layout is None:
            super_ = super(ListView, self)
            self._CSS_CLS = super_._CSS_CLS
            self._export_row = super_._export_row
            self._wrap_exported_rows = super_._wrap_exported_rows
            self._export_group_heading = super_._export_group_heading
            self.export = super_.export
        else:
            self._meta = [(self._field(id), id in list_layout.meta_labels())
                          for id in list_layout.meta()]
            self._image = list_layout.image() and self._field(list_layout.image())
            anchor = list_layout.anchor()
            if not anchor and list_layout.allow_index():
                anchor = camel_case_to_lower(self._name, '-') + '-%s'
            if anchor:
                anchor = anchor.replace('%s', '%%(%s)s' % self._key)
            self._anchor = anchor

    def export(self, context):
        self._exported_row_index = []
        return super(ListView, self).export(context)
        
    def _export_row(self, context, row, n, row_id):
        layout = self._list_layout
        g = context.generator()
        parser = lcg.Parser()
        if isinstance(layout.title(), lcg.TranslatableText):
            title = self._interpolate(context, layout.title(), row)
        else:
            title = row[layout.title()].export()
        anchor = self._anchor
        if anchor:
            anchor = anchor % self._Interpolator(lambda key: row[key].export())
            heading = g.a('', name=anchor) + title
        else:
            heading = title
        if layout.allow_index():
            self._exported_row_index.append(g.li(g.a(title, href='#'+anchor)))
        if self._row_actions and layout.popup_actions():
            heading += self._export_popup_ctrl(context, row, 'h3')
        parts = [g.h(heading, level=3)]
        if self._image and row.visible(self._image.id):
            img = self._export_field(context, self._image) 
            if img:
                parts.append(g.span(img, cls='list-layout-image'))
        if self._meta:
            meta = [(labeled and
                     g.span(field.label+":", cls='label id-'+ field.id)+" " or '') + \
                    self._export_field(context, field)
                    for field, labeled in self._meta if row.visible(field.id)]
            if meta:
                parts.append(g.div(concat(meta, separator=', '), cls='meta'))
        if layout.layout():
            parts.append(self._export_group(context, layout.layout()))
        for item in layout.content():
            if isinstance(item, collections.Callable):
                content = item(row)
                if content is None:
                    continue
                cls = 'dynamic-content'
            elif row.visible(item) and row[item].value() is not None:
                field = self._fields[item]
                if field.spec.text_format() == pytis.presentation.TextFormat.LCG:
                    text = row[item].export()
                    content = lcg.Container(parser.parse(text))
                else:
                    content = lcg.HtmlContent(self._export_field(context, field))
                cls = 'content id-'+ item
            else:
                continue
            # Hack: Add a fake container to force the heading level start at 4.
            container = lcg.Container(lcg.Section('', lcg.Section('', content, anchor=anchor)))
            parts.append(g.div(content.export(context), cls=cls))
        if self._row_actions and not layout.popup_actions():
            parts.extend(self._export_actions(context, row,
                                              self._uri_provider(row, UriType.LINK, None)))
        # We use only css class name from row_style, because we consider the
        # other attributes to be BrowseForm specific.
        cls = 'list-item ' + self._row_style(row, n)['cls']
        result = g.div(parts, id=row_id, cls=cls)
        for tree_level in reversed(range(self._tree_level() or 0)):
            result = g.div(result, cls='tree-indent tree-level-%d' % (tree_level + 1))
        return result

    def _export_group_heading(self, context):
        #return context.generator().h(self._export_field(context, field), 3, cls='group-heding')
        return None

    def _wrap_exported_rows(self, context, rows, summary, count, page, pages):
        g = context.generator()
        result = ()
        if self._row_actions:
            context.resource('lcg.js')
            context.resource('lcg-widgets.css')
        if self._exported_row_index:
            result += (g.div(g.ul(*self._exported_row_index), cls="index"),)
        columns = self._list_layout.columns()
        if columns > 1:
            n, mod = divmod(len(rows), columns)
            # Add empty cells to prevent spanning of unfinished grid rows.
            rows.extend(['' for i in range(columns-mod)])
            rows = g.table([g.tr([g.td(r, width="%d%%" % (100/columns), valign='top')
                                  for r in rows[i*columns:(i+1)*columns]])
                            for i in range(n+min(mod, 1))], border=0, cls='grid')
        result += (g.div(rows, cls="body"),
                   g.div(summary, cls="summary"))
        return concat(result)


class ItemizedView(BrowseForm):
    """Simplified listing of records in a form of itemized list.

    This form behaves similarly to a regular 'BrowseForm', but the records are
    presented as items in an unordered bullet list rather than as table rows.
    """
    
    _CSS_CLS = 'itemized-view'
    
    def __init__(self, view, req, row, columns=None, separator=', ', template=None, **kwargs):
        """Arguments:

          columns -- an explicit list of fields shown for each record.  Only
            the first column of the underlying view is shown by default, but if
            a sequence of column identifierrs is passed, multiple values will
            be shown (separated by the 'separator').
          separator -- string used to separate individual values when multiple
          'columns' are shown.
          template -- if used, the list items will be formatted using given
            template string.  The string must be an 'lcg.TranslatableText'
            instance.  The final item text will be produced by interpolating
            variables in the string by the formatted values of corresponding
            fields.  The argument may also be a callable object (function),
            which returns the template string when called with a 'PresentedRow'
            instance as an argument.

          See the parent classes for definition of the remaining arguments.
          
        """
        if not columns:
            columns = (view.columns()[0],) # Include just the first column by default.
        super(ItemizedView, self).__init__(view, req, row, columns=columns, **kwargs)
        assert isinstance(separator, basestring)
        assert template is None or isinstance(template, (lcg.TranslatableText, collections.Callable))
        self._separator = separator
        self._template = template
        
    def _export_row(self, context, row, n, row_id):
        g = context.generator()
        template = self._template
        if template:
            content = self._interpolate(context, template, row)
        else:
            content = concat([self._export_field(context, field)
                              for field in self._column_fields
                              if row.visible(field.id) and row[field.id].value() is not None],
                             separator=self._separator)
        return g.li(content, id=row_id)
        
    def _export_group_heading(self, context):
        #TODO: Create multi-level lists.
        return None
    
    def _wrap_exported_rows(self, context, rows, summary, count, page, pages):
        g = context.generator()
        return g.ul(concat(rows))


class CheckRowsForm(BrowseForm, _SubmittableForm):
    """Web form with checkable boolean columns in each row.

    The form is rendered as an ordinary table, but boolean columns (all or only
    the selected) are represented by a checkbox in each row and the form has
    submit controls.  Thus the user can modify the values in all rows and
    submit the changes in one step.

    *Processing the submitted form:*

    Each checkbox column is represented by one query parameter.  Its name is
    the column identifier and the values are the key column values of all
    checked rows.
    
    """
    _HTTP_METHOD = 'POST'
    
    def __init__(self, view, req, row, check_columns=None, limits=(), limit=None, **kwargs):
        """Arguments:

          check_columns -- a sequence of column identifiers for which the
            checkboxes will be created.  If the argument is omitted, checkboxes
            will automatically appear for all boolean columns.

          See the parent classes for definition of the remaining arguments.

        """
        super(CheckRowsForm, self).__init__(view, req, row, limits=limits, limit=limit, **kwargs)
        assert check_columns is None or isinstance(check_columns, (list, tuple)), check_columns
        if check_columns is None:
            check_columns = tuple([field.id for field in self._column_fields
                                   if isinstance(field.type, pd.Boolean)])
        else:
            if __debug__:
                for cid in check_columns:
                    assert cid in self._row, cid
        self._check_columns = check_columns

    def _export_cell(self, context, row, n, field, editable=False):
        if field.id in self._check_columns:
            return context.generator().checkbox(name=field.id, value=self._row.format(self._key),
                                                checked=self._row[field.id].value())
        else:
            return super(CheckRowsForm, self)._export_cell(context, row, n, field, editable=editable)
        

class EditableBrowseForm(BrowseForm):
    """Web BrowseForm with editable fields in certain columns.

    The form is rendered as an ordinary table, but columns given by constructor argument editable_columns

    are rendered as editable fields in each row.  The form has no submit
    controls -- it must be used inside another submittable form.

    Editable fields use row key in field identifier to allow processing the
    form values on submit.  So for example, The value of column 'count' for the
    row with exported key value '654' will be submitted as parameter
    'count-654'.
    
    """
    
    def __init__(self, view, req, row, editable_columns=None, limits=(), limit=None, **kwargs):
        """Arguments:

          check_columns -- a sequence of column identifiers whoose fields will be editable.

          See the parent classes for definition of the remaining arguments.

        """
        assert isinstance(editable_columns, (list, tuple)), editable_columns
        self._editable_columns = editable_columns
        super(EditableBrowseForm, self).__init__(view, req, row, limits=limits, limit=limit, **kwargs)
        if __debug__:
            for cid in editable_columns:
                assert cid in self._row, cid

    def _export_cell(self, context, row, n, field, editable=False):
        if field.id in self._editable_columns:
            editable = True
        return super(EditableBrowseForm, self)._export_cell(context, row, n, field, editable=editable)

    def _field(self, id, multirow=False):
        if id in self._editable_columns:
            multirow = True
        return super(EditableBrowseForm, self)._field(id, multirow=multirow)
