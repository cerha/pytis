# -*- coding: utf-8 -*-
#
# Copyright (C) 2006-2015 Brailcom, o.p.s.
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
`pytis.form' package.

Pytis currently does not include support running the actual web application.
Application framework which makes use of pytis web forms is implemented
separately.  See the Wiking project at http://www.freebsoft.org/wiking for more
information.

All the content generation is done using the LCG framework.  See
http://www.freebsoft.org/lcg.

"""

import collections
import lcg
import copy
import re

import pytis.data as pd

from pytis.presentation import ActionContext, ViewSpec, GroupSpec, Orientation, Text, Button, \
    Profiles, FilterSet, Filter
import pytis.util

from .field import Field, DateTimeField, RadioField, UriType, Link, localizable_export

_ = pytis.util.translations('pytis-web')

    
class BadRequest(Exception):
    """Exception raised by 'EditForm.ajax_response()' on invalid request parameters."""
    pass
    
class Exporter(lcg.Content):
    """Helper class to simplify returning exported content in AJAX responses.

    The constructor's 'exporter' argument is be a callable object of one
    argument -- the LCG's export context (lcg.HtmlExporter.Context instance).

    """
    def __init__(self, exporter, **kwargs):
        self._exporter = exporter
        super(Exporter, self).__init__(**kwargs)

    def export(self, context):
        return self._exporter(context)


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
    def __init__(self, view, req, row, handler='#', hidden=(), name=None,
                 uri_provider=None, actions=None, **kwargs):
        """Arguments:

          view -- presentation specification as a 'pytis.presentation.ViewSpec'
            instance.
          req -- instance of a class implementing the 'Request' API.
          row -- 'pytis.presentation.PresentedRow' instance.
          handler -- form handler URI as a string.  This URI is used in the
            form's 'action' attribute.
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
        assert actions is None or isinstance(actions, (tuple, list, collections.Callable)), actions
        self._view = view
        self._req = req
        self._row = row
        self._key = row.data().key()[0].id()
        self._handler = handler
        self._uri_provider = uri_provider
        self._hidden = list(hidden)
        self._name = name
        self._actions = actions

    def _export_body(self, context):
        return []

    def _export_javascript(self, context):
        return None

    def _export_form(self, context):
        return self._export_body(context)

    def _export_actions(self, context, record, uri):
        g = context.generator()
        buttons = [
            g.form([g.hidden(name, value is True and 'true' or value) for name, value in
                    [('action', action.id()),
                     ('__invoked_from', self.__class__.__name__),
                     ] + action.kwargs().items()] +
                   action.form_content(g, record, enabled) +
                   [g.button(g.span(action.title()), title=action.descr(),
                             disabled=not enabled, type='submit',
                             cls='action-' + action.id() + (not enabled and ' disabled' or ''))],
                   action=uri)
            for action, enabled in self._visible_actions(context, record)]
        if buttons:
            return [g.div(buttons, cls='actions module-actions-' + self._name)]
        else:
            return []

    def _visible_actions(self, context, record):
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

    def row(self):
        """Return the form row as a pytis.presentation.PresentedRow' instance."""
        return self._row

    def export(self, context):
        g = context.generator()
        cls = 'pytis-form ' + self._CSS_CLS
        if self._name:
            cls += ' ' + pytis.util.camel_case_to_lower(self._name, '-')
        self._form_id = form_id = context.unique_id()
        javascript = self._export_javascript(context)
        if javascript:
            # Javascript dependencies must be allocated before the form is
            # exported because form field export may rely on these dependencies
            # to be available.
            context.resource('prototype.js')
            context.resource('gettext.js')
            context.resource('pytis.js')
            if context.lang() != 'en':
                # Translations for Javascript
                context.resource('pytis-web.%s.po' % context.lang())
        result = g.div(self._export_form(context), cls=cls, id=form_id)
        if javascript:
            result += g.script(javascript)
        context.resource('pytis-forms.css')
        return result

    def heading_info(self):
        """Return basestring to be possibly put into a document heading.

        If the return value is 'None' or an empty basestring, nothing is added
        to the document heading.  Otherwise the returned basestring may or may
        not be added to some document heading, based on decision of the code
        making the final document.

        """
        return None


class FieldForm(Form):
    """Form with formattable fields."""
    
    def __init__(self, *args, **kwargs):
        super(FieldForm, self).__init__(*args, **kwargs)
        self._fields = dict([(f.id(), self._field(f.id())) for f in self._view.fields()])
        
    def _field(self, id, multirow=False):
        return Field.create(self._row, self._view.field(id), self, self._uri_provider,
                            multirow=multirow)
        
    def _export_field(self, context, field, editable=False):
        if editable:
            result = field.editor(context)
        else:
            formatted = field.format(context)
            if field.spec.text_format() == pytis.presentation.TextFormat.LCG:
                if field.spec.printable():
                    uri = self._uri_provider(self._row, UriType.PRINT, field.id)
                    if uri:
                        g = context.generator()
                        img = context.resource('print-field.png')
                        if img:
                            label = g.img(src=context.uri(img), alt=_("Print"))
                        else:
                            label = _("Print")
                        formatted = g.a(label, href=uri,
                                        title=_("Export the contents of this field into PDF"),
                                        cls='print-field-link') + formatted
                wrap = context.generator().div
            else:
                wrap = context.generator().span
            result = wrap(formatted, cls='field id-' + field.id)
        return result

    def _interpolate(self, context, template, row):
        g = context.generator()
        def export_field(fid):
            if row.visible(fid):
                return self._export_field(context, self._fields[fid])
            else:
                return g.escape('')
        if isinstance(template, collections.Callable):
            template = template(row)
        # Translation is called immediately to force immediate interpolation
        # (with the current row data).  Delayed translation (which invokes the
        # interpolation) would use invalid row data (the 'PresentedRow'
        # instance is reused and filled with table data row by row).
        interpolated = context.localize(template.interpolate(export_field))
        return g.escape(interpolated)
    

class LayoutForm(FieldForm):
    """Form with fields arranged according to pytis layout specification."""
    _MAXLEN = 100
    _ALIGN_NUMERIC_FIELDS = False
    _ALLOW_NOT_NULL_INDICATORS = True
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
        if layout is None:
            layout = view.layout().group()
        if isinstance(layout, (tuple, list)):
            layout = GroupSpec(layout, orientation=Orientation.VERTICAL)
        assert isinstance(layout, GroupSpec)
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
                            # vertical group if possible and not specifically disabled.
                            if not field.spec.compact() and group.align_hgroups():
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
                raise pytis.util.ProgramError("Unsupported layout item type:", item)
        if group.orientation() == Orientation.HORIZONTAL:
            if not content.content():
                result = []
            elif group.flexible():
                result = [g.div([g.div((label or '') + content_)
                                 for i, (label, content_, __, ___) in enumerate(content.content())],
                                cls='horizontal-group')]
            else:
                result = [g.table(
                    [g.tr([((g.th(label, cls='label') if label else '') +
                            g.td(content_, cls='ctrl'))
                           for i, (label, content_, __, ___) in enumerate(content.content())])],
                    role='presentation',
                    cls='horizontal-group' + (not omit_first_field_label and ' expanded' or ''))]
        else:
            def td(label, content_, fullsize, right_aligned, fullspan, normalspan):
                if fullsize:
                    return g.td(content_, cls='ctrl', colspan=fullspan)
                else:
                    if content.allow_right_aligned_fields() and right_aligned:
                        spacer = g.td('', cls='spacer')
                        kwargs = dict(cls='ctrl', align='right')
                    else:
                        spacer = ''
                        kwargs = dict(cls='ctrl expanded', colspan=normalspan)
                    return (g.th(label or '', cls='label', align='right') +
                            g.td(content_, **kwargs) + spacer)
            if content.has_labeled_items():
                if content.allow_right_aligned_fields():
                    normalspan = 2
                    fullspan = 3
                else:
                    normalspan = None
                    fullspan = 2
                rows = [g.tr(td(label_, content_, fullsize, right_aligned_, fullspan, normalspan))
                        for label_, content_, fullsize, right_aligned_ in content.content()]
                if rows:
                    result = [g.table(rows, cls='vertical-group', role='presentation')]
                else:
                    result = []
            else:
                result = [item_[1] for item_ in content.content()]
        if result:
            cls = 'group' + (id and ' ' + id or '')
            if group.label():
                cls += ' label-' + lcg.text_to_id(group.label())
                result = g.fieldset(group.label() + ':', result, cls=cls)
            elif content.needs_panel():
                # If there are any items which need a panel and there is no
                # fieldset panel, add a panel styled div.
                result = g.div(result, cls=cls)
            elif not inner:
                # This fieldset fixes MSIE display of top-level horizontal groups...
                result = g.fieldset(None, result, cls='outer')
            else:
                result = lcg.concat(result, separator="\n")
            return result
        else:
            return ''

    def _export_field_label(self, context, field):
        if field.label:
            g = context.generator()
            cls = 'field-label id-' + field.id
            html_id = None
            sign = ''
            if self._EDITABLE:
                html_id = field.html_id()
                if field.indicate_not_null() and self._ALLOW_NOT_NULL_INDICATORS:
                    sign = g.sup("*", cls="not-null")
                if not self._row.editable(field.id):
                    cls += ' disabled'
            return g.label((field.label, sign, ':'), for_=html_id, cls=cls)
        else:
            return None
    
    def _export_field_help(self, context, field):
        return None

    def _field_order(self):
        return self._layout.order()


class _SingleRecordForm(LayoutForm):

    def __init__(self, view, req, row, layout=None, **kwargs):
        layout = layout or view.layout().group()
        super(_SingleRecordForm, self).__init__(view, req, row, layout=layout, **kwargs)
        
    def _export_body(self, context):
        return [self._export_group(context, self._layout)]
    
    
class _SubmittableForm(Form):
    """Mix-in class for forms with submit buttons."""

    def __init__(self, view, req, row, submit_buttons=None,
                 show_cancel_button=False, show_reset_button=True, **kwargs):
        """Arguments:

          submit_buttons -- submit buttons as a sequence of (NAME, LABEL)
            pairs, where LABEL is the button label and NAME is the name of the
            corresponding request parameter which has the value '1' if the form
            was submitted using given submit button.  If NAME is None, no
            request parameter is sent by the button.  The default is just one
            button labeled "Submit" with no NAME.
          show_cancel_button -- boolean flag indicating whether the cancel
            button is displayed.  The cancel button will appear as another
            submit button with name '_cancel'.  If pressed, the form is
            submitted, so the server processing must take the appropriate
            action if the request parameter '_cancel' is present (with value
            '1').
          show_reset_button -- boolean flag indicating whether the form reset
            button is displayed.  Pressing this button will reset all form
            fields to their initial state.
            
          See the parent classes for definition of the remaining arguments.

        """
        self._submit_buttons = submit_buttons or ((None, _("Submit")),)
        self._show_cancel_button = show_cancel_button
        self._show_reset_button = show_reset_button
        self._enctype = None
        self._last_validation_errors = []
        super(_SubmittableForm, self).__init__(view, req, row, **kwargs)
    
    def _export_form(self, context):
        g = context.generator()
        return [g.form((super(_SubmittableForm, self)._export_form(context) +
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
            [g.button(g.span(label), name=name, value='1' if name else None,
                      type='submit', title=_("Submit the form"))
             for name, label in self._submit_buttons]
        if self._show_cancel_button:
            content.append(g.button(g.span(_("Cancel")), type='submit', name='_cancel', value='1'))
        if self._show_reset_button:
            content.append(g.button(g.span(_("Reset")), type='reset', title=_("Undo all changes")))
        return [g.div(content, cls='submit')]

    def _field_order(self):
        """Return the order of visible fields in the form as a sequence of field ids."""
        raise Exception("This method must be defined in derived class.")

    def _check(self):
        errors = []
        for check in self._view.check():
            result = check(self._row)
            if result:
                if isinstance(result, (str, unicode)):
                    result = (result, _("Integrity check failed."))
                else:
                    assert isinstance(result, tuple) and len(result) == 2, \
                        ('Invalid check() result:', result)
                errors.append(result)
        return errors

    def prefill(self, req):
        """Use values passed as request arguments as form prefill.

        The request parameters may contain values to be prefilled in the form
        fields.  The values are displayed in fields even if they are invalid
        (but they will not invoke computers in this case).  Validation errors
        are however not displayed above the form -- the user may edit the
        prefilled values and validation errors will be displayed after form
        submission if any.

        """
        locale_data = req.localizer().locale_data()
        for fid in self._field_order():
            if req.has_param(fid) and self._row.editable(fid):
                field = self._fields[fid]
                if not isinstance(field.type, (pd.Binary, pd.Password)):
                    # Even if the value is vot valid, the field will contain
                    # the passed value, but the error will not be displayed
                    # by '_export_errors()'.
                    field.validate(req, locale_data)

    def validate(self, req):
        """Validate form values and return True if all of them are valid.

        False is returned if at least one field value is invalid or
        specifications's 'check' constraints are not passed.

        """
        self._last_validation_errors = errors = []
        row = self._row
        locale_data = req.localizer().locale_data()
        for fid in self._field_order():
            if row.visible(fid) and row.editable(fid):
                field = self._fields[fid]
                error = field.validate(req, locale_data)
                # lcg.log("Validation:", (fid, req.param(fid), error))
                if error:
                    errors.append((fid, error.message()))
        subform = self._find_subform(self._layout)
        if subform and not subform.validate(req):
            errors.append((None, _("Invalid subform values.")))
        if not errors:
            errors.extend(self._check())
        return not errors


class ShowForm(_SingleRecordForm):
    _CSS_CLS = 'show-form'
    _ALIGN_NUMERIC_FIELDS = True

    def _export_form(self, context):
        uri = self._req.make_uri(self._req.uri())
        return (super(ShowForm, self)._export_form(context) +
                self._export_actions(context, self._row, uri))


class EditForm(_SingleRecordForm, _SubmittableForm):
    _CSS_CLS = 'edit-form'
    _EDITABLE = True
    
    def __init__(self, view, req, row, multipart=None, show_footer=True, **kwargs):
        """Arguments:

          multipart -- force form encoding type to 'multipart/form-data'.  If
            None, the encoding is set to 'multipart/form-data' automatically
            when the form includes any binary (file upload) fields.  If False,
            the form will always use the default encoding type (The 'enctype'
            HTML form attribute is not used) and if True, encoding is always
            forced to 'multipart/form-data'.
          show_footer -- boolean flag which may be used to disable showing the
            form footer.  The footer is the area below the submit buttons that
            includes the note that "Fields marked by an asterisk are
            mandatory".

          See the parent classes for definition of the remaining arguments.

        """
        super(EditForm, self).__init__(view, req, row, **kwargs)
        key = self._key
        order = tuple(self._layout.order())
        if not self._row.new() and key not in order + tuple([k for k, v in self._hidden]):
            self._hidden += [(key, self._row[key].export())]
        assert multipart in (None, True, False), multipart
        if multipart is None:
            multipart = any([f for f in order if isinstance(self._row.type(f), pytis.data.Binary)])
        self._enctype = (multipart and 'multipart/form-data' or None)
        self._error = None
        self._show_footer = show_footer

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

    def _export_form(self, context):
        return (self._export_errors(context) +
                super(EditForm, self)._export_form(context) +
                self._export_footer(context))
    
    def _export_error(self, context, fid, message):
        g = context.generator()
        if fid:
            field = self._fields.get(fid)
            if field:
                href = '#' + field.html_id()
                if isinstance(field, RadioField):
                    # HACK: The first radio field in the radio group is the control
                    # which may get focus...
                    href += '-0'
                label = g.a(field.label, href=href)
            else:
                label = fid
            content = g.strong(label) + ": " + message
        else:
            content = message
        return g.div(content)

    def _export_errors(self, context):
        g = context.generator()
        content = [self._export_error(context, fid, message)
                   for fid, message in self._last_validation_errors]
        if self._error:
            content.append(self._export_error(context, *self._error))
        if content:
            return [g.div(content, cls='errors')]
        else:
            return []

    def _export_javascript(self, context):
        g = context.generator()
        layout_fields = self._layout.order()
        form_id = self._form_id
        fields = [self._fields[fid].javascript(context, form_id, layout_fields)
                  for fid in layout_fields if self._row.visible(fid)]
        return g.js_call('new pytis.Form', form_id, fields) + ';'

    def _export_footer(self, context):
        if self._show_footer:
            for f in self._fields.values():
                if f.label and f.indicate_not_null() and f.id in self._layout.order():
                    g = context.generator()
                    return [g.div(g.span("*", cls="not-null") + ") " +
                                  _("Fields marked by an asterisk are mandatory."),
                                  cls='footer')]
        return []

    def _attachment_storage_request(self, req):
        method_name = req.param('_pytis_attachment_storage_request')
        try:
            method = getattr(self, '_attachment_storage_' + method_name)
        except AttributeError:
            raise BadRequest()
        storage = self._row.attachment_storage(req.param('_pytis_attachment_storage_field'))
        if not storage:
            raise BadRequest()
        return method(req, self._row, storage)

    def _resource2dict(self, resource):
        result = dict([(attr, getattr(resource, attr)())
                       for attr in ('filename', 'uri', 'title', 'descr')],
                      type=resource.__class__.__name__,
                      **(resource.info() or {}))
        if isinstance(resource, lcg.Image) and resource.thumbnail():
            result['thumbnail'] = self._resource2dict(resource.thumbnail())
        return result

    def _attachment_storage_get(self, req, row, storage):
        resource = storage.resource(req.param('filename'))
        return resource and self._resource2dict(resource)
        
    def _attachment_storage_list(self, req, row, storage):
        return [self._resource2dict(r) for r in storage.resources()]
    
    def _attachment_storage_update(self, req, row, storage):
        try:
            import json
        except:
            import simplejson as json
        return storage.update(req.param('filename'), json.loads(req.param('values')))

    def _attachment_storage_insert(self, req, row, storage):
        upload = req.param('upload')
        if not upload:
            raise BadRequest()
        error = storage.insert(upload.filename(), upload.file(), dict(mime_type=upload.mime_type()))
        return {'success': error is None, 'message': error and req.localizer().localize(error),
                'filename': upload.filename()}

    def _find_subform(self, group):
        for item in group.items():
            if isinstance(item, collections.Callable):
                item = item(self._row)
            if isinstance(item, GroupSpec):
                result = self._find_subform(item)
                if result is not None:
                    return result
            elif isinstance(item, EditableBrowseForm):
                return item
        return None

    def is_ajax_request(self, req):
        """Return True if the request is an AJAX request.

        If the current request is a pytis form update request, return True,
        Otherwise return False.  If True is returned, the request should return
        the result of the method 'ajax_response()'.

        """
        return req.param('_pytis_form_update_request') is not None

    def ajax_response(self, req):
        """Return the AJAX request response data.

        This method acts as the server side counter-part of the client side
        code defined in the pytis form JavaScript code in 'pytis.js'.  The
        returned value is typically a data structure to be encoded into a JSON
        string and sent back to the client within the response body with the
        'application/json' content type set in HTTP response headers.  That way
        the client side code will be able to process it.  Alternatively, the
        result may be an 'lcg.Content' instance which should be exported and
        sent back to the client as 'text/html'.

        May raise 'pytis.web.BadRequest' exception if the request parameters
        don't make sense.

        """
        if req.param('_pytis_attachment_storage_request'):
            return self._attachment_storage_request(req)
        if req.param('_pytis_insert_new_row'):
            form = self._find_subform(self._layout)
            return form.ajax_response(req)
        request_number = req.param('_pytis_form_update_request')
        changed_field = str(req.param('_pytis_form_changed_field'))
        order = self._field_order()
        # Validate the changed field last (for AJAX update request) to invoke computers correctly.
        if changed_field:
            if changed_field in order:
                order.remove(changed_field)
            order.append(changed_field)
        state = req.param('_pytis_form_state')
        if state:
            import urlparse
            field_states = dict((k, v[0]) for k, v in urlparse.parse_qs(state).items())
        else:
            field_states = {}
        fields = {}
        localizer = req.localizer()
        localize = localizer.localize
        locale_data = localizer.locale_data()
        row = self._row
        # Validate all fields first.
        for fid in order:
            try:
                field = self._fields[fid]
            except KeyError:
                # May happen e.g. in automated attack attempts
                return dict(request_number=request_number, fields={})
            fields[fid] = fdata = {}
            computer = field.spec.computer()
            if computer and changed_field and changed_field in computer.depends():
                # Don't validate fields which depend on the field currently changed by
                # the user during AJAX form updates.
                error = None
            elif row.editable(fid): # non-editable field values are empty in the request!
                error = field.validate(req, locale_data)
                if error:
                    fdata['error'] = localize(error.message())
        # Compute field state after all fields are validated.
        for fid in order:
            if fid != changed_field:
                field = self._fields[fid]
                fdata = fields[fid]
                fdata['editable'] = row.editable(fid)
                if ((field.spec.computer() and row.invalid_string(fid) is None
                     and not isinstance(field.type, pd.Binary))):
                    exported_value = row[fid].export()
                    localized_value = localize(localizable_export(row[fid]))
                    # Values of disabled fields are not in the request, so send them always...
                    if ((not req.has_param(fid) or
                         req.param(fid) not in (exported_value, localized_value))):
                        fdata['value'] = exported_value
                        fdata['localized_value'] = localized_value
                    # lcg.log('-', fid, localized_value, req.param(fid), field.value());
                if fid in field_states:
                    old_state = field_states[fid]
                    new_state = field.state()
                    if new_state != old_state:
                        enumeration = [(value, localize(label))
                                       for value, label in row.enumerate(fid)]
                        fdata['state'] = new_state
                        fdata['enumeration'] = enumeration
                        if isinstance(field.type, pd.Array):
                            func = self._uri_provider(row, UriType.LINK, fid)
                            def link(value):
                                lnk = func(value)
                                if isinstance(lnk, Link):
                                    return dict(href=lnk.uri(),
                                                title=localize(lnk.title()),
                                                target=lnk.target())
                                else:
                                    return dict(href=lnk)
                            if func:
                                fdata['links'] = dict([(value, link(value))
                                                       for (value, display) in enumeration])
        for fid, error in self._check():
            if fid in fields:
                fields[fid]['error'] = localize(error)
        return dict(request_number=request_number, fields=fields)

    def set_error(self, field_id, error):
        """Arguments:

           field_id -- identifier of the field to which the error relates.  Can
             be None for messages which don't belong to any particular field
             and it is also legal to pass field identifiers, which don't appear
             in the form or even don't exist in its specification (typically
             for fields which only appear in the underlying database objects).

           error -- error message.

        """
        self._error = (field_id, error)


class VirtualForm(EditForm):
    """Edit form with dynamically created in-memory data object

    Pass specification arguments to form constructor to create a form with
    arbitrary fields, layout and other logic.

    """
    _CSS_CLS = 'edit-form virtual-form'

    class FormRecord(pytis.presentation.PresentedRow):

        def __init__(self, req, *args, **kwargs):
            self._req = req
            super(VirtualForm.FormRecord, self).__init__(*args, **kwargs)

        def req(self):
            return self._req

    def __init__(self, req, resolver, spec_kwargs, show_reset_button=False, **kwargs):
        specification = pytis.presentation.Specification.create_from_kwargs(
            resolver,
            data_cls=pytis.data.RestrictedMemData,
            **spec_kwargs
        )
        view = specification.view_spec()
        data = specification.data_spec().create()
        row = self.FormRecord(req, view.fields(), data, None, resolver=resolver, new=True)
        super(VirtualForm, self).__init__(view, req, row, show_reset_button=show_reset_button,
                                          **kwargs)


class QueryFieldsForm(VirtualForm):
    """Special form for representation of browse form query fields (for internal use only)."""
    _CSS_CLS = 'edit-form query-fields-form'
    _ALLOW_NOT_NULL_INDICATORS = False
    _SAVED_EMPTY_VALUE = '-'

    def __init__(self, req, resolver, query_fields, filter_sets, immediate_filters=True,
                 async_load=False):
        if query_fields:
            spec_kwargs = dict(query_fields.view_spec_kwargs())
            fields = list(spec_kwargs.pop('fields'))
            layout = spec_kwargs.pop('layout')
        else:
            spec_kwargs = {}
            fields = []
            layout = pytis.presentation.HGroup()
        if filter_sets:
            fields[0:0] = [self._filter_set_field(fs) for fs in filter_sets]
            ids = tuple([fs.id() for fs in filter_sets])
            if not any(fsid in layout.order() for fsid in ids):
                if layout.orientation() == pytis.presentation.Orientation.HORIZONTAL:
                    layout = pytis.presentation.HGroup(*(ids + layout.items()))
                else:
                    layout = pytis.presentation.HGroup(ids, layout)
        super(QueryFieldsForm, self).__init__(req, resolver,
                                              dict(fields=fields, layout=layout, **spec_kwargs))
        row = self._row
        self._immediate_filters = (immediate_filters and
                                   all(row.type(f).enumerator() is not None
                                       for f in self._field_order()))
        self._async_load = async_load
        if req.param('list-form-controls-submitted'):
            self.validate(req)
        if not self.is_ajax_request(req):
            for field in self.fields():
                if not row.visible(field.id):
                    continue
                cookie = 'pytis-query-field-%s-%s' % (self._name, field.id)
                if req.param('list-form-controls-submitted'):
                    error = row.validation_error(field.id)
                    if not error:
                        value = row[field.id].export()
                        if value == '':
                            value = self._SAVED_EMPTY_VALUE
                        req.set_cookie(cookie, value)
                else:
                    saved_value = req.cookie(cookie)
                    if saved_value is not None:
                        if saved_value == self._SAVED_EMPTY_VALUE:
                            saved_value = ''
                        row.validate(field.id, saved_value)

    def _filter_set_field(self, filter_set):
        # Determine the current set of user selectable filters.
        null_filter = pytis.util.find(None, filter_set, key=lambda f: f.condition())
        if null_filter:
            null_display = null_filter.title()
            filters = [f for f in filter_set if f is not null_filter]
        else:
            # Translators: Label used in filter selection box for the
            # option which disables filtering and thus results in all
            # records to be displayed.
            null_display = _("All items")
            filters = filter_set
        class Enum(pytis.presentation.Enumeration):
            enumeration = [(f.id(), f.title()) for f in filters]
        return pytis.presentation.Field(filter_set.id(), filter_set.title(),
                                        null_display=null_display, not_null=False,
                                        enumerator=Enum, default=filter_set.default())

    def _export_form(self, context):
        g = context.generator()
        return (self._export_errors(context) +
                [g.div(self._export_body(context), cls='body')] +
                self._export_submit(context))

    def _export_submit(self, context):
        g = context.generator()
        # Translators: Button for manual filter invocation.
        submit_button = g.button(g.span(_("Change filters")), type='submit', cls='apply-filters')
        if self._immediate_filters:
            # Hide the submit button, but leave it in place for non-Javascript browsers.
            submit_button = g.noscript(submit_button)
        return [g.div(submit_button, cls='submit')]

    def _export_javascript(self, context):
        script = super(QueryFieldsForm, self)._export_javascript(context)
        if self._immediate_filters and not self._async_load:
            # When the form is loaded asynchronously, the change handlers are assigned
            # in pytis.js (bind_controls)!
            script += ("$('%s').select('select, checkbox, radio').each(function (element) { "
                       "element.onchange = function (e) { this.form.submit(); return true; }; "
                       "});" % self._form_id)
        return script

    def fields(self):
        return [self._fields[f] for f in self._field_order()]


class InlineEditForm(EditForm):
    """Special form for representation of browse form inline edit fields (for internal use only)."""

    def __init__(self, view_spec, req, row, uri, field_id):
        super(InlineEditForm, self).__init__(view_spec, req, row, layout=GroupSpec((field_id,)),
                                             handler=uri)

    def _export_body(self, context):
        field = self._fields[self._layout.order()[0]]
        return [self._export_field(context, field, editable=True)]

    def _export_submit(self, context):
        g = context.generator()
        return [g.button(g.span(_("Save")), type='submit', name='save-edited-cell', value='1',
                         cls='save-edited-cell')]

    def _export_error(self, context, fid, message):
        return message


class FilterForm(EditForm):
    """Simple form for displaying a list of fields for advanced filtering."""
    # This form is currently only used in Wiking Biblio CatalogNews module.
    # The whole thing needs some further work to be generally usable ...
    _CSS_CLS = 'edit-form filter-form'
    
    def __init__(self, fields, req, row, **kwargs):
        view = ViewSpec(_("Filter"), fields)
        kwargs['reset'] = kwargs.get('reset') # Default to None in this class.
        kwargs['submit'] = kwargs.get('submit', _("Apply Filter"))
        super(FilterForm, self).__init__(view, req, row, **kwargs)
        
    def _export_footer(self, context):
        return []

    
class BrowseForm(LayoutForm):
    _CSS_CLS = 'browse-form'
    _HTTP_METHOD = 'GET'
    _SORTING_DIRECTIONS = {pytis.data.ASCENDENT: 'asc',
                           pytis.data.DESCENDANT: 'desc'}
    _EXPORT_EMPTY_TABLE = False
    _SEARCH_STRING_SPLITTER = re.compile('([^\s"]+)|"([^"]*)"|„([^„”]*)”')
    """Determines whether the table is present on output even if it contains no rows."""

    def __init__(self, view, req, row, uri_provider=None, condition=None, arguments=None,
                 columns=None, sorting=None, transform_sorting=None, grouping=None, message=None,
                 limits=(25, 50, 100, 200, 500), limit=50, offset=0, search=None,
                 allow_text_search=None, text_search_condition=None, permanent_text_search=False,
                 filter=None, filter_sets=None, profiles=None, query_fields=None,
                 condition_provider=None, argument_provider=None, immediate_filters=True,
                 top_actions=False, bottom_actions=True, row_actions=False, async_load=False,
                 cell_editable=None, expand_row=None, async_row_expansion=False,
                 on_update_row=None, **kwargs):
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
          transform_sorting -- function of one argument (sorting specification)
            returning a transformed sorting specification.  If not None, this
            function is applied to form sorting specification (visible in the
            user interface) before passing it to the actual data select.
          grouping -- visual grouping of table rows.  The value is a column
            identifier or a sequence of column identifiers.  Grouping allows
            you to visually distinguish table rows, which have the same
            value(s) in grouping columns(s).  This usually only makes sense
            when the table is sorted by these columns so grouping is ignored
            when the user changes sorting.  If not None, this value overrides
            the default grouping defined by the specification, but may be
            further overriden by the grouping defined by the currently selected
            form profile (see 'profiles').
          message -- function returning a custom search result message.  If
            none, a default message will be used, such as 'Found 5 records
            matching the search expression.'.  A function of one argument (the
            form instance) may be used to return a custom message as a string
            (possibly an 'lcg.Localizable' instance).  An example of a custom
            message might be 'Found 15 articles in category Python'.
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
          allow_text_search -- explicitly enable or disable displaying the
            search controls.  Search controls allow filtering the form records
            by a text string.  By default (when None), search controls are
            displayed automatically when the number of records exceeds one
            page.  The controls initially contain only a "Search" button,
            which, when pressed, displays a text field for entering the search
            string above the form.  When the search string is submitted, the
            form is filtered to contain only matching records (see
            'text_search_condition' for explanation how the filtering condition
            is constructed).  Passing True to this argument will force
            displaying the search field without any user action.  Passing
            False, on the other hand, will disable the search controls
            altogether.
          text_search_condition -- define a custom text search condition. A
            function of one argument -- the search string.  If None, the
            default condition is created by splitting the search string into
            separate words by blank characters.  Matching records must contain
            all the words in any of its string columns.
          permanent_text_search -- set to True to make the text search
            permanent.  The search string for given form will be saved in
            browser's cookies to make the searching permanent until the user
            explicitly cancels the search using the Cancel button.  Query
            fields, profiles and filters normally behave this way.  Text search
            does not by default, but you may turn it on using this argument.
          filter -- filter condition as a 'pytis.data.Operator' instance.  This
            condition will be appended to 'condition', but the difference is
            that 'condition' is invisible to the user, but 'filter' may be
            indicated in the user interface.
          filter_sets -- Deprecated. Filter sets can be implemented using query
            fields.
          profiles -- specification of form profiles as a 'Profiles' instance
            or a sequence of 'Profile' instances.  These profiles will be
            available in the user interface for user's selection.  If None, the
            default set of profiles defined by specification is used.  If not
            None, the profiles from specification are ignored.  This argument
            is mostly useful to construct the list of profiles dynamically
            (specification profiles are static).
          query_fields -- Specification of query fields as a sequence of
            'pytis.presentation.Field' instances.  Overrides the form
            specification attribute 'query_fields'.
          condition_provider -- Overrides the form specification attribute
            'condition_provider'.
          argument_provider -- Overrides the form specification attribute
            'argument_provider'.
          immediate_filters -- when True, filters and profiles apply
            immediately after their selection in the corresponding selector;
            when False, there is a separate button for filter application.
            When 'query_fields' are present, filters must always be applied
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
          cell_editable -- function of two arguments (the form row as a
            'PresentedRow' instance and the column's field id) returning
            boolean, indicating whether the cell is editable inline.  By
            default, no cells are editable inline.
          expand_row -- function of two arguments.  The first argument is the
            form row as a 'PresentedRow' instance, the second is the form
            instance.  The function returns expanded row content as lcg.Content
            instance.  When present (not null) each row may be expanded in the
            user interface to display this additional content for each row.
            Typically some additional columns which don't fit in the table can
            be displayed this way eliminating the need to go to another page
            and back to display more details about certain records.
          async_row_expansion -- boolean indicating whether row expansion
            should be loaded asynchronously.  When True, the expanded content
            is loaded on demand for each row.  When False, expansion for all
            rows is part of the initial form data (regardless whether the
            initial form data is loaded synchronously or asynchronously).
          on_update_row -- callback to perform the row update operation after
            cell editation.  If None, the row is saved using the the standard
            'update()' method on forms's data object.  If not None, it must be
            a function of one argument -- the PresentedRow instance.  This
            function is called instead of of the standard update and is
            responsible for saving the changed row.  The return value is None
            on success or a tuple (FIELD_ID, ERROR) on error, where FIELD_ID is
            the id of the field causing the error or None when the error is not
            related to a particular field and ERROR is the error message string.

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
        assert allow_text_search is None or isinstance(allow_text_search, bool), allow_text_search
        assert isinstance(permanent_text_search, bool), permanent_text_search
        assert transform_sorting is None or isinstance(transform_sorting, collections.Callable), \
            transform_sorting
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
        # Process filter sets and profiles first.
        if filter_sets is None:
            filter_sets = self._view.filter_sets()
        if profiles is None:
            profiles = self._view.profiles()
        elif not isinstance(profiles, Profiles):
            profiles = Profiles(*profiles)
        self._profiles = profiles
        self._filter = filter
        self._filters = []
        if profiles:
            assert 'profile' not in [fs.id() for fs in filter_sets]
            # Add profile selection as another filter set, since the user interface is the same.
            filter_set = FilterSet('profile', profiles.label() or _("Profile"),
                                   [Filter(p.id(), p.title(), p.filter())
                                    for p in profiles.unnest()],
                                   default=profiles.default())
            filter_sets = (filter_set,) + tuple(filter_sets)
        self._filter_sets = filter_sets
        if query_fields is None:
            query_fields = self._view.query_fields()
        if query_fields or filter_sets:
            self._query_fields_form = form = QueryFieldsForm(req, self._row.resolver(),
                                                             query_fields, filter_sets,
                                                             immediate_filters=immediate_filters,
                                                             async_load=async_load)
            query_fields_row = form.row()
        else:
            self._query_fields_form = None
        self._current_profile = None
        for filter_set in filter_sets:
            filter_id = query_fields_row[filter_set.id()].value()
            if filter_id:
                matching_filter = pytis.util.find(filter_id, filter_set, key=lambda f: f.id())
                if matching_filter:
                    cond = matching_filter.condition()
                    if cond:
                        self._filters.append(cond)
        if profiles:
            profile_id = query_fields_row['profile'].value() or profiles.default()
            profile = pytis.util.find(profile_id, profiles.unnest(), key=lambda p: p.id())
            if profile:
                self._current_profile = profile
                if profile.columns() is not None:
                    columns = profile.columns()
                if profile.sorting() is not None:
                    sorting = profile.sorting()
                if profile.grouping() is not None:
                    grouping = profile.grouping()
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
        if transform_sorting:
            self._data_sorting = transform_sorting(sorting)
        else:
            self._data_sorting = sorting
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
        # Determine the current text search condition.
        if allow_text_search or allow_text_search is None:
            if req.param('list-form-controls-submitted'):
                text_search_string = param('query', unicode)
            else:
                text_search_string = None
            if permanent_text_search:
                cookie = 'pytis-search-string-%s' % (self._name)
                if req.param('list-form-controls-submitted'):
                    req.set_cookie(cookie, text_search_string)
                else:
                    text_search_string = req.cookie(cookie)
            show_search_field = bool(text_search_string or param('show-search-field', bool)
                                     or allow_text_search)
            allow_search_field = True
        else:
            show_search_field = False
            allow_search_field = False
        if text_search_string:
            if text_search_condition:
                text_search_condition_ = text_search_condition(text_search_string)
            else:
                locale_data = self._req.localizer().locale_data()
                def search_condition(field, string):
                    t = field.type
                    if isinstance(t, (pd.Password, pd.Binary, pd.Big, pd.Boolean)):
                        return None
                    if isinstance(t.enumerator(), pd.FixedEnumerator):
                        for value, label in self._row.enumerate(field.id):
                            if req.localizer().localize(label) == string:
                                return pd.EQ(field.id, pd.Value(t, value))
                    if isinstance(t, pd.String):
                        return pd.WM(field.id, pd.WMValue(f.type, '*' + string + '*'))
                    if isinstance(field, DateTimeField):
                        kwargs = dict(format=field.datetime_format(locale_data))
                    else:
                        kwargs = dict()
                    value, error = t.validate(string, strict=False, **kwargs)
                    if not error:
                        return pd.EQ(field.id, value)
                    else:
                        return None
                text_search_condition_ = pd.AND(*[
                    pd.OR(*[c for c in [search_condition(f, g1 or g2 or g3)
                                        for f in self._fields.values() if not f.virtual]
                            if c is not None])
                    for g1, g2, g3 in self._SEARCH_STRING_SPLITTER.findall(text_search_string)
                ])
        else:
            text_search_condition_ = None
        self._text_search_string = text_search_string
        self._text_search_condition = text_search_condition_
        self._show_search_field = show_search_field
        self._allow_search_field = allow_search_field
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
        self._message = message
        provider_kwargs = dict(req=self._req)
        if query_fields:
            provider_kwargs['query_fields'] = query_fields_row
        if condition_provider is None:
            condition_provider = self._view.condition_provider()
        if condition_provider:
            provider_condition = condition_provider(**provider_kwargs)
            if provider_condition:
                if condition:
                    condition = pd.AND(condition, provider_condition)
                else:
                    condition = provider_condition
        if argument_provider is None:
            argument_provider = self._view.argument_provider()
        if argument_provider:
            provider_arguments = argument_provider(**provider_kwargs)
            if provider_arguments:
                if arguments:
                    arguments.update(provider_arguments)
                else:
                    arguments = provider_arguments
        if data.arguments():
            assert arguments, "No arguments passed to a table function"
        else:
            assert not arguments, "Arguments passed to a non-table function"
        self._condition = condition
        self._arguments = arguments
        self._top_actions = top_actions
        self._bottom_actions = bottom_actions
        self._row_actions = row_actions
        self._async_load = async_load
        self._cell_editable = cell_editable or (lambda x: False)
        self._expand_row = expand_row
        self._async_row_expansion = async_row_expansion
        self._on_update_row = on_update_row
        self._select_columns = [c.id() for c in self._row.data().columns()
                                if not isinstance(c.type(), pytis.data.Big)]
        self._row_count = None

    def _tree_level(self):
        if self._tree_order_column:
            order = self._row[self._tree_order_column].value()
            if order is not None:
                # Strip, since LTree values look like '0.2.1', but TreeOrder like '.0.2.1'
                return len(order.strip('.').split('.')) - 1
        return None

    def _export_popup_ctrl(self, context, row, selector):
        actions = self._visible_actions(context, row)
        # Suppress the popup menu when it consists of a single item for action='view'.
        if actions and not (len(actions) == 1 and actions[0][0].id() == 'view'):
            items = [lcg.PopupMenuItem(action.title(),
                                       tooltip=action.descr(),
                                       enabled=enabled,
                                       uri=self._uri_provider(row, UriType.ACTION, action))
                     for action, enabled in self._visible_actions(context, row)]
            ctrl = lcg.PopupMenuCtrl(items, _("Popup the menu of actions for this record"),
                                     selector)
            return ctrl.export(context)
        else:
            return ''

    def _export_cell(self, context, row, n, field, editable=False):
        g = context.generator()
        content = self._export_field(context, field, editable=editable)
        if field.id == self._column_fields[0].id:
            tree_level = self._tree_level()
            if tree_level is not None and tree_level > 0:
                indent = tree_level * g.span(2 * '&nbsp;', cls='tree-indent')
                content = indent + '&bull;&nbsp;' + g.span(content, cls='tree-node')
            # &#8227 does not work in MSIE
            if self._row_actions:
                content += self._export_popup_ctrl(context, row, 'tr')
        return content

    def _style(self, style):
        def color(c):
            if type(c) is tuple:
                return '#%02x%02x%02x !important' % c
            elif c:
                return c + ' !important'
            else:
                return None
        styles = [name + ': ' + f(attr()) for attr, name, f in (
            (style.foreground, 'color', color),
            (style.background, 'background-color', color),
            (style.slanted, 'font-style', lambda x: x and 'italic' or 'normal'),
            (style.bold, 'font-weight', lambda x: x and 'bold' or 'normal'),
            (style.overstrike, 'text-decoration', lambda x: x and 'line-through' or 'none'),
            (style.underline, 'text-decoration', lambda x: x and 'underline' or 'none'),
        ) if attr() is not None]
        return '; '.join(styles)
    
    def _field_style(self, context, field, row):
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
        if self._cell_editable(row, field.id):
            if 'cls' in kwargs:
                kwargs['cls'] += ' editable-cell'
            else:
                kwargs['cls'] = 'editable-cell'
            if isinstance(field, DateTimeField):
                # This is a quick hack to make date fields inline editable
                # (their UI depends on those additional resources).  Better
                # might be adding these resources dynamically on AJAX response
                # (as the ajax response export actually knows about the
                # dependencies) or at least asking the field instance.
                context.resource('calendarview.js')
                context.resource('calendarview.css')
        return kwargs

    def _row_attr(self, row, n):
        row_style = self._view.row_style()
        if isinstance(row_style, collections.Callable):
            row_style = row_style(row)
        cls = ['data-row',
               n % 2 and 'even' or 'odd',
               self._group and 'even-group' or 'odd-group']
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
        attr = dict(cls=' '.join(cls))
        if style:
            attr['style'] = style
        attr['data-pytis-row-key'] = row[self._key].export()
        return attr
        
    def _columns_count(self):
        n = len(self._column_fields)
        if self._expand_row:
            n += 1 # One extra column for row expansion controls.
        return n
    
    def _export_row(self, context, row, n, row_id):
        g = context.generator()
        cells = [g.td(self._export_cell(context, row, n, field), align=self._align.get(field.id),
                      **self._field_style(context, field, row))
                 for field in self._column_fields]
        result = g.tr(cells, id=row_id, **self._row_attr(row, n))
        if self._expand_row and not self._async_row_expansion:
            content = self._expand_row(row, self)
            result += g.tr(g.td(content.export(context), colspan=self._columns_count()),
                           cls='row-expansion', style="display: none;")
        return result

    def _export_aggregation(self, context, op):
        def export_aggregation_value(data, op, field):
            if not field.virtual and isinstance(field.type, pytis.data.Number):
                return data.select_aggregate((op, field.id), condition=self._conditions()).export()
            else:
                return ''
        g = context.generator()
        data = self._row.data()
        agg_id, label = {pytis.data.Data.AGG_SUM: ('agg-sum', _("Sum")),
                         pytis.data.Data.AGG_AVG: ('agg-avg', _("Average")),
                         pytis.data.Data.AGG_MIN: ('agg-min', _("Minimum")),
                         pytis.data.Data.AGG_MAX: ('agg-max', _("Maximum"))}[op]
        cells = [i == 0 and
                 g.th(label + ':', scope='row') or
                 g.td(export_aggregation_value(data, op, field), cls='id-' + field.id)
                 for i, field in enumerate(self._column_fields)]
        return g.tr(cells, cls='aggregation-results ' + agg_id)
    
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
        return g.tr(g.th(heading, colspan=self._columns_count()), cls='group-heading')
    
    def _export_headings(self, context):
        g = context.generator()
        current_sorting_column, current_dir = self._sorting[0]
        def sorting_indicator(field):
            sorting = pytis.util.find(field.id, self._sorting, key=lambda x: x[0])
            # Don't indicate default sorting.  It works only when the default
            # sorting is ascending and on only single column.  In all other
            # cases the asc-desc-default sorting cycle principle produces
            # unintended and confusing results.
            if sorting and self._sorting != self._view.sorting():
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
        conditions = [c for c in (self._condition,
                                  self._filter,
                                  self._text_search_condition,
                                  condition,
                                  ) + tuple(self._filters)
                      if c is not None]
        if len(conditions) == 0:
            return None
        elif len(conditions) == 1:
            return conditions[0]
        else:
            return pytis.data.AND(*conditions)

    def _export_body(self, context):
        g = context.generator()
        if self._async_load: # TODO: Avoid for robot's requests
            if self._query_fields_form:
                # Quick HACK: Make sure all resources needed by query fields,
                # such as JavaScript and CSS files, ale allocated in advance.
                # Better solution will be dynamically adding these resources
                # allocated within the async request processing, but it will
                # need a little more work...  Thus we export the query fields
                # form just for side effects and ignore the result.
                self._query_fields_form.export(context)
            content = [g.div(g.div(_("Loading form data..."), cls='ajax-loading'),
                             cls='ajax-container')]
        else:
            content = self._export_table(context)
        actions = self._export_actions(context, None,
                                       self._uri_provider(None, UriType.LINK, None))
        if actions:
            if self._top_actions:
                content[0:0] = actions
            if self._bottom_actions:
                content += actions
            if self._row_actions:
                # Call here to allocate the resources in the main page (before the async load
                # request)
                context.resource('lcg.js')
                context.resource('lcg-widgets.css')
        return content

    def _export_javascript(self, context):
        g = context.generator()
        uri = self._uri_provider(None, UriType.LINK, None)
        return g.js_call("new pytis.BrowseForm", self._form_id, self._name, uri) + ';'

    def _set_row(self, row):
        # Please, don't think about passing reset=True here.  See PresentedRow.display()
        # for some hints.  It would unnecessarily call computers, some of which may be
        # really expensive (i.e. on binary fields).
        self._row.set_row(row)

    def _table_rows(self):
        data = self._row.data()
        self._row_count = data.select(columns=self._select_columns,
                                      condition=self._conditions(),
                                      arguments=self._arguments,
                                      sort=self._data_sorting)
        def generator():
            try:
                while True:
                    row = data.fetchone()
                    if row is None:
                        break
                    yield row
            finally:
                try:
                    data.close()
                except:
                    pass
        return generator()

    def _export_table(self, context):
        data = self._row.data()
        limit = self._limit
        exported_rows = []
        rows = self._table_rows()
        row_count = self._row_count
        found = False
        offset = self._offset
        if self._search:
            dist = data.search(self._search)
            if dist:
                found = True
                offset = dist - 1
        if limit is not None:
            page = int(max(0, min(offset, row_count - 1)) / limit)
            first_record_offset = page * limit
        else:
            page = 0
            first_record_offset = 0
        if row_count == 0:
            pages = 0
        elif limit is None:
            pages = 1
        else:
            pages, modulo = divmod(row_count, min(limit, row_count))
            pages += modulo and 1 or 0
        self._group = True
        self._last_group = None
        group_values = last_group_values = None
        current_row_number = 0
        data.skip(first_record_offset)
        for row in rows:
            self._set_row(row)
            if self._grouping:
                group_values = [self._row[cid].value() for cid in self._grouping]
                if group_values != last_group_values:
                    self._group = not self._group
                    last_group_values = group_values
                    group_heading = self._export_group_heading(context)
                    if group_heading is not None:
                        exported_rows.append(group_heading)
            if found and (limit is None and offset == current_row_number or
                          limit is not None and offset == (current_row_number + page * limit)):
                row_id = 'found-record'
            else:
                row_id = None
            exported_rows.append(self._export_row(context, self._row, current_row_number, row_id))
            self._last_group = self._group
            current_row_number += 1
            if limit is not None and current_row_number >= limit:
                break
        count_on_page = current_row_number
        data.close()
        if count_on_page == 0 and not self._EXPORT_EMPTY_TABLE:
            body = None
        else:
            g = context.generator()
            body = g.div((
                self._wrap_exported_rows(context, exported_rows, page, pages),
                self._export_summary(context, limit, first_record_offset, count_on_page),
            ), cls='body')
        return [x for x in
                (self._export_message(context),
                 self._export_controls(context, page, pages),
                 body,
                 self._export_controls(context, page, pages, bottom=True))
                if x]

    def _wrap_exported_rows(self, context, rows, page, pages):
        g = context.generator()
        if page + 1 == pages:
            # Display aggregations on the last page.
            foot = g.tfoot([self._export_aggregation(context, op)
                            for op in self._view.aggregations()])
        else:
            foot = ''
        headings = self._export_headings(context)
        cls = ['data-table']
        if self._expand_row:
            cls.append('expansible-rows')
        return g.table((g.thead(g.tr(headings, cls='column-headings')),
                        foot,
                        g.tbody(rows)),
                       border=1, cls=' '.join(cls))

    def _export_summary(self, context, limit, first_record_offset, count_on_page):
        g = context.generator()
        row_count = self._row_count
        if limit is None or row_count <= self._limits[0]:
            summary = _("Total records:") + ' ' + g.strong(str(row_count))
        else:
            # Translators: The variables '%(first)s', '%(last)s' and
            # '%(total)s' are replaced by the numbers corresponding to the
            # current listing range.
            summary = _("Displayed records %(first)s-%(last)s of total %(total)s",
                        first=g.strong(str(first_record_offset + 1)),
                        last=g.strong(str(first_record_offset + count_on_page)),
                        total=g.strong(str(row_count)))
        return g.div(summary, cls='summary')
        
    
    def _link_ctrl_uri(self, generator, **kwargs):
        if not kwargs.get('sort') and self._user_sorting:
            sorting_column, direction = self._user_sorting
            kwargs = dict(kwargs, sort=sorting_column, dir=self._SORTING_DIRECTIONS[direction])
        args = [('form_name', self._name)]
        if self._query_fields_form:
            row = self._query_fields_form.row()
            args += [(key, row[key].export()) for key in row.keys()]
        # TODO: Excluding the 'submit' argument is actually a hack, since it is
        # defined in Wiking and should be transparent for the form.
        args += [(k, v) for k, v in self._hidden if k != 'submit']
        return generator.uri(self._handler, *args, **kwargs)

    def _index_search_condition(self, search_string):
        value = pd.Value(pd.String(), search_string + "*")
        return pytis.data.WM(self._data_sorting[0][0], value, ignore_case=False)
    
    def _export_index_search_controls(self, context):
        g = context.generator()
        field = self._field(self._sorting[0][0])
        if not isinstance(field.type, pd.String) or field.type.enumerator():
            return ()
        result = []
        data = self._row.data()
        for level in range(len(self._index_search_string) + 1):
            if level:
                search_string = self._index_search_string[:level]
                condition = self._index_search_condition(search_string)
            else:
                search_string = None
                condition = None
            if context.lang() == 'cs':
                # Hack allowing correct usage of Czech character 'ch' in index search.
                # We need to look for longer prefixes and eliminate duplicates here.
                if self._index_search_string[level - 1:].lower().startswith('ch'):
                    # Skip prefixes ending with 'C' when they actyally end with 'CH'.
                    continue
                values = []
                for v in data.distinct(field.id, prefix=level + 2,
                                       condition=self._conditions(condition)):
                    value = v.value()
                    if value is not None:
                        if not value.lower().endswith('ch'):
                            value = value[:-1]
                        if value not in values:
                            values.append(value)
            else:
                values = [v.value() for v in data.distinct(field.id, prefix=level + 1,
                                                           condition=self._conditions(condition))
                          if v.value() is not None]
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
            links = [g.a(v, href=self._link_ctrl_uri(g, index_search=v) + '#found-record',
                         # Translators: Index search controls link tooltip.
                         title=_('Skip to the first record beginning with "%s"', v))
                     for v in values]
            result.append(g.div(label + ' ' + lcg.concat(links, separator=' ')))
        return (g.div(result, cls='index-search-controls'),)

    def _export_message(self, context):
        if self._message:
            msg = self._message(self)
        elif self._text_search_string:
            # Translators: This string uses plural forms.  '%d' is replaced by
            # the number and this number also denotes the plural form used.
            # Please supply translations for all plural forms relevant for the
            # target language.
            msg = _.ngettext("Found %d record matching the search expression.",
                             "Found %d records matching the search expression.",
                             self._row_count)
        elif self._row_count == 0 and not self._EXPORT_EMPTY_TABLE:
            # Translators: Used in empty list forms.  "Records" refers to
            # database records in the most generic senese possible.
            msg = _("No records.")
        else:
            msg = None
        if msg:
            return context.generator().div(msg, cls='results')
        else:
            return None

    def _export_controls(self, context, page, pages, bottom=False):
        g = context.generator()
        ids = context.id_generator()
        content = []
        show_search_field = self._show_search_field
        if self._query_fields_form:
            exported_query_fields = self._query_fields_form.export(context)
            if bottom:
                # The form must be present to preserve the query field values
                # when the bottom controls are used.  Simple hidden fields
                # don't work for all field types (datetime etc.).
                exported_query_fields = g.div(exported_query_fields, style='display: none;')
            # TODO: Hide when there are no records and no active filtering conditions?
            #       and (count or [v for v in self._filter_ids.values() if v is not None])
            content.append(exported_query_fields)
        count, limit, limits = self._row_count, self._limit, self._limits
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
                # Translators: Paging controls allow navigation in long lists which are split into
                # several pages.  The user can select a specific page or browse forward/backwards.
                controls += (
                    g.span((
                        g.label(_("Page") + ':', ids.offset),
                        g.select(name='offset', id=ids.offset, selected=page * limit,
                                 title=(_("Page") + ' ' + _("(Use ALT+arrow down to select)")),
                                 onchange='this.form.submit(); return true',
                                 options=[(str(i + 1), i * limit) for i in range(pages)]),
                        g.span(str(page + 1), cls='current-page'),
                        g.span(' / ', cls='separator'),
                        g.span(str(pages), cls='total-pages'),
                    ), cls="offset"),
                    g.span((
                        g.button(g.span(_("Previous")), title=_("Go to previous page"),
                                 name='prev', value='1', disabled=(page == 0),
                                 type='submit', cls='prev-page'),
                        g.button(g.span(_("Next")), title=_("Go to next page"),
                                 name='next', value='1', disabled=(page + 1) * limit >= count,
                                 type='submit', cls='next-page'),
                        g.button(g.span(_("Search")), type='submit', cls='search',
                                 style=show_search_field and 'display:none' or None)
                        if self._allow_search_field else '',
                    ), cls="buttons"),
                )
            controls += (g.span((g.label(_("Records per page") + ':', ids.limit),
                                 g.select(name='limit', id=ids.limit, selected=limit,
                                          title=(_("Records per page") + ' ' +
                                                 _("(Use ALT+arrow down to select)")),
                                          onchange='this.form.submit(); return true',
                                          options=[(str(i), i) for i in limits])),
                                cls='limit'),
                         g.noscript(g.button(g.span(_("Go")), type='submit',
                                             cls='goto-page-button')))
            if controls:
                cls = 'paging-controls' + (pages == 1 and ' one-page' or '')
                content.append(g.div(controls, cls=cls))
        if content:
            if not bottom and self._allow_search_field:
                search_field = g.div(
                    (g.label(_("Search expression") + ':', ids.search),
                     g.input(type='search', value=self._text_search_string,
                             name='query', id=ids.search, cls='text-search-field'),
                     g.hidden('show-search-field', show_search_field and '1' or ''),
                     # Translators: Search button label.
                     g.button(g.span(_("Search")), type='submit', cls='search-button'),
                     g.button(g.span(_("Cancel")), type='submit', cls='cancel-search')),
                    cls='query', style=not show_search_field and 'display:none' or None,
                )
                content.insert(0, search_field)
            if self._name is not None:
                content.append(g.hidden('form_name', self._name))
            if self._user_sorting:
                sorting_column, direction = self._user_sorting
                content.extend((g.hidden('sort', sorting_column),
                                g.hidden('dir', self._SORTING_DIRECTIONS[direction])))
            content.append(g.hidden('list-form-controls-submitted', '1'))
            return g.form(
                content,
                action=g.uri(self._handler), method='GET',
                cls='list-form-controls %s-list-form-controls' % ('bottom' if bottom else 'top'),
            )
        else:
            return None

    def _set_async_request_row(self, req):
        data = self._row.data()
        key_value = req.param('_pytis_row_key')
        key_type = data.find_column(self._key).type()
        key, err = key_type.validate(key_value)
        row = data.row(key)
        self._row.set_row(row)

    def _edit_cell_response(self, req):
        column_id = req.param('_pytis_column_id')
        self._set_async_request_row(req)
        if not self._cell_editable(self._row, column_id):
            raise BadRequest()
        form = InlineEditForm(self._view, req, self._row, self._handler, column_id)
        if req.param('save-edited-cell'):
            # The cell edit form was submitted.
            if form.validate(req):
                if self._on_update_row:
                    error = self._on_update_row(self._row)
                else:
                    # Update all columns as other columns may
                    # change due to computer dependencies.
                    data = self._row.data()
                    key = self._row[self._key]
                    rowdata = [(c.id(), self._row[c.id()]) for c in data.columns()]
                    try:
                        data.update(key, pytis.data.Row(rowdata))
                    except pd.DBException as e:
                        if e.exception():
                            error = (None, unicode(e.exception()).strip())
                        else:
                            error = (None, e.message())
                    else:
                        error = None
                if error:
                    form.set_error(*error)
                else:
                    def export_cell(context):
                        return self._export_field(context, self._fields[column_id],
                                                  editable=False)
                    return Exporter(export_cell)
        # Show the form inside the cell.
        return form

    def _expand_row_response(self, req):
        self._set_async_request_row(req)
        if not self._expand_row or not self._async_row_expansion:
            raise BadRequest()
        return self._expand_row(self._row, self)

    def export(self, context):
        if self._async_load and self._req.param('_pytis_async_load_request'):
            return lcg.concat(self._export_table(context))
        else:
            return super(BrowseForm, self).export(context)
        
    def heading_info(self):
        if self._query_fields_form:
            row = self._query_fields_form.row()
            filter_names = []
            for filter_set in self._filter_sets:
                f = pytis.util.find(row[filter_set.id()].value(), filter_set, key=lambda x: x.id())
                if f and f.condition():
                    filter_names.append(f.title())
            if filter_names:
                return _("filtered by: ") + lcg.concat(filter_names, separator=', ')
        return None

    def query_field_values(self):
        """Return the current filter field values as a list of pairs (field_id, value).

        Filter id is the id from the 'query_fields' specification.  Values are
        'pytis.data.Value' instances.  All fields present in the 'query_fields'
        specification are returned.

        """
        row = self._query_fields_form.row()
        fs_keys = [fs.id() for fs in self._filter_sets]
        return [(key, row[key]) for key in row.keys() if key not in fs_keys]

    def current_profile(self):
        """Return the current form profile as 'pytis.presentation.Profile' instance."""
        return self._current_profile
        
    def condition(self):
        """Return the current form condition as 'pytis.data.Operator' instance."""
        return self._conditions()
        
    def arguments(self):
        """Return the current select arguments as a dictionary."""
        return self._arguments

    def rows(self):
        """Return a generator returning all form data rows as 'pytis.data.Row' instances.
        
        This method can not be called inside 'export()' and vice versa (export
        can not be called before the row generator is exhausted).

        """
        return self._table_rows()

    def row_count(self):
        """Return the number of rows in the form as integer.
        
        This method will return None before either 'export()' or 'rows()' is called.

        """
        return self._row_count

    def text_search_string(self):
        """Return the current text of the search field or None if text search is not active."""
        return self._text_search_string

    def is_ajax_request(self, req):
        """Return True if the request is an AJAX request.

        If the current request is a pytis form update request, return True,
        Otherwise return False.  If True is returned, the request should return
        the result of the method 'ajax_response()'.

        BrowseForm may emit ajax requests for query fields form updates.

        """
        if ((req.param('_pytis_form_update_request') and
             (req.param('_pytis_edit_cell') or req.param('_pytis_expand_row')))):
            return True
        elif self._query_fields_form:
            return self._query_fields_form.is_ajax_request(req)
        else:
            return False

    def ajax_response(self, req):
        """Return the AJAX request response as a JSON encoded data structure.

        Same rules for the returned value apply as in 'EditForm.ajax_response()'.

        """
        if req.param('_pytis_expand_row'):
            return self._expand_row_response(req)
        elif req.param('_pytis_edit_cell'):
            return self._edit_cell_response(req)
        else:
            return self._query_fields_form.ajax_response(req)


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
                anchor = pytis.util.camel_case_to_lower(self._name, '-') + '-%s'
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
        layout_title = layout.title()
        if isinstance(layout_title, lcg.TranslatableText):
            title = self._interpolate(context, layout_title, row)
        else:
            title = row[layout_title].export()
        anchor = self._anchor
        if anchor:
            anchor = anchor % self._Interpolator(lambda key: row[key].export())
            heading = g.a('', name=anchor) + title
        else:
            heading = title
        if layout.allow_index():
            self._exported_row_index.append(g.li(g.a(title, href='#' + anchor)))
        if self._row_actions and layout.popup_actions():
            heading += self._export_popup_ctrl(context, row, 'h3')
        parts = [g.h(heading, level=3)]
        if self._image and row.visible(self._image.id):
            img = self._export_field(context, self._image)
            if img:
                parts.append(g.span(img, cls='list-layout-image'))
        if self._meta:
            meta = [(g.span(field.label + ":", cls='label id-' + field.id) + " "
                     if labeled else g.noescape('')) +
                    self._export_field(context, field)
                    for field, labeled in self._meta if row.visible(field.id)]
            if meta:
                parts.append(g.div(lcg.concat(meta, separator=', '), cls='meta'))
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
                cls = 'content id-' + item
            else:
                continue
            # Hack: Create a fake container to force the heading level start at 4.
            lcg.Container(lcg.Section('', lcg.Section('', content, id=anchor)))
            parts.append(g.div(content.export(context), cls=cls))
        if self._row_actions and not layout.popup_actions():
            parts.extend(self._export_actions(context, row,
                                              self._uri_provider(row, UriType.LINK, None)))
        # We use only css class name from row_style, because we consider the
        # other attributes to be BrowseForm specific.
        cls = 'list-item ' + self._row_attr(row, n)['cls']
        result = g.div(parts, id=row_id, cls=cls)
        for tree_level in reversed(range(self._tree_level() or 0)):
            result = g.div(result, cls='tree-indent tree-level-%d' % (tree_level + 1))
        return result

    def _export_group_heading(self, context):
        # return context.generator().h(self._export_field(context, field), 3, cls='group-heding')
        return None

    def _wrap_exported_rows(self, context, rows, page, pages):
        g = context.generator()
        if self._row_actions:
            context.resource('lcg.js')
            context.resource('lcg-widgets.css')
        columns = self._list_layout.columns()
        if columns > 1:
            n, mod = divmod(len(rows), columns)
            # Add empty cells to prevent spanning of unfinished grid rows.
            rows.extend(['' for i in range(columns - mod)])
            rows = g.table([g.tr([g.td(r, width="%d%%" % (100 / columns), valign='top')
                                  for r in rows[i * columns:(i + 1) * columns]])
                            for i in range(n + min(mod, 1))], border=0, cls='grid')
        result = g.div(rows, cls='content')
        if self._exported_row_index:
            result = g.concat(g.div(g.ul(*self._exported_row_index), cls="index"),
                              result)
        return result


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
        assert (template is None or
                isinstance(template, (lcg.TranslatableText, collections.Callable)))
        self._separator = separator
        self._template = template
        
    def _export_row(self, context, row, n, row_id):
        g = context.generator()
        template = self._template
        if template:
            content = self._interpolate(context, template, row)
        else:
            content = lcg.concat([self._export_field(context, field)
                                  for field in self._column_fields
                                  if row.visible(field.id) and row[field.id].value() is not None],
                                 separator=self._separator)
        if self._row_actions:
            content += self._export_popup_ctrl(context, row, 'li')
        return g.li(content, id=row_id)
        
    def _export_group_heading(self, context):
        # TODO: Create multi-level lists.
        return None
    
    def _wrap_exported_rows(self, context, rows, page, pages):
        g = context.generator()
        return g.ul(lcg.concat(rows))


class CheckRowsForm(BrowseForm, _SubmittableForm):
    """Web form with checkable boolean columns in each row.

    The form is rendered as an ordinary table, but boolean columns (all or only
    the selected) are represented by a checkbox in each row and the form has
    submit controls.  Thus the user can modify the values in all rows and
    submit the changes in one step.

    *Processing the submitted form:*

    Each checkbox column is represented by one request parameter.  Its name is
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
            return super(CheckRowsForm, self)._export_cell(context, row, n, field,
                                                           editable=editable)
        

class EditableBrowseForm(BrowseForm):
    """Web BrowseForm with editable fields in certain columns.

    The form is rendered as an ordinary table, but columns given by constructor
    argument 'editable_columns'.

    are rendered as editable fields in each row.  The form has no submit
    controls -- it must be used inside another submittable form.

    Editable fields use row key in field identifier to allow processing the
    form values on submit.  So for example, The value of column 'count' for the
    row with exported key value '654' will be submitted as parameter
    'count-654'.

    """
    _EXPORT_EMPTY_TABLE = True
    
    def __init__(self, view, req, row, editable_columns=None, set_row_callback=None,
                 allow_insertion=False, extra_rows=0, **kwargs):
        """Arguments:

            editable_columns -- a sequence of column identifiers whoose fields
              will be editable.
            set_row_callback -- callback function called on each form row
              initialization.  Function of one argument - PresentedRow instance.
            allow_insertion -- boolean flag indicating whether insertion of new
              table rows is allowed.
            extra_rows -- number of extra rows to insert into the form
              (allow_insertion must be True)

          See the parent classes for definition of the remaining arguments.
          The arguments 'limits' and 'limit' are ignored.

        """
        assert isinstance(editable_columns, (list, tuple)), editable_columns
        assert set_row_callback is None or isinstance(set_row_callback, collections.Callable), \
            set_row_callback
        self._editable_columns = editable_columns
        self._set_row_callback = set_row_callback
        self._valid_rows = ()
        self._removed_rows = []
        self._allow_insertion = allow_insertion
        self._extra_rows = extra_rows
        super(EditableBrowseForm, self).__init__(view, req, row,
                                                 **dict(kwargs, limits=(), limit=None))
        if __debug__:
            for cid in editable_columns:
                assert cid in self._row, cid

    def _export_cell(self, context, row, n, field, editable=False):
        if field.id in self._editable_columns:
            editable = True
        result = super(EditableBrowseForm, self)._export_cell(context, row, n, field,
                                                              editable=editable)
        if field.id == self._column_fields[0].id:
            g = context.generator()
            icon = context.resource('delete-record.png')
            result = (g.a(g.img(src=context.uri(icon), alt=_("Remove this row")),
                          href='javascript:void(0)',
                          title=_("Remove this row"), cls='remove-row') +
                      result)
        return result

    def _row_attr(self, row, n):
        attr = super(EditableBrowseForm, self)._row_attr(row, n)
        if row[self._key].value() is not None:
            key = row[self._key].export()
        else:
            key = 'pytis-inserted-row-%d' % row.inserted_row_number
        attr['data-pytis-row-key'] = key
        return attr

    def _set_row(self, row):
        super(EditableBrowseForm, self)._set_row(row)
        if row is None and not self._row.new():
            # Apply default values manually here.  This hack is needed, because
            # the internal PresentedRow instance is not set as new and thus it
            # doesn't supply default values automatically, but is actually is
            # a new row when 'row' is None.
            for field in self._fields.values():
                # Don't apply default value to the key column because it is used to distinguish
                # inserted rows from edited rows (maybe that's wrong).
                if self._row[field.id].value() is None and field.id != self._key:
                    default = field.spec.default()
                    # Exclude serial fields to avoid wasting sequences here.
                    if default is not None and not isinstance(field.type, pd.Serial):
                        if isinstance(default, collections.Callable):
                            try:
                                default = default(transaction=self._row.transaction())
                            except TypeError:
                                default = default()
                        self._row[field.id] = pd.Value(field.type, default)
                    else:
                        self._row[field.id] = field.type.default_value()
        if self._set_row_callback:
            self._set_row_callback(self._row)
        if self._req.param('submit') and not self._req.param('_pytis_insert_new_row'):
            # If we are displaying a submitted form (after validation failed in
            # 'validate()'), we need to revalidate editable fields in each row
            # to be able to display the form with the invalid user values
            # and validation error messages within the fields.
            locale_data = self._req.localizer().locale_data()
            for cid in self._editable_columns:
                field = self._fields[cid]
                field.validate(self._req, locale_data)

    def _field(self, id, multirow=False):
        if id in self._editable_columns:
            multirow = True
        return super(EditableBrowseForm, self)._field(id, multirow=multirow)

    def _export_field(self, context, field, editable=False):
        result = super(EditableBrowseForm, self)._export_field(context, field, editable=editable)
        if editable:
            error = self._row.validation_error(field.id)
            if error:
                result += context.generator().div(error.message(), cls='validation-error')
        return result

    def _export_javascript(self, context):
        g = context.generator()
        uri = self._uri_provider(None, UriType.LINK, None)
        return g.js_call("new pytis.BrowseForm",
                         self._form_id, self._name, uri, self._allow_insertion) + ';'

    def _removed_keys(self):
        param = '_pytis_removed_row_key_' + self._name
        if self._req.has_param(param):
            removed_keys = pytis.util.xtuple(self._req.param(param))
        else:
            removed_keys = ()
        return removed_keys
            
    def _table_rows(self):
        rows = super(EditableBrowseForm, self)._table_rows()
        self._row.inserted_row_number = None
        removed_keys = self._removed_keys()
        self._removed_rows = []
        def g():
            for row in rows:
                if row[self._key].export() in removed_keys:
                    self._removed_rows.append(row)
                else:
                    yield row
            for i in range(self._extra_rows):
                self._row.inserted_row_number = i
                if 'pytis-inserted-row-%d' % i not in removed_keys:
                    yield None
        return g()

    def _wrap_exported_rows(self, context, rows, page, pages):
        g = context.generator()
        hidden = g.hidden('_pytis_inserted_rows_' + self._name, self._extra_rows)
        for row_key in self._removed_keys():
            hidden += g.hidden('_pytis_removed_row_key_' + self._name, row_key)
        return super(EditableBrowseForm, self)._wrap_exported_rows(context, rows,
                                                                   page, pages) + hidden

    def _export_summary(self, *args, **kwargs):
        if self._allow_insertion:
            return ''
        else:
            return super(EditableBrowseForm, self)._export_summary(*args, **kwargs)

    def ajax_response(self, req):
        """Return the AJAX request response as a JSON encoded data structure.

        Same rules for the returned value apply as in 'EditForm.ajax_response()'.

        """
        try:
            self._row.inserted_row_number = int(req.param('_pytis_inserted_rows_' + self._name))
        except (TypeError, ValueError):
            self._row.inserted_row_number = 0
        self._set_row(None)
        def export_row(context):
            self._group = True
            self._last_group = None
            return self._export_row(context, self._row, 0, None)
        return Exporter(export_row)

    def validate(self, req):
        """Validate the submitted form and return True if no errors are found.

        Returns False if validation of at least one field fails.  If all form
        fields are valid, their values are saved and can be obtained later by
        the method 'rows()'.

        """
        data = self._row.data()
        try:
            self._extra_rows = int(req.param('_pytis_inserted_rows_' + self._name))
        except (TypeError, ValueError):
            pass
        locale_data = req.localizer().locale_data()
        column_ids = [c.id() for c in data.columns()]
        for cid in self._editable_columns:
            if cid not in column_ids:
                column_ids.append(cid)
        rows = []
        valid = True
        for row in self._table_rows():
            self._set_row(row)
            for cid in self._editable_columns:
                field = self._fields[cid]
                error = field.validate(req, locale_data)
                if error:
                    valid = False
            rows.append(pd.Row([(cid, self._row[cid]) for cid in column_ids]))
        self._valid_rows = tuple(rows)
        return valid

    def valid_rows(self):
        """Return validated form data as a tuple of 'pytis.data.Row' instances.

        Returns a tuple of rows, where each row contains all non-virtual
        columns plus also values of all editable virtual columns.  'validate()'
        must be called before.  Invalid field values (when 'validate()'
        returned false) will not be present in the returned data.

        """
        return self._valid_rows

    def removed_rows(self):
        """Return all rows removed from the form as a tuple of 'pytis.data.Row' instances.

        'validate()' must be called before.

        """
        return tuple(self._removed_rows)
