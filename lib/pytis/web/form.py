# -*- coding: utf-8 -*-
#
# Copyright (C) 2006-2011 Brailcom, o.p.s.
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

import string

from pytis.web import *

_ = lcg.TranslatableTextFactory('pytis')

class Type(pd.Type):
    def _validation_error(self, id, **kwargs):
        message = self._validation_messages[id]
        if kwargs:
            message = _(message, **kwargs)
        return pd.ValidationError(message)
    # Needed to postpone variable interpolation to translation time.
    pd.Type._validation_error = _validation_error


class Form(lcg.Content):
    _HTTP_METHOD = 'POST'
    _CSS_CLS = None
    def __init__(self, view, row, handler='#', prefill=None, hidden=(), name=None,
                 uri_provider=None, **kwargs):
        """Arguments:

          view -- presentation specification as a 'pytis.presentation.ViewSpec'
            instance.
          row -- 'pytis.presentation.PresentedRow' instance.
          handler -- form handler URI as a string.  This URI is used in the
            form's 'action' attribute.
          prefill -- form prefill data as a dictionary of string values.
          uri_provider -- callable object (function) returning URIs for form
            fields.  This makes Pytis web forms independent on the
            application's URI scheme.  The function must accept one positional
            argument (the 'pytis.presentation.PresentedRow' instance) and two
            keyword arguments.  The first of them -- 'cid' -- may also be used
            as positional and denotes the identifier of the field, for which
            the URI is requested.  It may be None when requesting URI for the
            whole record.  The later argument 'type' will always be one of
            'UriType' constants.  It is used for distinction of the purpose,
            for which the uri us used (eg. for a link or an image src).  The
            return value may be None for fields which don't link anywhere,
            string URI if the field links to that URI or a 'pytis.web.Link'
            instance if it is necessary to specify also some extended link
            attributes, such as title (tooltip text) or target (such as
            _blank).  For array fields (when 'cid' belongs to a field of type
            'pytis.data.Array'), the return value must be a function of one
            argument -- the internal python value of the field's inner type.
            The function will return an URI or Link instance as above for given
            array value.
          hidden -- hardcoded hidden form fields as a sequence of pairs (name,
            value).
          name -- form name as a string or None.  This name will be sent as a
            hidden field and used to distinguish which request parameters
            belong to which form.

        """
        super(Form, self).__init__(**kwargs)
        assert isinstance(view, ViewSpec), view
        assert isinstance(row, pytis.presentation.PresentedRow), row
        assert isinstance(handler, (str, unicode)), handler
        assert isinstance(hidden, (tuple, list)), hidden
        self._view = view
        self._row = row
        self._key = row.data().key()[0].id()
        self._handler = handler
        self._prefill = prefill or {}
        self._uri_provider = uri_provider
        self._hidden = list(hidden)
        self._name = name
        self._enctype = None
        self._id = 'x%x' % positive_id(self)

    def _export_body(self, context):
        pass
    
    def _export_submit(self, context):
        pass
    
    def _export_footer(self, context):
        pass
    
    def export(self, context):
        g = context.generator()
        content = [self._export_body(context)] + \
                  [g.div(part, cls=name) for part, name in
                   (self._export_submit(context), 'submit'),
                   (self._export_footer(context), 'footer') if part]
        cls = 'pytis-form ' + self._CSS_CLS
        if self._name:
            cls += ' ' + camel_case_to_lower(self._name, '-')
        return g.form(content, action=g.uri(self._handler), method=self._HTTP_METHOD, cls=cls,
                      id=self._id, enctype=self._enctype)

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
        
    def _field(self, id):
        return Field(self._row, self._view.field(id), self._row.type(id), self, self._uri_provider)
        
    def _export_field(self, context, field, editable=False):
        if editable:
            result = field.exporter.editor(context, prefill=self._prefill.get(field.id),
                                           error=dict(self._errors).get(field.id))
        else:
            formatted = field.exporter.format(context)
            if isinstance(field.type, pd.StructuredText):
                if field.spec.printable():
                    uri = self._uri_provider(self._row, field.id, type=UriType.PRINT)
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
            result = wrap(formatted, cls='field id-'+field.id)
        return result

    def _interpolate(self, context, template, row):
        if callable(template):
            template = template(row)
        result = template.interpolate(lambda fid: self._export_field(context, self._fields[fid]))
        # Translation is called immediately to force immediate interpolation
        # (with the current row data).  Delayed translation (which invokes the
        # interpolation) would use invalid row data (the 'PresentedRow'
        # instance is reused and filled with table data row by row).
        return context.translate(result)
    

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
            
    def __init__(self, view, row, layout=None, **kwargs):
        assert layout is None or isinstance(layout, GroupSpec)
        self._layout = layout
        super(LayoutForm, self).__init__(view, row, **kwargs)
        
    def _export_group(self, context, group, inner=False, id=None, omit_first_field_label=False):
        g = context.generator()
        content = self._GroupContent()
        subgroup_number = 0
        for i, item in enumerate(group.items()):
            if callable(item):
                item = item(self._row)
            if isinstance(item, (str, unicode)):
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
                    # Codebook field display is not numeric even though the underlying type is...
                    right_aligned = (self._ALIGN_NUMERIC_FIELDS and not field.type.enumerator()
                                     and isinstance(field.type, pytis.data.Number))
                    content.append(ctrl, label=label, right_aligned=right_aligned,
                                   needs_panel=True, fullsize=False)
            elif isinstance(item, GroupSpec):
                subgroup_number += 1
                subgroup_id = '%s-%d' % (id or 'group', subgroup_number)
                label = None
                if group.orientation() == Orientation.VERTICAL \
                        and item.orientation() == Orientation.HORIZONTAL \
                        and isinstance(item.items()[0], str):
                    field = self._fields[item.items()[0]]
                    # Nested horizontal group which starts with a labeled field will be aligned
                    # within the current vertical group if possible.
                    if not field.spec.compact():
                        label = self._export_field_label(context, field)
                content.append(self._export_group(context, item, inner=True, id=subgroup_id,
                                                  omit_first_field_label=label is not None),
                               label=label, fullsize=label is None)
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
            result = [g.table([g.tr(cells)], cellspacing=0, cellpadding=0,
                              cls='horizontal-group' + (not omit_first_field_label
                                                        and ' expanded' or ''))]
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
                result = [g.table(rows, cls='vertical-group')]
            else:
                result = [item[1] for item in content.content()]
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

            

    

    def _export_field_label(self, context, field):
        if field.label:
            return context.generator().label(field.label, None) + ":"
        else:
            return None
    
    def _export_field_help(self, context, field):
        return None


class _SingleRecordForm(LayoutForm):

    def __init__(self, view, row, layout=None, **kwargs):
        layout = layout or view.layout().group()
        super(_SingleRecordForm, self).__init__(view, row, layout=layout, **kwargs)
        
    def _export_body(self, context):
        return self._export_group(context, self._layout)
    
    
class _SubmittableForm(Form):
    """Mix-in class for forms with submit buttons."""

    def __init__(self, view, row, submit=_("Submit"), reset=_("Undo all changes"), **kwargs):
        """Arguments:

          submit -- custom submit buttons as a sequence of (label, name) pairs.
            A single unnamed button can be passed as just the label string.
            Default is one button labeled `Submit'.
          reset -- reset button label as a string or None to omit the reset
            button.
            
          See the parent classes for definition of the remaining arguments.

        """
        if not isinstance(submit, (list, tuple)):
            submit = ((submit or _("Submit"), None),)
        self._submit = submit
        self._reset = reset
        super(_SubmittableForm, self).__init__(view, row, **kwargs)
    
    def _export_submit(self, context):
        g = context.generator()
        result = [g.hidden(k, v) for k, v in self._hidden] + \
                 [g.hidden('form_name', self._name)] + \
                 [g.button(g.span(label), name=name, value='1', title=_("Submit the form"))
                  for label, name in self._submit]
        if self._reset:
            result.append(g.button(g.span(_("Reset")), type='reset', title=self._reset))
        return result


class ShowForm(_SingleRecordForm):
    _CSS_CLS = 'show-form'
    _ALIGN_NUMERIC_FIELDS = True
    
    
class EditForm(_SingleRecordForm, _SubmittableForm):
    _CSS_CLS = 'edit-form'
    _EDITABLE = True
    
    def __init__(self, view, row, errors=(), **kwargs):
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

          See the parent classes for definition of the remaining arguments.

        """
        super(EditForm, self).__init__(view, row, **kwargs)
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
        binary = [id for id in order if isinstance(self._row.type(id), pytis.data.Binary)]
        self._enctype = (binary and 'multipart/form-data' or None)

    def _has_not_null_indicator(self, field):
        type = field.type
        return type.not_null() and not isinstance(type, pytis.data.Boolean) and \
               (self._row.new() or not isinstance(type, (pytis.data.Password, pytis.data.Binary)))
    
    def _export_field_label(self, context, field):
        g = context.generator()
        if not field.label:
            return None
        if self._has_not_null_indicator(field):
            sign = g.sup("*", cls="not-null")
        else:
            sign = ''
        return g.label(field.label, field.unique_id) + sign + ":"
        
    def _export_field_help(self, context, field):
        descr = field.spec.descr()
        if descr:
            g = context.generator()
            id = field.unique_id + '-help'
            # Set through a script to avoid invalid HTML ('aria-describedby' in not valid HTML).
            script = "document.getElementById('%s').setAttribute('aria-describedby', '%s');"
            return g.div(descr, id=id, cls="help") + g.script(script % (field.unique_id, id))
        else:
            return None

    def _export_errors(self, context):
        g = context.generator()
        errors = []
        for id, msg in self._errors:
            if id is not None:
                f = self._view.field(id)
                msg = g.strong(f and f.label() or id) + ": " + msg
            errors.append(g.p(msg))
        if errors:
            return g.div(errors, cls='errors')
        else:
            return None
    
    def _export_body(self, context):
        errors = self._export_errors(context)
        body = super(EditForm, self)._export_body(context)
        if errors:
            return concat(errors, body)
        else:
            return body
    
    def _export_footer(self, context):
        for f in self._fields.values():
            if f.label and self._has_not_null_indicator(f) and f.id in self._layout.order():
                g = context.generator()
                return g.span("*", cls="not-null") +") "+\
                       _("Fields marked by an asterisk are mandatory.")
        return None

    @classmethod
    def _op2str(self, operator):
        # Return a unique string representation of 'pytis.data.Operator' for
        # storing current runtime filter state during AJAX updates.  The
        # default 'Operator' __str__ method doesn't work for this purpose since
        # it doesnt expose argument values.
        def arg2str(arg):
            if isinstance(arg, pd.Operator):
                return self._op2str(arg)
            elif isinstance(arg, (pd.Value, pd.WMValue)):
                value = arg.value()
                if isinstance(arg.type(), pd.DateTime):
                    value = str(value)
                return repr(value)
            else:
                return repr(arg)
        if operator is None:
            return ''
        else:
            return '%s(%s)' % (operator.name(), ','.join([arg2str(a) for a in operator.args()]))
    
    def export(self, context):
        result = super(EditForm, self).export(context)
        layout_fields = self._layout.order()
        field_handlers = []
        filters = {}
        g = context.generator()
        for fid in layout_fields:
            field = self._fields[fid]
            active = self._row.depends(fid, layout_fields)
            required = self._has_not_null_indicator(field)
            runtime_filter = self._row.runtime_filter(fid)
            if runtime_filter is not None:
                filters[fid] = self._op2str(runtime_filter)
            handler = field.exporter.handler(context, self._id, active, required)
            field_handlers.append(handler)
        context.resource('prototype.js')
        context.resource('pytis.js')
        result += g.script("new pytis.FormHandler('%s', [%s], %s)" %
                           (self._id, ', '.join(field_handlers), g.js_value(filters)))
        return result

    @classmethod
    def ajax_response(cls, req, row, layout, errors, translator, uri_provider=None):
        """Return the AJAX request response as a JSON encoded data structure.

        Arguments:
          req -- AJAX request object as an instance of class implementing the
            pytis 'Request' API.
          row -- edited form record as a 'PresentedRow' instance.
          layout -- edited form layout as a 'GroupSpec' instance.
          errors -- form data validation result as a sequence of pairs
            (field_id, error_message).
          translator -- 'lcg.Translator' instance used for localization of
            computed field values (such as dates, numbers, etc).
          uri_provider -- URI provider function same as in the Form constructor
            argument of the same name.

        This method acts as the server side counter-part of the client side
        code defined in the pytis form JavaScript code in 'pytis.js'.  The
        returned string is supposed to be sent back to the client within the
        response body with the 'application/json' content type set in HTTP
        response headers.  That way the client side code will be able to
        process it.
        
        """
        try:
            import json
        except:
            import simplejson as json
        request_number = req.param('_pytis_form_update_request')
        changed_field = str(req.param('_pytis_form_changed_field'))
        filters = json.loads(req.param('_pytis_form_filter_state'))
        fields = {}
        computed_fields = [f.id() for f in row.fields() if f.computer() is not None]
        for fid in layout.order():
            fields[fid] = fdata = {}
            if fid != changed_field:
                fdata['editable'] = row.editable(fid)
                if fid in computed_fields and row.invalid_string(fid) is None:
                    value = row[fid]
                    if isinstance(value.type(), pd.DateTime):
                        exported_value = localizable_datetime(value)
                    else:
                        exported_value = value.export()
                    localized_value = translator.translate(exported_value)
                    # Values of disabled fields are not in the request, so send them always...
                    if not req.has_param(fid) or localized_value != req.param(fid):
                        fdata['value'] = localized_value
                if filters.has_key(fid):
                    old_filter = filters[fid]
                    new_filter = cls._op2str(row.runtime_filter(fid))
                    if new_filter != old_filter:
                        enumeration = [(value, translator.translate(label))
                                       for value, label in row.enumerate(fid)]
                        fdata['filter'] = new_filter
                        fdata['enumeration'] = enumeration
                        if uri_provider and isinstance(row.type(fid), pd.Array):
                            func = uri_provider(row, fid, type=UriType.LINK)
                            def link(value):
                                lnk = func(value)
                                if isinstance(lnk, Link):
                                    return dict(href=lnk.uri(),
                                                title=translator.translate(lnk.title()),
                                                target=lnk.target())
                                else:
                                    return dict(href=lnk)
                            if func:
                                fdata['links'] = dict([(value, link(value))
                                                       for (value, display) in enumeration])
        for fid, error in errors:
            if fields.has_key(fid):
                fields[fid]['error'] = error
        return json.dumps(dict(request_number=request_number, fields=fields))

    
class FilterForm(EditForm):
    """Simple form for displaying a list of fields for advanced filtering."""
    # This form is currently only used in Wiking Biblio CatalogNews module.
    # The whole thing needs some further work to be generally usable ...
    _CSS_CLS = 'edit-form filter-form'
    
    def __init__(self, fields, row, **kwargs):
        view = ViewSpec(_("Filter"), fields)
        kwargs['reset'] = kwargs.get('reset') # Default to None in this class.
        kwargs['submit'] = kwargs.get('submit', _("Apply Filter"))
        super(FilterForm, self).__init__(view, row, **kwargs)
        
    def _export_footer(self, context):
        return None

    
class BrowseForm(LayoutForm):
    _CSS_CLS = 'browse-form'
    _HTTP_METHOD = 'GET'
    _SORTING_DIRECTIONS = {pytis.data.ASCENDENT: 'asc',
                           pytis.data.DESCENDANT: 'desc'}
    _NULL_FILTER_ID = '-'

    def __init__(self, view, row, req=None, uri_provider=None, condition=None, arguments=None,
                 columns=None, sorting=None, grouping=None,
                 limits=(25, 50, 100, 200, 500), limit=50, offset=0,
                 search=None, query=None, allow_query_search=None, filter=None, message=None, 
                 filter_sets=None, profiles=None, filter_fields=None, immediate_filters=True,
                 **kwargs):
        """Arguments:

          req -- instance of a class implementing the 'Request' API.  The form
            state (sorting, paging etc) will be set up according to the reqest
            parameters, if the request includes them (form controls were used
            to submit the form).  The constructor argument 'name' (defined in
            parent class) may be used to distinguish between multiple forms on
            one page.  If this parameter was passed, it is sent as the request
            argument 'form_name'.  Thus if this argument doesn't match the form
            name, the request arguments are ignored.
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
            request parameter 'limit' overrides this value if 'req' is passed
            and it is stored as a cookie.  The cookie has lower precedence than
            the request parameter, but still higher than the 'limit'
            constructor argument, so this argument really only serves as a
            default value.  The request parameter/cookie is checked against the
            'limits' argument (see below) and if it is not one of the values
            defined there, it is ignored, so the user may only use one of the
            allowed limits.
          limits -- a sequence of available 'limit' values.  These values are
            used to create the limit selection control and also determine valid
            values of the 'limit' request parameter and cookie (described
            above).
          offset -- determines the page within paged listing.  The number
            indicates the offset of the record within all the records of the
            current select.  The page, which contains this record will be
            displayed if possible.  If not (the listing is shorter than given
            number), the nearest page is displayed.  The request argument
            'offset' overrides this value if 'req' is passed.  Also request
            arguments 'next' and 'prev' modify this value.
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
          profiles -- specification of form profiles as a 'Profiles' instance.
            These profiles will be available in the user interface for user's
            selection.  If None, the default set of profiles defined by
            specification is used.  If not None, the profiles from
            specification are ignored.  This argument is mostly useful to
            construct the list of profiles dynamically (specification profiles
            are static).  
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

        See the parent classes for definition of the remaining arguments.

        """
        if uri_provider:
            def browse_form_uri_provider(row, cid=None, type=UriType.LINK):
                if cid == self._columns[0] and type==UriType.LINK:
                    uri = uri_provider(row, None, type=type)
                    if uri is not None:
                        return uri
                return uri_provider(row, cid, type=type)
        else:
            browse_form_uri_provider = None
        super(BrowseForm, self).__init__(view, row, uri_provider=browse_form_uri_provider, **kwargs)
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
        if profiles:
            assert 'profile' not in [fs.id() for fs in filter_sets]
            # Add profile selection as another filter set, since the user interface is the same.
            filter_sets.append(FilterSet('profile', _("Filter"),
                                         [Filter(p.id(), p.name(), p.filter()) for p in profiles],
                                         default=profiles.default()))
        self._profiles = profiles
        self._filter_ids = {}
        self._filter_sets = filter_sets
        for i, filter_set in enumerate(filter_sets):
            filter_set_id = filter_set.id()
            # Determine the current set of user selectable filters.
            null_filter = find(None, filter_set, key=lambda f: f.condition())
            if not null_filter:
                # Translators: Label used in filter selection box for the
                # option which disables filtering and thus results in all
                # records to be displayed.
                null_filter = Filter(self._NULL_FILTER_ID, _("All items"), None)
                filter_set_filters = [null_filter] + [f for f in filter_set]
                filter_sets[i] = FilterSet(filter_set_id, filter_set.title(), filter_set_filters)
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
                    if filter and cond:
                        filter = pd.AND(filter, cond)
                    elif cond:
                        filter = cond
                else:
                    filter_id = None
            self._filter_ids[filter_set_id] = filter_id
        if profiles:
            profile_id = self._filter_ids['profile']
            profile = find(profile_id, profiles, key=lambda p: p.id())
            if profile.columns() is not None:
                columns = profile.columns()
            if profile.sorting() is not None:
                sorting = profile.sorting()
            if profile.grouping() is not None:
                grouping = profile.grouping()
        self._filter = filter
        # Determine the current sorting.
        self._user_sorting = None
        sorting_column, direction = param('sort', str), param('dir', str)
        if sorting_column and direction and self._row.data().find_column(sorting_column):
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
            if cookie and isinstance(cookie, (str, unicode)) and cookie.isdigit():
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
                type = self._row.data().find_column(self._key).type()
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
        self._column_fields = cfields = [self._fields[cid] for cid in self._columns]
        self._align = dict([(f.id, 'right') for f in cfields
                            if not f.type.enumerator() and isinstance(f.type, pd.Number)])
        self._custom_message = message
        # Hack allowing locale dependent index search controls.  The method
        # 'prefered_language()' is Wiking specific so it is not correct to rely
        # on it here.
        try:
            self._lang = str(req.prefered_language())
        except:
            self._lang = None
        self._init_filter_fields(req, filter_fields or ())
        self._immediate_filters = immediate_filters

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
                    locale_data = lcg.GettextTranslator(self._lang, fallback=True).locale_data()
                    if isinstance(ftype, pd.Date):
                        format = locale_data.date_format
                    elif isinstance(ftype, pd.Time):
                        format = locale_data.exact_time_format
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
            filter_fields = [Field(row, f, row.type(f.id()), self, self._uri_provider)
                             for f in fields]
        else:
            row = None
            condition = None
            filter_fields = []
        # TODO: self._errors doesn't really belong here -- it belongs to
        # Editform, but it is used within _export_field() so we need to
        # initialize it here or (better) fix _export_field().
        self._errors = errors
        self._filter_fields = filter_fields
        self._filter_fields_condition = condition
        self._filter_fields_arguments = arguments
        self._filter_fields_row = row

    def _export_cell(self, context, field):
        value = self._export_field(context, field)
        if field.id == self._column_fields[0].id and self._tree_order_column:
            order = self._row[self._tree_order_column].value()
            if order is not None:
                # Strip, since LTree values look like '0.2.1', but TreeOrder like '.0.2.1'
                level = len(order.strip('.').split('.')) - 1
                if level > 0:
                    g = context.generator()
                    indent = level * g.span(2*'&nbsp;', cls='tree-indent')
                    value = indent + '&bull;&nbsp;'+ g.span(value, cls='tree-node')
                    # &#8227 does not work in MSIE
        return value

    def _style(self, style, row, n, field=None):
        def color(c):
            if type(c) is tuple:
                return '#%02x%02x%02x' % c
            else:
                return c
        if callable(style):
            style = style(row)
        cls = []
        if field is None: # For row style only
            cls.append(n % 2 and 'even' or 'odd')
            if self._group != self._last_group:
                cls.append('group-start')
                if n != 0:
                    cls.append('group-change')
            cls.append(self._group and 'even-group' or 'odd-group')
        #else:
        #    cls.append('id-'+field.id)
        if style is None:
            return cls and dict(cls=' '.join(cls)) or {}
        elif style.name() is not None:
            cls.append(style.name())
            return dict(cls=' '.join(cls))
        styles = [name +': '+ f(attr()) for attr, name, f in (
            (style.foreground, 'color',            color),
            (style.background, 'background-color', color),
            (style.slanted,    'font-style',       lambda x: x and 'italic' or 'normal'),
            (style.bold,       'font-weight',      lambda x: x and 'bold' or 'normal'),
            (style.overstrike, 'text-decoration',  lambda x: x and 'line-through' or 'none'),
            (style.underline,  'text-decoration',  lambda x: x and 'underline' or 'none'),
            ) if attr() is not None]
        return dict(cls=' '.join(cls), style='; '.join(styles))
    
    def _export_row(self, context, row, n, id):
        g = context.generator()
        cells = [g.td(self._export_cell(context, field), align=self._align.get(field.id),
                      **self._style(field.style, row, n, field))
                 for field in self._column_fields]
        return g.tr(cells, id=id, **self._style(self._view.row_style(), row, n))

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
        cells = [i==0 and \
                 g.th(label+':', scope='row') or \
                 g.td(export_aggregation_value(data, op, field), cls='id-'+field.id)
                 for i, field in enumerate(self._column_fields)]
        return g.tr(cells, cls='aggregation-results '+agg_id)
    
    def _export_group_heading(self, context):
        group_heading = self._view.group_heading()
        if group_heading is None:
            return None
        if isinstance(group_heading, lcg.TranslatableText):
            heading = self._interpolate(context, group_heading, self._row)
        else:
            field = self._fields[group_heading]
            heading = self._export_field(context, field)
        g = context.generator()
        return g.tr(g.th(heading, colspan=len(self._column_fields)), cls='group-heading')
    
    def _export_headings(self, context):
        g = context.generator()
        current_sorting_column, current_dir = self._sorting[0]
        def label(field):
            result = field.column_label
            if not field.virtual:
                if field.id == current_sorting_column:
                    dir = current_dir
                else:
                    dir = None
                if dir in (None, pytis.data.DESCENDANT):
                    new_dir = pytis.data.ASCENDENT
                else:
                    new_dir = pytis.data.DESCENDANT
                result = g.link(result, self._link_ctrl_uri(g, sort=field.id,
                                                            dir=self._SORTING_DIRECTIONS[new_dir]))
                if dir:
                    # Characters u'\u25be' and u'\u25b4' won't display in MSIE...
                    sign = dir == pytis.data.ASCENDENT and '&darr;' or '&uarr;'
                    result += ' '+ g.span(sign, cls='sorting-sign')
            return result
        return concat([g.th(label(f)) for f in self._column_fields])

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

    def _wrap_exported_rows(self, context, rows, summary):
        g = context.generator()
        if self._page + 1 == self._pages:
            # Display aggregations on the last page.
            foot_rows = [self._export_aggregation(context, op)
                         for op in self._view.aggregations()]
        else:
            foot_rows = []
        foot_rows.append(g.tr(g.td(summary, colspan=len(self._column_fields))))
        return g.table((g.thead(g.tr(self._export_headings(context))),
                        g.tfoot(foot_rows),
                        g.tbody(rows)), border=1)
    
    def _export_body(self, context):
        data = self._row.data()
        row = self._row
        limit = self._limit
        exported_rows = []
        if self._arguments or self._filter_fields_arguments:
            arguments = self._arguments or {}
            arguments.update(dict(self._filter_fields_arguments))
        else:
            arguments = None
        self._count = count = data.select(condition=self._conditions(), sort=self._sorting,
                                          arguments=arguments)
        found = False
        offset = self._offset
        if self._search:
            dist = data.search(self._search)
            if dist:
                found = True
                offset = dist - 1
        if limit is not None:
            page = int(max(0, min(offset, count)) / limit)
            first_record_offset = page*limit
        else:
            page = 0
            first_record_offset = 0
        self._page = page
        if count == 0:
            pages = 0
        elif limit is None:
            pages = 1
        else:
            pages, modulo = divmod(count, min(limit, count))
            pages += modulo and 1 or 0
        self._pages = pages
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
                id = 'found-record'
            else:
                id = None
            exported_rows.append(self._export_row(context, row, n, id))
            self._last_group = self._group
            n += 1 
            if limit is not None and n >= limit:
                break
        data.close()
        g = context.generator()
        if n == 0:
            if self._message(0):
                # The message about search/filter results is already printed.
                return '' 
            else:
                # Translators: Used in empty list forms.  "Records" refers to
                # database records in the most generic senese possible.
                return g.strong(_("No records."))
        else:
            if limit is None or count <= self._limits[0]:
                summary = _("Total records:") +' '+ g.strong(str(count))
            else:
                # Translators: The variables '%(first)s', '%(last)s' and
                # '%(total)s' are replaced by the numbers corresponding to the
                # current listing range.
                summary = _("Displayed records %(first)s-%(last)s of total %(total)s",
                            first=g.strong(str(first_record_offset+1)),
                            last=g.strong(str(first_record_offset+n)),
                            total=g.strong(str(count)))
            return self._wrap_exported_rows(context, exported_rows, summary)

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
                # A more appropriate sloution would be to handle that on the database level.
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
                              and (values[i][-1].lower() in (u'á',u'è',u'ï',u'é',u'ì') \
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
            links = [g.link(v, self._link_ctrl_uri(g, index_search=v)+'#found-record',
                            # Translators: Index search controls link tooltip.
                            title=_('Skip to the first record beginning with "%s"', v))
                     for v in values]
            result.append(g.div(label +' '+ concat(links, separator=' ')))
        return (g.div(result, cls='index-search-controls'),)

    def _export_controls(self, context, bottom=False):
        limit, page, count, pages = self._limit, self._page, self._count, self._pages
        g = context.generator()
        id = (bottom and '0' or '1') + self._id
        content = []
        # Construct a list of filters for export.
        show_filters = (self._filter_sets and (count or
                                               [v for v in self._filter_ids.values()
                                                if v is not None])
                        or self._filter_fields)
        show_query_field = self._show_query_field
        if not bottom:
            msg = self._message(count)
            if msg:
                content.append(g.div(msg, cls='results'))
        if show_query_field and not bottom:
            query_id = 'filter-' + id
            content.append(g.div((g.label(_("Search expression") +': ', query_id),
                                  g.field(self._query, name='query', id=query_id,
                                          cls='query-field'),
                                  g.hidden('show_query_field', '1'),
                                  # Translators: Search button label.
                                  g.button(g.span(_("Search")), cls='search-button')),
                                 cls='query' + (show_filters and ' with-filter' or '')))
        if show_filters and not bottom:
            # Translators: Button for manual filter invocation.
            submit_button = g.button(g.span(_("Change filters")), cls='apply-filters')
            if self._immediate_filters and not self._filter_fields:
                onchange = 'this.form.submit(); return true'
                # Leave the submit button in place for non-Javascript browsers.
                submit_button = g.noscript(submit_button)
            else:
                onchange = None
            filter_content = []
            for field in self._filter_fields:
                filter_content.extend((
                        g.label(field.label+':', field.unique_id),
                        self._export_field(context, field, editable=True)))
            for filter_set in self._filter_sets:
                filter_set_id = filter_set.id()
                filter_name = 'filter_%s' % (filter_set_id,)
                filter_id = filter_name + '-' + id
                filter_set_content = (
                    g.label(filter_set.title()+': ', filter_id),
                    g.select(name=filter_name, id=filter_id,
                             selected=self._filter_ids.get(filter_set_id),
                             # Translators: Label of filter selection box.
                             # Filtering limits the displayed form records by
                             # certain criterias.
                             title=(_("Filter")+' '+
                                    # Translators: Tooltip text suggesting
                                    # keyboard combination to use for selection
                                    # without unexpected invocation of the
                                    # option.
                                    _("(Use ALT+arrow down to select)")),
                             options=[(f.name(), f.id()) for f in filter_set],
                             onchange=onchange),
                    )
                filter_content.append(g.span(filter_set_content))
            filter_content.append(submit_button)
            content.append(g.div(filter_content, cls="filter"))
        if limit is not None and count > self._limits[0]:
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
                offset_id = 'offset-' + id
                if not show_query_field and self._allow_query_field:
                    search_button = g.button(g.span(_("Search")), name='show_query_field',
                                             value='1', cls='search-button')
                else:
                    search_button = None
                # Translators: Paging controls allow navigation in long lists which are split into
                # several pages.  The user can select a specific page or browse forward/backwards.
                controls += (g.span((g.label(_("Page")+': ', offset_id),
                                     g.select(name='offset', id=offset_id, selected=page*limit,
                                              title=(_("Page")+' '+
                                                     _("(Use ALT+arrow down to select)")),
                                              onchange='this.form.submit(); return true',
                                              options=[(str(i+1), i*limit) for i in range(pages)]),
                                     ' / ',
                                     g.strong(str(pages))), cls="offset"),
                             g.span((g.button(g.span(_("Previous")), name='prev', value='1',
                                              title=_("Go to previous page"), disabled=(page == 0),
                                              cls='prev-page-button'),
                                     g.button(g.span(_("Next")), name='next', value='1',
                                              title=_("Go to next page"),
                                              disabled=(page+1)*limit >= count,
                                              cls='next-page-button'),
                                     ) + (search_button and (search_button,) or ()),
                                    cls="buttons"))
            limit_id = 'limit-' + id
            controls += (g.span((g.label(_("Records per page")+':', limit_id)+' ',
                                 g.select(name='limit', id=limit_id, selected=limit,
                                          title=(_("Records per page")+' '+
                                                 _("(Use ALT+arrow down to select)")),
                                          onchange='this.form.submit(); return true',
                                          options=[(str(i), i) for i in self._limits])),
                                cls='limit'),
                         g.noscript(g.button(g.span(_("Go")), cls='goto-page-button')))
            if controls:
                cls = 'paging-controls' + (pages == 1 and ' one-page' or '')
                content.append(g.div(controls, cls=cls))
        if content:
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
        form = super(BrowseForm, self).export(context)
        content = (self._export_controls(context),
                   form,
                   self._export_controls(context, bottom=True))
        return concat([c for c in content if c], separator="\n")

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
            info = _("filtered by: ") + lcg.concat(filter_labels, separator=', ')
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
    layout can be customized through the 'list_view' argument within
    specification (see 'ViewSpec').  If 'list_view' is not defined, the form
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

    def __init__(self, view, row, **kwargs):
        self._list_layout = list_layout = view.list_layout()
        layout = list_layout and list_layout.layout() or None
        super(ListView, self).__init__(view, row, layout=layout, **kwargs)
        if list_layout is None:
            super_ = super(ListView, self)
            self._CSS_CLS = super_._CSS_CLS
            self._export_row = super_._export_row
            self._wrap_exported_rows = super_._wrap_exported_rows
            self._export_group_heading = super_._export_group_heading
        else:
            self._meta = [(self._field(id), id in list_layout.meta_labels())
                          for id in list_layout.meta()]
            self._image = list_layout.image() and self._field(list_layout.image())
            self._anchor = anchor = list_layout.anchor()
            if not anchor and list_layout.allow_index():
                self._anchor = camel_case_to_lower(self._name, '-') + '-%s'

    def _export_body(self, context):
        self._exported_row_index = []
        return super(ListView, self)._export_body(context)
        
    def _export_row(self, context, row, n, id):
        layout = self._list_layout
        g = context.generator()
        parser = lcg.Parser()
        if isinstance(layout.title(), lcg.TranslatableText):
            title = self._interpolate(context, layout.title(), row)
        else:
            title = self._row[layout.title()].export()
        anchor = self._anchor
        if anchor:
            anchor = anchor % row[self._key].export()
            heading = g.link(title, None, name=anchor)
        else:
            heading = title
        if layout.allow_index():
            self._exported_row_index.append(g.link(title, '#'+anchor))
        parts = [g.h(heading, level=3)]
        if layout.image():
            img = self._export_field(context, self._image) #cls='list-layout-image')
            if img:
                parts.append(img)
        if self._meta:
            meta = [(labeled and
                     g.span(field.label+":", cls='label id-'+ field.id)+" " or '') + \
                    self._export_field(context, field)
                    for field, labeled in self._meta]
            parts.append(g.div(concat(meta, separator=', '), cls='meta'))
        if layout.layout():
            parts.append(self._export_group(context, layout.layout()))
        for item in layout.content():
            if callable(item):
                content = item(self._row)
                if content is None:
                    continue
                cls = 'dynamic-content'
            elif self._row[item].value() is not None:
                if isinstance(self._row.type(item), pd.SimpleFormattedText):
                    content = lcg.HtmlContent(self._export_field(context, self._fields[item]))
                else:
                    text = self._row[item].export()
                    content = lcg.Container(parser.parse(text))
                cls = 'content id-'+ item
            else:
                continue
            content.set_parent(self.parent())
            # Hack: Add a fake container to force the heading level start at 4.
            container = lcg.Container(lcg.Section('', lcg.Section('', content, anchor=anchor)))
            parts.append(g.div(content.export(context), cls=cls))
        # We use only css class name from row_style, since other attributes are
        # BrowseForm specific.
        cls = self._style(self._view.row_style(), row, n)['cls']
        return g.div(parts, id=id, cls='list-item '+cls)

    def _export_group_heading(self, context):
        #return context.generator().h(self._export_field(context, field), 3, cls='group-heding')
        return None

    def _wrap_exported_rows(self, context, rows, summary):
        g = context.generator()
        result = ()
        if self._exported_row_index:
            result += (g.div(g.list(self._exported_row_index), cls="index"),)
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
    
    def __init__(self, view, row, columns=None, separator=', ', template=None, **kwargs):
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
        super(ItemizedView, self).__init__(view, row, columns=columns, **kwargs)
        assert isinstance(separator, basestring)
        assert template is None or isinstance(template, lcg.TranslatableText) or callable(template)
        self._separator = separator
        self._template = template
        
    def _export_row(self, context, row, n, id):
        template = self._template
        if template:
            return self._interpolate(context, template, row)
        else:
            fields = [self._export_field(context, field)
                      for field in self._column_fields if row[field.id].value() is not None]
            return concat(fields, separator=self._separator)

    def _export_group_heading(self, context):
        #TODO: Create multi-level lists.
        return None
    
    def _wrap_exported_rows(self, context, rows, summary):
        g = context.generator()
        return g.list(rows)


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
    def __init__(self, view, row, check_columns=None, limits=(), limit=None, **kwargs):
        """Arguments:

          check_columns -- a sequence of column identifiers for which the
            checkboxes will be created.  If the argument is omitted, checkboxes
            will automatically appear for all boolean columns.

          See the parent classes for definition of the remaining arguments.

        """
        super(CheckRowsForm, self).__init__(view, row, limits=limits, limit=limit, **kwargs)
        assert isinstance(check_columns, (list, tuple)), check_columns
        if __debug__:
            for cid in check_columns:
                assert self._row.has_key(cid), cid
        if check_columns is None:
            check_columns = tuple([field.id for field in self._column_fields
                                   if isinstance(field.type, pd.Boolean)])
        self._check_columns = check_columns

    def _export_cell(self, context, field):
        if field.id in self._check_columns:
            return context.generator().checkbox(name=field.id, value=self._row.format(self._key),
                                                checked=self._row[field.id].value())
        else:
            return super(CheckRowsForm, self)._export_cell(context, field)

