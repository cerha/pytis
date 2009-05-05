# -*- coding: iso-8859-2 -*-

# Copyright (C) 2006-2009 Brailcom, o.p.s.
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

This module provides an implementations of Pytis forms which can be used for web interfaces to
Pytis informations systems.  The intention is to be able to generate web forms from the same
specification as GUI forms.

Pytis currently does not include support running the actual web application.  Application framework
which makes use of pytis web forms is implemented separately.  See the Wiking project at
http://www.freebsoft.org/wiking for more information.

All the content generation is done using the LCG framework.  See http://www.freebsoft.org/lcg.

"""

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

          view -- presentation specification as a 'pytis.presentation.ViewSpec' instance.
          
          row -- 'pytis.presentation.PresentedRow' instance.
          
          handler -- form handler URI as a string.  This URI is used in the form's 'action'
            attribute.
          
          prefill -- form prefill data as a dictionary of string values.
          
          uri_provider -- callable object (function) returning URIs for form fields.  This makes
            Pytis web forms independent on the application's URI scheme.  The function must accept
            one positional argument (the 'pytis.presentation.PresentedRow' instance) and two
            keyword arguments.  The first of them -- 'cid' -- may also be used as positional and
            denotes the identifier of the field, for which the URI is requested.  It may be None
            when requesting URI for the whole record.  The later argument 'type' will always be one
            of 'UriType' constants.  It is used for distinction of the purpose, for which the uri
            us used (eg. for a link or an image src).
          
          hidden -- hardcoded hidden form fields as a sequence of pairs (name, value).

          name -- form name as a string or None.  This name will be sent as a hidden field and used
            to distinguish which request parameters belong to which form.

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


    def _export_body(self, context):
        pass
    
    def _export_submit(self, context):
        pass
    
    def _export_footer(self, context):
        pass
    
    def export(self, context):
        g = context.generator()
        content = [self._export_body(context)] + \
                  [wrap(part, cls=name) for wrap, part, name in
                   ((g.div, self._export_submit(context), 'submit'),
                    (g.div, self._export_footer(context), 'footer')) if part]
        cls = 'pytis-form ' + self._CSS_CLS
        if self._name:
            cls += ' ' + camel_case_to_lower(self._name, '-')
        return g.form(content, action=g.uri(self._handler), method=self._HTTP_METHOD, cls=cls,
                      enctype=self._enctype)


class FieldForm(Form):
    """Form with formattable fields."""
    
    def __init__(self, *args, **kwargs):
        super(FieldForm, self).__init__(*args, **kwargs)
        self._fields = dict([(f.id(), self._field(f.id())) for f in self._view.fields()])
        
    def _field(self, id):
        from field import _Field
        return _Field(self._view.field(id), self._row[id].type(), self, self._uri_provider)
        
    def _format_field(self, context, field):
        if isinstance(field.type, pd.StructuredText):
            wrap = context.generator().div
        else:
            wrap = context.generator().span
        return wrap(field.formatter.format(context, self._row, field), cls='field id-'+field.id)

    def _interpolate(self, context, template, row):
        if callable(template):
            template = template(row)
        result = template.interpolate(lambda fid: self._format_field(context, self._fields[fid]))
        # Translation is called immediately to force immediate interpolation (with the current row
        # data).  Delayed translation (which invokes the interpolation) would use invalid row data
        # (the 'PresentedRow' instance is reused and filled with table data row by row).
        return context.translate(result)
    

class LayoutForm(FieldForm):
    """Form with fields arranged according to pytis layout specification."""
    _MAXLEN = 100
    _ALIGN_NUMERIC_FIELDS = False

    def __init__(self, view, row, layout=None, allow_table_layout=True, **kwargs):
        assert layout is None or isinstance(layout, GroupSpec)
        self._layout = layout
        super(LayoutForm, self).__init__(view, row, **kwargs)
        self._allow_table_layout = allow_table_layout
        
    def _export_group(self, context, group, inner=False):
        g = context.generator()
        result = []
        fields = []
        wrap = False
        for item in group.items():
            if isinstance(item, Button):
                pass
            elif isinstance(item, lcg.Content):
                result.append(item.export(context))
            elif isinstance(item, GroupSpec):
                if fields:
                    result.append(self._export_fields(g, fields))
                fields = []
                result.append(self._export_group(context, item, inner=True))
            else:
                field = self._fields[item]
                ctrl = self._export_field(context, field)
                if ctrl is not None:
                    label = self._export_field_label(context, field)
                    help = self._export_field_help(context, field)
                    fields.append((field, label, ctrl, help))
                    wrap = True
        if fields:
            result.append(self._export_fields(g, fields))
        if group.orientation() == Orientation.HORIZONTAL:
            result = [g.table(g.tr([g.td(x, valign='top', cls=(i != 0 and 'spaced' or None))
                                    for i, x in enumerate(result)]),
                              cellspacing=0, cellpadding=0, cls='horizontal-group')]
            wrap = False
        if group.label():
            result = g.fieldset(group.label()+':', result, cls='group')
        elif wrap:
            result = g.div(result, cls='group')
        elif not inner:
            # This fieldset fixes MSIE display of top-level horizontal groups...
            result = g.fieldset(None, result, cls='outer')
        else:
            result = concat(result, separator="\n")
        return result

    def _export_packed_field(self, g, field, label, ctrl, help, single=False):
        if self._allow_table_layout:
            if help is not None:
                ctrl += help
            if field.spec.compact():
                if label:
                    label += g.br() +"\n"
                if single:
                    return label + ctrl
                td = g.td(label + ctrl, colspan=3)
            else:
                td = g.th(label or '', valign='top', cls='label', align='right')
                if self._ALIGN_NUMERIC_FIELDS and not field.type.enumerator() \
                       and isinstance(field.type, pytis.data.Number):
                    # Codebook field display is not numeric even though the underlying type is...
                    td += g.td(ctrl, cls='ctrl', valign='top', align='right') + \
                          g.td('', width='100%', cls='spacer')
                else:
                    td += g.td(ctrl, cls='ctrl', valign='top', width='100%', colspan=2)
            return g.tr(td)
        else:
            rows = (concat(label, g.br()), concat(ctrl, g.br()))
            if help:
                rows += (help,)
            return g.div(rows, cls="field")
        
    def _export_fields(self, g, fields):
        if len(fields) == 1 and fields[0][0].spec.compact():
            # Avoid unnecessary packing of a single compact field.
            field, label, ctrl, help = fields[0]
            return self._export_packed_field(g, field, label, ctrl, help, single=True)
        rows = [self._export_packed_field(g, field, label, ctrl, help)
                for field, label, ctrl, help in fields]
        if self._allow_table_layout:
            rows = g.table(rows, cls='packed-fields')
        return concat(rows, separator="\n")

    def _export_field(self, context, field):
        return None

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

          submit -- custom submit buttons as a sequence of (label, name) pairs.  A single unnamed
            button can be passed as just the label string.  Default is one button labeled `Submit'.

          reset -- reset button label as a string or None to omit the reset button.
            
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
                 [g.submit(label, name=name, title=_("Submit the form"))
                  for label, name in self._submit]
        if self._reset:
            result.append(g.reset(_("Reset", title=self._reset)))
        return result


class ShowForm(_SingleRecordForm):
    _CSS_CLS = 'show-form'
    _ALIGN_NUMERIC_FIELDS = True
    
    def _export_field(self, context, field):
        return self._format_field(context, field)

    
class EditForm(_SingleRecordForm, _SubmittableForm):
    _CSS_CLS = 'edit-form'
    
    def __init__(self, view, row, errors=(), **kwargs):
        """Arguments:

          errors -- a sequence of error messages to display within the form (results of previous
            attempt to commit the form).  The sequence consists of pairs (ID, MESSAGE), where ID is
            the field identifier and MESSAGE is the error message for given field.  ID can also be
            None for messages which don't belong to any particular field and it is also legal to
            pass field identifiers, which don't appear in the current form or even don't exist in
            the current specification (typically for fields which only appear in the underlying
            database objects).
            
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
        binary = [id for id in order if isinstance(self._row[id].type(), pytis.data.Binary)]
        self._enctype = (binary and 'multipart/form-data' or None)

    def _export_field(self, context, field):
        g = context.generator()
        value = self._row[field.id]
        type = value.type()
        attr = {'name': field.id,
                'id': field.unique_id}
        if isinstance(type, pytis.data.Boolean):
            ctrl = g.checkbox
            attr['value'] = 'T'
            attr['checked'] = value.value()
        elif isinstance(type, pytis.data.Binary):
            ctrl = g.upload
        elif type.enumerator() and field.selection_type in (SelectionType.CHOICE, None):
            ctrl = g.select
            enum = self._row.enumerate(field.id)
            attr['options'] = [("&nbsp;", "")] + \
                              [(g.escape(display).replace(' ',  '&nbsp;'), type.export(val))
                               for val, display in enum]
            if value.value() in [val for val, display in enum]:
                attr['selected'] = type.export(value.value())
        else:
            if field.spec.height() > 1:
                ctrl = g.textarea
                attr['rows'] = field.spec.height()
                attr['cols'] = width = field.spec.width()
                if width >= 80:
                    attr['cls'] = 'fullsize'
            else:
                if isinstance(type, pytis.data.String):
                    maxlen = type.maxlen()
                else:
                    maxlen = None
                ctrl = g.field
                attr['size'] = field.spec.width(maxlen)
                attr['maxlength'] = maxlen
            if isinstance(type, pytis.data.Password):
                attr['password'] = True
            else:
                attr['value'] = self._prefill.get(field.id) or value.export()
        if not self._row.editable(field.id):
            attr['disabled'] = True
            attr['name'] = None # w3m bug workaround (sends disabled fields)
        if field.id in [id for id, msg in self._errors]:
            attr['cls'] = (attr.has_key('cls') and attr['cls']+' ' or '') + 'invalid'
        result = ctrl(**attr)
        if isinstance(type, pytis.data.Password) and type.verify():
            attr['id'] = attr['id'] + '-verify-pasword'
            result += g.br() + ctrl(**attr)
        return result

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
        return descr and context.generator().div(descr, cls="help")

    def _export_body(self, context):
        g = context.generator()
        errors = []
        for id, msg in self._errors:
            if id is not None:
                f = self._view.field(id)
                msg = g.strong(f and f.label() or id) + ": " + msg
            errors.append(g.p(msg))
        if errors:
            errors = g.div(errors, cls='errors')
        return concat(errors, super(EditForm, self)._export_body(context))
                      
    def _export_footer(self, context):
        for f in self._fields.values():
            if f.label and self._has_not_null_indicator(f) and f.id in self._layout.order():
                g = context.generator()
                return g.span("*", cls="not-null") +") "+\
                       _("Fields marked by an asterisk are mandatory.")
        return None

    
class BrowseForm(LayoutForm):
    _CSS_CLS = 'browse-form'
    _HTTP_METHOD = 'GET'
    _SORTING_DIRECTIONS = ((pytis.data.ASCENDENT, 'asc'),
                           (pytis.data.DESCENDANT, 'desc'),
                           (None, 'none'))
    _NULL_FILTER_ID = '-'

    def __init__(self, view, row, columns=None, condition=None, sorting=None,
                 limits=(25, 50, 100, 200, 500), limit=50, offset=0, search=None, query=None,
                 filter=None, message=None, req=None, **kwargs):
        """Arguments:

          columns -- sequence of column identifiers to be displayed or None for the default columns
            defined by specification.
            
          condition -- current condition for filtering the records as 'pytis.data.Operator'
            instance or None.
            
          sorting -- form sorting specification in the format recognized by the 'sort' argument of
            'pytis.data.Data.select()'.
          
          limit -- maximal number of rows per page.  If the current condition produces more rows,
            the listing will be split into pages and the form will include controls for navigation
            between these pages.  None value results in an unlimited list -- all records will be
            printed on just one page and paging controls will be disabled.  The request argument
            'limit' overrides this value if 'req' is passed and the value is one of the valid
            'limits' (see below).

          limits -- a sequence of available 'limit' values.  These values are used to create the
            limit selection control and also determine valid values of the 'limit' request
            parameter.
            
          offset -- determines the page within paged listing.  The number indicates the offset of
            the record within all the records of the current select.  The page, which contains this
            record will be displayed if possible.  If not (the listing is shorter than given
            number), the nearest page is displayed.  The request argument 'offset' overrides this
            value if 'req' is passed.  Also request arguments 'next' and 'prev' modify this value.

          search -- search condition as a 'pytis.data.Operator' instance or None.  If used, the
            offset will be set automatically to ensure, that the first record matching the search
            condition will be displayed on the current page.  The request parameters 'search' or
            'index_search' can be also used to initialize the search condition (if this constructor
            argument is not used).  The request parameter 'search' is for searching by the value of
            the key column.  The request parameter 'index_search' is for searching by a prefix
            string and is always performed on the primary sorting column.  If no search condition
            is passed (either to the constructor or through the request), the offset is controlled
            by the 'offset' argument.

            Searching is ignored when the current limit is greater than the total number of
            records.

          query -- query search string.  If None, the form automatically displays search controls
            when the number of records exceeds one page.  If 'query' is passed, these embedded
            search conrols are disabled (it is considered, that the application has it's own search
            interface), but otherwise the form behaves as if the query was filled in its own search
            field.  The query string is split into query words by space and the form is filtered to
            contain only records containing all the words in any of its string columns.

          filter -- filter condition as a 'pytis.data.Operator' instance.  This condition will be
            appended to 'condition', but the difference is that 'condition' is invisible to the
            user, but 'filter' may be indicated in the user interface.

          message -- function returning a custom search result message.  If none, a default message
            will be used according to current query, such as 'Found 5 records matching the search
            expression.'.  A function of one argument (integer determining the number of records in
            the form) may be used to return a custom message as a string (possibly LCG Translatable
            object).  An example of a custom message might be 'Found 15 articles in category
            Python'.

          req -- instance of a class implementing the 'Request' API.  The form state (sorting,
            paging etc) will be set up according to the reqest parameters, if the request includes
            them (form controls were used to submit the form).  The constructor argument 'name'
            (defined in parent class) may be used to distinguish between multiple forms on one
            page.  If this parameter was passed, it is sent as the request argument 'form_name'.
            Thus if this argument doesn't match the form name, the request arguments are ignored.

        See the parent classes for definition of the remaining arguments.

        """
        self._columns = columns or view.columns()
        uri_provider = kwargs.pop('uri_provider')
        if uri_provider:
            def browse_form_uri_provider(row, cid=None, type=UriType.LINK):
                if cid == self._columns[0] and type==UriType.LINK:
                    uri = uri_provider(row, None, type=type)
                    if uri is not None:
                        return uri
                return uri_provider(row, cid, type=type)
            kwargs['uri_provider'] = browse_form_uri_provider
        super(BrowseForm, self).__init__(view, row, **kwargs)
        self._condition = condition
        params = {}
        if req is not None and req.param('form_name') == self._name:
            # Process request params if they belong to the current form.
            for param, func in (('sort', str), ('dir', str),
                                ('limit', int), ('offset', int),
                                ('next', bool), ('prev', bool),
                                ('search', str), ('index_search', None),
                                ('filter', str), ('query', unicode),
                                ('show_query_field', bool)):
                if req.has_param(param):
                    value = req.param(param)
                    if func:
                        try:
                            value = func(value)
                        except:
                            continue
                    params[param] = value
        # Determine the current sorting.
        if params.has_key('sort') and params.has_key('dir'):
            cid = params['sort']
            dir = dict([(b, a) for a, b in self._SORTING_DIRECTIONS]).get(params['dir'])
            if self._row.data().find_column(cid) and dir:
                sorting = ((cid, dir),)
        if sorting is None:
            sorting = self._view.sorting()
        if sorting is None:
            sorting = ((self._key, pytis.data.ASCENDENT),)
        self._sorting = sorting
        # Determine the limit of records per page.
        self._limits = limits
        if req is not None:
            limit_ = params.get('limit')
            if limit_ is None:
                cookie = req.cookie('pytis-form-limit')
                if cookie and isinstance(cookie, (str, unicode)) and cookie.isdigit():
                    limit_ = int(cookie)
            else:
                req.set_cookie('pytis-form-limit', limit_)
            if limit_ in limits:
                limit = limit_
        self._limit = limit
        self._index_search_string = ''
        # Determine the current offset.
        if limit is None:
            offset = 0
        elif req is not None and search is None:
            if params.has_key('search'):
                type = self._row.data().find_column(self._key).type()
                value, error = type.validate(params['search'])
                if not error:
                    search = pytis.data.EQ(self._key, value)
            elif params.has_key('index_search'):
                if isinstance(self._row[sorting[0][0]].type(), pd.String):
                    self._index_search_string = search_string = params['index_search']
                    search = self._index_search_condition(search_string)
            else:
                if params.has_key('offset'):
                    offset = params['offset']
                if params.has_key('next'):
                    offset += limit
                if params.has_key('prev') and offset >= limit:
                    offset -= limit
        self._offset = offset
        self._search = search
        # Determine the current query search condition.
        if query is not None:
            show_query_field = False
            allow_query_field = False
        else:
            query = params.get('query')
            show_query_field = bool(query or params.get('show_query_field'))
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
        # Determine the current filter.
        filter_id = params.get('filter', self._view.default_filter())
        if filter_id and filter_id != self._NULL_FILTER_ID:
            condition = find(filter_id, self._view.filters(), key=lambda f: f.id())
            if condition:
                c = condition.condition()
                if filter:
                    filter = pd.AND(filter, c)
                else:
                    filter = c
            else:
                filter_id = None
        self._filter_id = filter_id
        self._filter = filter
        # Determine whether tree emulation should be used.
        if sorting and isinstance(self._row[sorting[0][0]].type(), pytis.data.TreeOrder):
            self._tree_order_column = sorting[0][0]
        else:
            self._tree_order_column = None
        self._column_fields = cfields = [self._fields[cid] for cid in self._columns]
        self._align = dict([(f.id, 'right') for f in cfields if isinstance(f.type, pd.Number)])
        self._custom_message = message

    def _export_cell(self, context, field):
        value = self._format_field(context, field)
        if field.id == self._column_fields[0].id and self._tree_order_column:
            order = self._row[self._tree_order_column].value()
            if order is not None:
                level = len(order.split('.')) - 2
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
    
    def _export_group_heading(self, context, field):
        g = context.generator()
        return g.tr(g.th(self._format_field(context, field), colspan=len(self._column_fields)),
                    cls='group-heading')
    
    def _export_headings(self, context):
        g = context.generator()
        current_sorting_column, current_dir = self._sorting[0]
        directions = [dir for dir, name in self._SORTING_DIRECTIONS]
        def label(field):
            result = field.column_label
            if not field.virtual:
                if field.id == current_sorting_column:
                    dir = current_dir
                else:
                    dir = None
                new_dir = directions[(directions.index(dir)+1) % len(directions)]
                arg = dict(self._SORTING_DIRECTIONS)[new_dir]
                result = g.link(result, self._link_ctrl_uri(g, sort=field.id, dir=arg))
                if dir:
                    # Characters u'\u25be' and u'\u25b4' won't display in MSIE...
                    sign = dir == pytis.data.ASCENDENT and '&darr;' or '&uarr;'
                    result += ' '+ g.span(sign, cls='sorting-sign')
            return result
        return concat([g.th(label(f)) for f in self._column_fields])

    def _conditions(self, condition=None):
        conditions = [c for c in (self._condition, self._filter, self._query_condition, condition)
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
            # Translators: This string uses plural forms.  '%d' is replaced by the number and
            # this number also denotes the plural form used.  Please supply translations for
            # all plural forms relevant for the target language.
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
        self._count = count = data.select(condition=self._conditions(), sort=self._sorting)
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
        else:
            pages, modulo = divmod(count, min(limit, count))
            pages += modulo and 1 or 0
        self._pages = pages
        grouping = self._view.grouping()
        if self._view.group_heading():
            group_heading = self._fields[self._view.group_heading()]
        else:
            group_heading = None
        if self._sorting != self._view.sorting():
            grouping = None
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
            if grouping:
                group_values = [row[cid].value() for cid in grouping]
                if group_values != last_group_values:
                    self._group = not self._group
                    last_group_values = group_values
                    if group_heading:
                        exported_heading = self._export_group_heading(context, group_heading)
                        if exported_heading is not None:
                            exported_rows.append(exported_heading)
            if found and offset == (n + page * limit):
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
                # Translators: Used in empty list forms.  "Records" refers to database records in
                # the most generic senese possible.
                return g.strong(_("No records."))
        else:
            if limit is None or count <= self._limits[0]:
                summary = _("Total records:") +' '+ g.strong(str(count))
            else:
                # Translators: The variables '%(first)s', '%(last)s' and '%(total)s' are replaced
                # by the numbers corresponding to the current listing range.
                summary = _("Displayed records %(first)s-%(last)s of total %(total)s",
                            first=g.strong(str(first_record_offset+1)),
                            last=g.strong(str(first_record_offset+n)),
                            total=g.strong(str(count)))
            return self._wrap_exported_rows(context, exported_rows, summary)

    def _link_ctrl_uri(self, generator, sort=None, dir=None, **kwargs):
        if sort is None:
            sort, dir_ = self._sorting[0]
            dir = dict(self._SORTING_DIRECTIONS)[dir_]
        return generator.uri(self._handler,
                             ('form_name', self._name),
                             ('filter', self._filter_id),
                             sort=sort, dir=dir, **kwargs)

    def _index_search_condition(self, search_string):
        value = pd.Value(pd.String(), search_string+"*")
        return pytis.data.WM(self._sorting[0][0], value, ignore_case=False)
    
    def _export_index_search_controls(self, context):
        g = context.generator()
        field = self._field(self._sorting[0][0])
        if not isinstance(field.type, pd.String):
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
            if len(values) < 3 or len(values) > 100:
                break
            if search_string:
                # Translators: This is a label preceding index search controls.  These controls
                # allow the user to move in a long alphabetically sorted list by alphabetical
                # prefix.  For example in a listing of books, one might have the following index
                # search controls: 'Author: A, B, C, D, ...' and selecting 'A' will move us to the
                # first author beginning with A.  At the same time the form will automatically
                # display subordinate index search controls for all authors beginning with 'A':
                # 'Author on "A": Ab, Ac, Ad, Af, ....'.  And this is where this label is used.
                # '%(label)s' is replaced by the label of the controlling column. '%(prefix)s' is
                # replaced by the selected letter or substring and 'on' has the meaning "beginning
                # with".  Also take care to use the correct quotation marks for the target language
                # (written as the corresponding unicode characters).
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
        id = (bottom and '0' or '1') + '%x' % positive_id(self)
        content = []
        filters = [(f.name(), f.id()) for f in self._view.filters()
                   if f.id() is not None and f.condition() is not None]
        show_filter = filters and (count or self._filter_id is not None)
        show_query_field = self._show_query_field
        if not bottom:
            msg = self._message(count)
            if msg:
                content.append(g.div(msg, cls='results'))
        if show_query_field and not bottom:
            query_id = 'filter-' + id
            content.append(g.div((g.label(_("Search expression") +': ', query_id),
                                  g.field(self._query, name='query', id=query_id),
                                  g.hidden('show_query_field', '1'),
                                  # Translators: Search button label.
                                  g.submit(_("Search"))),
                                 cls='query' + (show_filter and ' with-filter' or '')))
        if show_filter and not bottom:
            null_filter = find(None, self._view.filters(), key=lambda f: f.condition())
            if null_filter:
                null_filter_name = null_filter.name()
            else:
                # Translators: Label used in filter selection box for the option which disables
                # filtering and thus results in all records to be displayed.
                null_filter_name = _("All items")
            filters.insert(0, (null_filter_name, self._NULL_FILTER_ID))
            filter_id = 'filter-' + id
            # Translators: Label of filter selection box.  Filtering limits the displayed records
            # by certain criterias.
            content.append(g.div((g.label(_("Filter")+': ', filter_id),
                                  g.select(name='filter', id=filter_id,
                                           selected=self._filter_id,
                                           title=(_("Filter")+' '+
                                                  # Translators: Tooltip text suggesting keyboard
                                                  # combination to use for selection without
                                                  # unexpected invocation of the option.
                                                  _("(Use ALT+arrow down to select)")),
                                           onchange='this.form.submit(); return true',
                                           options=filters),
                                  # Translators: Button for manual selection invocation (when
                                  # JavaScript is off.
                                  g.noscript(g.submit(_("Apply")))),
                                 cls="filter"))
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
                    search_button = g.submit(_("Search"), name='show_query_field', cls='search')
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
                             g.span((g.submit(_("Previous"), name='prev', cls='prev',
                                              title=_("Go to previous page"), disabled=(page == 0)),
                                     g.submit(_("Next"),  name='next', cls='next',
                                              title=_("Go to next page"),
                                              disabled=(page+1)*limit >= count),
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
                         g.noscript(g.submit(_("Go"))))
            if controls:
                cls = 'paging-controls' + (pages == 1 and ' one-page' or '')
                content.append(g.div(controls, cls=cls))
        if content:
            if self._name is not None:
                content.append(g.hidden('form_name', self._name))
            if len(self._sorting) == 1:
                cid, dir = self._sorting[0]
                content.extend((g.hidden('sort', cid),
                                g.hidden('dir', dict(self._SORTING_DIRECTIONS)[dir])))
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


class ListView(BrowseForm):
    """Listing with a customizable layout for each record.

    This form behaves similarly to a regular 'BrowseForm', but the records are not represented as
    table rows but as individual (sub)sections of a document.  Field values can be displayed within
    each section and their layout can be customized through the 'list_view' argument within
    specification (see 'ViewSpec').  If 'list_view' is not defined, the form will use the standard
    table presentation like 'BrowseForm'.
    
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
                    self._format_field(context, field)
                    for field, labeled in self._meta]
            parts.append(g.div(concat(meta, separator=', '), cls='meta'))
        if layout.layout():
            parts.append(self._export_group(context, layout.layout()))
        for fid in layout.content():
            if self._row[fid].value() is not None:
                text = self._row[fid].export()
                content = lcg.SectionContainer(parser.parse(text), toc_depth=0)
                content.set_parent(self.parent())
                # Hack: Add a fake container to force the heading level start at 4.
                container = lcg.SectionContainer(lcg.Section('', lcg.Section('', content,
                                                                             anchor=anchor)))
                parts.append(g.div(content.export(context), cls='content id-'+ fid))
        return g.div(parts, id=id, cls='list-item ' + (n % 2 and 'even' or 'odd'))

    def _export_field(self, context, field):
        return self._format_field(context, field)

    def _export_group_heading(self, context, field):
        #return context.generator().h(self._format_field(context, field), 3, cls='group-heding')
        return None

    def _wrap_exported_rows(self, context, rows, summary):
        g = context.generator()
        result = ()
        if self._exported_row_index:
            result += (g.div(g.list(self._exported_row_index), cls="index"),)
        columns = self._list_layout.columns()
        if columns > 1:
            n, mod = divmod(len(rows), columns)
            rows = g.table([g.tr([g.td(r, width="%d%%" % (100/columns), valign='top')
                                  for r in rows[i*columns:(i+1)*columns]])
                            for i in range(n+min(mod, 1))], border=0, cls='grid')
        result += (g.div(rows, cls="body"),
                   g.div(summary, cls="summary"))
        return concat(result)


class ItemizedView(BrowseForm):
    """Simplified listing of records in a form of itemized list.

    This form behaves similarly to a regular 'BrowseForm', but the records are presented as items
    in an unordered bullet list rather than as table rows.

    """
    
    _CSS_CLS = 'itemized-view'
    
    def __init__(self, view, row, columns=None, separator=', ', template=None, **kwargs):
        """Arguments:

          columns -- an explicit list of fields shown for each record.  Only the first column of
            the underlying view is shown by default, but if a sequence of column identifierrs is
            passed, multiple values will be shown (separated by the 'separator').
          separator -- string used to separate individual values when multiple 'columns' are shown.
          template -- if used, the list items will be formatted using given template string.  The
            string must be an `lcg.TranslatableText' instance.  The final item text will be
            produced by interpolating variables in the string by the formatted values of
            corresponding fields.  The argument may also be a callable object (function), which
            returns the template string when called with a `PresentedRow' instance as an argument.

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
            fields = [self._format_field(context, field)
                      for field in self._column_fields if row[field.id].value() is not None]
            return concat(fields, separator=self._separator)

    def _export_group_heading(self, context, field):
        #TODO: Create multi-level lists.
        return None
    
    def _wrap_exported_rows(self, context, rows, summary):
        g = context.generator()
        return g.list(rows)


class CheckRowsForm(BrowseForm, _SubmittableForm):
    """Web form with checkable boolean columns in each row.

    The form is rendered as an ordinary table, but boolean columns (all or only the selected) are
    represented by a checkbox in each row and the form has submit controls.  Thus the user can
    modify the values in all rows and submit the changes in one step.

    *Processing the submitted form:*

    Each checkbox column is represented by one query parameter.  Its name is the column identifier
    and the values are the key column values of all checked rows.
    
    """
    def __init__(self, view, row, check_columns=None, limits=(), limit=None, **kwargs):
        """Arguments:

          check_columns -- a sequence of column identifiers for which the checkboxes will be
            created.  If the argument is omitted, checkboxes will automatically appear for all
            boolean columns.

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

