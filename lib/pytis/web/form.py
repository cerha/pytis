# -*- coding: iso-8859-2 -*-

# Copyright (C) 2006, 2007 Brailcom, o.p.s.
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

This module provides an implementations of Pytis forms, which can be used for web interfaces to
Pytis informations systems.  Ideally, we should be able to generate web forms from the same
specification.  Since some aspects of the specifications are bound to a GUI environment (such as
callback functions, which run dialogs and forms directly), not all features are currently
available.  However the core functionality is implemented and data can be viewed and manipulated
(inserted, edited) using web forms.

Pytis currently does not include support running the actual web application which would make use of
these forms.  Such an application framework which makes use of pytis web forms, is implemented
separately.  See the Wiking project at http://www.freebsoft.org/wiking for more information.

All the content generation is done using the LCG framework.  See http://www.freebsoft.org/lcg.

The classes defined below are currently just prototypes, so they are not documented yet and the API
may change drastically!

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
    def __init__(self, data, view, resolver, row=None, prefill=None, new=False, handler='#',
                 hidden=(), submit=None, name=None, uri_provider=None, **kwargs):
        """Initialize the instance.

        Arguments:

          data -- data object as a 'pytis.data.Data' instance
          
          view -- presentation specification as a 'pytis.presentation.ViewSpec' instance
          
          resolver -- pytis name resolver as a pytis.util.Resolver' instance
          
          row -- current row as a 'pytis.data.Row' instance or None
          
          prefill -- current records prefill as a dictionary or None.  Same as the
            'pytis.presentation.PresentedRow' constructor argument of the same name.
          
          new -- flag indicating, that the current record is a new (inserted) one.  Same as the
            'pytis.presentation.PresentedRow' constructor argument of the same name.

          uri_provider -- callable object (function) returning URIs for form fields.  This makes
            Pytis web forms independent on the application's URI scheme.  The function must accept
            one positional argument (the 'pytis.presentation.PresentedRow' instance) and two
            keyword arguments.  The first of them -- 'cid' -- may also be used as positional and
            denotes the identifier of the field, for which the URI is requested.  It may be None
            when requesting URI for the whole record.  The later argument 'type' will always be one
            of 'UriType' constants.  It is used for distinction of the purpose, for which the uri
            us used (eg. for a link or an image src).
          
          handler -- form handler URI as a string.  This URI is used in the form's 'action'
            attribute.
          
          hidden -- hardcoded hidden form fields as a sequence of pairs (name, value).

          submit -- custom submit buttons as a sequence of (label, name) pairs.  The default submit
            buttons are ((_('Submit'), None),)
            
          name -- form name as a string or None.  This name will be sent as a hidden field and used
            to distinguish which request parameters belong to which form.

        """
        super(Form, self).__init__(**kwargs)
        assert isinstance(data, pytis.data.Data), data
        assert isinstance(view, ViewSpec), view
        assert isinstance(resolver, pytis.util.Resolver), resolver
        assert row is None or isinstance(row, pytis.data.Row), row
        assert isinstance(handler, (str, unicode)), handler
        assert isinstance(hidden, (tuple, list)), hidden
        self._data = data
        self._key = data.key()[0].id()
        self._view = view
        self._prefill = prefill or {}
        self._uri_provider = uri_provider
        self._row = PresentedRow(view.fields(), data, row, resolver=resolver,
                                 prefill=self._valid_prefill(), new=new)
        self._handler = handler
        self._hidden = list(hidden)
        self._submit = submit or ((_("Submit"), None),)
        self._name = name
        self._enctype = None

    def _valid_prefill(self):
        # Return a dictionary of Python values for the prefill argument.
        valid = {}
        for f in self._view.fields():
            id = f.id()
            if self._prefill.has_key(id):
                f = self._view.field(id)
                type = f.type(self._data)
                if not isinstance(type, pytis.data.Password):
                    value, error = type.validate(self._prefill[id], strict=False)
                    if not error:
                        valid[id] = value
        return valid

    def _export_body(self, exporter):
        pass
    
    def _export_submit(self, exporter):
        pass
    
    def _export_footer(self, exporter):
        pass
    
    def export(self, exporter):
        g = exporter.generator()
        content = [self._export_body(exporter)] + \
                  [wrap(part, cls=name) for wrap, part, name in
                   ((g.div, self._export_submit(exporter),   'submit'),
                    (g.div, self._export_footer(exporter),   'footer')) if part]
        cls = 'pytis-form ' + self._CSS_CLS
        if self._name:
            cls += ' ' + camel_case_to_lower(self._name, '-')
        return g.form(content, action=g.uri(self._handler), method=self._HTTP_METHOD, cls=cls,
                      enctype=self._enctype)


class FieldForm(Form):
    """Form with formattable fields."""
    
    def __init__(self, *args, **kwargs):
        super(FieldForm, self).__init__(*args, **kwargs)
        self._fields = [self._field(id) for id in self._used_fields()]
        
    def _field(self, id):
        from field import _Field
        return _Field(self._view.field(id), self._row[id].type(), self, self._uri_provider)
        
    def _used_fields(self):
        # Override this method to return a sequence of field identifiers for all used fields.
        return ()
        
    def _format_field(self, exporter, field):
        return field.formatter.format(exporter.generator(), self._row, field)
    

class LayoutForm(FieldForm):
    """Form with fields arranged according to pytis layout specification."""
    _MAXLEN = 100
    _ALIGN_NUMERIC_FIELDS = False

    def __init__(self, data, view, resolver, layout=None, allow_table_layout=True, **kwargs):
        assert layout is None or isinstance(layout, GroupSpec)
        self._layout = layout
        super(LayoutForm, self).__init__(data, view, resolver, **kwargs)
        self._allow_table_layout = allow_table_layout
        self._field_dict = dict([(f.id, f) for f in self._fields])
        
    def _used_fields(self):
        return self._layout.order()
    
    def _export_group(self, exporter, group):
        g = exporter.generator()
        result = []
        fields = []
        wrap = False
        for item in group.items():
            if isinstance(item, Button):
                pass
            elif isinstance(item, GroupSpec):
                if fields:
                    result.append(self._export_fields(g, fields))
                fields = []
                result.append(self._export_group(exporter, item))
            else:
                field = self._field_dict[item]
                ctrl = self._export_field(exporter, field)
                if ctrl is not None:
                    label = self._export_field_label(exporter, field)
                    help = self._export_field_help(exporter, field)
                    fields.append((field, label, ctrl, help))
                    wrap = True
        if fields:
            result.append(self._export_fields(g, fields))
        if group.orientation() == Orientation.HORIZONTAL:
            result = [g.table(g.tr([g.td(x, valign='top', cls=(i != 0 and 'spaced' or None))
                                    for i, x in enumerate(result)]),
                              width='100%', cellspacing=0, cellpadding=0, cls='horizontal-group')]
            wrap = False
        if group.label():
            result = g.fieldset(group.label()+':', result, cls='group')
        elif wrap:
            result = g.div(result, cls='group')
        else:
            result = concat(result, separator="\n")
        return result

    def _export_packed_field(self, g, field, label, ctrl, help):
        if self._allow_table_layout:
            if help is not None:
                ctrl += help
            if field.spec.compact():
                if label:
                    label += g.br() +"\n"
                td = g.td(label + ctrl, colspan=3)
            else:
                td = g.td(label or '', valign='top', cls='label')
                if self._ALIGN_NUMERIC_FIELDS and isinstance(field.type, pytis.data.Number):
                    td += g.td(ctrl, cls='ctrl', align='right') + \
                          g.td('', width='100%', cls='spacer')
                else:
                    td += g.td(ctrl, cls='ctrl', width='100%', colspan=2)
            return g.tr(td)
        else:
            rows = (concat(label, g.br()), concat(ctrl, g.br()))
            if help:
                rows += (help,)
            return g.div(rows, cls="field")
        
    def _export_fields(self, g, fields):
        rows = [self._export_packed_field(g, field, label, ctrl, help)
                for field, label, ctrl, help in fields]
        if self._allow_table_layout:
            rows = g.table(rows, cls='packed-fields')
        return concat(rows, separator="\n")

    def _export_field(self, exporter, field):
        return None

    def _export_field_label(self, exporter, field):
        if field.label:
            return exporter.generator().label(field.label, None) + ":"
        else:
            return None
    
    def _export_field_help(self, exporter, field):
        return None


class _SingleRecordForm(LayoutForm):

    def __init__(self, data, view, resolver, row, layout=None, **kwargs):
        super(_SingleRecordForm, self).__init__(data, view, resolver, row=row,
                                                layout=layout or view.layout().group(), **kwargs)
        
    def _export_body(self, exporter):
        return self._export_group(exporter, self._layout)
    
    
class _SubmittableForm(Form):
    """Mix-in class for forms with submit buttons."""
    
    def _export_submit(self, exporter):
        g = exporter.generator()
        return [g.hidden(k, v) for k, v in self._hidden] + \
               [g.hidden('form-name', self._name)] + \
               [g.submit(label, name=name, title=_("Submit the form"))
                for label, name in self._submit] + \
               [g.reset(_("Reset", title=_("Undo all changes")))]


class ShowForm(_SingleRecordForm):
    _CSS_CLS = 'show-form'
    _ALIGN_NUMERIC_FIELDS = False
    
    def _export_field(self, exporter, field):
        return self._format_field(exporter, field)

    
class EditForm(_SingleRecordForm, _SubmittableForm):
    _CSS_CLS = 'edit-form'
    
    def __init__(self, data, view, resolver, row, errors=(), **kwargs):
        super(EditForm, self).__init__(data, view, resolver, row, **kwargs)
        key, order = self._key, tuple(self._layout.order())
        self._hidden += [(k, v) for k, v in self._prefill.items()
                         if view.field(k) and not k in order and k != key]
        if not self._row.new() and key not in order + tuple([k for k,v in self._hidden]):
            self._hidden += [(key,  self._row[key].export())]
        assert isinstance(errors, (tuple, list, str, unicode)), errors
        self._errors = errors
        binary = [id for id in order if isinstance(self._row[id].type(), pytis.data.Binary)]
        self._enctype = (binary and 'multipart/form-data' or None)

    def _export_field(self, exporter, field):
        g = exporter.generator()
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
        elif type.enumerator():
            ctrl = g.select
            enum = self._row.enumerate(field.id)
            attr['options'] = [("&nbsp;", "")] + \
                              [(display, str(val)) for val, display in enum]
            if value.value() in [val for val, display in enum]:
                attr['selected'] = str(value.value())
        else:
            if field.spec.height() > 1:
                ctrl = g.textarea
                attr['rows'] = field.spec.height()
                attr['cols'] = field.spec.width()
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
        result = ctrl(**attr)
        if isinstance(type, pytis.data.Password) and type.verify():
            attr['id'] = attr['id'] + '-verify-pasword'
            result += g.br() + ctrl(**attr)
        return result

    def _export_field_label(self, exporter, field):
        g = exporter.generator()
        if not field.label:
            return None
        type = field.type
        if type.not_null() and not isinstance(type, pytis.data.Boolean) and \
               (self._row.new() or not isinstance(type, (pytis.data.Password, pytis.data.Binary))):
            sign = g.sup("*", cls="not-null")
        else:
            sign = ''
        return g.label(field.label, field.unique_id) + sign + ":"
        
    def _export_field_help(self, exporter, field):
        descr = field.spec.descr()
        return descr and exporter.generator().div(descr, cls="help")

    def _export_body(self, exporter):
        g = exporter.generator()
        if isinstance(self._errors, (str, unicode)):
            errors = g.p(self._errors)
        else:
            errors = [g.p(self._view.field(id) and
                          self._view.field(id).label() or id, ": ",
                          msg) +"\n"
                      for id, msg in self._errors]
        if errors:
            errors = g.div(errors, cls='errors')
        return concat(errors, super(EditForm, self)._export_body(exporter))
                      
    def _export_footer(self, exporter):
        g = exporter.generator()
        return g.span("*", cls="not-null") +") "+ _("Fields marked by an asterisk are mandatory.")

    
class BrowseForm(LayoutForm):
    _CSS_CLS = 'browse-form'
    _HTTP_METHOD = 'GET'
    _SORTING_DIRECTIONS = ((pytis.data.ASCENDENT, 'asc'),
                           (pytis.data.DESCENDANT, 'desc'),
                           (None, 'none'))

    def __init__(self, data, view, resolver, columns=None, condition=None, sorting=None,
                 limits=(25, 50, 100, 200, 500), limit=50, offset=0, search=None, req=None,
                 **kwargs):
        """Initialize the instance.

        Arguments:

          columns -- sequence of column identifiers to be displayed or None for the default columns
            defined by specification
            
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
            condition will be displayed on the current page.  The request parameter 'search' can be
            also used to generate a search condition (and overrides this argument).  In this case
            only searching by the value of the key column is supported (it is not possible to pass
            a generic condition within the request).  If no matching record is found, the offset is
            controlled by the offset argument.  Searching is ignored when the current limit is
            greater than the total number of records.

          req -- instance of a class implementing the 'Request' API.  The form state (sorting,
            paging etc) will be set up according to the reqest parameters, if the request includes
            them (form controls were used to submit the form).  The constructor argument 'name'
            (defined in parent class) may be used to distinguish between multiple forms on one
            page.  If this parameter was passed, it is sent as the request argument 'form-name'.
            Thus if this argument doesn't match the form name, the request arguments are ignored.

        See the parent classes for definition of the remaining arguments.

        """
        self._columns = columns or view.columns()
        uri_provider = kwargs.pop('uri_provider')
        if uri_provider:
            def browse_form_uri_provider(row, cid=None, **kwargs):
                if cid == self._columns[0]:
                    cid = None
                return uri_provider(row, cid, **kwargs)
            kwargs['uri_provider'] = browse_form_uri_provider
        super(BrowseForm, self).__init__(data, view, resolver, **kwargs)
        self._condition = condition
        params = {}
        if req is not None:
            # Ignore particular request params if they don't belong to the current form.
            valid_params = (('search', str),)
            if req.param('form-name') == self._name:
                valid_params += (('sort', str), ('dir', str), ('limit', int), ('offset', int),
                                 ('next', bool), ('prev', bool))
            for param, func in valid_params:
                if req.has_param(param):
                    value = req.param(param)
                    try:
                        value = func(value)
                    except:
                        pass
                    else:
                        params[param] = value
        # Determine the current sorting.
        if params.has_key('sort') and params.has_key('dir'):
            cid = params['sort']
            dir = dict([(b, a) for a, b in self._SORTING_DIRECTIONS]).get(params['dir'])
            if self._data.find_column(cid) and dir:
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
        # Determine the current offset.
        if limit is None:
            offset = 0
        elif req is not None:
            if params.has_key('search'):
                type = self._data.find_column(self._key).type()
                value, error = type.validate(params['search'])
                if not error:
                    search = pytis.data.EQ(self._key, value)
            else:
                if params.has_key('offset'):
                    offset = params['offset']
                if params.has_key('next'):
                    offset += limit
                if params.has_key('prev') and offset >= limit:
                    offset -= limit
        self._offset = offset
        self._search = search
        # Determine whether tree emulation should be used.
        if sorting and isinstance(self._row[sorting[0][0]].type(), pytis.data.TreeOrder):
            self._tree_order_column = sorting[0][0]
        else:
            self._tree_order_column = None
        self._align = dict([(f.id, 'right') for f in self._fields if isinstance(f.type, pd.Number)])

    def _used_fields(self):
        return self._columns

    def _export_cell(self, exporter, field):
        value = self._format_field(exporter, field)
        if field.id == self._fields[0].id and self._tree_order_column:
            order = self._row[self._tree_order_column].value()
            if order is not None:
                level = len(order.split('.')) - 2
                if level > 0:
                    g = exporter.generator()
                    indent = level * g.span(2*'&nbsp;', cls='tree-indent')
                    value = indent + '&bull;&nbsp;'+ g.span(value, cls='tree-node')
                    # &#8227 does not work in MSIE
        return value

    def _style(self, style, row, n=None):
        def color(c):
            if type(c) is tuple:
                return '#%02x%02x%02x' % c
            else:
                return c
        if callable(style):
            style = style(row)
        cls = n is not None and (n % 2 and 'even' or 'odd') or None
        if style is None:
            return cls and dict(cls=cls) or {}
        if style.name() is not None:
            if cls is None:
                cls = style.name()
            else:
                cls += ' ' + style.name()
            return dict(cls=cls)
        styles = [name +': '+ f(attr()) for attr, name, f in (
            (style.foreground, 'color',            color),
            (style.background, 'background-color', color),
            (style.slanted,    'font-style',       lambda x: x and 'italic' or 'normal'),
            (style.bold,       'font-weight',      lambda x: x and 'bold' or 'normal'),
            (style.overstrike, 'text-decoration',  lambda x: x and 'line-through' or 'none'),
            (style.underline,  'text-decoration',  lambda x: x and 'underline' or 'none'),
            ) if attr() is not None]
        return dict(cls=cls, style='; '.join(styles))
    
    def _export_row(self, exporter, row, n):
        g = exporter.generator()
        cells = [g.td(self._export_cell(exporter, field), align=self._align.get(field.id),
                      **self._style(field.style, row))
                 for field in self._fields]
        return g.tr(cells, **self._style(self._view.row_style(), row, n))
    
    def _export_headings(self, exporter):
        g = exporter.generator()
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
                result = g.link(result, g.uri(self._handler, ('form-name', self._name),
                                              sort=field.id, dir=arg, limit=self._limit))
                if dir:
                    # Characters u'\u25be' and u'\u25b4' won't display in MSIE...
                    sign = dir == pytis.data.ASCENDENT and '&darr;' or '&uarr;'
                    result += ' '+ g.span(sign, cls='sorting-sign')
            return result
        return concat([g.th(label(f)) for f in self._fields])
    
    def _wrap_exported_rows(self, exporter, rows, summary):
        g = exporter.generator()
        return g.table((g.thead(g.tr(self._export_headings(exporter))),
                        g.tfoot(g.tr(g.td(summary, colspan=len(self._fields)))),
                        g.tbody(rows)), border=1)

    def _export_body(self, exporter):
        data = self._data
        row = self._row
        limit = self._limit
        exported_rows = []
        self._count = count = data.select(condition=self._condition, sort=self._sorting)
        if self._search:
            dist = data.search(self._search)
            if dist:
                self._offset = dist - 1
        if limit is not None:
            page = int(max(0, min(self._offset, count)) / limit)
            offset = page*limit
        else:
            page = 0
            offset = 0
        self._page = page
        n = 0
        data.skip(offset)
        while True:
            r = data.fetchone()
            if r is None:
                break
            row.set_row(r)
            exported_rows.append(self._export_row(exporter, row, n))
            n += 1 
            if limit is not None and n >= limit:
                break
        data.close()
        g = exporter.generator()
        if n == 0:
            return g.strong(_("No records."))
        else:
            if limit is None or count < self._limits[0]:
                summary = _("Total records:") +' '+ g.strong(str(count))
            else:
                summary = _("Displayed records %(first)s-%(last)s of total %(total)s",
                            first=g.strong(str(offset+1)),
                            last=g.strong(str(offset+n)),
                            total=g.strong(str(count)))
            return self._wrap_exported_rows(exporter, exported_rows, summary)
                   

    def _export_controls(self, exporter, second=False):
        limit, page, count = self._limit, self._page, self._count
        g = exporter.generator()
        pages, modulo = divmod(count, min(limit, count))
        pages += modulo and 1 or 0
        id = (second and '0' or '1') + '%x' % positive_id(self)
        offset_id = 'offset-' + id
        limit_id = 'limit-' + id
        result = ()
        if pages > 1:
            result += (g.span((g.label(_("Page")+': ', offset_id),
                               g.select(name='offset', id=offset_id, selected=page*limit,
                                        title=(_("Page")+' '+_("(Use ALT+arrow down to select)")),
                                        options=[(str(i+1), i*limit) for i in range(pages)],
                                        onchange='this.form.submit(); return true') + ' / ',
                               g.strong(str(pages))), cls="offset"),
                       g.span((g.submit(_("Previous"), name='prev', cls='prev',
                                        title=_("Go to previous page"), disabled=(page == 0)),
                               g.submit(_("Next"),  name='next', cls='next',
                                        title=_("Go to next page"),
                                        disabled=(page+1)*limit >= count)), cls="buttons"))
        result += (g.span((g.label(_("Records per page")+':', limit_id)+' ',
                           g.select(name='limit', id=limit_id,
                                    title=(_("Records per page")+' '+
                                           _("(Use ALT+arrow down to select)")),
                                    options=[(str(i), i) for i in self._limits], selected=limit,
                                    onchange='this.form.submit(); return true')), cls='limit'),
                   g.noscript(g.submit(_("Go"))))
        if self._name is not None:
            result += (g.hidden('form-name', self._name),)
        if len(self._sorting) == 1:
            cid, dir = self._sorting[0]
            result += (g.hidden('sort', cid),
                       g.hidden('dir', dict(self._SORTING_DIRECTIONS)[dir]))
        return g.form(result, action=g.uri(self._handler), method='GET',
                      cls=self._CSS_CLS+'-controls')

    def export(self, exporter):
        result = super(BrowseForm, self).export(exporter)
        if self._limit is not None and self._count > self._limits[0]:
            result = concat(self._export_controls(exporter),
                            result,
                            self._export_controls(exporter, second=True), separator="\n")
        return result


class ListView(BrowseForm):
    _CSS_CLS = 'list-view'
    
    def __init__(self, data, view, resolver, **kwargs):
        self._list_layout = list_layout = view.list_layout()
        layout = list_layout and list_layout.layout() or None
        super(ListView, self).__init__(data, view, resolver, layout=layout, **kwargs)
        if list_layout is None:
            super_ = super(ListView, self)
            self._CSS_CLS = super_._CSS_CLS
            self._export_row = super_._export_row
            self._wrap_exported_rows = super_._wrap_exported_rows
        else:
            self._meta = [(self._field(id), id in list_layout.meta_labels())
                          for id in list_layout.meta()]
            self._image = list_layout.image() and self._field(list_layout.image())
            
    def _used_fields(self):
        layout = self._view.list_layout()
        if layout:
            return layout.layout() and layout.layout().order() or ()
        else:
            return super(ListView, self)._used_fields()
    
    def _export_row(self, exporter, row, n):
        layout = self._list_layout
        g = exporter.generator()
        parser = lcg.Parser()
        title = self._row[layout.title()].export()
        if layout.anchor():
            name = layout.anchor() % row[self._key].export()
            title = g.link(title, None, name=name)
        parts = [g.h(title, level=3)]
        if layout.image():
            img = self._export_field(exporter, self._image) #cls='list-layout-image')
            if img:
                parts.append(img)
        if self._meta:
            meta = [g.span(labeled and g.span(field.label, cls='label')+": " or '' + \
                           self._format_field(exporter, field), cls=field.id)
                    for field, labeled in self._meta]
            parts.append(g.div(concat(meta, separator=', '), cls='meta'))
        if layout.layout():
            parts.append(self._export_group(exporter, layout.layout()))
        if layout.content():
            text = self._row[layout.content()].export()
            content = lcg.Container(parser.parse(text))
            content.set_parent(self.parent())
            parts.append(g.div(content.export(exporter), cls='content'))
        return g.div(parts, cls='list-item ' + (n % 2 and 'even' or 'odd'))

    def _export_field(self, exporter, field):
        return self._format_field(exporter, field)
    
    def _wrap_exported_rows(self, exporter, rows, summary):
        g = exporter.generator()
        return g.div(rows, cls="body") +"\n"+ g.div(summary, cls="summary")

    
class CheckRowsForm(BrowseForm, _SubmittableForm):
    """Web form with checkable boolean columns in each row.

    The form is rendered as an ordinary table, but boolean columns (all or only the selected) are
    represented by a checkbox in each row and the form has submit controls.  Thus the user can
    modify the values in all rows and submit the changes in one step.

    *Processing the submitted form:*

    Each checkbox column is represented by one query parameter.  Its name is the column identifier
    and the values are the key column values of all checked rows.
    
    """
    def __init__(self, data, view, resolver, check_columns=None, limits=(), limit=None, **kwargs):
        """Initialize the instance.

        Arguments:

          check_columns -- a sequence of column identifiers for which the checkboxes will be
            created.  If the argument is omitted, checkboxes will automatically appear for all
            boolean columns.

          See the parent classes for definition of the remaining arguments.

        """
        super(CheckRowsForm, self).__init__(data, view, resolver, limits=limits, limit=limit,
                                            **kwargs)
        assert isinstance(check_columns, (list, tuple)), check_columns
        if __debug__:
            for cid in check_columns:
                assert self._row.has_key(cid), cid
        if check_columns is None:
            check_columns = tuple([field.id for field in self._fields
                                   if isinstance(field.type, pd.Boolean)])
        self._check_columns = check_columns

    def _export_cell(self, exporter, field):
        if field.id in self._check_columns:
            return exporter.generator().checkbox(name=field.id, value=self._row.format(self._key),
                                                 checked=self._row[field.id].value())
        else:
            return super(CheckRowsForm, self)._export_cell(exporter, field)

