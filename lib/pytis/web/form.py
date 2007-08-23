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

import pytis.data
from pytis.presentation import *

import lcg
from lcg import concat

_ = lcg.TranslatableTextFactory('pytis')

# TODO: This hack is necessary as long as pytis default language is Czech.
pd = pytis.data
pd.Type._VM_NULL_VALUE_MSG = _("Empty value")
pd.Type._VM_INVALID_VALUE_MSG = _("Invalid value")
pd.Limited._VM_MINLEN_MSG = _("Minimal length %(minlen)s not satisfied")
pd.Limited._VM_MAXLEN_MSG = _("Maximal length %(maxlen)s exceeded")
pd.Integer._VM_NONINTEGER_MSG = _("Not an integer")
pd.Float._VM_INVALID_NUMBER_MSG = _("Invalid number")
pd.String._VM_MINLEN_MSG = _("Minimal length %(minlen)s not satisfied")
pd.String._VM_MAXLEN_MSG = _("String exceeds max length %(maxlen)s")
pd.Password._VM_PASSWORD_MSG = _("Enter the password twice to eliminate typos")
pd.Password._VM_PASSWORD_VERIFY_MSG = _("Passwords don't match")
pd.RegexString._VM_FORMAT_MSG = _("Invalid format")
pd.Color._VM_FORMAT_MSG = _("Invalid color format ('#RGB' or '#RRGGBB')")
pd.DateTime._VM_DT_FORMAT_MSG = _("Invalid date or time format")
pd.DateTime._VM_DT_VALUE_MSG = _("Invalid date or time")
pd.DateTime._VM_DT_AGE_MSG = _("Date outside the allowed range")
pd.Binary._VM_MINLEN_MSG = _("Minimal size %(minlen)s not satisfied")
pd.Binary._VM_MAXLEN_MSG = _("Maximal size %(maxlen)s exceeded")
pd.Image._VM_MAXSIZE_MSG = _("Maximal pixel size %(maxsize)s exceeded")
pd.Image._VM_MINSIZE_MSG = _("Minimal pixel size %(minsize)s exceeded")
pd.Image._VM_FORMAT_MSG = _("Unsupported format %(format)s; valid formats: %(formats)s")

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
    
    def __init__(self, data, view, resolver, row=None, prefill=None, new=False, link_provider=None,
                 handler='#', hidden=(), submit=None, name=None, **kwargs):
        super(Form, self).__init__(**kwargs)
        assert isinstance(data, pytis.data.Data), data
        assert isinstance(view, ViewSpec), view
        assert isinstance(resolver, pytis.util.Resolver), resolver
        assert row is None or isinstance(row, pytis.data.Row), row
        assert isinstance(handler, str), handler
        assert isinstance(hidden, (tuple, list)), hidden
        self._data = data
        self._view = view
        self._prefill = prefill or {}
        self._link_provider = link_provider
        self._row = PresentedRow(view.fields(), data, row, resolver=resolver,
                                 prefill=self._valid_prefill(), new=new)
        self._handler = handler
        self._hidden = list(hidden)
        self._submit = submit or ((_("Submit"), None),)
        self._name = name
        self._enctype = None

    def _uri(self, row, cid):
        if self._link_provider:
            return self._link_provider(row, cid)
        else:
            return None
        
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
    
    def _export_controls(self, exporter):
        pass
    
    def _export_submit(self, exporter):
        pass
    
    def _export_footer(self, exporter):
        pass
    
    def export(self, exporter):
        g = exporter.generator()
        content = [self._export_body(exporter)] + \
                  [wrap(part, cls=name) for wrap, part, name in
                   ((g.fieldset, self._export_controls(exporter), 'controls'),
                    (g.fieldset, self._export_submit(exporter),   'submit'),
                    (g.div,      self._export_footer(exporter),   'footer')) if part]
        cls = self._CSS_CLS + (self._name and ' ' + camel_case_to_lower(self._name, '-') or '')
        return g.form(content, action=self._handler, method=self._HTTP_METHOD, cls=cls,
                      enctype=self._enctype)
    

class _SubmittableForm(Form):
    """Mix-in class for forms with submit buttons."""
    
    def _export_submit(self, exporter):
        g = exporter.generator()
        return [g.hidden(k, v) for k, v in self._hidden] + \
               [g.hidden('form-name', self._name)] + \
               [g.submit(label, name=name, title=_("Submit the form"))
                for label, name in self._submit] + \
               [g.reset(_("Reset", title=_("Undo all changes")))]
    
    
class LayoutForm(Form):

    def __init__(self, *args, **kwargs):
        self._allow_table_layout = kwargs.pop('allow_table_layout', True)
        super(LayoutForm, self).__init__(*args, **kwargs)
    
    def _export_body(self, exporter):
        return self._export_group(exporter, self._view.layout().group())

    def _export_group(self, exporter, group):
        g = exporter.generator()
        result = []
        fields = []
        wrap = False
        for item in group.items():
            if isinstance(item, Button):
                continue
            if isinstance(item, GroupSpec):
                if fields:
                    result.append(self._export_fields(g, fields))
                fields = []
                result.append(self._export_group(exporter, item))
            else:
                field = self._view.field(item)
                if field.width() == 0:
                    continue
                items = self._export_field(exporter, field)
                if items:
                    fields.append((field,) + items)
                    wrap = True
        if fields:
            result.append(self._export_fields(g, fields))
        if wrap or group.label():
            result = exporter.generator().fieldset(result, legend=group.label(), cls='body')
        else:
            result = concat(result, separator="\n")
        #if group.orientation() == Orientation.VERTICAL:
        return result
        
    def _export_field(self, exporter, f):
        g = exporter.generator()
        value = self._row[f.id()]
        type = value.type()
        attr = {'name': f.id(),
                'id': "f%x-%s" % (positive_id(self), f.id())}
        if isinstance(type, pytis.data.Boolean):
            ctrl = g.checkbox
            attr['value'] = 'T'
            attr['checked'] = value.value()
        elif isinstance(type, pytis.data.Binary):
            ctrl = g.upload
        elif type.enumerator():
            ctrl = g.select
            enum = self._row.enumerate(f.id())
            attr['options'] = [("&nbsp;", "")] + \
                              [(display, str(val)) for val, display in enum]
            if value.value() in [val for val, display in enum]:
                attr['selected'] = str(value.value())
        else:
            if f.height() > 1:
                ctrl = g.textarea
                attr['rows'] = f.height()
                attr['cols'] = f.width()
            else:
                if isinstance(type, pytis.data.String):
                    maxlen = type.maxlen()
                else:
                    maxlen = None
                ctrl = g.field
                attr['size'] = f.width(maxlen)
                attr['maxlength'] = maxlen
            if isinstance(type, pytis.data.Password):
                attr['password'] = True
            else:
                attr['value'] = self._prefill.get(f.id()) or value.export()
        if not self._row.editable(f.id()):
            attr['disabled'] = True
            attr['name'] = None # w3m bug workaround (sends disabled fields)
        if type.not_null() and not isinstance(type, pytis.data.Boolean) and \
               (self._row.new() or not isinstance(type, (pytis.data.Password,
                                                         pytis.data.Binary))):
            sign = g.sup("*", cls="not-null")
        else:
            sign = ''
        label = g.label(f.label(), attr['id']) + sign + ":"
        field = ctrl(**attr)
        if isinstance(type, pytis.data.Password):
            attr['id'] = attr['id'] + '-verify-pasword'
            field += g.br() + ctrl(**attr)
        help = f.descr() and g.div(f.descr(), cls="help") or ''
        return (label, field, help)

    def _export_packed_field(self, g, field, label, ctrl, help):
        if self._allow_table_layout:
            if help is not None:
                ctrl += help
            if field.compact():
                td = '<td colspan="2">'+ label + g.br() +"\n"+ ctrl +'</td>'
            else:
                td = '<td valign="top" align="right" class="label">'+ label + \
                     '</td><td width="100%" class="ctrl" >'+ ctrl +'</td>'
            return '<tr>'+ td +'</tr>'
        else:
            rows = (concat(label, g.br()), concat(ctrl, g.br()))
            if help:
                rows += (help,)
            return g.div(rows, cls="field")
        
    def _export_fields(self, g, fields):
        rows = [self._export_packed_field(g, field, label, ctrl, help)
                for field, label, ctrl, help in fields]
        if self._allow_table_layout:
            rows = ["<table>"] + rows + ["</table>"]
        return concat(rows, separator="\n")
    

class ShowForm(LayoutForm):
    _MAXLEN = 100
    _CSS_CLS = 'show-form'
    
    def _export_field(self, exporter, f):
        g = exporter.generator()
        type = self._row[f.id()].type()
        if isinstance(type, pytis.data.Password):
            return None
        elif isinstance(type, pytis.data.Binary):
            buf = self._row[f.id()].value()
            if buf:
                size = ' (%s)' % format_byte_size(len(buf))
                uri = self._uri(self._row, f.id())

                label = buf.filename() or isinstance(type, pd.Image) \
                        and _("image") or _("file")
                if isinstance(type, pd.Image) and f.thumbnail():
                    src = self._uri(self._row, f.thumbnail())
                    if src:
                        label = g.img(src, alt=label+size)
                        size = ''
                        if uri == src:
                            uri = None
                if uri:
                    value = g.link(label, uri) + size
                else:
                    value = label + size
            else:
                value = ""
        elif isinstance(type, pytis.data.Boolean):
            value = self._row[f.id()].value() and _("Yes") or _("No")
        elif isinstance(type, pytis.data.Color):
            color = self._row[f.id()].export()
            value = g.span(color or '&nbsp;', cls="color-value") +' '+ \
                    g.span('&nbsp;', cls="color-display", style="background-color: %s;" % color)
        elif type.enumerator():
            value = self._row.display(f.id())
        else:
            value = self._row[f.id()].export()
            if len(value) > self._MAXLEN:
                value = g.textarea(f.id(), value=value, readonly=True,
                                       rows=min(f.height(), 5), cols=80)
                #end = value.find(' ', self._MAXLEN-20, self._MAXLEN)
                #value = concat(value[:(end != -1 and end or self._MAXLEN)],
                #               ' ... (', _("reduced"), ')')
        return (g.label(f.label(), f.id()) + ":", value, None)

    
class EditForm(LayoutForm, _SubmittableForm):
    _CSS_CLS = 'edit-form'
    
    def __init__(self, data, view, resolver, row, errors=(), **kwargs):
        super(EditForm, self).__init__(data, view, resolver, row, **kwargs)
        key = self._data.key()[0].id()
        self._hidden += [(k, v) for k, v in self._prefill.items()
                         if view.field(k) and not k in view.layout().order() and k!= key]
        #if not self._row.new():
        #    self._hidden += [(key,  self._row.format(key))]
        assert isinstance(errors, (tuple, list, str, unicode)), errors
        self._errors = errors
        binary = [id for id in self._view.layout().order()
                  if isinstance(self._row[id].type(), pytis.data.Binary)]
        self._enctype = (binary and 'multipart/form-data' or None)
        
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
    
        
class BrowseForm(Form):
    _CSS_CLS = 'browse-form'
    _HTTP_METHOD = 'GET'
    _LIMITS = (10, 50, 100, 200, 500)
    _DEFAULT_LIMIT = 50
    _SORTING_DIRECTIONS = ((pytis.data.ASCENDENT, 'asc'),
                           (pytis.data.DESCENDANT, 'desc'),
                           (None, 'none'))

    @classmethod
    def form_args(cls, req):
        """Convert request form state parameters to form constructor arguments.

        The 'req' argument is an instance of a class implementing the 'Request' API.  All request
        parameters not related to form state are ignored.

        Returns a dictionary of valid constructor keyword arguments related to form state.

        """
        limit, offset = [req.param(arg, '').isdigit() and int(req.param(arg)) or None
                         for arg in ('limit', 'offset')]
        if limit not in cls._LIMITS:
            limit = None
        if limit is None:
            offset = 0
        else:
            if offset is None:
                offset = 0
            if req.has_param('next'):
                offset += limit
            if req.has_param('prev') and offset >= limit:
                offset -= limit
        sorting = None
        if req.has_param('sort') and req.has_param('dir'):
            cid = str(req.param('sort'))
            trans = dict([(name, dir) for dir, name in cls._SORTING_DIRECTIONS])
            dir = trans.get(req.param('dir'))
            if dir:
                sorting = ((cid, dir),)
        return dict(limit=limit, offset=offset, sorting=sorting)
    
    def __init__(self, data, view, resolver, columns=None, condition=None, sorting=None,
                 limit=None, offset=0, **kwargs):
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
            between these pages.  None value
          
          offset -- determines the page within paged listing.  The number indicates the offset of
            the record within all the records of the current select.  The page, which contains this
            record will be displayed if possible.  If not (the listing is shorter than given
            number), the nearest page is displayed.
           
        See the parent classes for definition of the remaining arguments.

        """
        super(BrowseForm, self).__init__(data, view, resolver, **kwargs)
        self._columns = [view.field(id) for id in columns or view.columns()]
        self._condition = condition
        if sorting is None:
            sorting = self._view.sorting()
        if sorting is None:
            key = self._data.key()[0].id()
            sorting = ((key, pytis.data.ASCENDENT),)
        self._sorting = sorting
        if limit is None:
            limit = self._DEFAULT_LIMIT
        self._limit = limit
        self._offset = offset
        if sorting and isinstance(self._row[sorting[0][0]].type(), pytis.data.TreeOrder):
            self._tree_order_column = sorting[0][0]
        else:
            self._tree_order_column = None

    def _export_value(self, exporter, row, col):
        g = exporter.generator()
        key = col.id()
        type = col.type(self._data)
        if isinstance(type, pytis.data.Boolean):
            value = row[key].value() and _("Yes") or _("No")
        elif isinstance(type, pytis.data.Binary):
            value = "--"
        elif type.enumerator():
            value = self._row.display(key)
        else:
            value = row[key].export()
            if not isinstance(value, lcg.Localizable):
                value = g.escape(row.format(key))
        uri = self._uri(row, key)
        if uri:
            value = g.link(value, uri)
        if self._tree_order_column and col == self._columns[0]:
            order = row[self._tree_order_column].value()
            if order is not None:
                level = len(order.split('.')) - 2
                if level > 0:
                    indent = level * g.span(2*'&nbsp;', cls='tree-indent')
                    value = indent + '&bull;&nbsp;'+ value # &#8227 does not work in MSIE
        return value
            
    def _export_row(self, exporter, row, n):
        cells = [concat('<td%s>' %
                        (isinstance(row[c.id()].type(), pd.Number) and ' align="right"' or ''),
                        self._export_value(exporter, row, c),
                        '</td>')
                 for c in self._columns]
        return concat('<tr class="%s">' % (n % 2 and 'even' or 'odd'), cells, '</tr>')

    def _export_headings(self, exporter):
        g = exporter.generator()
        current_sorting_column, current_dir = self._sorting[0]
        directions = [dir for dir, name in self._SORTING_DIRECTIONS]
        def label(col):
            result = col.column_label()
            if not col.virtual():
                cid = col.id()
                if cid == current_sorting_column:
                    dir = current_dir
                else:
                    dir = None
                new_dir = directions[(directions.index(dir)+1) % len(directions)]
                arg = dict(self._SORTING_DIRECTIONS)[new_dir]
                uri = '%s?form-name=%s;sort=%s;dir=%s' % (self._handler, self._name, cid, arg)
                if self._limit is not None:
                    uri += ';limit=%s' % self._limit
                result = g.link(result, uri)
                if dir:
                    # Characters u'\u25be' and u'\u25b4' won't display in MSIE...
                    sign = dir == pytis.data.ASCENDENT and '&darr;' or '&uarr;'
                    result += ' '+ g.span(sign, cls='sorting-sign')
            return result
        return concat([concat('<th>', label(c), '</th>') for c in self._columns])
    
    def _wrap_exported_rows(self, exporter, rows, summary):
        n = len(self._columns)
        return concat('<table border="1">',
                      concat('<thead><tr>', self._export_headings(exporter), '</tr></thead>'),
                      concat('<tfoot><tr><td colspan="%d">' % n, summary, '</td></tr></tfoot>'),
                      '<tbody>', rows, '</tbody>', 
                      '</table>\n', separator="\n")

    def _export_body(self, exporter):
        data = self._data
        row = self._row
        limit = self._limit
        exported_rows = []
        self._count = count = data.select(condition=self._condition, sort=self._sorting)
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
            if limit is None or count < self._LIMITS[0]:
                summary = _("Total records:") +' '+ g.strong(str(count))
            else:
                summary = _("Displayed records %(first)s-%(last)s of total %(total)s",
                            first=g.strong(str(offset+1)),
                            last=g.strong(str(offset+n)),
                            total=g.strong(str(count)))
            return self._wrap_exported_rows(exporter, exported_rows, summary)

    def _export_controls(self, exporter):
        limit, page, count = self._limit, self._page, self._count
        if limit is None or count < self._LIMITS[0]:
            return None
        g = exporter.generator()
        offset_id = '%x-offset' % positive_id(self)
        limit_id = '%x-limit' % positive_id(self)
        result = (g.label(_("Page:"), offset_id),
                  g.select(name='offset', id=offset_id, selected=page*limit, 
                           options=[(str(i+1), i*limit) for i in range(count/limit+1)],
                           onchange='this.form.submit(); return true'),
                  '/',
                  g.strong(str(count/limit+1)),
                  g.submit(_("Previous"), name='prev', cls='prev', title=_("Go to previous page"),
                           disabled=(page == 0)),
                  g.submit(_("Next"),  name='next', cls='next', title=_("Go to next page"),
                           disabled=(page+1)*limit >= count),
                  g.span((g.label(_("Records per page:"), limit_id)+' ',
                          g.select(name='limit', id=limit_id,
                                   options=[(str(i), i) for i in self._LIMITS], selected=limit,
                                   onchange='this.form.submit(); return true')),
                         cls='limit'))
        if len(self._sorting) == 1:
            cid, dir = self._sorting[0]
            result += (g.hidden('sort', cid),
                       g.hidden('dir', dict(self._SORTING_DIRECTIONS)[dir]))
        return result + (g.submit(_("Go"), cls='hidden'),)
        

class CheckRowsForm(BrowseForm, _SubmittableForm):
    """Web form with checkable boolean columns in each row.

    The form is rendered as an ordinary table, but boolean columns (all or only the selected) are
    represented by a checkbox in each row and the form has submit controls.  Thus the user can
    modify the values in all rows and submit the changes in one step.

    *Processing the submitted form:*

    Each checkbox column is represented by one query parameter.  Its name is the column identifier
    and the values are the key column values of all checked rows.
    
    """
    _DEFAULT_LIMIT = None
    def __init__(self, data, view, resolver, check_columns=(), **kwargs):
        """Initialize the instance.

        Arguments:

          check_columns -- a sequence of column identifiers for which the checkboxes will be
            created.  If the argument is omitted, checkboxes will automatically appear for all
            boolean columns.

          See the parent classes for definition of the remaining arguments.

        """
        super(CheckRowsForm, self).__init__(data, view, resolver, **kwargs)
        assert isinstance(check_columns, (list, tuple)), check_columns
        if __debug__:
            for cid in check_columns:
                assert self._row.has_key(cid), cid
                assert isinstance(self._row[cid].type(), pd.Boolean), cid
        self._check_columns = check_columns

    def _export_value(self, exporter, row, col):
        cid = col.id()
        if cid in self._check_columns or \
               not self._check_columns and isinstance(self._row[cid].type(), pd.Boolean):
            key = self._data.key()[0].id()
            return exporter.generator().checkbox(name=cid, value=self._row.format(key),
                                                 checked=self._row[cid].value()),
        else:
            return super(CheckRowsForm, self)._export_value(exporter, row, col)

    
