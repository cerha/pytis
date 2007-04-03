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

This module provides an implementations of Pytis forms, which can be used for
web interfaces to Pytis informations systems.  Ideally, we should be able to
generate web forms from the same specification.  Since some aspects of the
specifications are bound to a GUI environment (such as callback functions,
which run dialogs and forms directly), not all features are currently
available.  However the core functionality is implemented and data can be
viewed and manipulated (inserted, edited) using web forms.

There is currently no support for running the actual web application.  Only
form classes are available and it's up to the user to manipulate them.  One
example of such an application is the Wiking Content Management System
(http://www.freebsoft.org/wiking).

All the content generation is done using the LCG framework.  See
http://www.freebsoft.org/lcg.

The classes defined below are currently just prototypes, so they are not
documented yet and the API may change drastically!

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
pd.Limited._VM_MAXLEN_MSG = _("Maximal length %(maxlen)d exceeded")
pd.Integer._VM_NONINTEGER_MSG = _("Not an integer")
pd.Float._VM_INVALID_NUMBER_MSG = _("Invalid number")
pd.String._VM_MAXLEN_MSG = _("String exceeds max length %(maxlen)d")
pd.Password._VM_PASSWORD_MSG = _("Enter the password twice to eliminate typos")
pd.Password._VM_PASSWORD_VERIFY_MSG = _("Passwords don't match")
pd.RegexString._VM_FORMAT_MSG = _("Invalid format")
pd.Color._VM_FORMAT_MSG = _("Invalid color format ('#RGB' or '#RRGGBB')")
pd.DateTime._VM_DT_FORMAT_MSG = _("Invalid date or time format")
pd.DateTime._VM_DT_VALUE_MSG = _("Invalid date or time")
pd.DateTime._VM_DT_AGE_MSG = _("Date outside the allowed range")
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
    # Needed to postpone variable interpolation to the translation time.
    pd.Type._validation_error = _validation_error


class Form(lcg.Content):

    def __init__(self, data, view, resolver, row=None, prefill=None,
                 new=False, link_provider=None, **kwargs):
        super(Form, self).__init__(**kwargs)
        assert isinstance(data, pytis.data.Data), data
        assert isinstance(view, ViewSpec), view
        assert isinstance(resolver, pytis.util.Resolver), resolver
        assert row is None or isinstance(row, pytis.data.Row), row
        self._data = data
        self._view = view
        self._prefill = prefill or {}
        self._link_provider = link_provider
        self._row = PresentedRow(view.fields(), data, row, resolver=resolver,
                                 prefill=self._valid_prefill(), new=new)

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
                value, error = type.validate(self._prefill[id], strict=False)
                if not error:
                    valid[id] = value
        return valid


class LayoutForm(Form):

    def __init__(self, *args, **kwargs):
        self._allow_table_layout = kwargs.pop('allow_table_layout', True)
        super(LayoutForm, self).__init__(*args, **kwargs)
    
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
            result = exporter.generator().fieldset(result,
                                                   legend=group.label(),
                                                   cls='body')
        else:
            result = concat(result, separator="\n")
        #if group.orientation() == Orientation.VERTICAL:
        return result
        
    def _export_field(self, exporter, f):
        g = exporter.generator()
        value = self._row[f.id()]
        type = f.type(self._data)
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
            attr['options'] = [("&nbsp;", "")] + \
                              [(uv, str(v)) for v, uv
                               in self._row.enumerate(f.id())]
            if value.value() in type.enumerator().values():
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
    
    def export(self, exporter):
        group = self._export_group(exporter, self._view.layout().group())
        return exporter.generator().form(group, cls="show-form") + "\n"

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
                    value = label
            else:
                value = ""
        elif isinstance(type, pytis.data.Boolean):
            value = self._row[f.id()].value() and _("Yes") or _("No")
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

    
class EditForm(LayoutForm):
    
    def __init__(self, data, view, resolver, row, handler='#', action=None,
                 errors=(), hidden=(), **kwargs):
        super(EditForm, self).__init__(data, view, resolver, row, **kwargs)
        assert isinstance(handler, str), handler
        assert action is None or isinstance(action, str), action
        assert isinstance(errors, (tuple, list, str, unicode)), errors
        assert isinstance(hidden, (tuple, list)), hidden
        self._handler = handler
        self._action = action
        self._errors = errors
        self._hidden = list(hidden)

    def export(self, exporter):
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
        group = self._export_group(exporter, self._view.layout().group())
        content = concat(errors, group, self._export_buttons(exporter))
        binary = [id for id in self._view.layout().order()
                  if isinstance(self._row[id].type(), pytis.data.Binary)]
        return g.form(content, action=self._handler, method='POST',
                      enctype=(binary and 'multipart/form-data' or None),
                      cls="edit-form") + "\n"
                      
    def _export_buttons(self, exporter):
        g = exporter.generator()
        key = self._data.key()[0].id()
        order = self._view.layout().order()
        hidden = self._hidden
        hidden += [(k, v) for k, v in self._prefill.items()
                   if self._view.field(k) and not k in order and k!= key]
        #if not self._row.new():
        #    hidden += [(key,  self._row.format(key))]
        if self._action:
            hidden += [('action', self._action)]
        result = [g.hidden(k, v) for k, v in hidden] + \
                 [g.submit(_("Submit")), g.reset(_("Reset"))]
        return g.fieldset(result, cls="submit") + \
               g.div("*) " + _("Fields marked by an asterisk are mandatory."),
                     cls='help')
    
        
class BrowseForm(Form):

    def __init__(self, data, view, resolver, rows, **kwargs):
        super(BrowseForm, self).__init__(data, view, resolver, **kwargs)
        assert isinstance(rows, (list, tuple)), rows
        self._rows = rows
        self._columns = [view.field(id) for id in view.columns()]

    def _export_value(self, exporter, row, col):
        g = exporter.generator()
        type = col.type(self._data)
        if isinstance(type, pytis.data.Boolean):
            value = row[col.id()].value() and _("Yes") or _("No")
        elif isinstance(type, pytis.data.Binary):
            value = "--"
        elif type.enumerator():
            value = self._row.display(col.id())
        else:
            value = row[col.id()].export()
            if not isinstance(value, lcg.Localizable):
                value = g.escape(row.format(col.id()))
        uri = self._uri(row, col.id())
        if uri:
            value = g.link(value, uri)
        return value
            
    def _export_cell(self, exporter, row, col):
        return 
        
    def _export_row(self, exporter, row):
        cells = [concat('<td>', self._export_value(exporter, row, c), '</td>')
                 for c in self._columns]
        return concat('<tr>', cells, '</tr>')
        
    def _wrap_exported_rows(self, exporter, rows):
        th = [concat('<th>', c.column_label(), '</th>') for c in self._columns]
        return concat('<table border="1" class="browse-form">',
                      concat('<tr>', th, '</tr>'), rows,
                      '</table>\n', separator="\n")

    def export(self, exporter):
        if not self._rows:
            return exporter.generator().strong(_("No records."))
        rows = []
        row = self._row
        for r in self._rows:
            row.set_row(r)
            rows.append(self._export_row(exporter, row))
        return self._wrap_exported_rows(exporter, rows)
        

