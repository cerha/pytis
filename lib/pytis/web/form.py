# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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
from lcg.export import _html
from lcg import concat

_ = lcg.TranslatableTextFactory('pytis')

# TODO: This hack is necessary as long as pytis default language is Czech.
pd = pytis.data
pd.Type._VM_NULL_VALUE_MSG = _("Empty value")
pd.Type._VM_INVALID_VALUE_MSG = _("Invalid value")
pd.Integer._VM_NONINTEGER_MSG = _("Not an integer")
pd.Float._VM_INVALID_NUMBER_MSG = _("Invalid number")
pd.String._VM_STRING_TOO_LONG_MSG = _("String exceeds max length %(maxlen)d")
pd.Color._VM_FORMAT_MSG = _("Invalid color format ('#RGB' or '#RRGGBB')")
pd.Identifier._VM_FORMAT_MSG = _("Invalid format")
pd.DateTime._VM_DT_FORMAT_MSG = _("Invalid date or time format")
pd.DateTime._VM_DT_VALUE_MSG = _("Invalid date or time")
pd.DateTime._VM_DT_AGE_MSG = _("Date outside the allowed range")


class Form(lcg.Content):

    def __init__(self, data, view, resolver, row=None, prefill=None,
                 new=False):
        super(Form, self).__init__()
        assert isinstance(data, pytis.data.Data), data
        assert isinstance(view, ViewSpec), view
        assert isinstance(resolver, pytis.util.Resolver), resolver
        assert row is None or isinstance(row, pytis.data.Row), row
        self._data = data
        self._view = view
        self._prefill = prefill or {}
        self._row = PresentedRow(view.fields(), data, row, resolver=resolver,
                                 prefill=self._valid_prefill(), new=new)
        
    def _valid_prefill(self):
        # Return a dictionary of Python values for the prefill argument.
        valid = {}
        kc = [c.id() for c in self._data.key()]
        for id in self._view.layout().order():
            if id not in kc and self._prefill.has_key(id):
                f = self._view.field(id)
                type = f.type(self._data)
                value, error = type.validate(self._prefill[id], strict=False)
                if not error:
                    valid[id] = value
        return valid


class LayoutForm(Form):
    
    def _export_group(self, group):
        #orientation = orientation2wx(group.orientation())
        #space = dlg2px(parent, group.space())
        #gap = dlg2px(parent, group.gap())
        #border = dlg2px(parent, group.border())
        #border_style = border_style2wx(group.border_style())
        if group.label() is not None:
            pass
        else:
            pass
        pack = []
        result = []
        for item in group.items():
            if isinstance(item, Button):
                continue
            item = self._view.field(item)
            if item.width() == 0:
                continue
            if isinstance(item, FieldSpec):
                label, ctrl = self._export_field(item)
                result.append((item, label, ctrl))
        if group.orientation() == Orientation.VERTICAL:
            x = self._export_pack(result)
        else:
            x = None
        if group.label():
            x = _html.fieldset(x, label=group.label())
        return x
        
    def _export_field(self, f):
        value = self._row[f.id()]
        type = f.type(self._data)
        attr = {'name': f.id(),
                'id': "%x-%s" % (positive_id(self), f.id())}
        if isinstance(type, pytis.data.Boolean):
            ctrl = _html.checkbox
            attr['value'] = 'T'
            attr['checked'] = value.value()
        elif type.enumerator():
            ctrl = _html.select
            attr['options'] = [("&nbsp;", "")] + \
                              [(uv, str(v)) for v, uv
                               in self._row.enumerate(f.id())]
            if value.value() in type.enumerator().values():
                attr['selected'] = str(value.value())
        else:
            if f.height() > 1:
                ctrl = _html.textarea
                attr['rows'] = f.height()
                attr['cols'] = f.width()
            else:
                if isinstance(type, pytis.data.String):
                    maxlen = type.maxlen()
                else:
                    maxlen = None
                ctrl = _html.field
                attr['size'] = f.width(maxlen)
                attr['maxlength'] = maxlen
            attr['value'] = self._prefill.get(f.id()) or value.export()
        if not self._row.editable(f.id()):
            attr['disabled'] = True
        return (_html.label(f.label(), attr['id']) + ":", ctrl(**attr))

    def _export_packed_field(self, field, label, ctrl):
        if field.compact():
            return concat('<tr><td colspan="2">', label, '<br/>', ctrl,
                          '</td></tr>')
        else:
            return concat('<tr><td valign="top" align="right" class="label">',
                          label,
                          '</td><td width="100%" class="ctrl">',
                          ctrl,
                          '</td></tr>')
        
    def _export_pack(self, pack):
        rows = [self._export_packed_field(field, label, ctrl)
                for field, label, ctrl in pack]
        return concat("<table>", rows, "</table>", separator="\n")
    
#             if group.orientation() == Orientation.VERTICAL \
#                    and (isinstance(item, InputField)
#                         and not item.spec().compact() \
#                         or isinstance(item, Button)):
#                 # This field will become a part of current pack.
#                 pack.append(item)
#                 continue
#             if len(pack) != 0:
#                 # Add the latest pack into the sizer (if there was one).
#                 sizer.Add(self._pack_fields(parent, pack, space, gap),
#                           0, wx.ALIGN_TOP|border_style, border)
#                 pack = []
#             if isinstance(item, GroupSpec):
#                 x = self._create_group(parent, item)
#             elif isinstance(item, InputField):
#                 if  item.spec().compact():
#                     # This is a compact field (not a part of the pack).
#                     x = wx.BoxSizer(wx.VERTICAL)
#                     x.Add(item.label(), 0, wx.ALIGN_LEFT)
#                     x.Add(item.widget())
#                 else:
#                     # This only happens in a HORIZONTAL group.
#                     x = self._pack_fields(parent, (item,), space, gap)
#             else:
#                 x = self._create_button(parent, item)
#             sizer.Add(x, 0, wx.ALIGN_TOP|border_style, border)
#         if len(pack) != 0:
#             # přidej zbylý sled políček (pokud nějaký byl)
#             sizer.Add(self._pack_fields(parent, pack, space, gap),
#                       0, wx.ALIGN_TOP|border_style, border)
#         # pokud má skupina orámování, přidáme ji ještě do sizeru s horním
#         # odsazením, jinak je horní odsazení příliš malé.
#         if group.label() is not None:
#             s = wx.BoxSizer(orientation)
#             s.Add(sizer, 0, wx.TOP, 2)
#             sizer = s
#         return sizer

    def _pack_fields(self, parent, items, space, gap):
        """Sestav skupinu pod sebou umístěných políček/tlačítek do gridu.

        Argumenty:

          items -- sekvence identifikátorů políček nebo instancí Button.
          space -- mezera mezi ovládacím prvkem a labelem políčka v dlg units;
            integer
          gap -- mezera mezi jednotlivými políčky v dlg units; integer

        Pro každý prvek skupiny vytvoří tlačítko nebo políčko
        'inputfield.InputField' a přidá jeho label a widget do vytvořené
        instance 'wx.FlexGridSizer'.

        Vrací: instanci 'wx.FlexGridSizer' naplněnou políčky a tlačítky.

        """
        grid = wx.FlexGridSizer(len(items), 2, gap, space)
        for item in items:
            if isinstance(item, Button):
                button = self._create_button(parent, item)
                style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                label = wx.StaticText(parent, -1, "",
                                      style=wx.ALIGN_RIGHT)
                grid.Add(label, 0, style, 2)
                grid.Add(button)                
            else:    
                if item.height() > 1:
                    style = wx.ALIGN_RIGHT|wx.ALIGN_TOP|wx.TOP
                else:
                    style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                grid.Add(item.label(), 0, style, 2)
                grid.Add(item.widget())
        return grid


class ShowForm(LayoutForm):
    _MAXLEN = 100
    
    def export(self, exporter):
        group = self._export_group(self._view.layout().group())
        return _html.form(_html.fieldset(group, cls='body'),
                          cls="show-form") + "\n"

    def _export_field(self, f):
        type = f.type(self._data)
        if isinstance(type, pytis.data.Boolean):
            value = self._row[f.id()].value() and _("Yes") or _("No")
        elif type.enumerator():
            value = self._row.display(f.id())
        else:
            value = self._row[f.id()].export()
        if len(value) > self._MAXLEN:
            end = value.find(' ', self._MAXLEN-20, self._MAXLEN)
            value = concat(value[:(end != -1 and end or self._MAXLEN)],
                           ' ... (', _("reduced"), ')')
        return (_html.label(f.label(), f.id()) + ":", value)

    
class EditForm(LayoutForm):
    
    def __init__(self, data, view, resolver, row, handler='#', action=None,
                 errors=(), **kwargs):
        super(EditForm, self).__init__(data, view, resolver, row, **kwargs)
        assert isinstance(handler, str), handler
        assert action is None or isinstance(action, str), action
        assert isinstance(errors, (tuple, list, str, unicode)), errors
        self._handler = handler
        self._action = action
        self._errors = errors

    def export(self, exporter):
        if isinstance(self._errors, (str, unicode)):
            errors = concat("<p>", self._errors, "</p>")
        else:
            errors = [concat("<p>", self._view.field(id).label(), ": ",
                             msg, "</p>\n")
                      for id, msg in self._errors]
        if errors:
            errors = _html.div(errors, cls='errors')
        group = self._export_group(self._view.layout().group())
        content = concat(errors,
                         _html.fieldset(group, cls='body'),
                         self._export_buttons())
        return _html.form(content, action=self._handler, method='POST',
                          cls="edit-form") + "\n"

    def _export_buttons(self):
        hidden = []
        if self._action:
            hidden += [('action', self._action)]
        if not self._row.new():
            hidden += [(c.id(),  self._row.format(c.id()))
                       for c in self._row.data().key()]
        result = [_html.hidden(k, v) for k, v in hidden] + \
                 [_html.submit(_("Submit")),
                  _html.reset(_("Reset"))]
        return _html.fieldset(result, cls="submit")
        
class BrowseForm(Form):

    def __init__(self, data, view, resolver, rows, link_provider=None):
        super(BrowseForm, self).__init__(data, view, resolver)
        assert isinstance(rows, (list, tuple)), rows
        self._rows = rows
        self._columns = [view.field(id) for id in view.columns()]
        self._link_provider = link_provider

    def _export_field(self, row, col):
        type = col.type(self._data)
        if isinstance(type, pytis.data.Boolean):
            value = row[col.id()].value() and _("Yes") or _("No")
        else:
            value = row[col.id()].value()
            if not isinstance(value, lcg.Localizable):
                value = _html.escape(row.format(col.id()))
        if self._link_provider:
            uri = self._link_provider(row, col)
            if uri:
                value = _html.link(value, uri)
        #cb = col.codebook(self._row.data())
        #if cb:
        #    value += " (%s)" % cb
        return value
        
    def _export_cell(self, row, col):
        return concat('<td>', self._export_field(row, col), '</td>')

    def _export_row(self, exporter, row):
        cells = concat([self._export_cell(row, c) for c in self._columns])
        return concat('<tr>', cells, '</tr>')
        
    def _wrap_exported_rows(self, rows):
        th = [concat('<th>', c.column_label(), '</th>') for c in self._columns]
        return concat('<table border="1" class="browse-form">',
                      concat('<tr>', th, '</tr>'), rows,
                      '</table>\n', separator="\n")

    def export(self, exporter):
        if not self._rows:
            return _html.strong(_("No records."))
        rows = []
        row = self._row
        for r in self._rows:
            row.set_row(r)
            rows.append(self._export_row(exporter, row))
        return self._wrap_exported_rows(rows)
        

