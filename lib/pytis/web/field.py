# -*- coding: utf-8 -*-

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

import re
import string

from pytis.web import *

_ = lcg.TranslatableTextFactory('pytis')


class UriType(object):
    """URI type for 'uri_provider' 'type' argument.

    URI provider is a function passed to the form constructor (and from there to
    the field constructors) that returns different kinds of URIs for form
    fields.  The constants defined by this class define the different kinds of
    links which may be queried.

    """
    LINK = 'LINK'
    """Target of a link to which the field value references.

    If an URI is returned by URI provider for this URI kind and given field, the
    field value is rendered as a link pointing to given URI.  If None is
    returned, the field value is not rendered as a link.  The returned URI may
    be either a string or unicode or a 'Link' instance.

    """
    IMAGE = 'IMAGE'
    """URI of an image which should be used to represent the field value.

    If an URI is returned by URI provider for this URI kind and given field, the
    field value is rendered as an image and the formatted text value of the
    field is used as image textual title.  If None is returned, the field value
    is not rendered as an image (the formatted textual value is displayed
    directly).

    """
    PRINT = 'PRINT'
    """URI of a print field link for printable fields.

    If an URI is returned by URI provider for this URI kind and given field, the
    field will render a user interface control supposed to export the field's
    value into a PDF document.  The application is responsible for handling
    given URI by returning an 'application/pdf' content representing the printed
    field value.  If None is returned, the field is not supposed to be
    printable.  The provider will only be queried for fields with printable=True
    in their specification for this kind of URI.
    
    """

class Link(object):
    """Link representation for 'uri_provider' returned value.

    The value returned by URI provider (function passed to the form constructor)
    is normally a string or unicode containing the URI.  If the URI provider
    wants to also specify the title or target of the link (as defined by the
    corresponding HTML A tag attributtes) it may return an instance of this
    class instead of a string.
    
    """
    def __init__(self, uri, title=None, target=None):
        self._uri = uri
        self._title = title
        self._target = target
    def uri(self):
        return self._uri
    def title(self):
        return self._title
    def target(self):
        return self._target

def localizable_datetime(value):
    """Try to convert a 'pytis.data.DateTime' value to 'lcg.LocalizableDateTime'.

    Arguemnts:
      value -- 'pytis.data.Value' instance to be converted.

    'lcg.LocalizableDateTime' instances may be included in LCG element's export
    result and will be automatically formatted according to the target locale
    during LCG export.  The function returns a string if the conversion is not
    possible (see below) or necessary (for null values).

    The conversion is applied only to base pytis.data.Date type and not to its
    descendants.  The derived classes may customize the export and since we are
    replacing the type's export here, the customized export would be igored.
    Thus it is safer to limit special handling to direct pytis.data.Date
    instances here, althought it is unpleasant, that derived types are not
    localized automatically.
    
    """
    if value.value() is not None:
        type_cls = value.type().__class__
        if type_cls is pytis.data.DateTime:
            format = '%Y-%m-%d %H:%M:%S'
            localizable = lcg.LocalizableDateTime
        elif type_cls is pytis.data.Date:
            format = '%Y-%m-%d'
            localizable = lcg.LocalizableDateTime
        elif type_cls is pytis.data.Time: 
            format = '%H:%M:%S'
            localizable = lcg.LocalizableTime
        else:
            return value.export()
        return localizable(value.value().strftime(format))
    else:
        return ''
    
    
class Field(object):
    """Internal form field representation (all attributes are read-only)."""
    def __init__(self, row, spec, type, form, uri_provider):
        self.spec = spec
        self.type = type
        self.unique_id = "f%x" % positive_id(self)
        # Duplicate selected specification options for simplified access
        self.id = spec.id()
        self.style = spec.style()
        self.label = spec.label()
        self.column_label = spec.column_label()
        self.label = spec.label()
        self.virtual = spec.virtual()
        # Initialize the exporter just once here to do most of the
        # decision-making and type checking during initialization.  This speeds
        # up the actual formatting, which is typically performed many times.
        if isinstance(type, pytis.data.Boolean):
            exporter = BooleanFieldExporter
        elif isinstance(type, pytis.data.Password):
            exporter = PasswordFieldExporter
        elif isinstance(type, pytis.data.Color):
            exporter = ColorFieldExporter
        elif isinstance(type, pytis.data.Binary):
            exporter = BinaryFieldExporter
        elif isinstance(type, pytis.data.Date):
            exporter = DateFieldExporter
        elif isinstance(type, pytis.data.DateTime):
            exporter = DateTimeFieldExporter
        elif isinstance(type, pytis.data.Array):
            inner_type = type.inner_type()
            if inner_type.enumerator():
                exporter = ChecklistFieldExporter
            else:
                raise Exception("Unsupported array field.")
        elif type.enumerator():
            selection_type = spec.selection_type()
            if selection_type == SelectionType.RADIO:
                exporter = RadioFieldExporter
            elif selection_type in (SelectionType.CHOICE, None):
                exporter = ChoiceFieldExporter
            else:
                exporter = CodebookFieldExporter
        elif spec.filename():
            exporter = FileFieldExporter
        elif isinstance(type, pytis.data.StructuredText):
            exporter = StructuredTextFieldExporter
        elif isinstance(type, pytis.data.SimpleFormattedText):
            exporter = SimpleFormattedTextFieldExporter
        elif spec.height() > 1:
            exporter = MultilineFieldExporter
        elif isinstance(type, pytis.data.String):
            exporter = StringFieldExporter
        else:
            exporter = TextFieldExporter
        self.exporter = exporter(row, self, form, uri_provider)


class FieldExporter(object):
    """Field value exporter for both read-only and editable field representations.

    Specific exporter classes are defined below for particular field types.
    The public methods 'format()' and 'editor()' implement the export for
    read-only and editable representation respectively (see below).
    
    """
    _HANDLER = 'pytis.Field'
    
    def __init__(self, row, field, form, uri_provider):
        self._row = row
        self._field = field
        self._showform = isinstance(form, ShowForm)
        self._uri_provider = uri_provider
        self._format_cache = {}
        self._format_cache_context = None

    def _format(self, context):
        """Return the formatted field value as a (localizable) string.

        This method is called as part of the public method 'format()' to
        provide the field's exported value.  The final result returned by
        'format()' will also include the additional information returned by the
        method '_display()' (if not None).  For fields represented by links,
        value returned by '_format() will be used as the link label and the
        '_display()' result will be added outside the link.

        """
        field = self._field
        value = self._value().export()
        if value and not isinstance(value, lcg.Localizable):
            g = context.generator()
            value = g.escape(self._row.format(field.id))
            lines = value.splitlines()
            if len(lines) > 1:
                if self._showform and len(lines) > field.spec.height()+2:
                    width = field.spec.width()
                    value = g.textarea(field.id, value=value, readonly=True,
                                       rows=min(field.spec.height(), 8), cols=width,
                                       cls=width >= 80 and 'fullsize' or None)
                else:
                    # Insert explicit linebreaks for non-css browsers.
                    value = g.span(g.br().join(lines), cls='multiline')
        return value

    def _display(self, context):
        """Additional information about the field value (see '_format()' for more info)."""
        return None

    def _value(self):
        """Return the field value as a 'pytis.data.Value' instance."""
        return self._row[self._field.id]
    
    def _editor_kwargs(self, context, prefill, error):
        """Return editor field keyword arguemnts as a dictionary.

        In this class the method returns the basic set of arguemnts common to
        all fields types.  Derived classes will usually just add field specific
        arguments (or may return completely different arguments when the field
        type requires it).

        """
        field = self._field
        return dict(id=field.unique_id,
                    name=field.id,
                    disabled=not self._row.editable(field.id) or None,
                    cls=error and 'invalid' or None)

    def _editor(self, context, **kwargs):
        """Return the field editor control as HTML string.

        The keyword arguments passed to this method are the values returned by
        '_editor_kwargs()'.  This allows easier customization of the final
        result by independent overriding of either of the two methods.

        """
        return None

    def format(self, context):
        """Return the exported read-only field representation."""
        if self._format_cache_context is not context:
            self._format_cache = {}
            self._format_cache_context = context
        fid = self._field.id
        field_value = self._value().value()
        try:
            value_info = self._format_cache.get(field_value)
        except TypeError:               # catch unhashable keys
            value_info = None
        if value_info is None:
            value = self._format(context)
            info = self._display(context)
            try:
                self._format_cache[field_value] = (value, info,)
            except TypeError:           # catch unhashable keys
                pass
        else:
            value, info = value_info
        if value and self._uri_provider:
            g = context.generator()
            src = self._uri_provider(self._row, fid, type=UriType.IMAGE)
            if src:
                if info is not None:
                    value += ' ('+ info +')'
                    info = None
                value = g.img(src, alt=value) #, cls=cls)
            link = self._uri_provider(self._row, fid, type=UriType.LINK)
            if link:
                if callable(link):
                    pass # Ignore array item links here
                elif type(link) in (str, unicode):
                    value = g.link(value, link)
                else:
                    value = g.link(value, link.uri(), title=link.title(), target=link.target())
            if info is not None:
                value += ' ('+ info +')'
        return value

    def editor(self, context, prefill=None, error=None):
        """Return the exported editable field control."""
        kwargs = self._editor_kwargs(context, prefill, error)
        return self._editor(context, **kwargs)

    def handler(self, context, form_id, active, required):
        """Return javascript code for creation of a field handler instance."""
        g = context.generator()
        return g.js_call("new %s" % self._HANDLER, form_id, self._field.unique_id,
                         self._field.id, active, required)


class TextFieldExporter(FieldExporter):

    def _maxlen(self):
        return None
    
    def _editor_kwargs(self, context, prefill, error):
        kwargs = super(TextFieldExporter, self)._editor_kwargs(context, prefill, error)
        value = prefill or self._value().export()
        maxlen = self._maxlen()
        size = self._field.spec.width(maxlen)
        return dict(kwargs, value=value, size=size, maxlength=maxlen)

    def _editor(self, context, **kwargs):
        return context.generator().field(**kwargs)

    
class StringFieldExporter(TextFieldExporter):
    
    def _maxlen(self):
        return self._field.type.maxlen()
    

class PasswordFieldExporter(StringFieldExporter):
    
    def _format(self, context):
        if self._showform:
            return None
        else:
            return super(PasswordFieldExporter, self)._format(context)

    def _editor(self, context, **kwargs):
        g = context.generator()
        result = g.field(password=True, **kwargs)
        if self._field.type.verify():
            kwargs['id'] += '-verify-pasword'
            result += g.br() + g.field(password=True, **kwargs)
        return result


class MultilineFieldExporter(FieldExporter):

    def _editor_kwargs(self, context, prefill, error):
        kwargs = super(MultilineFieldExporter, self)._editor_kwargs(context, prefill, error)
        value = prefill or self._value().export()
        width, height = self._field.spec.width(), self._field.spec.height()
        cls = kwargs.get('cls')
        if width >= 80:
            cls = (cls and cls+' ' or '') + 'fullsize'
        return dict(kwargs, value=value, rows=height, cols=width, cls=cls)

    def _editor(self, context, **kwargs):
        return context.generator().textarea(**kwargs)

    
class StructuredTextFieldExporter(MultilineFieldExporter):

    def __init__(self, *args, **kwargs):
        self._parser = lcg.Parser()
        super(StructuredTextFieldExporter, self).__init__(*args, **kwargs)

    def _format(self, context):
        blocks = self._parser.parse(context.translate(self._value().export()))
        if len(blocks) == 1 and isinstance(blocks[0], lcg.Paragraph):
            content = lcg.Container(blocks[0].content())
        else:
            content = lcg.Container(blocks)
        content.set_parent(context.node())
        return context.generator().div(content.export(context))

class SimpleFormattedTextFieldExporter(MultilineFieldExporter):

    def _format(self, context):
        text = self._value().export()
        nbsp = '&nbsp;'; len_nbsp = len(nbsp)
        lt = '&lt;'; len_lt = len(lt)
        gt = '&gt;'; len_gt = len(gt)
        def convert_line(line):
            line_length = len(line)
            i = 0
            while i < line_length and line[i] == ' ':
                i += 1
            if i > 0:
                line = nbsp*i + line[i:]
                line_length += (len_nbsp - 1) * i
                i = len_nbsp * i
            while i < line_length:
                if line[i] == '<':
                    line = line[:i] + lt + line[i+1:]
                    line_length += len_lt - 1
                    i += len_lt
                elif line[i] == '>':
                    line = line[:i] + gt + line[i+1:]
                    line_length += len_gt - 1
                    i += len_gt
                elif line[i] == ' ':
                    j = i + 1
                    while j < line_length and line[j] == ' ':
                        j += 1
                    if j > i + 1:
                        line = line[:i] + nbsp*(j-i) + line[j:]
                        line_length += (len_nbsp - 1) * (j - i)
                        i += len_nbsp * (j - i)
                    else:
                        i += 1
                else:
                    i += 1
            return line
        lines = [convert_line(l) for l in text.splitlines()]
        # Join lines and substitute links for HTML links
        converted_text = re.sub(
            r'(https?://.+?)(?=[\),.:;?!\]]?(\s|&nbsp;|&lt;|&gt;|<br/?>|$))',
            r'<a href="\1">\1</a>',
            string.join(lines, '<br>\n'))
        return context.generator().div(converted_text)

class DateTimeFieldExporter(TextFieldExporter):
    
    def _format(self, context):
        return localizable_datetime(self._value())
    
    def _editor_kwargs(self, context, prefill, error):
        kwargs = super(DateTimeFieldExporter, self)._editor_kwargs(context, prefill, error)
        # Use localizable values also inside editor fields (exported value is used by default).
        return dict(kwargs, value=self._format(context))
    
class DateFieldExporter(DateTimeFieldExporter):
    
    def _maxlen(self):
        return 10
    
    def _editor(self, context, **kwargs):
        result = super(DateFieldExporter, self)._editor(context, **kwargs)
        g = context.generator()
        result += g.script_write(g.button('...', id='%s-button' % kwargs['id'], type='button',
                                          cls='selection-invocation calendar-invocation',
                                          disabled=kwargs['disabled']))
        if not kwargs['disabled']:
            context.resource('prototype.js')
            context.resource('calendarview.js')
            context.resource('calendarview.css')
            locale_data = context.locale_data()
            js_values = dict(
                id = kwargs['id'],
                format = locale_data.date_format,
                today = context.translate(_(u"today")),
                day_names = g.js_value([context.translate(lcg.week_day_name(i, abbrev=True))
                                        for i in (6,0,1,2,3,4,5)]),
                month_names = g.js_value([context.translate(lcg.month_name(i))
                                          for i in range(12)]),
                first_week_day = (locale_data.first_week_day + 1) % 7,
                )
            result += g.script("""
               Calendar.setup({dateField: '%(id)s',
                               triggerElement: '%(id)s-button',
                               dateFormat: '%(format)s'});
               Calendar.TODAY = '%(today)s';
               Calendar.SHORT_DAY_NAMES = %(day_names)s;
               Calendar.MONTH_NAMES = %(month_names)s;
               Calendar.FIRST_WEEK_DAY = %(first_week_day)d;
               """ % js_values)
        return result
class ColorFieldExporter(StringFieldExporter):

    def _format(self, context):
        g = context.generator()
        color = self._value().export()
        return g.span(color or '&nbsp;', cls="color-value") +' '+ \
               g.span('&nbsp;', cls="color-display", style="background-color: %s;" %color)


class BooleanFieldExporter(FieldExporter):
    _HANDLER = 'pytis.CheckboxField'

    def _format(self, context):
        # Translators: Boolean value display.  Should be Yes/No in the meaning On/Off.
        fid = self._field.id
        return self._row.display(fid) or self._row[fid].value() and _(u"Yes") or _(u"No")

    def _editor(self, context, **kwargs):
        return context.generator().checkbox(value='T', checked=self._value().value(), **kwargs)


class BinaryFieldExporter(FieldExporter):

    def _format(self, context):
        buf = self._value().value()
        if buf:
            # Translators: The label "image"/"file" is used in textual representation of binary
            # data values, usually as a link to download the actual binary file.
            return buf.filename() or isinstance(type, pd.Image) and _(u"image") or _(u"file")
        else:
            return ""

    def _display(self, context):
        buf = self._value().value()
        if buf:
            return format_byte_size(len(buf))
        else:
            return None

    def _editor(self, context, **kwargs):
        return context.generator().upload(**kwargs)

    
class CodebookFieldExporter(FieldExporter):
    
    def _format(self, context):
        fid = self._field.id
        if self._row.prefer_display(fid):
            return self._row.display(fid)
        value = self._row[fid].export()
        if self._showform:
            # The display value is returned by the _display method in this case...
            return value
        display = self._row.display(fid)
        if display and value:
            return context.generator().abbr(value, title=display)
        else:
            return value
        
    def _display(self, context):
        # Display is only shown in showform.  In other form types it is shown
        # as ABBR within the value (see _format).  This is quite a hack and
        # should probably be handled in the same way for all field types on the
        # level of the form.
        if self._showform and not self._row.prefer_display(self._field.id):
            return self._row.display(self._field.id) or None
        else:
            return None

    def _enumeration(self, context):
        g = context.generator()
        type = self._field.type
        if isinstance(type, pytis.data.Array):
            type = type.inner_type()
        return [(val, type.export(val), g.escape(display).replace(' ',  '&nbsp;'))
                for val, display in self._row.enumerate(self._field.id)]


class RadioFieldExporter(CodebookFieldExporter):
    _HANDLER = 'pytis.RadioField'
    
    def _editor(self, context, id=None, **kwargs):
        g = context.generator()
        value = self._value().value()
        radios = []
        for i, (val, strval, display) in enumerate(self._enumeration(context)):
            radio_id = id +'-'+ str(i)
            radio = g.radio(value=strval, checked=(val==value), id=radio_id, **kwargs)
            label = g.label(display, radio_id)
            radios.append(g.div(radio + label))
        return g.div(radios, id=id, cls='radio-group')


class ChoiceFieldExporter(CodebookFieldExporter):
    _HANDLER = 'pytis.ChoiceField'
    
    def _editor(self, context, **kwargs):
        enumeration = self._enumeration(context)
        options = [(self._field.spec.null_display() or "&nbsp;", "")] + \
                  [(display, strval) for val, strval, display in enumeration]
        value = self._value().value()
        if value in [val for val, strval, display in enumeration]:
            selected = self._field.type.export(value)
        else:
            selected = None
        return context.generator().select(options=options, selected=selected, **kwargs)
    
class ChecklistFieldExporter(CodebookFieldExporter):
    _HANDLER = 'pytis.ChecklistField'
    
    def _format(self, context):
        return self._editor(context, id=self._field.unique_id, readonly=True)
        
    def _editor(self, context, id=None, name=None, disabled=None, readonly=False, cls=None):
        g = context.generator()
        values = [v.value() for v in self._value().value() or ()]
        # URI provider must return a function of the array value for array fields.
        if self._uri_provider:
            uri_provider = self._uri_provider(self._row, self._field.id, type=UriType.LINK)
        else:
            uri_provider = None
        def checkbox(i, value, strval, display):
            # Beware!  Any changes in checkbox rendering made here should be
            # also reflected in the javascript code rendering the items
            # dynamically on form changes.
            checkbox_id = id+'-'+str(i)
            checked = value in values
            if readonly:
                onchange = "this.checked=" + (checked and 'true' or 'false')
            else:
                onchange = None
            result = (g.checkbox(id=checkbox_id, name=name, value=strval, checked=checked,
                                 disabled=disabled, onchange=onchange) +'&nbsp;'+
                      g.label(display, checkbox_id))
            if uri_provider:
                uri = uri_provider(value)
                if uri:
                    if type(uri) in (str, unicode):
                        link = g.link(strval, uri)
                    else:
                        link = g.link(strval, uri.uri(), title=uri.title(), target=uri.target())
                    result += '&nbsp;['+ link +']'
            return result
        checkboxes = [g.div(checkbox(i, val, strval, display) )
                      for i, (val, strval, display) in enumerate(self._enumeration(context))]
        return g.div(checkboxes, id=id, cls='checkbox-group')
    
    def _display(self, context):
        return None


class FileFieldExporter(TextFieldExporter):
    # Don't confuse with BinaryFieldExporter! Maybe deprecate?
    
    def _format(self, context):
        value, info = self._value().export(), None
        if value:
            value = self._row[self._field.spec.filename()].export()
            info = format_byte_size(len(value))
        return value
