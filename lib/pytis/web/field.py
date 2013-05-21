# -*- coding: utf-8 -*-

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

import collections
import re

from pytis.web import *

_ = lcg.TranslatableTextFactory('pytis')


class UriType(object):
    """URI type for 'uri_provider' 'kind' argument.

    URI provider is a function passed to the form constructor (and from there
    to the field constructors) that returns different kinds of URIs.  The
    constants defined by this class define the different kinds of links which
    may be queried.

    """
    LINK = 'LINK'
    """Target of a link to which the field value references.

    If an URI is returned by URI provider for this URI kind and given field, the
    field value is rendered as a link pointing to given URI.  If None is
    returned, the field value is not rendered as a link.  The returned URI may
    be either a string or unicode or a 'Link' instance.

    """
    ACTION = 'ACTION'
    """Link to a pytis record's action.

    Allows querying URIs for pytis actions.  The target of the URI provider
    request (the last argument of the provider call) is the
    'pytis.presentation.Action' instance.

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

def localizable_export(value):
    """Try to convert a pytis value into a corresponding 'lcg.Localizable'.

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
            return lcg.LocalizableDateTime(value.value().strftime('%Y-%m-%d %H:%M:%S'),
                                           utc=value.type().utc())
        elif type_cls is pytis.data.Date:
            return lcg.LocalizableDateTime(value.value().strftime('%Y-%m-%d'))
        elif type_cls is pytis.data.Time:
            return lcg.LocalizableTime(value.value().strftime('%H:%M:%S'))
        elif type_cls is pytis.data.Monetary:
            return lcg.Monetary(value.value(), precision=value.type().precision())
        elif type_cls is pytis.data.Float:
            return lcg.Decimal(value.value(), precision=value.type().precision())
        elif type_cls is pytis.data.Integer:
            return lcg.Decimal(value.value())
        else:
            return value.export()
        return localizable
    else:
        return ''

# For backwards compatibility.
localizable_datetime = localizable_export

    
class Field(object):
    """Field value exporter for both read-only and editable field representations.

    An instance of this class represents a pytis form field an all kinds of
    pytis web forms.  It defines public methods and attributes to query field
    properties and methods 'format()' and 'editor()' which render the final
    HTML representation of the field ('format()' renders the read-only
    representation and 'editor()' the editable field).  All public attributes
    must be treated as read-only!

    Subclasses of this class define specific kinds of fields with different
    user interface and behavior.  A new instance is created by the static
    method 'create()', which automatically decides which particular subclass
    must be used for given field according to specification.
    
    """
    _HANDLER = 'pytis.Field'
    _URL_MATCHER = re.compile(r'(https?://.+?)(?=[\),.:;?!\]]?\.*(\s|&nbsp;|&lt;|&gt;|<br/?>|$))')

    @staticmethod
    def create(row, spec, form, uri_provider, multirow=False):
        """Create a new instance of the corresponding Field subclass.

        Arguments:

          row -- 'pytis.presentation.PresentedRow' instance
          spec -- 'pytis.presentation.Field' specification instance of the field to create
          form -- 'pytis.web.Form' instance of the field's parent form
          uri_provider -- URI provider function as described in 'UriType' class
          multirow -- boolean flag; True if the field appears in a multiline
            edit form, where the HTML field name and identifier must contain
            the row identification (will be suffixed by the exported row key
            value separated by a dash from the name and id of the field
            itself).  If False, no suffix is used.
        
        """
        data_type = row.type(spec.id())
        if isinstance(data_type, pytis.data.Password):
            cls = PasswordField
        elif isinstance(data_type, pytis.data.Color):
            cls = ColorField
        elif isinstance(data_type, pytis.data.Binary):
            cls = BinaryField
        elif isinstance(data_type, pytis.data.Date):
            cls = DateField
        elif isinstance(data_type, pytis.data.DateTime):
            cls = DateTimeField
        elif isinstance(data_type, pytis.data.Array):
            inner_type = data_type.inner_type()
            if inner_type.enumerator():
                cls = ChecklistField
            else:
                raise Exception("Unsupported array field.")
        elif data_type.enumerator():
            selection_type = spec.selection_type()
            if selection_type == SelectionType.RADIO:
                cls = RadioField
            elif selection_type is None and isinstance(data_type, pytis.data.Boolean):
                cls = BooleanField
            elif selection_type in (SelectionType.CHOICE, None):
                cls = ChoiceField
            else:
                cls = CodebookField
        elif spec.filename():
            cls = FileField
        elif spec.text_format() == TextFormat.LCG:
            cls = StructuredTextField
        elif spec.text_format() == TextFormat.HTML:
            cls = HtmlField
        elif spec.height() > 1:
            cls = MultilineField
        elif isinstance(data_type, pytis.data.String):
            cls = StringField
        elif isinstance(data_type, pytis.data.Number):
            cls = NumericField
        else:
            cls = TextField
        return cls(row, spec, form, uri_provider, multirow=multirow)

    def __init__(self, row, spec, form, uri_provider, multirow=False):
        """Don't use directly - use 'Field.create()'."""
        fid = spec.id()
        t = row.type(fid)
        self._html_id = "f%x" % positive_id(self)
        self._row = row
        self._showform = isinstance(form, ShowForm)
        self._uri_provider = uri_provider
        self._format_cache = {}
        self._format_cache_context = None
        self._multirow = multirow
        self._key = row.data().key()[0].id()
        self._not_null = t.not_null() and not isinstance(t, pytis.data.Boolean) and \
            (row.new() or not isinstance(t, (pytis.data.Password, pytis.data.Binary)))
        # All public attributes must be treated as read-only!
        self.id = fid
        self.type = t
        self.spec = spec
        # Duplicate selected specification options for simplified access
        self.style = spec.style()
        self.label = spec.label()
        self.column_label = spec.column_label()
        self.label = spec.label()
        self.virtual = spec.virtual()

    def _format(self, context):
        """Return the formatted field value as a (localizable) string.

        This method is called as part of the public method 'format()' to
        provide the field's exported value.  The final result returned by
        'format()' will also include the additional information returned by the
        method '_display()' (if not None).  For fields represented by links,
        value returned by '_format() will be used as the link label and the
        '_display()' result will be added outside the link.

        """
        value = self._exported_value()
        if value and not isinstance(value, lcg.Localizable):
            g = context.generator()
            escaped = g.escape(self._row.format(self.id))
            lines = escaped.splitlines()
            if self._showform and len(lines) > self.spec.height()+2:
                width = self.spec.width()
                value = g.textarea(self.id, value=escaped, readonly=True,
                                   rows=min(self.spec.height(), 8), cols=width,
                                   cls=width >= 80 and 'fullsize' or None)
            else:
                # Preserve linebreaks and indentation in multiline text.
                nbsp = '&nbsp;'
                len_nbsp = len(nbsp)
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
                        if line[i] == ' ':
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
                # Join lines and substitute links for HTML links
                converted_text = '<br>\n'.join(convert_line(l) for l in lines)
                value = self._URL_MATCHER.sub(r'<a href="\1">\1</a>', converted_text)
                if len(lines) > 1:
                    value = g.div(value)
        return value

    def _exported_value(self):
        return localizable_export(self._value())
    
    def _display(self, context):
        """Additional information about the field value (see '_format()' for more info)."""
        return None

    def _value(self):
        """Return the field value as a 'pytis.data.Value' instance."""
        return self._row[self.id]
    
    def _editor_kwargs(self, context, prefill, error):
        """Return editor field keyword arguemnts as a dictionary.

        In this class the method returns the basic set of arguemnts common to
        all fields types.  Derived classes will usually just add field specific
        arguments (or may return completely different arguments when the field
        type requires it).

        """
        name = self.id
        if self._multirow:
            name += '-' + self._row[self._key].export()
        return dict(id=self.html_id(),
                    name=name,
                    disabled=not self._row.editable(self.id) or None,
                    cls=error and 'invalid' or None)

    def _editor(self, context, **kwargs):
        """Return the field editor control as HTML string.

        The keyword arguments passed to this method are the values returned by
        '_editor_kwargs()'.  This allows easier customization of the final
        result by independent overriding of either of the two methods.

        """
        return None

    def _validate(self, string_value, locale_data, **kwargs):
        return self._row.validate(self.id, string_value, **kwargs)

    def validate(self, string_value, locale_data):
        # TODO: This validation is currently only used for query fields and
        # only works for some field kinds.  It is necessary to move all the
        # logic of the method wiking.PytisModule._validate() into pytis fields
        # to make it work in all cases.  Now it is only hacked for Datetime and
        # Boolean fields (by copying the relevant part of the above mentioned
        # method).
        return self._validate(string_value, locale_data)

    def html_id(self):
        """Return the unique HTML identifier of the field."""
        html_id = self._html_id
        if self._multirow:
            html_id += '-' + self._row[self._key].export()
        return html_id

    def not_null(self):
        """Return True if the field is NOT NULL (the value is required)."""
        return self._not_null
        
    def format(self, context):
        """Return the exported read-only field representation."""
        if self._format_cache_context is not context:
            self._format_cache = {}
            self._format_cache_context = context
        field_value = self._value().value()
        try:
            value, info = self._format_cache[field_value]
        except (TypeError, KeyError): # catch unhashable keys
            value = self._format(context)
            info = self._display(context)
            try:
                self._format_cache[field_value] = (value, info,)
            except TypeError: # catch unhashable keys
                pass
        if value and self._uri_provider:
            g = context.generator()
            src = self._uri_provider(self._row, UriType.IMAGE, self.id)
            if src:
                if info is not None:
                    value += ' ('+ info +')'
                    info = None
                value = g.img(src, alt=value) #, cls=cls)
            link = self._uri_provider(self._row, UriType.LINK, self.id)
            if link:
                if isinstance(link, collections.Callable):
                    pass # Ignore array item links here
                elif isinstance(link, basestring):
                    value = g.a(value, href=link)
                else:
                    value = g.a(value, href=link.uri(), title=link.title(), target=link.target())
            if info is not None:
                value += ' ('+ info +')'
        return value

    def editor(self, context, prefill=None, error=None):
        """Return the exported editable field control."""
        kwargs = self._editor_kwargs(context, prefill, error)
        return self._editor(context, **kwargs)

    def javascript(self, context, form_id, active):
        """Return JavaScript code for creation of field handler instance."""
        g = context.generator()
        return g.js_call("new %s" % self._HANDLER, form_id, self.html_id(),
                         self.id, active, self.not_null())


class TextField(Field):

    def _maxlen(self):
        return None
    
    def _editor_kwargs(self, context, prefill, error):
        kwargs = super(TextField, self)._editor_kwargs(context, prefill, error)
        value = prefill or self._exported_value()
        maxlen = self._maxlen()
        size = self.spec.width(maxlen)
        return dict(kwargs, value=value, size=size, maxlength=maxlen)

    def _editor(self, context, **kwargs):
        return context.generator().field(**kwargs)

    
class NumericField(TextField):
    pass

    
class StringField(TextField):
    
    def _maxlen(self):
        return self.type.maxlen()
    

class PasswordField(StringField):
    _HANDLER = 'pytis.PasswordField'
    
    def _format(self, context):
        if self._showform:
            return None
        else:
            return super(PasswordField, self)._format(context)

    def _editor(self, context, **kwargs):
        g = context.generator()
        result = g.field(password=True, **kwargs)
        if self.type.verify():
            kwargs['id'] += '-verify-pasword'
            result += g.br() + g.field(password=True, **kwargs)
        return result


class MultilineField(Field):

    def _editor_kwargs(self, context, prefill, error):
        kwargs = super(MultilineField, self)._editor_kwargs(context, prefill, error)
        value = prefill or self._value().export()
        width, height = self.spec.width(), self.spec.height()
        cls = kwargs.get('cls')
        if width >= 80:
            cls = (cls and cls+' ' or '') + 'fullsize'
        return dict(kwargs, value=value, rows=height, cols=width, cls=cls)

    def _editor(self, context, **kwargs):
        return context.generator().textarea(**kwargs)

    
class StructuredTextField(MultilineField):

    def __init__(self, *args, **kwargs):
        self._parser = lcg.Parser()
        super(StructuredTextField, self).__init__(*args, **kwargs)

    def _format(self, context):
        blocks = self._parser.parse(context.localize(self._value().export()))
        if len(blocks) == 1 and isinstance(blocks[0], lcg.Paragraph):
            content = lcg.Container(blocks[0].content())
        else:
            content = lcg.Container(blocks)
        return context.generator().div(content.export(context))


class HtmlField(MultilineField):
    _HANDLER = 'pytis.HtmlField'

    class AcfRule(object):
        """A single ACF rule for CKEditor"""
        def __init__(self, elements, attributes=(), styles=(), classes=()):
            self.elements = elements
            self.attributes = attributes
            self.styles = styles
            self.classes = classes
        def __str__(self):
            res = " ".join(self.elements)
            if (self.attributes):
                res += " [" + ",".join(map(str, self.attributes)) + "]"
            if (self.styles):
                res += " {" + ", ".join(self.styles) + "}"
            if (self.classes):
                res += " (" + ", ".join(self.classes) + ")"
            return res;
        
    class AcfRequiredAttribute(object):
        """Required attribute in ACF rule for CKEditor"""
        def __init__(self, attribute):
            self.attribute = attribute
        def __str__(self):
            return '!' + self.attribute

    def _format(self, context):
        return context.localize(self._value().export())
    
    def _editor(self, context, **kwargs):
        content = super(HtmlField, self)._editor(context, **kwargs)
        if context.resource('ckeditor/ckeditor.js'):
            g = context.generator()
            context.resource('pytis-ckeditor.js')
            context.resource('swfobject.js')
            context.resource('flash.js')
            context.resource('ckeditor.css')
            context.resource('ASCIIMathML.js')
            context.resource('ckeditor.css')
            toolbar = (
                ('clipboard',   ('Cut', 'Copy', 'Paste', 'PasteText', #'PasteFromWord',
                                 '-', 'Undo', 'Redo')),
                ('editing',     ('Find', 'Replace', '-', 'SelectAll')),
                ('basicstyles', ('Bold', 'Italic', 'Underline', #'Strike',
                                 'Subscript', 'Superscript', '-', 'RemoveFormat')),
                ('tools',       ('Source', 'Maximize', 'ShowBlocks','-', 'About')),
                ('/', None),
                ('styles',      ('Format', 'Language')), #'Font','FontSize')),
                ('paragraph',   ('NumberedList','BulletedList','DefinitionList','-',
                                 'Outdent','Indent','-', 'Blockquote', 'BlockquoteFooter', '-',
                                 'JustifyLeft','JustifyCenter','JustifyRight','JustifyBlock',
                                 # '-', 'BidiLtr','BidiRtl'
                                 )),
                ('pytis',       ('PytisImage', 'PytisAudio', 'PytisVideo', 'PytisResource',
                                 'PytisExercise', 'PytisMathML')),
                ('links',       ('Link', 'Unlink', 'Anchor', 'PytisIndexItem')),
                ('insert',      ('Table','HorizontalRule', 'PageBreak', 'SpecialChar')), #'Smiley',
                )

            Rule = self.AcfRule
            R = self.AcfRequiredAttribute
            acf_rules = (
                # Text content
                # Links
                Rule(['a'], ['href', 'name']),
                Rule(['h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9'], ['align']),
                Rule(['p'], ['align']),
                Rule(['pre']),
                # Breaks and dividers
                Rule(['br']),
                Rule(['div'], styles=['page-break-after']),
                Rule(['hr']),
                # Tables and lists
                Rule(['table', 'tr', 'td'], ['align']),
                Rule(['thead', 'tfoot', 'caption']),
                Rule(['th'], ['scope']),
                Rule(['ul', 'ol', 'dl', 'li', 'dt', 'dd']),
                # Quotations and footers
                Rule(['blockquote', 'footer']),
                # Inline markup
                Rule(['strong', 'em', 'u', 'strike', 'sub', 'sup']),
                # Pytis resources
                Rule(['a'], [R('href'), R('data-lcg-link-type')], classes=['lcg-image']),
                Rule(['img'], [R('src'), R('data-lcg-resource'), 'align', 'alt', 'title']),
                Rule(['a'], [R('href'), R('data-lcg-resource')], classes=['lcg-audio']),
                Rule(['a'], [R('href'), R('data-lcg-resource')], classes=['lcg-video']),
                Rule(['a'], [R('href'), R('data-lcg-resource')], classes=['lcg-resource']),
                # Exercises
                Rule(['pre'], classes=['lcg-exercise'], attributes=['data-type', 'contenteditable'], styles=['display']),
                # Mathematics
                Rule(['span'], ['contenteditable'], styles=['display'], classes=['lcg-mathml']),
                Rule(['math'], ['contenteditable', 'xmlns'], styles=['display']),
                Rule(['maction', 'maligngroup', 'malignmark', 'menclose', 'merror', 'mfenced',
                      'mfrac', 'mglyph', 'mi', 'mlabeledtr', 'mlongdiv', 'mmultiscripts', 'mn', 'mo',
                      'mover', 'mpadded', 'mphantom', 'mroot', 'mrow', 'ms', 'mscarries', 'mscarry',
                      'msgroup', 'msline', 'mspace', 'msqrt', 'msrow', 'mstack', 'mstyle', 'msub',
                      'msup', 'msubsup', 'mtable', 'mtd', 'mtext', 'mtr', 'munder', 'munderover',
                      'semantics', 'annotation'], ['*']),
                # Language marking
                Rule(['*'], ['lang'], classes=['cke-explicit-language']),
                Rule(['span'], [R('lang')], classes=['cke-explicit-language']),
                # Figures and captions
                Rule(['figure'], ['data-lcg-align']),
                Rule(['figcaption']),
                );
            config = dict(toolbar=[i and dict(name=n, items=i) or n for n, i in toolbar],
                          language=context.lang(),
                          removePlugins = 'forms,image',
                          contentsCss = context.uri(lcg.Stylesheet('ckeditor-content.css')),
                          entities=False,
                          entities_greek=False,
                          entities_latin=False,
                          entities_processNumerical=False,
                          allowedContent="; ".join(map(str, acf_rules)),
                          )
            html_id = self.html_id()
            if self._row.attachment_storage(self.id) is not None:
                from lcg import exercises
                config['extraPlugins'] = 'pytis-attachments'
                config['pytisFieldId'] = html_id
                config['lcgExerciseTypes'] = [(cls.__name__,
                                               context.localize(cls.name()),
                                               context.localize(cls.authoring().export(context)))
                                              for cls in (exercises.HiddenAnswers,
                                                          exercises.TrueFalseStatements,
                                                          exercises.MultipleChoiceQuestions,
                                                          exercises.GapFilling,
                                                          exercises.Selections,
                                                          exercises.WrittenAnswers,
                                                          exercises.NumberedCloze,
                                                          exercises.NumberedExposedCloze,
                                                          exercises.Cloze,
                                                          exercises.ExposedCloze,
                                                          )]
            content += g.script(g.js_call('CKEDITOR.replace', html_id, config))
        return content

    
class DateTimeField(TextField):
    _HANDLER = 'pytis.DateTimeField'
    
    def _datetime_format(self, locale_data):
        if hasattr(self.type, 'exact') and not self.type.exact(): # for wiking.DateTime
            time_format = locale_data.time_format
        else:
            time_format = locale_data.exact_time_format
        return locale_data.date_format +' '+ time_format

    def _maxlen(self):
        # TODO: Respect date format!
        return 18
    
    def _editor(self, context, **kwargs):
        result = super(DateTimeField, self)._editor(context, **kwargs)
        g = context.generator()
        result += g.script_write(g.button('...', id='%s-button' % kwargs['id'], type='button',
                                          cls='selection-invocation calendar-invocation',
                                          disabled=kwargs['disabled']))
        context.resource('prototype.js')
        context.resource('calendarview.js')
        context.resource('calendarview.css')
        locale_data = context.locale_data()
        js_values = dict(
            id = kwargs['id'],
            format = self._datetime_format(locale_data),
            today = context.localize(_(u"today")),
            day_names = g.js_value([context.localize(lcg.week_day_name(i, abbrev=True))
                                    for i in (6,0,1,2,3,4,5)]),
            month_names = g.js_value([context.localize(lcg.month_name(i))
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

    def _validate(self, string_value, locale_data, **kwargs):
        # TODO: Take locale data from somewhere as in _editor() above?
        return super(DateTimeField, self)._validate(string_value, locale_data, 
                                                    format=self._datetime_format(locale_data),
                                                    **kwargs)

    
class DateField(DateTimeField):

    def _datetime_format(self, locale_data):
        return locale_data.date_format

    def _maxlen(self):
        # TODO: Respect date format!
        return 10


class TimeField(DateTimeField):

    def _datetime_format(self, locale_data):
        return locale_data.exact_time_format


class ColorField(StringField):

    def _format(self, context):
        g = context.generator()
        color = self._value().export()
        return g.span(color or '&nbsp;', cls="color-value") +' '+ \
               g.span('&nbsp;', cls="color-display", style="background-color: %s;" %color)


class BooleanField(Field):
    _HANDLER = 'pytis.CheckboxField'

    def _format(self, context):
        # Translators: Boolean value display.  Should be Yes/No in the meaning True/False.
        return self._row.display(self.id) or self._row[self.id].value() and _(u"Yes") or _(u"No")

    def _editor(self, context, **kwargs):
        return context.generator().checkbox(value='T', checked=self._value().value(), **kwargs)

    def _validate(self, string_value, locale_data, **kwargs):
        if string_value is None:
            string_value = 'F'
        return super(BooleanField, self)._validate(string_value, locale_data, **kwargs)


class BinaryField(Field):
    _HANDLER = 'pytis.FileUploadField'

    def _format(self, context):
        buf = self._value().value()
        if buf:
            if buf.filename():
                return buf.filename()
            filename = self._row.filename(self.id)
            if filename:
                return filename
            elif isinstance(type, pd.Image):
                # Translators: The label "image"/"file" is used in textual representation of binary
                # data values, usually as a link to download the actual binary file.
                return _(u"image")
            else:
                return _(u"file")
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

    
class EnumerationField(Field):
    
    def _format(self, context):
        fid = self.id
        if self._row.prefer_display(fid):
            return self._row.display(fid)
        if isinstance(self.type, pd.Boolean):
            # Boolean fields may by also rendered as radio, etc. when
            # selection_type is defined.
            value = self._row[fid].value() and _(u"Yes") or _(u"No")
        else:
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
        if self._showform and not self._row.prefer_display(self.id):
            return self._row.display(self.id) or None
        else:
            return None

    def _enumeration(self, context):
        g = context.generator()
        type = self.type
        if isinstance(type, pytis.data.Array):
            type = type.inner_type()
        return [(val, type.export(val),
                 g.escape(display).replace(' ', '&nbsp;').replace("\n", "<br/>"))
                for val, display in self._row.enumerate(self.id)]


class RadioField(EnumerationField):
    _HANDLER = 'pytis.RadioField'
    
    def _editor(self, context, id=None, **kwargs):
        g = context.generator()
        value = self._value()
        radios = []
        choices = self._enumeration(context)
        if not value.type().not_null():
            null_display = self.spec.null_display()
            if null_display:
                choices.insert(0, (None, '', null_display))
        for i, (val, strval, display) in enumerate(choices):
            radio_id = id +'-'+ str(i)
            radio = g.radio(value=strval, checked=(val==value.value()), id=radio_id, **kwargs)
            label = g.label(display, radio_id)
            radios.append(g.div(radio + label))
        return g.div(radios, id=id, cls='radio-group')


class ChoiceField(EnumerationField):
    _HANDLER = 'pytis.ChoiceField'
    
    def _editor(self, context, **kwargs):
        enumeration = self._enumeration(context)
        options = [(self.spec.null_display() or "&nbsp;", "")] + \
                  [(display, strval) for val, strval, display in enumeration]
        value = self._value().value()
        if value in [val for val, strval, display in enumeration]:
            selected = self.type.export(value)
        else:
            selected = None
        return context.generator().select(options=options, selected=selected, **kwargs)
    
class ChecklistField(EnumerationField):
    _HANDLER = 'pytis.ChecklistField'
    
    def _format(self, context):
        return self._editor(context, id=self.html_id(), readonly=True)
        
    def _editor(self, context, id=None, name=None, disabled=None, readonly=False, cls=None):
        g = context.generator()
        values = [v.value() for v in self._value().value() or ()]
        # URI provider must return a function of the array value for array fields.
        if self._uri_provider:
            uri_provider = self._uri_provider(self._row, UriType.LINK, self.id)
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
                        link = g.a(strval, href=uri)
                    else:
                        link = g.a(strval, href=uri.uri(), title=uri.title(), target=uri.target())
                    result += '&nbsp;['+ link +']'
            return result
        checkboxes = [g.div(checkbox(i, val, strval, display) )
                      for i, (val, strval, display) in enumerate(self._enumeration(context))]
        return g.div(checkboxes, id=id, cls='checkbox-group')
    
    def _display(self, context):
        return None


class CodebookField(EnumerationField, TextField):
    pass
    

class FileField(TextField):
    """Special case of string fields with 'filename' specification.

    The contents of such string fields is considered to be a file and the user
    interface doesn't show the value itself, but provides a link to save it.

    """
    def _format(self, context):
        if self._value().value() is not None:
            value = self._row.filename(self.id)
        else:
            value = ''
        return value

    def _display(self, context):
        value = self._value().value()
        if value:
            return format_byte_size(len(value))
        else:
            return None
    
