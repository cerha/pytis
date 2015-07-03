# -*- coding: utf-8 -*-

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

import collections
import lcg
import pytis.data as pd
import pytis.util

from pytis.presentation import SelectionType, TextFormat, Orientation

_ = pytis.util.translations('pytis-web')


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
    TOOLTIP = 'TOOLTIP'
    """URI for loading an asynchronous tooltip content for given field.

    If an URI is returned by URI provider for this URI kind and given field,
    the field will display the content loaded from this uri as a tooltip when
    the mouse hovers over the field value.  Any errors loading the URI are
    silently ignored and the tooltip is just not dispayed in this case.

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

def localizable_export(value, **kwargs):
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
        if type_cls is pd.DateTime:
            return lcg.LocalizableDateTime(value.value().strftime('%Y-%m-%d %H:%M:%S'),
                                           utc=value.type().utc())
        elif type_cls is pd.Date:
            return lcg.LocalizableDateTime(value.value().strftime('%Y-%m-%d'))
        elif type_cls is pd.Time:
            return lcg.LocalizableTime(value.value().strftime('%H:%M:%S'))
        elif type_cls is pd.Monetary:
            return lcg.Monetary(value.value(), precision=value.type().precision())
        elif type_cls is pd.Float:
            return lcg.Decimal(value.value(), precision=value.type().precision())
        elif type_cls is pd.Integer:
            return lcg.Decimal(value.value())
        else:
            return value.export(**kwargs)
    else:
        return ''

    
class Content(pd.Type):
    """Data type for representation of lcg.Content within pytis fields.

    The type currently doesn't support DB serialization/deserialization and
    thus is only suitable for ineditable virtual fields.  It just allows to
    display arbitrary content in web forms.

    """
    pass


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
        if isinstance(data_type, pd.Password):
            cls = PasswordField
        elif isinstance(data_type, pd.Color):
            cls = ColorField
        elif isinstance(data_type, pd.Binary):
            cls = FileUploadField
        elif isinstance(data_type, pd.Date):
            cls = DateField
        elif isinstance(data_type, pd.DateTime):
            cls = DateTimeField
        elif isinstance(data_type, Content):
            cls = ContentField
        elif isinstance(data_type, pd.Array):
            inner_type = data_type.inner_type()
            if inner_type.enumerator():
                cls = ChecklistField
            else:
                cls = ArrayField
        elif data_type.enumerator():
            selection_type = spec.selection_type()
            if selection_type == SelectionType.RADIO:
                cls = RadioField
            elif selection_type is None and isinstance(data_type, pd.Boolean):
                cls = CheckboxField
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
        elif isinstance(data_type, pd.Email):
            cls = EmailField
        elif isinstance(data_type, pd.String):
            cls = StringField
        elif isinstance(data_type, pd.Number):
            cls = NumericField
        else:
            cls = TextField
        return cls(row, spec, form, uri_provider, multirow=multirow)

    def __init__(self, row, spec, form, uri_provider, multirow=False):
        """Don't use directly - use 'Field.create()'."""
        from form import ShowForm
        fid = spec.id()
        t = row.type(fid)
        self._html_id = "f%x" % pytis.util.positive_id(self)
        self._row = row
        self._showform = isinstance(form, ShowForm)
        self._uri_provider = uri_provider
        self._format_cache = {}
        self._format_cache_context = None
        self._multirow = multirow
        self._key = row.data().key()[0].id()
        self._not_null = t.not_null() and not isinstance(t, pd.Boolean) and \
            (row.new() or not isinstance(t, (pd.Password, pd.Binary)))
        self._big = isinstance(t, (pd.Big, pd.Large))
        # All public attributes must be treated as read-only!
        self.id = fid
        self.type = t
        self.spec = spec
        # Duplicate selected specification options for simplified access
        self.style = spec.style()
        self.label = spec.label()
        self.column_label = spec.column_label()
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
        value = self._row.format(self.id, export=localizable_export)
        if value and not isinstance(value, lcg.Localizable):
            numlines = len(value.splitlines())
            value = lcg.format_text(value)
            if numlines > 1:
                g = context.generator()
                height = self.spec.height()
                cls = 'multiline'
                style = None
                if self._showform and height > 1 and numlines > height + 2:
                    style = 'height: %dem;' % int(height * 1.4)
                    cls += ' scrolled'
                value = g.div(value, cls=cls, style=style)
        return value

    def _exported_value(self):
        return localizable_export(self._value())
    
    def _display(self, context):
        """Additional information about the field value (see '_format()' for more info)."""
        return None

    def _value(self):
        """Return the field value as a 'pytis.data.Value' instance."""
        return self._row[self.id]
    
    def _editor_kwargs(self, context):
        """Return editor field keyword arguemnts as a dictionary.

        In this class the method returns the basic set of arguemnts common to
        all fields types.  Derived classes will usually just add field specific
        arguments (or may return completely different arguments when the field
        type requires it).

        """
        return dict(id=self.html_id(),
                    name=self.name(),
                    disabled=not self._row.editable(self.id) or None,
                    cls=self._row.validation_error(self.id) and 'invalid' or None)

    def _editor(self, context, **kwargs):
        """Return the field editor control as HTML string.

        The keyword arguments passed to this method are the values returned by
        '_editor_kwargs()'.  This allows easier customization of the final
        result by independent overriding of either of the two methods.

        """
        return None

    def _validate(self, value, locale_data, **kwargs):
        return self._row.validate(self.id, value, **kwargs)

    def name(self):
        name = self.id
        if self._multirow:
            key = self._row[self._key].export()
            if not key:
                key = 'pytis-inserted-row-%d' % self._row.inserted_row_number
            name += '-' + key
        return name

    def html_id(self):
        """Return the unique HTML identifier of the field."""
        html_id = self._html_id
        if self._multirow:
            html_id += '-' + self._row[self._key].export()
        return html_id

    def not_null(self):
        """Return True if the field is NOT NULL (the value is required)."""
        return self._not_null

    def label_in_front(self):
        """Return True if the label is in front of the field."""
        return True

    def validate(self, req, locale_data):
        """Validate the submitted field value and return ValidationError or None.

        Returns None if the field value in given request is valid.  Returns
        ValidationError if validation fails.

        The valid field value is propagated to the underlying PresentedRow
        instance as a side effect.  If the submitted value is invalid, it is
        also available later through 'PresentedRow.invalid_string()'.

        """
        value = req.param(self.name())
        return self._validate(value, locale_data)

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
        if self._uri_provider and (value or self._big):
            # Big values are not present in the select (value is None) but they may still
            # return URI (typically images).
            g = context.generator()
            src = self._uri_provider(self._row, UriType.IMAGE, self.id)
            if src:
                if info is not None:
                    value += ' (' + info + ')'
                    info = None
                value = g.img(src, alt=value) #, cls=cls)
            if value:
                link = self._uri_provider(self._row, UriType.LINK, self.id)
                if link:
                    if isinstance(link, collections.Callable):
                        kwargs = None # Ignore array item links here
                    elif isinstance(link, basestring):
                        kwargs = dict(href=link)
                    else:
                        kwargs = dict(href=link.uri(), title=link.title(), target=link.target())
                    if kwargs:
                        tooltip_uri = self._uri_provider(self._row, UriType.TOOLTIP, self.id)
                        if tooltip_uri:
                            kwargs = dict(
                                kwargs,
                                onmouseover="pytis.show_tooltip(event, '%s')" % tooltip_uri,
                                onmouseout="pytis.hide_tooltip(this)",
                            )
                        value = g.a(value, **kwargs)
                if info is not None:
                    value += ' (' + info + ')'
        return value

    def editor(self, context):
        """Return the exported editable field control."""
        kwargs = self._editor_kwargs(context)
        return self._editor(context, **kwargs)

    def state(self):
        """Return the string representation of field runtime filters and arguments state.

        We rely on the fact, that a stringified 'pytis.data.Operator' uniquely
        represents the corresponding runtime filter state.

        """
        if self.spec.runtime_filter() or self.spec.runtime_arguments():
            state = 'f=%s;a=%s' % (
                self._row.runtime_filter(self.id),
                self._row.runtime_arguments(self.id),
            )
        else:
            state = None
        return state

    def javascript(self, context, form_id, layout_fields):
        """Return JavaScript code for creation of field handler instance."""
        g = context.generator()
        active = self._row.depends(self.id, layout_fields)
        return g.js_call("new %s" % self._HANDLER, form_id, self.html_id(),
                         self.id, self.state(), active, self.not_null())


class TextField(Field):

    def _maxlen(self):
        return None

    def html_id(self):
        html_id = super(TextField, self).html_id()
        if self.id == 'login':
            # HACK: This makes the Safari's password suggestion work in forms, which
            # call the login name field 'login' instead of 'username' (Wiking).
            html_id += '-username'
        return html_id
    
    def _editor_kwargs(self, context):
        kwargs = super(TextField, self)._editor_kwargs(context)
        invalid_string = self._row.invalid_string(self.id)
        if invalid_string is not None:
            value = invalid_string
        else:
            value = self._exported_value()
        maxlen = self._maxlen()
        size = self.spec.width(maxlen)
        return dict(kwargs, value=value, size=size, maxlength=maxlen,
                    cls=((kwargs.get('cls') or '') + ' text-field').strip())

    def _editor(self, context, **kwargs):
        return context.generator().input(**kwargs)

    
class NumericField(TextField):

    def _validate(self, value, locale_data, **kwargs):
        if value:
            # Convert the value to 'C' locale formatting before validation.
            if isinstance(self.type, pd.Monetary):
                decimal_point = locale_data.mon_decimal_point
                thousands_sep = locale_data.mon_thousands_sep
            else:
                decimal_point = locale_data.decimal_point
                thousands_sep = locale_data.thousands_sep
            if thousands_sep:
                value = value.replace(thousands_sep, '')
            if decimal_point != '.':
                value = value.replace(decimal_point, '.')
        return super(NumericField, self)._validate(value, locale_data, **kwargs)

    
class StringField(TextField):
    
    def _maxlen(self):
        return self.type.maxlen()
    

class PasswordField(StringField):
    _HANDLER = 'pytis.PasswordField'
    
    def _validate(self, value, locale_data, **kwargs):
        if not value and not self._row.new():
            # Keep the original password when the field is empty.
            return None
        if self.type.verify():
            if isinstance(value, tuple) and len(value) == 2:
                value, kwargs['verify'] = value
            else:
                kwargs['verify'] = ''
        return super(PasswordField, self)._validate(value, locale_data, **kwargs)

    def _format(self, context):
        if self._showform:
            return None
        else:
            return super(PasswordField, self)._format(context)

    def _editor(self, context, **kwargs):
        g = context.generator()
        result = g.input(type='password', **kwargs)
        if self.type.verify():
            # Note, the 'confirm' substring in the id is important for Safari's password
            # suggestion functionality.
            kwargs['id'] += '-confirm-password'
            result += g.br() + g.input(type='password', **kwargs)
        return result


class EmailField(StringField):

    def _editor_kwargs(self, context):
        return dict(super(EmailField, self)._editor_kwargs(context), type='email')


class MultilineField(Field):

    def _editor_kwargs(self, context):
        kwargs = super(MultilineField, self)._editor_kwargs(context)
        invalid_string = self._row.invalid_string(self.id)
        if invalid_string is not None:
            value = invalid_string
        else:
            value = self._value().export()
        width, height = self.spec.width(), self.spec.height()
        cls = kwargs.get('cls')
        if width >= 80:
            cls = (cls and cls + ' ' or '') + 'fullsize'
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


class ContentField(MultilineField):

    def _format(self, context):
        content = self._value().value()
        return content.export(context)


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
            return res
        
    class AcfRequiredAttribute(object):
        """Required attribute in ACF rule for CKEditor"""
        def __init__(self, attribute):
            self.attribute = attribute
        def __str__(self):
            return '!' + self.attribute

    def _format(self, context):
        exported = self._value().export()
        escape = not isinstance(exported, (lcg.HtmlEscapedUnicode, lcg.Concatenation,))
        return lcg.HtmlEscapedUnicode(context.localize(exported), escape=escape)
    
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
                ('clipboard', ('Cut', 'Copy', 'Paste', 'PasteText', #'PasteFromWord',
                               '-', 'Undo', 'Redo')),
                ('editing', ('Find', 'Replace', '-', 'SelectAll')),
                ('basicstyles', ('Bold', 'Italic', 'Underline', #'Strike',
                                 'Subscript', 'Superscript', '-', 'RemoveFormat')),
                ('tools', ('Source', 'Maximize', 'ShowBlocks', '-', 'About')),
                ('/', None),
                ('styles', ('Format', 'Language')), #'Font','FontSize')),
                ('paragraph', ('NumberedList', 'BulletedList', 'DefinitionList', '-',
                               'Outdent', 'Indent', '-', 'Blockquote', 'BlockquoteFooter', '-',
                               'JustifyLeft', 'JustifyCenter', 'JustifyRight', 'JustifyBlock',
                               # '-', 'BidiLtr','BidiRtl'
                               )),
                ('pytis', ('PytisImage', 'PytisAudio', 'PytisVideo', 'PytisResource',
                           'PytisExercise', 'PytisMathML')),
                ('links', ('Link', 'Unlink', 'Anchor', 'PytisIndexItem')),
                ('insert', ('Table', 'HorizontalRule', 'PageBreak', 'SpecialChar')), #'Smiley',
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
                Rule(['table'], [R('data-lcg-transformations')]),
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
                Rule(['pre'], classes=['lcg-exercise'],
                     attributes=['data-type', 'contenteditable'], styles=['display']),
                # Mathematics
                Rule(['span'], ['contenteditable'], styles=['display'], classes=['lcg-mathml']),
                Rule(['math'], ['contenteditable', 'xmlns'], styles=['display']),
                Rule(['maction', 'maligngroup', 'malignmark', 'menclose', 'merror', 'mfenced',
                      'mfrac', 'mglyph', 'mi', 'mlabeledtr', 'mlongdiv', 'mmultiscripts', 'mn',
                      'mo', 'mover', 'mpadded', 'mphantom', 'mroot', 'mrow', 'ms', 'mscarries',
                      'mscarry', 'msgroup', 'msline', 'mspace', 'msqrt', 'msrow', 'mstack',
                      'mstyle', 'msub', 'msup', 'msubsup', 'mtable', 'mtd', 'mtext', 'mtr',
                      'munder', 'munderover', 'semantics', 'annotation'], ['*']),
                # Language marking
                Rule(['*'], ['lang'], classes=['cke-explicit-language']),
                Rule(['span'], [R('lang')], classes=['cke-explicit-language']),
                # Figures and captions
                Rule(['figure'], ['data-lcg-align']),
                Rule(['figcaption']),
            )
            config = dict(
                toolbar=[i and dict(name=n, items=i) or n for n, i in toolbar],
                language=context.lang(),
                removePlugins='forms,image',
                contentsCss=context.uri(lcg.Stylesheet('ckeditor-content.css')),
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
                                                          exercises.Cloze,
                                                          exercises.ModelCloze,
                                                          )]
            content += g.script(g.js_call('CKEDITOR.replace', html_id, config))
        return content

    
class DateTimeField(TextField):
    _HANDLER = 'pytis.DateTimeField'
    
    def datetime_format(self, locale_data):
        if hasattr(self.type, 'exact') and not self.type.exact(): # for wiking.DateTime
            time_format = locale_data.time_format
        else:
            time_format = locale_data.exact_time_format
        return locale_data.date_format + ' ' + time_format

    def _maxlen(self):
        # TODO: Respect date format!
        return 18
    
    def _editor(self, context, **kwargs):
        result = super(DateTimeField, self)._editor(context, **kwargs)
        g = context.generator()
        result += g.button('...', id='%s-button' % kwargs['id'], type='button',
                           cls='selection-invocation calendar-invocation',
                           disabled=kwargs['disabled'])
        context.resource('prototype.js')
        context.resource('calendarview.js')
        context.resource('calendarview.css')
        locale_data = context.locale_data()
        js_values = dict(
            id=kwargs['id'],
            format=self.datetime_format(locale_data),
            today=context.localize(_(u"today")),
            day_names=g.js_value([context.localize(lcg.week_day_name(i, abbrev=True))
                                  for i in (6, 0, 1, 2, 3, 4, 5)]),
            month_names=g.js_value([context.localize(lcg.month_name(i))
                                    for i in range(12)]),
            first_week_day=(locale_data.first_week_day + 1) % 7,
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

    def _validate(self, value, locale_data, **kwargs):
        return super(DateTimeField, self)._validate(value, locale_data,
                                                    format=self.datetime_format(locale_data),
                                                    **kwargs)

    
class DateField(DateTimeField):

    def datetime_format(self, locale_data):
        return locale_data.date_format

    def _maxlen(self):
        # TODO: Respect date format!
        return 10


class TimeField(DateTimeField):

    def datetime_format(self, locale_data):
        return locale_data.exact_time_format


class ColorField(StringField):

    def _format(self, context):
        g = context.generator()
        color = self._value().export()
        return (g.span(color or '&nbsp;', cls="color-value") + ' ' +
                g.span('&nbsp;', cls="color-display", style="background-color: %s;" % color))


class CheckboxField(Field):
    _HANDLER = 'pytis.CheckboxField'

    def _format(self, context):
        # Translators: Boolean value display.  Should be Yes/No in the meaning True/False.
        return self._row.display(self.id) or self._row[self.id].value() and _(u"Yes") or _(u"No")

    def _editor(self, context, **kwargs):
        return context.generator().checkbox(value='T', checked=self._value().value(), **kwargs)

    def _validate(self, value, locale_data, **kwargs):
        if value is None:
            value = 'F'
        return super(CheckboxField, self)._validate(value, locale_data, **kwargs)

    def label_in_front(self):
        return False
        

class FileUploadField(Field):
    _HANDLER = 'pytis.FileUploadField'

    def validate(self, req, locale_data):
        if req.param(self.name()) is None:
            # Handle AJAX request for validation of file size (only
            # size is sent by the form to prevent uploading potentially
            # large files through AJAX requests before form submission).
            size = req.param('_pytis_file_size_' + self.name())
            if size and size.isdigit():
                size = int(size)
                if self.type.minlen() is not None and size < self.type.minlen():
                    error = _(u"Minimal size %(minlen)s not satisfied",
                              minlen=pytis.util.format_byte_size(self.type.minlen()))
                    return pd.ValidationError(error)
                if self.type.maxlen() is not None and size > self.type.maxlen():
                    error = _(u"Maximal size %(maxlen)s exceeded",
                              maxlen=pytis.util.format_byte_size(self.type.maxlen()))
                    return pd.ValidationError(error)
                return None
        return super(FileUploadField, self).validate(req, locale_data)

    def _validate(self, value, locale_data, **kwargs):
        if value is not None:
            kwargs = dict(kwargs, filename=value.filename(), mime_type=value.mime_type())
            value = value.file()
        elif not self._row.new():
            # The original file is kept if no file is uploaded to replace it,
            # so empty field is ok.
            return None
        return super(FileUploadField, self)._validate(value, locale_data, **kwargs)

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
            return pytis.util.format_byte_size(len(buf))
        else:
            return None

    def _editor(self, context, **kwargs):
        return context.generator().upload(**kwargs)

    
class EnumerationField(Field):
    
    def _format(self, context):
        fid = self.id
        if self._row.prefer_display(fid):
            return self._row.display(fid, export=localizable_export)
        if isinstance(self.type, pd.Boolean):
            # Boolean fields may be also rendered as radio, etc. when
            # selection_type is defined.
            value = self._row[fid].value() and _(u"Yes") or _(u"No")
        else:
            value = localizable_export(self._value())
        if self._showform:
            # The display value is returned by the _display method in this case...
            return value
        display = self._row.display(fid, export=localizable_export)
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
            return self._row.display(self.id, export=localizable_export) or None
        else:
            return None

    def _enumeration(self, context):
        g = context.generator()
        type = self.type
        if isinstance(type, pd.Array):
            type = type.inner_type()
        result = []
        for val, display in self._row.enumerate(self.id):
            if isinstance(display, lcg.Localizable):
                display = context.localize(display)
            escaped_display = g.noescape(g.escape(display)
                                         .replace(' ', '&nbsp;')
                                         .replace("\n", "<br/>"))
            result.append((val, type.export(val), escaped_display,))
        return result


class RadioField(EnumerationField):
    _HANDLER = 'pytis.RadioField'
    
    def _editor(self, context, id, **kwargs):
        g = context.generator()
        value = self._value()
        radios = []
        choices = self._enumeration(context)
        if not value.type().not_null():
            null_display = self.spec.null_display()
            if null_display:
                choices.insert(0, (None, '', null_display))
        if self.spec.orientation() == Orientation.VERTICAL or self.spec.height() > 1:
            wrap = g.div
        else:
            wrap = g.span
        for i, (val, strval, display) in enumerate(choices):
            radio_id = id + '-' + str(i)
            radio = g.radio(value=strval, checked=(val == value.value()), id=radio_id, **kwargs)
            label = g.label(display, radio_id)
            radios.append(wrap(radio + label))
        return wrap(radios, id=id, cls='radio-group')


class ChoiceField(EnumerationField):
    _HANDLER = 'pytis.ChoiceField'
    
    def _editor(self, context, **kwargs):
        g = context.generator()
        enumeration = self._enumeration(context)
        options = [(self.spec.null_display() or g.noescape("&nbsp;"), "")] + \
                  [(display, strval) for val, strval, display in enumeration]
        value = self._value().value()
        if value in [val for val, strval, display in enumeration]:
            selected = self.type.export(value)
        else:
            selected = None
        return g.select(options=options, selected=selected, **kwargs)
    
class ChecklistField(EnumerationField):
    _HANDLER = 'pytis.ChecklistField'

    def _validate(self, value, locale_data, **kwargs):
        if value:
            value = pytis.util.xtuple(value)
        else:
            value = ()
        return super(ChecklistField, self)._validate(value, locale_data, **kwargs)
    
    def _format(self, context):
        return self._editor(context, id=self.html_id(), readonly=True)
        
    def _editor(self, context, id, name=None, disabled=None, readonly=False, cls=None):
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
            checkbox_id = id + '-' + str(i)
            checked = value in values
            if readonly:
                onchange = "this.checked=" + (checked and 'true' or 'false')
            else:
                onchange = None
            result = (g.checkbox(id=checkbox_id, name=name, value=strval, checked=checked,
                                 disabled=disabled, onchange=onchange) +
                      g.noescape('&nbsp;') + g.label(display, checkbox_id))
            if uri_provider:
                uri = uri_provider(value)
                if uri:
                    if type(uri) in (str, unicode):
                        link = g.a(strval, href=uri)
                    else:
                        link = g.a(strval, href=uri.uri(), title=uri.title(), target=uri.target())
                    result += (g.noescape('&nbsp;[') + link + g.noescape(']'))
            return result
        checkboxes = [g.div(checkbox(i, val, strval, display))
                      for i, (val, strval, display) in enumerate(self._enumeration(context))]
        return g.div(checkboxes, id=id, cls='checkbox-group')
    
    def _display(self, context):
        return None


class ArrayField(EnumerationField):

    def _format(self, context):
        g = context.generator()
        return ', '.join([g.span(display)
                          for val, strval, display in self._enumeration(context)])

    def _editor(self, context, **kwargs):
        raise Exception("Array field editation unsupported.")

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
            return pytis.util.format_byte_size(len(value))
        else:
            return None
