# -*- coding: utf-8 -*-

# Formátování výstupu
#
# Copyright (C) 2002-2015 Brailcom, o.p.s.
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

"""Formátování výstupu.

Modul má na starost formátování textu a dat pro výstup dle zadaných šablon.
Zpracovává specifikace v podobě značek definovaných v modulu 'markup'.  Šablony
mají podobu pythonových souborů, obsahujících následující funkce vracející
odpovídající specifikace:

  doc_header -- hlavička dokumentu; implicitně prázdná
  doc_footer -- zakončení dokumentu; implicitně prázdné
  page_header -- hlavička stránky; implicitně prázdná
  page_footer -- patička stránky; implicitně číslo stránky
  first_page_header -- hlavička první stránky; implicitně shodná
    s 'page_header'
  page_layout -- dictionary parametrů určujících velikosti stránky a jejích
    okrajů; klíči jsou 'PAGE_*' konstanty modulu, hodnoty jsou popsány
    v dokumentaci těchto konstant
  background -- pozadí stránky; implicitně prázdné bílé
  body -- obsah dokumentu samotného; tato funkce musí být povinně přítomna

Hlavní třídou modulu je třída 'Formatter'.  Ta zajišťuje načtení a zpracování
šablon a doručení zformátovaných dat.

"""
from __future__ import unicode_literals
from past.builtins import basestring

import copy
import re
import string
import io
import lcg

from lcg import UMm
import pytis.data
import pytis.output
import pytis.presentation
import pytis.util

from pytis.util import EVENT, Popen, ResolverError, dev_null_stream, form_view_data, log, xtuple

_ = pytis.util.translations('pytis-wx')

PAGE_WIDTH = 'pwidth'
"""Šířka stránky, instance třídy 'Unit'."""
PAGE_HEIGHT = 'pheight'
"""Výška stránky, instance třídy 'Unit'."""
# Pokud nejsou uvedeny rozměry PAGE_WIDTH a PAGE_HEIGHT,
# použijí se v pdf exporteru lcg standardní rozměry stránky A4.
PAGE_TOP_MARGIN = 'top_margin'
"""Velikost horního okraje stránky, instance třídy 'Unit'."""
PAGE_BOTTOM_MARGIN = 'bottom_margin'
"""Velikost dolního okraje stránky, instance třídy 'Unit'."""
PAGE_LEFT_MARGIN = 'left_margin'
"""Velikot levého okraje stránky, instance třídy 'Unit'."""
PAGE_RIGHT_MARGIN = 'right_margin'
"""Velikost pravého okraje stránky, instance třídy 'Unit'."""
PAGE_LANDSCAPE_MODE = 'landscape_mode'
"""Právě když je pravdivé, bude zaměněna výška a šířka stránky."""
# Nastavení landscape módu se uplatní jen v případě, že nejsou
# explicitně nastaveny PAGE_WIDTH a PAGE_HEIGHT. Ovlivňuje se tedy jen
# landscape mód pro defaultní stránka A4.


class AbortOutput(Exception):
    """Exception raised when printing should be aborted."""


class _ProxyDict(dict):

    def __getitem__(self, key):
        result = dict.__getitem__(self, key)
        if ((not isinstance(result, (basestring, lcg.Content, _ProxyDict)) and
             callable(result))):
            result = self[key] = result()
        return result


class HashableDict(dict):

    def __hash__(self):
        return 0

    def __setitem__(self, key, value):
        raise Exception('immutable object')


class _DataIterator(lcg.SubstitutionIterator):

    class _CodebookDictionary(dict):

        def __init__(self, row, field_id, columns, secret_columns):
            self._row = row
            self._field_id = field_id
            self._secret_columns = secret_columns
            data = [(c, True,) for c in columns]
            dict.__init__(self, data)

        def get(self, key, default=None):
            try:
                value = self._row.cb_value(self._field_id, key)
            except KeyError:
                return default
            result = value.secret_export() if key in self._secret_columns else value.export()
            return result

    class _RowDictionary(dict):

        def __init__(self, row, codebooks):
            if codebooks is None:
                keys = row.keys()
            else:
                keys = codebooks.keys()
            dict.__init__(self, [(k, True,) for k in keys])
            self._row = row
            self._codebooks = codebooks

        def get(self, key, default=None):
            try:
                if self._codebooks is None:
                    return self._row.format(key, secure=True)
                else:
                    columns, secret_columns = self._codebooks[key]
                    return _DataIterator._CodebookDictionary(self._row, key, columns,
                                                             secret_columns)
            except KeyError:
                return default

    def __init__(self, resolver, form_name, condition, sorting, transaction, codebooks=None):
        self._transaction = transaction
        self._select_kwargs = dict(condition=condition, sort=sorting, transaction=transaction)
        view, self._data = form_view_data(resolver, form_name)
        self._presented_row = pytis.presentation.PresentedRow(view.fields(), self._data, None,
                                                              singleline=True)
        self._codebooks = None
        if codebooks:
            self._codebooks = {}
            for field_id, cb_fields, cb in codebooks:
                permitted = pytis.form.has_access(form_name, perm=pytis.data.Permission.VIEW,
                                                  column=field_id)
                columns = []
                secret_columns = []
                for cb_id, _cb_label in cb_fields:
                    columns.append(cb_id)
                    if (not permitted or
                        not pytis.form.has_access(cb, perm=pytis.data.Permission.VIEW,
                                                  column=cb_id)):
                        secret_columns.append(cb_id)
                self._codebooks[field_id] = (columns, secret_columns)
        super(_DataIterator, self).__init__()

    def _init_select(self):
        # It is necessary to delay the select until its data is actually used,
        # otherwise many connections may be opened concurrently when using row
        # templates.
        if self._select_kwargs is not None:
            count = self._data.select(**self._select_kwargs)
            self._select_kwargs = None
            if count > pytis.config.output_row_limit:
                import pytis.form
                message = (_("Going to format a table with many rows (%d).", count) + "\n" +
                           _("Do you want to continue printing anyway?"))
                if not pytis.form.run_dialog(pytis.form.Question, message):
                    raise pytis.form.UserBreakException()

    def _value(self):
        return self._RowDictionary(self._presented_row, self._codebooks)

    def _next(self):
        self._init_select()
        row = self._data.fetchone()
        if row is None:
            self._data.close()
            return False
        self._presented_row.set_row(row)
        return True

    def _reset(self):
        if self._select_kwargs is None:
            self._data.rewind()


class _FormDataIterator(_DataIterator):

    def __init__(self, resolver, form, transaction, parameters):
        name = form.name()
        condition = form.condition()
        sorting = form.sorting()
        if condition is None:
            condition = parameters.get(name + '/' + pytis.output.P_CONDITION)
        if sorting is None:
            sorting = parameters.get(name + '/' + pytis.output.P_SORTING)
        super(_FormDataIterator, self).__init__(resolver, name, condition=condition,
                                                sorting=sorting, transaction=transaction)


class _FakeDataIterator(lcg.SubstitutionIterator):

    def _value(self):
        raise lcg.SubstitutionIterator.NotStartedError(None)

    def _reset(self):
        pass


class LCGFormatter(object):
    """LCG based formatter."""

    class _DummyDict(dict):

        def __getitem__(self, key):
            return ''

    class _LCGGlobals(_ProxyDict):

        def __init__(self, resolver, form, form_bindings, codebooks, transaction,
                     current_row=None, parameters={}):
            self._resolver = resolver
            self._form = form
            self._form_bindings = form_bindings
            self._transaction = transaction
            dictionary = self._initial_dictionary(form, form_bindings, codebooks, current_row,
                                                  parameters)
            _ProxyDict.__init__(self, dictionary)

        def _initial_dictionary(self, form, form_bindings, codebooks, current_row, parameters):
            dictionary = _ProxyDict()
            if form is not None:
                import pytis.form  # must be placed before first `pytis' use here
                if current_row is None:
                    dictionary['data'] = _FormDataIterator(self._resolver, form,
                                                           transaction=self._transaction,
                                                           parameters=parameters)
                else:
                    # Using `data' iteration in row templates is most likely an error.
                    # And since it may cause various performance or system problems we forbid it.
                    dictionary['data'] = _FakeDataIterator()
                if current_row is None:
                    current_row = form.current_row()
                if current_row is None:
                    current_row_dictionary = LCGFormatter._DummyDict()
                else:
                    current_row_dictionary = dict([(k, current_row.format(k, secure=True),)
                                                   for k in current_row.keys()
                                                   if not isinstance(current_row[k].type(),
                                                                     pytis.data.Binary)])
                dictionary['current_row'] = current_row_dictionary
                dictionary['table'] = self._make_table
                dictionary['agg'] = agg = _ProxyDict()
                for name, op in(('min', pytis.data.Data.AGG_MIN,),
                                ('max', pytis.data.Data.AGG_MAX,),
                                ('count', pytis.data.Data.AGG_COUNT,),
                                ('sum', pytis.data.Data.AGG_SUM,),
                                ('avg', pytis.data.Data.AGG_AVG,),):
                    def value(op=op):
                        return self._make_agg(op)
                    agg[name] = value
                if form_bindings:
                    dictionary['Binding'] = binding_dictionary = _ProxyDict()
                    for binding in form_bindings:
                        form_name = binding.name()
                        if form_name is None:
                            continue
                        if ((pytis.form.has_access(form_name) and
                             pytis.form.has_access(form_name, perm=pytis.data.Permission.PRINT))):
                            # I tried to use closure here, but it produced unexpected results
                            class MakeBinding(object):

                                def __init__(self, binding, processor, current_row):
                                    self._binding = binding
                                    self._current_row = current_row
                                    self._processor = processor

                                def __call__(self):
                                    return self._processor(self._binding, self._current_row)
                            binding_id = re.sub('[^A-Za-z0-9_]', '_', binding.id())
                            binding_dictionary[binding_id] = MakeBinding(binding,
                                                                         self._make_binding,
                                                                         current_row)
                if codebooks and current_row:
                    dictionary['codebook'] = codebook_dictionary = {}
                    form_name = form.name()
                    for field_id, cb_fields, cb in codebooks:
                        permitted = current_row.permitted(field_id, pytis.data.Permission.VIEW)
                        codebook_dictionary[field_id] = field_dictionary = _ProxyDict()
                        for cb_id, _cb_label in cb_fields:
                            if (permitted and
                                pytis.form.has_access(cb, perm=pytis.data.Permission.VIEW,
                                                      column=cb_id)):
                                def cb_value(current_row=current_row, field_id=field_id,
                                             cb_id=cb_id):
                                    return current_row.cb_value(field_id, cb_id).export()
                            else:
                                def cb_value(current_row=current_row, field_id=field_id,
                                             cb_id=cb_id):
                                    return current_row.cb_value(field_id, cb_id).secret_export()
                            field_dictionary[cb_id] = cb_value
            return dictionary

        def _make_table(self):
            form = self._form
            if form is None:
                return lcg.Content()
            table = pytis.output.data_table(form.view_spec(), form.data(),
                                            condition=form.condition(), sorting=form.sorting(),
                                            transaction=self._transaction)
            return table.lcg()

        def _make_agg(self, op):
            if self._form is None:
                return lcg.Content()
            dictionary = _ProxyDict()
            for column in self._form.data().columns():
                if op == pytis.data.Data.AGG_COUNT or isinstance(column.type(), pytis.data.Number):
                    def value(column=column):
                        return self._make_agg_value(op, column)
                    dictionary[column.id()] = value
            return dictionary

        def _make_agg_value(self, op, column):
            form = self._form
            if form is None:
                return lcg.Content()
            colid = column.id()
            if not pytis.form.has_access(form.name(), perm=pytis.data.Permission.VIEW,
                                         column=colid):
                return column.type().secret_export()
            data = form.data()
            condition = form.condition()
            return data.select_aggregate((op, colid,), condition=condition,
                                         transaction=self._transaction).value()

        def _make_binding(self, binding, current_row):
            binding_dictionary = {}
            binding_name = binding.name()
            binding_condition = binding.condition()
            binding_column = binding.binding_column()
            if binding_column and current_row is not None:
                condition = pytis.data.EQ(binding_column, current_row.row()[0])
                if binding_condition is not None:
                    condition = pytis.data.AND(condition, binding_condition(current_row))
            elif binding_condition and current_row is not None:
                condition = binding_condition(current_row)
            else:
                condition = pytis.data.AND()
            view, data = form_view_data(self._resolver, binding_name)
            table = pytis.output.data_table(view, data, condition=condition, sorting=(),
                                            transaction=self._transaction)
            binding_dictionary['table'] = table.lcg()
            binding_dictionary['data'] = _DataIterator(self._resolver, binding_name,
                                                       condition=condition, sorting=(),
                                                       transaction=self._transaction)
            codebooks = LCGFormatter._retrieve_codebooks(view, resolver=self._resolver)
            binding_dictionary['codebook'] = _DataIterator(self._resolver, binding_name,
                                                           condition=condition, sorting=(),
                                                           transaction=self._transaction,
                                                           codebooks=codebooks)
            return binding_dictionary

    def __init__(self, resolver, output_resolvers, template_id, form=None, form_bindings=None,
                 parameters={}, language=None, translations=(), spec_kwargs=None):
        """Arguments:

          resolver -- form specification resolver
          output_resolvers -- resolver of template names and data objects; may
            also be a non-empty sequence of resolvers, in such a case the first
            resolver not throwing 'ResolverError' when accessing the template
            will be used
          template_id -- id of the output template, string
          form -- current form; 'Form' instance or 'None'
          form_bindings -- bindings of the current form (if it is the main form
            of a dual form) as a sequence of 'Binding' instances; or 'None'
          parameters -- dictionary of form parameters
          language -- language code to pass to the exporter context
          translations -- translations to pass to PDFExporter
          spec_kwargs -- dictionary of keyword arguments to pass to the print
            specification constructor

        """
        self._resolver = resolver
        self._output_resolvers = xtuple(output_resolvers)
        self._template_id = template_id
        self._parameters = HashableDict(parameters)
        self._language = language
        self._translations = translations
        self._spec_kwargs = spec_kwargs or {}
        if not self._resolve(template_id, 'init')[0]:
            raise AbortOutput()
        self._doc_header, __ = self._resolve(template_id, 'doc_header')
        self._doc_footer, __ = self._resolve(template_id, 'doc_footer')
        self._page_header, __ = self._resolve(template_id, 'page_header', default=None)
        self._first_page_header, __ = self._resolve(template_id, 'first_page_header',
                                                    default=self._page_header)
        page_number = pytis.output.Center('Strana ', pytis.output.PageNumber())
        self._page_footer, __ = self._resolve(template_id, 'page_footer', default=page_number)
        self._page_background, __ = self._resolve(template_id, 'background', default=None)
        self._page_layout, __ = self._resolve(template_id, 'page_layout', default={})
        style, __ = self._resolve(template_id, 'style', default=None)
        if style:
            style_parser = lcg.StyleFile()
            style_parser.read(io.StringIO(style))  # doesn't work with cStringIO
            self._style = style_parser.presentations()
        else:
            self._style = None
        body, __ = self._resolve(template_id, 'body')
        if body is None:
            # In order to apply style parameters correctly
            temp_body = pytis.output.StructuredText('')
        else:
            temp_body = body
        parameters = copy.copy(self._template_parameters(temp_body))
        for p, a in (('page_header', self._page_header,),
                     ('page_footer', self._page_footer,),
                     ('first_page_header', self._first_page_header,),
                     ('page_background', self._page_background,),
                     ):
            if p not in parameters:
                if a is None:
                    value = None
                elif isinstance(a, list):  # presentation style
                    value = a
                elif isinstance(a, lcg.Content):
                    value = a
                else:
                    value = a.lcg()
                parameters[p] = value
        self._body_parameters = parameters
        if ((not isinstance(body, pytis.output.Document) and
             body and
             not (isinstance(body, (tuple, list)) and body and
                  isinstance(body[0], pytis.output.Document)))):
            body.lcg()  # to generate parameters
            body = pytis.output.Document(body, **parameters)
        else:
            # It's unclear how to interpret this situation.  In the Lout
            # formatter the parameters were apparently taken from
            # specifications and applied globally, but they could also be given
            # in the Document's (and ignored there?), with the exception of
            # background.  So let's them add to Document's if they are not
            # present there yet.
            if isinstance(body, pytis.output.Document):
                body_list = [body]
            else:
                body_list = body
            if isinstance(body_list, (tuple, list)):
                for document in body_list:
                    for k, v in parameters.items():
                        name = 'arg_' + k
                        if getattr(document, name) is None:
                            setattr(document, name, v)
        self._body = body
        self._form = form
        self._form_bindings = form_bindings
        self._row_template, __ = self._resolve(template_id, 'row', default=None)
        self._application_variables, __ = self._resolve(template_id, 'variables', default={})
        if form is None:
            self._codebooks = None
        else:
            self._codebooks = self._retrieve_codebooks(form.view_spec(), resolver=self._resolver)

    def _resolve(self, template_id, element, default=''):
        result = default
        for resolver in xtuple(self._output_resolvers):
            try:
                result = resolver.get(template_id, element, parameters=self._parameters,
                                      **self._spec_kwargs)
            except ResolverError:
                continue
            break
        else:
            resolver = None
        return result, resolver

    @classmethod
    def _retrieve_codebooks(class_, view_spec, resolver=None):
        if resolver is None:
            resolver = pytis.util.resolver()
        codebooks = []
        for field in view_spec.fields():
            cb = field.codebook()
            if cb:
                try:
                    cb_spec = resolver.get(cb, 'view_spec')
                except ResolverError:
                    cb_spec = None
                if cb_spec:
                    cb_fields = [(f.id(), f.label(),) for f in cb_spec.fields() if f.label()]
                    if cb_fields:
                        codebooks.append((field.id(), cb_fields, cb,))
        return codebooks

    def _template_parameters(self, template):
        if isinstance(template, pytis.output.StructuredText):
            template.lcg()
            parameters = template.parameters()
        else:
            parameters = {}
        return parameters

    def _pdf(self):
        start_time = pytis.data.DateTime.now()
        T = pytis.data.DBTransactionDefault
        transaction = T(connection_data=pytis.config.dbconnection, isolation=T.REPEATABLE_READ)
        template_nodes = []
        if self._form is not None and self._row_template is not None:
            i = 1
            row_template = self._row_template
            for row in self._form.presented_rows():
                row_lcg_globals = self._LCGGlobals(self._resolver, self._form, self._form_bindings,
                                                   self._codebooks, transaction, current_row=row,
                                                   parameters=self._parameters)
                id_ = 'pytissubdoc%d' % (i,)
                row_template_lcg = row_template.lcg()
                parameters = self._template_parameters(row_template)
                document = lcg.ContentNode(id=id_, title=' ',  # let's avoid printing the id
                                           content=row_template_lcg, globals=row_lcg_globals,
                                           **parameters)
                template_nodes.append(document)
                i += 1
        lcg_globals = self._LCGGlobals(self._resolver, self._form, self._form_bindings,
                                       self._codebooks, transaction, parameters=self._parameters)
        lcg_globals['app'] = self._application_variables

        def margin(key):
            size = self._page_layout.get(key)
            if size is None:
                size = UMm(10)
            return size
        presentation = lcg.Presentation(
            font_name='DejaVu',
            font_family=lcg.FontFamily.FIXED_WIDTH,
            top_margin=margin(PAGE_TOP_MARGIN),
            bottom_margin=margin(PAGE_BOTTOM_MARGIN),
            left_margin=margin(PAGE_LEFT_MARGIN),
            right_margin=margin(PAGE_RIGHT_MARGIN),
            page_width=self._page_layout.get(PAGE_WIDTH),
            page_height=self._page_layout.get(PAGE_HEIGHT),
            landscape=self._page_layout.get(PAGE_LANDSCAPE_MODE),
        )
        start_time_export = pytis.data.DateTime.now()
        exporter = lcg.pdf.PDFExporter(translations=self._translations)
        body = xtuple(self._body) if self._body else ()
        body_nodes = [doc.lcg_document(globals=lcg_globals) for doc in body]
        root_node = lcg.ContentNode(id='__dummy', content=lcg.Content(),
                                    children=body_nodes + template_nodes,
                                    **self._body_parameters)
        context = exporter.context(root_node, self._language,
                                   presentation=lcg.PresentationSet(self._style or []))
        try:
            pdf = exporter.export(context, global_presentation=presentation)
        except lcg.SubstitutionIterator.IteratorError as e:
            message = _("Invalid use of iterator.\n"
                        "Maybe you refer to an non-existent or inaccessible object in the table?")
            message += "\n" + unicode(e)
            pytis.form.run_dialog(pytis.form.Error, message)
            return ''
        show_time = pytis.data.DateTime.now()
        log(EVENT, ('Output formatting took %.3fs (PDF export %.3fs)' %
                    (pytis.data.DateTime.diff_seconds(start_time, show_time),
                     pytis.data.DateTime.diff_seconds(start_time_export, show_time),)))
        try:
            transaction.commit()
        except pytis.data.DBSystemException:
            pass
        return pdf

    def pdf(self):
        "Return the formatted document as PDF data (basestring)."
        return self._pdf()

    def preview(self, stream):
        """Return the formatted document as a plain text.

        Arguments:

          stream -- stream open for writing, providing 'write' method.

        'stream' gets closed by this method after its writing is finished.

        """

    def printout(self, stream):
        """Send the document as PDF to 'stream'.

        Arguments:

          stream -- stream open for writing, providing 'write' method.

        Closing the 'stream' is left up to the caller.

        """
        stream.write(self._pdf())

    def cleanup(self):
        """Call the cleanup method from the print specification.

        This method should be called after displaying the result of
        'printout()' to the user.

        """
        self._resolve(self._template_id, 'cleanup')

    def close(self):
        """Obsolete, no need to call this method anymore."""
        pass

    @staticmethod
    def template_help(row, module):
        """Return help text to put into a template editing form.

        Arguments:

          module -- name of the specification to generate the help for, string

        """
        cid = _("COLUMN_IDENTIFIER")
        text = (_("Primary template variables:") + "\n" +
                "".join(["  ${%s} ... %s\n" % (name, descr) for name, descr in (
                    ('current_row.' + cid, _("column value insertion")),
                    ('table', _("complete table insertion")),
                    ('data.' + cid, _("value insertion in repeated table row")))]) + "\n")
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(module, 'view_spec')
        except ResolverError:
            return text
        bindings = view_spec.bindings()
        bindings = [b for b in bindings if b.name() and pytis.form.has_access(b.name())]
        text += _("Column identifiers:") + "\n"
        for field in view_spec.fields():
            text += '  %s ... %s\n' % (field.id(), field.label(),)
        text += "\n"
        codebooks = LCGFormatter._retrieve_codebooks(view_spec, resolver=resolver)
        if codebooks:
            text += _("Codebooks:") + "\n"
            for field_id, cb_fields, _cb in codebooks:
                for cb_id, cb_label in cb_fields:
                    text += "  ${codebook.%s.%s} ... %s\n" % (field_id, cb_id, cb_label,)
        if bindings:
            text += _("Side forms:") + "\n"
            for b in bindings:
                binding_id = re.sub('[^A-Za-z0-9_]', '_', b.id())
                form_name = b.name()
                if ((pytis.form.has_access(form_name) and
                     pytis.form.has_access(form_name, perm=pytis.data.Permission.PRINT))):
                    text += '  ${Binding.%s.table} ... %s\n' % (binding_id, b.title(),)
                    text += ('  ${Binding.%s.data.%s} ... %s\n' %
                             (binding_id, cid, b.title(),))
                    text += "  " + _("Column identifiers:") + "\n"
                    sub_view_spec = resolver.get(b.name(), 'view_spec')
                    for field in sub_view_spec.fields():
                        text += '    %s ... %s\n' % (field.id(), field.label(),)
                    codebooks = LCGFormatter._retrieve_codebooks(sub_view_spec, resolver=resolver)
                    if codebooks:
                        text += "  " + _("Codebooks:")
                        for field_id, cb_fields, _cb in codebooks:
                            for cb_id, cb_label in cb_fields:
                                text += ('  ${Binding.%s.codebook.%s.%s} ... %s\n' %
                                         (binding_id, field_id, cb_id, cb_label,))
                    text += "\n"
        text += (_("Aggregation variables:") + "\n" +
                 "".join(["  ${agg.%s.%s} ...%s %s\n" % (agg, cid, '.' * (3 - len(name)), name)
                          for agg, name in (('min', _("minimum")),
                                            ('max', _("maximum")),
                                            ('count', _("count")),
                                            ('sum', _("sum")),
                                            ('avg', _("average")))]))
        return text


Formatter = LCGFormatter
"""Implicitní formátovač."""


class PrintSpecification(object):
    """Specification of printed output.

    Every method provides specification of the corresponding printed output
    element.  Every printed specification should define 'body' content, the
    other specifications are optional.

    Additionally, it's possible to define initial actions to be performed
    before printing, typically asking user for dynamic parameters of the
    output.  You can use 'init()' method for that purpose.

    """

    def __init__(self, parameters):
        """
        Arguments:

          parameters -- dictionary of print parameters

        """
        self._parameters = dict(parameters)

    def _parameter(self, name, default=None):
        if isinstance(name, (tuple, list)):
            name = '/'.join(name)
        return self._parameters.get(name, default)

    def _add_parameter(self, name, value):
        self._parameters[name] = value

    def init(self):
        """Run actions to be performed before the printing starts.

        Return true if printing can continue.  If the user interaction
        indicates that printing should be aborted, return false.

        """
        return True

    def cleanup(self):
        """Run actions to be performed after output formatting."""
        pass

    def body(self):
        """Return body of the document.

        Returns pytis 'Document' instance or a sequence of 'Document' instances
        or any pytis markup content acceptable by 'Document' constructor.

        """
        return None

    def page_header(self):
        """Return header of a page.

        Returns '_Mark' instance.

        """
        return None

    def first_page_header(self):
        """Return header of the first page.

        It is necessary to define this only when the first page header should
        be different from the header returned from 'page_header'.

        Returns '_Mark' instance.

        """
        return self.page_header()

    def page_footer(self):
        """Return footer of a page.

        Returns '_Mark' instance.

        """
        return pytis.output.Center('Strana ', pytis.output.PageNumber())

    def doc_header(self):
        """Return initial part of the whole document.

        Returns '_Mark' instance.

        """
        return None

    def doc_footer(self):
        """Return closing part of the document.

        Returns '_Mark' instance.

        """
        return None

    def page_layout(self):
        """Return dictionary of page parameters.

        Returns dictionary of supported page parameters.

        """
        return {}

    def background(self):
        """Return background image to be put on each page.

        Returns '_Mark' instance.

        """
        return None

    def style(self):
        """Return style specification of the document.

        Returns textual style specification as basestring.

        """
        return None
