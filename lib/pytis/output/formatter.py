# -*- coding: utf-8 -*-

# Formátování výstupu
# 
# Copyright (C) 2002-2011 Brailcom, o.p.s.
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

from __future__ import unicode_literals

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

import copy
import cStringIO
import codecs
import config
import functools
import operator
import os
import re
import string
import StringIO
import thread

import pytis.data
import pytis.form
from pytis.output import *
import pytis.presentation
import lcg

PAGE_WIDTH = 'pwidth'
"""Šířka stránky, instance třídy 'Unit'."""
PAGE_HEIGHT = 'pheight'
"""Výška stránky, instance třídy 'Unit'."""
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


class AbortOutput(Exception):
    """Exception raised when printing should be aborted."""

    
class _ProxyDict(dict):
    def __getitem__(self, key):
        result = dict.__getitem__(self, key)
        if (not isinstance(result, (basestring, lcg.Content, _ProxyDict,)) and
            callable(result)):
            result = self[key] = result()
        return result
class HashableDict(dict):
    def __hash__(self):
        return 0
    def __setitem__(self, key, value):
        raise Exception('immutable object')
class _DataIterator(lcg.SubstitutionIterator):
    def __init__(self, resolver, form_name, condition, sorting, transaction):
        self._transaction = transaction
        view = resolver.get(form_name, 'view_spec')
        data_spec = resolver.get(form_name, 'data_spec')
        import config
        self._data = data_spec.create(dbconnection_spec=config.dbconnection)
        self._data.select(condition=condition, sort=sorting, transaction=transaction)
        self._presented_row = pytis.presentation.PresentedRow(view.fields(), self._data, None,
                                                              singleline=True)
        super(_DataIterator, self).__init__()
    def _value(self):
        row = self._presented_row
        return dict([(k, row[k].export(),) for k in row.keys()])
    def _next(self):
        row = self._data.fetchone(transaction=self._transaction)
        if row is None:
            return False
        self._presented_row.set_row(row)
        return True
    def _reset(self):
        self._data.rewind()
class _FormDataIterator(_DataIterator):
    def __init__(self, resolver, form, transaction):
        name = form.name()
        condition=form.condition()
        sorting=form.sorting()
        if condition is None:
            try:
                condition = resolver.p((name, P_CONDITION))
            except ResolverError:
                pass
        if sorting is None:
            sorting = resolver.p((name, P_SORTING))
        super(_FormDataIterator, self).__init__(resolver, name, condition=condition,
                                                sorting=sorting, transaction=transaction)


class LCGFormatter(object):
    """LCG based formatter."""

    class _DummyDict(dict):
        def __getitem__(self, key):
            return ''

    class _LCGGlobals(_ProxyDict):
        def __init__(self, resolvers, form, form_bindings, current_row=None):
            self._resolvers = resolvers
            if form is not None:
                name = form.name()
                for r in resolvers:
                    try:
                        r.get(name, 'view_spec')
                    except ResolverError:
                        continue
                    self._selected_resolver = r
                    break
                else:
                    log(EVENT, 'No working resolver found ' + name)
                    self._selected_resolver = resolvers[0]
            self._form = form
            self._form_bindings = form_bindings
            import config
            T = pytis.data.DBTransactionDefault
            self._transaction = T(connection_data=config.dbconnection, isolation=T.SERIALIZABLE)
            dictionary = self._initial_dictionary(form, form_bindings, current_row)
            _ProxyDict.__init__(self, dictionary)
        def _initial_dictionary(self, form, form_bindings, current_row):
            dictionary = _ProxyDict()
            if form is not None:
                if current_row is None:
                    current_row = form.current_row()
                if current_row is None:
                    current_row_dictionary = LCGFormatter._DummyDict()
                else:
                    current_row_dictionary = dict([(k, current_row[k].export(),)
                                                   for k in current_row.keys()])
                dictionary['current_row'] = current_row_dictionary
                dictionary['data'] = _FormDataIterator(self._selected_resolver, form,
                                                       transaction=self._transaction)
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
                        if (pytis.form.has_access(form_name) and
                            pytis.form.has_access(form_name, perm=pytis.data.Permission.PRINT)):
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
            return dictionary
        def _make_table(self):
            form = self._form
            if form is None:
                return lcg.Content()
            table = pytis.output.data_table(self._selected_resolver, form.name(),
                                            condition=form.condition(), sorting=form.sorting(),
                                            transaction=self._transaction)
            return table.lcg()
        def _make_agg(self, op):
            if self._form is None:
                return lcg.Content()
            dictionary = _ProxyDict()
            for column in self._form.data().columns():
                if op == pytis.data.Data.AGG_COUNT or isinstance(column.type(), pytis.data.Number):
                    colid = column.id()
                    def value(colid=colid):
                        return self._make_agg_value(op, colid)
                    dictionary[colid] = value
            return dictionary
        def _make_agg_value(self, op, column):
            form = self._form
            if form is None:
                return lcg.Content()
            data = form.data()
            condition = form.condition()
            return data.select_aggregate((op, column,), condition=condition,
                                         transaction=self._transaction).value()
        def _make_binding(self, binding, current_row):
            binding_dictionary = {}
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
            table = pytis.output.data_table(self._selected_resolver, binding.name(), condition=condition,
                                            sorting=(), transaction=self._transaction)
            binding_dictionary['table'] = table.lcg()
            binding_dictionary['data'] = _DataIterator(self._selected_resolver, binding.name(),
                                                       condition=condition, sorting=(),
                                                       transaction=self._transaction)
            return binding_dictionary
        
    def __init__(self, resolvers, template_id, form=None, form_bindings=None):
        """
        Arguments:

          resolvers -- resolver of template names and data objects; may also be
            a non-empty sekvence of resolvers, in such a case the first
            resolver not throwing 'ResolverError' when accessing the
            template will be used
          template_id -- id of the output template, string
          form -- current form; 'Form' instance or 'None'
          form_bindings -- bindings of the current form (if it is the main form
            of a dual form) as a sequence of 'Binding' instances; or 'None'
            
        """
        self._resolvers = xtuple(resolvers)
        self._template_id = template_id
        output_parameters, r = self._resolve(template_id, 'init')
        if r is not None:
            if output_parameters is None:
                raise AbortOutput()
            r.add_output_parameters(output_parameters)
        self._doc_header, __ = self._resolve(template_id, 'doc_header')
        self._doc_footer, __ = self._resolve(template_id, 'doc_footer')
        self._page_header, __ = self._resolve(template_id, 'page_header', default=None)
        self._first_page_header, __ = self._resolve(template_id, 'first_page_header',
                                                    default=self._page_header)
        self._page_footer, __ = self._resolve(template_id, 'page_footer',
                                              default=Center('Strana ', PageNumber()))
        self._page_background, __ = self._resolve(template_id, 'background', default=None)
        self._page_layout, __ = self._resolve(template_id, 'page_layout', default={})
        style, __ = self._resolve(template_id, 'style', default=None)
        if style:
            style_parser = lcg.StyleFile()
            style_parser.read(StringIO.StringIO(style)) # doesn't work with cStringIO
            self._style = style_parser.presentations()
        else:
            self._style = None
        body, __ = self._resolve(template_id, 'body')
        parameters = copy.copy(self._template_parameters(body))
        for p, a in (('page_header', self._page_header,),
                     ('page_footer', self._page_footer,),
                     ('first_page_header', self._first_page_header,),
                     ('page_background', self._page_background,),
                     ('presentation', self._style,),
                     ):
            if p not in parameters:
                if a is None:
                    value = None
                elif isinstance(a, list): # presentation style
                    value = a
                else:
                    value = a.lcg()
                parameters[p] = {None: value}
        self._body_parameters = parameters
        if (not isinstance(body, Document) and
            body and
            not (is_sequence(body) and body and isinstance(body[0], Document))):
            body.lcg() # to generate parameters
            simple_parameters = dict([(k, v[None],) for k, v in parameters.items()])
            body = Document(body, **simple_parameters)
        else:
            # It's unclear how to interpret this situation.  In the Lout
            # formatter the parameters were apparently taken from
            # specifications and applied globally, but they could also be given
            # in the Document's (and ignored there?), with the exception of
            # background.  So let's them add to Document's if they are not
            # present there yet.
            if isinstance(body, Document):
                body_list = [body]
            else:
                body_list = body
            if is_sequence(body_list):
                for document in body_list:
                    for k, v in parameters.items():
                        name = 'arg_' + k
                        if getattr(document, name) is None:
                            setattr(document, name, v[None])
        self._body = body
        self._form = form
        self._form_bindings = form_bindings
        self._row_template, __ = self._resolve(template_id, 'row', default=None)
        self._application_variables, __ = self._resolve(template_id, 'variables', default={})

    def _resolve(self, template_id, element, default=''):
        result = default
        for resolver in xtuple(self._resolvers):
            try:
                result = resolver.get(template_id, element)
            except ResolverError:
                continue
            break
        else:
            resolver = None
        return result, resolver

    def _template_parameters(self, template):
        if isinstance(template, StructuredText):
            template.lcg()
            parameters = template.parameters()
        else:
            parameters = {}
        return parameters

    def _pdf(self):
        start_time = pytis.data.DateTime.now()
        children = []
        if self._form is not None and self._row_template is not None:
            i = 1
            row_template = self._row_template
            for row in self._form.presented_rows():
                row_lcg_globals = self._LCGGlobals(self._resolvers, self._form, self._form_bindings,
                                                   current_row=row)
                id_ = 'pytissubdoc%d' % (i,)    
                row_template_lcg = row_template.lcg()
                parameters = self._template_parameters(row_template)
                document = lcg.ContentNode(id=id_, title=' ', # let's avoid printing the id
                                           content=row_template_lcg, globals=row_lcg_globals,
                                           **parameters)
                children.append(document)
                i += 1
        lcg_globals = self._LCGGlobals(self._resolvers, self._form, self._form_bindings)
        lcg_globals['app'] = self._application_variables
        body = self._body
        if not body:
            body = []
        elif not isinstance(body, (list, tuple,)):
            body = [body]            
        children = ([document.lcg_document(globals=lcg_globals) for document in body] +
                    children)
        lcg_content = lcg.ContentNode(id='__dummy', content=lcg.Content(), children=children,
                                      **self._body_parameters)
        presentation = lcg.Presentation()
        presentation.font_name = 'DejaVu'
        presentation.font_family = lcg.FontFamily.FIXED_WIDTH
        def margin(key):
            size = self._page_layout.get(key)
            if size is None:
                size = UMm(10)
            return size
        presentation.top_margin = margin(PAGE_TOP_MARGIN)
        presentation.bottom_margin = margin(PAGE_BOTTOM_MARGIN)
        presentation.left_margin = margin(PAGE_LEFT_MARGIN)
        presentation.right_margin = margin(PAGE_RIGHT_MARGIN)
        presentation.page_width = self._page_layout.get(PAGE_WIDTH)
        presentation.page_height = self._page_layout.get(PAGE_HEIGHT)
        presentation.landscape = self._page_layout.get(PAGE_LANDSCAPE_MODE)
        start_time_export = pytis.data.DateTime.now()
        exporter = lcg.pdf.PDFExporter()
        presentation_args = [(presentation, lcg.ContentMatcher(),)] + (self._style or [])
        presentation_set = lcg.PresentationSet(presentation_args)
        context = exporter.context(lcg_content, None, presentation=presentation_set)
        try:
            pdf = exporter.export(context)
        except lcg.SubstitutionIterator.IteratorError, e:
            message = _("Chybné použití iterátoru.\n"
                        "Možná se v tabulce odkazujete na neexistující nebo nepřístupný objekt?\n")
            message += unicode(e)
            pytis.form.run_dialog(pytis.form.Error, message)
            return ''
        show_time = pytis.data.DateTime.now()
        log(EVENT, ('Output formatting took %.3fs (PDF export %.3fs)' %
                    (pytis.data.DateTime.diff_seconds(start_time, show_time),
                     pytis.data.DateTime.diff_seconds(start_time_export, show_time),)))
        return pdf
        
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

        'stream' gets closed by this method after its writing is finished.

        """
        stream.write(self._pdf())
        stream.close()
        self._resolve(self._template_id, 'cleanup')
    
    def printdirect(self):
        """Send the document as PDF to the standard input of 'printing_command'."""
        process = Popen(config.printing_command, from_child=dev_null_stream('w'))
        stream = process.to_child()
        self.printout(stream)
    
    def close(self):
        """Obsolete, no need to call this method anymore."""
        pass

    @staticmethod
    def template_help(row, module):
        """Return help text to put into a template editing form.

        Arguments:

          module -- name of the specification to generate the help for, string

        """
        text = u""
        text += _("""Proměnné pro hlavní šablonu:
  ${current_row.IDENTIFIKÁTOR_SLOUPCE} ... vložení hodnoty sloupce
  ${table} ... vložení celé tabulky
  ${data.IDENTIFIKÁTOR_SLOUPCE} ... vložení hodnoty v opakovaném řádku tabulky
  
""")
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(module, 'view_spec')
        except ResolverError:
            return text
        bindings = view_spec.bindings()
        bindings = [b for b in bindings if b.name() and pytis.form.has_access(b.name())]
        text += _("Identifikátory sloupců:\n")
        for field in view_spec.fields():
            text += _("  %s ... %s\n") % (field.id(), field.label(),)
        text += "\n"
        if bindings:
            text += _("Vedlejší formuláře:\n")
            for b in bindings:
                binding_id = re.sub('[^A-Za-z0-9_]', '_', b.id())
                form_name = b.name()
                if (pytis.form.has_access(form_name) and
                    pytis.form.has_access(form_name, perm=pytis.data.Permission.PRINT)):
                    text += "  ${Binding.%s.table} ... %s\n" % (binding_id, b.title(),)
                    text += "  ${Binding.%s.data.IDENTIFIKÁTOR_SLOUPCE} ... %s\n" % (binding_id, b.title(),)
                    text += _("  Identifikátory sloupců:\n")
                    sub_view_spec = resolver.get(b.name(), 'view_spec')
                    for field in sub_view_spec.fields():
                        text += _("    %s ... %s\n") % (field.id(), field.label(),)
                    text += "\n"
        text += _("""Agregační proměnné:
  ${agg.min.IDENTIFIKÁTOR_SLOUPCE} ..... minimum
  ${agg.max.IDENTIFIKÁTOR_SLOUPCE} ..... maximum
  ${agg.count.IDENTIFIKÁTOR_SLOUPCE} ... počet
  ${agg.sum.IDENTIFIKÁTOR_SLOUPCE} ..... součet
  ${agg.avg.IDENTIFIKÁTOR_SLOUPCE} ..... průměr
  
""")
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
    def __init__(self, resolver):
        """
        Arguments:

          resolver -- 'Resolver' instance available to instance methods
        
        """
        self._resolver = resolver

    def init(self):
        """Run actions to be performed before the printing starts.

        Return dictionary of arbitrary keys and values.  The dictionary is
        later given as an argument to the content generating methods.  If the
        user interaction indicates that printing should be aborted, return
        'None'.
        
        """
        return HashableDict()
        
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
        return Center('Strana ', PageNumber())
    
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
