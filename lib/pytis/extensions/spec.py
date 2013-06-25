# -*- coding: utf-8 -*-
#
# Copyright (C) 2005-2013 Brailcom, o.p.s.
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

"""Funkce a třídy pro zjednodušení a zpřehlednění tvorby specifikačních souborů.

Do tohoto modulu patří vše, co nějakým způsobem obaluje API Pytisu.  Jedná se
většinou o funkce, které se hodí v různých konkrétních situacích, kde vyjádření
nějaké konstrukce vyžaduje složitější zápis, ale protože se tato konstrukce
často opakuje, je možné ji parametrizovaně vytvořit automaticky.

"""

from pytis.extensions import *
from pytis.presentation import *

import collections
import tempfile
import config
import pytis.util

_ = pytis.util.translations('pytis-wx')


# Zkratky na často používané identifikátory.
    
ASC = pytis.data.ASCENDENT
DESC = pytis.data.DESCENDANT

UPCASE = PostProcess.UPPER
LOWER = PostProcess.LOWER

ALPHA = TextFilter.ALPHA
NUMERIC = TextFilter.NUMERIC
ALPHANUMERIC = TextFilter.ALPHANUMERIC
ASCII = TextFilter.ASCII
FLOAT = TextFilter.FLOAT

ALWAYS = Editable.ALWAYS
ONCE = Editable.ONCE
NEVER = Editable.NEVER

BROWSE_FORM = FormType.BROWSE
EDIT_FORM = FormType.EDIT
INSERT_FORM = FormType.INSERT
VIEW_FORM = FormType.VIEW

FIELD_STYLE_DEFAULT = Style()
FIELD_STYLE_EMPHASIS = Style(bold=True)
FIELD_STYLE_WARNING = Style(foreground=Color.RED)

# Backwards compatibility alias.
FieldStyle = Style

# Funkce pro zjednodušení vytváření položek menu.

def run_form_mitem(title, name, form_class, hotkey=None, **kwargs):
    cmd = pytis.form.Application.COMMAND_RUN_FORM
    args = dict(form_class=form_class, name=name, **kwargs)
    descr = {
        pytis.form.BrowseForm: u"řádkový formulář",
        pytis.form.PopupEditForm: u"editační formulář",
        pytis.form.Form: u"duální řádkový formulář",
        pytis.form.CodebookForm: u"číselníkový formulář",
        pytis.form.DescriptiveDualForm: u"duální náhledový formulář",
    }.get(form_class, u"formulář")
    help = _(u'Otevřít %s "%s"') % (descr, title.replace('&', ''))
    return pytis.form.MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)

def new_record_mitem(title, name, hotkey=None, **kwargs):
    cmd = pytis.form.Application.COMMAND_NEW_RECORD
    args = dict(kwargs, name=name)
    help = _(u'Otevřít vstupní formulář "%s"') % title
    return pytis.form.MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)

def run_procedure_mitem(title, name, proc_name, hotkey=None, groups=None, enabled=None, **kwargs):
    cmd = pytis.form.Application.COMMAND_RUN_PROCEDURE
    if groups is not None:
        assert isinstance(groups, (tuple, list))
        assert enabled is None or isinstance(enabled, collections.Callable)
        enabled_ = enabled
        def enabled(**kwargs_):
            if not is_in_groups(groups):
                return False
            if enabled_ is not None:
                return enabled_(**kwargs_)
            return True
    return pytis.form.MItem(title, command=cmd, hotkey=hotkey,
                            args=dict(spec_name=name, proc_name=proc_name,
                                      enabled=enabled, **kwargs),
                            help='Spustit proceduru "%s"' % title)

nr = new_record_mitem
rp = run_procedure_mitem
def bf(title, name, hotkey=None, **kwargs):
    return run_form_mitem(title, name, pytis.form.BrowseForm, hotkey, **kwargs)
def df(title, name, hotkey=None, **kwargs):
    return run_form_mitem(title, name, pytis.form.BrowseDualForm, hotkey,
                          **kwargs)
def mf(title, name, hotkey=None, **kwargs):
    return run_form_mitem(title, name, pytis.form.MultiBrowseDualForm, hotkey, **kwargs)
def sf(title, name, hotkey=None, **kwargs):
    return run_form_mitem(title, name, pytis.form.ShowForm, hotkey, **kwargs)
def ddf(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.DescriptiveDualForm, hotkey)
def ef(title, name, hotkey=None, **kwargs):
    return run_form_mitem(title, name, pytis.form.PopupEditForm, hotkey,
                          **kwargs)


# Další funkce pro zjednodušení často používaných konstrukcí

def get_value(value, default=None):
    """Return the Python value of given 'pytis.data.Value' instance.

    Accepts a 'pytis.data.Value' instance or None.  If the 'value' is None, the 'default' value
    will be returned without complaining.  If the 'value' is a 'pytis.data.Value' instance, its
    internal (Python) value will be returned.
    
    """
    if value is None:
        return default
    else:
        return value.value()
    
def format_value(value, default=None):
    """Return the formatted (string) value of given 'pytis.data.Value' instance.

    Accepts a 'pytis.data.Value' instance or None.  If the 'value' is None, the 'default' value
    will be returned without complaining.  If the 'value' is a 'pytis.data.Value' instance, its
    formatted (string) value will be returned.
    
    """
    if value is None:
        return default
    else:
        return value.export()

def rp_handler(spec_name, proc_name, *args, **kwargs):
    """Vrať handler uživatelské akce, který spustí proceduru s danými argumenty.

    Vrácený handler vyvolá proceduru 'proc_name' ze specifikace 'spec_name' a
    předá jí všechny hodnoty aktuálního řádku odpovídající klíčům předaným v
    'args'.  Args je tedy seznamem identifikátorů sloupců.  Hodnoty jsou
    proceduře předány jako poziční argumenty v daném pořadí.

    Klíčové argumenty jsou předány beze změn.
    
    """
    if __debug__:
        for arg in (spec_name, proc_name) + args:
            assert isinstance(arg, basestring)
    return lambda row: pytis.form.run_procedure(spec_name, proc_name,
                                                *[row[key] for key in args],
                                                **kwargs)


def cb2colvalue(value, column=None, transaction=None):
    """Převeď hodnotu políčka na hodnotu uvedeného sloupce navázaného číselníku.
    
    Argumenty:

      value -- Instance `Value', jejíž typ má definován enumerátor typu
        'pytis.data.DataEnumerator'.
      column -- název sloupce číselníku poskytujícího výslednou hodnotu.
      transaction -- transakce pro předání datovým operacím.

    Pokud odpovídající řádek není nalezen, bude vrácena instance 'Value'
    stejného typu, jako je typ argumentu 'value' s hodnotou nastavenou na
    'None'.  Takováto hodnota nemusí být validní hodnotou typu, ale
    zjednodušuje se tím práce s výsledkem.  Pokud je zapotřebí korektnějšího
    chování, je doporučeno použít přímo metodu 'DataEnumerator.row()'
    (například voláním 'value.type().enumerator().row(value.value())'.
        
    """
    assert isinstance(value, pytis.data.Value)
    assert value.type().enumerator() is not None
    if column is None:
        return value
    else:
        row = value.type().enumerator().row(value.value(), transaction=transaction)
        if row is not None:
            return row[column]
        else:
            return pytis.data.Value(value.type(), None)


def run_cb(spec, begin_search=None, condition=None, sort=(),
           columns=None, select_row=0, multirow=False,
           filter=None, transaction=None):
    """Vyvolá číselník určený specifikací.

    Argumenty:

      spec -- název specifikace číselníku.
      begin_search -- None nebo jméno sloupce, nad kterým se má vyvolat
        inkrementální vyhledávání.
      condition -- podmínka pro filtrování záznamů.
      sort -- řazení (viz pytis.data.select())
      columns -- seznam sloupců, pokud se má lišit od seznamu uvedeného
        ve specifikaci.
      select_row -- řádek, na který se má nastavit kurzor.
      multirow -- umožní výběr více řádků.
      transaction -- transakce pro předání CodebookFormu.
    
    Vrací None (pokud není vybrán žádný řádek) nebo vybraný řádek nebo
    tuple vybraných řádků (pokud je argument multirow nastaven).
    
    """
    if multirow:
        class_ = pytis.form.SelectRowsForm
    else:
        class_ = pytis.form.CodebookForm
    return pytis.form.run_form(class_, spec, columns=columns,
                               begin_search=begin_search,
                               condition=condition,
                               sorting=sort,
                               select_row=select_row,
                               filter=filter,
                               transaction=transaction)


def help_window(inputfile=None, format=TextFormat.PLAIN):
    if not inputfile:
        pytis.form.run_dialog(pytis.form.Warning, _(u"Textový soubor nenalezen"))
        return
    path = os.path.join(config.help_dir, inputfile)
    if not os.path.exists(path):
        dir, xx = os.path.split(os.path.realpath(pytis.extensions.__file__))
        p = os.path.normpath(os.path.join(dir, '../../../doc', inputfile))
        if os.path.exists(p):
            path = p
        else:
            log(OPERATIONAL, "Soubor nenalezen:", p)
    try:
        f = open(path, 'r')
    except IOError as e:
        pytis.form.run_dialog(pytis.form.Error,
                              _(u"Nemohu otevřít soubor nápovědy: %s") % e)
    else:
        text = f.read()
        f.close()
        pytis.form.InfoWindow(_(u"Nápověda"), text=text, format=format)
        

def run_any_form():
    form_types = (
        ("BrowseForm", pytis.form.BrowseForm),
        ("PopupEditForm", pytis.form.PopupEditForm),
        ("PopupInsertForm", pytis.form.PopupInsertForm),
        ("BrowseDualForm", pytis.form.BrowseDualForm),
        ("MultiBrowseDualForm", pytis.form.MultiBrowseDualForm),
        ("CodebookForm", pytis.form.CodebookForm),
    )
    row = pytis.form.run_form(
        pytis.form.InputForm,
        title=_("Spustit formulář"),
        fields=(Field('type', _(u"Typ formuláře"), not_null=True,
                      enumerator=pytis.data.FixedEnumerator([x[0] for x in form_types]),
                      default='BrowseForm'),
                Field('name', _(u"Název specifikace"), width=40),
                ))
    if row is not None:
        form_type = dict(form_types)[row['type'].value()]
        pytis.form.run_form(form_type, row['name'].value())
cmd_run_any_form = \
    pytis.form.Application.COMMAND_HANDLED_ACTION(handler=run_any_form)


def printdirect(resolver, spec, print_spec, row, output_file=None, **kwargs):
    """Print specification to an output file or show it in a PDF viewer.

    Arguments:

      resolver -- resolver for standard specification resolving
      spec -- name of the specification for print resolver
      print_spec -- name of the print specification for 'pytis.output.Formatter'
      row -- row data for print resolver
      output_file -- name of the file to write output PDF data to, string; if
        'None' then show the output in an external PDF viewer
      kwargs -- passed to the print resolver for use in the print procedure

    Return True if the document was printed or displayed; return False if the
    action was aborted.

    """
    import pytis.output
    class _PrintResolver (pytis.output.OutputResolver):
        P_NAME = 'P_NAME'
        class _Spec:
            def body(self, resolver):
                return None
            def doc_header(self, resolver):
                return None
            def doc_footer(self, resolver):
                return None
        def __init__(self, print_resolver, specification_resolver, old=False, **kwargs):
            pytis.output.OutputResolver.__init__(self, print_resolver, specification_resolver,
                                                 **kwargs)
            self._old = old
        def _get_module(self, module_name):
            if self._old:
                module_name = os.path.join('output', module_name)
            try:
                result = pytis.output.OutputResolver._get_module(self, module_name)
            except pytis.util.ResolverError:
                result = self._Spec()
            return result
        
    log(EVENT, 'Vyvolání tiskového formuláře')
    P = _PrintResolver
    parameters = {(spec + '/' + pytis.output.P_ROW): row}
    parameters.update({P.P_NAME: spec})
    parameters.update(kwargs)
    print_file_resolver = pytis.output.FileResolver(config.print_spec_dir)
    print_resolver = P(print_file_resolver, resolver, parameters=parameters)
    old_print_resolver = P(print_file_resolver, resolver, parameters=parameters, old=True)
    resolvers = (print_resolver, old_print_resolver,)
    try:
        formatter = pytis.output.Formatter(resolvers, print_spec)
    except pytis.output.AbortOutput:
        return False
    if output_file:
        formatter.printout(output_file)
    else:
        file_ = pytis.util.TemporaryFile()
        formatter.printout(file_, hook=(lambda: pytis.form.run_viewer(file_)))
    return True
        

def print2mail(resolver, spec, print_spec, row, to, from_, subject, msg, filename=None,
               charset='UTF-8', **kwargs):
    """Tiskni specifikaci pomocí příkazu config.printing_command nebo ulož do output_file.

    Argumenty:

      spec -- název specifikace pro PrintResolver
      print_spec -- název tiskové specifikace pro pytis.output.Formatter
      row -- řádek s daty pro PrintResolver
      to -- adresát
      from_ -- adresa odesílatele
      subject -- subject emailu
      msg -- tělo emailu
      filename -- název pro soubor s přílohou; pokud je None, bude vygenerován
      
      Klíčové argumenty jsou dále předány PrintResolver pro použití v tiskové proceduře.
    """
    import tempfile
    import os
    fd, fname = tempfile.mkstemp(suffix='.pdf')
    handle = os.fdopen(fd, 'wb')
    printdirect(resolver, spec, print_spec, row, output_file=handle, **kwargs)
    document = None
    with open(fname, 'rb') as f:
        document = f.read()
    if os.path.exists(fname):
        os.remove(fname)
    if document:
        if not filename:
            filename = os.path.basename(fname)

        mail = ComplexEmail(to, from_, subject, msg, charset=charset)
        mail.add_content_data(document, filename)
        result = mail.send()
        if not result:
            # Sending email failed -- return an error message
            return mail.get_error_msg()
        else:
            # Success - return no error message
            return None
    else:
        return "No print data available."
    
class ReusableSpec:
    def __init__(self, resolver):
        self._resolver = resolver
        self._bindings = self._bindings()
        self._fields = self._fields()

    def __getitem__(self, id):
        return find(id, self._fields, key=lambda f: f.id())

    def _bindings(self):
        pass

    def _fields(self):
        pass

    def fields(self, *args):
        """Vrať seznam specifikací sloupců vyjmenovaných sloupců.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'Field'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() in args, self._fields)

    def bindings(self, *args):
        """Vrať seznam specifikací sloupců vyjmenovaných sloupců.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'pytis.data.DBColumnBinding'.

        """
        if len(args) == 0:
            return self._bindings
        else:
            return filter(lambda b: b.id() in args, self._bindings)

    def fields_complement(self, *args):
        """Vrať seznam specifikací sloupců, které nejsou vyjmenovány.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'Field'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() not in args, self._fields)


def mime_type_constraint(*allowed_mime_types):
    """Return a validation function checking the binary vaslue's MIME type.

    The function is designed to be used in 'pytis.data.Binary' data type's
    constraints as follows:

       type = pytis.data.Binary(constraints=(mime_type_constraint('application/pdf'),))
    
    """
    import magic
    def constraint(value):
        m = magic.open(magic.MAGIC_MIME_TYPE)
        try:
            m.load()
            mime_type = m.buffer(str(value.buffer()))
        finally:
            m.close()
        if mime_type in allowed_mime_types:
            return None
        else:
            if len(allowed_mime_types) == 1:
                allowed_types_msg = _(u"Požadován je typ %s.") % allowed_mime_types
            else:
                allowed_types_msg = _(u"Povolené typy jsou %s") % ', '.join(allowed_mime_types)
            return _(u"Detekována data typu %s. %s") % (mime_type, allowed_types_msg)
    return constraint
