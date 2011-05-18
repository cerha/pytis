# -*- coding: utf-8 -*-
#
# Copyright (C) 2005, 2006, 2007, 2008, 2009, 2011 Brailcom, o.p.s.
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
import config

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
        pytis.form.BrowseForm:          "řádkový formulář",
        pytis.form.PopupEditForm:       "editační formulář",
        pytis.form.Form:                "duální řádkový formulář",
        pytis.form.CodebookForm:        "číselníkový formulář",
        pytis.form.DescriptiveDualForm: "duální náhledový formulář",
        }.get(form_class, "formulář")
    help = _('Otevřít %s "%s"') % (descr, title.replace('&', ''))
    return pytis.form.MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)

def new_record_mitem(title, name, hotkey=None, **kwargs):
    cmd = pytis.form.Application.COMMAND_NEW_RECORD
    args = dict(kwargs, name=name)
    help = _('Otevřít vstupní formulář "%s"') % title
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
           columns=None, select_row=0, multirow=False, transaction=None):
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
    result = pytis.form.run_dialog(pytis.form.RunFormDialog)
    if result is not None:
        pytis.form.run_form(*result)
cmd_run_any_form = \
           pytis.form.Application.COMMAND_HANDLED_ACTION(handler=run_any_form)


def printdirect(resolver, spec, print_spec, row, output_file=None, **kwargs):
    """Tiskni specifikaci pomocí příkazu config.printing_command nebo ulož do output_file.

    Argumenty:

      spec -- název specifikace pro PrintResolver
      print_spec -- název tiskové specifikace pro pytis.output.Formatter
      row -- řádek s daty pro PrintResolver

      Klíčové argumenty jsou dále předány PrintResolver pro použití v tiskové proceduře.
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
            def coding(self, resolver):
                if wx.Font_GetDefaultEncoding() == \
                   wx.FONTENCODING_ISO8859_2:
                    result = pytis.output.Coding.LATIN2
                else:
                    result = pytis.output.Coding.ASCII
                return result
        def _get_module(self, module_name):
            try:
                result = pytis.output.OutputResolver._get_module(self,
                                                               module_name)
            except ResolverModuleError:
                result = self._Spec()
            return result
        
    log(EVENT, 'Vyvolání tiskového formuláře')
    spec_path = os.path.join('output', print_spec)
    P = _PrintResolver    
    parameters = {(spec+'/'+pytis.output.P_ROW): row}
    parameters.update({P.P_NAME: spec})
    parameters.update(kwargs)
    print_resolver = P(resolver, parameters=parameters)
    resolvers = (print_resolver,)
    formatter = pytis.output.Formatter(resolvers, spec_path)
    if output_file:
        formatter.printout(output_file)
    else:
        formatter.printdirect()


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


