# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2005-2016 OUI Technology Ltd.
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
from past.builtins import basestring

import io
import os
import time
import tempfile

import pytis.data as pd
import pytis.util

from pytis.util import log, find, EVENT, ResolverError
from pytis.presentation import (
    Color, Editable, Field, FormType, PostProcess, Style, TextFilter,
    Computer, SelectionType, PresentedRow, specification_path,
)

from .email_ import ComplexEmail
from .defs import get_form_defs

_ = pytis.util.translations('pytis-wx')


# Zkratky na často používané identifikátory.

ASC = pd.ASCENDENT
DESC = pd.DESCENDANT

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
    import pytis.form
    cmd = pytis.form.Application.COMMAND_RUN_FORM
    args = dict(form_class=form_class, name=name, **kwargs)
    help = _('Open %s "%s"', form_class.DESCR or _("form"), title.replace('&', ''))
    return pytis.form.MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)


def new_record_mitem(title, name, hotkey=None, **kwargs):
    import pytis.form
    cmd = pytis.form.Application.COMMAND_NEW_RECORD
    args = dict(kwargs, name=name)
    help = _('Open insertion form "%s"', title)
    return pytis.form.MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)


def run_procedure_mitem(title, name, proc_name, hotkey=None, groups=None, enabled=None, **kwargs):
    import pytis.form
    cmd = pytis.form.Application.COMMAND_RUN_PROCEDURE
    if groups is not None:
        assert isinstance(groups, (tuple, list))
        assert enabled is None or callable(enabled)
        enabled_ = enabled

        def enabled(**kwargs_):
            if not pd.is_in_groups(groups):
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
    import pytis.form
    return run_form_mitem(title, name, pytis.form.BrowseForm, hotkey, **kwargs)


def df(title, name, hotkey=None, **kwargs):
    import pytis.form
    return run_form_mitem(title, name, pytis.form.BrowseDualForm, hotkey, **kwargs)


def mf(title, name, hotkey=None, **kwargs):
    import pytis.form
    return run_form_mitem(title, name, pytis.form.MultiBrowseDualForm, hotkey, **kwargs)


def sf(title, name, hotkey=None, **kwargs):
    import pytis.form
    return run_form_mitem(title, name, pytis.form.ShowForm, hotkey, **kwargs)


def ddf(title, name, hotkey=None):
    import pytis.form
    return run_form_mitem(title, name, pytis.form.DescriptiveDualForm, hotkey)


def ef(title, name, hotkey=None, **kwargs):
    import pytis.form
    return run_form_mitem(title, name, pytis.form.PopupEditForm, hotkey, **kwargs)


# Další funkce pro zjednodušení často používaných konstrukcí

def get_value(value, default=None):
    """Return the Python value of given 'pd.Value' instance.

    Accepts a 'pd.Value' instance or None.  If the 'value' is None, the 'default' value
    will be returned without complaining.  If the 'value' is a 'pd.Value' instance, its
    internal (Python) value will be returned.

    """
    if value is None:
        return default
    else:
        return value.value()


def format_value(value, default=None):
    """Return the formatted (string) value of given 'pd.Value' instance.

    Accepts a 'pd.Value' instance or None.  If the 'value' is None, the 'default' value
    will be returned without complaining.  If the 'value' is a 'pd.Value' instance, its
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
    import pytis.form
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
        'pd.DataEnumerator'.
      column -- název sloupce číselníku poskytujícího výslednou hodnotu.
      transaction -- transakce pro předání datovým operacím.

    Pokud odpovídající řádek není nalezen, bude vrácena instance 'Value'
    stejného typu, jako je typ argumentu 'value' s hodnotou nastavenou na
    'None'.  Takováto hodnota nemusí být validní hodnotou typu, ale
    zjednodušuje se tím práce s výsledkem.  Pokud je zapotřebí korektnějšího
    chování, je doporučeno použít přímo metodu 'DataEnumerator.row()'
    (například voláním 'value.type().enumerator().row(value.value())'.

    """
    assert isinstance(value, pd.Value)
    assert value.type().enumerator() is not None
    if column is None:
        return value
    else:
        row = value.type().enumerator().row(value.value(), transaction=transaction)
        if row is not None:
            return row[column]
        else:
            return pd.Value(value.type(), None)


def run_cb(spec, begin_search=None, condition=None, sort=(),
           columns=None, select_row=0, multirow=False,
           filter=None, transaction=None):
    """Vyvolá číselník určený specifikací.

    Argumenty:

      spec -- název specifikace číselníku.
      begin_search -- None nebo jméno sloupce, nad kterým se má vyvolat
        inkrementální vyhledávání.
      condition -- podmínka pro filtrování záznamů.
      sort -- řazení (viz pd.select())
      columns -- seznam sloupců, pokud se má lišit od seznamu uvedeného
        ve specifikaci.
      select_row -- řádek, na který se má nastavit kurzor.
      multirow -- umožní výběr více řádků.
      transaction -- transakce pro předání CodebookFormu.

    Vrací None (pokud není vybrán žádný řádek) nebo vybraný řádek nebo
    tuple vybraných řádků (pokud je argument multirow nastaven).

    """
    import pytis.form
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


def make_presented_row(specname, prefill={}):
    import pytis.form
    data = pytis.util.data_object(specname)
    resolver = pytis.config.resolver
    spec = resolver.get(specname, 'view_spec')
    prow = PresentedRow(spec.fields(), data, None, prefill=prefill, new=True)
    return prow


def run_any_form():
    import pytis.form
    form_types = (
        ("BrowseForm", pytis.form.BrowseForm),
        ("PopupEditForm", pytis.form.PopupEditForm),
        ("PopupInsertForm", pytis.form.PopupInsertForm),
        ("BrowseDualForm", pytis.form.BrowseDualForm),
        ("MultiBrowseDualForm", pytis.form.MultiBrowseDualForm),
        ("CodebookForm", pytis.form.CodebookForm),
    )
    all_defs = []
    for d in get_form_defs():
        all_defs.append(specification_path(d)[1])
    all_defs.sort()

    def name_runtime_filter(row):
        name_substr = row['name_substr'].value()
        if name_substr:
            return lambda val: val.lower().find(name_substr.lower()) != -1
        else:
            return None
    row = pytis.form.run_form(
        pytis.form.InputForm,
        title=_("Run Form"),
        fields=(Field('type', _("Form type"), not_null=True,
                      enumerator=pd.FixedEnumerator([x[0] for x in form_types]),
                      default='BrowseForm'),
                Field('name_substr', _("Search string"), width=40,),
                Field('name', _("Specification name"), width=40, height=10,
                      enumerator=pd.FixedEnumerator(all_defs), not_null=True,
                      selection_type=SelectionType.LISTBOX,
                      runtime_filter=Computer(name_runtime_filter, depends=('name_substr',))
                      ),
                ))
    if row is not None:
        form_type = dict(form_types)[row['type'].value()]
        pytis.form.run_form(form_type, row['name'].value())


def cmd_run_any_form():
    import pytis.form
    return pytis.form.Application.COMMAND_HANDLED_ACTION(handler=run_any_form)


def printdirect(resolver, spec_name, template_id, row, output_file=None,
                language=None, translations=(), spec_kwargs=None, **kwargs):
    """Deprecated.  Use 'pytis.form.printout()' instead."""
    pytis.form.printout(spec_name, template_id, parameters=kwargs,
                        output_file=output_file, row=row, language=language,
                        spec_kwargs=spec_kwargs)


def print2mail(resolver, spec_name, template_id, row, to, from_, subject, msg, filename=None,
               charset='UTF-8', **kwargs):
    """Tiskni specifikaci a odešli jako přílohu e-mailu.

    Argumenty:

      spec_name -- název specifikace pro PrintResolver
      template_id -- název tiskové specifikace pro pytis.output.Formatter
      row -- řádek s daty pro PrintResolver
      to -- adresát
      from_ -- adresa odesílatele
      subject -- subject emailu
      msg -- tělo emailu
      filename -- název pro soubor s přílohou;

    Klíčové argumenty jsou dále předány PrintResolver pro použití v tiskové proceduře.

    """
    assert filename, filename
    output = io.BytesIO()
    pytis.form.printout(spec_name, template_id, output_file=output, parameters=kwargs, row=row)
    document = output.getvalue()
    if document:
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


class ReusableSpec(object):

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
            return [f for f in self._fields if f.id() in args]

    def bindings(self, *args):
        """Vrať seznam specifikací sloupců vyjmenovaných sloupců.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'pd.DBColumnBinding'.

        """
        if len(args) == 0:
            return self._bindings
        else:
            return [b for b in self._bindings if b.id() in args]

    def fields_complement(self, *args):
        """Vrať seznam specifikací sloupců, které nejsou vyjmenovány.

        Pokud nejsou vyjmenovány žádné identifikátory sloupců, vrátí seznam
        všech sloupců.  Vrací sekvenci instancí 'Field'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return [f for f in self._fields if f.id() not in args]


def mime_type_constraint(*allowed_mime_types):
    """Return a validation function checking the binary vaslue's MIME type.

    The function is designed to be used in 'pd.Binary' data type's
    constraints as follows:

       type = pd.Binary(constraints=(mime_type_constraint('application/pdf'),))

    """
    import magic

    def constraint(value):
        if hasattr(magic, 'detect_from_content'):
            # Hack for temporary compatibility with both python 'magic' modules...
            mime_type = magic.detect_from_content(str(value)).mime_type
        else:
            mime_type = magic.from_buffer(str(value), mime=True)

        if mime_type in allowed_mime_types:
            return None
        else:
            return _("Detected data type %(detected)s. Expected %(expected)s.",
                     detected=mime_type, expected=', '.join(allowed_mime_types))
    return constraint
