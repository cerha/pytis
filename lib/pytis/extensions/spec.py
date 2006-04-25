# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2005, 2006 Brailcom, o.p.s.
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

"""Třídy pro zjednodušení a zpřehlednění tvorby specifikačních souborů.""" 

from pytis.extensions import *
from pytis.presentation import *


# Zkratky na často používané identifikátory.
    
Field = FieldSpec

ASC = LookupForm.SORTING_ASCENDENT
DESC = LookupForm.SORTING_DESCENDANT

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

def run_procedure_mitem(title, name, proc_name, hotkey=None):
    return MItem(title, command=pytis.form.Application.COMMAND_RUN_PROCEDURE,
                 args=dict(spec_name=name, proc_name=proc_name),
                 hotkey=hotkey, help='Spustit proceduru "%s"' % title)

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
    help = _('Otevřít %s "%s"') % (descr, title)
    return MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)

def new_record_mitem(title, name, hotkey=None):
    cmd = pytis.form.Application.COMMAND_NEW_RECORD
    args = dict(name=name)
    help = _('Otevřít vstupní formulář "%s"') % title
    return MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)

def help_mitem(title, inputfile, hotkey=None, format=TextFormat.WIKI):
    return MItem(title, hotkey=hotkey,
                 command=cmd_help_window,
                 args={'inputfile': inputfile, 'format': format})

def context_mitem(title, handler, **kwargs):
    """Funkce pro zpětnou kompatibilitu.

    Tato funkce nechť již není používána!  Namísto ní nechť jsou uživatelské
    akce specifikovány pomocí argumentu `actions' třídy `ViewSpec'.

    """
    action = Action(title, handler, **kwargs)
    return MItem(title, command=ListForm.COMMAND_CONTEXT_ACTION(action=action),
                 hotkey=action.hotkey())


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
            assert isinstance(arg, types.StringType)
    return lambda row: run_procedure(spec_name, proc_name,
                                     *[row[key] for key in args], **kwargs)

context_rp = rp_handler # Pro zpětnou kompatibilitu.  Časem smazat!  2006-03-11

_user_cmd_caller = {}
def user_cmd(name, handler, spec=None, block_refresh_=False, **kwargs):
    if spec:
        # TODO: toto zlobí
        # name = name + "_" + spec.upper().replace(':', '_')
        name = name + "_" + spec.upper()
    name = name.upper().replace('-', '_')
    if hasattr(BrowseForm, 'COMMAND_'+name):
        cmd = getattr(BrowseForm, 'COMMAND_'+name)
        caller = _user_cmd_caller[name]
        if caller != stack_info(depth=2).splitlines()[0]:
            raise ProgramError("Command '%s' already defined:" % name, caller)
        return cmd
    if block_refresh_:
        h = lambda *a, **aa: block_refresh(handler, *a, **aa)
    else:
        h = handler
    _user_cmd_caller[name] = stack_info(depth=2).splitlines()[0]
    return Command(BrowseForm, name, handler=h, **kwargs)


nr = new_record_mitem
rp = run_procedure_mitem
def bf(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.BrowseForm, hotkey)
def df(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.BrowseDualForm, hotkey)
def ddf(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.DescriptiveDualForm, hotkey)
def ef(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.PopupEditForm, hotkey)

def enum(name):
    """Vytvoř instanci 'DataEnumerator' nad danou specifikací.

    Takto vytvořený enumerátor lze použít jako argument 'enumerator'
    konstruktoru datového typu.  Argument 'name' je řetězec určující název
    specifikace, ze které bude získán datový objekt enumerátoru.
    
    """
    data_spec = pytis.form.resolver().get(name, 'data_spec')
    kwargs = dict(dbconnection_spec=config.dbconnection)
    return pytis.data.DataEnumerator(data_spec, data_factory_kwargs=kwargs)





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
        všech sloupců.  Vrací sekvenci instancí 'FieldSpec'.

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
        všech sloupců.  Vrací sekvenci instancí 'FieldSpec'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() not in args, self._fields)


