# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2005 Brailcom, o.p.s.
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


class DataSpec(pytis.data.DataFactory):
    """Třída zjednodušující tvorbu datové specifikace.

    Konstruktor této třídy přijímá argumenty ve zjednodušené formě a schovává
    tak některé nízkoúrovňové detaily před tvůrcem specifikace.  Zároveň je
    odstraněna duplicita některých informací, které se při přímém použití
    specifikačních tříd datového rozhraní není možné zcela vyhnout.

    Podrobný popis rozhraní viz. konstruktor třídy.

    Po vytvoření instance této třídy je možné získat odpovídající instanci
    'pytis.data.DataFactory' voláním metody 'make()'.
    
    """
    
    def __init__(self, table, columns, key, oid=None, access_rights=None,
                 ignore_enumerators=False,
                 data_class_=pytis.data.DBDataDefault):
        """Inicializuj specifikaci.

        Argumenty:

          table -- název datové tabulky jako řetězec.
          columns -- sekvence specifikací sloupců jako instancí 'Column'.
            Jedná se vždy o sloupce z tabulky 'table'.
          key -- název klíčového sloupce jako řetězec.  Sloupec s tímto
            identifikátorem musí být přítomný v 'columns'.
          oid -- seznam názvů OID sloupců (tuple).  Pokud je None (výchozí
            hodnota), bude doplněn jeden sloupec s názvem 'oid'.  Pro všechny
            uvedené sloupce budou automaticky přidány příslušné vazby.  Pokud
            tabulka nemá žádný mít žádný oid sloupec, uvedeme prázdný seznam.
            Pokud je sloupec jen jeden, není nutno jej obalovat do tuplu.
          access_rights -- práva jako instance 'pytis.data.AccessRights' nebo
            None, pokud mají být práva neomezená.
          ignore_enumerators -- pokud bude předána pravdivá hodnota, budou
            enumerátory všech sloupců ignorovány.
          data_class_ -- třída datového objektu, odvozená od `Data'.
            
        Pokud 'columns' neobsahují sloupec s identifikátorem 'oid', bude
        automaticky doplněn sloupec 'oid' typu 'pytis.data.Oid'.

        """
        assert isinstance(table, types.StringType)
        assert isinstance(columns, (types.ListType, types.TupleType))
        assert isinstance(key, types.StringType)
        assert isinstance(key, (types.StringType, types.ListType,
                                types.TupleType)) or oid is None
        assert isinstance(ignore_enumerators, types.BooleanType)
        assert isinstance(access_rights, pytis.data.AccessRights) \
               or access_rights is None
        assert find(key, columns, key=lambda c: c.id()) is not None
        for c in columns:
            assert isinstance(c, Column)
        if oid is None:
            if find('oid', columns, key=lambda c: c.id()):
                oid = ()
            else:    
                oid = ('oid',)
        else:
            oid = xtuple(oid)
            for c in oid:
                assert isinstance(c, types.StringType)
        if access_rights is None:
            perm = pytis.data.Permission.ALL
            access_rights = pytis.data.AccessRights((None, (None, perm)))
        bindings = []
        columns += tuple([Column(c, type=pytis.data.Oid()) for c in oid])
        for c in columns:
            type = c.type()
            kwargs = c.kwargs()
            e = c.enumerator()
            if ignore_enumerators:
                e = None
                kwargs = {}
            enumerator = e and pytis.form.resolver().get(e, 'data_spec') or None
            bindings.append(pytis.data.DBColumnBinding(c.id(), table,
                                                       c.column(),
                                                       enumerator=enumerator,
                                                       type_=type, **kwargs))
        key = find(key, bindings, key=lambda b: b.column())
        super(DataSpec, self).__init__(data_class_, bindings, key,
                                       access_rights=access_rights)
        
    def make(self):
        """Vtať instanci 'pytis.data.DataFactory' odpovídající specifikaci."""
        # TODO: Časem smazat.  Nyní stačí předávat přímo instanci.
        return self

    
class Column(object):
    def __init__(self, id, column=None, enumerator=None, type=None, **kwargs):
        """Inicializuj specifikaci enumerátoru.

        Argumenty:
        
          id -- identifikátor sloupce (řetězec).  Pod tímto identifikátorem
            bude sloubec vystupovat v aplikaci.
          column -- název databázového sloupce (řetězec nebo None).  Implicitně
            je doplněna hodnota 'id', takže pokud se název sloupce
            shoduje s identifikátorem, není jej třeba definovat.
          enumerator -- název specifikace pro resolver (řetězec nebo None).  Z
            této specifikace bude získán datový objekt a použit jako číselník.
            V takovém případě bude typ tohoto sloupečku automaticky 
            'pytis.data.Codebook', pokud není určen explicitně (viz. níže).
          type -- explicitní určení datového typu sloupce (instance
            'pytis.data.Type', nebo None).  Tento argument by měl být použit
            pouze pokud chceme určit vlastní (odvozený) datový typ, nikoliv
            pokud chceme měnit parametry standardních typů.  Ty je možno
            nastavit předáním klíčovách argumentů (viz níže).
          **kwargs -- pokud jsou uvedeny jakékoliv další klíčové argumenty,
            budou tyto předány konstruktoru datového typu sloupce.  Tento
            postup by měl být preferován před explicitní definicí instance typu
            argumentem 'type', pokud je to možné.

        """
        assert isinstance(id, types.StringType), \
               "Invalid value for argument 'id': %s" % id
        assert isinstance(column, types.StringType) or column is None, \
               "Invalid value for argument 'column': %s" % column
        assert isinstance(enumerator, types.StringType) or enumerator is None, \
               "Invalid value for argument 'enumerator': %s" % enumerator
        assert isinstance(type, pytis.data.Type) or type is None, \
               "Invalid value for argument 'type': %s" % type
        assert enumerator is None or type is None \
               or isinstance(type, pytis.data.Codebook), \
               "Invalid codebook type: %s" % type
        assert type is None or kwargs == {}, \
               "When the 'type' is defined explicitly, " + \
               "using kwargs makes no sense: %s" % kwargs
        self._id = id
        if column is None:
            column = id
        self._column = column
        self._enumerator = enumerator
        self._type = type
        self._kwargs = kwargs
    
    def id(self):
        return self._id
    
    def column(self):
        return self._column

    def enumerator(self):
        return self._enumerator

    def type(self):
        return self._type
    
    def kwargs(self):
        return self._kwargs

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

# Odvozené specializované třídy

class HGroup(GroupSpec):
    """Horizontální seskupení políček."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        super(HGroup, self).__init__(items, **kwargs)

class VGroup(GroupSpec):
    """Vertikální seskupení políček."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.VERTICAL
        super(VGroup, self).__init__(items, **kwargs)
        
class LHGroup(HGroup):
    """Horizontální seskupení políček s labelem a orámováním."""
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LHGroup, self).__init__(*items, **kwargs)

class LVGroup(VGroup):
    """Vertikální seskupení políček s labelem a orámováním."""
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LVGroup, self).__init__(*items, **kwargs)

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

_user_cmd_caller = {}
def user_cmd(name, handler, **kwargs):
    name = name.upper().replace('-', '_')
    if hasattr(BrowseForm, 'COMMAND_'+name):
        cmd = getattr(BrowseForm, 'COMMAND_'+name)
        caller = _user_cmd_caller[name]
        if caller != stack_info(depth=2).splitlines()[0]:
            raise ProgramError("Command '%s' already defined:" % name, caller)
        return cmd
    _user_cmd_caller[name] = stack_info(depth=2).splitlines()[0]
    return Command(BrowseForm, name, handler=handler, **kwargs)


nr = new_record_mitem
rp = run_procedure_mitem
def bf(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.BrowseForm, hotkey)
def df(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.BrowseDualForm, hotkey)
def ddf(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.DescriptiveDualForm,
                          hotkey)
def ef(title, name, hotkey=None):
    return run_form_mitem(title, name, pytis.form.PopupEditForm, hotkey)

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


