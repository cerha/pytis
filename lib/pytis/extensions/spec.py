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

"""T��dy pro zjednodu�en� a zp�ehledn�n� tvorby specifika�n�ch soubor�.""" 

from pytis.extensions import *
from pytis.presentation import *


class DataSpec(pytis.data.DataFactory):
    """T��da zjednodu�uj�c� tvorbu datov� specifikace.

    Konstruktor t�to t��dy p�ij�m� argumenty ve zjednodu�en� form� a schov�v�
    tak n�kter� n�zko�rov�ov� detaily p�ed tv�rcem specifikace.  Z�rove� je
    odstran�na duplicita n�kter�ch informac�, kter� se p�i p��m�m pou�it�
    specifika�n�ch t��d datov�ho rozhran� nen� mo�n� zcela vyhnout.

    Podrobn� popis rozhran� viz. konstruktor t��dy.

    Po vytvo�en� instance t�to t��dy je mo�n� z�skat odpov�daj�c� instanci
    'pytis.data.DataFactory' vol�n�m metody 'make()'.
    
    """
    
    def __init__(self, table, columns, key, oid=None, access_rights=None,
                 ignore_enumerators=False,
                 data_class_=pytis.data.DBDataDefault):
        """Inicializuj specifikaci.

        Argumenty:

          table -- n�zev datov� tabulky jako �et�zec.
          columns -- sekvence specifikac� sloupc� jako instanc� 'Column'.
            Jedn� se v�dy o sloupce z tabulky 'table'.
          key -- n�zev kl��ov�ho sloupce jako �et�zec.  Sloupec s t�mto
            identifik�torem mus� b�t p��tomn� v 'columns'.
          oid -- seznam n�zv� OID sloupc� (tuple).  Pokud je None (v�choz�
            hodnota), bude dopln�n jeden sloupec s n�zvem 'oid'.  Pro v�echny
            uveden� sloupce budou automaticky p�id�ny p��slu�n� vazby.  Pokud
            tabulka nem� ��dn� m�t ��dn� oid sloupec, uvedeme pr�zdn� seznam.
            Pokud je sloupec jen jeden, nen� nutno jej obalovat do tuplu.
          access_rights -- pr�va jako instance 'pytis.data.AccessRights' nebo
            None, pokud maj� b�t pr�va neomezen�.
          ignore_enumerators -- pokud bude p�ed�na pravdiv� hodnota, budou
            enumer�tory v�ech sloupc� ignorov�ny.
          data_class_ -- t��da datov�ho objektu, odvozen� od `Data'.
            
        Pokud 'columns' neobsahuj� sloupec s identifik�torem 'oid', bude
        automaticky dopln�n sloupec 'oid' typu 'pytis.data.Oid'.

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
        """Vta� instanci 'pytis.data.DataFactory' odpov�daj�c� specifikaci."""
        # TODO: �asem smazat.  Nyn� sta�� p�ed�vat p��mo instanci.
        return self

    
class Column(object):
    def __init__(self, id, column=None, enumerator=None, type=None, **kwargs):
        """Inicializuj specifikaci enumer�toru.

        Argumenty:
        
          id -- identifik�tor sloupce (�et�zec).  Pod t�mto identifik�torem
            bude sloubec vystupovat v aplikaci.
          column -- n�zev datab�zov�ho sloupce (�et�zec nebo None).  Implicitn�
            je dopln�na hodnota 'id', tak�e pokud se n�zev sloupce
            shoduje s identifik�torem, nen� jej t�eba definovat.
          enumerator -- n�zev specifikace pro resolver (�et�zec nebo None).  Z
            t�to specifikace bude z�sk�n datov� objekt a pou�it jako ��seln�k.
            V takov�m p��pad� bude typ tohoto sloupe�ku automaticky 
            'pytis.data.Codebook', pokud nen� ur�en explicitn� (viz. n�e).
          type -- explicitn� ur�en� datov�ho typu sloupce (instance
            'pytis.data.Type', nebo None).  Tento argument by m�l b�t pou�it
            pouze pokud chceme ur�it vlastn� (odvozen�) datov� typ, nikoliv
            pokud chceme m�nit parametry standardn�ch typ�.  Ty je mo�no
            nastavit p�ed�n�m kl��ov�ch argument� (viz n�e).
          **kwargs -- pokud jsou uvedeny jak�koliv dal�� kl��ov� argumenty,
            budou tyto p�ed�ny konstruktoru datov�ho typu sloupce.  Tento
            postup by m�l b�t preferov�n p�ed explicitn� definic� instance typu
            argumentem 'type', pokud je to mo�n�.

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

# Zkratky na �asto pou��van� identifik�tory.
    
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

# Odvozen� specializovan� t��dy

class HGroup(GroupSpec):
    """Horizont�ln� seskupen� pol��ek."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.HORIZONTAL
        super(HGroup, self).__init__(items, **kwargs)

class VGroup(GroupSpec):
    """Vertik�ln� seskupen� pol��ek."""
    def __init__(self, *items, **kwargs):
        kwargs['orientation'] = Orientation.VERTICAL
        super(VGroup, self).__init__(items, **kwargs)
        
class LHGroup(HGroup):
    """Horizont�ln� seskupen� pol��ek s labelem a or�mov�n�m."""
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LHGroup, self).__init__(*items, **kwargs)

class LVGroup(VGroup):
    """Vertik�ln� seskupen� pol��ek s labelem a or�mov�n�m."""
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
        pytis.form.BrowseForm:          "��dkov� formul��",
        pytis.form.PopupEditForm:       "edita�n� formul��",
        pytis.form.Form:                "du�ln� ��dkov� formul��",
        pytis.form.CodebookForm:        "��seln�kov� formul��",
        pytis.form.DescriptiveDualForm: "du�ln� n�hledov� formul��",
        }.get(form_class, "formul��")
    help = _('Otev��t %s "%s"') % (descr, title)
    return MItem(title, command=cmd, args=args, hotkey=hotkey, help=help)

def new_record_mitem(title, name, hotkey=None):
    cmd = pytis.form.Application.COMMAND_NEW_RECORD
    args = dict(name=name)
    help = _('Otev��t vstupn� formul�� "%s"') % title
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
        """Vra� seznam specifikac� sloupc� vyjmenovan�ch sloupc�.

        Pokud nejsou vyjmenov�ny ��dn� identifik�tory sloupc�, vr�t� seznam
        v�ech sloupc�.  Vrac� sekvenci instanc� 'FieldSpec'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() in args, self._fields)

    def bindings(self, *args):
        """Vra� seznam specifikac� sloupc� vyjmenovan�ch sloupc�.

        Pokud nejsou vyjmenov�ny ��dn� identifik�tory sloupc�, vr�t� seznam
        v�ech sloupc�.  Vrac� sekvenci instanc� 'pytis.data.DBColumnBinding'.

        """
        if len(args) == 0:
            return self._bindings
        else:
            return filter(lambda b: b.id() in args, self._bindings)


    def fields_complement(self, *args):
        """Vra� seznam specifikac� sloupc�, kter� nejsou vyjmenov�ny.

        Pokud nejsou vyjmenov�ny ��dn� identifik�tory sloupc�, vr�t� seznam
        v�ech sloupc�.  Vrac� sekvenci instanc� 'FieldSpec'.

        """
        if len(args) == 0:
            return self._fields
        else:
            return filter(lambda f: f.id() not in args, self._fields)


