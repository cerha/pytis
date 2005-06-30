# -*- coding: iso-8859-2 -*-

# Definice u�ivatelsk�ch p��kaz�
# 
# Copyright (C) 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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

"""Definice u�ivatelsk�ch p��kaz�.

Tento soubor obsahuje jednak t��du 'Command', slou��c� k�definici v�ech
podporovan�ch p��kaz�, a�jednak definice p��kaz�.

V�echny p��kazy jsou centr�ln� definov�ny zde, z�d�vodu konzistence a
p�ehlednosti, zejm�na z�pohledu tv�rc� definic aplikac�.  V�echny definovan�
p��kazy jsou konstantami n�kter� z�t��d modulu 'pytis.form'; n�zvy t�chto
konstant za��naj� prefixem 'COMMAND_' (tuto konvenci je nutno dodr�ovat).

P�i�azen� kl�ves p��kaz�m:

V�aplikaci mimo modul 'defs' nepou��v�me p��m� z�pis kl�ves, n�br� pouze jim
odpov�daj�c� p��kazy.  To n�m umo��uje nestarat se v�aplika�n�m k�du
o�p�i�azen� kl�ves, ta definujeme na jedin�m m�st� v�tomto modulu.

"""


from pytis.form import *


class Command(object):
    """Reprezentace p��kazu u�ivatelsk�ho rozhran�.

    Kl�vesa, kter� p��kaz vyvol�v�, je dostupn� ve form� ve�ejn�ho atributu
    `key' a jej� hodnotu lze v�defini�n�ch souborech konkr�tn� aplikace
    prost�ednictv�m tohoto atributu zm�nit.

    P�i definici u�ivatelsk�ch p��kaz� lze definovat vlastn� obslu�n� rutiny
    pro jejich zpracov�n�.  U�ivatelsk�m p��kazem je my�len p��kaz, kter� nen�
    standardn� o�et�ov�n ��dnou t��dou u�ivatelsk�ho rozhran� (formul��em).
    Pokud v�ak formul�� podporuje u�ivatelsk� p��kazy, vyvol� funkci
    specifikovanou argumentem konstruktoru 'handler' s parametry, kter� z�vis�
    na typu dan� t��dy u�ivatelsk�ho rozhran�. T��da BrowseForm tak nap��klad
    jako argument p�ed� data aktu�ln�ho ��dku seznamu apod. V�ce v dokumentaci
    jednotliv�ch formul��ov�ch t��d.
    
    Identifik�tor p��kazu je v�tuto chv�li v�znamn� pouze pro logov�n�.
    P��kazy jsou rozpozn�v�ny dle konkr�tn�ch instanc�, ne podle sv�ho
    identifik�toru.

    """
    def __init__(self, id, key=None, handler=None, log_=True,
                 enabled=True, access_groups=None, static=False):
        """Definuj p��kaz.

        Argumenty:

          id -- identifik�tor p��kazu, libovoln� nepr�zdn� �et�zec mezi
            identifik�tory p��kazu unik�tn�
          handler -- funkce volan� p�i zpracov�n� p��kazu.  M� v�znam p�i
            definici u�ivatelsk�ch p��kaz�.  Bl�e viz dokumentace t��dy.
            Hodnotou je callable object, nebo None.
          log_ -- pr�v� kdy� je pravdiv�, je vyvol�n� p��kazu logov�no jako
            EVENT, jinak je logov�no pouze jako DEBUG
          access_groups -- sekvence jmen skupin (strings), kter� maj� pr�vo
            p��kaz vyvolat; m��e b�t t� 'None', v�kter�m�to p��pad� p��kaz
            mohou vyvolat v�echny skupiny.  Toto opr�vn�n� je form�ln�,
            zohledn�n� jen v�u�ivatelsk�m rozhran�, nem� faktickou bezpe�nostn�
            roli.  P��tomnost u�ivatele ve skupin�ch je zji��ov�na pouze jednou
            p�i inicializaci instance p��kazu, co� je v�t�inou p�i startu
            aplikace.
          enabled -- bu�to p��mo boolean hodnota ur�uj�c�, zda je p��kaz
            aktivn�, nebo odkaz na funkci, kter� toto zjist� (a vr�t�
            odpov�daj�c� boolean hodnotu).  Jde o funkci t�� argument� (APPL,
            COMMAND, ARGS), kde APPL je instance aplikace (t��dy
            'Application'), COMMAND je instance p��kazu a ARGS je slovn�k
            arguemnt� s jak�mi bude p��kaz vol�n.
          static -- pokud je p�ed�na pravdiv� hodnota, bude hodnota vr�cen�
            funkc� `enabled' pova�ov�na za nem�nnou a v�sledek tedy bude
            cachov�n.  V opa�n�m p��pad� (v�choz� hodnota) bude funkce vol�na
            p�i ka�d�m po�adavku na zji�t�n� hodnoty enabled.
    
        """
        assert is_anystring(id)
        assert handler is None or callable(handler)
        assert key is None or is_string(key) or is_sequence(key)
        assert callable(enabled) or isinstance(enabled, types.BooleanType)
        assert isinstance(static, types.BooleanType)
        assert access_groups is None or \
               isinstance(access_groups,
                          (types.StringType, types.TupleType, types.ListType))
        self._id = id
        if key is not None:
            log(OPERATIONAL,
                "Pou�it potla�en� argument `key' t��dy `Command':", (key, id))
        self._handler = handler
        self._log = log_
        self._has_access = True
        if access_groups is not None:
            access_groups = xtuple(access_groups)
            dbconnection = config.dbconnection
            groups = pytis.data.DBDataDefault.class_access_groups(dbconnection)
            if groups is not None:
                self._has_access = some(lambda g: g in groups, access_groups)
        self._enabled = enabled
        self._static = static
        self._cache = {}
    
    def id(self):
        """Vra� identifik�tor zadan� v�konstruktoru."""
        return self._id

    def handler(self):
        """Vra� rutinu pro zpracov�n� p��kazu."""
        return self._handler

    def enabled(self, application, args):
        if not self._has_access:
            return False
        enabled = self._enabled
        if not isinstance(enabled, types.BooleanType):
            if self._static:
                cache_key = tuple(args.items())
                try:
                    enabled = self._cache[cache_key]
                except KeyError:
                    enabled = bool(enabled(application, self, args))
                    self._cache[cache_key] = enabled
            else:
                enabled = bool(enabled(application, self, args))
        return enabled
    
    def log_kind(self):
        """Vra� druh logovac� hl�ky, pod kter�m m� b�t p��kaz logov�n."""
        if self._log:
            kind = EVENT
        else:
            kind = DEBUG
        return kind

    def __cmp__(self, other):
        if sameclass(self, other):
            if self._id == other._id:
                return 0
            elif self._id < other._id:
                return -1
            else:
                return 1
        else:
            return compare_objects(self, other)
        
    def __str__(self):
        return '<Command: %s>' % (self._id,)

    def __setattr__(self, name, value):
        # TODO: �asem zru�it.
        if name == 'key':
            log(OPERATIONAL,
                "Nastaven potla�en� atribut `key' t��dy `Command':",
                (value, self._id))
        self.__dict__[name] = value
            

def invoke_command(command, **kwargs):
    """Vyvolej glob�ln� zpracov�n� p��kazu 'command'.

    Argumenty:

      command -- instance t��dy 'Command'
      kwargs -- parametry p��kazu

    """
    appl = pytis.form.application._application
    if command.enabled(appl, kwargs):
        return appl.on_command(command, **kwargs)
    else:
        return False

    
def define_cmd(cls, name, doc):
    id = cls.__name__ +'.'+ name.lower().replace('_', '-')
    name = 'COMMAND_' + name
    if issubclass(cls, Form):
        get_handler = lambda appl: appl.current_form()
    elif issubclass(cls, (InputField, Invocable)):
        get_handler = lambda appl: InputField.focused()
    elif issubclass(cls, Application):
        get_handler = lambda appl: appl
    elif issubclass(cls, Dialog):
        get_handler = lambda appl: appl.top_window()
    else:
        raise ProgramError("Unknown command handler class:", cls)
    def _enabled(appl, cmd, args):
        # TODO: Asi by bylo vhodn�j�� toto p�esunout n�kam do k�du vlastn�ho
        # handleru.
        handler = get_handler(appl)
        if handler is not None and hasattr(handler, name) \
               and getattr(handler, name) == cmd:
            try:
                f = getattr(handler, 'can_' + name[8:].lower())
            except AttributeError:
                return True
            return f(**args)
        else:
            return False
    setattr(cls, name, Command(id, enabled=_enabled))
    
define_cmd(Application, 'EXIT',
           "Ukon�en� aplikace.")
define_cmd(Application, 'BREAK',
           "P�eru�en� aktu�ln� prov�d�n� operace.")
define_cmd(Application, 'REFRESH',
           "Vy��d�n� obnoven� obsahu aktivn�ho formul��e.")
define_cmd(Application, 'NEW_RECORD',
           "Vlo�en� nov�ho z�znamu.")
define_cmd(Application, 'RUN_FORM',
           "Spu�t�n� formul��e.")
define_cmd(Application, 'RUN_PROCEDURE',
           "Spu�t�n� procedury.")
define_cmd(Application, 'LEAVE_FORM',
           "Odstran�n� aktivn�ho okna formul��e z�aplikace.")
define_cmd(Application, 'RAISE_FORM',
           "Vyzvednut� okna formul��e v okn� aplikace.")
define_cmd(Application, 'PREV_FORM',
           "Vyzvednut� okna p�edchoz�ho formul��e.")
define_cmd(Application, 'NEXT_FORM',
           "Vyzvednut� okna formul��e n�sleduj�c�ho za aktivn�m oknem.")
define_cmd(Application, 'SHOW_POPUP_MENU',
           "Zobraz kontextov� menu aktivn�ho prvku, pokud to lze.")
define_cmd(Form, 'PRINT',
           "Tisk aktu�ln�ho obsahu formul��e.")
define_cmd(LookupForm, 'FILTER',
           "Filtrov�n� z�znam�.")
define_cmd(LookupForm, 'JUMP',
           "Skok na z�znam.")
define_cmd(LookupForm, 'SEARCH',
           "Hled�n� z�znamu.")
define_cmd(LookupForm, 'SEARCH_PREVIOUS',
           "Hled�n� P�edchoz�ho z�znamu bez dialogu.")
define_cmd(LookupForm, 'SEARCH_NEXT',
           "Hled�n� dal��ho z�znamu bez dialogu.")
define_cmd(LookupForm, 'SORT_COLUMN',
           "Set��d�n� podle sloupce.")
define_cmd(RecordForm, 'DELETE_RECORD',
           #enabled=_current_form_can_delete)
           "Vymaz�n� editovan�ho z�znamu z�datab�ze.")
define_cmd(ListForm, 'ACTIVATE',
           "Vyvol�n� aktiva�n� funkce pro aktu�ln� ��dek formul��e.")
define_cmd(ListForm, 'ACTIVATE_ALTERNATE',
           "Vyvol�n� alternativn� aktiva�n� funkce pro aktu�ln� ��dek.")
define_cmd(ListForm, 'SHOW_CELL_CODEBOOK',
           "Vyvol�n� ��seln�ku aktivn� bu�ky ��dkov�ho formul��e.")
define_cmd(ListForm, 'SELECT_CELL',
           "V�b�r bu�ky seznamu.")
define_cmd(ListForm, 'FIRST_COLUMN',
           "P�echod na prvn� sloupec tabulky.")
define_cmd(ListForm, 'LAST_COLUMN',
           "P�echod na posledn� sloupec tabulky.")
define_cmd(ListForm, 'INCREMENTAL_SEARCH',
           "Prefixov� inkrement�ln� hled�n� z�znamu.")
define_cmd(ListForm, 'FULL_INCREMENTAL_SEARCH',
           "Pln� inkrement�ln� hled�n� z�znamu.")
define_cmd(ListForm, 'EDIT',
           #enabled=_current_form_can_update)
           "Vyvol�n� inline editace aktu�ln� bu�ky.")
define_cmd(ListForm, 'COPY_CELL',
           "Zkop�rov�n� obsahu aktu�ln� bu�ky do clipboardu.")
define_cmd(ListForm, 'FILTER_BY_CELL',
           "Vyfiltrov�n� formul��e podle hodnoty aktu�ln� bu�ky.")
define_cmd(ListForm, 'EXPORT_CSV',
           "Export ��dkov�ho formul��e do csv souboru.")
define_cmd(ListForm, 'LINE_COMMIT',
           "Dokon�en� editace z�znamu (ulo�en�).")
define_cmd(ListForm, 'LINE_ROLLBACK',
           "Kompletn� zru�en� editace z�znamu.")
define_cmd(ListForm, 'LINE_SOFT_ROLLBACK',
           "Kompletn� zru�en� editace zat�m nezm�n�n�ho z�znamu.")
define_cmd(ListForm, 'FINISH_EDITING',
           "Opu�t�n� editace ��dku.")
define_cmd(ListForm, 'CELL_COMMIT',
           "Ukon�en� editace pol��ka s�novou hodnotou.")
define_cmd(ListForm, 'CELL_ROLLBACK',
           "Ukon�en� editace pol��ka s�vr�cen�m p�vodn� hodnoty.")
define_cmd(ListForm, 'NEW_LINE_AFTER',
           #enabled=_current_form_can_insert)
           "Vlo�en� nov�ho z�znamu za aktu�ln� ��dek.")
define_cmd(ListForm, 'NEW_LINE_AFTER_COPY',
           #enabled=_current_form_can_insert)
           "Vlo�en� nov�ho z�znamu za aktu�ln� ��dek jako jeho kopie.")
define_cmd(ListForm, 'NEW_LINE_BEFORE',
           #enabled=_current_form_can_insert)
           "Vlo�en� nov�ho z�znamu p�ed aktu�ln� ��dek.")
define_cmd(ListForm, 'NEW_LINE_BEFORE_COPY',
           #enabled=_current_form_can_insert)
           "Vlo�en� nov�ho z�znamu p�ed aktu�ln� ��dek jako jeho kopie.")
define_cmd(ListForm, 'SET_GROUPING_COLUMN',
           #enabled=ListForm.can_set_grouping)
           "Zm�na sloupce vizu�n�ho seskupov�n�.")
define_cmd(BrowseForm, 'NEW_RECORD',
           #enabled=_current_form_can_insert)
           "Otev�en� edita�n�ho formul��e pro vlo�en� nov�ho z�znamu.")
define_cmd(BrowseForm, 'NEW_RECORD_COPY',
           #enabled=_current_form_can_insert)
           "Otev�en� edita�n�ho formul��e pro nov� z�znam kopi� aktu�ln�ho.")
define_cmd(BrowseForm, 'EDIT_RECORD',
           #enabled=_current_form_can_update)
           "Editace aktu�ln�ho z�znamu v edita�n�m formul��i.")
define_cmd(BrowseForm, 'IMPORT_INTERACTIVE',
           #enabled=_current_form_can_insert)
           "Import CSV dat s potvrzen�m a mo�nost� editace ka�d�ho z�znamu.")
define_cmd(EditForm, 'COMMIT_RECORD',
           "Ukon�en� edita�n�ho formul��e s ulo�en�m zm�n.")
define_cmd(EditForm, 'NAVIGATE',
           "Navigace mezi pol��ky edita�n�ho formul��e.")
define_cmd(EditForm, 'NAVIGATE_BACK',
           "Zp�tn� navigace mezi pol��ky edita�n�ho formul��e.")
define_cmd(BrowsableShowForm, 'NEXT_RECORD',
           "P�echod na dal�� z�znam.")
define_cmd(BrowsableShowForm, 'PREVIOUS_RECORD',
           "P�echod na p�edchoz� z�znam.")
define_cmd(BrowsableShowForm, 'FIRST_RECORD',
           "P�echod na prvn� z�znam.")
define_cmd(BrowsableShowForm, 'LAST_RECORD',
           "P�echod na posledn� z�znam.")
define_cmd(DualForm, 'OTHER_FORM',
           "P�echod mezi podformul��i du�ln�ho formul��e.")
define_cmd(PrintForm, 'NEXT_PAGE',
           "P�echod na dal�� str�nku tiskov�ho n�hledu.")
define_cmd(PrintForm, 'PREVIOUS_PAGE',
           "P�echod na p�edchoz� str�nku tiskov�ho n�hledu.")
define_cmd(InputField, 'RESET_FIELD',
           "Vr�cen� p�vodn� hodnoty vstupn�ho pol��ka.")
define_cmd(InputField, 'COMMIT_FIELD',
           "�sp�n� ukon�en� editace vstupn�ho pol��ka.")
define_cmd(InputField, 'LEAVE_FIELD',
           "Odchod z editace vstupn�ho pol��ka.")
define_cmd(Invocable, 'INVOKE_SELECTION',
           "Vyvol�n� v�b�ru hodnoty vstupn�ho pol��ka.")
define_cmd(Invocable, 'INVOKE_SELECTION_ALTERNATE',
           "Vyvol�n� alternativn�ho zp�sobu v�b�ru hodnoty vstupn�ho pol��ka.")
define_cmd(ListField, 'INVOKE_EDIT_FORM',
           "Vyvol�n� edita�n�ho formul��e nad aktu�ln�m z�znamem 'ListField'.")
define_cmd(ListField, 'INVOKE_BROWSE_FORM',
           "Zobrazen� aktu�ln�ho z�znamu 'ListField' v nov�m formul��i.")
define_cmd(ListField, 'CHOOSE_KEY',
           "V�b�r n�vratov�ho sloupce a hodnoty pro 'ListField'.")
define_cmd(Dialog, 'CLOSE_DIALOG',
           "Opu�t�n� dialogu bez potvrzen�.")
define_cmd(Dialog, 'COMMIT_DIALOG',
           "Odesl�n� dialogu stejn� jako stiskem v�choz�ho tla��tka.")


DEFAULT_COMMAND_KEYS = (
    (Application.COMMAND_BREAK,                   'Ctrl-g'),
    (Application.COMMAND_LEAVE_FORM,              'Escape'),
    (Application.COMMAND_NEXT_FORM,               'Ctrl-Down'),
    (Application.COMMAND_PREV_FORM,               'Ctrl-Up'),
    (Application.COMMAND_REFRESH,                 'Ctrl-l'),
    (Application.COMMAND_SHOW_POPUP_MENU,         'Ctrl-M'),
    (Form.COMMAND_PRINT,                         ('Ctrl-x', 'p')),
    (RecordForm.COMMAND_DELETE_RECORD,            'F8'),
    (LookupForm.COMMAND_SORT_COLUMN,              'F4'),
    (LookupForm.COMMAND_FILTER,                   'Ctrl-F4'),
    (LookupForm.COMMAND_SEARCH_NEXT,              'Ctrl-s'),
    (LookupForm.COMMAND_SEARCH_PREVIOUS,          'Ctrl-r'),
    (LookupForm.COMMAND_SEARCH,                   'F3'),
    (LookupForm.COMMAND_JUMP,                     'Ctrl-j'),
    (ListForm.COMMAND_INCREMENTAL_SEARCH,         'Ctrl-F3'),
    (ListForm.COMMAND_FULL_INCREMENTAL_SEARCH,   ('Ctrl-u', 'Ctrl-F3')),
    (ListForm.COMMAND_ACTIVATE,                   'Enter'),
    (ListForm.COMMAND_ACTIVATE_ALTERNATE,         ' '),
    (ListForm.COMMAND_COPY_CELL,                  'Ctrl-c'),
    (ListForm.COMMAND_FIRST_COLUMN,               'Home'),
    (ListForm.COMMAND_LAST_COLUMN,                'End'),
    (ListForm.COMMAND_EXPORT_CSV,                 'Ctrl-e'),
    (ListForm.COMMAND_EDIT,                       'F9'),
    (ListForm.COMMAND_LINE_ROLLBACK,              'Ctrl-F12'),
    (ListForm.COMMAND_FINISH_EDITING,             'Escape'),
    (ListForm.COMMAND_LINE_COMMIT,                'F12'),
    (ListForm.COMMAND_CELL_COMMIT,                'Enter'),
    (ListForm.COMMAND_CELL_ROLLBACK,              'Escape'),
    (ListForm.COMMAND_NEW_LINE_AFTER,             'Insert'),
    (ListForm.COMMAND_NEW_LINE_AFTER_COPY,        'F7'),
    (ListForm.COMMAND_NEW_LINE_BEFORE,            'Ctrl-Insert'),
    (ListForm.COMMAND_NEW_LINE_BEFORE_COPY,       'Ctrl-F7'),
    (BrowseForm.COMMAND_NEW_RECORD,               'F6'),
    (BrowseForm.COMMAND_NEW_RECORD_COPY,          'Ctrl-F6'),
    (BrowseForm.COMMAND_IMPORT_INTERACTIVE,       'Alt-F6'),
    (BrowseForm.COMMAND_EDIT_RECORD,              'F5'),
    (EditForm.COMMAND_COMMIT_RECORD,              'Ctrl-Enter'),
    (EditForm.COMMAND_NAVIGATE,                   'Tab'),
    (EditForm.COMMAND_NAVIGATE_BACK,              'Shift-Tab'),        
    (BrowsableShowForm.COMMAND_NEXT_RECORD,       'Next'),
    (BrowsableShowForm.COMMAND_PREVIOUS_RECORD,   'Prior'),
    (BrowsableShowForm.COMMAND_FIRST_RECORD,      'Home'),
    (BrowsableShowForm.COMMAND_LAST_RECORD,       'End'),
    (DualForm.COMMAND_OTHER_FORM,                 'Ctrl-Tab'),
    (PrintForm.COMMAND_NEXT_PAGE,                 'Next'),
    (PrintForm.COMMAND_PREVIOUS_PAGE,             'Prior'),
    (InputField.COMMAND_COMMIT_FIELD,             'Enter'),
    (InputField.COMMAND_LEAVE_FIELD,              'Escape'),
    (Invocable.COMMAND_INVOKE_SELECTION,          'F2'),
    (Invocable.COMMAND_INVOKE_SELECTION_ALTERNATE,'Ctrl-F2'),
    (Dialog.COMMAND_CLOSE_DIALOG,                 'Escape'),
    (Dialog.COMMAND_COMMIT_DIALOG,                'Ctrl-Enter'))



if __debug__:
    Application.COMMAND_CUSTOM_DEBUG = Command('application.custom-debug')
    """Pomocn� p��kaz pro vyvol�n� pomocn� lad�c� funkce.

    Vyvolan� funkce je 'config.custom_debug'.

    """
    DEFAULT_COMMAND_KEYS += \
        ((Application.COMMAND_CUSTOM_DEBUG, 'Ctrl-Backspace'),)


FORM_COMMAND_MENU = ((
    (_("P�edchoz� okno"),             Application.COMMAND_PREV_FORM),
    (_("N�sleduj�c� okno"),           Application.COMMAND_NEXT_FORM),
    (_("Zav��t aktu�ln� okno"),       Application.COMMAND_LEAVE_FORM),
    ),(
    (_("Skok na z�znam"),             LookupForm.COMMAND_JUMP),
    (_("Hledat"),                     LookupForm.COMMAND_SEARCH),
    (_("Hledat dal��"),               LookupForm.COMMAND_SEARCH_NEXT),
    (_("Hledat p�edchoz�"),           LookupForm.COMMAND_SEARCH_PREVIOUS),
    (_("Inkrement�ln� hled�n�"),      ListForm.COMMAND_INCREMENTAL_SEARCH),
    (_("Inkrement�ln� hled�n� pod�et�zce"),
                                      ListForm.COMMAND_FULL_INCREMENTAL_SEARCH),
    ),(
    (_("T��d�n�"),                    LookupForm.COMMAND_SORT_COLUMN),
    (_("Filtrov�n�"),                 LookupForm.COMMAND_FILTER),
    ),(
    (_("Nov� z�znam"),                BrowseForm.COMMAND_NEW_RECORD),
    (_("Nov� z�znam - kopie"),        BrowseForm.COMMAND_NEW_RECORD_COPY),
    (_("Editovat z�znam"),            BrowseForm.COMMAND_EDIT_RECORD),
    (_("Vlo�it ��dku nad"),           ListForm.COMMAND_NEW_LINE_BEFORE),
    (_("Vlo�it ��dku pod"),           ListForm.COMMAND_NEW_LINE_AFTER),
    (_("Kop�rovat ��dku nad"),        ListForm.COMMAND_NEW_LINE_BEFORE_COPY),
    (_("Kop�rovat ��dku pod"),        ListForm.COMMAND_NEW_LINE_AFTER_COPY),
    (_("Editace bu�ky"),              ListForm.COMMAND_EDIT),
    (_("Smazat z�znam"),              RecordForm.COMMAND_DELETE_RECORD),
    ),(
    (_("Ulo�it"),                     ListForm.COMMAND_LINE_COMMIT),
    (_("Zru�it zm�ny"),               ListForm.COMMAND_LINE_ROLLBACK),
    ),(
    (_("Export do textov�ho souboru"),ListForm.COMMAND_EXPORT_CSV),
    ),(
    (_("Zobrazit n�hled z�znamu"),    ListForm.COMMAND_ACTIVATE),
    (_("N�hled v du�ln�m formul��i"), ListForm.COMMAND_ACTIVATE_ALTERNATE),
    ))
