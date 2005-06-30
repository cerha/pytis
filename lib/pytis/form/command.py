# -*- coding: iso-8859-2 -*-

# Definice uživatelských příkazů
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

"""Definice uživatelských příkazů.

Tento soubor obsahuje jednak třídu 'Command', sloužící k definici všech
podporovaných příkazů, a jednak definice příkazů.

Všechny příkazy jsou centrálně definovány zde, z důvodu konzistence a
přehlednosti, zejména z pohledu tvůrců definic aplikací.  Všechny definované
příkazy jsou konstantami některé z tříd modulu 'pytis.form'; názvy těchto
konstant začínají prefixem 'COMMAND_' (tuto konvenci je nutno dodržovat).

Přiřazení kláves příkazům:

V aplikaci mimo modul 'defs' nepoužíváme přímý zápis kláves, nýbrž pouze jim
odpovídající příkazy.  To nám umožňuje nestarat se v aplikačním kódu
o přiřazení kláves, ta definujeme na jediném místě v tomto modulu.

"""


from pytis.form import *


class Command(object):
    """Reprezentace příkazu uživatelského rozhraní.

    Klávesa, která příkaz vyvolává, je dostupná ve formě veřejného atributu
    `key' a její hodnotu lze v definičních souborech konkrétní aplikace
    prostřednictvím tohoto atributu změnit.

    Při definici uživatelských příkazů lze definovat vlastní obslužné rutiny
    pro jejich zpracování.  Uživatelským příkazem je myšlen příkaz, který není
    standardně ošetřován žádnou třídou uživatelského rozhraní (formulářem).
    Pokud však formulář podporuje uživatelské příkazy, vyvolá funkci
    specifikovanou argumentem konstruktoru 'handler' s parametry, které závisí
    na typu dané třídy uživatelského rozhraní. Třída BrowseForm tak například
    jako argument předá data aktuálního řádku seznamu apod. Více v dokumentaci
    jednotlivých formulářových tříd.
    
    Identifikátor příkazu je v tuto chvíli významný pouze pro logování.
    Příkazy jsou rozpoznávány dle konkrétních instancí, ne podle svého
    identifikátoru.

    """
    def __init__(self, id, key=None, handler=None, log_=True,
                 enabled=True, access_groups=None, static=False):
        """Definuj příkaz.

        Argumenty:

          id -- identifikátor příkazu, libovolný neprázdný řetězec mezi
            identifikátory příkazu unikátní
          handler -- funkce volaná při zpracování příkazu.  Má význam při
            definici uživatelských příkazů.  Blíže viz dokumentace třídy.
            Hodnotou je callable object, nebo None.
          log_ -- právě když je pravdivé, je vyvolání příkazu logováno jako
            EVENT, jinak je logováno pouze jako DEBUG
          access_groups -- sekvence jmen skupin (strings), které mají právo
            příkaz vyvolat; může být též 'None', v kterémžto případě příkaz
            mohou vyvolat všechny skupiny.  Toto oprávnění je formální,
            zohledněné jen v uživatelském rozhraní, nemá faktickou bezpečnostní
            roli.  Přítomnost uživatele ve skupinách je zjišťována pouze jednou
            při inicializaci instance příkazu, což je většinou při startu
            aplikace.
          enabled -- buďto přímo boolean hodnota určující, zda je příkaz
            aktivní, nebo odkaz na funkci, která toto zjistí (a vrátí
            odpovídající boolean hodnotu).  Jde o funkci tří argumentů (APPL,
            COMMAND, ARGS), kde APPL je instance aplikace (třídy
            'Application'), COMMAND je instance příkazu a ARGS je slovník
            arguemntů s jakými bude příkaz volán.
          static -- pokud je předána pravdivá hodnota, bude hodnota vrácená
            funkcí `enabled' považována za neměnnou a výsledek tedy bude
            cachován.  V opačném případě (výchozí hodnota) bude funkce volána
            při každém požadavku na zjištění hodnoty enabled.
    
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
                "Použit potlačený argument `key' třídy `Command':", (key, id))
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
        """Vrať identifikátor zadaný v konstruktoru."""
        return self._id

    def handler(self):
        """Vrať rutinu pro zpracování příkazu."""
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
        """Vrať druh logovací hlášky, pod kterým má být příkaz logován."""
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
        # TODO: Časem zrušit.
        if name == 'key':
            log(OPERATIONAL,
                "Nastaven potlačený atribut `key' třídy `Command':",
                (value, self._id))
        self.__dict__[name] = value
            

def invoke_command(command, **kwargs):
    """Vyvolej globální zpracování příkazu 'command'.

    Argumenty:

      command -- instance třídy 'Command'
      kwargs -- parametry příkazu

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
        # TODO: Asi by bylo vhodnější toto přesunout někam do kódu vlastního
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
           "Ukončení aplikace.")
define_cmd(Application, 'BREAK',
           "Přerušení aktuálně prováděné operace.")
define_cmd(Application, 'REFRESH',
           "Vyžádání obnovení obsahu aktivního formuláře.")
define_cmd(Application, 'NEW_RECORD',
           "Vložení nového záznamu.")
define_cmd(Application, 'RUN_FORM',
           "Spuštění formuláře.")
define_cmd(Application, 'RUN_PROCEDURE',
           "Spuštění procedury.")
define_cmd(Application, 'LEAVE_FORM',
           "Odstranění aktivního okna formuláře z aplikace.")
define_cmd(Application, 'RAISE_FORM',
           "Vyzvednutí okna formuláře v okně aplikace.")
define_cmd(Application, 'PREV_FORM',
           "Vyzvednutí okna předchozího formuláře.")
define_cmd(Application, 'NEXT_FORM',
           "Vyzvednutí okna formuláře následujícího za aktivním oknem.")
define_cmd(Application, 'SHOW_POPUP_MENU',
           "Zobraz kontextové menu aktivního prvku, pokud to lze.")
define_cmd(Form, 'PRINT',
           "Tisk aktuálního obsahu formuláře.")
define_cmd(LookupForm, 'FILTER',
           "Filtrování záznamů.")
define_cmd(LookupForm, 'JUMP',
           "Skok na záznam.")
define_cmd(LookupForm, 'SEARCH',
           "Hledání záznamu.")
define_cmd(LookupForm, 'SEARCH_PREVIOUS',
           "Hledání Předchozího záznamu bez dialogu.")
define_cmd(LookupForm, 'SEARCH_NEXT',
           "Hledání dalšího záznamu bez dialogu.")
define_cmd(LookupForm, 'SORT_COLUMN',
           "Setřídění podle sloupce.")
define_cmd(RecordForm, 'DELETE_RECORD',
           #enabled=_current_form_can_delete)
           "Vymazání editovaného záznamu z databáze.")
define_cmd(ListForm, 'ACTIVATE',
           "Vyvolání aktivační funkce pro aktuální řádek formuláře.")
define_cmd(ListForm, 'ACTIVATE_ALTERNATE',
           "Vyvolání alternativní aktivační funkce pro aktuální řádek.")
define_cmd(ListForm, 'SHOW_CELL_CODEBOOK',
           "Vyvolání číselníku aktivní buňky řádkového formuláře.")
define_cmd(ListForm, 'SELECT_CELL',
           "Výběr buňky seznamu.")
define_cmd(ListForm, 'FIRST_COLUMN',
           "Přechod na první sloupec tabulky.")
define_cmd(ListForm, 'LAST_COLUMN',
           "Přechod na poslední sloupec tabulky.")
define_cmd(ListForm, 'INCREMENTAL_SEARCH',
           "Prefixové inkrementální hledání záznamu.")
define_cmd(ListForm, 'FULL_INCREMENTAL_SEARCH',
           "Plné inkrementální hledání záznamu.")
define_cmd(ListForm, 'EDIT',
           #enabled=_current_form_can_update)
           "Vyvolání inline editace aktuální buňky.")
define_cmd(ListForm, 'COPY_CELL',
           "Zkopírování obsahu aktuální buňky do clipboardu.")
define_cmd(ListForm, 'FILTER_BY_CELL',
           "Vyfiltrování formuláře podle hodnoty aktuální buňky.")
define_cmd(ListForm, 'EXPORT_CSV',
           "Export řádkového formuláře do csv souboru.")
define_cmd(ListForm, 'LINE_COMMIT',
           "Dokončení editace záznamu (uložení).")
define_cmd(ListForm, 'LINE_ROLLBACK',
           "Kompletní zrušení editace záznamu.")
define_cmd(ListForm, 'LINE_SOFT_ROLLBACK',
           "Kompletní zrušení editace zatím nezměněného záznamu.")
define_cmd(ListForm, 'FINISH_EDITING',
           "Opuštění editace řádku.")
define_cmd(ListForm, 'CELL_COMMIT',
           "Ukončení editace políčka s novou hodnotou.")
define_cmd(ListForm, 'CELL_ROLLBACK',
           "Ukončení editace políčka s vrácením původní hodnoty.")
define_cmd(ListForm, 'NEW_LINE_AFTER',
           #enabled=_current_form_can_insert)
           "Vložení nového záznamu za aktuální řádek.")
define_cmd(ListForm, 'NEW_LINE_AFTER_COPY',
           #enabled=_current_form_can_insert)
           "Vložení nového záznamu za aktuální řádek jako jeho kopie.")
define_cmd(ListForm, 'NEW_LINE_BEFORE',
           #enabled=_current_form_can_insert)
           "Vložení nového záznamu před aktuální řádek.")
define_cmd(ListForm, 'NEW_LINE_BEFORE_COPY',
           #enabled=_current_form_can_insert)
           "Vložení nového záznamu před aktuální řádek jako jeho kopie.")
define_cmd(ListForm, 'SET_GROUPING_COLUMN',
           #enabled=ListForm.can_set_grouping)
           "Změna sloupce vizuáního seskupování.")
define_cmd(BrowseForm, 'NEW_RECORD',
           #enabled=_current_form_can_insert)
           "Otevření editačního formuláře pro vložení nového záznamu.")
define_cmd(BrowseForm, 'NEW_RECORD_COPY',
           #enabled=_current_form_can_insert)
           "Otevření editačního formuláře pro nový záznam kopií aktuálního.")
define_cmd(BrowseForm, 'EDIT_RECORD',
           #enabled=_current_form_can_update)
           "Editace aktuálního záznamu v editačním formuláři.")
define_cmd(BrowseForm, 'IMPORT_INTERACTIVE',
           #enabled=_current_form_can_insert)
           "Import CSV dat s potvrzením a možností editace každého záznamu.")
define_cmd(EditForm, 'COMMIT_RECORD',
           "Ukončení editačního formuláře s uložením změn.")
define_cmd(EditForm, 'NAVIGATE',
           "Navigace mezi políčky editačního formuláře.")
define_cmd(EditForm, 'NAVIGATE_BACK',
           "Zpětná navigace mezi políčky editačního formuláře.")
define_cmd(BrowsableShowForm, 'NEXT_RECORD',
           "Přechod na další záznam.")
define_cmd(BrowsableShowForm, 'PREVIOUS_RECORD',
           "Přechod na předchozí záznam.")
define_cmd(BrowsableShowForm, 'FIRST_RECORD',
           "Přechod na první záznam.")
define_cmd(BrowsableShowForm, 'LAST_RECORD',
           "Přechod na poslední záznam.")
define_cmd(DualForm, 'OTHER_FORM',
           "Přechod mezi podformuláři duálního formuláře.")
define_cmd(PrintForm, 'NEXT_PAGE',
           "Přechod na další stránku tiskového náhledu.")
define_cmd(PrintForm, 'PREVIOUS_PAGE',
           "Přechod na předchozí stránku tiskového náhledu.")
define_cmd(InputField, 'RESET_FIELD',
           "Vrácení původní hodnoty vstupního políčka.")
define_cmd(InputField, 'COMMIT_FIELD',
           "Úspěšné ukončení editace vstupního políčka.")
define_cmd(InputField, 'LEAVE_FIELD',
           "Odchod z editace vstupního políčka.")
define_cmd(Invocable, 'INVOKE_SELECTION',
           "Vyvolání výběru hodnoty vstupního políčka.")
define_cmd(Invocable, 'INVOKE_SELECTION_ALTERNATE',
           "Vyvolání alternativního způsobu výběru hodnoty vstupního políčka.")
define_cmd(ListField, 'INVOKE_EDIT_FORM',
           "Vyvolání editačního formuláře nad aktuálním záznamem 'ListField'.")
define_cmd(ListField, 'INVOKE_BROWSE_FORM',
           "Zobrazení aktuálního záznamu 'ListField' v novém formuláři.")
define_cmd(ListField, 'CHOOSE_KEY',
           "Výběr návratového sloupce a hodnoty pro 'ListField'.")
define_cmd(Dialog, 'CLOSE_DIALOG',
           "Opuštění dialogu bez potvrzení.")
define_cmd(Dialog, 'COMMIT_DIALOG',
           "Odeslání dialogu stejně jako stiskem výchozího tlačítka.")


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
    """Pomocný příkaz pro vyvolání pomocné ladící funkce.

    Vyvolaná funkce je 'config.custom_debug'.

    """
    DEFAULT_COMMAND_KEYS += \
        ((Application.COMMAND_CUSTOM_DEBUG, 'Ctrl-Backspace'),)


FORM_COMMAND_MENU = ((
    (_("Předchozí okno"),             Application.COMMAND_PREV_FORM),
    (_("Následující okno"),           Application.COMMAND_NEXT_FORM),
    (_("Zavřít aktuální okno"),       Application.COMMAND_LEAVE_FORM),
    ),(
    (_("Skok na záznam"),             LookupForm.COMMAND_JUMP),
    (_("Hledat"),                     LookupForm.COMMAND_SEARCH),
    (_("Hledat další"),               LookupForm.COMMAND_SEARCH_NEXT),
    (_("Hledat předchozí"),           LookupForm.COMMAND_SEARCH_PREVIOUS),
    (_("Inkrementální hledání"),      ListForm.COMMAND_INCREMENTAL_SEARCH),
    (_("Inkrementální hledání podřetězce"),
                                      ListForm.COMMAND_FULL_INCREMENTAL_SEARCH),
    ),(
    (_("Třídění"),                    LookupForm.COMMAND_SORT_COLUMN),
    (_("Filtrování"),                 LookupForm.COMMAND_FILTER),
    ),(
    (_("Nový záznam"),                BrowseForm.COMMAND_NEW_RECORD),
    (_("Nový záznam - kopie"),        BrowseForm.COMMAND_NEW_RECORD_COPY),
    (_("Editovat záznam"),            BrowseForm.COMMAND_EDIT_RECORD),
    (_("Vložit řádku nad"),           ListForm.COMMAND_NEW_LINE_BEFORE),
    (_("Vložit řádku pod"),           ListForm.COMMAND_NEW_LINE_AFTER),
    (_("Kopírovat řádku nad"),        ListForm.COMMAND_NEW_LINE_BEFORE_COPY),
    (_("Kopírovat řádku pod"),        ListForm.COMMAND_NEW_LINE_AFTER_COPY),
    (_("Editace buňky"),              ListForm.COMMAND_EDIT),
    (_("Smazat záznam"),              RecordForm.COMMAND_DELETE_RECORD),
    ),(
    (_("Uložit"),                     ListForm.COMMAND_LINE_COMMIT),
    (_("Zrušit změny"),               ListForm.COMMAND_LINE_ROLLBACK),
    ),(
    (_("Export do textového souboru"),ListForm.COMMAND_EXPORT_CSV),
    ),(
    (_("Zobrazit náhled záznamu"),    ListForm.COMMAND_ACTIVATE),
    (_("Náhled v duálním formuláři"), ListForm.COMMAND_ACTIVATE_ALTERNATE),
    ))
