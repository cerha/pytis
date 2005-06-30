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


class Command:
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


# Funkce zjišťující dostupnost konkrétních příkazů (definovaných níže).

def _check_perm(perm, name):
    try:
        data_spec = resolver().get(name, 'data_spec')
    except ResolverError:
        return True
    rights = data_spec.access_rights()
    if not rights:
        return True
    groups = pytis.data.DBDataDefault.class_access_groups(config.dbconnection)
    return rights.permitted(perm, groups)

def _can_run_form(appl, cmd, args):
    perm = pytis.data.Permission.VIEW
    if issubclass(args['form_class'], pytis.form.DualForm):
        try:
            dual_spec = resolver().get(args['name'], 'dual_spec')
        except ResolverError:
            return True
        result = _check_perm(perm, dual_spec.main_name()) and \
                 _check_perm(perm, dual_spec.side_name())
    else:
        result = _check_perm(perm, args['name'])
    return result

def _can_insert(appl, cmd, args):
    return _check_perm(pytis.data.Permission.INSERT, args['name'])

# TODO: Asi by bylo vhodnější pro tyto funkce vymyslet nějaký mechanismus,
# kterým by se např. automaticky volala metoda určitého jména a nebylo
# by tak zde třeba definovat celou řadu velice podobných funkcí.

def _current_form_can_insert(appl, cmd, args):
    f = appl.current_form()
    return f and f.check_permission(pytis.data.Permission.INSERT)

def _current_form_can_update(appl, cmd, args):
    f = appl.current_form()
    # TODO:uievent_id= neaktivní v DescriptiveDualFormu
    return f and f.check_permission(pytis.data.Permission.UPDATE)

def _current_form_can_delete(appl, cmd, args):
    f = appl.current_form()
    return f and f.check_permission(pytis.data.Permission.DELETE)

def _current_form_searching(appl, cmd, args):
    f = appl.current_form()
    return f and isinstance(f, LookupForm) and f.is_searching()

def _current_form_changed(appl, cmd, args):
    f = appl.current_form()
    return f and isinstance(f, ListForm) and f.is_changed()
       
# InputField properties

def _current_field_enabled(appl, cmd, args):
    f = InputField.focused()
    return f and f.is_enabled()
    
def _current_field_modified(appl, cmd, args):
    f = InputField.focused()
    return f and f.is_modified()
    
def _current_field_has_selection(appl, cmd, args):
    f = InputField.focused()
    return f and isinstance(f, ListField) and f.has_selection()
    
# Vlastní definice příkazů   

Application.COMMAND_EXIT = Command('application.exit')
"""Ukončení aplikace."""

Application.COMMAND_BREAK = Command('application.break')
"""Přerušení aktuálně prováděné operace."""

Application.COMMAND_REFRESH = Command('application.refresh')
"""Vyžádání obnovení obsahu aktivního formuláře."""

Application.COMMAND_NEW_RECORD = Command('application.new-record',
                                         enabled=_can_insert, static=True)
"""Vložení nového záznamu."""

Application.COMMAND_RUN_FORM = Command('application.run-form',
                                       enabled=_can_run_form, static=True)
"""Spuštění formuláře."""

Application.COMMAND_RUN_PROCEDURE = Command('application.run-procedure')
"""Spuštění procedury."""

Application.COMMAND_LEAVE_FORM = \
    Command('application.leave-form',
            enabled=lambda appl, cmd, args: appl.current_form() is not None)
"""Odstranění aktivního okna formuláře z aplikace."""

Application.COMMAND_RAISE_FORM = Command('application.raise-form')
"""Vyzvednutí okna formuláře v okně aplikace (argument je instance `Form')."""

Application.COMMAND_PREV_FORM = \
    Command('application.prev-form',
            enabled=lambda appl, cmd, args: appl.window_count() > 1)
"""Vyzvednutí okna předchozího formuláře."""

Application.COMMAND_NEXT_FORM = \
    Command('application.next-form',
            enabled=lambda appl, cmd, args: appl.window_count() > 1)
"""Vyzvednutí okna formuláře následujícího za aktivním oknem."""


Application.COMMAND_SHOW_POPUP_MENU = Command('application.show-popup-menu')
"""Zobraz kontextové menu aktivního prvku, pokud to pro daný prvek lze."""

Form.COMMAND_PRINT = Command('form.print')
"""Tisk aktuálního obsahu formuláře."""

LookupForm.COMMAND_FILTER = Command('lookup-form.filter')
"""Filtrování záznamů."""

LookupForm.COMMAND_JUMP = Command('lookup-form.jump')
"""Skok na záznam."""

LookupForm.COMMAND_SEARCH = Command('lookup-form.search')
"""Hledání záznamu."""

LookupForm.COMMAND_SEARCH_PREVIOUS = Command('lookup-form.search-previous',
                                             enabled=_current_form_searching)
"""Hledání Předchozího záznamu bez dialogu."""

LookupForm.COMMAND_SEARCH_NEXT = Command('lookup-form.search-next',
                                         enabled=_current_form_searching)
"""Hledání dalšího záznamu bez dialogu."""

LookupForm.COMMAND_SORT_COLUMN = Command('lookup-form.sort-column',
                                         enabled=LookupForm.can_sort)
"""Setřídění podle sloupce."""

ListForm.COMMAND_ACTIVATE = Command('list-form.activate')
"""Vyvolání aktivační funkce pro řádek řádkového formuláře."""

ListForm.COMMAND_ACTIVATE_ALTERNATE = Command('list-form.activate-alternate')
"""Vyvolání alternativní aktivační funkce pro řádek řádkového formuláře."""

ListForm.COMMAND_SHOW_CELL_CODEBOOK = \
    Command('list-form.show-cell-codebook',
            enabled=ListForm.can_show_cell_codebook)
"""Vyvolání číselníku aktivní buňky řádkového formuláře."""

ListForm.COMMAND_SELECT_CELL = Command('list-form.select-cell', log_=False)
"""Výběr buňky seznamu."""

ListForm.COMMAND_FIRST_COLUMN = Command('list-form.first-column', log_=False)
"""Přechod na první sloupec tabulky."""

ListForm.COMMAND_LAST_COLUMN = Command('list-form.last-column', log_=False)
"""Přechod na poslední sloupec tabulky."""

ListForm.COMMAND_INCREMENTAL_SEARCH = Command('list-form.incremental-search')
"""Prefixové inkrementální hledání záznamu."""

ListForm.COMMAND_FULL_INCREMENTAL_SEARCH = Command('list-form.full-incremental-search')
"""Plné inkrementální hledání záznamu."""

ListForm.COMMAND_EDIT = Command('list-form.edit',
                                enabled=_current_form_can_update)
"""Vyvolání inline editace aktuální buňky."""

ListForm.COMMAND_COPY_CELL = Command('list-form.copy-cell')
"""Zkopírování obsahu aktuální buňky do clipboardu."""

ListForm.COMMAND_FILTER_BY_CELL = Command('list-form.filter-by-cell')
"""Vyfiltrování formuláře podle hodnoty aktuální buňky."""

ListForm.COMMAND_EXPORT_CSV = Command('list-form.export-csv')
"""Export řádkového formuláře do csv souboru."""

ListForm.COMMAND_LINE_COMMIT = Command('list-form.line-commit',
                                       enabled=_current_form_changed)
"""Dokončení editace záznamu (uložení)."""

ListForm.COMMAND_LINE_ROLLBACK = Command('list-form.line-rollback',
                                         enabled=_current_form_changed)
"""Kompletní zrušení editace záznamu."""

ListForm.COMMAND_LINE_SOFT_ROLLBACK = Command('list-form.line-soft-rollback')
"""Kompletní zrušení editace zatím nezměněného záznamu."""

ListForm.COMMAND_FINISH_EDITING = Command('list-form.finish-editing')
"""Opuštění editace řádku."""

ListForm.COMMAND_LINE_DELETE = Command('list-form.line-delete',
                                       enabled=_current_form_can_delete)
"""Smazání aktuálního záznamu."""

ListForm.COMMAND_CELL_COMMIT = Command('list-form.cell-commit')
"""Ukončení editace políčka s novou hodnotou."""

ListForm.COMMAND_CELL_ROLLBACK = Command('list-form.cell-rollback')
"""Ukončení editace políčka s vrácením původní hodnoty."""

ListForm.COMMAND_NEW_LINE_AFTER = Command('list-form.new-line-after',
                                          enabled=_current_form_can_insert)
"""Inline vložení nového záznamu za aktuální řádek."""

ListForm.COMMAND_NEW_LINE_AFTER_COPY = Command('list-form.new-line-after-copy',
                                               enabled=_current_form_can_insert)
"""Inline vložení nového záznamu za aktuální řádek jako jeho kopie."""

ListForm.COMMAND_NEW_LINE_BEFORE = Command('list-form.new-line-before',
                                          enabled=_current_form_can_insert)
"""Inline vložení nového záznamu před aktuální řádek."""

ListForm.COMMAND_NEW_LINE_BEFORE_COPY =Command('list-form.new-line-before-copy',
                                               enabled=_current_form_can_insert)
"""Inline vložení nového záznamu před aktuální řádek jako jeho kopie."""

ListForm.COMMAND_SET_GROUPING_COLUMN = \
    Command('list-form.set-grouping-column',
            enabled=ListForm.can_set_grouping)
"""Změna sloupce vizuáního seskupování (vyžaduje argument 'column_id')."""

BrowseForm.COMMAND_NEW_RECORD = Command('browse-form.new-record',
                                         enabled=_current_form_can_insert)
"""Formulářová editace nového záznamu v řádkovém formuláři."""

BrowseForm.COMMAND_NEW_RECORD_COPY = Command('browse-form.new-record-copy',
                                             enabled=_current_form_can_insert)
"""Formulářová editace nového záznamu jako kopie aktuálního záznamu."""

BrowseForm.COMMAND_EDIT_RECORD = Command('browse-form.edit-record',
                                         enabled=_current_form_can_update)
"""Editace aktuálního záznamu v popup formuláři."""

BrowseForm.COMMAND_IMPORT_INTERACTIVE =Command('browse-form.import-interactive',
                                               enabled=_current_form_can_insert)
"""Import CSV dat s potvrzením a možností editace každého záznamu."""

EditForm.COMMAND_RECORD_INSERT = Command('edit-form.record-insert',
                                         enabled=_current_form_can_insert)
"""Vložení nového záznamu z editačního formuláře."""

EditForm.COMMAND_RECORD_UPDATE = Command('edit-form.record-update',
                                         enabled=_current_form_can_update)
"""Uložení editovaného záznamu v editačním formuláři."""

EditForm.COMMAND_RECORD_DELETE = Command('edit-form.record-delete',
                                         enabled=_current_form_can_delete)
"""Vymazání editovaného záznamu z databáze."""

EditForm.COMMAND_RECORD_COMMIT = Command('edit-form.record-commit')
"""Ukončení editačního formuláře s uložením změn."""

EditForm.COMMAND_NAVIGATE = Command('edit-form.navigate')
"""Navigace mezi políčky editačního formuláře."""

EditForm.COMMAND_NAVIGATE_BACK = Command('edit-form.navigate-back')
"""Zpětná navigace mezi políčky editačního formuláře."""

BrowsableShowForm.COMMAND_NEXT_RECORD = Command('edit-form.next-record')
"""Přechod na další záznam."""

BrowsableShowForm.COMMAND_PREVIOUS_RECORD= Command('edit-form.previous-record')
"""Přechod na předchozí záznam."""

BrowsableShowForm.COMMAND_FIRST_RECORD = Command('edit-form.first-record')
"""Přechod na první záznam."""

BrowsableShowForm.COMMAND_LAST_RECORD = Command('edit-form.last-record')
"""Přechod na poslední záznam."""

DualForm.COMMAND_OTHER_FORM = Command('dual-form.other-form')
"""Přechod mezi podformuláři duálního formuláře."""

PrintForm.COMMAND_NEXT_PAGE = Command('print-form.next-page')
"""Přechod na další stránku tiskového náhledu."""

PrintForm.COMMAND_PREVIOUS_PAGE = Command('print-form.previous-page')
"""Přechod na předchozí stránku tiskového náhledu."""

InputField.COMMAND_RESET_FIELD = Command('input-field.command-reset-field',
                                         enabled=_current_field_modified)
"""Vrácení původní hodnoty vstupního políčka."""

InputField.COMMAND_COMMIT_FIELD = Command('input-field.command-commit-field')
"""Úspěšné ukončení editace vstupního políčka."""

InputField.COMMAND_LEAVE_FIELD = Command('input-field.command-leave-field')
"""Odchod z editace vstupního políčka."""

Invocable.COMMAND_INVOKE_SELECTION = Command('invocable.invoke-selection',
                                             enabled=_current_field_enabled)
"""Vyvolání výběru hodnoty vstupního políčka."""

Invocable.COMMAND_INVOKE_SELECTION_ALTERNATE = \
    Command('invocable.invoke-selection-alternate',
            enabled=_current_field_enabled)
"""Vyvolání alternativního způsobu výběru hodnoty vstupního políčka."""

ListField.COMMAND_INVOKE_EDIT_FORM = \
    Command('list-field.invoke-edit-form',
            enabled=_current_field_has_selection)
"""Vyvolání editačního formuláře nad aktuálním záznamem 'ListField'."""

ListField.COMMAND_INVOKE_BROWSE_FORM = \
    Command('list-field.invoke-browse-form',
            enabled=_current_field_has_selection)
"""Zobrazení aktuálního záznamu 'ListField' ve formuláři 'BrowseForm'."""

ListField.COMMAND_CHOOSE_KEY = Command('list-field.choose-key',
                                       enabled=_current_field_has_selection)
"""Výběr návratového sloupce a hodnoty pro 'ListField'."""

Dialog.COMMAND_CLOSE_DIALOG = Command('dialog.close-dialog')
"""Opuštění dialogu bez potvrzení."""

Dialog.COMMAND_COMMIT_DIALOG = Command('dialog.commit-dialog')
"""Odeslání dialogu stejně jako stiskem výchozího tlačítka."""


DEFAULT_COMMAND_KEYS = (
    (Application.COMMAND_BREAK,                   'Ctrl-g'),
    (Application.COMMAND_LEAVE_FORM,              'Escape'),
    (Application.COMMAND_NEXT_FORM,               'Ctrl-Down'),
    (Application.COMMAND_PREV_FORM,               'Ctrl-Up'),
    (Application.COMMAND_REFRESH,                 'Ctrl-l'),
    (Application.COMMAND_SHOW_POPUP_MENU,         'Ctrl-M'),
    (Form.COMMAND_PRINT,                         ('Ctrl-x', 'p')),
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
    (ListForm.COMMAND_LINE_DELETE,                'F8'),
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
    (EditForm.COMMAND_RECORD_DELETE,              'F8'),
    (EditForm.COMMAND_RECORD_INSERT,              'F7'),
    (EditForm.COMMAND_RECORD_UPDATE,              'F12'),
    (EditForm.COMMAND_RECORD_COMMIT,              'Ctrl-Enter'),
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
