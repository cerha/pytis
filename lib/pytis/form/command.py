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
                 access_groups=None):
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
            roli.
    
        """
        assert is_anystring(id)
        assert key is None or is_string(key) or is_sequence(key)
        self._id = id
        if key is not None:
            log(OPERATIONAL,
                "Použit potlačený argument `key' třídy `Command':", (key, id))
            self.key = xtuple(key)
        self._handler = handler
        self._log = log_
        if access_groups is not None:
            access_groups = xtuple(access_groups)
        self._access_groups = access_groups
    
    def id(self):
        """Vrať identifikátor zadaný v konstruktoru."""
        return self._id

    def handler(self):
        """Vrať rutinu pro zpracování příkazu."""
        return self._handler

    def access_groups(self):
        """Vrať tuple jmen skupin (strings) s právem přístupu nebo 'None'.

        Vrácená hodnota odpovídá argumentu `access_groups' konstruktoru.

        """
        return self._access_groups

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
    return pytis.form.application._application.on_command(command, **kwargs)


Application.COMMAND_EXIT = Command('application.exit')
"""Ukončení aplikace."""
Application.COMMAND_RUN_FORM = Command('application.run-form')
"""Spuštění formuláře."""
Application.COMMAND_RUN_PROCEDURE = Command('application.run-procedure')
"""Spuštění procedury."""
Application.COMMAND_NEW_RECORD = Command('application.new-record')
"""Vložení nového záznamu."""
Application.COMMAND_LEAVE_FORM = Command('application.leave-form')
"""Odstranění aktivního okna formuláře z aplikace."""
Application.COMMAND_RAISE_FORM = Command('application.raise-form')
"""Vyzvednutí okna formuláře v okně aplikace (argument je instance `Form')."""
Application.COMMAND_PREV_FORM = Command('application.prev-form')
"""Vyzvednutí okna formuláře předcházejícího aktivní okno."""
Application.COMMAND_NEXT_FORM = Command('application.next-form')
"""Vyzvednutí okna formuláře následujícího za aktivním oknem."""
Application.COMMAND_BREAK = Command('application.break')
"""Přerušení aktuálně prováděné operace."""
Application.COMMAND_REFRESH = Command('application.refresh')
"""Vyžádání obnovení obsahu aktivního formuláře."""
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
LookupForm.COMMAND_SEARCH_PREVIOUS = Command('lookup-form.search')
"""Hledání Předchozího záznamu bez dialogu."""
LookupForm.COMMAND_SEARCH_NEXT = Command('lookup-form.search')
"""Hledání dalšího záznamu bez dialogu."""
LookupForm.COMMAND_SORT_COLUMN = Command('lookup-form.sort-column')
"""Setřídění podle sloupce."""

ListForm.COMMAND_ACTIVATE = Command('list-form.activate')
"""Vyvolání aktivační funkce pro řádek řádkového formuláře."""
ListForm.COMMAND_ACTIVATE_ALTERNATE = Command('list-form.activate-alternate')
"""Vyvolání alternativní aktivační funkce pro řádek řádkového formuláře."""
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
ListForm.COMMAND_EDIT = Command('list-form.edit')
"""Vyvolání inline editace aktuální buňky."""
ListForm.COMMAND_COPY_CELL = Command('list-form.copy-cell')
"""Zkopírování obsahu aktuální buňky do clipboardu."""
ListForm.COMMAND_FILTER_BY_CELL = Command('list-form.filter-by-cell')
"""Vyfiltrování formuláře podle hodnoty aktuální buňky."""
ListForm.COMMAND_EXPORT_CSV = Command('list-form.export-csv')
"""Export řádkového formuláře do csv souboru."""
ListForm.COMMAND_LINE_COMMIT = Command('list-form.line-commit')
"""Dokončení editace záznamu (uložení)."""
ListForm.COMMAND_LINE_ROLLBACK = Command('list-form.line-rollback')
"""Kompletní zrušení editace záznamu."""
ListForm.COMMAND_LINE_SOFT_ROLLBACK = Command('list-form.line-soft-rollback')
"""Kompletní zrušení editace zatím nezměněného záznamu."""
ListForm.COMMAND_FINISH_EDITING = Command('list-form.finish-editing')
"""Opuštění editace řádku."""
ListForm.COMMAND_LINE_DELETE = Command('list-form.line-delete')
"""Smazání aktuálního záznamu."""
ListForm.COMMAND_CELL_COMMIT = Command('list-form.cell-commit')
"""Ukončení editace políčka s novou hodnotou."""
ListForm.COMMAND_CELL_ROLLBACK = Command('list-form.cell-rollback')
"""Ukončení editace políčka s vrácením původní hodnoty."""
ListForm.COMMAND_NEW_LINE_AFTER = Command('list-form.new-line-after')
"""Inline vložení nového záznamu za aktuální řádek."""
ListForm.COMMAND_NEW_LINE_AFTER_COPY = Command('list-form.new-line-after-copy')
"""Inline vložení nového záznamu za aktuální řádek jako jeho kopie."""
ListForm.COMMAND_NEW_LINE_BEFORE = Command('list-form.new-line-before')
"""Inline vložení nového záznamu před aktuální řádek."""
ListForm.COMMAND_NEW_LINE_BEFORE_COPY = Command('list-form.new-line-before-copy')
"""Inline vložení nového záznamu před aktuální řádek jako jeho kopie."""
ListForm.COMMAND_SET_GROUPING_COLUMN = Command('list-form.set-grouping-column')
"""Změna sloupce vizuáního seskupování (vyžaduje argument 'column_id')."""

BrowseForm.COMMAND_NEW_RECORD = Command('browse-form.new-record')
"""Formulářová editace nového záznamu v řádkovém formuláři."""
BrowseForm.COMMAND_NEW_RECORD_COPY = Command('browse-form.new-record-copy')
"""Formulářová editace nového záznamu jako kopie aktuálního záznamu."""
BrowseForm.COMMAND_RECORD_EDIT = Command('browse-form.edit-record')
"""Editace aktuálního záznamu v popup formuláři."""

EditForm.COMMAND_RECORD_DELETE = Command('edit-form.record-delete')
"""Vymazání editovaného záznamu z databáze."""
EditForm.COMMAND_RECORD_INSERT = Command('edit-form.record-insert')
"""Vložení nového záznamu z editačního formuláře."""
EditForm.COMMAND_RECORD_UPDATE = Command('edit-form.record-update')
"""Uložení editovaného záznamu v editačním formuláři."""
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

InputField.COMMAND_RESET_FIELD = Command('input-field.command-reset-field')
"""Vrácení původní hodnoty vstupního políčka."""
InputField.COMMAND_COMMIT_FIELD = Command('input-field.command-commit-field')
"""Úspěšné ukončení editace vstupního políčka."""
InputField.COMMAND_LEAVE_FIELD = Command('input-field.command-leave-field')
"""Odchod z editace vstupního políčka."""
Invocable.COMMAND_INVOKE_SELECTION = Command('invocable.invoke-selection')
"""Vyvolání výběru hodnoty vstupního políčka."""
Invocable.COMMAND_INVOKE_SELECTION_ALTERNATE = \
    Command('invocable.invoke-selection-alternate')
"""Vyvolání alternativního způsobu výběru hodnoty vstupního políčka."""
ListField.COMMAND_INVOKE_EDIT_FORM = Command('list-field.invoke-edit-form')
"""Vyvolání editačního formuláře nad aktuálním záznamem 'ListField'."""
ListField.COMMAND_INVOKE_BROWSE_FORM = Command('list-field.invoke-browse-form')
"""Zobrazení aktuálního záznamu 'ListField' ve formuláři 'BrowseForm'."""
ListField.COMMAND_CHOOSE_KEY = Command('list-field.choose-key')
"""Výběr návratového sloupce a hodnoty pro 'ListField'."""

Dialog.COMMAND_CLOSE_DIALOG = Command('dialog.close-dialog')
"""Opuštění dialogu bez potvrzení."""
Dialog.COMMAND_COMMIT_DIALOG = Command('dialog.commit-dialog')
"""Odeslání dialogu stejně jako stiskem výchozího tlačítka."""

DEFAULT_COMMAND_KEYS = (
    (Application.COMMAND_BREAK,                   'Ctrl-g'),
    (Application.COMMAND_LEAVE_FORM,              'Escape'),
    (Application.COMMAND_NEXT_FORM,               'Ctrl-Up'),
    (Application.COMMAND_PREV_FORM,               'Ctrl-Down'),
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
    (BrowseForm.COMMAND_RECORD_EDIT,              'F5'),
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
