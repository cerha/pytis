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

Všechny příkazy jsou centrálně definovány zde z důvodu konzistence a
přehlednosti, zejména z pohledu tvůrců definic aplikací.  Pro všechny
definované příkazy jsou automaticky vytvořeny konstanty ve třídě, které je
příkaz určen; názvy těchto konstant začínají prefixem 'COMMAND_' (tuto konvenci
je nutno dodržovat).

Například definice Command(Application, 'EXIT', 'Ukončení aplikace.')  vytvoří
konstantu Application.COMMAND_EXIT a pod tímto názvem je možné s instancí
příkazu dále pracovat.

Přiřazení kláves příkazům:

V aplikaci nikde nepoužíváme přímý zápis kláves, nýbrž pouze jim odpovídající
příkazy.  To nám umožňuje nestarat se v aplikačním kódu o přiřazení kláves.  To
definujeme na jediném místě v tomto modulu.

"""

from pytis.form import *
    
Command(Application, 'EXIT',
        "Ukončení aplikace.")
Command(Application, 'BREAK',
        "Přerušení aktuálně prováděné operace.")
Command(Application, 'REFRESH',
        "Vyžádání obnovení obsahu aktivního formuláře.")
Command(Application, 'NEW_RECORD',
        "Vložení nového záznamu.")
Command(Application, 'RUN_FORM',
        "Spuštění formuláře.")
Command(Application, 'RUN_PROCEDURE',
        "Spuštění procedury.")
Command(Application, 'LEAVE_FORM',
        "Odstranění aktivního okna formuláře z aplikace.")
Command(Application, 'RAISE_FORM',
        "Vyzvednutí okna formuláře v okně aplikace.")
Command(Application, 'PREV_FORM',
        "Vyzvednutí okna předchozího formuláře.")
Command(Application, 'NEXT_FORM',
        "Vyzvednutí okna následujícího formuláře.")
Command(Application, 'SHOW_POPUP_MENU',
        "Zobraz kontextové menu aktivního prvku.")
Command(Form, 'PRINT',
        "Tisk aktuálního obsahu formuláře.")
Command(LookupForm, 'FILTER',
        "Filtrování záznamů.")
Command(LookupForm, 'JUMP',
        "Skok na záznam.")
Command(LookupForm, 'SEARCH',
        "Hledání záznamu.")
Command(LookupForm, 'SEARCH_PREVIOUS',
        "Hledání Předchozího záznamu bez dialogu.")
Command(LookupForm, 'SEARCH_NEXT',
        "Hledání dalšího záznamu bez dialogu.")
Command(LookupForm, 'SORT_COLUMN',
        "Setřídění podle sloupce.")
Command(RecordForm, 'NEW_RECORD',
        "Vložení nového záznamu pomocí editačního formuláře.")
Command(RecordForm, 'NEW_RECORD_COPY',
        "Vložení kopie záznamu pomocí editačního formuláře.")
Command(RecordForm, 'EDIT_RECORD',
        "Editace aktuálního záznamu v editačním formuláři.")
Command(RecordForm, 'DELETE_RECORD',
        "Vymazání editovaného záznamu z databáze.")
Command(ListForm, 'ACTIVATE',
        "Aktivační funkce pro aktuální řádek formuláře.")
Command(ListForm, 'ACTIVATE_ALTERNATE',
        "Alternativní aktivační funkce pro aktuální řádek.")
Command(ListForm, 'SHOW_CELL_CODEBOOK',
        "Vyvolání číselníku aktivní buňky řádkového formuláře.")
Command(ListForm, 'SELECT_CELL',
        "Výběr buňky seznamu.")
Command(ListForm, 'FIRST_COLUMN',
        "Přechod na první sloupec tabulky.")
Command(ListForm, 'LAST_COLUMN',
        "Přechod na poslední sloupec tabulky.")
Command(ListForm, 'INCREMENTAL_SEARCH',
        "Prefixové inkrementální hledání záznamu.")
Command(ListForm, 'FULL_INCREMENTAL_SEARCH',
        "Plné inkrementální hledání záznamu.")
Command(ListForm, 'EDIT',
        "Vyvolání inline editace aktuální buňky.")
Command(ListForm, 'COPY_CELL',
        "Zkopírování obsahu aktuální buňky do clipboardu.")
Command(ListForm, 'FILTER_BY_CELL',
        "Vyfiltrování formuláře podle hodnoty aktuální buňky.")
Command(ListForm, 'EXPORT_CSV',
        "Export řádkového formuláře do csv souboru.")
Command(ListForm, 'LINE_COMMIT',
        "Dokončení editace záznamu (uložení).")
Command(ListForm, 'LINE_ROLLBACK',
        "Kompletní zrušení editace záznamu.")
Command(ListForm, 'LINE_SOFT_ROLLBACK',
        "Kompletní zrušení editace zatím nezměněného záznamu.")
Command(ListForm, 'FINISH_EDITING',
        "Opuštění editace řádku.")
Command(ListForm, 'CELL_COMMIT',
        "Ukončení editace políčka s novou hodnotou.")
Command(ListForm, 'CELL_ROLLBACK',
        "Ukončení editace políčka s vrácením původní hodnoty.")
Command(ListForm, 'NEW_LINE_AFTER',
        "Vložení nového záznamu za aktuální řádek.")
Command(ListForm, 'NEW_LINE_AFTER_COPY',
        "Vložení záznamu za aktuální řádek jako jeho kopie.")
Command(ListForm, 'NEW_LINE_BEFORE',
        "Vložení nového záznamu před aktuální řádek.")
Command(ListForm, 'NEW_LINE_BEFORE_COPY',
        "Vložení záznamu před aktuální řádek jako jeho kopie.")
Command(ListForm, 'SET_GROUPING_COLUMN',
        "Změna sloupce vizuáního seskupování.")
Command(ListForm, 'ENLARGE_COLUMN',
        "Rozšíření sloupce.")
Command(ListForm, 'CONTRACT_COLUMN',
        "Zůžení sloupce.")
Command(ListForm, 'TOGGLE_COLUMN',
        "Skrytí/zobrazení sloupce.")
Command(ListForm, 'RESET_COLUMNS',
        "Vrácení výchozího nastavení sloupců.")
Command(BrowseForm, 'IMPORT_INTERACTIVE',
        "Interaktivní import dat z CSV souboru.")
Command(EditForm, 'COMMIT_RECORD',
        "Ukončení editačního formuláře s uložením změn.")
Command(EditForm, 'NAVIGATE',
        "Navigace mezi políčky editačního formuláře.")
Command(EditForm, 'NAVIGATE_BACK',
        "Zpětná navigace mezi políčky editačního formuláře.")
Command(BrowsableShowForm, 'NEXT_RECORD',
        "Přechod na další záznam.")
Command(BrowsableShowForm, 'PREVIOUS_RECORD',
        "Přechod na předchozí záznam.")
Command(BrowsableShowForm, 'FIRST_RECORD',
        "Přechod na první záznam.")
Command(BrowsableShowForm, 'LAST_RECORD',
        "Přechod na poslední záznam.")
Command(DualForm, 'OTHER_FORM',
        "Přechod mezi podformuláři duálního formuláře.")
Command(PrintForm, 'NEXT_PAGE',
        "Přechod na další stránku tiskového náhledu.")
Command(PrintForm, 'PREVIOUS_PAGE',
        "Přechod na předchozí stránku tiskového náhledu.")
Command(InputField, 'RESET_FIELD',
        "Vrácení původní hodnoty vstupního políčka.")
Command(InputField, 'COMMIT_FIELD',
        "Úspěšné ukončení editace vstupního políčka.")
Command(InputField, 'LEAVE_FIELD',
        "Odchod z editace vstupního políčka.")
Command(Invocable, 'INVOKE_SELECTION',
        "Vyvolání výběru hodnoty vstupního políčka.")
Command(Invocable, 'INVOKE_SELECTION_ALTERNATE',
        "Vyvolání alternativního výběru hodnoty políčka.")
Command(GenericCodebookField, 'INVOKE_CODEBOOK_FORM',
        "Vyvolání alternativního výběru hodnoty políčka.")
Command(ListField, 'SELECT',
        "Výběr aktuálního záznamu.")
Command(ListField, 'SHOW_SELECTED',
        "Výběr aktuálního záznamu.")
Command(ListField, 'INVOKE_EDIT_FORM',
        "Vyvolání editačního formuláře nad akt. záznamem.")
Command(ListField, 'INVOKE_BROWSE_FORM',
        "Zobrazení aktuálního záznamu v novém formuláři.")
Command(Dialog, 'CLOSE_DIALOG',
        "Opuštění dialogu bez potvrzení.")
Command(Dialog, 'COMMIT_DIALOG',
        "Potvrzení dialogu.")
Command(Dialog, 'FORCE_COMMIT_DIALOG',
        "Odeslání dialogu, jako by bylo stisknuto výchozí tlačítko.")


DEFAULT_COMMAND_KEYS = (
    (Application.COMMAND_BREAK,                   'Ctrl-g'),
    (Application.COMMAND_LEAVE_FORM,              'Escape'),
    (Application.COMMAND_NEXT_FORM,               'Ctrl-Down'),
    (Application.COMMAND_PREV_FORM,               'Ctrl-Up'),
    (Application.COMMAND_REFRESH,                 'Ctrl-l'),
    (Application.COMMAND_SHOW_POPUP_MENU,         'Ctrl-M'),
    (Form.COMMAND_PRINT,                         ('Ctrl-x', 'p')),
    (RecordForm.COMMAND_NEW_RECORD,               'F6'),
    (RecordForm.COMMAND_NEW_RECORD_COPY,          'Ctrl-F6'),
    (RecordForm.COMMAND_EDIT_RECORD,              'F5'),
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
    (ListForm.COMMAND_EDIT,                       'F2'),
    (ListForm.COMMAND_EDIT,                       'F9'), # backw. compatibility
    (ListForm.COMMAND_LINE_ROLLBACK,              'Ctrl-F12'),
    (ListForm.COMMAND_FINISH_EDITING,             'Escape'),
    (ListForm.COMMAND_LINE_COMMIT,                'F12'),
    (ListForm.COMMAND_CELL_COMMIT,                'Enter'),
    (ListForm.COMMAND_CELL_ROLLBACK,              'Escape'),
    (ListForm.COMMAND_NEW_LINE_AFTER,             'Insert'),
    (ListForm.COMMAND_NEW_LINE_AFTER_COPY,        'F7'),
    (ListForm.COMMAND_NEW_LINE_BEFORE,            'Ctrl-Insert'),
    (ListForm.COMMAND_NEW_LINE_BEFORE_COPY,       'Ctrl-F7'),
    (ListForm.COMMAND_ENLARGE_COLUMN,             'Alt-Right'),
    (ListForm.COMMAND_CONTRACT_COLUMN,            'Alt-Left'),
    (BrowseForm.COMMAND_IMPORT_INTERACTIVE,       'Alt-F6'),
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
    (ListField.COMMAND_SHOW_SELECTED,             'Backspace'),  
    (Dialog.COMMAND_CLOSE_DIALOG,                 'Escape'),
    (Dialog.COMMAND_COMMIT_DIALOG,                'Enter'),
    (Dialog.COMMAND_FORCE_COMMIT_DIALOG,          'Ctrl-Enter'))



if __debug__:
    Command(Application, 'CUSTOM_DEBUG',
            "Pomocný příkaz pro vyvolání pomocné ladící funkce.")
    DEFAULT_COMMAND_KEYS += \
        ((Application.COMMAND_CUSTOM_DEBUG, 'Ctrl-Backspace'),)


FORM_COMMAND_MENU = ((
    (_("Předchozí okno"),             Application.COMMAND_PREV_FORM),
    (_("Následující okno"),           Application.COMMAND_NEXT_FORM),
    (_("Zavřít aktuální okno"),       Application.COMMAND_LEAVE_FORM),
    ),(#---------------
    (_("Skok na záznam"),             LookupForm.COMMAND_JUMP),
    (_("Hledat"),                     LookupForm.COMMAND_SEARCH),
    (_("Hledat další"),               LookupForm.COMMAND_SEARCH_NEXT),
    (_("Hledat předchozí"),           LookupForm.COMMAND_SEARCH_PREVIOUS),
    (_("Inkrementální hledání"),      ListForm.COMMAND_INCREMENTAL_SEARCH),
    (_("Inkrementální hledání podřetězce"),
                                      ListForm.COMMAND_FULL_INCREMENTAL_SEARCH),
    ),(#---------------
    (_("Třídění"),                    LookupForm.COMMAND_SORT_COLUMN),
    (_("Filtrování"),                 LookupForm.COMMAND_FILTER),
    ),(#---------------
    (_("Nový záznam"),                BrowseForm.COMMAND_NEW_RECORD),
    (_("Nový záznam - kopie"),        BrowseForm.COMMAND_NEW_RECORD_COPY),
    (_("Editovat záznam"),            BrowseForm.COMMAND_EDIT_RECORD),
    (_("Vložit řádku nad"),           ListForm.COMMAND_NEW_LINE_BEFORE),
    (_("Vložit řádku pod"),           ListForm.COMMAND_NEW_LINE_AFTER),
    (_("Kopírovat řádku nad"),        ListForm.COMMAND_NEW_LINE_BEFORE_COPY),
    (_("Kopírovat řádku pod"),        ListForm.COMMAND_NEW_LINE_AFTER_COPY),
    (_("Editace buňky"),              ListForm.COMMAND_EDIT),
    (_("Smazat záznam"),              RecordForm.COMMAND_DELETE_RECORD),
    ),(#---------------
    (_("Uložit"),                     ListForm.COMMAND_LINE_COMMIT),
    (_("Zrušit změny"),               ListForm.COMMAND_LINE_ROLLBACK),
    ),(#---------------
    (_("Export do textového souboru"),ListForm.COMMAND_EXPORT_CSV),
    ),(#---------------
    (_("Zobrazit náhled záznamu"),    ListForm.COMMAND_ACTIVATE),
    (_("Náhled v duálním formuláři"), ListForm.COMMAND_ACTIVATE_ALTERNATE),
    ))
