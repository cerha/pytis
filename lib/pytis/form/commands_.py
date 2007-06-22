# -*- coding: iso-8859-2 -*-

# Definice uživatelských příkazů
# 
# Copyright (C) 2002-2006, 2007 Brailcom, o.p.s.
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
        "Ukončení aplikace")
Command(Application, 'HELP',
        "Vyvolání nápovědy")
Command(Application, 'BREAK',
        "Přerušení aktuálně prováděné operace")
Command(Application, 'REFRESH',
        "Vyžádání obnovení obsahu aktivního formuláře")
Command(Application, 'NEW_RECORD',
        "Vložení nového záznamu")
Command(Application, 'RUN_FORM',
        "Spuštění formuláře")
Command(Application, 'RUN_PROCEDURE',
        "Spuštění procedury")
Command(Application, 'RAISE_FORM',
        "Vyzvednutí okna formuláře v okně aplikace")
Command(Application, 'RAISE_PREV_FORM',
        "Vyzvednutí okna předchozího formuláře")
Command(Application, 'RAISE_NEXT_FORM',
        "Vyzvednutí okna následujícího formuláře")
Command(Application, 'RAISE_RECENT_FORM',
        "Vyzvednutí okna posledně aktivního formuláře")
Command(Application, 'CLEAR_RECENT_FORMS',
        "Vyčisti menu posledně otevřených formulářů")
Command(Application, 'HANDLED_ACTION',
        "Proveď uživatelem definovanou akci.") # arg. 'handler', 'enabled'
Command(Application, 'NOTHING',
        "Falešný příkaz, který nic nedělá.") # arg. 'enabled'
Command(Form, 'LEAVE_FORM',
        "Uzavření formuláře")
Command(Form, 'HELP',
        "Zobrazení nápovědy formuláře")
Command(InnerForm, 'PRINT',
        "Tisk aktuálního obsahu formuláře")
Command(InnerForm, 'RELOAD_FORM_STATE',
        "Zapomeň uživ. nastavení formuláře a vrať posledně uložené hodnoty")
Command(InnerForm, 'RESET_FORM_STATE',
        "Zahoď uložené uživ. nastavení formuláře a vrať výchozí nastavení")
Command(LookupForm, 'FILTER',
        "Filtrování záznamů")
Command(LookupForm, 'UNFILTER',
        "Zrušení filtrování záznamů")
Command(LookupForm, 'FILTER_BY_VALUE',
        "Vyfiltrování formuláře podle dané hodnoty") # arg. 'column_id', 'value'
Command(LookupForm, 'JUMP',
        "Skok na záznam")
Command(LookupForm, 'SEARCH',
        "Hledání záznamu")
Command(LookupForm, 'SORT',
        "Setřídění podle sloupce")
Command(LookupForm, 'FIRST_RECORD',
        "Skok na první záznam")
Command(LookupForm, 'LAST_RECORD',
        "Skok na poslední záznam")
Command(RecordForm, 'NEW_RECORD',
        "Vložení nového záznamu pomocí editačního formuláře")
Command(RecordForm, 'IMPORT_INTERACTIVE',
        "Interaktivní import dat z CSV souboru")
Command(RecordForm, 'EDIT_RECORD',
        "Editace aktuálního záznamu v editačním formuláři")
Command(RecordForm, 'DELETE_RECORD',
        "Vymazání editovaného záznamu z databáze")
Command(ListForm, 'ACTIVATE',
        "Aktivační funkce pro aktuální řádek formuláře") # arg. 'alternate'
Command(ListForm, 'FIRST_COLUMN',
        "Přechod na první sloupec tabulky")
Command(ListForm, 'LAST_COLUMN',
        "Přechod na poslední sloupec tabulky")
Command(ListForm, 'INCREMENTAL_SEARCH',
        "Prefixové inkrementální hledání záznamu") # arg. 'full'
Command(ListForm, 'EDIT',
        "Vyvolání inline editace aktuální buňky")
Command(ListForm, 'COPY_CELL',
        "Zkopírování obsahu aktuální buňky do clipboardu")
Command(ListForm, 'FILTER_BY_CELL',
        "Vyfiltrování formuláře podle hodnoty aktuální buňky")
Command(ListForm, 'AUTOFILTER',
        "Zobrazení menu autofilteru") # arg 'col' a 'position'
Command(ListForm, 'TOGGLE_AGGREGATION',
        "Přidání/odebrání agregační funkce") # arg 'operation'
Command(ListForm, 'AGGREGATE',
        "Přidání agregační funkce") # arg 'operation'
Command(ListForm, 'UNAGGREGATE',
        "Odebrání agregační funkce") # arg 'operation'
Command(ListForm, 'EXPORT_CSV',
        "Export řádkového formuláře do csv souboru")
Command(ListForm, 'LINE_COMMIT',
        "Dokončení editace záznamu (uložení)")
Command(ListForm, 'LINE_ROLLBACK',
        "Zrušení změn v editovaném záznamu") # arg. 'soft'
Command(ListForm, 'FINISH_EDITING',
        "Opuštění editace řádku")
Command(ListForm, 'CELL_COMMIT',
        "Ukončení editace políčka s novou hodnotou")
Command(ListForm, 'CELL_ROLLBACK',
        "Ukončení editace políčka s vrácením původní hodnoty")
Command(ListForm, 'INSERT_LINE',
        "In-line vložení nového záznamu") # arg. 'before' a 'copy'
Command(ListForm, 'SET_GROUPING_COLUMN',
        "Změna sloupce vizuáního seskupování")
Command(ListForm, 'RESIZE_COLUMN',
        "Rozšíření/zůžení sloupce") # arg. 'diff' udává počet +/- pixelů
Command(ListForm, 'MOVE_COLUMN',
        "Přesunutí sloupce doprava/doleva") # arg. 'diff' +/- o kolik sloupců
Command(ListForm, 'TOGGLE_COLUMN',
        "Skrytí/zobrazení sloupce")
Command(ListForm, 'TOGGLE_ROW_LABELS',
        "Skrytí/zobrazení sloupce záhlaví řádků")
Command(ListForm, 'CONTEXT_ACTION',
        "Vyvolání akce nad aktuálním řádkem formuláře")
        # Povinný argument 'action' je instancí specifikacní třídy 'Action'.
Command(ListForm, 'CONTEXT_MENU',
        "Zobrazení kontextového menu aktivní buňky")
Command(EditForm, 'COMMIT_RECORD',
        "Ukončení editačního formuláře s uložením změn")
Command(EditForm, 'NAVIGATE',
        "Navigace mezi políčky editačního formuláře") # arg. 'back'
Command(BrowsableShowForm, 'NEXT_RECORD',
        "Přechod na další záznam vpřed/vzad") # arg. 'back'
Command(DualForm, 'OTHER_FORM',
        "Přechod mezi podformuláři duálního formuláře")
Command(PrintFormInternal, 'NEXT_PAGE',
        "Přechod na další stránku tiskového náhledu")
Command(PrintFormInternal, 'PREVIOUS_PAGE',
        "Přechod na předchozí stránku tiskového náhledu")
Command(InputField, 'RESET',
        "Vrácení původní hodnoty vstupního políčka")
Command(InputField, 'CONTEXT_MENU',
        "Zobrazení kontextového menu vstupního políčka")
Command(InputField, 'LEAVE_INLINE_EDIT',
        "Odchod z editace vstupního políčka")
Command(TextField, 'CUT',
        "Vyjmutí označeného textu do schránky.")
Command(TextField, 'COPY',
        "Zkopírování označeného textu do schránky.")
Command(TextField, 'PASTE',
        "Vložení textu ze schránky do políčka.")
Command(TextField, 'SELECT_ALL',
        "Provedení výběru celého textu políčka.")
Command(Invocable, 'INVOKE_SELECTION',
        "Vyvolání výběru hodnoty vstupního políčka") # arg. 'alternate'
Command(GenericCodebookField, 'INVOKE_CODEBOOK_FORM',
        "Vyvolání alternativního výběru hodnoty políčka")
Command(ListField, 'SELECT',
        "Výběr aktuálního záznamu")
Command(ListField, 'SHOW_SELECTED',
        "Výběr aktuálního záznamu")
Command(ListField, 'INVOKE_EDIT_FORM',
        "Vyvolání editačního formuláře nad akt. záznamem")
Command(FileField, 'LOAD',
        "Nahrání soubodu jako nové hodnoty políčka.")
Command(FileField, 'SAVE',
        "Uložení objektu z databáze do soborového systému.")
Command(FileField, 'CLEAR',
        "Vynulování nastavené hodnoty políčka.")
Command(ImageField, 'VIEW',
        "Otevření náhledu obrázku v prohlížeči.")

Command(Dialog, 'CLOSE_DIALOG',
        "Opuštění dialogu bez potvrzení")
Command(Dialog, 'COMMIT_DIALOG',
        "Potvrzení dialogu") # arg. 'force' simuluje stisk výchozího tlačítka
Command(Dialog, 'HELP',
        "Vyvolání nápovědy dialogu")

DEFAULT_KEYMAP = (
    ('F1',            Application.COMMAND_HELP(topic='pytis')),
    ('Ctrl-g',        Application.COMMAND_BREAK),
    ('Ctrl-Down',     Application.COMMAND_RAISE_NEXT_FORM),
    ('Ctrl-Up',       Application.COMMAND_RAISE_PREV_FORM),
    ('Ctrl-w',        Application.COMMAND_RAISE_RECENT_FORM),
    ('Ctrl-l',        Application.COMMAND_REFRESH),
    ('Ctrl-F1',       Form.COMMAND_HELP),
    ('Escape',        Form.COMMAND_LEAVE_FORM),
    ('Ctrl-p',        InnerForm.COMMAND_PRINT),
    ('Ctrl-Backspace',       InnerForm.COMMAND_RELOAD_FORM_STATE),
    ('Ctrl-Shift-Backspace', InnerForm.COMMAND_RESET_FORM_STATE),
    ('F6',            RecordForm.COMMAND_NEW_RECORD),
    ('Ctrl-F6',       RecordForm.COMMAND_NEW_RECORD(copy=True)),
    ('Alt-F6',        RecordForm.COMMAND_IMPORT_INTERACTIVE),
    ('F5',            RecordForm.COMMAND_EDIT_RECORD),
    ('F8',            RecordForm.COMMAND_DELETE_RECORD),
    ('Ctrl-f',        LookupForm.COMMAND_FILTER),
    ('Ctrl-Alt-f',    LookupForm.COMMAND_FILTER(last=True)),
    ('Ctrl-Shift-f',  LookupForm.COMMAND_UNFILTER),
    ('F4',            LookupForm.COMMAND_SORT),
    ('F3',            LookupForm.COMMAND_SEARCH),
    ('Ctrl-s',        LookupForm.COMMAND_SEARCH(next=True)),
    ('Ctrl-r',        LookupForm.COMMAND_SEARCH(next=True, back=True)),
    ('Ctrl-j',        LookupForm.COMMAND_JUMP),
    ('Ctrl-Home',     LookupForm.COMMAND_FIRST_RECORD),
    ('Ctrl-Prior',    LookupForm.COMMAND_FIRST_RECORD),
    ('Ctrl-End',      LookupForm.COMMAND_LAST_RECORD),
    ('Ctrl-Next',     LookupForm.COMMAND_LAST_RECORD),
    ('Ctrl-F3',       ListForm.COMMAND_INCREMENTAL_SEARCH),
    ('Alt-F3',        ListForm.COMMAND_INCREMENTAL_SEARCH(full=True)),
    ('Enter',         ListForm.COMMAND_ACTIVATE),
    (' ',             ListForm.COMMAND_ACTIVATE(alternate=True)),
    ('Ctrl-c',        ListForm.COMMAND_COPY_CELL),
    ('Home',          ListForm.COMMAND_FIRST_COLUMN),
    ('End',           ListForm.COMMAND_LAST_COLUMN),
    ('Ctrl-e',        ListForm.COMMAND_EXPORT_CSV),
    ('F2',            ListForm.COMMAND_EDIT),
    ('F9',            ListForm.COMMAND_EDIT),
    ('Ctrl-F12',      ListForm.COMMAND_LINE_ROLLBACK),
    ('Escape',        ListForm.COMMAND_FINISH_EDITING),
    ('F12',           ListForm.COMMAND_LINE_COMMIT),
    ('Enter',         ListForm.COMMAND_CELL_COMMIT),
    ('Escape',        ListForm.COMMAND_CELL_ROLLBACK),
    ('Insert',        ListForm.COMMAND_INSERT_LINE()),
    ('F7',            ListForm.COMMAND_INSERT_LINE(copy=True)),
    ('Ctrl-Insert',   ListForm.COMMAND_INSERT_LINE(before=True)),
    ('Ctrl-F7',       ListForm.COMMAND_INSERT_LINE(before=True, copy=True)),
    ('Alt-Right',     ListForm.COMMAND_RESIZE_COLUMN(diff=+5)),
    ('Alt-Left',      ListForm.COMMAND_RESIZE_COLUMN(diff=-5)),
    ('Shift-Right',   ListForm.COMMAND_MOVE_COLUMN(diff=+1)),
    ('Shift-Left',    ListForm.COMMAND_MOVE_COLUMN(diff=-1)),
    ('Alt-Down',      ListForm.COMMAND_CONTEXT_MENU),
    ('Alt-Enter',     ListForm.COMMAND_CONTEXT_MENU),
    ('F11',           ListForm.COMMAND_TOGGLE_ROW_LABELS),
    ('Ctrl-Enter',    EditForm.COMMAND_COMMIT_RECORD),
    ('Tab',           EditForm.COMMAND_NAVIGATE),
    ('Shift-Tab',     EditForm.COMMAND_NAVIGATE(back=True)),
    ('Next',          BrowsableShowForm.COMMAND_NEXT_RECORD),
    ('Prior',         BrowsableShowForm.COMMAND_NEXT_RECORD(back=True)),
    ('Ctrl-Tab',      DualForm.COMMAND_OTHER_FORM),
    ('Next',          PrintFormInternal.COMMAND_NEXT_PAGE),
    ('Prior',         PrintFormInternal.COMMAND_PREVIOUS_PAGE),
    ('Ctrl-Backspace',InputField.COMMAND_RESET),
    ('Alt-Down',      InputField.COMMAND_CONTEXT_MENU),
    ('Alt-Enter',     InputField.COMMAND_CONTEXT_MENU),
    ('Ctrl-x',        TextField.COMMAND_CUT),
    ('Ctrl-c',        TextField.COMMAND_COPY),
    ('Ctrl-v',        TextField.COMMAND_PASTE),
    ('F2',            Invocable.COMMAND_INVOKE_SELECTION),
    ('Ctrl-F2',       Invocable.COMMAND_INVOKE_SELECTION(alternate=True)),
    ('Backspace',     FileField.COMMAND_CLEAR),
    ('Delete',        FileField.COMMAND_CLEAR),
    ('Backspace',     ListField.COMMAND_SHOW_SELECTED),
    ('Escape',        Dialog.COMMAND_CLOSE_DIALOG),
    ('Enter',         Dialog.COMMAND_COMMIT_DIALOG),
    ('Ctrl-Enter',    Dialog.COMMAND_COMMIT_DIALOG(force=True)),

    # Just for backwards compatibility
    ('Ctrl-F4',       LookupForm.COMMAND_FILTER),

    )



if __debug__:
    Command(Application, 'CUSTOM_DEBUG',
            "Pomocný příkaz pro vyvolání pomocné ladící funkce")
    DEFAULT_KEYMAP += \
        (('Ctrl-d', Application.COMMAND_CUSTOM_DEBUG),)


FORM_COMMAND_MENU = ((
    (LookupForm.COMMAND_JUMP,
     _("Skok na záznam"),
     _("Skok na záznam podle čísla řádku.")),
    (LookupForm.COMMAND_SEARCH,
     _("Hledat"),
     _("Vyhledávat záznam podle zadaných podmínek.")),
    (LookupForm.COMMAND_SEARCH(next=True),
     _("Hledat další"),
     _("Vyhledat další záznam odpovídající zadaným podmínkám.")),
    (LookupForm.COMMAND_SEARCH(next=True, back=True),
     _("Hledat předchozí"),
     _("Vyhledat předchozí záznam odpovídající zadaným podmínkám.")),
    (ListForm.COMMAND_INCREMENTAL_SEARCH,
     _("Inkrementální hledání"),
     _("Postupně vyhledávat záznam podle hodnoty políčka.")),
    (ListForm.COMMAND_INCREMENTAL_SEARCH(full=True),
     _("Inkrementální hledání podřetězce"),
     _("Postupně vyhledávat záznam podle části hodnoty políčka.")),
    ),(#---------------
    (LookupForm.COMMAND_SORT,
     _("Řazení"),
     _("Určit podmínky řazení záznamů.")),
    (LookupForm.COMMAND_FILTER,
     _("Filtrování"),
     _("Filtrovat záznamy podle zadaných podmínek.")),
    (LookupForm.COMMAND_UNFILTER,
     _("Zruš filtr"),
     _("Zrušit filtrování záznamů.")),
    ),(#---------------
    (RecordForm.COMMAND_NEW_RECORD,
     _("Nový záznam"),
     _("Vložit nový záznam v samostatném formuláři.")),
    (RecordForm.COMMAND_NEW_RECORD(copy=True),
     _("Nový záznam - kopie"),
     _("Vložit nový záznam zkopírováním hodnot aktuálního záznamu.")),
    (RecordForm.COMMAND_EDIT_RECORD,
     _("Editovat záznam"),
     _("Upravit hodnoty záznamu v samostatném formuláři.")),
    (RecordForm.COMMAND_DELETE_RECORD,
     _("Smazat záznam"),
     _("Smazat aktuální záznam.")),
    ),(#---------------
    (ListForm.COMMAND_INSERT_LINE,
     _("Vložit řádek pod"),
     _("Vložit nový záznam v režimu inline editace.")),
    (ListForm.COMMAND_INSERT_LINE(before=True),
     _("Vložit řádek nad"),
     _("Vložit nový záznam v režimu inline editace.")),
    (ListForm.COMMAND_INSERT_LINE(copy=True),
     _("Kopírovat řádek pod"),
     _("Vložit nový záznam v režimu inline editace jako kopii současného.")),
    (ListForm.COMMAND_INSERT_LINE(copy=True, before=True),
     _("Kopírovat řádek nad"),
     _("Vložit nový záznam v režimu inline editace jako kopii současného.")),
    ),(#---------------
    (ListForm.COMMAND_EXPORT_CSV,
     _("Export do textového souboru"),
     _("Exportovat data do textového souboru ve formátu CSV.")),
    (RecordForm.COMMAND_IMPORT_INTERACTIVE,
     _("Import z textového souboru"),
     _("Importovat data z textového souboru ve formátu CSV.")),
    ),(#---------------
    (InnerForm.COMMAND_RELOAD_FORM_STATE,
     _("Vrátit předchozí nastavení formuláře"),
     _("Zahodit změny nastavení formuláře provedené během tohoto spuštění "
       "aplikace")),
    (InnerForm.COMMAND_RESET_FORM_STATE,
     _("Vrátit výchozí nastavení formuláře"),
     _("Zahodit veškeré uživatelské nastavení formuláře")),
     ))

_ASC = LookupForm.SORTING_ASCENDENT
_DESC = LookupForm.SORTING_DESCENDANT
_NONE = LookupForm.SORTING_NONE

COMMAND_ICONS = (
    (Application.COMMAND_HELP,                             wx.ART_HELP),
    (Application.COMMAND_EXIT,                             wx.ART_QUIT),
    (Application.COMMAND_RAISE_NEXT_FORM,                  'next-form'),
    (Application.COMMAND_RAISE_PREV_FORM,                  'prev-form'),
    (Application.COMMAND_RAISE_RECENT_FORM,                'recent-form'),
    #(Application.COMMAND_NEW_RECORD,                      'new-record'),
    #(Application.COMMAND_RUN_FORM,                        'run-form'),
    (Form.COMMAND_HELP,                                    'help-book'),
    (Form.COMMAND_LEAVE_FORM,                              'close'),
    (InnerForm.COMMAND_PRINT,                              wx.ART_PRINT),
    (InputField.COMMAND_RESET,                             wx.ART_UNDO),
    (TextField.COMMAND_CUT,                                wx.ART_CUT),
    (TextField.COMMAND_COPY,                               wx.ART_COPY),
    (TextField.COMMAND_PASTE,                              wx.ART_PASTE),
    (FileField.COMMAND_LOAD,                               wx.ART_FILE_OPEN),
    (FileField.COMMAND_SAVE,                               wx.ART_FILE_SAVE),
    (FileField.COMMAND_CLEAR,                              wx.ART_DELETE),
    (ListForm.COMMAND_COPY_CELL,                           wx.ART_COPY),
    (ListForm.COMMAND_EDIT,                                'edit-cell'),
    (ListForm.COMMAND_FILTER_BY_CELL,                      'filter-by-cell'),
    (ListForm.COMMAND_AUTOFILTER,                          'autofilter'),
    (ListForm.COMMAND_INCREMENTAL_SEARCH,                  'search-incremental'),
    (ListForm.COMMAND_EXPORT_CSV,                          'export'),
    (ListForm.COMMAND_IMPORT_INTERACTIVE,                  'import'),
    (ListForm.COMMAND_INSERT_LINE(before=True, copy=True), 'insert-line-before-copy'),
    (ListForm.COMMAND_INSERT_LINE(before=True),            'insert-line-before'),
    (ListForm.COMMAND_INSERT_LINE(copy=True),              'insert-line-copy'),
    (ListForm.COMMAND_INSERT_LINE,                         'insert-line'),
    (ListForm.COMMAND_TOGGLE_COLUMN(col=None),             'hide-column'),
    (ListForm.COMMAND_AGGREGATE,                           'aggregate'),
    (ListForm.COMMAND_UNAGGREGATE,                         'unaggregate'),
    (RecordForm.COMMAND_EDIT_RECORD,                       'edit-record'),
    (RecordForm.COMMAND_NEW_RECORD(copy=True),             'new-record-copy'),
    (RecordForm.COMMAND_NEW_RECORD,                        'new-record'),
    (RecordForm.COMMAND_DELETE_RECORD,                     'delete-record'),
    (LookupForm.COMMAND_SORT(direction=_ASC),               'sort-asc'),
    (LookupForm.COMMAND_SORT(direction=_DESC),              'sort-desc'),
    (LookupForm.COMMAND_SORT(direction=_NONE),              'unsort'),
    (LookupForm.COMMAND_SORT,                              'sort'),
    (LookupForm.COMMAND_FILTER_BY_VALUE,                   'filter-by-cell'),
    (LookupForm.COMMAND_FILTER(last=True),                 'filter'),
    (LookupForm.COMMAND_FILTER,                            'filter-form'),
    (LookupForm.COMMAND_UNFILTER,                          'unfilter'),
    (LookupForm.COMMAND_SEARCH(next=True, back=True),      'search-backwards'),
    (LookupForm.COMMAND_SEARCH(next=True),                 'search-forward'),
    (LookupForm.COMMAND_SEARCH,                            'search'),
    (LookupForm.COMMAND_JUMP,                              'jump'),
    (InnerForm.COMMAND_RESET_FORM_STATE,                   'reset-form-state'),
    (InnerForm.COMMAND_RELOAD_FORM_STATE,                  'reload-form-state'),
    )
