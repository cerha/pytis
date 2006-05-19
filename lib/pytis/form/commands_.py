# -*- coding: iso-8859-2 -*-

# Definice uživatelských příkazů
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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
Command(Form, 'LEAVE_FORM',
        "Uzavření formuláře")
Command(Form, 'PRINT',
        "Tisk aktuálního obsahu formuláře")
Command(Form, 'HELP',
        "Zobrazení nápovědy formuláře")
Command(Form, 'RELOAD_FORM_STATE',
        "Zapomeň uživ. nastavení formuláře a vrať posledně uložené hodnoty")
Command(LookupForm, 'FILTER',
        "Filtrování záznamů")
Command(LookupForm, 'JUMP',
        "Skok na záznam")
Command(LookupForm, 'SEARCH',
        "Hledání záznamu")
Command(LookupForm, 'SORT_COLUMN',
        "Setřídění podle sloupce")
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
Command(ListForm, 'SHOW_CELL_CODEBOOK',
        "Vyvolání číselníku aktivní buňky řádkového formuláře")
Command(ListForm, 'SELECT_CELL',
        "Výběr buňky seznamu")
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
Command(ListForm, 'EXPORT_CSV',
        "Export řádkového formuláře do csv souboru")
Command(ListForm, 'LINE_COMMIT',
        "Dokončení editace záznamu (uložení)")
Command(ListForm, 'LINE_ROLLBACK',
        "Kompletní zrušení editace záznamu") # arg. 'soft'
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
Command(ListForm, 'RESET_COLUMNS',
        "Vrácení výchozího nastavení sloupců")
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
        "Přechod na další záznam")
Command(BrowsableShowForm, 'PREVIOUS_RECORD',
        "Přechod na předchozí záznam")
Command(BrowsableShowForm, 'FIRST_RECORD',
        "Přechod na první záznam")
Command(BrowsableShowForm, 'LAST_RECORD',
        "Přechod na poslední záznam")
Command(DualForm, 'OTHER_FORM',
        "Přechod mezi podformuláři duálního formuláře")
Command(PrintForm, 'NEXT_PAGE',
        "Přechod na další stránku tiskového náhledu")
Command(PrintForm, 'PREVIOUS_PAGE',
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
    ('Ctrl-Backspace',InnerForm.COMMAND_RELOAD_FORM_STATE),
    ('F6',            RecordForm.COMMAND_NEW_RECORD),
    ('Ctrl-F6',       RecordForm.COMMAND_NEW_RECORD(copy=True)),
    ('Alt-F6',        RecordForm.COMMAND_IMPORT_INTERACTIVE),
    ('F5',            RecordForm.COMMAND_EDIT_RECORD),
    ('F8',            RecordForm.COMMAND_DELETE_RECORD),
    ('F4',            LookupForm.COMMAND_SORT_COLUMN),
    ('Ctrl-F4',       LookupForm.COMMAND_FILTER),
    ('F3',            LookupForm.COMMAND_SEARCH),
    ('Ctrl-s',        LookupForm.COMMAND_SEARCH(direction=pytis.data.FORWARD)),
    ('Ctrl-r',        LookupForm.COMMAND_SEARCH(direction=pytis.data.BACKWARD)),
    ('Ctrl-j',        LookupForm.COMMAND_JUMP),
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
    ('Ctrl-m',        ListForm.COMMAND_CONTEXT_MENU),
    ('Ctrl-Enter',    EditForm.COMMAND_COMMIT_RECORD),
    ('Tab',           EditForm.COMMAND_NAVIGATE),
    ('Shift-Tab',     EditForm.COMMAND_NAVIGATE(back=True)),
    ('Next',          BrowsableShowForm.COMMAND_NEXT_RECORD),
    ('Prior',         BrowsableShowForm.COMMAND_PREVIOUS_RECORD),
    ('Home',          BrowsableShowForm.COMMAND_FIRST_RECORD),
    ('End',           BrowsableShowForm.COMMAND_LAST_RECORD),
    ('Ctrl-Tab',      DualForm.COMMAND_OTHER_FORM),
    ('Next',          PrintForm.COMMAND_NEXT_PAGE),
    ('Prior',         PrintForm.COMMAND_PREVIOUS_PAGE),
    ('Ctrl-Backspace',InputField.COMMAND_RESET),
    ('Ctrl-m',        InputField.COMMAND_CONTEXT_MENU),
    ('Ctrl-x',        TextField.COMMAND_CUT),
    ('Ctrl-c',        TextField.COMMAND_COPY),
    ('Ctrl-v',        TextField.COMMAND_PASTE),
    ('F2',            Invocable.COMMAND_INVOKE_SELECTION),
    ('Ctrl-F2',       Invocable.COMMAND_INVOKE_SELECTION(alternate=True)),
    ('Backspace',     ListField.COMMAND_SHOW_SELECTED),
    ('Escape',        Dialog.COMMAND_CLOSE_DIALOG),
    ('Enter',         Dialog.COMMAND_COMMIT_DIALOG),
    ('Ctrl-Enter',    Dialog.COMMAND_COMMIT_DIALOG(force=True)),
    )



if __debug__:
    Command(Application, 'CUSTOM_DEBUG',
            "Pomocný příkaz pro vyvolání pomocné ladící funkce")
    DEFAULT_KEYMAP += \
        (('Ctrl-d', Application.COMMAND_CUSTOM_DEBUG),)


FORM_COMMAND_MENU = ((
    (_("Předchozí okno"),
     _("Přepnout na předchozí okno v pořadí seznamu oken."),
     Application.COMMAND_RAISE_PREV_FORM),
    (_("Následující okno"),
     _("Přepnout na následující okno v pořadí seznamu oken."),
     Application.COMMAND_RAISE_NEXT_FORM),
    (_("Posledně aktivní okno"),
     _("Umožňuje cyklicky přepínat mezi dvěma posledně aktivními okny."),
     Application.COMMAND_RAISE_RECENT_FORM),
    (_("Uzavřít aktuální okno"),
     _("Uzavřít okno aktuálního formuláře."),
     Form.COMMAND_LEAVE_FORM),
    ),(#---------------
    (_("Skok na záznam"),
     _("Skok na záznam podle čísla řádku."),
     LookupForm.COMMAND_JUMP),
    (_("Hledat"),
     _("Vyhledávat záznam podle zadaných podmínek."),
     LookupForm.COMMAND_SEARCH),
    (_("Hledat další"),
     _("Vyhledat další záznam odpovídající zadaným podmínkám."),
     LookupForm.COMMAND_SEARCH(direction=pytis.data.FORWARD)),
    (_("Hledat předchozí"),
     _("Vyhledat předchozí záznam odpovídající zadaným podmínkám."),
     LookupForm.COMMAND_SEARCH(direction=pytis.data.BACKWARD)),
    (_("Inkrementální hledání"),
     _("Postupně vyhledávat záznam podle hodnoty políčka."),
      ListForm.COMMAND_INCREMENTAL_SEARCH),
    (_("Inkrementální hledání podřetězce"),
     _("Postupně vyhledávat záznam podle části hodnoty políčka."),
     ListForm.COMMAND_INCREMENTAL_SEARCH(full=True)),
    ),(#---------------
    (_("Třídění"),
     _("Určit podmínky řazení záznamů."),
     LookupForm.COMMAND_SORT_COLUMN),
    (_("Filtrování"),
     _("Filtrovat záznamy podle zadaných podmínek."),
     LookupForm.COMMAND_FILTER),
    ),(#---------------
    (_("Nový záznam"),
     _("Vložit nový záznam."),
     BrowseForm.COMMAND_NEW_RECORD),
    (_("Nový záznam - kopie"),
     _("Vložit nový záznam zkopírováním hodnot aktuálního záznamu."),
     BrowseForm.COMMAND_NEW_RECORD(copy=True)),
    (_("Editovat záznam"),
     _("Upravit hodnoty záznamu v samostatném formuláři."),
     BrowseForm.COMMAND_EDIT_RECORD),
    (_("Vložit řádku pod"),
     _("Vložit nový záznam v režimu inline editace."),
     ListForm.COMMAND_INSERT_LINE()),
    (_("Vložit řádku nad"),
     _("Vložit nový záznam v režimu inline editace."),
     ListForm.COMMAND_INSERT_LINE(before=True)),
    (_("Kopírovat řádku pod"),
     _("Vložit nový záznam v režimu inline editace jako kopii současného."),
     ListForm.COMMAND_INSERT_LINE(copy=True)),
    (_("Kopírovat řádku nad"),
     _("Vložit nový záznam v režimu inline editace jako kopii současného."),
     ListForm.COMMAND_INSERT_LINE(copy=True, before=True)),
    (_("Editace buňky"),
     _("Upravit hodnotu aktivní buňky v režimu inline editace."),
     ListForm.COMMAND_EDIT),
    (_("Smazat záznam"),
     _("Smazat aktuální záznam."),
     RecordForm.COMMAND_DELETE_RECORD),
    ),(#---------------
    (_("Uložit"),
     _("Uložit změny v právě editovaném záznamu."),
     ListForm.COMMAND_LINE_COMMIT),
    (_("Zrušit změny"),
     _("Zrušir změny v právě editovaném záznamu (návrat původních hodnot)."),
     ListForm.COMMAND_LINE_ROLLBACK),
    ),(#---------------
    (_("Export do textového souboru"),
     _("Exportovat data do textového souboru ve formátu CSV."),
     ListForm.COMMAND_EXPORT_CSV),
    ),(#---------------
    (_("Zobrazit náhled záznamu"),
     _("Zobrazit záznam v náhledovém formuláři."),
     ListForm.COMMAND_ACTIVATE),
    (_("Náhled v duálním formuláři"),
     _("Zobrazit záznam v duálním náhledovém formuláři."),
     ListForm.COMMAND_ACTIVATE(alternate=True)),
    ))

WX_COMMAND_ICONS = {Application.COMMAND_HELP: wx.ART_HELP,
                    Application.COMMAND_EXIT: wx.ART_QUIT,
                    Form.COMMAND_PRINT:       wx.ART_PRINT,
                    InputField.COMMAND_RESET: wx.ART_UNDO,
                    TextField.COMMAND_CUT:    wx.ART_CUT,
                    TextField.COMMAND_COPY:   wx.ART_COPY,
                    TextField.COMMAND_PASTE:  wx.ART_PASTE,
                    }
