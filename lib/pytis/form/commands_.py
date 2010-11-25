# -*- coding: iso-8859-2 -*-

# Definice uživatelských příkazů
# 
# Copyright (C) 2002-2006, 2007, 2008, 2009, 2010 Brailcom, o.p.s.
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

"""Definition of available user commands.

All commands are defined centrally in this module for consistency.  This gives us a good overview
of available commands which is especially helpful for application developers.

Class constants named after the command with a 'COMMAND_' prefix are automatically created for all
defined commands.  For example the definition:

  Command(Application, 'EXIT', 'Exit the application.')
  
will automatically create a constant 'Application.COMMAND_EXIT' and this constant may be further
used to invoke the command, etc.

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
        "Perform application defined action (arg. 'handler', 'enabled')")
Command(Application, 'RELOAD_RIGHTS',
        "Reload application rights and menu from the database")
Command(Application, 'NOTHING',
        "Fake command which does nothing (arg. 'enabled')")
Command(BrowseForm, 'PRINT',
        "Print the current form data")
Command(Form, 'LEAVE_FORM',
        "Close the form")
Command(Form, 'SAFE_LEAVE_FORM',
        "Close the form in idle loop")
Command(Form, 'HELP',
        "Show the form help in the help browser")
Command(InnerForm, 'DESCRIBE',
        "Show form description in a eparate window")
Command(InnerForm, 'AGGREGATION_MENU',
        "Show aggregation menu for the current form")
Command(InnerForm, 'PRINT_MENU',
        "Show print menu for the current form")
Command(InnerForm, 'RELOAD_FORM_STATE',
        "Zapomeň uživ. nastavení formuláře a vrať posledně uložené hodnoty")
Command(InnerForm, 'RESET_FORM_STATE',
        "Zahoď uložené uživ. nastavení formuláře a vrať výchozí nastavení")
Command(LookupForm, 'PROFILE_MENU',
        "Show profile menu for the current form")
Command(LookupForm, 'FILTER',
        "Filtrování záznamů")
Command(LookupForm, 'UNFILTER',
        "Zrušení filtrování záznamů")
Command(LookupForm, 'APPLY_PROFILE',
        "Apply given form profile on the current form (arg. 'profile')")
Command(LookupForm, 'FILTER_BY_VALUE',
        "Vyfiltrování formuláře podle dané hodnoty (arg. 'column_id', 'value')")
Command(LookupForm, 'SAVE_PROFILE',
        "Save the current profile as a named user profile."),
Command(LookupForm, 'UPDATE_SAVED_PROFILE',
        "Update an existing named profile by the current setup."),
Command(LookupForm, 'DELETE_SAVED_PROFILE',
        "Delete the current saved profile."),
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
Command(RecordForm, 'CONTEXT_ACTION',
        "Vyvolání akce nad aktuálním řádkem formuláře (mandatory arg. 'action')")
Command(ListForm, 'ACTIVATE',
        "Aktivační funkce pro aktuální řádek formuláře (arg. 'alternate')")
Command(ListForm, 'FIRST_COLUMN',
        "Přechod na první sloupec tabulky")
Command(ListForm, 'LAST_COLUMN',
        "Přechod na poslední sloupec tabulky")
Command(ListForm, 'INCREMENTAL_SEARCH',
        "Prefixové inkrementální hledání záznamu (arg. 'full')")
Command(ListForm, 'EDIT',
        "Vyvolání inline editace aktuální buňky")
Command(ListForm, 'COPY_CELL',
        "Zkopírování obsahu aktuální buňky do clipboardu")
Command(ListForm, 'FILTER_BY_CELL',
        "Vyfiltrování formuláře podle hodnoty aktuální buňky")
Command(ListForm, 'AUTOFILTER',
        "Zobrazení menu autofilteru (arg. 'col' a 'position')")
Command(ListForm, 'TOGGLE_AGGREGATION',
        "Přidání/odebrání agregační funkce (arg. 'operation')")
Command(ListForm, 'AGGREGATE',
        "Přidání agregační funkce (arg. 'operation')")
Command(ListForm, 'UNAGGREGATE',
        "Odebrání agregační funkce (arg. 'operation')")
Command(ListForm, 'AGGREGATED_VIEW',
        "Zobrazení agregovaného náhledu pro daný formulář.")
Command(ListForm, 'COPY_AGGREGATION_RESULT',
        "Zkopírování výsledku agreganí funkce do schránky (arg. 'operation', 'cid')")
Command(ListForm, 'EXPORT_CSV',
        "Export řádkového formuláře do csv souboru")
Command(ListForm, 'EXPORT_FILE',
        "Export řádkového formuláře do souboru")
Command(ListForm, 'LINE_COMMIT',
        "Dokončení editace záznamu (uložení)")
Command(ListForm, 'LINE_ROLLBACK',
        "Zrušení změn v editovaném záznamu (arg. 'soft')")
Command(ListForm, 'FINISH_EDITING',
        "Opuštění editace řádku")
Command(ListForm, 'CELL_COMMIT',
        "Ukončení editace políčka s novou hodnotou")
Command(ListForm, 'CELL_ROLLBACK',
        "Ukončení editace políčka s vrácením původní hodnoty")
Command(ListForm, 'INSERT_LINE',
        "In-line vložení nového záznamu (arg. 'before' and 'copy')")
Command(ListForm, 'SET_GROUPING_COLUMN',
        "Změna sloupce vizuáního seskupování")
Command(ListForm, 'RESIZE_COLUMN',
        "Rozšíření/zůžení sloupce (arg. 'diff' +/- pixels)") 
Command(ListForm, 'MOVE_COLUMN',
        "Přesunutí sloupce doprava/doleva (arg. 'diff' +/- number of columns)")
Command(ListForm, 'TOGGLE_COLUMN',
        "Skrytí/zobrazení sloupce")
Command(ListForm, 'TOGGLE_ROW_LABELS',
        "Skrytí/zobrazení sloupce záhlaví řádků")
Command(ListForm, 'CONTEXT_MENU',
        "Zobrazení kontextového menu aktivní buňky")
Command(FoldableForm, 'EXPAND_OR_COLLAPSE',
        "Sbalování a rozbalování řádků")
Command(FoldableForm, 'EXPAND_OR_COLLAPSE_SUBTREE',
        "Sbalování a rozbalování podstromů")
Command(FoldableForm, 'EXPAND_ALL',
        "Kompletní rozbalení celého formuláře")
Command(FoldableForm, 'COLLAPSE_ALL',
        "Sbalení všech hlavních uzlů formuláře")
Command(FoldableForm, 'FOLDING_LEVEL',
        "Rozbalení uzlů formuláře na zadanou úroveň")
Command(EditForm, 'COMMIT_RECORD',
        "Ukončení editačního formuláře s uložením změn (arg. 'close', PopupEditForm also 'next')")
Command(EditForm, 'NAVIGATE',
        "Navigace mezi políčky editačního formuláře (arg. 'back')")
Command(BrowsableShowForm, 'NEXT_RECORD',
        "Přechod na další záznam vpřed/vzad (arg. 'back')") 
Command(DualForm, 'OTHER_FORM',
        "Přechod mezi podformuláři duálního formuláře")
Command(MultiForm, 'NEXT_FORM',
        "Advance to next/previous tab in a multi-form (arg. 'back')")
Command(PrintFormInternal, 'PRINT',
        "Pošli data na tiskárnu")
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
        "Vyvolání výběru hodnoty vstupního políčka (arg. 'alternate')")
Command(SpinnableField, 'SPIN',
        "Spinning the field value up/down (arg. 'up')")
Command(GenericCodebookField, 'INVOKE_CODEBOOK_FORM',
        "Vyvolání alternativního výběru hodnoty políčka")
Command(ListField, 'SELECT',
        "Výběr aktuálního záznamu")
Command(ListField, 'SHOW_SELECTED',
        "Výběr aktuálního záznamu")
Command(ListField, 'EDIT_SELECTED',
        "Vyvolání editačního formuláře nad aktuálním záznamem.")
Command(ListField, 'DELETE_SELECTED',
        "Smazání aktuálního záznamu z číselníku.")
Command(ListField, 'NEW_CODEBOOK_RECORD',
        "Vyvolání formuláře pro vložení nového záznamu do číselníku.")
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
        "Potvrzení dialogu (arg. 'force' simulates pressing the default button.)")
Command(Dialog, 'HELP',
        "Vyvolání nápovědy dialogu")

DEFAULT_KEYMAP = (
    ('F1',               Application.COMMAND_HELP(topic='pytis')),
    ('Ctrl-g',           Application.COMMAND_BREAK),
    ('Ctrl-Down',        Application.COMMAND_RAISE_NEXT_FORM),
    ('Ctrl-Up',          Application.COMMAND_RAISE_PREV_FORM),
    ('Ctrl-w',           Application.COMMAND_RAISE_RECENT_FORM),
    ('Ctrl-l',           Application.COMMAND_REFRESH),
    ('Ctrl-F1',          Form.COMMAND_HELP),
    ('Escape',           Form.COMMAND_SAFE_LEAVE_FORM),
    ('Ctrl-p',           BrowseForm.COMMAND_PRINT),
    ('Ctrl-p',           PrintFormInternal.COMMAND_PRINT),
    ('Ctrl-Backspace',   InnerForm.COMMAND_RELOAD_FORM_STATE),
    ('Ctrl-Shift-Backspace', InnerForm.COMMAND_RESET_FORM_STATE),
    ('F6',               RecordForm.COMMAND_NEW_RECORD),
    ('Ctrl-F6',          RecordForm.COMMAND_NEW_RECORD(copy=True)),
    ('Alt-F6',           RecordForm.COMMAND_IMPORT_INTERACTIVE),
    ('F5',               RecordForm.COMMAND_EDIT_RECORD),
    ('F8',               RecordForm.COMMAND_DELETE_RECORD),
    ('Ctrl-f',           LookupForm.COMMAND_FILTER),
    ('Ctrl-Alt-f',       LookupForm.COMMAND_FILTER(last=True)),
    ('Ctrl-Shift-f',     LookupForm.COMMAND_UNFILTER),
    ('F4',               LookupForm.COMMAND_SORT),
    ('F3',               LookupForm.COMMAND_SEARCH),
    ('Ctrl-s',           LookupForm.COMMAND_SEARCH(next=True)),
    ('Ctrl-r',           LookupForm.COMMAND_SEARCH(next=True, back=True)),
    ('Ctrl-j',           LookupForm.COMMAND_JUMP),
    ('Ctrl-Home',        LookupForm.COMMAND_FIRST_RECORD),
    ('Ctrl-Prior',       LookupForm.COMMAND_FIRST_RECORD),
    ('Ctrl-End',         LookupForm.COMMAND_LAST_RECORD),
    ('Ctrl-Next',        LookupForm.COMMAND_LAST_RECORD),
    ('Ctrl-F3',          ListForm.COMMAND_INCREMENTAL_SEARCH),
    ('Alt-F3',           ListForm.COMMAND_INCREMENTAL_SEARCH(full=True)),
    ('Enter',            ListForm.COMMAND_ACTIVATE),
    (' ',                ListForm.COMMAND_ACTIVATE(alternate=True)),
    ('Ctrl-c',           ListForm.COMMAND_COPY_CELL),
    ('Home',             ListForm.COMMAND_FIRST_COLUMN),
    ('End',              ListForm.COMMAND_LAST_COLUMN),
    ('Ctrl-e',           ListForm.COMMAND_EXPORT_FILE),
    ('F2',               ListForm.COMMAND_EDIT),
    ('F9',               ListForm.COMMAND_EDIT),
    ('Ctrl-F12',         ListForm.COMMAND_LINE_ROLLBACK),
    ('Escape',           ListForm.COMMAND_FINISH_EDITING),
    ('F12',              ListForm.COMMAND_LINE_COMMIT),
    ('Enter',            ListForm.COMMAND_CELL_COMMIT),
    ('Escape',           ListForm.COMMAND_CELL_ROLLBACK),
    ('Insert',           ListForm.COMMAND_INSERT_LINE()),
    ('F7',               ListForm.COMMAND_INSERT_LINE(copy=True)),
    ('Ctrl-Insert',      ListForm.COMMAND_INSERT_LINE(before=True)),
    ('Ctrl-F7',          ListForm.COMMAND_INSERT_LINE(before=True, copy=True)),
    ('Ctrl-Shift-Right', ListForm.COMMAND_RESIZE_COLUMN(diff=+5)),
    ('Ctrl-Shift-Left',  ListForm.COMMAND_RESIZE_COLUMN(diff=-5)),
    ('Shift-Right',      ListForm.COMMAND_MOVE_COLUMN(diff=+1)),
    ('Shift-Left',       ListForm.COMMAND_MOVE_COLUMN(diff=-1)),
    ('Alt-Down',         ListForm.COMMAND_CONTEXT_MENU),
    ('Alt-Enter',        ListForm.COMMAND_CONTEXT_MENU),
    ('F11',              ListForm.COMMAND_TOGGLE_ROW_LABELS),
    ('Ctrl-=',           ListForm.COMMAND_FILTER_BY_CELL),
    ('\\',               FoldableForm.COMMAND_EXPAND_OR_COLLAPSE),
    ('Ctrl-\\',          FoldableForm.COMMAND_EXPAND_OR_COLLAPSE_SUBTREE),
    ('Shift-\\',         FoldableForm.COMMAND_EXPAND_ALL),
    ('Ctrl-Shift-\\',    FoldableForm.COMMAND_COLLAPSE_ALL),
    ('Ctrl-Shift-j',     FoldableForm.COMMAND_FOLDING_LEVEL),
    ('Ctrl-Enter',       EditForm.COMMAND_COMMIT_RECORD),
    ('Ctrl-Shift-Enter', EditForm.COMMAND_COMMIT_RECORD(next=True)),
    ('Tab',              EditForm.COMMAND_NAVIGATE),
    ('Shift-Tab',        EditForm.COMMAND_NAVIGATE(back=True)),
    ('Next',             BrowsableShowForm.COMMAND_NEXT_RECORD),
    ('Prior',            BrowsableShowForm.COMMAND_NEXT_RECORD(back=True)),
    ('Ctrl-Tab',         DualForm.COMMAND_OTHER_FORM),
    ('Alt-Right',        MultiForm.COMMAND_NEXT_FORM),
    ('Alt-Left',         MultiForm.COMMAND_NEXT_FORM(back=True)),
    ('Next',             PrintFormInternal.COMMAND_NEXT_PAGE),
    ('Prior',            PrintFormInternal.COMMAND_PREVIOUS_PAGE),
    ('Ctrl-Backspace',   InputField.COMMAND_RESET),
    ('Alt-Down',         InputField.COMMAND_CONTEXT_MENU),
    ('Alt-Enter',        InputField.COMMAND_CONTEXT_MENU),
    ('Ctrl-x',           TextField.COMMAND_CUT),
    ('Ctrl-c',           TextField.COMMAND_COPY),
    ('Ctrl-v',           TextField.COMMAND_PASTE),
    ('F2',               Invocable.COMMAND_INVOKE_SELECTION),
    ('Ctrl-F2',          Invocable.COMMAND_INVOKE_SELECTION(alternate=True)),
    ('Backspace',        FileField.COMMAND_CLEAR),
    ('Delete',           FileField.COMMAND_CLEAR),
    ('Backspace',        ListField.COMMAND_SHOW_SELECTED),
    ('Prior',            SpinnableField.COMMAND_SPIN(up=True)),
    ('Next',             SpinnableField.COMMAND_SPIN(up=False)),
    ('Escape',           Dialog.COMMAND_CLOSE_DIALOG),
    ('Enter',            Dialog.COMMAND_COMMIT_DIALOG),
    ('Ctrl-Enter',       Dialog.COMMAND_COMMIT_DIALOG(force=True)),
    ('Alt-=',            ListForm.COMMAND_AGGREGATED_VIEW),

    # Just for backwards compatibility
    ('Ctrl-F4',       LookupForm.COMMAND_FILTER),

    )



if __debug__:
    Command(Application, 'CUSTOM_DEBUG',
            "Pomocný příkaz pro vyvolání pomocné ladící funkce")
    Command(Application, 'INSPECT',
            "Vyvolání vývojářského nástroje pro inspekci wx Widgets")
    DEFAULT_KEYMAP += \
        (('Ctrl-d', Application.COMMAND_CUSTOM_DEBUG),
         ('Ctrl-i', Application.COMMAND_INSPECT),)


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
    (Form.COMMAND_LEAVE_FORM,                              wx.ART_CROSS_MARK), #'close'),
    (InnerForm.COMMAND_DESCRIBE,                           'describe'),
    (InnerForm.COMMAND_AGGREGATION_MENU,                   'aggregate'),
    (InnerForm.COMMAND_PRINT_MENU,                         wx.ART_PRINT),
    (BrowseForm.COMMAND_PRINT,                             wx.ART_PRINT),
    (PrintFormInternal.COMMAND_PRINT,                     wx.ART_PRINT),
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
    (ListForm.COMMAND_EXPORT_FILE,                         wx.ART_FILE_SAVE), #'export'),
    (ListForm.COMMAND_IMPORT_INTERACTIVE,                  wx.ART_FILE_OPEN), #'import'),
    (ListForm.COMMAND_INSERT_LINE(before=True, copy=True), 'insert-line-before-copy'),
    (ListForm.COMMAND_INSERT_LINE(before=True),            'insert-line-before'),
    (ListForm.COMMAND_INSERT_LINE(copy=True),              'insert-line-copy'),
    (ListForm.COMMAND_INSERT_LINE,                         'insert-line'),
    (ListForm.COMMAND_TOGGLE_COLUMN(col=None),             'hide-column'),
    (ListForm.COMMAND_AGGREGATE,                           'aggregate'),
    (ListForm.COMMAND_UNAGGREGATE,                         'unaggregate'),
    (ListForm.COMMAND_COPY_AGGREGATION_RESULT,             wx.ART_COPY),
    (ListForm.COMMAND_AGGREGATED_VIEW,                     'aggregate'),
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


class UICommands(object):
    PYTIS_HELP = UICommand(
        Application.COMMAND_HELP(topic='pytis'),
        _("Nápověda systému Pytis"),
        _("Zobrazit uživatelskou příručku popisující ovládání aplikace")
        )
    HELP = UICommand(
        Form.COMMAND_HELP(),
        _("Nápověda k aktuálnímu formuláři"),
        _("Zobrazit podrobnou nápovědu k aktuálnímu formuláři v prohlížeči nápovědy"))
    DESCRIBE = UICommand(
        InnerForm.COMMAND_DESCRIBE(),
        _("Popis aktuálního formuláře"),
        _("Zobrazit popis aktuálního formuláře v samostatném okně"))
    AGGREGATION_MENU = UICommand(
        InnerForm.COMMAND_AGGREGATION_MENU(),
        _("Zobrazit menu agregačních funkcí"),
        _("Zobrazit menu agregačních funkcí pro aktivní formulář"))
    PRINT_MENU = UICommand(
        InnerForm.COMMAND_PRINT_MENU(),
        _("Zobrazit tiskové menu"),
        _("Zobrazit menu tiskových sestav pro aktivní formulář"))
    JUMP = UICommand(
        LookupForm.COMMAND_JUMP(),
        _("Skok na záznam"),
        _("Skok na záznam podle čísla řádku."))
    SEARCH = UICommand(
        LookupForm.COMMAND_SEARCH(),
        _("Hledat"),
        _("Vyhledávat záznam podle zadaných podmínek."))
    SEARCH_NEXT = UICommand(
        LookupForm.COMMAND_SEARCH(next=True),
        _("Hledat další"),
        _("Vyhledat další záznam odpovídající zadaným podmínkám."))
    SEARCH_PREVIOUS = UICommand(
        LookupForm.COMMAND_SEARCH(next=True, back=True),
        _("Hledat předchozí"),
        _("Vyhledat předchozí záznam odpovídající zadaným podmínkám."))
    INCREMENTAL_SEARCH = UICommand(
        ListForm.COMMAND_INCREMENTAL_SEARCH(),
        _("Inkrementální hledání"),
        _("Postupně vyhledávat záznam podle hodnoty políčka."))
    SORT = UICommand(
        LookupForm.COMMAND_SORT(),
        _("Řazení"),
        _("Určit podmínky řazení záznamů."))
    PROFILE_MENU = UICommand(
        LookupForm.COMMAND_PROFILE_MENU(),
        _("Výběr aktivního profilu"),
        _("Zobrazuje aktivní profil a umožňuje výběr z ostatních předdefinovaných profilů."))
    FILTER = UICommand(
        LookupForm.COMMAND_FILTER(),
        _("Filtrování"),
        _("Filtrovat záznamy podle zadaných podmínek."))
    UNFILTER = UICommand(
        LookupForm.COMMAND_UNFILTER(),
        _("Zruš filtr"),
        _("Zrušit filtrování záznamů."))
    EDIT_RECORD = UICommand(
        RecordForm.COMMAND_EDIT_RECORD(),
        _("Upravit záznam"),
        _("Upravit hodnoty záznamu v samostatném formuláři."))
    NEW_RECORD = UICommand(
        RecordForm.COMMAND_NEW_RECORD(),
        _("Nový záznam"),
        _("Vložit nový záznam v samostatném formuláři."))
    NEW_RECORD_COPY = UICommand(
        RecordForm.COMMAND_NEW_RECORD(copy=True),
        _("Nový záznam - kopie"),
        _("Vložit nový záznam zkopírováním hodnot aktuálního záznamu."))
    DELETE_RECORD = UICommand(
        RecordForm.COMMAND_DELETE_RECORD(),
        _("Smazat záznam"),
        _("Smazat aktuální záznam."))
    INSERT_LINE = UICommand(
        ListForm.COMMAND_INSERT_LINE(),
        _("Vložit řádek pod"),
        _("Vložit nový záznam v režimu inline editace."))
    INSERT_LINE_BEFORE = UICommand(
        ListForm.COMMAND_INSERT_LINE(before=True),
        _("Vložit řádek nad"),
        _("Vložit nový záznam v režimu inline editace."))
    INSERT_LINE_COPY = UICommand(
        ListForm.COMMAND_INSERT_LINE(copy=True),
        _("Kopírovat řádek pod"),
        _("Vložit nový záznam v režimu inline editace jako kopii současného."))
    INSERT_LINE_COPY_BEFORE = UICommand(
        ListForm.COMMAND_INSERT_LINE(copy=True, before=True),
        _("Kopírovat řádek nad"),
        _("Vložit nový záznam v režimu inline editace jako kopii současného."))
    EXPORT_FILE = UICommand(
        ListForm.COMMAND_EXPORT_FILE(),
        _("Export do souboru"),
        _("Exportovat data do souboru."))
    IMPORT_INTERACTIVE = UICommand(
        RecordForm.COMMAND_IMPORT_INTERACTIVE(),
        _("Import z textového souboru"),
        _("Importovat data z textového souboru ve formátu CSV."))
    AGGREGATED_VIEW = UICommand(
        ListForm.COMMAND_AGGREGATED_VIEW(),
        _("Zobrazit agregovaný náhled"),
        _("Zobrazit duální formulář se zvolenými agregačními funkcemi."))
    RELOAD_FORM_STATE = UICommand(
        InnerForm.COMMAND_RELOAD_FORM_STATE(),
        _("Vrátit předchozí nastavení formuláře"),
        _("Zahodit změny nastavení formuláře provedené během tohoto spuštění aplikace"))
    RESET_FORM_STATE = UICommand(
        InnerForm.COMMAND_RESET_FORM_STATE(),
        _("Vrátit výchozí nastavení formuláře"),
        _("Zahodit veškeré uživatelské nastavení formuláře"))
    OTHER_FORM = UICommand(
        DualForm.COMMAND_OTHER_FORM(),
        _("Přepnout aktivní formulář duálního formuláře"),
        _("Přechod mezi horním a dolním formulářem duálního formuláře"))
    LEAVE_FORM = UICommand(
        Form.COMMAND_LEAVE_FORM(),
        _("Uzavřít formulář"),
        _("Uzavřít aktuální formulář"))


TOOLBAR_COMMANDS = ((
        UICommands.NEW_RECORD,
        UICommands.EDIT_RECORD,
        UICommands.DELETE_RECORD,
        ),(
        UICommands.EXPORT_FILE,
        UICommands.PRINT_MENU,
        UICommands.OTHER_FORM,
        ),(
        UICommands.INCREMENTAL_SEARCH,
        UICommands.SEARCH,
        UICommands.SEARCH_PREVIOUS,
        UICommands.SEARCH_NEXT,
        UICommands.JUMP,
        ),(
        UICommands.AGGREGATION_MENU,
        ),(
        UICommands.FILTER,
        UICommands.PROFILE_MENU,
        ),(
        UICommands.PYTIS_HELP,
        UICommands.HELP,
        UICommands.DESCRIBE,
        ),(
        UICommands.LEAVE_FORM,
        ))

FORM_MENU_COMMANDS = ((
        UICommands.INCREMENTAL_SEARCH,
        UICommands.SEARCH,
        UICommands.SEARCH_PREVIOUS,
        UICommands.SEARCH_NEXT,
        UICommands.JUMP,
        ),(
        UICommands.SORT,
        UICommands.FILTER,
        UICommands.UNFILTER,
        UICommands.AGGREGATED_VIEW,
        ),(
        UICommands.EDIT_RECORD,
        UICommands.NEW_RECORD,
        UICommands.NEW_RECORD_COPY,
        UICommands.DELETE_RECORD,
        ),(
        UICommands.INSERT_LINE,
        UICommands.INSERT_LINE_BEFORE,
        UICommands.INSERT_LINE_COPY,
        UICommands.INSERT_LINE_COPY_BEFORE,
        ),(
        UICommands.EXPORT_FILE,
        UICommands.IMPORT_INTERACTIVE,
        ),(
        UICommands.RELOAD_FORM_STATE,
        UICommands.RESET_FORM_STATE,
))
