# -*- coding: utf-8 -*-

# Definice uživatelských příkazů
# 
# Copyright (C) 2002-2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Brailcom, o.p.s.
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

import wx

import pytis.util
from command import Command, UICommand
from dialog import Dialog
from dualform import DualForm, MultiForm, MultiSideForm
from form import BrowsableShowForm, EditForm, Form, InnerForm, LookupForm, RecordForm
from inputfield import FileField, GenericCodebookField, InputField, Invocable, \
    ListField, SpinnableField, StructuredTextField, TextField
from list import BrowseForm, FoldableForm, ListForm
from screen import Browser, DualFormSwitcher, DualFormResplitter, ProfileSelector
from application import Application

_ = pytis.util.translations('pytis-wx')

    
Command(Application, 'EXIT',
        "Ukončení aplikace")
Command(Application, 'HELP',
        "Vyvolání nápovědy")
Command(Application, 'BREAK',
        "Přerušení aktuálně prováděné operace")
Command(Application, 'REFRESH',
        "Request refresh of currently visible form(s)")
Command(Application, 'RELOAD_SPECIFICATIONS',
        "Request reload of specification files (development/tuning)")
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
Command(LookupForm, 'FILTER',
        "Filtrování záznamů")
Command(LookupForm, 'UNFILTER',
        "Zrušení filtrování záznamů")
Command(LookupForm, 'PROFILE_MENU',
        "Show profile menu for the current form")
Command(LookupForm, 'APPLY_PROFILE',
        "Apply given form profile on the current form (arg. 'index')")
Command(LookupForm, 'SAVE_NEW_PROFILE',
        "Save the current form state as a new profile."),
Command(LookupForm, 'UPDATE_PROFILE',
        "Update the existing profile according to the current form state."),
Command(LookupForm, 'DELETE_PROFILE',
        "Delete the current saved profile (only for user profiles)."),
Command(LookupForm, 'RENAME_PROFILE',
        "Rename the saved user defined profile."),
Command(LookupForm, 'RELOAD_PROFILE',
        "Reinitialize the form to the last saved state of the current profile.")
Command(LookupForm, 'RESET_PROFILE',
        ("Discard all saved profile changes and load the original settings "
         "(only for predefined profiles)"))
Command(LookupForm, 'SET_INITIAL_PROFILE',
        "Use the current profile as the initial profile on next form startup")
Command(LookupForm, 'FILTER_BY_VALUE',
        "Vyfiltrování formuláře podle dané hodnoty (arg. 'column_id', 'value')")
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
Command(RecordForm, 'OPEN_EDITOR',
        "Open StructuredTextEditor form for field given by the argument 'field_id'")
Command(RecordForm, 'REFRESH_DB',
        "Refresh the underlying database object")
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
        "Display an aggregated view (arg. 'aggregated_view_id').")
Command(ListForm, 'DELETE_AGGREGATED_VIEW',
        "Delete an aggregated view (arg. 'aggregated_view_id').")
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
        "Rozšíření/zúžení sloupce (arg. 'diff' +/- pixels)")
Command(ListForm, 'MOVE_COLUMN',
        "Přesunutí sloupce doprava/doleva (arg. 'diff' +/- number of columns)")
Command(ListForm, 'TOGGLE_COLUMN',
        "Skrytí/zobrazení sloupce")
Command(ListForm, 'TOGGLE_ROW_LABELS',
        "Skrytí/zobrazení sloupce záhlaví řádků")
Command(ListForm, 'CONTEXT_MENU',
        "Zobrazení kontextového menu aktivní buňky")
Command(ListForm, 'CLEAR_SELECTION',
        "Clear the current selection of rows in the form.")
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
Command(DualForm, 'RESPLIT',
        "Toggle the splitter orientation between vertical/horizontal")
Command(MultiForm, 'NEXT_FORM',
        "Advance to next/previous tab in a multi-form (arg. 'back')")
Command(MultiSideForm, 'TOGGLE_SIDEFORM',
        "Show/hide given side form tab. (arg. 'index' (side form index))")
Command(MultiSideForm, 'FILTER_BY_SIDEFORM',
        "Filter main form rows having non-zero side form rows. (arg. 'index' (side form index))")
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
Command(FileField, 'OPEN',
        "Otevření souboru ve výchozí aplikaci.")
Command(StructuredTextField, 'SEARCH',
        "Search for a string within the text field content.")
Command(StructuredTextField, 'SEARCH_AND_REPLACE',
        "Search and replace text within the text field content.")
Command(StructuredTextField, 'UNDO',
        "Undo last text edit operation.")
Command(StructuredTextField, 'REDO',
        "Redo last text edit operation.")
Command(StructuredTextField, 'HEADING',
        "Insert markup for heading.")
Command(StructuredTextField, 'STRONG',
        "Insert markup for strong text.")
Command(StructuredTextField, 'EMPHASIZED',
        "Insert markup for emphasized text.")
Command(StructuredTextField, 'UNDERLINED',
        "Insert markup for underlined text.")
Command(StructuredTextField, 'LINK',
        "Insert markup for a hypertext link.")
Command(StructuredTextField, 'ATTACHMENT',
        "Insert attachment.")
Command(StructuredTextField, 'IMAGE',
        "Insert image.")
Command(StructuredTextField, 'LINEBREAK',
        "Insert markup for a forced line break.")
Command(StructuredTextField, 'ITEMIZE',
        "Insert markup for a bullet/numbered list item (arg. style).")
Command(StructuredTextField, 'VERBATIM',
        "Insert markup for verbatim text.")
Command(StructuredTextField, 'PREVIEW',
        "Preview the text formatted as HTML in a browser.")
Command(StructuredTextField, 'EXPORT_PDF',
        "Preview the text formatted as PDF in a viewer.")
Command(StructuredTextField, 'OPEN_IN_EDITOR',
        "Edit the field text in a standalone editor.")

Command(Dialog, 'CLOSE_DIALOG',
        "Opuštění dialogu bez potvrzení")
Command(Dialog, 'COMMIT_DIALOG',
        "Potvrzení dialogu (arg. 'force' simulates pressing the default button.)")
Command(Dialog, 'HELP',
        "Vyvolání nápovědy dialogu")

Command(Browser, 'GO_FORWARD',
        "Go to the next page in browser history")
Command(Browser, 'GO_BACK',
        "Go to the previous page in browser history")
Command(Browser, 'STOP_LOADING',
        "Stop loading")
Command(Browser, 'RELOAD',
        "Reload the current document")
Command(Browser, 'LOAD_URI',
        "Load the URI given by the 'uri' argument of the command.")

DEFAULT_KEYMAP = (
    ('F1',               Application.COMMAND_HELP(topic='pytis')),
    ('Ctrl-g',           Application.COMMAND_BREAK),
    ('Ctrl-Down',        Application.COMMAND_RAISE_NEXT_FORM),
    ('Ctrl-Up',          Application.COMMAND_RAISE_PREV_FORM),
    ('Ctrl-w',           Application.COMMAND_RAISE_RECENT_FORM),
    ('Ctrl-l',           Application.COMMAND_REFRESH),
    ('Ctrl-F11',         Application.COMMAND_RELOAD_SPECIFICATIONS),
    ('Ctrl-F1',          Form.COMMAND_HELP),
    ('Escape',           Form.COMMAND_SAFE_LEAVE_FORM),
    ('Ctrl-p',           BrowseForm.COMMAND_PRINT),
    ('F6',               RecordForm.COMMAND_NEW_RECORD),
    ('Ctrl-F6',          RecordForm.COMMAND_NEW_RECORD(copy=True)),
    ('Alt-F6',           RecordForm.COMMAND_IMPORT_INTERACTIVE),
    ('F5',               RecordForm.COMMAND_EDIT_RECORD),
    ('F8',               RecordForm.COMMAND_DELETE_RECORD),
    ('Ctrl-F10',         RecordForm.COMMAND_REFRESH_DB),
    ('Ctrl-f',           LookupForm.COMMAND_FILTER),
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
    ('Ctrl-Backspace',   LookupForm.COMMAND_RELOAD_PROFILE),
    ('Ctrl-Shift-Backspace', LookupForm.COMMAND_RESET_PROFILE),
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
    ('+',                FoldableForm.COMMAND_EXPAND_OR_COLLAPSE),
    ('Ctrl-+',           FoldableForm.COMMAND_EXPAND_OR_COLLAPSE_SUBTREE),
    ('Shift-+',          FoldableForm.COMMAND_EXPAND_ALL),
    ('Shift--',          FoldableForm.COMMAND_COLLAPSE_ALL),
    ('Ctrl-Shift-+',     FoldableForm.COMMAND_FOLDING_LEVEL),
    ('Ctrl-Enter',       EditForm.COMMAND_COMMIT_RECORD),
    ('Ctrl-Shift-Enter', EditForm.COMMAND_COMMIT_RECORD(next=True)),
    ('Tab',              EditForm.COMMAND_NAVIGATE),
    ('Shift-Tab',        EditForm.COMMAND_NAVIGATE(back=True)),
    ('Next',             BrowsableShowForm.COMMAND_NEXT_RECORD),
    ('Prior',            BrowsableShowForm.COMMAND_NEXT_RECORD(back=True)),
    ('Ctrl-Tab',         DualForm.COMMAND_OTHER_FORM),
    ('Ctrl-.',           DualForm.COMMAND_RESPLIT),
    ('Alt-Right',        MultiForm.COMMAND_NEXT_FORM),
    ('Alt-Left',         MultiForm.COMMAND_NEXT_FORM(back=True)),
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
    ('F6',               ListField.COMMAND_NEW_CODEBOOK_RECORD),
    ('F5',               ListField.COMMAND_EDIT_SELECTED),
    ('F8',               ListField.COMMAND_DELETE_SELECTED),
    ('Prior',            SpinnableField.COMMAND_SPIN(up=True)),
    ('Next',             SpinnableField.COMMAND_SPIN(up=False)),
    ('Escape',           Dialog.COMMAND_CLOSE_DIALOG),
    ('Enter',            Dialog.COMMAND_COMMIT_DIALOG),
    ('Ctrl-Enter',       Dialog.COMMAND_COMMIT_DIALOG(force=True)),

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
    (Form.COMMAND_LEAVE_FORM,                              'close'),
    (InnerForm.COMMAND_DESCRIBE,                           'describe'),
    (InnerForm.COMMAND_AGGREGATION_MENU,                   'aggregate'),
    (InnerForm.COMMAND_PRINT_MENU,                         wx.ART_PRINT),
    (BrowseForm.COMMAND_PRINT,                             wx.ART_PRINT),
    (InputField.COMMAND_RESET,                             wx.ART_UNDO),
    (TextField.COMMAND_CUT,                                wx.ART_CUT),
    (TextField.COMMAND_COPY,                               wx.ART_COPY),
    (TextField.COMMAND_PASTE,                              wx.ART_PASTE),
    (FileField.COMMAND_LOAD,                               wx.ART_FILE_OPEN),
    (FileField.COMMAND_SAVE,                               wx.ART_FILE_SAVE),
    (FileField.COMMAND_CLEAR,                              wx.ART_DELETE),
    (StructuredTextField.COMMAND_SEARCH,                   wx.ART_FIND),
    (StructuredTextField.COMMAND_SEARCH_AND_REPLACE,       wx.ART_FIND_AND_REPLACE),
    (StructuredTextField.COMMAND_UNDO,                     wx.ART_UNDO),
    (StructuredTextField.COMMAND_REDO,                     wx.ART_REDO),
    (StructuredTextField.COMMAND_STRONG,                   'text-bold'),
    (StructuredTextField.COMMAND_EMPHASIZED,               'text-italic'),
    (StructuredTextField.COMMAND_UNDERLINED,               'text-underlined'),
    (StructuredTextField.COMMAND_LINK,                     'text-link'),
    (StructuredTextField.COMMAND_IMAGE,                    'text-image'),
    (StructuredTextField.COMMAND_ATTACHMENT,               'text-attachment'),
    (StructuredTextField.COMMAND_LINEBREAK,                'text-linebreak'),
    (StructuredTextField.COMMAND_ITEMIZE(style='bullet'),  'text-bullet-list'),
    (StructuredTextField.COMMAND_ITEMIZE(style='numbered'),'text-numbered-list'),
    (StructuredTextField.COMMAND_VERBATIM(),               'text-verbatim'),
    (StructuredTextField.COMMAND_PREVIEW,                  'preview'),
    (StructuredTextField.COMMAND_EXPORT_PDF,               'print-preview'),
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
    (EditForm.COMMAND_COMMIT_RECORD,                       wx.ART_FILE_SAVE),
    (LookupForm.COMMAND_SORT(direction=_ASC),              'sort-asc'),
    (LookupForm.COMMAND_SORT(direction=_DESC),             'sort-desc'),
    (LookupForm.COMMAND_SORT(direction=_NONE),             'unsort'),
    (LookupForm.COMMAND_SORT,                              'sort'),
    (LookupForm.COMMAND_FILTER_BY_VALUE,                   'filter-by-cell'),
    (LookupForm.COMMAND_FILTER,                            'filter-form'),
    (LookupForm.COMMAND_UNFILTER,                          'unfilter'),
    (LookupForm.COMMAND_SEARCH(next=True, back=True),      'search-backwards'),
    (LookupForm.COMMAND_SEARCH(next=True),                 'search-forward'),
    (LookupForm.COMMAND_SEARCH,                            'search'),
    (LookupForm.COMMAND_JUMP,                              'jump'),
    (LookupForm.COMMAND_RESET_PROFILE,                     'reset-profile'),
    (LookupForm.COMMAND_RELOAD_PROFILE,                    'reload-profile'),
    (MultiSideForm.COMMAND_FILTER_BY_SIDEFORM,                         'filter'),
    (Browser.COMMAND_GO_BACK,                              wx.ART_GO_BACK),
    (Browser.COMMAND_GO_FORWARD,                           wx.ART_GO_FORWARD),
    (Browser.COMMAND_STOP_LOADING,                         wx.ART_CROSS_MARK),
    (Browser.COMMAND_RELOAD,                               'reload'),
)


class UICommands(object):
    PYTIS_HELP = UICommand(
        Application.COMMAND_HELP(topic='pytis'),
        _("Pytis Help"),
        _("Open the user's manual describing work with the application in general"))
    HELP = UICommand(
        Form.COMMAND_HELP(),
        _("Help for the current form"),
        _("Open the detailed description of the current form in the help browser"))
    DESCRIBE = UICommand(
        InnerForm.COMMAND_DESCRIBE(),
        _("Current form description"),
        _("Display a breif description of the current form"))
    AGGREGATION_MENU = UICommand(
        InnerForm.COMMAND_AGGREGATION_MENU(),
        _("Show aggregation functions menu"),
        _("Show the menu of aggregation functions for the current form"))
    PRINT_MENU = UICommand(
        InnerForm.COMMAND_PRINT_MENU(),
        _("Show print menu"),
        _("Show the menu of print reports for the current form"))
    JUMP = UICommand(
        LookupForm.COMMAND_JUMP(),
        _("Jump to record"),
        _("Jump to record by row number"))
    SEARCH = UICommand(
        LookupForm.COMMAND_SEARCH(),
        _("Search"),
        _("Search a record matching given conditions."))
    SEARCH_NEXT = UICommand(
        LookupForm.COMMAND_SEARCH(next=True),
        _("Search next"),
        _("Search next record matching given conditions."))
    SEARCH_PREVIOUS = UICommand(
        LookupForm.COMMAND_SEARCH(next=True, back=True),
        _("Search previous"),
        _("Search previous record matching given conditions."))
    INCREMENTAL_SEARCH = UICommand(
        ListForm.COMMAND_INCREMENTAL_SEARCH(),
        _("Search incrementally"),
        _("Progressively search record by typing column value."))
    SORT = UICommand(
        LookupForm.COMMAND_SORT(),
        _("Sort"),
        _("Define the terms of sorting records."))
    PROFILE_MENU = UICommand(
        LookupForm.COMMAND_PROFILE_MENU(),
        _("Profile selection"),
        _("Shows the current profile and allows selection from predefined profiles."),
        ctrl=(ProfileSelector, dict(size=(270, 25))))
    FILTER = UICommand(
        LookupForm.COMMAND_FILTER(),
        _("Filter"),
        _("Filter records matching given conditions."))
    UNFILTER = UICommand(
        LookupForm.COMMAND_UNFILTER(),
        _("Unfilter"),
        _("Cancel current filtering condition."))
    EDIT_RECORD = UICommand(
        RecordForm.COMMAND_EDIT_RECORD(),
        _("Edit record"),
        _("Open a form to edit the current record."))
    NEW_RECORD = UICommand(
        RecordForm.COMMAND_NEW_RECORD(),
        _("New record"),
        _("Open a form to create a new record."))
    NEW_RECORD_COPY = UICommand(
        RecordForm.COMMAND_NEW_RECORD(copy=True),
        _("New record - copy"),
        _("Create a new record by copying the values of the current record."))
    DELETE_RECORD = UICommand(
        RecordForm.COMMAND_DELETE_RECORD(),
        _("Delete record"),
        _("Delete the current record."))
    INSERT_LINE = UICommand(
        ListForm.COMMAND_INSERT_LINE(),
        _("Insert row below"),
        _("Insert a new record below the current row in inline mode."))
    INSERT_LINE_BEFORE = UICommand(
        ListForm.COMMAND_INSERT_LINE(before=True),
        _("Insert row above"),
        _("Insert a new record above the current row in inline mode."))
    INSERT_LINE_COPY = UICommand(
        ListForm.COMMAND_INSERT_LINE(copy=True),
        _("Copy row below"),
        _("Insert a new record as a copy below the current row in inline mode."))
    INSERT_LINE_COPY_BEFORE = UICommand(
        ListForm.COMMAND_INSERT_LINE(copy=True, before=True),
        _("Copy row above"),
        _("Insert a new record as a copy above the current row in inline mode."))
    EXPORT_FILE = UICommand(
        ListForm.COMMAND_EXPORT_FILE(),
        _("Export to file"),
        _("Export form data into a file."))
    IMPORT_INTERACTIVE = UICommand(
        RecordForm.COMMAND_IMPORT_INTERACTIVE(),
        _("Import from file"),
        _("Import data from a text file in CSV format."))
    OTHER_FORM = UICommand(
        DualForm.COMMAND_OTHER_FORM(),
        _("Switch the active form"),
        _("Switch between the top and the bottom form of a dual form."),
        ctrl=DualFormSwitcher)
    RESPLIT = UICommand(
        DualForm.COMMAND_RESPLIT(),
        _("Switch splitter orientation"),
        _("Switch between horizontal and vertical arrangement of the main form and side form."),
        ctrl=DualFormResplitter)
    LEAVE_FORM = UICommand(
        Form.COMMAND_LEAVE_FORM(),
        _("Close form"),
        _("Close the current form window."))


TOOLBAR_COMMANDS = ((UICommands.NEW_RECORD,
                     UICommands.EDIT_RECORD,
                     UICommands.DELETE_RECORD,
                     ),
                    (UICommands.EXPORT_FILE,
                     UICommands.PRINT_MENU,
                     UICommands.OTHER_FORM,
                     UICommands.RESPLIT,
                     ),
                    (UICommands.INCREMENTAL_SEARCH,
                     UICommands.SEARCH,
                     UICommands.SEARCH_PREVIOUS,
                     UICommands.SEARCH_NEXT,
                     UICommands.JUMP,
                     ),
                    (UICommands.AGGREGATION_MENU,
                     ),
                    (UICommands.FILTER,
                     UICommands.PROFILE_MENU,
                     ),
                    (UICommands.PYTIS_HELP,
                     UICommands.HELP,
                     UICommands.DESCRIBE,
                     ),
                    (UICommands.LEAVE_FORM,
                     ),
                    )

FORM_MENU_COMMANDS = ((UICommands.INCREMENTAL_SEARCH,
                       UICommands.SEARCH,
                       UICommands.SEARCH_PREVIOUS,
                       UICommands.SEARCH_NEXT,
                       UICommands.JUMP,
                       ),
                      (UICommands.SORT,
                       UICommands.FILTER,
                       UICommands.UNFILTER,
                       ),
                      (UICommands.EDIT_RECORD,
                       UICommands.NEW_RECORD,
                       UICommands.NEW_RECORD_COPY,
                       UICommands.DELETE_RECORD,
                       ),
                      (UICommands.INSERT_LINE,
                       UICommands.INSERT_LINE_BEFORE,
                       UICommands.INSERT_LINE_COPY,
                       UICommands.INSERT_LINE_COPY_BEFORE,
                       ),
                      (UICommands.EXPORT_FILE,
                       UICommands.IMPORT_INTERACTIVE,
                       ),
                      )
