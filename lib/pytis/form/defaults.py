# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2016 OUI Technology Ltd.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
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

"""

import wx
import pytis
import pytis.data
import pytis.util

from pytis.presentation import Command

from .command import UICommand
from .dialog import Dialog
from .dualform import DualForm, MultiForm, MultiSideForm
from .form import (
    BrowsableShowForm, EditForm, Form, InnerForm, LookupForm, RecordForm,
)
from .inputfield import (
    FileField, InputField, Invocable, ListField, SpinnableField,
    StructuredTextField, TextField,
)
from .list import BrowseForm, FoldableForm, ListForm
from .screen import (
    Browser, DualFormSwitcher, DualFormResplitter, ProfileSelector, KeyboardSwitcher,
)
from .application import Application

_ = pytis.util.translations('pytis-wx')


DEFAULT_KEYMAP = (
    ('F1', Command(Application.help, topic='pytis')),
    ('Ctrl-g', Command(Application.stop)),
    ('Ctrl-Alt-Right', Command(Application.activate_next_form)),
    ('Ctrl-Alt-Left', Command(Application.activate_next_form, back=True)),
    ('Ctrl-w', Command(Application.activate_recent_form)),
    ('Ctrl-l', Command(Application.refresh)),
    ('Ctrl-F11', Command(Application.reload_specifications)),
    ('Ctrl-F1', Command(Form.help)),
    ('Escape', Command(Form.leave_form)),
    ('Ctrl-p', Command(BrowseForm.printout)),
    ('F6', Command(RecordForm.new_record)),
    ('Ctrl-F6', Command(RecordForm.new_record, copy=True)),
    ('Alt-F6', Command(RecordForm.import_interactive)),
    ('F5', Command(RecordForm.edit_record)),
    ('F8', Command(RecordForm.delete_record)),
    ('Ctrl-F10', Command(RecordForm.refresh_db)),
    ('Ctrl-f', Command(LookupForm.select_filter)),
    ('Ctrl-Shift-f', Command(LookupForm.unfilter)),
    ('F4', Command(LookupForm.sort)),
    ('F3', Command(LookupForm.search)),
    ('Ctrl-s', Command(LookupForm.search, next=True)),
    ('Ctrl-r', Command(LookupForm.search, next=True, back=True)),
    ('Ctrl-j', Command(LookupForm.jump)),
    ('Ctrl-Home', Command(LookupForm.first_record)),
    ('Ctrl-Prior', Command(LookupForm.first_record)),
    ('Ctrl-End', Command(LookupForm.last_record)),
    ('Ctrl-Next', Command(LookupForm.last_record)),
    ('Ctrl-Backspace', Command(LookupForm.reload_profile)),
    ('Ctrl-Shift-Backspace', Command(LookupForm.reset_profile)),
    ('Ctrl-F3', Command(ListForm.incremental_search)),
    ('Alt-F3', Command(ListForm.incremental_search, full=True)),
    ('Enter', Command(ListForm.activate)),
    (' ', Command(ListForm.activate, alternate=True)),
    ('Ctrl-c', Command(ListForm.copy_cell)),
    ('Home', Command(ListForm.first_column)),
    ('End', Command(ListForm.last_column)),
    ('Ctrl-e', Command(ListForm.export_file)),
    ('Ctrl-Shift-Right', Command(ListForm.resize_column, diff=+5)),
    ('Ctrl-Shift-Left', Command(ListForm.resize_column, diff=-5)),
    ('Shift-Right', Command(ListForm.move_column, diff=+1)),
    ('Shift-Left', Command(ListForm.move_column, diff=-1)),
    ('Alt-Down', Command(ListForm.context_menu)),
    ('Alt-Enter', Command(ListForm.context_menu)),
    ('F11', Command(ListForm.toggle_row_labels)),
    ('Ctrl-=', Command(ListForm.filter_by_cell)),
    ('+', Command(FoldableForm.expand_or_collapse)),
    ('Ctrl-+', Command(FoldableForm.expand_or_collapse_subtree)),
    ('Shift-+', Command(FoldableForm.expand_all)),
    ('Shift--', Command(FoldableForm.collapse_all)),
    ('Ctrl-Shift-+', Command(FoldableForm.select_folding_level)),
    ('Ctrl-Enter', Command(EditForm.commit_record)),
    ('Ctrl-Shift-Enter', Command(EditForm.commit_record, close=False, advance=True)),
    ('Tab', Command(EditForm.navigate)),
    ('Shift-Tab', Command(EditForm.navigate, back=True)),
    ('Next', Command(BrowsableShowForm.next_record)),
    ('Prior', Command(BrowsableShowForm.next_record, back=True)),
    ('Ctrl-Tab', Command(DualForm.other_form)),
    ('Ctrl-.', Command(DualForm.resplit)),
    ('Alt-Right', Command(MultiForm.next_form)),
    ('Alt-Left', Command(MultiForm.next_form, back=True)),
    ('Ctrl-Backspace', Command(InputField.reset)),
    ('Alt-Down', Command(InputField.context_menu)),
    ('Alt-Enter', Command(InputField.context_menu)),
    ('Ctrl-x', Command(TextField.cut)),
    ('Ctrl-c', Command(TextField.copy)),
    ('Ctrl-v', Command(TextField.paste)),
    ('F2', Command(Invocable.invoke_selection)),
    ('Ctrl-F2', Command(Invocable.invoke_selection, alternate=True)),
    ('Backspace', Command(FileField.clear)),
    ('Delete', Command(FileField.clear)),
    ('Backspace', Command(ListField.show_selected)),
    ('F6', Command(ListField.new_codebook_record)),
    ('F5', Command(ListField.edit_selected)),
    ('F8', Command(ListField.delete_selected)),
    ('Prior', Command(SpinnableField.spin, up=True)),
    ('Next', Command(SpinnableField.spin, up=False)),
    ('Escape', Command(Dialog.close_dialog)),
    ('Enter', Command(Dialog.commit_dialog)),
    ('Ctrl-Enter', Command(Dialog.commit_dialog, force=True)),

    # Just for backwards compatibility
    ('Ctrl-F4', Command(LookupForm.select_filter)),

)


if __debug__:
    DEFAULT_KEYMAP += \
        (('Ctrl-d', Command(Application.custom_debug)),
         ('Ctrl-i', Command(Application.inspect)),)


COMMAND_ICONS = (
    (Command(Application.help), wx.ART_HELP),
    (Command(Application.exit), wx.ART_QUIT),
    (Command(Application.activate_next_form), 'next-form'),
    (Command(Application.activate_next_form, back=True), 'prev-form'),
    (Command(Application.activate_recent_form), 'recent-form'),
    # (Command(Application.new_record), 'new-record'),
    # (Command(Application.run_form), 'run-form'),
    (Command(Form.help), 'help-book'),
    (Command(Form.leave_form), 'close'),
    (Command(InnerForm.describe), 'describe'),
    (Command(InnerForm.aggregation_menu), 'aggregate'),
    (Command(InnerForm.print_menu), wx.ART_PRINT),
    (Command(BrowseForm.printout), wx.ART_PRINT),
    (Command(InputField.reset), wx.ART_UNDO),
    (Command(TextField.cut), wx.ART_CUT),
    (Command(TextField.copy), wx.ART_COPY),
    (Command(TextField.paste), wx.ART_PASTE),
    (Command(FileField.load), wx.ART_FILE_OPEN),
    (Command(FileField.save), wx.ART_FILE_SAVE),
    (Command(FileField.clear), wx.ART_DELETE),
    (Command(StructuredTextField.search), wx.ART_FIND),
    (Command(StructuredTextField.search_and_replace), wx.ART_FIND_AND_REPLACE),
    (Command(StructuredTextField.undo), wx.ART_UNDO),
    (Command(StructuredTextField.redo), wx.ART_REDO),
    (Command(StructuredTextField.strong), 'text-bold'),
    (Command(StructuredTextField.emphasized), 'text-italic'),
    (Command(StructuredTextField.underlined), 'text-underlined'),
    (Command(StructuredTextField.link), 'text-link'),
    (Command(StructuredTextField.image), 'text-image'),
    (Command(StructuredTextField.attachment), 'text-attachment'),
    (Command(StructuredTextField.linebreak), 'text-linebreak'),
    (Command(StructuredTextField.itemize, style='bullet'), 'text-bullet-list'),
    (Command(StructuredTextField.itemize, style='numbered'), 'text-numbered-list'),
    (Command(StructuredTextField.verbatim), 'text-verbatim'),
    (Command(StructuredTextField.preview), 'preview'),
    (Command(StructuredTextField.export_pdf), 'print-preview'),
    (Command(ListForm.copy_cell), wx.ART_COPY),
    (Command(ListForm.filter_by_cell), 'filter-by-cell'),
    (Command(ListForm.autofilter), 'autofilter'),
    (Command(ListForm.incremental_search), 'search-incremental'),
    (Command(ListForm.export_file), wx.ART_FILE_SAVE),  # 'export'),
    (Command(ListForm.import_interactive), wx.ART_FILE_OPEN),  # 'import'),
    # (Command(ListForm.insert_line, before=True, copy=True), 'insert-line-before-copy'),
    # (Command(ListForm.insert_line, before=True), 'insert-line-before'),
    # (Command(ListForm.insert_line, copy=True), 'insert-line-copy'),
    # (Command(ListForm.insert_line), 'insert-line'),
    (Command(ListForm.toggle_column, position=None), 'hide-column'),
    (Command(ListForm.aggregate), 'aggregate'),
    (Command(ListForm.unaggregate), 'unaggregate'),
    (Command(ListForm.copy_aggregation_result), wx.ART_COPY),
    (Command(ListForm.aggregated_view), 'aggregate'),
    (Command(RecordForm.edit_record), 'edit-record'),
    (Command(RecordForm.new_record, copy=True), 'new-record-copy'),
    (Command(RecordForm.new_record), 'new-record'),
    (Command(RecordForm.delete_record), 'delete-record'),
    (Command(EditForm.commit_record), wx.ART_FILE_SAVE),
    (Command(LookupForm.sort, direction=pytis.data.ASCENDENT), 'sort-asc'),
    (Command(LookupForm.sort, direction=pytis.data.DESCENDANT), 'sort-desc'),
    (Command(LookupForm.sort, direction=LookupForm.UNSORT), 'unsort'),
    (Command(LookupForm.sort), 'sort'),
    (Command(LookupForm.filter_by_value), 'filter-by-cell'),
    (Command(LookupForm.select_filter), 'filter-form'),
    (Command(LookupForm.unfilter), 'unfilter'),
    (Command(LookupForm.search, next=True, back=True), 'search-backwards'),
    (Command(LookupForm.search, next=True), 'search-forward'),
    (Command(LookupForm.search), 'search'),
    (Command(LookupForm.jump), 'jump'),
    (Command(LookupForm.update_profile), 'save'),
    (Command(ProfileSelector.duplicate_profile), wx.ART_COPY),
    (Command(ProfileSelector.rename_profile), 'edit-text'),
    (Command(LookupForm.delete_profile), 'delete'),
    (Command(LookupForm.set_initial_profile), 'star'),
    (Command(LookupForm.reload_profile), 'revert-to-saved'),
    (Command(LookupForm.reset_profile), 'reset-profile'),
    (Command(LookupForm.export_profiles), wx.ART_FILE_SAVE),
    (Command(LookupForm.import_profiles), wx.ART_FILE_OPEN),
    (Command(MultiSideForm.filter_by_sideform), 'filter'),
    (Command(Browser.go_back), wx.ART_GO_BACK),
    (Command(Browser.go_forward), wx.ART_GO_FORWARD),
    (Command(Browser.stop_loading), wx.ART_CROSS_MARK),
    (Command(Browser.reload), 'reload'),
    (Command(Application.run_procedure, 'configui', 'ui_settings'), 'config-ui'),
    (Command(Application.run_procedure, 'configui', 'export_settings'), 'config-export'),
    # Config menu items refer to run_procedure through 'Application' in dmp_menu()
    # or through 'app' (Pytis API) in direct menu specifications.
    (Command(Application.api_run_procedure, 'configui', 'ui_settings'), 'config-ui'),
    (Command(Application.api_run_procedure, 'configui', 'export_settings'), 'config-export'),
)


class UICommands(object):
    PYTIS_HELP = UICommand(
        Command(Application.help, topic='pytis'),
        _("User interface handbook"),
        _("Open the user's manual describing work with the application in general"))
    HELP = UICommand(
        Command(Form.help),
        _("Help for the current form"),
        _("Open the detailed description of the current form in the help browser"))
    DESCRIBE = UICommand(
        Command(InnerForm.describe),
        _("Current form description"),
        _("Display a breif description of the current form"))
    AGGREGATION_MENU = UICommand(
        Command(InnerForm.aggregation_menu),
        _("Show aggregation functions menu"),
        _("Show the menu of aggregation functions for the current form"))
    PRINT_MENU = UICommand(
        Command(InnerForm.print_menu),
        _("Show print menu"),
        _("Show the menu of print reports for the current form"))
    JUMP = UICommand(
        Command(LookupForm.jump),
        _("Jump to record"),
        _("Jump to record by row number"))
    SEARCH = UICommand(
        Command(LookupForm.search),
        _("Search"),
        _("Search a record matching given conditions."))
    SEARCH_NEXT = UICommand(
        Command(LookupForm.search, next=True),
        _("Search next"),
        _("Search next record matching given conditions."))
    SEARCH_PREVIOUS = UICommand(
        Command(LookupForm.search, next=True, back=True),
        _("Search previous"),
        _("Search previous record matching given conditions."))
    INCREMENTAL_SEARCH = UICommand(
        Command(ListForm.incremental_search),
        _("Search incrementally"),
        _("Progressively search record by typing column value."))
    SORT = UICommand(
        Command(LookupForm.sort),
        _("Sort"),
        _("Define the terms of sorting records."))
    PROFILE_MENU = UICommand(
        Command(Application.nothing),
        _("Profile selection"),
        _("Shows the current profile and allows selection from predefined profiles."),
        ctrl=(ProfileSelector, dict(size=(270, 25))))
    FILTER = UICommand(
        Command(LookupForm.select_filter),
        _("Filter"),
        _("Filter records matching given conditions."))
    UNFILTER = UICommand(
        Command(LookupForm.unfilter),
        _("Unfilter"),
        _("Cancel current filtering condition."))
    EDIT_RECORD = UICommand(
        Command(RecordForm.edit_record),
        _("Edit record"),
        _("Open a form to edit the current record."))
    NEW_RECORD = UICommand(
        Command(RecordForm.new_record),
        _("New record"),
        _("Open a form to create a new record."))
    NEW_RECORD_COPY = UICommand(
        Command(RecordForm.new_record, copy=True),
        _("New record - copy"),
        _("Create a new record by copying the values of the current record."))
    DELETE_RECORD = UICommand(
        Command(RecordForm.delete_record),
        _("Delete record"),
        _("Delete the current record."))
    EXPORT_FILE = UICommand(
        Command(ListForm.export_file),
        _("Export to file"),
        _("Export form data into a file."))
    IMPORT_INTERACTIVE = UICommand(
        Command(RecordForm.import_interactive),
        _("Import from file"),
        _("Import data from a text file in CSV format."))
    OTHER_FORM = UICommand(
        Command(DualForm.other_form),
        _("Switch the active form"),
        _("Switch between the top and the bottom form of a dual form."),
        ctrl=DualFormSwitcher)
    SWITCH_KEYBOARD_LAYOUT = UICommand(
        Command(Application.nothing),
        _("Switch the current keyboard layout"),
        _("Switch between keyboard layouts defined in configuration."),
        ctrl=KeyboardSwitcher)
    RESPLIT = UICommand(
        Command(DualForm.resplit),
        _("Switch splitter orientation"),
        _("Switch between horizontal and vertical arrangement of the main form and side form."),
        ctrl=DualFormResplitter)
    LEAVE_FORM = UICommand(
        Command(Form.leave_form),
        _("Close form"),
        _("Close the current form window."))


TOOLBAR_COMMANDS = ((
    UICommands.NEW_RECORD,
    UICommands.EDIT_RECORD,
    UICommands.DELETE_RECORD,
), (
    UICommands.EXPORT_FILE,
    UICommands.PRINT_MENU,
    UICommands.OTHER_FORM,
    UICommands.RESPLIT,
), (
    UICommands.INCREMENTAL_SEARCH,
    UICommands.SEARCH,
    UICommands.SEARCH_PREVIOUS,
    UICommands.SEARCH_NEXT,
    UICommands.JUMP,
), (
    UICommands.AGGREGATION_MENU,
), (
    UICommands.FILTER,
    UICommands.PROFILE_MENU,
), (
    UICommands.PYTIS_HELP,
    UICommands.HELP,
    UICommands.DESCRIBE,
), (
    (UICommands.SWITCH_KEYBOARD_LAYOUT,) if pytis.config.keyboard_layouts else ()
) + (
    UICommands.LEAVE_FORM,
))

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
                      (UICommands.EXPORT_FILE,
                       UICommands.IMPORT_INTERACTIVE,
                       ),
                      )
