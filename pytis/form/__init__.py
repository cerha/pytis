# -*- coding: utf-8 -*-

# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2013 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""User interface implementation.

The module contains everything related to the user interface and everything
related to wxWidgets.  All UI handling is implemented in this module; only
generic specifications of individual UI elements are defined outside this
module.

"""

from __future__ import print_function

# Imports use 'X as X' form (PEP 484) to mark names as explicitly re-exported
# without maintaining a separate __all__ list.
from .event import (
    UserBreakException as UserBreakException,
    top_level_exception as top_level_exception,
    last_user_event as last_user_event, last_event_age as last_event_age,
    wx_callback as wx_callback, unlock_callbacks as unlock_callbacks,
    yield_ as yield_, idle_blocked as idle_blocked, block_idle as block_idle,
    standard_stop_check_function as standard_stop_check_function,
    interrupt_watcher as interrupt_watcher, interrupt_init as interrupt_init,
)

from .command import (
    CommandHandler as CommandHandler, UICommand as UICommand,
)

from .screen import (
    WxKey as WxKey, Keymap as Keymap, KeyHandler as KeyHandler,
    CallbackHandler as CallbackHandler, ToolTipWindow as ToolTipWindow,
    ToolTip as ToolTip,
    StatusBar as StatusBar, InfoWindow as InfoWindow,
    ProfileSelectorPopup as ProfileSelectorPopup, ProfileSelector as ProfileSelector,
    TextHeadingSelector as TextHeadingSelector,
    FormStateToolbarControl as FormStateToolbarControl,
    KeyboardSwitcher as KeyboardSwitcher,
    DualFormSwitcher as DualFormSwitcher, DualFormResplitter as DualFormResplitter,
    LocationBar as LocationBar,
    Browser as Browser, mupdfProcessor as mupdfProcessor,
    FileViewerButtonPanel as FileViewerButtonPanel, FileViewer as FileViewer,
    FileViewerFrame as FileViewerFrame, make_in_operator as make_in_operator,
    beep as beep, microsleep as microsleep, busy_cursor as busy_cursor,
    is_busy_cursor as is_busy_cursor, modal as modal,
    copy_to_clipboard as copy_to_clipboard, paste_from_clipboard as paste_from_clipboard,
    hotkey_string as hotkey_string, char2px as char2px, dlg2px as dlg2px,
    acceskey_prefix as acceskey_prefix, orientation2wx as orientation2wx,
    make_fullname as make_fullname, uicommand_mitem as uicommand_mitem,
    get_icon as get_icon,
)

from .dialog import (
    Dialog as Dialog, Message as Message, Warning as Warning, Error as Error,
    Question as Question, ProgressDialog as ProgressDialog,
    Calendar as Calendar, ColorSelector as ColorSelector,
    BugReport as BugReport, CheckListDialog as CheckListDialog,
    AggregationSetupDialog as AggregationSetupDialog,
    FileDialog as FileDialog, DirDialog as DirDialog,
)

from .search import (
    SFSColumn as SFSColumn, SFSDialog as SFSDialog, SortingDialog as SortingDialog,
    SFDialog as SFDialog, SearchDialog as SearchDialog,
    FilterDialog as FilterDialog, sfs_columns as sfs_columns,
)

from .inputfield import InputField as InputField, TextField as TextField

from .form import (
    FormProfile as FormProfile, FormSettings as FormSettings, Form as Form,
    InnerForm as InnerForm, Refreshable as Refreshable, PopupForm as PopupForm,
    LookupForm as LookupForm, RecordForm as RecordForm, EditForm as EditForm,
    PopupEditForm as PopupEditForm,
    InputForm as InputForm, QueryFieldsForm as QueryFieldsForm,
    ResizableInputForm as ResizableInputForm,
    PopupInsertForm as PopupInsertForm, ShowForm as ShowForm,
    BrowsableShowForm as BrowsableShowForm,
    ViewerForm as ViewerForm, WebForm as WebForm, FileViewerForm as FileViewerForm,
)

from .list import (
    ListForm as ListForm, FoldableForm as FoldableForm, CodebookForm as CodebookForm,
    SelectRowsForm as SelectRowsForm, BrowseForm as BrowseForm,
    SideBrowseForm as SideBrowseForm, AggregationForm as AggregationForm,
)

from .dualform import (
    DualForm as DualForm,
    ImmediateSelectionDualForm as ImmediateSelectionDualForm,
    PostponedSelectionDualForm as PostponedSelectionDualForm,
    SideBrowseDualForm as SideBrowseDualForm, BrowseDualForm as BrowseDualForm,
    AggregationDualForm as AggregationDualForm,
    ShowDualForm as ShowDualForm, BrowseShowDualForm as BrowseShowDualForm,
    DescriptiveDualForm as DescriptiveDualForm, MultiForm as MultiForm,
    MultiSideForm as MultiSideForm, MultiBrowseDualForm as MultiBrowseDualForm,
)

from .application import (
    Application as Application, run_form as run_form, db_operation as db_operation,
    # Backwards compatibility aliases.
    MSeparator as MSeparator, Menu as Menu, MItem as MItem,
    CheckItem as CheckItem, RadioItem as RadioItem, close_forms as close_forms,
    recent_forms_menu as recent_forms_menu, config_menu_items as config_menu_items,
)

from .defaults import (
    DEFAULT_KEYMAP as DEFAULT_KEYMAP, COMMAND_ICONS as COMMAND_ICONS,
    TOOLBAR_COMMANDS as TOOLBAR_COMMANDS, FORM_MENU_COMMANDS as FORM_MENU_COMMANDS,
    UICommands as UICommands,
)

from .managers import (
    UserSetttingsManager as UserSetttingsManager,
    ApplicationConfigManager as ApplicationConfigManager,
    FormSettingsManager as FormSettingsManager,
    FormProfileManager as FormProfileManager,
    FormProfileParamsManager as FormProfileParamsManager,
    AggregatedViewsManager as AggregatedViewsManager,
)

app = None
"""Current wx application instance for internal usage inside Pytis.

    As opposed to 'pytis.api.app', this variable points directly to the wx
    application instance and thus should only be used internally from Pytis code
    to access its public methods and attributes.

    It is recommended to always refer to this variable by its full name
    'pytis.form.app', while 'pytis.api.app' can be referred simply as 'app'.

"""
