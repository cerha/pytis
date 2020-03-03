# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Práce s uživatelským rozhraním.

Modul obsahuje vše co souvisí s uživatelským rozhraním a vše co má něco
společného s wxWidgets.  Všechny obslužné záležitosti uživatelského rozhraní
jsou implementovány v tomto modulu, mimo tento modul se definují pouze
specifikace jednotlivých prvků rozhraní.

"""

from .event import (  # noqa: F401
    UserBreakException, top_level_exception, last_user_event, last_event_age,
    wx_callback, unlock_callbacks, yield_, idle_blocked, block_idle,
    standard_stop_check_function, interrupt_watcher, interrupt_init,
)

from .command import (  # noqa: F401
    CommandHandler, Command, UICommand,
)

from .screen import (  # noqa: F401
    WxKey, Keymap, KeyHandler, CallbackHandler, MSeparator,
    Menu, MItem, CheckItem, RadioItem, MenuBar, ToolTipWindow, ToolTip,
    StatusBar, InfoWindow, ProfileSelectorPopup, ProfileSelector,
    TextHeadingSelector, FormStateToolbarControl, KeyboardSwitcher,
    DualFormSwitcher, DualFormResplitter, LocationBar, HelpProc,
    Browser, mupdfProcessor, FileViewerButtonPanel, FileViewer,
    FileViewerFrame, IN, beep, microsleep, busy_cursor,
    is_busy_cursor, modal, copy_to_clipboard, paste_from_clipboard,
    hotkey_string, file_menu_items, help_proc, char2px,
    dlg2px, acceskey_prefix, orientation2wx, make_fullname, mitem,
    popup_menu, get_icon, select_file, select_files,
    select_directory, make_selected_file, write_selected_file,
    open_selected_file, open_file, write_file, launch_file,
    open_data_as_file, launch_url,
)

from .dialog import (  # noqa: F401
    Dialog, Message, Warning, Error, MultiQuestion, Question, OperationDialog,
    ProgressDialog, RepeatedOperationDialog, Calendar, ColorSelector,
    BugReport, CheckListDialog, CheckMatrixDialog, AggregationSetupDialog,
    FileDialog, DirDialog,
)

from .search import (  # noqa: F401
    SFSColumn, SFSDialog, SortingDialog, SFDialog, SearchDialog,
    FilterDialog, sfs_columns,
)

from .inputfield import InputField, TextField  # noqa: F401

from .form import (  # noqa: F401
    FormProfile, FormSettings, Form, InnerForm, Refreshable, PopupForm,
    TitledForm, LookupForm, RecordForm, EditForm, PopupEditForm,
    InputForm, QueryFieldsForm, ResizableEditForm, ResizableInputForm,
    StructuredTextEditor, PopupInsertForm, ShowForm, BrowsableShowForm,
    ViewerForm, WebForm, FileViewerForm,
)

from .list import (  # noqa: F401
    ListForm, FoldableForm, CodebookForm, SelectRowsForm, BrowseForm,
    SideBrowseForm, AggregationForm,
)

from .dualform import (  # noqa: F401
    DualForm, ImmediateSelectionDualForm, PostponedSelectionDualForm,
    SideBrowseDualForm, BrowseDualForm, AggregationDualForm,
    ShowDualForm, BrowseShowDualForm, DescriptiveDualForm, MultiForm,
    MultiSideForm, MultiBrowseDualForm,
)

from .output import (  # noqa: F401
    PrintForm,
)

from .application import (  # noqa: F401
    Application, ApplicationProxy, DbActionLogger, InputDialog, InputNumeric, InputDate,
    run_form, run_dialog, run_procedure, input_text, input_number, input_date,
    new_record, delete_record, refresh, exit, db_operation, db_op,
    delete_record_question, current_form, top_window,
    recent_forms_menu, wx_frame, profile_manager, form_settings_manager,
    aggregated_views_manager, decrypted_names, log_user_action,
    frame_title, close_forms, set_status, refresh_status, message,
    global_keymap, block_refresh, _dump_rights,
    init_access_rights, has_access, action_has_access, wx_yield_,
    block_yield, custom_command,
    built_in_status_fields, remote_connection_initially_available,
    get_recent_directory, set_recent_directory, menu,
)

from .configui import (  # noqa: F401
    ConfigForm, config_menu_items, configurable_options,
)

from .commands_ import (  # noqa: F401
    UICommands, DEFAULT_KEYMAP, COMMAND_ICONS, TOOLBAR_COMMANDS,
    FORM_MENU_COMMANDS,
)

from .managers import (  # noqa: F401
    UserSetttingsManager, ApplicationConfigManager,
    FormSettingsManager, FormProfileManager,
    FormProfileParamsManager, AggregatedViewsManager,
)

app = ApplicationProxy()
