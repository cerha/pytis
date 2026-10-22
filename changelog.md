# Changelog

All notable user visible changes to Pytis.  The entries are brief, see the
documentation of the linked code for details.  The rules for maintaining this
file are described in [doc/standards/changelog.md](doc/standards/changelog.md).

## 1.3.1.dev1 (unreleased)

### Wx application
- Fixed a traceback after a database error during initialization of a side
  form in a [`MultiSideForm`](pytis/form/dualform.py).  The error is only
  reported to the user and the side form tab remains empty.
- Fixed dialogs not responding to mouse clicks when opened by a mouse click,
  such as a database error dialog after switching a side form tab.
- Fixed a traceback on a key press in a side form whose initialization failed.
- Fixed user profiles missing in a form whose user profiles filter by another
  user profile.  New profiles saved in such a form were lost or overwrote the
  missing ones ([`FormProfileManager`](pytis/form/managers.py)).

## 1.3.0 (2026-10-21)

### Wx application
- The application state is saved whenever a form is opened or closed, so the
  open forms, recent forms and window geometries survive an unclean exit
  ([`ApplicationState`](pytis/form/managers.py)).
- The form active on last exit is shown and focused on startup.
- The size and position of the help browser window is remembered.
- Actions with [`ActionContext.GLOBAL`](pytis/presentation/spec.py) are now
  supported in wx forms.  They are offered in the context menu of an empty
  form (or below the last row), in the selection context menu and below a
  separator in the row context menu.  Their handler receives the form as
  `pytis.api.Form`.
- Actions on row selection iterate lazily over a stable database snapshot
  without a row limit ([`Form.selection`](pytis/form/form.py)).  The user is
  warned when the data are refreshed during or after the row selection.
- The tabular variant of the check list dialog (`columns`) works again
  (requires wxPython 4.1).  Items may be disabled through
  [`CheckListItem`](pytis/presentation/spec.py), long values are truncated
  with the full value in a tooltip.  New API method
  [`app.checklist()`](pytis/api/api.py).
- New configuration option [`stylesheets`](pytis/util/configuration.py) and
  specification methods
  [`Application.stylesheets()`](pytis/presentation/spec.py) and
  [`Application.resource_path()`](pytis/presentation/spec.py) for styling the
  content displayed in the built-in browser.  Style sheets given in the
  `resources` of `lcg.Content` are applied too.
- Pytis2Go clients communicate over JSON-RPC.  The protocol is detected
  automatically and shown in the status bar, RPyC remains supported including
  the clients older than 2.3.0 ([`pytis.remote`](pytis/remote/remote.py)).
  [`tools/x2go-ping.py`](tools/x2go-ping.py) works with both protocols.
- Viewer form content is defined by
  [`ViewerContent`](pytis/presentation/spec.py) subclasses (`WebContent`,
  `PdfContent`, `XMLContent` and their `ColumnContent` variants) passed to
  `Binding`.  The `Binding` argument `content_type` is deprecated.
- Navigation policy of viewer forms and
  [`app.web_view()`](pytis/api/api.py): `restrict_navigation` may be a
  function, `SAME_ORIGIN` or `NO_NAVIGATION`, and the new `on_navigation`
  hook may replace a navigation by an action.  The browser implements
  [`pytis.api.Browser`](pytis/api/api.py) and `app.web_view()` accepts an
  URI.
- XML documents are displayed with syntax highlighting
  ([`XMLContent`](pytis/presentation/spec.py),
  [`xml_to_html()`](pytis/util/util.py), uses `pygments` when available).
- New API method [`app.popup_menu()`](pytis/api/api.py).
- The published Pytis API is recorded in
  [`pytis/api/signature.txt`](pytis/api/signature.txt) and guarded by tests,
  [`tools/check-api.py`](tools/check-api.py) reports the incompatible changes
  against a given revision.
- The dynamic API namespaces `app.status`, `app.param`, `form.field` and
  `form.selection` have their API classes for type checkers
  ([`StatusFields`](pytis/api/api.py), `Params`, `Fields`, `Selection`).
- Callable layout items ([`GroupSpec`](pytis/presentation/spec.py)) are
  supported in wx forms.
- Array values are formatted for display in the grid.
- Local files are launched on macOS.
- **Incompatible:** API implementations must derive from
  [`pytis.api.APIImplementation`](pytis/api/api.py).  Implementation errors
  are reported as `TypeError`.
- **Incompatible:** `pytis.api.BaseApplication` moved to
  [`pytis.application.BaseApplication`](pytis/application.py).
- The configuration option [`resource_path`](pytis/util/configuration.py) only
  needs to list application specific directories, Pytis built-in directories
  are searched automatically ([`resource_dirs()`](pytis/util/util.py)).
- The custom URI schemes `form:`, `call:` and `help:` are only honored in the
  content produced by the application, not in remotely loaded pages.
- [`Form.initialized()`](pytis/form/form.py) only returns true when the
  initialization finished successfully, the former meaning is available as
  `initialization_started()`.  Multiforms no longer return partially
  initialized subforms.
- The active form is focused when the main frame is activated, so the
  keyboard commands work right after startup.
- The keyboard focus returns to the active form after clicking a notebook tab.
- Fixed selection context actions operating on another row than the
  highlighted one.
- Fixed actions on row selection in forms over table functions.
- Fixed rows from wrong positions displayed after prepending into a full
  fetch buffer.
- Fixed slow grid operations with wxPython 4.1 and newer.
- Fixed `main_form` and `side_form` API properties of dual forms.
- Fixed editing and deleting ListField codebook items.
- Fixed the first help topic display in a new help browser window and links
  to sections within specification help pages.
- Fixed PDF rendering with PyMuPDF 1.14.17 and newer.
- Fixed `readlines()` of files opened by `app.open_file()`.
- Fixed moving the focus away from a query field on autoapply refresh.
- Fixed status bar updates from other than the main thread.
- Fixed crash on startup when the environment defines no language (macOS).
- Fixed falling back to another Pytis2Go UI backend when Tcl/Tk is broken.

### Common
- New function [`sql.select()`](pytis/data/gensqlalchemy.py) for more
  convenient view query definitions.  The helpers `_exclude()`, `_alias()` and
  `reorder_columns()` are deprecated.
- New tool [`gsql-migrate`](tools/gsql-migrate.py) building schema migrations
  from templates, `NEW`/`OLD` shorthands in rule bodies.  It sets the
  comments of the altered tables and generates PL/Python functions as
  `plpython3u`.  Comments alone are generated by `gsql --comments`.
- New methods [`Data.first_row()`](pytis/data/data.py) and
  `Data.unique_row()`, new function
  [`pytis.data.create()`](pytis/data/dbdata.py).
- [`Data.rows()`](pytis/data/data.py) accepts `limit` and `offset`,
  [`Data.select_aggregate()`](pytis/data/data.py) accepts `arguments`.
- New abstract base class [`pytis.data.Transaction`](pytis/data/dbdata.py).
- New data type [`Iban`](pytis/data/types_.py).
- [`dbfunction()`](pytis/data/dbdata.py) accepts `connection_name` and
  `connection_data`.
- `RestrictedData.update()` returns the reason when access rights block all
  updated columns, `insert()` logs it
  ([`pytis/data/access.py`](pytis/data/access.py)).
- New declarative command line wrapper for scripts
  [`pytis.util.CLI`](pytis/util/util.py).
- Pytis API and widely used functions have type annotations, the package is
  marked as typed (PEP 561).
- Database specifications and gensqlalchemy migrated to SQLAlchemy 2.0.
- `MemData` initial data are loaded regardless of access rights.
- Runtime data (resources, translations) are kept under the package
  directory `assets`.
- Docstrings converted to Google style
  ([doc/standards/docstrings.md](doc/standards/docstrings.md)).
- Development dependencies are defined as dependency groups in
  [`pyproject.toml`](pyproject.toml), see [readme.md](readme.md) for the build
  steps.
- Depends on `standard-mailcap` in Python 3.13 and newer.
- Fixed passing arrays of range values to database queries and functions.
- Fixed "current transaction is aborted" errors on new connections with a
  password in databases not using Pytis crypto.
- Fixed a successful `DBParams` update reported as a failure.
- Fixed the deprecated method `Binary.Data.buffer()`.
- Fixed `gsql` hanging during database specification traversal.
- Several Python 3 fixes.

### REST API
- Multi-column keys for top-level relations, composite key path parameters
  are documented in OpenAPI.
- New configuration option [`api_key`](pytis/util/configuration.py), a dict
  of sender names to keys.  A missing or wrong key is answered by 401.
  Operations may be restricted to particular senders
  ([`require_identity()`](pytis/rest/rest.py)).
- [`ResourceSpec.sources`](pytis/rest/rest.py) declares where each column
  value comes from: `Body`, `Header`, `Raw`, `Derived`, `Default` or
  `Identity`.
- `ResourceSpec.unreturned` leaves a column out of responses.
- `ResourceSpec.create_status` and `update_status` let the written record
  decide the response status ([`Status`](pytis/rest/rest.py)).
- Writes through view based resources.
- A JSON object in the DETAIL of a database error becomes the response body,
  a value refused by the database is answered by 422.  The schema declares
  the possible error codes.
- Columns not supplied by a write are read back from the database.
- Write operations are logged.

### Web
- The page and the number of records per page are selected by links in
  dropdowns.
- The index search is a dropdown among the paging buttons.
- Browse form table rows are stacked as cards when the table doesn't fit.
  Stacking may be disabled by the CSS custom property
  `--pytis-table-stacking: none`, the table then scrolls horizontally.
- Field labels are displayed above the fields on narrow screens and edit form
  fields shrink to the available width.
- Only icons of the paging buttons are displayed on narrow screens.
- The list of page numbers displayed without JavaScript is abbreviated.
- Date input is accepted with or without the spaces of the locale format.
- The found record of a browse form is focused (for screen readers) and the
  calendar button of date fields is labeled.
- `pytis.cms` passes the current menu record and the attachment storage
  authorization through `req.vars`.
- **Incompatible:** New URI kind [`UriType.RECORD`](pytis/web/field.py) for
  the URI of the record itself (form submission, action buttons, asynchronous
  requests).  URI providers must handle it, including `record` being `None`.
- **Incompatible:** Profiles and filters are selected by links in dropdowns
  when all query fields are enumerations, the
  [`BrowseForm`](pytis/web/form.py) argument `immediate_filters` was removed.
- **Incompatible:** Form icons are generic LCG icons, the images in
  `resources/images` were removed.
- JavaScript rewritten to ES6 and minified by `rjsmin`, its translations
  exported by `pojson`.
- Python 2 compatibility code removed from `pytis.web` (web applications
  are Python 3 only since 2020).
- Fixed descending sorting on the default sorting column.
- Fixed bypassing the `export()` method of `ListView` subclasses.

## History from 1.1.0 to 1.3.0.dev1

Rough overview of the main topics of each year, by the year when the work was
merged.  Only a few releases were made in this period, see the respective
years.

### 2026 (until 1.3.0.dev1)
- Wx: Row selection in list forms: new command `ListForm.select_all()`
  (Ctrl-a), `unselect_selected_rows()` renamed to `clear_selection()`,
  selection items in the context menu of `SelectRowsForm`.  Selection
  iteration no longer allocates row lists for large selections, yields the
  originally selected rows even when the data change meanwhile and handles
  rows deleted before being processed.
- Wx: Interactive import (`import_interactive()`): the input encoding may be
  chosen and decoding errors are detected upfront, the separator selection is
  easier, invalid lines may be inserted and fixed interactively, skipped or
  the batch cancelled, and the position within the input file is displayed.
- Wx: New argument `arguments` of `app.run_form()`.
- Wx: **Incompatible:** The DMP menu is no longer constructed automatically,
  DMP applications call `pytis.extensions.dmp_menu()` in their
  `Application.menu()`.
- Wx: Dialog icons are included in Pytis, as newer systems (Debian 13) lack
  them in 48 px.
- Wx: Support for PyMuPDF 1.24.3 and newer (module `pymupdf` instead of
  `fitz`) and for newer wx PDF viewer versions.
- Wx: Remote API methods `session_password`, `enter_text` and `select_option`
  backported for older Pytis2Go clients.
- Pytis is a flit package installed by pip (dependencies in `pyproject.toml`
  with the extra `wx`), the sources moved from `lib/` to the repository root.
  `setup.py`, `requirements.txt` and `install.md` were removed, see the
  readme for installation.  The legacy `bin/pytis` launcher and `lib/pytis`
  symlink keep existing deployments working.
- Default resource, icon and translation directories are located within the
  installed package (`assets`).  `PYTIS_TRANSLATION_PATH` is still accepted,
  Pytis and LCG translations are added to it automatically.
- `make update` updates the generated files after git checkout without
  building documentation.
- `pytis.data.transaction()` is used throughout Pytis,
  `DBPostgreSQLTransaction` is a context manager committing or rolling back
  on exit.
- Translation source messages of `pytis.defs` and `pytis.cms` are in English,
  remaining Czech docstrings and comments translated.
- Fixed generating random keys and passwords in Python 3 (`tools/crypto`).
- Depends on `standard-cgitb` in Python 3.13 and newer.
- New tool `tools/dump-search.py` searching database dumps (with optional
  prefiltering by grep or ripgrep), `tools/dump-subset.py` handles sequences
  referenced by column defaults and sequence adjusting is optional.
- New functions dumping the application structure (menu, views, database
  objects, columns and relations) to Markdown (`pytis/extensions/dump.py`).
- Pytis Demo: action processing a long table row selection, Pytis CMS
  disabled (it requires Wiking).
- REST API: New FastAPI based framework `pytis.rest` for declarative HTTP
  REST APIs over database specifications.  Resources may combine data from
  multiple tables into nested JSON structures with CRUD operations over the
  whole hierarchy.  Pydantic models are generated from the specifications
  (column docs become field descriptions, internal keys are hidden).
  Authorization, API key authentication, explicit sorting, filtering and
  column exclusion are supported.  The dependencies are the optional `rest`
  extra.

### 2025
- Wx: Forms of the main window are displayed in notebook tabs instead of a
  window stack.  The tabs have a context menu, show the form title as their
  tooltip and long tab titles are shortened dynamically.  The active form is
  remembered between runs.  The Window menu was removed and the logo in the
  empty main frame (option `logo`) is no longer displayed.
- Wx: The form activation commands were renamed from `raise_*` to
  `activate_*`, `raise_next_form` and `raise_prev_form` were merged into
  `Application.activate_next_form(back=False)`.  Their keys changed from
  Ctrl-Down/Ctrl-Up to Ctrl-Alt-Right/Ctrl-Alt-Left.
- Wx: New Pytis API attribute `Form.title` and method `Form.activate()`
  (returns false when blocked by a modal form), new argument `name` of
  `app.web_view()`.
- Wx: Command definition and handling rewritten.  Command methods are marked
  by `@Command.define` in `CommandHandler` subclasses and commands are
  `pytis.presentation.Command` instances holding their arguments.  Menu items
  are defined through the public API, e.g.
  `MenuItem(title, Command(app.run_form, 'spec.Name'))`.
  **Incompatible:** The `COMMAND_*` class constants and the `MenuItem`
  argument `args` were removed (the arguments go to the `Command`).  Legacy
  aliases remain for `Application.COMMAND_NEW_RECORD`, `COMMAND_RUN_FORM`,
  `COMMAND_RUN_PROCEDURE`, `COMMAND_EXIT`, `COMMAND_HANDLED_ACTION`,
  `COMMAND_RELOAD_RIGHTS` and `COMMAND_CLEAR_RECENT_FORMS`.
- Wx: New API method `app.call()` calling any function, e.g. as a menu command
  `Command(app.call, function, ...)`.  **Incompatible:** `ConfigForm`,
  `Application.handled_action` and application defined custom commands were
  removed in favor of `app.call()` and `app.input_form()`.  Configuration
  editing moved to `pytis.defs.configui` (procedures `ui_settings` and
  `export_settings`).
- Wx: Menus with application managed content are identified by the `Menu`
  argument `id` (constants such as `Menu.RECENT_FORMS_MENU`).
  **Incompatible:** The `Menu` argument `name` was renamed to `id`.
  `pytis.form.recent_forms_menu()` is deprecated.
- Wx: Dialog buttons rewritten: buttons are recognized by their values rather
  than labels and may have icons and tooltips.  Icons were added to the
  standard dialogs, edit form submit buttons and the batch insertion, bug
  report, editor conflict and Find/Sort/Filter dialogs.  The argument
  `compact` of `wx_button()` was removed.
- Wx: `Question` and `MultiQuestion` merged into `Question`, whose answers may
  be given as dictionaries with `label`, `value`, `descr` (tooltip) and
  `icon`.  **Incompatible:** `MultiQuestion` was removed, the `GenericDialog`
  arguments `report` and `report_format` were replaced by `content` (as in the
  Pytis API) and `report_size` was removed.
- Wx: Progress dialog reimplemented in Python with reliable refreshing.  Time
  measures are localized, their precision is set by the new `app.run()`
  argument `time_precision`, a progress of -1 switches to an indeterminate
  mode.  `OperationDialog` was removed (use `progress=False`).  The progress
  of `app.run()` with `over` is computed correctly.
- Wx: A single text editor replaced `ResizableEditForm`,
  `ResizableInputForm` and `StructuredTextEditor`.  Multiline fields of
  `app.input_text()` are resizable and its new arguments `text_format`,
  `attachment_storage` and `compact` are accepted, `label=None` gives an
  unlabeled field in `app.input_*()`.  **Incompatible:** `ResizableEditForm`
  and `StructuredTextEditor` were removed.
- Wx: Form profiles can be exported to and imported from JSON files, the
  initial profile is marked by a star, profile actions have icons.
- Wx: New configuration option `icon_path` (directories searched for `.png`
  or `.svg` icons), so applications can use their own icons.
- Wx: `--startup-forms` recognizes multiforms automatically, the forms don't
  need to be present in the menu and are opened on top of the saved forms.
- Wx: The remote connection status tooltip and the log show the client
  version, OS, X2Go session and version, local and remote Python and RPyC
  versions and the UI backend.
- Wx: New configuration option `allow_local_file_dialogs` (false in X2Go
  sessions by default) controlling whether file dialogs fall back to the
  server file system when no remote connection is available.
- Wx: `app.echo()` works in standalone scripts (`BaseApplication`).
- Wx: **Incompatible:** The form constructor argument `spec_kwargs` was
  removed, also from `app.new_record()`.  `TitledForm` was removed.
- Wx: Specification reloading fixed in Python 3, works also for `pytis.demo`.
- Wx: The help is generated from the current application menu, the
  application menu is used when the DMP menu tables are empty.
- Wx: Locale aware sorting works on macOS (new `pytis.util.strxfrm()`), fixed
  `Float` validation in macOS locale formats.
- Wx: Fixed grid column resizing with wxPython 4.2, keyboard commands in web
  forms, toolbar buttons all disabled when the last one was disabled.
- Wx: Fixed actions using the `rows` iterator without a selection, the Ok
  button and exit question of insert forms with `inserted_data`.
- Wx: Remote file fixes: writing `pytis.data.Binary.Data` values, writing in
  text mode with an encoding in Python 3, a lost connection in
  `app.open_file()`.
- Wx: Fixed an endless loop when closing idle transactions in Python 3, the
  dialog return value on Escape (`None`), CSV import errors, browser reload
  and the local file viewer for unknown MIME types.
- Wx: Form action logging no longer fails when the log table doesn't exist.
- New iterator `Data.rows()` over selected rows, usable as a context manager,
  replacing `fetchone()` loops throughout Pytis.
- `pytis.util.data_object()` accepts gensqlalchemy table and view
  specifications.
- New function `pytis.util.send_bug_report()`.  The configuration option
  `sender_address` defaults to the database user at the host's domain.
- `DBParams` raises `ProgramError` when reading or updating its row fails.
- `gsql pytis.dbdefs` generates SQL for Pytis' own database objects.
- **Incompatible:** Removed `pytis.util.resolver()`, the `resolver` argument
  of `Specification.__init__()`, the deprecated `LayoutSpec` (use `GroupSpec`
  directly), `pytis.util.super_()` and `pytis.util.XStack`.
- Removed the tool `tools/config-updater.py` and the configuration file
  templates.
- New argument `prefix` of `pytis.util.public_attributes()` and
  `public_attr_values()`.
- Crypto functions work with both PyCrypto and PyCryptodome.
- Python 2/3 compatibility fixes in gensqlalchemy, `JSON` values and schema
  validation, `pytis.util.Structure` and the resolver.
- `requirements.txt` pins SQLAlchemy 1.4, and RPyC 3.3.0 and PyMuPDF 1.14.20
  for Python 2.7, pygobject <= 3.50 on Linux.
- Pytis help updated (aggregated views, dual forms, keyboard shortcuts).
- GitHub test workflows for Python 2.7 and 3, which can also run locally by
  `act` (see `doc/tutorials/testing.org`), tests converted to pytest.
- New tool `tools/dump-subset.py` dumping the rows of a table matching
  `--where` together with all rows they reference by foreign keys (cycles
  handled).  The output is COPY blocks or INSERTs with
  `--on-conflict-do-nothing` for merging into an existing database, sequences
  are adjusted, the connection options are psql-like.
- Pytis Demo: reorganized menu, a panel of dialog tests and a web form demo.

### 2024
- Wx: New argument `preview` of `app.run_form()` (a read-only preview of the
  current record in a side form).  New argument `force` of `app.exit()`
  (exit without asking, still saves the user configuration).
- Wx: `app.run_form()`, `app.new_record()`, `app.edit_record()` and similar
  methods accept a specification class or instance in place of the
  specification name.  `app.edit_record()` allows overriding the layout.
- Wx: New Pytis API property `app.forms` and method `Form.close()`.
  `pf.close_forms()` is deprecated, use the Pytis API instead.
- Wx: New argument `transaction` of `app.input_form()`.  `app.run()` with
  `over` no longer allows passing `progress` to `update()` (progress is
  updated automatically).  The experimental argument `new_thread` of
  `app.run()` was removed.
- Wx: Basic support for the dark theme (requires the optional `darkdetect`
  module, without it nothing changes).
- Wx: The application frame size and position are saved and restored.
- Wx: **Incompatible:** Removed unused `pytis.form` functions and classes,
  including `run_dialog` (use `pf.app.run_dialog`), `refresh`, `exit`,
  `launch_url`, `select_file` and similar, `new_record`, `delete_record`,
  `run_procedure`, `help_proc`, `printout`, `message`, `current_form`,
  `top_window`, `refresh_status`, `set_status`, `has_access`, `IN`,
  `InputDialog`, `InputNumeric`, `InputDate` and `LookupForm.SORTING_*`.
- Wx: Status bar help texts are displayed for popup menus.
- Wx: Bug report dialog is displayed for errors in runtime filters and check
  functions.  Errors in computer functions are no longer suppressed in
  `row.get()` and `row.format()`.
- Wx: Web forms embedded in wx forms keep their content on refresh and on
  non-navigation actions.
- Wx: Remote access: obsolete Pytis2Go client versions are announced and
  Pytis2Go gets the Python version from a file.
- Wx: Fixed `app.run_form()` with `multi=False`, `app.launch_file()` in
  `local` mode, `app.run()` with `over` in Python 3 and completion sorting
  case sensitivity.
- **Incompatible:** DMP code moved to `pytis.extensions.dmp`, application DMP
  commands are limited to a known subset and `ConfigForm` is excluded from
  `pe.get_menu_forms()`.
- Logging of all exports to the database (table `e_pytis_action_log`).
  **Incompatible** for existing databases with that table: drop its column
  `info` and add a column `data` of type `json`.
- Subscript and superscript support in output markup.  Print specification
  `body()` may return a tuple or list, so `pytis.output.NewPage()` works at
  the top level.  `pytis.output.LCGFormatter` renamed to `Formatter`.
- New type `pytis.data.PgName` for legacy specifications, argument
  `precision` of timestamps in gensqlalchemy, `JSON`/`JSONB` in the
  gensqlalchemy type map, `pytis.data.binval()` exported.  `t_changes.id`
  and `log_trigger` use `BigInteger`.
- Fixed `pd.dbfunction()` for functions returning void and for functions
  given by name.  `DBParams` assignment to an invalid column raises
  `AttributeError`.  `PresentedRow` got `items()` and `values()`.
- **Incompatible:** Removed legacy code of user settings managers (settings
  must be migrated to JSON), the configuration option `cache_spec_onstart`
  and `pytis.extensions.cache_spec`, the unused `pytis.extensions` functions
  `nr`, `sf`, `ddf`, `ef`, `pytis.util.Stack` and
  `pytis.data.gensqlalchemy.include()`.
- Python 3.12 support (`importlib` instead of `imp`), `pycryptodome` instead
  of `pycrypto`, support for newer `pymupdf` and `PyPDF2` versions.
- Web: Fixed `pytis.cms.specname` conflict with `Specification._spec_name()`.

### 2023
- Wx: The Pytis API covers most of what applications used to do through
  `pytis.form` functions and `run_dialog()`.  New methods `app.run()` (runs a
  function with a progress dialog, optionally repeated `over` a sequence of
  arguments), `app.launch_file()`, `app.launch_url()`, `app.input_form()`,
  `app.splitpath()`, `app.has_access()`, `app.edit_record()`,
  `app.new_record()`, `app.delete_record()`, `app.show_record()`,
  `app.run_form()`, `app.codebook()`, `app.run_procedure()`,
  `app.printout()`, `app.decrypted_areas()` and `app.web_view()`.  The
  preferred import convention is `from pytis.api import app`.
- Wx: `app.question()` supports multiple choice questions and the arguments
  `title` and `timeout`, `app.message()`, `app.error()` and `app.warning()`
  accept `title` and `content` (including `lcg.Content`).  File selection
  methods moved to the API with only `filetypes` supported.
- Wx: **Incompatible:** The input functions `InputDialog`, `InputNumeric` and
  `InputDate` of `pytis.form` were replaced by `app.input_text()` and related
  API methods.  The unused form constructor argument `data_kwargs` and
  `RepeatedOperationDialog` were removed.
- Wx: Status bar fields, the application title and forms are accessible
  through the API: `app.status` (field `text`, `icon` and `tooltip`),
  `app.title`, `app.main_form`, `app.side_form`, `form.profile` and
  `form.select_row()`.  `pytis.form.built_in_status_fields()` is deprecated,
  call `super().status_fields()` in applications.  Status fields are
  initialized after database login, so `Application.status_fields()` may use
  the database.
- Wx: Progress dialog: new argument `maximum`, `message` replaces `newmsg` in
  the update call, fixed abortion and pulsing with recent wx versions.
- Wx: Batch insertion (`inserted_data` of edit forms, `app.input_form()` and
  interactive CSV import) accepts iterators of unknown length, loads one
  record at a time, reports the saved and remaining records and asks about
  unsaved changes.
- Wx: Files returned by `app.open_selected_file()` support `mode` and
  `encoding` (also with old Pytis2Go clients), the iterator protocol and
  transparent buffering making remote access much faster.  `app.open_file()`
  supports `encoding` and its default mode is `'r'`.  CSV import no longer
  creates a temporary file in remote mode.
- Wx: A dialog is displayed when editing a locked record fails.
- Wx: The error dialog for invalid values always names the column, XLSX
  export no longer splits `Range` fields, new configuration option
  `xlsx_export_author`.
- Wx: Fixed editing records with binary fields from list forms, ListField
  codebook record actions, menu events in popup forms, the keyboard context
  menu in empty forms, aggregation on table functions without arguments and
  the DMP menu falling back to the static menu when empty.
- Explicit table locking specification `SQLView.lock_tables` and `lock_key`
  replaced the unreliable and slow automatic detection of locked tables.
- New class `pytis.presentation.Folding` distinguishing everything unfolded
  from the default folding in profiles.
- The symbolic `IN` operator moved to `pytis.presentation`, saved profiles
  with it and with null filters load correctly.
- New method `Enumeration.label()`.
- `insert_many()` inserts multiple rows in a single statement.  Fixed INSERT
  into views with a serial key without `RETURNING`.
- Logged database operations include row data only in the DEBUG mode.
- `tools/config-updater.py`: options `--user` and `--dry-run`, table
  selection, handling of invalid specifications.
- New statement level logging trigger for tables with large bulk changes,
  `plpython3u` as the language of the control data functions.
- Fixed `%` handling in gensqlalchemy table definitions and materialized view
  refresh, `rsa_encrypt()` accepts non-ASCII passwords.

### 2022
- Wx: New context menu actions manipulating the row selection (add or remove
  the current row, clear the selection).  Actions with `context=SELECTION`
  get the default icon `selection.png`, row actions are not offered while a
  selection is active.  The selection is no longer created by mouse drag
  (use Ctrl or Shift).
- Wx: The query fields panel can refresh materialized views.  Forms over a
  `materialized_view` show the time of the last refresh and a refresh button,
  new argument `concurrently` of the data object method `refresh()`.
- Wx: New Pytis API methods `app.refresh()` and `app.exit()`, form properties
  `row`, `selection`, `main_form` and `side_form` and the form method
  `refresh()`.
- Wx: New function `pf.temp_file_launcher()`, `pf.open_data_as_file()`
  accepts a file-like object, `pf.select_file()` and `pf.select_directory()`
  (and the corresponding `pytis.remote` functions) accept an initial
  `directory`.
- Wx: Autocompletion in text fields uses the wxPython built-in implementation.
  The field text selection is kept when the field loses focus.
- Wx: Remote client: new macOS UI backend (JXA scripting through `osascript`),
  viewers launched in a new instance on macOS, `TkUIBackend` dialogs stay on
  top.
- Wx: Remote connection status detection reworked: `client_available()` means
  the connection is possibly available, the former meaning is available as
  `client_connection_ok()`.  An X2Go session without Pytis2Go is treated as
  local and a lost connection is reported consistently.
- Wx: `pytis.remote.open_selected_file()` and related functions: new argument
  `filetypes` replacing `patterns` and `pattern` (the old arguments still
  work), the unused `template` removed.
- Wx: **Incompatible:** Removed the obsolete configuration options
  `clipboard_primary_selection` and `use_wx_clipboard`, the arguments
  `initial_folding` (use `folding`) and `codebook_runtime_filter` (use
  `runtime_filter`), the `BindingSpec` argument `sash_ratio` and
  `CodebookSpec.arguments()`.
- Wx: User settings (application options, form settings, profiles and
  aggregated views) are saved as JSON, the old formats are still read.
- Wx: Fixed browser navigation, menu icons under GTK, attachment dialog
  usability and incremental search in aggregated views.
- New data types `UUID`, `JSON` and `JSONB` (values are Python dicts or lists,
  optional JSON schema validation).
- Database specifications: `plpython3u` support (gsql option `--plpython3`),
  new gsql option `--limit-class`, fixed `when` clause of triggers,
  `with_oids` dropped (PostgreSQL 12 compatibility).
- The column `doc` of database definitions is used as the field description
  and view columns inherit `label` and `descr` from the table columns.
- Python 3 compatibility fixes (binary files, `tools/dmp`, gensqlalchemy
  function bodies, binary values), `pygobject` added to the dependencies.
- Removed obsolete modules `pytis.extensions.gensql` and
  `pytis.extensions.www`.
- Web: **Incompatible:** Pytis CMS database definitions are extended in place
  by `pytis.dbdefs.db_pytis_cms.use_cms()` and the decorator `extend()` (e.g.
  `@CmsRoles.extend`) instead of overriding classes.
- Web: `DbAttachmentStorage` support in Pytis CMS (`db:table:column` format of
  `PYTIS_CMS_ATTACHMENTS_STORAGE`).

### 2021
- Wx: Password fields have an eye icon toggling the display of the value.
- Wx: The search, filter and sort dialog content is scrollable, so the dialog
  no longer exceeds the screen with many condition items.  The aggregation
  setup dialog is no longer tiny.
- Wx: No "Unsaved changes" dialog when closing `InputForm` and no unjustified
  "unapplied changes" dialog in forms with query fields and `autoinit`.
- Wx: A database connection closed by PostgreSQL for inactivity is retried
  (`DBRetryException`) when setting `search_path` and when fetching rows
  during scrolling, so the exception mostly no longer reaches application
  code.
- Wx: Fixed the PDF viewer with newer PyMuPDF versions, a major slowdown of
  list forms with wxPython 4.1.0 and newer, and modality after closing a
  codebook form opened above another modal dialog.
- Wx: **Incompatible:** Removed remote functions `library_version()` and
  `x2goclient_version()` from `pytis.remote`, the client version is kept in
  `RPCInfo`.
- Wx: Pytis2Go client UI backends work on macOS: Zenity is allowed and Tk
  preferred, the Win32 backend is used only on Windows, files are opened by
  `open`, "All files" filters allow files without extension, and the Tk and
  Zenity backends work in Python 2 and 3.
- Wx: Crypto is initialized right after database login, the login dialog
  behaves correctly after an invalid password.
- New function `pytis.data.dbfunction()` for calling database functions by
  name or by function specification, with positional or keyword arguments
  and the `transaction` argument.  `dbtable()`, `dbfunction()` and
  `transaction()` live in `pytis.data`, `pytis.extensions.dbfunction` is
  deprecated.
- **Incompatible:** Removed `DBConfig`, `cfg_param` and the extension
  functions accessing database parameters from `pytis.extensions`, as well as
  the unused `smssend()`, `load_field()` and `save_field()`.
  `PostgreSQLUserGroups` is no longer public in `pytis.data`.
- `pytis.util.send_mail()`: new arguments for the SMTP server, port and
  message id, `Attachment` accepts any `mime_type`, fixed sending to multiple
  recipients, sender name and date formatting.  Bug report emails are sent
  through it as well.
- New `pytis.util.NullLogger`.
- `pytis.extensions.GPGEmail` works with gnupg newer than 2.1.19.
- Crypto: the plain text key is no longer used when `db_key` is not defined in
  the database, crypto functions don't use `SQLRaw`, new
  `pytis.data.reset_crypto_password()`.
- The `connection_data` argument of `pytis.data.dbtable()` is optional.
- Depends on `psycopg2-binary` instead of `psycopg2`, support for older
  psycopg2 versions dropped.  `pytis.config` no longer depends on LCG.
- PDF output: argument `compact` of `Table` and `LongTable` suppresses the
  default cell padding, letter list item indexes.
- gensqlalchemy: SQLAlchemy 1.4 support, casting to `Time` and `DateTime`,
  `SQLTrigger` with a WHEN condition, no default `RETURNING` clause in insert
  rules.
- gensqlalchemy: **Incompatible:** `SQLFunction.from_clause` no longer
  returns a select, use the new `from_clause_select` for the previous
  behavior.
- Web: Fixed inline editing in web forms.

### 2020
- Wx: New public Pytis API `pytis.api` for applications, accessed as
  `pytis.api.app`.  Only what is marked by `pytis.api` decorators is public.
  It covers shared parameters, forms (`name`, `field`, `condition`, sorting,
  `clear_selection()`), fields (`Field.write()`, `Field.on_list_change()`)
  and basic dialogs (`app.message()`, `app.warning()`, `app.error()`,
  `app.question()`).  It is usable from actions with `context=SELECTION` and
  in scripts after `app.init()`.  A tutorial and examples were added.
- Wx: New shared parameters `app.param` (`DBParams`) replacing
  `pytis.extensions.DBConfig`, with row caching, multiple callbacks per value,
  `DBParam.cbvalue()` and `DBParams.reload()`.
- Wx: New function `pytis.form.printout()` replacing
  `pytis.extensions.printdirect()` (removed), all print use cases share the
  same resolvers.
- Wx: Query fields: the Apply button may trigger a callback, no data are shown
  before applying the query fields when `autoinit` and `autoapply` are false.
  The argument provider is called even without `query_fields`.
- Wx: Experimental `hidden` mode of `pytis.form.Application` for running
  application tests without visible windows.
- Wx: Export to CSV/XML can save the file (also in remote mode), the default
  `export_encoding` is UTF-8, the option `export_directory` and the command
  `EXPORT_CSV` were removed.
- Wx: **Incompatible:** `pytis.form.message()` no longer supports the
  arguments `timeout`, `log_`, `kind` and `data`.  `row.form()` access to
  form instances was disabled, use the API through `row.form` instead.
- Wx: Remote client: the client API may be pushed from the server, the Pytis2Go
  RPyC service may run in a separate process, `pytis.remote.select_file()`
  and `select_directory()` accept `title`.  The "direct connection" mode and
  the `rpc_*` options were dropped.
- Most of the code made Python 3 compatible while keeping Python 2
  compatibility, migration and testing tutorials added.
- Help displays access rights of columns and actions.  New script
  `tools/help-generator.py` generating help non-interactively and function
  `pytis.util.run_as_script()`.
- New function `pytis.util.send_mail()` replacing the `*Email` classes and
  `pytis.extensions.emailsend()` (removed).
- Pytis Demo merged into the Pytis repository, including its database
  definitions with initial data and a script recreating its database.
- New decorator `pytis.presentation.procedure`.  `ViewSpec` accepts only
  named arguments, so that procedures may be defined as public methods of
  specification classes.
- New configuration option `resource_path` and environment variable
  `PYTIS_HELP_DIR`.  Invalid configuration paths are reported.
- gensqlalchemy supports the `Interval` type and `REFERENCING` of statement
  triggers and no longer uses `oid` columns (PostgreSQL 12).  The obsolete
  `gensql.py` was removed.
- **Incompatible:** Removed support for `proc_spec` specifications,
  `app_commands.py`, plain text print specifications, output parameters
  passed through the output resolver, the configuration options
  `printing_command` and `fallback_table_print` and custom validation error
  messages.
- `Data.search()` accepts `arguments`.  Encryption works with multiple
  database connections.
- Web: Range fields, binary fields displayed `inline`, the summary line may be
  suppressed.  Web applications are Python 3 only.
- Web: Adapted to Wiking authentication, session and application API changes,
  support for the Wiking role `OWNER` dropped.
- Web: Restyled web forms (borderless tables, full size single line text
  fields, row expansion styling, found record highlighted by outline).
- Web: Fixed search, record insertion, HTML escaping in headings and the
  position of the "No records" message.

### 2019
- Wx: Field state indication redesigned: invalid fields are marked by the
  label color and a state icon (also for fields unavailable due to access
  rights and for non-editable fields) instead of background colors.  Field
  sizes are computed more consistently and the filter dialog supports range
  fields.
- Wx: Input dialogs (`InputDialog`, `InputNumeric`, `InputDate`) replaced by
  `InputForm`, new functions `input_text()`, `input_number()` and
  `input_date()` should be used instead.  Form layout may include
  `lcg.Content`.
- Wx: Row fetching and caching in list forms reimplemented.  The fetch buffer
  is a general `pytis.data.FetchBuffer` growing in both directions with
  absolute cursor moves, the grid table (`ListTable` renamed to `GridTable`)
  caches rows and styles in an LRU cache, which makes scrolling, grouping and
  styling faster.  Grid column sizing rewritten.
- Wx: Inline editing of grid cells removed (it was unusable).
- Wx: Side forms are managed purely through `AuiNotebook`, which fixes
  commands invoked on a wrong side form.  Side forms newly added to the
  specification are shown even to users who customized the tabs.
- Wx: Structured text fields can be opened as PDF (`COMMAND_VIEW_FIELD_PDF`).
- Wx: Improved bug report dialog, exceptions in `Application.init()` and
  `post_init()` display it as well.
- Wx: Smoother password entry for encryption keys.
- Wx: New command line option `--autostart-saved-forms`.
- Wx: wxPython 4.0 and newer PyMuPDF versions in the PDF viewer,
  `wx.CheckListBox` instead of `CheckListCtrl`, the clipboard code is GTK 3
  only, remote clipboard syncing dropped.
- Wx: `Browser.load_content()` accepts `lcg.Content`, so the `content`
  function of a `Binding` may return it directly.
- Wx: Fixed browser server shutdown on exit, remote writing of binary data
  and time field spin buttons.
- **Incompatible:** `pytis.data.Binary` values contain `bytes` directly,
  `Binary.Buffer` was renamed to `Binary.Data`, `buffer()` is deprecated and
  `load()`, `save()` and `path()` were removed.  New function
  `pytis.data.binval()`.
- **Incompatible:** `fetch(position)` is the preferred way of retrieving rows
  from data objects, `fetchone()` remains as an alias and its deprecated
  `transaction` argument was removed.
- **Incompatible:** Applications must use `pytis.config` instead of
  `import config`.  Removed the deprecated `pytis.extensions.data_object()`
  and `pytis.form.create_data_object()`, the `Field` argument `type_` (use
  `type`), the defaults of the options `config_file` and `user_config_file`
  and the option `lc_numeric`.
- Port to Python 3 started while keeping Python 2 compatibility: no asterisk
  imports, `bytes` for binary values, requirements files.
- New function `pytis.util.data_object()` creating data objects universally
  (also in scripts), new `pytis.data.transaction()`.
- `python-gnupg` instead of `GnuPGInterface`, `file-magic` for MIME type
  detection.
- Mail functions use UTF-8 by default and handle Unicode headers.
- Output: `HGroup`/`VGroup` accept `width`, `height` and padding, containers
  accept alignment, PDF output supports font and background colors,
  `FieldSet` orientation, new function `lcg_node()`.
- gensqlalchemy: SQLAlchemy 1.3.5 compatibility, `DateTime` type, removed
  `WITH (isstrict)` unsupported by PostgreSQL 11.
- Fixed Excel export of time zone aware values, exported files are opened
  after export.

### 2018
- Wx: Migration to wxPython 3.0 and further to 4.0 and GTK 3.  The embedded
  GTK WebKit browser was replaced by `wx.WebView`, wx 2.8 support dropped,
  tab navigation between input fields implemented by Pytis, many sizing,
  focus and event handling problems fixed.  Resources of the browser content
  (including help) are served by a separate local HTTP server.
- Wx: New `pytis.form.FileViewer` (also as a standalone frame) displaying PDF
  documents in a side form with a button panel (PyMuPDF).
- Wx: **Incompatible:** `Binding` arguments for content side forms unified:
  `content` (a function or a column id) together with the new `content_type`
  (`'html'`, `'lcg'`, `'uri'` or `'pdf'`).  `uri` is deprecated and
  `preview_column` removed (use `content_type='pdf'`).
- Wx: The user is asked on startup which forms saved on exit to restore.  All
  open forms are saved on exit, the exit confirmation was removed.  Forms are
  restored also in applications not using DMP.
- Wx: Improved bug report dialog: the user may describe the problem and send
  the report by email, the traceback is shown as HTML.
- Wx: Input fields with help display a help icon next to the field.
- Wx: Radio box fields have an explicit item for the NULL value, nullable
  radio fields should define `null_display`.
- Wx: Interactive CSV import may read a file from the client machine in
  remote sessions (`pytis.form.open_selected_file()`), respects the prefill
  of side forms and validates values in the layout order.
- Wx: New procedure `application.close_forms()`, `new_record()` accepts
  `set_values`.
- Wx: Menu cached after reloading menu, rights and specifications, redundant
  selects removed on form startup.
- Wx: The main frame title shows the X2Go display number instead of the
  database and user (shown in the status bar), new icons for the remote
  connection status.
- Wx: New command line option `--dump-queries COUNT` dumping the most time
  consuming SQL queries on exit, fixed `--profile`.
- Wx: **Incompatible:** Support for legacy application specifications (an
  `application` module with functions instead of the `Application` class)
  dropped.
- Wx: The X2Go session password prefills the database password,
  `config.dbuser` is updated after login.
- Wx: Pytis2Go rewritten as a systray service: a single instance runs
  multiple X2Go clients in separate processes (including Windows), a session
  manager lists running sessions, profiles may be grouped into submenus,
  new arguments `--no-agent-authentication` and
  `--no-kerberos-authentication`.
- Wx: **Incompatible:** The client side code (`pytis.x2goclient` and the
  client parts of `pytis.remote`) moved to the separate library `p2go`.
- gensqlalchemy: partial indexes, PostgreSQL 10 identity columns instead of
  `SERIAL`, Pytis database objects in a separate schema.
- `pytis.output.Image` accepts image data in memory.  Temporary PDF files are
  kept until the external viewer finishes.
- Logging messages translated to English, first `setup.py`.
- Web: New experimental argument `embed` of web forms for embedding in another
  form, improved paging controls and label alignment.

### 2017
- Wx: Pytis2Go (`bin/pytis2go`) has a new startup application with profile
  and session selection, an authentication dialog with a username field and
  a menu of auxiliary functions.  It tries the previously successful
  authentication method first.
- Wx: Pytis2Go checks for a Pytis upgrade before starting a session, manages
  desktop shortcuts and can run as a task bar service.
- Wx: The X2Go client no longer depends on Pyhoca CLI and is localized (new
  submodule `pytis.x2goclient`).  Each Pytis2Go instance starts its own RPyC
  server, which is more robust against blocked ports and tunnel failures.
- Wx: Status bar fields can be refreshed explicitly (`StatusBar.refresh`).
- Wx: Help icons next to input fields with help.
- Wx: Per field check functions defined by the new `Field` argument `check`,
  validation messages shown in the status line.
- Wx: Computer input fields may be validated before the computer is called
  (`validate` and `novalidate` arguments of `Computer`), the computer is not
  called with invalid input and a `fallback` value may be defined.  New script
  `tools/dump-recursive-computers.py` detecting cyclic computer dependencies.
- Wx: Automatic links to codebooks pass runtime arguments to the linked form,
  explicit links may define `arguments`.
- Wx: Query fields are validated before the providers are called, `check()`
  is handled in query fields.
- Wx: Virtual field values are preserved on form commit.
- Wx: Saved filters may contain the `NOT` operator.
- Wx: Fixed handling of closed database connections in commit, rollback and
  validation, a database error in the row counting thread no longer freezes
  the UI.
- `PresentedRow` computed values reimplemented, inner Python values may be
  assigned directly (`row['x'] = 5`), validation results are cached.
  **Incompatible:** `PresentedRow.original_presented_row()` and `validated()`
  were removed.
- **Incompatible:** `pp.row_function` removed (use `pp.computer`), legacy
  filter sets and the alias `FieldSpec` (use `Field`) dropped.
- New configuration options `use_dmp_roles` and `use_dmp_rights`, DMP objects
  may be created in a separate schema.
- New configuration option `session_variables` setting custom PostgreSQL
  session variables on each new connection.
- gensqlalchemy: new method `SQLFunction.from_clause`, better handling of
  exclude constraints.
- The help generator no longer depends on DMP, Pytis help texts revised.
- `MemData` accepts `key` and rows as tuples of Python values, `Specification`
  may define `MemData` rows.  New `maxlen` of `pytis.data.sval`, new output
  parameter `P_ARGUMENTS`.
- Web: Experimental inline editing in web forms (update, insert, copy and
  delete through the new `DeletionForm`) in `ListView` and browse forms.
- Web: `Binding` supports `enabled`, `pp.Binding` replaces `wiking.Binding`.
  The calendar widget is bundled with Pytis JavaScript and CSS.
- Web: Flexible layout of vertical groups and query fields, fixes in query
  fields, HTML escaping and textarea sizing.

### 2016
- Wx: Export to XLSX (`xlsxwriter`) in list forms, export of selected rows
  only.  **Incompatible:** Export to XLS using `pyExcelerator` was removed.
- Wx: Status bar reimplemented: fields (`StatusField`) may display
  periodically refreshed values and icons, tooltips (also multiline or given
  by a function) and an on-click handler.  Built-in fields are defined by
  `pytis.form.built_in_status_fields()`, `pytis.form.set_status_text()` was
  renamed to `set_status()`.
- Wx: Visible columns of a list form may be toggled in a dialog.
- Wx: Side form context menu filters offer also `NOT IN`, `pytis.form.IN`
  accepts `condition`.
- Wx: New arguments `language`, `handler` and `context` of `PrintAction`.
- Wx: New `Field` argument `codebook_update_set_values`.
- Wx: Actions with `secondary_context` are enabled in single forms.
- Wx: `ProgressDialog` supports an indeterminate mode.
- Wx: Directories last used in file dialogs are remembered per context.
- Wx: Remote client operations (`pytis.remote`) reimplemented with multiple UI
  backends (wx, Tk, Zenity, Win32) chosen by platform.  New functions
  `select_directory()` and `run_python()`, file dialogs with a title and
  multiple filename filters (`patterns`), a broken connection is detected
  before falling back to local operation, the required client version is
  checked on startup.
- Wx: Fixed remote viewers for non-PDF files and launching exported files in
  remote mode, `launch_file` is no longer blocking.
- Wx: New X2Go client launcher `bin/pytis2go.py` with broker support,
  `tools/x2go-ping.py`, X2Go support documented.
- **Incompatible:** `LTree` sorts as ltree by default, pass `text=True` for
  ltree values with national characters.
- Database introspection is performed again for fields with an explicit
  `type`, which may change some field properties in applications.
- Output: suppressing the first line indent, box masks and specific image
  sizing in print templates.
- Big fields are omitted from selects where the type comes from database
  definitions.
- `form-profile-validator.py` can update profiles and filters.
- Fixed gensqlalchemy table returning functions, trigger functions and
  `ALTER ... OWNER TO`.
- Web: The first profile is the default in web forms.
- Web: Accessibility improvements (WCAG 2 AA): no `<th>` in presentational
  tables, contrast and screen reader handling of the not null indicator.
- Web: Improved printing of web forms, fixed HTML escaping in tree views and
  CKEditor icons.

### 2015
- Wx: The application may be specified by a class derived from
  `pytis.presentation.Application` with methods such as `menu()` and `init()`
  and application commands as `cmd_*` methods.  The legacy module level
  functions and `app_commands` still work.
- Wx: New specification argument `on_copy_record`, `pytis.form.new_record()`
  accepts `copied_row`, `on_new_record` may return a dictionary of values
  prefilling the default insertion form.
- Wx: Keyboard layout is saved between sessions.
- Wx: Error dialog supports a custom `message`.
- Wx: **Incompatible:** Field editability is no longer implicitly derived from
  `computer` or a zero `width`.  Such fields need explicit
  `editable=Editable.NEVER` (added by
  `tools/modify-specifications.py --set-explicit-ineditable`).
- Wx: Fixed editability of checkbox, radio box, image and file fields.
- Wx: X2Go client (`x2goclient.py`): X2Go ssh broker, GSS authentication,
  session resuming and terminating, desktop shortcuts, wx dialogs, settings
  file and own VcXsrv variants for Windows.  New `x2go-ping` utility and
  `x2goproxy.py`.
- Wx: Initial support of client side encryption (`open_data_as_file()`
  argument `decrypt`).
- gensqlalchemy: crypto areas, foreign tables and user mappings, unlogged
  tables and array types (arrays also in `pytis.data` and read-only in web
  forms).
- Time zones: `pytis.data.DateTime` and `Time` know whether they carry a time
  zone (`without_timezone`), mismatches on insert or update are reported.  New
  script `tools/tzcheck` finding specifications with a possible mismatch.
- Range types: unbound ranges, arbitrary bound kinds, new operators, native
  SQLAlchemy range types.
- New data type `pytis.data.Email`.  `Float` uses decimal values (new `digits`
  and `precision` arguments of `fval`).  A list or tuple passed as
  `enumerator` is converted to `FixedEnumerator`.
- Database functions may be given by their database specification instead of
  the name.  Field types are taken from the column binding type when
  available.
- New select argument `limit`, the obsolete `pgsql` database backend removed.
- Output: `pytis.output.Image` argument `standalone`, dimensions and colors of
  output markup given directly, `Group` padding and spacing, new `HGroup` and
  `VGroup`.
- New tools `tools/dump-specifications.py` and `tools/modify-specifications.py`
  and tutorial `doc/tutorials/Fields.txt`.
- Web: Expandable table rows in `BrowseForm` (arguments `expand_row` and
  `async_row_expansion`).
- Web: New `VirtualForm`, virtual fields may produce LCG content.
- Web: `Action` accepts `icon` and `form_content`, row actions menu with
  action icons also in `ItemizedView`.
- Web: Search covers non-string columns and enumerator values, flexible
  horizontal groups and responsive layouts, `email` input type for `Email`
  fields.

### 2014
- Wx: Range type fields (`DateRange`, `DateTimeRange` etc.) supported in edit
  and browse forms.
- Wx: Form profiles may be grouped into named groups (nested profile menus).
  `unnest()` gives flat sequences of `ViewSpec.profiles()` and `actions()`
  (replacing the argument `linear` of `actions()`), `filter_sets` of
  `Profile` and `ViewSpec` are deprecated.
- Wx: Dual form split orientation may be specified for multiforms and
  switched by the new command `DualForm.COMMAND_RESPLIT`.
- Wx: Query fields: new `QueryFields` arguments `load` and `save`, values
  reloaded on form refresh, warning about unapplied values when switching
  forms, the panel is on top by default.
- Wx: **Incompatible:** The `QueryFields` argument `autoapply` now means
  automatic updates after each field change without the Apply button.  The
  initial automatic apply is controlled by the new argument `autoinit`, so
  existing uses of `autoapply` must be changed to `autoinit`.
- Wx: New field argument `formatter` formatting the value for read-only
  presentation.
- Wx: Values which don't fit table cells are shown as grid tooltips, numeric
  columns are aligned right.
- Wx: The help browser may open forms and run procedures.
- Wx: New application specification function `post_init`, startup forms not
  present in the application menu are not opened.
- Wx: New configuration option `login_selection` restricting the login dialog
  to a selection of login names, optionally with passwords.
- Wx: The main frame title can be changed by `pytis.form.frame_title()`.
- Wx: Keyboard layout switcher (command `SWITCH_KEYBOARD_LAYOUT`).
- Wx: Big fields omitted from `ShowForm` selects.
- Wx: More robust handling of closed transactions and timeouts in forms.
- Wx: **Incompatible:** `pytis.windows` renamed to `pytis.remote`
  (`client_available()` replacing `windows_available()`).  Communication with
  the client uses RPyC over X2Go, with `x2gobroker` support and GNU/Linux
  clients.
- Wx: X2Go client `x2goclient.py` with new options, zenity dialogs, automated
  client upgrades and Windows support.
- Crypto administration form (adding crypto users, changing passwords).
- **Incompatible:** Output specifications are less dependent on resolvers
  (`pytis.output.Formatter` takes the resolver as the first argument,
  `data_table()` takes a view and data objects).
- New `PrintSpecification.style`, `lcg.Content` in output markup.
- New data types `LocalDateTime` and `LocalTime`, argument `without_timezone`
  of date and time types, type cloning works for more types.
- gensqlalchemy: materialized views, `EXCLUDE` constraints, check constraints
  defined by a method, better upgrades of defaults and indexes.
- New tool `tools/dbdefscheck` checking form specifications against database
  definitions.
- Web: Dynamic `EditableBrowseForm` with row insertion and deletion.
- Web: Query fields support runtime dependencies and application defined
  layouts, profiles and filter sets are implemented as query fields.
- Web: Inline editing of table cells in browse forms.
- Web: Form field validation moved from Wiking into `pytis.web`, database
  errors displayed in edit forms.
- Web: CKEditor improvements.

### 2013
- Wx: **Incompatible:** `argument_provider` is called consistently in wx and
  web forms: without `query_fields` with no arguments, with `query_fields`
  with a `PresentedRow` of the query field values.
- Wx: Query fields: the panel can be moved and minimized, the table function
  is called after the query fields are submitted, query fields work in side
  forms and aggregation forms, `query_fields` may be a method.  New
  specification argument `condition_provider`.
- Wx: Idle transactions are closed or rolled back, select transactions are
  read-only.  New configuration option `edit_form_timeout` closing an edit
  form after a period of inactivity.
- Wx: Access rights of codebook fields are checked before running insert
  forms.
- Wx: Basic support for wx 2.9 and macOS.
- Wx: Remote client API: new functions `select_directory`, `select_file` and
  `open_file` in `pytis.windows`, communication works also with X2Go.
- Internationalization works for wx applications.  Translations are split into
  domains (`pytis-wx`, `pytis-data`, `pytis-defs`, `pytis-web`), the path may
  be set by `PYTIS_TRANSLATION_PATH`.
- Initial support for menu translations in DMP.
- Data types are cloned in specifications.  Passing data type arguments
  directly to `Field` is deprecated, use `type` with a type instance (converted
  by `tools/convert-field-type-specifications.py`).
- New helpers for field property changes in specifications, codebooks derived
  from database references, presentation specifications related to
  gensqlalchemy specifications.
- New field specification attribute `visible`.
- Support for PostgreSQL range types, `Numeric` mapped to `pytis.data.Float`,
  `DateTime` with microseconds.
- gensqlalchemy: functions and views as return types, overloaded and
  aggregate functions, functional and unique indexes, `InsertFromSelect`.
- gensqlalchemy moved from `pytis.extensions` to `pytis.data`, Pytis database
  definitions converted to gensqlalchemy.
- Database upgrades: `gsql --upgrade` compares the specifications with the
  database and generates upgrade commands for tables, columns, indexes,
  constraints, triggers and functions.  New `gsql` options.
- Emacs support for database specifications (`gensqlalchemy.el`), new tool
  `specinfo` (with `specinfo.el`).
- New database change logging (`LogTrigger`, `f_view_log`, log rotation), new
  utility `tools/dbrename.py`.
- Remote debugging support (`pytis.util.debug`).
- Web: **Incompatible:** Query fields replace `filter_fields`, use
  `query_fields` with `argument_provider` or `condition_provider`.
- Web: Editable browse forms (`EditableBrowseForm`), asynchronous profile
  selection, index search and sorting, asynchronous tooltips
  (`UriType.TOOLTIP`).  Web form CSS and CKEditor style sheets moved from
  Wiking to Pytis, `BooleanField` renamed to `CheckboxField`.
- Web: CKEditor 4.1 with table editing, figure captions, definition lists and
  index items.

### 2012
- Released 1.2.0 (2012-03-20) and 1.2.1 (2012-03-30).
- Wx: New help browser based on the embedded WebKit browser replacing the wx
  help controller.  Help is stored in the database, managed through DMP
  (specifications, fields, bindings, actions, profiles, menu items) and
  access is limited by DMP rights.  Pytis help moved into the repository as
  text files, help classes moved to the new module `pytis.help`.
- Wx: Attachments (images and other files) in structured text fields through
  a new dialog.  Attachments are handled by the new `AttachmentStorage` API
  (`FileAttachmentStorage`, `HttpAttachmentStorage`, `DbAttachmentStorage`),
  new `Field` argument `attachments_directory`.
- Wx: Sensitive data support: new `tools/crypto` utility managing encryption
  keys, crypto password dialogs, encrypted columns (also binary ones).
- Wx: `query_fields` in `ViewSpec`: a query panel passing its values to
  `argument_provider` (table functions).
- Wx: `ActionContext.SELECTION` uses the current row when no selection is
  active, the selection can be cleared from the context menu.
- Wx: Files and binary field values can be opened as local files (also on
  remote Windows workstations) and saved with a default file name.  New
  Windows remote service commands `version`, `run_python` and `restart`.
- Wx: Dynamically changing enumerations in `ListBoxField` and `ChoiceField`,
  `Field` arguments `inline_display` and `inline_referer`.
- Wx: Side forms of a multiform can be closed and reopened, their order is
  saved, `wx.aui.AuiNotebook` replaced `wx.Notebook`.  Form profiles may have
  a `description`.
- Wx: Login, password and `run_form` dialogs replaced by `InputForm`, new
  dialog choosing the default printer, configuration option
  `rpc_remote_view`.
- Wx: New command reloading all specifications.
- Codebooks can be printed in custom print templates, new configuration option
  `output_row_limit`.
- New class `Enumeration` for code based enumerations.
- New data types `ISODateTime`, `Name`, `DoublePrecision`, `SmallInteger`,
  `LargeSerial` and `Serial`.
- Access rights processing reworked (column rights computed in DMP),
  `tools/dmp` can remove rights and exclude columns.
- Transactions use repeatable read instead of serializable isolation, lost
  database connections are recovered when starting a transaction.
- New `gensqlalchemy` module (`pytis.extensions.gensqlalchemy`) defining
  database objects as Python classes on top of SQLAlchemy, the `gsql.py`
  script generating SQL, automatic conversion of `gensql` specifications,
  Emacs helpers `gensqlalchemy.el` and `dump-compare.el`.  Pytis database
  specifications converted (`pytis.dbdefs`).
- `ViewSpec` argument `referer`.
- Web: Row actions as a popup menu (`popup_actions` of `ListLayout`).
- Web: HTML fields use CKEditor with Pytis dialogs for images, video, audio
  and other attachments, MathML and LCG exercises.
- Web: Asynchronous loading of web forms, time zone conversion, textual
  codebook fields.
- Web: Pytis CMS image attachments (`PYTIS_CMS_ATTACHMENTS_STORAGE`).

### 2011
- Released 1.1.1 (2011-01-17).
- Wx: Form profiles replace the saved filters and form state.  A profile
  bundles the filter, sorting, grouping, columns and their widths, folding
  and aggregations, is stored in the database and selected in the form
  toolbar.  Predefined profiles are defined by `Profile` and `Profiles` (the
  `profiles` argument of `ViewSpec`).  `tools/config-updater.py` and
  `tools/form-profile-validator.py` convert and validate the stored data.
  **Incompatible:** `initial_folding` renamed to `folding`, the deprecated
  `conditions` argument is no longer supported.
- Wx: User defined aggregated views, saved and managed through the UI or
  defined in the specification (`AggregatedView`).
- Wx: The built-in HTML viewer was replaced by an embedded WebKit browser.
  Forms may display web content in side forms (`Binding`), new configuration
  option `http_proxy`.
- Wx: Editor of structured text fields with a toolbar, search and replace,
  preview and PDF export.  **Incompatible:** The data types `StructuredText`
  and `SimpleFormattedText` were removed in favor of the field attribute
  `text_format`.
- Wx: New form class `InputForm`, new argument `set_values` of `EditForm`, new
  directory selection dialog.
- Wx: Optional slider in number fields, `minimum` and `maximum` of `Integer`
  and `Float`.
- Wx: The current grid row is marked by a rectangle, layout items may be
  functions.
- Wx: Context menu `IN` and `NOT IN` filters for all links.
- Wx: Lengthy form starts, sorting, filtering and profile changes may be
  interrupted (`stop_check` argument of `select`), side forms are initialized
  lazily.
- Wx: Clipboard handling reworked.
- Wx: New remote Windows services (`pytis.windows`) for Pytis running under
  X2Go/NX: file export, launching files and URLs on the client, clipboard
  access, remote upgrade.  New configuration option
  `rpc_communication_enabled`.
- Wx: Logging of user actions into the database.
- Python 2.6 required, `datetime` instead of `mx.DateTime`, unicode strings
  throughout, Pyro remote calls dropped.  **Incompatible:** Applications must
  be converted to unicode strings.
- Printing reimplemented on top of LCG and ReportLab (PDF), the Lout formatter
  was removed.  Print specifications are defined by `PrintSpecification` and
  `PrintAction`, print rights are integrated into DMP, users may edit their
  print templates stored in the database.
- The specification resolver was rewritten to work on Python modules, Pytis
  built-in specifications moved to `pytis.defs`.  **Incompatible:**
  Specifications no longer receive the resolver as an argument.
- `dmp` reorganized with new commands `add-member`, `copy-rights`,
  `rename-specification`, `convert-system-rights` and `ls`.
- New data type `Array`, encrypted database columns (`crypto_names`),
  `TimeInterval` for the `interval` type.
- New data operators `OpFunction` and `ANY_OF`, shorthand functions for
  `Value` creation (`sval()` etc.), `RawCondition` removed.
- Web: Form profiles in `BrowseForm`, user editable filter fields, table
  function arguments through `filter_fields`, runtime filters for all
  filtered fields.
- Web: Row action buttons, forms in `ListView` items, styling of web form
  buttons and field labels, busy cursor during AJAX updates.
- Web: Depends on LCG 0.5.1.

### 2010 (since 1.1.0)
- Wx: Workarounds of wx crashes when leaving forms (new command
  `SAFE_LEAVE_FORM`) and of the missing row crash in list forms.
- Wx: Computed and default values work in `ListField`.
- Fixed `fetchone` to always use the transaction of the select.
- New conditional operator `FunctionCondition`.
- Web: `simplejson` is not required in Python versions including `json`.

## 1.1.0 (2010-11-22)
- Solved major problems for running the fat client with wx 2.8.
- Several fixes of display and AJAX handling in web forms.
- Common DMP library.
- Dual aggregation forms for the fat client.
- Asynchronous row counting in wx forms.
- Support for time interval values.
- Several gensql improvements and fixes.
- Added support for ARIA properties `aria-required` and `aria-described-by`
  in web forms.
- Added support for multiple filters in web forms.
- Cache codebook display values in web forms.
- Form statistics (database update needed!) and new configuration option
  `form_statistics`.
- Support for setting search_path in PostgreSQL and new configuration option
  `dbschemas`.
- Many bug fixes and other minor improvements.

## 1.0.0 (2010-01-21)
- First official release.
