# Pytis framework

[![Tests](https://github.com/cerha/pytis/actions/workflows/tests.yml/badge.svg)](https://github.com/cerha/pytis/actions/workflows/tests.yml)

Pytis is a Python framework for declarative construction of information systems.  
It automatically builds database-driven applications from abstract
specifications, managing both relational data models and user interfaces with
minimal code.

As a developer, you define abstract specifications of database views in Python,
and Pytis generates the frontend application and manages forms that allow users
to interact with the underlying relational database.  Typical user actions include:

* browsing records in user customizable tables,
* inserting, updating, and deleting records (according to defined access rights),
* searching and filtering records based on user-defined conditions,
* incremental (type-ahead) searching,
* performing aggregations and defining custom aggregated views,
* displaying related records from other views in side forms,
* printing displayed data and defining custom reports,
* importing and exporting records from files, and more.

A typical deployment runs the application server on Linux using *wxWidgets*/*GTK*
for the GUI frontend and *PostgreSQL* for the database backend. Clients connect
to the server from *Windows*, *macOS*, or *Linux* using *X2Go*.

Relational database models are defined in Python using *SQLAlchemy*, and tools
are provided to keep the database schema synchronized with the model. The
data-access API used throughout Pytis — and exposed to application developers —
is fully abstracted from backend specifics. Pytis currently implements support
for *PostgreSQL* and leverages many of its advanced features and data types.


## Key features

* **Declarative by design:** Well-chosen abstractions enable highly flexible UI
  design without any GUI programming.
* **Stable API:** Custom interactions and extensions can access existing UI
  elements and their data through a consistent API.
* **Database abstraction:** Application code works purely with Python objects —
  no SQL involved.


## License

Pytis is Free Software, distributed under the terms of the **GNU General Public
License v2 (GPLv2)**.  See the `COPYING` file for details.

## Installation

Pytis is a pure Python library that runs on **Python 2.7** or **Python 3.5** and
later.

### wxPython dependency

Pytis GUI applications use **wxPython**, which may not be available through PyPI
on all Linux distributions.  You may need to install a suitable binary wheel
manually before installing Pytis.  For example, on Debian 11:

```
pip install wxPython==4.2.1 -f https://extras.wxpython.org/wxPython4/extras/linux/gtk3/debian-11
```

Available binary wheels can be found at
<https://extras.wxpython.org/wxPython4/extras/linux/gtk3>
Make sure the chosen package matches your Python version (the package name
includes a Python version identifier).

### Required system packages

The following system packages must also be installed:

- `fontconfig`
- `fonts-freefont-ttf` (or `ttf-freefont` on some systems)
- `libmupdf-dev`

### Installation steps

Once your virtual environment contains wxPython and the required libraries,
install Pytis as follows:

```
pip install babel flit
make build
pip install $(echo dist/pytis-*-any.whl)[wx]
```

## Documentation

The Pytis codebase is well documented, but currently lacks a complete guide or
tutorial.  Until such documentation is available, new users may need to explore
the source code to learn how to use the framework effectively.
