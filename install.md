Pytis Installation Instructions
===============================

Prerequisities:

  * Python 2.7 or Python 3.5 or later
  * PostgreSQL 9.4 or later

Pytis is a pure Python library so it can be used directly from the checked out
tree.  You need to:

  * Add the `lib` subdirectory to PYTHONPATH.
  * Run 'make' after each checkout to update generated files.
  * Create a Python virtual environment and install required packages as
    described below.

The needed Python packages for desktop applications (using wxPython) are
defined in file requirements.txt.

In order to install `pymupdf` package, you need to install libmupdf-dev
package.

For web applications use the Wiking framework which defines its own
requirements.txt.

Because wxPython binary packages are not available through PyPI, you need to
pass the wxPython binary archive URL to `pip install`:

```
pip install -r requirements.txt -f URL
```
The URL for Debian is:
  https://extras.wxpython.org/wxPython4/extras/linux/gtk3/debian-9
For Ubuntu:
  https://extras.wxpython.org/wxPython4/extras/linux/gtk3/ubuntu-18.04

See https://wxpython.org/ for more details.
