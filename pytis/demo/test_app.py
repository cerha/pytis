# -*- coding: utf-8 -*-

# Copyright (C) 2026 Tomáš Cerha <t.cerha@gmail.com>
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

"""Tests run inside a live Pytis Demo application.

Invoke via:

    python -m pytis.run --config /path/to/config.py --run-tests

Running these tests directly with pytest will skip the whole module because
the application instance is not initialized that way.

"""

from __future__ import unicode_literals
from __future__ import print_function

import os
import sys

import pytest

from pytis.api import app
from pytis.remote.test import interactive

pytestmark = pytest.mark.skipif(
    not os.getenv('PYTIS_RUN_TESTS'),
    reason="Run via 'python -m pytis.run --run-tests'",
)


class TestApp:
    """Tests requiring the running Pytis Demo wx application."""

    def test_api_form(self):
        assert app.form is None
        app.run_form('misc.Products')
        assert app.form is not None
        assert app.form.name == 'misc.Products'

    def test_query_fields_api(self):
        app.run_form('misc.RandomNumbers')
        assert app.form.query_fields is not None
        assert app.form.query_fields.row is not None
        assert app.form.query_fields.row['count'].value() == 10
        assert app.form.query_fields.row['minimum'].value() == 0
        assert app.form.query_fields.row['maximum'].value() == 100

    @interactive
    def test_write_selected_file(self):
        for data, mode, encoding in ((u'some text', 'w', 'utf-8'),
                                     (b'some bytes', 'w', 'utf-8'),
                                     (b'some bytes', 'wb', None),
                                     (u'some text', 'wt', None)):
            filename = app.write_selected_file(data, 'test.txt', mode=mode, encoding=encoding)
            with app.open_file(filename, mode='r') as f:
                assert f.read() == data

    @interactive
    def test_write_selected_file_type_errors(self):
        if sys.version_info[0] > 2:
            with pytest.raises(TypeError):
                app.write_selected_file(u'text', 'text.txt', mode='wb')
            with pytest.raises(TypeError):
                app.write_selected_file(b'bytes', 'test.txt', mode='w')
        with pytest.raises(TypeError):
            app.write_selected_file(8, 'test.txt', mode='wb')
