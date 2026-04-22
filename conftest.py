# -*- coding: utf-8 -*-
import os
import sys

import pytest

# pytis.rest requires Python 3 (type-union syntax, dataclasses with slots, …).
# Exclude it from collection entirely when running under Python 2 so that
# pytest does not choke on syntax errors before any test starts.
if sys.version_info[0] < 3:
    collect_ignore_glob = ['pytis/rest/*', 'pytis/extensions/dump.py']


def pytest_addoption(parser):
    parser.addoption(
        '--interactive', action='store_true', default=False,
        help='Run interactive tests that open GUI dialogs and require user interaction.',
    )


def pytest_configure(config):
    config.addinivalue_line(
        'markers',
        'interactive: test opens GUI dialogs; skipped by default, run with --interactive',
    )


def pytest_sessionstart(session):
    if session.config.getoption('--interactive', default=False):
        os.environ['PYTIS_TEST_INTERACTIVE'] = '1'


def pytest_collection_modifyitems(config, items):
    run_interactive = config.getoption('--interactive') or os.getenv('PYTIS_TEST_INTERACTIVE')
    if not run_interactive:
        skip = pytest.mark.skip(reason='interactive test; use --interactive or set '
                                        'PYTIS_TEST_INTERACTIVE to run')
        for item in items:
            if item.get_closest_marker('interactive'):
                item.add_marker(skip)
