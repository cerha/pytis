# -*- coding: utf-8 -*-
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
    parser.addoption(
        '--legacy-service', action='store_true', default=False,
        help='Skip tests that require pytis2go rpc-json branch features.',
    )


def pytest_configure(config):
    config.addinivalue_line(
        'markers',
        'interactive: test opens GUI dialogs; skipped by default, run with --interactive',
    )
    config.addinivalue_line(
        'markers',
        'new_service: requires pytis2go rpc-json branch; skipped with --legacy-service',
    )


def pytest_collection_modifyitems(config, items):
    if config.getoption('--legacy-service'):
        legacy_skip = pytest.mark.skip(reason='requires pytis2go rpc-json; use without --legacy-service')
        for item in items:
            if item.get_closest_marker('new_service'):
                item.add_marker(legacy_skip, append=False)
    if not config.getoption('--interactive'):
        default_skip = pytest.mark.skip(reason='interactive test; use --interactive to run')
        for item in items:
            if item.get_closest_marker('interactive'):
                # If a parent class already carries a skipif that would fire,
                # use its reason so all items in the class show the same message.
                # Without this, the method-level skip mark would win because
                # iter_markers yields own markers before parent markers.
                reason = next(
                    (m.kwargs.get('reason', 'skipif')
                     for m in item.iter_markers('skipif') if m.args[0]),
                    None,
                )
                item.add_marker(
                    pytest.mark.skip(reason=reason) if reason else default_skip,
                    append=False,
                )
