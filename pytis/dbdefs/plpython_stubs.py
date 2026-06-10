# -*- coding: utf-8 -*-

# Type stubs for PL/Python runtime globals (plpy, TD, args).
#
# PL/Python functions executed inside PostgreSQL have access to these globals
# at runtime, but they are invisible to static type checkers. Import this
# module under TYPE_CHECKING in dbdefs files to eliminate false pyright errors:
#
#   if TYPE_CHECKING:
#       from pytis.dbdefs.plpython_stubs import plpy, TD, args  # noqa: F401

try:
    from typing import Any, Dict, Iterator, List, TYPE_CHECKING
except ImportError:
    TYPE_CHECKING = False

if TYPE_CHECKING:

    class _SPIResult(object):
        def __iter__(self):
            # type: () -> Iterator[Dict[str, Any]]
            pass

        def __len__(self):
            # type: () -> int
            pass

        def __getitem__(self, index):
            # type: (int) -> Dict[str, Any]
            pass

    class _Plpy(object):
        def execute(self, query, limit=0):
            # type: (str, int) -> _SPIResult
            pass

        def error(self, msg):
            # type: (str) -> None
            pass

        def info(self, msg):
            # type: (str) -> None
            pass

        def warning(self, msg):
            # type: (str) -> None
            pass

        def debug(self, msg):
            # type: (str) -> None
            pass

        def log(self, msg):
            # type: (str) -> None
            pass

    plpy = _Plpy()
    TD = {}   # type: Dict[str, Any]
    args = [] # type: List[Any]
