# Type annotations for CLI are defined here rather than inline because
# CLI.arg() requires keyword-only parameters after *args, which is Python 3
# only syntax while util.py must remain Python 2 compatible at runtime.

import argparse
from typing import Any, Callable, Iterable, Sequence, Type

_ArgSpec = tuple[tuple[str, ...], dict[str, Any]]
_Arguments = tuple[_ArgSpec, ...] | Callable[[Callable[..., argparse.Action]], Any]
_Command = Callable[[argparse.Namespace], None]
_Decorator = Callable[[_Command], _Command]

class CLI(object):
    @staticmethod
    def arg(
        *args: str,
        action: str | Type[argparse.Action] = ...,
        nargs: int | str | None = ...,
        const: Any = ...,
        default: Any = ...,
        type: Callable[[str], Any] | argparse.FileType | str = ...,
        choices: Iterable[Any] | None = ...,
        required: bool = ...,
        help: str | None = ...,
        metavar: str | tuple[str, ...] | None = ...,
        dest: str | None = ...,
        **kwargs: Any,
    ) -> _ArgSpec: ...
    @staticmethod
    def _apply_arguments(
        arguments: _Arguments,
        add_argument: Callable[..., argparse.Action],
    ) -> None: ...
    def __init__(
        self,
        description: str,
        arguments: _Arguments = ...,
        config: bool = ...,
    ) -> None: ...
    def command(
        self,
        help: str = ...,
        arguments: _Arguments = ...,
        name: str | None = ...,
        config: bool = ...,
    ) -> _Decorator: ...
    def main(self, argv: Sequence[str] | None = ...) -> None: ...
