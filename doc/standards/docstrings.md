Docstring formatting rules
==========================

All docstrings in this project comply with Google style and are compatible
with both Sphinx (Napoleon extension) and mkdocstrings (autorefs plugin).

**Structure**

- Sections: `Arguments:`, `Returns:`, `Raises:`, `Attributes:` — each item
  on its own line: name, type in parentheses, colon, description.
- Section item indentation: 2 spaces. Continuation of a wrapped description:
  4 spaces.
- Single-line docstring: everything on one line, no blank lines.
- Multi-line docstring: first line is a brief summary, then a blank line, then
  the body. There must be a blank line before the closing `"""` (allows Emacs
  M-q wrapping).  No blank line after the closing `"""` .
- Text inside docstrings wraps at 80 characters (total line length including
  indentation). The first line (summary) is never wrapped — it stays on one
  line even if it exceeds 80 characters.
- Body prose uses `**bold**` for emphasis and standard lists (`-`, `1.`).
  Avoid reStructuredText-only constructs such as underlined section headers
  (`---`, `===`) and literal-block markers (`::`) — they break rendering in
  Markdown-based tools.

**Backticks and cross-references**

- All references to code identifiers use single backticks: `` `Name` ``.
- Single quotes represent a quoted string *value*, e.g. `'active'`.
- `None`, `True` and `False` use backticks but need no explicit role.
- No explicit Sphinx roles (`:class:`, `:meth:`, `:func:`, etc.) — plain
  backticks work in both Sphinx (via `default_role = 'any'`) and
  mkdocstrings (via the autorefs implicit references).
- This applies uniformly to all identifier kinds: classes, exceptions,
  modules, functions, methods, and parameter names referenced in prose.
- Function/method references don't use () at the end -- this breaks lookup for
  both Sphinx and mkdocstrings and renders as code, no link.
- Exception names in `Raises:` items are also backtick-wrapped for
  mkdocstrings compatibility (Sphinx/Napoleon resolves them either way).

**Type annotations**

- When a function or method has type annotations (native Python 3 annotations
  or `# type:` comments), omit the `(type)` from `Arguments:`, `Returns:`,
  and `Attributes:` items — the type is already captured in the annotation.
- When no annotation is present, include the type: `name (type): description`.
  Use `str | None` style (Python 3.10+) for union types.
- `*args` is documented as `args (tuple)`; `**kwargs` as `kwargs (dict)`.

**Example:**

```python
"""Module summary.

Longer module description if needed.

"""
...

class Foo:
    """Brief summary of the class.

    Longer description if needed. May span multiple paragraphs.
    References to other classes like `Bar` or modules like `pytis.data`
    and methods like `frob` need no explicit role.

    Attributes:
      bar (str): Description of the attribute — type included, no annotation.
      baz (int | None): Description of an optional attribute.

    """

    def frob(self, value, flag=False, **kwargs):
        # type: (str, bool, dict) -> str | None
        """Brief summary of the method.

        Longer description referencing parameter `value` or the flag
        `flag` in prose.

        Arguments:
          value: Description of the argument. Longer descriptions wrap at
            80 characters with 4-space continuation indent.
          flag: Optional flag, defaults to `False`.
          kwargs: Additional keyword arguments passed to `_apply`.

        Returns:
          The processed value, or `None` if nothing was produced.

        Raises:
          `ValueError`: If `value` is empty.
          `ProgramError`: If the internal state is inconsistent.

        """
        ...

    def legacy(self, value, flag=False):
        """Brief summary — no type annotation, so types go in the docstring.

        Arguments:
          value (str): Description of the argument.
          flag (bool): Optional flag, defaults to `False`.

        Returns:
          The processed value as a `str`, or `None` if nothing was produced.

        """
        ...

    @classmethod
    def from_string(cls, s):
        """Create a `Foo` instance from a string."""
        ...

```
