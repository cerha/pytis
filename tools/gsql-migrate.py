#!/usr/bin/env python3

"""Generate SQL update file from gsql template.

Template example:
--------------------------
@drop view1, view2;
alter table table2 add column amount int;
@create view1, view2;
--------------------------

The most important benefit is correct reordering of the tables/views passed to
a single @drop or @create statement according to their dependencies (if they
are defined correctly).

Lines starting with '--#' are template-only comments -- '--#' keeps SQL-comment
syntax so editors still highlight it (e.g. a note on how to regenerate the SQL);
they are not emitted into the output.  Ordinary SQL comments ('--') are passed
through into the generated SQL.

"""

import argparse
import importlib
import os
import re
import sys
import subprocess


def specification(module, name):
    """Return the gensqlalchemy specification class of given name or None.

    The name may be the specification class name or the DB object name (the
    class' `name` attribute).
    """
    importlib.import_module(module)
    from pytis.data.gensqlalchemy import _PytisBaseMetaclass as specs
    cls = specs.specification_by_class_name(name)
    if cls is None:
        by_name = specs.specifications_by_name(name)
        cls = sorted(by_name, key=lambda c: c.__name__)[0] if by_name else None
    return cls


def altered_tables(template_text):
    """Return the names of the tables altered by the template, in order of appearance.

    Comments of a table are only set when the table is created, so a migration
    altering an existing table would leave its documentation at the state of
    its creation -- a new column would have none at all.  The names found here
    get their comments refreshed at the end of the generated script.

    """
    names = []
    for match in re.finditer(r'^[ \t]*alter\s+table\s+(?:if\s+exists\s+)?(?:only\s+)?'
                             r'([^\s;(]+)', template_text, re.IGNORECASE | re.MULTILINE):
        name = match.group(1).replace('"', '').split('.')[-1]
        if name not in names:
            names.append(name)
    return names


def resolve_names(module, arguments):
    """Resolve macro arguments to gensqlalchemy specification class names.

    A macro argument may be given either as the specification class name or as
    the DB object name (the class' `name` attribute).  Projects differ in their
    naming convention (class name equal to the DB name, or its CamelCase form),
    so we look the argument up in the module's registered specifications and
    return the actual class name, which is what `gsql --limit` matches against.
    """
    names = []
    for arg in arguments:
        cls = specification(module, arg)
        if cls is None:
            sys.exit(f"gsql-update: unknown specification '{arg}' in module '{module}'")
        names.append(cls.__name__)
    return names

def find_module_path(module, start):
    """Return the directory to add to the path so that `module` is importable.

    Walk up from the directory of `start` (the template file) and, in each
    ancestor, look for the module both directly (e.g. ``<dir>/invoices/dbdefs``)
    and under a ``lib`` subdirectory (``<dir>/lib/invoices/dbdefs``).  Return the
    matching directory (or its ``lib`` subdirectory), or None if not found -- in
    which case we rely on an externally provided PYTHONPATH.
    """
    relpath = os.path.join(*module.split('.'))
    def found(path):
        return os.path.isdir(path) or os.path.isfile(path + '.py')
    directory = os.path.dirname(os.path.abspath(start))
    while True:
        if found(os.path.join(directory, relpath)):
            return directory
        if found(os.path.join(directory, 'lib', relpath)):
            return os.path.join(directory, 'lib')
        parent = os.path.dirname(directory)
        if parent == directory:  # reached the filesystem root
            return None
        directory = parent

def gsql(module, pythonpath, names, names_only=False, comments_only=False):
    args = ['--pretty=1', '--no-deps', '--plpython3',
            '--limit=^({})$'.format('|'.join(names))]
    if names_only:
        args += ['--names']
    if comments_only:
        args += ['--comments']
    output = subprocess.check_output(['gsql'] + args + [module], encoding='utf-8',
                                     env=dict(PYTHONPATH=':'.join(pythonpath),
                                              PATH=os.environ['PATH'],
                                              PYENV_ROOT=os.environ['PYENV_ROOT']))
    if 'Traceback (most recent call last):' in output:
        sys.stderr.write(output)
        sys.exit(1)
    return output

def expand_macro(module, pythonpath, command, arguments):
    names = resolve_names(module, arguments)
    if command == '@create':
        return gsql(module, pythonpath, names)
    if command == '@comments':
        return gsql(module, pythonpath, names, comments_only=True)
    if command == '@drop':
        ordered_names = gsql(module, pythonpath, names, names_only=True)
        return ''.join([f'DROP {line};\n'
                        for line in reversed(ordered_names.splitlines())])

def main():
    p = argparse.ArgumentParser()
    p.add_argument('--no-transaction', help="Don't wrap the final SQL in BEGIN/COMMIT.",
                   action='store_true', default=False)
    p.add_argument('--no-auto-comments', action='store_true', default=False,
                   help="Don't append the comments of the tables altered by the template.")
    p.add_argument('--output', '-o', help="output file name (if not defined, print to STDOUT)")
    p.add_argument('module', help="name of the Python module including the gensql DB specifications"),
    p.add_argument('filename', help="input template file name")
    args = p.parse_args()

    module_path = find_module_path(args.module, args.filename)
    if module_path and module_path not in sys.path:
        sys.path.insert(0, module_path)
    pythonpath = list(sys.path)
    try:
        template = open(args.filename, 'rt', encoding='utf-8')
    except IOError as e:
        sys.stderr.write(f'Unable to read input file {args.filename}: {e}\n')
        sys.exit(1)

    output = ''
    if not args.no_transaction:
        output += 'BEGIN;\n'

    with template:
        template_text = template.read()

    lines = iter(template_text.splitlines(keepends=True))
    for text in lines:
        if text.lstrip().startswith('--#'):
            # Template-only comment ('--#' keeps SQL-comment syntax so editors
            # still highlight it): documents the template itself (e.g. how to
            # regenerate the SQL) and is not emitted.  Ordinary SQL comments
            # ('--') are passed through.
            continue
        elif text.startswith(('@create ', '@drop ', '@comments ')):
            while not text.strip().endswith(';'):
                text += next(lines)
            macro, arguments = text.strip().rstrip(';').split(maxsplit=1)
            output += expand_macro(args.module, pythonpath,
                                   macro, [arg.strip() for arg in arguments.split(',')])
        else:
            output += text

    # Comments are only set when an object is created, so a table altered above
    # would keep the documentation it had when it was created and a new column
    # would have none.  Refresh them without having to think of it.
    names = [cls.__name__ for cls in
             (specification(args.module, name) for name in altered_tables(template_text))
             if cls is not None]
    if names and not args.no_auto_comments:
        output += ('\n-- Comments of the tables altered above (added automatically).\n'
                   + gsql(args.module, pythonpath, names, comments_only=True))

    if not args.no_transaction:
        output += '\nCOMMIT;'

    if args.output:
        with open(args.output, 'wt', encoding='utf-8') as outfile:
            outfile.write(output.rstrip('\n') + '\n')
    else:
        print(output)

if __name__ == '__main__':
    main()
