#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright (C) 2015 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
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

"""Programatical batch modification of Pytis specifications.

This script can be run on a single file, multiple files or recursively on whole
directories.  Python Abstract Syntax Tree parser is used to walk through the
source file AST tree and get information about positions of particular Python
constructs in the source file, allowing syntax aware modifications of the
source code.

These modifications are often needed when the API of Pytis specifications needs
to be changed.

The script consists of a generic infrastructure for running various
modifications and then of particullar commands implementing some modifications
which were already needed.  These commands are defined as functions with name
prefixed by 'cmd_'.  Several modifications can be run in one invocation of the
script.  Some commands may not modify the source code, they just print warnings
and let the user perform modifications manually.  The printed output is
formatted to allow easy acces to the source code points in Emacs compilation
mode.

For example:

  modify-specifications.py --type-kwargs --check-codebooks-not-null directory

Will run the functions 'cmd_type_kwargs()' and 'cmd_check_codebooks_not_null()'
on all *.py files in given directory.

Most commands are single purpose -- they are not going to be needed in the
future once they are applied on all applications.  They may remain here for
future reference and examples or they may be removed sometimes in the future as
new commands are added.

"""

import sys
import ast
import cStringIO
import os
from pytis.extensions.ast_unparser import Unparser
from pytis.util import find

def die(message, *args):
    sys.stderr.write(message % args + '\n')
    sys.exit(1)

def unparse(node):
    x = cStringIO.StringIO()
    Unparser(node, x)
    return x.getvalue()

def warn(filename, node, message=None, *args):
    if not message:
        message = unparse(node)
    elif args:
        message %= args
    print "File %s, line %d\n  %s" % (filename, node.keywords[0].value.lineno, message)

def attr(x):
    return tuple(x for x in dir(x) if not x.startswith('_'))


class Position(object):
    def __init__(self, ln, offset):
        self.ln = ln
        self.offset = offset
    

class Arg(object):
    def __init__(self, lines, kw, previous):
        self.kw = kw
        self.name = kw.arg
        self.value = value = kw.value
        ln = value.lineno - 1
        offset = value.col_offset
        while lines[ln][offset - 1] != '=':
            # First step back up to the keyword argument assignment sign as
            # there may be parens prior to the actual value.
            ln, offset = self._step_back(ln, offset, lines)
        while lines[ln][offset - 1] not in ',(':
            # Lookup the end of the previous argument or the beginning paren
            # starting at the beginning of the current argument's value.
            ln, offset = self._step_back(ln, offset, lines)
        if lines[ln][offset - 1] == ',':
            offset -= 1
        self.start = Position(ln, offset)
        if previous:
            while lines[ln].lstrip().startswith('#'):
                ln -= 1
                offset = len(lines[ln].rstrip().rstrip(','))
            previous.end = Position(ln, offset)
        if isinstance(value, (ast.Attribute, ast.Num, ast.Str, ast.Name)):
            # This will be replaced by the start point of the next arg except for the last arg.
            self.end = Position(value.lineno - 1, value.col_offset + len(unparse(value)))
        elif ((isinstance(value, (ast.Call, ast.Tuple)) and
               lines[value.lineno - 1][value.col_offset:].startswith(unparse(value)))):
            # Simple calls (typically without arguments) can be determined like this.
            self.end = Position(value.lineno - 1, value.col_offset + len(unparse(value)))
        elif ((isinstance(value, ast.Tuple) and
               lines[value.lineno - 1][value.col_offset - 1:].startswith(unparse(value)))):
            # Tuples seem to have coll_offset after the initial paren.
            self.end = Position(value.lineno - 1, value.col_offset - 1 + len(unparse(value)))
        else:
            self.end = None

    def _step_back(self, ln, offset, lines):
        offset -= 1
        if offset == 0:
            ln -= 1
            offset = len(lines[ln])
        return ln, offset


class FieldLocator(ast.NodeVisitor):
    
    def __init__(self, process_fields=True, process_customize_fields=True, process_override=True):
        self._process_fields = process_fields
        self._process_customize_fields = process_customize_fields
        self._process_override = process_override

    def visit_FunctionDef(self, node):
        if node.name == '_customize_fields':
            self._inside_customize_fields = True
            try:
                self.generic_visit(node)
            finally:
                self._inside_customize_fields = False
        else:
            self.generic_visit(node)

    def visit_Assign(self, node):
        if ((len(node.targets) == 1 and hasattr(node.targets[0], 'id')
             and node.targets[0].id in ('override', 'overriden'))):
            self._inside_override = True
            try:
                self.generic_visit(node)
            finally:
                self._inside_override = False
        else:
            self.generic_visit(node)

    def visit_Call(self, node):
        def fname_in(*names):
            f = node.func
            return hasattr(f, 'id') and f.id in names or hasattr(f, 'attr') and f.attr in names
        if ((fname_in('Field') and self._process_fields and 
             (self._process_override or not self._inside_override)
             or
             fname_in('modify', 'modify_many', 'modify_except')
             and self._inside_customize_fields and self._process_customize_fields)):
            args = []
            previous = None
            for kw in node.keywords:
                arg = Arg(self._lines, kw, previous)
                args.append(arg)
                previous = arg
            self._found.append((node, args))
        elif fname_in('_inherited_fields'):
            self._inside_inherited_fields = True
            try:
                self.generic_visit(node)
            finally:
                self._inside_inherited_fields = False
        else:
            self.generic_visit(node)

    def search_fields(self, lines, filename):
        self._found = []
        self._lines = lines
        self._inside_customize_fields = False
        self._inside_override = False
        self._inside_inherited_fields = False
        self.visit(ast.parse(''.join(lines), filename))
        return self._found

#------------------------------------------------------------------------------------------------

def cmd_set_codebooks_not_null(filename, lines):
    """Add explicit not_null=True to field specifications with codebook or enumerator.
    
    This must be done as a preparation for the planned pytis change of the
    implicit not_null dependency on codebook and enumerator.
    
    """
    for node, arguments in FieldLocator(True, False, False).search_fields(lines, filename):
        args = dict((a.name, a) for a in arguments)
        if 'not_null' not in args:
            for cb in (args.get('codebook'), args.get('enumerator')):
                val = cb and cb.value
                if cb and not (isinstance(val, ast.Name) and val.id == 'None'):
                    if not (isinstance(val, ast.Str) # String values are ok
                            or (isinstance(val, ast.Attribute) and 
                                val.attr[0] == val.attr[0].upper())
                            or (isinstance(val, ast.Name) and
                                val.id[0] == val.id[0].upper())
                            or (isinstance(val, ast.Call) and
                                isinstance(val.func, ast.Attribute) and
                                val.func.attr in ('enum', 'FixedEnumerator'))
                            or (isinstance(val, ast.Call) and
                                isinstance(val.func, ast.Name) and
                                val.func.id in ('enum', 'FixedEnumerator'))):
                        warn(filename, node, "Can't determine '%s' value: %s",
                             cb.name, unparse(val))
                    else:
                        ln = cb.start.ln
                        offset = cb.start.offset
                        lines[ln] = lines[ln][:offset] + ', not_null=True' + lines[ln][offset:]
                    break

def cmd_revert_set_codebooks_not_null_in_modify(filename, lines):
    """Revert specific cases of previous wrong cmd_set_codebooks_not_null.
    
    cmd_set_codebooks_not_null is now fixed, this is left jast as an example...
    
    """
    lines_to_delete = []
    for node, arguments in FieldLocator(False, True).search_fields(lines, filename):
        args = dict((a.name, a) for a in arguments)
        cb = args.get('codebook') or args.get('enumerator')
        nn = args.get('not_null')
        if cb and nn and nn.value.id == 'True':
            if cb.start.offset == nn.end.offset and cb.start.ln == nn.end.ln:
                so, eo, sl, el = nn.start.offset, nn.end.offset, nn.start.ln, nn.end.ln
                lines[sl] = lines[sl][:so] + lines[el][eo:]
                lines_to_delete.extend(range(sl+1, el+1))
    for i, ln in enumerate(sorted(lines_to_delete)):
        del lines[ln - i]

def cmd_check_codebooks_not_null(filename, lines):
    for node, arguments in FieldLocator(True, False).search_fields(lines, filename):
        args = dict((a.name, a) for a in arguments)
        if (((args.get('codebook') or args.get('enumerator'))
             and not args.get('not_null') and not args.get('inherit'))):
            warn(filename, node)

def cmd_type_kwargs(filename, lines):
    """Convert type kwargs in field specifications to type instance aruments."""
    lines_to_delete = []
    type_kwargs = ('not_null', 'unique', 'constraints', 'minlen', 'maxlen', 'minimum', 'maximum',
                   'encrypted', 'precision', 'format', 'mindate', 'maxdate', 'utc',
                   'validation_messages', 'inner_type',
                   'minsize', 'maxsize', 'formats', 'strength', 'md5', 'verify', 'text',)
    for node, args in FieldLocator().search_fields(lines, filename):
        #print "*", filename, node.lineno, node.args and unparse(node.args[0]) or '?'
        type_arg = None
        type_args = []
        for arg in reversed(args):
            #print "  -", arg.start, arg.end, unparse(arg.kw), arg.name, arg.value
            if arg.name == 'type' and isinstance(arg.value, (ast.Name, ast.Attribute, ast.Call)):
                type_arg = arg
            elif arg.name in type_kwargs:
                type_args.insert(0, arg)
                if arg.end is None:
                    # The end of the last argument may not be always obvious!
                    print "File %s, line %d\n  Can't determine end of %s" % \
                        (filename, arg.start.ln + 1, unparse(arg.kw))
                else:
                    lines[arg.start.ln] = (lines[arg.start.ln][:arg.start.offset] +
                                           lines[arg.end.ln][arg.end.offset:])
                    for ln in range(arg.start.ln + 1, arg.end.ln + 1):
                        lines_to_delete.append(ln)
                    if type_arg and type_arg.end.ln == arg.end.ln:
                        type_arg.end.ln = arg.start.ln
                        type_arg.end.offset -= arg.end.offset - arg.start.offset
        unparsed_type_args = ', '.join([unparse(a.kw) for a in type_args])
        if type_arg and (unparsed_type_args or not isinstance(type_arg.value, ast.Call)):
            if not type_arg.end:
                field_id = node.args and unparse(node.args[0]) or '?'
                print ("File %s, line %d\n"
                       "  Can't detect end of '%s' field type specification.\n"
                       "  Please reformat the source code (should not be the last argument).") % \
                    (filename, node.lineno, field_id)
                insert = None
            else:
                ln, offset = type_arg.end.ln, type_arg.end.offset
                assert lines[ln][:offset].endswith(unparse(type_arg.value)), \
                    "%s line %d: '%s', '%s' " % (filename, ln, lines[ln][:offset],
                                                 unparse(type_arg.value))
                if isinstance(type_arg.value, ast.Call):
                    offset -= 1
                    insert = unparsed_type_args
                    if lines[ln][offset - 1] != '(':
                        insert = ', ' + insert
                else:
                    insert = '(' + unparsed_type_args + ')'
        elif type_args:
            ln, offset = type_args[0].start.ln, type_args[0].start.offset
            argnames = [a.name for a in type_args]
            
            if 'maxlen' in argnames:
                type_cls = 'pd.String'
            elif 'precision' in argnames:
                type_cls = 'pd.Float'
            elif 'utc' in argnames:
                type_cls = 'pd.DateTime'
            elif 'format' in argnames:
                fmt = unparse(find('format', type_args, key=lambda a: a.name).value)
                if fmt.startswith('pd.Date.'):
                    type_cls = 'pd.Date'
                elif fmt.startswith('pd.Time.'):
                    type_cls = 'pd.Time'
                elif fmt.startswith('pd.DateTime.'):
                    type_cls = 'pd.DateTime'
                else:
                    type_cls = None
            else:
                type_cls = None
            if type_cls:
                insert = ', type=%s(%s)' % (type_cls, unparsed_type_args)
            elif [name for name in argnames if name not in ('not_null', 'unique')]:
                field_id = node.args and unparse(node.args[0]) or '?'
                print "File %s, line %d\n  Can't determine data type of field %s (%s)" % \
                    (filename, node.lineno, field_id, unparsed_type_args)
                insert = None
            else:
                insert = ', ' + unparsed_type_args
        else:
            insert = None
        if insert:
            lines[ln] = lines[ln][:offset] + insert + lines[ln][offset:]
    for i, ln in enumerate(sorted(lines_to_delete)):
        del lines[ln - i]

#------------------------------------------------------------------------------------------------

def run_commands(commands, filename, no_act=False):
    if os.path.isfile(filename) and filename.endswith('.py'):
        lines = open(filename).readlines()
        original_text = ''.join(lines)
        for command in commands:
            command(filename, lines)
        new_text = ''.join(lines)
        if new_text != original_text:
            try:
                ast.parse(new_text)
            except SyntaxError as e:
                text_lines = new_text.splitlines() # Resplit to make sure that line numbers match.
                print "File %s, line %d\n  Invalid syntax after conversion:" % (filename, e.lineno)
                print ''.join(['  %s%d: %s\n' % ('>' if ln + 1 == e.lineno else ' ', 
                                                ln + 1, text_lines[ln])
                               for ln in range(max(0, e.lineno - 6),
                                               min(e.lineno + 2, len(text_lines) - 1))])
                sys.exit(1)
            else:
                #print ''.join(['%d: %s' % (ln, lines[ln]) for ln in range(len(lines))])
                if not no_act:
                    open(filename, 'w').write(new_text)
    elif os.path.isdir(filename):
        for x in os.listdir(filename):
            run_commands(commands, os.path.join(filename, x), no_act=no_act)

if __name__ == '__main__':
    no_act = False
    commands = []
    i = 1 # Start from the second item (the first argument).
    while i < len(sys.argv) and sys.argv[i].startswith('-'):
        arg = sys.argv[i]
        i += 1
        if arg in ('-n', '--no-act'):
            no_act = True
        elif arg.startswith('--'):
            try:
                command = locals()['cmd_' + arg[2:].replace('-', '_')]
            except KeyError:
                die("Invalid argument: %s", arg)
            else:
                commands.append(command)
        else:
            die("Invalid argument: %s", arg)
    if not commands:
        die("No command given. Pass at least one of the following commands:\n%s",
            ''.join('  --' + x[4:].replace('_', '-') + '\n'
                    for x in locals() if x.startswith('cmd_')))
    if not sys.argv[i:]:
        die("No files or directories given.")
    for path in sys.argv[i:]:
        if not os.path.exists(path):
            die("Invalid path: %s", path)
        run_commands(commands, path, no_act=no_act)
