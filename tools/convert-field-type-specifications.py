#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import ast
import cStringIO
import os
from pytis.extensions.ast_unparser import Unparser
from pytis.util import find

TYPE_KWARGS = ('not_null', 'unique', 'constraints', 'minlen', 'maxlen', 'minimum', 'maximum',
               'encrypted', 'precision', 'format', 'mindate', 'maxdate', 'utc',
               'validation_messages', 'inner_type',
               'minsize', 'maxsize', 'formats', 'strength', 'md5', 'verify', 'text',)

make_changes = True

def unparse(node):
    x = cStringIO.StringIO()
    Unparser(node, x)
    return x.getvalue()


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
    
    def visit_FunctionDef(self, node):
        if node.name == '_customize_fields':
            self._inside_customize_fields = True
            self.generic_visit(node)
            self._inside_customize_fields = False
        else:
            self.generic_visit(node)

    def visit_Call(self, node):
        def fname_in(*names):
            f = node.func
            return hasattr(f, 'id') and f.id in names or hasattr(f, 'attr') and f.attr in names
        if ((fname_in('Field') or
             fname_in('modify', 'modify_many', 'modify_except') and self._inside_customize_fields)):
            args = []
            previous = None
            for kw in node.keywords:
                arg = Arg(self._lines, kw, previous)
                args.append(arg)
                previous = arg
            self._found.append((node, args))
        else:
            self.generic_visit(node)

    def search_fields(self, lines, filename):
        self._found = []
        self._lines = lines
        self._inside_customize_fields = False
        self.visit(ast.parse(''.join(lines), filename))
        return self._found


def convert(filename):
    lines = open(filename).readlines()
    original_text = ''.join(lines)
    locator = FieldLocator()
    lines_to_delete = []
    for node, args in locator.search_fields(lines, filename):
        #print "*", filename, node.lineno, node.args and unparse(node.args[0]) or '?'
        type_arg = None
        type_args = []
        for arg in reversed(args):
            #print "  -", arg.start, arg.end, unparse(arg.kw), arg.name, arg.value
            if arg.name == 'type' and isinstance(arg.value, (ast.Name, ast.Attribute, ast.Call)):
                type_arg = arg
            elif arg.name in TYPE_KWARGS:
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
    new_text = ''.join(lines)
    if new_text != original_text:
        try:
            ast.parse(new_text)
        except SyntaxError as e:
            print "File %s, line %d\n  Invalid syntax after conversion:" % (filename, e.lineno)
            print ''.join(['%d: %s' % (ln, lines[ln]) for ln
                           in range(max(0, e.lineno - 6), min(e.lineno + 2, len(lines) - 1))])
            sys.exit(1)
        else:
            #print ''.join(['%d: %s' % (ln, lines[ln]) for ln in range(len(lines))])
            if make_changes:
                open(filename, 'w').write(new_text)

def run(filename):
    if os.path.isfile(filename) and filename.endswith('.py'):
        convert(filename)
    elif os.path.isdir(filename):
        for x in os.listdir(filename):
            run(os.path.join(filename, x))

if __name__ == '__main__':
    for arg in sys.argv[1:]:
        if arg in ('-n', '--no-act'):
            make_changes = False
        else:
            run(arg)
