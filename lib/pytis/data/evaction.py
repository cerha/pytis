# -*- coding: utf-8 -*-

# Copyright (C) 2007, 2009 Brailcom, o.p.s.
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


"""Parsing of the PostgreSQL pg_rewrite.ev_action.structure."""


import string


class EvparseException(Exception):

    def __init__ (self, kind, text):
        if len(text) > 14:
            text = text[:10] + '...'
        Exception.__init__(self, 'ev_action_parse_error on '+kind, text)

def pg_parse_ev_action(text):
    value, text = _evparse(text)
    if text != '':
        raise EvparseException('extra text', text)
    return value

def _evparse(text):
    try:
        char = text[0]
        if char == '(':
            return _evparse_list(text[1:])
        elif char == '{':
            return _evparse_structure(text[1:])
        elif char == '<':
            return _evparse_empty(text[1:])
        elif char == ' ':
            return _evparse(text[1:]) 
        elif char == ':':
            return _evparse_identifier(text[1:], skip_whitespace=False)
        elif char == '"':
            return _evparse_string(text[1:])
        elif char in string.digits + '-':
            return _evparse_number(text)
        elif text[:4] == 'true' or text[:5] == 'false':
            return _evparse_boolean(text)
        elif char == '[':
            return _evparse_array(text[1:])
        else:
            return _evparse_identifier(text)
    except IndexError:
        raise EvparseException('premature end', text)

def _evparse_list(text):
    list = []
    def parse(text):
        if text[0] == ')':
            return True, text[1:]
        value, new_text = _evparse(text)
        list.append(value)
        return False, new_text
    while True:
        finished, text = parse(text)
        if finished:
            break
    return list, text    

def _evparse_structure(text):
    name, text = _evparse_identifier(text)
    structure = {'__name': name}
    def parse(text):
        i = 0
        while text[i] == ' ':
            i = i + 1
        text = text[i:]
        if text[0] == '}':
            return True, text[1:]
        if text[0] == '[':
            __ignore, text = _evparse_array(text[1:])
            return parse(text)
        elif text[0] != ':':
            raise EvparseException('structure slot name', text)
        ident, text_1 = _evparse_identifier(text[1:])
        value, new_text = _evparse(text_1)
        structure[ident] = value
        return False, new_text
    while True:
        finished, text = parse(text)
        if finished:
            break
    return structure, text

def _evparse_empty(text):
    if text[0] == '>':
        return None, text[1:]
    else:
        raise EvparseException('empty', text)

def _evparse_identifier(text, skip_whitespace=True):
    if skip_whitespace:
        while text[0] == ' ':
            text = text[1:]
    i = 0
    letters = string.ascii_letters + string.digits + '_*?'
    while True:
        char = text[i]
        if char == '\\':
            i = i + 2
        elif char in letters:
            i = i + 1
        else:
            break
    if i == 0:
        raise EvparseException('identifier', text)
    return text[:i], text[i:]

def _evparse_string(text):
    i = 0
    while text[i] != '"':
        i = i + 1
    return text[:i], text[i+1:]
    
def _evparse_boolean(text):
    if text[:len('false')] == 'false':
        return False, text[len('false'):]
    elif text[:len('true')] == 'true':
        return True, text[len('true'):]
    else:
        raise EvparseException('boolean', text)

def _evparse_number(text):
    if text[0] == '-':
        i = 1
        sign = -1
    else:
        i = 0
        sign = 1
    while text[i] in string.digits:
        i = i + 1
    return sign * int(text[:i]), text[i:]

def _evparse_array(text):
    i = 0
    while text[i] != ']':
        i = i + 1
    return None, text[i+1:]
