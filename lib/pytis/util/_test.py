#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2008, 2011 Brailcom, o.p.s.
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


import operator
import StringIO
import unittest

import util
import test
import caching


tests = test.TestSuite()


###########
# util.py #
###########


class Counter(unittest.TestCase):
    def test_it(self):
        counter = util.Counter()
        assert counter.current() == 0
        assert counter.next() == 1
        assert counter.next() == 2
        assert counter.next() == 3
        assert counter.current() == 3
        counter.reset()
        assert counter.current() == 0
        assert counter.next() == 1
        assert counter.next() == 2
        assert counter.current() == 2

class Pipe(unittest.TestCase):
    def test_it(self):
        p = util.Pipe()
        p.write('foo')
        p.write('bar')
        r = p.read(4)
        assert r == 'foob', r
        p.write('baz')
        p.close()
        r = p.read()
        assert r == 'arbaz', r
        r = p.read()
        assert r is None, r
    def test_cc(self):
        s = StringIO.StringIO()
        p = util.Pipe(cc=s)
        p.write('foo')
        p.write('bar')
        r = p.read(4)
        assert r == 'foob', r
        p.write('baz')
        assert s.getvalue() == 'foobarbaz', s.getvalue()
        p.close()
        r = p.read()
        assert r == 'arbaz', r
        r = p.read()
        assert r is None, r
    def test_multi_cc(self):
        s = StringIO.StringIO()
        s1 = StringIO.StringIO()
        p = util.Pipe(cc=[s,s1])
        p.write('foo')
        p.write('bar')
        r = p.read(4)
        assert r == 'foob', r
        p.write('baz')
        assert s.getvalue() == 'foobarbaz', s.getvalue()
        assert s1.getvalue() == 'foobarbaz', s.getvalue()
        p.close()
        r = p.read()
        assert r == 'arbaz', r
        r = p.read()
        assert r is None, r
tests.add(Pipe)


class Popen(unittest.TestCase):
    COMMAND = 'tr a b'
    SCOMMAND = ['tr', 'a', 'b']
    STRING = 'foobar'
    RSTRING = 'foobbr'
    def _test_auto(self, command):
        popen = util.Popen(command)
        from_, to = popen.from_child(), popen.to_child()
        to.write(self.STRING)
        to.close()
        assert from_.read() == self.RSTRING
    def test_auto(self):
        self._test_auto(self.COMMAND)
    def test_command_as_sequence(self):
        self._test_auto(self.SCOMMAND)
    def test_explicit(self):
        cat = 'cat'
        popen1 = util.Popen(cat)
        popen3 = util.Popen(cat)
        popen2 = util.Popen(self.COMMAND,
                            to_child=popen1.from_child(),
                            from_child=popen3.to_child())
        out = popen1.to_child()
        out.write(self.STRING)
        out.close()
        assert popen3.from_child().read() == self.RSTRING
tests.add(Popen)


class Stack(unittest.TestCase):
    def test_it(self):
        stack = util.Stack()
        assert stack.empty()
        a = 'AAA'
        stack.push(a)
        assert not stack.empty()
        assert stack.top() is a
        b = 'BBB'
        stack.push(b)
        assert not stack.empty()
        assert stack.top() is b
        stack.pop()
        assert not stack.empty()
        assert stack.top() is a
        stack.pop()
        assert stack.empty()
tests.add(Stack)
        

class XStack(unittest.TestCase):
    def test_it(self):
        xstack = util.XStack()
        assert xstack.empty()
        assert xstack.active() is None
        assert xstack.prev() is None and xstack.next() is None
        a = 'AAA'
        b = 'BBB'
        c = 'CCC'
        d = 'DDD'
        xstack.push(a)
        assert xstack.items() == (a,)
        assert xstack.active() is a
        assert xstack.prev() is None and xstack.next() is None
        xstack.push(b)
        assert xstack.items() == (b, a,)
        assert xstack.active() is b
        assert xstack.prev() is a
        assert xstack.next() is a
        xstack.push(c)
        assert xstack.items() == (c, b, a,)
        assert xstack.active() is c
        assert xstack.prev() is a
        assert xstack.next() is b
        xstack.activate(a)
        assert xstack.active() is a
        assert xstack.prev() is b
        assert xstack.next() is c
        xstack.activate(b)
        assert xstack.active() is b
        assert xstack.prev() is c
        assert xstack.next() is a
        xstack.remove(b)
        assert xstack.items() == (c, a,)
        assert xstack.active() is a
        xstack.push(d)
        assert xstack.items() == (c, d, a,)
        assert xstack.active() is d
        xstack.pop()
        xstack.pop()
        assert xstack.active() is c
        assert xstack.prev() is None and xstack.next() is None
tests.add(XStack)


class Sameclass(unittest.TestCase):
    class A:
        pass
    class B:
        pass
    def test_function(self):
        assert util.sameclass(Sameclass.A(), Sameclass.A()), \
               'same classes not recognized'
        assert not util.sameclass(Sameclass.A(), Sameclass.B()), \
               'different classes not recognized'
        assert util.sameclass(1, 1), 'same inteeger classes not recognized'
        assert not util.sameclass(1, 1.0), 'different classes not recognized'
tests.add(Sameclass)


class CompareObjects(unittest.TestCase):
    class A:
        pass
    class B:
        pass
    def test_function(self):
        o1 = None
        o2 = 'foo'
        o3 = 'bar'
        o4 = CompareObjects.A()
        o5 = CompareObjects.A()
        o6 = CompareObjects.B()
        assert util.compare_objects(o1, o1) == 0, 'comparison error'
        assert util.compare_objects(o4, o4) == 0, 'comparison error'
        assert util.compare_objects(o4, o5) != 0, 'comparison error'
        assert util.compare_objects(o1, o4) < 0, 'comparison error'
        assert util.compare_objects(o6, o2) > 0, 'comparison error'
        assert util.compare_objects(o5, o6) != 0, 'comparison error'
        assert util.compare_objects(o1, o2) != 0, 'comparison error'
        assert util.compare_objects(o2, o3) > 0, 'comparison error'
        assert util.compare_objects(o3, o2) < 0, 'comparison error'
tests.add(CompareObjects)


class MiscFunctions(unittest.TestCase):
    def test_is_sequence(self):
        assert util.is_sequence((1, 2, 3)), 'sequence not a sequence'
        assert util.is_sequence(()), 'sequence not a sequence'
        assert util.is_sequence([]), 'sequence not a sequence'
        assert util.is_sequence([1]), 'sequence not a sequence'
        assert not util.is_sequence(None), 'non-sequence as sequence'
        assert not util.is_sequence(self), 'non-sequence as sequence'
        assert not util.is_sequence({}), 'non-sequence as sequence'
    def test_safedel(self):
        d = {'alpha': 1, 'beta': 2, 'gamma': 3}
        l = ['alpha', 'beta', 'gamma']
        assert util.safedel(d, 'delta') == d
        assert util.safedel (l, 3) == l
        dx = util.safedel(d, 'alpha')
        assert dx == {'beta': 2, 'gamma': 3}
        lx = util.safedel(l, 1)
        assert lx == ['alpha', 'gamma']
        assert util.safedel({}, 'foo') == {}
        assert util.safedel([], 2) == []
tests.add(MiscFunctions)


class ListUtils(unittest.TestCase):
    def test_some(self):
        T = True
        F = False
        assert not util.some(util.identity, ())
        assert util.some(util.identity, (T,))
        assert not util.some(util.identity, (F,))
        assert util.some(util.identity, (F, T))
        import operator
        assert util.some(operator.and_, (T, T), (T, F))
        def all3(x,y,z):
            return x and y and z
        assert not util.some(all3, (T, T), (T, F), (F, T))
    def test_remove_duplicates(self):
        assert util.remove_duplicates([]) == [], 'empty list failed'
        assert util.remove_duplicates([1, 1]) == [1]
        l = util.remove_duplicates([2, 2, 1, 3, 1, 2])
        l.sort()
        assert l == [1, 2, 3]
    def test_flatten(self):
        result = util.flatten([])
        assert result == [] or result == (), result
        result = util.flatten([[([])]])
        assert result == [] or result == (), result
        result = util.flatten([[1,2],3,[[4]],[(5,[6,7],8)]])
        assert result == [1,2,3,4,5,6,7,8] or result == (1,2,3,4,5,6,7,8), \
               result
    def test_nreverse(self):
        assert util.nreverse([1,2,3]) == [3,2,1]
        assert util.nreverse([]) == []
tests.add(ListUtils)


class CopyStream(unittest.TestCase):
    def test_it(self):
        value = 'abc' + 'x'*10000
        import StringIO
        input = StringIO.StringIO(value)
        input.seek(0)
        output = StringIO.StringIO(value)
        util.copy_stream(input, output)
        assert output.getvalue() == value


class FindUtils(unittest.TestCase):
    _ALIST = [(0, 5), (1, 6), (1, 7)]
    def test_position(self):
        assert util.position(42, []) == None
        assert util.position(42, [42]) == 0
        assert util.position(42, [1, 42]) == 1
        assert util.position(42, [1, 42, 2]) == 1
        assert util.position(42, [1, 2, 3]) == None
    def test_find(self):
        assert util.find(42, []) == None
        assert util.find(42, [42.0]) == 42
        assert util.find(42, [1, 2, 3]) == None
        assert util.find(42, [-41, -42, -43], key=operator.neg) == -42
        assert util.find(42, [41, 42, 43, 44], test=(lambda x, y: x<y)) == 43
        assert util.find(42, [-40, -41, -42, -43], key=operator.neg,
                         test=(lambda x, y: x<y)) == -43
    def test_assoc(self):
        assert util.assoc(1, self._ALIST) == (1, 6)
        assert util.assoc(2, self._ALIST) == None
        assert util.assoc(5, self._ALIST) == None
    def test_rassoc(self):
        assert util.rassoc(1, self._ALIST) == None
        assert util.rassoc(5, self._ALIST) == (0, 5)
    def test_ecase(self):
        settings = ((1, 'alpha'), (2, 'beta'), (3, 'gamma'), (3, 'delta'))
        assert util.ecase(1, *settings) == 'alpha'
        assert util.ecase(3, *settings) == 'gamma'
        try:
            util.ecase(0, *settings)
            ok = 0
        except util.ProgramError:
            ok = 1
        assert ok
tests.add(FindUtils)


class StringFunctions(unittest.TestCase):
    def test_starts_with(self):
        assert util.starts_with('abcdef', 'abc')
        assert util.starts_with('abc', '')
        assert not util.starts_with('foo', 'bar')
        assert not util.starts_with('foobar', 'bar')
tests.add(StringFunctions)


class NonLocalTransfers(unittest.TestCase):
    def _function(self, arg):
        if arg == 'foo':
            return 1
        elif arg == 'bar':
            util.throw('foobar', 2)
        else:
            util.throw('foobar')
    def test_basic(self):
        assert util.catch('foobar', self._function, 'foo') == 1
        assert util.catch('foobar', self._function, 'bar') == 2
        assert util.catch('foobar', self._function, 'foobar') is None
    def test_tags(self):
        try:
            util.catch('foo', self._function, 'foobar')
        except util._Throw:
            pass
        else:
            raise Exception('Uncaughtable throw catched')
        util.catch('foobar', util.catch, 'foo', self._function, 'bar')
tests.add(NonLocalTransfers)


class DevNullStream(unittest.TestCase):
    def test_read(self):
        f = util.dev_null_stream('r')
        assert f.read() == ''
        assert f.read() == ''
    def test_write(self):
        f = util.dev_null_stream('w')
        f.write('a'*100)
        f.write('b'*100000)
        f.close()
tests.add(DevNullStream)


class Mktempdir(unittest.TestCase):
    def test_it(self):
        import os
        import config
        dir1 = dir2 = dir3 = None
        try:
            dir1 = util.mktempdir('foo')
            dir2 = util.mktempdir('bar')
            dir3 = util.mktempdir('foo')
            assert dir1 != dir3, dir1
            for d in dir1, dir2, dir3:
                assert (os.stat(d)[0] & 0o7777) == 0700, (d, os.stat(d)[0])
        finally:
            for d in dir1, dir2, dir3:
                if d:
                    try:
                        os.rmdir(d)
                    except:
                        pass
tests.add(Mktempdir)


class Classes(unittest.TestCase):
    class _A:
        x = 1
    class _B(_A):
        y = 2
    class _C(_A): pass
    class _D(_B, _C): pass
    def test_next_subclass(self):
        x = self._D()
        assert util.next_subclass(self._D, x) == self._B
        assert util.next_subclass(self._C, x) == None
        assert util.next_subclass(self._B, x) == self._A
        assert util.next_subclass(self._A, x) == self._C
    def test_public_attributes(self):
        result = util.public_attributes(self._B)
        result = list(result)
        result.sort()
        assert result == ['x', 'y'], ('Invalid public attributes', result)
    def test_direct_public_members(self):
        assert util.direct_public_members(self._A) == ('x',)
        assert util.direct_public_members(self._B) == ('y',)
        assert util.direct_public_members(self._C) == ()
        assert util.direct_public_members(self._D) == ()
        self_members = list(util.direct_public_members(self))
        self_members.sort()
        assert self_members == ['test_direct_public_members',
                                'test_next_subclass',
                                'test_public_attributes']
tests.add(Classes)


class Caching(unittest.TestCase):
    def test_simple_cache(self):
        counter = [0]
        def get(key):
            counter[0] += 1
            return counter[0]
        c = caching.SimpleCache(get)
        assert c['a'] == 1
        assert c['b'] == 2
        assert c['a'] == 1
        c.reset()
        assert c['a'] == 3
tests.add(Caching)


################


def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main()
