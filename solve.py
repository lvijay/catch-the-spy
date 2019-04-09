#!/usr/bin/env python
# -*- coding: utf-8; -*-

## spy-puzzle -- represent and solve a puzzle from the book,
## "Algorithmic Puzzles", by Anany Levitin and Maria Levitin, (2011,
## Oxford University Press).
##
## Copyright (c) 2019, Vijay Lakshminarayanan <laksvij@hawk.iit.edu>
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU Affero General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

## In a computer game, a spy is located on a one-dimensional line. At
## time `0`, the spy is at location `a`. With each time interval, the
## spy moves `b` units to the right if `b≥0`, and `|b|` units to the
## left if `b<0`.  Both `a` and `b` are fixed integers, but they are
## unknown to you. Your goal is to identify the spy’s location by
## asking at each time interval (starting at time 0) whether the spy
## is currently at some location of your choosing. For example, you
## can ask whether the spy is currently at location 19, to which you
## will receive a truthful yes/no answer. If the answer is “yes,” you
## reach your goal; if theanswer is “no,” you can ask the next time
## whether the spy is at the same or another location of your
## choice. Devise an algorithm that will find the spy after a finite
## number questions.

import httplib

def pairs(n):
    l = []
    for i in xrange(n+1):
        l.append((i, n))
        if n != i: l.append((n, i))
    return l

def neg_pairs(n):
    if n == 0:
        return pairs(n)
    l = []
    for a, b in pairs(n):
        l.append(( a,  b))
        if b > 0:           l.append(( a, -b))
        if a > 0:           l.append((-a,  b))
        if a > b and b > 0: l.append((-a, -b))
    return l

class Game(object):
    def __init__(self, host='localhost', port=4242):
        self.gameid = None
        self.time = 0
        self.finished = False
        self.connection = httplib.HTTPConnection(host, port) # reuse connection
        self.a = None
        self.b = None
    def start(self):
        self.connection.request(method='GET', url='/')
        resp = self.connection.getresponse()
        self.gameid = resp.getheader('x-spy-game-id')
        resp.data = resp.read()
        return resp
    def solve(self):
        i = 0
        while not self.finished:
            for a, b in neg_pairs(i):
                location = self.time * b + a
                self.guess(location)
                if self.finished:
                    game.a, game.b = a, b
                    break
            i += 1
    def guess(self, location):
        path = '/locate?location=%d&gameid=%s' % (location, self.gameid)
        self.connection.request(method='GET', url=path)
        resp = self.connection.getresponse()
        self.finished = resp.getheader('x-spy-found') == 'true'
        self.time += 1
        resp.data = resp.read()
        return resp

if __name__ == '__main__':
    game = Game()
    game.start()
    game.solve()
    print 'Found the spy at time %d a=%d b=%d' % (game.time, game.a, game.b)
    
## solve.py ends here
