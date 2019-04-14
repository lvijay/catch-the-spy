#!/usr/bin/env python
# -*- coding: utf-8; -*-

import solve
import PyGnuplot as gp

if __name__ == '__main__':
    import sys

    a = int(sys.argv[1]) if len(sys.argv) > 2 else 2
    b = int(sys.argv[1]) if len(sys.argv) > 3 else 3
    n = 40
        
    spies = [a + b * i for i in xrange(n+1)]

    guesses = []
    time, i = 0, 0
    while time < 40:
        for aa, bb in solve.neg_pairs(i):
            location = time * bb + aa
            guesses.append(location)
            time += 1
        i += 1

    print spies
    print guesses
    meet_at = [i for i in xrange(len(spies)) if spies[i] == guesses[i]][0]
    print meet_at, spies[meet_at], guesses[meet_at]
    spies = spies[:meet_at+1]
    guesses = guesses[:meet_at+1]

    xmin, xmax = min(min(spies), min(guesses))-4, max(max(spies), max(guesses))+4
    ymin, ymax = 1, 4

    ## when b is negative, xmin will be greater
    xmin, xmax = min(xmin, xmax), max(xmin, xmax)

    prefix = '''
set terminal png size 1200, 300 enhanced #font 'Monaco,10';
set title "a=2, b=3"
set key off;
set xtics %(xmin)s+1,10,%(xmax)s-1;
set ytics scale 0;
set mxtics 5;
set format y "";
set xrange [%(xmin)s:%(xmax)s];
set yrange [%(ymin)s:%(ymax)s];
''' % { 'xmin': xmin, 'xmax': xmax, 'ymin': ymin, 'ymax': ymax }
    Xs, Ys = [], []
    Xg, Yg = [], []
    i = 0
    for xs, xg in zip(spies, guesses):
        ys, yg = 3, 2
        Xs.append(xs); Ys.append(ys)
        Xg.append(xg); Yg.append(yg)
        plotfile = 'p%03d.plot' % i
        spyfile = 'spy%03d.dat' % i
        gesfile = 'ges%03d.dat' % i
        matchx, matchy = [guesses[-1], guesses[-1]], [ymin, ymax]
        gp.s([Xs, Ys], filename=spyfile) # we won't use this anymore
        gp.s([Xg, Yg], filename=gesfile) # we won't use this anymore
        gp.s([matchx, matchy], filename='match.dat')
        with open(plotfile, 'w') as out:
            print >> out, prefix

            imgname = 'o%03d.png' % i
            print >> out, 'set output "%s";' % imgname
            print >> out, 'plot "%s" using 1:2 w lp lw 2 lc 2, \\' % spyfile
            print >> out, '     "%s" using 1:2 w lp lw 2 lc 3'     % gesfile
            i += 1

    for j in xrange(1, 11):
        plotfile = 'p%03d.plot' % (i+j)
        with open(plotfile, 'w') as out:
            print >> out, prefix

            imgname = 'o%03d.png' % (i+j)
            print >> out, 'set output "%s";' % imgname
            print >> out, 'plot "%s" using 1:2 w lp lw 2 lc 2, \\' % spyfile
            print >> out, '     "%s" using 1:2 w lp lw 2 lc 3, \\' % gesfile
            print >> out, '  "match.dat" u 1:2 w l lw 1 lc 6'

## spy-moving-ab.py ends here
