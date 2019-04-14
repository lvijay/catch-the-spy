#!/usr/bin/env python
# -*- coding: utf-8; -*-

import solve
import PyGnuplot as gp

if __name__ == '__main__':
    import sys

    a = int(sys.argv[1]) if len(sys.argv) > 2 else 0
    b = int(sys.argv[1]) if len(sys.argv) > 3 else 8
    n = 17
        
    spies = [a + b * i for i in xrange(1, n+1)]
    counts = [((-1)**i)*i/2 for i in xrange(n)]
    guesses = [v * (i+1) for i, v in enumerate(counts)]
    
    xmin, xmax = min(min(spies), min(guesses))-4, max(max(spies), max(guesses))+4
    ymin, ymax = 1, 4

    ## when b is negative, xmin will be greater
    xmin, xmax = min(xmin, xmax), max(xmin, xmax)

    prefix = '''
set terminal png size 1000, 200 enhanced #font 'Monaco,10';
set title "a=0, b=8"
set key off;
set xtics %(xmin)s,16,%(xmax)s;
set ytics scale 0;
set format y "";

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
        matchx, matchy = [136, 136], [ymin, ymax]
        gp.s([Xs, Ys], filename=spyfile) # we won't use this anymore
        gp.s([Xg, Yg], filename=gesfile) # we won't use this anymore
        gp.s([matchx, matchy], filename='match.dat')
        with open(plotfile, 'w') as out:
            print >> out, prefix
            print >> out, 'set xrange [%s:%s];' % (xmin, xmax)
            print >> out, 'set yrange [%s:%s];' % (ymin, ymax)

            imgname = 'o%03d.png' % i
            print >> out, 'set output "%s";' % imgname
            print >> out, 'plot "%s" using 1:2 w lp lw 2 lc 2, \\' % spyfile
            print >> out, '     "%s" using 1:2 w lp lw 2 lc 3'     % gesfile
            i += 1

    for j in xrange(10):
        plotfile = 'p%03d.plot' % (i+1)
        with open(plotfile, 'w') as out:
            print >> out, prefix
            print >> out, 'set xrange [%s:%s];' % (xmin, xmax)
            print >> out, 'set yrange [%s:%s];' % (ymin, ymax)

            imgname = 'o%03d.png' % i
            print >> out, 'set output "%s";' % imgname
            print >> out, 'plot "%s" using 1:2 w lp lw 2 lc 2, \\' % spyfile
            print >> out, '     "%s" using 1:2 w lp lw 2 lc 3, \\' % gesfile
            print >> out, '  "match.dat" u 1:2 w l lw 1 lc 6'
            i += 1


## draw-solution.py ends here
