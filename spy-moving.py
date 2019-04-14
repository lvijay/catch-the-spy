#!/usr/bin/env python
# -*- coding: utf-8; -*-

import solve
import PyGnuplot as gp

if __name__ == '__main__':
    import sys

    n = int(sys.argv[1]) if len(sys.argv) > 1 else 20
    a = int(sys.argv[1]) if len(sys.argv) > 2 else 12
    b = int(sys.argv[1]) if len(sys.argv) > 3 else 5
    locs = [a + b * t for t in xrange(n)]

    xmin, xmax = 0, str(max(locs) + 1)
    ymin, ymax = 1, 3

    if xmin > xmax:
        xmin, xmax = xmax, xmin

    prefix = '''
set terminal png size 1200, 300 enhanced #font 'Monaco,10';
set key off;
set title "a=12, b=5"
set xtics %(xmin)s,5,%(xmax)s;
set ytics scale 0;
set format y "";

''' % { 'xmin': xmin, 'xmax': xmax, 'ymin': ymin, 'ymax': ymax }
    X = []
    Y = []
    i = 0
    for x in locs:
        y = 2
        X.append(x)
        Y.append(y)
        plotfile = 'p%03d.plot' % i
        datfile = '%03d.dat' % i
        gp.s([X, Y], filename=datfile) # we won't use this anymore
        with open(plotfile, 'w') as out:
            print >> out, prefix
            print >> out, 'set xrange [%s:%s];' % (xmin, xmax)
            print >> out, 'set yrange [%s:%s];' % (ymin, ymax)

            imgname = 'o%03d.png' % i
            print >> out, 'set output "%s";' % imgname
            print >> out, 'plot "%s" using 1:2 w lp lw 2 lc 2' % datfile
            i += 1

## draw-solution.py ends here
