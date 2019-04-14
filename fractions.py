#!/usr/bin/env python
# -*- coding: utf-8; -*-

import solve
import PyGnuplot as gp
from decimal import Decimal

if __name__ == '__main__':
    import sys

    n = 17
    guesses = []
    for i in xrange(1+n):
        pairs = [Decimal(a) / b for a, b in solve.neg_pairs(i) if b]
        pairs.sort()
        if not pairs:
            pairs = [Decimal('0.0')]
        guesses.append(pairs)
        i += 1

    xmin, xmax = min(map(min, guesses)), max(map(max, guesses))
    ymin, ymax = 1, 3

    ## when b is negative, xmin will be greater
    xmin, xmax = min(xmin, xmax), max(xmin, xmax)
    mxtics = 4

    prefix = '''
set terminal png size 1200, 300 enhanced #font 'Monaco,10';
set title "The rational numbers"
set key off;
set xtics %(xmin)s,1,%(xmax)s;
set ytics scale 0;
set format y "";
set mxtics %(mxtics)s
set xrange [%(xmin)s:%(xmax)s];
set yrange [%(ymin)s:%(ymax)s];
''' % { 'xmin': xmin, 'xmax': xmax, 'ymin': ymin, 'ymax': ymax,
        'mxtics': mxtics }
    i = 0

    datafile = lambda v: 'd%03d.dat' % v

    ## write the data files
    for i, row in enumerate(guesses):
        Xdata = row
        Ydata = [2 for _ in xrange(len(row))]
        gp.s([Xdata, Ydata], filename=datafile(i))

    ## write the vertical line data files
    #for i in xrange(int(mxtics)):
    #    val = xmin + i * mxtics
    #    Xdata, Ydata = [val, val], [ymin, ymax]

    ## write the plot files
    for i, guess in enumerate(guesses):
        plotfile = 'p%03d.plot' % i
        with (open(plotfile, 'w')) as out:
            print >> out, prefix
            print >> out, 'set output "o%03d.png";' % i
            s = ', '.join(['"%s" using 1:2 w p lw 2 lc %d' % (datafile(j), j)
                           for j in xrange(i + 1)])
            print >> out, 'plot ' + s

    ## hold the last scene
    for k in xrange(1, 10):
        plotfile = 'p%03d.plot' % (i + k)
        with (open(plotfile, 'w')) as out:
            print >> out, prefix
            print >> out, 'set output "o%03d.png";' % (i + k)
            s = ', '.join(['"%s" using 1:2 w p lw 2 lc %d' % (datafile(j), j)
                           for j in xrange(i + 1)])
            print >> out, 'plot ' + s

## fractions.py ends here
