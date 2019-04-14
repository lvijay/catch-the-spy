#!/usr/bin/env python
# -*- coding: utf-8; -*-

import solve
import PyGnuplot as gp

if __name__ == '__main__':
    import sys

    n = int(sys.argv[1]) if len(sys.argv) > 1 else 8
    all_vals = [solve.pairs(i) for i in xrange(n)]
    vals = []
    [vals.extend(sorted(v)) for v in all_vals]

    xvals = [v[0] for v in vals]
    yvals = [v[1] for v in vals]
    xmin, xmax = str(min(xvals)), str(max(xvals))
    ymin, ymax = str(min(yvals)), str(max(yvals))

    prefix = '''
set terminal png size 1000, 1000 enhanced #font 'Monaco,10';
set key off;
set xrange [%(xmin)s-0.5:%(xmax)s+0.5];
set yrange [%(ymin)s-0.5:%(ymax)s+0.5];
set xtics %(xmin)s,1,%(xmax)s;
set ytics %(ymin)s,1,%(ymax)s;
#set mxtics 1;
#set mytics 1;
#show mxtics;
#show mytics;
#set grid mxtics mytics;
#show grid;

''' % { 'xmin': xmin, 'xmax': xmax, 'ymin': ymin, 'ymax': ymax }
    X = []
    Y = []
    i = 0
    for x, y in vals:
        X.append(x)
        Y.append(y)
        plotfile = 'p%03d.plot' % i
        datfile = '%03d.dat' % i
        gp.s([X, Y], filename=datfile) # we won't use this after this
        with open(plotfile, 'w') as out:
            print >> out, prefix

            imgname = 'o%03d.png' % i
            print >> out, 'set output "%s";' % imgname
            print >> out, 'plot "%s" using 1:2 w lp lw 2' % datfile
            i += 1

## draw-solution.py ends here
