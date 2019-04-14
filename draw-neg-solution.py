#!/usr/bin/env python
# -*- coding: utf-8; -*-

import solve
import PyGnuplot as gp

if __name__ == '__main__':
    import sys

    n = int(sys.argv[1]) if len(sys.argv) > 1 else 8
    all_vals = [solve.neg_pairs(i) for i in xrange(n)]

    xvals = [v[0] for row in all_vals for v in row]
    yvals = [v[1] for row in all_vals for v in row]
    xmin, xmax = str(min(xvals)), str(max(xvals))
    ymin, ymax = str(min(yvals)), str(max(yvals))

    import random
    clrs = 'red blue green yellow black'.split()
    random.shuffle(clrs)
    
    prefix = '''
set terminal png size 1000, 1000 enhanced #font 'Monaco,10';
set title "Plotting all integer pairs of (a, b)"
set key off;
set xrange [%(xmin)s-0.5:%(xmax)s+0.5];
set yrange [%(ymin)s-0.5:%(ymax)s+0.5];
set xtics %(xmin)s,1,%(xmax)s;
set ytics %(ymin)s,1,%(ymax)s;
set grid xtics ytics;
show grid;

''' % { 'xmin': xmin, 'xmax': xmax, 'ymin': ymin, 'ymax': ymax }
    datfile = lambda i: '%03d.dat' % i
    r = 1 + abs(int(xmax)) + abs(int(xmin)) # range
    for i, row in enumerate(all_vals):
        row.sort(cmp=lambda a, b: (a[0]*r + a[1]) - (b[0]*r + b[1]))
        X = [v[0] for v in row]
        Y = [v[1] for v in row]
        gp.s([X, Y], filename=datfile(i)) # we won't use this after this
    plotter = lambda v: ', '.join(['"%(f)s" using 1:2 w p pointtype 7 lw 3 lc rgb "%(clr)s"'
                                           % {'f': datfile(j), 'clr': clrs[j%len(clrs)] }
                                   for j in xrange(v+1)])
    for i in xrange(len(all_vals)):
        plotfile = 'p%03d.plot' % i
        with open(plotfile, 'w') as out:
            print >> out, prefix
            imgname = 'o%03d.png' % i
            print >> out, 'set output "%s";' % imgname
            plots = plotter(i)
            print >> out, 'plot ', plots
    for repeat in xrange(5):
        plotfile = 'p%03d.plot' % (repeat + i)
        with open(plotfile, 'w') as out:
            print >> out, prefix
            imgname = 'o%03d.png' % (repeat + i)
            print >> out, 'set output "%s";' % imgname
            plots = plotter(i)
            print >> out, 'plot ', plots
        
## draw-neg-solution.py ends here
