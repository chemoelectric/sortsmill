# -*- coding: utf-8 -*-

"""
A module for use with Asymptote.
See http://asymptote.sourceforge.net/



Copyright (c) 2011 Barry Schwartz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

import fontforge

#--------------------------------------------------------------------------

def asymptote_path(contour):
    s = ''
    i = 0
    while i < len(contour) - 1:
        s += '(' + str(contour[i].x) + ',' + str(contour[i].y) + ')'
        if contour[i + 1].on_curve:
            s += '--\n'
            i += 1
        else:
            s += ('..controls (' +
                  str(contour[i + 1].x) + ',' + str(contour[i + 1].y) + ') and (' +
                  str(contour[i + 2].x) + ',' + str(contour[i + 2].y) + ')..\n')
            i += 3
    if i < len(contour):
        s += '(' + str(contour[i].x) + ',' + str(contour[i].y) + ')'
        if contour.closed:
            s += '--\n'
    if contour.closed:
        s += 'cycle'
    return s

#--------------------------------------------------------------------------

def print_asymptote_paths(glyph):
    for c in glyph.layers[glyph.activeLayer]:
        print(asymptote_path(c))

fontforge.registerMenuItem((lambda _, glyph: print_asymptote_paths(glyph)),
                           None, None, 'Glyph', 'None',
                           'Output Asymptote paths')

#--------------------------------------------------------------------------
