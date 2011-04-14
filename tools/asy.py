# -*- coding: utf-8 -*-

"""
A module for use with Asymptote.
See http://asymptote.sourceforge.net/
(But Asymptote support is likely to be
dropped or reduced.)

For now also a module for use with OCaml, a programming language
favored at the Sorts Mill. See http://caml.inria.fr/
And some of it might be used with any programming or scripting
language your prefer.


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

import cmath
import fontforge
import imp
import subprocess

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

direction_tolerance = 1e-4
linear_tolerance = 1e-4
decimal_places = 4

def move_to_first_quadrant(c):
    return complex(abs(c.real), abs(c.imag))

def caml_complex(c):
    x = c.real
    y = c.imag

    if x < 0:
        s_x = ("x'({0:." + str(decimal_places) + "f})").format(x)
    elif x == 0:
        s_x = None
    else:
        s_x = ("x' {0:." + str(decimal_places) + "f}").format(x)

    if y < 0:
        s_y = ("y'({0:." + str(decimal_places) + "f})").format(y)
    elif y == 0:
        s_y = None
    else:
        s_y = ("y' {0:." + str(decimal_places) + "f}").format(y)

    if s_x is None and s_y is None:
        s = "zero"
    elif s_x is None:
        s = s_y
    elif s_y is None:
        s = s_x
    else:
        s = s_x + " + " + s_y

    return s

def caml_path(contour):

    "Returns OCaml code for a contour, in the notations of the Sorts Mill's Fontdesign module."

    plist = list(contour)
    if 3 <= len(plist) and not plist[-2].on_curve and not plist[-1].on_curve:
        plist = plist[-1:] + plist[:-1]

    plist2 = []
    i = 0
    while i < len(plist):
        if plist[i].on_curve:
            if i + 1 < len(plist) and not plist[i + 1].on_curve:
                plist2 += [plist[i], plist[i], plist[i + 1]]
                i += 2
            else:
                plist2 += [plist[i], plist[i], plist[i]]
                i += 1
        elif i + 2 < len(plist) and not plist[i + 2].on_curve:
            plist2 += [plist[i], plist[i + 1], plist[i + 2]]
            i += 3
        else:
            plist2 += [plist[i], plist[i + 1], plist[i + 1]]
            i += 2

    s = "of_node_list [\n"
    i = 0
    while i < len(plist2):
        inhandle_x = plist2[i].x - plist2[i + 1].x
        inhandle_y = plist2[i].y - plist2[i + 1].y
        oncurve_x = plist2[i + 1].x
        oncurve_y = plist2[i + 1].y
        outhandle_x = plist2[i + 2].x - plist2[i + 1].x
        outhandle_y = plist2[i + 2].y - plist2[i + 1].y

        c_inhandle = complex(inhandle_x, inhandle_y)
        c_oncurve = complex(oncurve_x, oncurve_y)
        c_outhandle = complex(outhandle_x, outhandle_y)

        a1 = abs(c_inhandle)
        a2 = abs(c_outhandle)
        d1 = c_inhandle / a1
        d2 = c_outhandle / a2
        df1 = move_to_first_quadrant(d1)
        df2 = move_to_first_quadrant(d2)

        is_flat = (linear_tolerance <= a1 and
                   linear_tolerance <= a2 and
                   abs(d1 + d2) < direction_tolerance)
        is_horizontal = (is_flat and
                         abs(df1 - 1) < direction_tolerance and
                         abs(df2 - 1) < direction_tolerance)
        is_vertical = (is_flat and
                       not is_horizontal and
                       abs(df1 - complex(0,1)) < direction_tolerance and
                       abs(df2 - complex(0,1)) < direction_tolerance)
        is_right = is_horizontal and abs(d1 + 1) < direction_tolerance
        is_left = is_horizontal and not is_right
        is_up = is_vertical and abs(d1 + complex(0,1)) < direction_tolerance
        is_down = is_vertical and not is_up

        if is_right:
            s += ("  make_right (" +
                  caml_complex(c_oncurve) + ") (" +
                  caml_complex(a1) + ") (" +
                  caml_complex(a2) + ");\n")
        elif is_left:
            s += ("  make_left (" +
                  caml_complex(c_oncurve) + ") (" +
                  caml_complex(a1) + ") (" +
                  caml_complex(a2) + ");\n")
        elif is_up:
            s += ("  make_up (" +
                  caml_complex(c_oncurve) + ") (" +
                  caml_complex(a1) + ") (" +
                  caml_complex(a2) + ");\n")
        elif is_down:
            s += ("  make_down (" +
                  caml_complex(c_oncurve) + ") (" +
                  caml_complex(a1) + ") (" +
                  caml_complex(a2) + ");\n")
        elif is_flat:
            s += ("  make_flat (" +
                  caml_complex(c_oncurve) + ") (" +
                  caml_complex(d2) + ") (" +
                  caml_complex(a1) + ") (" +
                  caml_complex(a2) + ");\n")
        else:
            s += ("  make_node (" +
                  caml_complex(c_inhandle) + ") (" +
                  caml_complex(c_oncurve) + ") (" +
                  caml_complex(c_outhandle) + ");\n")
        i += 3
    s += "] <@@ " + ("true" if contour.closed else "false")

    return s

#--------------------------------------------------------------------------

def load_glyph_data_from_given_asy(glyph, asy_file):
    module_name = 'asy_glyph_data'
    asy_command = 'write_glyph_data(\'' + glyph.glyphname + '\')'
    pipe = subprocess.Popen(['asy', '-u', asy_command, asy_file], stdout=subprocess.PIPE).stdout
    module = imp.load_module(module_name, pipe, "<stdin>", ('', '', imp.PY_SOURCE))

def load_asy_glyph_data(glyph):
    asy_file = glyph.font.fontname + '.asy'
    load_glyph_data_from_given_asy(glyph, asy_file)

fontforge.registerMenuItem((lambda _, glyph: load_asy_glyph_data(glyph)),
                           None, None, 'Glyph', 'None',
                           'Load Asymptote glyph data')

#--------------------------------------------------------------------------

def load_private_data_from_given_asy(font, asy_file):
    module_name = 'asy_private_data'
    asy_command = 'write_private_data()'
    pipe = subprocess.Popen(['asy', '-u', asy_command, asy_file], stdout=subprocess.PIPE).stdout
    module = imp.load_module(module_name, pipe, "<stdin>", ('', '', imp.PY_SOURCE))

def load_asy_private_data(font):
    asy_file = font.fontname + '.asy'
    load_private_data_from_given_asy(font, asy_file)

fontforge.registerMenuItem((lambda _, font: load_asy_private_data(font)),
                           None, None, 'Font', 'None',
                           'Load Private table data from Asymptote')

#--------------------------------------------------------------------------

def print_asymptote_paths(glyph):
    for c in glyph.layers[glyph.activeLayer]:
        print(asymptote_path(c))

fontforge.registerMenuItem((lambda _, glyph: print_asymptote_paths(glyph)),
                           None, None, 'Glyph', 'None',
                           'Output Asymptote paths')

#--------------------------------------------------------------------------

def print_caml_contours(glyph):
    for c in glyph.layers[glyph.activeLayer]:
        print(caml_path(c))

fontforge.registerMenuItem((lambda _, glyph: print_caml_contours(glyph)),
                           None, None, 'Glyph', 'None',
                           'Output OCaml contours')

#--------------------------------------------------------------------------
