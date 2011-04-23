# -*- coding: utf-8 -*-

"""
A module for use with external development tools/methods, such as the
OCaml programming favored at the Sorts Mill.


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

    s = ""
    i = 0
    while i < len(plist2):
        join = "<@> " if i != 0 else ""

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

        if a1 < linear_tolerance and a2 < linear_tolerance:
            s += "  " + join + "make_pin (" + caml_complex(c_oncurve) + ");\n"
        elif a1 < linear_tolerance or a2 < linear_tolerance:
            s += ("  " + join + "make_node (" +
                  caml_complex(c_inhandle) + ") (" +
                  caml_complex(c_oncurve) + ") (" +
                  caml_complex(c_outhandle) + ");\n")
        else:
            d1 = c_inhandle / a1
            d2 = c_outhandle / a2
            df1 = move_to_first_quadrant(d1)
            df2 = move_to_first_quadrant(d2)

            is_flat = abs(d1 + d2) < direction_tolerance
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
                s += ("  " + join + "make_right (" +
                      caml_complex(c_oncurve) + ") (" +
                      caml_complex(a1) + ") (" +
                      caml_complex(a2) + ")\n")
            elif is_left:
                s += ("  " + join + "make_left (" +
                      caml_complex(c_oncurve) + ") (" +
                      caml_complex(a1) + ") (" +
                      caml_complex(a2) + ")\n")
            elif is_up:
                s += ("  " + join + "make_up (" +
                      caml_complex(c_oncurve) + ") (" +
                      caml_complex(a1) + ") (" +
                      caml_complex(a2) + ")\n")
            elif is_down:
                s += ("  " + join + "make_down (" +
                      caml_complex(c_oncurve) + ") (" +
                      caml_complex(a1) + ") (" +
                      caml_complex(a2) + ")\n")
            elif is_flat:
                s += ("  " + join + "make_flat (" +
                      caml_complex(c_oncurve) + ") (" +
                      caml_complex(d2) + ") (" +
                      caml_complex(a1) + ") (" +
                      caml_complex(a2) + ")\n")
            else:
                s += ("  " + join + "make_node (" +
                      caml_complex(c_inhandle) + ") (" +
                      caml_complex(c_oncurve) + ") (" +
                      caml_complex(c_outhandle) + ")\n")
        i += 3
    s += "  <@@ " + ("true" if contour.closed else "false")

    return s

#--------------------------------------------------------------------------

def load_glyph_data_from_program(glyph, program):
    module_name = 'program_generated_glyph_data'
    pipe = subprocess.Popen([program, glyph.glyphname], stdout=subprocess.PIPE).stdout
    module = imp.load_module(module_name, pipe, "<stdin>", ('', '', imp.PY_SOURCE))

def load_program_glyph_data(glyph):
    program = './' + glyph.font.fontname + '_glyph_update'
    load_glyph_data_from_program(glyph, program)

fontforge.registerMenuItem((lambda _, glyph: load_program_glyph_data(glyph)),
                           None, None, 'Glyph', 'None',
                           'Load glyph data from program')

#--------------------------------------------------------------------------

def print_caml_contours(glyph):
    for c in glyph.layers[glyph.activeLayer]:
        print(caml_path(c))

fontforge.registerMenuItem((lambda _, glyph: print_caml_contours(glyph)),
                           None, None, 'Glyph', 'None',
                           'Output OCaml contours')

#--------------------------------------------------------------------------
