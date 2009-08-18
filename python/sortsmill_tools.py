# -*- coding: utf-8 -*-

"""
Copyright (c) 2009 Barry Schwartz

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

def point_list_to_contour(point_list, is_closed, is_quadratic):
    c = fontforge.contour()
    c.is_quadratic = is_quadratic
    c.closed = is_closed
    for p in point_list:
        c += p
    return c

def make_pin(glyph):
    glyph.preserveLayerAsUndo()
    new_layer = fontforge.layer()
    layer = glyph.layers[glyph.activeLayer]
    for contour in layer:
        points = list(contour)
        for i in range(0, len(points)):
            if points[i].on_curve and points[i].selected:
                if i == 0 and contour.closed and not points[-1].on_curve:
                    points[-1].x = points[0].x
                    points[-1].y = points[0].y
                elif i == len(points) - 1 and contour.closed and not points[0].on_curve:
                    points[0].x = points[-1].x
                    points[0].y = points[-1].y
                if i != len(points) - 1 and not points[i + 1].on_curve:
                    points[i + 1].x = points[i].x
                    points[i + 1].y = points[i].y
                if i != 0 and not points[i - 1].on_curve:
                    points[i - 1].x = points[i].x
                    points[i - 1].y = points[i].y
        c = point_list_to_contour(points, contour.closed, contour.is_quadratic)
        new_layer += c
    glyph.layers[glyph.activeLayer] = new_layer

def points_are_selected(glyph):
    layer = glyph.layers[glyph.activeLayer]
    if layer.is_quadratic:
        return False         # Quadratics are not currently supported.
    for contour in layer:
        for point in contour:
            if point.selected:
                return True
    return False

fontforge.registerMenuItem((lambda _, glyph: make_pin(glyph)),
                           (lambda _, glyph: points_are_selected(glyph)),
                           None, "Glyph", "None",
                           "Turn points into pins")

#--------------------------------------------------------------------------

def reautohint(f):
    for g in f.glyphs():
        if not g.manualHints:
            g.autoHint()

fontforge.registerMenuItem((lambda _, font: reautohint(font)),
                           None, None, "Font", "None",
                           "Re-autohint autohintable glyphs")

#--------------------------------------------------------------------------
