#!/usr/bin/python

# Copyright (c) 2009 Barry Schwartz
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

import fontforge
import re

#--------------------------------------------------------------------------

def blend_fonts(blended_font, fonts_to_blend, blender):

    height1 = f1.ascent + f1.descent
    height2 = f2.ascent + f2.descent
    (new_height, _) = blender([(f.ascent + f.descent, 0) for f in fonts_to_blend])
    (blended_font.ascent, _) = blender([(f.ascent, 0) for f in fonts_to_blend])
    blended_font.descent = new_height - blended_font.ascent

    (blended_font.italicangle, _) = blender([(f.italicangle, 0) for f in fonts_to_blend])

    for glyph_name in fonts_to_blend[0]:
        if glyphs_are_blendable(fonts_to_blend, glyph_name):
            g = blended_font.createChar(fontforge.unicodeFromName(glyph_name), glyph_name)
            blend_glyphs(g, [f[glyph_name] for f in fonts_to_blend], blender)

def blend_glyphs(blended_glyph, glyphs, blender):

    (blended_glyph.width, _) = blender([(g.width, 0) for g in glyphs])

    blended_layer = fontforge.layer()
    layer0 = glyphs[0].foreground
    for i in range(0, len(layer0)):
        blended_contour = fontforge.contour()
        blended_contour.closed = layer0[i].closed
        for j in range(0, len(layer0[i])):
            (x, y) = blender([(g.foreground[i][j].x, g.foreground[i][j].y) for g in glyphs])
            blended_contour += fontforge.point(x, y, layer0[i][j].on_curve)
        blended_contour.simplify(0) # Remove redundant off-curve control points.
        blended_layer += blended_contour
    blended_glyph.foreground = blended_layer

def glyphs_are_blendable(fonts, glyph_name):
    blendable = False
    if glyph_exists_in_all(fonts, glyph_name):
        blendable = glyphs_have_compatible_contours([f[glyph_name] for f in fonts])
    return blendable

def glyphs_have_compatible_contours(glyphs):
    if len(glyphs) == 2:
        g1 = glyphs[0]
        g2 = glyphs[1]
        layer1 = g1.foreground
        layer2 = g2.foreground
        if len(layer1) != len(layer2):
            return False
        for i in range(0, len(layer1)):
            c1 = insert_redundant_control_points(layer1[i])
            c2 = insert_redundant_control_points(layer2[i])
            if c1.closed != c2.closed or len(c1) != len(c2):
                return False
            for j in range(0, len(c1)):
                if c1[j].on_curve != c2[j].on_curve:
                    return False
    else:
        for g in glyphs[1:]:
            if not glyphs_have_compatible_contours(glyphs[0], g):
                return False
    return True

def insert_redundant_control_points(contour):

    new_contour = fontforge.contour()
    new_contour.closed = contour.closed

    for i in range(0, len(contour) - 1):
        new_contour += fontforge.point(contour[i].x, contour[i].y, contour[i].on_curve)
        if contour[i].on_curve and contour[i + 1].on_curve:
            new_contour += fontforge.point(contour[i].x, contour[i].y, False)
            new_contour += fontforge.point(contour[i + 1].x, contour[i + 1].y, False)

    if contour.closed and contour[-1].on_curve and contour[0].on_curve:
            new_contour += fontforge.point(contour[-1].x, contour[-1].y, False)
            new_contour += fontforge.point(contour[0].x, contour[0].y, False)

    return new_contour

def glyph_exists_in_all(fonts, glyph_name):
    for f in fonts:
        if glyph_name not in f:
            return False
    return True

#--------------------------------------------------------------------------

if __name__ == "__main__":

    import sys

    fontforge.loadPrefs()

    result_font = sys.argv[1]
    font1 = sys.argv[2]
    font2 = sys.argv[3]
    design_size = sys.argv[4]

    f1 = fontforge.open(font1)
    f2 = fontforge.open(font2)

    # There may end up being special cases where the following
    # canonicalization doesn't do the correct thing. Let's deal with
    # that problem if it crops up.
    for f in [f1, f2]:
        f.selection.all()
        f.canonicalContours()
        f.canonicalStart()

    match1 = re.match('([^0-9]*)([0-9]+)(.*)', f1.fontname)
    match2 = re.match('([^0-9]*)([0-9]+)(.*)', f2.fontname)

    design_size1 = match1.group(2)
    design_size2 = match2.group(2)
    b = (float(design_size) - float(design_size1)) / (float(design_size2) - float(design_size1))
    a = 1.0 - b

    blender = lambda p: (a*p[0][0] + b*p[1][0], a*p[0][1] + b*p[1][1])

    f = fontforge.font()
    f.encoding = 'Unicode'
    f.fontname = match1.group(1) + design_size + match1.group(3)
    f.familyname = match1.group(1) + design_size
    m = re.match('(.*)' + design_size1 + '(.*)', f1.fullname)
    f.fullname = m.group(1) + design_size + m.group(2)
    f.copyright = f1.copyright
    f.weight = f1.weight
    f.version = f1.version

    blend_fonts(f, [f1, f2], blender)

    f.save(result_font)

#--------------------------------------------------------------------------
