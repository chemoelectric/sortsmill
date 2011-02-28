"""
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

#--------------------------------------------------------------------------

import fontforge

#--------------------------------------------------------------------------

def anchor_points_correspond(point1, point2):
    return (point1[0] == point2[0] and
            point1[1] == point2[1] and
            (point1[1] != 'ligature' or point1[5] == point2[5]));


def find_corresponding_anchor_point(points, point1):
    for p in points:
        if anchor_points_correspond(p, point1):
            return p
    return None

def interpolate_anchor_points(points1, points2, weight):
    v = 1 - weight
    interpolated_points = []
    for p in points1:
        q = find_corresponding_anchor_point(points2, p)
        if q is not None:
            x = (v * p[2]) + (weight * q[2])
            y = (v * p[3]) + (weight * q[3])
            selected = False
            if p[1] == 'ligature':
                new_point = (p[0], p[1], x, y, selected, p[5])
            else:
                new_point = (p[0], p[1], x, y, selected)
            interpolated_points.append(new_point)
    return interpolated_points

#--------------------------------------------------------------------------

def interpolate_glyphs(blended_font, font1, font2, weight):
    for g in font1:
        if g in font2:
            blended_font.createInterpolatedGlyph(font1[g], font2[g], weight)

def interpolate_glyphs_anchor_points(blended_font, font1, font2, weight):
    points_map = {}
    anchor_classes = set()

    for g in font1:
        if g in font2:
            points = interpolate_anchor_points(font1[g].anchorPointsWithSel,
                                               font2[g].anchorPointsWithSel, weight)
            points_map[g] = points
            for p in points:
                anchor_classes.add(p[0])

    # FIX/TODO: support other lookup types, flags, etc.
    blended_font.addLookup('blended_anchors', 'gpos_mark2base', (), ())
    blended_font.addLookupSubtable('blended_anchors', 'blended_anchors-1')
    for ac in anchor_classes:
        blended_font.addAnchorClass('blended_anchors-1', ac)

    for g in points_map:
        blended_font[g].anchorPointsWithSel = points_map[g]

#--------------------------------------------------------------------------
