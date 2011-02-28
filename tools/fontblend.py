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
