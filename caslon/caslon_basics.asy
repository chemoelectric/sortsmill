// Copyright (c) 2011 Barry Schwartz
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

import sortsmill_font;
import sortsmill_glyph;

typedef Glyph.OutlinePoint Pt;

//-------------------------------------------------------------------------

path corner(Pt corner,
            real arclength_before,
            real arclength_after,
            guide guide)
{
    path corner_path;
    if (arclength_before != infinity && arclength_after != infinity) {
        Pt p1 = corner - arclength_before;
        Pt p2 = corner + arclength_after;
        corner_path = p1.point{dir(p1)}..guide..{dir(p2)}p2.point;
    }
    return corner_path;
}

real default_corner_side = 10;
guide default_corner_guide = nullpath..nullpath;

path default_corner(Pt point)
{
    return corner(point, default_corner_side, default_corner_side, default_corner_guide);
}

struct Corner {
    pair point;
    real before;
    real after;
    guide guide;

    void operator init(pair point, real before, real after, guide guide) {
        this.point = point;
        this.before = before;
        this.after = after;
        this.guide = guide;
    }

    path path(Glyph glyph) {
        return corner(glyph@point, before, after, guide);
    }
};

//-------------------------------------------------------------------------

struct AngledTrim {
    pair point;
    pair dir;
    real segment_before;
    real segment_after;
    guide guide;
    real before;
    real after;
    bool reversed;

    void operator init(pair point,
                       pair dir,
                       real segment_before,
                       real segment_after,
                       guide guide,
                       bool reversed=false,
                       real before=0,
                       real after=0) {
        this.point = point;
        this.dir = dir;
        this.segment_before = segment_before;
        this.segment_after = segment_after;
        this.guide = guide;
        this.reversed = reversed;
        this.before = before;
        this.after = after;
    }

    path path(Glyph glyph) {
        path intersector = ((0,0) - segment_before*dir)--((0,0) + segment_after*dir);
        Pt xsect[] = glyph.intersections(shift(point)*intersector);
        if (xsect.length < 2)
            abort('intersections not found');
        Pt p1 = xsect[reversed ? 1 : 0] - before;
        Pt p2 = xsect[reversed ? 0 : 1] + after;
        return p1.point{dir(p1)}..guide..{dir(p2)}p2.point;
    }
};

//-------------------------------------------------------------------------

struct TwoPointTrim {
    pair point1;
    pair point2;
    guide guide;
    real before;
    real after;
    int nodenum1;
    int nodenum2;

    void operator init(pair point1, pair point2, guide guide,
                       real before=0, real after=0,
                       int nodenum1=undefined, int nodenum2=undefined) {
        this.point1 = point1;
        this.point2 = point2;
        this.guide = guide;
        this.before = before;
        this.after = after;
        this.nodenum1 = nodenum1;
        this.nodenum2 = nodenum2;
    }

    path path(Glyph glyph) {
        Pt p1 = (glyph@point1)^nodenum1 - before;
        Pt p2 = (glyph@point2)^nodenum2 + after;
        return p1.point{dir(p1)}..guide..{dir(p2)}p2.point;
    }
};

//-------------------------------------------------------------------------
