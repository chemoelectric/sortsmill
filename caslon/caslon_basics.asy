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

real my_fuzz = 1e-6;

typedef path postprocessor(path);

path identity(path p)
{
    return p;
}

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

path corner(pair corner,
            path path1,
            path path2,
            real arclength_before,
            real arclength_after,
            guide guide)
{
    path corner_path;
    if (arclength_before != infinity && arclength_after != infinity) {
        real t1 = time_at_point(path1, corner);
        real t2 = time_at_point(path2, corner);
        real arclength1 = arclength(subpath(path1, 0, t1)) - arclength_before;
        real arclength2 = arclength(subpath(path2, 0, t2)) + arclength_after;
        real t1a = arctime(path1, arclength1);
        real t2a = arctime(path2, arclength2);
        pair p1 = point(path1, t1a);
        pair p2 = point(path2, t2a);
        corner_path = p1{dir(path1,t1a)}..guide..{dir(path2,t2a)}p2;
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
    postprocessor postprocess;

    void operator init(pair point, real before, real after, guide guide,
                       postprocessor postprocess=identity) {
        this.point = point;
        this.before = before;
        this.after = after;
        this.guide = guide;
        this.postprocess = postprocess;        
    }

    void operator init(real before, real after, guide guide,
                       postprocessor postprocess=identity) {
        this.point = (infinity,infinity);
        this.before = before;
        this.after = after;
        this.guide = guide;
        this.postprocess = postprocess;
    }

    path path(Glyph glyph) {
        return postprocess(corner(glyph@point, before, after, guide));
    }

    path path(Pt p) {
        return postprocess(corner(p, before, after, guide));
    }

    path path(pair point, path path1, path path2) {
        return postprocess(corner(point, path1, path2, before, after, guide));
    }
};

//-------------------------------------------------------------------------

struct AngledTrim {
    pair point;
    real angle;
    real segment_before;
    real segment_after;
    guide guide;
    real before;
    real after;
    bool reversed;
    pair endpoint;
    postprocessor postprocess;

    void operator init(pair point,
                       real angle,
                       guide guide=nullpath--nullpath,
                       real segment_before=0.1,
                       real segment_after=10000,
                       bool reversed=false,
                       real before=0,
                       real after=0,
                       pair endpoint=(infinity,infinity),
                       postprocessor postprocess=identity) {
        this.point = point;
        this.angle = angle;
        this.segment_before = segment_before;
        this.segment_after = segment_after;
        this.guide = guide;
        this.reversed = reversed;
        this.before = before;
        this.after = after;
        this.endpoint = endpoint;
        this.postprocess = postprocess;
    }

    path path(Glyph glyph) {
        pair dir = dir(angle);
        path intersector = (-segment_before*dir)--(segment_after*dir);
        Pt xsect[] = glyph.intersections(shift(point)*intersector);
        path q;
        if (endpoint.x == infinity || endpoint.y == infinity) {
            if (xsect.length < 2)
                abort('intersections not found');
            Pt p1 = xsect[reversed ? 1 : 0] - before;
            Pt p2 = xsect[reversed ? 0 : 1] + after;
            q = p1.point{dir(p1)}..guide..{dir(p2)}p2.point;
        } else {
            if (xsect.length < 1)
                abort('intersection not found');
            pair p1 = (reversed ? xsect[0].point : endpoint) - before;
            pair p2 = (reversed ? endpoint : xsect[0].point) + after;
            q = p1..guide..p2;
        }
        return postprocess(q);
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
    postprocessor postprocess;

    void operator init(pair point1, pair point2, guide guide,
                       real before=0, real after=0,
                       int nodenum1=undefined, int nodenum2=undefined,
                       postprocessor postprocess=identity) {
        this.point1 = point1;
        this.point2 = point2;
        this.guide = guide;
        this.before = before;
        this.after = after;
        this.nodenum1 = nodenum1;
        this.nodenum2 = nodenum2;
        this.postprocess = postprocess;
    }

    path path(Glyph glyph) {
        Pt p1 = (glyph@point1)^nodenum1 - before;
        Pt p2 = (glyph@point2)^nodenum2 + after;
        return postprocess(p1.point{dir(p1)}..guide..{dir(p2)}p2.point);
    }
};

//-------------------------------------------------------------------------

struct BottomSerifTrim
{
    AngledTrim left_end;
    AngledTrim right_end;
    guide bottom_guide;
    postprocessor postprocess;

    void operator init(AngledTrim left_end, AngledTrim right_end, guide bottom_guide,
                       postprocessor postprocess=identity) {
        this.left_end = left_end;
        this.right_end = right_end;
        this.bottom_guide = bottom_guide;
        this.postprocess = postprocess;
    }

    path path(Glyph glyph) {
        path left_path = left_end.path(glyph);
        path right_path = right_end.path(glyph);
        return postprocess(right_path..bottom_guide..left_path);
    }
}

//-------------------------------------------------------------------------

struct FlagSerifTrim
{
    AngledTrim top_slope;
    Corner top_corner;
    Corner left_corner;
    postprocessor postprocess;

    void operator init(AngledTrim top_slope, Corner top_corner, Corner left_corner,
                       postprocessor postprocess=identity) {
        this.top_slope = top_slope;
        this.top_corner = top_corner;
        this.left_corner = left_corner;
        this.postprocess = postprocess;
    }

    path path(Glyph glyph) {
        path slope = top_slope.path(glyph);
        pair point1 = point(slope, 0);
        pair point2 = point(slope, length(slope));
        Pt p1 = glyph@point1;
        Pt p2 = glyph@point2;
        path left = left_corner.path(point1, p1.path(), slope);
        path top = top_corner.path(point2, slope, p2.path());
        pair point3 = point(left, length(left));
        pair point4 = point(top, 0);
        real t1 = time_at_point(slope, point3);
        real t2 = time_at_point(slope, point4);
        return postprocess(left & subpath(slope, t1, t2) & top);
    }
}

//-------------------------------------------------------------------------

Glyph shoulder_punch(real left_angle, real right_angle, pair shoulder_top,
                     Corner notch, pair approx_dimensions=(500,1000))
{
    Glyph punch = Glyph((0,0)--approx_dimensions.x*dir(left_angle)--approx_dimensions--
                        (approx_dimensions.x, shoulder_top.y)---shoulder_top..{-dir(right_angle)}cycle);
    punch.splice_in(notch.path(punch@(0,0)));
    return punch;
}

Glyph left_stem_punch(real top_angle, real bottom_angle, real side_angle, real side_height,
                      Corner top_corner=Corner(infinity, infinity, nullpath),
                      Corner bottom_corner=Corner(infinity, infinity, nullpath),
                      real approx_width=1000)
{
    pair side_top = side_height*(Tan(90 - side_angle),1);
    pair top_left = side_top - (approx_width/Cos(top_angle))*dir(top_angle);
    pair bottom_left = -(approx_width/Cos(bottom_angle))*dir(bottom_angle);
    Glyph punch = Glyph((0,0)--bottom_left--top_left--side_top--cycle);
    punch.splice_in(top_corner.path(punch@side_top));
    punch.splice_in(bottom_corner.path(punch@(0,0)));
    return punch;
}

Glyph right_stem_punch(real top_angle, real bottom_angle, real side_angle, real side_height,
                      Corner top_corner=Corner(infinity, infinity, nullpath),
                      Corner bottom_corner=Corner(infinity, infinity, nullpath),
                      real approx_width=1000)
{
    pair side_top = side_height*(Tan(90 - side_angle),1);
    pair top_right = side_top + (approx_width/Cos(top_angle))*dir(top_angle);
    pair bottom_right = (approx_width/Cos(bottom_angle))*dir(bottom_angle);
    Glyph punch = Glyph((0,0)--side_top--top_right--bottom_right--cycle);
    punch.splice_in(top_corner.path(punch@side_top));
    punch.splice_in(bottom_corner.path(punch@(0,0)));
    return punch;
}

Glyph right_r_punch(real bottom_angle, real side_angle, real side_height, guide top_guide,
                    Corner bottom_corner=Corner(infinity, infinity, nullpath),
                    real approx_width=1000)
{
    pair side_top = side_height*(Tan(90 - side_angle),1);
    pair bottom_right = (approx_width/Cos(bottom_angle))*dir(bottom_angle);
    path top_path = side_top{dir(side_angle)}..top_guide;
    pair top_path_end = point(top_path, length(top_path));
    pair top_right = (approx_width, top_path_end.y);
    Glyph punch = Glyph((0,0)--side_top & top_path & top_path_end---top_right--bottom_right--(0,0) & cycle);
    punch.splice_in(bottom_corner.path(punch@(0,0)));
    return punch;
}

//-------------------------------------------------------------------------
