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

import geometry;
from sortsmill_orientation access is_oriented, is_clockwise, make_clockwise, normalize_orientations;
from sortsmill_overlap access apply_punch;

real point_fuzz = 0.00001;

real time_at_point(path p, pair point, real fuzz = point_fuzz)
{
    // This call can't be relied on without some fuzz.
    return intersect(p, point, fuzz)[0];
}

//-------------------------------------------------------------------------
//
// FontForge-related stuff.

bool round_points = false;
bool simplify_slightly = false;

string fontforge_contour_code(path p, string contour_name)
// Converts a path to Python code for creating an instance of
// |fontforge.contour| describing the same bezier spline.
{
    string s = '';
    s += contour_name + ' = fontforge.contour()\n';
    for (int i = 0; i < length(p); i += 1) {
        s += contour_name + ' += fontforge.point(';
        s += format('%f,', point(p,i).x);
        s += format('%f)\n', point(p,i).y);

        s += contour_name + ' += fontforge.point(';
        s += format('%f,', postcontrol(p,i).x);
        s += format('%f, False)\n', postcontrol(p,i).y);

        s += contour_name + ' += fontforge.point(';
        s += format('%f,', precontrol(p,(i + 1) % length(p)).x);
        s += format('%f, False)\n', precontrol(p,(i + 1) % length(p)).y);
    }
    s += contour_name + '.closed = ' + (cyclic(p) ? 'True' : 'False') + '\n';
    return s;
}

string pythonize_hints(real[][] hints, real offset)
{
    string s = '(';
    for (real[] h : sort(hints)) {
        s = s + '(';
        s = s + format('%f', h[0] - offset);
        s = s + ',';
        s = s + format('%f', h[1]);
        s = s + '),';
    }
    s = s + ')\n';
    return s;
}

//-------------------------------------------------------------------------
//
// Curve improvement.

real default_smoothing_max_distance = 25;
real default_smoothing_max_angle = 1;

path smooth_close_points(path p,
                         int start_time,
                         int end_time,
                         real max_distance = default_smoothing_max_distance,
                         real max_angle = default_smoothing_max_angle)
{
    guide g;
    for (int i = start_time; i < end_time; i += 1) {
        pair dir1 = dir(p, i, 1);
        pair dir2 = dir(p, i + 1, -1);
        if (0 < abs(dir1) && 0 < abs(dir2) && abs(degrees(dir2) - degrees(dir1)) <= max_angle) {
            guide g1 = point(p,i)---point(p,i + 1);
            if (arclength(g1) <= max_distance)
                g = g & g1;
            else
                g = g & subpath(p, i, i + 1);
        } else {
            g = g & subpath(p, i, i + 1);
        }
    }
    if (cyclic(p))
        g = g & cycle;
    return g;
}

path smooth_close_points(path outline,
                         real max_distance = default_smoothing_max_distance,
                         real max_angle = default_smoothing_max_angle)
{
    return smooth_close_points(outline, 0, size(outline), max_distance, max_angle);
}

path add_extrema(path p)
{
    path q;
    for (int i = 0; i < length(p); ++i) {
        path p_i = subpath(p, i, i + 1);
        real[] times;
        times.push(0);
        times.append(mintimes(p_i));
        times.append(maxtimes(p_i));
        times.push(1);
        times = sort(times);
        for (int j = 0; j < 5; ++j)
            if (times[j] < times[j + 1])
                q = q & subpath(p_i, times[j], times[j + 1]);
    }
    if (cyclic(p))
        q = q & cycle;
    return q;
}

guide round(path p)
// Round the path's coordinates to integers.
{
    guide q;
    for (int i = 0; i < length(p); i += 1) {
        pair point = point(p, i);
        pair postcontrol = postcontrol(p, i);
        pair precontrol = precontrol(p, i + 1);
        pair i_point = (round(point.x), round(point.y));
        pair i_postcontrol = (round(postcontrol.x), round(postcontrol.y));
        pair i_precontrol = (round(precontrol.x), round(precontrol.y));
        q = q .. (i_point..controls i_postcontrol and i_precontrol..nullpath);
    }
    if (cyclic(p))
        q = q .. cycle;
    return q;
}

//-------------------------------------------------------------------------

struct Glyph {
    path[] outlines;
    string name;
    int unicode = undefined;
    real baseline = 0;
    real lsb = 25;
    real rsb = 25;
    real[][] horiz_hints;
    real[][] vert_hints;

    static struct OutlinePoint {
        Glyph glyph;
        int outline_no;
        real time;
        pair point;

        void refresh() {
            if (glyph.outlines.length == 1) {
                outline_no = 0;
                time = time_at_point(glyph.outlines[0], this.point);
                this.point = point(glyph.outlines[outline_no], time);
            } else {
                outline_no = 0;
                time = time_at_point(glyph.outlines[0], point);
                pair pt = point(glyph.outlines[0], time);
                for (int i = 1; i < glyph.outlines.length; ++i) {
                    real new_time = time_at_point(glyph.outlines[i], point);
                    pair new_point = point(glyph.outlines[i], new_time);
                    if (abs(this.point - new_point) < abs(this.point - pt)) {
                        outline_no = i;
                        time = new_time;
                        pt = new_point;
                    }
                }
                this.point = pt;
            }
        }

        void operator init(Glyph glyph, pair point) {
            assert(glyph.outlines.length != 0);
            this.glyph = glyph;
            this.point = point;
            this.refresh();
        }

        pair dir(bool normalize=true) {
            return dir(glyph.outlines[outline_no], time, normalize);
        }

        OutlinePoint nearby_node(int n = 0) {
            OutlinePoint p = new OutlinePoint;
            p.glyph = glyph;
            p.outline_no = outline_no;
            p.time = round(time) + n;
            p.point = point(glyph.outlines[outline_no], p.time);
            return p;
        }

        void displace_along_outline(real displacement) {
            path outline = glyph.outlines[outline_no];
            real here_length = arclength(subpath(outline, 0, time));
            real there_length = here_length + displacement;
            time = arctime(outline, there_length);
            point = point(outline, time);
        }
    };

    void operator init(path p) {
        outlines = new path[] { make_clockwise(p) };
    }

    void operator init(guide p) {
        operator init((path) p);
    }

    void operator init(path[] outlines) {
        this.outlines = normalize_orientations(outlines);
    }

    void operator init(pair corner1, pair corner2) {
        // Make a rectangular blank.
        path rect = corner1---(corner1.x,corner2.y)---corner2---(corner2.x,corner1.y)---cycle;
        operator init(rect);
    }

    void draw() {
        for (path p : outlines)
            draw(p);
    }
    
    void fill() {
        for (path p : outlines)
            if (is_clockwise(p))
                fill(p);
            else
                unfill(p);
    }

    void apply_transform(transform t) {
        for (int i = 0; i < outlines.length; ++i)
            outlines[i] = t * outlines[i];
    }

    void apply_punch(Glyph punch) {
        outlines = apply_punch(punch.outlines, outlines);
    }

    void chop(pair point, real angle) {
        // Cuts off a chunk (likely a big one), along an angled
        // straight line.

        real bignum = 1e6;
        Glyph g = Glyph((-bignum,0)---(-bignum,bignum)---(bignum,bignum)---(bignum,0)---cycle);
        g.apply_transform(shift(point)*rotate(angle));
        apply_punch(g);
    }

    void chop(pair point1, pair point2) {
        // Cuts off a chunk (likely a big one), along a line that
        // passes through two points.

        chop(point1, degrees(point2 - point1));
    }

    void splice_in(path new_part) {
        if (new_part != nullpath) {
            OutlinePoint a = OutlinePoint(this, point(new_part, 0));
            OutlinePoint b = OutlinePoint(this, point(new_part, length(new_part)));
            if (a.outline_no != b.outline_no)
                abort('splice points are on different outlines');
            path outline = a.glyph.outlines[a.outline_no];
            path p = (a.time < b.time) ?
                subpath(outline, b.time, a.time + length(outline)) :
                subpath(outline, b.time, a.time);
            a.glyph.outlines[a.outline_no] = new_part & p & cycle;
        }
    }

    OutlinePoint[] intersections(path p) {
        real[][] xsect;
        bool my_less(int a, int b) {
            return xsect[a][0] < xsect[b][0];
        }
        OutlinePoint[] points;
        for (int i = 0; i < outlines.length; ++i) {
            path outline = outlines[i];
            real[][] t = intersections(p, outline);
            xsect.append(t);
            for (int j = 0; j < t.length; ++j) {
                OutlinePoint p = new OutlinePoint;
                p.glyph = this;
                p.outline_no = i;
                p.time = t[j][1];
                p.point = point(outline, p.time);
                points.push(p);
            }
        }
        int[] keys = xsect.keys;
        keys = sort(keys, my_less);
        OutlinePoint[] reordered_points;
        for (int k : keys)
            reordered_points.push(points[k]);
        return reordered_points;
    }

    OutlinePoint[] points_at_x(real x) {
        bool y_less(OutlinePoint a, OutlinePoint b) {
            return a.point.y < b.point.y;
        }
        OutlinePoint[] points;
        for (int i = 0; i < outlines.length; ++i) {
            path outline = outlines[i];
            real[] t = times(outline, x);
            for (int j = 0; j < t.length; ++j) {
                OutlinePoint p = new OutlinePoint;
                p.glyph = this;
                p.outline_no = i;
                p.time = t[j];
                p.point = point(outline, t[j]);
                points.push(p);
            }
        }
        return sort(points, y_less);
    }

    OutlinePoint[] points_at_y(real y) {
        bool x_less(OutlinePoint a, OutlinePoint b) {
            return a.point.x < b.point.x;
        }
        OutlinePoint[] points;
        for (int i = 0; i < outlines.length; ++i) {
            path outline = outlines[i];
            real[] t = times(outline, (0,y));
            for (int j = 0; j < t.length; ++j) {
                OutlinePoint p = new OutlinePoint;
                p.glyph = this;
                p.outline_no = i;
                p.time = t[j];
                p.point = point(outline, t[j]);
                points.push(p);
            }
        }
        return sort(points, x_less);
    }

    void smooth_close_points(real max_distance = default_smoothing_max_distance,
                             real max_angle = default_smoothing_max_angle) {
        for (int i = 0; i < outlines.length; ++i)
            outlines[i] = smooth_close_points(outlines[i], max_distance, max_angle);
    }

    void add_extrema() {
        for (int i = 0; i < outlines.length; ++i)
            outlines[i] = add_extrema(outlines[i]);
    }

    void round() {
        for (int i = 0; i < outlines.length; ++i)
            outlines[i] = round(outlines[i]);
        baseline = round(baseline);
        lsb = round(lsb);
        rsb = round(rsb);
        for (int i = 0; i < horiz_hints.length; ++i) {
            horiz_hints[i][0] = round(horiz_hints[i][0]);
            horiz_hints[i][1] = round(horiz_hints[i][1]);
        }
        for (int i = 0; i < vert_hints.length; ++i) {
            vert_hints[i][0] = round(vert_hints[i][0]);
            vert_hints[i][1] = round(vert_hints[i][1]);
        }
    }

    void add_horiz_hint(real x, real width) {
        real[] hint = { x, width };
        horiz_hints.push(hint);
    }

    void add_vert_hint(real x, real width) {
        real[] hint = { x, width };
        vert_hints.push(hint);
    }

    void write_fontforge_code(file outp = stdout,
                              string glyphvar_name = 'my_glyph',
                              string contourvar_name = 'my_contour') {
        real min_x = 1e5;
        for (path contour : outlines)
            min_x = min(min_x, min(contour).x);
        write(outp, glyphvar_name + '.foreground = fontforge.layer()\n');
        for (path contour : outlines) {
            write(outp, fontforge_contour_code(shift(lsb - min_x,-baseline) * contour, contourvar_name));
            write(outp, glyphvar_name + '.foreground += ' + contourvar_name + '\n');
        }

        write(outp, glyphvar_name + '.hhints = ' + pythonize_hints(horiz_hints, baseline));
        write(outp, glyphvar_name + '.vhints = ' + pythonize_hints(vert_hints, min_x - lsb));
        // FIXME: Add hint mask support (when it becomes available and known about in FontForge).
        // FIXME: Add diagonal hint code and support for TT instructions.

        write(outp, glyphvar_name + '.right_side_bearing = ');
        write(outp, rsb);
        write(outp, '\n');
        if (round_points)
            write(outp, glyphvar_name + '.round()\n');
        if (simplify_slightly) {
            write(outp, glyphvar_name + '.simplify(0)\n');
            if (round_points)
                write(outp, glyphvar_name + '.round()\n'); // Does this do anything?
        }
        write(outp, glyphvar_name + '.canonicalContours()\n');
        write(outp, glyphvar_name + '.canonicalStart()\n');
    }
};

Glyph copy(Glyph glyph)
{
    Glyph new_glyph;
    new_glyph.outlines = copy(glyph.outlines);
    new_glyph.name = glyph.name;
    new_glyph.unicode = glyph.unicode;
    new_glyph.baseline = glyph.baseline;
    new_glyph.lsb = glyph.lsb;
    new_glyph.rsb = glyph.rsb;
    new_glyph.horiz_hints = copy(glyph.horiz_hints);
    new_glyph.vert_hints = copy(glyph.vert_hints);
    return new_glyph;
}

Glyph operator *(transform t, Glyph glyph)
{
    Glyph new_glyph = copy(glyph);
    new_glyph.apply_transform(t);
    return new_glyph;
}

Glyph.OutlinePoint copy(Glyph.OutlinePoint p)
{
    Glyph.OutlinePoint q = new Glyph.OutlinePoint;
    q.glyph = p.glyph;
    q.outline_no = p.outline_no;
    q.time = p.time;
    q.point = p.point;
    return q;
}

Glyph.OutlinePoint operator +(Glyph.OutlinePoint p, real distance)
{
    Glyph.OutlinePoint q = copy(p);
    q.displace_along_outline(distance);
    return q;
}

Glyph.OutlinePoint operator -(Glyph.OutlinePoint p, real distance)
{
    return p + (-distance);
}

real operator -(Glyph.OutlinePoint p, Glyph.OutlinePoint q)
{
    assert(p.glyph == q.glyph && p.outline_no == q.outline_no);
    path outline = p.glyph.outlines[p.outline_no];
    real p_length = arclength(subpath(outline, 0, p.time));
    real q_length = arclength(subpath(outline, 0, q.time));
    return p_length - q_length;
}

Glyph.OutlinePoint operator ^(Glyph.OutlinePoint p, int n) {
    return p.nearby_node(n);
}

pair dir(Glyph.OutlinePoint p)
{
    return p.dir();
}

void splice_in(Glyph.OutlinePoint a, Glyph.OutlinePoint b, guide new_part)
{
    if (a.glyph != b.glyph)
        abort('splice points are on different glyphs');
    if (a.outline_no != b.outline_no)
        abort('splice points are on different outlines');
    path outline = a.glyph.outlines[a.outline_no];
    path p = (a.time < b.time) ?
        subpath(outline, b.time, a.time + length(outline)) :
        subpath(outline, b.time, a.time);
    path q = (a.point){dir(outline,a.time)}..new_part..{dir(outline,b.time)}(b.point);
    a.glyph.outlines[a.outline_no] = q & p & cycle;
    if (new_part != nullpath) {
        if (a.glyph != b.glyph)
            abort('splice points are on different glyphs');
        if (a.outline_no != b.outline_no)
            abort('splice points are on different outlines');
        path outline = a.glyph.outlines[a.outline_no];
        path p = (a.time < b.time) ?
            subpath(outline, b.time, a.time + length(outline)) :
            subpath(outline, b.time, a.time);
        path q = (a.point){dir(outline,a.time)}..new_part..{dir(outline,b.time)}(b.point);
        a.glyph.outlines[a.outline_no] = q & p & cycle;
    }
}

//-------------------------------------------------------------------------
