/*
  Copyright (c) 2011 Barry Schwartz

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation files
  (the "Software"), to deal in the Software without restriction,
  including without limitation the rights to use, copy, modify, merge,
  publish, distribute, sublicense, and/or sell copies of the Software,
  and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*/

import geometry;

real point_fuzz = 0.001;
bool round_points = false;
bool simplify_slightly = false;

//-------------------------------------------------------------------------

struct glyph_data {
    string name;
    int unicode = -9999;
    path[] contours;
    real baseline = 0;
    real lsb = 25;
    real rsb = 25;
};

glyph_data[] glyph_list;

int find_glyph(string glyphname)
{
    int i = 0;
    while (i < glyph_list.length && glyph_list[i].name != glyphname)
        i += 1;
    return i;
}

glyph_data get_glyph(string glyphname)
{
    glyph_data glyph;
    int i = find_glyph(glyphname);
    if (i < glyph_list.length)
        glyph = glyph_list[i];
    return glyph;
}

void set_glyph(glyph_data glyph)
{
    int i = find_glyph(glyph.name);
    glyph_list[i] = glyph;
}

//-------------------------------------------------------------------------

path reshape_subpath(path p, real start_time, real end_time, path new_subpath(path subpath))
// Replaces a subpath of |p| with a new subpath. The new subpath is
// constructed by |new_subpath|, which is given the original subpath
// as a parameter.
{
    while (end_time < start_time)
        end_time += length(p);
    path p1 = new_subpath(subpath(p, start_time, end_time));
    path p2 = subpath(p, end_time, length(p) + start_time);
    path q = p1 & p2;
    return cyclic(p) ? q & cycle : q;
}

path reshape_subpath(path p, real start_time, real end_time, guide new_part = nullpath .. nullpath)
// Replaces a subpath of |p| with a new subpath that is specified by
// |new_part|.
{
    path new_subpath(path q)
    {
        return point(q,0) {dir(q,0)} .. new_part .. {dir(q,length(q))} point(q,length(q));
    }

    return reshape_subpath(p, start_time, end_time, new_subpath);
}

path reshape_subpath(path p, real[] intersection_times, guide new_part = nullpath .. nullpath)
{
    return reshape_subpath(p, intersection_times[0], intersection_times[1], new_part);
}

path reshape_subpath(path p, real[][] intersection_times, guide new_part = nullpath .. nullpath)
{
    return reshape_subpath(p, intersection_times[0][1], intersection_times[1][1], new_part);
}

path reshape_subpath(path p, pair point1, pair point2, guide new_part = nullpath .. nullpath)
{
    real t1 = intersect(p, point1, point_fuzz)[0];
    real t2 = intersect(p, point2, point_fuzz)[0];
    return reshape_subpath(p, t1, t2, new_part);
}

path reshape_subpath(path p, pair[] points, guide new_part = nullpath .. nullpath)
{
    real t1 = intersect(p, points[0], point_fuzz)[0];
    real t2 = intersect(p, points[1], point_fuzz)[0];
    return reshape_subpath(p, t1, t2, new_part);
}

path reshape_subpath(path p, path intersecting_path, guide new_part = nullpath .. nullpath)
{
    path result = p;
    real[][] xsect = intersections(intersecting_path, p);

    if (2 < xsect.length)
        abort('reshape_subpath(path,path[,guide]) cannot handle more than two intersections; ' +
              format('%d', xsect.length) + ' intersections encountered');
    else if (xsect.length == 2)
        result = reshape_subpath(p, xsect[0][1], xsect[1][1], new_part);
    return result;
}

real time_at_distance_along_arc(path p, real time, real distance)
// Finds the point that is at a given arclength from a point on the
// path |p|.
{
    real here_length = arclength(subpath(p, 0, time));
    real there_length = here_length + distance;
    return arctime(p, there_length);
}

pair point_at_distance_along_arc(path p, real time, real distance)
// Finds the point that is at a given arclength from a point on the
// path |p|.
{
    return point(p, time_at_distance_along_arc(p, time, distance));
}

pair point_at_distance_along_arc(path p, pair point, real distance)
// Finds the point that is at a given arclength from a point on the
// path |p|.
{
    real[] time = intersect(p, point, point_fuzz);
    return point_at_distance_along_arc(p, time[0], distance);
}

path reshape_arc(path p, real time, real distance_before, real distance_after, guide new_part = nullpath .. nullpath)
// Calls |reshape_subpath| for a subpath extending given arclength
// from a point on |p|. Can be used like a metal file; good for
// rounding or beveling corners, for instance.
{
    real t1 = time_at_distance_along_arc(p, time, -distance_before);
    real t2 = time_at_distance_along_arc(p, time, distance_after);
    return reshape_subpath(p, t1, t2, new_part);
}

path reshape_arc(path p, real time, real distance, guide new_part = nullpath .. nullpath)
// A version of |reshape_arc| that uses the same arclength on either
// side of the point.
{
    return reshape_arc(p, time, distance, distance, new_part);
}

path reshape_arc(path p, real[] times, real distance_before, real distance_after, guide new_part = nullpath .. nullpath)
// Calls |reshape_subpath| for a subpath extending given arclength
// from two points on |p|. Can be used like a metal file; good for
// rounding or beveling ends of serifs, for instance.
{
    real t1 = time_at_distance_along_arc(p, times[0], -distance_before);
    real t2 = time_at_distance_along_arc(p, times[1], distance_after);
    return reshape_subpath(p, t1, t2, new_part);
}

path reshape_arc(path p, real[] times, real distance, guide new_part = nullpath .. nullpath)
// A version of |reshape_arc| that uses the same arclength on either
// side of the two points.
{
    return reshape_arc(p, times, distance, distance, new_part);
}

path reshape_arc(path p, pair point, real distance_before, real distance_after, guide new_part = nullpath .. nullpath)
// Calls |reshape_subpath| for a subpath extending given arclength
// from a point on |p|. Can be used like a metal file; good for
// rounding or beveling corners, for instance.
{
    real t = intersect(p, point, point_fuzz)[0];
    real t1 = time_at_distance_along_arc(p, t, -distance_before);
    real t2 = time_at_distance_along_arc(p, t, distance_after);
    return reshape_subpath(p, t1, t2, new_part);
}

path reshape_arc(path p, pair point, real distance, guide new_part = nullpath .. nullpath)
// A version of |reshape_arc| that uses the same arclength on either
// side of the point.
{
    return reshape_arc(p, point, distance, distance, new_part);
}

path reshape_arc(path p, pair[] points, real distance_before, real distance_after, guide new_part = nullpath .. nullpath)
// Calls |reshape_subpath| for a subpath extending given arclength
// from two points on |p|. Can be used like a metal file; good for
// rounding or beveling ends of serifs, for instance.
{
    real t1 = time_at_distance_along_arc(p, intersect(p, points[0], point_fuzz)[0], -distance_before);
    real t2 = time_at_distance_along_arc(p, intersect(p, points[1], point_fuzz)[0], distance_after);
    return reshape_subpath(p, t1, t2, new_part);
}

path reshape_arc(path p, pair[] points, real distance, guide new_part = nullpath .. nullpath)
// A version of |reshape_arc| that uses the same arclength on either
// side of the two points.
{
    return reshape_arc(p, points, distance, distance, new_part);
}

//-------------------------------------------------------------------------

path chop(path p, path chop_shape)
// Chops off a piece of a cycle path |p| that lies within the cyclic
// path |chop_shape|. Can be used like a counterpunch or metal file.
{
    path result = p;
    real[][] xsect = intersections(p, chop_shape);
    
    if (2 < xsect.length) {
        abort('chop(path,path) cannot handle more than two intersections; ' +
              format('%d', xsect.length) + ' intersections encountered');
    } else if (xsect.length == 2) {
        real[] xsect2;

        // Choose the subpath of |p| that is outside |chop_shape|, and
        // the subpath of |chop_shape| that is inside |p|.
        xsect2[0] = xsect[1][1];
        xsect2[1] = xsect[0][1];
        path q1 = subpath(p, xsect[0][0], xsect[1][0]);
        if (inside(chop_shape, midpoint(q1))) {
            xsect2[0] = xsect[0][1];
            xsect2[1] = xsect[1][1];
            q1 = subpath(p, xsect[1][0], length(p) + xsect[0][0]);
        }
        if (xsect2[1] < xsect2[0])
            xsect2[1] += length(chop_shape);
        path q2 = subpath(chop_shape, xsect2[1], xsect2[0]);
        if (!inside(p, midpoint(q2)))
            q2 = subpath(chop_shape, length(chop_shape) + xsect2[0], xsect2[1]);

        result = q1 & q2 & cycle;
    }
    return result;
}

path chop(path p, pair point, real angle)
// Chops along an angled straight line.
{
    real bignum = 1e6;
    pair offset = bignum * dir(angle);
    path cut_line = (point - offset) --- (point + offset);
    return reshape_subpath(p, cut_line, nullpath---nullpath);
}

//-------------------------------------------------------------------------

string fontforge_contour_code(path p, string contour_name)
// Converts a path to Python code for creating an instance of
// |fontforge.contour| describing the same bezier spline.
{
  string s = '';
  s += contour_name + ' = fontforge.contour()\n';
  for (int i = 0; i < length(p); i += 1)
    {
      s += contour_name + ' += fontforge.point(';
      s += format('%f,', point(p,i).x);
      s += format('%f)\n', point(p,i).y);

      s += contour_name + ' += fontforge.point(';
      s += format('%f,', postcontrol(p,i).x);
      s += format('%f, False)\n', postcontrol(p,i).y);

      s += contour_name + ' += fontforge.point(';
      s += format('%f, ', precontrol(p,(i + 1) % length(p)).x);
      s += format('%f, False)\n', precontrol(p,(i + 1) % length(p)).y);
    }
  s += contour_name + '.closed = ' + (cyclic(p) ? 'True' : 'False') + '\n';
  return s;
}

void write_fontforge_glyph_list_code(string contour_name, file outp = stdout, string unicode_function = '')
{
    if (unicode_function == '') {
        write(outp, 'from sortsmill.glyphbuild import preferred_unicode\n');
        unicode_function = 'preferred_unicode';
    }

    for (glyph_data g : glyph_list) {
        string uni;
        if (-1 <= g.unicode)
            uni = format('%d', g.unicode);
        else
            uni = unicode_function + '(\'' + g.name + '\')';
        write(outp, 'glyph = f.createChar(' + uni + ', \'' + g.name + '\')\n');
        write(outp, 'glyph.unicode = ' + uni + '\n');
        for (path contour : g.contours) {
            write(outp, fontforge_contour_code(shift(0,-g.baseline) * contour, 'contour'));
            write(outp, 'glyph.foreground += contour\n');
        }
        write(outp, 'glyph.left_side_bearing = ');
        write(outp, g.lsb);
        write(outp, '\n');
        write(outp, 'glyph.right_side_bearing = ');
        write(outp, g.rsb);
        write(outp, '\n');
        if (round_points)
            write(outp, 'glyph.round()\n');
        if (simplify_slightly) {
            write(outp, 'glyph.simplify(0)\n');
            write(outp, 'glyph.round()\n'); // Does this do anything?
        }
    }
}

void write_glyph_data(string glyphname)
{
    write('import fontforge');
    glyph_data g = get_glyph(glyphname);
    if (g.name != '') {
        write('glyph = fontforge.activeGlyph()');
        real min_x = 1e5;
        for (path contour : g.contours)
            min_x = min(min_x, min(contour).x);
        write('glyph.foreground = fontforge.contour()');
        for (path contour : g.contours) {
            write(fontforge_contour_code(shift(g.lsb - min_x,0) * contour, 'contour'));
            write('glyph.foreground += contour');
        }
        write('glyph.right_side_bearing = ', g.rsb);
        if (round_points)
            write('glyph.round()');
        if (simplify_slightly) {
            write('glyph.simplify(0)');
            write('glyph.round()'); // Does this do anything?
        }
    }
}

//-------------------------------------------------------------------------
