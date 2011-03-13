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

void add_to_sorted_list(real[] array, real v)
{
    real[] a = array;
    int i = 0;
    while (i < a.length && a[i] != v) // Maybe this test should have some fuzz?
        i += 1;
    a[i] = v;
    a = sort(a);
    for (i = 0; i < a.length; i += 1)
        array[i] = a[i];
}

//-------------------------------------------------------------------------

struct glyph_data {
    string name;
    int unicode = -9999;
    path[] contours;
    real baseline = 0;
    real lsb = 25;
    real rsb = 25;
    real[][] horiz_hints;
    real[][] vert_hints;
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

void add_horiz_hint(glyph_data glyph, real x, real width)
{
    real[] hint = { x, width };
    glyph.horiz_hints[glyph.horiz_hints.length] = hint;
}

void add_vert_hint(glyph_data glyph, real x, real width)
{
    real[] hint = { x, width };
    glyph.vert_hints[glyph.vert_hints.length] = hint;
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

real std_hw = 0;
real std_vw = 0;
real[] stemsnap_h;
real[] stemsnap_v;
real bluefuzz = 0;
real blueshift = 7;

string pythonize_array(real[] a)
{
    string s = '[';
    for (real element : a)
        s = s + format('%f,', element);
    return s;
}

string fontforge_ps_private_code(string font_obj_name)
{
    // FIXME: Add FontForge scripting support for clearing the Private
    // dictionary, and use that support here. FontForge's Python
    // support for the Private dictionary is spotty (12 March 2011).
    string s = '';
    if (0 < std_hw)
        s = s + font_obj_name + '.private[\'StdHW\'] = [' + string(std_hw) + ']\n';
    if (0 < std_vw)
        s = s + font_obj_name + '.private[\'StdVW\'] = [' + string(std_vw) + ']\n';
    if (0 < stemsnap_h.length)
        s = s + font_obj_name + '.private[\'StemSnapH\'] = ' + pythonize_array(stemsnap_h) + ']\n';
    if (0 < stemsnap_v.length)
        s = s + font_obj_name + '.private[\'StemSnapV\'] = ' + pythonize_array(stemsnap_v) + ']\n';
    s = s + font_obj_name + '.private[\'BlueShift\'] = ' + string(blueshift) + '\n';
    s = s + font_obj_name + '.private[\'BlueFuzz\'] = ' + string(bluefuzz) + '\n';
    return s;
}

void add_to_stemsnap_h(real v)
{
    add_to_sorted_list(stemsnap_h, v);
}

void add_to_stemsnap_v(real v)
{
    add_to_sorted_list(stemsnap_v, v);
}

void write_private_data()
{
    write('import fontforge');
    write('font = fontforge.activeFont()');
    write(fontforge_ps_private_code('font'));
}

//-------------------------------------------------------------------------

path reshape_subpath(path p, real start_time, real end_time, guide new_subpath_guide(path subpath))
// Replaces a subpath of cyclic path |p| with a new subpath. The new
// subpath is constructed by |new_subpath_guide|, which is given the
// original subpath as a parameter.
{
    while (end_time < start_time)
        end_time += length(p);
    guide g_new = new_subpath_guide(subpath(p, start_time, end_time));
    guide g_old = subpath(p, end_time, length(p) + start_time);
    path q = g_old & g_new & cycle;
    return q;
}

path reshape_subpath(path p, real start_time, real end_time, guide new_part = nullpath .. nullpath)
// Replaces a subpath of |p| with a new subpath that is specified by
// |new_part|.
{
    guide new_subpath_guide(path q)
    {
        return point(q,0) {dir(q,0)} .. new_part .. {dir(q,length(q))} point(q,length(q));
    }

    return reshape_subpath(p, start_time, end_time, new_subpath_guide);
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

void write_fontforge_glyph_data(glyph_data g, string contour_name, file outp)
{
    real min_x = 1e5;
    for (path contour : g.contours)
        min_x = min(min_x, min(contour).x);
    write(outp, 'glyph.foreground = fontforge.contour()\n');
    for (path contour : g.contours) {
        write(outp, fontforge_contour_code(shift(g.lsb - min_x,-g.baseline) * contour, 'contour'));
        write(outp, 'glyph.foreground += contour\n');
    }

    write(outp, 'glyph.hhints = ' + pythonize_hints(g.horiz_hints, g.baseline));
    write(outp, 'glyph.vhints = ' + pythonize_hints(g.vert_hints, min_x - g.lsb));
    // FIXME: Add hint mask support (when it becomes available and known about in FontForge).
    // FIXME: Add diagonal hint code and support for TT instructions.

    write(outp, 'glyph.right_side_bearing = ');
    write(outp, g.rsb);
    write(outp, '\n');
    if (round_points)
        write(outp, 'glyph.round()\n');
    if (simplify_slightly) {
        write(outp, 'glyph.simplify(0)\n');
        if (round_points)
            write(outp, 'glyph.round()\n'); // Does this do anything?
    }
    write(outp, 'glyph.canonicalContours()\n');
    write(outp, 'glyph.canonicalStart()\n');
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
        write_fontforge_glyph_data(g, contour_name, outp);
    }
}

void write_glyph_data(string glyphname)
{
    write('import fontforge');
    glyph_data g = get_glyph(glyphname);
    if (g.name != '') {
        write('glyph = fontforge.activeGlyph()');
        write_fontforge_glyph_data(g, 'contour', stdout);
    }
}

//-------------------------------------------------------------------------

pair point_after(path outline, pair point, int n = 1)
{
    real t = round(intersect(outline, point, point_fuzz)[0]);
    return point(outline, t + n);
}

pair point_before(path outline, pair point, int n = 1)
{
    return point_after(outline, point, -n);
}

//-------------------------------------------------------------------------
