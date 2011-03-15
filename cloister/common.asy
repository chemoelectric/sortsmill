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
    
import fontforge;

round_points = true;
simplify_slightly = true;

real boldness;

real corner_rounding_distance = 10;
real corner_rounding_tension = 0.75;

struct reshape_params {
    real distance_before = -infinity;
    real distance_after = -infinity;
    pair point_before = (-infinity, -infinity);
    pair point_after = (-infinity, -infinity);
    guide shape = nullpath..nullpath;
};

struct stem_counter_params {
    real stem_height;
    real top_angle;
    real bottom_angle;
    reshape_params top_corner;
    reshape_params bottom_corner;
};

struct bottom_serif_params {
    real upper_left_x;
    real upper_right_x;
    pair lower_left;
    pair lower_right;
    pair lower_left_control;
    pair lower_right_control;
};

struct ascender_serif_params {
    pair point_on_slope;
    real slope_angle;
    reshape_params left_corner;
    reshape_params right_corner;
};

real transform(real value, real a)
{
    return value + (a/300)*(boldness - 400);
}

pair transform(explicit pair value, explicit pair a)
{
    return value + (a/300)*(boldness - 400);
}

//-------------------------------------------------------------------------

string version = '0.1';

string copyright_notice = 'Copyright (c) 2011 Barry Schwartz';

string license_notice =
    'Permission is hereby granted, free of charge, to any person obtaining a copy\\n'
    'of this software and associated documentation files (the "Software"), to deal\\n'
    'in the Software without restriction, including without limitation the rights\\n'
    'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell\\n'
    'copies of the Software, and to permit persons to whom the Software is\\n'
    'furnished to do so, subject to the following conditions:\\n'
    '\\n'
    'The above copyright notice and this permission notice shall be included in\\n'
    'all copies or substantial portions of the Software.\\n'
    '\\n'
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\\n'
    'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\\n'
    'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE\\n'
    'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\\n'
    'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,\\n'
    'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN\\n'
    'THE SOFTWARE.\\n';

string license_url = 'http://www.opensource.org/licenses/mit-license.php';

string vendor_url = 'http://sortsmill.googlecode.com';
string designer_name = 'Barry Schwartz';
string designer_url = 'http://crudfactory.com';

string fontname = 'Untitled';
string familyname = 'Untitled';
string fullname = 'Untitled';
string fontweight = 'Regular';
string sfnt_family = 'Untitled';
string sfnt_style = 'Regular';
string preferred_family = '';
string preferred_style = '';
string wws_family = '';
string wws_style = '';

// FIXME: Support design ranges, once multiple design sizes are
// available.
real design_size = 0;

//-------------------------------------------------------------------------

void write_python()
{
    write('#!/usr/bin/env python

import fontforge
import psMat

f = fontforge.font()

f.version = \'' + version + '\'
f.copyright = \'' + copyright_notice + '\'
f.fontname = \'' + fontname + '\'
f.familyname = \'' + familyname + '\'
f.fullname = \'' + fullname + '\'
f.weight = \'' + fontweight + '\'

lang = \'English (US)\'
f.appendSFNTName(lang, \'Family\', \'' + sfnt_family + '\')
f.appendSFNTName(lang, \'SubFamily\', \'' + sfnt_style + '\')');

    if (preferred_family != '')
        write('f.appendSFNTName(lang, \'Preferred Family\', \'' + preferred_family + '\')');
    if (preferred_style != '')
        write('f.appendSFNTName(lang, \'Preferred Styles\', \'' + preferred_style + '\')');
    
    if (wws_family != '')
        write('f.appendSFNTName(lang, \'WWS Family\', \'' + wws_family + '\')');
    if (wws_style != '')
        write('f.appendSFNTName(lang, \'WWS Subfamily\', \'' + wws_style + '\')');
    
    write('f.appendSFNTName(lang, \'License\', \'' + copyright_notice + '\\n\\n' + license_notice + '\');
f.appendSFNTName(lang, \'License URL\', \'' + license_url + '\');
f.appendSFNTName(lang, \'Vendor URL\', \'' + vendor_url + '\');
f.appendSFNTName(lang, \'Designer\', \'' + designer_name + '\');
f.appendSFNTName(lang, \'Designer URL\', \'' + designer_url + '\');
');

    if (0 < design_size)
        write('f.size_feature = (' + string(design_size) + ',)');

    write_fontforge_glyph_list_code('contour');

    write('f.encoding = \'UnicodeBMP\'');
    write(fontforge_ps_private_code('f'));
}

void write_sfd(string sfd_file)
{
    write_python();
    write('f.save(\'' + sfd_file + '\')');
}

void generate(string otf_file)
{
    write_python();
    write('f.generate(\'' + otf_file + '\')');
}

//-------------------------------------------------------------------------

path reshape_around_point(path p, pair point, reshape_params params)
{
    path q;
    if (params.distance_before <= -infinity)
        if (params.distance_after <= -infinity)
            q = reshape_subpath(p, params.point_before, params.point_after, params.shape);
        else
            q = reshape_arc(p, point, params.point_before, params.distance_after, params.shape);
    else if (params.distance_after <= -infinity)
        q = reshape_arc(p, point, params.distance_before, params.point_after, params.shape);
    else
        q = reshape_arc(p, point, params.distance_before, params.distance_after, params.shape);
    return q;
}

path reshape_around_point(path p, real time, reshape_params params)
{
    path q;
    if (params.distance_before <= -infinity)
        if (params.distance_after <= -infinity)
            q = reshape_subpath(p, params.point_before, params.point_after, params.shape);
        else
            q = reshape_arc(p, time, params.point_before, params.distance_after, params.shape);
    else if (params.distance_after <= -infinity)
        q = reshape_arc(p, time, params.distance_before, params.point_after, params.shape);
    else
        q = reshape_arc(p, time, params.distance_before, params.distance_after, params.shape);
    return q;
}

path smooth_a_corner(path p, pair point,
                     real rounding_distance = corner_rounding_distance,
                     guide new_subpath_guide = nullpath..tension corner_rounding_tension..nullpath)
{
    return reshape_arc(p, point, corner_rounding_distance, new_subpath_guide);
}

//-------------------------------------------------------------------------

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
        if (abs(degrees(dir2) - degrees(dir1)) <= max_angle) {
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

//-------------------------------------------------------------------------

path left_stem_counter(stem_counter_params params)
// Counterpunch for the left side of roman 'l', 'i', etc.
{
    real bignum = 2000;

    path counter = (-bignum,-bignum)---(-bignum,bignum)---(0,bignum)---(0,-bignum)---cycle;
    counter = chop(counter, (0, params.stem_height), params.top_angle);
    counter = chop(counter, (0,0), 180 + params.bottom_angle);
    counter = reshape_around_point(counter, (0, params.stem_height), params.top_corner);
    counter = reshape_around_point(counter, (0,0), params.bottom_corner);
    return counter;
}

path right_stem_counter(stem_counter_params params)
// Counterpunch for the right side of roman 'l', 'i', etc.
{
    real bignum = 2000;

    path counter = (0,-bignum)---(0,bignum)---(bignum,bignum)---(bignum,-bignum)---cycle;
    counter = chop(counter, (0, params.stem_height), params.top_angle);
    counter = chop(counter, (0,0), 180 + params.bottom_angle);
    counter = reshape_around_point(counter, (0, params.stem_height), params.top_corner);
    counter = reshape_around_point(counter, (0,0), params.bottom_corner);
    return counter;
}

//-------------------------------------------------------------------------

path form_ascender_serif(path outline, ascender_serif_params params)
{
    // Cut the sloped top.
    outline = chop(outline, params.point_on_slope, params.slope_angle);

    // Shape the upper right corner.
    real t_left_corner = floor(intersect(outline, params.point_on_slope, point_fuzz)[0]);
    outline = reshape_around_point(outline, t_left_corner + 1, params.right_corner);

    // Shape the left corner.
    t_left_corner = floor(intersect(outline, params.point_on_slope, point_fuzz)[0]);
    outline = reshape_around_point(outline, t_left_corner, params.left_corner);

    return outline;
}

//-------------------------------------------------------------------------

path form_bottom_serif(path outline, bottom_serif_params params)
{
    // Cut the sloped unshaped bottom.
    outline = chop(outline, params.lower_right, params.lower_left);

    // Cut the left end of the serif.
    path cut_line = (params.lower_left.x,-100)---(params.upper_left_x,100);
    outline = reshape_subpath(outline, cut_line, nullpath---nullpath);

    // Cut the right end of the serif.
    cut_line = (params.upper_right_x,100)---(params.lower_right.x,-100);
    outline = reshape_subpath(outline, cut_line, nullpath---nullpath);

    // Round off the upper sharp corners
    outline = smooth_a_corner(outline, point_after(outline, params.lower_left));
    outline = smooth_a_corner(outline, point_before(outline, params.lower_right));

    // Make the bottom of the serif convex.
    outline = reshape_subpath(outline, params.lower_right, params.lower_left,
                              nullpath..
                              controls params.lower_right_control and params.lower_left_control..
                              nullpath);

    // Round off the bottom sharp corners.
    outline = smooth_a_corner(outline, params.lower_left);
    outline = smooth_a_corner(outline, params.lower_right);

    return outline;
}

//-------------------------------------------------------------------------
