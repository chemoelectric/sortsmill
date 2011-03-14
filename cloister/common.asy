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

real boldness;

real ascender_height;
real stem_width;
real corner_rounding_distance = 10;
real corner_rounding_tension = 0.75;

struct reshape_params {
    real distance_before;
    real distance_after;
    guide shape;
};

struct stem_counter_params {
    real stem_height;
    real top_angle;
    real bottom_angle;
    reshape_params top_corner;
    reshape_params bottom_corner;
};

struct bottom_serif {
    pair lower_left;
    pair lower_right;
};

struct ascender_serif_params {
    real angle;
    reshape_params right_corner;
    pair left_stem_top;//////////////////////////???????????????????????????????????????????????????????????????
};

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
    return reshape_arc(p, point, params.distance_before, params.distance_after, params.shape);
}

path reshape_around_point(path p, real time, reshape_params params)
{
    return reshape_arc(p, time, params.distance_before, params.distance_after, params.shape);
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
    // Cut the upper left.
    pair top_serif_offset1 = (-41.5, 54);
    pair point4 = params.left_stem_top + top_serif_offset1;
    outline = chop(outline, point4, params.angle);

    // Cut the far left.
    pair point5 = intersectionpoint(outline, (point4 - (0,1))---(point4 - (0,100)));
    outline = reshape_subpath(outline, point5, point4, nullpath---nullpath);

    // Shape the upper right of the top serif.
    real t4 = round(intersect(outline, point4, point_fuzz)[0]);
    outline = reshape_around_point(outline, t4 + 1, params.right_corner);

    // Round off the sharp corners.
    outline = smooth_a_corner(outline, point4);
    outline = smooth_a_corner(outline, point5);

    return outline;
}

//-------------------------------------------------------------------------
