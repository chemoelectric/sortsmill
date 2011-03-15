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

import common;
import fontforge;
import rmparam;

round_points = true;
simplify_slightly = true;

fontname = 'CloisterStM-Bold';
familyname = 'Sorts Mill Cloister';
fullname = 'Sorts Mill Cloister Bold';
fontweight = 'Bold';
sfnt_family = 'Sorts Mill Cloister';
sfnt_style = 'Bold';
design_size = 14;

//-------------------------------------------------------------------------

boldness = 2;                   // FIXME: is this of any use?

stem_width = 90;
ascender_height = 644;

l_left_stem_position = (0,40);

l_left_stem_params = new stem_counter_params;
l_left_stem_params.stem_height = 477;
l_left_stem_params.top_angle = -40;
l_left_stem_params.bottom_angle = 11;
l_left_stem_params.top_corner.distance_before = 10;
l_left_stem_params.top_corner.distance_after = 10;
l_left_stem_params.top_corner.shape = nullpath..nullpath;
l_left_stem_params.bottom_corner.distance_before = 10;
l_left_stem_params.bottom_corner.distance_after = 10;
l_left_stem_params.bottom_corner.shape = nullpath..nullpath;

l_right_stem_position = (stem_width - 3,40);

l_right_stem_params = new stem_counter_params;
l_right_stem_params.stem_height = 512;
l_right_stem_params.top_angle = 77;
l_right_stem_params.bottom_angle = 1.4;
l_right_stem_params.top_corner.distance_before = 0.85 * l_right_stem_params.stem_height;
l_right_stem_params.top_corner.distance_after = 40;
l_right_stem_params.top_corner.shape = nullpath..tension 1 and 1 ..nullpath;
l_right_stem_params.bottom_corner.distance_before = 40;
l_right_stem_params.bottom_corner.distance_after = 0.1 * l_right_stem_params.stem_height;
l_right_stem_params.bottom_corner.shape = nullpath..tension 0.75 and 1.1..nullpath;

l_bottom_serif = new bottom_serif;
l_bottom_serif.lower_left = (-53,-7);
l_bottom_serif.lower_right = (149,3);

l_ascender_serif_params = new ascender_serif_params;
l_ascender_serif_params.angle = 30;
l_ascender_serif_params.right_corner.distance_before = 55;
l_ascender_serif_params.right_corner.distance_after = 60;
pair top_point = (stem_width - 1, ascender_height);
pair right_point = top_point + (15,-15);
l_ascender_serif_params.right_corner.shape =
    nullpath..tension 1.2..
    top_point{right}..tension 2.0..
    right_point{down}..tension 2.0..
    nullpath;

import rmlower;

std_vw = stem_width;
add_to_stemsnap_v(stem_width);
bluefuzz = 0;

usersetting();

//-------------------------------------------------------------------------
