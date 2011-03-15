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

fontname = 'CloisterStM';
familyname = 'Sorts Mill Cloister';
fullname = 'Sorts Mill Cloister';
fontweight = 'Regular';
sfnt_family = 'Sorts Mill Cloister';
sfnt_style = 'Regular';
design_size = 14;

//-------------------------------------------------------------------------

boldness = 1;                   // FIXME: is this of any use?

stem_width = 68;
ascender_height = 644;

l_left_stem_position = (0,39);

l_left_stem_params = new stem_counter_params;
l_left_stem_params.stem_height = 500;
l_left_stem_params.top_angle = -32;
l_left_stem_params.bottom_angle = 13;
l_left_stem_params.top_corner.distance_before = 30;
l_left_stem_params.top_corner.distance_after = 0.09 * l_left_stem_params.stem_height;
l_left_stem_params.top_corner.shape = nullpath..nullpath;
l_left_stem_params.bottom_corner.distance_before = 0.90 * l_left_stem_params.stem_height;
l_left_stem_params.bottom_corner.distance_after = 25;
l_left_stem_params.bottom_corner.shape = nullpath..controls (0,50) and (-2,0)..nullpath;

l_right_stem_position = (stem_width - 3,36);

l_right_stem_params = new stem_counter_params;
l_right_stem_params.stem_height = 515;
l_right_stem_params.top_angle = 77;
l_right_stem_params.bottom_angle = 1.4;
l_right_stem_params.top_corner.distance_before = 0.85 * l_right_stem_params.stem_height;
l_right_stem_params.top_corner.distance_after = 35;
l_right_stem_params.top_corner.shape =
    nullpath..
    controls (0,l_right_stem_params.stem_height - 200) and (0,l_right_stem_params.stem_height)..
    nullpath;
l_right_stem_params.bottom_corner.distance_before = 40;
l_right_stem_params.bottom_corner.distance_after = 0.13 * l_right_stem_params.stem_height;
l_right_stem_params.bottom_corner.shape = nullpath..tension 0.75 and 1.1..nullpath;

l_bottom_serif_params = new bottom_serif_params;
l_bottom_serif_params.upper_left_x = -49;
l_bottom_serif_params.upper_right_x = 134;
l_bottom_serif_params.lower_left = (-49,-5);
l_bottom_serif_params.lower_right = (134,3);
l_bottom_serif_params.lower_left_control = l_bottom_serif_params.lower_left + (60,3);
l_bottom_serif_params.lower_right_control = l_bottom_serif_params.lower_right - (111,2);

pair l_left_stem_top = l_left_stem_position + (0,l_left_stem_params.stem_height);

l_ascender_serif_params = new ascender_serif_params;
l_ascender_serif_params.point_on_slope = l_left_stem_top + (-42, 54);
pair point_below_slope =
    intersectionpoint(l_left_stem_top---(l_left_stem_top - 200*dir(l_left_stem_params.top_angle)),
                      (-42,0)---(-42,2000));
l_ascender_serif_params.slope_angle = 27;
l_ascender_serif_params.left_corner.point_before =
    point_below_slope + 10*dir(l_left_stem_params.top_angle);
l_ascender_serif_params.left_corner.point_after =
    l_ascender_serif_params.point_on_slope + 10*dir(l_ascender_serif_params.slope_angle);
l_ascender_serif_params.left_corner.shape =
    nullpath..tension corner_rounding_tension..
    (point_below_slope + (0,10)){up}---
    (l_ascender_serif_params.point_on_slope - (0,10)){up}..tension corner_rounding_tension..
    nullpath;
l_ascender_serif_params.right_corner.point_before =
    intersectionpoint((l_right_stem_position - (24,0))---((l_right_stem_position - (24,0)) + 2000*dir(90)),
                      l_ascender_serif_params.point_on_slope---(l_ascender_serif_params.point_on_slope +
                                                                1000*dir(l_ascender_serif_params.slope_angle)));
pair top_point = (stem_width - 1, ascender_height);
pair right_point = top_point + (15,-15);
pair l_right_stem_top = l_right_stem_position + (0,l_right_stem_params.stem_height);
l_ascender_serif_params.right_corner.point_after =
    intersectionpoint((-1000,top_point.y - 42)---(1000,top_point.y - 42),
                      l_right_stem_top---(l_right_stem_top + 1000*dir(l_right_stem_params.top_angle)));
l_ascender_serif_params.right_corner.shape =
    nullpath..tension 1.2..
    top_point{right}..tension 1.2..
    right_point{down}..tension 1.2..
    nullpath;

import rmlower;

std_vw = stem_width;
add_to_stemsnap_v(stem_width);
bluefuzz = 0;

usersetting();

//-------------------------------------------------------------------------
