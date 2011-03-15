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

//=========================================================================
//
// Lowercase 'l'.

real l_ascender_height = 644;
real l_stem_width = 65 + (22/300)*(boldness - 400);

l_left_stem_position = (0, 39 + (3/300)*(boldness - 400));

real l_left_stem_rightmost = 0.90 + (0.02/300)*(boldness - 400);
l_left_stem_params = new stem_counter_params;
l_left_stem_params.stem_height = 500 - (21/300)*(boldness - 400);
l_left_stem_params.top_angle = -32 - (5/300)*(boldness - 400);
l_left_stem_params.bottom_angle = 13;
l_left_stem_params.top_corner.distance_before = 30;
l_left_stem_params.top_corner.distance_after = (1 - l_left_stem_rightmost - 0.01) * l_left_stem_params.stem_height;
l_left_stem_params.top_corner.shape = nullpath..nullpath;
l_left_stem_params.bottom_corner.distance_before = 0.90 * l_left_stem_params.stem_height;
l_left_stem_params.bottom_corner.distance_after = 35;
l_left_stem_params.bottom_corner.shape =
    nullpath..tension 10.0..
    (-2 - (2/300)*(boldness - 400),166)..controls (-3 - (3/300)*(boldness - 400),10) and (-3 - (3/300)*(boldness - 400),0)..
    nullpath;

l_right_stem_position = (l_stem_width, 36 + (4/300)*(boldness - 400));

real l_right_stem_leftmost = 0.1 - (0.04/300)*(boldness - 400);
l_right_stem_params = new stem_counter_params;
l_right_stem_params.stem_height = 515 - (9/300)*(boldness - 400);
l_right_stem_params.top_angle = 77;
l_right_stem_params.bottom_angle = 1.4;
l_right_stem_params.top_corner.distance_before = (1 - l_right_stem_leftmost - 0.02) * l_right_stem_params.stem_height;
l_right_stem_params.top_corner.distance_after = 35;
l_right_stem_params.top_corner.shape =
    nullpath..
    controls (0,l_right_stem_params.stem_height - 200) and (0,l_right_stem_params.stem_height)..
    nullpath;
l_right_stem_params.bottom_corner.distance_before = 40;
l_right_stem_params.bottom_corner.distance_after = l_right_stem_leftmost * l_right_stem_params.stem_height;
l_right_stem_params.bottom_corner.shape = nullpath..tension 0.75 and 1.1..nullpath;

l_bottom_serif_params = new bottom_serif_params;
l_bottom_serif_params.upper_left_x = -49 - (4/300)*(boldness - 400);
l_bottom_serif_params.upper_right_x = 134 + (15/300)*(boldness - 400);
l_bottom_serif_params.lower_left = (l_bottom_serif_params.upper_left_x,-5);
l_bottom_serif_params.lower_right = (l_bottom_serif_params.upper_right_x,3);
l_bottom_serif_params.lower_left_control =
    l_bottom_serif_params.lower_left + (60,3) + ((-24,-2)/300)*(boldness - 400);
l_bottom_serif_params.lower_right_control =
    l_bottom_serif_params.lower_right - (111,2) - ((36,-1)/300)*(boldness - 400);

pair l_left_stem_top = l_left_stem_position + (0,l_left_stem_params.stem_height);

l_ascender_serif_params = new ascender_serif_params;
pair point_on_slope_adjustment = (3/300,-2/300)*(boldness - 400);
    l_ascender_serif_params.point_on_slope = l_left_stem_top + (-42, 54) + point_on_slope_adjustment;
pair point_below_slope =
    intersectionpoint(l_left_stem_top---(l_left_stem_top - 200*dir(l_left_stem_params.top_angle)),
                      (-42 + point_on_slope_adjustment.x,0)---(-42 + point_on_slope_adjustment.x,2000));
l_ascender_serif_params.slope_angle = 27 + (3/300)*(boldness - 400);
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
pair top_point = (l_stem_width + 2, l_ascender_height);
pair right_point = top_point + (15,-15);
pair l_right_stem_top = l_right_stem_position + (0,l_right_stem_params.stem_height);
l_ascender_serif_params.right_corner.point_after =
    intersectionpoint((-1000,top_point.y - 40)---(1000,top_point.y - 40),
                      l_right_stem_top---(l_right_stem_top + 1000*dir(l_right_stem_params.top_angle)));
l_ascender_serif_params.right_corner.shape =
    nullpath..tension 1.2..
    top_point{right}..tension 1.2..
    right_point{down}..tension 1.2..
    nullpath;

//-------------------------------------------------------------------------

// Create a rectangular blank.
real bignum = 1000;
real blank_width = 200 + l_stem_width;
path outline =
    shift(l_stem_width/2,0) *
    ((-blank_width/2,-bignum)---(-blank_width/2,bignum)---(blank_width/2,bignum)---(blank_width/2,-bignum)---cycle);

// Form the stem by "punching" a counter on either side.
outline = chop(outline, shift(l_left_stem_position) * left_stem_counter(l_left_stem_params));
outline = chop(outline, shift(l_right_stem_position) * right_stem_counter(l_right_stem_params));

outline = form_ascender_serif(outline, l_ascender_serif_params);
outline = form_bottom_serif(outline, l_bottom_serif_params);
outline = smooth_close_points(outline);

//-------------------------------------------------------------------------

pair top_point = point(outline, maxtimes(outline)[1]);
pair bottom_point = point(outline, mintimes(outline)[1]);

glyph_data glyph;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
glyph.lsb = l_bottom_serif_params.lower_left.x;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
glyph.name = 'l';
add_vert_hint(glyph, 0, l_stem_width);
add_horiz_hint(glyph, top_point.y, -20);
add_horiz_hint(glyph, bottom_point.y + 21, -21);
glyph.contours[0] = outline;
set_glyph(glyph);

//-------------------------------------------------------------------------
