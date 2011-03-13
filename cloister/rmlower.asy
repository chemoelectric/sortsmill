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

pair top_serif_offset1 = (-42, 54);
real top_serif_xoffset2 = -2;

real stem_right_height = 0.815 * ascender_height;
real stem_left_height = 0.783 * ascender_height;

// Create a rectangular blank.
real bignum = 1000;
real blank_width = 200 + stem_width;
path outline = shift(stem_width/2,0) *
    ((-blank_width/2,-bignum)---(-blank_width/2,bignum)---(blank_width/2,bignum)---(blank_width/2,-bignum)---cycle);

// Form the stem by "punching" a counter on either side.
path left_counter = left_stem_counter(l_stem_left_pos, stem_left_height, 0.9 * stem_left_height,
                                      l_top_left_counter_angle, 7, -12, 30, 20, 30, 1, 20, 0.9, 0.9, 1);
path right_counter = right_stem_counter(l_stem_right_pos, stem_right_height, 0.2 * stem_right_height,
                                        4, 77, 1.4, 15, 40, 46, 15, 30, 3, 1, 0.9, 0.8, 1, 8);
outline = chop(outline, left_counter);
outline = chop(outline, shift(stem_width,0) * right_counter);

//-------------------------------------------
//
// Form the bottom serif.

outline = chop(outline, l_bottom_serif.lower_right, l_bottom_serif.lower_left);

path cut_line = ((l_bottom_serif.lower_left.x,100) + dir(-90)*1000)---(l_bottom_serif.lower_left.x,100);
pair[] points1 = intersectionpoints(cut_line, outline);
outline = reshape_subpath(outline, cut_line, nullpath---nullpath);
path cut_line = (l_bottom_serif.lower_right.x,100)---((l_bottom_serif.lower_right.x,100) + dir(-90)*1000);
pair[] points2 = intersectionpoints(cut_line, outline);
outline = reshape_subpath(outline, cut_line, nullpath---nullpath);

outline = reshape_subpath(outline, points2[1], points1[0],
                          nullpath{left} ..
                          controls points2[1] + 0.7*(points2[1].x - points1[0].x)*dir(181)
                          and points1[0] + 0.4 * (points2[1] - points1[0]) ..
                          nullpath);

// Round the sharp corners of the bottom serif.
pair point3 = point_at_distance_along_arc(outline, points1[1], corner_rounding_distance);
outline = reshape_arc(outline, points1[0], corner_rounding_distance, nullpath..tension corner_rounding_tension..nullpath);
outline = reshape_arc(outline, points1[1], corner_rounding_distance, nullpath..tension corner_rounding_tension..nullpath);
outline = reshape_arc(outline, points2[0], corner_rounding_distance, nullpath..tension corner_rounding_tension..nullpath);
outline = reshape_arc(outline, points2[1], corner_rounding_distance, nullpath..tension corner_rounding_tension..nullpath);

// Smooth a rough spot on the left top of the lower serif.
// FIXME: Write a routine to go over an outline and recognize and fix these spots.
real t3 = round(intersect(outline, point3, point_fuzz)[0]);
outline = reshape_subpath(outline, t3, t3 + 1, nullpath---nullpath);

//-------------------------------------------
//
// Form the upper serif.

// Cut the upper left.
pair point4 = (0, l_stem_left_pos + stem_left_height) + top_serif_offset1;
outline = chop(outline, point4, l_top_serif_angle1);

// Cut the far left.
pair point5 = intersectionpoint(outline, (point4 - (0,1))---(point4 - (0,100)));
outline = reshape_subpath(outline, point5, point4, nullpath---nullpath);

// Shape the upper right of the top serif.
real t4 = round(intersect(outline, point4, point_fuzz)[0]);
pair point4a = point_at_distance_along_arc(outline, t4 + 1, -50);
pair top_point = point4a + (17,5);
outline = reshape_arc(outline, t4 + 1, 50, 41, nullpath..top_point{right}..nullpath);

// Smooth a spot on the right bottom of the top serif.
// FIXME: Write a routine to go over an outline and recognize and fix these spots.
pair point4b = point_after(outline, top_point);
pair point4c = point_after(outline, top_point, 2);
outline = reshape_subpath(outline, point4b, point4c, nullpath---nullpath);

// Round the sharp corners of the top serif.
pair point5a = point_at_distance_along_arc(outline, point5, -corner_rounding_distance);
outline = reshape_arc(outline, point4, corner_rounding_distance, nullpath..tension corner_rounding_tension..nullpath);
outline = reshape_arc(outline, point5, corner_rounding_distance, nullpath..tension corner_rounding_tension..nullpath);

// Smooth a rough spot on the left bottom of the top serif.
// FIXME: Write a routine to go over an outline and recognize and fix these spots.
real t5a = round(intersect(outline, point5a, point_fuzz)[0]);
outline = reshape_subpath(outline, t5a - 1, t5a, nullpath---nullpath);

//-------------------------------------------

pair bot_point = point(outline, mintimes(outline)[1]);

glyph_data glyph;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
glyph.lsb = l_bottom_serif.lower_left.x;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
glyph.name = 'l';
add_vert_hint(glyph, 0, stem_width);
add_horiz_hint(glyph, top_point.y, -20);
add_horiz_hint(glyph, bot_point.y + 21, -21);
glyph.contours[0] = outline;
set_glyph(glyph);

//=========================================================================
