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

// Create a rectangular blank.
real bignum = 1000;
real blank_width = 200 + stem_width;
path outline =
    shift(stem_width/2,0) *
    ((-blank_width/2,-bignum)---(-blank_width/2,bignum)---(blank_width/2,bignum)---(blank_width/2,-bignum)---cycle);

// Form the stem by "punching" a counter on either side.
outline = chop(outline, shift(l_left_stem_position) * left_stem_counter(l_left_stem_params));
outline = chop(outline, shift(l_right_stem_position) * right_stem_counter(l_right_stem_params));

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
outline = smooth_a_corner(outline, points1[0]);
outline = smooth_a_corner(outline, points1[1]);
outline = smooth_a_corner(outline, points2[0]);
outline = smooth_a_corner(outline, points2[1]);

//-------------------------------------------
//
// Form the upper serif.

// Cut the upper left.
pair top_serif_offset1 = (-41.5, 54);
pair point4 = (0, l_left_stem_position.y + l_left_stem_params.stem_height) + top_serif_offset1;
outline = chop(outline, point4, l_ascender_serif.angle);

// Cut the far left.
pair point5 = intersectionpoint(outline, (point4 - (0,1))---(point4 - (0,100)));
outline = reshape_subpath(outline, point5, point4, nullpath---nullpath);

// Shape the upper right of the top serif.
real t4 = round(intersect(outline, point4, point_fuzz)[0]);
pair point4a = point_at_distance_along_arc(outline, t4 + 1, -50);
pair top_point = (point4a.x + 21, ascender_height);
pair right_point = top_point + (14.5, -14);
outline = reshape_arc(outline, t4 + 1, 55, 60,
                      nullpath..tension 1.2..top_point{right}..right_point{down}..tension 2.0..nullpath);

// Round the sharp corners of the top serif.
outline = smooth_a_corner(outline, point4);
outline = smooth_a_corner(outline, point5);

//-------------------------------------------

outline = smooth_close_points(outline);

pair top_point = point(outline, maxtimes(outline)[1]);
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
