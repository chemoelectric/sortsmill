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

outline = form_ascender_serif(outline, l_ascender_serif_params);
outline = form_bottom_serif(outline, l_bottom_serif_params);
outline = smooth_close_points(outline);

pair top_point = point(outline, maxtimes(outline)[1]);
pair bottom_point = point(outline, mintimes(outline)[1]);

glyph_data glyph;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
glyph.lsb = l_bottom_serif_params.lower_left.x;
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
glyph.name = 'l';
add_vert_hint(glyph, 0, stem_width);
add_horiz_hint(glyph, top_point.y, -20);
add_horiz_hint(glyph, bottom_point.y + 21, -21);
glyph.contours[0] = outline;
set_glyph(glyph);

//=========================================================================
