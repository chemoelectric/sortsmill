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

boldness = 1;

stem_width = 68;
ascender_height = 644;
l_stem_left_pos = 36;
l_stem_right_pos = 36;

l_left_stem_counter = new left_stem_counter;
l_left_stem_counter.stem_height = 503;
l_left_stem_counter.top_angle = -32;
l_left_stem_counter.bottom_angle = 9;
l_left_stem_counter.tension1 = 5 * boldness;
l_left_stem_counter.tension2 = 20 * boldness;

l_right_stem_counter = new right_stem_counter;
l_right_stem_counter.stem_height = 525;
l_right_stem_counter.top_angle = 77;
l_right_stem_counter.bottom_angle = 1.4;

l_bottom_serif = new bottom_serif;
l_bottom_serif.lower_left = (-49,-5);
l_bottom_serif.lower_right = (134,3);

l_ascender_serif = new ascender_serif;
l_ascender_serif.angle = 27;

import rmlower;

std_vw = stem_width;
add_to_stemsnap_v(stem_width);
bluefuzz = 0;

usersetting();

//-------------------------------------------------------------------------
