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

stem_width = 92;
ascender_height = 644;
l_stem_left_pos = 38;
l_stem_right_pos = 40;

l_bottom_serif = new bottom_serif;
l_bottom_serif.lower_left = (-52,-7);
l_bottom_serif.lower_right = (149,3);

l_top_serif_angle1 = 30;
l_top_left_counter_angle = -40;

import rmlower;

std_vw = stem_width;
add_to_stemsnap_v(stem_width);
bluefuzz = 0;

usersetting();

//-------------------------------------------------------------------------
