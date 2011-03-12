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

round_points = true;
simplify_slightly = true;

//-------------------------------------------------------------------------

///////////////real asc_height = 645;
///////////////pair tiptop = (150, asc_height);
///////////////pair top_point = tiptop - (20, 0);
///////////////real top_angle = 30;

real stem_width = 69;

//-------------------------------------------------------------------------
//
// Lowercase 'l'.

real stem_left_height = 504;
real stem_right_height = 521;
real stem_left_pos = 35;
real stem_right_pos = 36;

// Create a rectangular blank.
real bignum = 1000;
real blank_width = 200 + stem_width;
path outline = shift(stem_width/2,0) *
    ((-blank_width/2,-bignum)---(-blank_width/2,bignum)---(blank_width/2,bignum)---(blank_width/2,-bignum)---cycle);

// Form the stem by "punching" a counter on either side.
path left_counter = left_stem_counter(stem_left_pos, stem_left_height, 0.9 * stem_left_height,
                                      -32, 7, -12, 30, 20, 30, 1, 20, 0.9, 0.9, 1);
path right_counter = right_stem_counter(stem_right_pos, stem_right_height, 77, 1.4);
outline = chop(outline, left_counter);
outline = chop(outline, shift(stem_width,0) * right_counter);

outline = chop(outline, (0.8 * stem_width,0), 180 + 2.5);

path cut_line = ((-49,100) + dir(-90)*1000)---(-49,100);
pair[] points1 = intersectionpoints(cut_line, outline);
outline = reshape_subpath(outline, cut_line, nullpath---nullpath);
path cut_line = (stem_width + 66,100)---((stem_width + 66,100) + dir(-90)*1000);
pair[] points2 = intersectionpoints(cut_line, outline);
outline = reshape_subpath(outline, cut_line, nullpath---nullpath);

outline = reshape_subpath(outline, points2[1], points1[0],
                          nullpath{left} ..
                          controls points2[1] + 0.7*(points2[1].x - points1[0].x)*dir(181)
                          and points1[0] + 0.4 * (points2[1] - points1[0]) ..
                          nullpath);

// Round the corners of the bottom serif.
outline = reshape_arc(outline, points1[0], 10, nullpath..tension 0.85 ..nullpath);
outline = reshape_arc(outline, points1[1], 10, nullpath..tension 0.85 ..nullpath);
outline = reshape_arc(outline, points2[0], 10, nullpath..tension 0.85 ..nullpath);
outline = reshape_arc(outline, points2[1], 10, nullpath..tension 0.85 ..nullpath);

glyph_data glyph;
glyph.name = 'l';
glyph.contours[0] = outline;
set_glyph(glyph);

//-------------------------------------------------------------------------

void write_python(string sfd_file = 'CloisterStM.sfd')
{
    write('#!/usr/bin/env python

import fontforge
import psMat
import sortsmill

from sortsmill.glyphbuild import preferred_unicode
from sortsmill import *

f = fontforge.font()

f.version = \'' + version + '\'
f.copyright = \'' + copyright_notice + '\'
f.fontname = \'CloisterStM\'
f.familyname = \'CloisterStM\'
f.fullname = \'CloisterStM\'
f.weight = \'Regular\'

lang = \'English (US)\'
f.appendSFNTName(lang, \'Family\', \'CloisterStM\')
f.appendSFNTName(lang, \'SubFamily\', \'Regular\')
f.appendSFNTName(lang, \'License\', \'' + copyright_notice + '\\n\\n' + license_notice + '\');
f.appendSFNTName(lang, \'License URL\', \'' + license_url + '\');
f.appendSFNTName(lang, \'Vendor URL\', \'' + vendor_url + '\');
f.appendSFNTName(lang, \'Designer\', \'' + designer_name + '\');
f.appendSFNTName(lang, \'Designer URL\', \'' + designer_url + '\');
');

    write_fontforge_glyph_list_code('contour');
    
    write('
f.encoding = \'UnicodeBMP\'
f.save(\'' + sfd_file + '\')
');
}

//-------------------------------------------------------------------------

usersetting();

//-------------------------------------------------------------------------
