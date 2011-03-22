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

import cloister_basics;
import cloister_glyphs;
import cloister_version;
import sortsmill_font;

//-------------------------------------------------------------------------

font.version = version_string;

font.fontname = 'CloisterStM-Bold';
font.familyname = 'Sorts Mill Cloister';
font.fullname = 'Sorts Mill Cloister Bold';
font.weight = 'Bold';
font.sfnt_family = 'Sorts Mill Cloister';
font.sfnt_subfamily = 'Bold';
font.design_size = 14;

//-------------------------------------------------------------------------

CornerParams my_corner_params = CornerParams(10, 10, nullpath..tension 0.75..nullpath);

Toolset tools = new Toolset;
tools.font = font;
tools.space_width = 200;
tools.letter_l_stem_width = 87;
tools.letter_l_left_counter = StemCounter(vert_offset=42,
                                          top_angle=-36,
                                          side_angle=-90,
                                          bottom_angle=192,
                                          side_height=477,
                                          past_top=20,
                                          on_side=0.9 * 477,
                                          past_bottom=11,
                                          top_tensions=nullpath::nullpath,
                                          bottom_tensions=nullpath..tension 100.0..nullpath,
                                          top_corner=CornerParams(infinity, infinity, nullpath),
                                          bottom_corner=CornerParams(30, 30, nullpath..tension 1.3..nullpath));
tools.letter_l_right_counter = StemCounter(vert_offset=42,
                                           top_angle=77,
                                           side_angle=90,
                                           bottom_angle=180,
                                           side_height=502,
                                           past_top=25,
                                           on_side=0.1 * 510,
                                           past_bottom=5,
                                           top_tensions=nullpath..tension 10.0..nullpath,
                                           bottom_tensions=nullpath..tension 4.5..nullpath,
                                           top_corner=CornerParams(20, 50, nullpath::nullpath),
                                           bottom_corner=CornerParams(35, 35, nullpath..tension 0.75..nullpath));
tools.letter_l_bottom_serif = BottomSerif(left_bottom_point=(-54,-5),
                                          right_bottom_point=(148,3),
                                          left_side_angle=90,
                                          right_side_angle=90,
                                          cup_params=new pair[] { 0.6*dir(180.3), 0.1*dir(0) },
                                          corner_params=my_corner_params);
tools.letter_l_angle_serif = AngleSerif(top_point=(82,643),
                                        top_slope=30,
                                        before_top_point=30,
                                        after_top_point=8,
                                        before_stem_join=20,
                                        right_corner_tensions=nullpath..tension 0.88..nullpath,
                                        left_corner_params=CornerParams(35, 35, nullpath..tension 0.9..nullpath));

//-------------------------------------------------------------------------

cut_glyphs(font, tools);
usersetting();

//-------------------------------------------------------------------------
