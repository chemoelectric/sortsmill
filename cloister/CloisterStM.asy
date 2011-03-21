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

font.fontname = 'CloisterStM';
font.familyname = 'Sorts Mill Cloister';
font.fullname = 'Sorts Mill Cloister';
font.weight = 'Regular';
font.sfnt_family = 'Sorts Mill Cloister';
font.sfnt_subfamily = 'Regular';
font.design_size = 14;

//-------------------------------------------------------------------------

Glyph make_letter_l_left_counter()
{
    real side_height = 498;
    Glyph counter;
    counter = left_stem_counter_rough(top_angle=-32, side_angle=-90,
                                      bottom_angle=180 + 12, side_height=side_height);
    Glyph.OutlinePoint p1 = Glyph.OutlinePoint(counter, (0,side_height)) - 20;
    Glyph.OutlinePoint p2 = Glyph.OutlinePoint(counter, (0,0)) + 11;
    counter.splice_in(p1.point{dir(p1)}::
                      {down}(0,0.85*side_height)..tension 20 ..
                      {dir(p2)}p2.point);
    counter.splice_in(corner_shaper(counter, (0,0), 30, 30, nullpath..tension 1.3..nullpath));
    counter = shift(0,40) * counter;
    return counter;
}

Glyph make_letter_l_right_counter()
{
    real side_height = 510;
    Glyph counter;
    counter = right_stem_counter_rough(top_angle=77, side_angle=90,
                                       bottom_angle=180, side_height=side_height);
    Glyph.OutlinePoint p1 = Glyph.OutlinePoint(counter, (0,0)) - 10;
    Glyph.OutlinePoint p2 = Glyph.OutlinePoint(counter, (0,side_height)) + 25;
    counter.splice_in(p1.point{dir(p1)}..tension 4.5 ..
                      {up}(0,0.2*side_height)..tension 10 ..
                      {dir(p2)}p2.point);
    counter.splice_in(corner_shaper(counter, (0,0), 30, 30, nullpath..tension 1 ..nullpath));
    counter.splice_in(corner_shaper(counter, (0,side_height), 20, 50, nullpath::nullpath));
    counter = shift(0,37) * counter;
    return counter;
}

CornerParams my_corner_params = CornerParams(10, 10, nullpath..tension 0.75..nullpath);

Toolset tools = new Toolset;
tools.font = font;
tools.space_width = 200;
tools.letter_l_stem_width = 66;
tools.letter_l_left_counter = make_letter_l_left_counter();
tools.letter_l_right_counter = make_letter_l_right_counter();
tools.letter_l_bottom_serif = BottomSerif(left_bottom_point=(-52,-5),
                                          right_bottom_point=(135,3),
                                          left_side_angle=90,
                                          right_side_angle=90,
                                          cup_params=new pair[] { 0.6*dir(180.3), 0.1*dir(0) },
                                          corner_params=my_corner_params);
tools.letter_l_angle_serif = AngleSerif(top_point=(56,643),
                                        top_slope=27,
                                        before_top_point=30,
                                        after_top_point=10,
                                        before_stem_join=20,
                                        right_corner_tensions=nullpath..tension 0.88..nullpath,
                                        left_corner_params=CornerParams(35, 35, nullpath..tension 0.9..nullpath));

//-------------------------------------------------------------------------

cut_glyphs(font, tools);
usersetting();

//-------------------------------------------------------------------------
