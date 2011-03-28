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

import caslon_basics;
import caslon_font;
import caslon_roman;
import caslon_output;
import caslon_version;

//-------------------------------------------------------------------------

font.version = version_string;

font.fontname = 'CaslonStM';
font.familyname = 'Sorts Mill Caslon';
font.fullname = 'Sorts Mill Caslon';
font.weight = 'Regular';
font.sfnt_family = 'Sorts Mill Caslon';
font.sfnt_subfamily = 'Regular';
font.design_size = 12;
font.boldness = 400;

//.........................................................................

pair c_upper_terminal_point = (78,266);
pair c_counter_position = (202,40);

Glyph c_outline = Glyph((172,-12){left}..tension 0.95 and 1.0..
                        (0,189){up}..(192,415){right}..tension 0.97..
                        (357,199){down}..tension 1.0 and 0.95..cycle);
Glyph c_counter = Glyph((0,0){left}..(-141,159){up}..(-19,336){right}..
                        c_upper_terminal_point{right}..(143,159){down}..cycle);

Pt p1 = c_outline.points_at_y(375)[1];
Pt p2 = c_outline.points_at_y(59)[1];

TwoPointTrim c_upper_terminal = TwoPointTrim(p1.point, shift(c_counter_position)*c_upper_terminal_point,
                                             nullpath..tension 0.95..nullpath);
AngledTrim c_lower_terminal = AngledTrim(p2.point, dir(130), 1, 1000,
                                         nullpath..tension 2.0..nullpath, reversed = true);

//.........................................................................

pair e_terminal_point = (349,97);
pair e_corner_point = (332,260);

Glyph e_outline = Glyph(e_terminal_point{curl 0.9}::
                        (186,-9){left}..tension 1.0 and 0.85..
                        (-2,200){up}..
                        {right}(176,413)..tension 0.95..
                        {down}e_corner_point..tension atleast 0.75..
                        {right}(e_terminal_point.x,220)--cycle);

Glyph e_top_counter = Glyph((0,0){curl 0.6}::{right}(100,109)..tension 0.92..{curl 0.6}(184,2)--cycle);
e_top_counter.splice_in(add_extrema(corner((e_top_counter@(0,0))^0, 25, 50, nullpath..tension 0.75..nullpath)));
e_top_counter.splice_in(add_extrema(corner((e_top_counter@(190,2))^0, 70, 35, nullpath..tension 0.9..nullpath)));

Glyph e_bowl = Glyph((150,-209){left}::(-4,-45){up}::{curl 1}(0,0)--(300,2){curl 1}::cycle);
e_bowl.splice_in(add_extrema(corner((e_bowl@(0,0))^0, 40, 40, nullpath..tension 1.0 and 2.0..nullpath)));

Corner e_corner = Corner(e_corner_point, 20, 20, nullpath..tension 1.1..nullpath);

AngledTrim e_terminal = AngledTrim(e_terminal_point - (0,0.01), dir(145), 1, 1000,
                                   nullpath..tension 2.0..nullpath, reversed=true);

//.........................................................................

Glyph o_outline = Glyph((185,-10){left}..tension 0.95 and 1.0..
                        (0,200){up}..tension 0.85 and 1.3..
                        (190,409){right}..
                        (384,200){down}..tension 1.0 and 0.95..cycle);
Glyph o_counter = shift(-8,-1)*Glyph((3,1){left}..tension 1.4 and 0.75..
                                     (-114,185){up}..tension 0.97..
                                     (5,356){right}..
                                     (134,170){down}..tension 0.75 and 1.4..cycle);

//.........................................................................

real t_approx_width = 262;
pair t_top_slope_point = (0,366);
pair t_left_punch_position = (160,-17);
pair t_left_punch_right1 = (-104,363);
pair t_left_punch_right2 = (-110,100);
pair t_right_punch_position = (185,25);
pair t_top_point = (116,525);

Glyph t_outline = Glyph((0,t_left_punch_position.y)--t_top_slope_point{dir(43)}..{dir(58)}t_top_point--
                        (t_approx_width,t_top_point.y)--(t_approx_width,t_left_punch_position.y)--cycle);

Glyph t_left_punch = Glyph((0,0)..(120,72)--(120,-1)--(-500, -1)--(-500,t_left_punch_right1.y)--
                           t_left_punch_right1{down}..tension atleast 0.75..{down}t_left_punch_right2::{right}cycle);
t_left_punch.splice_in(corner(t_left_punch@t_left_punch_right1, 13, 50, nullpath::nullpath));

Glyph t_crossbar_punch = Glyph((-170,48)--(14,54)--(0,0)--(-170,4)--cycle);
t_crossbar_punch.splice_in(default_corner(t_crossbar_punch@(0,0)));
t_crossbar_punch.splice_in(default_corner(t_crossbar_punch@(14,53)));
Glyph t_right_punch = Glyph((0,0){left}..(-70,101)---(-70,322)..tension 3.0..(-42,550)--
                            (1000,550)---(1000,51)--(88,51)..tension 0.9 and 1.0..cycle);
t_right_punch.punch(shift(57,322)*t_crossbar_punch);
t_right_punch.splice_in(corner((t_right_punch@(-68,322))^1, 15, 15, nullpath::nullpath));
t_right_punch.splice_in(add_extrema(corner((t_right_punch@(-42,550))^-1, 20, 40,
                                           nullpath..tension 0.75 and 10.0..nullpath)));

TwoPointTrim t_left_corner =
    TwoPointTrim(t_top_slope_point, t_top_slope_point, nullpath..nullpath,
                 nodenum1=-1, nodenum2=0, before=10, after=10);

TwoPointTrim t_top_corner =
    TwoPointTrim(t_top_point, t_top_point, nullpath..nullpath,
                 nodenum1=0, nodenum2=1, before=10, after=15);

Pt p1 = t_left_punch.points_at_x(t_approx_width - t_left_punch_position.x)[1];
AngledTrim t_terminal = AngledTrim(p1.point + t_left_punch_position, dir(120), 1, 1000,
                                   nullpath..tension 2.0..nullpath, reversed=true, after=0.01);

//.........................................................................

Toolset tools = Toolset(

    default_corner_side = default_corner_side,
    default_corner_guide = default_corner_guide,
    default_corner = default_corner,

    space_width=200,
    ex_height=403,
    curve_overshoot=10,

    c_outline=c_outline,
    c_counter=shift(c_counter_position)*c_counter,
    c_upper_terminal=c_upper_terminal,
    c_lower_terminal=c_lower_terminal,

    e_outline=e_outline,
    e_top_counter=shift(73,280)*e_top_counter,
    e_bowl=shift(56,256)*e_bowl,
    e_corner=e_corner,
    e_terminal=e_terminal,

    o_outline=o_outline,
    o_counter=shift(188,23)*o_counter,

    t_outline=t_outline,
    t_left_punch=shift(t_left_punch_position)*t_left_punch,
    t_right_punch=shift(t_right_punch_position)*t_right_punch,
    t_left_corner=t_left_corner,
    t_top_corner=t_top_corner,
    t_terminal=t_terminal

    /* */);

cut_glyphs(tools);

usersetting();

//-------------------------------------------------------------------------
