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

//-------------------------------------------------------------------------

real lowercase_serif_below_baseline = -2;
real lowercase_serif_angle = 2;

BottomSerifTrim lowercase_bottom_serif(pair left_point,
                                       pair right_point,
                                       pair between_point,
                                       real serif_angle = lowercase_serif_angle,
                                       real segment_length = 10)
{
    return BottomSerifTrim(left_end=AngledTrim(point=left_point, angle=90, endpoint=left_point,
                                               guide=nullpath{left}..tension 2.5..{dir(lowercase_serif_angle)}nullpath,
                                               segment_before=-0.1),
                           right_end=AngledTrim(point=right_point, angle=90, endpoint=right_point,
                                                guide=nullpath{dir(-lowercase_serif_angle)}..tension 2.5..{left}nullpath,
                                                reversed=true, segment_before=-0.1),
                           bottom_guide=(right_point{left}..tension 0.75 and 1.5..
                                         (between_point + (0.5*segment_length,0))---
                                         (between_point - (0.5*segment_length,0))..
                                         tension 1.5 and 0.75..{left}left_point));
}

BottomSerifTrim lowercase_baseline_serif(pair stem_left_position,
                                         pair stem_right_position,
                                         real left_length,
                                         real right_length,
                                         real segment_length = 10)
{
    return lowercase_bottom_serif(left_point=(stem_left_position.x - left_length, lowercase_serif_below_baseline),
                                  right_point=(stem_right_position.x + right_length, lowercase_serif_below_baseline),
                                  between_point=(0.5*(stem_left_position.x + stem_right_position.x), 0));
}

FlagSerifTrim flag_serif(pair top_point,
                         real angle,
                         real relative_angle1 = -5,
                         real relative_angle2 = 5,
                         Corner top_corner = Corner(before=20, after=20, guide=nullpath..tension 0.75..nullpath,
                                                    postprocess=add_extrema),
                         Corner left_corner = Corner(before=20, after=20, guide=nullpath..nullpath,
                                                     postprocess=add_extrema))
{
    return FlagSerifTrim(top_slope=AngledTrim(point=top_point, angle=angle,
                                              guide=(nullpath{dir(angle + relative_angle1)}
                                                     ..{dir(angle + relative_angle2)}top_point),
                                              segment_before=10000, segment_after=0.1),
                         top_corner=top_corner,
                         left_corner=left_corner);
}

//-------------------------------------------------------------------------

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
AngledTrim c_lower_terminal = AngledTrim(point=p2.point, angle=130,
                                         guide=nullpath..tension 2.0..nullpath, reversed = true);

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

AngledTrim e_terminal = AngledTrim(point=e_terminal_point - (0,0.01), angle=145,
                                   guide=nullpath..tension 2.0..nullpath, reversed=true);

//.........................................................................

pair i_left_punch_position = (100,30);
pair i_right_punch_position = i_left_punch_position + (58,0);
real i_left_side_height = 271;
real i_left_side_angle = 90;
real i_right_side_height = 278;
real i_right_side_angle = 90;
real i_right_top_angle = 87;
Glyph i_left_punch = left_stem_punch(top_angle=145, bottom_angle=180 + lowercase_serif_angle,
                                     side_angle=i_left_side_angle, side_height=i_left_side_height,
                                     top_corner=Corner(30, 50, nullpath::nullpath),
                                     bottom_corner=Corner(70, 30, nullpath..tension atleast 1.1..nullpath));
Glyph i_right_punch = right_stem_punch(top_angle=i_right_top_angle, bottom_angle=-lowercase_serif_angle,
                                       side_angle=90, side_height=i_right_side_height,
                                       top_corner=Corner(20, 20, nullpath..nullpath),
                                       bottom_corner=Corner(16, 80, nullpath..tension atleast 1.1..nullpath));
FlagSerifTrim i_flag_serif = flag_serif(top_point=(i_right_punch_position +
                                                   i_right_side_height*dir(i_right_side_angle) +
                                                   107*dir(i_right_top_angle)),
                                        angle=24);
BottomSerifTrim i_bottom_serif = lowercase_baseline_serif(i_left_punch_position, i_right_punch_position, 60, 55);

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

real r_left_serif_height = 29;
real r_right_serif_height = 29;
pair r_left_origin = (100,0);
pair r_right_origin = r_left_origin + (59,0);
pair r_left_punch_position = r_left_origin + (0,r_left_serif_height);
pair r_right_punch_position = r_right_origin + (0,r_right_serif_height);
pair r_shoulder_punch_position = r_right_punch_position + (-2,280);
pair r_shoulder_point = (142,91);
pair r_point1 = (78,306);
pair r_point2 = (162,278);

Glyph r_shoulder_punch = shoulder_punch(left_angle=90, right_angle=50, shoulder_top=r_shoulder_point,
                                        notch=Corner(10, 10, nullpath..nullpath));
Glyph r_left_punch = left_stem_punch(top_angle=145, bottom_angle=180 + lowercase_serif_angle,
                                     side_angle=90, side_height=280,
                                     top_corner=Corner(20, 20, nullpath::nullpath),
                                     bottom_corner=Corner(80, 16, nullpath..tension atleast 1.1..nullpath));
Glyph r_right_punch = right_r_punch(bottom_angle=-lowercase_serif_angle, side_angle=90, side_height=233,
                                    top_guide=nullpath..tension 1.1..r_point1{right}..{right}r_point2,
                                    bottom_corner=Corner(16, 80, nullpath..tension atleast 1.1..nullpath));
path r_terminal = ((r_shoulder_punch_position + r_shoulder_point){right}..
                   (r_shoulder_punch_position + (208,41)){down}..
                   {left}(r_right_punch_position + r_point2));
FlagSerifTrim r_flag_serif = flag_serif(top_point=r_shoulder_punch_position + 112*dir(90), angle=27);
BottomSerifTrim r_bottom_serif = lowercase_baseline_serif(r_left_punch_position, r_right_punch_position, 55, 60);

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
AngledTrim t_terminal = AngledTrim(point=p1.point + t_left_punch_position, angle=120,
                                   guide=nullpath..tension 2.0..nullpath,
                                   reversed=true, after=0.01);

//.........................................................................

Toolset tools = Toolset(

    default_corner_side=default_corner_side,
    default_corner_guide=default_corner_guide,
    default_corner=default_corner,

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

    i_left_punch_position=i_left_punch_position,
    i_right_punch_position=i_right_punch_position,
    i_left_punch=i_left_punch,
    i_right_punch=i_right_punch,
    i_flag_serif=i_flag_serif,
    i_bottom_serif=i_bottom_serif,

    o_outline=o_outline,
    o_counter=shift(188,23)*o_counter,

    r_left_punch_position=r_left_punch_position,
    r_right_punch_position=r_right_punch_position,
    r_shoulder_punch_position=r_shoulder_punch_position,
    r_shoulder_punch=r_shoulder_punch,
    r_left_punch=r_left_punch,
    r_right_punch=r_right_punch,
    r_terminal=r_terminal,
    r_flag_serif=r_flag_serif,
    r_bottom_serif=r_bottom_serif,

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
