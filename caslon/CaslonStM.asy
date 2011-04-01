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

default_corner_side = 10;
default_corner_guide = nullpath..nullpath;

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
                         Corner top_corner = Corner(before=20, after=20, guide=nullpath..nullpath, postprocess=add_extrema),
                         Corner left_corner = Corner(before=20, after=20, guide=nullpath..nullpath))
{
    return FlagSerifTrim(top_slope=AngledTrim(point=top_point, angle=angle,
                                              guide=(nullpath{dir(angle + relative_angle1)}
                                                     ..{dir(angle + relative_angle2)}top_point),
                                              segment_before=10000, segment_after=0.1),
                         top_corner=top_corner,
                         left_corner=left_corner);
}

//-------------------------------------------------------------------------

pair c_upper_terminal_p1 = (334,339);
pair c_upper_terminal_p1a = (316,374);
pair c_upper_terminal_p2 = (305,305);
pair c_lower_terminal_p1 = (334,69);
pair c_lower_terminal_p2 = (320,78);
real c_lower_terminal_angle1 = 15;
real c_lower_terminal_angle2 = 45;
guide c_outline_lower_right_guide=nullpath{dir(270 - c_lower_terminal_angle1)}..tension 1.1..nullpath;
guide c_outline_lower_left_guide=nullpath..tension 0.95..nullpath;
guide c_outline_upper_left_guide=nullpath..tension 0.95 and 1.0..nullpath;
guide c_outline_upper_right_guide=nullpath..nullpath;
guide c_counter_lower_right_guide=nullpath{dir(270 - c_lower_terminal_angle2)}..nullpath;
guide c_counter_lower_left_guide=nullpath..tension 0.91..nullpath;
guide c_counter_upper_left_guide=nullpath..nullpath;
guide c_counter_upper_right_guide=nullpath..nullpath;

Glyph c_outline = c_outline(upper_terminal_point=c_upper_terminal_p1,
                            upper_terminal_point_a=c_upper_terminal_p1a,
                            top_point=(215,414), left_point=(25,188), bottom_point=(200,-11),
                            lower_terminal_point=c_lower_terminal_p1,
                            lower_right_guide=c_outline_lower_right_guide,
                            lower_left_guide=c_outline_lower_left_guide,
                            upper_left_guide=c_outline_upper_left_guide,
                            upper_right_guide=c_outline_upper_right_guide);
Glyph c_counter = c_counter(upper_terminal_point=c_upper_terminal_p2,
                            top_point=(211,375), left_point=(85,204), bottom_point=(227,41),
                            lower_terminal_point=c_lower_terminal_p2,
                            lower_right_guide=c_counter_lower_right_guide,
                            lower_left_guide=c_counter_lower_left_guide,
                            upper_left_guide=c_counter_upper_left_guide,
                            upper_right_guide=c_counter_upper_right_guide);
TwoPointTrim c_upper_terminal = TwoPointTrim(point1=c_upper_terminal_p1, point2=c_upper_terminal_p2, before=0.1,
                                             guide=nullpath{down}..{left}nullpath);
TwoPointTrim c_lower_terminal = TwoPointTrim(point1=c_lower_terminal_p2, point2=c_lower_terminal_p1,
                                             guide=(nullpath{dir(c_lower_terminal_angle2)}..
                                                    tension 2.0..{dir(270 - c_lower_terminal_angle1)}nullpath));

//.........................................................................

pair e_left_point = (0,205);
pair e_top_point = (181,414);
pair e_bottom_point = (190,-11);
pair e_corner_point = (332,259);
pair e_terminal_point = (354,95);
pair e_lower_counter_left = (0,0);
pair e_lower_counter_right = (343,0);
pair e_lower_counter_bottom = (150,-214);
pair e_upper_counter_position = (77,282);
pair e_lower_counter_position = (60,e_corner_point.y);

Glyph e_outline = e_outline(left_point=e_left_point, top_point=e_top_point, bottom_point=e_bottom_point,
                            corner_point=e_corner_point, terminal_point=e_terminal_point,
                            lower_right_guide = nullpath{curl 0.9}..nullpath,
                            lower_left_guide = nullpath..tension 1.0 and 0.85..nullpath,
                            upper_left_guide = nullpath..nullpath,
                            upper_right_guide = nullpath..{curl 1.4}nullpath);
Glyph e_upper_counter =
    e_upper_counter(apex_point=(100,109), left_point=(0,0), right_point=(177,0),
                    left_guide=nullpath{curl 0.6}::nullpath, right_guide=nullpath..tension 0.92..{curl 0.6}nullpath,
                    left_corner=Corner(before=45, after=30, guide=nullpath..tension atleast 0.75..nullpath),
                    right_corner=Corner(before=70, after=30, guide=nullpath..tension 0.9..nullpath,
                                        postprocess=add_extrema));
Glyph e_lower_counter =
    e_lower_counter(left_point=e_lower_counter_left,
                    right_point=e_lower_counter_right,
                    bottom_point=e_lower_counter_bottom,
                    left_guide=nullpath..{curl 0.6}nullpath,
                    right_guide=nullpath{dir(240)}..
                    controls (e_lower_counter_right - (49,191)) and (e_lower_counter_bottom + (59,0))..nullpath,
                    left_corner=Corner(before=50, after=50, guide=nullpath..nullpath, postprocess=add_extrema));
Corner e_corner = Corner(point=e_corner_point, before=10, after=30, guide=nullpath::nullpath);
AngledTrim e_terminal = AngledTrim(point=e_terminal_point - (0,0.01), angle=145,
                                   guide=nullpath..tension 2.0..nullpath, reversed=true);

//.........................................................................

real i_left_serif_height = 29;
real i_right_serif_height = 29;
pair i_left_origin = (100,0);
pair i_right_origin = i_left_origin + (57,0);
pair i_left_punch_position = i_left_origin + (0,i_left_serif_height);
pair i_right_punch_position = i_right_origin + (0,i_left_serif_height);
pair i_dot_position = i_right_origin + (-42,623);
real i_left_side_height = 301 - i_left_serif_height;
real i_left_side_angle = 90;
real i_right_side_height = 308 - i_right_serif_height;
real i_right_side_angle = 90;
real i_right_top_angle = 86;
Glyph i_left_punch = left_stem_punch(top_angle=145, bottom_angle=180 + lowercase_serif_angle,
                                     side_angle=i_left_side_angle, side_height=i_left_side_height,
                                     top_corner=Corner(25, i_left_side_height - 220 + i_left_serif_height,
                                                       nullpath::nullpath),
                                     bottom_corner=Corner(150 - i_left_serif_height, 35, nullpath::nullpath));
Glyph i_right_punch = right_stem_punch(top_angle=i_right_top_angle, bottom_angle=-lowercase_serif_angle,
                                       side_angle=90, side_height=i_right_side_height,
                                       top_corner=Corner(20, 20, nullpath..nullpath),
                                       bottom_corner=Corner(20, 40, nullpath..tension atleast 1.1..nullpath));
FlagSerifTrim i_flag_serif = flag_serif(top_point=(i_right_punch_position +
                                                   i_right_side_height*dir(i_right_side_angle) +
                                                   107*dir(i_right_top_angle)),
                                        angle=24,
                                        top_corner=Corner(before=20, after=20, nullpath..tension 0.9..nullpath,
                                                          postprocess=add_extrema));
BottomSerifTrim i_bottom_serif = lowercase_baseline_serif(i_left_punch_position, i_right_punch_position, 65, 55);

Glyph i_dot = Glyph(scale(54)*unitcircle);

//.........................................................................

Glyph o_outline = shift(0,-2)*Glyph((185,-10){left}..tension 0.95 and 1.0..
                                    (-1,200){up}..tension 0.85 and 1.3..
                                    (190,409){right}..
                                    (382,200){down}..tension 1.0 and 0.95..cycle);
Glyph o_counter = shift(-8,-3)*Glyph((3,1){left}..tension 1.4 and 0.75..
                                     (-115,185){up}..tension 0.97..
                                     (5,356){right}..
                                     (135,170){down}..tension 0.75 and 1.4..cycle);

//.........................................................................

real r_left_serif_height = 29;
real r_right_serif_height = 29;
pair r_left_origin = (100,0);
pair r_right_origin = r_left_origin + (61,0);
pair r_left_punch_position = r_left_origin + (0,r_left_serif_height);
pair r_right_punch_position = r_right_origin + (0,r_right_serif_height);
pair r_shoulder_punch_position = r_right_origin + (-1,310);
pair r_shoulder_point = (135,89);
pair r_point1 = (74,335 - r_right_serif_height);
pair r_point2 = (162,307 - r_right_serif_height);

Glyph r_shoulder_punch = shoulder_punch(left_angle=90, right_angle=50, shoulder_top=r_shoulder_point,
                                        notch=Corner(10, 10, nullpath..nullpath));
Glyph r_left_punch = left_stem_punch(top_angle=145, bottom_angle=180 + lowercase_serif_angle,
                                     side_angle=90, side_height=309 - r_left_serif_height,
                                     top_corner=Corner(20, 20, nullpath::nullpath),
                                     bottom_corner=Corner(80, 16, nullpath..tension atleast 1.1..nullpath));
Glyph r_right_punch = right_r_punch(bottom_angle=-lowercase_serif_angle, side_angle=90.5,
                                    side_height=260 - r_right_serif_height,
                                    top_guide=nullpath..tension 1.2..r_point1{right}..{right}r_point2,
                                    bottom_corner=Corner(14, 80, nullpath..tension atleast 1.1..nullpath));
path r_terminal = ((r_shoulder_punch_position + r_shoulder_point){right}..
                   (r_shoulder_punch_position + (207,41)){down}..
                   {left}(r_right_punch_position + r_point2));
FlagSerifTrim r_flag_serif = flag_serif(top_point=r_shoulder_punch_position + 112*dir(90), angle=27,
                                        top_corner=Corner(before=20, after=20, guide=nullpath..tension 0.75..nullpath,
                                                          postprocess=add_extrema));
BottomSerifTrim r_bottom_serif = lowercase_baseline_serif(r_left_punch_position, r_right_punch_position, 55, 60);

//.........................................................................

pair t_left_punch_position = (160,-16);
pair t_slope_start = (0,366 - t_left_punch_position.y);
pair t_right_punch_position = (185,26);
pair t_crossbar_top_point = (14,54);
pair t_crossbar_position = (57,322);

Glyph t_left_punch = t_left_punch(side_point1=(-104,363), side_point2=(-110,97),
                                  tail_curve_point=(120,65),
                                  tail_guide=nullpath..tension 0.75 and 3.0..nullpath,
                                  corner=Corner(10, 100, nullpath::nullpath, postprocess=add_extrema));
Glyph t_crossbar_punch = t_crossbar_punch(left_upper=(-170,48), left_lower=(-170,4),
                                          right_upper=t_crossbar_top_point,
                                          top_corner=Corner(t_crossbar_top_point),
                                          bottom_corner=Corner((0,0)));
Glyph t_right_punch = t_right_punch(top_point=(-42,550), tail_point=(88,51),
                                    side_point1=(-70,322), side_point2=(-70,105),
                                    top_guide=nullpath..tension 3.0..nullpath,
                                    tail_left_guide=nullpath..tension 0.9..nullpath,
                                    tail_right_guide=nullpath..tension 1.3 and 1.0..nullpath,
                                    shift(t_crossbar_position)*t_crossbar_punch,
                                    upper_corner=Corner(20, 40, nullpath::nullpath),
                                    lower_corner=Corner(15, 15, nullpath::nullpath));
TwoPointTrim t_left_corner = TwoPointTrim(t_slope_start, t_slope_start, nullpath..nullpath,
                                          nodenum1=-1, nodenum2=0, before=10, after=10);

Pt p1 = t_left_punch.points_at_x(100)[1];
AngledTrim t_tail_end = AngledTrim(point=p1.point + t_left_punch_position, angle=115,
                                   guide=nullpath..tension 2.0..nullpath, reversed=true, after=0.01);

AngledTrim t_top_trim = AngledTrim(point=(113,525), angle=0, guide=nullpath---nullpath,
                                   segment_before=10000, segment_after=10000);
FlagSerifTrim t_sheared_terminal = flag_serif(top_point=(114,525), angle=55,
                                              top_corner=Corner(before=infinity, after=infinity, guide=nullpath));
TwoPointTrim t_top_rounding = TwoPointTrim(point1=(114,525), guide=nullpath..nullpath, before=10, after=12);



//.........................................................................

Toolset tools = Toolset(

    default_corner_side=default_corner_side,
    default_corner_guide=default_corner_guide,
    default_corner=default_corner,

    space_width=200,
    ex_height=403,
    curve_overshoot=10,

    c_outline=c_outline,
    c_counter=c_counter,
    c_upper_terminal=c_upper_terminal,
    c_lower_terminal=c_lower_terminal,

    e_outline=e_outline,
    e_upper_counter=shift(e_upper_counter_position)*e_upper_counter,
    e_lower_counter=shift(e_lower_counter_position)*e_lower_counter,
    e_corner=e_corner,
    e_terminal=e_terminal,

    i_left_punch=shift(i_left_punch_position)*i_left_punch,
    i_right_punch=shift(i_right_punch_position)*i_right_punch,
    i_dot=shift(i_dot_position)*i_dot,
    i_flag_serif=i_flag_serif,
    i_bottom_serif=i_bottom_serif,

    o_outline=o_outline,
    o_counter=shift(188,23)*o_counter,

    r_left_punch=shift(r_left_punch_position)*r_left_punch,
    r_right_punch=shift(r_right_punch_position)*r_right_punch,
    r_shoulder_punch=shift(r_shoulder_punch_position)*r_shoulder_punch,
    r_terminal=r_terminal,
    r_flag_serif=r_flag_serif,
    r_bottom_serif=r_bottom_serif,

    t_left_punch=shift(t_left_punch_position)*t_left_punch,
    t_right_punch=shift(t_right_punch_position)*t_right_punch,
    t_left_corner=t_left_corner,
    t_top_rounding=t_top_rounding,
    t_tail_end=t_tail_end,
    t_top_trim=t_top_trim,
    t_sheared_terminal=t_sheared_terminal

    /* */);

cut_glyphs(tools);

usersetting();

//-------------------------------------------------------------------------
