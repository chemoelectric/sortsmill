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
import sortsmill_font;

//-------------------------------------------------------------------------

real interpolate(Font font,
                 real a1 = -1e6, // Value for design size 14pt, boldness 400.
                 real a2 = -1e6, // Value for design size 14pt, boldness 700.
                 real b1 = -1e6, // Value for design size 6pt, boldness 400.
                 real b2 = -1e6, // Value for design size 6pt, boldness 700.
                 real c1 = -1e6, // Reserved for 18pt, boldness 400.
                 real c2 = -1e6, // Reserved for 18pt, boldness 700.
                 real d1 = -1e6) // Value for design size 72pt, boldness 400.
{
    real size = (font.interpolated_size != 0) ? font.interpolated_size : font.design_size;
    real ab1;
    real ab2;
    if (font.design_size < 14.5) {
        ab1 = a1 - (b1 - a1)*(size - 14)/8.0;
        ab2 = a2 - (b2 - a2)*(size - 14)/8.0;
    } else {
        ab1 = a1 + (d1 - a1)*(size - 14)/58.0;
        ab2 = a2;             // No support for larger bold sizes yet.
    }
    return ab1 + (ab2 - ab1)*(font.boldness - 400)/300.0;
}

pair interpolate(Font font,
                 explicit pair a1 = (-1e6,-1e6), // Value for design size 14pt, boldness 400.
                 explicit pair a2 = (-1e6,-1e6), // Value for design size 14pt, boldness 700.
                 explicit pair b1 = (-1e6,-1e6), // Value for design size 6pt, boldness 400.
                 explicit pair b2 = (-1e6,-1e6), // Value for design size 6pt, boldness 700.
                 explicit pair c1 = (-1e6,-1e6), // Reserved for 18pt, boldness 400.
                 explicit pair c2 = (-1e6,-1e6), // Reserved for 18pt, boldness 700.
                 explicit pair d1 = (-1e6,-1e6)) // Value for design size 72pt, boldness 400.
{
    real size = (font.interpolated_size != 0) ? font.interpolated_size : font.design_size;
    pair ab1;
    pair ab2;
    if (font.design_size < 14.5) {
        ab1 = a1 - (b1 - a1)*(size - 14)/8.0;
        ab2 = a2 - (b2 - a2)*(size - 14)/8.0;
    } else {
        ab1 = a1 + (d1 - a1)*(size - 14)/58.0;
        ab2 = a2;             // No support for larger bold sizes yet.
    }
    return ab1 + (ab2 - ab1)*(font.boldness - 400)/300.0;
}

//-------------------------------------------------------------------------

CornerParams my_corner_params(Font font)
{
    return CornerParams(interpolate(font, a1=10, a2=10, b1=20, b2=20, d1=7),
                        interpolate(font, a1=10, a2=10, b1=20, b2=20, d1=7),
                        nullpath..tension 0.75..nullpath);
}

Toolset get_tools(Font font)
{
    Toolset tools = new Toolset;

    tools.font = font;
    tools.space_width = 200;

    tools.letter_e_eye_counter =
        EyeCounter(bounding_box=interpolate(font,a1=(131,102),a2=(131,116),b1=(150,110),b2=(160,175),d1=(131,102)),
                   bottom_angle=14,
                   apex_x=interpolate(font,a1=60,a2=62,b1=70,b2=87,d1=60),
                   left_tensions=nullpath{curl 0}..tension atleast 1.1..nullpath,
                   right_tensions=nullpath..tension atleast 1.0..{curl 0}nullpath);

    tools.letter_e_bowl_counter =
        EyeCounter(bounding_box=interpolate(font,a1=(350,207),a2=(360,203),b1=(450,233),b2=(435,211),d1=(350,207)),
                   bottom_angle=11.5,
                   apex_x=interpolate(font,a1=257,a2=264,b1=331,b2=325,d1=257),
                   left_tensions=nullpath{curl 0}..tension atleast 1.15..nullpath,
                   right_tensions=nullpath..{curl interpolate(font,a1=0.4,a2=0.4,b1=0.6,b2=0.6,d1=0.4)}nullpath);

    tools.letter_e =
        Letter_e(eye_position=interpolate(font,a1=(86,215),a2=(117,215),b1=(127,276),b2=(157,233),d1=(86,215)),
                 bowl_position=interpolate(font,a1=(82,180),a2=(119,178),b1=(124,218),b2=(165,182),d1=(82,180)),
                 left_point=interpolate(font,a1=(0,155),a2=(0,167),b1=(0,200),b2=(0,200),d1=(0,155)),
                 top_point=interpolate(font,a1=(165,358),a2=(190,374),b1=(229,460),b2=(270,458),d1=(165,358)),
                 bottom_point=interpolate(font,a1=(160,-9),a2=(190,-9),b1=(220,-11),b2=(240,-12),d1=(160,-9)),
                 terminal_point=interpolate(font,a1=(315,66),a2=(370,77),b1=(422,83),b2=(480,90),d1=(315,66)),
                 terminal_angle=interpolate(font,a1=120,a2=120,b1=120,b2=120,d1=120),
                 arc_end_x=interpolate(font,a1=302,a2=356,b1=415,b2=470,d1=302),
                 knob_end_x=interpolate(font,a1=310,a2=366,b1=425,b2=484,d1=310),
                 knob_angle=interpolate(font,a1=98,a2=98,b1=98,b2=98,d1=98),
                 flattening_point_relative=interpolate(font,a1=0.4,a2=0.2,b1=0.4,b2=0.4,d1=0.4),
                 flattening_tensions=(nullpath..
                                      tension interpolate(font,a1=1.05,a2=1.08,b1=1.05,b2=1.05,d1=1.05)..
                                      nullpath),
                 lower_right_tensions=(nullpath{curl 0.3}..
                                       tension 1.2 and interpolate(font,a1=0.9,a2=0.9,b1=0.9,b2=0.9,d1=0.9)..
                                       nullpath),
                 lower_left_tensions=nullpath..tension 0.85 and 1.0..nullpath,
                 upper_left_tensions=nullpath..tension 0.95 and 1.0..nullpath,
                 upper_right_tensions=(nullpath..tension interpolate(font,a1=1,a2=0.9,b1=1,b2=1,d1=1)
                                       ..{curl interpolate(font,a1=0.7,a2=1.2,b1=0.7,b2=0.7,d1=0.7)}nullpath),
                 corner_params=my_corner_params(font));

    tools.letter_l_left_counter =
        StemCounter(top_angle=interpolate(font,a1=-32,a2=-36,b1=-42,b2=-42,d1=-33),
                    side_angle=-90,
                    bottom_angle=192,
                    side_height=interpolate(font,a1=499,a2=477,b1=545,b2=556,d1=535),
                    past_top=20,
                    on_side=interpolate(font,a1=435,a2=429,b1=450,b2=471,d1=455),
                    past_bottom=interpolate(font,a1=11,a2=11,b1=14,b2=14,d1=12),
                    top_tensions=(nullpath..tension atleast interpolate(font,a1=0.9,a2=1,b1=0.9,b2=0.9,d1=0.9)..nullpath),
                    bottom_tensions=(nullpath..tension interpolate(font,a1=20,a2=100,b1=20,b2=17,d1=20)..nullpath),
                    top_corner=CornerParams(infinity, infinity, nullpath),
                    bottom_corner=CornerParams(interpolate(font, a1=30, a2=30,b1=30, b2=40, d1=30),
                                               interpolate(font, a1=20, a2=20,b1=40, b2=40, d1=20),
                                               nullpath..
                                               tension interpolate(font, a1=0.75, a2=1,b1=0.75, b2=0.75, d1=0.75)
                                               and interpolate(font, a1=0.9, a2=1,b1=0.9, b2=0.9, d1=0.9)..
                                               nullpath));

    tools.letter_l_left_counter_position =
        interpolate(font,a1=(0,39),a2=(0,42),b1=(0,63),b2=(0,54),d1=(0,29));

    tools.letter_l_right_counter =
        StemCounter(top_angle=interpolate(font,a1=77,a2=77,b1=80,b2=81,d1=79),
                    side_angle=90,
                    bottom_angle=180,
                    side_height=interpolate(font,a1=510,a2=502,b1=510,b2=535,d1=536),
                    past_top=interpolate(font,a1=25,a2=25,b1=25,b2=60,d1=30),
                    on_side=interpolate(font,a1=102,a2=20,b1=120,b2=90,d1=102),
                    past_bottom=interpolate(font,a1=10,a2=5,b1=5,b2=20,d1=6),
                    top_tensions=nullpath..tension 10.0..nullpath,
                    bottom_tensions=(nullpath..
                                     tension atleast interpolate(font,a1=4.5,a2=3.8,b1=4.5,b2=1,d1=4.5)..
                                     nullpath),
                    top_corner=CornerParams(interpolate(font,a1=30,a2=30,b1=50,b2=50,d1=50),
                                            interpolate(font,a1=20,a2=20,b1=50,b2=50,d1=50),
                                            (nullpath..tension interpolate(font,a1=1,a2=1.2,b1=1,b2=1,d1=1)..nullpath)),
                    bottom_corner=CornerParams(32, 40,
                                               (nullpath..
                                                tension interpolate(font,a1=0.8,a2=0.9,b1=0.8,b2=0.8,d1=1.2)
                                                and interpolate(font,a1=0.9,a2=0.9,b1=0.9,b2=0.9,d1=0.75)..
                                                nullpath)));

    tools.letter_l_right_counter_position =
        interpolate(font,a1=(66,37),a2=(87,42),b1=(107,64),b2=(121,55),d1=(54,28));

    tools.letter_l_bottom_serif =
        BottomSerif(left_bottom_point=interpolate(font,a1=(-49,-5),a2=(-54,-5),b1=(-57,-7),b2=(-58,-5),d1=(-52,-5)),
                    right_bottom_point=interpolate(font,a1=(135,3),a2=(148,3),b1=(184,1),b2=(196,5),d1=(121,2)),
                    left_side_angle=interpolate(font,a1=90,a2=90,b1=90,b2=90,d1=83),
                    right_side_angle=interpolate(font,a1=90,a2=90,b1=90,b2=90,d1=85),
                    cup_params=(new pair[] { 0.6*dir(interpolate(font,a1=180.3,a2=180.3,b1=180.3,b2=180.3,d1=180)),
                                0.1*dir(0) }),
                    corner_params=my_corner_params(font));

    tools.letter_l_angle_serif =
        AngleSerif(top_point=interpolate(font,a1=(56,643),a2=(82,643),b1=(80,775),b2=(105,770),d1=(54,664)),
                   top_slope=interpolate(font,a1=27,a2=30,b1=24,b2=28,d1=30),
                   before_top_point=interpolate(font,a1=30,a2=30,b1=30,b2=30,d1=5),
                   after_top_point=interpolate(font,a1=10,a2=8,b1=20,b2=20,d1=7),
                   before_stem_join=interpolate(font,a1=30,a2=30,b1=80,b2=40,d1=10),
                   right_corner_tensions=nullpath..tension 0.88..nullpath,
                   left_corner_params=CornerParams(interpolate(font,a1=35,a2=35,b1=60,b2=35,d1=27),
                                                   interpolate(font,a1=35,a2=35,b1=60,b2=35,d1=27),
                                                   nullpath..tension 0.9..nullpath));
    return tools;
}

//-------------------------------------------------------------------------
