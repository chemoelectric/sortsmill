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
                 real a1, // Value for design size 14pt, boldness 400.
                 real a2, // Value for design size 14pt, boldness 700.
                 real b1, // Value for design size 6pt, boldness 400.
                 real b2) // Value for design size 6pt, boldness 700.
{
    real size = (font.interpolated_size != 0) ? font.interpolated_size : font.design_size;
    real ab1 = a1 - (b1 - a1)*(size - 14)/8.0;
    real ab2 = a2 - (b2 - a2)*(size - 14)/8.0;
    return ab1 + (ab2 - ab1)*(font.boldness - 400)/300.0;
}

pair interpolate(Font font,
                 explicit pair a1, // Value for design size 14pt, boldness 400.
                 explicit pair a2, // Value for design size 14pt, boldness 700.
                 explicit pair b1, // Value for design size 6pt, boldness 400.
                 explicit pair b2) // Value for design size 6pt, boldness 700.
{
    real size = (font.interpolated_size != 0) ? font.interpolated_size : font.design_size;
    pair ab1 = a1 - (b1 - a1)*(size - 14)/8.0;
    pair ab2 = a2 - (b2 - a2)*(size - 14)/8.0;
    return ab1 + (ab2 - ab1)*(font.boldness - 400)/300.0;
}

//-------------------------------------------------------------------------

CornerParams my_corner_params(Font font)
{
    return CornerParams(interpolate(font, 10, 10, 20, 20),
                        interpolate(font, 10, 10, 20, 20),
                        nullpath..tension 0.75..nullpath);
}

Toolset get_tools(Font font)
{
    Toolset tools = new Toolset;
    tools.font = font;
    tools.space_width = 200;
    tools.letter_e_eye_counter = EyeCounter(bounding_box=interpolate(font, (131,102), (131,116), (150,110), (160,175)),
                                            bottom_angle=14,
                                            apex_x=interpolate(font, 60, 62, 70, 95),
                                            left_tensions=nullpath{curl 0}..tension atleast 1.1..nullpath,
                                            right_tensions=nullpath..tension atleast 1.0..{curl 0}nullpath);
    tools.letter_e_bowl_counter = EyeCounter(bounding_box=interpolate(font, (350,207), (360,203), (450,233), (435,211)),
                                             bottom_angle=11.5,
                                             apex_x=interpolate(font, 257, 264, 331, 325),
                                             left_tensions=nullpath{curl 0}..tension atleast 1.15..nullpath,
                                             right_tensions=nullpath..{curl interpolate(font, 0.4, 0.4, 0.6, 0.6)}nullpath);
    tools.letter_e = Letter_e(eye_position=interpolate(font, (86,215), (117,215), (127,276), (157,233)),
                              bowl_position=interpolate(font, (82,180), (119,178), (124,218), (165,182)),
                              left_point=interpolate(font, (0,155), (0,167), (0,200), (0,200)),
                              top_point=interpolate(font, (165,358), (190,374), (229,460), (270,458)),
                              bottom_point=interpolate(font, (160,-9), (190,-9), (220,-11), (240,-12)),
                              terminal_point=interpolate(font, (315,66), (370,77), (422,83), (480,90)),
                              terminal_angle=interpolate(font, 120, 120, 120, 120),
                              arc_end_x=interpolate(font, 302, 356, 415, 470),
                              knob_end_x=interpolate(font, 310, 366, 425, 484),
                              knob_angle=interpolate(font, 98, 98, 98, 98),
                              flattening_point_relative=interpolate(font, 0.4, 0.2, 0.4, 0.4),
                              flattening_tensions=(nullpath..tension interpolate(font, 1.05, 1.08, 1.05, 1.05)..nullpath),
                              lower_right_tensions=(nullpath{curl 0.3}..
                                                    tension 1.2 and interpolate(font, 0.9, 0.9, 0.9, 0.9)..
                                                    nullpath),
                              lower_left_tensions=nullpath..tension 0.85 and 1.0..nullpath,
                              upper_left_tensions=nullpath..tension 0.95 and 1.0..nullpath,
                              upper_right_tensions=(nullpath..tension interpolate(font, 1, 0.9, 1, 1)
                                                    ..{curl interpolate(font, 0.7, 1.2, 0.7, 0.7)}nullpath),
                              corner_params=my_corner_params(font));
    tools.letter_l_left_counter = StemCounter(top_angle=interpolate(font, -32, -36, -42, -42),
                                              side_angle=-90,
                                              bottom_angle=192,
                                              side_height=interpolate(font, 499, 477, 550, 556),
                                              past_top=20,
                                              on_side=interpolate(font, 435, 429, 450, 471),
                                              past_bottom=interpolate(font, 11, 11, 14, 14),
                                              top_tensions=(nullpath..
                                                            tension atleast interpolate(font, 0.9, 1, 0.9, 0.9)..
                                                            nullpath),
                                              bottom_tensions=(nullpath..
                                                               tension interpolate(font, 20, 100, 20, 17)..
                                                               nullpath),
                                              top_corner=CornerParams(infinity, infinity, nullpath),
                                              bottom_corner=CornerParams(interpolate(font, 30, 30, 30, 40),
                                                                         interpolate(font, 20, 20, 40, 40),
                                                                         nullpath..
                                                                         tension interpolate(font, 0.75, 1, 0.75, 0.75)
                                                                         and interpolate(font, 0.9, 1, 0.9, 0.9)..
                                                                         nullpath));
    tools.letter_l_left_counter_position = interpolate(font, (0,39), (0,42), (0,63), (0,54));
    tools.letter_l_right_counter = StemCounter(top_angle=interpolate(font, 77, 77, 80, 81),
                                               side_angle=90,
                                               bottom_angle=180,
                                               side_height=interpolate(font, 510, 502, 510, 535),
                                               past_top=interpolate(font, 25, 25, 25, 60),
                                               on_side=interpolate(font, 102, 20, 120, 90),
                                               past_bottom=interpolate(font, 10, 5, 5, 20),
                                               top_tensions=nullpath..tension 10.0..nullpath,
                                               bottom_tensions=(nullpath..
                                                                tension atleast interpolate(font, 4.5, 3.8, 4.5, 1)..
                                                                nullpath),
                                               top_corner=CornerParams(interpolate(font, 30, 30, 50, 50),
                                                                       interpolate(font, 20, 20, 50, 50),
                                                                       (nullpath..
                                                                        tension interpolate(font, 1, 1.2, 1, 1)..
                                                                        nullpath)),
                                               bottom_corner=CornerParams(32, 40,
                                                                          (nullpath..
                                                                           tension interpolate(font, 0.8, 0.9, 0.8, 0.8)
                                                                           and 0.9..
                                                                           nullpath)));
    tools.letter_l_right_counter_position = interpolate(font, (66,37), (87,42), (107,64), (121,55));
    tools.letter_l_bottom_serif = BottomSerif(left_bottom_point=interpolate(font, (-49,-5), (-54,-5), (-57,-7), (-58,-5)),
                                              right_bottom_point=interpolate(font, (135,3), (148,3), (184,1), (196,5)),
                                              left_side_angle=90,
                                              right_side_angle=90,
                                              cup_params=new pair[] { 0.6*dir(180.3), 0.1*dir(0) },
                                              corner_params=my_corner_params(font));
    tools.letter_l_angle_serif = AngleSerif(top_point=interpolate(font, (56,643), (82,643), (80,775), (105,770)),
                                            top_slope=interpolate(font, 27, 30, 24, 28),
                                            before_top_point=interpolate(font, 30, 30, 30, 30),
                                            after_top_point=interpolate(font, 10, 8, 20, 20),
                                            before_stem_join=interpolate(font, 30, 30, 80, 40),
                                            right_corner_tensions=nullpath..tension 0.88..nullpath,
                                            left_corner_params=CornerParams(interpolate(font, 35, 35, 60, 35),
                                                                            interpolate(font, 35, 35, 60, 35),
                                                                            nullpath..tension 0.9..nullpath));
    return tools;
}

//-------------------------------------------------------------------------
