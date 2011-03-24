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
import sortsmill_font;
import sortsmill_glyph;

//-------------------------------------------------------------------------

Glyph make_space(Font font, Toolset tools)
{
    Glyph glyph;
    glyph.lsb = 0;
    glyph.rsb = tools.space_width;
    glyph.name = 'space';
    return glyph;
}

Glyph make_letter_e(Font font, Toolset tools)
{
    Glyph.OutlinePoint p1;
    Glyph.OutlinePoint p2;
    Letter_e le = tools.letter_e;
    pair eye_position = le.eye_position;
    pair bowl_position = le.bowl_position;
    pair top_point = le.top_point;
    pair left_point = le.left_point;
    pair bottom_point = le.bottom_point;
    pair terminal_point = le.terminal_point;
    real terminal_angle = le.terminal_angle;
    real arc_end_x = le.arc_end_x;
    real knob_end_x = le.knob_end_x;
    real knob_angle = le.knob_angle;
    real flattening_point_relative = le.flattening_point_relative;
    guide lower_right_tensions = le.lower_right_tensions;
    guide lower_left_tensions = le.lower_left_tensions;
    guide upper_left_tensions = le.upper_left_tensions;
    guide upper_right_tensions = le.upper_right_tensions;
    guide flattening_tensions = le.flattening_tensions;
    CornerParams corner_params = le.corner_params;
    EyeCounter eye_counter_params = tools.letter_e_eye_counter;
    EyeCounter bowl_counter_params = tools.letter_e_bowl_counter;

    real blank_width = terminal_point.x + 50;
    Glyph glyph = Glyph((0,bottom_point.y), (blank_width,top_point.y), name='e');

    // Cut the outline.
    glyph.splice_in((terminal_point + (50,0))---
                    terminal_point..lower_right_tensions..
                    bottom_point{left}..lower_left_tensions..
                    {up}left_point..upper_left_tensions..
                    {right}top_point);

    // Flatten the upper left.
    Glyph.OutlinePoint left = Glyph.OutlinePoint(glyph, left_point)^0;
    Glyph.OutlinePoint top = Glyph.OutlinePoint(glyph, top_point)^0;
    p1 = left + flattening_point_relative*(top - left);
    glyph.splice_in(p1.point{dir(p1)}..flattening_tensions..{dir(top)}top.point);

    // Punch the "eye".
    glyph.apply_punch(shift(eye_position) * eye_counter(eye_counter_params));

    // Punch the bowl. This is an inverted "eye" counter.
    real crossbar_slope = Tan(bowl_counter_params.bottom_angle);
    pair crossbar_vector = (bowl_counter_params.bounding_box.x,
                            bowl_counter_params.bounding_box.x * crossbar_slope);
    Glyph bowl = eye_counter(bowl_counter_params);
    bowl = rotate(180) * bowl;
    bowl = shift(bowl_position + crossbar_vector) * bowl;
    bowl.add_extrema();
    glyph.apply_punch(bowl);

    // Cut the terminal.
    pair vector = 300*dir(terminal_angle);
    p1 = glyph.intersections((terminal_point + 0.1*vector)---(terminal_point + 500*vector))[0];
    glyph.splice_in(p1.point---terminal_point);
    glyph.splice_in(corner_shaper(glyph, terminal_point, corner_params /*, add_extrema=true*/));
    glyph.splice_in(corner_shaper(glyph, p1.point, corner_params));

    // Finish the outline, except for the "knob".
    Glyph.OutlinePoint[] points = glyph.points_at_x(arc_end_x);
    Glyph.OutlinePoint hypothetical_arc_end = points[points.length - 2];
    top.refresh();
    path arc = top.point{right}..upper_right_tensions..hypothetical_arc_end.point;
    path ray = eye_position---(eye_position + 1000*dir(bowl_counter_params.bottom_angle));
    path arc = subpath(arc, 0, intersect(arc, ray)[0]);
    pair arc_end = point(arc, 1);
    ray = arc_end---(arc_end + 1000*dir(bowl_counter_params.bottom_angle));
    p1 = glyph.intersections(ray)[0];
    glyph.splice_in(arc & arc_end---p1.point);

    // Cut the "knob".
    points = glyph.points_at_x(knob_end_x);
    p1 = points[points.length - 2];
    vector = 300*dir(knob_angle);
    p2 = glyph.intersections((p1.point + 0.1*vector)---(p1.point + 500*vector))[0];
    glyph.splice_in(p2.point---p1.point);
    p1.refresh();
    glyph.splice_in(corner_shaper(glyph, p1.point, corner_params /*, add_extrema=true*/));
    p2.refresh();
    real d = corner_params.after;
    glyph.splice_in((p2^-1).point{dir(bowl_counter_params.bottom_angle)}..corner_params.tensions..
                    {-dir(knob_angle)}(p2 + d).point);

    return glyph;
}

Glyph make_letter_l(Font font, Toolset tools)
{
    Glyph glyph = Glyph((-200,-200), (200,1200), name='l');
    glyph.apply_punch(shift(tools.letter_l_left_counter_position) * left_stem_counter(tools.letter_l_left_counter));
    glyph.apply_punch(shift(tools.letter_l_right_counter_position) * right_stem_counter(tools.letter_l_right_counter));
    glyph = cut_bottom_serif(glyph, tools.letter_l_bottom_serif);
    glyph = cut_angle_serif(glyph, tools.letter_l_angle_serif);
    return glyph;
}

//-------------------------------------------------------------------------

typedef Glyph glyph_cutter(Font font, Toolset tools);

glyph_cutter[] cutters = new glyph_cutter[] {
    make_space,
    make_letter_e,
    make_letter_l
};

void cut_glyphs(Font font, Toolset tools)
{
    for (glyph_cutter cutter : cutters) {
        Glyph glyph = cutter(font, tools);
        glyph.smooth_close_points();
        glyph.round();
        font.set_glyph(glyph);
    }
}

//-------------------------------------------------------------------------
