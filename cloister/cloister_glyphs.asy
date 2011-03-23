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

    real blank_width = tools.letter_e.terminal_point.x + 50;
    Glyph glyph = Glyph((0,tools.letter_e.bottom_point.y), (blank_width,tools.letter_e.top_point.y), name='e');

    // Cut the outline.
    glyph.splice_in(tools.letter_e.terminal_point{curl 0.3}..tension 1.2 and 0.9..
                    tools.letter_e.bottom_point{left}..tension 0.85 and 1.0..
                    {up}tools.letter_e.left_point..tension 0.95 and 1.0..
                    {right}tools.letter_e.top_point);

    // Flatten the upper left.
    Glyph.OutlinePoint left = Glyph.OutlinePoint(glyph, tools.letter_e.left_point)^0;
    Glyph.OutlinePoint top = Glyph.OutlinePoint(glyph, tools.letter_e.top_point)^0;
    p1 = left + 0.4*(top - left);
    glyph.splice_in(p1.point{dir(p1)}..tension 1.05..{dir(top)}top.point);

    // Punch the "eye".
    glyph.apply_punch(shift(tools.letter_e.eye_position) * eye_counter(tools.letter_e_eye_counter));

    // Punch the bowl. This is an inverted "eye" counter.
    real crossbar_slope = Tan(tools.letter_e_bowl_counter.bottom_angle);
    pair crossbar_vector = (tools.letter_e_bowl_counter.bounding_box.x,
                            tools.letter_e_bowl_counter.bounding_box.x * crossbar_slope);
    Glyph bowl = eye_counter(tools.letter_e_bowl_counter);
    bowl = rotate(180) * bowl;
    bowl = shift(tools.letter_e.bowl_position + crossbar_vector) * bowl;
    bowl.add_extrema();
    glyph.apply_punch(bowl);

    // Cut the terminal.
    pair vector = 300*dir(tools.letter_e.terminal_angle);
    p1 = glyph.intersections((tools.letter_e.terminal_point + 0.001*vector)---
                             (tools.letter_e.terminal_point + 500*vector))[0];
    glyph.splice_in(p1.point---tools.letter_e.terminal_point);
    glyph.splice_in(corner_shaper(glyph, tools.letter_e.terminal_point,
                                  CornerParams(10, 10, nullpath..tension 0.75..nullpath),
                                  add_extrema=true));
    glyph.splice_in(corner_shaper(glyph, p1.point, CornerParams(10, 10, nullpath..tension 0.75..nullpath)));

    // Finish the outline, except for the "knob".
    Glyph.OutlinePoint[] points = glyph.points_at_x(302);
    Glyph.OutlinePoint hypothetical_arc_end = points[points.length - 2];
    top.refresh();
    path arc = top.point{right}..{curl 0.7}hypothetical_arc_end.point;
    path ray = tools.letter_e.eye_position---
        (tools.letter_e.eye_position + 1000*dir(tools.letter_e_bowl_counter.bottom_angle));
    path arc = subpath(arc, 0, intersect(arc, ray)[0]);
    pair arc_end = point(arc, 1);
    ray = arc_end---(arc_end + 1000*dir(tools.letter_e_bowl_counter.bottom_angle));
    p1 = glyph.intersections(ray)[0];
    glyph.splice_in(arc & arc_end---p1.point);

    // Cut the "knob".
    points = glyph.points_at_x(tools.letter_e.knob_end_x);
    p1 = points[points.length - 2];
    vector = 300*dir(tools.letter_e.knob_angle);
    p2 = glyph.intersections((p1.point + 0.001*vector)---(p1.point + 500*vector))[0];
    glyph.splice_in(p2.point---p1.point);
    p1.refresh();
    glyph.splice_in(corner_shaper(glyph, p1.point, CornerParams(10, 10, nullpath..tension 0.75..nullpath),
                                  add_extrema=true));
    p2.refresh();
    glyph.splice_in((p2^-1).point{dir(tools.letter_e_bowl_counter.bottom_angle)}..tension 0.75..
                    {-dir(tools.letter_e.knob_angle)}(p2 + 10).point);

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
