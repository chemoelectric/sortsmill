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

import sortsmill_font;
import sortsmill_glyph;

//-------------------------------------------------------------------------

struct CornerParams {
    real before = infinity;
    real after = infinity;
    guide tensions = nullpath;

    void operator init(real before, real after, guide tensions) {
        this.before = before;
        this.after = after;
        this.tensions = tensions;
    }
};

struct BottomSerif {
    pair left_bottom_point;
    pair right_bottom_point;
    real left_side_angle;
    real right_side_angle;
    pair[] cup_params;
    CornerParams corner_params;

    void operator init(pair left_bottom_point,
                       pair right_bottom_point,
                       real left_side_angle,
                       real right_side_angle,
                       pair[] cup_params,
                       CornerParams corner_params) {
        this.left_bottom_point = left_bottom_point;
        this.right_bottom_point = right_bottom_point;
        this.left_side_angle = left_side_angle;
        this.right_side_angle = right_side_angle;
        this.cup_params = cup_params;
        this.corner_params = corner_params;
    }

    BottomSerif shifted(pair vector) {
        BottomSerif new_serif = new BottomSerif;
        new_serif.left_bottom_point = this.left_bottom_point + vector;
        new_serif.right_bottom_point = this.right_bottom_point + vector;
        new_serif.left_side_angle = this.left_side_angle;
        new_serif.right_side_angle = this.right_side_angle;
        new_serif.cup_params = this.cup_params;
        new_serif.corner_params = this.corner_params;
        return new_serif;
    }
};

struct AngleSerif {
    pair top_point;
    real top_slope;
    real before_top_point;
    real after_top_point;
    real before_stem_join;
    guide right_corner_tensions;
    CornerParams left_corner_params;

    void operator init(pair top_point,
                       real top_slope,
                       real before_top_point,
                       real after_top_point,
                       real before_stem_join,
                       guide right_corner_tensions,
                       CornerParams left_corner_params) {
        this.top_point = top_point;
        this.top_slope = top_slope;
        this.before_top_point = before_top_point;
        this.after_top_point = after_top_point;
        this.before_stem_join = before_stem_join;
        this.right_corner_tensions = right_corner_tensions;
        this.left_corner_params = left_corner_params;
    }
};

struct StemCounter {
    real top_angle;
    real side_angle;
    real bottom_angle;
    real side_height;
    real past_top;
    real on_side;
    real past_bottom;
    guide top_tensions;
    guide bottom_tensions;
    CornerParams top_corner;
    CornerParams bottom_corner;
    
    void operator init(real top_angle,
                       real side_angle,
                       real bottom_angle,
                       real side_height,
                       real past_top,
                       real on_side,
                       real past_bottom,
                       guide top_tensions,
                       guide bottom_tensions,
                       CornerParams top_corner,
                       CornerParams bottom_corner) {
        this.top_angle = top_angle;
        this.side_angle = side_angle;
        this.bottom_angle = bottom_angle;
        this.side_height = side_height;
        this.past_top = past_top;
        this.on_side = on_side;
        this.past_bottom = past_bottom;
        this.top_tensions = top_tensions;
        this.bottom_tensions = bottom_tensions;
        this.top_corner = top_corner;
        this.bottom_corner = bottom_corner;
    }
};

struct EyeCounter {             // The "eye" of a letter "e".
    pair bounding_box;
    real bottom_angle;
    real apex_x;
    guide left_tensions;
    guide right_tensions;

    void operator init(pair bounding_box,
                       real bottom_angle,
                       real apex_x,
                       guide left_tensions,
                       guide right_tensions) {
        this.bounding_box = bounding_box;
        this.bottom_angle = bottom_angle;
        this.apex_x = apex_x;
        this.left_tensions = left_tensions;
        this.right_tensions = right_tensions;
    }
};

struct Letter_e {
    pair eye_position;
    pair bowl_position;
    pair left_point;
    pair top_point;
    pair bottom_point;
    pair terminal_point;
    real terminal_angle;
    real arc_end_x;
    real knob_end_x;
    real knob_angle;
    real flattening_point_relative;
    guide lower_right_tensions;
    guide lower_left_tensions;
    guide upper_left_tensions;
    guide upper_right_tensions;
    guide flattening_tensions;
    CornerParams corner_params;

    void operator init(pair eye_position,
                       pair bowl_position,
                       pair left_point,
                       pair top_point,
                       pair bottom_point,
                       pair terminal_point,
                       real terminal_angle,
                       real arc_end_x,
                       real knob_end_x,
                       real knob_angle,
                       real flattening_point_relative,
                       guide lower_right_tensions,
                       guide lower_left_tensions,
                       guide upper_left_tensions,
                       guide upper_right_tensions,
                       guide flattening_tensions,
                       CornerParams corner_params) {
        this.eye_position = eye_position;
        this.bowl_position = bowl_position;
        this.left_point = left_point;
        this.top_point = top_point;
        this.bottom_point = bottom_point;
        this.terminal_point = terminal_point;
        this.terminal_angle = terminal_angle;
        this.arc_end_x = arc_end_x;
        this.knob_end_x = knob_end_x;
        this.knob_angle = knob_angle;
        this.flattening_point_relative = flattening_point_relative;
        this.lower_right_tensions = lower_right_tensions;
        this.lower_left_tensions = lower_left_tensions;
        this.upper_left_tensions = upper_left_tensions;
        this.upper_right_tensions = upper_right_tensions;
        this.flattening_tensions = flattening_tensions;
        this.corner_params = corner_params;
    }
    
};

struct Toolset {
    Font font;
    real space_width;
    EyeCounter letter_e_eye_counter;
    EyeCounter letter_e_bowl_counter;
    Letter_e letter_e;
    StemCounter letter_l_left_counter;
    StemCounter letter_l_right_counter;
    pair letter_l_left_counter_position;
    pair letter_l_right_counter_position;
    BottomSerif letter_l_bottom_serif;
    AngleSerif letter_l_angle_serif;
};

//-------------------------------------------------------------------------

path corner_shaper(Glyph.OutlinePoint corner,
                   real arclength_before,
                   real arclength_after,
                   guide tensions,
                   bool add_extrema = false)
{
    path corner_path;
    if (arclength_before != infinity && arclength_after != infinity) {
        Glyph.OutlinePoint p1 = corner - arclength_before;
        Glyph.OutlinePoint p2 = corner + arclength_after;
        corner_path = p1.point{dir(p1)}..tensions..{dir(p2)}p2.point;
    }
    if (add_extrema)
        corner_path = add_extrema(corner_path);
    return corner_path;
}

path corner_shaper(Glyph glyph,
                   pair corner_point,
                   real arclength_before,
                   real arclength_after,
                   guide tensions,
                   bool add_extrema = false)
{
    return corner_shaper(Glyph.OutlinePoint(glyph, corner_point),
                         arclength_before, arclength_after, tensions,
                         add_extrema);
}

path corner_shaper(Glyph.OutlinePoint corner, CornerParams params,
                   bool add_extrema = false)
{
    return corner_shaper(corner, params.before, params.after, params.tensions,
                         add_extrema);
}

path corner_shaper(Glyph glyph, pair corner_point, CornerParams params,
                   bool add_extrema = false)
{
    return corner_shaper(glyph, corner_point, params.before, params.after, params.tensions,
                         add_extrema);
}

path cup_shaper(Glyph glyph,
                pair point1, pair point2,
                pair[] cup_params)
{
    real length = abs(point2 - point1);
    return point1..
        controls (point1 + length*cup_params[0]) and (point2 + length*cup_params[1])..
        point2;
}

//-------------------------------------------------------------------------

Glyph cut_bottom_serif(Glyph glyph, BottomSerif serif)
{
    Glyph.OutlinePoint left_top =
        glyph.intersections(shift(serif.left_bottom_point)*((0,0)---2000*dir(serif.left_side_angle)))[0];
    Glyph.OutlinePoint right_top =
        glyph.intersections(shift(serif.right_bottom_point)*((0,0)---2000*dir(serif.right_side_angle)))[0];
    pair left_top_point = left_top.point;
    pair right_top_point = right_top.point;
    glyph.splice_in(right_top_point---serif.right_bottom_point---serif.left_bottom_point---left_top_point);
    glyph.splice_in(cup_shaper(glyph, serif.right_bottom_point, serif.left_bottom_point, serif.cup_params));
    glyph.splice_in(corner_shaper(glyph, right_top_point, serif.corner_params));
    glyph.splice_in(corner_shaper(glyph, serif.right_bottom_point, serif.corner_params));
    glyph.splice_in(corner_shaper(glyph, serif.left_bottom_point, serif.corner_params));
    glyph.splice_in(corner_shaper(glyph, left_top_point, serif.corner_params));
    return glyph;
}

Glyph cut_angle_serif(Glyph glyph, AngleSerif serif)
{
    Glyph.OutlinePoint left_corner =
        glyph.intersections(serif.top_point---(serif.top_point + 2000*dir(180 + serif.top_slope)))[0];
    Glyph.OutlinePoint right_corner = glyph.intersections(serif.top_point---(serif.top_point + 2000*dir(0)))[0];
    glyph.splice_in(left_corner.point---serif.top_point---right_corner.point);

    Glyph.OutlinePoint top_pt = Glyph.OutlinePoint(glyph, serif.top_point);
    glyph.splice_in(add_extrema(corner_shaper(Glyph.OutlinePoint(glyph, serif.top_point).nearby_node(-1),
                                              serif.left_corner_params)));
    top_pt.refresh();
    Glyph.OutlinePoint p1 = top_pt - serif.before_top_point;
    Glyph.OutlinePoint p2 = top_pt + serif.after_top_point;
    Glyph.OutlinePoint p3 = top_pt^2 - serif.before_stem_join;
    glyph.splice_in(add_extrema(p1.point{dir(p1)}::
                                {right}p2.point..serif.right_corner_tensions..
                                {dir(p3)}p3.point));

    // Thanks to add_extrema() sticking a node between p2 and p3, we
    // no longer need or want the node at p3.
    p3.refresh();
    glyph.splice_in((p3^-1).point{dir(p3^-1)}::{dir(p3^1)}(p3^1).point);

    return glyph;
}

Glyph left_stem_counter(StemCounter params)
{
    real side_height = params.side_height;
    Glyph counter = Glyph((-1000,-500), (1000,1000));
    real side_slope = Tan(90 - params.side_angle);
    pair side_top = slant(side_slope) * (0,params.side_height);
    counter.chop(side_top, params.top_angle);
    counter.chop(side_top, params.side_angle);
    counter.chop((0,0), params.bottom_angle);
    Glyph.OutlinePoint p1 = Glyph.OutlinePoint(counter, side_top) - params.past_top;
    Glyph.OutlinePoint on_side = Glyph.OutlinePoint(counter, slant(side_slope)*(0,params.on_side));
    Glyph.OutlinePoint p2 = Glyph.OutlinePoint(counter, (0,0)) + params.past_bottom;
    counter.splice_in(p1.point{dir(p1)}..params.top_tensions..
                      {dir(params.side_angle)}(on_side.point)..params.bottom_tensions..
                      {dir(p2)}p2.point);
    on_side.refresh();
    counter.splice_in(corner_shaper(on_side^-1, params.top_corner));
    on_side.refresh();
    counter.splice_in(corner_shaper(on_side^1, params.bottom_corner));
    return counter;
}

Glyph right_stem_counter(StemCounter params)
{
    real side_height = params.side_height;
    Glyph counter = Glyph((-1000,-500), (1000,1000));
    real side_slope = Tan(90 - params.side_angle);
    pair side_top = slant(side_slope) * (0,params.side_height);
    counter.chop(side_top, params.top_angle);
    counter.chop((0,0), params.side_angle);
    counter.chop((0,0), params.bottom_angle);
    Glyph.OutlinePoint p1 = Glyph.OutlinePoint(counter, (0,0)) - params.past_bottom;
    Glyph.OutlinePoint on_side = Glyph.OutlinePoint(counter, slant(side_slope)*(0,params.on_side));
    Glyph.OutlinePoint p2 = Glyph.OutlinePoint(counter, side_top) + params.past_top;
    counter.splice_in(p1.point{dir(p1)}..params.bottom_tensions..
                      {dir(params.side_angle)}(on_side.point)..params.top_tensions..
                      {dir(p2)}p2.point);
    on_side.refresh();
    counter.splice_in(corner_shaper(on_side^-1, params.bottom_corner));
    on_side.refresh();
    counter.splice_in(corner_shaper(on_side^1, params.top_corner));
    return counter;
}

Glyph eye_counter(EyeCounter params)
{
    real bottom_slope = Tan(params.bottom_angle);
    path counter_path = ((0,0)..params.left_tensions..
                         (params.apex_x,params.bounding_box.y){right}..params.right_tensions..
                         (params.bounding_box.x,params.bounding_box.x * bottom_slope)---cycle);
    return Glyph(counter_path);
}

//-------------------------------------------------------------------------
//
// Functions to call with "asy -u" (the usersetting() function).
// These require that the Font be declared already.

Font font;

void write_glyph_data(string glyphname)
{
    font.write_activeGlyph_update_code(glyphname);
}

void generate(string fontfile_name ... string[] flags)
{
    font.write_script_code();
    string flags_string;
    for (string s : flags)
        flags_string += '\'' + s + '\',';
    write('my_font.generate(\'' + fontfile_name + '\', flags=[' + flags_string + '])');
}

void save(string sfd_name)
{
    font.write_script_code();
    write('my_font.save(\'' + sfd_name + '\')');
}

//-------------------------------------------------------------------------
