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

import sortsmill_glyph;

round_points = true;
simplify_slightly = true;

//-------------------------------------------------------------------------

struct CornerParams {
    real before;
    real after;
    guide tensions;

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

struct Toolset {
    Glyph letter_l_left_counter;
    Glyph letter_l_right_counter;
    BottomSerif letter_l_bottom_serif;
    AngleSerif letter_l_angle_serif;
};

//-------------------------------------------------------------------------

path corner_shaper(Glyph.OutlinePoint corner,
                   real arclength_before,
                   real arclength_after,
                   guide tensions)
{
    Glyph.OutlinePoint p1 = corner - arclength_before;
    Glyph.OutlinePoint p2 = corner + arclength_after;
    return p1.point{dir(p1)}..tensions..{dir(p2)}p2.point;
}

path corner_shaper(Glyph glyph,
                   pair corner_point,
                   real arclength_before,
                   real arclength_after,
                   guide tensions)
{
    return corner_shaper(Glyph.OutlinePoint(glyph, corner_point),
                         arclength_before, arclength_after, tensions);
}

path corner_shaper(Glyph.OutlinePoint corner, CornerParams params)
{
    return corner_shaper(corner, params.before, params.after, params.tensions);
}

path corner_shaper(Glyph glyph, pair corner_point, CornerParams params)
{
    return corner_shaper(glyph, corner_point, params.before, params.after, params.tensions);
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

Glyph left_stem_counter_rough(real top_angle,
                              real side_angle,
                              real bottom_angle,
                              real side_height,
                              pair corner1 = (-1000,-500),
                              pair corner2 = (1000,1000))
{
    Glyph counter = Glyph(corner1, corner2);
    counter.chop((0,side_height), top_angle);
    counter.chop((0,side_height), side_angle);
    counter.chop((0,0), bottom_angle);
    return counter;
}

Glyph right_stem_counter_rough(real top_angle,
                               real side_angle,
                               real bottom_angle,
                               real side_height,
                               pair corner1 = (-1000,-500),
                               pair corner2 = (1000,1000))
{
    Glyph counter = Glyph(corner1, corner2);
    counter.chop((0,side_height), top_angle);
    counter.chop((0,0), side_angle);
    counter.chop((0,0), bottom_angle);
    return counter;
}

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

//-------------------------------------------------------------------------
