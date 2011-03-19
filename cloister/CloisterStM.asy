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

//-------------------------------------------------------------------------

round_points = true;
simplify_slightly = true;

//-------------------------------------------------------------------------

Font font;

font.version = '0.1';           // Maybe move this elsewhere.

font.fontname = 'CloisterStM';
font.familyname = 'Sorts Mill Cloister';
font.fullname = 'Sorts Mill Cloister';
font.weight = 'Regular';
font.sfnt_family = 'Sorts Mill Cloister';
font.sfnt_subfamily = 'Regular';
font.design_size = 14;

//-------------------------------------------------------------------------

real ascender_height = 644;
real stem_width = 65;

pair left_stem_position = (0,39);
real left_stem_height = 500;
real left_top_angle = -32;
real left_bottom_angle = 13;

pair right_stem_position = (stem_width, 36);
real right_stem_height = 515;
real right_top_angle = 77;
real right_bottom_angle = 1.4;

pair bottom_serif_lower_left = (-49,-5);
pair bottom_serif_lower_right = (133,3);
pair bottom_serif_lower_left_control = bottom_serif_lower_left + (60,3);
pair bottom_serif_lower_right_control = bottom_serif_lower_right + -(111,2);

pair top_serif_point_on_slope = left_stem_position + (0,left_stem_height) + (-42, 54);
real top_serif_slope_angle = 27;

//-------------------------------------------------------------------------

Glyph glyph = Glyph((-200,-200), (200,1200));

Glyph.OutlinePoint lower_left()
{
    return Glyph.OutlinePoint(glyph, bottom_serif_lower_left);
}

Glyph.OutlinePoint lower_right()
{
    return Glyph.OutlinePoint(glyph, bottom_serif_lower_right);
}

Glyph.OutlinePoint upper_left()
{
    return glyph.points_at_x(-49)[1];
}

Glyph.OutlinePoint upper_right()
{
    return glyph.points_at_x(133)[1];
}

Glyph.OutlinePoint middle_left()
{
    Glyph.OutlinePoint lower = lower_left();
    Glyph.OutlinePoint upper = upper_left();
    real d = upper - lower;
    return Glyph.OutlinePoint(lower.glyph, (lower + 0.5*d).point);
}

Glyph.OutlinePoint point_on_slope()
{
    return Glyph.OutlinePoint(glyph, top_serif_point_on_slope);
}

Glyph.OutlinePoint point_below_slope()
{
    Glyph.OutlinePoint[] points = glyph.points_at_x(top_serif_point_on_slope.x);
    int i = (points.length <= 2) ? 0 : 2;
    return points[i];
}

//-------------------------------------------------------------------------

Glyph left_counter = Glyph((-1000,-501), (0,1201));
left_counter.chop((0,left_stem_height), left_top_angle);
left_counter.chop((0,0), 180 + left_bottom_angle);
Glyph.OutlinePoint stem_bottom = Glyph.OutlinePoint(left_counter, (0,0));
Glyph.OutlinePoint stem_top = Glyph.OutlinePoint(left_counter, (0,left_stem_height));
Glyph.OutlinePoint extreme = stem_bottom + 0.9*(stem_top - stem_bottom);
splice_in(stem_top - 30, stem_bottom + 35,
          nullpath..
          (extreme.point){down}..tension 10.0..
          (-2,166)..controls (-3,10) and (-3,0)..
          nullpath);

Glyph right_counter = Glyph((0,-501), (1000,1201));
right_counter.chop((0,right_stem_height), right_top_angle);
right_counter.chop((0,0), 180 + right_bottom_angle);
Glyph.OutlinePoint stem_bottom = Glyph.OutlinePoint(right_counter, (0,0));
Glyph.OutlinePoint stem_top = Glyph.OutlinePoint(right_counter, (0,right_stem_height));
Glyph.OutlinePoint extreme = stem_bottom + 0.15*(stem_top - stem_bottom);
splice_in(stem_bottom - 40, stem_top + 35,
          nullpath..tension 0.9 and 0.75..
          extreme.point{up}..controls (0,right_stem_height - 200) and (0,right_stem_height)..
          nullpath);

glyph.apply_punch(shift(left_stem_position)*left_counter);
glyph.apply_punch(shift(right_stem_position)*right_counter);

//-------------------------------------------------------------------------

glyph.chop(bottom_serif_lower_right, bottom_serif_lower_left);
glyph.splice_in(bottom_serif_lower_right..
                controls bottom_serif_lower_right_control and bottom_serif_lower_left_control..
                bottom_serif_lower_left);
glyph.splice_in(lower_left().point---upper_left().point);
glyph.splice_in(upper_right().point---lower_right().point);
splice_in(lower_left() - 10, lower_left() + 10, nullpath..tension 0.75..nullpath);
splice_in(upper_left() - 10, upper_left() + 10, nullpath..tension 0.75..nullpath);
splice_in(lower_right() - 10, lower_right() + 10, nullpath..tension 0.75..nullpath);
splice_in(upper_right() - 10, upper_right() + 10, nullpath..tension 0.75..nullpath);

//-------------------------------------------------------------------------

pair top_point = (stem_width + 2, ascender_height);
pair right_point = top_point + (15,-15);

Glyph.OutlinePoint slope_point()
{
    return glyph.points_at_y(top_point.y)[0];
}

Glyph.OutlinePoint right_protrusion_point()
{
    return glyph.points_at_y(top_point.y - 16)[1];
}

glyph.chop(top_serif_point_on_slope, top_serif_slope_angle);
glyph.splice_in(point_below_slope().point---point_on_slope().point);
splice_in(point_below_slope() - 10, point_below_slope() + 10, nullpath..tension 0.75..nullpath);
splice_in(point_on_slope() - 10, point_on_slope() + 10, nullpath..tension 0.75..nullpath);
glyph.chop(slope_point().point, top_point);
splice_in(slope_point() - 10, Glyph.OutlinePoint(glyph, top_point), nullpath..tension 1.0..nullpath);
glyph.splice_in(top_point{right}..{down}(right_protrusion_point().point - (2,0)));
glyph.splice_in((right_protrusion_point().point - (2,0)){down}..tension 1.2..
                {(right_protrusion_point() + 20).dir()}(right_protrusion_point() + 20).point);

//-------------------------------------------------------------------------

glyph.smooth_close_points();
glyph.name = 'l';
font.set_glyph(glyph);

//glyph.fill();

//-------------------------------------------------------------------------

void write_glyph_data(string glyphname)
{
    font.write_activeGlyph_update_code(glyphname);
}

void generate(string fontfile_name,
              string flags = '("opentype",)')
{
    font.write_script_code();
    write('my_font.generate(\'' + fontfile_name + '\', flags=' + flags + ')');
}

void save(string sfd_name)
{
    font.write_script_code();
    write('my_font.save(\'' + sfd_name + '\')');
}

usersetting();

//-------------------------------------------------------------------------
