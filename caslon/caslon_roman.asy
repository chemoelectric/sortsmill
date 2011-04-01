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

//-------------------------------------------------------------------------

struct Toolset {

    real default_corner_side;
    guide default_corner_guide;
    static path default_corner(Pt);

    real space_width;
    real ex_height;
    real curve_overshoot;

    Glyph c_outline;
    Glyph c_counter;
    TwoPointTrim c_upper_terminal;
    TwoPointTrim c_lower_terminal;

    Glyph e_outline;
    Glyph e_upper_counter;
    Glyph e_lower_counter;
    Glyph e_terminal_trim;
    Corner e_corner;
    AngledTrim e_terminal;

    Glyph i_left_punch;
    Glyph i_right_punch;
    Glyph i_dot;
    FlagSerifTrim i_flag_serif;
    BottomSerifTrim i_bottom_serif;

    Glyph o_outline;
    Glyph o_counter;

    Glyph r_left_punch;
    Glyph r_right_punch;
    Glyph r_shoulder_punch;
    path r_terminal;
    FlagSerifTrim r_flag_serif;
    BottomSerifTrim r_bottom_serif;
                 
    Glyph t_left_punch;
    Glyph t_right_punch;
    TwoPointTrim t_left_corner;
    TwoPointTrim t_top_rounding;
    AngledTrim t_tail_end;
    AngledTrim t_top_trim;
    FlagSerifTrim t_sheared_terminal;

    void operator init(real default_corner_side,
                       guide default_corner_guide,
                       path default_corner(Pt),
                       real space_width,
                       real ex_height,
                       real curve_overshoot,
                       Glyph c_outline,
                       Glyph c_counter,
                       TwoPointTrim c_upper_terminal,
                       TwoPointTrim c_lower_terminal,
                       Glyph e_outline,
                       Glyph e_upper_counter,
                       Glyph e_lower_counter,
                       Corner e_corner,
                       AngledTrim e_terminal,
                       Glyph i_left_punch,
                       Glyph i_right_punch,
                       Glyph i_dot,
                       FlagSerifTrim i_flag_serif,
                       BottomSerifTrim i_bottom_serif,
                       Glyph o_outline,
                       Glyph o_counter,
                       Glyph r_left_punch,
                       Glyph r_right_punch,
                       Glyph r_shoulder_punch,
                       Glyph t_left_punch,
                       Glyph t_right_punch,
                       path r_terminal,
                       FlagSerifTrim r_flag_serif,
                       BottomSerifTrim r_bottom_serif,
                       TwoPointTrim t_left_corner,
                       TwoPointTrim t_top_rounding,
                       AngledTrim t_tail_end,
                       AngledTrim t_top_trim,
                       FlagSerifTrim t_sheared_terminal
                       /* */) {

        this.default_corner_side = default_corner_side;
        this.default_corner_guide = default_corner_guide;
        this.default_corner = default_corner;

        this.space_width = space_width;
        this.ex_height = ex_height;
        this.curve_overshoot = curve_overshoot;

        this.c_outline = c_outline;
        this.c_counter = c_counter;
        this.c_upper_terminal = c_upper_terminal;
        this.c_lower_terminal = c_lower_terminal;

        this.e_outline = e_outline;
        this.e_upper_counter = e_upper_counter;
        this.e_lower_counter = e_lower_counter;
        this.e_corner = e_corner;
        this.e_terminal = e_terminal;

        this.i_left_punch = i_left_punch;
        this.i_right_punch = i_right_punch;
        this.i_dot = i_dot;
        this.i_flag_serif = i_flag_serif;
        this.i_bottom_serif = i_bottom_serif;

        this.o_outline = o_outline;
        this.o_counter = o_counter;

        this.r_left_punch = r_left_punch;
        this.r_right_punch = r_right_punch;
        this.r_shoulder_punch = r_shoulder_punch;
        this.r_terminal = r_terminal;
        this.r_flag_serif = r_flag_serif;
        this.r_bottom_serif = r_bottom_serif;

        this.t_left_punch = t_left_punch;
        this.t_right_punch = t_right_punch;
        this.t_left_corner = t_left_corner;
        this.t_top_rounding = t_top_rounding;
        this.t_tail_end = t_tail_end;
        this.t_top_trim = t_top_trim;
        this.t_sheared_terminal = t_sheared_terminal;
    }
};

//-------------------------------------------------------------------------

Glyph cut_space(Toolset tools)
{
    Glyph glyph = Glyph(name = 'space');
    glyph.lsb = 0;
    glyph.rsb = tools.space_width;
    return glyph;
}

Glyph cut_c(Toolset tools)
{
    Glyph glyph = tools.c_outline;
    glyph.name = 'c';
    glyph.punch(tools.c_counter);
    glyph.splice_in(tools.c_lower_terminal.path(glyph));
    glyph.splice_in(tools.c_upper_terminal.path(glyph));
    return glyph;
}

Glyph cut_e(Toolset tools)
{
    Glyph glyph = copy(tools.e_outline);
    glyph.name = 'e';
    glyph.punch(tools.e_upper_counter);
    glyph.punch(tools.e_lower_counter);
    glyph.splice_in(tools.e_corner.path(glyph));
    glyph.splice_in(tools.e_terminal.path(glyph));
    return glyph;
}

Glyph cut_dotlessi(Toolset tools)
{
    Glyph glyph = Glyph((0,-50), (500,1000), name = 'dotlessi');
    glyph.punch(tools.i_left_punch);
    glyph.punch(tools.i_right_punch);
    glyph.splice_in(tools.i_flag_serif.path(glyph));
    glyph.splice_in(tools.i_bottom_serif.path(glyph));
    return glyph;
}

Glyph cut_i(Toolset tools)
{
    Glyph glyph = copy(font.get_glyph('dotlessi'));
    glyph.name = 'i';
    glyph.overlay(tools.i_dot);
    return glyph;
}

Glyph cut_o(Toolset tools)
{
    Glyph glyph = copy(tools.o_outline);
    glyph.name = 'o';
    glyph.punch(tools.o_counter);
    return glyph;
}

Glyph cut_r(Toolset tools)
{
    Glyph glyph = Glyph((0,-50), (500,1000), name = 'r');
    glyph.punch(tools.r_left_punch);
    glyph.punch(tools.r_right_punch);
    glyph.punch(tools.r_shoulder_punch);
    glyph.splice_in(tools.r_terminal);
    glyph.splice_in(tools.r_flag_serif.path(glyph));
    glyph.splice_in(tools.r_bottom_serif.path(glyph));
    return glyph;
}

Glyph cut_t(Toolset tools)
{
    Glyph glyph = Glyph((-500,-500), (1000,1000), name = 't');
    glyph.punch(tools.t_left_punch);
    glyph.punch(tools.t_right_punch);
    path top_trim = tools.t_top_trim.path(glyph);
    glyph.splice_in(top_trim);
    glyph.splice_in(tools.t_sheared_terminal.path(glyph));
    glyph.splice_in(tools.t_top_rounding.path(glyph, point2=point(top_trim, 1)));
    glyph.splice_in(tools.t_tail_end.path(glyph));
    return glyph;
}

//-------------------------------------------------------------------------

typedef Glyph glyph_cutter(Toolset);

glyph_cutter[] cutters = new glyph_cutter[] {
    cut_space,
    cut_c,
    cut_e,

    cut_dotlessi,
    cut_i,

    cut_o,
    cut_r,
    cut_t
};

void cut_glyphs(Toolset tools)
{
    for (glyph_cutter cutter : cutters) {
        Glyph glyph = cutter(tools);
        for (int i = 0; i < 2; ++i) {
            glyph.smooth_close_points();
            glyph.round();
        }
        font.set_glyph(glyph);
    }
}

//-------------------------------------------------------------------------
