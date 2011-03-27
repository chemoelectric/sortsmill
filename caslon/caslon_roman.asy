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
    AngledTrim c_lower_terminal;

    Glyph e_outline;
    Glyph e_top_counter;
    Glyph e_bowl;
    Glyph e_terminal_trim;
    Corner e_corner;
    AngledTrim e_terminal;

    Glyph o_outline;
    Glyph o_counter;

    Glyph t_outline;
    Glyph t_left_punch;
    Glyph t_right_punch;
    TwoPointTrim t_left_corner;
    TwoPointTrim t_top_corner;
    AngledTrim t_terminal;

    void operator init(real default_corner_side,
                       guide default_corner_guide,
                       path default_corner(Pt),
                       real space_width,
                       real ex_height,
                       real curve_overshoot,
                       Glyph c_outline,
                       Glyph c_counter,
                       TwoPointTrim c_upper_terminal,
                       AngledTrim c_lower_terminal,
                       Glyph e_outline,
                       Glyph e_top_counter,
                       Glyph e_bowl,
                       Corner e_corner,
                       AngledTrim e_terminal,
                       Glyph o_outline,
                       Glyph o_counter,
                       Glyph t_outline,
                       Glyph t_left_punch,
                       Glyph t_right_punch,
                       TwoPointTrim t_left_corner,
                       TwoPointTrim t_top_corner,
                       AngledTrim t_terminal
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
        this.e_top_counter = e_top_counter;
        this.e_bowl = e_bowl;
        this.e_corner = e_corner;
        this.e_terminal = e_terminal;

        this.o_outline = o_outline;
        this.o_counter = o_counter;

        this.t_outline = t_outline;
        this.t_left_punch = t_left_punch;
        this.t_right_punch = t_right_punch;
        this.t_left_corner = t_left_corner;
        this.t_top_corner = t_top_corner;
        this.t_terminal = t_terminal;
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
    Glyph glyph = copy(tools.c_outline);
    glyph.name = 'c';
    glyph.punch(tools.c_counter);

    // Cut through the right side. (FIXME: May become unnecessary if I
    // ever add splicing between two outlines.)
    path counter = tools.c_counter.outlines[0];
    pair midpoint = 0.5*(max(counter) + min(counter));
    glyph.punch(Glyph(midpoint--(midpoint + (1000,0))--(midpoint + (1000,-1))--(midpoint + (0,-1))--cycle));

    // Finish the terminals.
    glyph.splice_in(add_extrema(tools.c_upper_terminal.path(glyph)));
    glyph.splice_in(tools.c_lower_terminal.path(glyph));

    return glyph;
}

Glyph cut_e(Toolset tools)
{
    Glyph glyph = copy(tools.e_outline);
    glyph.name = 'e';
    glyph.punch(tools.e_top_counter);
    glyph.punch(tools.e_bowl);
    glyph.punch(tools.e_terminal_trim);
    glyph.splice_in(add_extrema(tools.e_corner.path(glyph)));
    glyph.splice_in(tools.e_terminal.path(glyph));
    return glyph;
}

Glyph cut_o(Toolset tools)
{
    Glyph glyph = copy(tools.o_outline);
    glyph.name = 'o';
    glyph.punch(tools.o_counter);
    return glyph;
}

Glyph cut_t(Toolset tools)
{
    Glyph glyph = copy(tools.t_outline);
    glyph.name = 't';
    glyph.punch(tools.t_left_punch);
    glyph.punch(tools.t_right_punch);
    glyph.splice_in(add_extrema(tools.t_left_corner.path(glyph)));
    glyph.splice_in(add_extrema(tools.t_top_corner.path(glyph)));
    glyph.splice_in(tools.t_terminal.path(glyph));
    return glyph;
}

//-------------------------------------------------------------------------

typedef Glyph glyph_cutter(Toolset);

glyph_cutter[] cutters = new glyph_cutter[] {
    cut_space,
    cut_c,
    cut_e,
    cut_o,
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
