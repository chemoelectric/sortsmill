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

Glyph make_letter_l(Font font, Toolset tools)
{
    Glyph glyph = Glyph((-200,-200), (200,1200));
    glyph.apply_punch(left_stem_counter(tools.letter_l_left_counter));
    glyph.apply_punch(shift(tools.letter_l_stem_width,0) * right_stem_counter(tools.letter_l_right_counter));
    glyph = cut_bottom_serif(glyph, tools.letter_l_bottom_serif);
    glyph = cut_angle_serif(glyph, tools.letter_l_angle_serif);
    glyph.name = 'l';
    return glyph;
}

//-------------------------------------------------------------------------

typedef Glyph glyph_cutter(Font font, Toolset tools);

glyph_cutter[] cutters = new glyph_cutter[] {
    make_space,
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
