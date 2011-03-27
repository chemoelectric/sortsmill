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
import caslon_roman;
import caslon_output;
import caslon_version;

//-------------------------------------------------------------------------

font.version = version_string;

font.fontname = 'CaslonStM';
font.familyname = 'Sorts Mill Caslon';
font.fullname = 'Sorts Mill Caslon';
font.weight = 'Regular';
font.sfnt_family = 'Sorts Mill Caslon';
font.sfnt_subfamily = 'Regular';
font.design_size = 12;
font.boldness = 400;

//.........................................................................

pair c_upper_terminal_point = (305,307);

Glyph c_outline = shift(-13,0)*Glyph((185,-11){left}..tension 0.95 and 1.0..
                                     (13,190){up}..(205,416){right}..tension 0.97..
                                     (370,200){down}..tension 1.0 and 0.95..cycle);
Glyph c_counter = shift(-227,-41)*Glyph((227,41){left}..(86,200){up}..(208,377){right}..
                                        c_upper_terminal_point{right}..(370,200){down}..cycle);

Pt p1 = c_outline.points_at_y(375)[1];
Pt p2 = c_outline.points_at_y(63)[1];

//.........................................................................

pair e_terminal_point = (349,97);
pair e_corner_point = (332,260);

Glyph e_outline = Glyph(e_terminal_point{curl 0.9}::
                        (186,-9){left}..tension 1.0 and 0.85..
                        (-2,200){up}..
                        {right}(176,413)..tension 0.95..
                        {down}e_corner_point..tension atleast 0.75..
                        {right}(e_terminal_point.x,220)--cycle);

Glyph e_top_counter = Glyph((0,0){curl 0.6}::{right}(100,109)..tension 0.92..{curl 0.6}(184,2)--cycle);
e_top_counter.splice_in(add_extrema(corner((e_top_counter@(0,0))^0, 25, 50, nullpath..tension 0.75..nullpath)));
e_top_counter.splice_in(add_extrema(corner((e_top_counter@(190,2))^0, 70, 35, nullpath..tension 0.9..nullpath)));

Glyph e_bowl = Glyph((150,-209){left}::(-4,-45){up}::{curl 1}(0,0)--(300,2){curl 1}::cycle);
e_bowl.splice_in(add_extrema(corner((e_bowl@(0,0))^0, 50, 30, nullpath..tension 0.75 and 3.0..nullpath)));

//.........................................................................

Glyph o_outline = Glyph((185,-10){left}..tension 0.95 and 1.0..
                        (0,200){up}..tension 0.85 and 1.3..
                        (190,409){right}..
                        (384,200){down}..tension 1.0 and 0.95..cycle);
Glyph o_counter = shift(-8,-1)*Glyph((3,1){left}..tension 1.4 and 0.75..
                                    (-114,185){up}..tension 0.97..
                                     (5,356){right}..
                                    (134,170){down}..tension 0.75 and 1.4..cycle);

//.........................................................................

Toolset tools = Toolset(

    space_width=200,
    ex_height=403,
    curve_overshoot=10,

    c_outline=shift(0,-1)*c_outline,
    c_counter=shift(202,40)*c_counter,

    e_outline=e_outline,
    e_top_counter=shift(73,280)*e_top_counter,
    e_bowl=shift(56,256)*e_bowl,
    e_corner=Corner(e_corner_point, 20, 20, nullpath..tension 1.1..nullpath),
    e_terminal=AngledTrim(e_terminal_point - (0,0.01), dir(145), 1, 1000, nullpath..tension 2.0..nullpath, reversed=true),

    o_outline=o_outline,
    o_counter=shift(188,23)*o_counter

    /* */);

cut_glyphs(tools);

usersetting();

//-------------------------------------------------------------------------
