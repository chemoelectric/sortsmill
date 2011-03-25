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
import cloister_params;
import cloister_version;
import sortsmill_font;

//-------------------------------------------------------------------------

font.version = version_string;

font.fontname = 'CloisterStM8-Bold';
font.familyname = 'Sorts Mill Cloister 8 Point';
font.fullname = 'Sorts Mill Cloister 8 Point Bold';
font.weight = 'Bold';
font.sfnt_family = 'Sorts Mill Cloister 8 Point';
font.sfnt_subfamily = 'Bold';
font.preferred_family = 'Sorts Mill Cloister';
font.preferred_subfamily = '8 Point Bold';
font.design_size = 8;
font.interpolated_size = 8.7;
font.boldness = 700;

Toolset tools = get_tools(font);
cut_glyphs(font, tools);
usersetting();

//-------------------------------------------------------------------------
