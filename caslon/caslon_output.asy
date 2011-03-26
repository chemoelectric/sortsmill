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
import caslon_font;

//-------------------------------------------------------------------------
//
// Functions to call with "asy -u" (the usersetting() function).
// These require that the Font be declared globally already.

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
