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
    
import fontforge;

//-------------------------------------------------------------------------

string version = '0.1';

string copyright_notice = 'Copyright (c) 2011 Barry Schwartz';

string license_notice =
    'Permission is hereby granted, free of charge, to any person obtaining a copy\\n'
    'of this software and associated documentation files (the "Software"), to deal\\n'
    'in the Software without restriction, including without limitation the rights\\n'
    'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell\\n'
    'copies of the Software, and to permit persons to whom the Software is\\n'
    'furnished to do so, subject to the following conditions:\\n'
    '\\n'
    'The above copyright notice and this permission notice shall be included in\\n'
    'all copies or substantial portions of the Software.\\n'
    '\\n'
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\\n'
    'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\\n'
    'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE\\n'
    'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\\n'
    'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,\\n'
    'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN\\n'
    'THE SOFTWARE.\\n';

string license_url = 'http://www.opensource.org/licenses/mit-license.php';

string vendor_url = 'http://sortsmill.googlecode.com';
string designer_name = 'Barry Schwartz';
string designer_url = 'http://crudfactory.com';

string fontname = 'Untitled';
string familyname = 'Untitled';
string fullname = 'Untitled';
string fontweight = 'Regular';
string sfnt_family = 'Untitled';
string sfnt_style = 'Regular';
string preferred_family = '';
string preferred_style = '';
string wws_family = '';
string wws_style = '';

//-------------------------------------------------------------------------

void write_python()
{
    write('#!/usr/bin/env python

import fontforge
import psMat

f = fontforge.font()

f.version = \'' + version + '\'
f.copyright = \'' + copyright_notice + '\'
f.fontname = \'' + fontname + '\'
f.familyname = \'' + familyname + '\'
f.fullname = \'' + fullname + '\'
f.weight = \'' + fontweight + '\'

lang = \'English (US)\'
f.appendSFNTName(lang, \'Family\', \'' + sfnt_family + '\')
f.appendSFNTName(lang, \'SubFamily\', \'' + sfnt_style + '\')');

    if (preferred_family != '')
        write('f.appendSFNTName(lang, \'Preferred Family\', \'' + preferred_family + '\')');
    if (preferred_style != '')
        write('f.appendSFNTName(lang, \'Preferred Styles\', \'' + preferred_style + '\')');
    
    if (wws_family != '')
        write('f.appendSFNTName(lang, \'WWS Family\', \'' + wws_family + '\')');
    if (wws_style != '')
        write('f.appendSFNTName(lang, \'WWS Subfamily\', \'' + wws_style + '\')');
    
    write('f.appendSFNTName(lang, \'License\', \'' + copyright_notice + '\\n\\n' + license_notice + '\');
f.appendSFNTName(lang, \'License URL\', \'' + license_url + '\');
f.appendSFNTName(lang, \'Vendor URL\', \'' + vendor_url + '\');
f.appendSFNTName(lang, \'Designer\', \'' + designer_name + '\');
f.appendSFNTName(lang, \'Designer URL\', \'' + designer_url + '\');
');

    write_fontforge_glyph_list_code('contour');
    
    write('f.encoding = \'UnicodeBMP\'');
}

void write_sfd(string sfd_file)
{
    write_python();
    write('f.save(\'' + sfd_file + '\')');
}

void generate(string otf_file)
{
    write_python();
    write('f.generate(\'' + otf_file + '\')');
}

//-------------------------------------------------------------------------

path left_stem_counter(real stem_position,
                       real stem_height,
                       real height_of_right_max,
                       real top_angle,
                       real bottom_angle,
                       real shape_param1,
                       real shape_param2,
                       real shape_param3,
                       real shape_param4,
                       real tension1,
                       real tension2,
                       real tension3,
                       real tension4,
                       real tension5)
// Counterpunch for the left side of roman 'l', 'i', etc.
{
    real bignum = 2000;

    path counter = (-bignum,-bignum)---(-bignum,bignum)---(0,bignum)---(0,-bignum)---cycle;
    counter = chop(counter, (0,stem_height), top_angle);
    counter = chop(counter, (0,0), 180 + bottom_angle);

    pair point1 = (0, height_of_right_max);
    pair point2 = intersectionpoint(counter, (shape_param1,100)---(shape_param1,-100));
    counter = reshape_subpath(counter, point1, point2, nullpath .. tension tension1 and tension2 .. nullpath);
    counter = reshape_arc(counter, point2, shape_param2, shape_param3, nullpath..tension tension3 and tension4..nullpath);

    counter = reshape_arc(counter, (0,stem_height), shape_param4, nullpath..tension tension5 ..nullpath);

    counter = shift(0,stem_position) * counter;
    return counter;
}

path right_stem_counter(real stem_position,
                        real stem_height,
                        real height_of_left_min,
                        real stem_indentation,
                        real top_angle,
                        real bottom_angle,
                        real shape_param1,
                        real shape_param2,
                        real shape_param3,
                        real shape_param4,
                        real shape_param5,
                        real tension1,
                        real tension2,
                        real tension3,
                        real tension4,
                        real tension5,
                        real tension6)
// Counterpunch for the right side of roman 'l', 'i', etc.
{
    real bignum = 2000;

    path counter = (0,-bignum)---(0,bignum)---(bignum,bignum)---(bignum,-bignum)---cycle;
    counter = chop(counter, (stem_indentation,stem_height), top_angle);
    counter = chop(counter, (0,0), 180 + bottom_angle);

    pair point1 = point_at_distance_along_arc(counter, (0,0), -shape_param1);
    counter = reshape_arc(counter, (0,0), shape_param1, height_of_left_min, nullpath..tension tension1 and tension2..nullpath);
    pair point2 = intersectionpoint(counter, (shape_param2,-50)---(shape_param2,50));
    pair point3 = intersectionpoint(counter, (-50,shape_param3)---(50,shape_param3));
    counter = reshape_subpath(counter, point2, point3, nullpath..tension tension3 and tension4..nullpath);

    pair point4 = intersectionpoint(counter, (-50,stem_height + shape_param4)--(50,stem_height + shape_param4));
    counter = reshape_subpath(counter, (0, height_of_left_min), point4, nullpath..tension tension5 and tension6..nullpath);
    counter = reshape_arc(counter, point4, shape_param5);

    counter = shift(-stem_indentation,stem_position) * counter;
    return counter;
}

//-------------------------------------------------------------------------
