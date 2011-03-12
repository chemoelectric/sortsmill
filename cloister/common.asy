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
                        real top_angle,
                        real bottom_angle)
// Counterpunch for the right side of roman 'l', 'i', etc.
{
    real bignum = 2000;

    path counter = (0,-bignum)---(0,bignum)---(bignum,bignum)---(bignum,-bignum)---cycle;
    counter = chop(counter, (3,stem_height), top_angle);
    counter = chop(counter, (0,0), 180 + bottom_angle);

    pair point1 = point_at_distance_along_arc(counter, (0,0), -15);
    counter = reshape_arc(counter, (0,0), 15, 0.2 * stem_height, nullpath..tension 3 and 1 ..nullpath);
    pair point2 = intersectionpoint(counter, (40,-50)---(40,50));
    pair point3 = intersectionpoint(counter, (-50,46)---(50,46));
    counter = reshape_subpath(counter, point2, point3, nullpath..tension 0.9 and 0.8 ..nullpath);

    counter = shift(-3,stem_position) * counter;
    return counter;
}

//-------------------------------------------------------------------------
