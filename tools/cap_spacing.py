# -*- coding: utf-8 -*-

"""
Copyright (c) 2009 Barry Schwartz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

import fontforge

def cap_spacing(font, caps_names, expansion):

    advance_widths = {}
    for n in caps_names:
        if n in font:
            w = font[n].width
            if w in advance_widths:
                advance_widths[w].append(n)
            else:
                advance_widths[w] = [n]

    rules = ''
    for w in sorted(advance_widths):
        advance_increase = int(round(expansion * w))
        pos = int(advance_increase / 2)
        glyphs = advance_widths[w]
        rules += '  pos '
        if len(glyphs) == 1:
            rules += '\\' + glyphs[0]
        else:
            rules += '[ '
            for g in glyphs:
                rules += '\\' + g + ' '
            rules += ']'
        rules += ' <' + str(pos) + ' 0 ' + str(advance_increase) + ' 0>;\n'

    return rules
