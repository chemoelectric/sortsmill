#!/usr/bin/python

# Copyright (c) 2009 Barry Schwartz
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

#--------------------------------------------------------------------------

import fontforge
import sys
import subprocess

layer_to_use = "Fore"
unwanted_glyphs = []
generation_flags = ("opentype", "round", "glyph-map-file")
font_extension = ".otf"

#--------------------------------------------------------------------------

for sfd_path in sys.argv[1:]:

    if sfd_path[-4:] != ".sfd":
        sfd_path += ".sfd"

    f = fontforge.open(sfd_path)

    for glyph_name in f:
        for suffix in unwanted_glyphs:
            if glyph_name[-len(suffix) - 1:] == "." + suffix:
                remove_glyph(f[glyph_name])

    sys.stdout.write("\n")
    sys.stdout.flush()

    # The anchors are for development only.
    for lookup in f.gpos_lookups:
        if f.getLookupInfo(lookup)[0] == 'gpos_mark2base':
            f.removeLookup(lookup)

    font_file = f.fontname + font_extension
    print "Generating", font_file
    f.generate(font_file, flags = generation_flags, layer = layer_to_use)
    print "Validating", font_file
    subprocess.call(["fontlint", font_file])

#--------------------------------------------------------------------------
