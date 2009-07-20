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
import os
from os.path import exists


def revised_fontname(fontname):
    s = ""
    for c in fontname:
        if c == "-":
            s += "_"
        else:
            s += c
    return s


def build_file_exists(bitbucket, font):
    build_file_name = font.fontname + "_build.py"
    return exists(build_file_name)


def load_build(bitbucket, font):
    build_file_name = font.fontname + "_build.py"
    f = open(build_file_name)
    exec f
    f.close()
    build_glyphs(bitbucket, font)


fontforge.registerMenuItem(load_build, build_file_exists,
                           None, "Font", "None", "Build glyphs")
