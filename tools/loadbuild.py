"""
Copyright (c) 2009-2011 Barry Schwartz

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
import spacing_by_anchors
import tools
from os.path import exists
from imp import reload

def build_file_exists(font):
    build_file_name = font.fontname + "_build.py"
    return exists(build_file_name)

def load_build_by_fontname(font, fontname):
    module_name = fontname + "_build"
    module = __import__(module_name)
    reload(module)
    module.build_glyphs(None, font)

def load_build(font):
    load_build_by_fontname(font, font.fontname)

def load_build_thoroughly(font):
    spacing_by_anchors.clear_cached_data(font)
    load_build(font)
    load_build(font)
    load_build(font)
    tools.reautohint(font)

fontforge.registerMenuItem((lambda _, font: load_build(font)),
                           (lambda _, font: build_file_exists(font)),
                           None, "Font", "None", "Build glyphs")

fontforge.registerMenuItem((lambda _, font: load_build_thoroughly(font)),
                           (lambda _, font: build_file_exists(font)),
                           None, "Font", "None", "Thoroughly build glyphs")
