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
from os.path import exists

#--------------------------------------------------------------------------

def merge_pair_positioning_subtables(font):
    for lookup in font.gpos_lookups:
        if font.getLookupInfo(lookup)[0] == 'gpos_pair':
            subtables = font.getLookupSubtables(lookup)
            i = 0
            while i < len(subtables) and font.isKerningClass(subtables[i]):
                i += 1
            for subtab in subtables[i + 1:]:
                if not font.isKerningClass(subtab):
                    font.mergeLookupSubtables(subtables[i], subtab)

#--------------------------------------------------------------------------

def feature_file_exists(bitbucket, font):
    feature_file_name = font.fontname + "_main.fea"
    return exists(feature_file_name)

def read_features(bitbucket, font):
    feature_file_name = font.fontname + "_main.fea"
    font.mergeFeature(feature_file_name)
#    merge_pair_positioning_subtables(font) # FIX/TODO: This seems to trigger a metrics view bug in FontForge.
#                                           # I need to investigate and report. Meanwhile, put the call in
#                                           # make-fonts.py, or just not bother.


def erase_and_read_features(bitbucket, font):
    all_lookups = font.gpos_lookups + font.gsub_lookups
    for lookup in all_lookups:
        if font.getLookupInfo(lookup)[0] <> 'gpos_mark2base':
            font.removeLookup(lookup)
    read_features(bitbucket, font)
    font.buildOrReplaceAALTFeatures()

fontforge.registerMenuItem(erase_and_read_features,
                           feature_file_exists, None, "Font", "None",
                           "Delete lookups and read _main feature file")

#--------------------------------------------------------------------------
