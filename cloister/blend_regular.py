#!/usr/bin/env python

import fontforge
import sortsmill
import sys

from sortsmill import fontblend
from sortsmill import *
from sortsmill.spacing_by_anchors import space_selected_by_anchors
from sortsmill.spacing_by_anchors import generate_kerning_and_read_features

weight = 0.2
f1 = fontforge.open('CloisterStM-Light.sfd')
f2 = fontforge.open('CloisterStM-Bold.sfd')

########################################################
for g in f1:
    if g not in ['a', 'b', 'c', 'd', 'e']:
        f1.removeGlyph(g)
########################################################

f = fontforge.font()

# FIX: Perhaps there is a better way to set the version here.
f.version = f1.version

f.copyright = 'Copyright (c) 2011 Barry Schwartz'

f.fontname = 'CloisterStM-NOTYETUSED'
f.familyname = 'Sorts Mill Cloister NOTYETUSED'
f.fullname = 'Sorts Mill Cloister NOTYETUSED'
f.weight = 'Regular'

lang = 'English (US)'
f.appendSFNTName(lang, 'Family', 'Sorts Mill Cloister')
f.appendSFNTName(lang, 'SubFamily', 'Regular')

fontblend.interpolate_glyphs(f, f1, f2, weight)
fontblend.interpolate_glyphs_anchor_points(f, f1, f2, weight)
f.encoding = 'UnicodeBMP'

f.selection.all()
f.round()
#space_selected_by_anchors(f)
f.selection.none()

#generate_kerning_and_read_features(None, f)

f.save()
