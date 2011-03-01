#!/usr/bin/env python

import fontforge
import sortsmill
import sys

from sortsmill import fontblend
from sortsmill import *
from sortsmill.spacing_by_anchors import space_selected_by_anchors
from sortsmill.spacing_by_anchors import generate_kerning_and_read_features

weight = 0.5
f1 = fontforge.open('CloisterStM.sfd')
f2 = fontforge.open('CloisterStM-Bold.sfd')

f = fontforge.font()

# FIX: Perhaps there is a better way to set the version here.
f.version = f1.version

f.copyright = 'Copyright (c) 2011 Barry Schwartz'

f.fontname = 'CloisterStM-Semibold'
f.familyname = 'Sorts Mill Cloister'
f.fullname = 'Sorts Mill Cloister Semibold'
f.weight = 'Semibold'

lang = 'English (US)'
f.appendSFNTName(lang, 'Family', 'Sorts Mill Cloister Semibold')
f.appendSFNTName(lang, 'SubFamily', 'Regular')
f.appendSFNTName(lang, 'Preferred Family', 'Sorts Mill Cloister')
f.appendSFNTName(lang, 'Preferred Styles', 'Semibold')

fontblend.interpolate_glyphs(f, f1, f2, weight)
fontblend.interpolate_glyphs_anchor_points(f, f1, f2, weight)
f.encoding = 'UnicodeBMP'

f.selection.all()
space_selected_by_anchors(f)
f.selection.none()

generate_kerning_and_read_features(None, f)

f.save()
