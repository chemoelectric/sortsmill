# Copyright (c) 2010 Barry Schwartz
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

ACLOCAL_AMFLAGS = -I m4 --install

@sortsmill_rules@
@sortsmill_mit_binpack_rules@
@sortsmill_ofl_binpack_rules@

MKFONT_FLAGS = --infer-licensing --ttf-emsize=4096 ${MKFONT_GRAPHITE_FLAGS}
FAMILYNAME = kis
FONTS = KisStM KisStM-Italic KisStM-Bold
FEA_KisStM = main locl desc liga generated_kerning
FEA_KisStM_Italic = main locl desc calt liga generated_kerning
FEA_KisStM_Bold = main locl desc generated_kerning
GDL_KisStM = KisStM.gdl KisStM_generated_kerning.gdl KisStM_features.gdh
GDL_KisStM_Italic = KisStM-Italic.gdl KisStM-Italic_generated_kerning.gdl KisStM_features.gdh
GDL_KisStM_Bold =

fonts_DATA = $(call expand_fonts, ${FONTS})

EXTRA_DIST = \
	OFL/OFL-FAQ.txt OFL/OFL.txt \
	$(addsuffix .sfd, ${FONTS}) \
	$(addsuffix _build.py, ${FONTS}) \
	${FEA_KisStM:%=KisStM_%.fea} \
	${FEA_KisStM_Italic:%=KisStM-Italic_%.fea} \
	${FEA_KisStM_Bold:%=KisStM-Bold_%.fea} \
	${GDL_KisStM} \
	${GDL_KisStM_Italic} \
	${GDL_KisStM_Bold}

KisStM.otf OFLKisStM.otf: ${FEA_KisStM:%=KisStM_%.fea}
KisStMTT.ttf OFLKisStMTT.ttf: ${FEA_KisStM:%=KisStM_%.fea} ${GDL_KisStM}
KisStM-Italic.otf OFLKisStM-Italic.otf: ${FEA_KisStM:%=KisStM-Italic_%.fea}
KisStMTT-Italic.ttf OFLKisStMTT-Italic.ttf: ${FEA_KisStM_Italic:%=KisStM-Italic_%.fea} ${GDL_KisStM_Italic}
KisStM-Bold.otf OFLKisStM-Bold.otf: ${FEA_KisStM:%=KisStM-Bold_%.fea}
KisStMTT-Bold.ttf OFLKisStMTT-Bold.ttf: ${FEA_KisStM_Bold:%=KisStM-Bold_%.fea} ${GDL_KisStM_Bold}

MOSTLYCLEANFILES = ${fonts_DATA} gdlerr.txt

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
