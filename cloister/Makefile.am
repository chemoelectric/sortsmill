# Copyright (c) 2011 Barry Schwartz
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
@sortsmill_mit_opentype_binpack_rules@

MKFONT_FLAGS = --infer-licensing --no-hints --no-instructions --ttf-emsize=4096 ${MKFONT_GRAPHITE_FLAGS}
FAMILYNAME = cloister

# TODO: Make CLoisterStM be 0.8*CloisterStM-Light +
# 0.2*CloisterStM-Bold, rather than directly edited.  Then the
# blending of the Semibold or the like also can be reformulated.
FONTS = CloisterStM-Light CloisterStM CloisterStM-Semibold CloisterStM-Bold

#FEA_CloisterStM = #case cpsp figures generated_kerning hist liga locl main sc
FEA_CloisterStM_Light = 
FEA_CloisterStM = generated_kerning liga main
FEA_CloisterStM_Bold = generated_kerning main
FEA_CloisterStM_Semibold = generated_kerning main

fonts_DATA = $(call expand_fonts, ${FONTS})

EXTRA_DIST = \
	$(addsuffix .sfd, ${FONTS}) \
	$(addsuffix _build.py, ${FONTS}) \
	${FEA_CloisterStM_Light:%=CloisterStM-Light_%.fea} \
	${FEA_CloisterStM:%=CloisterStM_%.fea} \
	${FEA_CloisterStM_Bold:%=CloisterStM-Bold_%.fea} \
	${FEA_CloisterStM_Semibold:%=CloisterStM-Semibold_%.fea}

CloisterStM-Semibold.sfd: CloisterStM-Light.sfd CloisterStM-Bold.sfd blend_semibold.py
	./blend_semibold.py

CloisterStM.otf: ${FEA_CloisterStM:%=CloisterStM_%.fea}
CloisterStMTT.ttf: ${FEA_CloisterStM:%=CloisterStM_%.fea}
CloisterStM-Bold.otf: ${FEA_CloisterStM_Bold:%=CloisterStM-Bold_%.fea}
CloisterStMTT-Bold.ttf: ${FEA_CloisterStM_Bold:%=CloisterStM-Bold_%.fea}
CloisterStM-Semibold.otf: ${FEA_CloisterStM_Semibold:%=CloisterStM-Semibold_%.fea}
CloisterStMTT-Semibold.ttf: ${FEA_CloisterStM_Semibold:%=CloisterStM-Semibold_%.fea}

MOSTLYCLEANFILES = ${fonts_DATA} gdlerr.txt
CLEANFILES = CloisterStM-Semibold.sfd

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
