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

MKFONT_FLAGS = --infer-licensing --no-instructions --ttf-emsize=4096 ${MKFONT_GRAPHITE_FLAGS}
FAMILYNAME = lindenhill
FONTS = LindenHill LindenHill-Italic LindenHill-Bold
FEA_LindenHill = case cpsp figures generated_kerning hist liga locl main sc
FEA_LindenHill_Italic = case cpsp figures generated_kerning hist liga locl main stylistic
FEA_LindenHill_Bold = case cpsp figures generated_kerning hist liga locl main

fonts_DATA = $(call expand_fonts, ${FONTS})

EXTRA_DIST = \
	$(addsuffix .sfd, ${FONTS}) \
	$(addsuffix _build.py, ${FONTS}) \
	${FEA_LindenHill:%=LindenHill_%.fea} \
	${FEA_LindenHill_Italic:%=LindenHill-Italic_%.fea} \
	${FEA_LindenHill_Bold:%=LindenHill-Bold_%.fea}

LindenHill.otf: ${FEA_LindenHill:%=LindenHill_%.fea}
LindenHillTT.ttf: ${FEA_LindenHill:%=LindenHill_%.fea}
LindenHill-Italic.otf: ${FEA_LindenHill_Italic:%=LindenHill-Italic_%.fea}
LindenHillTT-Italic.ttf: ${FEA_LindenHill_Italic:%=LindenHill-Italic_%.fea}
LindenHill-Bold.otf: ${FEA_LindenHill_Bold:%=LindenHill-Bold_%.fea}
LindenHillTT-Bold.ttf: ${FEA_LindenHill_Bold:%=LindenHill-Bold_%.fea}

MOSTLYCLEANFILES = ${fonts_DATA} gdlerr.txt

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
