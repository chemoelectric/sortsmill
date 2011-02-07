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
FAMILYNAME = tofeus
FONTS = Tofeus
FEA_Tofeus = # case cpsp figures generated_kerning hist liga locl main sc

fonts_DATA = $(call expand_fonts, ${FONTS})

EXTRA_DIST = \
	$(addsuffix .sfd, ${FONTS})
#	$(addsuffix _build.py, ${FONTS}) \
#	${FEA_Tofeus:%=Tofeus_%.fea}

#Tofeus.otf: ${FEA_Tofeus:%=Tofeus_%.fea}
#TofeusTT.ttf: ${FEA_Tofeus:%=Tofeus_%.fea}

MOSTLYCLEANFILES = ${fonts_DATA} gdlerr.txt

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
