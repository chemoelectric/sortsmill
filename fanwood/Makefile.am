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
@sortsmill_mit_opentype_binpack_rules@

MKFONT_FLAGS = --infer-licensing --no-instructions --ttf-emsize=4096 ${MKFONT_GRAPHITE_FLAGS}
FAMILYNAME = fanwood
FONTS = Fanwood Fanwood-Italic FanwoodText FanwoodText-Italic
FEA_Fanwood = case cpsp figures generated_kerning hist liga locl main sc
FEA_Fanwood_Italic = case cpsp figures generated_kerning hist liga locl main
FEA_FanwoodText = main generated_kerning
FEA_FanwoodText_Italic = main generated_kerning

fonts_DATA = $(call expand_fonts, ${FONTS})

EXTRA_DIST = \
	$(addsuffix .sfd, ${FONTS}) \
	$(addsuffix _build.py, ${FONTS}) \
	${FEA_Fanwood:%=Fanwood_%.fea} \
	${FEA_Fanwood_Italic:%=Fanwood-Italic_%.fea} \
	${FEA_FanwoodText:%=FanwoodText_%.fea} \
	${FEA_FanwoodText_Italic:%=FanwoodText-Italic_%.fea}

Fanwood.otf: ${FEA_Fanwood:%=Fanwood_%.fea}
FanwoodTT.ttf: ${FEA_Fanwood:%=Fanwood_%.fea}
Fanwood-Italic.otf: ${FEA_Fanwood_Italic:%=Fanwood-Italic_%.fea}
FanwoodTT-Italic.ttf: ${FEA_Fanwood_Italic:%=Fanwood-Italic_%.fea}
FanwoodText.otf: ${FEA_FanwoodText:%=FanwoodText_%.fea}
FanwoodTextTT.ttf: ${FEA_FanwoodText:%=FanwoodText_%.fea}
FanwoodText-Italic.otf: ${FEA_FanwoodText_Italic:%=FanwoodText-Italic_%.fea}
FanwoodTextTT-Italic.ttf: ${FEA_FanwoodText_Italic:%=FanwoodText-Italic_%.fea}

MOSTLYCLEANFILES = ${fonts_DATA} gdlerr.txt

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
