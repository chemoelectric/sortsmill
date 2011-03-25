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

@asymptote_rules@
@sortsmill_binpack_rules@

FAMILYNAME = cloister
FONTS6 = CloisterStM6 CloisterStM6-Medium CloisterStM6-Semibold	\
	CloisterStM6-Bold
FONTS8 = CloisterStM8 CloisterStM8-Medium CloisterStM8-Semibold	\
	CloisterStM8-Bold
FONTS10 = CloisterStM10 CloisterStM10-Medium CloisterStM10-Semibold	\
	CloisterStM10-Bold
FONTS12 = CloisterStM12 CloisterStM12-Medium CloisterStM12-Semibold	\
	CloisterStM12-Bold
FONTS14 = CloisterStM14 CloisterStM14-Medium CloisterStM14-Semibold	\
	CloisterStM14-Bold
FONTS = ${FONTS6} ${FONTS8} ${FONTS10} ${FONTS12} ${FONTS14}
fonts_DATA = $(addsuffix .otf, ${FONTS})

FONTDEPS = cloister_basics.asy cloister_glyphs.asy cloister_params.asy	\
	cloister_version.asy

EXTRA_DIST = $(addsuffix .asy, ${FONTS}) ${FONT_DEPS}

${fonts_DATA} : ${FONTDEPS}

MOSTLYCLEANFILES = ${fonts_DATA} # gdlerr.txt

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
