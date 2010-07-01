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

FONTS = KisStM KisStM-Italic KisStM-Bold
FEA_KisStM = main locl desc liga generated_kerning
FEA_KisStM_Italic = main locl desc calt liga generated_kerning
FEA_KisStM_Bold = main locl desc generated_kerning

fontsdir = ${datarootdir}/fonts
fonts_DATA = $(call expand_fonts, ${FONTS})

EXTRA_DIST = \
	$(addsuffix .sfd, ${FONTS}) \
	$(addsuffix _build.py, ${FONTS}) \
	$(addsuffix _*.fea, ${FONTS})

MOSTLYCLEANFILES = ${fonts_DATA}
