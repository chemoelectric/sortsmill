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

include rules.mk

.PHONY: all complete incomplete
all: complete
complete: ${COMPLETE}
incomplete: ${INCOMPLETE}

COMPLETE =
INCOMPLETE = ${KIS_FONTS}
MOSTLYCLEANFILES = ${COMPLETE} ${INCOMPLETE}

KIS_FONTS = \
	kis/KisStM.otf kis/KisStMTT.ttf kis/OFLKisStM.otf kis/OFLKisStMTT.ttf \
	kis/KisStM-Italic.otf kis/KisStMTT-Italic.ttf kis/OFLKisStM-Italic.otf kis/OFLKisStMTT-Italic.ttf \
	kis/KisStM-Bold.otf kis/KisStMTT-Bold.ttf kis/OFLKisStM-Bold.otf kis/OFLKisStMTT-Bold.ttf

${KIS_FONTS}:
	(cd kis; $(MAKE) $(basename $<))

.PHONY: install install-complete install-incomplete
install: install-complete
install-complete: ${COMPLETE}
	${CP} $^ ${FONTSDIR}
	${FC_CACHE} ${FONTSDIR}
install-incomplete: ${INCOMPLETE}
	${CP} $^ ${FONTSDIR}
	${FC_CACHE} ${FONTSDIR}
