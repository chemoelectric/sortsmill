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

include ../rules.mk

OTF     = KisStM.otf KisStM-Italic.otf
OFL_OTF = OFLKisStM.otf OFLKisStM-Italic.otf
TTF     = KisStMTT.ttf KisStMTT-Italic.ttf
OFL_TTF = OFLKisStMTT.ttf OFLKisStMTT-Italic.ttf

ALL = ${OTF} ${OFL_OTF} ${TTF} ${OFL_TTF}
MOSTLYCLEANFILES = ${ALL}

all : ${ALL}

install : ${ALL}
	${CP} $^ ${FONTSDIR}
	${FC_CACHE} ${FONTSDIR}
