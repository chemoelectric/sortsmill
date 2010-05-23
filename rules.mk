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

PYTHON    = @python@
PYTHONDIR = @python_dir@
MAKEFONT  = PYTHONPATH="${PYTHONDIR}$(if ${PYTHONPATH},:${PYTHONPATH})" ${PYTHON} make-fonts.py

fontsdir   = ${datarootdir}/fonts

nullify  =
opentype = ${1}
truetype = $(foreach f, ${1}, $(shell echo ${f} | sed -e 's/\(.*\)\(-.*\)\.otf/\1TT\2.ttf/;s/\(.*\)\.otf/\1TT.ttf/'))
mit      = ${1}
ofl      = $(foreach f, ${1}, OFL${f})

expand_fonts = \
	$(call @mit_func@, $(call @opentype_func@, ${1})) \
	$(call @ofl_func@, $(call @opentype_func@, ${1})) \
	$(call @mit_func@, $(call @truetype_func@, ${1})) \
	$(call @ofl_func@, $(call @truetype_func@, ${1}))
list_fonts        = $(call expand_fonts, $(shell cat ${1}))
list_fonts_in_dir = $(addprefix ${1}/, $(call expand_fonts, $(shell cat ${1}/fonts)))

OFL%.otf              : %.sfd  ; ${MAKEFONT} $(basename $@)
%.otf                 : %.sfd  ; ${MAKEFONT} $(basename $@)
OFL%TT.ttf            : %.sfd  ; ${MAKEFONT} $(basename $@)
%TT.ttf               : %.sfd  ; ${MAKEFONT} $(basename $@)

# Is there a more general and simple way to handle the following
# within make?
OFL%TT-Italic.ttf     : %-Italic.sfd  ; ${MAKEFONT} $(basename $@)
%TT-Italic.ttf        : %-Italic.sfd  ; ${MAKEFONT} $(basename $@)
OFL%TT-Bold.ttf       : %-Bold.sfd  ; ${MAKEFONT} $(basename $@)
%TT-Bold.ttf          : %-Bold.sfd  ; ${MAKEFONT} $(basename $@)
OFL%TT-BoldItalic.ttf : %-BoldItalic.sfd  ; ${MAKEFONT} $(basename $@)
%TT-BoldItalic.ttf    : %-BoldItalic.sfd  ; ${MAKEFONT} $(basename $@)
