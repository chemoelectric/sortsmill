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

PYTHON   = python
MAKEFONT = ${PYTHON} make-fonts.py
CP       = cp -v
FC_CACHE = fc-cache -r -v
FONTS_DESTDIR = `echo $${SORTSMILL_FONTS_DESTDIR:-"$${HOME}/.fonts/."}`

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
