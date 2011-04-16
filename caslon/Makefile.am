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

MLFIND = $(OCAMLFIND) $(OCAMLFINDFLAGS)
OCAMLC = $(OCAMLFIND) c $(OCAMLFINDFLAGS) -g
OCAMLOPT = $(OCAMLFIND) opt $(OCAMLFINDFLAGS) -g

OCAMLPACKAGES = camlp4,batteries,batteries.syntax,sortsmill
OCAMLFINDFLAGS += -package $(OCAMLPACKAGES) -syntax camlp4o

@ocaml_rules@
@sortsmill_binpack_rules@

%_program: %_program.ml
	$(OCAMLC) -thread -linkpkg $^ -o $@

FAMILYNAME = caslon
FONTS = CaslonStM
fonts_DATA = $(addsuffix .otf, ${FONTS})

#FONTDEPS = caslon_basics.asy caslon_font.asy caslon_output.asy	\
#	caslon_roman.asy caslon_version.asy
FONTDEPS =

EXTRA_DIST = $(addsuffix .ml, ${FONTS}) ${FONT_DEPS}

${fonts_DATA} : ${FONTDEPS}
MOSTLYCLEANFILES = ${fonts_DATA} # gdlerr.txt

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
