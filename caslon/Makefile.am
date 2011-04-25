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

if USE_OCAMLOPT
OCAMLCOMPILER = $(OCAMLOPT)
CMO = cmx
else
OCAMLCOMPILER = $(OCAMLC)
CMO = cmo
endif

@ocaml_rules@
@sortsmill_binpack_rules@

# Let's not bother writing interface files. (Note: make may issue a
# warning about circular dependency.)
%.cmi: %.cmo ; @:
%.cmi: %.cmx ; @:

CaslonStM_program: CaslonStM_program.ml caslon_roman.$(CMO)
	$(OCAMLCOMPILER) -thread -linkpkg caslon_roman.$(CMO) $< -o $@

CaslonStM-Bold_program: CaslonStM_Bold_program.ml caslon_roman.$(CMO)
	$(OCAMLCOMPILER) -thread -linkpkg caslon_roman.$(CMO) $< -o $@

%_glyph_update: Makefile
	echo '#!/bin/sh' > $@
	echo 'make '$*'_program 1>/dev/null || exit $$?' >> $@
	echo 'exec ./'$*'_program --glyph=$${1}' >> $@
	chmod +x $@

FAMILYNAME = caslon
FONTS = CaslonStM CaslonStM-Bold
fonts_DATA = $(addsuffix .otf, ${FONTS})

dist_bin_SCRIPTS = $(addsuffix _glyph_update, ${FONTS})

EXTRA_DIST = $(addsuffix _program.ml, ${FONTS}) caslon_roman.ml

$(addsuffix _program.$(CMO), ${FONTS}): $(addprefix caslon_roman., $(CMO) cmi)

MOSTLYCLEANFILES = ${fonts_DATA} $(addsuffix _program, ${FONTS})		\
	$(addsuffix _program.$(CMO), ${FONTS}) $(addsuffix _program.cmi,	\
	${FONTS}) $(addprefix caslon_roman., $(CMO) cmi)

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@ 

DISTCLEANFILES = ChangeLog
