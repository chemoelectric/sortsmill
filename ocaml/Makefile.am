#  Copyright (c) 2011 Barry Schwartz
#
#  Permission is hereby granted, free of charge, to any person
#  obtaining a copy of this software and associated documentation
#  files (the "Software"), to deal in the Software without
#  restriction, including without limitation the rights to use,
#  copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the
#  Software is furnished to do so, subject to the following
#  conditions:
#
#  The above copyright notice and this permission notice shall be
#  included in all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
#  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#  OTHER DEALINGS IN THE SOFTWARE.

#--------------------------------------------------------------------------

SORTSMILL = sortsmill
PSMAT = psmat
FONTDESIGN = fontdesign

MLFIND = $(OCAMLFIND) $(OCAMLFINDFLAGS)
OCAMLC = $(OCAMLFIND) c $(OCAMLFINDFLAGS) -g
OCAMLOPT = $(OCAMLFIND) opt $(OCAMLFINDFLAGS) -g

OCAMLPACKAGES = camlp4,caml2geom,batteries,batteries.syntax,lacaml
OCAMLFINDFLAGS += -package $(OCAMLPACKAGES) -syntax camlp4o

EXTRA_DIST = META $(PSMAT).ml $(PSMAT).mli $(FONTDESIGN).ml	\
	$(FONTDESIGN).mli

#--------------------------------------------------------------------------

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $< -o $@

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $< -o $@

#--------------------------------------------------------------------------

all: $(addprefix $(SORTSMILL)., cma cmxa) $(PSMAT).cmi	\
	$(FONTDESIGN).cmi

$(addprefix $(FONTDESIGN)., cmi cmo): $(PSMAT).cmi

$(SORTSMILL).cma: $(PSMAT).cmo $(FONTDESIGN).cmo
	$(OCAMLC) -a -o $@ $^ $(LDFLAGS)

$(SORTSMILL).cmxa: $(PSMAT).cmx $(FONTDESIGN).cmx
	$(OCAMLOPT) -a -o $@ $^ $(LDFLAGS)

$(SORTSMILL).a: $(SORTSMILL).cmxa

install-data-local: all
	-$(OCAMLFIND) remove $(SORTSMILL)
	$(OCAMLFIND) install $(SORTSMILL) $(srcdir)/META \
		$(SORTSMILL).cma $(SORTSMILL).cmxa $(SORTSMILL).a \
		$(addprefix $(PSMAT)., cmi mli ml) \
		$(addprefix $(FONTDESIGN)., cmi mli ml)

uninstall-local:
	$(MLFIND) remove $(SORTSMILL)

MOSTLYCLEANFILES = \
	$(SORTSMILL).cmxa $(SORTSMILL).cma $(SORTSMILL).a \
	$(addprefix $(PSMAT)., cmx cmo cmi) \
	$(addprefix $(FONTDESIGN)., cmx cmo cmi) \
	ChangeLog

ChangeLog::
	hg log --style=changelog --only-branch=default "${top_srcdir}" > $@

#--------------------------------------------------------------------------
