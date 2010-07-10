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

#serial 2

# STM_DISABLE_OPENTYPE
# --------------------
AC_DEFUN([STM_DISABLE_OPENTYPE],
[AC_ARG_ENABLE(opentype,
        [AS_HELP_STRING([--disable-opentype],
                        [do not build PostScript-flavored OpenType fonts (default is to build them)])],
        [build_opentype=${enableval}],
        [build_opentype=yes])
opentype_func=nullify
test x"${build_opentype}" = x"yes" && opentype_func=opentype
AC_SUBST(opentype_func)])

# STM_DISABLE_TRUETYPE
# --------------------
AC_DEFUN([STM_DISABLE_TRUETYPE],
[AC_ARG_ENABLE(truetype,
        [AS_HELP_STRING([--disable-truetype],
                        [do not build TrueType fonts (default is to build them)])],
        [build_truetype=${enableval}],
        [build_truetype=yes])
truetype_func=nullify
test x"${build_truetype}" = x"yes" && truetype_func=truetype
AC_SUBST(truetype_func)])

# STM_ENABLE_OFL
# --------------
AC_DEFUN([STM_ENABLE_OFL],
[AC_ARG_ENABLE(ofl,
        [AS_HELP_STRING([--enable-ofl],
                        [build SIL Open Font License versions of fonts (default is not to build them)])],
        [build_ofl=${enableval}],
        [build_ofl=no])
ofl_func=nullify
test x"${build_ofl}" = x"yes" && ofl_func=ofl
AC_SUBST(ofl_func)
])

# STM_DISABLE_MIT
# ---------------
AC_DEFUN([STM_DISABLE_MIT],
[AC_ARG_ENABLE(mit,
        [AS_HELP_STRING([--disable-mit],
                        [do not build MIT-license versions of fonts (default is to build them)])],
        [build_mit=${enableval}],
        [build_mit=yes])
mit_func=nullify
test x"${build_mit}" = x"yes" && mit_func=mit
AC_SUBST(mit_func)
])

# STM_DISABLE_GRAPHITE
# --------------------
AC_DEFUN([STM_DISABLE_GRAPHITE],
[AC_ARG_ENABLE(graphite,
        [AS_HELP_STRING([--disable-graphite],
                        [do not include Graphite support in TrueType fonts (default is to include Graphite support)])],
        [build_graphite=${enableval}],
        [build_graphite=yes])
])

# STM_SORTSMILL_ENABLES
# ---------------------
AC_DEFUN([STM_SORTSMILL_ENABLES],
[STM_DISABLE_OPENTYPE
STM_DISABLE_TRUETYPE
STM_ENABLE_OFL
STM_DISABLE_MIT
STM_DISABLE_GRAPHITE])

# STM_SORTSMILL_NONLICENSE_ENABLES
# --------------------------------
AC_DEFUN([STM_SORTSMILL_NONLICENSE_ENABLES],
[STM_DISABLE_OPENTYPE
STM_DISABLE_TRUETYPE])

# STM_PROG_FONTLINT
# -----------------
AC_DEFUN([STM_PROG_FONTLINT],
[AC_CHECK_PROG(HAVE_FONTLINT, [fontlint], [yes])
test x"${HAVE_FONTLINT}" = x"yes" || AC_MSG_ERROR([I need fontlint, which is part of fontforge.])])

# STM_PROG_GRCOMPILER
# -------------------
AC_DEFUN([STM_PROG_GRCOMPILER],
[if test x"${build_graphite}" = x"yes" -a x"${build_truetype}" = x"yes" ; then
AC_CHECK_PROG(HAVE_GRCOMPILER, [grcompiler], [yes])
test x"${HAVE_GRCOMPILER}" = x"yes" || AC_MSG_ERROR([Graphite compiler not found. Add \"--disable-graphite\" to build TrueType without Graphite support.])
fi
])

# STM_CHECK_FONTFORGE_EXTENSION
# -----------------------------
AC_DEFUN([STM_CHECK_FONTFORGE_EXTENSION],
[AC_MSG_CHECKING([for the 'fontforge' extension for Python])
if ${PYTHON} -c 'import fontforge' 2>/dev/null ; then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need the 'fontforge' extension.])
fi])

# STM_FONTSDIR
# ------------
AC_DEFUN([STM_FONTSDIR],
[AC_ARG_WITH(fontsdir,
        [AS_HELP_STRING([--with-fontsdir=DIR],
                        [directory for generated fonts [DIR=DATAROOTDIR/fonts/sortsmill]])],
        [fontsdir=${withval}],
        [fontsdir=${datarootdir}/fonts/sortsmill])
AC_SUBST([fontsdir])
])

# STM_SORTSMILL_PREREQUISITES
# ---------------------------
AC_DEFUN([STM_SORTSMILL_PREREQUISITES],
[AM_PATH_PYTHON([2.6])
AC_REQUIRE([STM_SORTSMILL_ENABLES])
AC_REQUIRE([STM_PROG_FONTLINT])
AC_REQUIRE([STM_PROG_GRCOMPILER])
AC_REQUIRE([STM_FONTSDIR])
])

# STM_SORTSMILL_NONLICENSE_PREREQUISITES
# --------------------------------------
AC_DEFUN([STM_SORTSMILL_NONLICENSE_PREREQUISITES],
[AM_PATH_PYTHON([2.6])
AC_REQUIRE([STM_SORTSMILL_NONLICENSE_ENABLES])
AC_REQUIRE([STM_PROG_FONTLINT])
AC_REQUIRE([STM_PROG_GRCOMPILER])
AC_REQUIRE([STM_FONTSDIR])
])

# STM_INIT_SORTSMILL
# ------------------
AC_DEFUN([STM_INIT_SORTSMILL],
[
AC_SUBST([nullify],[])
AC_SUBST([opentype],['$(foreach f, ${1}, ${f}.otf)'])
AC_SUBST([truetype],['$(foreach f, ${1}, $(shell echo ${f} | sed -e "/-/{s/\(.*\)\(-.*\)/\1TT\2.ttf/; p; d};s/\(.*\)/\1TT.ttf/"))'])
AC_SUBST([mit],['${1}'])
AC_SUBST([ofl],['$(foreach f, ${1}, OFL${f})'])

AC_SUBST([MAKEFONTS],[make-fonts])
AC_SUBST([MKFONT],['${MAKEFONTS} --input-directory=${srcdir} --output-directory=${builddir} ${MKFONT_FLAGS}'])

AC_SUBST([expand_fonts],[' \
	$(call ${mit_func}, $(call ${opentype_func}, ${1})) \
	$(call ${mit_func}, $(call ${truetype_func}, ${1})) \
	$(call ${ofl_func}, $(call ${opentype_func}, ${1})) \
	$(call ${ofl_func}, $(call ${truetype_func}, ${1}))'])
AC_SUBST([expand_fonts_without_license],[' \
	$(call opentype, ${1}) \
	$(call truetype, ${1})'])
AC_SUBST([expand_mit_fonts],[' \
	$(call mit, $(call opentype, ${1})) \
	$(call mit, $(call truetype, ${1}))'])
AC_SUBST([expand_ofl_fonts],[' \
	$(call ofl, $(call opentype, ${1})) \
	$(call ofl, $(call truetype, ${1}))'])
AC_SUBST([expand_nonlicense_fonts],[' \
	$(call ${opentype_func}, ${1}) \
	$(call ${truetype_func}, ${1})'])

AM_SUBST_NOTMAKE(sortsmill_rules)
AC_SUBST([sortsmill_rules],['
OFL%.otf              : %.sfd  ; ${MKFONT} $(basename [$]@)
%.otf                 : %.sfd  ; ${MKFONT} $(basename [$]@)
OFL%TT.ttf            : %.sfd  ; ${MKFONT} $(basename [$]@)
%TT.ttf               : %.sfd  ; ${MKFONT} $(basename [$]@)

# Is there a more general and simple way to handle the following
# within make?
OFL%TT-Italic.ttf     : %-Italic.sfd  ; ${MKFONT} $(basename [$]@)
%TT-Italic.ttf        : %-Italic.sfd  ; ${MKFONT} $(basename [$]@)
OFL%TT-Bold.ttf       : %-Bold.sfd    ; ${MKFONT} $(basename [$]@)
%TT-Bold.ttf          : %-Bold.sfd    ; ${MKFONT} $(basename [$]@)
OFL%TT-BoldItalic.ttf : %-BoldItalic.sfd  ; ${MKFONT} $(basename [$]@)
%TT-BoldItalic.ttf    : %-BoldItalic.sfd  ; ${MKFONT} $(basename [$]@)
'
])
])

# STM_TARGET_MIT_BINPACK
# ----------------------
AC_DEFUN([STM_TARGET_MIT_BINPACK],[
AC_SUBST([MIT_BINPACK_FILES],['${srcdir}/COPYING $(call expand_mit_fonts, ${FONTS})'])
AC_SUBST([MIT_BINPACK],['${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_mit_binpack_rules])
AC_SUBST([sortsmill_mit_binpack_rules],['
${MIT_BINPACK}: ${MIT_BINPACK_FILES}
	rm -f ${MIT_BINPACK}
	zip -j ${MIT_BINPACK} ${MIT_BINPACK_FILES}
'
])
])

# STM_TARGET_OFL_BINPACK
# ----------------------
AC_DEFUN([STM_TARGET_OFL_BINPACK],[
AC_SUBST([OFL_BINPACK_FILES],
    ['${srcdir}/OFL/FONTLOG.txt ${srcdir}/OFL/OFL.txt ${srcdir}/OFL/OFL-FAQ.txt \
      $(call expand_ofl_fonts, ${FONTS})'])
AC_SUBST([OFL_BINPACK],['ofl-${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_ofl_binpack_rules])
AC_SUBST([sortsmill_ofl_binpack_rules],['
${OFL_BINPACK}: ${OFL_BINPACK_FILES}
	rm -f ${OFL_BINPACK}
	zip -j ${OFL_BINPACK} ${OFL_BINPACK_FILES}
'
])
])

# STM_TARGET_NONLICENSE_BINPACK
# -----------------------------
AC_DEFUN([STM_TARGET_NONLICENSE_BINPACK],[
AC_SUBST([NONLICENSE_BINPACK_FILES],['${srcdir}/COPYING $(call expand_nonlicense_fonts, ${FONTS})'])
AC_SUBST([NONLICENSE_BINPACK],['${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_nonlicense_binpack_rules])
AC_SUBST([sortsmill_nonlicense_binpack_rules],['
${NONLICENSE_BINPACK}: ${NONLICENSE_BINPACK_FILES}
	rm -f ${NONLICENSE_BINPACK}
	zip -j ${NONLICENSE_BINPACK} ${NONLICENSE_BINPACK_FILES}
'
])
])
