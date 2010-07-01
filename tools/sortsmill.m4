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

#serial 1

# AC_SORTSMILL_DISABLE_OPENTYPE
# -----------------------------
AC_DEFUN([AC_SORTSMILL_DISABLE_OPENTYPE],
[AC_ARG_ENABLE(opentype,
        [AS_HELP_STRING([--disable-opentype],
                        [do not build PostScript-flavored OpenType fonts (default is to build them)])],
        [build_opentype=${enableval}],
        [build_opentype=yes])
opentype_func=nullify
test x"${build_opentype}" = x"yes" && opentype_func=opentype
AC_SUBST(opentype_func)])

# AC_SORTSMILL_DISABLE_TRUETYPE
# -----------------------------
AC_DEFUN([AC_SORTSMILL_DISABLE_TRUETYPE],
[AC_ARG_ENABLE(truetype,
        [AS_HELP_STRING([--disable-truetype],
                        [do not build TrueType fonts (default is to build them)])],
        [build_truetype=${enableval}],
        [build_truetype=yes])
truetype_func=nullify
test x"${build_truetype}" = x"yes" && truetype_func=truetype
AC_SUBST(truetype_func)])

# AC_SORTSMILL_ENABLE_OFL
# -----------------------
AC_DEFUN([AC_SORTSMILL_ENABLE_OFL],
[AC_ARG_ENABLE(ofl,
        [AS_HELP_STRING([--enable-ofl],
                        [build SIL Open Font License versions of fonts (default is not to build them)])],
        [build_ofl=${enableval}],
        [build_ofl=no])
ofl_func=nullify
test x"${build_ofl}" = x"yes" && ofl_func=ofl
AC_SUBST(ofl_func)])

# AC_SORTSMILL_DISABLE_MIT
# ------------------------
AC_DEFUN([AC_SORTSMILL_DISABLE_MIT],
[AC_ARG_ENABLE(mit,
        [AS_HELP_STRING([--disable-mit],
                        [do not build MIT-license versions of fonts (default is to build them)])],
        [build_mit=${enableval}],
        [build_mit=yes])
mit_func=nullify
test x"${build_mit}" = x"yes" && mit_func=mit
AC_SUBST(mit_func)])

# AC_SORTSMILL_ENABLES
# --------------------
AC_DEFUN([AC_SORTSMILL_ENABLES],
[AC_SORTSMILL_DISABLE_OPENTYPE
AC_SORTSMILL_DISABLE_TRUETYPE
AC_SORTSMILL_ENABLE_OFL
AC_SORTSMILL_DISABLE_MIT])

# AC_PROG_FONTLINT
# ----------------
AC_DEFUN([AC_PROG_FONTLINT],
[AC_CHECK_PROG(HAVE_FONTLINT, [fontlint], [yes])
test x"${HAVE_FONTLINT}" = x"yes" || AC_MSG_ERROR([I need fontlint, which is part of fontforge.])])

# AC_CHECK_FONTFORGE_EXTENSION
# ----------------------------
AC_DEFUN([AC_CHECK_FONTFORGE_EXTENSION],
[AC_MSG_CHECKING([for the 'fontforge' extension for Python])
if ${PYTHON} -c 'import fontforge' 2>/dev/null ; then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need the 'fontforge' extension.])
fi])

# AC_SORTSMILL_PREREQUISITES
# --------------------------
AC_DEFUN([AC_SORTSMILL_PREREQUISITES],
[AM_PATH_PYTHON([2.6])
AC_REQUIRE([AC_SORTSMILL_ENABLES])
AC_REQUIRE([AC_PROG_FONTLINT])])

# AM_INIT_SORTSMILL
# -----------------
m4_define([AM_INIT_SORTSMILL],
[m4_syscmd(echo > sortsmill-rules.am '# generated automatically by autoconf

MAKEFONTS = make-fonts
DISTCLEANFILES = sortsmill-rules.am

nullify  =
opentype = $(foreach f, ${1}, ${f}.otf)
truetype = $(foreach f, ${1}, $(shell echo ${f} | sed -e "/-/{s/\(.*\)\(-.*\)/\1TT\2.ttf/; p; d};s/\(.*\)/\1TT.ttf/"))
mit      = ${1}
ofl      = $(foreach f, ${1}, OFL${f})

expand_fonts = \
	$(call @mit_func@, $(call @opentype_func@, ${1})) \
	$(call @ofl_func@, $(call @opentype_func@, ${1})) \
	$(call @mit_func@, $(call @truetype_func@, ${1})) \
	$(call @ofl_func@, $(call @truetype_func@, ${1}))

OFL%.otf              : %.sfd  ; ${MAKEFONTS} $(basename [$]@)
%.otf                 : %.sfd  ; ${MAKEFONTS} $(basename [$]@)
OFL%TT.ttf            : %.sfd  ; ${MAKEFONTS} $(basename [$]@)
%TT.ttf               : %.sfd  ; ${MAKEFONTS} $(basename [$]@)

# Is there a more general and simple way to handle the following
# within make?
OFL%TT-Italic.ttf     : %-Italic.sfd  ; ${MAKEFONTS} $(basename [$]@)
%TT-Italic.ttf        : %-Italic.sfd  ; ${MAKEFONTS} $(basename [$]@)
OFL%TT-Bold.ttf       : %-Bold.sfd  ; ${MAKEFONTS} $(basename [$]@)
%TT-Bold.ttf          : %-Bold.sfd  ; ${MAKEFONTS} $(basename [$]@)
OFL%TT-BoldItalic.ttf : %-BoldItalic.sfd  ; ${MAKEFONTS} $(basename [$]@)
%TT-BoldItalic.ttf    : %-BoldItalic.sfd  ; ${MAKEFONTS} $(basename [$]@)
'
)
