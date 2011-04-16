#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#--------------------------------------------------------------------------

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

AC_PREREQ([2.68])
AC_INIT([sortsmill-ocaml],[0.1],[http://code.google.com/p/sortsmill/issues/])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([fontdesign.ml])

# Checks for programs.
AC_ARG_VAR([OCAMLFIND], [ocamlfind command])
AC_ARG_VAR([OCAMLFINDFLAGS], [ocamlfind flags])

if test -z "${OCAMLFIND}" ; then
   AC_CHECK_PROGS(OCAMLFIND, [ocamlfind])
   if test -z "${OCAMLFIND}" ; then
      AC_MSG_ERROR([The ocamlfind program is required but not found. Giving up.])
   fi
fi

# Checks for libraries.
#AC_ARG_VAR([PA_INFIX], [full path to the pa_infix.cmo camlp4 module])
#
#if test -z "${PA_INFIX}" ; then
#   if ${OCAMLFIND} query pa_do >/dev/null ; then
#      if test -e "`${OCAMLFIND} query pa_do`/pa_infix.cmo" ; then
#         PA_INFIX="`${OCAMLFIND} query pa_do`/pa_infix.cmo"
#      fi
#   fi
#   if test -z "${PA_INFIX}" ; then
#      echo "The pa_infix camlp4 module is required but not found. Giving up."
#      exit 1
#   fi
#fi

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
