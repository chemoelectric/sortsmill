#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

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

AC_PREREQ([2.65])
AC_INIT([sortsmill], [0.0], [http://code.google.com/p/sortsmill/issues/])
AM_INIT_AUTOMAKE

python_dir=`pwd`/python
AC_SUBST(python_dir)

AC_ARG_ENABLE(families,
        [AS_HELP_STRING([--enable-families=FAM1,FAM2,...],
                        [build the listed font families (default is --enable-families=goudy)])],
        [build_families=${enableval}],
        [build_families=goudy])
test x"${build_families}" = x"no" && build_families=
build_families="`echo ${build_families} | sed -e 's/,/ /g'`"
AC_SUBST(build_families)

AC_ARG_ENABLE(opentype,
        [AS_HELP_STRING([--disable-opentype],
                        [do not build PostScript-flavored OpenType fonts (default is to build them)])],
        [build_opentype=${enableval}],
        [build_opentype=yes])
AC_ARG_ENABLE(truetype,
        [AS_HELP_STRING([--disable-truetype],
                        [do not build TrueType fonts (default is to build them)])],
        [build_truetype=${enableval}],
        [build_truetype=yes])
AC_ARG_ENABLE(ofl,
        [AS_HELP_STRING([--enable-ofl],
                        [build SIL Open Font License versions of fonts (default is not to build them)])],
        [build_ofl=${enableval}],
        [build_ofl=no])
AC_ARG_ENABLE(mit,
        [AS_HELP_STRING([--disable-mit],
                        [do not build MIT-license versions of fonts (default is to build them)])],
        [build_mit=${enableval}],
        [build_mit=yes])

truetype_func=nullify
test x"${build_truetype}" = x"yes" && truetype_func=truetype
AC_SUBST(truetype_func)

opentype_func=nullify
test x"${build_opentype}" = x"yes" && opentype_func=opentype
AC_SUBST(opentype_func)

ofl_func=nullify
test x"${build_ofl}" = x"yes" && ofl_func=ofl
AC_SUBST(ofl_func)

mit_func=nullify
test x"${build_mit}" = x"yes" && mit_func=mit
AC_SUBST(mit_func)


# Checks for programs.

python=${PYTHON:-python}
AC_SUBST(python,[${python}])
AC_CHECK_PROG(HAVE_PYTHON, [${python}], [yes])
test x"${HAVE_PYTHON}" = x"yes" || AC_MSG_ERROR([I need python.])

AC_MSG_CHECKING([that the python version is at least 2.6])
if ${python} -c 'import sys; 0x2060000 <= sys.hexversion or exit(1)' 2>/dev/null ; then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need python 2.6 or greater.])
fi

AC_CHECK_PROG(HAVE_FONTLINT, [fontlint], [yes])
test x"${HAVE_FONTLINT}" = x"yes" || AC_MSG_ERROR([I need fontlint, which is part of fontforge.])

AC_MSG_CHECKING([for the 'fontforge' python extension])
if ${python} -c 'import fontforge' 2>/dev/null ; then
   AC_MSG_RESULT([yes])
   HAVE_PYTHON=yes
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need the 'fontforge' extension.])
fi


# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.


AC_CONFIG_FILES([Makefile
                 goudy/Makefile
                 kis/Makefile])
AC_OUTPUT
