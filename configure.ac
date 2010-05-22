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

# Checks for programs.

python=${PYTHON:-python}
AC_SUBST(python,[${python}])
AC_CHECK_PROG(HAVE_PYTHON, [${python}], [yes])
[[ "${HAVE_PYTHON}" == "yes" ]] || AC_MSG_ERROR([I need python.])

AC_MSG_CHECKING([that the python version is at least 2.6])
if ${python} -c 'import sys; 0x2060000 <= sys.hexversion or exit(1)' 2>/dev/null ; then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need python 2.6 or greater.])
fi

AC_CHECK_PROG(HAVE_FONTLINT, [fontlint], [yes])
[[ "${HAVE_FONTLINT}" == "yes" ]] || AC_MSG_ERROR([I need fontlint, which is part of fontforge.])

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
                 kis/Makefile])
AC_OUTPUT
