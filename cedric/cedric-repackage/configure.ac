#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([CEDRIC], [97], [granger@ucar.edu])
AC_PREFIX_DEFAULT([/opt/local])
AC_CONFIG_SRCDIR([source/CUTILS.c])
AC_CONFIG_HEADERS([source/config.h])
AC_CONFIG_FILES([
Makefile
source/Makefile
])
AM_INIT_AUTOMAKE([])

# Checks for programs.
AC_PROG_CC
AC_PROG_F77([gfortran])
#AC_FC_PP_DEFINE([echo "FORTRAN preprocessing supported."],
#		      [AC_MSG_FAILURE(["FORTRAN preprocessing not supported."])])
#AC_FC_FIXEDFORM([echo "FORTRAN fixed form supported."],
#		      [AC_MSG_FAILURE(["FORTRAN fixed form not supported."])])

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h malloc.h memory.h netinet/in.h stdlib.h string.h strings.h sys/param.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_REALLOC
AC_FUNC_WAIT3
AC_CHECK_FUNCS([atexit bzero floor ftruncate gethostname gettimeofday memmove memset modf pow select strtoul])

AC_OUTPUT
