#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([CEDRIC], [97], [granger@ucar.edu])
AC_CONFIG_AUX_DIR([config])
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

# Checks for libraries.
NCARGDIR=
AC_ARG_WITH([ncarg],
 [AS_HELP_STRING([--with-ncarg],
[Enable support for plotting with NCAR Graphics.
 Set to the installation directory for NCAR Graphics, or else
 if no argument (--with-ncarg) then assume a system installation.
 Use --with-ncarg=no or --without-ncarg to disable NCAR Graphics plots.])],
 [if test "x$with_ncarg" != xyes; then NCARGDIR="$with_ncarg"; fi],
 [with_ncarg="yes"])
          
LIBNCARG=
AS_IF([test "x$with_ncarg" != xno],
 [AC_CHECK_LIB([ncarg], [main],
               [AC_SUBST([LIBNCARG], ["-lncarg -lncarg_gks -lncarg_ras -lncarg_c"])
	        AC_DEFINE([HAVE_LIBNCARG], [1],
                          [Define if you have libncarg])
               ],
               [AC_MSG_FAILURE(
                 [NCARG test failed (--without-ncarg to disable)])],
	       [-lncarg -lncarg_gks -lncarg_ras -lncarg_c])])


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
