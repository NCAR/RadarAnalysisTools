#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([CEDRIC], [97], [granger@ucar.edu])
AC_CONFIG_AUX_DIR([config])
AC_PREFIX_DEFAULT([/opt/local])
AC_CONFIG_SRCDIR([source/CUTILS.c])
# Use a config header to avoid cluttering the compiler command line with
# all the symbol definitions.
AC_CONFIG_HEADERS([source/config.h])
AC_CONFIG_FILES([
Makefile
source/Makefile
])
AM_INIT_AUTOMAKE

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

AC_DEFINE([CEDRIC_USE_NCARG], [0], [Define to 1 to use NCAR Graphics])
LIBNCARG=
AS_IF([test "x$with_ncarg" != xno],
 [AC_CHECK_LIB([ncarg], [main],
               [AC_SUBST([LIBNCARG], ["-lncarg -lncarg_gks -lncarg_ras -lncarg_c"])
	        AC_DEFINE([CEDRIC_USE_NCARG], [1],
                          [Define if you have libncarg])
               ],
               [AC_MSG_FAILURE(
                 [NCARG test failed (--without-ncarg to disable)])],
	       [-lncarg -lncarg_gks -lncarg_ras -lncarg_c])])


AC_DEFUN([AC_FIND_LOCALHDR],
[for ac_incdir in $2
do
if test -r $ac_incdir/$1; then
   LOCAL_INCLUDES="$LOCAL_INCLUDES -I$ac_incdir"
   AC_MSG_RESULT([found in $ac_incdir])
fi
done
] )

LOCAL_INCLUDES=
AC_MSG_CHECKING("for netcdf.h ")
AC_FIND_LOCALHDR(netcdf.h,/usr/include /usr/include/netcdf /usr/local/include /usr/local/include/netcdf /usr/lib /usr/lib/netcdf /usr/local/lib/netcdf /usr/include/netcdf-3)
NETCDFINCS="$LOCAL_INCLUDES"
AC_MSG_RESULT( NETCDFINCS: $NETCDFINCS )

AC_MSG_CHECKING("for libnetcdf.a ")
NETCDFLIBDIR=
NETCDFLIBS=
for ac_xdir in /usr/lib /lib /local/lib /usr/local/lib /usr/lib/netcdf /lib/netcdf /local/lib/netcdf /usr/local/lib/netcdf /usr/lib/netcdf-3
do
if test -r $ac_xdir/libnetcdf.a; then
   NETCDFLIBDIR=$ac_xdir
   AC_MSG_RESULT(found in $ac_xdir )
fi
done

if test -n "$NETCDFLIBDIR"; then
   NETCDFLIBS="-L$NETCDFLIBDIR -lnetcdf -lnetcdff -ludunits2 -lhdf5 -lhdf5_hl -lz"
else
   NETCDFLIBS="-lnetcdf -lnetcdff -ludunits2 -lhdf5 -lhdf5_hl -lz"
fi
AC_MSG_RESULT( NETCDFLIBS: $NETCDFLIBS )

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

AM_CONDITIONAL([MAKE_WITH_NCARG], [test "$with_ncarg" != no])

AC_SUBST(NETCDFLIBS)
AC_SUBST(NETCDFINCS)
AC_SUBST(CEDRIC_USE_NCARG)

AC_OUTPUT
