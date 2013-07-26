#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
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
python/Makefile
tests/Makefile
])
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_F77([gfortran])
AC_PROG_RANLIB
# Using AC_PROG_FC without AC_PROG_F77 causes a warning from autoconf about
# "preprocessed Fortran 77 seen but F77 macro not included".  However,
# using just AC_PROG_F77 without AC_PROG_FC causes the Fortran tests below
# for NCAR graphics to fail for AC_LANG([Fortran]).  Thus the
# AC_LANG([Fortran 77]) is significant because it causes the Fortran link
# tests to use the F77 variable defined by AC_PROG_F77.
#
AC_PROG_FC([gfortran])
AC_LANG([Fortran 77])

GLIBS="-lX11 -lXext -lpng -lz"

# Some NCAR Graphics builds build a separate ncarg_gks_cairo library, so
# that ncarg_gks_cairo requires cairo but ncarg_gks does not.  Other NCAR
# Graphics builds apparently require cairo even for ncarg_gks.  The
# settings below assume that latter, but should still work on most Linux
# systems (with cairo installed) even if ncarg_gks does not require cairo.
# If there is a system where this doesn't work, then this may be the most
# convenient place to customize it, or it may require more logic or
# variables for the configure script.
# 
gkslibs="-lncarg_gks -lcairo -lfreetype"

# Checks for libraries.
AC_ARG_WITH([ncarg],
 [AS_HELP_STRING([--with-ncarg],
[Enable support for plotting with NCAR Graphics.
 Set to the installation directory for NCAR Graphics, or else
 if no argument (--with-ncarg) then assume a system installation.
 Use --with-ncarg=no or --without-ncarg to disable NCAR Graphics plots.])],
 [if test "x$with_ncarg" != xyes; then NCARGDIR="$with_ncarg"; fi],
 [with_ncarg="yes"])

ncgdirs="/opt/local/ncarg /usr/local/ncarg /opt/local /usr/local /opt/local/ncarg-5.2.1"
if test "x$with_ncarg" != xno; then

   found=0
   if test -n "$NCARGDIR"; then
      LIBS="$LIBS -L$NCARGDIR/lib"
      INCLUDES="-I${NCARGDIR}/include"
      echo "checking if link succeeds using $NCARGDIR/lib"
      AC_CHECK_LIB([ncarg], [OPNGKS],
      	            [found=1
		     NCARGLIBS="-L${NCARGDIR}/lib -lncarg $gkslibs -lncarg_ras -lncarg_c $GLIBS"
		     NCARGINCS="-I${NCARGDIR}/include"],
		    [found=0],
                    [$gkslibs -lncarg_ras -lncarg_c $GLIBS])
   else
      # See if link succeeds without NCARGDIR
      echo "checking if NCARG link succeeds without extra paths set"
      AC_CHECK_LIB([ncarg], [OPNGKS],
      	            [found=1
		     NCARGLIBS="-lncarg $gkslibs -lncarg_ras -lncarg_c $GLIBS"
		     NCARGINCS=""],
		    [found=0],
                    [$gkslibs -lncarg_ras -lncarg_c $GLIBS])

      if test $found -eq 0; then
	  # Look for NCARGDIR in some typical places.
	  savelibs="$LIBS"
	  saveincs="$INCLUDES"
	  for ngdir in $ncgdirs ; do

	      # Need to unset any value cached from previous tests
	      unset ac_cv_lib_ncarg_OPNGKS
	      echo "checking for NCARG in $ngdir"
	      LIBS="$savelibs -L$ngdir/lib"
	      INCLUDES="-I${ngdir}/include"
	      AC_CHECK_LIB([ncarg], [OPNGKS],
			    [found=1
			     NCARGLIBS="-L$ngdir/lib -lncarg $gkslibs -lncarg_ras -lncarg_c $GLIBS"
			     NCARGINCS="-I${ngdir}/include"
			     NCARGDIR="$ngdir"],
			    [found=0
			    echo " ...not found in $ngdir"],
			    [$gkslibs -lncarg_ras -lncarg_c $GLIBS])
	      if test $found -ne 0; then
	         echo "...found in $ngdir"
		 break
              fi

	  done
	  LIBS="$savelibs"
	  INCLUDES="$saveincs"
       fi
   fi
   if test $found -eq 0; then
       AC_MSG_FAILURE([NCARG library could not be found])
   else
       AC_MSG_RESULT([NCARG library setting: $NCARGLIBS])
   fi
fi


AC_MSG_RESULT( NETCDFINCS: $NETCDFINCS )

if test -z "$NETCDFLIBS" ; then
if test -n "$NETCDFLIBDIR"; then
   NETCDFLIBS="-L$NETCDFLIBDIR -lnetcdf -lnetcdff -ludunits2 -lhdf5 -lhdf5_hl -lz"
else
   NETCDFLIBS="-lnetcdf -lnetcdff -ludunits2 -lhdf5 -lhdf5_hl -lz"
fi
fi
AC_MSG_RESULT( NETCDFLIBS: $NETCDFLIBS )

# We don't use any C checks at the moment, but without it autoconf warns
# about the existence of C source files without a C compiler check.  So
# setup the C compiler but without any of the usual checks.  (This were
# included by the initial autoscan.)
AC_PROG_CC
# Checks for header files.
AC_LANG([C])
#AC_CHECK_HEADERS([fcntl.h malloc.h memory.h netinet/in.h stdlib.h string.h strings.h sys/param.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_TYPE_OFF_T
#AC_TYPE_PID_T
#AC_TYPE_SIZE_T

# Checks for library functions.
#AC_FUNC_MALLOC
#AC_FUNC_MKTIME
#AC_FUNC_REALLOC
#AC_FUNC_WAIT3
#AC_CHECK_FUNCS([atexit bzero floor ftruncate gethostname gettimeofday memmove memset modf pow select strtoul])

AM_CONDITIONAL([MAKE_WITH_NCARG], [test "$with_ncarg" != no])

AC_SUBST(NETCDFINCDIR)
AC_SUBST(NETCDFLIBDIR)
AC_SUBST(NETCDFLIBS)
AC_SUBST(NETCDFINCS)
AC_SUBST(NCARGDIR)
AC_SUBST(NCARGLIBS)
AC_SUBST(NCARGINCS)

AC_OUTPUT
