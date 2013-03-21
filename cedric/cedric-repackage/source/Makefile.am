# -*- makefile -*-
#

if MAKE_WITH_NCARG
bin_PROGRAMS = cedric gcedric
else
bin_PROGRAMS = cedric
endif

# SUBDIRS = . $(GCEDRICDIR)

sd =
include $(srcdir)/Makefile.sources


#
# FORTRAN include does not look in /usr/include by default, so it must be
# specified if that is where netcdf.inc is located.
#
FINCLUDES = -I/usr/include
AM_FFLAGS = -g -O0 -std=legacy $(DEFINES) -fno-range-check \
          -fsecond-underscore $(NETCDFINCS) $(FINCLUDES)
AM_CFLAGS = -g -O0 $(DEFINES) $(NETCDFINCS)
AM_LDFLAGS =

cedric_SOURCES = $(FSRCS) $(CSRCS) $(MDVSRCS) $(GBYTESSRCS)
cedric_FFLAGS = -DCEDRIC_USE_NCARG=0 $(AM_FFLAGS)
cedric_LDADD = $(NETCDFLIBS)

if MAKE_WITH_NCARG
gcedric_SOURCES = $(FSRCS) $(CSRCS) $(NGSRCS) $(MDVSRCS) $(GBYTESSRCS)
gcedric_FFLAGS = -DCEDRIC_USE_NCARG=1 $(AM_FFLAGS) $(NCARGINCS)
gcedric_LDADD = $(NCARGLIBS) $(XLIBS) $(NETCDFLIBS)
endif
