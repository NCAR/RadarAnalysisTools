# -*- makefile -*-
#

if MAKE_WITH_NCARG
GCEDRICDIR = gcedric
else
GCEDRICDIR = 
endif

SUBDIRS = . $(GCEDRICDIR)

sd =
include $(srcdir)/Makefile.sources

bin_PROGRAMS = cedric
cedric_SOURCES = $(SRCS)

#
# FORTRAN include does not look in /usr/include by default, so it must be
# specified if that is where netcdf.inc is located.
#
#FINCLUDES = -I/usr/include
AM_FFLAGS = -g -O0 -std=legacy $(DEFINES) -fno-range-check \
          -fsecond-underscore $(NETCDFINCS)
AM_CFLAGS = -g -O0 $(DEFINES) $(NETCDFINCS)
AM_LDFLAGS =

DEFINES = -DCEDRIC_USE_NCARG=0
SRCS = $(FSRCS) $(CSRCS) $(MDVSRCS) $(GBYTESSRCS)
cedric_LDADD = $(NETCDFLIBS)
