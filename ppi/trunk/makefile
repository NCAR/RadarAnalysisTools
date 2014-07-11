SHELL=/bin/sh
#
# makefile for PPI_MMM
#
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named 'ppi.e'.
#
include makefiles.list

# The Fortran compiler which works with this makefile is gfortran
FC			=	gfortran

# Tunable parameters
#
# COMMMAND	Name of the executable
# FFLAGS	Flags to the fortan compiler
# CFLAGS Flags to the C compiler
COMMAND	=	ppi.e
FFLAGS	=	-fPIC -fno-range-check -O -fsecond-underscore
CFLAGS	=	-g -O0

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	libppi.a $(COMMAND)

# Put our objects into a library so that we can avoid passing a long list
# of objects to ncargf77 when building the binary. (The ncargf77 script barfs
# if the list of objects is longer than 1024 characters, and we exceed that
# by quite a bit...)
libppi.a:	$(OBJS)
	ar rcs $@ $^

$(COMMAND):	PPI_MMM.o libppi.a
	# we must use ncargf77 to link the binary
	ncargf77 $(LDFLAGS) $(FFLAGS) -o $@ $^ $(LIBS) $(NG_LIBS) $(GC_LIBS)

include includes.list

clean:
	rm -f *.o *.l *.a

clobber:	clean
	rm -f $(COMMAND)

.f.o:
	$(FC) $(FFLAGS) -c $(INCLUDES) $<
