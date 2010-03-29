SHELL=/bin/sh
#  simple makefile for Cedric; to create executable, type "make"
#

include ./makefiles.list

CFT 	= gfortran -fPIC -fno-second-underscore -fno-range-check -O
LDR	= gfortran -fPIC -fno-second-underscore -fno-range-check -O
LIBS	= -L/opt/local/ncl_ncarg/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lXext -lpng -lz -lg2c
CDFLIB	= -L/opt/local/netcdf-3/lib -lnetcdff -lnetcdf
COMMAND	= cedric_linux.e
#
FFLAGS	=	-g -static -O0
CFLAGS	=	-g -O0
LDFLAGS	=

$(COMMAND):	$(OBJS) $(INCLFLS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(LIBS) $(CDFLIB)

$(INCLFLS):	CEDRIC.INC

clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)

void:	clobber
	rm -f $(SRCS) makefile
