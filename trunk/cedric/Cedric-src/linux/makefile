SHELL=/bin/sh
#  simple makefile for Cedric; to create executable, type "make"
#

include ./makefiles.list

CFT	=	g77
LDR	=	g77
LIBS	= -L /opt/local/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lm
#CDFLIB	= /usr/local/netcdf/lib/libnetcdf.a
#MMM CDFLIB	= -L /usr/local/netcdf/lib -lnetcdf
#CDFLIB	= -L /opt/local/netcdf-3/lib -lnetcdf
CDFLIB = -lnetcdf -lnetcdff
COMMAND	= cedric_linux.e
#
FFLAGS	=	-g -static -O0
CFLAGS	=	-g -O0
LDFLAGS	=
#FFLAGS=	-p 
#CFLAGS= -p
#LDFLAGS= -p


$(COMMAND):	$(OBJS) $(INCLFLS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(LIBS) $(CDFLIB)

$(INCLFLS):	CEDRIC.INC

clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)

void:	clobber
	rm -f $(SRCS) makefile
