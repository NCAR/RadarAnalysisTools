SHELL=/bin/sh
#  simple makefile for Cedric; to create executable, type "make"
#

include ./makefiles.list

CFT=	f77
LDR=	f77
LIBS= -L /usr/local/lib -lncarg -lncarg_gks -lncarg_c -lnetcdf -lX11
COMMAND=cedric_sgi.e
#
FFLAGS=	-g -static
CFLAGS= -g
LDFLAGS= 


$(COMMAND):	$(OBJS) $(INCLFLS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(LIBS)

$(INCLFLS):	CEDRIC.INC

clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)

void:	clobber
	rm -f $(SRCS) makefile
