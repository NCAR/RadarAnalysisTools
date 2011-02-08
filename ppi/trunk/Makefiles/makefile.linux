[SHELL=/bin/sh
#
# This makefile was produced by /usr/bin//fmgen at 01:49:37 PM on 03/24/94
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#	make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin//fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#

include makefiles.list

# Tunable parameters
#
# CFT		Name of the fortran compiling system to use
# FFLAGS	Flags to the fortan compiler
# LDR		Name of the loader
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# COMMMAND	Name of the executable
# PROFLIB	Library needed for profiling
#
#MMM LIBS=	-L /usr/local/ncarg/lib -lncarg -lncarg_gks -lncarg_c -lX11
#EOL LIBS=	-L /opt/local/lib -lncarg -lncarg_gks -lncarg_c -lX11
#
#CFT	=	g77
#LDR 	=	g77
#CFT	=	/opt/local/bin/ncargf77
#LDR	=	/opt/local/bin/ncargf77
#
#CFT	=	gfortran
#LDR	=	gfortran
#LIBS=		-L /opt/local/lib -lgfortran -lgcc -lm -lncarg -lncarg_gks -lncarg_c -lX11 -lXext -lz -lg2c -lpng
#
CFT 	= gfortran -fPIC -fno-second-underscore -fno-range-check -O
LDR	= gfortran -fPIC -fno-second-underscore -fno-range-check -O
LIBS	= -L/opt/local/ncl_ncarg/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lXext -lpng -lz -lg2c
COMMAND=	ppi_linux.e
#
FFLAGS	=	-g -static
CFLAGS	=	-g
#FFLAGS	=	-g -static -O0
#CFLAGS	=	-g -O0
LDFLAGS=	

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
$(COMMAND):	$(OBJS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(LIBS)

include includes.list

clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)

void:	clobber
	rm -f $(SRCS) makefile
