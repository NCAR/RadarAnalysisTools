SHELL=/bin/sh
#
# This makefile was produced by /usr/bin//fmgen
# If it is envoked by the command line
#	make 
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and then use LDR to produce an executable
# named a.out.
#
# To make a profiled version of a.out (named a.out.prof) on a CRAY 2
# Computer System use the command line
#	make  a.out.prof
# WARNING:  DO NOT PROFILE AND FLOWTRACE AT THE SAME TIME
#
# To remove all the object and listing files but leave the executable
# use the command line
#	make  clean
#
# To remove everything but the source files use the command line
#	make  clobber
#
# To remove the source files created by /usr/bin//fmgen and this makefile
# use the command line
#	make  void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#

include makefiles.list

#
# Tunable parameters
#
# CFT		Name of the fortran compiler to use
# FFLAGS	Flags to the fortran compiler
# LDR		Program to load the objects into an executable
# LDFLAGS	For definition of lmset and csu on the Cray 2
#		Flags to the loader
# LIBS		A list of libraries to use
#		Note that the C library (-lc) must ALWAYS be the last library
#		in the load step on both the XMP and the Cray 2
# COMMAND	Name of the executable
#
CFT	=	f77
LDR	=	f77
LIBS	=	-L /usr/local/lib -lncarg -lncarg_c
COMMAND	=	sprint_solaris.e

#
# To compile with FlowTrace turned on Comment out the next line (Put
# a '#' in front of it)
# WARNING:  DO NOT PROFILE (-p) AND FLOWTRACE (?) AT THE SAME TIME
# Options: -Bx where x is dynamic (shared) or static (no shared) 
#          loading of libraries
#          Add -C to FFLAGS for checking subscript bounds, but this
#          will cause all DIMENSION VARIABLE(1) constructs to fail.
#
#FFLAGS	=	-g -Bstatic
#CFLAGS	=	-g
#LDFLAGS	=	-Bstatic
FFLAGS	=	-p
CFLAGS	=	-p
LDFLAGS	=	-p

#
# On Cray-2 systems
# To compile with FlowTrace turned on unComment the next line (Take
# out the '#' in front of it) and unComment the line following it
# WARNING:  DO NOT PROFILE (-p) AND FLOWTRACE (?) AT THE SAME TIME
#
#FFLAGS	=	-e f
#LIBS	=	-lflow

#
# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
$(COMMAND):	$(OBJS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(LIBS)

#
# Make the profiled version of the command and call it a.out.prof
# WARNING:  DO NOT PROFILE AND FLOWTRACE AT THE SAME TIME
#
$(COMMAND).prof:	$(OBJS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(PROFLIB) $(LIBS)

include includes.list

clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)

void:	clobber
	rm -f $(SRCS) makefile
