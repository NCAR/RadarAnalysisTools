SHELL=/bin/sh
# simple makefile for cedio

# to use to make an executable file for reading cedric files:
# -replace driver_read.f and driver_read.o in the source (SRCSR)
#  and object (OBJSR) lists with your filenames
# -then type "make cedread"
# -the executable file will be called "cedread"
#
SRCSR=	ALTER.f		ASDPMD.f	BLHED.f		BLKDAT.f \
	CDATE.c 	CEDERX.f	CEDLOG.f	CEDREAD.f \
	CEDUTIL.f	CIN.c		COUT.c		CRTHIN.f \
	FETCHZ.f	IMHSUM.f	LOCFLDID.f	PLANST.f \
	SHIRBL.f	TAPMES.f	VERSOUT.f	DMPCHAR.f \
	DMPINTGR.f	DMPFLOAT.f	driver_read.f
OBJSR=	ALTER.o		ASDPMD.o	BLHED.o		BLKDAT.o \
	CDATE.o 	CEDERX.o	CEDLOG.o	CEDREAD.o \
	CEDUTIL.o	CIN.o		COUT.o		CRTHIN.o \
	FETCHZ.o	IMHSUM.o	LOCFLDID.o	PLANST.o \
	SHIRBL.o	TAPMES.o	VERSOUT.o	DMPCHAR.o \
	DMPINTGR.o	DMPFLOAT.o	driver_read.o
INCSR=	ASDPMD.o	BLHED.o		CEDREAD.o	FETCHZ.o \
	SHIRBL.o	driver_read.o
COMMR=	cedread

# to use to make an executable file for writing cedric files:
# -replace driver_write.f and driver_write.o in the source (SRCSW)
#  and object (OBJSR) lists with your filenames
# -then type "make cedwrite"
# -the executable file will be called "cedwrite"
# 
SRCSW=	BLHED.f		BLKDAT.f	CDATE.c		CEDHEAD.f \
	CEDLOG.f	CEDUTIL.f	CIN.c		COUT.c \
	HEADBLD.f	IMHSUM.f	SHIRBL.f	WRITCED.f \
	VERSOUT.f	DMPCHAR.f	DMPINTGR.f	DMPFLOAT.f \
	driver_write.f
OBJSW=	BLHED.o		BLKDAT.o	CDATE.o		CEDHEAD.o \
	CEDLOG.o	CEDUTIL.o	CIN.o		COUT.o \
	HEADBLD.o	IMHSUM.o	SHIRBL.o	WRITCED.o \
	VERSOUT.o	DMPCHAR.o	DMPINTGR.o	DMPFLOAT.o \
	driver_write.o
INCSW=	BLHED.o		CEDHEAD.o	HEADBLD.o	SHIRBL.o \
	WRITCED.o	driver_write.o
COMMW=	cedwrite

# to use to make an executable file for reading/writing cedric files:
# -replace driver_rdwrt.f and driver_rdwrt.o in the source (SRCSB)
#  and object (OBJSB) lists with your filenames
# -then type "make cedrdwrt"
# -the executable file will be called "cedrdwrt"
# 
SRCSB=	ALTER.f		ASDPMD.f	BLHED.f		BLKDAT.f \
	CDATE.c		CEDERX.f	CEDHEAD.f	CEDLOG.f \
	CEDREAD.f	CEDUTIL.f	CIN.c		COUT.c \
	CRTHIN.f	FETCHZ.f	HEADBLD.f	IMHSUM.f \
	LOCFLDID.f	PLANST.f	SHIRBL.f	TAPMES.f \
	WRITCED.f	VERSOUT.f  	DMPCHAR.f	DMPINTGR.f \
	DMPFLOAT.f	driver_rdwrt.f
OBJSB=	ALTER.o		ASDPMD.o	BLHED.o		BLKDAT.o \
	CDATE.o		CEDERX.o	CEDHEAD.o	CEDLOG.o \
	CEDREAD.o	CEDUTIL.o	CIN.o		COUT.o \
	CRTHIN.o	FETCHZ.o	HEADBLD.o	IMHSUM.o \
	LOCFLDID.o	PLANST.o	SHIRBL.o	TAPMES.o \
	WRITCED.o	VERSOUT.o  	DMPCHAR.o	DMPINTGR.o \
	DMPFLOAT.o	driver_rdwrt.o
INCSB=	ASDPMD.o	BLHED.o		CEDHEAD.o	CEDREAD.o \
	FETCHZ.o	HEADBLD.o	SHIRBL.o 	WRITCED.o \
	driver_rdwrt.o 
COMMB=	cedrdwrt

#LDR=	ncargf77
CFT 	= gfortran -fPIC -fno-second-underscore -fno-range-check -O
LDR	= gfortran -fPIC -fno-second-underscore -fno-range-check -O
LIBS	= -L/opt/local/ncl_ncarg/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lXext -lpng -lz -lg2c
CDFLIB	= -L/opt/local/netcdf-3/lib -lnetcdff -lnetcdf
#
FFLAGS	=	-g -static -O0
CFLAGS	=	-g -O0
LDFLAGS	=

$(COMMR):	$(OBJSR) 
	$(LDR) $(LDFLAGS) -o $(COMMR) $(OBJSR) $(LIBS) $(CDFLIB)

$(INCSR):	CEDRIC.INC

$(COMMW):	$(OBJSW)
	$(LDR) $(LDFLAGS) -o $(COMMW) $(OBJSW) $(LIBS) $(CDFLIB)

$(INCSW):	CEDRIC.INC

$(COMMB):	$(OBJSB) $(INCLSB)
	$(LDR) $(LDFLAGS) -o $(COMMB) $(OBJSB) $(LIBS) $(CDFLIB)

$(INCSB):	CEDRIC.INC

#
clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)

void:	clobber
	rm -f $(SRCS) makefile
