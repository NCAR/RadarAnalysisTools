SHELL=/bin/sh
# simple makefile for cedio

# to use to make an executable file for reading cedric files:
# -replace driver_read.f and driver_read.o with your files that
# -type "make cedread"
# -the executable file will be called "cedread"
#
# to use to make an executable file for writing cedric files:
# -replace driver_write.f and driver_write.o with your files that
# -type "make cedwrite"
# -the executable file will be called "cedwrite"
# 

SRCSR=	ALTER.f	   ASDPMD.f   BLHED.f  BLKDAT.f  CDATE.c  CEDERX.f  CEDLOG.f\
        CEDREAD.f  CEDUTIL.f  CIN.c    COUT.c    CRTHIN.f FETCHZ.f  IMHSUM.f\
        LOCFLDID.f PLANST.f   SHIRBL.f TAPMES.f  driver_read.f
OBJSR=	ALTER.o	   ASDPMD.o   BLHED.o  BLKDAT.o  CDATE.o  CEDERX.o  CEDLOG.o\
        CEDREAD.o  CEDUTIL.o  CIN.o    COUT.o    CRTHIN.o FETCHZ.o  IMHSUM.o\
        LOCFLDID.o PLANST.o   SHIRBL.o TAPMES.o  driver_read.o
COMMR=	cedread

SRCSW=	BLHED.f    BLKDAT.f   CDATE.c  CEDLOG.f  CEDUTIL.f CIN.c    COUT.c\
        HEADBLD.f  IMHSUM.f   SHIRBL.f WRITCED.f driver_write.f
OBJSW=	BLHED.o    BLKDAT.o   CDATE.o  CEDLOG.o  CEDUTIL.o CIN.o    COUT.o\
        HEADBLD.o  IMHSUM.o   SHIRBL.o WRITCED.o driver_write.o
COMMW=	cedwrite

LDR=	ncargf77

#$(OBJSR):	$(SRCSR)
#$(OBJSW):	$(SRCSW)

$(COMMR):	$(OBJSR)
	$(LDR) -o $(COMMR) $(OBJSR)

$(COMMW):	$(OBJSW)
	$(LDR) -o $(COMMW) $(OBJSW)

