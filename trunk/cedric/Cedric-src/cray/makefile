SHELL=/bin/sh
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
SRCS=	ALTER.f		DSPSYN.f	INTGRT.f	REDHED.f	STATZI.f \
	ASDPMD.f	DWITER.f	ISOSRF.f        REGCAL.f	STINT.f \
	EQSLV.f		ISUFLD.f	REGINT.f	STPDRV.f        BNDFIL.f \
	EXPAND.f	IVBGEN.f	REGRES.f	STPLOT.f 	BNDVAL.f \
	EXTEND.f	KARDIN.f	REGRZI.f	STPOBD.f 	CALUVW.f \
	FEETCL.f	KILDAT.f	RELXUV.f	STRID.f 	CEDERX.f \
	FETCHD.f	KNTOCR.f	REMAP.f		STRMLN.f 	CEDRIC.f \
	FETCHZ.f	LAPDRV.f	REMINT.f	SYMINT.f 	CHANGE.f \
        FILTER.f	LAPSOL.f	REMPOS.f	SYNDRV.f 	CODER.f \
	FIXIDS.f	LL2XY.f		RESHUF.f	SYNNER.f 	COMCHK.f \
	FLDSET.f	LOCATE.f	RGINI.f		T5FLTR.f 	COMMNT.f \
	FORADV.f	LOCFLD.f	RITER.f		TAPMES.f 	CONFLD.f \
	FUNCTN.f	LSFILW.f	RLXDRV.f 	CONTHALF.f	FUNCTS.f \
	LSQFIL.f	SAMPLR.f	TRANSF.f 	COPRX.f		FUNK.f \
	LSTSQR.f	SCLCAL.f 	CORCAL.f	GET3D.f		MAJMIN.f \
	SCTDSP.f	UNFOLD.f 	CORR.f		GLODEC.f	MAXZ.f \
	SELDEV.f	UNFSLP.f 	CREATE.f	MS2DRV.f	UNPAUT.f \
	CROSS.f		HDUMP.f		NEWHED.f	SETAXS.f	UNPFOR.f \
	CRTHIN.f	HMARKS.f	NEXACT.f	SETBAD.f	UPDHED.f \
	CRTMAP.f	HSTGRM.f	SETCNT.f	VALUE.f 	HSTINT.f \
	OVSETP.f	SETDCD.f	VECDRW.f 	DEFINE.f	HSTLST.f \
	PATCHR.f	SETVOL.f	VECDSP.f 	DESYNR.f	IABPCK.f \
	PCONVG.f	SHADAM.f	WINDFD.f 	DIFADV.f	IADFLD.f \
	PLACED.f	SHFTER.f	WINDIJ.f 	DIGITP.f	ICONSC.f \
	SHIRBL.f	WINSET.f 	DOFILT.f	ICTMOD.f	PLNFLD.f \
	SHOEDF.f	WRITVL.f 	DOSHFT.f	IDCMPR.f	PLOTAD.f \
	SHOEDW.f	XY2LL.f 	DSFSTK.f	IDSYND.f	PLOTCH.f \
	SIMPLDIG.f	genmap.f	DSINIT.f	IFIND.f		PLTDRV.f \
	SKIPP.f 	DSP3D.f		IFINOT.f	PUT3D.f		SRFACE.f \
	DSPCOD.f	IGET16.f	RDKARD.f	SRFGK.f 	DSPSRF.f \
	IMHSUM.f	READVL.f	STATS.f         DFCLRS.f        SETCOL.f \
        COLRAM.f        LBFILL.f        MYFRAME.f       COORD.f         COINTRP.f \
        CRINTRP.f       ROVECT.f        SHIFTIT.f       TRPCO.f         DIVZERO.f \
        HSTLBL.f        HISTGR.f        MEDHST.f        COUT.c          ISGFXY.f \
        ISST3D.f 	CIN.c           CEDUTIL.f       CGETMEM.c       MINMAX.f\
        GETD.f          PUTD.f          IFINDC.f        COPIX.f         COPCX.f \
        COPCIX.f        LOCINT.f        LOCFLDID.f      BLHED.f         BLDDES.f \
        PLANST.f        FLAT.f          COPIRX.f        FILL3D.f        CONCHAR.f \
        CDATE.c         CEDLOG.f        VERSOUT.f       XY2LLDRV.f      LL2XYDRV.f \
        LINFIT2.f       PLTACT.f        READAIR.f       CEDCDF.f        SHILBL.f \
        READSTA.f       PLTSTA.f        GETLEN.f        FAREA.f         OUTPCK.f\
        SETPAT.f        LABBOX.f        GRIDAIR.f       AIRACCUM.f      AIROUT.f\
	FLDNAMOUT.f     GUIPROC.f       BLKDAT.f        RANF.f          BCKGRND.f\
        PLTHIST.f       DATAIN.f

OBJS=	ALTER.o		DSPSYN.o	INTGRT.o	REDHED.o	STATZI.o \
	ASDPMD.o	DWITER.o	ISOSRF.o        REGCAL.o	STINT.o \
	EQSLV.o		ISUFLD.o	REGINT.o	STPDRV.o        BNDFIL.o \
	EXPAND.o	IVBGEN.o	REGRES.o	STPLOT.o 	BNDVAL.o \
	EXTEND.o	KARDIN.o	REGRZI.o	STPOBD.o 	CALUVW.o \
	FEETCL.o	KILDAT.o	RELXUV.o	STRID.o 	CEDERX.o \
	FETCHD.o	KNTOCR.o	REMAP.o		STRMLN.o 	CEDRIC.o \
	FETCHZ.o	LAPDRV.o	REMINT.o	SYMINT.o 	CHANGE.o \
        FILTER.o	LAPSOL.o	REMPOS.o	SYNDRV.o 	CODER.o \
	FIXIDS.o	LL2XY.o		RESHUF.o	SYNNER.o 	COMCHK.o \
	FLDSET.o	LOCATE.o	RGINI.o		T5FLTR.o 	COMMNT.o \
	FORADV.o	LOCFLD.o	RITER.o		TAPMES.o 	CONFLD.o \
	FUNCTN.o	LSFILW.o	RLXDRV.o 	CONTHALF.o	FUNCTS.o \
	LSQFIL.o	SAMPLR.o	TRANSF.o 	COPRX.o		FUNK.o \
	LSTSQR.o	SCLCAL.o 	CORCAL.o	GET3D.o		MAJMIN.o \
	SCTDSP.o	UNFOLD.o 	CORR.o		GLODEC.o	MAXZ.o \
	SELDEV.o	UNFSLP.o 	CREATE.o	MS2DRV.o	UNPAUT.o \
	CROSS.o		HDUMP.o		NEWHED.o	SETAXS.o	UNPFOR.o \
	CRTHIN.o	HMARKS.o	NEXACT.o	SETBAD.o	UPDHED.o \
	CRTMAP.o	HSTGRM.o	SETCNT.o	VALUE.o 	HSTINT.o \
	OVSETP.o	SETDCD.o	VECDRW.o 	DEFINE.o	HSTLST.o \
	PATCHR.o	SETVOL.o	VECDSP.o 	DESYNR.o	IABPCK.o \
	PCONVG.o	SHADAM.o	WINDFD.o 	DIFADV.o	IADFLD.o \
	PLACED.o	SHFTER.o	WINDIJ.o 	DIGITP.o	ICONSC.o \
	SHIRBL.o	WINSET.o 	DOFILT.o	ICTMOD.o	PLNFLD.o \
	SHOEDF.o	WRITVL.o 	DOSHFT.o	IDCMPR.o	PLOTAD.o \
	SHOEDW.o	XY2LL.o 	DSFSTK.o	IDSYND.o	PLOTCH.o \
	SIMPLDIG.o	genmap.o	DSINIT.o	IFIND.o		PLTDRV.o \
	SKIPP.o 	DSP3D.o		IFINOT.o	PUT3D.o		SRFACE.o \
	DSPCOD.o	IGET16.o	RDKARD.o	SRFGK.o 	DSPSRF.o \
	IMHSUM.o	READVL.o	STATS.o         DFCLRS.o        SETCOL.o \
        COLRAM.o        LBFILL.o        MYFRAME.o       COORD.o         COINTRP.o \
        CRINTRP.o       ROVECT.o        SHIFTIT.o       TRPCO.o         DIVZERO.o \
        HSTLBL.o        HISTGR.o        MEDHST.o        COUT.o          ISGFXY.o \
        ISST3D.o 	CIN.o           CEDUTIL.o       CGETMEM.o       MINMAX.o\
        GETD.o          PUTD.o          IFINDC.o        COPIX.o         COPCX.o \
        COPCIX.o        LOCINT.o        LOCFLDID.o      BLHED.o         BLDDES.o \
        PLANST.o        FLAT.o          COPIRX.o        FILL3D.o        CONCHAR.o \
        CDATE.o         CEDLOG.o        VERSOUT.o       XY2LLDRV.o      LL2XYDRV.o \
        LINFIT2.o       PLTACT.o        READAIR.o       CEDCDF.o        SHILBL.o \
        READSTA.o       PLTSTA.o        GETLEN.o        FAREA.o         OUTPCK.o\
        SETPAT.o        LABBOX.o        GRIDAIR.o       AIRACCUM.o      AIROUT.o\
	FLDNAMOUT.o     GUIPROC.o       BLKDAT.o        RANF.o          BCKGRND.o\
        PLTHIST.o       DATAIN.o


  

#
# Tunable parameters
#
# CFT		Name of the fortran compiler to use
# FFLAGS	Flags to the fortran compiler
# LDR		Program to load the objects into an executable
# LDFLAGS	For definition of lmset and csu on the Cray 2
#		Flags to the loader
# LIBS		A list of libraries to use on the Cray 2
#		Note that the C library (-lc) must ALWAYS be the last library
#		in the load step on both the XMP and the Cray 2
# COMMAND	Name of the executable
#

INCLFLS=	ASDPMD.o  CEDRIC.o COINTRP.o CRINTRP.o FETCHD.o NEWHED.o \
                PLACED.o  REMAP.o  SHILBL.o  SHIRBL.o  TRPCO.o  UPDHED.o \
                WRITVL.o  IGET16.o


CFT=	cft77 
FFLAGS=	-Wf"-a static -i 64 -A fast "
LDR=	segldr
LDFLAGS= -f indef -L/lib,/usr/lib,/usr/local/lib -lncaro,ncarg,ncarg_gks,ncarg_loc,ncarg_c,X11,netcdf
LIBS=	
COMMAND=a.out

#
# To compile with FlowTrace turned on Comment out the next line (Put
# a '#' in front of it)
# WARNING:  DO NOT PROFILE AND FLOWTRACE AT THE SAME TIME
#






#
# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
$(COMMAND):	$(OBJS) $(INCLFLS)
	$(LDR) $(LDFLAGS) -o $(COMMAND) $(OBJS) $(LIBS)
#

$(INCLFLS):	CEDRIC.INC

clean:
	rm -f *.o *.l

clobber:	clean
	rm -f $(COMMAND)

