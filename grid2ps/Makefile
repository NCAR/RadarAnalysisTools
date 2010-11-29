
IFORTDIR = /opt/local/intel

FFLAGS  = -g -Vaxlib
#   CC = icc
FC = $(IFORTDIR)/bin/ifort
#   OLD_LIB = -L/opt/intel/compiler70/ia32/old_lib -lc-2.2.93

OBJS = CIN.o COUT.o Char2Int.o ReadInp.o ALTER.o ASDPMD.o \
 AssignVar.o BLHED.o CEDERX.o CEDREAD.o CEDUTIL.o CRTHIN.o \
 FETCHZ.o IMHSUM.o LOCFLDID.o PLACEPLANE.o PLANST.o ReadVar.o \
 SAVEARRY.o SHIRBL.o TAPMES.o UserSub.o TextRead.o WritePS.o \
 contour.o main.o CDATE.o CEDLOG.o

NCARGDIR = /opt/local/ncarg/

RLIBFLAGS = -static-intel

grid2ps: $(OBJS)
	$(FC) -O1 -v $(FFLAGS) $(RLIBFLAGS) $(OBJS) $(FMISC) -o $@ -L$(NCARGDIR)/lib -I$(NCARGDIR)/include -lnsl -lncarg -lncarg_gks -lncarg_c -lpng -lgfortran -lXpm -lX11 -lXext -i_dynamic

clean:
	rm -f $(OBJS) grid2ps *~
