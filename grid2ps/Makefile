

   FFLAGS  = -g -Vaxlib
   CC = icc
   FC = ifort
#   OLD_LIB = -L/opt/intel/compiler70/ia32/old_lib -lc-2.2.93
   OBJS = CIN.o COUT.o Char2Int.o ReadInp.o ALTER.o ASDPMD.o \
   AssignVar.o BLHED.o CEDERX.o CEDREAD.o CEDUTIL.o CRTHIN.o \
   FETCHZ.o IMHSUM.o LOCFLDID.o PLACEPLANE.o PLANST.o ReadVar.o \
   SAVEARRY.o SHIRBL.o TAPMES.o UserSub.o TextRead.o WritePS.o \
   contour.o main.o CDATE.o CEDLOG.o

   grid2ps: $(OBJS)
	ifort -O1 -v $(FFLAGS) $(OBJS) $(FMISC) -o $@ -L/export/artemis2/poseidon2/mbell/ncarg-ifc/lib -I/export/artemis2/poseidon2/mbell/ncarg-ifc/include -L/usr/X11R6/lib -I/usr/X11R6/include -lnsl -lncarg -lncarg_gks -lncarg_c -lXpm -lX11 -lXext -i_dynamic

