

   FFLAGS  = -g -Vaxlib -W0
   EXE = ifc
   OBJS = CIN.o COUT.o Char2Int.o ReadInp.o ALTER.o ASDPMD.o \
   AssignVar.o BLHED.o CEDERX.o CEDREAD.o CEDUTIL.o CRTHIN.o \
   FETCHZ.o IMHSUM.o LOCFLDID.o PLACEPLANE.o PLANST.o ReadVar.o \
   SAVEARRY.o SHIRBL.o TAPMES.o UserSub.o TextRead.o WritePS.o \
   contour.o main.o

   CIN.o: CIN.c
	icc -g -c CIN.c
   COUT.o: COUT.c
	icc -g -c COUT.c
   Char2Int.o: Char2Int.c
	icc -g -c Char2Int.c
   ReadInp.o: ReadInp.c
	icc -g -c ReadInp.c
   ALTER.o: ALTER.f
	$(EXE) $(FFLAGS) -c ALTER.f
   ASDPMD.o: ASDPMD.f
	$(EXE) $(FFLAGS) -c ASDPMD.f
   AssignVar.o: AssignVar.f
	$(EXE) $(FFLAGS) -c AssignVar.f
   BLHED.o: BLHED.f
	$(EXE) $(FFLAGS) -c BLHED.f
   CEDERX.o: CEDERX.f
	$(EXE) $(FFLAGS) -c CEDERX.f
   CEDREAD.o: CEDREAD.f
	$(EXE) $(FFLAGS) -c CEDREAD.f
   CEDUTIL.o: CEDUTIL.f
	$(EXE) $(FFLAGS) -c CEDUTIL.f
   CRTHIN.o: CRTHIN.f
	$(EXE) $(FFLAGS) -c  CRTHIN.f
   FETCHZ.o: FETCHZ.f
	$(EXE) $(FFLAGS) -c  FETCHZ.f
   IMHSUM.o: IMHSUM.f
	$(EXE) $(FFLAGS) -c  IMHSUM.f
   LOCFLDID.o: LOCFLDID.f
	$(EXE) $(FFLAGS) -c  LOCFLDID.f
   PLACEPLANE.o: PLACEPLANE.f
	$(EXE) $(FFLAGS) -c  PLACEPLANE.f
   PLANST.o: PLANST.f
	$(EXE) $(FFLAGS) -c  PLANST.f
   ReadVar.o: ReadVar.f
	$(EXE) $(FFLAGS) -c  ReadVar.f
   SAVEARRY.o: SAVEARRY.f
	$(EXE) $(FFLAGS) -c  SAVEARRY.f
   SHIRBL.o: SHIRBL.f
	$(EXE) $(FFLAGS) -c  SHIRBL.f
   TAPMES.o: TAPMES.f
	$(EXE) $(FFLAGS) -c  TAPMES.f
   UserSub.o: UserSub.f
	$(EXE) $(FFLAGS) -c  UserSub.f
   TextRead.o: TextRead.f
	$(EXE) $(FFLAGS) -c  TextRead.f
   WritePS.o: WritePS.f
	$(EXE) $(FFLAGS) -c  WritePS.f
   contour.o: contour.f
	$(EXE) $(FFLAGS) -c  contour.f
   main.o: main.f
	$(EXE) $(FFLAGS) -c  main.f
   grid2ps: $(OBJS)
	ifc -v $(FFLAGS) $(OBJS) $(FMISC) -o $@ -L/scr/poseidon1/mbell/ncarg-ifc/lib -I/scr/poseidon1/mbell/ncarg-ifc/include -L/usr/X11R6/lib -I/usr/X11R6/include -lnsl -lncarg -lncarg_gks -lncarg_c -lXpm -lX11 -lXext

