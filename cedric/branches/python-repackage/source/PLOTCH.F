      SUBROUTINE PLOTCH(ID,CAPA,NX,NY,PLWIND,BAD,ZLEV,NAMF,SCLSET,
     X     ITIT,IFLD,CSP,NCX,NCWORD,IVECT,WBUF,IGREY,ISKPI,ISKPJ,
     X     IAIR,ISTA,LABFLG,THKLIN,LATLON,THKMAP)
C     
C     MAKES APPROPRIATE CALLS TO CREATE CONTOUR/HAFTONE PLOTS WITH 
C     APPROPRIATE LABELING
C     
C     IPLTYP: 
C            1- CONTOUR PLOT ONLY
C            2- HAFTON PLOT ONLY
C            3- CONTOUR AND HAFTON COMBINED
C        (4,9)- COLOR AREA FILL
C     
C     IVECT: 
C            0- NO VECTOR OVERLAY
C           >0- VECTOR OVERLAYS ACTIVATED
C     
C     ICOLAY: Contains all of the contouring options (P5)
C            1- Label contour lines
C            2- Mark relative highs and lows
C            3- Dashed line pattern (0-9)
C            4- Stop contours at missing data locations
C            5- Indicate maximum value on the plot
C            6- Digitize the field on the plot background
C            7- Overlay vector, airtrack, and stations (0-7)
C            8- Overlay map and landmarks (0-3)
C
C     Note:
C        CSP(1,1), CSP(2,1), CSP(3,1) = XMIN, XMAX, XDEL
C        CSP(1,2), CSP(2,2), CSP(3,2) = YMIN, YMAX, YDEL
C        CSP(1,3), CSP(2,3), CSP(3,3) = ZMIN, ZMAX, ZDEL
C

      INCLUDE 'CEDRIC.INC'
c      PARAMETER (MXVSCT=20)
      PARAMETER (MAXLEV=61)
      PARAMETER (RNULL= -32768.)
c      PARAMETER (NRWRK=7000,NIWRK=2000,NAWRK=50000,NIARA=20)
      DIMENSION LEVUSE(3),WBUF(1)
      COMMON /CONTUR/ CLB(MAXLEV,NFMAX),NLB(NFMAX),ICSPCS(10,NFMAX)
      COMMON /RANGEC/ XBEG,XEND,YBEG,YEND,XRANGE(2),YRANGE(2),DELRG(2)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /OVRLAY/ IOLAY,NPOLAY(5,MXVSCT),ICOLAY(10)
      DIMENSION CAPA(NX,NY),ZMAX(3),CABDBZ(MAXLEV)
      DIMENSION PLWIND(2,3),CSP(3,3),NCX(3),NCWORD(3),ZMIN(3)
      DIMENSION ID(NID)
      CHARACTER*16 CFMTX,IFMTX,IFMTY
      CHARACTER*16 CFMTY
      CHARACTER*80 IPAT(3)
      CHARACTER*8 NOWDAT,ITIT(5)
      CHARACTER*80 LABEL
      CHARACTER*80 CITIT
      CHARACTER*2 NAMF(4)
      CHARACTER*80 IPAX
      LOGICAL LATLON
      DATA IPAT/  'S', 'D', 'L' /
      DATA IPAX/'+'/
      DATA LEVUSE/3,1,1/
      DATA CABDBZ/ MAXLEV*RNULL/
      DATA NDOT,NSET/1,1/
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
      SAVE

      LOCPLT(R)=1023.*R
      CALL GSCLIP(0)
C     
C     TRANSFER TO CHAR ARRAY
C     
      WRITE (CITIT(1:80),500)(ITIT(I),I=1,5),BLANK
 500  FORMAT(5A8,39A1,'X')
      print *,'PLOTCH: itit =',itit
      print *,'PLOTCH: citit=',citit
C     
C     PLOT SPECIFICATIONS
C     
      IF(IOLAY.NE.0) THEN
C     
C     OVERLAY IMAGE TO BE GENERATED
C     
         ILABC=ICOLAY(1)
         NHIC =ICOLAY(2)
         NPATC=ICOLAY(3)
         IOFFP=ICOLAY(4)
         NMAXC=ICOLAY(5)
         IF(ICOLAY(6).NE.0) NHIC=-1
         JVECT=ICOLAY(7)
         JMAPB=ICOLAY(8)
         IPLTYP=ICOLAY(9)
      ELSE
C     
C     SINGLE PLOT TO BE GENERATED
C     
         ILABC=ICSPCS(1,IFLD)
         NHIC =ICSPCS(2,IFLD)
         NPATC=ICSPCS(3,IFLD)
         IOFFP=ICSPCS(4,IFLD)
         NMAXC=ICSPCS(5,IFLD)
         IF(ICSPCS(6,IFLD).NE.0) NHIC=-1
         JVECT=ICSPCS(7,IFLD)
         JMAPB=ICSPCS(8,IFLD)
         IPLTYP=ICSPCS(9,IFLD)
      END IF

      NL=NLB(IFLD)
      L1=NCWORD(1)
      L2=NCWORD(2)
      L3=NCWORD(3)
C     ESTABLISH VIEWING PARAMETERS
      CALL RGINI(XL,XR,YB,YT,PLWIND,CSP,NCX,NCWORD)
      CALL KILDAT(CAPA,NX,NY,XBEG,XEND,YBEG,YEND,NPX,NPY,ISKPI,ISKPJ)
      ZMAX(1)=RNULL
      ZMIN(1)=RNULL
C      CALL MINZ(CAPA,NX,NPX,NPY,ZMIN,BAD)
      CALL MAXZ(CAPA,NX,NPX,NPY,ZMAX,BAD)
      IF(IOLAY.EQ.2) GO TO 5
      CALL MAJMIN(XRANGE(1),DELRG(1),IFMTX,MAJORX,MINORX,NDIG1,ISZ1,
     X            YRANGE(1),DELRG(2),IFMTY,MAJORY,MINORY,NDIG2,ISZ2)
      CALL SET(XL,XR,YB,YT,XRANGE(1),XRANGE(2),YRANGE(1),YRANGE(2),1)
      WRITE (CFMTX,510)IFMTX
 510  FORMAT(A16)
      WRITE (CFMTY,510)IFMTY

C     Always plot and label grid
      CALL LABMOD(CFMTX,CFMTY,NDIG1,NDIG2,ISZ1,ISZ2,4,4,0)
      CALL PERIML(MAJORX,MINORX,MAJORY,MINORY)

C     JUST DRAW A BOX AROUND PLOT-NO TICK MARKS
c         CALL TICKS(0,0)
c         CALL PERIM(0,0,0,0)
c         CALL TICKS(12,8)
C     
C     THE FOLLOWING INFORMATION IS DERIVED FROM ID HEADER
C     WHICH VARIES ACCORDING TO THE FORMAT
C     
      CALL DATEE(NOWDAT)
      CF=1./ID(69)
      XOR=ID(40)*CF
      SF=1./ID(68)
      XREL=ID(41)*SF
      YREL=ID(42)*SF
      
      WRITE (LABEL,101)(ID(I),I=116,121),(ID(I),I=125,127),
     X     (ID(I),I=13,15),AXNAM(L3),
     X     ZLEV,LABAXS(L3,IUNAXS),(NAMF(I),I=1,4)
 101  FORMAT(I2.2,'/',I2.2,'/',I2.2,6X,I2.2,2(':',I2.2),'-',
     X     I2.2,2(':',I2.2),7X,3A2,7X,A2,'=',F7.2,' ',A4,6X,4A2)

      WRITE (*,501)AXNAM(L3),ZLEV,LABAXS(L3,IUNAXS)
 501  FORMAT(3X,A1,'=',F7.2,' ',A4)

      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(60,1010,LABEL,12.,0.,-1.)
         WRITE (LABEL,102)NOWDAT
 102     FORMAT('(AS OF ',A8,')')
         CALL MY_PLCHMQ(10,985,LABEL(1:16),12.,0.,-1.)
         WRITE (LABEL,106)XREL,YREL,XOR
 106     FORMAT('ORIGIN=(',F7.2,',',F7.2,') KM   X-AXIS=',F5.1,' DEG')
         CALL MY_PLCHMQ(430,985,LABEL(1:46),12.,0.,-1.)
         CALL MY_PLCHMQ(200,960,CITIT(1:80),12.,0.,-1.)
         WRITE (LABEL,103)AXNAM(L2), LABAXS(L2,IUNAXS)
 103     FORMAT(A3,' ',A4)
      END IF
      LOCY=LOCPLT(YB+(YT-YB)*0.15)
      IF (LABFLG.GT.5) THEN
         CALL MY_PLCHMQ(10,LOCY,LABEL(1:7),12.,90.,-1.)
         WRITE (LABEL,103)AXNAM(L1), LABAXS(L1,IUNAXS)
         LOCY=LOCPLT(YB)-50
         CALL MY_PLCHMQ(150,LOCY,LABEL(1:7),12.,0.,-1.)
      END IF
    5 CONTINUE
      IF(IOLAY.EQ.2 .AND. LABFLG.GT.5) THEN
         WRITE (LABEL,105)(NAMF(I),I=1,4)
 105     FORMAT('OVERLAY FIELD IS ',4A2)
         CALL MY_PLCHMQ(500,900,LABEL(1:25),12.,0.,-1.)
      END IF
      CALL SET(XL,XR,YB,YT,XBEG,XEND,YBEG,YEND,1)
      LOCX=LOCPLT(XR)-7
      LOCY=925
      IF(IOLAY.GT.0) LOCY=LOCY-35
      IF(ZMAX(1).LT.CLB(1,IFLD)) GO TO 50
C     
C     GENERATE DATA PLOTS ACCORDING TO THE VALUE OF IPLTYP
C     
      IF(NHIC.LT.0)
     X     CALL SCLCAL(CLB(1,IFLD),NL,SCLSET)
      RX=FLOAT(LOCX)/1024.
      RY=FLOAT(LOCY)/1024.
      CALL CONTHALF(CAPA,NX,NPX,NPY,CLB(1,IFLD),NL,NHIC,ILABC,
     X     CABDBZ,IOFFP,BAD,NPATC,IPLTYP,RMAX,RMIN,RX,
     X     RY,YB,WBUF,IGREY,THKLIN,LABFLG)
      
      IF(ZMIN(1).EQ.RNULL) GO TO 70
      IF(NMAXC.EQ.0) GOTO 70
C
C     LOCATE MINIMUM VALUE ON PLOT
C
      IF (LABFLG.GT.5) THEN
         WRITE(LABEL,104)ZMIN(1)
         CALL GSCLIP(0)
         CALL MY_PLCHMQ(LOCX,LOCY-40,LABEL(1:6),12.,0.,-1.)
         CALL MY_PLCHMQ(LOCX+72,LOCY-40,'-',12.,0.,-1.)
         CALL PLCHMQ(ZMIN(2),ZMIN(3),'-',15.,0.,-1.)
      END IF
      LOCY=LOCY-30
      IF (IOLAY.EQ.2) LOCX=LOCX+65
 70   CONTINUE
      IF (IPLTYP.NE.1) GO TO 50
 10   CONTINUE
C     
C     LABEL CONTOUR LEVELS
C     

      IFLTFLG=0
      DO I=1,NL
C
C     DETERMINE IF INTS CAN BE USED FOR LABELS OR IF FLOATS ARE NEEDED
C
         IF (CLB(NL-I+1,IFLD).NE.0.0) THEN
            FVAL=FLOAT(INT(CLB(NL-I+1,IFLD)))/CLB(NL-I+1,IFLD)
         ELSE
            FVAL=1.0
         END IF
         IF (FVAL.NE.1.0) IFLTFLG=1
      END DO

      if (iolay.eq.2) locx=locx+100
      if (iolay.gt.0) locy=locy-30
      DO 35 I=1,NL
         IF (CLB(I,IFLD).GT.RMAX .AND. LABFLG.NE.1) GOTO 35
         IF (CLB(I,IFLD).LT.RMIN .AND. LABFLG.NE.1) GOTO 35
         IF (IFLTFLG.EQ.1) THEN
            WRITE(LABEL,104) CLB(I,IFLD)
 104        FORMAT(F6.1)
         ELSE
            IVAL=CLB(I,IFLD)
            IF (ABS(IVAL).LT.10) THEN
               WRITE(LABEL,'(I5)')IVAL
            ELSE IF (ABS(IVAL).LT.100) THEN
               WRITE(LABEL,'(I5)')IVAL
            ELSE IF (ABS(IVAL).LT.1000) THEN
               WRITE(LABEL,'(I5)')IVAL
            ELSE
               WRITE(LABEL,'(I5)')IVAL
            END IF
         END IF
         CALL GSCLIP(0)
         IF (IOLAY.NE.2 .OR. LABFLG.GT.5) THEN
            CALL MY_PLCHMQ(LOCX+30,LOCY-45,LABEL(1:6),12.,0.,
     X           -1.)
         END IF
         IF (IOLAY.GT.0) THEN
C
C     DECODE REQUESTED DASHLINE PATTERN FOR OVERLAY PLOTS
C
            ITPAT=-NPATC
            IF(ITPAT.GE.46656) THEN
               J=1
            ELSE IF (ITPAT.GE.3125 .AND. ITPAT.LT.46656) THEN
               J=2
            ELSE IF (ITPAT.GE.256 .AND. ITPAT.LT.3125) THEN
               J=3
            ELSE IF (ITPAT.GE.27 .AND. ITPAT.LT.256) THEN
               J=2.5-SIGN(1.0,CLB(I,IFLD))
            ELSE
               J=MOD(I-1,3) + 1
            END IF
         ELSE
C
C     NOT AN OVERLAY
C
            IF (NPATC.LT.0) THEN
               J=4+NPATC
            ELSE
               J=MOD(I-1,3) + 1
               IF(NPATC.EQ.0 .OR. NPATC.EQ.2 .OR. NPATC.EQ.4 .OR.
     X              NPATC.EQ.6 .OR. NPATC.EQ.8) 
     X              J=2.5-SIGN(1.0,CLB(I,IFLD))
            END IF
         END IF
         IF (LABFLG.GT.5) THEN
            LX=LOCX+104
            LY=LOCY-45
            print *,'PLOTCH: j,lx,ly,IPAT(J)(1:1)=',j,lx,ly,IPAT(J)(1:1)
            CALL MY_PLCHMQ(LOCX+104,LOCY-45,IPAT(J)(1:1),8.,0.,
     X           -1.)
         END IF
         LOCY=LOCY-30
         CABDBZ(I)=RNULL
         CALL GSCLIP(1)
 35   CONTINUE
 50   CONTINUE
      IF(NMAXC.EQ.0) GO TO 60
      IF(ZMAX(1).EQ.RNULL) GO TO 60
C     
C     LOCATE MAXIMUM VALUE ON PLOT
C     
      IF (LABFLG.GT.5) THEN
         WRITE (LABEL,104)ZMAX(1)
         CALL GSCLIP(0)
c         IF (IOLAY.EQ.1) LOCX=LOCX+70
c         IF (IOLAY.EQ.2) LOCX=LOCX+35
         CALL MY_PLCHMQ(LOCX+25,LOCY-40,LABEL(1:6),12.,0.,-1.)
         CALL MY_PLCHMQ(LOCX+99,LOCY-40,IPAX(1:1),18.,0.,-1.)
         CALL PLCHMQ(ZMAX(2),ZMAX(3),'+',15.,0.,-1.)
         XPLT=CSP(1,L1)+(ZMAX(2)-1)*CSP(3,L1)
         YPLT=CSP(1,L2)+(ZMAX(3)-1)*CSP(3,L2)
c         print *,'PLOTCH: just before'
         WRITE (*,53)(NAMF(N),N=1,4),ZMAX(1),
     X        AXNAM(L1),AXNAM(L2),XPLT,YPLT
 53      FORMAT(16X,'Max ',4A2,' is ',f8.3,
     X        ' at (',A4,',',A4,') = (',f8.3,',',f8.3,')')
c         print *,'PLOTCH: just after'
      END IF
 60   CONTINUE

      IF(L3.EQ.3)THEN
C     
C        PLOT MAP BACKGROUND IF REQUESTED AND Z-PLOT IS BEING PRODUCED
C        genmap.f - includes plotting of landmarks and political map
C     
c         IF(JMAPB.GE.1 .AND. JMAPB.LE.3)THEN
         IF(JMAPB.GE.1 .AND. JMAPB.LE.6)THEN
            CALL GENMAP(XL,XR,YB,YT,XRANGE(1),XRANGE(2),YRANGE(1),
     X           YRANGE(2),IPLTYP,ID,JMAPB,LATLON,THKMAP)
         END IF

CNotYet - LJM (11/27/02) haven't completed implementing the plotting
C         of NLDN or LMA lightning locations.  Plotting flag #7 can
C         only have values of 0-9 so that vectors, aircraft track, 
C         and mesonet stations either singly or in combination already
C         uses up 1-7.  Could use plotting flag #8 that controls the
C         plotting of landmarks and map background, singly or in
C         combination along with NLDN and LMA.
C
C     Plot NLDN locations - FLASH buffer #5 generated in PLTNLD
C
cNotYet         IF(JMAPB.EQ.4 .OR. JMAPB.EQ.6)CALL GFLAS3(5)
      IF(JMAPB.EQ.4 .OR. JMAPB.EQ.6)CALL GFLAS3(5)

C     Plot LMA locations - FLASH buffer #6 generated in PLTLMA
C
cNotYet         IF(JMAPB.EQ.5 .OR. JMAPB.EQ.6)CALL GFLAS3(6)

      END IF	
C     
C     IF VECTOR PLOTTING IS ACTIVE AN OVERLAY IS DESIRED FOR THIS PLOT
C     
      IF(IVECT.NE.0.AND. (JVECT.EQ.1 .OR. JVECT.EQ.3 .OR. JVECT.EQ.5
     X     .OR. JVECT.EQ.7)) CALL GFLAS3(1)
      IF(IAIR.EQ.1 .AND. (JVECT.EQ.2 .OR. JVECT.EQ.3 .OR. JVECT.EQ.6
     X     .OR. JVECT.EQ.7)) CALL GFLAS3(3)
      IF(ISTA.EQ.1 .AND. (JVECT.EQ.4 .OR. JVECT.EQ.5 .OR. JVECT.EQ.6
     X     .OR. JVECT.EQ.7)) CALL GFLAS3(4)

C     Next line is a placeholder until I figure out best way to handle
C     NLDN and/or LMA plots, singly or in combination with other overlays.
C
c      IF(INLD.NE.0 .AND. (JVECT.EQ.4 .OR. JVECT.EQ.5 .OR. JVECT.EQ.6
c     X     .OR. JVECT.EQ.7)) CALL GFLAS3(5)

C     DRAW PERIMETER ONLY AGAIN IN CASE AREA FILLS COVERED IT UP
      CALL PERIM(MAJORX,MINORX,MAJORY,MINORY)

      IF (LABFLG.GT.5) THEN
         IF(IOLAY.NE.1) CALL MYFRAME
      ELSE
c         CALL PERIM(0,0,0,0)
         IF (IOLAY.NE.1) CALL FRAME
      END IF

      CALL GSCLIP(1)
      RETURN
      END

