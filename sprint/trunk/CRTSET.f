      SUBROUTINE CRTSET(KRD,MAXPLN,MXCRT,ICRTST,MAXYZ)
C
C        SETS THE SPECIFICATIONS FOR THE GRIDS TO WHICH
C        INTERPOLATION IS TO BE DONE; THE GRIDS CAN EITHER
C        BE A STANDARD 3-D CARTESIAN GRIDS OR 2-D CARTESIAN 
C        GRIDS ON COPLANES OR ELEVATION SCAN SURFACES.  THE
C        CARTESIAN GRID CAN BE EITHER XY or LonLat.
C
C   VARIABLE  DESCRIPTION                  FIELD  NOTES
C   --------  -----------                  -----  -----
C
C   KOMM      'GRI'                         P1    COMMAND
C   X1        STARTING X                    P2    IN KM
C   X2        ENDING X                      P3    IN KM
C   Y1        STARTING Y                    P4    IN KM
C   Y2        ENDING Y                      P5    IN KM
C   XYD       DELTA(SPACING)FOR X ! Y AXES  P6    IN KM
C   Z1        STARTING Z OR COPLANE ANGLE   P7    IN (KM + MSL)
C   Z2        ENDING Z OR COPLANE ANGLE     P8    IN (KM + MSL)
C   ZD        DELTA (SPACING) FOR 3RD CRD.  P9    IN KM
C   ANGXAX    ANGLE OF X-AXIS,              P10   IN DEG,
C                CLOCKWISE FROM NORTH                ROTATED ABOUT THE
C                                                    LOWER LEFT CORNER
C                                                    OF THE CARTESIAN
C                                                    COORDINATE SYSTEM
C
C
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /AXUNIT/ IFIXAX,IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3),
     X     ORDER_LABL
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      CHARACTER*8 KRD(10)
      CHARACTER*8 XLABL,YLABL,ZLABL,AXNAM
      CHARACTER*11 ORDER_LABL

c      DATA MXCRT/255/
      DATA EPS/1.E-3/
      ICRTST=1

      READ (KRD,101)X1,X2,Y1,Y2,XYD,Z1,Z2,ZD,ANGXAX
 101  FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
C
C     -----------------------------------------------------------------
C     Set coplane flag (ICOPLANE) and output grid flag as a function of
C     the type of gridding (GRID or GRIDXYZ, GRIDCPL, GRIDPPI, GRIDLLE, 
C     GRIDLLZ) to be done and the type of input radar scan.  The only
C     values for ICOPLANE set here are: 0, 1, or 2.
C     -----------------------------------------------------------------
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C
C     ICOPLANE = 3  ==>  COPLANE SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 4  ==>  RHI SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 5  ==>  AIRBORNE SWEEPS, INTERPOLATING TO CART GRID
C                        Set in RADAR (DORADE format)
C     -----------------------------------------------------------------
C     Additional qualifier flags for type of output grid:
C     -----------------------------------------------------------------
C     The IPPI variable indicates whether or not the horizontal portion
C     of the interpolations are to be 2-D projected vertically onto the 
C     original scan surface.  The ILLE variable indicates whether the 
C     horizontal 2-D grid is XY or LonLat.
C     -----------------------------------------------------------------
C     IPPI     = 0  ==>  Normal interpolations to cartesian grid
C     IPPI     = 1  ==>  XY interpolations to original elevation surfaces
C     ILLE     = 1  ==>  LonLat interpolations to original elevation surfaces
C     ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C
      IF (KRD(1).EQ.'GRID' .OR. KRD(1).EQ.'GRIDXYZ') THEN
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     IPPI,ILLE,ILLZ = 0  ==>  Normal interpolations to cartesian grid
C
         ICOPLANE=0
         IPPI=0
         ILLE=0
         ILLZ=0
         XLABL=' X  (KM)'
         YLABL=' Y  (KM)'
         ZLABL=' Z  (KM)'
         ORDER_LABL=' X   Y   Z '

      ELSE IF (KRD(1).EQ.'GRIDCPL' .AND. Z1.EQ.0.0 .AND. Z2.EQ.0.0
     +    .AND. ZD.EQ.0.0) THEN
C
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     IPPI,ILLE,ILLZ = 0  ==>  Normal interpolations to cartesian grid
C
         ICOPLANE=1
         IPPI=0
         ILLE=0
         ILLZ=0
         XLABL=' X  (KM)'
         YLABL=' Y  (KM)'
         ZLABL=' C  (DG)'
         ORDER_LABL=' X   Y   C '

      ELSE IF (KRD(1).EQ.'GRIDCPL' .AND. (Z1.NE.0.0 .OR. Z2.NE.0.0
     +         .OR. ZD.NE.0.0)) THEN
C
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C     IPPI,ILLE,ILLZ = 0  ==>  Normal interpolations to cartesian grid
C
         ICOPLANE=2
         IPPI=0
         ILLE=0
         ILLZ=0
         XLABL=' X  (KM)'
         YLABL=' Y  (KM)'
         ZLABL=' C  (DG)'
         ORDER_LABL=' X   Y   C '

      ELSE IF (KRD(1).EQ.'GRIDPPI' .OR. KRD(1).EQ.'GRIDXYE') THEN
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     IPPI     = 1  ==>  XY interpolations to original elevation surfaces
C
         ICOPLANE=0
         IPPI=1
         ILLE=0
         ILLZ=0
         XLABL=' X  (KM)'
         YLABL=' Y  (KM)'
         ZLABL='ELE (DG)'
         ORDER_LABL='X Y ELE'

      ELSE IF (KRD(1).EQ.'GRIDLLE') THEN
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     IPPI     = 1  ==>  Cartesian interpolations to original elevation surfaces
C     ILLE     = 1  ==>  LonLat interpolations to original elevation surfaces
C
         ICOPLANE=0
         IPPI=1
         ILLE=1
         ILLZ=0
         XLABL='LON (DG)'
         YLABL='LAT (DG)'
         ZLABL='ELE (DG)'
         ORDER_LABL='LON LAT ELE'

      ELSE IF (KRD(1).EQ.'GRIDLLZ') THEN
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C
         ICOPLANE=0
         IPPI=0
         ILLE=0
         ILLZ=1
         XLABL='LON (DG)'
         YLABL='LAT (DG)'
         ZLABL=' Z  (KM)'
         ORDER_LABL='LON LAT  Z '
      END IF

      AXNAM(1)=XLABL
      AXNAM(2)=YLABL
      AXNAM(3)=ZLABL
C
C     Force consistency between min, max, spacing and number of grid points
C
      IF(XYD.LE.0.0) XYD=1.0
      X1=XYD*NINT(X1/XYD)
      Y1=XYD*NINT(Y1/XYD)
      X2=XYD*NINT(X2/XYD)
      Y2=XYD*NINT(Y2/XYD)
      NX=INT(1.0+EPS+(X2-X1)/XYD)
      NY=INT(1.0+EPS+(Y2-Y1)/XYD)
      X2=X1+(NX-1)*XYD
      Y2=Y1+(NY-1)*XYD

      XD = XYD
      YD = XYD
      REELX=(X2-X1)/XYD
      REELY=(Y2-Y1)/XYD
      NIX=REELX+EPS
      NIY=REELY+EPS
      RTSX=INT(FLOAT(NIX)+EPS)
      RTSY=INT(FLOAT(NIY)+EPS)
c      IF (REELX.GT.RTSX .OR. REELY.GT.RTSY) GO TO 9
c      GO TO 10
c 9    CONTINUE
c      PRINT 201
c 201  FORMAT (/'  +++ ERROR +++'/
c     X      '  AXES ARE NOT EVENLY DIVISIBLE BY SPACING INTERVAL'/
c     X      '         PLEASE RESPECIFY')
c      print *,'CRTSET: X1,X2,Y1,Y2,XYD=',x1,x2,y1,y2,xyd
c      print *,'CRTSET: REELX,REELY,NIX,NIY,RTSX,RTSY=',
c     +     REELX,REELY,NIX,NIY,RTSX,RTSY
c      STOP 3332
 10   CONTINUE
      IF(NX.GT.0.AND.NX.LE.MXCRT.AND.NY.GT.0.AND.NY.LE.MXCRT)GO TO 15
      PRINT 301, NX,NY,NX*NY,MXCRT
  301 FORMAT(/' +++  ERROR  +++',/,
     X     ' Number of  X grid points = ',I3,/,
     X     ' Number of  Y grid points = ',I3,/,
     X     ' Number of XY grid points = ',I6,/,
     X     ' NUMBER OF POINTS ALONG EITHER HORIZONTAL AXIS MUST BE IN ',
     X    /' THE RANGE OF 1 TO ',I3,'.  RE-SPECIFY HORIZONTAL GRID.')
      STOP
   15 CONTINUE
C
C     SCALE X AND Y AXIS SPECIFICATIONS
C
      X1=X1/SCLAXS(1,IUNAXS)
      X2=X2/SCLAXS(1,IUNAXS)
      XYD=XYD/SCLAXS(1,IUNAXS)
      Y1=Y1/SCLAXS(2,IUNAXS)
      Y2=Y2/SCLAXS(2,IUNAXS)
      XYD=XYD/SCLAXS(2,IUNAXS)
C
C     ONLY EXECUTE THE NEXT LINES IF NOT INTERP. TO COPLANE OR IF
C     INTERPOLATING TO USER SPECIFIED COPLANES
C
      IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.2 .OR. ICOPLANE.EQ.3 .OR.
     X     ICOPLANE.EQ.4 .OR. ICOPLANE.EQ.5) THEN
         IF(ZD.LE.0.0) ZD=1.0
         RZ=(Z2-Z1)/ZD+1.0+EPS
         NZ=RZ
c         IF(ABS(FLOAT(NZ)-RZ).GT.0.01) GO TO 9
      ELSE IF (ICOPLANE.EQ.1) THEN
         NZ=0
      END IF
C
C     SCALE VERTICAL AXIS TO PROPER UNITS
C
      Z1=Z1/SCLAXS(3,IUNAXS)
      Z2=Z2/SCLAXS(3,IUNAXS)
      ZD=ZD/SCLAXS(3,IUNAXS)
      IF (ANGXAX.EQ.0.0 .AND. ICOPLANE.NE.0) THEN
         WRITE(*,*)'+++ ERROR: X-AXIS ORIENTATION ANGLE MUST BE',
     X             ' SPECIFIED FOR COPLANE COORDINATE SYSTEMS +++'
         STOP
      END IF
      IF(ANGXAX.LE.0.0) ANGXAX=90.0
      IF(ANGXAX.GT.0.0.AND.ANGXAX.LT.270.0) GO TO 45
      PRINT 121
  121 FORMAT(/' +++  ERROR  +++'/
     X        ' ANGLE MUST BE IN THE RANGE 0-270 DEGREES ')
      STOP
   45 CONTINUE
C
C     CHECK IF ANYTHING IS OUT OF RANGE AND RETURN
C
      IF(NZ.LE.MXCRT) GO TO 95
      PRINT 114, NZ, MXCRT
  114 FORMAT(/' +++  ERROR  +++'/
     X       ' NUMBER OF LEVELS=',I6,' ...CANNOT EXCEED ',I6)
      STOP
   95 CONTINUE
      NPLANE=NX*NY
      NTOTAL=NZ*NPLANE
      IF(NTOTAL.LE.MAXYZ) GO TO 96
      PRINT 112, NTOTAL,MAXYZ
  112 FORMAT(/' +++  ERROR  +++'/
     X ' NUMBER OF CARTESIAN PTS=',I8,' ... CANNOT EXCEED',I8)
      STOP
   96 CONTINUE
      IF(NPLANE.LE.MAXPLN) GO TO 99
      PRINT 115, NPLANE,MAXPLN
  115 FORMAT(/' +++  ERROR  +++'/
     X       ' NUMBER OF PTS/PLANE=',I6,' ...CANNOT EXCEED ',I6)
      STOP

   99 CONTINUE
      PRINT 885
885   FORMAT(//5X,'SUMMARY OF GRID COMMAND ')
      PRINT 887
887   FORMAT(5X,'------- -- ---- ------- ')
      PRINT *,'     ICOPLANE=',ICOPLANE,
     X     ' and IPPI,ILLE,ILLZ=',IPPI,ILLE,ILLZ
      PRINT 888, XLABL,X1,X2,XYD,NX,
     X           YLABL,Y1,Y2,XYD,NY,
     X           ZLABL,Z1,Z2,ZD, NZ,
     X           ORDER_LABL,ANGXAX
 888  FORMAT(/5X,'GRIDDED OUTPUT COORDINATE SYSTEM SPECIFICATIONS: ',
     X     //8X,'AXIS',7X,'MINIMUM',3X,'MAXIMUM',5X,'DELTA',
     X     3X,'NO. OF PTS.',
     X      /8X,'----',7X,'-------',3X,'-------',5X,'-----',
     X     3X,'-----------',
     X     /6X,A8,2X,3F10.3,I10,
     X     /6X,A8,2X,3F10.3,I10,
     X     /6X,A8,2X,3F10.3,I10,
     X     //6X,'AXIS ORDER: [',A11,']',
     X     /6X,'ANGLE OF THE X-AXIS RELATIVE TO NORTH: ',
     X     F6.1,' DEGREES ',
     X     /6X,'(X,Y)  AXES ARE SPECIFIED ',
     X     'RELATIVE TO:  (0.00,0.00)'//)
      IF(IPPI.EQ.1)THEN
         MAXZ=MAXYZ/MAXPLN
         PRINT 111, MAXZ
 111     FORMAT(
     X   6x,'       +++  WARNING - GRIDPPI or GRIDLLE +++        ',/,
     X   6x,'NUMBER OF SWEEPS THAT CAN BE PROCESSED CANNOT EXCEED',I4,/)
      END IF

      RETURN
      END
