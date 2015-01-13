c
c----------------------------------------------------------------------X
c
      SUBROUTINE COORD(IOUT,NIN1,NGRD,IDIR,X0,Y0,H0,AVGI,C1,C2,C3,C4,
     X     ANGXAX)
C
C  EXTRACT COORDINATES RELATIVE TO (C1,C2,C3) AND SPECIFIED BY NIN1:
C
C     IOUT      - Output field number
C     NIN1(1:1) - Name of desired coordinates (X,Y,Z,R,H,A,E)
C     NGRD      - Generate NIN1(1:1) at regular (NGRD='REG') or
C                 radar sampling locations (NGRD='   ')
C     IDIR      - Will designate 'WEST' or 'EAST' longitude.
C     ANGXAX    - Azimuth angle of +X axis
C
C     AZA,ELA   - AZIMUTH AND ELEVATION ANGLES OF RADAR BEAMS
C     ITPOLD    - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                         ELA     "     AZIMUTH    "  ]
C     X0,Y0,HO  - (X,Y,Z) COORDINATES FOR  THIS   RADAR
C     C1,C2,C3  -    "         "       "  ANOTHER   "
C               - When the output coordinate is latitude or longitude,
C                 then (C1,C2)=(latitude,longitude) for the origin.
C     C4        - FLAG: (0) C1,C2,C3 ARE FOR  THIS   RADAR
C                       (1)     "     "   "  ANOTHER   "
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      PARAMETER (MXFUN=9,KMX=10,MXW=255)
      DIMENSION RNUM(9)
      CHARACTER*8 LFUN(MXFUN),NIN1,NCOR
      CHARACTER*3 NGRD
      CHARACTER*4 IDIR
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA PI,PI2,PI4/3.141592654,6.283185307,12.56637061/
      DATA EPS/0.001/
      DATA LFUN/'X       ','Y       ','Z       ','R       ',
     +          'H       ','A       ','E       ','LAT     ',
     +          'LON     '/
      DATA IFLG/1/

C     FIND THE INDEX (IFUN) OF THE FUNCTION (NIN1)
C     WITHIN A LONG LIST (LFUN) OF LENGTH MXFUN.
C
      NCOR='        '
      NCOR(1:3)=NIN1(1:3)
      IFUN=IFIND(NCOR,LFUN,MXFUN)
c      print *,'lfun=',lfun(ifun),ifun
c      print *,'c1-4,angx =',c1,c2,c3,c4,angxax
c      print *,'RADAR(XYZ)=',x0,y0,h0
      IF(IFUN.EQ.0)THEN
         PRINT *,'*** WARNING - UNKNOWN COORDINATE ***'
         RETURN
      END IF

C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C     RHI (3) - AZA contains elevation angle and FXOLD = azimuth
C     Others  - AZA contains azimuth   angle and FXOLD = elevation
C
      IF(NGRD.EQ.'REG')THEN
         ISW=2
      ELSE
         ISW=1
      END IF
c      print *,'COORD: ngrd,ifun,isw,itp=',ngrd,'xx',ifun,isw,itpold

      IF(IFUN.EQ.8 .OR. IFUN.EQ.9)THEN
         RLAT=C1
         IF(IDIR.EQ.'EAST')THEN
            RLON=-1.0*ABS(C2)
         ELSE
            RLON=+1.0*ABS(C2)
         END IF
      END IF

      DO 14 J=1,NANG(ISW)

C        RHI scan:
C
         IF(ITPOLD.EQ.3)THEN
            IF(ISW.EQ.2)THEN
               AZRAD=FXOLD-AZROT(ITPOLD)
            ELSE
               AZRAD=ELA(J,ISW)
            END IF
            ELRAD=AZA(J,ISW)

C        All other scans:
C
         ELSE
            AZRAD=AZA(J,ISW)-AZROT(ITPOLD)
            IF(ISW.EQ.2)THEN
               ELRAD=FXOLD
            ELSE
               ELRAD=ELA(J,ISW)
            END IF
            IF(AZRAD.LT.0.0)AZRAD=AZRAD+360.0
         END IF
         SINA=SIN(AZRAD*TORAD)
         COSA=COS(AZRAD*TORAD)
         SINE=SIN(ELRAD*TORAD)
         COSE=COS(ELRAD*TORAD)
c         print *,'COORD: azrad,sina,cosa=',azrad,sina,cosa
c         print *,'COORD: elrad,sine,cose=',elrad,sine,cose

         IF(C4.EQ.0.0)THEN
            RNUM(6)=AZRAD
            RNUM(7)=ELRAD
            IF(RNUM(6).LT.0.0)RNUM(6)=RNUM(6)+360.0
         END IF

         DO 12 I=MNGATE,MXGATE
            HRNG=RNG(I,ISW)*COSE
            X=X0+HRNG*SINA
            Y=Y0+HRNG*COSA
            Z=H0+RNG(I,ISW)*SINE+0.5*HRNG*HRNG*REI

c     NINT operation for testing nearest Cartesian grid point
c     This XY_QUANT is not passed around to other routines
c     such as PLT_RGLOC.  It is only used for testing within
c     this routine.
c
            XY_QUANT=5.0
            X=XY_QUANT*NINT(X/XY_QUANT)
            Y=XY_QUANT*NINT(Y/XY_QUANT)
            if(iflg.eq.0)then
               write(4,1700)j,i,azrad,rng(i,isw),x,y
 1700          format('COORD: j,i,ar,xy=',2i5,4f8.3)
            endif

            RNUM(1)=X
            RNUM(2)=Y
            RNUM(3)=Z
            IF(IFUN.EQ.8 .OR. IFUN.EQ.9)THEN
               IF (ABS(X).LT.EPS .AND. ABS(Y).LT.EPS)THEN
                  PLAT=RLAT
                  PLON=RLON
               ELSE
                  CALL XY2LLDRV(PLAT,PLON,X,Y,RLAT,RLON,ANGXAX)
               END IF
               RNUM(8)=PLAT
               RNUM(9)=PLON
            END IF

C  CALCULATE (R,H,A,E) COORDINATES FROM (C4=0.0) THIS RADAR, 
C                                    OR (C4=1.0) ANOTHER RADAR
C
            IF(C4.EQ.0.0)THEN
               RNUM(4)=RNG(I,ISW)
               RNUM(5)=HRNG
            ELSE
               XP=X-C1
               YP=Y-C2
               ZP=Z-C3
               RNUM(4)=SQRT(XP*XP+YP*YP+ZP*ZP)
               RNUM(5)=SQRT(XP*XP+YP*YP)
               IF(YP.EQ.0.0.AND.XP.EQ.0.0)THEN
                  AZ=180.0
               ELSE IF(YP.EQ.0.0.AND.XP.GT.0.0)THEN
                  AZ=90.0
               ELSE IF(YP.EQ.0.0.AND.XP.LT.0.0)THEN
                  AZ=270.0
               ELSE
                  AZ=TODEG*ATAN2(XP,YP)
               END IF
               IF(AZ.LT.0.0)THEN
                  RNUM(6)=AZ+360.0
               ELSE
                  RNUM(6)=AZ
               END IF
               HDIST=SQRT(XP*XP+YP*YP)
               IF(HDIST.EQ.0.0.AND.ZP.EQ.0.0)THEN
                  EL=0.0
               ELSE IF(HDIST.EQ.0.0.AND.ZP.GT.0.0)THEN
                  EL=90.0
               ELSE IF(HDIST.EQ.0.0.AND.ZP.LT.0.0)THEN
                  EL=-90.0
               ELSE
                  EL=TODEG*ATAN2(ZP,HDIST)
               END IF
               RNUM(7)=EL
            END IF

            DAT(I,J,IOUT)=RNUM(IFUN)
   12    CONTINUE
   14 CONTINUE

      if(iflg.eq.0)iflg=1
      RETURN
      END



