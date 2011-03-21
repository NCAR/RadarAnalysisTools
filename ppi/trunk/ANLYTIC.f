c
c----------------------------------------------------------------------X
c
      SUBROUTINE ANLYTIC(DAT,IOUT,NIN1,NIN2,C1,C2,C3,C4,BDVAL,MNGATE,
     X                   MXGATE,NANG,AZA,ELA,ITPOLD,X0,Y0,H0,R0,DROLD,
     X                   GXMIN,GXMAX,GYMIN,GYMAX,MXR,MXA,MXF)
C
C  SEVERAL ANALYTIC FUNCTIONS:
C
C     IOUT   - OUTPUT FIELD NUMBER
C     NIN1   - NAME OF ANALYTIC FUNCTION
C
C     AZA,ELA     - AZIMUTH AND ELEVATION ANGLES OF RADAR BEAMS
C     ITPOLD      - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                           ELA     "     AZIMUTH    "  ]
C     R0,DROLD    - INITIAL RANGE (KM) AND GATE SPACING
C
      PARAMETER (MXFUN=5,KMX=10,MXW=255)
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DIMENSION GXMIN(8),GXMAX(8),GYMIN(8),GYMAX(8)
      DIMENSION AMP(KMX),SX(KMX),SY(KMX),ANGL(KMX),XC(KMX),YC(KMX)
      DIMENSION RNUM(6),RNOF(6)
      DIMENSION DWT(MXW)
      CHARACTER*8 LFUN(MXFUN),NFILT(5)
      CHARACTER*8 NIN1,NIN2,IFLTYP
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA PI,PI2,PI4/3.141592654,6.283185307,12.56637061/
      DATA IR1/3/
      DATA RNOF/0.5,0.5,0.5,-0.5,0.0,0.0/
      DATA LFUN/'COSPROD ','GAUSIAN ','COSXYZ  ','COSRNG  ',
     +          'FILTER  '/
      DATA NFILT/'UNI','TRI','CRE','QUA','EXP'/

C     FIND THE INDEX (IFUN) OF THE FUNCTION (NIN1)
C     WITHIN A LONG LIST (LFUN) OF LENGTH MXFUN.
C
      IFUN=IFIND(NIN1,LFUN,MXFUN)
      IF(IFUN.EQ.0)THEN
         PRINT *,'*** WARNING - UNKNOWN ANALYTIC FUNCTION ***'
         GO TO 1000
      END IF
      GO TO ( 10, 20, 30, 40, 50)IFUN

C     COSPROD: F(OUT)=A*COS(2*PI*X/RX)*COS(2*PI*Y/RY)*COS(2*PI*Z/RZ)
C             C1,C2,C3,C4 - AMPLITUDE AND (X,Y,Z) WAVELENGTHS
C
   10 A=C1
      RX=C2
      RY=C3
      RZ=C4
      DO 14 J=1,NANG
         IF(ITPOLD.EQ.3)THEN
            SINA=SIN(ELA(J,1)*TORAD)
            COSA=COS(ELA(J,1)*TORAD)
            SINE=SIN(AZA(J,1)*TORAD)
            COSE=COS(AZA(J,1)*TORAD)
         ELSE
            SINA=SIN(AZA(J,1)*TORAD)
            COSA=COS(AZA(J,1)*TORAD)
            SINE=SIN(ELA(J,1)*TORAD)
            COSE=COS(ELA(J,1)*TORAD)
         END IF
         DO 12 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            RNG=R0+(I-1)*DROLD
            HRNG=RNG*COSE
            X=X0+HRNG*SINA
            Y=Y0+HRNG*COSA
            Z=H0+RNG*SINE+0.5*HRNG*HRNG*REI

C           RX=0: SCALAR DOESN'T VARY IN X; OTHERWISE VARIES
C
            IF(RX.NE.0.0)THEN
               XCOS=COS(PI2*X/RX)
            ELSE
               XCOS=1.0
            END IF

C           RY=0: SCALAR DOESN'T VARY IN Y; OTHERWISE VARIES
C
            IF(RY.NE.0.0)THEN
               YCOS=COS(PI2*Y/RY)
            ELSE
               YCOS=1.0
            END IF

C           RZ=0: SCALAR DOESN'T VARY IN Z; OTHERWISE VARIES
C
            IF(RZ.NE.0.0)THEN
               ZCOS=COS(PI2*Z/RZ)
            ELSE
               ZCOS=1.0
            END IF
            DAT(I,J,IOUT)=A*XCOS*YCOS*ZCOS
   12    CONTINUE
   14 CONTINUE
      GO TO 1000

C     GAUSIAN: F(OUT)=C1*EXP(-(XP/C2)**2-(YP/C3)**2)
C              C1,C2,C3 - AMPLITUDE AND (X,Y) SCALING FACTORS
C              C4 - NUMBER OF ELLIPSES
C
   20 CONTINUE
C      IR1=IR1+2
      XDST=GXMAX(ITPOLD)-GXMIN(ITPOLD)
      YDST=GYMAX(ITPOLD)-GYMIN(ITPOLD)
      KEND=NINT(C4*RANF())
      IF(KEND.GT.KMX)KEND=KMX
      IF(KEND.LT.1)KEND=1
         WRITE(6,1770)KEND
1770     FORMAT(/,1X,' NUMBER OF GAUSSIAN CELLS=',I4)
         WRITE(6,1772)C1,C2,C3,180.0,XDST,YDST
1772     FORMAT(1X,'  C1,C2,C3,ANG,XDST,YDST=',4X,6F8.2)
      DO 28 K=1,KEND
         DO 22 L=1,6
            RNUM(L)=RANF()+RNOF(L)
   22    CONTINUE
         WRITE(6,1774)(RNUM(L),L=1,6)
1774     FORMAT(1X,'                    RNUM=',4X,6F8.2)
         AMP(K)=C1*RNUM(1)
         SX(K)=C2*RNUM(2)
         SY(K)=C3*RNUM(3)
         ANGL(K)=180.0*RNUM(4)
         SOR=SIN(ANGL(K)*TORAD)
         COR=COS(ANGL(K)*TORAD)
         XC(K)=GXMIN(ITPOLD)+XDST*RNUM(5)
         YC(K)=GYMIN(ITPOLD)+YDST*RNUM(6)
         WRITE(6,1776)K,AMP(K),SX(K),SY(K),ANGL(K),XC(K),YC(K)
1776     FORMAT(1X,'        K,A,SXY,ANGL,XYC=',I4,6F8.2)
         DO 26 J=1,NANG
            IF(ITPOLD.EQ.3)THEN
               SINA=SIN(ELA(J,1)*TORAD)
               COSA=COS(ELA(J,1)*TORAD)
               SINE=SIN(AZA(J,1)*TORAD)
               COSE=COS(AZA(J,1)*TORAD)
            ELSE
               SINA=SIN(AZA(J,1)*TORAD)
               COSA=COS(AZA(J,1)*TORAD)
               SINE=SIN(ELA(J,1)*TORAD)
               COSE=COS(ELA(J,1)*TORAD)
            END IF
            DO 24 I=MNGATE,MXGATE
               IF(K.EQ.1)DAT(I,J,IOUT)=0.0
               RNG=R0+(I-1)*DROLD
               HRNG=RNG*COSE
               X=X0+HRNG*SINA-XC(K)
               Y=Y0+HRNG*COSA-YC(K)
               XP=( X*COR+Y*SOR)/SX(K)
               YP=(-X*SOR+Y*COR)/SY(K)
               DAT(I,J,IOUT)=DAT(I,J,IOUT)+AMP(K)*EXP(-(XP*XP)-(YP*YP))
   24       CONTINUE
   26    CONTINUE
   28 CONTINUE

C     COSXYZ : F(OUT)=A*COS(2*PI*((X/RX)+(Y/RY)+(Z/RZ)))
C             C1,C2,C3,C4 - AMPLITUDE AND (X,Y,Z) WAVELENGTHS
C
   30 A=C1
      RX=C2
      RY=C3
      RZ=C4
      DO 34 J=1,NANG
         IF(ITPOLD.EQ.3)THEN
            SINA=SIN(ELA(J,1)*TORAD)
            COSA=COS(ELA(J,1)*TORAD)
            SINE=SIN(AZA(J,1)*TORAD)
            COSE=COS(AZA(J,1)*TORAD)
         ELSE
            SINA=SIN(AZA(J,1)*TORAD)
            COSA=COS(AZA(J,1)*TORAD)
            SINE=SIN(ELA(J,1)*TORAD)
            COSE=COS(ELA(J,1)*TORAD)
         END IF
         DO 32 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            RNG=R0+(I-1)*DROLD
            HRNG=RNG*COSE
            X=X0+HRNG*SINA
            Y=Y0+HRNG*COSA
            Z=H0+RNG*SINE+0.5*HRNG*HRNG*REI

C           RX=0: SCALAR DOESN'T VARY IN X; OTHERWISE VARIES
C
            IF(RX.NE.0.0)THEN
               XX=PI2*X/RX
            ELSE
               XX=0.0
            END IF

C           RY=0: SCALAR DOESN'T VARY IN Y; OTHERWISE VARIES
C
            IF(RY.NE.0.0)THEN
               YY=PI2*Y/RY
            ELSE
               YY=0.0
            END IF

C           RZ=0: SCALAR DOESN'T VARY IN Z; OTHERWISE VARIES
C
            IF(RZ.NE.0.0)THEN
               ZZ=PI2*Z/RZ
            ELSE
               ZZ=0.0
            END IF
            DAT(I,J,IOUT)=A*COS(XX+YY+ZZ)
   32    CONTINUE
   34 CONTINUE
      GO TO 1000

C     COSRNG : F(OUT)=A*COS(2*PI*RNG/RR)
C             C1,C2 - AMPLITUDE AND (RR) WAVELENGTH
C
   40 A=C1
      RR=C2
      DO 44 J=1,NANG
         DO 42 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            RNG=R0+(I-1)*DROLD
            IF(RR.NE.0.0)DAT(I,J,IOUT)=A*COS(PI2*RNG/RR)
   42    CONTINUE
   44 CONTINUE
      GO TO 1000

C     FILTER : F(OUT)=NFILT(INDX) EVALUATED AT MIDRANGE
C             C2 - RADIUS IN RANGE
C
   50 CONTINUE
      IFLTYP=NIN2
      INDX=IFIND(IFLTYP,NFILT,5)
      IF(INDX.EQ.0)THEN
         WRITE(6,52)
   52    FORMAT(1X,'*** WARNING - UNKNOWN FILTER, RESET TO TRI ***')
         INDX=2
      END IF
      I1=-INT(C1)
      I2= INT(C1)
      DDX=(C1+1.0)*DROLD
      BIGR=DDX*DDX
      SUMW=0.0
      DO 60 I=I1,I2
         II=I-I1+1
         SMALR=I*I*DROLD*DROLD
         GO TO (53,54,55,56,57),INDX
   53    DWT(II)=1.0
         GO TO 58
   54    DWT(II)=1-SQRT(SMALR/BIGR)
         GO TO 58
   55    DWT(II)=(BIGR-SMALR)/(BIGR+SMALR)
         GO TO 58
   56    DWT(II)=1-(SMALR/BIGR)
         GO TO 58
   57    DWT(II)=EXP(-4.0*SMALR/BIGR)
   58    SUMW=SUMW+DWT(II)
   60 CONTINUE
      MDGATE=(MXGATE+MNGATE)/2
      I1=MDGATE-INT(C1)
      I2=MDGATE+INT(C1)
      SUMW=1.0
      DO 64 J=1,NANG
         II=0
         DO 62 I=MNGATE,MXGATE
            IF(I.GE.I1.AND.I.LE.I2)THEN
               II=II+1
               DAT(I,J,IOUT)=DWT(II)/SUMW
            ELSE
               DAT(I,J,IOUT)=0.0
            END IF
   62    CONTINUE
   64 CONTINUE
      GO TO 1000

 1000 RETURN
      END
