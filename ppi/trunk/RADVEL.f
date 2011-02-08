c
c----------------------------------------------------------------------X
c
      SUBROUTINE RADVEL(DAT,IOUT,NIN1,C1,C2,C3,C4,C5,BDVAL,MNGATE,
     X     MXGATE,NANG,AZA,ELA,ITPOLD,X0,Y0,H0,R0,DROLD,MXR,MXA,MXF)
C
C  FUNCTION - RADVEL: F(OUT)=RADIAL VELOCITY FOR A RADAR AT (X0,Y0,H0)
C                            FROM ANALYTIC CARTESIAN WINDS
C
C               U = Spd * sin (Dir-PI)
C               V = Spd * cos (Dir-PI)
C            Vrad = U*sin(A)*cos(E) + V*cos(A)*cos(E) + W*sin(E)
C
C----------------------------------------------------------------------X
C  Analytic cartesian winds:
C
C  1) Sinusoid wind speed and linear direction; maximum at zm=hgtmax
C                 DIR(z) = D(zm) + DIR(zm)*(z-zm)
C                 SPD(z) = SPD(zm)*SIN[PI*(z-zo)/(2*zm)]
C  2) Linear wind with convergence and deformations.  Neglects vorticity.
C               U = U0 - 0.5*CON*X + 0.5*STR*X + 0.5*SHR*Y
C               V = V0 - 0.5*CON*Y - 0.5*STR*Y + 0.5*SHR*X
C  3) Horizontal rolls with divergence A (m/s per km), and three
C     wavelengths RX, RY, and RZ (km).
C        Formulate horizontal divergence - divide divergence equally
C        between DU/DX AND DV/DY; Note that DW/DZ = -(DU/DX + DV/DY).
C        solve (DU/DX,DV/DY,DW/DZ) for (U,V,W).  No density weighting.
C        ---------------------------------------------------------------
C        DIV   =            A * COS[2*PI*(X/RX+Y/RY)] * COS(2*PI*Z/RZ)
C        DU/DX =          A/2 * COS[2*PI*(X/RX+Y/RY)] * COS(2*PI*Z/RZ)
C        DV/DY =          A/2 * COS[2*PI*(X/RX+Y/RY)] * COS(2*PI*Z/RZ)
C        DW/DZ =           -A * COS[2*PI*(X/RX+Y/RY)] * COS(2*PI*Z/RZ)
C        U     =  (A*RX/4*PI) * SIN[2*PI*(X/RX+Y/RY)] * COS(2*PI*Z/RZ)
C        V     =  (A*RY/4*PI) * SIN[2*PI*(X/RX+Y/RY)] * COS(2*PI*Z/RZ)
C        W     = -(A*RZ/2*PI) * COS[2*PI*(X/RX+Y/RY)] * SIN(2*PI*Z/RZ)
C           The level of non-divergence and zero-winds is at RZ/4.
C           The magnitude of divergence is a maximum at z=0 and z=RZ/2,
C           where the vertical motion is zero.
C----------------------------------------------------------------------X
C
C     IOUT     - OUTPUT FIELD NUMBER
C     AZA,ELA  - AZIMUTH AND ELEVATION ANGLES OF RADAR BEAMS
C     ITPOLD   - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                        ELA     "     AZIMUTH    "  ]
C     X0,Y0,H0 - (X,Y,Z) COORDINATES OF THE RADAR
C     R0,DROLD - INITIAL RANGE (KM) AND GATE SPACING
C     C1,C2,C3,C4,C5 - WIND PARAMETERS
C
C
      PARAMETER (MXV=10)
      CHARACTER*8 VFUN(MXV),NIN1

      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA PI,PI2,PI4/3.141592654,6.283185307,12.56637061/

      DATA VFUN/'SINUSOID','LINEAR  ','ROLLS   ',
     +        7*'??????? '/

C     FIND THE INDEX (IFUN) OF THE FUNCTION (NIN1)
C     WITHIN A LONG LIST (LFUN) OF LENGTH MXFUN.
C
      IFUN=IFIND(NIN1,VFUN,MXV)
      IF(IFUN.EQ.0)THEN
         PRINT *,'*** WARNING - UNKNOWN RADVEL FUNCTION ***',VFUN
         RETURN
      END IF

      GO TO (11,12,13)IFUN

C     SINUSOID SPEED - LINEAR DIRECTION
C     
 11   CONTINUE
      SPDMAX = C1
      DIRMAX = C2
      DELDIR = C3
      HGTMAX = C4
      PIZMAX = PI/(2.0*HGTMAX)
      GO TO 21

C     LINEAR WINDS - (CONVERGENCE AND DEFORMATION)
C     
 12   CONTINUE
      U0  = C1
      V0  = C2
      CON = C3
      SHR = C4
      STR = C5
      GO TO 21

C     HORIZONTAL ROLLS
C     
 13   CONTINUE
      A  = C1
      RX = C2
      RY = C3
      RZ = C4
      IF((RX*RY*RZ).EQ.0.0)THEN
         WRITE(6,131)
 131     FORMAT(1X,'*** ERROR: WAVELENGTH MUST BE NON-ZERO ***')
         STOP
      END IF
      GO TO 21

 21   CONTINUE
      DO 200 J=1,NANG
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
         DO 190 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            RNG=R0+(I-1)*DROLD
            IF(RNG.LE.0.0)GO TO 190
            HRNG=RNG*COSE
            X=X0+HRNG*SINA
            Y=Y0+HRNG*COSA
C            Z=H0+RNG*SINE+0.5*HRNG*HRNG*REI
            Z=RNG*SINE+0.5*HRNG*HRNG*REI

            GO TO (31,32,33)IFUN

 31         CONTINUE
C     SINUSOID SPEED - LINEAR DIRECTION
C     
            SPD = SPDMAX*SIN(Z*PIZMAX)
            DIR = DIRMAX + DELDIR*(Z-HGTMAX)
            U = SPD*SIN(TORAD*(DIR-180.0))
            V = SPD*COS(TORAD*(DIR-180.0))
            W = 0.0
            GO TO 110

 32         CONTINUE
C     LINEAR WITH DIVERGENCE AND DEFORMATION
C     
            U = U0 - 0.5*CON*X + 0.5*STR*X + 0.5*SHR*Y
            V = V0 - 0.5*CON*Y - 0.5*STR*Y + 0.5*SHR*X
            W = 0.0
            GO TO 110
            
 33         CONTINUE
C     HORIZONTAL ROLLS
C     
C        INFINITELY LONG X WAVELENGTH (RX=999.9):
C           RX=999.9 and RY .NE. 999.9; U=0 and V varies
C        INFINITELY LONG Y WAVELENGTH (RY=999.9):
C           RX .NE. 999.9 and RY=999.9; U varies and V=0
C        FINITE X and Y WAVELENGTHS:
C           Both RX and RY .NE. 999.9;  U and V vary
C
            IF(RX.EQ.999.9.AND.RY.NE.999.9)THEN
               XYSIN=SIN(PI2*Y/RY)
               XYCOS=COS(PI2*Y/RY)
               ARX=0.0
               ARY=A*RY/PI4
            ELSE IF(RX.NE.999.9.AND.RY.EQ.999.9)THEN
               XYSIN=SIN(PI2*X/RX)
               XYCOS=COS(PI2*X/RX)
               ARX=A*RX/PI4
               ARY=0.0
            ELSE
               XYSIN=SIN(PI2*(X/RX+Y/RY))
               XYCOS=COS(PI2*(X/RX+Y/RY))
               ARX=A*RX/PI4
               ARY=A*RY/PI4
            END IF

C           RZ=999.9: W COMPONENT IS 0; OTHERWISE VARIES
C
            IF(RZ.NE.999.9)THEN
               ZSIN=SIN(PI2*Z/RZ)
               ZCOS=COS(PI2*Z/RZ)
               ARZ=-A*RZ/PI2
            ELSE
               ZSIN=0.0
               ZCOS=1.0
               ARZ=0.0
            END IF

            U= ARX*XYSIN*ZCOS
            V= ARY*XYSIN*ZCOS
            W= ARZ*XYCOS*ZSIN

 110        DAT(I,J,IOUT) = U*SINA*COSE+V*COSA*COSE+W*SINE

 190     CONTINUE
 200  CONTINUE

      RETURN
      END







