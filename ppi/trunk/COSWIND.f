c
c----------------------------------------------------------------------X
c
      SUBROUTINE COSWIND(DAT,IOUT,C1,C2,C3,C4,BDVAL,MNGATE,MXGATE,MANG,
     X     AZA,ELA,AZROT,ITPOLD,X0,Y0,H0,R0,DROLD,MXR,MXA,MXF,NGRD,
     X     FXOLD)
C
C  FUNCTION - COSWIND: F(OUT)=RADIAL VELOCITY FOR A RADAR AT (X0,Y0,H0)
C                             FROM ANALYTIC CARTESIAN WINDS
C
C       -----------------------------------------------------------------------
C       ANALYTIC CARTESIAN WINDS:  (AMPLITUDES AND WAVELENGTHS) = (A,RX,RY,RZ)
C          COMPUTES CARTESIAN COMPONENTS, ASSUMING THE CURRENT
C          (X,Y,Z) COORDINATES ARE RELATIVE CARTESIAN COORDINATES.
C          FORMULATE HORIZONTAL DIVERGENCE - DIVIDE DIVERGENCE EQUALLY
C          BETWEEN DU/DX AND DV/DY; NOTE THAT DW/DZ = -(DU/DX + DV/DY).
C          SOLVE (DU/DX,DV/DY,DW/DZ) FOR (U,V,W).  NO DENSITY WEIGHTING.
C       -----------------------------------------------------------------------
C       DIV   =            A * COS(2*PI*X/RX) * COS(2*PI*Y/RY) * COS(2*PI*Z/RZ)
C       DU/DX =          A/2 * COS(2*PI*X/RX) * COS(2*PI*Y/RY) * COS(2*PI*Z/RZ)
C       DV/DY =          A/2 * COS(2*PI*X/RX) * COS(2*PI*Y/RY) * COS(2*PI*Z/RZ)
C       DW/DZ =           -A * COS(2*PI*X/RX) * COS(2*PI*Y/RY) * COS(2*PI*Z/RZ)
C       U     =  (A*RX/4*PI) * SIN(2*PI*X/RX) * COS(2*PI*Y/RY) * COS(2*PI*Z/RZ)
C       V     =  (A*RY/4*PI) * COS(2*PI*X/RX) * SIN(2*PI*Y/RY) * COS(2*PI*Z/RZ)
C       W     = -(A*RZ/2*PI) * COS(2*PI*X/RX) * COS(2*PI*Y/RY) * SIN(2*PI*Z/RZ)
C               THE LEVEL OF NON-DIVERGENCE AND ZERO-WINDS IS AT RZ/4.
C               THE MAGNITUDE OF DIVERGENCE IS A MAXIMUM AT Z=0 AND Z=RZ/2,
C               WHERE THE VERTICAL MOTION IS ZERO.
C       -----------------------------------------------------------------------
C
C     AZA,ELA     - AZIMUTH AND ELEVATION ANGLES OF RADAR BEAMS
C     ITPOLD      - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                           ELA     "     AZIMUTH    "  ]
C     X0,Y0,H0    - (X,Y,Z) COORDINATES OF THE RADAR
C     R0,DROLD    - INITIAL RANGE (KM) AND GATE SPACING
C     C1,C2,C3,C4 - AMPLITUDE AND (X,Y,Z) WAVELENGTHS OF ANALYTIC WINDS
C
C     IOUT   - OUTPUT FIELD NUMBER
C
      CHARACTER*3 NGRD
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA PI,PI2,PI4/3.141592654,6.283185307,12.56637061/

      A=C1
      AX=C1
      AY=C1
      RX=C2
      RY=C3
      RZ=C4

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

      DO 100 J=1,MANG

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

         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            RNG=R0+(I-1)*DROLD
            HRNG=RNG*COSE
            X=X0+HRNG*SINA
            Y=Y0+HRNG*COSA
            Z=H0+RNG*SINE+0.5*HRNG*HRNG*REI

C           RX=0: U COMPONENT IS 0; OTHERWISE VARIES
C
            IF(C1.GE.0.)THEN

            IF(RX.GT.0.0)THEN
               XSIN=SIN(PI2*X/RX)
               XCOS=COS(PI2*X/RX)
            ELSE
               AY=2.0*A
               XSIN=1.0
               XCOS=1.0
            END IF

C           RY=0: V COMPONENT IS 0; OTHERWISE VARIES
C
            IF(RY.GT.0.0)THEN
               YSIN=SIN(PI2*Y/RY)
               YCOS=COS(PI2*Y/RY)
            ELSE
               AX=2.0*A
               YSIN=1.0
               YCOS=1.0
            END IF

C           RZ=0: W COMPONENT IS 0; OTHERWISE VARIES
C
            IF(RZ.GT.0.0)THEN
               ZSIN=SIN(PI2*Z/RZ)
               ZCOS=COS(PI2*Z/RZ)
            ELSE
               ZSIN=1.0
               ZCOS=1.0
            END IF
            U= (AX*RX/PI4)*XSIN*YCOS*ZCOS
            V= (AY*RY/PI4)*XCOS*YSIN*ZCOS
            W=-(A *RZ/PI2)*XCOS*YCOS*ZSIN
            ELSE
               U=C2
               V=C3
               W=C4
            END IF
            DAT(I,J,IOUT)=U*SINA*COSE+V*COSA*COSE+W*SINE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
