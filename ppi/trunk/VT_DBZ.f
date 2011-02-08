c
c----------------------------------------------------------------------X
c
      SUBROUTINE VT_DBZ(DAT,IOUT,IIN1,H0,C1,C2,C3,C4,BDVAL,MNGATE,
     X     MXGATE,NANG,MXR,MXA,MXF,AZA,ELA,R0,DROLD,ITPOLD)
C
C  FUNCTION - VT_DBZ: F(OUT)=[A*Z**B]*[Do/D(z)]**0.4, where D=Density.
C     Compute particle fallspeed from measured radar reflectivity using
C     a power-law relationship, corrected for density aloft where
C     Density(z) = Do*exp(-kz) with k=1/10 ==> DenCor = exp(0.4*kz)
C
C     X0,Y0,HO  - (X,Y,Z) COORDINATES FOR  THIS   RADAR
C     C1,C2  - Coefficient, Exponent in power-law relationship (A,B)
C     C3     - Exponent in density-correction term (C)
C     C4     - Coefficient (k) in density term D(z)=Do*exp(-k*z)
C              Note: C4 .LT. 0 turns off density weighting
C            - Default (A,B,C,K) = (1.5,0.105,0.4,0.1)
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)

      REAL KAPPA
      DATA KAPPA/0.1/
      DATA A0,B0,C0/1.5,0.105,0.4/
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD/0.017453293/

      IF(C1.NE.0.0)THEN
         A=C1
      ELSE
         A=A0
      END IF
      IF(C2.NE.0.0)THEN
         B=C2
      ELSE
         B=B0
      END IF
      IF(C3.NE.0.0)THEN
         C=C3
      ELSE
         C=C0
      END IF
      IF(C4.LT.0.0)THEN
         D=0.0
      ELSE IF(C4.EQ.0.0)THEN
         D=KAPPA
      ELSE IF(C4.GT.0.0)THEN
         D=C4
      END IF
      print *,'VT_DBZ: a,b,c,d=',a,b,c,d

      DO 100 J=1,NANG

C        RHI scan: Elevation angle in AZA
C        Other scans: Elevation angle in ELA
C
         IF(ITPOLD.EQ.3)THEN
            ELRAD=AZA(J,1)
         ELSE
            ELRAD=ELA(J,1)
         END IF
         SINE=SIN(ELRAD*TORAD)
         COSE=COS(ELRAD*TORAD)

         DO 90 I=1,MXR

            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            IF(I.LT.MNGATE)GO TO 90
            IF(I.GT.MXGATE)GO TO 90
            IF(DATIN1.EQ.BDVAL)GO TO 90

            RNG=R0+(I-1)*DROLD
            HRNG=COSE*RNG
            Z=H0+RNG*SINE+0.5*HRNG*HRNG*REI
            DENCOR=EXP(D*Z*C)
            VT=DENCOR*A*10.0**(0.1*B*DATIN1)
            DAT(I,J,IOUT)=VT

   90    CONTINUE
  100 CONTINUE
      RETURN
      END
