c
c----------------------------------------------------------------------X
c
      SUBROUTINE CSUBN(DAT,IOUT,IIN1,C1,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - CSUBN: F(OUT)=(PI**5)(K**2)(MM6/M3)/(0.38*WVL**11/3)
C     CONVERT FROM RADAR REFLECTIVITY FACTOR (DBZ) TO REFRACTIVE INDEX
C     STRUCTURE CONSTANT CNSQ.  With DBZ in mm6/m3 and wvl in cm, CNSQ
C     units are 10^-13 m^(-2/3).  AT 10.71 CM WAVELENGTH, CNSQ IS NEARLY 
C     THE SAME AS DBZ.  If the input wavelength (C1) is < 0, then output
C     radar reflectivity factor from input Cn^2.
C
C     C1     - WAVELENGTH IN CM
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF)
      DATA PI,DIFAC/3.141592654,0.93/

      PI5=PI**5
      COEF=(PI5*DIFAC/0.38)*10.0**(7./3.)
      COEFI=1.0/(COEF)
      WAVL=ABS(C1)
      WVL113=WAVL**3.666667
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            CNSQ=(COEF/WVL113)*10.0**(0.1*DATIN1)
            REFL=(COEFI*WVL113)*10.0**(0.1*DATIN1)
            IF(C1.GT.0.0)THEN
               IF(CNSQ.LE.0.0)GO TO 90
               IF(DATIN1.NE.BDVAL)DAT(I,J,IOUT)=10.0*ALOG10(CNSQ)
            ELSE IF(C1.LT.0.0)THEN
               IF(REFL.LE.0.0)GO TO 90
               IF(DATIN1.NE.BDVAL)DAT(I,J,IOUT)=10.0*ALOG10(REFL)
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
