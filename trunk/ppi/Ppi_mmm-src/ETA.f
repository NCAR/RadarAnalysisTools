c
c----------------------------------------------------------------------X
c
      SUBROUTINE ETA(DAT,IOUT,IIN1,C1,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - ETA: F(OUT)=(PI**5)(K**2)(MM6/M3)/(WVL**4)
C     Convert from radar reflectivity factor (DBZ) to radar reflectivity.
C     With DBZ in mm6/m3 and wvl in cm, Eta units are 10^-12 cm^(-1).  
C     If the input wavelength (C1) is < 0, then output radar reflectivity 
C     factor (DBZ) from input Eta.
C
C     C1     - WAVELENGTH IN CM
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF)
      DATA PI,DIFAC/3.141592654,0.93/

      PI5=PI**5
      COEF=PI5*DIFAC
      COEFI=1.0/(COEF)
      WAVL=ABS(C1)
      WVL4=WAVL**4.0
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            ZETA=(COEF/WVL4)*10.0**(0.1*DATIN1)
            REFL=(COEFI*WVL4)*10.0**(0.1*DATIN1)
            IF(C1.GT.0.0)THEN
               IF(ZETA.LE.0.0)GO TO 90
               IF(DATIN1.NE.BDVAL)DAT(I,J,IOUT)=10.0*ALOG10(ZETA)
            ELSE IF(C1.LT.0.0)THEN
               IF(REFL.LE.0.0)GO TO 90
               IF(DATIN1.NE.BDVAL)DAT(I,J,IOUT)=10.0*ALOG10(REFL)
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
