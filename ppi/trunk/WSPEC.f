c
c----------------------------------------------------------------------X
c
      SUBROUTINE WSPEC(DAT,IOUT,IIN1,IIN2,C1,VNYQ,BDVAL,MNGATE,MXGATE,
     X     NANG,MXR,MXA,MXF)
C
C  FUNCTION - WSPEC: F(OUT)=(2*VNYQ/PI)*SQRT[ln(1/F(IN)], where
C                    F(IN) = NCP
C     CONVERT Normalized coherent power to spectral width
C     Modify NCP by [P/(P-N)], where P is total power, N is
C            noise power, and P-N is signal power
C            Note: Powers must be converted to linear from dBm
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     IIN2   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)
      DATA PI/3.1415926535898/

      print *,'WSPEC: vnyq=',vnyq

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.LE.0.0)GO TO 90
            IF(DATIN2.EQ.BDVAL)GO TO 90
            PMEA=10.0**(0.1*DATIN2)
            PNOI=10.0**(0.1*C1)
            PSIG=PMEA-PNOI
            DATIN1=DATIN1*PMEA/PSIG
            IF(DATIN1.NE.BDVAL)THEN
               DAT(I,J,IOUT)=(2.0*VNYQ/PI)*SQRT(ALOG(1.0/DATIN1))
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
