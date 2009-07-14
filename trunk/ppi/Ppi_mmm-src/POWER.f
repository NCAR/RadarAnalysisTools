c
c----------------------------------------------------------------------X
c
      SUBROUTINE POWER(DAT,IOUT,IIN1,C1,C2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - POWER: F(OUT)=C1*F(IN)**C2
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF)

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.EQ.BDVAL.OR.DATIN1.LT.0.0)GO TO 90
            DAT(I,J,IOUT)=C1*(DATIN1**C2)
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
