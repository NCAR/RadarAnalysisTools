c
c----------------------------------------------------------------------X
c
      SUBROUTINE LOGTEN(DAT,IOUT,IIN1,C1,C2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - LOGTEN: F(OUT)=C1*ALOG10(C2*F(IN))
C     CONVERT LINEAR TO DB
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
            IF(DATIN1.LE.0.0)GO TO 90
            IF(DATIN1.NE.BDVAL)THEN
               DAT(I,J,IOUT)=C1*ALOG10(C2*DATIN1)
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
