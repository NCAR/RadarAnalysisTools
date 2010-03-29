c
c----------------------------------------------------------------------X
c
      SUBROUTINE SUMDB(DAT,IOUT,IIN1,IIN2,C1,C2,C3,BDVAL,MNGATE,MXGATE,
     X                 NANG,MXR,MXA,MXF)
C
C  FUNCTION - SUMDB: F(OUT)=10.0*ALOG10(C1*PIN1+C2*PIN2)
C                    PIN1=10.0**(0.1*F(IN1))
C                    PIN2=10.0**(0.1*F(IN2))
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)

C     LIMIT DBZ(S)-DBZ(X) TO DBDIF TO SET A NATURAL LIMIT ON DZR
C     IF DIFFERENCE EXCEEDS DBDIF, THEN SET DBZ(S)=DBZ(X)+DBDIF
C
      DBDIF=C3
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.NE.BDVAL.AND.DATIN2.NE.BDVAL)THEN
               IF(DBDIF.GT.0.0)THEN
                  IF( (DATIN2-DATIN1).GT.DBDIF)DATIN2=DATIN1+DBDIF
               END IF
               PIN1=10.0**(0.1*DATIN1)
               PIN2=10.0**(0.1*DATIN2)
               SUM=C1*PIN1+C2*PIN2
               IF(SUM.GT.0.0)DAT(I,J,IOUT)=10.0*ALOG10(SUM)
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
