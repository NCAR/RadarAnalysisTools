c
c----------------------------------------------------------------------X
c
      SUBROUTINE REFLECT(DAT,IOUT,IIN1,C1,BDVAL,MNGATE,MXGATE,NANG,RCOR,
     X     MXR,MXA,MXF)
C
C  FUNCTION - REFLECT: F(OUT)= C1+F(IN)+20*LOG(RANGE), if C1 > 0
C                      F(IN) assumed to be power in dBM
C             POWER:   F(OUT)= C1+F(IN)-20*LOG(RANGE), if C1 < 0
C                      F(IN) assumed to be reflectivity in dBZ
C
C        C1  - RADAR CONSTANT
C      RCOR  - 20*LOG(RANGE (KM))
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF),RCOR(MXR)

      SIGN_C1 = SIGN(1.0,C1)
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.NE.BDVAL)THEN
               DAT(I,J,IOUT)=C1+DATIN1+SIGN_C1*RCOR(I)
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
