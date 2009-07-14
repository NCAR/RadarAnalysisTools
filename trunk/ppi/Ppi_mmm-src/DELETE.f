c
c----------------------------------------------------------------------X
c
      SUBROUTINE DELETE(DAT,IOUT,IIN1,C1,C2,C3,C4,BDVAL,MNGATE,MXGATE,
     X                  NANG,AZA,R0,DROLD,IAZC,MXR,MXA,MXF)
C
C  FUNCTION - DELETE: IF INSIDE (RNG,ANG) WINDOW F(OUT)=BDVAL;
C                     ELSE F(OUT)=F(IIN1).
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     C1,C2  - RANGE WINDOW (MIN,MAX)
C     C3,C4  - ANGLE    "       "
C     IF C1 IS NEGATIVE THEN KEEP EVERY C1 RANGE GATE; IF C2 IS ALSO
C              NEGATIVE THEN FILL BACK IN WITH A LINEAR INTERPOLATION
C
      LOGICAL IAZC
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2)

      DATA EPS/0.1/

      RMN=C1
      RMX=C2
      PAMN=C3
      PAMX=C4
      IF(C1.LT.0.)GO TO 250

      DO 100 J=1,NANG
         PANG=AZA(J,1)
         IF(PANG.LT.0.0)THEN
            TANG=PANG+360.0
         ELSE
            TANG=PANG
         END IF
         DO 90 I=MNGATE,MXGATE
            RNG=R0+(I-1)*DROLD
            DATIN1=DAT(I,J,IIN1)
            IF( (TANG.GE.PAMN .AND. TANG.LE.PAMX) .AND.
     +          ( RNG.GE.RMN  .AND.  RNG.LE.RMX) )THEN
               DAT(I,J,IOUT)=BDVAL
            ELSE
               DAT(I,J,IOUT)=DATIN1
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN

 250  CONTINUE
      ISKIP=-NINT(C1)
      DEL=-C1
      DO 300 J=1,NANG
         DO 310 I=MNGATE,MXGATE

            IF(MOD(I,ISKIP).EQ.0)THEN
               DAT(I,J,IOUT)=DAT(I,J,IIN1)
            ELSE
               DAT(I,J,IOUT)=BDVAL
            END IF
 310     CONTINUE
         IF(C2.LT.0.)THEN
            DO 320 I=MNGATE,MXGATE-ISKIP
               IF(DAT(I,J,IOUT).EQ.BDVAL.OR.
     +          DAT(I+ISKIP,J,IOUT).EQ.BDVAL)GO TO 320
               DIFF=DAT(I+ISKIP,J,IOUT)-DAT(I,J,IOUT)
               DO 330 K=1,ISKIP-1
                  DAT(I+K,J,IOUT)=DAT(I,J,IOUT)+DIFF*FLOAT(K)/DEL
 330           CONTINUE
 320        CONTINUE
         END IF
 300  CONTINUE
      RETURN
      END
