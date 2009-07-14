c
c----------------------------------------------------------------------X
c
      SUBROUTINE ROTATE(DAT,IOUT,IIN1,IIN2,C1,C2,BDVAL,MNGATE,MXGATE,
     X                  NANG,ANG,MXR,MXA,MXF)
C
C  FUNCTION - ROTATE: F(OUT)=C1*F(IN1)*COS(ANG)+C2*F(IN2)*SIN(ANG)
C                     U= VR*COS(E) - VE*SIN(E); (C1,C2)=(+1,-1)
C                     W= VE*COS(E) + VR*SIN(E); (C1,C2)=)+1,+1)
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     IIN2   -    "     "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF),ANG(MXA,2)
      DATA TORAD,TODEG/0.017453293,57.29577951/

      DO 100 J=1,NANG
         SINA=SIN(TORAD*ANG(J,1))
         COSA=COS(TORAD*ANG(J,1))
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.NE.BDVAL.AND.DATIN2.NE.BDVAL)THEN
               DAT(I,J,IOUT)=C1*COSA*DATIN1+C2*SINA*DATIN2
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
