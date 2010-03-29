c
c----------------------------------------------------------------------X
c
      SUBROUTINE VEC_DIR(DAT,IOUT,IIN1,IIN2,BDVAL,AZA,MNGATE,MXGATE,
     X                   NANG,MXR,MXA,MXF)
C
C  FUNCTION - VEC_DIR: F(OUT)=TODEG*ATAN2(F(IIN1)/F(IIN2))
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C     DAT1   - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DAT2   - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2)
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA EPS /1.0E-12/

      DO 100 J=1,NANG
         IF(AZA(J,1).LT.0.0)THEN
            AZM=AZA(J,1)+360.0
         ELSE
            AZM=AZA(J,1)
         END IF
         DO 90 I=MNGATE,MXGATE
            DAT1=DAT(I,J,IIN1)
            DAT2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(ABS(DAT1*DAT2).LE.EPS)GO TO 90
            IF(DAT1.NE.BDVAL.AND.DAT2.NE.BDVAL)THEN
               ANGL=TODEG*ATAN2(DAT1,DAT2)
               IF(ANGL.LT.  0.0)ANGL=ANGL+360.0
               ANGL=ANGL+AZM
               IF(ANGL.GT.360.0)ANGL=ANGL-360.0
               DAT(I,J,IOUT)=ANGL
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
