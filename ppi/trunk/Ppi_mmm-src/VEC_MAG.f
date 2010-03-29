c
c----------------------------------------------------------------------X
c
      SUBROUTINE VEC_MAG(DAT,IOUT,IIN1,IIN2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - VEC_MAG: F(OUT)=SQRT(F(IIN1)*F(IIN1)+F(IIN2)*F(IIN2))
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C     DAT1   - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DAT2   - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)

      DO J=1,MXA
         DO I=1,MXR
            DAT(I,J,IOUT)=BDVAL
         END DO
      END DO

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DAT1=DAT(I,J,IIN1)
            DAT2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DAT1.NE.BDVAL.AND.DAT2.NE.BDVAL)THEN
               DAT(I,J,IOUT)=SQRT(DAT1*DAT1+DAT2*DAT2)
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
