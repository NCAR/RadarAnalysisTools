c
c----------------------------------------------------------------------X
c
      SUBROUTINE GAMMA(DAT,IOUT,IIN1,IIN2,C1,C2,C3,C4,BDVAL,MNGATE,
     X     MXGATE,NANG,MXR,MXA,MXF)
C
C  FUNCTION - CONC: F(OUT)=F(IIN1)*GAMMA(C1)/(F(IIN2)^C2)

C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C
      DIMENSION DAT(MXR,MXA,MXF)

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            IF(DAT(I,J,IIN1).NE.BDVAL.AND.DAT(I,J,IIN2).NE.BDVAL)THEN
               DENOM=DAT(I,J,IIN2)**C2
               DAT(I,J,IOUT)=DAT(I,J,IIN1)*EXP(GAMMLN(C1))/DENOM
            ELSE
               DAT(I,J,IOUT)=BDVAL
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
