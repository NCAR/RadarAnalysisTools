c
c----------------------------------------------------------------------X
c
      SUBROUTINE SUMFLD(DAT,IOUT,IIN1,IIN2,C1,C2,C3,C4,BDVAL,MNGATE,
     X     MXGATE,NANG,MXR,MXA,MXF)
C
C  FUNCTION - SUMFLD: F(OUT)=C1*F(IIN1)+C2*F(IIN2)
C                     WITH RANGE SHIFTS OF C3 AND C4
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "   (FIELD ONE)
C     IIN2   -  INPUT   "      "   (  "   TWO)
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)

      I1=NINT(C3)
      I2=NINT(C4)
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            IS1=I+I1
            IF(IS1.GE.1 .AND. IS1.LE.MXGATE)THEN
               DATIN1=DAT(IS1,J,IIN1)
            ELSE
               DATIN1=BDVAL
            END IF
            IS2=I+I2
            IF(IS2.GE.1 .AND. IS2.LE.MXGATE)THEN
               DATIN2=DAT(IS2,J,IIN2)
            ELSE
               DATIN2=BDVAL
            END IF
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.NE.BDVAL.AND.DATIN2.NE.BDVAL)THEN
               DAT(I,J,IOUT)=C1*DATIN1+C2*DATIN2
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
