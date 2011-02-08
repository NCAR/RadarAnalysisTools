c
c----------------------------------------------------------------------X
c
      SUBROUTINE CEILING(DAT,IOUT,IIN1,IIN2,C1,C2,BDVAL,MNGATE,MXGATE,
     X     NANG,MXR,MXA,MXF)
C
C  FUNCTION - CEILING: F(OUT)=F(IIN1); IF F(IIN1) .LT. C1
C                        "   =C1        "    "    .GE. C1
C                        "   =C2        "    "    .EQ. BDVAL
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF)

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            IF(DATIN1.EQ.BDVAL)THEN
               DAT(I,J,IOUT)=C2
               GO TO 90
            END IF
            IF(DATIN1.LT.C1)THEN
               DAT(I,J,IOUT)=DATIN1
            ELSE
               DAT(I,J,IOUT)=C1
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
