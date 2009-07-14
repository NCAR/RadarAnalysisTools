c
c----------------------------------------------------------------------X
c
      SUBROUTINE FLOOR(DAT,IOUT,IIN1,IIN2,C1,C2,C3,BDVAL,MNGATE,MXGATE,
     X     NANG,MXR,MXA,MXF)
C
C  FUNCTION - FLOOR: F(OUT)=F(IIN1); IF F(IIN1) .GT. C1
C                      "   =C1        "    "    .LE. C1 
C                      "   =C2        "    "    .EQ. BDVAL
C  Note:  Coded to allow inflation of a dB field by amount C3, while 
C         maintaining the input FLOOR value.  Convert to linear values 
C         before multiplying by C3, then convert back to dB.
C         LJM - 07/11/96
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF)

      P1=10.0**(0.1*C1)
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            IF(DATIN1.EQ.BDVAL)THEN
               DAT(I,J,IOUT)=C2
               GO TO 90
            END IF
            IF(DATIN1.GT.C1)THEN
               IF(C3.NE.0.0)THEN
                  PIN1=10.0**(0.1*DATIN1)
                  PIN2=C3*(PIN1-P1)+P1
                  DAT(I,J,IOUT)=10.0*ALOG10(PIN2)
               ELSE
                  DAT(I,J,IOUT)=DATIN1
               END IF
            ELSE
               DAT(I,J,IOUT)=C1
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
