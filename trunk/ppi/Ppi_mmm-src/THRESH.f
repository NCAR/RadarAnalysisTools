c
c----------------------------------------------------------------------X
c
      SUBROUTINE THRESH(DAT,IOUT,IIN1,IIN2,C1,C2,BDVAL,MNGATE,MXGATE,
     X                  NANG,MXR,MXA,MXF)
C
C  FUNCTION - THRESHOLD: F(OUT)=F(IN1); IF C1 .LE. F(IN2) .LE. C2
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     IIN2   -    "     "      "  (THRESHOLDING ON THIS FIELD)
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN2.GE.C1 .AND. DATIN2.LE.C2)THEN
               DAT(I,J,IOUT)=DATIN1
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
