c
c----------------------------------------------------------------------X
c
      SUBROUTINE ORELSE(DAT,IOUT,IIN1,IIN2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - ORELSE: F(OUT)=F(IIN1) IF NOT BDVAL .ORELSE.
C                            F(IIN2) IF NOT BDVAL
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     IIN2   -    "     "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C     DATIN2 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN2
C
      DIMENSION DAT(MXR,MXA,MXF)

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.NE.BDVAL)THEN
               DAT(I,J,IOUT)=DATIN1
            ELSE IF(DATIN2.NE.BDVAL)THEN
               DAT(I,J,IOUT)=DATIN2
            END IF
   90    CONTINUE
  100 CONTINUE

      RETURN
      END
