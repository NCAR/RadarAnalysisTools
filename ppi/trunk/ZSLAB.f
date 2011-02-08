c
c----------------------------------------------------------------------X
c
      SUBROUTINE ZSLAB(DAT,IOUT,IIN1,C1,C2,BDVAL,MNGATE,MXGATE,NANG,
     X                 AZA,ELA,R0,DROLD,H0,MXR,MXA,MXF)
C
C  FUNCTION - ZSLAB: F(OUT)=F(IN) IF C1 .LE. HEIGHT .LE. C2
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)
      DATA RE,TORAD/17000.0,0.017453293/

      DO 100 J=1,NANG
         SINE=SIN(ELA(J,1)*TORAD)
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            Z=H0+SINE*(R0+(I-1)*DROLD)
            IF(Z.GE.C1 .AND. Z.LE.C2)THEN
               DAT(I,J,IOUT)=DATIN1
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
