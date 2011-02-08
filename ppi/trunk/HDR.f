c
c----------------------------------------------------------------------X
c
      SUBROUTINE HDR(DAT,IOUT,IIN1,IIN2,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)

C     CALCULATE THE DIFFERENTIAL REFLECTIVITY HAIL SIGNAL (HDR) AS DEFINDED
C     BY AYDIN ET AL. (JCAM, 1986).  HDR= ZH-f(ZDR) WHERE
C                  ZH=HORIZONTAL REFLECTIVITY FACTOR
C                  f(ZDR)=  27        for ZDR<=0 dB
C                        =  19*ZDR+27 for 0<ZDR<=1.74
C                        =  60        for ZDR>1.74
C
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT HORIZONTAL REFLECTIVITY FIELD NUMBER
C     IIN2   -  INPUT ZDR FIELD NUMBER
C     IOUT   -  OUTPUT HDR FIELD NUMBER


C
      DIMENSION DAT(MXR,MXA,MXF)

      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DATIN2=DAT(I,J,IIN2)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.NE.BDVAL.AND.DATIN2.NE.BDVAL)THEN
               IF(DATIN2.LE.0)THEN
                  DAT(I,J,IOUT)=DATIN1-27.
               ELSE IF(DATIN2 .GT. 1.74)THEN
                  DAT(I,J,IOUT)=DATIN1-60.
               ELSE
                  DAT(I,J,IOUT)=DATIN1-27.0-19.0*DATIN2
               END IF
            END IF
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
