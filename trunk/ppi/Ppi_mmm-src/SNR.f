c
c----------------------------------------------------------------------X
c
      SUBROUTINE SNR(DAT,IOUT,IIN1,C1,BDVAL,MNGATE,MXGATE,NANG,
     X     MXR,MXA,MXF)
C
C  FUNCTION - SNR: F(OUT)=PSIG-PNOI (DB)
C             PSIG=10*ALOG10(10**(0.1*P)-10**(0.1*C1))
C
C     P      - MEASURED SIGNAL+NOISE POWER (DBM)
C     C1     - AVERAGE NOISE POWER (DBM)
C     DAT    - OUTPUT SNR POWER (DB SCALE)
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C     DATIN1 - TEMPORARY VARIABLE TO ALLOW IOUT=IIN1
C
      DIMENSION DAT(MXR,MXA,MXF)

C     CALCULATE LINEAR (SIGNAL+NOISE) AND NOISE POWERS,
C     THEN SUBTRACT AND CONVERT BACK TO SIGNAL POWER (DBM).
C
      PNOI=10.0**(0.1*C1)
      DO 100 J=1,NANG
         DO 90 I=MNGATE,MXGATE
            DATIN1=DAT(I,J,IIN1)
            DAT(I,J,IOUT)=BDVAL
            IF(DATIN1.EQ.BDVAL)GO TO 90
            PMEA=10.0**(0.1*DATIN1)
            IF(PMEA.LE.PNOI)GO TO 90
            PSIG=10.0*ALOG10(PMEA-PNOI)
            DAT(I,J,IOUT)=PSIG-C1
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
