c
c----------------------------------------------------------------------X
c
      SUBROUTINE PRVOLMFF(IHSK,IPREC,RNGCOR,NFRAME)
C
C  PRINT OUT RP-6 OR RP-7 VOLUME HOUSEKEEPER
C
      CHARACTER*4 SMODE(9)
      DIMENSION IHSK(256)
      DATA CF/182.044444/
      DATA SMODE/'CALB','SECT','COPL',
     +           'RHI ','VERT','FIXD',
     +           'MANL','IDLE','SURV'/
      DATA S_BWLOSS,X_BWLOSS/3.2,3.2/
      DATA S_INBYSS,X_INBYSS/0.0,2.5/
C
      LREC   = IHSK(1)
      IYR    = IHSK(4)
      IMON   = IHSK(5)
      IDAY   = IHSK(6)
      IHR    = IHSK(7)
      IMIN   = IHSK(8)
      ISEC   = IHSK(9)
      AZ     = IHSK(10)/CF
      EL     = IHSK(11)/CF
      DR     = IHSK(14)/1000.
      R0     = IHSK(12)+IHSK(13)*.001+RNGCOR+DR
      IF(EL.GT.180.0)EL=EL-360.0
      IGSP   = IHSK(14)
      NGTS   = IHSK(15)
      VNYQ   = FLOAT(IHSK(20))*FLOAT(IHSK(21))*.0000025
      ISWP   = IHSK(22)
      MODE   = IHSK(26)
      CWANGL = IHSK(27)/CF
      CCANGL = IHSK(28)/CF
      TTANGL = IHSK(29)/CF
      BBANGL = IHSK(30)/CF
      FXANG  = IHSK(31)/CF
      IVOL   = IHSK(48)
      IFN    = IHSK(49)
      ITF    = IHSK(61)
      IDPROC = IHSK(62)
      IDRADR = IHSK(63)
      N16H   = IHSK(65)
      NFLD   = IHSK(68)
      IF(IDRADR.EQ.2.AND.IDPROC.EQ.6)THEN
         IZDR=AND(IHSK(247),1)
         IF(IZDR.EQ.1)VNYQ=0.5*VNYQ
      END IF
      IDATE  = 10000*IYR+100*IMON+IDAY
      ITIME  = 10000*IHR+100*IMIN+ISEC

C     CALIBRATION INFORMATION
C     IHSK(16) - NUMBER OF TIME SERIES SAMPLES
C     IHSK(17) - TEST PULSE LEVEL (DBM*10)
C     IHSK(18) - AVERAGE TRANSMITTED POWER (DBM*10)
C     IHSK(19) - PULSE DURATION (NANOSEC)
C     IHSK(20) - PULSE REPETITION FREQUENCY (HZ*10)
C     IHSK(21) - WAVELENGTH (CM*100)
C     IHSK(33) - ALL SYSTEM LOSSES (DB*10)
C     IHSK(39) - SYSTEM GAIN (DB*10)
C     IHSK(44) - ANTENNA BEAMWIDTH (DEGREES*100)
C
      NSMP   = IHSK(16)
      TPL    = FLOAT(IHSK(17))/10.0
      PAVG   = FLOAT(IHSK(18))/10.0
      TAU    = FLOAT(IHSK(19))
      PRF    = FLOAT(IHSK(20))/10.0
      WAVL   = FLOAT(IHSK(21))/100.0
      CLOSS  = FLOAT(IHSK(33))/10.0
      GAIN   = FLOAT(IHSK(39))/10.0
      BEAMW  = FLOAT(IHSK(44))/100.0
      PEAK   = 0.0
      RADCON = 0.0
      IF(PRF*TAU .GT. 0.0)PEAK = PAVG-10.0*ALOG10(PRF*TAU*1.0E-09)

C     PRIMARY WAVELENGTH RADAR CONSTANT
C
      IF(BEAMW.GT.0.0)RPARM=PRF*WAVL*WAVL/(BEAMW*BEAMW)
      IF(RPARM.GT.0.0)THEN
         RADCON=164.306+10.0*ALOG10(RPARM)-PAVG-2.0*GAIN
     +          +S_BWLOSS+S_INBYSS
      END IF

C     SECONDARY (X-BAND) WAVELENGTH RADAR CONSTANT
C
      IF(IDRADR.EQ.2.AND.IDPROC.EQ.6)THEN
         PAVGX  = FLOAT(IHSK(202))/10.0
         PRFX   = FLOAT(IHSK(204))/10.0
         WAVLX  = FLOAT(IHSK(201))/100.0
         GAINX  = FLOAT(IHSK(205))/10.0
         BEAMWX = FLOAT(IHSK(207))/100.0
         RADCONX= 0.0
         IF(BEAMWX.GT.0.0)RPARMX=PRFX*WAVLX*WAVLX/(BEAMWX*BEAMWX)
         IF(RPARMX.GT.0.0)THEN
            RADCONX=164.306+10.0*ALOG10(RPARMX)-PAVGX-2.0*GAINX
     +              +X_BWLOSS+X_INBYSS
         END IF
      END IF

      WRITE(6,35)IVOL,IDATE,ITIME,CWANGL,CCANGL,BBANGL,TTANGL,
     +           FXANG,SMODE(MODE+1),NFLD,IFN,NGTS,IGSP,NSMP,
     +           BEAMW,VNYQ,WAVL,PRF,TAU,PAVG,PEAK,GAIN,CLOSS,
     +           LREC,IPREC
 35   FORMAT(1X,/,
     +' VOLUME = ',  I6,
     +'  DATE=',I6.6,'  TIME=',  I6.6,'   CWL=',F6.2,'   CCL=', F6.2,
     +'   BEG=',F6.2,'   END=',F6.2,'   FXA=',F6.2,'  SMOD=',2X,A4,/,
     +16X,'  NFLD=',  I6,'  NFLT=',  I6,'  NGTS=',  I6,'   GSP=',   I6,
     +'  NSMP=',  I6,'  BEAM=',F6.2,'  VNYQ=',F6.2,'  WAVL=', F6.2,/,
     +16X,'   PRF=',F6.1,'   TAU=',F6.1,'  PAVG=',F6.1,'  PEAK=', F6.1,
     +'  GAIN=',F6.1,'  LOSS=',F6.1,'  LREC=',  I6,'  PREC=',   I6)
      IF(IDRADR.EQ.2.AND.IDPROC.EQ.6)THEN
         WRITE(6,37)R0,RADCON,S_BWLOSS,S_INBYSS,
     +                 RADCONX,X_BWLOSS,X_INBYSS
 37      FORMAT(18X,'Nominal range to leading edge of first gate:',F6.3,
     +        /,18X,'  Primary wavelength radar constant:',F6.1,
     +              ' BW loss:',F6.1,' Integration bias:',F6.1,
     +        /,18X,'Secondary wavelength radar constant:',F6.1,
     +              ' BW loss:',F6.1,' Integration bias:',F6.1)
      ELSE
         WRITE(6,39)R0,RADCON,S_BWLOSS,S_INBYSS
 39      FORMAT(18X,'Nominal range to leading edge of first gate:',F6.3,
     +         /,18X,' Primary wavelength radar constant:',F6.1,
     +              ' BW loss:',F6.1,' Integration bias:',F6.1)
      END IF

      WRITE(6,41)NFRAME+1
 41   FORMAT(/,' VOLUME SCAN STARTING FRAME =',I6)

      RETURN
      END






