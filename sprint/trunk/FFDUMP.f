
      SUBROUTINE FFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
C
C     Routine to dump ATD/FOF Field Format (RP3-7) housekeeping
C     STATUS OF READ: (Set in RAYIN, entry NWRAY)
C            NST = (0) Good read, (1) End-of-file, (3) End-of-data
C-----------------------------------------------------------------------
C     ATD/FOF Radar processor (3-7) Housekeeping words:
C         4 - Year of the data (last two digits)
C         5 - Month of the data
C         6 - Day of the data
C         7 - Hour of the data
C         8 - Minute of the data
C         9 - Second of the data
C        10 - Current azimuth angle (degrees * CF)
C        11 - Current elevation angle (degrees *CF)
C        14 - Gate spacing (m)
C        15 - Number of range gates
C        20 - Primary wavelength PRF (Hz * 10)
C        21 - Primary wavelength (cm * 100)
C        22 - Sequential sweep number
C        26 - Scan mode: (0) Calibration, (1) PPI or sector, (2) Coplane,
C                        (3) RHI, (4) Vertical, (5) Target, (6) Manual, 
C                        (7) Idle, (8) Surveillance.
C        31 - Fixed angle (degrees * CF)
C        36 - Primary coplane baseline angle (degrees *CF)
C        61 - Beam transition flag (0 - in the scan, 1 - in transition)
C        62 - Data system identifier (RP3, 4, 5, 6, or 7)
C        63 - Radar system identifier (CP2, 3, 4 or "MH", 
C             with MH=4d48 hexadecimal)
C        68 - Number of parameters (fields) associated with each gate
C-----------------------------------------------------------------------
C
      DIMENSION IBUF(MAXIN)
      CHARACTER*8 NAMUF
      DATA CF/182.044444/
      DATA PXMIT/55.0/

      IF (NST.EQ.0)THEN
         
C     OBTAIN OTHER BASIC HOUSEKEEPING INFORMATION
C
         LREC  = IBUF(1)
         IYR   = IBUF(4)
         IMON  = IBUF(5)
         IDAY  = IBUF(6)
         IDATE = 10000*IBUF(4)+100*IBUF(5)+IBUF(6)
         ITIME = IBUF(7)*10000+IBUF(8)*100+IBUF(9)
         AZ    = IBUF(10)/CF
         EL    = IBUF(11)/CF
         DR    = IBUF(14)/1000.
         IF(EL.GT.180.0)EL=EL-360.0
         FXANG = IBUF(31)/CF
         NGTS  = IBUF(15)
         VNYQ  = FLOAT(IBUF(21))*FLOAT(IBUF(20))*.0000025
         ISWP  = IBUF(22)
         ITP   = IBUF(26)
         IVOL  = IBUF(48)
         ITF   = IBUF(61)
         IDPROC= IBUF(62)
         IDRADR= IBUF(63)
         NTBITS= 16*IBUF(66)
         LRECN = AND(IBUF(67),255)
         NFLD  = IBUF(68)
         IF(IDRADR.EQ.2.AND.IDPROC.EQ.6)THEN
            IZDR=AND(IBUF(247),1)
            IF(IZDR.EQ.1)VNYQ=0.5*VNYQ
            PAVG=FLOAT(IBUF(18))/10.0
            IF(PAVG.LT.PXMIT)ITF=1
         END IF

         WRITE(7,771)IDATE,ITIME,AZ,EL,FXANG,ITP,ITF,
     +        NFLD,NGTS,DR,VNYQ,IVOL,NAZZ,LRECN,LREC,IPREC
 771     FORMAT(1X,' D=',I6,' T=',I6.6,' A=',F6.2,' E=',F6.2,
     +        ' Fx=',F6.2,' M=',I1,' Fl=',I1,' Nfl=',I1,' Ng=',I4,
     +        ' Dr=',F5.3,' Nq=',F5.2,' Vol=',I3,' Na=',I4,
     +        ' Lrn=',I2,' Lr=',I5,' Tr=',I5)
      ELSE IF(NST.EQ.1)THEN
         NAZZ=NAZZ-1
         IF(NAZZ.LT.0)NAZZ=0
         WRITE(7,772)IPREC,NAZZ
 772     FORMAT(1X,'FFDUMP: End-of-file: iprec,nang=',I4,I5)
      ELSE IF(NST.EQ.2)THEN
         NAZZ=NAZZ-1
         IF(NAZZ.LT.0)NAZZ=0
         WRITE(7,773)IPREC,NAZZ
 773     FORMAT(1X,'FFDUMP: End-of-data: iprec,nang=',I4,I5)
      END IF     

      RETURN
      END
